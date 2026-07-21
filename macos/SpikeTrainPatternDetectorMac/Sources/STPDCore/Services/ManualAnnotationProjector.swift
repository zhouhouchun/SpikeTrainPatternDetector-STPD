import Foundation

/// The result of projecting manual annotations onto one train's per-ISI auto labels. Pure value type;
/// carries both the resolved per-ISI maps and audit/diagnostic counts. All maps are keyed by ISI index.
public struct ManualAnnotationProjection: Hashable, Sendable {
    /// Positive manual label (canonical pattern string) per covered ISI.
    public let manualPositiveLabelByISI: [Int: String]
    /// ISIs under an active `notBurst` veto.
    public let manualNegativeVetoByISI: Set<Int>
    /// Manual-first final label per ISI: positive manual wins; otherwise the auto label that survived
    /// the (optional) burst veto. Negative/veto labels never appear here.
    public let finalLabelByISI: [Int: String]
    /// Auto labels still available for public output after lock + veto suppression. Equals the input
    /// auto map when there is no manual effect.
    public let effectiveAutoLabelByISI: [Int: String]
    /// Auto-labeled ISIs suppressed because a positive manual label covers them AND `honorManualLock`.
    public let autoBlockedByManualLockISIs: Set<Int>
    /// Burst-family auto-labeled ISIs suppressed because a `notBurst` veto covers them.
    public let autoBurstBlockedByVetoISIs: Set<Int>
    public let positiveCoverageISICount: Int
    public let negativeCoverageISICount: Int
    /// Annotations that did not resolve to this train (wrong train / outside its time span).
    public let skippedAnnotationCount: Int

    public init(
        manualPositiveLabelByISI: [Int: String],
        manualNegativeVetoByISI: Set<Int>,
        finalLabelByISI: [Int: String],
        effectiveAutoLabelByISI: [Int: String],
        autoBlockedByManualLockISIs: Set<Int>,
        autoBurstBlockedByVetoISIs: Set<Int>,
        positiveCoverageISICount: Int,
        negativeCoverageISICount: Int,
        skippedAnnotationCount: Int
    ) {
        self.manualPositiveLabelByISI = manualPositiveLabelByISI
        self.manualNegativeVetoByISI = manualNegativeVetoByISI
        self.finalLabelByISI = finalLabelByISI
        self.effectiveAutoLabelByISI = effectiveAutoLabelByISI
        self.autoBlockedByManualLockISIs = autoBlockedByManualLockISIs
        self.autoBurstBlockedByVetoISIs = autoBurstBlockedByVetoISIs
        self.positiveCoverageISICount = positiveCoverageISICount
        self.negativeCoverageISICount = negativeCoverageISICount
        self.skippedAnnotationCount = skippedAnnotationCount
    }

    /// True when at least one manual annotation actually affected the projection.
    public var hasManualEffect: Bool {
        !manualPositiveLabelByISI.isEmpty || !manualNegativeVetoByISI.isEmpty
    }
}

/// Projects manual annotations onto a train's per-ISI auto labels, mirroring the R/Shiny semantics:
/// final label is manual-positive-over-auto (always), a manual lock blanks auto under positive
/// coverage for public output, and a `notBurst` veto blocks only the burst-family auto interpretation.
public enum ManualAnnotationProjector {
    /// Canonical burst-family pattern strings a `notBurst` veto blocks (mirrors R's burst-only veto).
    /// These are label identities, NOT fixed-millisecond biological boundaries.
    public static let burstFamilyLabels: Set<String> = [
        "burst",
        "long_burst",
        "possible_burst",
        "high_frequency_burst"
    ]

    public static func project(
        train: SpikeTrain,
        autoLabelsByISI: [Int: String],
        annotations: [ManualAnnotation],
        honorManualLock: Bool,
        manualNegativeLabelsEnabled: Bool
    ) -> ManualAnnotationProjection {
        var positiveByISI: [Int: String] = [:]
        var vetoISIs: Set<Int> = []
        var skipped = 0

        for annotation in annotations {
            // Wrong-train annotations are skipped (and counted) — `skippedAnnotationCount` reflects
            // every annotation that did not resolve to this train (wrong train or outside its span).
            guard annotation.trainID == train.id else {
                skipped += 1
                continue
            }
            let geometry = ManualAnnotationGeometryResolver.resolve(annotation: annotation, in: train)
            guard geometry.isWithinTrain, let covered = geometry.coveredISIIndices else {
                if !geometry.isWithinTrain {
                    skipped += 1
                }
                continue
            }

            switch annotation.label.polarity {
            case .positive:
                if let pattern = annotation.label.finalPatternString {
                    // Later overlapping annotations override earlier ones (most-recent edit wins).
                    for isi in covered {
                        positiveByISI[isi] = pattern
                    }
                }
            case .negative:
                // Phase 1A consumes only `notBurst`; any other (future/unknown) veto label is inert.
                if manualNegativeLabelsEnabled, ManualAnnotationLabel.consumedVetoLabels.contains(annotation.label) {
                    for isi in covered {
                        vetoISIs.insert(isi)
                    }
                }
            }
        }

        var effectiveAuto: [Int: String] = [:]
        var blockedByLock: Set<Int> = []
        var burstBlockedByVeto: Set<Int> = []

        for (isi, autoLabel) in autoLabelsByISI {
            if positiveByISI[isi] != nil {
                // Positive manual coverage. Under lock the auto label is suppressed for public output;
                // without lock it stays available (the final label is still manual-first below).
                if honorManualLock {
                    blockedByLock.insert(isi)
                } else {
                    effectiveAuto[isi] = autoLabel
                }
            } else if vetoISIs.contains(isi), burstFamilyLabels.contains(autoLabel) {
                burstBlockedByVeto.insert(isi)
            } else {
                effectiveAuto[isi] = autoLabel
            }
        }

        // Final = effective auto, then manual-positive override (manual-first, independent of lock).
        var finalByISI = effectiveAuto
        for (isi, pattern) in positiveByISI {
            finalByISI[isi] = pattern
        }

        return ManualAnnotationProjection(
            manualPositiveLabelByISI: positiveByISI,
            manualNegativeVetoByISI: vetoISIs,
            finalLabelByISI: finalByISI,
            effectiveAutoLabelByISI: effectiveAuto,
            autoBlockedByManualLockISIs: blockedByLock,
            autoBurstBlockedByVetoISIs: burstBlockedByVeto,
            positiveCoverageISICount: positiveByISI.count,
            negativeCoverageISICount: vetoISIs.count,
            skippedAnnotationCount: skipped
        )
    }

    /// Project manual annotations onto PUBLIC event annotations for display/export. Two suppressions are
    /// applied to each auto event annotation, then it is split into the contiguous runs of its surviving
    /// ISIs (dropping fully-suppressed annotations):
    ///
    /// - **`not_burst` veto** (`vetoedBurstISIsByTrain`, from `autoBurstBlockedByVetoISIs`): removes only
    ///   **burst-family** ISIs the user vetoed. Non-burst annotations are never touched by the veto.
    /// - **positive-manual lock** (`lockSuppressedISIsByTrain`, from `autoBlockedByManualLockISIs`):
    ///   removes any auto ISI a positive manual label covers under lock, so the manual label (drawn on
    ///   the manual strip) is the visible one — i.e. positive manual overrides auto, as designed.
    ///
    /// Each surviving run is produced with `ClassicAnchorEventAnnotation.clipped(to:in:)`, which
    /// recomputes the run's time bounds and keeps `candidateID` (so manual-review rejection + raw audit
    /// still map). Annotations on trains with no suppression pass through unchanged. Returns the
    /// projected annotations and the count of burst-family ISIs suppressed specifically by the veto
    /// (`manual_veto_suppressed_isi_count`). Pure — inputs are value types and are never mutated.
    public static func projectPublicEventAnnotations(
        _ annotations: [ClassicAnchorEventAnnotation],
        vetoedBurstISIsByTrain: [String: Set<Int>],
        lockSuppressedISIsByTrain: [String: Set<Int>],
        trainsByID: [String: SpikeTrain]
    ) -> (annotations: [ClassicAnchorEventAnnotation], vetoSuppressedISICount: Int) {
        var output: [ClassicAnchorEventAnnotation] = []
        output.reserveCapacity(annotations.count)
        var vetoSuppressedISICount = 0

        for annotation in annotations {
            // The veto only applies to burst-family labels; the positive lock applies to any label.
            let veto = burstFamilyLabels.contains(annotation.label.rawValue)
                ? (vetoedBurstISIsByTrain[annotation.trainID] ?? [])
                : []
            let lock = lockSuppressedISIsByTrain[annotation.trainID] ?? []
            guard !veto.isEmpty || !lock.isEmpty,
                  let train = trainsByID[annotation.trainID],
                  let span = annotation.coveredISIIndices(in: train) else {
                output.append(annotation)
                continue
            }

            func isSuppressed(_ isi: Int) -> Bool { veto.contains(isi) || lock.contains(isi) }
            guard span.contains(where: isSuppressed) else {
                output.append(annotation)
                continue
            }
            vetoSuppressedISICount += span.filter { veto.contains($0) }.count

            // Split [span] into contiguous runs of surviving (non-suppressed) ISIs.
            var runStart: Int?
            var runEnd: Int?
            func flush() {
                if let start = runStart, let end = runEnd,
                   let run = annotation.clipped(to: start...end, in: train) {
                    output.append(run)
                }
                runStart = nil
                runEnd = nil
            }
            for isi in span {
                if isSuppressed(isi) {
                    flush()
                } else if runStart == nil {
                    runStart = isi
                    runEnd = isi
                } else {
                    runEnd = isi
                }
            }
            flush()
        }

        return (output, vetoSuppressedISICount)
    }
}
