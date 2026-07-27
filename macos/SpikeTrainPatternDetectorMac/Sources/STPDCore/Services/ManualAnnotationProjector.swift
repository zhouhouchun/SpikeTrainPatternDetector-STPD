import Foundation

/// The result of projecting manual annotations onto one train's per-ISI auto labels. Pure value type;
/// carries both the resolved per-ISI maps and audit/diagnostic counts. All maps are keyed by ISI index.
public struct ManualAnnotationProjection: Hashable, Sendable {
    /// Positive manual label (canonical pattern string) per covered ISI.
    public let manualPositiveLabelByISI: [Int: String]
    /// UUID of the positive annotation that owns each effective manual label after canonical
    /// last-edit-wins resolution and structural validation.
    public let manualPositiveOwnerByISI: [Int: UUID]
    /// ISIs under an active `notBurst` veto.
    public let manualNegativeVetoByISI: Set<Int>
    /// UUID of the active negative annotation that owns each vetoed ISI after canonical
    /// last-edit-wins resolution.
    public let manualNegativeVetoOwnerByISI: [Int: UUID]
    /// Manual-first final label per ISI: positive manual wins; otherwise the auto label that survived
    /// lock/veto suppression and final burst-family structural validation. Negative/veto labels never
    /// appear here. With no effective manual action, this remains an exact copy of the auto input.
    public let finalLabelByISI: [Int: String]
    /// Auto labels still available for public output after lock + veto suppression. Burst fragments
    /// created by those suppressions are structurally validated. With locking disabled, the detector's
    /// auto map remains unchanged while `finalLabelByISI` applies manual-first structural validation.
    public let effectiveAutoLabelByISI: [Int: String]
    /// Auto-labeled ISIs suppressed because a positive manual label covers them AND `honorManualLock`.
    public let autoBlockedByManualLockISIs: Set<Int>
    /// Burst-family auto-labeled ISIs suppressed because a `notBurst` veto covers them.
    public let autoBurstBlockedByVetoISIs: Set<Int>
    public let positiveCoverageISICount: Int
    public let negativeCoverageISICount: Int
    /// Annotations that were superseded by a newer version of the same id, did not resolve to this
    /// train (wrong train / outside its time span), or failed a structural minimum.
    public let skippedAnnotationCount: Int
    /// Burst-family ISIs removed because a manual lock, veto, or final manual override left a
    /// contiguous public run below `burstMinimumISIRunLength`. This is audit metadata; it never
    /// rewrites the raw automatic input map.
    public let finalBurstISIsRemovedByStructuralMinimum: Set<Int>

    public init(
        manualPositiveLabelByISI: [Int: String],
        manualNegativeVetoByISI: Set<Int>,
        finalLabelByISI: [Int: String],
        effectiveAutoLabelByISI: [Int: String],
        autoBlockedByManualLockISIs: Set<Int>,
        autoBurstBlockedByVetoISIs: Set<Int>,
        positiveCoverageISICount: Int,
        negativeCoverageISICount: Int,
        skippedAnnotationCount: Int,
        finalBurstISIsRemovedByStructuralMinimum: Set<Int> = [],
        manualPositiveOwnerByISI: [Int: UUID] = [:],
        manualNegativeVetoOwnerByISI: [Int: UUID] = [:]
    ) {
        self.manualPositiveLabelByISI = manualPositiveLabelByISI
        self.manualPositiveOwnerByISI = manualPositiveOwnerByISI
        self.manualNegativeVetoByISI = manualNegativeVetoByISI
        self.manualNegativeVetoOwnerByISI = manualNegativeVetoOwnerByISI
        self.finalLabelByISI = finalLabelByISI
        self.effectiveAutoLabelByISI = effectiveAutoLabelByISI
        self.autoBlockedByManualLockISIs = autoBlockedByManualLockISIs
        self.autoBurstBlockedByVetoISIs = autoBurstBlockedByVetoISIs
        self.positiveCoverageISICount = positiveCoverageISICount
        self.negativeCoverageISICount = negativeCoverageISICount
        self.skippedAnnotationCount = skippedAnnotationCount
        self.finalBurstISIsRemovedByStructuralMinimum = finalBurstISIsRemovedByStructuralMinimum
    }

    /// True when at least one manual annotation actually affected the projection.
    public var hasManualEffect: Bool {
        !manualPositiveLabelByISI.isEmpty || !manualNegativeVetoByISI.isEmpty
    }
}

/// Stateful public-event projection result.
///
/// `annotations` contains only events that remain visible after the current manual overlay. The
/// separate source archive preserves automatic evidence even when a veto or positive lock suppresses
/// an event completely, so a later projection after deleting that manual action can reconstruct the
/// same automatic baseline without exposing a placeholder event in the raster or exports.
public struct ManualPublicEventProjectionState: Hashable, Sendable {
    public let annotations: [ClassicAnchorEventAnnotation]
    public let vetoSuppressedISICount: Int
    public let automaticSourceArchive: [ClassicAnchorAutomaticEventSource]

    init(
        annotations: [ClassicAnchorEventAnnotation],
        vetoSuppressedISICount: Int,
        automaticSourceArchive: [ClassicAnchorAutomaticEventSource]
    ) {
        self.annotations = annotations
        self.vetoSuppressedISICount = vetoSuppressedISICount
        self.automaticSourceArchive = automaticSourceArchive
    }
}

/// Projects manual annotations onto a train's per-ISI auto labels, mirroring the R/Shiny semantics:
/// final label is manual-positive-over-auto (always), a manual lock blanks auto under positive
/// coverage for public output, and a `notBurst` veto blocks only the burst-family auto interpretation.
public enum ManualAnnotationProjector {
    /// Burst-family annotations need at least 2 ISIs / 3 spikes in every public projection surface.
    public static let burstMinimumISIRunLength = 2

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
        manualNegativeLabelsEnabled: Bool,
        minValidISISeconds: Double = 0
    ) -> ManualAnnotationProjection {
        var positiveByISI: [Int: String] = [:]
        var positiveOwnerByISI: [Int: UUID] = [:]
        var vetoISIs: Set<Int> = []
        var negativeOwnerByISI: [Int: UUID] = [:]
        let orderedAnnotations = canonicalizedAnnotations(annotations)
        let minimumValid = minValidISISeconds.isFinite ? max(0, minValidISISeconds) : 0
        var skipped = annotations.count - orderedAnnotations.count

        // Process canonical annotation versions old-to-new so overlapping positive annotations
        // implement "most recent edit wins" independently of caller array order.

        for annotation in orderedAnnotations {
            // Wrong-train annotations are skipped (and counted) — `skippedAnnotationCount` reflects
            // every annotation that did not resolve to this train (wrong train or outside its span).
            guard annotation.trainID == train.id else {
                skipped += 1
                continue
            }
            let geometry = ManualAnnotationGeometryResolver.resolve(annotation: annotation, in: train)
            guard geometry.isWithinTrain, let covered = geometry.coveredISIIndices else {
                skipped += 1
                continue
            }
            let coveredIndices = covered.filter { index in
                guard train.isiSec.indices.contains(index),
                      let value = train.isiSec[index] else {
                    return false
                }
                return value.isFinite && value >= minimumValid
            }
            guard !coveredIndices.isEmpty else {
                skipped += 1
                continue
            }

            switch annotation.label.polarity {
            case .positive:
                if let pattern = annotation.label.finalPatternString {
                    for isi in coveredIndices {
                        positiveByISI[isi] = pattern
                        positiveOwnerByISI[isi] = annotation.id
                    }
                }
            case .negative:
                // Phase 1A consumes only `notBurst`; any other (future/unknown) veto label is inert.
                if manualNegativeLabelsEnabled, ManualAnnotationLabel.consumedVetoLabels.contains(annotation.label) {
                    for isi in coveredIndices {
                        vetoISIs.insert(isi)
                        negativeOwnerByISI[isi] = annotation.id
                    }
                }
            }
        }

        var structurallyRemovedBurstISIs: Set<Int> = []
        var ownersCountedAsSkipped: Set<UUID> = []
        func removeInvalidManualBurstISIs(_ indices: Set<Int>) {
            let removable = indices.filter { isi in
                positiveByISI[isi].map(burstFamilyLabels.contains) == true
            }
            let removedOwners = Set(removable.compactMap { positiveOwnerByISI[$0] })
            for isi in removable {
                positiveByISI.removeValue(forKey: isi)
                positiveOwnerByISI.removeValue(forKey: isi)
            }
            structurallyRemovedBurstISIs.formUnion(removable)
            let retainedOwners = Set(positiveOwnerByISI.values)
            for owner in removedOwners
            where !retainedOwners.contains(owner) && ownersCountedAsSkipped.insert(owner).inserted {
                skipped += 1
            }
        }

        // Judge a manual burst against the prospective FINAL burst-family coverage, not against the
        // manual mark in isolation. This permits a one-ISI manual edge to extend an adjacent two-ISI
        // automatic burst, while still rejecting an isolated one-ISI manual burst. Positive manual
        // labels overwrite auto labels in this prospective map, matching the final projection order.
        var prospectiveFinal = autoLabelsByISI
        for (isi, pattern) in positiveByISI { prospectiveFinal[isi] = pattern }
        removeInvalidManualBurstISIs(invalidBurstFragmentISIs(in: prospectiveFinal))

        struct ResolvedMaps {
            var effectiveAuto: [Int: String]
            var blockedByLock: Set<Int>
            var burstBlockedByVeto: Set<Int>
            var final: [Int: String]
            var removedBurstISIs: Set<Int>
        }
        func resolveMaps() -> ResolvedMaps {
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

            // A lock or veto can split a previously valid auto burst into a public singleton. Manual
            // burst coverage is allowed to provide the missing adjacent support: validate the suppressed
            // auto contribution against that burst-only support map before removing auto fragments. This
            // avoids pruning a valid auto+manual burst before the two sources have been composed.
            var removed: Set<Int> = []
            if !blockedByLock.isEmpty || !burstBlockedByVeto.isEmpty {
                var burstSupportByISI = effectiveAuto
                for (isi, pattern) in positiveByISI where burstFamilyLabels.contains(pattern) {
                    burstSupportByISI[isi] = pattern
                }
                let invalidAuto = invalidBurstFragmentISIs(in: burstSupportByISI).filter { isi in
                    effectiveAuto[isi].map(burstFamilyLabels.contains) == true
                }
                removed.formUnion(invalidAuto)
                for isi in invalidAuto { effectiveAuto.removeValue(forKey: isi) }
            }

            // Final = effective auto, then manual-positive override (manual-first, independent of lock).
            var finalByISI = effectiveAuto
            for (isi, pattern) in positiveByISI { finalByISI[isi] = pattern }

            // An unlocked positive override can split a burst even though the auto map remains available.
            // Enforce the same final family-level minimum used by event projection and reviewed export.
            if !positiveByISI.isEmpty || !blockedByLock.isEmpty || !burstBlockedByVeto.isEmpty {
                let invalid = invalidBurstFragmentISIs(in: finalByISI)
                removed.formUnion(invalid)
                for isi in invalid { finalByISI.removeValue(forKey: isi) }
            }
            return ResolvedMaps(
                effectiveAuto: effectiveAuto,
                blockedByLock: blockedByLock,
                burstBlockedByVeto: burstBlockedByVeto,
                final: finalByISI,
                removedBurstISIs: removed
            )
        }

        var resolved = resolveMaps()
        // A veto can remove the automatic neighbor that made a manual one-ISI burst structurally valid.
        // If that happens, discard the now-invalid manual burst and recompute once so its lock cannot hide
        // the original automatic label underneath it.
        let invalidManualAfterSuppression = Set(resolved.removedBurstISIs.filter { isi in
            positiveByISI[isi].map(burstFamilyLabels.contains) == true
        })
        if !invalidManualAfterSuppression.isEmpty {
            removeInvalidManualBurstISIs(invalidManualAfterSuppression)
            resolved = resolveMaps()
        }
        structurallyRemovedBurstISIs.formUnion(resolved.removedBurstISIs)

        return ManualAnnotationProjection(
            manualPositiveLabelByISI: positiveByISI,
            manualNegativeVetoByISI: vetoISIs,
            finalLabelByISI: resolved.final,
            effectiveAutoLabelByISI: resolved.effectiveAuto,
            autoBlockedByManualLockISIs: resolved.blockedByLock,
            autoBurstBlockedByVetoISIs: resolved.burstBlockedByVeto,
            positiveCoverageISICount: positiveByISI.count,
            negativeCoverageISICount: vetoISIs.count,
            skippedAnnotationCount: skipped,
            finalBurstISIsRemovedByStructuralMinimum: structurallyRemovedBurstISIs,
            manualPositiveOwnerByISI: positiveOwnerByISI,
            manualNegativeVetoOwnerByISI: negativeOwnerByISI
        )
    }

    /// Project manual annotations onto PUBLIC event annotations for display/export. Two suppressions are
    /// applied to automatic coverage. Burst-family output is then rebuilt from the valid train-level
    /// connected components rather than emitted independently per source candidate:
    ///
    /// - **`not_burst` veto** (`vetoedBurstISIsByTrain`, from `autoBurstBlockedByVetoISIs`): removes only
    ///   **burst-family** ISIs the user vetoed. Non-burst annotations are never touched by the veto.
    /// - **positive-manual lock** (`lockSuppressedISIsByTrain`, from `autoBlockedByManualLockISIs`):
    ///   removes any auto ISI a positive manual label covers under lock, so the manual label (drawn on
    ///   the manual strip) is the visible one — i.e. positive manual overrides auto, as designed.
    /// - **validated final burst support**: surviving automatic burst coverage is unioned across all
    ///   source candidates on a train, then composed with `validatedManualBurstSupportISIsByTrain`
    ///   (from burst-family entries in `manualPositiveLabelByISI`). Structural minimums are evaluated
    ///   on that final train-level graph, so adjacent clipped pieces from different candidates can
    ///   support one another. Separated singletons remain invalid.
    ///
    /// Each valid burst component is emitted once. Its deterministic primary `candidateID` preserves
    /// backward-compatible review links, while `sourceCandidateIDs` retains every contributing source.
    /// A component completed by validated manual burst support includes that support in its public event
    /// geometry. Non-burst annotations remain candidate-local and are clipped as before. Trains with no
    /// suppression or manual burst support pass through unchanged. For repeated projection, use
    /// `projectPublicEventState`: its separate automatic-source archive also preserves evidence when the
    /// visible annotation list becomes empty after complete suppression. Structured automatic-support
    /// indices prevent removed manual coverage from becoming detector evidence.
    /// The veto count is the number of distinct `(train, ISI)` pairs removed from burst-family automatic
    /// coverage, never the number of overlapping annotation hits.
    public static func projectPublicEventAnnotations(
        _ annotations: [ClassicAnchorEventAnnotation],
        vetoedBurstISIsByTrain: [String: Set<Int>],
        lockSuppressedISIsByTrain: [String: Set<Int>],
        validatedManualBurstSupportISIsByTrain: [String: Set<Int>] = [:],
        trainsByID: [String: SpikeTrain],
        burstMinimumISIRunLength: Int = ManualAnnotationProjector.burstMinimumISIRunLength
    ) -> (annotations: [ClassicAnchorEventAnnotation], vetoSuppressedISICount: Int) {
        let state = projectPublicEventState(
            annotations,
            vetoedBurstISIsByTrain: vetoedBurstISIsByTrain,
            lockSuppressedISIsByTrain: lockSuppressedISIsByTrain,
            validatedManualBurstSupportISIsByTrain: validatedManualBurstSupportISIsByTrain,
            trainsByID: trainsByID,
            burstMinimumISIRunLength: burstMinimumISIRunLength
        )
        return (state.annotations, state.vetoSuppressedISICount)
    }

    /// Begin a repeatable manual projection while retaining an explicit automatic-source archive.
    ///
    /// `trainsByID` should contain every train referenced by the annotations. When train geometry is
    /// temporarily unavailable, visible input is preserved and archived-only evidence remains in the
    /// archive; restoration is deferred until the referenced train is supplied on a later projection.
    public static func projectPublicEventState(
        _ annotations: [ClassicAnchorEventAnnotation],
        vetoedBurstISIsByTrain: [String: Set<Int>],
        lockSuppressedISIsByTrain: [String: Set<Int>],
        validatedManualBurstSupportISIsByTrain: [String: Set<Int>],
        trainsByID: [String: SpikeTrain],
        burstMinimumISIRunLength: Int = ManualAnnotationProjector.burstMinimumISIRunLength
    ) -> ManualPublicEventProjectionState {
        projectPublicEventState(
            annotations,
            automaticSourceArchive: [],
            vetoedBurstISIsByTrain: vetoedBurstISIsByTrain,
            lockSuppressedISIsByTrain: lockSuppressedISIsByTrain,
            validatedManualBurstSupportISIsByTrain: validatedManualBurstSupportISIsByTrain,
            trainsByID: trainsByID,
            burstMinimumISIRunLength: burstMinimumISIRunLength
        )
    }

    /// Re-project a prior state. The archive is required only when a previous manual overlay removed
    /// every visible fragment of an automatic event; ordinary app callers that always start from raw
    /// automatic annotations may continue using `projectPublicEventAnnotations`.
    ///
    /// `trainsByID` should contain every train referenced by the state. Missing train geometry never
    /// destroys archived evidence: archived-only restoration is deferred until a later call supplies
    /// that train, while any already-visible input remains unchanged.
    public static func projectPublicEventState(
        _ prior: ManualPublicEventProjectionState,
        vetoedBurstISIsByTrain: [String: Set<Int>],
        lockSuppressedISIsByTrain: [String: Set<Int>],
        validatedManualBurstSupportISIsByTrain: [String: Set<Int>],
        trainsByID: [String: SpikeTrain],
        burstMinimumISIRunLength: Int = ManualAnnotationProjector.burstMinimumISIRunLength
    ) -> ManualPublicEventProjectionState {
        projectPublicEventState(
            prior.annotations,
            automaticSourceArchive: prior.automaticSourceArchive,
            vetoedBurstISIsByTrain: vetoedBurstISIsByTrain,
            lockSuppressedISIsByTrain: lockSuppressedISIsByTrain,
            validatedManualBurstSupportISIsByTrain: validatedManualBurstSupportISIsByTrain,
            trainsByID: trainsByID,
            burstMinimumISIRunLength: burstMinimumISIRunLength
        )
    }

    private static func projectPublicEventState(
        _ annotations: [ClassicAnchorEventAnnotation],
        automaticSourceArchive priorSourceArchive: [ClassicAnchorAutomaticEventSource],
        vetoedBurstISIsByTrain: [String: Set<Int>],
        lockSuppressedISIsByTrain: [String: Set<Int>],
        validatedManualBurstSupportISIsByTrain: [String: Set<Int>],
        trainsByID: [String: SpikeTrain],
        burstMinimumISIRunLength: Int
    ) -> ManualPublicEventProjectionState {
        let visibleSourceArchive = ClassicAnchorEventAnnotation.normalizedAutomaticSources(
            annotations.flatMap(\.automaticEventSources)
        )
        let sourceArchive = ClassicAnchorEventAnnotation.normalizedAutomaticSources(
            priorSourceArchive + visibleSourceArchive
        )
        // Restore only archived evidence that is no longer represented by the visible input. A raw
        // automatic candidate may legitimately arrive as multiple clipped fragments; their canonical
        // visible-source union already represents the archive and must remain byte-for-byte unchanged
        // under an empty state re-projection.
        let visibleSources = Set(visibleSourceArchive)
        let missingArchivedSources = priorSourceArchive.filter { !visibleSources.contains($0) }
        let archivedAnnotations = missingArchivedSources.flatMap { source -> [ClassicAnchorEventAnnotation] in
            guard let train = trainsByID[source.trainID] else { return [] }
            return contiguousRanges(in: Set(source.supportISIIndices)).compactMap { range in
                ClassicAnchorEventAnnotation.materializedAutomaticComponent(
                    source: source,
                    to: range,
                    in: train,
                    manualLockApplied: false
                )
            }
        }
        let visibleInputCount = annotations.count
        let visibleAutomaticSources = Set(annotations.flatMap(\.automaticEventSources))
        let projectionInputs = annotations + archivedAnnotations
        let affectedTrainIDs = Set(vetoedBurstISIsByTrain.compactMap { $0.value.isEmpty ? nil : $0.key })
            .union(lockSuppressedISIsByTrain.compactMap { $0.value.isEmpty ? nil : $0.key })
            .union(validatedManualBurstSupportISIsByTrain.compactMap { $0.value.isEmpty ? nil : $0.key })
            .union(missingArchivedSources.map(\.trainID))
            .union(projectionInputs.compactMap { annotation in
                annotation.decisionPath.split(separator: ";").contains {
                    $0.hasPrefix("manual_projection_")
                } ? annotation.trainID : nil
            })
        guard !affectedTrainIDs.isEmpty else {
            return ManualPublicEventProjectionState(
                annotations: annotations,
                vetoSuppressedISICount: 0,
                automaticSourceArchive: sourceArchive
            )
        }

        struct BurstSource {
            let order: Int
            let annotation: ClassicAnchorEventAnnotation
            let source: ClassicAnchorAutomaticEventSource
            let survivingISIs: Set<Int>
        }
        struct OrderedAnnotation {
            let order: Int
            let annotation: ClassicAnchorEventAnnotation
        }

        // Structural validity belongs to the final public burst coverage, not to any one source
        // candidate. Build that train-level support graph before constructing event objects.
        var survivingAutoBurstISIsByTrain: [String: Set<Int>] = [:]
        var burstSourcesByTrain: [String: [BurstSource]] = [:]
        var distinctVetoedBurstISIsByTrain: [String: Set<Int>] = [:]
        var passthroughOrNonBurstOutput: [OrderedAnnotation] = []
        var processedBurstSources: Set<ClassicAnchorAutomaticEventSource> = []
        var processedProjectedNonBurstSources: Set<ClassicAnchorAutomaticEventSource> = []
        passthroughOrNonBurstOutput.reserveCapacity(projectionInputs.count)

        for (order, annotation) in projectionInputs.enumerated() {
            let isBurst = burstFamilyLabels.contains(annotation.label.rawValue)
            let veto = isBurst ? (vetoedBurstISIsByTrain[annotation.trainID] ?? []) : []
            let lock = lockSuppressedISIsByTrain[annotation.trainID] ?? []
            guard let train = trainsByID[annotation.trainID],
                  let span = annotation.coveredISIIndices(in: train) else {
                passthroughOrNonBurstOutput.append(OrderedAnnotation(order: order, annotation: annotation))
                continue
            }

            if isBurst {
                if !affectedTrainIDs.contains(annotation.trainID) {
                    passthroughOrNonBurstOutput.append(OrderedAnnotation(order: order, annotation: annotation))
                    continue
                }
                for source in annotation.automaticEventSources where source.trainID == annotation.trainID {
                    guard processedBurstSources.insert(source).inserted else { continue }
                    // `supportISIIndices` is the immutable automatic baseline established before the
                    // manual layer. A prior manual projection may have narrowed `span`, so intersecting
                    // with that projected geometry here would permanently erase temporarily suppressed
                    // evidence and make lock/veto removal non-reversible.
                    let automaticSupport = Set(source.supportISIIndices).filter { isi in
                        isi >= 1 && isi < train.isiSec.count
                    }
                    let surviving = automaticSupport.subtracting(veto).subtracting(lock)
                    for isi in automaticSupport where veto.contains(isi) {
                        distinctVetoedBurstISIsByTrain[annotation.trainID, default: []].insert(isi)
                    }
                    survivingAutoBurstISIsByTrain[annotation.trainID, default: []].formUnion(surviving)
                    burstSourcesByTrain[annotation.trainID, default: []].append(
                        BurstSource(
                            order: order,
                            annotation: annotation,
                            source: source,
                            survivingISIs: surviving
                        )
                    )
                }
                continue
            }

            let wasProjectedByNonBurstLock = annotation.decisionPath.split(separator: ";").contains {
                $0 == "manual_projection_non_burst_lock=true"
            }
            guard !lock.isEmpty || wasProjectedByNonBurstLock else {
                // Visible public events are authoritative when they still carry this automatic
                // source. The archive only restores a source that disappeared completely; it must
                // not emit a second clean copy on an idempotent state reprojection.
                let isArchivedInput = order >= visibleInputCount
                let duplicatesVisibleSource = annotation.automaticEventSources.contains { source in
                    visibleAutomaticSources.contains(source)
                }
                if isArchivedInput && duplicatesVisibleSource {
                    continue
                }
                passthroughOrNonBurstOutput.append(OrderedAnnotation(order: order, annotation: annotation))
                continue
            }

            let sources = annotation.automaticEventSources.filter { $0.trainID == annotation.trainID }
            if sources.isEmpty {
                // Defensive compatibility for malformed/legacy values. Current constructors always
                // provide source snapshots, so ordinary projection never enters this branch.
                if lock.isEmpty || !span.contains(where: lock.contains) {
                    passthroughOrNonBurstOutput.append(OrderedAnnotation(order: order, annotation: annotation))
                }
                continue
            }
            for source in sources {
                guard processedProjectedNonBurstSources.insert(source).inserted else { continue }
                let baselineSupport = Set(source.supportISIIndices).filter { isi in
                    isi >= 1 && isi < train.isiSec.count
                }
                let surviving = baselineSupport.subtracting(lock)
                for component in contiguousRanges(in: surviving) {
                    guard let run = ClassicAnchorEventAnnotation.materializedAutomaticComponent(
                        source: source,
                        to: component,
                        in: train,
                        manualLockApplied: !lock.isEmpty
                    ) else {
                        continue
                    }
                    passthroughOrNonBurstOutput.append(OrderedAnnotation(order: order, annotation: run))
                }
            }
        }

        let supportTrainIDs = Set(survivingAutoBurstISIsByTrain.keys)
            .union(validatedManualBurstSupportISIsByTrain.keys)
        var validFinalBurstSupportISIsByTrain: [String: Set<Int>] = [:]
        validFinalBurstSupportISIsByTrain.reserveCapacity(supportTrainIDs.count)
        for trainID in supportTrainIDs {
            guard let train = trainsByID[trainID] else { continue }
            var support = survivingAutoBurstISIsByTrain[trainID] ?? []
            support.formUnion(
                (validatedManualBurstSupportISIsByTrain[trainID] ?? []).filter { isi in
                    isi >= 1 && isi < train.isiSec.count
                }
            )
            let supportByISI = Dictionary(uniqueKeysWithValues: support.map { ($0, "burst") })
            let invalid = invalidBurstFragmentISIs(
                in: supportByISI,
                minimumRunLength: burstMinimumISIRunLength
            )
            validFinalBurstSupportISIsByTrain[trainID] = support.subtracting(invalid)
        }

        var orderedOutput = passthroughOrNonBurstOutput
        for trainID in burstSourcesByTrain.keys.sorted() {
            guard let train = trainsByID[trainID],
                  let sources = burstSourcesByTrain[trainID] else {
                continue
            }
            let validSupport = validFinalBurstSupportISIsByTrain[trainID] ?? []
            let survivingAuto = survivingAutoBurstISIsByTrain[trainID] ?? []
            let manualSupport = validatedManualBurstSupportISIsByTrain[trainID] ?? []

            for component in contiguousRanges(in: validSupport) {
                let automaticPart = Set(component).intersection(survivingAuto)
                guard !automaticPart.isEmpty else { continue }
                let contributors = sources.filter { !$0.survivingISIs.isDisjoint(with: automaticPart) }
                guard let primary = contributors.sorted(by: { lhs, rhs in
                    if lhs.source.priority != rhs.source.priority {
                        return lhs.source.priority > rhs.source.priority
                    }
                    if lhs.source.score != rhs.source.score {
                        return lhs.source.score > rhs.source.score
                    }
                    if lhs.source.candidateID != rhs.source.candidateID {
                        return lhs.source.candidateID < rhs.source.candidateID
                    }
                    if lhs.source.supportISIIndices.first != rhs.source.supportISIIndices.first {
                        return (lhs.source.supportISIIndices.first ?? Int.max)
                            < (rhs.source.supportISIIndices.first ?? Int.max)
                    }
                    if lhs.source.supportISIIndices.last != rhs.source.supportISIIndices.last {
                        return (lhs.source.supportISIIndices.last ?? Int.max)
                            < (rhs.source.supportISIIndices.last ?? Int.max)
                    }
                    return lhs.source.annotationID < rhs.source.annotationID
                }).first else {
                    continue
                }
                // The event method applies conservative possible-burst authority while retaining each
                // contributor's original identity. That mapping is what lets a later split restore the
                // canonical and review-level fragments independently.
                // Preserve each contributor's complete automatic baseline. The projected event derives
                // its current visible automatic support from `component`; retaining only this component
                // here would make a later lock/veto removal unable to restore suppressed automatic ISIs.
                let componentSources = contributors.map(\.source)
                let includesManualSupport = !Set(component).isDisjoint(with: manualSupport)
                guard let projected = primary.annotation.projectedBurstComponent(
                    to: component,
                    in: train,
                    automaticEventSources: componentSources,
                    includesManualSupport: includesManualSupport
                ) else {
                    continue
                }
                let firstOrder = contributors.map(\.order).min() ?? primary.order
                orderedOutput.append(OrderedAnnotation(order: firstOrder, annotation: projected))
            }
        }

        orderedOutput.sort {
            if $0.order != $1.order { return $0.order < $1.order }
            if $0.annotation.startISISecIndex != $1.annotation.startISISecIndex {
                return $0.annotation.startISISecIndex < $1.annotation.startISISecIndex
            }
            return $0.annotation.id < $1.annotation.id
        }
        let vetoSuppressedISICount = distinctVetoedBurstISIsByTrain.values.reduce(0) { $0 + $1.count }
        return ManualPublicEventProjectionState(
            annotations: orderedOutput.map(\.annotation),
            vetoSuppressedISICount: vetoSuppressedISICount,
            automaticSourceArchive: sourceArchive
        )
    }

    private static func contiguousRanges(in indices: Set<Int>) -> [ClosedRange<Int>] {
        let sorted = indices.sorted()
        guard var start = sorted.first else { return [] }
        var end = start
        var ranges: [ClosedRange<Int>] = []
        for index in sorted.dropFirst() {
            if index == end + 1 {
                end = index
            } else {
                ranges.append(start...end)
                start = index
                end = index
            }
        }
        ranges.append(start...end)
        return ranges
    }

    /// Collapse multiple serialized/edit versions of the same logical annotation before projection
    /// or calibration. The newest `(updatedAt, createdAt)` version wins globally, even if an edit moved
    /// the annotation to another train. Remaining ties are resolved from annotation content, making the
    /// result deterministic and independent of caller array order.
    public static func canonicalizedAnnotations(_ annotations: [ManualAnnotation]) -> [ManualAnnotation] {
        let ordered = annotations.sorted(by: annotationIsOrderedBefore)
        var latestByID: [UUID: ManualAnnotation] = [:]
        latestByID.reserveCapacity(ordered.count)
        for annotation in ordered {
            latestByID[annotation.id] = annotation
        }
        return latestByID.values.sorted(by: annotationIsOrderedBefore)
    }

    /// Return burst-family ISIs that belong to a final contiguous run shorter than the public minimum.
    /// Labels outside the burst family are never removed and split otherwise-adjacent burst runs.
    static func invalidBurstFragmentISIs(
        in positiveLabelsByISI: [Int: String],
        minimumRunLength: Int = ManualAnnotationProjector.burstMinimumISIRunLength,
        burstLabels: Set<String> = ManualAnnotationProjector.burstFamilyLabels
    ) -> Set<Int> {
        let minimum = max(1, minimumRunLength)
        let burstIndices = positiveLabelsByISI
            .compactMap { burstLabels.contains($0.value) ? $0.key : nil }
            .sorted()
        guard !burstIndices.isEmpty else { return [] }

        var invalid: Set<Int> = []
        var run: [Int] = []
        func flushRun() {
            if run.count < minimum { invalid.formUnion(run) }
            run.removeAll(keepingCapacity: true)
        }

        for index in burstIndices {
            if let previous = run.last, index != previous + 1 {
                flushRun()
            }
            run.append(index)
        }
        flushRun()
        return invalid
    }

    private static func annotationIsOrderedBefore(_ lhs: ManualAnnotation, _ rhs: ManualAnnotation) -> Bool {
        let updatedOrder = timestampOrder(lhs.updatedAt, rhs.updatedAt)
        if updatedOrder != 0 { return updatedOrder < 0 }
        let createdOrder = timestampOrder(lhs.createdAt, rhs.createdAt)
        if createdOrder != 0 { return createdOrder < 0 }
        if lhs.id != rhs.id { return lhs.id.uuidString < rhs.id.uuidString }
        if lhs.trainID != rhs.trainID { return lhs.trainID < rhs.trainID }
        if lhs.label.rawValue != rhs.label.rawValue { return lhs.label.rawValue < rhs.label.rawValue }
        if lhs.startSec.bitPattern != rhs.startSec.bitPattern { return lhs.startSec.bitPattern < rhs.startSec.bitPattern }
        if lhs.endSec.bitPattern != rhs.endSec.bitPattern { return lhs.endSec.bitPattern < rhs.endSec.bitPattern }
        let startISIOrder = optionalIntOrder(lhs.startISIIndex, rhs.startISIIndex)
        if startISIOrder != 0 { return startISIOrder < 0 }
        let endISIOrder = optionalIntOrder(lhs.endISIIndex, rhs.endISIIndex)
        if endISIOrder != 0 { return endISIOrder < 0 }
        let startSpikeOrder = optionalIntOrder(
            lhs.startSpikeIndex,
            rhs.startSpikeIndex
        )
        if startSpikeOrder != 0 { return startSpikeOrder < 0 }
        let endSpikeOrder = optionalIntOrder(
            lhs.endSpikeIndex,
            rhs.endSpikeIndex
        )
        if endSpikeOrder != 0 { return endSpikeOrder < 0 }
        let noteOrder = optionalStringOrder(lhs.note, rhs.note)
        if noteOrder != 0 { return noteOrder < 0 }
        let annotatorOrder = optionalStringOrder(lhs.annotator, rhs.annotator)
        if annotatorOrder != 0 { return annotatorOrder < 0 }
        return optionalStringOrder(
            lhs.annotatorIdentitySource?.rawValue,
            rhs.annotatorIdentitySource?.rawValue
        ) < 0
    }

    private static func optionalIntOrder(_ lhs: Int?, _ rhs: Int?) -> Int {
        switch (lhs, rhs) {
        case (.none, .none):
            return 0
        case (.none, .some):
            return -1
        case (.some, .none):
            return 1
        case let (.some(left), .some(right)):
            if left == right { return 0 }
            return left < right ? -1 : 1
        }
    }

    private static func optionalStringOrder(
        _ lhs: String?,
        _ rhs: String?
    ) -> Int {
        switch (lhs, rhs) {
        case (.none, .none):
            return 0
        case (.none, .some):
            return -1
        case (.some, .none):
            return 1
        case let (.some(left), .some(right)):
            if left == right { return 0 }
            return left < right ? -1 : 1
        }
    }

    /// Total ordering for serialized dates, including malformed non-finite values.
    ///
    /// Result-package construction rejects non-finite annotation timestamps before projection,
    /// but the projector is also a public utility. Keeping its ordering total and deterministic
    /// prevents a malformed `Date` from making `sorted(by:)` order-dependent.
    private static func timestampOrder(_ lhs: Date, _ rhs: Date) -> Int {
        let left = lhs.timeIntervalSince1970
        let right = rhs.timeIntervalSince1970
        if left.bitPattern == right.bitPattern { return 0 }
        if left.isFinite != right.isFinite { return left.isFinite ? -1 : 1 }
        if left.isFinite {
            return left < right ? -1 : 1
        }
        return left.bitPattern < right.bitPattern ? -1 : 1
    }
}
