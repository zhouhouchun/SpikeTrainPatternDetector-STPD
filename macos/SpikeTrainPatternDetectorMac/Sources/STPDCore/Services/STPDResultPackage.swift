import Foundation

public enum STPDResultPackageOwnership {
    public static let ownerName = "Zhou Houchun"
    public static let ownerEmail = "zhouhouchun@outlook.com"
}

public enum STPDResultPackageSourceMode: String, Hashable, Sendable {
    /// Final events and ISI labels were derived from the automatic detector projection only.
    case automatic
    /// Manual annotations changed the public projection, without a formal candidate-review action.
    case manual
    /// At least one formal candidate-review action is authoritative for the public projection.
    case reviewed
}

public enum STPDCandidateReviewStatus: String, Hashable, Sendable, CaseIterable {
    /// The reviewer confirmed the automatic candidate without changing its public projection.
    case accepted
    /// The reviewer removed the automatic candidate from the public projection.
    case rejected
    /// The reviewer changed the public projection, with a linked manual annotation carrying the
    /// authoritative replacement geometry and label.
    case modified
    /// The candidate still needs review and therefore cannot authorize a reviewed result.
    case needsReview = "needs_review"

    var grantsReviewAuthority: Bool {
        self != .needsReview
    }
}

enum STPDCandidateReviewIdentity {
    static func make(
        candidateUID: String,
        status: STPDCandidateReviewStatus,
        reviewer: String,
        note: String,
        reviewedAtUnixSec: String,
        reviewedRunID: String
    ) -> String {
        STPDStableIdentifier.make(
            prefix: "review",
            domain: "stpd_candidate_review_uid_v3",
            components: [
                candidateUID,
                status.rawValue,
                reviewer,
                note,
                reviewedAtUnixSec,
                reviewedRunID,
            ]
        )
    }
}

public struct STPDCandidateReviewInput: Hashable, Sendable {
    public let sourceCandidateID: String
    public let status: STPDCandidateReviewStatus
    public let reviewer: String
    public let note: String
    public let reviewedAt: Date?
    /// Detector execution whose candidate geometry and decision were reviewed.
    ///
    /// Candidate IDs can be stable across reruns, so the candidate ID alone is not sufficient
    /// authority. An authority-bearing review is valid only for this exact detector run.
    public let reviewedRunID: String

    public init(
        sourceCandidateID: String,
        status: STPDCandidateReviewStatus,
        reviewer: String = "",
        note: String = "",
        reviewedAt: Date? = nil,
        reviewedRunID: String = ""
    ) {
        self.sourceCandidateID = sourceCandidateID
        self.status = status
        self.reviewer = reviewer
        self.note = note
        self.reviewedAt = reviewedAt
        self.reviewedRunID = reviewedRunID
    }
}

public enum STPDResultReviewEvidence: Hashable, Sendable {
    case manualAnnotation(UUID)
    case candidateReview(sourceCandidateID: String)
}

/// A causal edge from one authoritative review action to one public ISI result.
///
/// Reviewed packages are fail-closed: merely carrying a manual annotation or review record is not
/// sufficient. Every authority-bearing record must be linked to the ISIs it actually governs.
public struct STPDResultReviewLink: Hashable, Sendable {
    public let trainID: String
    public let isiIndex: Int
    public let evidence: STPDResultReviewEvidence

    public init(
        trainID: String,
        isiIndex: Int,
        evidence: STPDResultReviewEvidence
    ) {
        self.trainID = trainID
        self.isiIndex = isiIndex
        self.evidence = evidence
    }
}

public struct STPDCandidateDiagnosticInput: Hashable, Sendable {
    public let sourceCandidateID: String
    public let stageName: String
    public let stageOrdinal: Int
    public let status: String
    public let details: String

    public init(
        sourceCandidateID: String,
        stageName: String,
        stageOrdinal: Int,
        status: String,
        details: String
    ) {
        self.sourceCandidateID = sourceCandidateID
        self.stageName = stageName
        self.stageOrdinal = max(0, stageOrdinal)
        self.status = status
        self.details = details
    }
}

/// All inputs needed to materialize a normalized result package.
///
/// The detector run is immutable evidence. `finalEvents` and `finalISILabelRows` are explicit so a
/// reviewed result cannot be silently reconstructed from automatic candidates and mislabeled as
/// human-reviewed. Use `automatic(dataset:run:)` only when automatic projection is intended.
public struct STPDResultPackageInput: Sendable {
    public let dataset: SpikeDataset
    public let run: ClassicAnchorDetectionRun
    public let sourceMode: STPDResultPackageSourceMode
    public let finalEvents: [ClassicAnchorEventAnnotation]
    public let finalISILabelRows: [ReviewedISIExportRow]
    public let manualAnnotations: [ManualAnnotation]
    public let candidateReviews: [STPDCandidateReviewInput]
    public let reviewLinks: [STPDResultReviewLink]
    public let candidateDiagnostics: [STPDCandidateDiagnosticInput]

    public init(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        sourceMode: STPDResultPackageSourceMode,
        finalEvents: [ClassicAnchorEventAnnotation],
        finalISILabelRows: [ReviewedISIExportRow],
        manualAnnotations: [ManualAnnotation] = [],
        candidateReviews: [STPDCandidateReviewInput] = [],
        reviewLinks: [STPDResultReviewLink] = [],
        candidateDiagnostics: [STPDCandidateDiagnosticInput] = []
    ) {
        self.dataset = dataset
        self.run = run
        self.sourceMode = sourceMode
        self.finalEvents = finalEvents
        self.finalISILabelRows = finalISILabelRows
        self.manualAnnotations = manualAnnotations
        self.candidateReviews = candidateReviews
        self.reviewLinks = reviewLinks
        self.candidateDiagnostics = candidateDiagnostics
    }

    public static func automatic(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        candidateDiagnostics: [STPDCandidateDiagnosticInput] = []
    ) -> STPDResultPackageInput {
        let events = run.eventAnnotations(
            in: dataset,
            tracks: [.event, .gap, .state]
        )
        let isiRows = ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: events,
            projectionsByTrain: [:]
        )
        return STPDResultPackageInput(
            dataset: dataset,
            run: run,
            sourceMode: .automatic,
            finalEvents: events,
            finalISILabelRows: isiRows,
            candidateDiagnostics: canonicalDiagnostics(candidateDiagnostics)
        )
    }

    /// Builds the current public result snapshot from automatic output plus optional review state.
    ///
    /// Only authority-bearing reviews with automatic public ISI coverage enter the reviewed
    /// projection. `needs_review` and reviews of audit-only/non-public candidates remain explicit
    /// diagnostics; they never gain authority merely because they were present in UI state.
    public static func snapshot(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        manualAnnotations: [ManualAnnotation] = [],
        candidateReviews: [STPDCandidateReviewInput] = [],
        candidateDiagnostics: [STPDCandidateDiagnosticInput] = []
    ) throws -> STPDResultPackageInput {
        let orderedReviews = canonicalReviews(candidateReviews)
        try validateUniqueCandidateReviews(
            orderedReviews,
            expectedRunID: run.runIdentity.runID
        )
        guard !manualAnnotations.isEmpty || !orderedReviews.isEmpty else {
            return automatic(
                dataset: dataset,
                run: run,
                candidateDiagnostics: candidateDiagnostics
            )
        }
        return try reviewed(
            dataset: dataset,
            run: run,
            manualAnnotations: manualAnnotations,
            candidateReviews: orderedReviews,
            candidateDiagnostics: candidateDiagnostics
        )
    }

    /// Constructs the causal reviewed projection used by the package validator.
    ///
    /// This is the sole production assembly path for manual/reviewed output. The builder recomputes
    /// the same projection and rejects missing, extra, or non-causal review links.
    public static func reviewed(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        manualAnnotations: [ManualAnnotation] = [],
        candidateReviews: [STPDCandidateReviewInput] = [],
        candidateDiagnostics: [STPDCandidateDiagnosticInput] = []
    ) throws -> STPDResultPackageInput {
        let automaticInput = automatic(dataset: dataset, run: run)
        let orderedReviews = canonicalReviews(candidateReviews)
        try validateUniqueCandidateReviews(
            orderedReviews,
            expectedRunID: run.runIdentity.runID
        )
        try validateFiniteManualAnnotationTimestamps(manualAnnotations)
        try validateNoConflictingLatestManualRevisions(manualAnnotations)
        let canonicalAnnotations =
            ManualAnnotationProjector.canonicalizedAnnotations(manualAnnotations)
        let resolvedAnnotations = try canonicalAnnotations.map { annotation in
            guard let resolved = ManualAnnotationGeometryResolver
                .resolvingIndicesIfCompatible(annotation, in: dataset.trains) else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(annotation.id.uuidString) is incompatible with the dataset"
                )
            }
            return resolved
        }
        try validateNoAmbiguousManualEditTies(resolvedAnnotations)
        let annotationsByTrain = Dictionary(
            grouping: resolvedAnnotations,
            by: \.trainID
        )
        let automaticIntervalRows = automaticInput.finalISILabelRows.filter {
            $0.isiIndex > 0
        }
        let automaticRowsByTrain = Dictionary(
            grouping: automaticIntervalRows,
            by: \.trainID
        )
        var projectionsByTrain: [String: ManualAnnotationProjection] = [:]

        for train in dataset.trains {
            let autoLabels = Dictionary(
                uniqueKeysWithValues: (automaticRowsByTrain[train.id] ?? [])
                    .filter { !$0.autoPattern.isEmpty }
                    .map { ($0.isiIndex, $0.autoPattern) }
            )
            projectionsByTrain[train.id] = ManualAnnotationProjector.project(
                train: train,
                autoLabelsByISI: autoLabels,
                annotations: annotationsByTrain[train.id] ?? [],
                honorManualLock: true,
                manualNegativeLabelsEnabled: true,
                minValidISISeconds: run.qualitySettings.artifactThresholdSec
            )
        }

        let manualOnlyRows = ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: automaticInput.finalEvents,
            projectionsByTrain: projectionsByTrain
        )
        let allRejectedCandidateIDs = Set(orderedReviews.compactMap {
            $0.status == .rejected ? $0.sourceCandidateID : nil
        })
        let rowsWithAllRejections = ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: automaticInput.finalEvents,
            projectionsByTrain: projectionsByTrain,
            reviewRejectedCandidateIDs: allRejectedCandidateIDs
        )
        let manualOnlyByKey = Dictionary(
            uniqueKeysWithValues: manualOnlyRows
                .filter { $0.isiIndex > 0 }
                .map { (isiKey(trainID: $0.trainID, isiIndex: $0.isiIndex), $0) }
        )
        let manualEvidenceOwnersByKey = manualEvidenceOwnerMap(
            dataset: dataset,
            run: run,
            automaticEvents: automaticInput.finalEvents,
            automaticRows: automaticInput.finalISILabelRows,
            resolvedAnnotations: resolvedAnnotations,
            projectionsByTrain: projectionsByTrain,
            finalRows: manualOnlyRows,
            reviewRejectedCandidateIDs: []
        )

        var effectiveReviews: [STPDCandidateReviewInput] = []
        var nonCausalReviews: [(STPDCandidateReviewInput, String)] = []
        for review in orderedReviews {
            guard review.status.grantsReviewAuthority else {
                nonCausalReviews.append((review, "non_authoritative_review_status"))
                continue
            }
            let candidateRows = automaticIntervalRows.filter {
                $0.autoCandidateID == review.sourceCandidateID
            }
            let hasCausalCoverage: Bool
            switch review.status {
            case .accepted:
                hasCausalCoverage = !candidateRows.isEmpty
                    && candidateRows.allSatisfy { automaticRow in
                        let key = isiKey(
                            trainID: automaticRow.trainID,
                            isiIndex: automaticRow.isiIndex
                        )
                        guard let reviewedRow = manualOnlyByKey[key] else {
                            return false
                        }
                        return reviewedRow.finalSource
                                == ReviewedISIExportBuilder.sourceAutoProjected
                            && STPDResultPackageBuilder
                                .isiProjectionComponents(reviewedRow)
                                == STPDResultPackageBuilder
                                .isiProjectionComponents(automaticRow)
                    }
            case .rejected:
                let publicCandidateRows = candidateRows.filter {
                    $0.finalSource == ReviewedISIExportBuilder.sourceAutoProjected
                }
                let rowsWithoutThisRejection = ReviewedISIExportBuilder.build(
                    dataset: dataset,
                    autoAnnotations: automaticInput.finalEvents,
                    projectionsByTrain: projectionsByTrain,
                    reviewRejectedCandidateIDs:
                        allRejectedCandidateIDs.subtracting([review.sourceCandidateID])
                )
                hasCausalCoverage = !publicCandidateRows.isEmpty
                    && !causalChangedISIKeys(
                        actualRows: rowsWithAllRejections,
                        counterfactualRows: rowsWithoutThisRejection
                    ).isEmpty
            case .modified:
                let changedKeys = candidateRows.compactMap { automaticRow -> String? in
                    let key = isiKey(
                        trainID: automaticRow.trainID,
                        isiIndex: automaticRow.isiIndex
                    )
                    guard let reviewedRow = manualOnlyByKey[key],
                          STPDResultPackageBuilder
                            .isiProjectionComponents(reviewedRow)
                            != STPDResultPackageBuilder
                            .isiProjectionComponents(automaticRow) else {
                        return nil
                    }
                    return key
                }
                hasCausalCoverage = !changedKeys.isEmpty
                    && changedKeys.allSatisfy {
                        !(manualEvidenceOwnersByKey[$0] ?? []).isEmpty
                    }
            case .needsReview:
                hasCausalCoverage = false
            }
            if hasCausalCoverage {
                try validateAuthorityBearingCandidateReview(
                    review,
                    expectedRunID: run.runIdentity.runID
                )
                effectiveReviews.append(review)
            } else {
                nonCausalReviews.append((review, "no_causal_public_projection"))
            }
        }

        let effectiveRejectedCandidateIDs = Set(effectiveReviews.compactMap {
            $0.status == .rejected ? $0.sourceCandidateID : nil
        })
        let finalRows = ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: automaticInput.finalEvents,
            projectionsByTrain: projectionsByTrain,
            reviewRejectedCandidateIDs: effectiveRejectedCandidateIDs
        )
        let finalRowsByKey = Dictionary(
            uniqueKeysWithValues: finalRows
                .filter { $0.isiIndex > 0 }
                .map { (isiKey(trainID: $0.trainID, isiIndex: $0.isiIndex), $0) }
        )
        let finalManualEvidenceOwnersByKey = manualEvidenceOwnerMap(
            dataset: dataset,
            run: run,
            automaticEvents: automaticInput.finalEvents,
            automaticRows: automaticInput.finalISILabelRows,
            resolvedAnnotations: resolvedAnnotations,
            projectionsByTrain: projectionsByTrain,
            finalRows: finalRows,
            reviewRejectedCandidateIDs: effectiveRejectedCandidateIDs
        )
        let rejectedISIsByTrain = Dictionary(
            grouping: automaticIntervalRows.filter {
                effectiveRejectedCandidateIDs.contains($0.autoCandidateID)
            },
            by: \.trainID
        )
        .mapValues { Set($0.map(\.isiIndex)) }
        var lockSuppressedByTrain: [String: Set<Int>] = [:]
        var vetoedBurstISIsByTrain: [String: Set<Int>] = [:]
        var manualBurstISIsByTrain: [String: Set<Int>] = [:]
        for train in dataset.trains {
            let projection = projectionsByTrain[train.id]
            lockSuppressedByTrain[train.id] =
                eventProjectionLockSuppressedISIs(
                    projection: projection,
                    automaticRows: automaticRowsByTrain[train.id] ?? [],
                    rejectedISIs: rejectedISIsByTrain[train.id] ?? []
                )
            vetoedBurstISIsByTrain[train.id] =
                projection?.autoBurstBlockedByVetoISIs ?? []
            manualBurstISIsByTrain[train.id] = Set(
                (projection?.manualPositiveLabelByISI ?? [:]).compactMap {
                    ManualAnnotationProjector.burstFamilyLabels.contains($0.value)
                        ? $0.key
                        : nil
                }
            )
        }
        let finalEvents = ManualAnnotationProjector.projectPublicEventAnnotations(
            automaticInput.finalEvents,
            vetoedBurstISIsByTrain: vetoedBurstISIsByTrain,
            lockSuppressedISIsByTrain: lockSuppressedByTrain,
            validatedManualBurstSupportISIsByTrain: manualBurstISIsByTrain,
            trainsByID: Dictionary(
                uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) }
            )
        ).annotations

        var reviewLinks: [STPDResultReviewLink] = []
        reviewLinks.append(contentsOf: finalManualEvidenceOwnersByKey
            .sorted { $0.key < $1.key }
            .flatMap { key, annotationIDs -> [STPDResultReviewLink] in
                guard let row = finalRowsByKey[key] else { return [] }
                return annotationIDs.sorted { $0.uuidString < $1.uuidString }.map {
                    STPDResultReviewLink(
                        trainID: row.trainID,
                        isiIndex: row.isiIndex,
                        evidence: .manualAnnotation($0)
                    )
                }
            })
        for review in effectiveReviews {
            if review.status == .rejected {
                let rowsWithoutThisRejection = ReviewedISIExportBuilder.build(
                    dataset: dataset,
                    autoAnnotations: automaticInput.finalEvents,
                    projectionsByTrain: projectionsByTrain,
                    reviewRejectedCandidateIDs:
                        effectiveRejectedCandidateIDs.subtracting([
                            review.sourceCandidateID
                        ])
                )
                let causalKeys = causalChangedISIKeys(
                    actualRows: finalRows,
                    counterfactualRows: rowsWithoutThisRejection
                )
                reviewLinks.append(contentsOf: finalRows
                    .filter {
                        $0.isiIndex > 0
                            && causalKeys.contains(isiKey(
                                trainID: $0.trainID,
                                isiIndex: $0.isiIndex
                            ))
                    }
                    .map { row in
                        STPDResultReviewLink(
                            trainID: row.trainID,
                            isiIndex: row.isiIndex,
                            evidence: .candidateReview(
                                sourceCandidateID: review.sourceCandidateID
                            )
                        )
                    })
                continue
            }
            reviewLinks.append(contentsOf: automaticIntervalRows
                .filter { automaticRow in
                    guard automaticRow.autoCandidateID == review.sourceCandidateID else {
                        return false
                    }
                    let key = isiKey(
                        trainID: automaticRow.trainID,
                        isiIndex: automaticRow.isiIndex
                    )
                    guard let reviewedRow = finalRowsByKey[key] else { return false }
                    switch review.status {
                    case .accepted:
                        return reviewedRow.finalSource
                                == ReviewedISIExportBuilder.sourceAutoProjected
                            && STPDResultPackageBuilder
                                .isiProjectionComponents(reviewedRow)
                                == STPDResultPackageBuilder
                                .isiProjectionComponents(automaticRow)
                    case .rejected:
                        return reviewedRow.finalSource
                            == ReviewedISIExportBuilder.sourceManualReviewRejected
                    case .modified:
                        return !(finalManualEvidenceOwnersByKey[key] ?? []).isEmpty
                            && STPDResultPackageBuilder
                                .isiProjectionComponents(reviewedRow)
                                != STPDResultPackageBuilder
                                .isiProjectionComponents(automaticRow)
                    case .needsReview:
                        return false
                    }
                }
                .map { row in
                    STPDResultReviewLink(
                        trainID: row.trainID,
                        isiIndex: row.isiIndex,
                        evidence: .candidateReview(
                            sourceCandidateID: review.sourceCandidateID
                        )
                    )
                })
        }
        let diagnostics = try appendingReviewDiagnostics(
            candidateDiagnostics,
            reviewsAndReasons: nonCausalReviews
        )
        guard !reviewLinks.isEmpty else {
            let containsActiveSpikeOnlyAnnotation = resolvedAnnotations.contains {
                $0.startISIIndex == nil &&
                    $0.endISIIndex == nil &&
                    $0.startSpikeIndex != nil &&
                    $0.startSpikeIndex == $0.endSpikeIndex
            }
            return STPDResultPackageInput(
                dataset: dataset,
                run: run,
                sourceMode: containsActiveSpikeOnlyAnnotation ? .manual : .automatic,
                finalEvents: automaticInput.finalEvents,
                finalISILabelRows: automaticInput.finalISILabelRows,
                manualAnnotations: resolvedAnnotations,
                candidateReviews: orderedReviews,
                reviewLinks: [],
                candidateDiagnostics: diagnostics
            )
        }

        let canonicalLinks = canonicalReviewLinks(reviewLinks)
        let sourceMode: STPDResultPackageSourceMode = canonicalLinks.contains {
            if case .candidateReview = $0.evidence { return true }
            return false
        } ? .reviewed : .manual
        return STPDResultPackageInput(
            dataset: dataset,
            run: run,
            sourceMode: sourceMode,
            finalEvents: finalEvents,
            finalISILabelRows: finalRows,
            manualAnnotations: resolvedAnnotations,
            candidateReviews: orderedReviews,
            reviewLinks: canonicalLinks,
            candidateDiagnostics: diagnostics
        )
    }

    private static func causalChangedISIKeys(
        actualRows: [ReviewedISIExportRow],
        counterfactualRows: [ReviewedISIExportRow]
    ) -> Set<String> {
        let counterfactualByKey = Dictionary(
            uniqueKeysWithValues: counterfactualRows
                .filter { $0.isiIndex > 0 }
                .map { (isiKey(trainID: $0.trainID, isiIndex: $0.isiIndex), $0) }
        )
        return Set(actualRows.compactMap { actual -> String? in
            guard actual.isiIndex > 0 else { return nil }
            let key = isiKey(trainID: actual.trainID, isiIndex: actual.isiIndex)
            guard let counterfactual = counterfactualByKey[key],
                  STPDResultPackageBuilder.isiProjectionComponents(actual)
                    != STPDResultPackageBuilder
                        .isiProjectionComponents(counterfactual) else {
                return nil
            }
            return key
        })
    }

    fileprivate static func manualEvidenceOwnerMap(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        automaticEvents: [ClassicAnchorEventAnnotation],
        automaticRows: [ReviewedISIExportRow],
        resolvedAnnotations: [ManualAnnotation],
        projectionsByTrain: [String: ManualAnnotationProjection],
        finalRows: [ReviewedISIExportRow],
        reviewRejectedCandidateIDs: Set<String>
    ) -> [String: Set<UUID>] {
        let finalByKey = Dictionary(
            uniqueKeysWithValues: finalRows
                .filter { $0.isiIndex > 0 }
                .map { (isiKey(trainID: $0.trainID, isiIndex: $0.isiIndex), $0) }
        )
        let automaticByKey = Dictionary(
            uniqueKeysWithValues: automaticRows
                .filter { $0.isiIndex > 0 }
                .map { (isiKey(trainID: $0.trainID, isiIndex: $0.isiIndex), $0) }
        )
        let annotationsByTrain = Dictionary(
            grouping: resolvedAnnotations,
            by: \.trainID
        )
        var owners: [String: Set<UUID>] = [:]
        for train in dataset.trains {
            let trainAnnotations = annotationsByTrain[train.id] ?? []
            guard !trainAnnotations.isEmpty else { continue }
            let autoLabelsByISI = Dictionary(
                uniqueKeysWithValues: automaticRows
                    .filter {
                        $0.trainID == train.id
                            && $0.isiIndex > 0
                            && !$0.autoPattern.isEmpty
                    }
                    .map { ($0.isiIndex, $0.autoPattern) }
            )

            func projection(removing annotationIDs: Set<UUID>)
                -> ManualAnnotationProjection {
                ManualAnnotationProjector.project(
                    train: train,
                    autoLabelsByISI: autoLabelsByISI,
                    annotations: trainAnnotations.filter {
                        !annotationIDs.contains($0.id)
                    },
                    honorManualLock: true,
                    manualNegativeLabelsEnabled: true,
                    minValidISISeconds: run.qualitySettings.artifactThresholdSec
                )
            }

            func recordChanges(
                from alternateProjection: ManualAnnotationProjection,
                ownerID: UUID
            ) {
                var alternateProjections = projectionsByTrain
                alternateProjections[train.id] = alternateProjection
                let alternateRows = ReviewedISIExportBuilder.build(
                    dataset: dataset,
                    autoAnnotations: automaticEvents,
                    projectionsByTrain: alternateProjections,
                    reviewRejectedCandidateIDs: reviewRejectedCandidateIDs
                )
                for alternate in alternateRows
                where alternate.trainID == train.id && alternate.isiIndex > 0 {
                    let key = isiKey(
                        trainID: alternate.trainID,
                        isiIndex: alternate.isiIndex
                    )
                    guard let final = finalByKey[key],
                          STPDResultPackageBuilder.isiProjectionComponents(final)
                            != STPDResultPackageBuilder
                                .isiProjectionComponents(alternate) else {
                        continue
                    }
                    owners[key, default: []].insert(ownerID)
                }
            }

            // Primary causal attribution: remove one annotation and retain every directly or
            // structurally changed ISI. This captures neighboring burst-minimum effects.
            for annotation in trainAnnotations {
                recordChanges(
                    from: projection(removing: [annotation.id]),
                    ownerID: annotation.id
                )
            }

            guard let finalProjection = projectionsByTrain[train.id] else {
                continue
            }

            // Removing only the newest of two equivalent edits reveals the older substitute and
            // produces no delta. For each currently active owner, remove the complete equivalence
            // class (same label and effective QC-valid ISI geometry), then assign the resulting
            // direct and structural changes to that deterministic last-edit-wins owner.
            func effectiveIndices(_ annotation: ManualAnnotation) -> Set<Int> {
                guard let covered = ManualAnnotationGeometryResolver
                    .resolve(annotation: annotation, in: train)
                    .coveredISIIndices else {
                    return []
                }
                return Set(covered.filter { index in
                    guard train.isiSec.indices.contains(index),
                          let value = train.isiSec[index] else {
                        return false
                    }
                    return value.isFinite
                        && value >= run.qualitySettings.artifactThresholdSec
                })
            }
            let annotationsByID = Dictionary(
                uniqueKeysWithValues: trainAnnotations.map { ($0.id, $0) }
            )
            let activeOwnerIDs = Set(
                finalProjection.manualPositiveOwnerByISI.values
            )
            .union(finalProjection.manualNegativeVetoOwnerByISI.values)
            for ownerID in activeOwnerIDs.sorted(by: {
                $0.uuidString < $1.uuidString
            }) {
                guard let owner = annotationsByID[ownerID] else { continue }
                let ownerIndices = effectiveIndices(owner)
                let equivalentIDs = Set(trainAnnotations.compactMap {
                    annotation -> UUID? in
                    annotation.label == owner.label
                        && effectiveIndices(annotation) == ownerIndices
                        ? annotation.id
                        : nil
                })
                guard equivalentIDs.count > 1 else { continue }
                recordChanges(
                    from: projection(removing: equivalentIDs),
                    ownerID: ownerID
                )
            }

            // A narrow direct fallback handles partially overlapping superseded edits without
            // claiming unrelated structural rows. Final-source precedence is authoritative:
            // positive manual output wins over a co-located veto, so the veto cannot co-own it.
            for final in finalRows
            where final.trainID == train.id && final.isiIndex > 0 {
                let key = isiKey(
                    trainID: final.trainID,
                    isiIndex: final.isiIndex
                )
                guard let automatic = automaticByKey[key],
                      STPDResultPackageBuilder.isiProjectionComponents(final)
                        != STPDResultPackageBuilder
                            .isiProjectionComponents(automatic) else {
                    continue
                }
                let ownerID: UUID?
                switch final.finalSource {
                case ReviewedISIExportBuilder.sourceManualPositive:
                    ownerID =
                        finalProjection.manualPositiveOwnerByISI[final.isiIndex]
                case ReviewedISIExportBuilder.sourceManualVetoRemoved:
                    ownerID =
                        finalProjection.manualNegativeVetoOwnerByISI[final.isiIndex]
                default:
                    ownerID = nil
                }
                if let ownerID {
                    owners[key, default: []].insert(ownerID)
                }
            }
        }
        return owners
    }

    private static func validateUniqueCandidateReviews(
        _ reviews: [STPDCandidateReviewInput],
        expectedRunID: String
    ) throws {
        let sourceIDs = reviews.map(\.sourceCandidateID)
        guard Set(sourceIDs).count == sourceIDs.count else {
            throw STPDResultPackageError.invalidInput(
                "candidate review source IDs are not unique"
            )
        }
        for review in reviews {
            try validateFiniteCandidateReviewTimestamp(review)
            if review.status.grantsReviewAuthority {
                try validateAuthorityBearingCandidateReview(
                    review,
                    expectedRunID: expectedRunID
                )
            }
        }
    }

    fileprivate static func validateFiniteCandidateReviewTimestamp(
        _ review: STPDCandidateReviewInput
    ) throws {
        guard let reviewedAt = review.reviewedAt else { return }
        guard reviewedAt.timeIntervalSince1970.isFinite else {
            throw STPDResultPackageError.invalidInput(
                "candidate review time must be finite"
            )
        }
    }

    fileprivate static func validateFiniteManualAnnotationTimestamps(
        _ annotations: [ManualAnnotation]
    ) throws {
        for annotation in annotations {
            guard annotation.createdAt.timeIntervalSince1970.isFinite,
                  annotation.updatedAt.timeIntervalSince1970.isFinite else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(annotation.id.uuidString) has a non-finite edit timestamp"
                )
            }
        }
    }

    fileprivate static func validateAuthorityBearingCandidateReview(
        _ review: STPDCandidateReviewInput,
        expectedRunID: String
    ) throws {
        try validateFiniteCandidateReviewTimestamp(review)
        guard review.reviewedRunID == expectedRunID else {
            throw STPDResultPackageError.invalidInput(
                "authority-bearing candidate review belongs to a different detector run"
            )
        }
        guard !review.reviewer
            .trimmingCharacters(in: .whitespacesAndNewlines).isEmpty,
              review.reviewedAt != nil else {
            throw STPDResultPackageError.invalidInput(
                "authority-bearing candidate review requires reviewer and review time"
            )
        }
    }

    fileprivate static func validateNoAmbiguousManualEditTies(
        _ annotations: [ManualAnnotation]
    ) throws {
        guard annotations.count > 1 else { return }
        let ordered = annotations.sorted {
            if $0.trainID != $1.trainID { return $0.trainID < $1.trainID }
            if $0.updatedAt != $1.updatedAt { return $0.updatedAt < $1.updatedAt }
            if $0.createdAt != $1.createdAt { return $0.createdAt < $1.createdAt }
            return $0.id.uuidString < $1.id.uuidString
        }
        for leftIndex in ordered.indices {
            let left = ordered[leftIndex]
            for right in ordered[ordered.index(after: leftIndex)...] {
                guard left.trainID == right.trainID,
                      left.updatedAt == right.updatedAt,
                      left.createdAt == right.createdAt,
                      manualAnnotationRangesOverlap(left, right) else {
                    continue
                }
                throw STPDResultPackageError.invalidInput(
                    "manual annotations \(left.id.uuidString) and " +
                        "\(right.id.uuidString) have an unresolved equal-timestamp overlap"
                )
            }
        }
    }

    /// Reject two different records that both claim to be the latest revision of one logical
    /// annotation. Choosing one by sort order would silently assign scientific authority to an
    /// arbitrary actor or geometry, so package construction fails closed before canonicalization.
    fileprivate static func validateNoConflictingLatestManualRevisions(
        _ annotations: [ManualAnnotation]
    ) throws {
        for (id, revisions) in Dictionary(grouping: annotations, by: \.id)
            where revisions.count > 1 {
            guard let latest = revisions.max(by: manualRevisionIsOlder) else {
                continue
            }
            let tiedLatest = revisions.filter {
                $0.updatedAt.timeIntervalSince1970
                    == latest.updatedAt.timeIntervalSince1970
                    && $0.createdAt.timeIntervalSince1970
                    == latest.createdAt.timeIntervalSince1970
            }
            guard Set(tiedLatest).count <= 1 else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(id.uuidString) has conflicting equal-latest revisions"
                )
            }
        }
    }

    private static func manualRevisionIsOlder(
        _ lhs: ManualAnnotation,
        _ rhs: ManualAnnotation
    ) -> Bool {
        let leftUpdated = lhs.updatedAt.timeIntervalSince1970
        let rightUpdated = rhs.updatedAt.timeIntervalSince1970
        if leftUpdated != rightUpdated {
            return leftUpdated < rightUpdated
        }
        let leftCreated = lhs.createdAt.timeIntervalSince1970
        let rightCreated = rhs.createdAt.timeIntervalSince1970
        return leftCreated != rightCreated && leftCreated < rightCreated
    }

    private static func manualAnnotationRangesOverlap(
        _ lhs: ManualAnnotation,
        _ rhs: ManualAnnotation
    ) -> Bool {
        if let lhsStart = lhs.startISIIndex,
           let lhsEnd = lhs.endISIIndex,
           let rhsStart = rhs.startISIIndex,
           let rhsEnd = rhs.endISIIndex {
            let leftLower = min(lhsStart, lhsEnd)
            let leftUpper = max(lhsStart, lhsEnd)
            let rightLower = min(rhsStart, rhsEnd)
            let rightUpper = max(rhsStart, rhsEnd)
            return max(leftLower, rightLower) <= min(leftUpper, rightUpper)
        }
        return max(lhs.normalizedStartSec, rhs.normalizedStartSec)
            <= min(lhs.normalizedEndSec, rhs.normalizedEndSec)
    }

    private static func isiKey(trainID: String, isiIndex: Int) -> String {
        "\(trainID)\u{1}\(isiIndex)"
    }

    /// Manual-positive authority changes row provenance, but an exact same-label edit does not
    /// create a scientific event boundary. Different-label edits and rejected candidates still
    /// suppress the automatic event projection at their governed ISIs.
    fileprivate static func eventProjectionLockSuppressedISIs(
        projection: ManualAnnotationProjection?,
        automaticRows: [ReviewedISIExportRow],
        rejectedISIs: Set<Int>
    ) -> Set<Int> {
        let automaticPatternByISI = Dictionary(
            uniqueKeysWithValues: automaticRows
                .filter { $0.isiIndex > 0 && !$0.autoPattern.isEmpty }
                .map { ($0.isiIndex, $0.autoPattern) }
        )
        let sameLabelManualPositiveISIs = Set(
            (projection?.manualPositiveLabelByISI ?? [:]).compactMap {
                automaticPatternByISI[$0.key] == $0.value ? $0.key : nil
            }
        )
        return (projection?.autoBlockedByManualLockISIs ?? [])
            .subtracting(sameLabelManualPositiveISIs)
            .union(rejectedISIs)
    }

    private static func canonicalReviews(
        _ reviews: [STPDCandidateReviewInput]
    ) -> [STPDCandidateReviewInput] {
        reviews.sorted { lhs, rhs in
            let left = [
                lhs.sourceCandidateID,
                lhs.status.rawValue,
                lhs.reviewer,
                canonicalReviewTimestamp(lhs.reviewedAt),
                lhs.reviewedRunID,
                lhs.note,
            ]
            let right = [
                rhs.sourceCandidateID,
                rhs.status.rawValue,
                rhs.reviewer,
                canonicalReviewTimestamp(rhs.reviewedAt),
                rhs.reviewedRunID,
                rhs.note,
            ]
            return left.lexicographicallyPrecedes(right)
        }
    }

    private static func canonicalReviewTimestamp(_ value: Date?) -> String {
        STPDCanonicalValue.double(value?.timeIntervalSince1970)
    }

    private static func canonicalDiagnostics(
        _ diagnostics: [STPDCandidateDiagnosticInput]
    ) -> [STPDCandidateDiagnosticInput] {
        diagnostics.sorted { lhs, rhs in
            if lhs.stageOrdinal != rhs.stageOrdinal {
                return lhs.stageOrdinal < rhs.stageOrdinal
            }
            let left = [
                lhs.sourceCandidateID,
                lhs.stageName,
                lhs.status,
                lhs.details,
            ]
            let right = [
                rhs.sourceCandidateID,
                rhs.stageName,
                rhs.status,
                rhs.details,
            ]
            return left.lexicographicallyPrecedes(right)
        }
    }

    private static func appendingReviewDiagnostics(
        _ supplied: [STPDCandidateDiagnosticInput],
        reviewsAndReasons: [(STPDCandidateReviewInput, String)]
    ) throws -> [STPDCandidateDiagnosticInput] {
        let orderedSupplied = canonicalDiagnostics(supplied)
        guard !reviewsAndReasons.isEmpty else {
            return orderedSupplied
        }
        let ordered = reviewsAndReasons.sorted { lhs, rhs in
            let left = canonicalReviews([lhs.0]).first!
            let right = canonicalReviews([rhs.0]).first!
            let leftKey = [
                left.sourceCandidateID,
                left.status.rawValue,
                left.reviewer,
                canonicalReviewTimestamp(left.reviewedAt),
                left.reviewedRunID,
                left.note,
                lhs.1,
            ]
            let rightKey = [
                right.sourceCandidateID,
                right.status.rawValue,
                right.reviewer,
                canonicalReviewTimestamp(right.reviewedAt),
                right.reviewedRunID,
                right.note,
                rhs.1,
            ]
            return leftKey.lexicographicallyPrecedes(rightKey)
        }
        let maximumOrdinal = orderedSupplied.map(\.stageOrdinal).max() ?? -1
        let (firstOrdinal, firstOverflow) =
            maximumOrdinal.addingReportingOverflow(1)
        let lastOffset = ordered.indices.last ?? 0
        let (_, lastOverflow) = firstOrdinal.addingReportingOverflow(lastOffset)
        guard !firstOverflow, ordered.isEmpty || !lastOverflow else {
            throw STPDResultPackageError.invalidInput(
                "candidate diagnostic stage ordinals cannot be extended safely"
            )
        }
        let generated = ordered.enumerated().map { offset, item in
            let review = item.0
            return STPDCandidateDiagnosticInput(
                sourceCandidateID: review.sourceCandidateID,
                stageName: "result_package_review_snapshot",
                stageOrdinal: firstOrdinal + offset,
                status: review.status.rawValue,
                details: [
                    "reason=\(item.1)",
                    "reviewer=\(review.reviewer)",
                    "reviewed_at_unix_sec=\(canonicalReviewTimestamp(review.reviewedAt))",
                    "reviewed_run_id=\(review.reviewedRunID)",
                    "note=\(review.note)",
                ].joined(separator: ";")
            )
        }
        return canonicalDiagnostics(orderedSupplied + generated)
    }

    private static func canonicalReviewLinks(
        _ links: [STPDResultReviewLink]
    ) -> [STPDResultReviewLink] {
        Array(Set(links)).sorted { lhs, rhs in
            if lhs.trainID != rhs.trainID { return lhs.trainID < rhs.trainID }
            if lhs.isiIndex != rhs.isiIndex { return lhs.isiIndex < rhs.isiIndex }
            return reviewEvidenceKey(lhs.evidence) < reviewEvidenceKey(rhs.evidence)
        }
    }

    private static func reviewEvidenceKey(
        _ evidence: STPDResultReviewEvidence
    ) -> String {
        switch evidence {
        case .manualAnnotation(let id):
            return "manual:\(id.uuidString.lowercased())"
        case .candidateReview(let sourceCandidateID):
            return "review:\(sourceCandidateID)"
        }
    }
}

public enum STPDResultColumnType: String, Codable, Hashable, Sendable {
    case string
    case integer
    case real
    case boolean
    case timestamp
    case stringList = "string_list"
}

public struct STPDResultColumnDefinition: Codable, Hashable, Sendable {
    public let name: String
    public let type: STPDResultColumnType
    public let nullable: Bool

    public init(name: String, type: STPDResultColumnType, nullable: Bool) {
        self.name = name
        self.type = type
        self.nullable = nullable
    }
}

private enum STPDResultTimestamp {
    static func parse(_ value: String) -> Date? {
        STPDCanonicalTimestamp.parse(value).map {
            Date(timeIntervalSince1970: $0.seconds)
        }
    }

    /// Parses the complete signed decimal instant without a negative-epoch cancellation step.
    static func exactSeconds(_ value: String) -> Double? {
        STPDCanonicalTimestamp.parse(value)?.seconds
    }
}

public struct STPDResultTableData: Sendable {
    public let contract: STPDResultTableContract
    public let headers: [String]
    public let columnDefinitions: [STPDResultColumnDefinition]
    public let rows: [[String]]

    public init(
        contract: STPDResultTableContract,
        headers: [String],
        columnDefinitions: [STPDResultColumnDefinition]? = nil,
        rows: [[String]]
    ) throws {
        guard Set(headers).count == headers.count else {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "duplicate header"
            )
        }
        for column in contract.requiredIdentityColumns + contract.primaryKey where !headers.contains(column) {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "missing required column \(column)"
            )
        }
        guard rows.allSatisfy({ $0.count == headers.count }) else {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "row width does not match header width"
            )
        }
        let resolvedDefinitions = columnDefinitions
            ?? STPDResultColumnCatalog.definitions(
                table: contract.table,
                headers: headers,
                nonNullable: Set(contract.requiredIdentityColumns + contract.primaryKey)
            )
        guard resolvedDefinitions.map(\.name) == headers else {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "column definitions do not exactly match ordered headers"
            )
        }
        guard Set(resolvedDefinitions.map(\.name)).count == resolvedDefinitions.count else {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "duplicate column definition"
            )
        }
        let definitionByName = Dictionary(
            uniqueKeysWithValues: resolvedDefinitions.map { ($0.name, $0) }
        )
        for primaryKeyColumn in contract.primaryKey
        where definitionByName[primaryKeyColumn]?.nullable != false {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "primary-key column \(primaryKeyColumn) must be non-nullable"
            )
        }
        for (rowIndex, row) in rows.enumerated() {
            for (columnIndex, value) in row.enumerated() {
                try Self.validateCell(
                    value,
                    definition: resolvedDefinitions[columnIndex],
                    table: contract.table,
                    rowIndex: rowIndex
                )
            }
        }

        let primaryKeyIndices = contract.primaryKey.compactMap { headers.firstIndex(of: $0) }
        self.contract = contract
        self.headers = headers
        self.columnDefinitions = resolvedDefinitions
        self.rows = rows.sorted { lhs, rhs in
            for index in primaryKeyIndices where lhs[index] != rhs[index] {
                return Self.cellPrecedes(
                    lhs[index],
                    rhs[index],
                    type: resolvedDefinitions[index].type
                )
            }
            return lhs.lexicographicallyPrecedes(rhs)
        }
    }

    public var rowCount: Int {
        rows.count
    }

    public var csvData: Data {
        STPDRFC4180.data(headers: headers, rows: rows)
    }

    public func value(row: Int, column: String) -> String? {
        guard rows.indices.contains(row),
              let index = headers.firstIndex(of: column) else {
            return nil
        }
        return rows[row][index]
    }

    private static let lexicallyOrderedStringListColumns: Set<String> = [
        "audit_final_selected_event_subtypes",
        "event_source_candidate_uids",
        "linked_isi_uids",
        "package_final_event_subtypes",
        "review_evidence_uids",
        "source_event_ids",
        "source_candidate_uids",
        "state_high_frequency_subtypes",
        "unresolved_candidate_ids",
        "unresolved_source_candidate_ids",
    ]

    private static let numericallyOrderedStringListColumns: Set<String> = [
        "automatic_support_isi_indices",
        "source_support_isi_indices",
    ]

    private static func validateCell(
        _ value: String,
        definition: STPDResultColumnDefinition,
        table: STPDResultTable,
        rowIndex: Int
    ) throws {
        if value.isEmpty {
            guard definition.nullable else {
                throw STPDResultPackageError.invalidTable(
                    table: table.rawValue,
                    reason: "row \(rowIndex) has an empty non-nullable \(definition.name)"
                )
            }
            return
        }

        let isValid: Bool
        switch definition.type {
        case .string:
            isValid = true
        case .stringList:
            if let values = STPDCanonicalValue.parseStringList(value) {
                let structurallyValid =
                    values.allSatisfy { !$0.isEmpty } &&
                    Set(values).count == values.count &&
                    STPDCanonicalValue.stringList(values) == value
                let orderValid: Bool
                if lexicallyOrderedStringListColumns.contains(definition.name) {
                    orderValid = values == values.sorted()
                } else if numericallyOrderedStringListColumns.contains(definition.name) {
                    let parsed = values.compactMap(Int.init)
                    orderValid =
                        parsed.count == values.count &&
                        parsed == parsed.sorted()
                } else {
                    orderValid = true
                }
                isValid = structurallyValid && orderValid
            } else {
                isValid = false
            }
        case .integer:
            isValid = Int(value) != nil
        case .real:
            isValid = Double(value)?.isFinite == true
        case .boolean:
            isValid = value == "true" || value == "false"
        case .timestamp:
            isValid = STPDResultTimestamp.parse(value) != nil
        }
        guard isValid else {
            throw STPDResultPackageError.invalidTable(
                table: table.rawValue,
                reason:
                    "row \(rowIndex) column \(definition.name) is not a valid " +
                    definition.type.rawValue
            )
        }
    }

    private static func cellPrecedes(
        _ lhs: String,
        _ rhs: String,
        type: STPDResultColumnType
    ) -> Bool {
        if lhs.isEmpty || rhs.isEmpty {
            return lhs.isEmpty && !rhs.isEmpty
        }
        switch type {
        case .integer:
            return (Int(lhs) ?? 0) < (Int(rhs) ?? 0)
        case .real:
            return (Double(lhs) ?? 0) < (Double(rhs) ?? 0)
        case .boolean:
            return lhs == "false" && rhs == "true"
        case .timestamp:
            guard let left = STPDResultTimestamp.parse(lhs),
                  let right = STPDResultTimestamp.parse(rhs) else {
                return lhs < rhs
            }
            return left < right
        case .string, .stringList:
            return lhs < rhs
        }
    }
}

private enum STPDResultColumnCatalog {
    private static let booleanColumns: Set<String> = [
        "anchor_is_seed",
        "anchor_band_ordered",
        "audit_hfs_selected_for_auto",
        "audit_burst_packet_like",
        "audit_burst_dominated",
        "audit_requires_review",
        "audit_review_required",
        "burst_boundary_applied",
        "burst_boundary_floor_hard",
        "burst_bridge_count_pass",
        "burst_bridge_fraction_pass",
        "burst_possible_boundary_pass",
        "burst_q90_bridge_pass",
        "burst_strict_boundary_pass",
        "build_reproducibility_attested",
        "candidate_selected_for_auto",
        "dataset_summary_included_target_train",
        "dataset_summary_self_inclusive",
        "hf_burst_packet_like",
        "hf_burst_packet_neighbor",
        "hard_threshold",
        "hf_burst_dominated",
        "is_audit_only",
        "is_selected",
        "manual_veto_suppressed",
        "may_propagate_to_dataset",
        "may_select_final_label",
        "package_final_projection_differs_from_audit",
        "profile_boundary_floor_hard",
        "qc_refractory_suspect",
        "requires_review",
        "review_changed_projection",
        "review_evidence_present",
        "selected_for_auto",
        "source_selected_for_auto",
        "state_continuity_authority_frozen",
        "state_continuity_merge_terminal",
        "suppressed_by_hf_state",
        "train_qc_input_was_unsorted",
    ]

    private static let integerColumns: Set<String> = [
        "audit_all_selected_burst_event_count",
        "audit_conflict_end_isi",
        "audit_conflict_start_isi",
        "audit_hfs_end_isi",
        "audit_hfs_end_isi_index",
        "audit_hfs_priority",
        "audit_hfs_start_isi",
        "audit_hfs_start_isi_index",
        "audit_long_burst_priority",
        "audit_packet_count",
        "audit_packet_evidence_event_count",
        "audit_pause_like_break_count",
        "audit_pause_like_break_group_count",
        "audit_selected_burst_ii_count",
        "audit_selected_long_burst_count",
        "audit_strongest_burst_priority",
        "audit_suppressed_burst_proposal_count",
        "candidate_end_isi_index",
        "candidate_end_spike_index",
        "candidate_priority",
        "candidate_start_isi_index",
        "candidate_start_spike_index",
        "burst_seed_run_end_isi",
        "burst_seed_run_start_isi",
        "end_isi_index",
        "end_spike_index",
        "end_spike_array_index",
        "end_spike_ordinal",
        "event_end_isi_index",
        "event_end_spike_index",
        "event_priority",
        "event_start_isi_index",
        "event_start_spike_index",
        "final_event_count",
        "final_isi_count",
        "hard_burst_core_isi_count",
        "hf_max_consecutive_large_isi",
        "hf_min_spikes_required",
        "isi_index",
        "n_isi",
        "n_spikes",
        "n_valid_isi",
        "priority",
        "profile_max_seed_run_length",
        "refractory_suspect_count",
        "row_count",
        "spike_count",
        "spike_index",
        "spike_array_index",
        "spike_ordinal",
        "source_event_index",
        "left_spike_array_index",
        "left_spike_ordinal",
        "right_spike_array_index",
        "right_spike_ordinal",
        "stage_ordinal",
        "state_core_burst_run_length",
        "start_isi_index",
        "start_spike_index",
        "start_spike_array_index",
        "start_spike_ordinal",
        "task_event_count",
        "train_qc_artifact_isi_count",
        "train_qc_dropped_duplicate_timestamp_count",
        "train_qc_duplicate_timestamp_count",
        "train_qc_input_duplicate_timestamp_step_count",
        "train_qc_input_nonmonotonic_step_count",
        "train_qc_input_zero_or_negative_step_count",
        "train_qc_refractory_suspect_isi_count",
        "train_qc_valid_isi_count",
        "train_qc_zero_or_negative_isi_count",
        "train_qc_zero_or_negative_timestamp_step_count",
        "train_count",
    ]

    private static let realColumns: Set<String> = [
        "anchor_contrast_geom_required",
        "anchor_contrast_min_required",
        "audit_packet_coverage",
        "event_local_percentile_median",
        "event_local_robust_z_median",
        "hf_embedded_burst_coverage",
        "profile_burst_contrast_s",
        "profile_possible_contrast_s",
        "profile_seed_high_percentile",
        "profile_seed_low_percentile",
        "cv",
        "cv2",
        "lv",
        "score",
        "state_local_percentile_median",
        "state_local_robust_z_median",
        "state_train_percentile_median",
        "burst_contrast_required",
        "burst_possible_contrast_required",
        "train_qc_firing_rate_hz",
    ]

    private static let timestampColumns: Set<String> = [
        "created_at",
        "reviewed_at",
        "updated_at",
    ]

    private static let stringListColumns: Set<String> = [
        "audit_final_selected_event_subtypes",
        "automatic_support_isi_indices",
        "event_source_candidate_uids",
        "linked_isi_uids",
        "package_final_event_subtypes",
        "review_evidence_uids",
        "source_event_ids",
        "source_candidate_uids",
        "source_support_isi_indices",
        "state_high_frequency_subtypes",
        "unresolved_candidate_ids",
        "unresolved_source_candidate_ids",
    ]

    static func definitions(
        table: STPDResultTable,
        headers: [String],
        nonNullable: Set<String>
    ) -> [STPDResultColumnDefinition] {
        let nullable = nullableColumns(for: table, headers: headers)
        return headers.map { name in
            STPDResultColumnDefinition(
                name: name,
                type: type(for: name),
                nullable: nullable.contains(name) && !nonNullable.contains(name)
            )
        }
    }

    /// Scientific nullability is table-specific. Columns are required by default; this whitelist
    /// records only values that can be absent under a valid detector or review state.
    private static func nullableColumns(
        for table: STPDResultTable,
        headers: [String]
    ) -> Set<String> {
        switch table {
        case .runMetadata:
            return ["dataset_source"]
        case .parametersReport,
             .candidateLedger,
             .candidateLedgerDiagnostic,
             .resultConsistencyCheck:
            return []
        case .resolvedParameters:
            return [
                "requested_value", "adaptive_value", "effective_value",
                "resolution_note", "histogram_value", "default_value",
            ]
        case .candidateFeatures, .candidateFeaturesDiagnostic:
            let mandatory: Set<String> = [
                "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
                "score",
                "anchor_band_lower_sec", "anchor_band_upper_sec", "anchor_band_source",
                "anchor_band_semantics", "anchor_band_ordered",
                "anchor_contrast_min_required", "anchor_contrast_geom_required",
                "refractory_suspect_count",
                "state_continuity_authority_frozen", "state_continuity_merge_terminal",
            ]
            return Set(headers).subtracting(mandatory)
        case .finalDecisions, .finalDecisionsDiagnostic:
            return [
                "audit_uncertainty_reason", "failure_reason",
                "audit_long_burst_definition_status",
                "state_tonic_subtype", "state_high_frequency_subtype",
            ]
        case .eventsFinal:
            return [
                "final_subtype",
                "state_tonic_subtype",
                "score", "priority",
            ]
        case .isiLabelsFinal:
            return [
                "auto_pattern", "auto_subtype",
                "auto_source_candidate_id", "auto_candidate_uid",
                "final_pattern", "final_subtype",
                "review_note",
                "train_qc_warning_message",
                "train_qc_duration_sec", "train_qc_firing_rate_hz",
                "train_qc_raw_min_isi_sec", "train_qc_min_valid_isi_sec",
                "train_qc_artifact_min_isi_sec", "train_qc_median_isi_sec",
                "train_qc_max_isi_sec", "train_qc_artifact_fraction",
                "train_qc_refractory_suspect_fraction",
            ]
        case .candidateDiagnosticAudit:
            return [
                "event_uid", "automatic_source_id",
                "source_support_isi_indices", "source_semantic_track",
                "source_event_track_class", "source_label", "source_lock_level",
                "source_state_tonic_subtype", "source_score", "source_priority",
                "source_decision_path",
            ]
        case .manualAnnotations:
            return [
                "start_isi_index", "end_isi_index", "note", "annotator",
                "annotator_identity_source",
            ]
        case .reviewStatus:
            return [
                "reviewer", "reviewed_run_id", "note",
                "reviewed_at", "reviewed_at_unix_sec",
            ]
        case .hfsBurstArbitrationAudit:
            return [
                "strongest_burst_candidate_uid", "strongest_long_burst_candidate_uid",
                "audit_hfs_acceptance_route",
                "audit_strongest_burst_candidate_id", "audit_strongest_burst_subtype",
                "audit_strongest_burst_raw_score", "audit_strongest_burst_priority",
                "audit_strongest_long_burst_candidate_id", "audit_long_burst_raw_score",
                "audit_long_burst_priority",
                "audit_pause_like_threshold_sec", "audit_pause_like_break_fraction",
                "audit_seed_band_lower_sec", "audit_seed_band_upper_sec",
                "audit_bridge_band_upper_sec", "audit_seed_fraction",
                "audit_bridge_fraction", "audit_hfs_short_fraction",
                "audit_hfs_bridge_fraction", "audit_hfs_large_fraction",
                "audit_hfs_cv", "audit_hfs_lv",
            ]
        case .taskEvents:
            return ["source"]
        }
    }

    private static func type(for name: String) -> STPDResultColumnType {
        if timestampColumns.contains(name) {
            return .timestamp
        }
        if stringListColumns.contains(name)
            || name.hasSuffix("_uids")
            || name.hasSuffix("_indices") {
            return .stringList
        }
        if booleanColumns.contains(name)
            || name.hasPrefix("is_")
            || name.hasPrefix("has_") {
            return .boolean
        }
        if integerColumns.contains(name)
            || name.hasSuffix("_count")
            || name.hasSuffix("_ordinal")
            || name.hasSuffix("_priority")
            || name.hasSuffix("_index") {
            return .integer
        }
        if realColumns.contains(name)
            || name.hasSuffix("_sec")
            || name.hasSuffix("_ms")
            || name.hasSuffix("_ratio")
            || name.hasSuffix("_fraction")
            || name.hasSuffix("_score")
            || name.hasSuffix("_cv")
            || name.hasSuffix("_cv2")
            || name.hasSuffix("_lv")
            || name.hasSuffix("_q10")
            || name.hasSuffix("_q25")
            || name.hasSuffix("_q40")
            || name.hasSuffix("_q50")
            || name.hasSuffix("_q75")
            || name.hasSuffix("_q80")
            || name.hasSuffix("_q90")
            || name.hasSuffix("_q95") {
            return .real
        }
        return .string
    }
}

public struct STPDResultPackage: Sendable {
    public let identity: DetectionRunIdentity
    public let sourceMode: STPDResultPackageSourceMode
    public let tables: [STPDResultTable: STPDResultTableData]
    public let manifest: STPDResultManifest

    public func table(_ table: STPDResultTable) -> STPDResultTableData? {
        tables[table]
    }
}

public enum STPDResultPackageError: Error, LocalizedError, Sendable {
    case invalidRunIdentity(String)
    case invalidInput(String)
    case invalidTable(table: String, reason: String)
    case duplicatePrimaryKey(table: String, key: String)
    case foreignKeyViolation(table: String, column: String, value: String)
    case missingTable(String)
    case destinationAlreadyExists(String)

    public var errorDescription: String? {
        switch self {
        case .invalidRunIdentity(let reason):
            return "Invalid detector run identity: \(reason)"
        case .invalidInput(let reason):
            return "Invalid result-package input: \(reason)"
        case .invalidTable(let table, let reason):
            return "Invalid result table \(table): \(reason)"
        case .duplicatePrimaryKey(let table, let key):
            return "Duplicate primary key in \(table): \(key)"
        case .foreignKeyViolation(let table, let column, let value):
            return "Foreign-key violation in \(table).\(column): \(value)"
        case .missingTable(let name):
            return "Missing required result table: \(name)"
        case .destinationAlreadyExists(let path):
            return "Result-package destination already exists: \(path)"
        }
    }
}

public enum STPDResultPackageBuilder {
    /// Verifies that an app export still represents the sealed detector invocation.
    ///
    /// The run identity check binds the export to the exact parsed dataset and detector-produced
    /// evidence. The second check compares the current detection-affecting settings with the
    /// invocation snapshot. Display formatting and learned-provenance descriptions are excluded
    /// because they do not alter detector output.
    public static func validateExportPreflight(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        currentSettingsSnapshot: DetectionRunSettingsSnapshot
    ) throws {
        try validateDeclaredIdentity(
            run.runIdentity,
            dataset: dataset,
            run: run
        )
        guard let invocationSnapshot = run.invocationSettingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity(
                "detector entry-point settings evidence is unavailable"
            )
        }

        let invocation = exportSemanticSettings(invocationSnapshot)
        let current = exportSemanticSettings(currentSettingsSnapshot)
        let keys = Set(invocation.keys).union(current.keys).sorted()
        let mismatches = keys.compactMap { key -> String? in
            guard invocation[key] != current[key] else {
                return nil
            }
            return "\(key):invocation=\(invocation[key] ?? "missing"),current=\(current[key] ?? "missing")"
        }
        guard mismatches.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "current detector inputs differ from the sealed run: " +
                    mismatches.joined(separator: "|")
            )
        }
    }

    public static func build(_ input: STPDResultPackageInput) throws -> STPDResultPackage {
        let identity = input.run.runIdentity
        try validateDeclaredIdentity(
            identity,
            dataset: input.dataset,
            run: input.run
        )
        let automaticEvents = input.run.eventAnnotations(
            in: input.dataset,
            tracks: [.event, .gap, .state]
        )
        let automaticRows = ReviewedISIExportBuilder.build(
            dataset: input.dataset,
            autoAnnotations: automaticEvents,
            projectionsByTrain: [:]
        )
        let candidates = try candidateRecords(
            candidates: input.run.candidates,
            datasetDigest: identity.datasetDigest,
            dataset: input.dataset
        )
        let publicCandidates = candidates.filter {
            isPublicCandidate($0.candidate)
        }
        let candidateUIDBySourceID = Dictionary(
            uniqueKeysWithValues: candidates.map { ($0.candidate.id, $0.uid) }
        )
        let candidateBySourceID = Dictionary(
            uniqueKeysWithValues: candidates.map { ($0.candidate.id, $0.candidate) }
        )
        let resolvedManualAnnotations = try resolvedManualAnnotations(
            input.manualAnnotations,
            dataset: input.dataset
        )
        let reviewAuthority = try resolvedReviewAuthority(
            input,
            automaticEvents: automaticEvents,
            automaticRows: automaticRows,
            resolvedManualAnnotations: resolvedManualAnnotations,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID
        )
        try validateSourceMode(input, reviewAuthority: reviewAuthority)
        let isiRows = try normalizedISIRows(
            input.finalISILabelRows,
            dataset: input.dataset,
            authoritativeAutomaticRows: automaticRows
        )
        let isiUIDByKey = Dictionary(
            uniqueKeysWithValues: isiRows.map { row in
                (
                    isiRowKey(row),
                    stableISIUID(
                        datasetDigest: identity.datasetDigest,
                        trainID: row.trainID,
                        isiIndex: row.isiIndex
                    )
                )
            }
        )
        guard isiProjectionFingerprint(isiRows)
                == isiProjectionFingerprint(reviewAuthority.authoritativeRows) else {
            throw STPDResultPackageError.invalidInput(
                "final ISI rows do not equal the causal automatic/manual/review projection"
            )
        }
        guard projectionFingerprint(input.finalEvents)
                == projectionFingerprint(reviewAuthority.projectedEvents) else {
            throw STPDResultPackageError.invalidInput(
                "final events do not equal the causal automatic/manual/review projection"
            )
        }
        let events = try eventRecords(
            automaticEvents: input.finalEvents,
            finalISIRows: isiRows,
            datasetDigest: identity.datasetDigest,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID,
            dataset: input.dataset,
            evidenceUIDsByISIKey: reviewAuthority.evidenceUIDsByISIKey,
            changedISIKeys: reviewAuthority.changedISIKeys
        )
        try validateFinalProjection(
            events: input.finalEvents,
            normalizedEvents: events,
            isiRows: isiRows,
            dataset: input.dataset
        )

        var tables: [STPDResultTable: STPDResultTableData] = [:]
        tables[.runMetadata] = try runMetadataTable(
            input: input,
            candidateCount: publicCandidates.count,
            diagnosticCandidateCount: candidates.count,
            finalEventCount: events.count,
            finalISICount: isiRows.count
        )
        tables[.parametersReport] = try parametersTable(identity: identity)
        tables[.resolvedParameters] = try resolvedParametersTable(
            identity: identity,
            run: input.run,
            dataset: input.dataset
        )
        tables[.candidateLedger] = try candidateLedgerTable(
            identity: identity,
            records: publicCandidates
        )
        tables[.candidateFeatures] = try candidateFeaturesTable(
            identity: identity,
            records: publicCandidates,
            candidateUIDBySourceID: candidateUIDBySourceID
        )
        tables[.finalDecisions] = try finalDecisionsTable(
            identity: identity,
            records: publicCandidates
        )
        tables[.candidateLedgerDiagnostic] = try candidateLedgerTable(
            identity: identity,
            records: candidates,
            tableKind: .candidateLedgerDiagnostic
        )
        tables[.candidateFeaturesDiagnostic] = try candidateFeaturesTable(
            identity: identity,
            records: candidates,
            candidateUIDBySourceID: candidateUIDBySourceID,
            tableKind: .candidateFeaturesDiagnostic
        )
        tables[.finalDecisionsDiagnostic] = try finalDecisionsTable(
            identity: identity,
            records: candidates,
            tableKind: .finalDecisionsDiagnostic
        )
        tables[.eventsFinal] = try eventsTable(
            identity: identity,
            records: events,
            evidenceUIDsByISIKey: reviewAuthority.evidenceUIDsByISIKey,
            changedISIKeys: reviewAuthority.changedISIKeys
        )
        tables[.isiLabelsFinal] = try isiLabelsTable(
            identity: identity,
            rows: isiRows,
            candidateUIDBySourceID: candidateUIDBySourceID,
            evidenceUIDsByISIKey: reviewAuthority.evidenceUIDsByISIKey,
            changedISIKeys: reviewAuthority.changedISIKeys,
            qualitySettings: input.run.qualitySettings,
            dataset: input.dataset
        )
        tables[.candidateDiagnosticAudit] = try diagnosticsTable(
            identity: identity,
            candidates: candidates,
            events: events,
            supplied: input.candidateDiagnostics,
            candidateUIDBySourceID: candidateUIDBySourceID
        )
        tables[.manualAnnotations] = try manualAnnotationsTable(
            identity: identity,
            annotations: resolvedManualAnnotations,
            manualUIDByUUID: reviewAuthority.manualUIDByUUID,
            linkedISIKeysByEvidenceUID: reviewAuthority.linkedISIKeysByEvidenceUID,
            isiUIDByKey: isiUIDByKey
        )
        tables[.reviewStatus] = try reviewStatusTable(
            identity: identity,
            reviews: input.candidateReviews,
            candidateUIDBySourceID: candidateUIDBySourceID,
            reviewUIDBySourceCandidateID: reviewAuthority.reviewUIDBySourceCandidateID,
            linkedISIKeysByEvidenceUID: reviewAuthority.linkedISIKeysByEvidenceUID,
            isiUIDByKey: isiUIDByKey
        )
        tables[.hfsBurstArbitrationAudit] = try hfsAuditTable(
            identity: identity,
            rows: input.run.hfsBurstArbitrationAuditRows,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID,
            dataset: input.dataset,
            finalEvents: events,
            sourceMode: input.sourceMode
        )
        tables[.taskEvents] = try taskEventsTable(
            identity: identity,
            events: input.dataset.taskEvents
        )

        let checks = try STPDResultPackageValidator.validate(
            identity: identity,
            sourceMode: input.sourceMode,
            tables: tables,
            expectedISICount: input.dataset.trains.reduce(0) { $0 + max(0, $1.spikeCount - 1) },
            expectedTaskEvents: input.dataset.taskEvents,
            expectedDatasetMetadata: input.run.datasetMetadataSnapshot,
            expectedTrainIDs: Set(input.dataset.trains.map(\.id)),
            expectedDataset: input.dataset,
            expectedQualitySettings: input.run.qualitySettings,
            expectedRun: input.run,
            expectedCandidateReviews: input.candidateReviews,
            expectedManualAnnotations: input.manualAnnotations,
            expectedCandidateDiagnostics: input.candidateDiagnostics
        )
        tables[.resultConsistencyCheck] = try consistencyTable(
            identity: identity,
            checks: checks
        )

        let completeChecks = try STPDResultPackageValidator.validate(
            identity: identity,
            sourceMode: input.sourceMode,
            tables: tables,
            expectedISICount: input.dataset.trains.reduce(0) { $0 + max(0, $1.spikeCount - 1) },
            expectedTaskEvents: input.dataset.taskEvents,
            expectedDatasetMetadata: input.run.datasetMetadataSnapshot,
            expectedTrainIDs: Set(input.dataset.trains.map(\.id)),
            expectedDataset: input.dataset,
            expectedQualitySettings: input.run.qualitySettings,
            expectedRun: input.run,
            expectedCandidateReviews: input.candidateReviews,
            expectedManualAnnotations: input.manualAnnotations,
            expectedCandidateDiagnostics: input.candidateDiagnostics
        )
        tables[.resultConsistencyCheck] = try consistencyTable(
            identity: identity,
            checks: completeChecks
        )
        _ = try STPDResultPackageValidator.validate(
            identity: identity,
            sourceMode: input.sourceMode,
            tables: tables,
            expectedISICount: input.dataset.trains.reduce(0) { $0 + max(0, $1.spikeCount - 1) },
            expectedTaskEvents: input.dataset.taskEvents,
            expectedDatasetMetadata: input.run.datasetMetadataSnapshot,
            expectedTrainIDs: Set(input.dataset.trains.map(\.id)),
            expectedDataset: input.dataset,
            expectedQualitySettings: input.run.qualitySettings,
            expectedRun: input.run,
            expectedCandidateReviews: input.candidateReviews,
            expectedManualAnnotations: input.manualAnnotations,
            expectedCandidateDiagnostics: input.candidateDiagnostics
        )

        let manifestTables = try STPDResultTable.allCases.map { table in
            guard let data = tables[table] else {
                throw STPDResultPackageError.missingTable(table.rawValue)
            }
            return STPDResultManifestTable(
                fileName: table.rawValue,
                grain: data.contract.grain,
                primaryKey: data.contract.primaryKey,
                columns: data.columnDefinitions,
                rowCount: data.rowCount,
                sha256: STPDStableIdentifier.digest(data.csvData)
            )
        }
        let manifest = STPDResultManifest(
            schemaVersion: identity.resultSchemaVersion,
            detectorVersion: identity.detectorVersion,
            runID: identity.runID,
            datasetDigest: identity.datasetDigest,
            settingsDigest: identity.settingsDigest,
            buildIdentifier: identity.buildCommit,
            buildIdentifierKind: "caller_supplied_unattested",
            buildReproducibilityAttested: false,
            sourceMode: input.sourceMode.rawValue,
            ownerName: STPDResultPackageOwnership.ownerName,
            ownerEmail: STPDResultPackageOwnership.ownerEmail,
            stringListEncoding: "json_array_utf8",
            tables: manifestTables
        )
        return STPDResultPackage(
            identity: identity,
            sourceMode: input.sourceMode,
            tables: tables,
            manifest: manifest
        )
    }
}

private struct STPDCandidateRecord {
    let candidate: ClassicAnchorCandidate
    let uid: String
}

/// Module-internal normalized event contract.
///
/// Kept internal so tests can exercise the same authority-partitioning path used by package
/// materialization without constructing a causally invalid `STPDResultPackageInput`.
struct STPDNormalizedEvent {
    let sourceEventIDs: [String]
    let trainID: String
    let trainName: String
    let finalLabel: String
    let finalSubtype: String
    let stateTonicSubtype: String
    let stateHighFrequencySubtypes: [String]
    let semanticTrack: String
    let eventTrackClass: String
    let lockLevel: String
    let startISIIndex: Int
    let endISIIndex: Int
    /// One-based scientific spike ordinals. These are not zero-based Swift array indices.
    let startSpikeOrdinal: Int
    let endSpikeOrdinal: Int
    let rawStartSec: Double
    let rawEndSec: Double
    let alignedStartSec: Double
    let alignedEndSec: Double
    let score: Double?
    let priority: Int?
    let automaticSupportISIIndices: [Int]
    let auditRecommendedSubtype: String
    let auditReviewStatus: String
    let decisionPath: String
    let authorityOrigin: String
    let automaticEventSources: [ClassicAnchorAutomaticEventSource]
}

struct STPDEventRecord {
    let event: STPDNormalizedEvent
    let uid: String
    let sourceCandidateUIDs: [String]
    let unresolvedSourceCandidateIDs: [String]
}

private struct STPDFinalEventSegment {
    let trainID: String
    let trainName: String
    let finalPattern: String
    let finalSubtype: String
    let startISIIndex: Int
    let endISIIndex: Int
    let evidenceUIDs: [String]
    let rows: [ReviewedISIExportRow]
}

struct STPDConsistencyCheck {
    let id: String
    let status: String
    let severity: String
    let details: String
}

private struct STPDResolvedReviewAuthority {
    let projectedEvents: [ClassicAnchorEventAnnotation]
    let authoritativeRows: [ReviewedISIExportRow]
    let manualUIDByUUID: [UUID: String]
    let reviewUIDBySourceCandidateID: [String: String]
    let evidenceUIDsByISIKey: [String: [String]]
    let linkedISIKeysByEvidenceUID: [String: [String]]
    let changedISIKeys: Set<String>
    let manualPositiveISIKeys: Set<String>
}

private extension STPDResultPackageBuilder {
    static func exportSemanticSettings(
        _ snapshot: DetectionRunSettingsSnapshot
    ) -> [String: String] {
        Dictionary(
            uniqueKeysWithValues: snapshot.entries.compactMap { entry in
                guard entry.key != "quality.display_unit",
                      !entry.key.hasPrefix("manual.learned_provenance.") else {
                    return nil
                }
                return (entry.key, entry.value)
            }
        )
    }

    static func eventProjectionMatches(
        row: ReviewedISIExportRow,
        finalLabel: String,
        finalSubtype: String,
        allowsManualPositive: Bool
    ) -> Bool {
        guard row.finalPattern == finalLabel else {
            return false
        }
        // A same-label manual-positive ISI may remain inside its automatic source event.
        // Its row-level provenance stays manual and never fabricates a detector subtype.
        if row.finalSource == ReviewedISIExportBuilder.sourceManualPositive {
            return allowsManualPositive && row.finalSubtype.isEmpty
        }
        return row.finalSubtype == finalSubtype
    }

    static func validateDeclaredIdentity(
        _ identity: DetectionRunIdentity,
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun
    ) throws {
        guard run.hasValidResultPackageAuthority else {
            throw STPDResultPackageError.invalidRunIdentity(
                "result package requires an internally sealed detector-produced run"
            )
        }
        guard identity.hasCompleteDeclaredRunIdentity else {
            throw STPDResultPackageError.invalidRunIdentity(
                "authoritative run UUID, input digests, settings snapshot, and build identifier are required"
            )
        }
        let actual = DetectionDatasetSnapshot.make(dataset: dataset)
        guard actual.digest == identity.datasetDigest else {
            throw STPDResultPackageError.invalidRunIdentity("dataset digest does not match package dataset")
        }
        guard actual.trainCount == identity.trainCount,
              actual.spikeCount == identity.spikeCount,
              actual.taskEventCount == identity.taskEventCount else {
            throw STPDResultPackageError.invalidRunIdentity("declared dataset counts do not match package dataset")
        }
        guard let metadataSnapshot = run.datasetMetadataSnapshot,
              metadataSnapshot == DetectionDatasetMetadataSnapshot.make(dataset: dataset) else {
            throw STPDResultPackageError.invalidRunIdentity(
                "dataset name/source metadata is not the detector entry-point snapshot"
            )
        }
        guard let snapshot = identity.settingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity("settings snapshot is unavailable")
        }
        guard let invocationSnapshot = run.invocationSettingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity(
                "detector entry-point settings evidence is unavailable"
            )
        }
        guard invocationSnapshot == snapshot,
              invocationSnapshot.digest == identity.settingsDigest else {
            throw STPDResultPackageError.invalidRunIdentity(
                "declared settings snapshot is not the detector entry-point invocation snapshot"
            )
        }
        let declared = Dictionary(uniqueKeysWithValues: snapshot.entries.map { ($0.key, $0.value) })
        let requiredSettings = [
            "band.min_valid_isi_sec":
                STPDCanonicalValue.double(run.bandSettings.minValidISISec),
            "band.histogram_bin_width_sec":
                STPDCanonicalValue.double(run.bandSettings.histogramBinWidthSec),
            "band.dataset_isi_boundary_floor_sec":
                STPDCanonicalValue.double(run.bandSettings.datasetISIBoundaryFloorSec),
            "quality.artifact_threshold_sec":
                STPDCanonicalValue.double(run.qualitySettings.artifactThresholdSec),
            "quality.refractory_suspect_threshold_sec":
                STPDCanonicalValue.double(run.qualitySettings.refractorySuspectThresholdSec),
            "quality.display_unit": run.qualitySettings.displayUnit.rawValue,
        ]
        let mismatchedSettings = requiredSettings.compactMap { key, expected -> String? in
            guard declared[key] == expected else {
                return "\(key):declared=\(declared[key] ?? "missing"),actual=\(expected)"
            }
            return nil
        }
        guard mismatchedSettings.isEmpty else {
            throw STPDResultPackageError.invalidRunIdentity(
                "settings snapshot contradicts the detector run: " +
                    mismatchedSettings.sorted().joined(separator: "|")
            )
        }

        let expectedTrainIDs = Set(dataset.trains.map(\.id))
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })

        let resultIDs = run.results.map(\.trainID)
        let duplicateResultIDs = Dictionary(grouping: resultIDs, by: { $0 })
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateResultIDs.isEmpty,
              run.results.count == dataset.trains.count,
              Set(resultIDs) == expectedTrainIDs else {
            let missing = expectedTrainIDs.subtracting(resultIDs).sorted()
            let extra = Set(resultIDs).subtracting(expectedTrainIDs).sorted()
            throw STPDResultPackageError.invalidInput(
                "detector results must cover every train exactly once; " +
                    "duplicates=\(duplicateResultIDs.joined(separator: "|"));" +
                    "missing=\(missing.joined(separator: "|"));" +
                    "extra=\(extra.joined(separator: "|"))"
            )
        }
        let datasetProfileCandidates = run.results
            .flatMap(\.candidates)
            .filter { $0.trainID == "__dataset__" }
        guard datasetProfileCandidates.count <= 1,
              datasetProfileCandidates.allSatisfy(isValidDatasetProfileCandidate) else {
            throw STPDResultPackageError.invalidInput(
                "dataset-scoped detector evidence must be at most one well-formed structural seed profile"
            )
        }
        for result in run.results {
            guard let train = trainsByID[result.trainID],
                  train.name == result.trainName else {
                throw STPDResultPackageError.invalidInput(
                    "detector result \(result.trainID) has an unknown or mismatched train identity"
                )
            }
            let mismatchedCandidateIDs = result.candidates.compactMap { candidate -> String? in
                (candidate.trainID == result.trainID &&
                    candidate.trainName == result.trainName) ||
                    isValidDatasetProfileCandidate(candidate)
                    ? nil
                    : candidate.id
            }
            guard mismatchedCandidateIDs.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "detector result \(result.trainID) has cross-train or mismatched candidates: " +
                        mismatchedCandidateIDs.sorted().joined(separator: "|")
                )
            }
            let mismatchedAuditIDs =
                result.hfsBurstArbitrationAuditRows.compactMap { row -> String? in
                    row.trainID == result.trainID &&
                        row.trainName == result.trainName
                        ? nil
                        : row.id
                }
            guard mismatchedAuditIDs.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "detector result \(result.trainID) has cross-train or mismatched HFS audit rows: " +
                        mismatchedAuditIDs.sorted().joined(separator: "|")
                )
            }
        }

        let evidenceIDs = run.resolvedThresholdEvidence.map(\.trainID)
        let duplicateEvidenceIDs = Dictionary(grouping: evidenceIDs, by: { $0 })
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateEvidenceIDs.isEmpty,
              run.resolvedThresholdEvidence.count == dataset.trains.count,
              Set(evidenceIDs) == expectedTrainIDs else {
            let missing = expectedTrainIDs.subtracting(evidenceIDs).sorted()
            let extra = Set(evidenceIDs).subtracting(expectedTrainIDs).sorted()
            throw STPDResultPackageError.invalidInput(
                "final resolved-threshold evidence must cover every train exactly once; " +
                    "duplicates=\(duplicateEvidenceIDs.joined(separator: "|"));" +
                    "missing=\(missing.joined(separator: "|"));" +
                    "extra=\(extra.joined(separator: "|"))"
            )
        }
        for evidence in run.resolvedThresholdEvidence {
            guard let train = trainsByID[evidence.trainID],
                  train.name == evidence.trainName,
                  !evidence.stagePath.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "resolved-threshold evidence \(evidence.trainID) has unknown or incomplete provenance"
                )
            }
            try validateResolvedThresholdEvidence(evidence)
        }

        let resolutionIDs = run.resolutions.map(\.trainID)
        let duplicateResolutionIDs = Dictionary(grouping: resolutionIDs, by: { $0 })
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateResolutionIDs.isEmpty,
              run.resolutions.count == dataset.trains.count,
              Set(resolutionIDs) == expectedTrainIDs else {
            let missing = expectedTrainIDs.subtracting(resolutionIDs).sorted()
            let extra = Set(resolutionIDs).subtracting(expectedTrainIDs).sorted()
            throw STPDResultPackageError.invalidInput(
                "adaptive-band resolutions must cover every train exactly once; " +
                    "duplicates=\(duplicateResolutionIDs.joined(separator: "|"));" +
                    "missing=\(missing.joined(separator: "|"));" +
                    "extra=\(extra.joined(separator: "|"))"
            )
        }
        for resolution in run.resolutions {
            guard let train = trainsByID[resolution.trainID],
                  train.name == resolution.trainName else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) has an unknown or mismatched train"
                )
            }
            let expectedValidCount = TrainAdaptiveBandResolver.validISIs(
                train: train,
                minValidISISec: run.bandSettings.minValidISISec
            ).count
            guard canonicalEqual(
                resolution.minValidISISec,
                run.bandSettings.minValidISISec
            ),
            canonicalEqual(
                resolution.histogramBinWidthSec,
                run.bandSettings.histogramBinWidthSec
            ),
            resolution.validISICount == expectedValidCount,
            resolution.seedBandProfile.nValidISI == expectedValidCount,
            canonicalEqual(
                resolution.seedBandProfile.datasetISIBoundaryFloorSec,
                run.bandSettings.datasetISIBoundaryFloorSec
            ) else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) contradicts its run settings or train data"
                )
            }
            try validateAdaptiveBandResolution(resolution)
        }
    }

    static func isValidDatasetProfileCandidate(
        _ candidate: ClassicAnchorCandidate
    ) -> Bool {
        candidate.trainID == "__dataset__" &&
            candidate.trainName == "Dataset structural seed profile" &&
            candidate.candidateLayer == "structural_dataset_seed_profile" &&
            candidate.candidateClass == "dataset_profile" &&
            candidate.finalLabel == .profile &&
            candidate.gateStatus == "profile" &&
            candidate.action == "audit_only" &&
            candidate.selectedForAuto == false &&
            candidate.selectionStatus == "not_selected" &&
            candidate.startISIIndex == 0 &&
            candidate.endISIIndex == 0 &&
            candidate.startSpikeIndex == 0 &&
            candidate.endSpikeIndex == 0 &&
            candidate.nISI == 0 &&
            candidate.anchorFamily == "structural_dataset_seed_profile" &&
            candidate.anchorLockLevel == .auditOnly
    }

    static func validateResolvedThresholdEvidence(
        _ evidence: ClassicAnchorResolvedThresholdEvidence
    ) throws {
        let profile = evidence.effectiveProfile
        let finiteNonnegative: (Double) -> Bool = { $0.isFinite && $0 >= 0 }
        let families = [
            ("burst", profile.burst, false),
            ("hfs", profile.hfs, false),
            ("hf_tonic", profile.hfTonic, false),
            ("tonic", profile.tonic, false),
            ("pause", profile.pause, true),
        ]
        for (name, family, allowsInfiniteUpper) in families {
            guard finiteNonnegative(family.lowerSec),
                  finiteNonnegative(family.upperSec) ||
                    (allowsInfiniteUpper && family.upperSec == .infinity),
                  family.lowerSec <= family.upperSec,
                  family.bridgeUpperSec == nil ||
                    (finiteNonnegative(family.bridgeUpperSec ?? -.infinity) &&
                        (family.bridgeUpperSec ?? -.infinity) >= family.upperSec),
                  family.minDurationSec == nil ||
                    finiteNonnegative(family.minDurationSec ?? -.infinity),
                  [
                      family.minSpikes,
                      family.classicMaxSpikes,
                      family.longMinSpikes,
                      family.longMaxSpikes,
                  ].compactMap({ $0 }).allSatisfy({ $0 >= 0 }) else {
                throw STPDResultPackageError.invalidInput(
                    "resolved-threshold evidence \(evidence.trainID) has invalid \(name) values"
                )
            }
        }
        let duplicateKeys = Dictionary(
            grouping: evidence.resolutionProvenance,
            by: \.key
        )
        .filter { $0.value.count > 1 }
        .keys
        .sorted()
        guard duplicateKeys.isEmpty,
              evidence.resolutionProvenance.allSatisfy({
                  [$0.adaptiveValue, $0.userValue, $0.effectiveValue]
                    .compactMap { $0 }
                    .allSatisfy(finiteNonnegative)
              }) else {
            throw STPDResultPackageError.invalidInput(
                "resolved-threshold evidence \(evidence.trainID) has duplicate or invalid provenance"
            )
        }
    }

    static func validateAdaptiveBandResolution(
        _ resolution: TrainAdaptiveBandResolution
    ) throws {
        let expectedPatterns = Set(AdaptiveBandPattern.allCases)
        guard Set(resolution.bands.keys) == expectedPatterns else {
            throw STPDResultPackageError.invalidInput(
                "adaptive-band resolution \(resolution.trainID) must contain one band per pattern"
            )
        }
        for band in resolution.bands.values {
            let bounds = [
                band.seedLowerSec,
                band.seedUpperSec,
                band.bridgeUpperSec,
            ]
            guard bounds.allSatisfy({ $0.isFinite && $0 >= 0 }),
                  band.contrastS == nil ||
                    (band.contrastS?.isFinite == true && (band.contrastS ?? 0) >= 0),
                  band.seedLowerSec <= band.seedUpperSec,
                  band.seedUpperSec <= band.bridgeUpperSec else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) has invalid \(band.pattern.rawValue) bounds"
                )
            }
        }

        let expectedKeys = Set(
            AdaptiveBandPattern.allCases.flatMap { pattern in
                AdaptiveBandField.allCases.map { field in
                    "\(pattern.rawValue)::\(field.rawValue)"
                }
            }
        )
        let keyedRows = Dictionary(
            grouping: resolution.thresholdRows,
            by: { "\($0.pattern.rawValue)::\($0.field.rawValue)" }
        )
        guard Set(keyedRows.keys) == expectedKeys,
              keyedRows.values.allSatisfy({ $0.count == 1 }) else {
            throw STPDResultPackageError.invalidInput(
                "adaptive-band resolution \(resolution.trainID) must contain one threshold row per pattern and field"
            )
        }
        for threshold in resolution.thresholdRows {
            guard threshold.effectiveSec == nil ||
                    (threshold.effectiveSec?.isFinite == true &&
                        (threshold.effectiveSec ?? 0) >= 0),
                  threshold.histogramSec == nil ||
                    (threshold.histogramSec?.isFinite == true &&
                        (threshold.histogramSec ?? 0) >= 0),
                  threshold.defaultSec == nil ||
                    (threshold.defaultSec?.isFinite == true &&
                        (threshold.defaultSec ?? 0) >= 0),
                  let band = resolution.bands[threshold.pattern] else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) has invalid threshold evidence"
                )
            }
            let expected: Double?
            switch threshold.field {
            case .seedLowerSec:
                expected = band.seedLowerSec
            case .seedUpperSec:
                expected = band.seedUpperSec
            case .bridgeUpperSec:
                expected = band.bridgeUpperSec
            case .contrastS:
                expected = band.contrastS
            }
            guard canonicalEqual(threshold.effectiveSec, expected) else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) threshold rows contradict final bands"
                )
            }
            switch threshold.source {
            case .histogram:
                guard threshold.histogramSec != nil else {
                    throw STPDResultPackageError.invalidInput(
                        "histogram threshold source lacks histogram evidence"
                    )
                }
            case .default:
                guard threshold.defaultSec != nil else {
                    throw STPDResultPackageError.invalidInput(
                        "default threshold source lacks default evidence"
                    )
                }
            case .structure, .none:
                break
            }
        }
        for band in resolution.bands.values {
            let seedUpper = keyedRows[
                "\(band.pattern.rawValue)::\(AdaptiveBandField.seedUpperSec.rawValue)"
            ]?.first
            guard band.primarySource == seedUpper?.source else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) primary source contradicts seed upper provenance"
                )
            }
        }
    }

    static func isPublicCandidate(_ candidate: ClassicAnchorCandidate) -> Bool {
        guard candidate.selectedForAuto,
              candidate.isEligibleForAutoSelection,
              candidate.finalLabel != .reject,
              candidate.finalLabel != .profile else {
            return false
        }

        let authorityFields = [
            candidate.action,
            candidate.gateStatus,
            candidate.selectionStatus,
        ].map {
            $0.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        }
        let denialTerms = [
            "blocked",
            "unwritten",
            "not_selected",
            "not selected",
            "suppressed",
            "rejected",
            "audit_only",
        ]
        return !authorityFields.contains { field in
            denialTerms.contains { field.contains($0) }
        }
    }

    static func candidateRecords(
        candidates: [ClassicAnchorCandidate],
        datasetDigest: String,
        dataset: SpikeDataset
    ) throws -> [STPDCandidateRecord] {
        let duplicateSourceIDs = Dictionary(grouping: candidates, by: \.id)
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateSourceIDs.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "candidate source IDs are not unique: \(duplicateSourceIDs.joined(separator: "|"))"
            )
        }
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        let candidatesByID = Dictionary(uniqueKeysWithValues: candidates.map { ($0.id, $0) })
        for candidate in candidates {
            try validateCandidate(candidate, trainsByID: trainsByID)
            try validateHFSuppressorReference(
                candidate,
                candidatesByID: candidatesByID
            )
        }

        let intrinsicUIDBySourceID = Dictionary(
            uniqueKeysWithValues: candidates.map { candidate in
                (
                    candidate.id,
                    STPDStableIdentifier.make(
                        prefix: "cand_intrinsic",
                        domain: "stpd_candidate_intrinsic_uid_v1",
                        components: [datasetDigest] +
                            candidateIntrinsicIdentityComponents(candidate)
                    )
                )
            }
        )
        let sorted = candidates.sorted {
            let lhs = candidateIdentityComponents(
                $0,
                intrinsicUIDBySourceID: intrinsicUIDBySourceID
            )
            let rhs = candidateIdentityComponents(
                $1,
                intrinsicUIDBySourceID: intrinsicUIDBySourceID
            )
            if lhs != rhs {
                return lhs.lexicographicallyPrecedes(rhs)
            }
            return $0.id < $1.id
        }
        let duplicateScientificIdentities = Dictionary(
            grouping: sorted,
            by: {
                compositeKey(
                    candidateIdentityComponents(
                        $0,
                        intrinsicUIDBySourceID: intrinsicUIDBySourceID
                    )
                )
            }
        )
        .filter { $0.value.count > 1 }
        .values
        .map { $0.map(\.id).sorted().joined(separator: "|") }
        .sorted()
        guard duplicateScientificIdentities.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "scientifically duplicate candidates are ambiguous: " +
                    duplicateScientificIdentities.joined(separator: ",")
            )
        }
        return sorted.map { candidate in
            let base = candidateIdentityComponents(
                candidate,
                intrinsicUIDBySourceID: intrinsicUIDBySourceID
            )
            let uid = STPDStableIdentifier.make(
                prefix: "cand",
                domain: "stpd_candidate_uid_v2",
                components: [datasetDigest] + base
            )
            return STPDCandidateRecord(candidate: candidate, uid: uid)
        }
    }

    static func eventRecords(
        automaticEvents: [ClassicAnchorEventAnnotation],
        finalISIRows: [ReviewedISIExportRow],
        datasetDigest: String,
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate],
        dataset: SpikeDataset,
        evidenceUIDsByISIKey: [String: [String]],
        changedISIKeys: Set<String>
    ) throws -> [STPDEventRecord] {
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        let duplicateEventIDs = Dictionary(grouping: automaticEvents, by: \.id)
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateEventIDs.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "final event source IDs are not unique: \(duplicateEventIDs.joined(separator: "|"))"
            )
        }
        let finalRowsByKey = Dictionary(
            uniqueKeysWithValues: finalISIRows.map { (isiRowKey($0), $0) }
        )
        for event in automaticEvents {
            guard let train = trainsByID[event.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) references unknown train \(event.trainID)"
                )
            }
            try validateEvent(
                event,
                train: train,
                candidateUIDBySourceID: candidateUIDBySourceID,
                candidateBySourceID: candidateBySourceID
            )
        }

        func scientificIdentity(event: STPDNormalizedEvent) -> [String] {
            return [
                event.trainID,
                event.finalLabel,
                event.finalSubtype,
                event.stateTonicSubtype,
                STPDCanonicalValue.stringList(event.stateHighFrequencySubtypes),
                String(event.startISIIndex),
                String(event.endISIIndex),
                String(event.startSpikeOrdinal),
                String(event.endSpikeOrdinal),
                STPDCanonicalValue.double(event.rawStartSec),
                STPDCanonicalValue.double(event.rawEndSec),
                STPDCanonicalValue.double(event.alignedStartSec),
                STPDCanonicalValue.double(event.alignedEndSec),
            ]
        }

        var occupiedISIKeys = Set<String>()
        var records: [STPDEventRecord] = []
        for sourceEvent in automaticEvents {
            guard let train = trainsByID[sourceEvent.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "event \(sourceEvent.id) references unknown train \(sourceEvent.trainID)"
                )
            }
            let sourceRows = try (sourceEvent.startISISecIndex...sourceEvent.endISISecIndex)
                .map { index -> ReviewedISIExportRow in
                    let key = isiEvidenceKey(trainID: sourceEvent.trainID, isiIndex: index)
                    guard let row = finalRowsByKey[key] else {
                        throw STPDResultPackageError.invalidInput(
                            "event \(sourceEvent.id) has no authoritative final ISI row at \(index)"
                        )
                    }
                    return row
                }
            let allSourceIDs = Array(
                Set(
                    sourceEvent.sourceCandidateIDs
                        + [sourceEvent.candidateID]
                        + sourceEvent.automaticEventSources.map(\.candidateID)
                )
            )
            .filter { !$0.isEmpty }
            .sorted()
            for sourceID in allSourceIDs {
                guard candidateUIDBySourceID[sourceID] != nil else {
                    throw STPDResultPackageError.invalidInput(
                        "event \(sourceEvent.id) references unknown candidate \(sourceID)"
                    )
                }
            }
            let allAutomaticSources = sourceEvent.automaticEventSources.sorted {
                let lhsUID = candidateUIDBySourceID[$0.candidateID] ?? ""
                let rhsUID = candidateUIDBySourceID[$1.candidateID] ?? ""
                if lhsUID != rhsUID { return lhsUID < rhsUID }
                if $0.supportISIIndices != $1.supportISIIndices {
                    return $0.supportISIIndices.lexicographicallyPrecedes(
                        $1.supportISIIndices
                    )
                }
                if $0.semanticTrack != $1.semanticTrack {
                    return $0.semanticTrack.rawValue < $1.semanticTrack.rawValue
                }
                return $0.annotationID < $1.annotationID
            }

            // The public event geometry may contain more than one authoritative final family.
            // For example, a manually bridged burst can join automatic `burst` and
            // `possible_burst` support in one projected annotation. The final ISI table remains
            // authoritative, so partition the source event by its row-level final label/subtype
            // rather than discarding rows that do not match the aggregate event label.
            var authoritativeSegments: [(
                rows: [ReviewedISIExportRow],
                finalLabel: String,
                finalSubtype: String
            )] = []
            var currentSegment: [ReviewedISIExportRow] = []

            var currentLabel = ""
            var currentSubtype = ""

            func appendCurrentSegment() {
                guard !currentSegment.isEmpty else { return }
                authoritativeSegments.append((
                    rows: currentSegment,
                    finalLabel: currentLabel,
                    finalSubtype: currentSubtype
                ))
                currentSegment = []
                currentLabel = ""
                currentSubtype = ""
            }

            for row in sourceRows {
                guard !row.finalPattern.isEmpty else {
                    appendCurrentSegment()
                    continue
                }
                let effectiveSubtype: String
                if row.finalSource == ReviewedISIExportBuilder.sourceManualPositive,
                   row.finalPattern == sourceEvent.label.rawValue,
                   row.finalSubtype.isEmpty {
                    // A same-family manual bridge inherits the automatic subtype only for
                    // event continuity. Row-level manual provenance and the empty row subtype
                    // remain unchanged in isi_labels_final.csv.
                    effectiveSubtype = sourceEvent.auditRecommendedSubtype
                } else {
                    effectiveSubtype = row.finalSubtype
                }
                if currentSegment.isEmpty {
                    currentLabel = row.finalPattern
                    currentSubtype = effectiveSubtype
                    currentSegment.append(row)
                } else if row.finalPattern == currentLabel,
                          effectiveSubtype == currentSubtype,
                          row.isiIndex == currentSegment.last!.isiIndex + 1 {
                    currentSegment.append(row)
                } else {
                    appendCurrentSegment()
                    currentLabel = row.finalPattern
                    currentSubtype = effectiveSubtype
                    currentSegment.append(row)
                }
            }
            appendCurrentSegment()

            for segment in authoritativeSegments {
                let eventRows = segment.rows
                guard let firstRow = eventRows.first, let lastRow = eventRows.last else {
                    continue
                }
                for row in eventRows {
                    let key = isiRowKey(row)
                    guard occupiedISIKeys.insert(key).inserted else {
                        throw STPDResultPackageError.invalidInput(
                            "authoritative final events overlap at \(row.trainID):\(row.isiIndex)"
                        )
                    }
                }

                let segmentLower = firstRow.isiIndex
                let segmentUpper = lastRow.isiIndex
                let segmentISIs = Set(segmentLower...segmentUpper)
                let automaticSources = allAutomaticSources.compactMap {
                    $0.retainingSupport(segmentISIs)
                }
                .sorted {
                    let lhsUID = candidateUIDBySourceID[$0.candidateID] ?? ""
                    let rhsUID = candidateUIDBySourceID[$1.candidateID] ?? ""
                    if lhsUID != rhsUID { return lhsUID < rhsUID }
                    if $0.supportISIIndices != $1.supportISIIndices {
                        return $0.supportISIIndices.lexicographicallyPrecedes(
                            $1.supportISIIndices
                        )
                    }
                    if $0.semanticTrack != $1.semanticTrack {
                        return $0.semanticTrack.rawValue < $1.semanticTrack.rawValue
                    }
                    return $0.annotationID < $1.annotationID
                }
                let sourceIDs = allSourceIDs.filter { sourceID in
                    if automaticSources.contains(where: { $0.candidateID == sourceID }) {
                        return true
                    }
                    guard let candidate = candidateBySourceID[sourceID] else {
                        return false
                    }
                    let lower = min(candidate.startISIIndex, candidate.endISIIndex)
                    let upper = max(candidate.startISIIndex, candidate.endISIIndex)
                    return lower <= segmentUpper && upper >= segmentLower
                }
                let sourceUIDs = sourceIDs.compactMap {
                    candidateUIDBySourceID[$0]
                }.sorted()
                let representativeSource = automaticSources.first {
                    $0.label.rawValue == segment.finalLabel
                        && (
                            segment.finalSubtype.isEmpty
                                || $0.auditRecommendedSubtype == segment.finalSubtype
                        )
                }
                let usesAggregateSource = segment.finalLabel == sourceEvent.label.rawValue
                let changed = eventRows.contains {
                    changedISIKeys.contains(isiRowKey($0))
                }
                let authorityOrigin =
                    changed || eventRows.contains {
                        $0.finalSource == ReviewedISIExportBuilder.sourceManualPositive
                    }
                    ? "manual_augmented"
                    : "automatic"
                let hfsSubtypes = Set(
                    sourceIDs.compactMap {
                        candidateBySourceID[$0]?.stateHighFrequencySubtype
                    }
                    .filter { !$0.isEmpty }
                ).sorted()
                let aligned = train.alignedTimestampsSec.count == train.spikeCount
                    ? train.alignedTimestampsSec
                    : train.timestampsSec
                let lowerTimestampIndex = segmentLower - 1
                let upperTimestampIndex = segmentUpper
                let normalized = STPDNormalizedEvent(
                    sourceEventIDs: [sourceEvent.id],
                    trainID: sourceEvent.trainID,
                    trainName: sourceEvent.trainName,
                    finalLabel: segment.finalLabel,
                    finalSubtype: segment.finalSubtype,
                    stateTonicSubtype:
                        representativeSource?.stateTonicSubtype
                        ?? (usesAggregateSource ? sourceEvent.stateTonicSubtype : nil)
                        ?? "",
                    stateHighFrequencySubtypes: hfsSubtypes,
                    semanticTrack:
                        representativeSource?.semanticTrack.rawValue
                        ?? (usesAggregateSource
                            ? sourceEvent.semanticTrack.rawValue
                            : manualSemanticTrack(for: segment.finalLabel)),
                    eventTrackClass:
                        representativeSource?.eventTrackClass
                        ?? (usesAggregateSource
                            ? sourceEvent.eventTrackClass
                            : segment.finalLabel),
                    lockLevel:
                        representativeSource?.lockLevel.rawValue
                        ?? sourceEvent.lockLevel.rawValue,
                    startISIIndex: segmentLower,
                    endISIIndex: segmentUpper,
                    startSpikeOrdinal: segmentLower,
                    endSpikeOrdinal: segmentUpper + 1,
                    rawStartSec: train.timestampsSec[lowerTimestampIndex],
                    rawEndSec: train.timestampsSec[upperTimestampIndex],
                    alignedStartSec: aligned[lowerTimestampIndex],
                    alignedEndSec: aligned[upperTimestampIndex],
                    score: representativeSource?.score ?? sourceEvent.score,
                    priority: representativeSource?.priority ?? sourceEvent.priority,
                    automaticSupportISIIndices:
                        Array(Set(automaticSources.flatMap(\.supportISIIndices))).sorted(),
                    auditRecommendedSubtype:
                        representativeSource?.auditRecommendedSubtype
                        ?? (usesAggregateSource
                            ? sourceEvent.auditRecommendedSubtype
                            : (segment.finalSubtype.isEmpty
                                ? segment.finalLabel
                                : segment.finalSubtype)),
                    auditReviewStatus:
                        representativeSource?.auditReviewStatus
                        ?? sourceEvent.auditReviewStatus,
                    decisionPath:
                        representativeSource?.decisionPath
                        ?? sourceEvent.decisionPath,
                    authorityOrigin: authorityOrigin,
                    automaticEventSources: automaticSources
                )
                let uid = STPDStableIdentifier.make(
                    prefix: "event",
                    domain: "stpd_normalized_public_event_uid_v5",
                    components: [datasetDigest] + scientificIdentity(event: normalized)
                )
                records.append(
                    STPDEventRecord(
                        event: normalized,
                        uid: uid,
                        sourceCandidateUIDs: sourceUIDs,
                        unresolvedSourceCandidateIDs: []
                    )
                )
            }
        }

        let uncoveredRows = finalISIRows.filter {
            $0.isiIndex > 0
                && !$0.finalPattern.isEmpty
                && !occupiedISIKeys.contains(isiRowKey($0))
        }
        let uncoveredAutomaticRows = uncoveredRows.filter {
            $0.finalSource != ReviewedISIExportBuilder.sourceManualPositive
        }
        guard uncoveredAutomaticRows.isEmpty else {
            let summary = uncoveredAutomaticRows
                .prefix(8)
                .map { "\($0.trainID):\($0.isiIndex):\($0.finalSource)" }
                .joined(separator: "|")
            throw STPDResultPackageError.invalidInput(
                "authoritative automatic final ISIs lack automatic event provenance: \(summary)"
            )
        }
        for segment in finalEventSegments(
            uncoveredRows,
            evidenceUIDsByISIKey: evidenceUIDsByISIKey
        ) {
            guard let train = trainsByID[segment.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "manual final event segment references unknown train \(segment.trainID)"
                )
            }
            let aligned = train.alignedTimestampsSec.count == train.spikeCount
                ? train.alignedTimestampsSec
                : train.timestampsSec
            let lowerTimestampIndex = segment.startISIIndex - 1
            let upperTimestampIndex = segment.endISIIndex
            let normalized = STPDNormalizedEvent(
                sourceEventIDs: [],
                trainID: segment.trainID,
                trainName: segment.trainName,
                finalLabel: segment.finalPattern,
                finalSubtype: segment.finalSubtype,
                stateTonicSubtype: segment.finalSubtype,
                stateHighFrequencySubtypes: [],
                semanticTrack: manualSemanticTrack(for: segment.finalPattern),
                eventTrackClass: segment.finalPattern,
                lockLevel: "manual_authority",
                startISIIndex: segment.startISIIndex,
                endISIIndex: segment.endISIIndex,
                startSpikeOrdinal: segment.startISIIndex,
                endSpikeOrdinal: segment.endISIIndex + 1,
                rawStartSec: train.timestampsSec[lowerTimestampIndex],
                rawEndSec: train.timestampsSec[upperTimestampIndex],
                alignedStartSec: aligned[lowerTimestampIndex],
                alignedEndSec: aligned[upperTimestampIndex],
                score: nil,
                priority: nil,
                automaticSupportISIIndices: [],
                auditRecommendedSubtype: "manual_positive",
                auditReviewStatus: "manual",
                decisionPath: "manual_positive_public_projection",
                authorityOrigin: "manual_only",
                automaticEventSources: []
            )
            let uid = STPDStableIdentifier.make(
                prefix: "event",
                domain: "stpd_normalized_public_event_uid_v5",
                components: [datasetDigest] + scientificIdentity(event: normalized)
            )
            records.append(
                STPDEventRecord(
                    event: normalized,
                    uid: uid,
                    sourceCandidateUIDs: [],
                    unresolvedSourceCandidateIDs: []
                )
            )
        }
        let duplicateUIDs = Dictionary(grouping: records, by: \.uid)
            .filter { $0.value.count > 1 }
            .keys
        guard duplicateUIDs.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "scientifically duplicate normalized final events are ambiguous"
            )
        }
        return records.sorted {
            if $0.event.trainID != $1.event.trainID {
                return $0.event.trainID < $1.event.trainID
            }
            if $0.event.startISIIndex != $1.event.startISIIndex {
                return $0.event.startISIIndex < $1.event.startISIIndex
            }
            if $0.event.endISIIndex != $1.event.endISIIndex {
                return $0.event.endISIIndex < $1.event.endISIIndex
            }
            return $0.uid < $1.uid
        }
    }

    static func finalEventSegments(
        _ rows: [ReviewedISIExportRow],
        evidenceUIDsByISIKey: [String: [String]]
    ) -> [STPDFinalEventSegment] {
        func evidenceUIDs(for row: ReviewedISIExportRow) -> [String] {
            Array(Set(evidenceUIDsByISIKey[isiRowKey(row)] ?? [])).sorted()
        }

        let positive = rows
            .filter { $0.isiIndex > 0 && !$0.finalPattern.isEmpty }
            .sorted {
                if $0.trainID != $1.trainID { return $0.trainID < $1.trainID }
                return $0.isiIndex < $1.isiIndex
            }
        var segments: [STPDFinalEventSegment] = []
        var current: [ReviewedISIExportRow] = []

        func appendCurrent() {
            guard let first = current.first, let last = current.last else { return }
            let segmentEvidenceUIDs = Array(
                Set(current.flatMap { evidenceUIDs(for: $0) })
            ).sorted()
            segments.append(
                STPDFinalEventSegment(
                    trainID: first.trainID,
                    trainName: first.trainName,
                    finalPattern: first.finalPattern,
                    finalSubtype: first.finalSubtype,
                    startISIIndex: first.isiIndex,
                    endISIIndex: last.isiIndex,
                    evidenceUIDs: segmentEvidenceUIDs,
                    rows: current
                )
            )
        }

        for row in positive {
            if let previous = current.last,
               previous.trainID == row.trainID,
               previous.finalPattern == row.finalPattern,
               previous.finalSubtype == row.finalSubtype,
               row.isiIndex == previous.isiIndex + 1 {
                current.append(row)
            } else {
                appendCurrent()
                current = [row]
            }
        }
        appendCurrent()
        return segments
    }

    static func manualSemanticTrack(for finalPattern: String) -> String {
        switch finalPattern {
        case ManualAnnotationLabel.burst.rawValue,
             ManualAnnotationLabel.longBurst.rawValue:
            return ClassicAnchorSemanticTrack.event.rawValue
        case ManualAnnotationLabel.pause.rawValue:
            return ClassicAnchorSemanticTrack.gap.rawValue
        default:
            return ClassicAnchorSemanticTrack.state.rawValue
        }
    }

    static func normalizedISIRows(
        _ rows: [ReviewedISIExportRow],
        dataset: SpikeDataset,
        authoritativeAutomaticRows: [ReviewedISIExportRow]
    ) throws -> [ReviewedISIExportRow] {
        guard rows.allSatisfy({ $0.isiIndex >= 0 }) else {
            throw STPDResultPackageError.invalidInput(
                "final ISI table contains a negative ISI index"
            )
        }
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        var seenPlaceholderTrains = Set<String>()
        for row in rows where row.isiIndex == 0 {
            guard let train = trainsByID[row.trainID],
                  train.spikeCount > 0,
                  !train.timestampsSec.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "structural ISI placeholder references an unknown or empty train \(row.trainID)"
                )
            }
            let aligned = train.alignedTimestampsSec.count == train.spikeCount
                ? train.alignedTimestampsSec
                : train.timestampsSec
            guard row.trainName == train.name,
                  row.spikeIndex == 0,
                  row.timestampSec.isFinite,
                  row.alignedTimestampSec.isFinite,
                  row.isiSec.isFinite,
                  canonicalEqual(row.timestampSec, train.timestampsSec[0]),
                  canonicalEqual(row.alignedTimestampSec, aligned[0]),
                  canonicalEqual(row.isiSec, 0),
                  row.autoPattern.isEmpty,
                  row.autoSubtype.isEmpty,
                  row.autoCandidateID.isEmpty,
                  row.finalPattern.isEmpty,
                  row.finalSubtype.isEmpty,
                  row.finalSource == ReviewedISIExportBuilder.sourceNone,
                  row.manualVetoSuppressed == false,
                  row.reviewNote.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "ISI index 0 must be the exact structural first-spike placeholder for train \(row.trainID)"
                )
            }
            guard seenPlaceholderTrains.insert(row.trainID).inserted else {
                throw STPDResultPackageError.invalidInput(
                    "duplicate structural ISI placeholder for train \(row.trainID)"
                )
            }
        }
        let intervalRows = rows.filter { $0.isiIndex > 0 }
        let automaticByKey = Dictionary(
            uniqueKeysWithValues: authoritativeAutomaticRows
                .filter { $0.isiIndex > 0 }
                .map { (isiRowKey($0), $0) }
        )
        var seen = Set<String>()
        for row in intervalRows {
            guard let train = trainsByID[row.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row references unknown train \(row.trainID)"
                )
            }
            guard row.isiIndex >= 1, row.isiIndex < train.spikeCount else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row index \(row.isiIndex) is outside train \(row.trainID)"
                )
            }
            guard row.trainName == train.name else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row train name does not match dataset train \(row.trainID)"
                )
            }
            guard row.spikeIndex == row.isiIndex else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row \(row.trainID):\(row.isiIndex) has inconsistent spike index"
                )
            }
            guard row.timestampSec.isFinite,
                  row.alignedTimestampSec.isFinite,
                  row.isiSec.isFinite else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row \(row.trainID):\(row.isiIndex) has non-finite geometry"
                )
            }
            let aligned = train.alignedTimestampsSec.count == train.spikeCount
                ? train.alignedTimestampsSec
                : train.timestampsSec
            let expectedTimestamp = train.timestampsSec[row.isiIndex]
            let expectedAligned = aligned[row.isiIndex]
            let expectedISI = expectedTimestamp - train.timestampsSec[row.isiIndex - 1]
            guard canonicalEqual(row.timestampSec, expectedTimestamp),
                  canonicalEqual(row.alignedTimestampSec, expectedAligned),
                  canonicalEqual(row.isiSec, expectedISI) else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row \(row.trainID):\(row.isiIndex) geometry differs from dataset"
                )
            }
            let key = isiRowKey(row)
            guard seen.insert(key).inserted else {
                throw STPDResultPackageError.invalidInput("duplicate final ISI row \(key)")
            }
            guard let automatic = automaticByKey[key],
                  row.autoPattern == automatic.autoPattern,
                  row.autoSubtype == automatic.autoSubtype,
                  row.autoCandidateID == automatic.autoCandidateID else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row \(row.trainID):\(row.isiIndex) automatic fields differ from detector projection"
                )
            }
        }
        let expected = dataset.trains.reduce(0) { $0 + max(0, $1.spikeCount - 1) }
        guard intervalRows.count == expected else {
            throw STPDResultPackageError.invalidInput(
                "final ISI table must contain exactly one row per interval; expected \(expected), got \(intervalRows.count)"
            )
        }
        return intervalRows
    }

    static func validateSourceMode(
        _ input: STPDResultPackageInput,
        reviewAuthority: STPDResolvedReviewAuthority
    ) throws {
        switch input.sourceMode {
        case .automatic:
            guard input.reviewLinks.isEmpty,
                  reviewAuthority.evidenceUIDsByISIKey.isEmpty,
                  reviewAuthority.changedISIKeys.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "automatic source mode cannot contain effective manual or review authority"
                )
            }
        case .manual:
            let hasManualAuthority = input.reviewLinks.contains {
                if case .manualAnnotation = $0.evidence { return true }
                return false
            }
            var hasActiveSpikeOnlyAuthority = false
            for annotation in input.manualAnnotations {
                let hasNoISIInterval =
                    annotation.startISIIndex == nil && annotation.endISIIndex == nil
                let spikeArrayIndex = annotation.startSpikeIndex
                let isSingletonSpike =
                    spikeArrayIndex != nil && spikeArrayIndex == annotation.endSpikeIndex
                if hasNoISIInterval && isSingletonSpike {
                    hasActiveSpikeOnlyAuthority = true
                    break
                }
            }
            let hasCandidateReviewAuthority = input.reviewLinks.contains {
                if case .candidateReview = $0.evidence { return true }
                return false
            }
            let hasLinkedManualAuthority =
                hasManualAuthority && !reviewAuthority.evidenceUIDsByISIKey.isEmpty
            guard (hasLinkedManualAuthority || hasActiveSpikeOnlyAuthority),
                  !hasCandidateReviewAuthority else {
                throw STPDResultPackageError.invalidInput(
                    "manual source mode requires manual-only causal authority"
                )
            }
        case .reviewed:
            let hasCandidateReviewAuthority = input.reviewLinks.contains {
                if case .candidateReview = $0.evidence { return true }
                return false
            }
            guard hasCandidateReviewAuthority,
                  !reviewAuthority.evidenceUIDsByISIKey.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "reviewed source mode requires formal candidate-review authority"
                )
            }
        }
    }

    static func resolvedReviewAuthority(
        _ input: STPDResultPackageInput,
        automaticEvents: [ClassicAnchorEventAnnotation],
        automaticRows: [ReviewedISIExportRow],
        resolvedManualAnnotations: [ManualAnnotation],
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate]
    ) throws -> STPDResolvedReviewAuthority {
        for diagnostic in input.candidateDiagnostics
        where candidateUIDBySourceID[diagnostic.sourceCandidateID] == nil {
            throw STPDResultPackageError.invalidInput(
                "candidate diagnostic references unknown candidate \(diagnostic.sourceCandidateID)"
            )
        }
        let annotationByID = Dictionary(
            uniqueKeysWithValues: resolvedManualAnnotations.map { ($0.id, $0) }
        )
        let reviewSourceIDs = input.candidateReviews.map(\.sourceCandidateID)
        guard Set(reviewSourceIDs).count == reviewSourceIDs.count else {
            throw STPDResultPackageError.invalidInput(
                "candidate review source IDs are not unique"
            )
        }
        let reviewBySourceID = Dictionary(
            uniqueKeysWithValues: input.candidateReviews.map {
                ($0.sourceCandidateID, $0)
            }
        )
        for review in input.candidateReviews {
            try STPDResultPackageInput.validateFiniteCandidateReviewTimestamp(review)
            guard candidateUIDBySourceID[review.sourceCandidateID] != nil,
                  candidateBySourceID[review.sourceCandidateID] != nil else {
                throw STPDResultPackageError.invalidInput(
                    "candidate review references unknown candidate \(review.sourceCandidateID)"
                )
            }
            if review.status.grantsReviewAuthority {
                try STPDResultPackageInput.validateAuthorityBearingCandidateReview(
                    review,
                    expectedRunID: input.run.runIdentity.runID
                )
            }
        }

        let manualUIDByUUID = Dictionary(
            uniqueKeysWithValues: resolvedManualAnnotations.map { annotation in
                (
                    annotation.id,
                    STPDStableIdentifier.make(
                        prefix: "manual",
                        domain: "stpd_manual_annotation_uid_v1",
                        components: [
                            input.run.runIdentity.datasetDigest,
                            annotation.id.uuidString.lowercased(),
                        ]
                    )
                )
            }
        )
        let reviewUIDBySourceCandidateID = try Dictionary(
            uniqueKeysWithValues: input.candidateReviews.map { review in
                guard let candidateUID = candidateUIDBySourceID[review.sourceCandidateID] else {
                    throw STPDResultPackageError.invalidInput(
                        "candidate review references unknown candidate \(review.sourceCandidateID)"
                    )
                }
                return (
                    review.sourceCandidateID,
                    STPDCandidateReviewIdentity.make(
                        candidateUID: candidateUID,
                        status: review.status,
                        reviewer: review.reviewer,
                        note: review.note,
                        reviewedAtUnixSec: STPDCanonicalValue.double(
                            review.reviewedAt?.timeIntervalSince1970
                        ),
                        reviewedRunID: review.reviewedRunID
                    )
                )
            }
        )

        let trainsByID = Dictionary(
            uniqueKeysWithValues: input.dataset.trains.map { ($0.id, $0) }
        )
        let automaticIntervalRows = automaticRows.filter { $0.isiIndex > 0 }
        let automaticByKey = Dictionary(
            uniqueKeysWithValues: automaticIntervalRows.map { (isiRowKey($0), $0) }
        )
        let annotationsByTrain = Dictionary(
            grouping: resolvedManualAnnotations,
            by: \.trainID
        )
        let automaticRowsByTrain = Dictionary(
            grouping: automaticIntervalRows,
            by: \.trainID
        )
        var projectionsByTrain: [String: ManualAnnotationProjection] = [:]
        for train in input.dataset.trains {
            let autoLabels = Dictionary(
                uniqueKeysWithValues: (automaticRowsByTrain[train.id] ?? [])
                    .filter { !$0.autoPattern.isEmpty }
                    .map { ($0.isiIndex, $0.autoPattern) }
            )
            projectionsByTrain[train.id] = ManualAnnotationProjector.project(
                train: train,
                autoLabelsByISI: autoLabels,
                annotations: annotationsByTrain[train.id] ?? [],
                honorManualLock: true,
                manualNegativeLabelsEnabled: true,
                minValidISISeconds: input.run.qualitySettings.artifactThresholdSec
            )
        }

        var seenLinks = Set<STPDResultReviewLink>()
        for link in input.reviewLinks {
            guard seenLinks.insert(link).inserted else {
                throw STPDResultPackageError.invalidInput(
                    "duplicate review link \(link.trainID):\(link.isiIndex)"
                )
            }
            let key = "\(link.trainID)\u{1}\(link.isiIndex)"
            guard let automaticRow = automaticByKey[key] else {
                throw STPDResultPackageError.invalidInput(
                    "review link targets an unknown dataset ISI \(link.trainID):\(link.isiIndex)"
                )
            }
            switch link.evidence {
            case .manualAnnotation(let annotationID):
                guard annotationByID[annotationID]?.trainID == link.trainID else {
                    throw STPDResultPackageError.invalidInput(
                        "manual review link references an unknown annotation or train"
                    )
                }
            case .candidateReview(let sourceCandidateID):
                guard let review = reviewBySourceID[sourceCandidateID],
                      review.status.grantsReviewAuthority,
                      let candidate = candidateBySourceID[sourceCandidateID],
                      isPublicCandidate(candidate),
                      candidate.trainID == link.trainID else {
                    throw STPDResultPackageError.invalidInput(
                        "candidate review link does not match automatic candidate coverage"
                    )
                }
                if review.status != .rejected {
                    guard min(candidate.startISIIndex, candidate.endISIIndex)
                            ... max(candidate.startISIIndex, candidate.endISIIndex)
                            ~= link.isiIndex,
                          automaticRow.autoCandidateID == sourceCandidateID else {
                        throw STPDResultPackageError.invalidInput(
                            "candidate review link does not match automatic candidate coverage"
                        )
                    }
                }
                try STPDResultPackageInput.validateAuthorityBearingCandidateReview(
                    review,
                    expectedRunID: input.run.runIdentity.runID
                )
            }
        }
        let rejectedCandidateIDs = Set(input.candidateReviews.compactMap {
            review -> String? in
            guard review.status == .rejected,
                  let candidate = candidateBySourceID[review.sourceCandidateID],
                  isPublicCandidate(candidate),
                  automaticIntervalRows.contains(where: {
                      $0.autoCandidateID == review.sourceCandidateID
                          && $0.finalSource
                              == ReviewedISIExportBuilder.sourceAutoProjected
                  }) else {
                return nil
            }
            return review.sourceCandidateID
        })
        let authoritativeRows = ReviewedISIExportBuilder.build(
            dataset: input.dataset,
            autoAnnotations: automaticEvents,
            projectionsByTrain: projectionsByTrain,
            reviewRejectedCandidateIDs: rejectedCandidateIDs
        )
        .filter { $0.isiIndex > 0 }
        let authoritativeByKey = Dictionary(
            uniqueKeysWithValues: authoritativeRows.map { (isiRowKey($0), $0) }
        )
        var rejectionCausalKeysBySourceID: [String: Set<String>] = [:]
        for sourceCandidateID in rejectedCandidateIDs {
            let hasPublicBaseline = automaticIntervalRows.contains {
                $0.autoCandidateID == sourceCandidateID
                    && $0.finalSource
                        == ReviewedISIExportBuilder.sourceAutoProjected
            }
            guard hasPublicBaseline else {
                rejectionCausalKeysBySourceID[sourceCandidateID] = []
                continue
            }
            let counterfactualRows = ReviewedISIExportBuilder.build(
                dataset: input.dataset,
                autoAnnotations: automaticEvents,
                projectionsByTrain: projectionsByTrain,
                reviewRejectedCandidateIDs:
                    rejectedCandidateIDs.subtracting([sourceCandidateID])
            )
            .filter { $0.isiIndex > 0 }
            let counterfactualByKey = Dictionary(
                uniqueKeysWithValues: counterfactualRows.map {
                    (isiRowKey($0), $0)
                }
            )
            rejectionCausalKeysBySourceID[sourceCandidateID] = Set(
                authoritativeRows.compactMap { actual -> String? in
                    let key = isiRowKey(actual)
                    guard let counterfactual = counterfactualByKey[key],
                          isiProjectionComponents(actual)
                            != isiProjectionComponents(counterfactual) else {
                        return nil
                    }
                    return key
                }
            )
        }
        let manualEvidenceOwnersByISIKey =
            STPDResultPackageInput.manualEvidenceOwnerMap(
                dataset: input.dataset,
                run: input.run,
                automaticEvents: automaticEvents,
                automaticRows: automaticRows,
                resolvedAnnotations: resolvedManualAnnotations,
                projectionsByTrain: projectionsByTrain,
                finalRows: authoritativeRows,
                reviewRejectedCandidateIDs: rejectedCandidateIDs
            )

        let rejectedISIsByTrain = Dictionary(
            grouping: automaticIntervalRows.filter {
                rejectedCandidateIDs.contains($0.autoCandidateID)
            },
            by: \.trainID
        )
        .mapValues { Set($0.map(\.isiIndex)) }
        var lockSuppressedByTrain: [String: Set<Int>] = [:]
        var vetoedBurstISIsByTrain: [String: Set<Int>] = [:]
        var validatedManualBurstISIsByTrain: [String: Set<Int>] = [:]
        for train in input.dataset.trains {
            let projection = projectionsByTrain[train.id]
            lockSuppressedByTrain[train.id] =
                STPDResultPackageInput.eventProjectionLockSuppressedISIs(
                    projection: projection,
                    automaticRows: automaticRowsByTrain[train.id] ?? [],
                    rejectedISIs: rejectedISIsByTrain[train.id] ?? []
                )
            vetoedBurstISIsByTrain[train.id] =
                projection?.autoBurstBlockedByVetoISIs ?? []
            validatedManualBurstISIsByTrain[train.id] = Set(
                (projection?.manualPositiveLabelByISI ?? [:]).compactMap {
                    ManualAnnotationProjector.burstFamilyLabels.contains($0.value)
                        ? $0.key
                        : nil
                }
            )
        }
        let projectedEvents = ManualAnnotationProjector.projectPublicEventAnnotations(
            automaticEvents,
            vetoedBurstISIsByTrain: vetoedBurstISIsByTrain,
            lockSuppressedISIsByTrain: lockSuppressedByTrain,
            validatedManualBurstSupportISIsByTrain: validatedManualBurstISIsByTrain,
            trainsByID: trainsByID
        ).annotations

        var evidenceUIDsByISIKey: [String: Set<String>] = [:]
        var linkedISIKeysByEvidenceUID: [String: Set<String>] = [:]
        for link in input.reviewLinks {
            let key = "\(link.trainID)\u{1}\(link.isiIndex)"
            guard let automaticRow = automaticByKey[key],
                  authoritativeByKey[key] != nil else {
                throw STPDResultPackageError.invalidInput(
                    "review link targets an unknown dataset ISI \(link.trainID):\(link.isiIndex)"
                )
            }
            let evidenceUID: String
            switch link.evidence {
            case .manualAnnotation(let annotationID):
                guard let annotation = annotationByID[annotationID],
                      annotation.trainID == link.trainID,
                      manualEvidenceOwnersByISIKey[key]?.contains(annotationID) == true,
                      let uid = manualUIDByUUID[annotationID] else {
                    throw STPDResultPackageError.invalidInput(
                        "manual review link does not match a causal annotation owner"
                    )
                }
                evidenceUID = uid
            case .candidateReview(let sourceCandidateID):
                guard let review = reviewBySourceID[sourceCandidateID],
                      review.status.grantsReviewAuthority,
                      let candidate = candidateBySourceID[sourceCandidateID],
                      isPublicCandidate(candidate),
                      candidate.trainID == link.trainID,
                      let uid = reviewUIDBySourceCandidateID[sourceCandidateID] else {
                    throw STPDResultPackageError.invalidInput(
                        "candidate review link does not match projected candidate coverage"
                    )
                }
                if review.status == .rejected {
                    guard rejectionCausalKeysBySourceID[sourceCandidateID]?
                            .contains(key) == true else {
                        throw STPDResultPackageError.invalidInput(
                            "candidate rejection link is not causally attributable"
                        )
                    }
                } else {
                    guard min(candidate.startISIIndex, candidate.endISIIndex)
                            ... max(candidate.startISIIndex, candidate.endISIIndex)
                            ~= link.isiIndex,
                          automaticRow.autoCandidateID == sourceCandidateID else {
                        throw STPDResultPackageError.invalidInput(
                            "candidate review link does not match projected candidate coverage"
                        )
                    }
                }
                evidenceUID = uid
            }
            evidenceUIDsByISIKey[key, default: []].insert(evidenceUID)
            linkedISIKeysByEvidenceUID[evidenceUID, default: []].insert(key)
        }

        let manualPositiveISIKeys = Set(authoritativeRows.compactMap { row in
            row.finalSource == ReviewedISIExportBuilder.sourceManualPositive
                ? isiRowKey(row)
                : nil
        })
        for annotation in resolvedManualAnnotations {
            guard let uid = manualUIDByUUID[annotation.id] else { continue }
            let linked = linkedISIKeysByEvidenceUID[uid] ?? []
            let expected = Set(manualEvidenceOwnersByISIKey.compactMap {
                key, owners -> String? in
                owners.contains(annotation.id) ? key : nil
            })
            guard linked == expected else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(annotation.id.uuidString) must link exactly to the ISIs it changes"
                )
            }
        }

        for review in input.candidateReviews {
            guard let uid = reviewUIDBySourceCandidateID[review.sourceCandidateID] else {
                continue
            }
            let linked = linkedISIKeysByEvidenceUID[uid] ?? []
            let publicCandidate = candidateBySourceID[review.sourceCandidateID]
                .map(isPublicCandidate) ?? false
            let candidateRows = automaticIntervalRows.filter {
                $0.autoCandidateID == review.sourceCandidateID
            }
            let candidateKeys = Set(candidateRows.map(isiRowKey))
            let changedKeys = Set(candidateRows.compactMap { automatic -> String? in
                let key = isiRowKey(automatic)
                guard let reviewed = authoritativeByKey[key],
                      isiProjectionComponents(automatic)
                        != isiProjectionComponents(reviewed) else {
                    return nil
                }
                return key
            })
            let expected: Set<String>
            switch review.status {
            case .accepted:
                let candidateIsWhollyUnchanged = publicCandidate
                    && !candidateRows.isEmpty
                    && candidateRows.allSatisfy { automatic in
                        let key = isiRowKey(automatic)
                        guard let reviewed = authoritativeByKey[key] else {
                            return false
                        }
                        return reviewed.finalSource
                                == ReviewedISIExportBuilder.sourceAutoProjected
                            && isiProjectionComponents(automatic)
                                == isiProjectionComponents(reviewed)
                    }
                expected = candidateIsWhollyUnchanged ? candidateKeys : []
            case .rejected:
                expected = publicCandidate
                    ? rejectionCausalKeysBySourceID[
                        review.sourceCandidateID
                    ] ?? []
                    : []
            case .modified:
                let allChangesHaveManualEvidence = publicCandidate
                    && !changedKeys.isEmpty
                    && changedKeys.allSatisfy {
                        !(manualEvidenceOwnersByISIKey[$0] ?? []).isEmpty
                    }
                expected = allChangesHaveManualEvidence ? changedKeys : []
            case .needsReview:
                expected = []
            }
            guard linked == expected else {
                throw STPDResultPackageError.invalidInput(
                    "candidate review \(review.sourceCandidateID) must link exactly to its causal public ISI coverage"
                )
            }
        }

        let normalizedEvidence = evidenceUIDsByISIKey.mapValues { $0.sorted() }
        let normalizedLinks = linkedISIKeysByEvidenceUID.mapValues { $0.sorted() }
        let changedISIKeys = Set(authoritativeRows.compactMap { reviewed -> String? in
            let key = isiRowKey(reviewed)
            guard let automatic = automaticByKey[key],
                  isiProjectionComponents(automatic)
                    != isiProjectionComponents(reviewed) else {
                return nil
            }
            return key
        })
        return STPDResolvedReviewAuthority(
            projectedEvents: projectedEvents,
            authoritativeRows: authoritativeRows,
            manualUIDByUUID: manualUIDByUUID,
            reviewUIDBySourceCandidateID: reviewUIDBySourceCandidateID,
            evidenceUIDsByISIKey: normalizedEvidence,
            linkedISIKeysByEvidenceUID: normalizedLinks,
            changedISIKeys: changedISIKeys,
            manualPositiveISIKeys: manualPositiveISIKeys
        )
    }

    static func resolvedManualAnnotations(
        _ annotations: [ManualAnnotation],
        dataset: SpikeDataset
    ) throws -> [ManualAnnotation] {
        try STPDResultPackageInput.validateFiniteManualAnnotationTimestamps(
            annotations
        )
        let ids = annotations.map(\.id)
        guard Set(ids).count == ids.count else {
            throw STPDResultPackageError.invalidInput("manual annotation UUIDs are not unique")
        }
        let resolved = try annotations.map { annotation in
            guard annotation.startSec.isFinite, annotation.endSec.isFinite else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(annotation.id.uuidString) has non-finite time geometry"
                )
            }
            guard let resolved = ManualAnnotationGeometryResolver
                .resolvingIndicesIfCompatible(annotation, in: dataset.trains) else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(annotation.id.uuidString) is incompatible with the dataset"
                )
            }
            return resolved
        }
        try STPDResultPackageInput.validateNoAmbiguousManualEditTies(resolved)
        return resolved
    }

    static func validateFinalProjection(
        events: [ClassicAnchorEventAnnotation],
        normalizedEvents: [STPDEventRecord],
        isiRows: [ReviewedISIExportRow],
        dataset: SpikeDataset
    ) throws {
        _ = ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: events,
            projectionsByTrain: [:]
        )
        for row in isiRows where row.isiIndex > 0 {
            let covering = normalizedEvents.filter { record in
                let event = record.event
                return event.trainID == row.trainID
                    && event.startISIIndex <= row.isiIndex
                    && row.isiIndex <= event.endISIIndex
            }
            if row.finalPattern.isEmpty {
                guard covering.isEmpty else {
                    throw STPDResultPackageError.invalidInput(
                        "unlabeled final ISI row \(row.trainID):\(row.isiIndex) " +
                            "is covered by a normalized final event"
                    )
                }
                continue
            }
            guard covering.count == 1,
                  eventProjectionMatches(
                      row: row,
                      finalLabel: covering[0].event.finalLabel,
                      finalSubtype: covering[0].event.finalSubtype,
                      allowsManualPositive: true
                  ) else {
                throw STPDResultPackageError.invalidInput(
                    "final ISI row \(row.trainID):\(row.isiIndex) is not represented " +
                        "exactly once by the normalized final-event projection"
                )
            }
        }
    }

    static func validateCandidate(
        _ candidate: ClassicAnchorCandidate,
        trainsByID: [String: SpikeTrain]
    ) throws {
        try validateDirectFiniteDoubles(
            candidate,
            entity: "candidate \(candidate.id)"
        )
        let isProfile = candidate.finalLabel == .profile
        var invalidFields: [String] = []
        if candidate.anchorBandLowerSec < 0 {
            invalidFields.append("anchorBandLowerSec=\(candidate.anchorBandLowerSec)")
        }
        if candidate.anchorBandUpperSec < 0 {
            invalidFields.append("anchorBandUpperSec=\(candidate.anchorBandUpperSec)")
        }
        if !isProfile && candidate.anchorBandUpperSec < candidate.anchorBandLowerSec {
            invalidFields.append(
                "anchorBandUpperSec=\(candidate.anchorBandUpperSec)<anchorBandLowerSec=\(candidate.anchorBandLowerSec)"
            )
        }
        if candidate.anchorContrastMinRequired < 0 {
            invalidFields.append("anchorContrastMinRequired")
        }
        if candidate.anchorContrastGeomRequired < 0 {
            invalidFields.append("anchorContrastGeomRequired")
        }
        if candidate.priority < 0 {
            invalidFields.append("priority")
        }
        if candidate.nISI < 0 {
            invalidFields.append("nISI")
        }
        if candidate.nValidISI < 0 || (!isProfile && candidate.nValidISI > candidate.nISI) {
            invalidFields.append("nValidISI")
        }
        if candidate.nSpikes < 0 {
            invalidFields.append("nSpikes")
        }
        if candidate.refractorySuspectCount < 0 {
            invalidFields.append("refractorySuspectCount")
        }
        let nonnegativeDoubles: [(String, Double?)] = [
            ("durationSec", candidate.durationSec),
            ("intraQ10Sec", candidate.intraQ10Sec),
            ("intraQ40Sec", candidate.intraQ40Sec),
            ("intraQ50Sec", candidate.intraQ50Sec),
            ("intraQ90Sec", candidate.intraQ90Sec),
            ("intraQ95Sec", candidate.intraQ95Sec),
            ("maxIntraISISec", candidate.maxIntraISISec),
            ("meanIntraISISec", candidate.meanIntraISISec),
            ("cv", candidate.cv),
            ("cv2", candidate.cv2),
            ("lv", candidate.lv),
            ("preGapSec", candidate.preGapSec),
            ("postGapSec", candidate.postGapSec),
            ("preRatioQ90", candidate.preRatioQ90),
            ("postRatioQ90", candidate.postRatioQ90),
            ("edgeContrastMinQ90", candidate.edgeContrastMinQ90),
            ("edgeContrastGeomQ90", candidate.edgeContrastGeomQ90),
            ("profileMedianISISec", candidate.profileMedianISISec),
            ("profileQ10ISISec", candidate.profileQ10ISISec),
            ("profileQ25ISISec", candidate.profileQ25ISISec),
            ("profileQ90ISISec", candidate.profileQ90ISISec),
            ("profileBridgeUpperSec", candidate.profileBridgeUpperSec),
            ("profileBoundaryFloorSec", candidate.profileBoundaryFloorSec),
            ("profileBurstContrastS", candidate.profileBurstContrastS),
            ("profilePossibleContrastS", candidate.profilePossibleContrastS),
            ("hfSpikingQ80Sec", candidate.hfSpikingQ80Sec),
            ("hfSpikingQ80MaxSec", candidate.hfSpikingQ80MaxSec),
            ("hfSpikingQ90MaxSec", candidate.hfSpikingQ90MaxSec),
            ("hfSpikingShortUpperSec", candidate.hfSpikingShortUpperSec),
            ("hfSpikingEpochBridgeSec", candidate.hfSpikingEpochBridgeSec),
            ("hfSpikingToleratedGapSec", candidate.hfSpikingToleratedGapSec),
            ("hfSpikingPatternMaxISISec", candidate.hfSpikingPatternMaxISISec),
            ("hfSpikingPauseBreakSec", candidate.hfSpikingPauseBreakSec),
            ("stateTrainPercentileMedian", candidate.stateTrainPercentileMedian),
            ("stateLocalPercentileMedian", candidate.stateLocalPercentileMedian),
            ("stateLocalPercentileQ90", candidate.stateLocalPercentileQ90),
            ("stateLocalRobustZAbsQ80", candidate.stateLocalRobustZAbsQ80),
            ("burstSeedBandLowerSec", candidate.burstSeedBandLowerSec),
            ("burstSeedBandUpperSec", candidate.burstSeedBandUpperSec),
            ("burstBridgeBandUpperSec", candidate.burstBridgeBandUpperSec),
            ("burstContrastRequired", candidate.burstContrastRequired),
            ("burstPossibleContrastRequired", candidate.burstPossibleContrastRequired),
            ("burstRequiredGapSec", candidate.burstRequiredGapSec),
            ("burstPossibleRequiredGapSec", candidate.burstPossibleRequiredGapSec),
            ("burstBoundaryFloorSec", candidate.burstBoundaryFloorSec),
            ("hardBurstSeedUpperSec", candidate.hardBurstSeedUpperSec),
            ("hardBurstBridgeUpperSec", candidate.hardBurstBridgeUpperSec),
            ("localBackgroundQ75Sec", candidate.localBackgroundQ75Sec),
            ("localCompressionQ90Ratio", candidate.localCompressionQ90Ratio),
            ("eventLocalMedianSec", candidate.eventLocalMedianSec),
            ("eventLocalPercentileMedian", candidate.eventLocalPercentileMedian),
            ("eventLocalPercentileQ90", candidate.eventLocalPercentileQ90),
            ("eventLocalRobustZAbsQ80", candidate.eventLocalRobustZAbsQ80),
        ]
        invalidFields.append(contentsOf: nonnegativeDoubles.compactMap { name, value in
            guard let value, value < 0 else {
                return nil
            }
            return name
        })
        let fractions: [(String, Double?)] = [
            ("profileSeedBandFraction", candidate.profileSeedBandFraction),
            ("profilePauseFraction", candidate.profilePauseFraction),
            ("hfSpikingShortFraction", candidate.hfSpikingShortFraction),
            ("hfSpikingQ90ShortFraction", candidate.hfSpikingQ90ShortFraction),
            ("hfSpikingBridgeFraction", candidate.hfSpikingBridgeFraction),
            ("hfSpikingLargeFraction", candidate.hfSpikingLargeFraction),
            ("hfSpikingToleratedFraction", candidate.hfSpikingToleratedFraction),
            ("hfSpikingEmbeddedBurstCoverage", candidate.hfSpikingEmbeddedBurstCoverage),
            ("stateRegularityScore", candidate.stateRegularityScore),
            ("stateBurstSeedFraction", candidate.stateBurstSeedFraction),
            ("stateLowTailFraction", candidate.stateLowTailFraction),
            ("stateLocalStabilityScore", candidate.stateLocalStabilityScore),
        ]
        invalidFields.append(contentsOf: fractions.compactMap { name, value in
            guard let value, value < 0 || value > 1 else {
                return nil
            }
            return name
        })
        let profilePercentiles: [(String, Double?)] = [
            ("profileSeedLowPercentileInTrain", candidate.profileSeedLowPercentileInTrain),
            ("profileSeedHighPercentileInTrain", candidate.profileSeedHighPercentileInTrain),
        ]
        invalidFields.append(contentsOf: profilePercentiles.compactMap { name, value in
            guard let value, value < 0 || value > 100 else {
                return nil
            }
            return name
        })
        if let low = candidate.profileSeedLowPercentileInTrain,
           let high = candidate.profileSeedHighPercentileInTrain,
           low > high {
            invalidFields.append("profileSeedPercentileOrder")
        }
        let unitPercentiles: [(String, Double?)] = [
            ("stateTrainPercentileMedian", candidate.stateTrainPercentileMedian),
            ("stateLocalPercentileMedian", candidate.stateLocalPercentileMedian),
            ("stateLocalPercentileQ90", candidate.stateLocalPercentileQ90),
            ("eventLocalPercentileMedian", candidate.eventLocalPercentileMedian),
            ("eventLocalPercentileQ90", candidate.eventLocalPercentileQ90),
        ]
        invalidFields.append(contentsOf: unitPercentiles.compactMap { name, value in
            guard let value, value < 0 || value > 1 else {
                return nil
            }
            return name
        })
        if let median = candidate.stateLocalPercentileMedian,
           let q90 = candidate.stateLocalPercentileQ90,
           median > q90 {
            invalidFields.append("stateLocalPercentileOrder")
        }
        if let median = candidate.eventLocalPercentileMedian,
           let q90 = candidate.eventLocalPercentileQ90,
           median > q90 {
            invalidFields.append("eventLocalPercentileOrder")
        }
        let optionalCounts: [(String, Int?)] = [
            ("profileSeedRunCount", candidate.profileSeedRunCount),
            ("profileMaxSeedRunLength", candidate.profileMaxSeedRunLength),
            ("hfSpikingMaxConsecutiveLargeISI", candidate.hfSpikingMaxConsecutiveLargeISI),
            ("hfSpikingMinSpikesRequired", candidate.hfSpikingMinSpikesRequired),
            ("hfSpikingEmbeddedBurstCount", candidate.hfSpikingEmbeddedBurstCount),
            ("hfSpikingEmbeddedBurstGroupCount", candidate.hfSpikingEmbeddedBurstGroupCount),
            ("stateCoreBurstRunLength", candidate.stateCoreBurstRunLength),
            ("hardBurstCoreISICount", candidate.hardBurstCoreISICount),
        ]
        invalidFields.append(contentsOf: optionalCounts.compactMap { name, value in
            guard let value, value < 0 else {
                return nil
            }
            return name
        })
        if !isProfile {
            let orderedQuantiles = [
                candidate.intraQ10Sec,
                candidate.intraQ40Sec,
                candidate.intraQ50Sec,
                candidate.intraQ90Sec,
                candidate.intraQ95Sec,
            ].compactMap { $0 }
            if zip(orderedQuantiles, orderedQuantiles.dropFirst()).contains(where: { $0 > $1 }) {
                invalidFields.append("intraQuantileOrder")
            }
            if let maximum = candidate.maxIntraISISec,
               orderedQuantiles.contains(where: { $0 > maximum }) {
                invalidFields.append("maxIntraISISec")
            }
        }
        guard invalidFields.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) has invalid fields: \(invalidFields.joined(separator: ","))"
            )
        }
        guard candidate.startISIIndex <= candidate.endISIIndex,
              candidate.startSpikeIndex <= candidate.endSpikeIndex else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) has inverted geometry"
            )
        }
        if isProfile {
            let validProfileClasses = Set(["train_profile", "dataset_profile"])
            guard validProfileClasses.contains(candidate.candidateClass),
                  candidate.startISIIndex == 0,
                  candidate.endISIIndex == 0,
                  candidate.startSpikeIndex == 0,
                  candidate.endSpikeIndex == 0,
                  candidate.nISI == 0 else {
                throw STPDResultPackageError.invalidInput(
                    "profile candidate \(candidate.id) has invalid class or sentinel geometry"
                )
            }
            if candidate.trainID == "__dataset__" {
                let totalSpikes = trainsByID.values.reduce(0) { $0 + $1.spikeCount }
                let totalIntervals = trainsByID.values.reduce(0) {
                    $0 + max(0, $1.spikeCount - 1)
                }
                guard candidate.candidateClass == "dataset_profile",
                      candidate.nSpikes == totalSpikes,
                      candidate.nValidISI <= totalIntervals else {
                    throw STPDResultPackageError.invalidInput(
                        "dataset profile \(candidate.id) has inconsistent aggregate counts"
                    )
                }
            } else {
                guard candidate.candidateClass == "train_profile",
                      let train = trainsByID[candidate.trainID],
                      candidate.trainName == train.name,
                      candidate.nSpikes == train.spikeCount,
                      candidate.nValidISI <= max(0, train.spikeCount - 1) else {
                    throw STPDResultPackageError.invalidInput(
                        "train profile \(candidate.id) has inconsistent train counts"
                    )
                }
            }
            return
        }
        guard candidate.trainID != "__dataset__" else {
            throw STPDResultPackageError.invalidInput(
                "non-profile candidate \(candidate.id) cannot use dataset sentinel ownership"
            )
        }
        guard let train = trainsByID[candidate.trainID],
              candidate.trainName == train.name else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) references an unknown or mismatched train"
            )
        }
        guard candidate.startISIIndex >= 1,
              candidate.endISIIndex < train.spikeCount,
              candidate.startSpikeIndex >= 1,
              candidate.endSpikeIndex <= train.spikeCount,
              candidate.startSpikeIndex == candidate.startISIIndex,
              candidate.endSpikeIndex == candidate.endISIIndex + 1,
              candidate.nISI == candidate.endISIIndex - candidate.startISIIndex + 1,
              candidate.nSpikes == candidate.endSpikeIndex - candidate.startSpikeIndex + 1,
              candidate.nSpikes == candidate.nISI + 1,
              candidate.refractorySuspectCount <= candidate.nISI else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) geometry or counts are inconsistent with its train"
            )
        }
        if let duration = candidate.durationSec {
            let expectedDuration =
                train.timestampsSec[candidate.endISIIndex] -
                train.timestampsSec[candidate.startISIIndex - 1]
            let tolerance = max(1e-12, abs(expectedDuration) * 1e-9)
            guard expectedDuration.isFinite,
                  abs(duration - expectedDuration) <= tolerance else {
                throw STPDResultPackageError.invalidInput(
                    "candidate \(candidate.id) duration contradicts its dataset span"
                )
            }
        }
        switch (candidate.burstSeedRunStartISI, candidate.burstSeedRunEndISI) {
        case (nil, nil):
            break
        case let (start?, end?):
            guard start >= candidate.startISIIndex,
                  start <= end,
                  end <= candidate.endISIIndex else {
                throw STPDResultPackageError.invalidInput(
                    "candidate \(candidate.id) burst seed run is outside its span"
                )
            }
        default:
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) has incomplete burst seed-run geometry"
            )
        }
    }

    static func validateHFSuppressorReference(
        _ candidate: ClassicAnchorCandidate,
        candidatesByID: [String: ClassicAnchorCandidate]
    ) throws {
        guard candidate.suppressedByHFSpikingState == true else {
            guard candidate.hfSpikingSuppressorID == nil else {
                throw STPDResultPackageError.invalidInput(
                    "candidate \(candidate.id) has an HFS suppressor without suppression authority"
                )
            }
            return
        }
        guard candidate.finalLabel == .reject,
              let originalLabel = candidate.suppressedOriginalLabel,
              let originalFamily = ClassicAnchorLabel(rawValue: originalLabel),
              Set([
                  ClassicAnchorLabel.tonic,
                  .highFrequencyTonic,
                  .burst,
                  .highFrequencyBurst,
                  .longBurst,
              ]).contains(originalFamily),
              let suppressorID = candidate.hfSpikingSuppressorID,
              !suppressorID.isEmpty,
              let suppressor = candidatesByID[suppressorID],
              suppressor.trainID == candidate.trainID,
              suppressor.trainName == candidate.trainName,
              suppressor.finalLabel == .highFrequencySpiking,
              suppressor.suppressedByHFSpikingState != true,
              spansOverlap(
                lhsStart: candidate.startISIIndex,
                lhsEnd: candidate.endISIIndex,
                rhsStart: suppressor.startISIIndex,
                rhsEnd: suppressor.endISIIndex
              ) else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) has an invalid HFS suppression relationship"
            )
        }
    }

    static func validateDirectFiniteDoubles(
        _ value: Any,
        entity: String
    ) throws {
        for child in Mirror(reflecting: value).children {
            if let number = child.value as? Double, !number.isFinite {
                throw STPDResultPackageError.invalidInput(
                    "\(entity) has a non-finite \(child.label ?? "numeric field")"
                )
            }
            let reflected = Mirror(reflecting: child.value)
            if reflected.displayStyle == .optional,
               let wrapped = reflected.children.first?.value as? Double,
               !wrapped.isFinite {
                throw STPDResultPackageError.invalidInput(
                    "\(entity) has a non-finite \(child.label ?? "optional numeric field")"
                )
            }
        }
    }

    static func validateEvent(
        _ event: ClassicAnchorEventAnnotation,
        train: SpikeTrain,
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate]
    ) throws {
        guard event.trainName == train.name else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) train name does not match dataset"
            )
        }
        guard event.startISISecIndex >= 1,
              event.startISISecIndex <= event.endISISecIndex,
              event.endISISecIndex < train.spikeCount,
              event.startSpikeIndex == event.startISISecIndex,
              event.endSpikeIndex == event.endISISecIndex + 1 else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) has invalid ISI/spike geometry"
            )
        }
        guard event.rawStartSec.isFinite,
              event.rawEndSec.isFinite,
              event.alignedStartSec.isFinite,
              event.alignedEndSec.isFinite,
              event.score.isFinite else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) has non-finite required scientific values"
            )
        }
        let aligned = train.alignedTimestampsSec.count == train.spikeCount
            ? train.alignedTimestampsSec
            : train.timestampsSec
        let lowerTimestampIndex = event.startISISecIndex - 1
        let upperTimestampIndex = event.endISISecIndex
        guard canonicalEqual(event.rawStartSec, train.timestampsSec[lowerTimestampIndex]),
              canonicalEqual(event.rawEndSec, train.timestampsSec[upperTimestampIndex]),
              canonicalEqual(event.alignedStartSec, aligned[lowerTimestampIndex]),
              canonicalEqual(event.alignedEndSec, aligned[upperTimestampIndex]),
              canonicalEqual(event.durationSec, event.rawEndSec - event.rawStartSec) else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) time geometry differs from dataset"
            )
        }

        let sourceIDs = Array(Set(event.sourceCandidateIDs + [event.candidateID]))
            .filter { !$0.isEmpty }
        guard !sourceIDs.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) has no source candidate"
            )
        }
        let visible = Set(event.startISISecIndex...event.endISISecIndex)
        for sourceID in sourceIDs {
            guard candidateUIDBySourceID[sourceID] != nil,
                  let candidate = candidateBySourceID[sourceID] else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) references unknown candidate \(sourceID)"
                )
            }
            guard candidate.trainID == event.trainID,
                  candidate.trainName == event.trainName else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) source candidate \(sourceID) belongs to another train"
                )
            }
            let candidateSpan = Set(
                min(candidate.startISIIndex, candidate.endISIIndex)
                    ... max(candidate.startISIIndex, candidate.endISIIndex)
            )
            guard !candidateSpan.intersection(visible).isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) source candidate \(sourceID) does not overlap its geometry"
                )
            }
        }
        guard Set(event.automaticSupportISIIndices).isSubset(of: visible) else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) automatic support lies outside event geometry"
            )
        }
        var automaticSourceSupport = Set<Int>()
        for source in event.automaticEventSources {
            guard source.trainID == event.trainID,
                  candidateUIDBySourceID[source.candidateID] != nil,
                  let candidate = candidateBySourceID[source.candidateID],
                  source.score.isFinite else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) contains an invalid automatic evidence source"
                )
            }
            let sourceSupport = Set(source.supportISIIndices)
            guard sourceSupport.allSatisfy({ 1 <= $0 && $0 < train.spikeCount }) else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) automatic source \(source.candidateID) " +
                        "has support outside its train"
                )
            }
            let candidateSpan = Set(
                min(candidate.startISIIndex, candidate.endISIIndex)
                    ... max(candidate.startISIIndex, candidate.endISIIndex)
            )
            guard sourceSupport.isSubset(of: candidateSpan),
                  source.semanticTrack.rawValue == candidate.auditRecommendedTrackRawValue,
                  source.eventTrackClass == candidate.auditRecommendedEventTrackClass,
                  source.auditRecommendedSubtype == candidate.auditRecommendedSubtype,
                  source.auditReviewStatus == candidate.auditReviewStatus,
                  source.label == candidate.finalLabel,
                  source.lockLevel == candidate.anchorLockLevel,
                  source.stateTonicSubtype == candidate.stateTonicSubtype,
                  canonicalEqual(source.score, candidate.score),
                  source.priority == candidate.priority,
                  source.decisionPath == candidate.decisionPath else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) automatic source \(source.candidateID) " +
                        "does not match its candidate snapshot"
                )
            }
            automaticSourceSupport.formUnion(sourceSupport)
        }
        guard automaticSourceSupport.intersection(visible)
                == Set(event.automaticSupportISIIndices) else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) aggregate automatic support does not equal " +
                    "the visible intersection of its immutable source baselines"
            )
        }
    }

    static func candidateIdentityComponents(
        _ candidate: ClassicAnchorCandidate,
        intrinsicUIDBySourceID: [String: String]
    ) -> [String] {
        let suppressorIntrinsicUID: String
        if let suppressorID = candidate.hfSpikingSuppressorID {
            suppressorIntrinsicUID = intrinsicUIDBySourceID[suppressorID] ?? ""
        } else {
            suppressorIntrinsicUID = ""
        }
        return candidateIntrinsicIdentityComponents(candidate) + [suppressorIntrinsicUID]
    }

    static func candidateIntrinsicIdentityComponents(
        _ candidate: ClassicAnchorCandidate
    ) -> [String] {
        [
            candidate.trainID,
            candidate.trainName,
            candidate.candidateLayer,
            candidate.candidateClass,
            candidate.finalLabel.rawValue,
            candidate.gateStatus,
            candidate.action,
            String(candidate.priority),
            STPDCanonicalValue.bool(candidate.selectedForAuto),
            candidate.selectionStatus,
            String(candidate.startISIIndex),
            String(candidate.endISIIndex),
            String(candidate.startSpikeIndex),
            String(candidate.endSpikeIndex),
            String(candidate.nISI),
            String(candidate.nValidISI),
            String(candidate.nSpikes),
            candidate.anchorFamily,
            candidate.anchorLockLevel.rawValue,
            STPDCanonicalValue.double(candidate.durationSec),
            STPDCanonicalValue.double(candidate.intraQ10Sec),
            STPDCanonicalValue.double(candidate.intraQ40Sec),
            STPDCanonicalValue.double(candidate.intraQ50Sec),
            STPDCanonicalValue.double(candidate.intraQ90Sec),
            STPDCanonicalValue.double(candidate.intraQ95Sec),
            STPDCanonicalValue.double(candidate.maxIntraISISec),
            STPDCanonicalValue.double(candidate.meanIntraISISec),
            STPDCanonicalValue.double(candidate.cv),
            STPDCanonicalValue.double(candidate.cv2),
            STPDCanonicalValue.double(candidate.lv),
            STPDCanonicalValue.double(candidate.preGapSec),
            STPDCanonicalValue.double(candidate.postGapSec),
            STPDCanonicalValue.double(candidate.preRatioQ90),
            STPDCanonicalValue.double(candidate.postRatioQ90),
            STPDCanonicalValue.double(candidate.edgeContrastMinQ90),
            STPDCanonicalValue.double(candidate.edgeContrastGeomQ90),
            STPDCanonicalValue.double(candidate.score),
            STPDCanonicalValue.double(candidate.anchorBandLowerSec),
            STPDCanonicalValue.double(candidate.anchorBandUpperSec),
            candidate.anchorBandSource.rawValue,
            STPDCanonicalValue.double(candidate.anchorContrastMinRequired),
            STPDCanonicalValue.double(candidate.anchorContrastGeomRequired),
            String(candidate.refractorySuspectCount),
            candidate.refractorySuspectAction?.rawValue ?? "",
            STPDCanonicalValue.double(candidate.profileSeedLowPercentileInTrain),
            STPDCanonicalValue.double(candidate.profileSeedHighPercentileInTrain),
            STPDCanonicalValue.double(candidate.profileSeedBandFraction),
            STPDCanonicalValue.int(candidate.profileSeedRunCount),
            STPDCanonicalValue.int(candidate.profileMaxSeedRunLength),
            STPDCanonicalValue.double(candidate.profileMedianISISec),
            STPDCanonicalValue.double(candidate.profileQ10ISISec),
            STPDCanonicalValue.double(candidate.profileQ25ISISec),
            STPDCanonicalValue.double(candidate.profileQ90ISISec),
            STPDCanonicalValue.double(candidate.profilePauseFraction),
            candidate.profilePhenotypePrior ?? "",
            STPDCanonicalValue.double(candidate.profileBridgeUpperSec),
            STPDCanonicalValue.double(candidate.profileBoundaryFloorSec),
            STPDCanonicalValue.bool(candidate.profileBoundaryFloorHard),
            STPDCanonicalValue.double(candidate.profileBurstContrastS),
            STPDCanonicalValue.double(candidate.profilePossibleContrastS),
            STPDCanonicalValue.double(candidate.hfSpikingQ80Sec),
            STPDCanonicalValue.double(candidate.hfSpikingQ80MaxSec),
            STPDCanonicalValue.double(candidate.hfSpikingQ90MaxSec),
            STPDCanonicalValue.double(candidate.hfSpikingShortUpperSec),
            STPDCanonicalValue.double(candidate.hfSpikingEpochBridgeSec),
            STPDCanonicalValue.double(candidate.hfSpikingToleratedGapSec),
            STPDCanonicalValue.double(candidate.hfSpikingPatternMaxISISec),
            STPDCanonicalValue.double(candidate.hfSpikingPauseBreakSec),
            STPDCanonicalValue.double(candidate.hfSpikingShortFraction),
            STPDCanonicalValue.double(candidate.hfSpikingQ90ShortFraction),
            STPDCanonicalValue.double(candidate.hfSpikingBridgeFraction),
            STPDCanonicalValue.double(candidate.hfSpikingLargeFraction),
            STPDCanonicalValue.double(candidate.hfSpikingToleratedFraction),
            STPDCanonicalValue.int(candidate.hfSpikingMaxConsecutiveLargeISI),
            STPDCanonicalValue.int(candidate.hfSpikingMinSpikesRequired),
            candidate.hfSpikingAcceptanceRoute ?? "",
            STPDCanonicalValue.bool(candidate.hfSpikingBurstDominated),
            STPDCanonicalValue.int(candidate.hfSpikingEmbeddedBurstCount),
            STPDCanonicalValue.int(candidate.hfSpikingEmbeddedBurstGroupCount),
            STPDCanonicalValue.double(candidate.hfSpikingEmbeddedBurstCoverage),
            STPDCanonicalValue.bool(candidate.hfSpikingBurstPacketLike),
            STPDCanonicalValue.bool(candidate.hfSpikingBurstPacketNeighbor),
            STPDCanonicalValue.bool(candidate.suppressedByHFSpikingState),
            candidate.suppressedOriginalLabel ?? "",
            STPDCanonicalValue.double(candidate.stateRegularityScore),
            STPDCanonicalValue.double(candidate.stateBurstSeedFraction),
            STPDCanonicalValue.double(candidate.stateLowTailFraction),
            STPDCanonicalValue.double(candidate.stateLocalStabilityScore),
            STPDCanonicalValue.int(candidate.stateCoreBurstRunLength),
            candidate.stateTonicSubtype ?? "",
            STPDCanonicalValue.bool(candidate.stateContinuityAuthorityFrozen),
            STPDCanonicalValue.bool(candidate.stateContinuityMergeTerminal),
            candidate.stateHighFrequencySubtype ?? "",
            STPDCanonicalValue.double(candidate.stateTrainPercentileMedian),
            STPDCanonicalValue.double(candidate.stateLocalPercentileMedian),
            STPDCanonicalValue.double(candidate.stateLocalPercentileQ90),
            STPDCanonicalValue.double(candidate.stateLocalRobustZMedian),
            STPDCanonicalValue.double(candidate.stateLocalRobustZAbsQ80),
            STPDCanonicalValue.double(candidate.stateLocalRobustZQ10),
            STPDCanonicalValue.int(candidate.burstSeedRunStartISI),
            STPDCanonicalValue.int(candidate.burstSeedRunEndISI),
            STPDCanonicalValue.double(candidate.burstSeedBandLowerSec),
            STPDCanonicalValue.double(candidate.burstSeedBandUpperSec),
            STPDCanonicalValue.double(candidate.burstBridgeBandUpperSec),
            STPDCanonicalValue.double(candidate.burstContrastRequired),
            STPDCanonicalValue.double(candidate.burstPossibleContrastRequired),
            STPDCanonicalValue.double(candidate.burstRequiredGapSec),
            STPDCanonicalValue.double(candidate.burstPossibleRequiredGapSec),
            STPDCanonicalValue.double(candidate.burstBoundaryFloorSec),
            STPDCanonicalValue.bool(candidate.burstBoundaryFloorHard),
            STPDCanonicalValue.bool(candidate.burstStrictBoundaryPass),
            STPDCanonicalValue.bool(candidate.burstPossibleBoundaryPass),
            STPDCanonicalValue.bool(candidate.burstBridgeCountPass),
            STPDCanonicalValue.bool(candidate.burstBridgeFractionPass),
            STPDCanonicalValue.bool(candidate.burstQ90BridgePass),
            candidate.burstSizeLabelBeforeReview ?? "",
            candidate.thresholdMode ?? "",
            STPDCanonicalValue.bool(candidate.hardThreshold),
            candidate.hardThresholdPattern ?? "",
            STPDCanonicalValue.double(candidate.hardBurstSeedUpperSec),
            STPDCanonicalValue.double(candidate.hardBurstBridgeUpperSec),
            STPDCanonicalValue.int(candidate.hardBurstCoreISICount),
            candidate.hardThresholdSource ?? "",
            STPDCanonicalValue.double(candidate.localBackgroundQ75Sec),
            STPDCanonicalValue.double(candidate.localCompressionQ90Ratio),
            STPDCanonicalValue.double(candidate.eventLocalMedianSec),
            STPDCanonicalValue.double(candidate.eventLocalPercentileMedian),
            STPDCanonicalValue.double(candidate.eventLocalPercentileQ90),
            STPDCanonicalValue.double(candidate.eventLocalRobustZMedian),
            STPDCanonicalValue.double(candidate.eventLocalRobustZAbsQ80),
            STPDCanonicalValue.double(candidate.eventLocalRobustZQ10),
            candidate.auditRecommendedTrackRawValue,
            candidate.auditRecommendedEventTrackClass,
            candidate.auditRecommendedFamily,
            candidate.auditRecommendedSubtype,
            candidate.auditRecommendedFinalClass,
            candidate.auditReviewStatus,
            STPDCanonicalValue.bool(candidate.auditReviewRequired),
            candidate.auditConfidenceTier,
            candidate.auditUncertaintyReason,
            candidate.auditLongBurstDefinitionStatus,
            candidate.failureReason,
            candidate.candidateDiagnosticClass,
        ]
    }

    static func eventIdentityComponents(
        _ event: ClassicAnchorEventAnnotation,
        sourceCandidateUIDs: [String],
        candidateUIDBySourceID: [String: String]
    ) -> [String] {
        let sourceEvidence = event.automaticEventSources.map { source in
            compositeKey([
                candidateUIDBySourceID[source.candidateID] ?? "",
                source.trainID,
                source.supportISIIndices.map(String.init).joined(separator: "|"),
                source.semanticTrack.rawValue,
                source.eventTrackClass,
                source.auditRecommendedSubtype,
                source.auditReviewStatus,
                source.label.rawValue,
                source.lockLevel.rawValue,
                source.stateTonicSubtype ?? "",
                STPDCanonicalValue.double(source.score),
                String(source.priority),
                source.decisionPath,
            ])
        }.sorted()
        return [
            event.trainID,
            event.trainName,
            event.semanticTrack.rawValue,
            event.eventTrackClass,
            event.label.rawValue,
            event.lockLevel.rawValue,
            event.stateTonicSubtype ?? "",
            String(event.startISISecIndex),
            String(event.endISISecIndex),
            String(event.startSpikeIndex),
            String(event.endSpikeIndex),
            STPDCanonicalValue.double(event.rawStartSec),
            STPDCanonicalValue.double(event.rawEndSec),
            STPDCanonicalValue.double(event.alignedStartSec),
            STPDCanonicalValue.double(event.alignedEndSec),
            STPDCanonicalValue.double(event.durationSec),
            STPDCanonicalValue.double(event.score),
            String(event.priority),
            sourceCandidateUIDs.joined(separator: "|"),
            event.automaticSupportISIIndices.map(String.init).joined(separator: "|"),
            event.auditRecommendedSubtype,
            event.auditReviewStatus,
            event.decisionPath,
        ] + sourceEvidence
    }

    static func eventProjectionComponents(_ event: ClassicAnchorEventAnnotation) -> [String] {
        let rawMap = Dictionary(
            uniqueKeysWithValues: Array(
                Set(event.sourceCandidateIDs + [event.candidateID] +
                    event.automaticEventSources.map(\.candidateID))
            ).map { ($0, $0) }
        )
        return [
            event.id,
            event.candidateID,
            event.sourceCandidateIDs.sorted().joined(separator: "|"),
        ] + eventIdentityComponents(
            event,
            sourceCandidateUIDs: event.sourceCandidateIDs.sorted(),
            candidateUIDBySourceID: rawMap
        )
    }

    static func projectionFingerprint(
        _ events: [ClassicAnchorEventAnnotation]
    ) -> [String] {
        events.map { compositeKey(eventProjectionComponents($0)) }.sorted()
    }

    static func isiProjectionFingerprint(_ rows: [ReviewedISIExportRow]) -> [String] {
        rows.map { compositeKey(isiProjectionComponents($0)) }.sorted()
    }

    static func isiRowKey(_ row: ReviewedISIExportRow) -> String {
        "\(row.trainID)\u{1}\(row.isiIndex)"
    }

    static func isiEvidenceKey(trainID: String, isiIndex: Int) -> String {
        "\(trainID)\u{1}\(isiIndex)"
    }

    static func stableISIUID(
        datasetDigest: String,
        trainID: String,
        isiIndex: Int
    ) -> String {
        STPDStableIdentifier.make(
            prefix: "isi",
            domain: "stpd_isi_uid_v1",
            components: [
                datasetDigest,
                trainID,
                String(isiIndex),
            ]
        )
    }

    static func qualitySnapshotValues(
        _ quality: SpikeTrainQuality
    ) -> [String: String] {
        [
            "warning_level": quality.warningLevel.rawValue,
            "warning_message": quality.warningMessage,
            "spike_count": String(quality.spikeCount),
            "first_spike_sec": STPDCanonicalValue.double(quality.firstSpikeSec),
            "last_spike_sec": STPDCanonicalValue.double(quality.lastSpikeSec),
            "duration_sec": STPDCanonicalValue.double(quality.durationSec),
            "firing_rate_hz": STPDCanonicalValue.double(quality.firingRateHz),
            "raw_min_isi_sec": STPDCanonicalValue.double(quality.rawMinISISec),
            "min_valid_isi_sec": STPDCanonicalValue.double(quality.minValidISISec),
            "artifact_min_isi_sec":
                STPDCanonicalValue.double(quality.artifactMinISISec),
            "median_isi_sec": STPDCanonicalValue.double(quality.medianISISec),
            "max_isi_sec": STPDCanonicalValue.double(quality.maxISISec),
            "duplicate_timestamp_count": String(quality.duplicateTimestampCount),
            "zero_or_negative_isi_count": String(quality.zeroOrNegativeISICount),
            "zero_or_negative_timestamp_step_count":
                String(quality.zeroOrNegativeTimestampStepCount),
            "input_was_unsorted": STPDCanonicalValue.bool(quality.inputWasUnsorted),
            "input_nonmonotonic_step_count":
                String(quality.inputNonmonotonicStepCount),
            "input_duplicate_timestamp_step_count":
                String(quality.inputDuplicateTimestampStepCount),
            "input_zero_or_negative_step_count":
                String(quality.inputZeroOrNegativeStepCount),
            "dropped_duplicate_timestamp_count":
                String(quality.droppedDuplicateTimestampCount),
            "duplicate_timestamp_policy": quality.duplicateTimestampPolicy.rawValue,
            "artifact_isi_count": String(quality.artifactISICount),
            "artifact_fraction": STPDCanonicalValue.double(quality.artifactFraction),
            "refractory_suspect_isi_count":
                String(quality.refractorySuspectISICount),
            "refractory_suspect_fraction":
                STPDCanonicalValue.double(quality.refractorySuspectFraction),
            "valid_isi_count": String(quality.validISICount),
            "percentile_status": quality.percentileStatus,
        ]
    }

    static func taskEventIdentityComponents(
        _ event: TaskEvent,
        datasetDigest: String
    ) -> [String] {
        [
            datasetDigest,
            event.id,
            event.name,
            STPDCanonicalValue.double(event.timeSec),
            event.column,
            String(event.eventIndex),
            event.trialID,
        ]
    }

    static func taskEventUID(
        _ event: TaskEvent,
        datasetDigest: String
    ) -> String {
        STPDStableIdentifier.make(
            prefix: "task",
            domain: "stpd_task_event_uid_v2",
            components: taskEventIdentityComponents(
                event,
                datasetDigest: datasetDigest
            )
        )
    }

    static func isiQCClass(
        _ value: Double,
        settings: SpikeQualitySettings
    ) -> String {
        let artifactTolerance = max(
            1e-12,
            abs(settings.artifactThresholdSec) * 1e-6
        )
        if value < settings.artifactThresholdSec - artifactTolerance {
            return "artifact_below_floor"
        }
        let refractoryTolerance = max(
            1e-12,
            abs(settings.refractorySuspectThresholdSec) * 1e-6
        )
        if settings.refractorySuspectThresholdSec
            > settings.artifactThresholdSec,
           value < settings.refractorySuspectThresholdSec
                - refractoryTolerance {
            return "refractory_suspect"
        }
        return "valid"
    }

    static func canonicalEqual(_ lhs: Double, _ rhs: Double) -> Bool {
        lhs.isFinite && rhs.isFinite &&
            STPDCanonicalValue.double(lhs) == STPDCanonicalValue.double(rhs)
    }

    static func canonicalEqual(_ lhs: Double?, _ rhs: Double?) -> Bool {
        switch (lhs, rhs) {
        case (nil, nil):
            return true
        case let (lhs?, rhs?):
            return canonicalEqual(lhs, rhs)
        default:
            return false
        }
    }

    static func contract(_ table: STPDResultTable) throws -> STPDResultTableContract {
        guard let contract = STPDResultSchema.tables.first(where: { $0.table == table }) else {
            throw STPDResultPackageError.missingTable(table.rawValue)
        }
        return contract
    }

    static func table(
        _ table: STPDResultTable,
        headers: [String],
        rows: [[String]]
    ) throws -> STPDResultTableData {
        try STPDResultTableData(
            contract: contract(table),
            headers: headers,
            rows: rows
        )
    }

    static func identityPrefix(_ identity: DetectionRunIdentity) -> [String] {
        [identity.runID, identity.settingsDigest]
    }

    static func compositeKey(_ components: [String]) -> String {
        components.map { "\(Data($0.utf8).count):\($0)" }.joined()
    }
}

extension STPDResultPackageBuilder {
    /// Module-internal authority-normalization entry point.
    ///
    /// Package materialization and focused contract tests share the same fail-closed implementation;
    /// this does not accept or construct an `STPDResultPackageInput`, so it cannot bypass causal
    /// review-projection validation.
    static func normalizedEventRecords(
        automaticEvents: [ClassicAnchorEventAnnotation],
        finalISIRows: [ReviewedISIExportRow],
        datasetDigest: String,
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate],
        dataset: SpikeDataset,
        evidenceUIDsByISIKey: [String: [String]],
        changedISIKeys: Set<String>
    ) throws -> [STPDEventRecord] {
        try eventRecords(
            automaticEvents: automaticEvents,
            finalISIRows: finalISIRows,
            datasetDigest: datasetDigest,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID,
            dataset: dataset,
            evidenceUIDsByISIKey: evidenceUIDsByISIKey,
            changedISIKeys: changedISIKeys
        )
    }

    static func authoritativeISIEvidenceKey(
        trainID: String,
        isiIndex: Int
    ) -> String {
        isiEvidenceKey(trainID: trainID, isiIndex: isiIndex)
    }
}

extension STPDResultPackageBuilder {
    /// The exact public-projection identity used by counterfactual attribution and
    /// package validation. Internal visibility lets regression tests pin the same
    /// semantics without maintaining a second, test-only field list.
    static func isiProjectionComponents(_ row: ReviewedISIExportRow) -> [String] {
        [
            row.trainID,
            row.trainName,
            String(row.spikeIndex),
            STPDCanonicalValue.double(row.timestampSec),
            STPDCanonicalValue.double(row.alignedTimestampSec),
            String(row.isiIndex),
            STPDCanonicalValue.double(row.isiSec),
            row.autoPattern,
            row.autoSubtype,
            row.autoCandidateID,
            row.finalPattern,
            row.finalSubtype,
            row.finalSource,
            STPDCanonicalValue.bool(row.manualVetoSuppressed),
            row.reviewNote,
        ]
    }
}

private extension STPDResultPackageBuilder {
    static func runMetadataTable(
        input: STPDResultPackageInput,
        candidateCount: Int,
        diagnosticCandidateCount: Int,
        finalEventCount: Int,
        finalISICount: Int
    ) throws -> STPDResultTableData {
        let identity = input.run.runIdentity
        guard let metadataSnapshot = input.run.datasetMetadataSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity(
                "detector entry-point dataset metadata is unavailable"
            )
        }
        let headers = [
            "run_id", "settings_digest", "dataset_digest", "result_schema_version",
            "detector_version", "build_identifier", "build_identifier_kind",
            "build_reproducibility_attested",
            "owner_name", "owner_email", "source_mode",
            "dataset_name", "dataset_source", "task_event_source_digest",
            "train_count", "spike_count", "task_event_count",
            "candidate_count", "diagnostic_candidate_count",
            "final_event_count", "final_isi_count",
        ]
        return try table(
            .runMetadata,
            headers: headers,
            rows: [
                makeRow(headers, values: [
                    "run_id": identity.runID,
                    "settings_digest": identity.settingsDigest,
                    "dataset_digest": identity.datasetDigest,
                    "result_schema_version": identity.resultSchemaVersion,
                    "detector_version": identity.detectorVersion,
                    "build_identifier": identity.buildCommit,
                    "build_identifier_kind": "caller_supplied_unattested",
                    "build_reproducibility_attested": "false",
                    "owner_name": STPDResultPackageOwnership.ownerName,
                    "owner_email": STPDResultPackageOwnership.ownerEmail,
                    "source_mode": input.sourceMode.rawValue,
                    "dataset_name": metadataSnapshot.name,
                    "dataset_source": metadataSnapshot.sourceDescription,
                    "task_event_source_digest":
                        metadataSnapshot.taskEventSourceDigest,
                    "train_count": String(identity.trainCount),
                    "spike_count": String(identity.spikeCount),
                    "task_event_count": String(identity.taskEventCount),
                    "candidate_count": String(candidateCount),
                    "diagnostic_candidate_count": String(diagnosticCandidateCount),
                    "final_event_count": String(finalEventCount),
                    "final_isi_count": String(finalISICount),
                ])
            ]
        )
    }

    static func parametersTable(
        identity: DetectionRunIdentity
    ) throws -> STPDResultTableData {
        guard let snapshot = identity.settingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity("settings snapshot is unavailable")
        }
        let headers = ["run_id", "settings_digest", "parameter_key", "requested_value"]
        let rows = snapshot.entries.map { entry in
            makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "parameter_key": entry.key,
                "requested_value": entry.value,
            ])
        }
        return try table(.parametersReport, headers: headers, rows: rows)
    }

    static func resolvedParametersTable(
        identity: DetectionRunIdentity,
        run: ClassicAnchorDetectionRun,
        dataset: SpikeDataset
    ) throws -> STPDResultTableData {
        guard let snapshot = identity.settingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity(
                "settings snapshot is unavailable"
            )
        }
        let headers = [
            "run_id", "settings_digest", "scope_type", "scope_id", "scope_name",
            "parameter_key", "requested_value", "adaptive_value", "effective_value",
            "source", "resolution_mode", "resolution_note",
            "histogram_value", "default_value",
        ]
        var rows: [[String]] = snapshot.entries.map { entry in
            makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "run",
                "scope_id": identity.runID,
                "scope_name": "detector_run",
                "parameter_key": entry.key,
                "requested_value": entry.value,
                "adaptive_value": "",
                "effective_value": "",
                "source": "requested",
                "resolution_mode": "requested",
                "resolution_note": "",
                "histogram_value": "",
                "default_value": "",
            ])
        }
        func appendRunProvenance(
            key: String,
            effective: String,
            source: String = "detector_run_provenance"
        ) {
            rows.append(makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "dataset",
                "scope_id": identity.datasetDigest,
                "scope_name": "dataset_prior",
                "parameter_key": key,
                "requested_value": "",
                "adaptive_value": "",
                "effective_value": effective,
                "source": source,
                "resolution_mode": "not_applicable",
                "resolution_note": "",
                "histogram_value": "",
                "default_value": "",
            ]))
        }
        let provenance = run.datasetRerunProvenance
        appendRunProvenance(
            key: "dataset_prior.stage_path",
            effective: provenance.stagePath.joined(separator: "|")
        )
        appendRunProvenance(
            key: "dataset_prior.source",
            effective: provenance.source
        )
        appendRunProvenance(
            key: "dataset_prior.train_count",
            effective: String(provenance.trainCount)
        )
        appendRunProvenance(
            key: "dataset_prior.bridge_expansion_used_dataset_summary",
            effective: STPDCanonicalValue.bool(provenance.bridgeExpansionUsedDatasetSummary)
        )
        appendRunProvenance(
            key: "dataset_prior.summary_applied_to_resolutions",
            effective: STPDCanonicalValue.bool(provenance.datasetSummaryAppliedToResolutions)
        )
        appendRunProvenance(
            key: "dataset_prior.rerun_executed",
            effective: STPDCanonicalValue.bool(provenance.rerunExecuted)
        )
        appendRunProvenance(
            key: "dataset_prior.rerun_train_count",
            effective: String(provenance.rerunTrainCount)
        )
        appendRunProvenance(
            key: "dataset_prior.summary_included_target_train",
            effective: STPDCanonicalValue.bool(provenance.datasetSummaryIncludedTargetTrain)
        )
        appendRunProvenance(
            key: "dataset_prior.applied_leave_one_out",
            effective: STPDCanonicalValue.bool(provenance.datasetPriorAppliedLeaveOneOut)
        )
        appendRunProvenance(
            key: "dataset_prior.bridge_applied_leave_one_out",
            effective: STPDCanonicalValue.bool(provenance.bridgeExpansionPriorAppliedLeaveOneOut)
        )
        appendRunProvenance(
            key: "dataset_prior.bridge_summary_included_target_train",
            effective: STPDCanonicalValue.bool(
                provenance.bridgeExpansionSummaryIncludedTargetTrain
            )
        )
        appendDatasetSeedSummary(
            provenance.initialDatasetSummary,
            prefix: "dataset_prior.initial",
            append: appendRunProvenance
        )
        appendDatasetSeedSummary(
            provenance.finalDatasetSummary,
            prefix: "dataset_prior.final",
            append: appendRunProvenance
        )

        for resolution in run.resolutions {
            let base = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "train",
                "scope_id": resolution.trainID,
                "scope_name": resolution.trainName,
            ]
            func append(
                key: String,
                effective: String,
                source: String,
                requested: String = "",
                adaptive: String = "",
                mode: String = "not_applicable",
                note: String = "",
                histogram: String = "",
                defaultValue: String = ""
            ) {
                rows.append(makeRow(headers, values: base.merging([
                    "parameter_key": key,
                    "requested_value": requested,
                    "adaptive_value": adaptive,
                    "effective_value": effective,
                    "source": source,
                    "resolution_mode": mode,
                    "resolution_note": note,
                    "histogram_value": histogram,
                    "default_value": defaultValue,
                ]) { _, new in new }))
            }
            append(
                key: "band.min_valid_isi_sec",
                effective: STPDCanonicalValue.double(resolution.minValidISISec),
                source: "requested"
            )
            append(
                key: "band.histogram_bin_width_sec",
                effective: STPDCanonicalValue.double(resolution.histogramBinWidthSec),
                source: "requested"
            )
            append(
                key: "band.valid_isi_count",
                effective: String(resolution.validISICount),
                source: "observed"
            )
            append(
                key: "structural_seed.origin",
                effective: resolution.structuralSeedSummary.origin.rawValue,
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.source",
                effective: resolution.structuralSeedSummary.source,
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.dataset_summary_included_target_train",
                effective: STPDCanonicalValue.bool(
                    resolution.structuralSeedSummary.datasetSummaryIncludedTargetTrain
                ),
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.burst_dataset_aggregatable",
                effective: STPDCanonicalValue.bool(
                    resolution.structuralSeedSummary.isBurstSeedDatasetAggregatable
                ),
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.tonic_dataset_aggregatable",
                effective: STPDCanonicalValue.bool(
                    resolution.structuralSeedSummary.isTonicSeedDatasetAggregatable
                ),
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.pause_dataset_aggregatable",
                effective: STPDCanonicalValue.bool(
                    resolution.structuralSeedSummary.isPauseSeedDatasetAggregatable
                ),
                source: "structural_seed_provenance"
            )
            for threshold in resolution.thresholdRows {
                append(
                    key: "adaptive.\(threshold.pattern.rawValue).\(threshold.field.rawValue)",
                    effective: STPDCanonicalValue.double(threshold.effectiveSec),
                    source: threshold.source.rawValue,
                    histogram: STPDCanonicalValue.double(threshold.histogramSec),
                    defaultValue: STPDCanonicalValue.double(threshold.defaultSec)
                )
            }
        }

        for evidence in run.resolvedThresholdEvidence {
            let profile = evidence.effectiveProfile
            let effectiveValues: [String: String] = [
                "burst.seed_lower_sec": STPDCanonicalValue.double(profile.burst.lowerSec),
                "burst.seed_upper_sec": STPDCanonicalValue.double(profile.burst.upperSec),
                "burst.bridge_upper_sec":
                    STPDCanonicalValue.double(profile.burst.bridgeUpperSec),
                "burst.min_spikes": STPDCanonicalValue.int(profile.burst.minSpikes),
                "burst.classic_max_spikes":
                    STPDCanonicalValue.int(profile.burst.classicMaxSpikes),
                "burst.long_min_spikes":
                    STPDCanonicalValue.int(profile.burst.longMinSpikes),
                "burst.long_max_spikes":
                    STPDCanonicalValue.int(profile.burst.longMaxSpikes),
                "hfs.seed_lower_sec": STPDCanonicalValue.double(profile.hfs.lowerSec),
                "hfs.seed_upper_sec": STPDCanonicalValue.double(profile.hfs.upperSec),
                "hfs.min_spikes": STPDCanonicalValue.int(profile.hfs.minSpikes),
                "hfs.min_duration_sec":
                    STPDCanonicalValue.double(profile.hfs.minDurationSec),
                "hf_tonic.isi_floor_sec":
                    STPDCanonicalValue.double(profile.hfTonic.lowerSec),
                "hf_tonic.isi_upper_sec":
                    STPDCanonicalValue.double(profile.hfTonic.upperSec),
                "hf_tonic.min_spikes":
                    STPDCanonicalValue.int(profile.hfTonic.minSpikes),
                "tonic.isi_lower_sec": STPDCanonicalValue.double(profile.tonic.lowerSec),
                "tonic.isi_upper_sec": STPDCanonicalValue.double(profile.tonic.upperSec),
                "tonic.min_spikes": STPDCanonicalValue.int(profile.tonic.minSpikes),
                "pause.isi_lower_sec": STPDCanonicalValue.double(profile.pause.lowerSec),
            ]
            let provenanceByKey = Dictionary(
                uniqueKeysWithValues: evidence.resolutionProvenance.map { ($0.key, $0) }
            )
            let base = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "train",
                "scope_id": evidence.trainID,
                "scope_name": evidence.trainName,
                "histogram_value": "",
                "default_value": "",
            ]
            func appendFinal(
                key: String,
                requested: String = "",
                adaptive: String = "",
                effective: String,
                source: String,
                mode: String,
                note: String = ""
            ) {
                rows.append(makeRow(headers, values: base.merging([
                    "parameter_key": key,
                    "requested_value": requested,
                    "adaptive_value": adaptive,
                    "effective_value": effective,
                    "source": source,
                    "resolution_mode": mode,
                    "resolution_note": note,
                ]) { _, new in new }))
            }
            appendFinal(
                key: "resolution.stage_path",
                effective: evidence.stagePath,
                source: "detector_stage",
                mode: "not_applicable"
            )
            for key in effectiveValues.keys.sorted() {
                let resolved = provenanceByKey[key]
                let notes = [
                    resolved?.note,
                    "post_clamp_effective_captured_at_authoritative_rerun",
                ].compactMap { $0 }
                appendFinal(
                    key: key,
                    requested: STPDCanonicalValue.double(resolved?.userValue),
                    adaptive: STPDCanonicalValue.double(resolved?.adaptiveValue),
                    effective: effectiveValues[key] ?? "",
                    source: resolved?.source.rawValue ?? "post_clamp_effective",
                    mode: resolved?.mode.rawValue ?? ThresholdMode.automatic.rawValue,
                    note: notes.joined(separator: "|")
                )
            }
            let unmatchedProvenance = evidence.resolutionProvenance
                .filter { effectiveValues[$0.key] == nil }
                .sorted { $0.key < $1.key }
            for resolved in unmatchedProvenance {
                appendFinal(
                    key: "resolver.\(resolved.key)",
                    requested: STPDCanonicalValue.double(resolved.userValue),
                    adaptive: STPDCanonicalValue.double(resolved.adaptiveValue),
                    effective: STPDCanonicalValue.double(resolved.effectiveValue),
                    source: resolved.source.rawValue,
                    mode: resolved.mode.rawValue,
                    note: resolved.note ?? ""
                )
            }
            for key in evidence.learnedProvenanceByKey.keys.sorted() {
                appendFinal(
                    key: "learned_provenance.\(key)",
                    effective: evidence.learnedProvenanceByKey[key] ?? "",
                    source: "learned_provenance",
                    mode: "not_applicable"
                )
            }
        }

        for train in dataset.trains {
            let quality = SpikeQualityAnalyzer.quality(
                for: train,
                settings: run.qualitySettings
            )
            let base = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "train",
                "scope_id": train.id,
                "scope_name": train.name,
                "source": "quality_analyzer",
                "requested_value": "",
                "adaptive_value": "",
                "resolution_mode": "not_applicable",
                "resolution_note": "",
                "histogram_value": "",
                "default_value": "",
            ]
            func appendQuality(_ key: String, _ value: String) {
                rows.append(makeRow(headers, values: base.merging([
                    "parameter_key": "quality.\(key)",
                    "effective_value": value,
                ]) { _, new in new }))
            }
            let qualityValues = qualitySnapshotValues(quality)
            for key in qualityValues.keys.sorted() {
                appendQuality(key, qualityValues[key] ?? "")
            }
        }
        return try table(.resolvedParameters, headers: headers, rows: rows)
    }

    static func appendDatasetSeedSummary(
        _ summary: StructuralDatasetSeedSummary,
        prefix: String,
        append: (_ key: String, _ effective: String, _ source: String) -> Void
    ) {
        func add(_ suffix: String, _ value: String) {
            append("\(prefix).\(suffix)", value, "dataset_seed_summary")
        }
        add("train_count", String(summary.trainCount))
        add("seeded_train_count", String(summary.seededTrainCount))
        add("burst_anchor_count", String(summary.burstAnchorCount))
        add("burst_support_weight", STPDCanonicalValue.double(summary.burstSupportWeight))
        add("burst_seed_upper_sec", STPDCanonicalValue.double(summary.burstSeedUpperSec))
        add("burst_bridge_upper_sec", STPDCanonicalValue.double(summary.burstBridgeUpperSec))
        add("tonic_anchor_count", String(summary.tonicAnchorCount))
        add("tonic_support_weight", STPDCanonicalValue.double(summary.tonicSupportWeight))
        add("tonic_seed_lower_sec", STPDCanonicalValue.double(summary.tonicSeedLowerSec))
        add("tonic_seed_upper_sec", STPDCanonicalValue.double(summary.tonicSeedUpperSec))
        add("pause_anchor_count", String(summary.pauseAnchorCount))
        add("pause_pool_anchor_count", String(summary.pausePoolAnchorCount))
        add("pause_support_weight", STPDCanonicalValue.double(summary.pauseSupportWeight))
        add(
            "pause_pool_support_weight",
            STPDCanonicalValue.double(summary.pausePoolSupportWeight)
        )
        add("pause_seed_lower_sec", STPDCanonicalValue.double(summary.pauseSeedLowerSec))
        add("pause_seed_upper_sec", STPDCanonicalValue.double(summary.pauseSeedUpperSec))
        add("pause_pool_source", summary.pausePoolSource)
        add("source", summary.source)
        add("is_self_inclusive", STPDCanonicalValue.bool(summary.isSelfInclusive))
    }

    static func candidateLedgerTable(
        identity: DetectionRunIdentity,
        records: [STPDCandidateRecord],
        tableKind: STPDResultTable = .candidateLedger
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "source_candidate_id", "train_id",
            "train_name", "candidate_layer", "candidate_class",
            "final_label", "gate_status", "action", "selected_for_auto",
            "selection_status", "start_isi_index",
            "end_isi_index", "start_spike_ordinal", "end_spike_ordinal", "n_isi",
            "n_valid_isi", "n_spikes", "anchor_family", "anchor_lock_level",
        ]
        let rows = records.map { record in
            let candidate = record.candidate
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": record.uid,
                "source_candidate_id": candidate.id,
                "train_id": candidate.trainID,
                "train_name": candidate.trainName,
                "candidate_layer": candidate.candidateLayer,
                "candidate_class": candidate.candidateClass,
                "final_label": candidate.finalLabel.rawValue,
                "gate_status": candidate.gateStatus,
                "action": candidate.action,
                "selected_for_auto":
                    STPDCanonicalValue.bool(candidate.selectedForAuto),
                "selection_status": candidate.selectionStatus,
                "start_isi_index": String(candidate.startISIIndex),
                "end_isi_index": String(candidate.endISIIndex),
                "start_spike_ordinal": String(candidate.startSpikeIndex),
                "end_spike_ordinal": String(candidate.endSpikeIndex),
                "n_isi": String(candidate.nISI),
                "n_valid_isi": String(candidate.nValidISI),
                "n_spikes": String(candidate.nSpikes),
                "anchor_family": candidate.anchorFamily,
                "anchor_lock_level": candidate.anchorLockLevel.rawValue,
            ])
        }
        return try table(tableKind, headers: headers, rows: rows)
    }

    static func candidateFeaturesTable(
        identity: DetectionRunIdentity,
        records: [STPDCandidateRecord],
        candidateUIDBySourceID: [String: String],
        tableKind: STPDResultTable = .candidateFeatures
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
            "duration_sec", "intra_q10_sec", "intra_q40_sec", "intra_q50_sec",
            "intra_q90_sec", "intra_q95_sec", "max_intra_isi_sec", "mean_intra_isi_sec",
            "cv", "cv2", "lv", "pre_gap_sec", "post_gap_sec", "pre_ratio_q90",
            "post_ratio_q90", "edge_contrast_min_q90", "edge_contrast_geom_q90", "score",
            "anchor_band_lower_sec", "anchor_band_upper_sec", "anchor_band_source",
            "anchor_band_semantics", "anchor_band_ordered",
            "anchor_contrast_min_required", "anchor_contrast_geom_required",
            "refractory_suspect_count", "refractory_suspect_action",
            "profile_seed_low_percentile", "profile_seed_high_percentile",
            "profile_seed_band_fraction", "profile_seed_run_count", "profile_max_seed_run_length",
            "profile_median_isi_sec", "profile_q10_isi_sec", "profile_q25_isi_sec",
            "profile_q90_isi_sec", "profile_pause_fraction", "profile_phenotype_prior",
            "profile_bridge_upper_sec", "profile_boundary_floor_sec",
            "profile_boundary_floor_hard", "profile_burst_contrast_s",
            "profile_possible_contrast_s",
            "hf_q80_sec", "hf_q80_max_sec", "hf_q90_max_sec", "hf_short_upper_sec",
            "hf_epoch_bridge_sec", "hf_tolerated_gap_sec", "hf_pattern_max_isi_sec",
            "hf_pause_break_sec", "hf_short_fraction", "hf_q90_short_fraction",
            "hf_bridge_fraction", "hf_large_fraction", "hf_tolerated_fraction",
            "hf_max_consecutive_large_isi", "hf_min_spikes_required",
            "hf_acceptance_route", "hf_burst_dominated",
            "hf_embedded_burst_count", "hf_embedded_burst_group_count",
            "hf_embedded_burst_coverage", "hf_burst_packet_like",
            "hf_burst_packet_neighbor", "suppressed_by_hf_state",
            "suppressed_original_label", "hf_suppressor_candidate_uid",
            "state_regularity_score", "state_burst_seed_fraction", "state_low_tail_fraction",
            "state_local_stability_score", "state_core_burst_run_length",
            "state_continuity_authority_frozen", "state_continuity_merge_terminal",
            "state_train_percentile_median", "state_local_percentile_median",
            "state_local_percentile_q90", "state_local_robust_z_median",
            "state_local_robust_z_abs_q80", "state_local_robust_z_q10",
            "burst_seed_run_start_isi", "burst_seed_run_end_isi", "burst_seed_band_lower_sec",
            "burst_seed_band_upper_sec", "burst_bridge_band_upper_sec",
            "burst_contrast_required", "burst_possible_contrast_required",
            "burst_required_gap_sec", "burst_possible_required_gap_sec",
            "burst_boundary_floor_sec", "burst_boundary_floor_hard",
            "burst_strict_boundary_pass", "burst_possible_boundary_pass",
            "burst_bridge_count_pass", "burst_bridge_fraction_pass", "burst_q90_bridge_pass",
            "burst_size_label_before_review",
            "threshold_mode", "hard_threshold", "hard_threshold_pattern",
            "hard_burst_seed_upper_sec", "hard_burst_bridge_upper_sec",
            "hard_burst_core_isi_count", "hard_threshold_source",
            "local_background_q75_sec", "local_compression_q90_ratio",
            "event_local_median_sec", "event_local_percentile_median",
            "event_local_percentile_q90", "event_local_robust_z_median",
            "event_local_robust_z_abs_q80", "event_local_robust_z_q10",
        ]
        let rows = records.map { record in
            let c = record.candidate
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": record.uid,
                "source_candidate_id": c.id,
                "duration_sec": STPDCanonicalValue.double(c.durationSec),
                "intra_q10_sec": STPDCanonicalValue.double(c.intraQ10Sec),
                "intra_q40_sec": STPDCanonicalValue.double(c.intraQ40Sec),
                "intra_q50_sec": STPDCanonicalValue.double(c.intraQ50Sec),
                "intra_q90_sec": STPDCanonicalValue.double(c.intraQ90Sec),
                "intra_q95_sec": STPDCanonicalValue.double(c.intraQ95Sec),
                "max_intra_isi_sec": STPDCanonicalValue.double(c.maxIntraISISec),
                "mean_intra_isi_sec": STPDCanonicalValue.double(c.meanIntraISISec),
                "cv": STPDCanonicalValue.double(c.cv),
                "cv2": STPDCanonicalValue.double(c.cv2),
                "lv": STPDCanonicalValue.double(c.lv),
                "pre_gap_sec": STPDCanonicalValue.double(c.preGapSec),
                "post_gap_sec": STPDCanonicalValue.double(c.postGapSec),
                "pre_ratio_q90": STPDCanonicalValue.double(c.preRatioQ90),
                "post_ratio_q90": STPDCanonicalValue.double(c.postRatioQ90),
                "edge_contrast_min_q90": STPDCanonicalValue.double(c.edgeContrastMinQ90),
                "edge_contrast_geom_q90": STPDCanonicalValue.double(c.edgeContrastGeomQ90),
                "score": STPDCanonicalValue.double(c.score),
                "anchor_band_lower_sec": STPDCanonicalValue.double(c.anchorBandLowerSec),
                "anchor_band_upper_sec": STPDCanonicalValue.double(c.anchorBandUpperSec),
                "anchor_band_source": c.anchorBandSource.rawValue,
                "anchor_band_semantics": c.finalLabel == .profile
                    ? "profile_boundary_summary"
                    : "ordered_candidate_band",
                "anchor_band_ordered": STPDCanonicalValue.bool(
                    c.anchorBandUpperSec >= c.anchorBandLowerSec
                ),
                "anchor_contrast_min_required": STPDCanonicalValue.double(c.anchorContrastMinRequired),
                "anchor_contrast_geom_required": STPDCanonicalValue.double(c.anchorContrastGeomRequired),
                "refractory_suspect_count": String(c.refractorySuspectCount),
                "refractory_suspect_action": c.refractorySuspectAction?.rawValue ?? "",
                "profile_seed_low_percentile": STPDCanonicalValue.double(c.profileSeedLowPercentileInTrain),
                "profile_seed_high_percentile": STPDCanonicalValue.double(c.profileSeedHighPercentileInTrain),
                "profile_seed_band_fraction": STPDCanonicalValue.double(c.profileSeedBandFraction),
                "profile_seed_run_count": STPDCanonicalValue.int(c.profileSeedRunCount),
                "profile_max_seed_run_length": STPDCanonicalValue.int(c.profileMaxSeedRunLength),
                "profile_median_isi_sec": STPDCanonicalValue.double(c.profileMedianISISec),
                "profile_q10_isi_sec": STPDCanonicalValue.double(c.profileQ10ISISec),
                "profile_q25_isi_sec": STPDCanonicalValue.double(c.profileQ25ISISec),
                "profile_q90_isi_sec": STPDCanonicalValue.double(c.profileQ90ISISec),
                "profile_pause_fraction": STPDCanonicalValue.double(c.profilePauseFraction),
                "profile_phenotype_prior": c.profilePhenotypePrior ?? "",
                "profile_bridge_upper_sec":
                    STPDCanonicalValue.double(c.profileBridgeUpperSec),
                "profile_boundary_floor_sec":
                    STPDCanonicalValue.double(c.profileBoundaryFloorSec),
                "profile_boundary_floor_hard":
                    STPDCanonicalValue.bool(c.profileBoundaryFloorHard),
                "profile_burst_contrast_s":
                    STPDCanonicalValue.double(c.profileBurstContrastS),
                "profile_possible_contrast_s":
                    STPDCanonicalValue.double(c.profilePossibleContrastS),
                "hf_q80_sec": STPDCanonicalValue.double(c.hfSpikingQ80Sec),
                "hf_q80_max_sec": STPDCanonicalValue.double(c.hfSpikingQ80MaxSec),
                "hf_q90_max_sec": STPDCanonicalValue.double(c.hfSpikingQ90MaxSec),
                "hf_short_upper_sec":
                    STPDCanonicalValue.double(c.hfSpikingShortUpperSec),
                "hf_epoch_bridge_sec":
                    STPDCanonicalValue.double(c.hfSpikingEpochBridgeSec),
                "hf_tolerated_gap_sec":
                    STPDCanonicalValue.double(c.hfSpikingToleratedGapSec),
                "hf_pattern_max_isi_sec":
                    STPDCanonicalValue.double(c.hfSpikingPatternMaxISISec),
                "hf_pause_break_sec":
                    STPDCanonicalValue.double(c.hfSpikingPauseBreakSec),
                "hf_short_fraction": STPDCanonicalValue.double(c.hfSpikingShortFraction),
                "hf_q90_short_fraction":
                    STPDCanonicalValue.double(c.hfSpikingQ90ShortFraction),
                "hf_bridge_fraction": STPDCanonicalValue.double(c.hfSpikingBridgeFraction),
                "hf_large_fraction": STPDCanonicalValue.double(c.hfSpikingLargeFraction),
                "hf_tolerated_fraction":
                    STPDCanonicalValue.double(c.hfSpikingToleratedFraction),
                "hf_max_consecutive_large_isi": STPDCanonicalValue.int(c.hfSpikingMaxConsecutiveLargeISI),
                "hf_min_spikes_required":
                    STPDCanonicalValue.int(c.hfSpikingMinSpikesRequired),
                "hf_acceptance_route": c.hfSpikingAcceptanceRoute ?? "",
                "hf_burst_dominated": STPDCanonicalValue.bool(c.hfSpikingBurstDominated),
                "hf_embedded_burst_count": STPDCanonicalValue.int(c.hfSpikingEmbeddedBurstCount),
                "hf_embedded_burst_group_count":
                    STPDCanonicalValue.int(c.hfSpikingEmbeddedBurstGroupCount),
                "hf_embedded_burst_coverage": STPDCanonicalValue.double(c.hfSpikingEmbeddedBurstCoverage),
                "hf_burst_packet_like":
                    STPDCanonicalValue.bool(c.hfSpikingBurstPacketLike),
                "hf_burst_packet_neighbor":
                    STPDCanonicalValue.bool(c.hfSpikingBurstPacketNeighbor),
                "suppressed_by_hf_state":
                    STPDCanonicalValue.bool(c.suppressedByHFSpikingState),
                "suppressed_original_label": c.suppressedOriginalLabel ?? "",
                "hf_suppressor_candidate_uid":
                    c.hfSpikingSuppressorID.flatMap {
                        candidateUIDBySourceID[$0]
                    } ?? "",
                "state_regularity_score": STPDCanonicalValue.double(c.stateRegularityScore),
                "state_burst_seed_fraction": STPDCanonicalValue.double(c.stateBurstSeedFraction),
                "state_low_tail_fraction": STPDCanonicalValue.double(c.stateLowTailFraction),
                "state_local_stability_score": STPDCanonicalValue.double(c.stateLocalStabilityScore),
                "state_core_burst_run_length": STPDCanonicalValue.int(c.stateCoreBurstRunLength),
                "state_continuity_authority_frozen":
                    STPDCanonicalValue.bool(c.stateContinuityAuthorityFrozen),
                "state_continuity_merge_terminal":
                    STPDCanonicalValue.bool(c.stateContinuityMergeTerminal),
                "state_train_percentile_median": STPDCanonicalValue.double(c.stateTrainPercentileMedian),
                "state_local_percentile_median": STPDCanonicalValue.double(c.stateLocalPercentileMedian),
                "state_local_percentile_q90": STPDCanonicalValue.double(c.stateLocalPercentileQ90),
                "state_local_robust_z_median": STPDCanonicalValue.double(c.stateLocalRobustZMedian),
                "state_local_robust_z_abs_q80": STPDCanonicalValue.double(c.stateLocalRobustZAbsQ80),
                "state_local_robust_z_q10": STPDCanonicalValue.double(c.stateLocalRobustZQ10),
                "burst_seed_run_start_isi": STPDCanonicalValue.int(c.burstSeedRunStartISI),
                "burst_seed_run_end_isi": STPDCanonicalValue.int(c.burstSeedRunEndISI),
                "burst_seed_band_lower_sec": STPDCanonicalValue.double(c.burstSeedBandLowerSec),
                "burst_seed_band_upper_sec": STPDCanonicalValue.double(c.burstSeedBandUpperSec),
                "burst_bridge_band_upper_sec": STPDCanonicalValue.double(c.burstBridgeBandUpperSec),
                "burst_contrast_required": STPDCanonicalValue.double(c.burstContrastRequired),
                "burst_possible_contrast_required": STPDCanonicalValue.double(c.burstPossibleContrastRequired),
                "burst_required_gap_sec": STPDCanonicalValue.double(c.burstRequiredGapSec),
                "burst_possible_required_gap_sec": STPDCanonicalValue.double(c.burstPossibleRequiredGapSec),
                "burst_boundary_floor_sec": STPDCanonicalValue.double(c.burstBoundaryFloorSec),
                "burst_boundary_floor_hard": STPDCanonicalValue.bool(c.burstBoundaryFloorHard),
                "burst_strict_boundary_pass": STPDCanonicalValue.bool(c.burstStrictBoundaryPass),
                "burst_possible_boundary_pass": STPDCanonicalValue.bool(c.burstPossibleBoundaryPass),
                "burst_bridge_count_pass": STPDCanonicalValue.bool(c.burstBridgeCountPass),
                "burst_bridge_fraction_pass": STPDCanonicalValue.bool(c.burstBridgeFractionPass),
                "burst_q90_bridge_pass": STPDCanonicalValue.bool(c.burstQ90BridgePass),
                "burst_size_label_before_review": c.burstSizeLabelBeforeReview ?? "",
                "threshold_mode": c.thresholdMode ?? "",
                "hard_threshold": STPDCanonicalValue.bool(c.hardThreshold),
                "hard_threshold_pattern": c.hardThresholdPattern ?? "",
                "hard_burst_seed_upper_sec": STPDCanonicalValue.double(c.hardBurstSeedUpperSec),
                "hard_burst_bridge_upper_sec": STPDCanonicalValue.double(c.hardBurstBridgeUpperSec),
                "hard_burst_core_isi_count": STPDCanonicalValue.int(c.hardBurstCoreISICount),
                "hard_threshold_source": c.hardThresholdSource ?? "",
                "local_background_q75_sec": STPDCanonicalValue.double(c.localBackgroundQ75Sec),
                "local_compression_q90_ratio": STPDCanonicalValue.double(c.localCompressionQ90Ratio),
                "event_local_median_sec": STPDCanonicalValue.double(c.eventLocalMedianSec),
                "event_local_percentile_median": STPDCanonicalValue.double(c.eventLocalPercentileMedian),
                "event_local_percentile_q90": STPDCanonicalValue.double(c.eventLocalPercentileQ90),
                "event_local_robust_z_median": STPDCanonicalValue.double(c.eventLocalRobustZMedian),
                "event_local_robust_z_abs_q80": STPDCanonicalValue.double(c.eventLocalRobustZAbsQ80),
                "event_local_robust_z_q10": STPDCanonicalValue.double(c.eventLocalRobustZQ10),
            ])
        }
        return try table(tableKind, headers: headers, rows: rows)
    }

    static func finalDecisionsTable(
        identity: DetectionRunIdentity,
        records: [STPDCandidateRecord],
        tableKind: STPDResultTable = .finalDecisions
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
            "final_label", "gate_status", "action", "priority", "selected_for_auto",
            "selection_status", "semantic_track", "event_track_class", "audit_family",
            "audit_subtype", "audit_final_class", "audit_review_status",
            "audit_review_required", "audit_confidence_tier", "audit_uncertainty_reason",
            "audit_long_burst_definition_status", "failure_reason",
            "candidate_diagnostic_class", "state_tonic_subtype",
            "state_high_frequency_subtype", "decision_path",
        ]
        let rows = records.map { record in
            let candidate = record.candidate
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": record.uid,
                "source_candidate_id": candidate.id,
                "final_label": candidate.finalLabel.rawValue,
                "gate_status": candidate.gateStatus,
                "action": candidate.action,
                "priority": String(candidate.priority),
                "selected_for_auto": STPDCanonicalValue.bool(candidate.selectedForAuto),
                "selection_status": candidate.selectionStatus,
                "semantic_track": candidate.auditRecommendedTrackRawValue,
                "event_track_class": candidate.auditRecommendedEventTrackClass,
                "audit_family": candidate.auditRecommendedFamily,
                "audit_subtype": candidate.auditRecommendedSubtype,
                "audit_final_class": candidate.auditRecommendedFinalClass,
                "audit_review_status": candidate.auditReviewStatus,
                "audit_review_required": STPDCanonicalValue.bool(candidate.auditReviewRequired),
                "audit_confidence_tier": candidate.auditConfidenceTier,
                "audit_uncertainty_reason": candidate.auditUncertaintyReason,
                "audit_long_burst_definition_status": candidate.auditLongBurstDefinitionStatus,
                "failure_reason": candidate.failureReason,
                "candidate_diagnostic_class": candidate.candidateDiagnosticClass,
                "state_tonic_subtype": candidate.stateTonicSubtype ?? "",
                "state_high_frequency_subtype":
                    candidate.stateHighFrequencySubtype ?? "",
                "decision_path": candidate.decisionPath,
            ])
        }
        return try table(tableKind, headers: headers, rows: rows)
    }

    static func eventsTable(
        identity: DetectionRunIdentity,
        records: [STPDEventRecord],
        evidenceUIDsByISIKey: [String: [String]],
        changedISIKeys: Set<String>
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "event_uid", "source_event_ids",
            "train_id", "train_name", "final_label", "final_subtype",
            "state_tonic_subtype",
            "state_high_frequency_subtypes", "semantic_track", "event_track_class",
            "lock_level", "authority_origin",
            "start_isi_index", "end_isi_index", "start_spike_ordinal", "end_spike_ordinal",
            "raw_start_sec", "raw_end_sec", "aligned_start_sec", "aligned_end_sec",
            "duration_sec", "score", "priority", "source_candidate_uids",
            "unresolved_source_candidate_ids", "automatic_support_isi_indices",
            "review_evidence_uids", "review_evidence_present",
            "review_changed_projection",
            "audit_recommended_subtype", "audit_review_status", "decision_path",
        ]
        let rows = records.map { record in
            let event = record.event
            let evidenceUIDs = Set(
                (event.startISIIndex...event.endISIIndex)
                    .flatMap {
                        evidenceUIDsByISIKey[
                            isiEvidenceKey(trainID: event.trainID, isiIndex: $0)
                        ] ?? []
                    }
            ).sorted()
            let changedProjection =
                (event.startISIIndex...event.endISIIndex)
                .contains {
                    changedISIKeys.contains(
                        isiEvidenceKey(trainID: event.trainID, isiIndex: $0)
                    )
                }
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "event_uid": record.uid,
                "source_event_ids": STPDCanonicalValue.stringList(event.sourceEventIDs),
                "train_id": event.trainID,
                "train_name": event.trainName,
                "final_label": event.finalLabel,
                "final_subtype": event.finalSubtype,
                "state_tonic_subtype": event.stateTonicSubtype,
                "state_high_frequency_subtypes":
                    STPDCanonicalValue.stringList(event.stateHighFrequencySubtypes),
                "semantic_track": event.semanticTrack,
                "event_track_class": event.eventTrackClass,
                "lock_level": event.lockLevel,
                "authority_origin": event.authorityOrigin,
                "start_isi_index": String(event.startISIIndex),
                "end_isi_index": String(event.endISIIndex),
                "start_spike_ordinal": String(event.startSpikeOrdinal),
                "end_spike_ordinal": String(event.endSpikeOrdinal),
                "raw_start_sec": STPDCanonicalValue.double(event.rawStartSec),
                "raw_end_sec": STPDCanonicalValue.double(event.rawEndSec),
                "aligned_start_sec": STPDCanonicalValue.double(event.alignedStartSec),
                "aligned_end_sec": STPDCanonicalValue.double(event.alignedEndSec),
                "duration_sec":
                    STPDCanonicalValue.double(event.rawEndSec - event.rawStartSec),
                "score": STPDCanonicalValue.double(event.score),
                "priority": STPDCanonicalValue.int(event.priority),
                "source_candidate_uids":
                    STPDCanonicalValue.stringList(record.sourceCandidateUIDs),
                "unresolved_source_candidate_ids":
                    STPDCanonicalValue.stringList(record.unresolvedSourceCandidateIDs),
                "automatic_support_isi_indices": STPDCanonicalValue.stringList(
                    event.automaticSupportISIIndices.map(String.init)
                ),
                "review_evidence_uids": STPDCanonicalValue.stringList(evidenceUIDs),
                "review_evidence_present":
                    STPDCanonicalValue.bool(!evidenceUIDs.isEmpty),
                "review_changed_projection":
                    STPDCanonicalValue.bool(changedProjection),
                "audit_recommended_subtype": event.auditRecommendedSubtype,
                "audit_review_status": event.auditReviewStatus,
                "decision_path": event.decisionPath,
            ])
        }
        return try table(.eventsFinal, headers: headers, rows: rows)
    }

    static func isiLabelsTable(
        identity: DetectionRunIdentity,
        rows: [ReviewedISIExportRow],
        candidateUIDBySourceID: [String: String],
        evidenceUIDsByISIKey: [String: [String]],
        changedISIKeys: Set<String>,
        qualitySettings: SpikeQualitySettings,
        dataset: SpikeDataset
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "isi_uid", "train_id", "train_name", "isi_index",
            "left_spike_array_index", "right_spike_array_index",
            "left_spike_ordinal", "right_spike_ordinal",
            "timestamp_sec", "aligned_timestamp_sec", "isi_sec",
            "auto_pattern", "auto_subtype", "auto_source_candidate_id",
            "auto_candidate_uid", "final_pattern", "final_subtype", "final_source",
            "manual_veto_suppressed", "review_note", "review_evidence_uids",
            "review_evidence_present", "review_changed_projection",
            "isi_qc_class", "artifact_floor_status", "qc_refractory_suspect",
            "qc_artifact_threshold_sec", "qc_refractory_threshold_sec",
            "train_qc_warning_level", "train_qc_warning_message",
            "train_qc_duration_sec", "train_qc_firing_rate_hz",
            "train_qc_raw_min_isi_sec", "train_qc_min_valid_isi_sec",
            "train_qc_artifact_min_isi_sec", "train_qc_median_isi_sec",
            "train_qc_max_isi_sec", "train_qc_duplicate_timestamp_count",
            "train_qc_zero_or_negative_isi_count",
            "train_qc_zero_or_negative_timestamp_step_count",
            "train_qc_input_was_unsorted",
            "train_qc_input_nonmonotonic_step_count",
            "train_qc_input_duplicate_timestamp_step_count",
            "train_qc_input_zero_or_negative_step_count",
            "train_qc_dropped_duplicate_timestamp_count",
            "train_qc_duplicate_timestamp_policy",
            "train_qc_artifact_isi_count", "train_qc_artifact_fraction",
            "train_qc_refractory_suspect_isi_count",
            "train_qc_refractory_suspect_fraction",
            "train_qc_valid_isi_count", "train_qc_percentile_status",
        ]
        let trainsByID = Dictionary(
            uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) }
        )
        let qualitiesByTrainID = Dictionary(
            uniqueKeysWithValues: dataset.trains.map { train in
                (
                    train.id,
                    SpikeQualityAnalyzer.quality(
                        for: train,
                        settings: qualitySettings
                    )
                )
            }
        )
        let materialized = try rows.map { row -> [String] in
            let sourceID = row.autoCandidateID
            let candidateUID = sourceID.isEmpty ? "" : candidateUIDBySourceID[sourceID]
            if !sourceID.isEmpty, candidateUID == nil {
                throw STPDResultPackageError.invalidInput(
                    "ISI row references unknown automatic candidate \(sourceID)"
                )
            }
            guard trainsByID[row.trainID] != nil,
                  let quality = qualitiesByTrainID[row.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row references unknown QC train \(row.trainID)"
                )
            }
            let key = isiRowKey(row)
            let evidenceUIDs = evidenceUIDsByISIKey[key] ?? []
            let qcClass = isiQCClass(
                row.isiSec,
                settings: qualitySettings
            )
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "isi_uid": stableISIUID(
                    datasetDigest: identity.datasetDigest,
                    trainID: row.trainID,
                    isiIndex: row.isiIndex
                ),
                "train_id": row.trainID,
                "train_name": row.trainName,
                "isi_index": String(row.isiIndex),
                "left_spike_array_index": String(row.spikeIndex - 1),
                "right_spike_array_index": String(row.spikeIndex),
                "left_spike_ordinal": String(row.spikeIndex),
                "right_spike_ordinal": String(row.spikeIndex + 1),
                "timestamp_sec": STPDCanonicalValue.double(row.timestampSec),
                "aligned_timestamp_sec": STPDCanonicalValue.double(row.alignedTimestampSec),
                "isi_sec": STPDCanonicalValue.double(row.isiSec),
                "auto_pattern": row.autoPattern,
                "auto_subtype": row.autoSubtype,
                "auto_source_candidate_id": sourceID,
                "auto_candidate_uid": candidateUID ?? "",
                "final_pattern": row.finalPattern,
                "final_subtype": row.finalSubtype,
                "final_source": row.finalSource,
                "manual_veto_suppressed": STPDCanonicalValue.bool(row.manualVetoSuppressed),
                "review_note": row.reviewNote,
                "review_evidence_uids": STPDCanonicalValue.stringList(evidenceUIDs),
                "review_evidence_present":
                    STPDCanonicalValue.bool(!evidenceUIDs.isEmpty),
                "review_changed_projection":
                    STPDCanonicalValue.bool(changedISIKeys.contains(key)),
                "isi_qc_class": qcClass,
                "artifact_floor_status":
                    qcClass == "artifact_below_floor"
                    ? "below_artifact_floor"
                    : "at_or_above_artifact_floor",
                "qc_refractory_suspect":
                    STPDCanonicalValue.bool(qcClass == "refractory_suspect"),
                "qc_artifact_threshold_sec":
                    STPDCanonicalValue.double(qualitySettings.artifactThresholdSec),
                "qc_refractory_threshold_sec":
                    STPDCanonicalValue.double(
                        qualitySettings.refractorySuspectThresholdSec
                    ),
                "train_qc_warning_level": quality.warningLevel.rawValue,
                "train_qc_warning_message": quality.warningMessage,
                "train_qc_duration_sec":
                    STPDCanonicalValue.double(quality.durationSec),
                "train_qc_firing_rate_hz":
                    STPDCanonicalValue.double(quality.firingRateHz),
                "train_qc_raw_min_isi_sec":
                    STPDCanonicalValue.double(quality.rawMinISISec),
                "train_qc_min_valid_isi_sec":
                    STPDCanonicalValue.double(quality.minValidISISec),
                "train_qc_artifact_min_isi_sec":
                    STPDCanonicalValue.double(quality.artifactMinISISec),
                "train_qc_median_isi_sec":
                    STPDCanonicalValue.double(quality.medianISISec),
                "train_qc_max_isi_sec":
                    STPDCanonicalValue.double(quality.maxISISec),
                "train_qc_duplicate_timestamp_count":
                    String(quality.duplicateTimestampCount),
                "train_qc_zero_or_negative_isi_count":
                    String(quality.zeroOrNegativeISICount),
                "train_qc_zero_or_negative_timestamp_step_count":
                    String(quality.zeroOrNegativeTimestampStepCount),
                "train_qc_input_was_unsorted":
                    STPDCanonicalValue.bool(quality.inputWasUnsorted),
                "train_qc_input_nonmonotonic_step_count":
                    String(quality.inputNonmonotonicStepCount),
                "train_qc_input_duplicate_timestamp_step_count":
                    String(quality.inputDuplicateTimestampStepCount),
                "train_qc_input_zero_or_negative_step_count":
                    String(quality.inputZeroOrNegativeStepCount),
                "train_qc_dropped_duplicate_timestamp_count":
                    String(quality.droppedDuplicateTimestampCount),
                "train_qc_duplicate_timestamp_policy":
                    quality.duplicateTimestampPolicy.rawValue,
                "train_qc_artifact_isi_count":
                    String(quality.artifactISICount),
                "train_qc_artifact_fraction":
                    STPDCanonicalValue.double(quality.artifactFraction),
                "train_qc_refractory_suspect_isi_count":
                    String(quality.refractorySuspectISICount),
                "train_qc_refractory_suspect_fraction":
                    STPDCanonicalValue.double(quality.refractorySuspectFraction),
                "train_qc_valid_isi_count":
                    String(quality.validISICount),
                "train_qc_percentile_status": quality.percentileStatus,
            ])
        }
        return try table(.isiLabelsFinal, headers: headers, rows: materialized)
    }

    static func diagnosticsTable(
        identity: DetectionRunIdentity,
        candidates: [STPDCandidateRecord],
        events: [STPDEventRecord],
        supplied: [STPDCandidateDiagnosticInput],
        candidateUIDBySourceID: [String: String]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "stage_id",
            "source_candidate_id", "stage_name", "stage_ordinal", "evidence_kind",
            "status", "details", "event_uid", "automatic_source_id",
            "source_support_isi_indices", "source_semantic_track",
            "source_event_track_class", "source_label", "source_lock_level",
            "source_state_tonic_subtype", "source_score", "source_priority",
            "source_decision_path",
        ]
        var rows: [[String]] = candidates.map { record in
            let candidate = record.candidate
            let stageID = STPDStableIdentifier.make(
                prefix: "stage",
                domain: "stpd_candidate_diagnostic_stage_v1",
                components: [record.uid, "terminal_decision", "0"]
            )
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": record.uid,
                "stage_id": stageID,
                "source_candidate_id": candidate.id,
                "stage_name": "terminal_decision",
                "stage_ordinal": "0",
                "evidence_kind": "candidate_terminal",
                "status": candidate.selectionStatus,
                "details": candidate.decisionPath,
            ])
        }
        for diagnostic in supplied {
            guard let candidateUID = candidateUIDBySourceID[diagnostic.sourceCandidateID] else {
                throw STPDResultPackageError.invalidInput(
                    "candidate diagnostic references unknown candidate \(diagnostic.sourceCandidateID)"
                )
            }
            let stageID = STPDStableIdentifier.make(
                prefix: "stage",
                domain: "stpd_candidate_diagnostic_stage_v1",
                components: [
                    candidateUID,
                    diagnostic.stageName,
                    String(diagnostic.stageOrdinal),
                ]
            )
            rows.append(makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": candidateUID,
                "stage_id": stageID,
                "source_candidate_id": diagnostic.sourceCandidateID,
                "stage_name": diagnostic.stageName,
                "stage_ordinal": String(diagnostic.stageOrdinal),
                "evidence_kind": "candidate_diagnostic",
                "status": diagnostic.status,
                "details": diagnostic.details,
            ]))
        }
        for record in events {
            let normalizedSources = record.event.automaticEventSources.sorted {
                if $0.candidateID != $1.candidateID {
                    return $0.candidateID < $1.candidateID
                }
                if $0.annotationID != $1.annotationID {
                    return $0.annotationID < $1.annotationID
                }
                return $0.supportISIIndices.lexicographicallyPrecedes(
                    $1.supportISIIndices
                )
            }
            for (ordinal, source) in normalizedSources.enumerated() {
                guard let candidateUID = candidateUIDBySourceID[source.candidateID] else {
                    throw STPDResultPackageError.invalidInput(
                        "event source references unknown candidate \(source.candidateID)"
                    )
                }
                let stageID = STPDStableIdentifier.make(
                    prefix: "stage",
                    domain: "stpd_event_source_evidence_stage_v1",
                    components: [
                        candidateUID,
                        record.uid,
                        source.annotationID,
                        source.supportISIIndices.map(String.init).joined(separator: "|"),
                    ]
                )
                rows.append(makeRow(headers, values: [
                    "run_id": identity.runID,
                    "settings_digest": identity.settingsDigest,
                    "candidate_uid": candidateUID,
                    "stage_id": stageID,
                    "source_candidate_id": source.candidateID,
                    "stage_name": "event_source",
                    "stage_ordinal": String(ordinal),
                    "evidence_kind": "event_source",
                    "status": "projected",
                    "details": source.decisionPath,
                    "event_uid": record.uid,
                    "automatic_source_id": source.annotationID,
                    "source_support_isi_indices": STPDCanonicalValue.stringList(
                        source.supportISIIndices.map(String.init)
                    ),
                    "source_semantic_track": source.semanticTrack.rawValue,
                    "source_event_track_class": source.eventTrackClass,
                    "source_label": source.label.rawValue,
                    "source_lock_level": source.lockLevel.rawValue,
                    "source_state_tonic_subtype": source.stateTonicSubtype ?? "",
                    "source_score": STPDCanonicalValue.double(source.score),
                    "source_priority": String(source.priority),
                    "source_decision_path": source.decisionPath,
                ]))
            }
        }
        return try table(.candidateDiagnosticAudit, headers: headers, rows: rows)
    }

    static func manualAnnotationsTable(
        identity: DetectionRunIdentity,
        annotations: [ManualAnnotation],
        manualUIDByUUID: [UUID: String],
        linkedISIKeysByEvidenceUID: [String: [String]],
        isiUIDByKey: [String: String]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "annotation_id",
            "annotation_semantic_digest", "source_annotation_uuid",
            "train_id", "label", "polarity", "start_sec", "end_sec",
            "start_isi_index", "end_isi_index",
            "start_spike_array_index", "end_spike_array_index",
            "start_spike_ordinal", "end_spike_ordinal",
            "linked_isi_uids", "link_scope", "note",
            "annotator", "annotator_identity_source",
            "created_at", "updated_at",
            "created_at_unix_sec", "updated_at_unix_sec",
        ]
        let sorted = annotations.sorted {
            let lhs = manualAnnotationIdentityComponents($0)
            let rhs = manualAnnotationIdentityComponents($1)
            if lhs != rhs {
                return lhs.lexicographicallyPrecedes(rhs)
            }
            return $0.id.uuidString < $1.id.uuidString
        }
        let rows = try sorted.map { annotation -> [String] in
            guard let annotationID = manualUIDByUUID[annotation.id] else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation is missing its resolved authority identity"
                )
            }
            let linkedUIDs = try (linkedISIKeysByEvidenceUID[annotationID] ?? [])
                .map { key -> String in
                    guard let uid = isiUIDByKey[key] else {
                        throw STPDResultPackageError.invalidInput(
                            "manual annotation links an unknown public ISI"
                        )
                    }
                    return uid
                }
                .sorted()
            let spikeOnly =
                linkedUIDs.isEmpty &&
                annotation.startISIIndex == nil &&
                annotation.endISIIndex == nil &&
                annotation.startSpikeIndex != nil &&
                annotation.startSpikeIndex == annotation.endSpikeIndex
            let linkScope = !linkedUIDs.isEmpty
                ? "public_projection"
                : (spikeOnly ? "active_spike_only" : "inactive_or_superseded")
            let startSpikeOrdinal = try checkedManualSpikeOrdinal(
                annotation.startSpikeIndex,
                annotationID: annotationID,
                field: "start_spike_array_index"
            )
            let endSpikeOrdinal = try checkedManualSpikeOrdinal(
                annotation.endSpikeIndex,
                annotationID: annotationID,
                field: "end_spike_array_index"
            )
            var values = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "annotation_id": annotationID,
                "source_annotation_uuid": annotation.id.uuidString.lowercased(),
                "train_id": annotation.trainID,
                "label": annotation.label.rawValue,
                "polarity": annotation.polarity.rawValue,
                "start_sec": STPDCanonicalValue.double(annotation.normalizedStartSec),
                "end_sec": STPDCanonicalValue.double(annotation.normalizedEndSec),
                "start_isi_index": STPDCanonicalValue.int(annotation.startISIIndex),
                "end_isi_index": STPDCanonicalValue.int(annotation.endISIIndex),
                "start_spike_array_index":
                    STPDCanonicalValue.int(annotation.startSpikeIndex),
                "end_spike_array_index":
                    STPDCanonicalValue.int(annotation.endSpikeIndex),
                "start_spike_ordinal":
                    STPDCanonicalValue.int(startSpikeOrdinal),
                "end_spike_ordinal":
                    STPDCanonicalValue.int(endSpikeOrdinal),
                "linked_isi_uids": STPDCanonicalValue.stringList(linkedUIDs),
                "link_scope": linkScope,
                "note": annotation.note ?? "",
                "annotator": annotation.annotator ?? "",
                "annotator_identity_source":
                    annotation.annotatorIdentitySource?.rawValue ?? "",
                "created_at": STPDCanonicalValue.date(annotation.createdAt),
                "updated_at": STPDCanonicalValue.date(annotation.updatedAt),
                "created_at_unix_sec":
                    STPDCanonicalValue.double(annotation.createdAt.timeIntervalSince1970),
                "updated_at_unix_sec":
                    STPDCanonicalValue.double(annotation.updatedAt.timeIntervalSince1970),
            ]
            values["annotation_semantic_digest"] =
                manualAnnotationSemanticDigest(values: values)
            return makeRow(headers, values: values)
        }
        return try table(.manualAnnotations, headers: headers, rows: rows)
    }

    private static func checkedManualSpikeOrdinal(
        _ arrayIndex: Int?,
        annotationID: String,
        field: String
    ) throws -> Int? {
        guard let arrayIndex else { return nil }
        let (ordinal, overflow) = arrayIndex.addingReportingOverflow(1)
        guard !overflow else {
            throw STPDResultPackageError.invalidInput(
                "manual annotation \(annotationID) \(field) cannot be represented as a one-based spike ordinal"
            )
        }
        return ordinal
    }

    static func reviewStatusTable(
        identity: DetectionRunIdentity,
        reviews: [STPDCandidateReviewInput],
        candidateUIDBySourceID: [String: String],
        reviewUIDBySourceCandidateID: [String: String],
        linkedISIKeysByEvidenceUID: [String: [String]],
        isiUIDByKey: [String: String]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "review_uid",
            "source_candidate_id", "status", "reviewer", "reviewed_run_id",
            "linked_isi_uids", "link_scope", "note",
            "reviewed_at", "reviewed_at_unix_sec",
        ]
        let rows = try reviews.map { review -> [String] in
            guard let candidateUID = candidateUIDBySourceID[review.sourceCandidateID] else {
                throw STPDResultPackageError.invalidInput(
                    "candidate review references unknown candidate \(review.sourceCandidateID)"
                )
            }
            guard let reviewUID =
                    reviewUIDBySourceCandidateID[review.sourceCandidateID] else {
                throw STPDResultPackageError.invalidInput(
                    "candidate review is missing its resolved authority identity"
                )
            }
            let linkedUIDs = try (linkedISIKeysByEvidenceUID[reviewUID] ?? [])
                .map { key -> String in
                    guard let uid = isiUIDByKey[key] else {
                        throw STPDResultPackageError.invalidInput(
                            "candidate review links an unknown public ISI"
                        )
                    }
                    return uid
                }
                .sorted()
            let reviewedAtUnixSec = review.reviewedAt.map {
                $0.timeIntervalSince1970
            }
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": candidateUID,
                "review_uid": reviewUID,
                "source_candidate_id": review.sourceCandidateID,
                "status": review.status.rawValue,
                "reviewer": review.reviewer,
                "reviewed_run_id": review.reviewedRunID,
                "linked_isi_uids": STPDCanonicalValue.stringList(linkedUIDs),
                "link_scope": linkedUIDs.isEmpty
                    ? "non_authoritative"
                    : "public_projection",
                "note": review.note,
                "reviewed_at": STPDCanonicalValue.date(review.reviewedAt),
                "reviewed_at_unix_sec":
                    STPDCanonicalValue.double(reviewedAtUnixSec),
            ])
        }
        return try table(.reviewStatus, headers: headers, rows: rows)
    }

    static func taskEventsTable(
        identity: DetectionRunIdentity,
        events: [TaskEvent]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "task_event_uid", "source_event_id",
            "event_name", "event_time_sec", "source_column", "source_event_index",
            "trial_id", "source",
        ]
        var seenSourceEventIDs = Set<String>()
        var seenTrialIDs = Set<String>()
        let records = try events.map { event -> (event: TaskEvent, uid: String) in
            guard !event.id.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty,
                  !event.name.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty,
                  event.timeSec.isFinite,
                  !event.column.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty,
                  event.eventIndex > 0,
                  !event.trialID.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "task event contains an invalid identity, time, source column, row index, or trial"
                )
            }
            guard seenSourceEventIDs.insert(event.id).inserted,
                  seenTrialIDs.insert(event.trialID).inserted else {
                throw STPDResultPackageError.invalidInput(
                    "task event source ids and trial ids must each be unique"
                )
            }
            return (
                event,
                taskEventUID(
                    event,
                    datasetDigest: identity.datasetDigest
                )
            )
        }.sorted {
            let lhs = taskEventIdentityComponents(
                $0.event,
                datasetDigest: identity.datasetDigest
            )
            let rhs = taskEventIdentityComponents(
                $1.event,
                datasetDigest: identity.datasetDigest
            )
            if lhs != rhs {
                return lhs.lexicographicallyPrecedes(rhs)
            }
            return $0.uid < $1.uid
        }
        let rows = records.map { record in
            let event = record.event
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "task_event_uid": record.uid,
                "source_event_id": event.id,
                "event_name": event.name,
                "event_time_sec": STPDCanonicalValue.double(event.timeSec),
                "source_column": event.column,
                "source_event_index": String(event.eventIndex),
                "trial_id": event.trialID,
                "source": event.source,
            ])
        }
        return try table(.taskEvents, headers: headers, rows: rows)
    }

    static func hfsAuditTable(
        identity: DetectionRunIdentity,
        rows: [HFSBurstArbitrationAuditRow],
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate],
        dataset: SpikeDataset,
        finalEvents: [STPDEventRecord],
        sourceMode: STPDResultPackageSourceMode
    ) throws -> STPDResultTableData {
        let baseHeaders = [
            "run_id", "settings_digest", "audit_row_id", "hfs_candidate_uid",
            "hfs_root_lineage_uid", "hfs_root_reference_kind",
            "strongest_burst_candidate_uid",
            "strongest_long_burst_candidate_uid", "unresolved_candidate_ids",
            "package_final_event_count", "package_final_event_subtypes",
            "package_final_projection_differs_from_audit",
        ]
        let auditHeaders = HFSBurstArbitrationAuditRow.csvHeader.map { "audit_\($0)" }
        let headers = baseHeaders + auditHeaders
        let records = try rows.map { row -> (
            row: HFSBurstArbitrationAuditRow,
            fields: [String],
            identity: [String],
            rootLineageUID: String,
            packageEventSubtypes: [String]
        ) in
            try validateHFSRow(
                row,
                candidateUIDBySourceID: candidateUIDBySourceID,
                candidateBySourceID: candidateBySourceID,
                dataset: dataset
            )
            try validateHFSFinalCandidateSnapshot(
                row,
                candidateBySourceID: candidateBySourceID
            )
            let packageEventSubtypes = finalEvents
                .filter {
                    $0.event.trainID == row.trainID &&
                        (ClassicAnchorLabel(rawValue: $0.event.finalLabel)?
                            .isBurstEventFamily == true) &&
                        spansOverlap(
                            lhsStart: row.hfsStartISIIndex,
                            lhsEnd: row.hfsEndISIIndex,
                            rhsStart: $0.event.startISIIndex,
                            rhsEnd: $0.event.endISIIndex
                        )
                }
                .map { hfsBurstSubtype($0.event) }
                .sorted()
            if sourceMode == .automatic,
               packageEventSubtypes != row.finalSelectedEventSubtypes.sorted() {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) contradicts the automatic final-event projection"
                )
            }
            let rootLineageUID = try hfsRootLineageUID(
                row,
                datasetDigest: identity.datasetDigest,
                candidateBySourceID: candidateBySourceID
            )
            let fields = hfsAuditCanonicalFields(row)
            let identity = hfsAuditIdentityComponents(
                fields: fields,
                candidateUIDBySourceID: candidateUIDBySourceID,
                rootLineageUID: rootLineageUID
            )
            return (
                row,
                fields,
                identity,
                rootLineageUID,
                packageEventSubtypes
            )
        }
        var seenIdentities = Set<String>()
        for record in records {
            let key = compositeKey(record.identity)
            guard seenIdentities.insert(key).inserted else {
                throw STPDResultPackageError.invalidInput(
                    "duplicate HFS arbitration snapshot for \(record.row.hfsCandidateID)"
                )
            }
        }
        let sorted = records.sorted {
            let lhs = $0.identity
            let rhs = $1.identity
            if lhs != rhs {
                return lhs.lexicographicallyPrecedes(rhs)
            }
            return $0.row.id < $1.row.id
        }
        let materialized = sorted.map { record in
            let row = record.row
            let base = record.identity
            let auditRowID = STPDStableIdentifier.make(
                prefix: "hfs_audit",
                domain: "stpd_hfs_burst_audit_uid_v2",
                components: [identity.datasetDigest] + base
            )
            var values: [String: String] = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "audit_row_id": auditRowID,
                "hfs_candidate_uid": candidateUIDBySourceID[row.hfsCandidateID] ?? "",
                "hfs_root_lineage_uid": record.rootLineageUID,
                "hfs_root_reference_kind":
                    candidateUIDBySourceID[row.hfsRootCandidateID] == nil
                    ? "split_lineage"
                    : "candidate",
                "strongest_burst_candidate_uid":
                    row.strongestBurstCandidateID.flatMap { candidateUIDBySourceID[$0] } ?? "",
                "strongest_long_burst_candidate_uid":
                    row.strongestLongBurstCandidateID.flatMap {
                        candidateUIDBySourceID[$0]
                    } ?? "",
                "unresolved_candidate_ids": STPDCanonicalValue.stringList([]),
                "package_final_event_count":
                    String(record.packageEventSubtypes.count),
                "package_final_event_subtypes":
                    STPDCanonicalValue.stringList(record.packageEventSubtypes),
                "package_final_projection_differs_from_audit":
                    STPDCanonicalValue.bool(
                        record.packageEventSubtypes !=
                            row.finalSelectedEventSubtypes.sorted()
                    ),
            ]
            for (header, field) in zip(auditHeaders, record.fields) {
                values[header] = field
            }
            values["audit_audit_id"] = auditRowID
            return makeRow(headers, values: values)
        }
        return try table(.hfsBurstArbitrationAudit, headers: headers, rows: materialized)
    }

    static func consistencyTable(
        identity: DetectionRunIdentity,
        checks: [STPDConsistencyCheck]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "check_id", "status", "severity", "details",
        ]
        let rows = checks.map { check in
            makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "check_id": check.id,
                "status": check.status,
                "severity": check.severity,
                "details": check.details,
            ])
        }
        return try table(.resultConsistencyCheck, headers: headers, rows: rows)
    }

    static func manualAnnotationIdentityComponents(
        _ annotation: ManualAnnotation
    ) -> [String] {
        [
            annotation.trainID,
            annotation.label.rawValue,
            annotation.polarity.rawValue,
            STPDCanonicalValue.double(annotation.normalizedStartSec),
            STPDCanonicalValue.double(annotation.normalizedEndSec),
            STPDCanonicalValue.int(annotation.startISIIndex),
            STPDCanonicalValue.int(annotation.endISIIndex),
            STPDCanonicalValue.int(annotation.startSpikeIndex),
            STPDCanonicalValue.int(annotation.endSpikeIndex),
        ]
    }

    private static let manualAnnotationSemanticColumns = [
        "source_annotation_uuid",
        "train_id",
        "label",
        "polarity",
        "start_sec",
        "end_sec",
        "start_isi_index",
        "end_isi_index",
        "start_spike_array_index",
        "end_spike_array_index",
        "start_spike_ordinal",
        "end_spike_ordinal",
        "linked_isi_uids",
        "link_scope",
        "note",
        "annotator",
        "annotator_identity_source",
        "created_at",
        "updated_at",
        "created_at_unix_sec",
        "updated_at_unix_sec",
    ]

    static func manualAnnotationSemanticDigest(
        values: [String: String]
    ) -> String {
        STPDStableIdentifier.make(
            prefix: "manual_semantics",
            domain: "stpd_manual_annotation_semantics_v1",
            components: manualAnnotationSemanticColumns.map {
                values[$0] ?? ""
            }
        )
    }

    static func hfsAuditIdentityComponents(
        fields: [String],
        candidateUIDBySourceID: [String: String],
        rootLineageUID: String
    ) -> [String] {
        var identity = fields
        identity[0] = ""
        for index in [4, 16, 20] {
            let sourceID = identity[index]
            identity[index] = sourceID.isEmpty ? "" : candidateUIDBySourceID[sourceID] ?? ""
        }
        identity[5] = rootLineageUID
        identity[49] = hfsStableDecisionReason(identity[49])
        return identity
    }

    static func hfsStableDecisionReason(_ reason: String) -> String {
        let transientKeys = Set(["hfs_candidate", "suppressed_event_ids"])
        return reason
            .split(separator: ";", omittingEmptySubsequences: true)
            .map(String.init)
            .filter { token in
                let key = token.split(
                    separator: "=",
                    maxSplits: 1,
                    omittingEmptySubsequences: false
                ).first.map(String.init) ?? token
                return !transientKeys.contains(key)
            }
            .sorted()
            .joined(separator: ";")
    }

    static func hfsRootLineageUID(
        _ row: HFSBurstArbitrationAuditRow,
        datasetDigest: String,
        candidateBySourceID: [String: ClassicAnchorCandidate]
    ) throws -> String {
        let rootStart: Int
        let rootEnd: Int
        if let rootCandidate = candidateBySourceID[row.hfsRootCandidateID] {
            let start = min(rootCandidate.startISIIndex, rootCandidate.endISIIndex)
            let end = max(rootCandidate.startISIIndex, rootCandidate.endISIIndex)
            guard rootCandidate.trainID == row.trainID,
                  rootCandidate.trainName == row.trainName,
                  rootCandidate.finalLabel == .highFrequencySpiking,
                  start <= row.hfsStartISIIndex,
                  end >= row.hfsEndISIIndex else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) root candidate does not contain its HFS span"
                )
            }
            rootStart = start
            rootEnd = end
        } else {
            let childPrefix = row.hfsRootCandidateID + "::state-split::"
            guard !row.hfsRootCandidateID.isEmpty,
                  row.hfsCandidateID.hasPrefix(childPrefix) else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) has an unresolved root that is not its split lineage"
                )
            }
            let children = candidateBySourceID.values
                .filter { $0.id.hasPrefix(childPrefix) }
            guard !children.isEmpty,
                  children.contains(where: { $0.id == row.hfsCandidateID }) else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) has no normalized current candidate in its split lineage"
                )
            }
            var childStarts: [Int] = []
            var childEnds: [Int] = []
            for child in children {
                guard let declared = hfsSplitGeometry(
                    candidateID: child.id,
                    rootID: row.hfsRootCandidateID
                ) else {
                    throw STPDResultPackageError.invalidInput(
                        "HFS split candidate \(child.id) has malformed lineage geometry"
                    )
                }
                let start = min(child.startISIIndex, child.endISIIndex)
                let end = max(child.startISIIndex, child.endISIIndex)
                guard child.trainID == row.trainID,
                      child.trainName == row.trainName,
                      child.finalLabel == .highFrequencySpiking,
                      declared.start == start,
                      declared.end == end else {
                    throw STPDResultPackageError.invalidInput(
                        "HFS split candidate \(child.id) contradicts its root, train, label, or geometry"
                    )
                }
                childStarts.append(start)
                childEnds.append(end)
            }
            guard let start = childStarts.min(),
                  let end = childEnds.max(),
                  start <= row.hfsStartISIIndex,
                  end >= row.hfsEndISIIndex else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) lies outside its normalized split lineage"
                )
            }
            rootStart = start
            rootEnd = end
        }
        return STPDStableIdentifier.make(
            prefix: "hfs_lineage",
            domain: "stpd_hfs_root_lineage_uid_v2",
            components: [
                datasetDigest,
                row.trainID,
                ClassicAnchorLabel.highFrequencySpiking.rawValue,
                STPDCanonicalValue.int(rootStart),
                STPDCanonicalValue.int(rootEnd),
            ]
        )
    }

    static func hfsSplitGeometry(
        candidateID: String,
        rootID: String
    ) -> (start: Int, end: Int)? {
        let prefix = rootID + "::state-split::"
        guard !rootID.isEmpty, candidateID.hasPrefix(prefix) else {
            return nil
        }
        let suffix = candidateID.dropFirst(prefix.count)
        let parts = suffix.split(
            separator: "-",
            omittingEmptySubsequences: false
        )
        guard parts.count == 2,
              let start = Int(parts[0]),
              let end = Int(parts[1]),
              start >= 1,
              start <= end else {
            return nil
        }
        return (start, end)
    }

    static func spansOverlap(
        lhsStart: Int,
        lhsEnd: Int,
        rhsStart: Int,
        rhsEnd: Int
    ) -> Bool {
        max(min(lhsStart, lhsEnd), min(rhsStart, rhsEnd)) <=
            min(max(lhsStart, lhsEnd), max(rhsStart, rhsEnd))
    }

    static func hfsBurstSubtype(_ candidate: ClassicAnchorCandidate) -> String {
        switch candidate.finalLabel {
        case .burst:
            return "burst_i"
        case .possibleBurst:
            return candidate.arbitrationTrack == .event
                ? "burst_ii"
                : "possible_burst_review"
        case .highFrequencyBurst:
            return "hf_burst"
        case .longBurst:
            return "long_burst"
        default:
            return candidate.finalLabel.rawValue
        }
    }

    static func hfsBurstSubtype(_ event: ClassicAnchorEventAnnotation) -> String {
        switch event.label {
        case .burst:
            return "burst_i"
        case .possibleBurst:
            return event.semanticTrack == .event
                ? "burst_ii"
                : "possible_burst_review"
        case .highFrequencyBurst:
            return "hf_burst"
        case .longBurst:
            return "long_burst"
        default:
            return event.label.rawValue
        }
    }

    static func hfsBurstSubtype(_ event: STPDNormalizedEvent) -> String {
        guard let label = ClassicAnchorLabel(rawValue: event.finalLabel) else {
            return event.finalLabel
        }
        switch label {
        case .burst:
            return "burst_i"
        case .possibleBurst:
            return event.semanticTrack == ClassicAnchorSemanticTrack.event.rawValue
                ? "burst_ii"
                : "possible_burst_review"
        case .highFrequencyBurst:
            return "hf_burst"
        case .longBurst:
            return "long_burst"
        default:
            return label.rawValue
        }
    }

    static func strongestHFSProposal(
        in candidates: [ClassicAnchorCandidate]
    ) -> ClassicAnchorCandidate? {
        candidates.max { lhs, rhs in
            if lhs.priority != rhs.priority {
                return lhs.priority < rhs.priority
            }
            if lhs.score != rhs.score {
                return lhs.score < rhs.score
            }
            if lhs.nISI != rhs.nISI {
                return lhs.nISI < rhs.nISI
            }
            return lhs.id > rhs.id
        }
    }

    static func validateHFSFinalCandidateSnapshot(
        _ row: HFSBurstArbitrationAuditRow,
        candidateBySourceID: [String: ClassicAnchorCandidate]
    ) throws {
        guard let hfsCandidate = candidateBySourceID[row.hfsCandidateID] else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has no normalized HFS candidate"
            )
        }
        let finalEvents = candidateBySourceID.values.filter {
            $0.trainID == row.trainID &&
                $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .event &&
                $0.finalLabel.isBurstEventFamily &&
                spansOverlap(
                    lhsStart: row.hfsStartISIIndex,
                    lhsEnd: row.hfsEndISIIndex,
                    rhsStart: $0.startISIIndex,
                    rhsEnd: $0.endISIIndex
                )
        }
        let finalSubtypes = finalEvents.map(hfsBurstSubtype).sorted()
        let selectedBurstIICount = finalEvents.filter {
            $0.finalLabel == .possibleBurst && $0.arbitrationTrack == .event
        }.count
        let selectedLongBurstCount = finalEvents.filter {
            $0.finalLabel == .longBurst
        }.count
        guard row.allSelectedBurstEventCount == finalEvents.count,
              row.selectedBurstIICount == selectedBurstIICount,
              row.selectedLongBurstCount == selectedLongBurstCount,
              row.finalSelectedEventSubtypes.sorted() == finalSubtypes else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) final burst counts or subtypes " +
                    "contradict normalized terminal candidates"
            )
        }

        let hfsSelected =
            hfsCandidate.finalLabel == .highFrequencySpiking &&
            hfsCandidate.selectedForAuto
        let hasLongBurst = selectedLongBurstCount > 0
        let expectedDecision: HFSBurstArbitrationDecision
        if hfsSelected {
            if !finalEvents.isEmpty && row.suppressedBurstProposalCount > 0 {
                expectedDecision = .hfsSelectedWithMixedBurstOutcomes
            } else if hasLongBurst {
                expectedDecision = .hfsSelectedWithLongBurstConflict
            } else if !finalEvents.isEmpty {
                expectedDecision = .hfsSelectedWithBurstConflict
            } else if row.suppressedBurstProposalCount > 0 {
                expectedDecision = .hfsSelectedBurstProposalSuppressed
            } else if row.strongestBurstCandidateID != nil {
                expectedDecision = .hfsSelectedWithBurstConflict
            } else {
                expectedDecision = .hfsSelectedNoBurstWinner
            }
        } else if row.burstDominated && !finalEvents.isEmpty {
            expectedDecision = .burstSelectedHFSRejectedPacketDominance
        } else if hasLongBurst {
            expectedDecision = .longBurstSelectedHFSRejected
        } else if !finalEvents.isEmpty {
            expectedDecision = .burstSelectedHFSNotSelected
        } else if hfsCandidate.finalLabel == .reject ||
                    hfsCandidate.gateStatus.lowercased().contains("reject") {
            expectedDecision = .hfsRejectedNoBurstWinner
        } else {
            expectedDecision = .unresolvedReview
        }

        let expectedReview: Bool
        if row.burstPacketLike && !row.burstDominated {
            expectedReview = true
        } else if row.suppressedBurstProposalCount > 0 ||
                    row.strongestLongBurstCandidateID != nil ||
                    hasLongBurst {
            expectedReview = true
        } else {
            switch expectedDecision {
            case .hfsSelectedWithMixedBurstOutcomes,
                 .hfsSelectedWithLongBurstConflict,
                 .hfsSelectedWithBurstConflict,
                 .hfsSelectedBurstProposalSuppressed,
                 .hfsRejectedNoBurstWinner,
                 .unresolvedReview:
                expectedReview = true
            default:
                expectedReview = false
            }
        }
        let eventText = finalSubtypes.isEmpty
            ? "none"
            : finalSubtypes.joined(separator: "|")
        let reasonTokens = Set(
            row.decisionReason.split(separator: ";").map(String.init)
        )
        guard row.finalDecision == expectedDecision,
              row.requiresReview == expectedReview,
              reasonTokens.contains("decision=\(expectedDecision.rawValue)"),
              reasonTokens.contains("selected_event_subtypes=\(eventText)") else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) decision, review flag, or reason " +
                    "contradicts normalized terminal candidates"
            )
        }
    }

    static func hfsAuditCanonicalFields(_ row: HFSBurstArbitrationAuditRow) -> [String] {
        [
            "", row.pipelineStage, row.trainID, row.trainName,
            row.hfsCandidateID, row.hfsRootCandidateID,
            STPDCanonicalValue.int(row.hfsStartISIIndex),
            STPDCanonicalValue.int(row.hfsEndISIIndex),
            STPDCanonicalValue.int(row.conflictStartISIIndex),
            STPDCanonicalValue.int(row.conflictEndISIIndex),
            row.scoreScaleNote, STPDCanonicalValue.double(row.hfsRawScore),
            STPDCanonicalValue.int(row.hfsPriority),
            STPDCanonicalValue.bool(row.hfsSelectedForAuto),
            row.hfsSelectionStatus, row.hfsAcceptanceRoute ?? "",
            row.strongestBurstCandidateID ?? "", row.strongestBurstSubtype ?? "",
            STPDCanonicalValue.double(row.strongestBurstRawScore),
            STPDCanonicalValue.int(row.strongestBurstPriority),
            row.strongestLongBurstCandidateID ?? "",
            STPDCanonicalValue.double(row.longBurstRawScore),
            STPDCanonicalValue.int(row.longBurstPriority),
            STPDCanonicalValue.int(row.packetEvidenceEventCount),
            STPDCanonicalValue.int(row.packetCount),
            STPDCanonicalValue.double(row.packetCoverage),
            STPDCanonicalValue.int(row.allSelectedBurstEventCount),
            STPDCanonicalValue.int(row.selectedBurstIICount),
            STPDCanonicalValue.int(row.selectedLongBurstCount),
            STPDCanonicalValue.int(row.suppressedBurstProposalCount),
            STPDCanonicalValue.double(row.pauseLikeThresholdSec),
            STPDCanonicalValue.int(row.pauseLikeBreakCount),
            STPDCanonicalValue.int(row.pauseLikeBreakGroupCount),
            STPDCanonicalValue.double(row.pauseLikeBreakFraction),
            STPDCanonicalValue.double(row.seedBandLowerSec),
            STPDCanonicalValue.double(row.seedBandUpperSec),
            STPDCanonicalValue.double(row.bridgeBandUpperSec),
            STPDCanonicalValue.double(row.seedFraction),
            STPDCanonicalValue.double(row.bridgeFraction),
            STPDCanonicalValue.double(row.hfsShortFraction),
            STPDCanonicalValue.double(row.hfsBridgeFraction),
            STPDCanonicalValue.double(row.hfsLargeFraction),
            STPDCanonicalValue.double(row.hfsCV),
            STPDCanonicalValue.double(row.hfsLV),
            STPDCanonicalValue.bool(row.burstPacketLike),
            STPDCanonicalValue.bool(row.burstDominated),
            row.finalDecision.rawValue,
            STPDCanonicalValue.stringList(row.finalSelectedEventSubtypes.sorted()),
            STPDCanonicalValue.bool(row.requiresReview),
            row.decisionReason,
        ]
    }

    static func validateHFSRow(
        _ row: HFSBurstArbitrationAuditRow,
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate],
        dataset: SpikeDataset
    ) throws {
        try validateDirectFiniteDoubles(
            row,
            entity: "HFS audit row \(row.id)"
        )
        guard let train = dataset.trains.first(where: { $0.id == row.trainID }),
              train.name == row.trainName else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) references an unknown train or mismatched train name"
            )
        }
        let lastISIIndex = max(0, train.spikeCount - 1)
        guard row.hfsStartISIIndex >= 1,
              row.hfsStartISIIndex <= row.hfsEndISIIndex,
              row.hfsEndISIIndex <= lastISIIndex,
              row.conflictStartISIIndex >= 1,
              row.conflictStartISIIndex <= row.conflictEndISIIndex,
              row.conflictEndISIIndex <= lastISIIndex else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has invalid ISI geometry"
            )
        }
        let requiredFinite: [Double?] = [
            row.hfsRawScore,
            row.strongestBurstRawScore,
            row.longBurstRawScore,
            row.packetCoverage,
            row.pauseLikeThresholdSec,
            row.pauseLikeBreakFraction,
            row.seedBandLowerSec,
            row.seedBandUpperSec,
            row.bridgeBandUpperSec,
            row.seedFraction,
            row.bridgeFraction,
            row.hfsShortFraction,
            row.hfsBridgeFraction,
            row.hfsLargeFraction,
            row.hfsCV,
            row.hfsLV,
        ]
        guard requiredFinite.allSatisfy({ $0 == nil || $0?.isFinite == true }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) contains a non-finite numeric value"
            )
        }
        let counts = [
            row.packetEvidenceEventCount,
            row.packetCount,
            row.allSelectedBurstEventCount,
            row.selectedBurstIICount,
            row.selectedLongBurstCount,
            row.suppressedBurstProposalCount,
            row.pauseLikeBreakCount,
            row.pauseLikeBreakGroupCount,
        ]
        guard counts.allSatisfy({ $0 >= 0 }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) contains a negative evidence count"
            )
        }
        let fractions: [Double?] = [
            row.packetCoverage,
            row.pauseLikeBreakFraction,
            row.seedFraction,
            row.bridgeFraction,
            row.hfsShortFraction,
            row.hfsBridgeFraction,
            row.hfsLargeFraction,
        ]
        guard fractions.allSatisfy({
            $0 == nil || (($0 ?? 0) >= 0 && ($0 ?? 0) <= 1)
        }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) contains a fraction outside 0...1"
            )
        }
        guard [row.pauseLikeThresholdSec, row.seedBandLowerSec, row.seedBandUpperSec,
               row.bridgeBandUpperSec].allSatisfy({ $0 == nil || ($0 ?? 0) > 0 }),
              [row.hfsCV, row.hfsLV].allSatisfy({ $0 == nil || ($0 ?? 0) >= 0 }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) contains a non-positive threshold or negative variability metric"
            )
        }
        if let lower = row.seedBandLowerSec, let upper = row.seedBandUpperSec,
           lower > upper {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has an inverted seed band"
            )
        }
        if let seedUpper = row.seedBandUpperSec, let bridgeUpper = row.bridgeBandUpperSec,
           seedUpper > bridgeUpper {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has a bridge upper bound below its seed upper bound"
            )
        }
        // The audit row intentionally mixes immutable proposal evidence captured before
        // arbitration (score, priority, acceptance route, and geometry) with terminal
        // selection fields. The normalized terminal candidate must preserve the former
        // while agreeing with the latter; otherwise the package cannot reconstruct the
        // HFS decision from a single, internally consistent lineage.
        guard let hfsCandidate = candidateBySourceID[row.hfsCandidateID],
              hfsCandidate.trainID == row.trainID,
              hfsCandidate.trainName == row.trainName,
              hfsCandidate.finalLabel == .highFrequencySpiking,
              canonicalEqual(hfsCandidate.score, row.hfsRawScore),
              hfsCandidate.priority == row.hfsPriority,
              hfsCandidate.selectedForAuto == row.hfsSelectedForAuto,
              hfsCandidate.selectionStatus == row.hfsSelectionStatus,
              hfsCandidate.hfSpikingAcceptanceRoute == row.hfsAcceptanceRoute,
              min(hfsCandidate.startISIIndex, hfsCandidate.endISIIndex) ==
                row.hfsStartISIIndex,
              max(hfsCandidate.startISIIndex, hfsCandidate.endISIIndex) ==
                row.hfsEndISIIndex else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) does not match its normalized HFS candidate"
            )
        }
        guard row.conflictStartISIIndex <= row.hfsStartISIIndex,
              row.conflictEndISIIndex >= row.hfsEndISIIndex else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) conflict span does not contain its HFS span"
            )
        }
        let strongestBurstSnapshotPresence = [
            row.strongestBurstCandidateID?.isEmpty == false,
            row.strongestBurstSubtype?.isEmpty == false,
            row.strongestBurstRawScore != nil,
            row.strongestBurstPriority != nil,
        ]
        guard strongestBurstSnapshotPresence.allSatisfy({ $0 }) ||
                strongestBurstSnapshotPresence.allSatisfy({ !$0 }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has an incomplete strongest-burst snapshot"
            )
        }
        let strongestLongBurstSnapshotPresence = [
            row.strongestLongBurstCandidateID?.isEmpty == false,
            row.longBurstRawScore != nil,
            row.longBurstPriority != nil,
        ]
        guard strongestLongBurstSnapshotPresence.allSatisfy({ $0 }) ||
                strongestLongBurstSnapshotPresence.allSatisfy({ !$0 }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has an incomplete strongest-long-burst snapshot"
            )
        }
        let sourceIDs = [
            row.hfsCandidateID,
            row.strongestBurstCandidateID,
            row.strongestLongBurstCandidateID,
        ].compactMap { $0 }.filter { !$0.isEmpty }
        guard sourceIDs.allSatisfy({ candidateUIDBySourceID[$0] != nil }) else {
            let unresolved = sourceIDs.filter {
                candidateUIDBySourceID[$0] == nil
            }.sorted()
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) references unknown candidates " +
                    unresolved.joined(separator: "|")
            )
        }
        if let rootCandidate = candidateBySourceID[row.hfsRootCandidateID] {
            guard rootCandidate.trainID == row.trainID,
                  rootCandidate.trainName == row.trainName,
                  rootCandidate.finalLabel == .highFrequencySpiking,
                  spansOverlap(
                    lhsStart: row.hfsStartISIIndex,
                    lhsEnd: row.hfsEndISIIndex,
                    rhsStart: rootCandidate.startISIIndex,
                    rhsEnd: rootCandidate.endISIIndex
                  ) else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) root candidate is not an overlapping HFS candidate on its train"
                )
            }
        }
        let overlappingBurstProposals = candidateBySourceID.values.filter {
            $0.trainID == row.trainID &&
                $0.finalLabel.isBurstEventFamily &&
                $0.arbitrationTrack == .event &&
                spansOverlap(
                    lhsStart: row.hfsStartISIIndex,
                    lhsEnd: row.hfsEndISIIndex,
                    rhsStart: $0.startISIIndex,
                    rhsEnd: $0.endISIIndex
                )
        }
        let expectedStrongestBurst = strongestHFSProposal(
            in: overlappingBurstProposals
        )
        guard row.strongestBurstCandidateID == expectedStrongestBurst?.id else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) strongest-burst reference is not the strongest overlapping event-track burst proposal"
            )
        }
        let expectedStrongestLongBurst = strongestHFSProposal(
            in: overlappingBurstProposals.filter { $0.finalLabel == .longBurst }
        )
        guard row.strongestLongBurstCandidateID == expectedStrongestLongBurst?.id else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) strongest-long-burst reference is not the strongest overlapping long-burst proposal"
            )
        }
        if let strongestBurstCandidateID = row.strongestBurstCandidateID,
           let strongestBurst = candidateBySourceID[strongestBurstCandidateID],
           let strongestBurstSubtype = row.strongestBurstSubtype,
           let strongestBurstRawScore = row.strongestBurstRawScore,
           let strongestBurstPriority = row.strongestBurstPriority {
            guard strongestBurst.trainID == row.trainID,
                  strongestBurst.finalLabel.isBurstEventFamily,
                  strongestBurstSubtype == hfsBurstSubtype(strongestBurst),
                  canonicalEqual(strongestBurst.score, strongestBurstRawScore),
                  strongestBurst.priority == strongestBurstPriority else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) strongest-burst snapshot contradicts its candidate"
                )
            }
        }
        if let strongestLongBurstCandidateID = row.strongestLongBurstCandidateID,
           let strongestLongBurst = candidateBySourceID[strongestLongBurstCandidateID],
           let longBurstRawScore = row.longBurstRawScore,
           let longBurstPriority = row.longBurstPriority {
            guard strongestLongBurst.trainID == row.trainID,
                  strongestLongBurst.finalLabel == .longBurst,
                  canonicalEqual(strongestLongBurst.score, longBurstRawScore),
                  strongestLongBurst.priority == longBurstPriority else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) strongest-long-burst snapshot contradicts its candidate"
                )
            }
        }
        _ = try hfsRootLineageUID(
            row,
            datasetDigest: DetectionDatasetSnapshot.make(dataset: dataset).digest,
            candidateBySourceID: candidateBySourceID
        )
    }

    static func makeRow(
        _ headers: [String],
        values: [String: String]
    ) -> [String] {
        headers.map { values[$0] ?? "" }
    }
}

enum STPDResultPackageValidator {
    private struct ValidatedCandidateRecord {
        let uid: String
        let sourceID: String
        let trainID: String
        let startISIIndex: Int
        let endISIIndex: Int

        func contains(isiIndex: Int) -> Bool {
            min(startISIIndex, endISIIndex)
                ... max(startISIIndex, endISIIndex) ~= isiIndex
        }
    }

    private struct ValidatedReviewRecord {
        let uid: String
        let candidateUID: String
        let sourceCandidateID: String
        let status: STPDCandidateReviewStatus
        let reviewer: String
        let note: String
        let reviewedAt: Date?
        let reviewedRunID: String
        let linkedISIUIDs: Set<String>
        let linkScope: String

        var input: STPDCandidateReviewInput {
            STPDCandidateReviewInput(
                sourceCandidateID: sourceCandidateID,
                status: status,
                reviewer: reviewer,
                note: note,
                reviewedAt: reviewedAt,
                reviewedRunID: reviewedRunID
            )
        }
    }

    private struct ValidatedISIRecord {
        let uid: String
        let trainID: String
        let trainName: String
        let index: Int
        let automaticPattern: String
        let automaticSubtype: String
        let automaticCandidateUID: String
        let automaticSourceCandidateID: String
        let finalPattern: String
        let finalSubtype: String
        let finalSource: String
        let manualVetoSuppressed: Bool
        let reviewNote: String
    }

    private struct ValidatedManualRecord {
        let uid: String
        let annotation: ManualAnnotation
        let linkedISIUIDs: Set<String>
        let linkScope: String
    }

    private struct ValidatedManualAuthority {
        let recordsByUID: [String: ValidatedManualRecord]
        let activeSpikeOnlyUIDs: Set<String>
    }

    private struct RecomputedISIProjection: Equatable {
        let finalPattern: String
        let finalSubtype: String
        let finalSource: String
        let manualVetoSuppressed: Bool
        let reviewNote: String
    }

    private enum CandidateIdentityTableSource {
        case ledger
        case features
        case decisions
    }

    private struct CandidateIdentityColumn {
        let source: CandidateIdentityTableSource
        let name: String

        static func ledger(_ name: String) -> Self {
            Self(source: .ledger, name: name)
        }

        static func features(_ name: String) -> Self {
            Self(source: .features, name: name)
        }

        static func decisions(_ name: String) -> Self {
            Self(source: .decisions, name: name)
        }
    }

    /// Ordered scientific identity material mirrored from
    /// `STPDResultPackageBuilder.candidateIntrinsicIdentityComponents`.
    ///
    /// The validator deliberately reads these values from all three normalized diagnostic
    /// tables. A caller therefore cannot make a stale or coherently relabeled candidate UID
    /// authoritative merely by rewriting foreign keys.
    private static let candidateIntrinsicIdentityColumns: [CandidateIdentityColumn] = [
        .ledger("train_id"),
        .ledger("train_name"),
        .ledger("candidate_layer"),
        .ledger("candidate_class"),
        .ledger("final_label"),
        .ledger("gate_status"),
        .ledger("action"),
        .decisions("priority"),
        .ledger("selected_for_auto"),
        .ledger("selection_status"),
        .ledger("start_isi_index"),
        .ledger("end_isi_index"),
        .ledger("start_spike_ordinal"),
        .ledger("end_spike_ordinal"),
        .ledger("n_isi"),
        .ledger("n_valid_isi"),
        .ledger("n_spikes"),
        .ledger("anchor_family"),
        .ledger("anchor_lock_level"),
        .features("duration_sec"),
        .features("intra_q10_sec"),
        .features("intra_q40_sec"),
        .features("intra_q50_sec"),
        .features("intra_q90_sec"),
        .features("intra_q95_sec"),
        .features("max_intra_isi_sec"),
        .features("mean_intra_isi_sec"),
        .features("cv"),
        .features("cv2"),
        .features("lv"),
        .features("pre_gap_sec"),
        .features("post_gap_sec"),
        .features("pre_ratio_q90"),
        .features("post_ratio_q90"),
        .features("edge_contrast_min_q90"),
        .features("edge_contrast_geom_q90"),
        .features("score"),
        .features("anchor_band_lower_sec"),
        .features("anchor_band_upper_sec"),
        .features("anchor_band_source"),
        .features("anchor_contrast_min_required"),
        .features("anchor_contrast_geom_required"),
        .features("refractory_suspect_count"),
        .features("refractory_suspect_action"),
        .features("profile_seed_low_percentile"),
        .features("profile_seed_high_percentile"),
        .features("profile_seed_band_fraction"),
        .features("profile_seed_run_count"),
        .features("profile_max_seed_run_length"),
        .features("profile_median_isi_sec"),
        .features("profile_q10_isi_sec"),
        .features("profile_q25_isi_sec"),
        .features("profile_q90_isi_sec"),
        .features("profile_pause_fraction"),
        .features("profile_phenotype_prior"),
        .features("profile_bridge_upper_sec"),
        .features("profile_boundary_floor_sec"),
        .features("profile_boundary_floor_hard"),
        .features("profile_burst_contrast_s"),
        .features("profile_possible_contrast_s"),
        .features("hf_q80_sec"),
        .features("hf_q80_max_sec"),
        .features("hf_q90_max_sec"),
        .features("hf_short_upper_sec"),
        .features("hf_epoch_bridge_sec"),
        .features("hf_tolerated_gap_sec"),
        .features("hf_pattern_max_isi_sec"),
        .features("hf_pause_break_sec"),
        .features("hf_short_fraction"),
        .features("hf_q90_short_fraction"),
        .features("hf_bridge_fraction"),
        .features("hf_large_fraction"),
        .features("hf_tolerated_fraction"),
        .features("hf_max_consecutive_large_isi"),
        .features("hf_min_spikes_required"),
        .features("hf_acceptance_route"),
        .features("hf_burst_dominated"),
        .features("hf_embedded_burst_count"),
        .features("hf_embedded_burst_group_count"),
        .features("hf_embedded_burst_coverage"),
        .features("hf_burst_packet_like"),
        .features("hf_burst_packet_neighbor"),
        .features("suppressed_by_hf_state"),
        .features("suppressed_original_label"),
        .features("state_regularity_score"),
        .features("state_burst_seed_fraction"),
        .features("state_low_tail_fraction"),
        .features("state_local_stability_score"),
        .features("state_core_burst_run_length"),
        .decisions("state_tonic_subtype"),
        .features("state_continuity_authority_frozen"),
        .features("state_continuity_merge_terminal"),
        .decisions("state_high_frequency_subtype"),
        .features("state_train_percentile_median"),
        .features("state_local_percentile_median"),
        .features("state_local_percentile_q90"),
        .features("state_local_robust_z_median"),
        .features("state_local_robust_z_abs_q80"),
        .features("state_local_robust_z_q10"),
        .features("burst_seed_run_start_isi"),
        .features("burst_seed_run_end_isi"),
        .features("burst_seed_band_lower_sec"),
        .features("burst_seed_band_upper_sec"),
        .features("burst_bridge_band_upper_sec"),
        .features("burst_contrast_required"),
        .features("burst_possible_contrast_required"),
        .features("burst_required_gap_sec"),
        .features("burst_possible_required_gap_sec"),
        .features("burst_boundary_floor_sec"),
        .features("burst_boundary_floor_hard"),
        .features("burst_strict_boundary_pass"),
        .features("burst_possible_boundary_pass"),
        .features("burst_bridge_count_pass"),
        .features("burst_bridge_fraction_pass"),
        .features("burst_q90_bridge_pass"),
        .features("burst_size_label_before_review"),
        .features("threshold_mode"),
        .features("hard_threshold"),
        .features("hard_threshold_pattern"),
        .features("hard_burst_seed_upper_sec"),
        .features("hard_burst_bridge_upper_sec"),
        .features("hard_burst_core_isi_count"),
        .features("hard_threshold_source"),
        .features("local_background_q75_sec"),
        .features("local_compression_q90_ratio"),
        .features("event_local_median_sec"),
        .features("event_local_percentile_median"),
        .features("event_local_percentile_q90"),
        .features("event_local_robust_z_median"),
        .features("event_local_robust_z_abs_q80"),
        .features("event_local_robust_z_q10"),
        .decisions("semantic_track"),
        .decisions("event_track_class"),
        .decisions("audit_family"),
        .decisions("audit_subtype"),
        .decisions("audit_final_class"),
        .decisions("audit_review_status"),
        .decisions("audit_review_required"),
        .decisions("audit_confidence_tier"),
        .decisions("audit_uncertainty_reason"),
        .decisions("audit_long_burst_definition_status"),
        .decisions("failure_reason"),
        .decisions("candidate_diagnostic_class"),
    ]

    static func validate(
        identity: DetectionRunIdentity,
        sourceMode: STPDResultPackageSourceMode,
        tables: [STPDResultTable: STPDResultTableData],
        expectedISICount: Int,
        expectedTaskEvents: [TaskEvent] = [],
        expectedDatasetMetadata: DetectionDatasetMetadataSnapshot? = nil,
        expectedTrainIDs: Set<String>? = nil,
        expectedDataset: SpikeDataset? = nil,
        expectedQualitySettings: SpikeQualitySettings? = nil,
        expectedRun: ClassicAnchorDetectionRun? = nil,
        expectedCandidateReviews: [STPDCandidateReviewInput]? = nil,
        expectedManualAnnotations: [ManualAnnotation]? = nil,
        expectedCandidateDiagnostics: [STPDCandidateDiagnosticInput]? = nil
    ) throws -> [STPDConsistencyCheck] {
        guard (expectedDataset == nil) == (expectedQualitySettings == nil) else {
            throw STPDResultPackageError.invalidInput(
                "dataset-backed validation requires both the dataset and its QC settings"
            )
        }
        guard expectedRun == nil || expectedDataset != nil else {
            throw STPDResultPackageError.invalidInput(
                "sealed-run validation requires the detector input dataset"
            )
        }
        guard expectedManualAnnotations == nil || expectedDataset != nil else {
            throw STPDResultPackageError.invalidInput(
                "sealed manual-annotation validation requires the detector input dataset"
            )
        }
        guard expectedCandidateDiagnostics == nil || expectedRun != nil else {
            throw STPDResultPackageError.invalidInput(
                "sealed candidate-diagnostic validation requires the detector run"
            )
        }
        let tableSet = Set(tables.keys)
        let completeSet = Set(STPDResultTable.allCases)
        let preConsistencySet = completeSet.subtracting([.resultConsistencyCheck])
        guard tableSet == preConsistencySet || tableSet == completeSet else {
            let missing = completeSet.subtracting(tableSet).map(\.rawValue).sorted()
            let unexpected = tableSet.subtracting(completeSet).map(\.rawValue).sorted()
            throw STPDResultPackageError.invalidTable(
                table: "package",
                reason: "table set mismatch; missing=\(missing.joined(separator: "|")); " +
                    "unexpected=\(unexpected.joined(separator: "|"))"
            )
        }

        var checks: [STPDConsistencyCheck] = [
            pass(
                "required_table_set",
                details: "all required tables for this validation stage are present"
            )
        ]

        for table in tables.keys.sorted(by: { $0.rawValue < $1.rawValue }) {
            guard let data = tables[table] else {
                throw STPDResultPackageError.missingTable(table.rawValue)
            }
            try validateIdentity(identity, table: data)
            try validatePrimaryKey(table: data)
        }
        checks.append(pass("run_identity", details: "all rows carry the declared run identity"))
        checks.append(pass("primary_keys", details: "all table primary keys are unique and non-empty"))

        guard let metadata = tables[.runMetadata],
              metadata.rowCount == 1,
              metadata.value(row: 0, column: "source_mode") == sourceMode.rawValue else {
            throw STPDResultPackageError.invalidTable(
                table: STPDResultTable.runMetadata.rawValue,
                reason: "source_mode does not match the package input"
            )
        }
        checks.append(pass("source_mode", details: "metadata source mode is explicit and consistent"))

        if let expectedRun, let expectedDataset {
            try STPDResultPackageBuilder.validateDeclaredIdentity(
                identity,
                dataset: expectedDataset,
                run: expectedRun
            )
            try validateExactAuthorityTable(
                required(.parametersReport, in: tables),
                expected: STPDResultPackageBuilder.parametersTable(
                    identity: identity
                )
            )
            try validateExactAuthorityTable(
                required(.resolvedParameters, in: tables),
                expected: STPDResultPackageBuilder.resolvedParametersTable(
                    identity: identity,
                    run: expectedRun,
                    dataset: expectedDataset
                )
            )
            let sealedCandidates = try STPDResultPackageBuilder
                .candidateRecords(
                    candidates: expectedRun.candidates,
                    datasetDigest: identity.datasetDigest,
                    dataset: expectedDataset
                )
            let sealedPublicCandidates = sealedCandidates.filter {
                STPDResultPackageBuilder.isPublicCandidate($0.candidate)
            }
            let sealedCandidateUIDBySourceID = Dictionary(
                uniqueKeysWithValues: sealedCandidates.map {
                    ($0.candidate.id, $0.uid)
                }
            )
            try validateExactAuthorityTable(
                required(.candidateLedger, in: tables),
                expected: try STPDResultPackageBuilder.candidateLedgerTable(
                    identity: identity,
                    records: sealedPublicCandidates
                )
            )
            try validateExactAuthorityTable(
                required(.candidateFeatures, in: tables),
                expected: try STPDResultPackageBuilder.candidateFeaturesTable(
                    identity: identity,
                    records: sealedPublicCandidates,
                    candidateUIDBySourceID: sealedCandidateUIDBySourceID
                )
            )
            try validateExactAuthorityTable(
                required(.finalDecisions, in: tables),
                expected: try STPDResultPackageBuilder.finalDecisionsTable(
                    identity: identity,
                    records: sealedPublicCandidates
                )
            )
            try validateExactAuthorityTable(
                required(.candidateLedgerDiagnostic, in: tables),
                expected: try STPDResultPackageBuilder.candidateLedgerTable(
                    identity: identity,
                    records: sealedCandidates,
                    tableKind: .candidateLedgerDiagnostic
                )
            )
            try validateExactAuthorityTable(
                required(.candidateFeaturesDiagnostic, in: tables),
                expected: try STPDResultPackageBuilder.candidateFeaturesTable(
                    identity: identity,
                    records: sealedCandidates,
                    candidateUIDBySourceID: sealedCandidateUIDBySourceID,
                    tableKind: .candidateFeaturesDiagnostic
                )
            )
            try validateExactAuthorityTable(
                required(.finalDecisionsDiagnostic, in: tables),
                expected: try STPDResultPackageBuilder.finalDecisionsTable(
                    identity: identity,
                    records: sealedCandidates,
                    tableKind: .finalDecisionsDiagnostic
                )
            )
            checks.append(pass(
                "parameter_authority",
                details: "requested and resolved parameter tables exactly regenerate from the sealed invocation snapshot, run provenance, resolutions, and dataset QC"
            ))
            checks.append(pass(
                "candidate_population_authority",
                details: "public and diagnostic candidate populations and all normalized candidate semantics exactly regenerate from the sealed detector run"
            ))
        }

        let publicCandidateUIDs = try values(
            table: required(.candidateLedger, in: tables),
            column: "candidate_uid"
        )
        let featureUIDs = try values(
            table: required(.candidateFeatures, in: tables),
            column: "candidate_uid"
        )
        let decisionUIDs = try values(
            table: required(.finalDecisions, in: tables),
            column: "candidate_uid"
        )
        guard publicCandidateUIDs == featureUIDs,
              publicCandidateUIDs == decisionUIDs else {
            throw STPDResultPackageError.invalidTable(
                table: "candidate tables",
                reason: "candidate ledger, features, and decisions do not have one-to-one UID coverage"
            )
        }
        try validatePublicCandidateRows(
            ledger: required(.candidateLedger, in: tables),
            decisions: required(.finalDecisions, in: tables)
        )
        checks.append(pass(
            "candidate_one_to_one",
            details: "public ledger, feature, and decision candidate UID sets are identical and authority-bearing"
        ))

        let diagnosticCandidateUIDs = try values(
            table: required(.candidateLedgerDiagnostic, in: tables),
            column: "candidate_uid"
        )
        let diagnosticFeatureUIDs = try values(
            table: required(.candidateFeaturesDiagnostic, in: tables),
            column: "candidate_uid"
        )
        let diagnosticDecisionUIDs = try values(
            table: required(.finalDecisionsDiagnostic, in: tables),
            column: "candidate_uid"
        )
        guard diagnosticCandidateUIDs == diagnosticFeatureUIDs,
              diagnosticCandidateUIDs == diagnosticDecisionUIDs else {
            throw STPDResultPackageError.invalidTable(
                table: "diagnostic candidate tables",
                reason: "diagnostic ledger, features, and decisions do not have one-to-one UID coverage"
            )
        }
        try validateCandidateStableUIDs(
            identity: identity,
            ledger: required(.candidateLedgerDiagnostic, in: tables),
            features: required(.candidateFeaturesDiagnostic, in: tables),
            decisions: required(.finalDecisionsDiagnostic, in: tables)
        )
        checks.append(pass(
            "candidate_diagnostic_one_to_one",
            details: "diagnostic ledger, feature, and decision candidate UID sets are identical, preserve every candidate, and deterministically rederive each scientific UID"
        ))

        let diagnosticTable = try required(.candidateDiagnosticAudit, in: tables)
        let diagnosticRegistryUIDs = try validateDiagnosticCandidateRegistry(
            diagnosticTable
        )
        guard diagnosticCandidateUIDs == diagnosticRegistryUIDs else {
            throw STPDResultPackageError.invalidTable(
                table: "diagnostic candidate tables",
                reason: "normalized diagnostic candidate tables and the diagnostic audit registry do not represent the same candidate population"
            )
        }
        guard publicCandidateUIDs.isSubset(of: diagnosticCandidateUIDs) else {
            throw STPDResultPackageError.invalidTable(
                table: "candidate tables",
                reason: "a public candidate is absent from the all-candidate diagnostic registry"
            )
        }
        try validatePublicDiagnosticCandidateProjection(
            publicTable: required(.candidateLedger, in: tables),
            diagnosticTable: required(.candidateLedgerDiagnostic, in: tables)
        )
        try validatePublicDiagnosticCandidateProjection(
            publicTable: required(.candidateFeatures, in: tables),
            diagnosticTable: required(.candidateFeaturesDiagnostic, in: tables)
        )
        try validatePublicDiagnosticCandidateProjection(
            publicTable: required(.finalDecisions, in: tables),
            diagnosticTable: required(.finalDecisionsDiagnostic, in: tables)
        )
        checks.append(pass(
            "candidate_public_diagnostic_partition",
            details: "public candidates are authority-bearing, form a subset of diagnostics, and are byte-identical to their diagnostic rows"
        ))

        try validateCandidateForeignKeys(
            table: diagnosticTable,
            columns: ["candidate_uid"],
            candidates: diagnosticCandidateUIDs
        )
        try validateCandidateForeignKeys(
            table: required(.reviewStatus, in: tables),
            columns: ["candidate_uid"],
            candidates: diagnosticCandidateUIDs
        )
        try validateCandidateForeignKeys(
            table: required(.isiLabelsFinal, in: tables),
            columns: ["auto_candidate_uid"],
            candidates: publicCandidateUIDs
        )
        try validateCandidateForeignKeys(
            table: required(.candidateFeatures, in: tables),
            columns: ["hf_suppressor_candidate_uid"],
            candidates: diagnosticCandidateUIDs
        )
        try validateCandidateForeignKeys(
            table: required(.candidateFeaturesDiagnostic, in: tables),
            columns: ["hf_suppressor_candidate_uid"],
            candidates: diagnosticCandidateUIDs
        )
        try validateCandidateForeignKeys(
            table: required(.hfsBurstArbitrationAudit, in: tables),
            columns: [
                "hfs_candidate_uid",
                "strongest_burst_candidate_uid", "strongest_long_burst_candidate_uid",
            ],
            candidates: diagnosticCandidateUIDs
        )
        try validateDelimitedCandidateForeignKeys(
            table: required(.eventsFinal, in: tables),
            column: "source_candidate_uids",
            candidates: publicCandidateUIDs
        )
        checks.append(pass(
            "candidate_foreign_keys",
            details: "public projections resolve to Candidate_ledger and diagnostic/audit references resolve to the all-candidate registry"
        ))

        try validateCandidateCounts(
            metadata: metadata,
            publicCount: publicCandidateUIDs.count,
            diagnosticCount: diagnosticCandidateUIDs.count
        )
        checks.append(pass(
            "candidate_counts",
            details: "run metadata reports the public and diagnostic candidate populations exactly"
        ))

        let isiTable = try required(.isiLabelsFinal, in: tables)
        guard isiTable.rowCount == expectedISICount else {
            throw STPDResultPackageError.invalidTable(
                table: STPDResultTable.isiLabelsFinal.rawValue,
                reason: "expected \(expectedISICount) interval rows, got \(isiTable.rowCount)"
            )
        }
        if let expectedDataset {
            try validateExactISICoverage(
                identity: identity,
                table: isiTable,
                dataset: expectedDataset
            )
        }
        if let expectedRun, let expectedDataset {
            try validateAutomaticProjectionAuthority(
                identity: identity,
                table: isiTable,
                candidateTable: required(.candidateLedgerDiagnostic, in: tables),
                publicCandidateUIDs: publicCandidateUIDs,
                dataset: expectedDataset,
                run: expectedRun
            )
            checks.append(pass(
                "automatic_projection_authority",
                details: "every automatic ISI label, subtype, source candidate, and candidate UID independently regenerates from the sealed detector run"
            ))
        }
        checks.append(pass(
            "isi_complete_coverage",
            details: expectedDataset == nil
                ? "the declared number of ISI rows is present"
                : "every dataset ISI is present exactly once with sealed train, spike, time, and UID geometry"
        ))

        try validateTaskEvents(
            identity: identity,
            table: required(.taskEvents, in: tables),
            expected: expectedTaskEvents
        )
        checks.append(pass(
            "task_event_projection",
            details: "Task_events exactly and deterministically represents every dataset task/stimulus event"
        ))

        let eventTable = try required(.eventsFinal, in: tables)
        let eventUIDs = try values(table: eventTable, column: "event_uid")
        try validateEventDiagnosticForeignKeys(
            table: required(.candidateDiagnosticAudit, in: tables),
            events: eventUIDs
        )
        checks.append(pass(
            "event_source_evidence",
            details: "every event-source diagnostic resolves to a final event"
        ))

        try validateFinalEventProjection(
            identity: identity,
            eventTable: eventTable,
            isiTable: isiTable
        )
        checks.append(pass(
            "final_event_isi_consistency",
            details: "final events exactly partition the labeled ISIs with matching labels, geometry, subtype semantics, and review evidence"
        ))
        try validateReviewEvidence(
            identity: identity,
            sourceMode: sourceMode,
            manualTable: required(.manualAnnotations, in: tables),
            reviewTable: required(.reviewStatus, in: tables),
            candidateTable: required(.candidateLedgerDiagnostic, in: tables),
            eventTable: eventTable,
            isiTable: isiTable,
            expectedTrainIDs: expectedTrainIDs,
            expectedDataset: expectedDataset,
            expectedQualitySettings: expectedQualitySettings,
            expectedCandidateReviews: expectedCandidateReviews,
            expectedManualAnnotations: expectedManualAnnotations,
            publicCandidateUIDs: publicCandidateUIDs
        )
        checks.append(pass(
            "review_authority",
            details: "review evidence, ISI links, and authority flags are causally closed"
        ))
        if let expectedRun, let expectedDataset {
            try validateEventSourceAuthority(
                identity: identity,
                eventTable: eventTable,
                diagnosticTable: diagnosticTable,
                isiTable: isiTable,
                manualTable: required(.manualAnnotations, in: tables),
                reviewTable: required(.reviewStatus, in: tables),
                candidateTable: required(
                    .candidateLedgerDiagnostic,
                    in: tables
                ),
                hfsTable: required(.hfsBurstArbitrationAudit, in: tables),
                dataset: expectedDataset,
                run: expectedRun,
                sourceMode: sourceMode,
                expectedCandidateReviews: expectedCandidateReviews,
                expectedManualAnnotations: expectedManualAnnotations,
                expectedCandidateDiagnostics: expectedCandidateDiagnostics
            )
            checks.append(pass(
                "event_source_authority",
                details: "event source candidates, automatic support, source annotations, diagnostics, and stage UIDs exactly regenerate from the sealed detector run plus validated manual/review authority"
            ))
        }

        try validateQCColumns(
            isiTable,
            resolvedParameters: required(.resolvedParameters, in: tables),
            expectedDataset: expectedDataset,
            expectedSettings: expectedQualitySettings
        )
        checks.append(pass(
            "isi_qc_provenance",
            details: "authoritative ISI classifications, thresholds, and train QC snapshots are internally consistent"
        ))

        try validateRunMetadata(
            identity: identity,
            sourceMode: sourceMode,
            metadata: metadata,
            expectedDatasetMetadata: expectedDatasetMetadata,
            tables: tables
        )
        checks.append(pass(
            "run_metadata",
            details: "run metadata exactly matches the sealed run identity, dataset snapshot, and materialized table populations"
        ))

        let sortedChecks = checks.sorted { $0.id < $1.id }
        if let consistencyTable = tables[.resultConsistencyCheck] {
            try validateConsistencyTable(consistencyTable, expected: sortedChecks)
        }
        return sortedChecks
    }

    private static func required(
        _ table: STPDResultTable,
        in tables: [STPDResultTable: STPDResultTableData]
    ) throws -> STPDResultTableData {
        guard let data = tables[table] else {
            throw STPDResultPackageError.missingTable(table.rawValue)
        }
        return data
    }

    private static func validateExactAuthorityTable(
        _ actual: STPDResultTableData,
        expected: STPDResultTableData
    ) throws {
        guard actual.contract == expected.contract,
              actual.headers == expected.headers,
              actual.columnDefinitions == expected.columnDefinitions,
              actual.rows == expected.rows else {
            throw STPDResultPackageError.invalidTable(
                table: actual.contract.table.rawValue,
                reason: "table does not exactly regenerate from sealed detector authority"
            )
        }
    }

    private static func validateAutomaticProjectionAuthority(
        identity: DetectionRunIdentity,
        table: STPDResultTableData,
        candidateTable: STPDResultTableData,
        publicCandidateUIDs: Set<String>,
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun
    ) throws {
        let candidatesByUID = try validatedCandidateRecords(candidateTable)
        let candidateUIDBySourceID = Dictionary(
            uniqueKeysWithValues: candidatesByUID.values.map {
                ($0.sourceID, $0.uid)
            }
        )
        let actualByUID = try validatedISIRecords(
            identity: identity,
            table
        )
        let automaticEvents = run.eventAnnotations(
            in: dataset,
            tracks: [.event, .gap, .state]
        )
        let expectedRows = ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: automaticEvents,
            projectionsByTrain: [:]
        )
        .filter { $0.isiIndex > 0 }

        guard expectedRows.count == actualByUID.count else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "automatic ISI projection row count differs from the sealed detector run"
            )
        }

        for expected in expectedRows {
            let uid = STPDResultPackageBuilder.stableISIUID(
                datasetDigest: identity.datasetDigest,
                trainID: expected.trainID,
                isiIndex: expected.isiIndex
            )
            guard let actual = actualByUID[uid] else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "sealed automatic projection ISI \(uid) is missing"
                )
            }
            let expectedSourceID = expected.autoCandidateID
            let expectedCandidateUID: String
            if expectedSourceID.isEmpty {
                expectedCandidateUID = ""
            } else {
                guard let uid = candidateUIDBySourceID[expectedSourceID],
                      publicCandidateUIDs.contains(uid) else {
                    throw STPDResultPackageError.invalidTable(
                        table: candidateTable.contract.table.rawValue,
                        reason: "sealed automatic source \(expectedSourceID) is absent from public candidate authority"
                    )
                }
                expectedCandidateUID = uid
            }
            guard actual.automaticPattern == expected.autoPattern,
                  actual.automaticSubtype == expected.autoSubtype,
                  actual.automaticSourceCandidateID == expectedSourceID,
                  actual.automaticCandidateUID == expectedCandidateUID else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "automatic projection for ISI \(uid) contradicts the sealed detector run"
                )
            }
        }
    }

    private static func validateEventSourceAuthority(
        identity: DetectionRunIdentity,
        eventTable: STPDResultTableData,
        diagnosticTable: STPDResultTableData,
        isiTable: STPDResultTableData,
        manualTable: STPDResultTableData,
        reviewTable: STPDResultTableData,
        candidateTable: STPDResultTableData,
        hfsTable: STPDResultTableData,
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        sourceMode: STPDResultPackageSourceMode,
        expectedCandidateReviews: [STPDCandidateReviewInput]?,
        expectedManualAnnotations: [ManualAnnotation]?,
        expectedCandidateDiagnostics: [STPDCandidateDiagnosticInput]?
    ) throws {
        let candidates = try STPDResultPackageBuilder.candidateRecords(
            candidates: run.candidates,
            datasetDigest: identity.datasetDigest,
            dataset: dataset
        )
        let candidateUIDBySourceID = Dictionary(
            uniqueKeysWithValues: candidates.map {
                ($0.candidate.id, $0.uid)
            }
        )
        let candidateBySourceID = Dictionary(
            uniqueKeysWithValues: candidates.map {
                ($0.candidate.id, $0.candidate)
            }
        )
        let finalRows = try reviewedISIRows(
            identity: identity,
            table: isiTable
        )
        let isiRecordsByUID = try validatedISIRecords(
            identity: identity,
            isiTable
        )
        let manualAuthority = try validateManualRows(
            identity: identity,
            table: manualTable,
            isiRecordsByUID: isiRecordsByUID,
            expectedTrainIDs: Set(dataset.trains.map(\.id)),
            expectedDataset: dataset
        )
        let validatedReviews = try validateReviewRows(
            identity: identity,
            table: reviewTable,
            candidateTable: candidateTable
        )
        let candidateReviews = expectedCandidateReviews
            ?? validatedReviews.values
                .sorted { $0.uid < $1.uid }
                .map(\.input)
        let manualAnnotations: [ManualAnnotation]
        if let expectedManualAnnotations {
            manualAnnotations = try STPDResultPackageBuilder
                .resolvedManualAnnotations(
                    expectedManualAnnotations,
                    dataset: dataset
                )
        } else {
            manualAnnotations = manualAuthority.recordsByUID.values
                .sorted { $0.uid < $1.uid }
                .map(\.annotation)
        }
        let projectedInput = try STPDResultPackageInput.snapshot(
            dataset: dataset,
            run: run,
            manualAnnotations: manualAnnotations,
            candidateReviews: candidateReviews
        )
        let expectedEvents = try STPDResultPackageBuilder.eventRecords(
            automaticEvents: projectedInput.finalEvents,
            finalISIRows: finalRows,
            datasetDigest: identity.datasetDigest,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID,
            dataset: dataset,
            evidenceUIDsByISIKey: [:],
            changedISIKeys: []
        )
        let expectedEventTable = try STPDResultPackageBuilder.eventsTable(
            identity: identity,
            records: expectedEvents,
            evidenceUIDsByISIKey: [:],
            changedISIKeys: []
        )
        let reviewDerivedEventColumns = Set([
            "review_evidence_uids",
            "review_evidence_present",
            "review_changed_projection",
        ])
        try validateExactColumnProjection(
            actual: eventTable,
            expected: expectedEventTable,
            columns: eventTable.headers.filter {
                !reviewDerivedEventColumns.contains($0)
            },
            authorityName: "sealed final-event projection"
        )

        let expectedDiagnostics = try STPDResultPackageBuilder.diagnosticsTable(
            identity: identity,
            candidates: candidates,
            events: expectedEvents,
            supplied: expectedCandidateDiagnostics ?? [],
            candidateUIDBySourceID: candidateUIDBySourceID
        )
        if expectedCandidateDiagnostics != nil {
            try validateExactAuthorityTable(
                diagnosticTable,
                expected: expectedDiagnostics
            )
        } else {
            try validateExactFilteredRows(
                actual: diagnosticTable,
                expected: expectedDiagnostics,
                filterColumn: "evidence_kind",
                filterValue: "candidate_terminal",
                authorityName: "sealed candidate-terminal diagnostics"
            )
            try validateExactFilteredRows(
                actual: diagnosticTable,
                expected: expectedDiagnostics,
                filterColumn: "evidence_kind",
                filterValue: "event_source",
                authorityName: "sealed event-source diagnostics"
            )
        }

        let expectedHFS = try STPDResultPackageBuilder.hfsAuditTable(
            identity: identity,
            rows: run.hfsBurstArbitrationAuditRows,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID,
            dataset: dataset,
            finalEvents: expectedEvents,
            sourceMode: sourceMode
        )
        try validateExactAuthorityTable(
            hfsTable,
            expected: expectedHFS
        )
    }

    private static func reviewedISIRows(
        identity: DetectionRunIdentity,
        table: STPDResultTableData
    ) throws -> [ReviewedISIExportRow] {
        _ = try validatedISIRecords(identity: identity, table)
        let trainIDIndex = try columnIndex("train_id", in: table)
        let trainNameIndex = try columnIndex("train_name", in: table)
        let spikeIndex = try columnIndex("right_spike_array_index", in: table)
        let timestampIndex = try columnIndex("timestamp_sec", in: table)
        let alignedTimestampIndex = try columnIndex(
            "aligned_timestamp_sec",
            in: table
        )
        let isiIndex = try columnIndex("isi_index", in: table)
        let isiSecIndex = try columnIndex("isi_sec", in: table)
        let autoPatternIndex = try columnIndex("auto_pattern", in: table)
        let autoSubtypeIndex = try columnIndex("auto_subtype", in: table)
        let autoCandidateIndex = try columnIndex(
            "auto_source_candidate_id",
            in: table
        )
        let finalPatternIndex = try columnIndex("final_pattern", in: table)
        let finalSubtypeIndex = try columnIndex("final_subtype", in: table)
        let finalSourceIndex = try columnIndex("final_source", in: table)
        let manualVetoIndex = try columnIndex(
            "manual_veto_suppressed",
            in: table
        )
        let reviewNoteIndex = try columnIndex("review_note", in: table)

        return try table.rows.map { row in
            ReviewedISIExportRow(
                trainID: row[trainIDIndex],
                trainName: row[trainNameIndex],
                spikeIndex: try parseInteger(
                    row[spikeIndex],
                    table: table,
                    column: "right_spike_array_index"
                ),
                timestampSec: try parseFiniteReal(
                    row[timestampIndex],
                    table: table,
                    column: "timestamp_sec"
                ),
                alignedTimestampSec: try parseFiniteReal(
                    row[alignedTimestampIndex],
                    table: table,
                    column: "aligned_timestamp_sec"
                ),
                isiIndex: try parseInteger(
                    row[isiIndex],
                    table: table,
                    column: "isi_index"
                ),
                isiSec: try parseFiniteReal(
                    row[isiSecIndex],
                    table: table,
                    column: "isi_sec"
                ),
                autoPattern: row[autoPatternIndex],
                autoSubtype: row[autoSubtypeIndex],
                autoCandidateID: row[autoCandidateIndex],
                finalPattern: row[finalPatternIndex],
                finalSubtype: row[finalSubtypeIndex],
                finalSource: row[finalSourceIndex],
                manualVetoSuppressed: try parseBoolean(
                    row[manualVetoIndex],
                    table: table,
                    column: "manual_veto_suppressed"
                ),
                reviewNote: row[reviewNoteIndex]
            )
        }
        .sorted {
            if $0.trainID != $1.trainID {
                return $0.trainID < $1.trainID
            }
            return $0.isiIndex < $1.isiIndex
        }
    }

    private static func validateExactColumnProjection(
        actual: STPDResultTableData,
        expected: STPDResultTableData,
        columns: [String],
        authorityName: String
    ) throws {
        let actualIndices = try columns.map {
            try columnIndex($0, in: actual)
        }
        let expectedIndices = try columns.map {
            try columnIndex($0, in: expected)
        }
        let actualRows = actual.rows.map { row in
            actualIndices.map { row[$0] }
        }
        .sorted(by: lexicographicallyPrecedes)
        let expectedRows = expected.rows.map { row in
            expectedIndices.map { row[$0] }
        }
        .sorted(by: lexicographicallyPrecedes)
        guard actualRows == expectedRows else {
            let firstDifference = zip(actualRows, expectedRows)
                .first { $0 != $1 }
            let actualDifference = firstDifference?.0
                ?? (actualRows.count > expectedRows.count
                    ? actualRows[expectedRows.count]
                    : [])
            let expectedDifference = firstDifference?.1
                ?? (expectedRows.count > actualRows.count
                    ? expectedRows[actualRows.count]
                    : [])
            throw STPDResultPackageError.invalidTable(
                table: actual.contract.table.rawValue,
                reason:
                    "\(authorityName) does not exactly regenerate; " +
                    "actual_count=\(actualRows.count); " +
                    "expected_count=\(expectedRows.count); " +
                    "first_actual=\(actualDifference); " +
                    "first_expected=\(expectedDifference)"
            )
        }
    }

    private static func validateExactFilteredRows(
        actual: STPDResultTableData,
        expected: STPDResultTableData,
        filterColumn: String,
        filterValue: String,
        authorityName: String
    ) throws {
        guard actual.headers == expected.headers else {
            throw STPDResultPackageError.invalidTable(
                table: actual.contract.table.rawValue,
                reason: "\(authorityName) column schema differs"
            )
        }
        let actualFilterIndex = try columnIndex(filterColumn, in: actual)
        let expectedFilterIndex = try columnIndex(filterColumn, in: expected)
        let actualRows = actual.rows
            .filter { $0[actualFilterIndex] == filterValue }
            .sorted(by: lexicographicallyPrecedes)
        let expectedRows = expected.rows
            .filter { $0[expectedFilterIndex] == filterValue }
            .sorted(by: lexicographicallyPrecedes)
        guard actualRows == expectedRows else {
            throw STPDResultPackageError.invalidTable(
                table: actual.contract.table.rawValue,
                reason: "\(authorityName) rows do not exactly regenerate"
            )
        }
    }

    private static func lexicographicallyPrecedes(
        _ lhs: [String],
        _ rhs: [String]
    ) -> Bool {
        lhs.lexicographicallyPrecedes(rhs)
    }

    private static func validateIdentity(
        _ identity: DetectionRunIdentity,
        table: STPDResultTableData
    ) throws {
        guard let runIndex = table.headers.firstIndex(of: "run_id"),
              let settingsIndex = table.headers.firstIndex(of: "settings_digest") else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "identity columns are unavailable"
            )
        }
        for row in table.rows
            where row[runIndex] != identity.runID ||
                row[settingsIndex] != identity.settingsDigest {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "row identity differs from the declared detector run"
            )
        }
    }

    private static func validatePrimaryKey(
        table: STPDResultTableData
    ) throws {
        let indices = table.contract.primaryKey.compactMap {
            table.headers.firstIndex(of: $0)
        }
        guard indices.count == table.contract.primaryKey.count else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "primary-key columns are unavailable"
            )
        }
        var seen = Set<String>()
        for row in table.rows {
            let parts = indices.map { row[$0] }
            guard parts.allSatisfy({ !$0.isEmpty }) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "primary-key value is empty"
                )
            }
            let key = parts.map { "\(Data($0.utf8).count):\($0)" }.joined()
            guard seen.insert(key).inserted else {
                throw STPDResultPackageError.duplicatePrimaryKey(
                    table: table.contract.table.rawValue,
                    key: parts.joined(separator: "|")
                )
            }
        }
    }

    private static func values(
        table: STPDResultTableData,
        column: String
    ) throws -> Set<String> {
        guard let index = table.headers.firstIndex(of: column) else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "missing column \(column)"
            )
        }
        return Set(table.rows.map { $0[index] })
    }

    private static func validatePublicDiagnosticCandidateProjection(
        publicTable: STPDResultTableData,
        diagnosticTable: STPDResultTableData
    ) throws {
        guard publicTable.headers == diagnosticTable.headers else {
            throw STPDResultPackageError.invalidTable(
                table: publicTable.contract.table.rawValue,
                reason: "public and diagnostic candidate schemas differ"
            )
        }
        let uidIndex = try columnIndex("candidate_uid", in: publicTable)
        let diagnosticRows = Dictionary(
            uniqueKeysWithValues: diagnosticTable.rows.map {
                ($0[uidIndex], $0)
            }
        )
        for row in publicTable.rows {
            let uid = row[uidIndex]
            guard diagnosticRows[uid] == row else {
                throw STPDResultPackageError.invalidTable(
                    table: publicTable.contract.table.rawValue,
                    reason: "public candidate \(uid) contradicts its diagnostic row"
                )
            }
        }
    }

    private static func validatePublicCandidateRows(
        ledger: STPDResultTableData,
        decisions: STPDResultTableData
    ) throws {
        let denialTerms = [
            "blocked",
            "unwritten",
            "not_selected",
            "not selected",
            "suppressed",
            "rejected",
            "audit_only",
        ]

        func validate(
            _ table: STPDResultTableData,
            authorityColumns: [String]
        ) throws {
            let uidIndex = try columnIndex("candidate_uid", in: table)
            let labelIndex = try columnIndex("final_label", in: table)
            let selectedIndex = try columnIndex("selected_for_auto", in: table)
            let authorityIndices = try authorityColumns.map {
                try columnIndex($0, in: table)
            }
            for row in table.rows {
                let uid = row[uidIndex]
                let selected = try parseBoolean(
                    row[selectedIndex],
                    table: table,
                    column: "selected_for_auto"
                )
                let normalizedLabel = row[labelIndex]
                    .trimmingCharacters(in: .whitespacesAndNewlines)
                    .lowercased()
                let denied = authorityIndices.contains { index in
                    let value = row[index]
                        .trimmingCharacters(in: .whitespacesAndNewlines)
                        .lowercased()
                    return denialTerms.contains { value.contains($0) }
                }
                guard selected,
                      normalizedLabel != ClassicAnchorLabel.reject.rawValue,
                      normalizedLabel != ClassicAnchorLabel.profile.rawValue,
                      !denied else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "candidate \(uid) is diagnostic-only but appears in a public authority table"
                    )
                }
            }
        }

        try validate(
            ledger,
            authorityColumns: ["gate_status", "action", "selection_status"]
        )
        try validate(
            decisions,
            authorityColumns: ["gate_status", "action", "selection_status"]
        )
    }

    private static func validateCandidateStableUIDs(
        identity: DetectionRunIdentity,
        ledger: STPDResultTableData,
        features: STPDResultTableData,
        decisions: STPDResultTableData
    ) throws {
        func rowsByUID(
            _ table: STPDResultTableData
        ) throws -> [String: [String: String]] {
            let uidIndex = try columnIndex("candidate_uid", in: table)
            var result: [String: [String: String]] = [:]
            for row in table.rows {
                let uid = row[uidIndex]
                let values = Dictionary(
                    uniqueKeysWithValues: zip(table.headers, row)
                )
                guard result.updateValue(values, forKey: uid) == nil else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "candidate \(uid) occurs more than once"
                    )
                }
            }
            return result
        }

        let ledgerRows = try rowsByUID(ledger)
        let featureRows = try rowsByUID(features)
        let decisionRows = try rowsByUID(decisions)
        let candidateUIDs = Set(ledgerRows.keys)
        guard candidateUIDs == Set(featureRows.keys),
              candidateUIDs == Set(decisionRows.keys) else {
            throw STPDResultPackageError.invalidTable(
                table: "diagnostic candidate tables",
                reason: "candidate UID rederivation requires identical row populations"
            )
        }

        func intrinsicComponents(for uid: String) throws -> [String] {
            guard let ledgerRow = ledgerRows[uid],
                  let featureRow = featureRows[uid],
                  let decisionRow = decisionRows[uid] else {
                throw STPDResultPackageError.invalidTable(
                    table: "diagnostic candidate tables",
                    reason: "candidate \(uid) is missing identity material"
                )
            }
            return try candidateIntrinsicIdentityColumns.map { column in
                let row: [String: String]
                switch column.source {
                case .ledger:
                    row = ledgerRow
                case .features:
                    row = featureRow
                case .decisions:
                    row = decisionRow
                }
                guard let value = row[column.name] else {
                    throw STPDResultPackageError.invalidTable(
                        table: "diagnostic candidate tables",
                        reason: "candidate identity column \(column.name) is missing"
                    )
                }
                return value
            }
        }

        var intrinsicUIDByCandidateUID: [String: String] = [:]
        var componentsByCandidateUID: [String: [String]] = [:]
        for uid in candidateUIDs {
            let components = try intrinsicComponents(for: uid)
            componentsByCandidateUID[uid] = components
            intrinsicUIDByCandidateUID[uid] = STPDStableIdentifier.make(
                prefix: "cand_intrinsic",
                domain: "stpd_candidate_intrinsic_uid_v1",
                components: [identity.datasetDigest] + components
            )
        }

        for uid in candidateUIDs {
            guard let components = componentsByCandidateUID[uid],
                  let featureRow = featureRows[uid] else {
                throw STPDResultPackageError.invalidTable(
                    table: "diagnostic candidate tables",
                    reason: "candidate \(uid) has incomplete identity material"
                )
            }
            let suppressorUID = featureRow["hf_suppressor_candidate_uid"] ?? ""
            let suppressorIntrinsicUID: String
            if suppressorUID.isEmpty {
                suppressorIntrinsicUID = ""
            } else if let resolved = intrinsicUIDByCandidateUID[suppressorUID] {
                suppressorIntrinsicUID = resolved
            } else {
                throw STPDResultPackageError.invalidTable(
                    table: features.contract.table.rawValue,
                    reason: "candidate \(uid) has an unresolved HF suppressor identity"
                )
            }
            let expectedUID = STPDStableIdentifier.make(
                prefix: "cand",
                domain: "stpd_candidate_uid_v2",
                components: [identity.datasetDigest] + components
                    + [suppressorIntrinsicUID]
            )
            guard uid == expectedUID else {
                throw STPDResultPackageError.invalidTable(
                    table: ledger.contract.table.rawValue,
                    reason: "candidate \(uid) does not match its deterministic scientific identity"
                )
            }
        }
    }

    private static func validateDiagnosticCandidateRegistry(
        _ table: STPDResultTableData
    ) throws -> Set<String> {
        let candidateIndex = try columnIndex("candidate_uid", in: table)
        let evidenceKindIndex = try columnIndex("evidence_kind", in: table)
        var rowCountByCandidate: [String: Int] = [:]
        var terminalCountByCandidate: [String: Int] = [:]
        for row in table.rows {
            let candidateUID = row[candidateIndex]
            guard !candidateUID.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "diagnostic row has an empty candidate UID"
                )
            }
            rowCountByCandidate[candidateUID, default: 0] += 1
            if row[evidenceKindIndex] == "candidate_terminal" {
                terminalCountByCandidate[candidateUID, default: 0] += 1
            }
        }
        let candidateUIDs = Set(rowCountByCandidate.keys)
        for candidateUID in candidateUIDs {
            guard terminalCountByCandidate[candidateUID] == 1 else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "candidate \(candidateUID) must have exactly one terminal diagnostic row"
                )
            }
        }
        return candidateUIDs
    }

    private static func validateCandidateCounts(
        metadata: STPDResultTableData,
        publicCount: Int,
        diagnosticCount: Int
    ) throws {
        let publicIndex = try columnIndex("candidate_count", in: metadata)
        let diagnosticIndex = try columnIndex(
            "diagnostic_candidate_count",
            in: metadata
        )
        let declaredPublic = try parseInteger(
            metadata.rows[0][publicIndex],
            table: metadata,
            column: "candidate_count"
        )
        let declaredDiagnostic = try parseInteger(
            metadata.rows[0][diagnosticIndex],
            table: metadata,
            column: "diagnostic_candidate_count"
        )
        guard declaredPublic == publicCount,
              declaredDiagnostic == diagnosticCount,
              declaredPublic <= declaredDiagnostic else {
            throw STPDResultPackageError.invalidTable(
                table: metadata.contract.table.rawValue,
                reason: "candidate population counts contradict public and diagnostic tables"
            )
        }
    }

    private static func validateTaskEvents(
        identity: DetectionRunIdentity,
        table: STPDResultTableData,
        expected: [TaskEvent]
    ) throws {
        guard identity.taskEventCount == expected.count,
              table.rowCount == expected.count else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "task-event row count contradicts the detector input identity"
            )
        }
        let uidIndex = try columnIndex("task_event_uid", in: table)
        let sourceIDIndex = try columnIndex("source_event_id", in: table)
        let nameIndex = try columnIndex("event_name", in: table)
        let timeIndex = try columnIndex("event_time_sec", in: table)
        let columnIndexValue = try columnIndex("source_column", in: table)
        let eventIndexValue = try columnIndex("source_event_index", in: table)
        let trialIndex = try columnIndex("trial_id", in: table)
        let sourceIndex = try columnIndex("source", in: table)

        var expectedByUID: [String: TaskEvent] = [:]
        var seenSourceEventIDs = Set<String>()
        var seenTrialIDs = Set<String>()
        for event in expected {
            guard !event.id.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty,
                  !event.name.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty,
                  event.timeSec.isFinite,
                  !event.column.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty,
                  event.eventIndex > 0,
                  !event.trialID.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "expected task event has invalid identity or geometry"
                )
            }
            guard seenSourceEventIDs.insert(event.id).inserted,
                  seenTrialIDs.insert(event.trialID).inserted else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "expected task event source ids and trial ids must each be unique"
                )
            }
            let uid = STPDResultPackageBuilder.taskEventUID(
                event,
                datasetDigest: identity.datasetDigest
            )
            guard expectedByUID.updateValue(event, forKey: uid) == nil else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "task events contain a duplicate scientific identity"
                )
            }
        }

        var seen = Set<String>()
        for row in table.rows {
            let uid = row[uidIndex]
            let parsedTime = try parseFiniteReal(
                row[timeIndex],
                table: table,
                column: "event_time_sec"
            )
            guard let parsedEventIndex = Int(row[eventIndexValue]) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "task-event row has an invalid source event index"
                )
            }
            let rowEvent = TaskEvent(
                id: row[sourceIDIndex],
                name: row[nameIndex],
                timeSec: parsedTime,
                column: row[columnIndexValue],
                eventIndex: parsedEventIndex,
                trialID: row[trialIndex],
                source: row[sourceIndex]
            )
            let rederivedUID = STPDResultPackageBuilder.taskEventUID(
                rowEvent,
                datasetDigest: identity.datasetDigest
            )
            guard let event = expectedByUID[uid],
                  seen.insert(uid).inserted,
                  rederivedUID == uid,
                  row[sourceIDIndex] == event.id,
                  row[nameIndex] == event.name,
                  row[timeIndex] == STPDCanonicalValue.double(event.timeSec),
                  row[columnIndexValue] == event.column,
                  row[eventIndexValue] == String(event.eventIndex),
                  row[trialIndex] == event.trialID,
                  row[sourceIndex] == event.source,
                  parsedTime.bitPattern == event.timeSec.bitPattern else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "task-event row does not match its deterministic detector-input projection"
                )
            }
        }
        guard seen == Set(expectedByUID.keys) else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "task-event table does not exactly cover the detector input"
            )
        }
    }

    private static func validateCandidateForeignKeys(
        table: STPDResultTableData,
        columns: [String],
        candidates: Set<String>
    ) throws {
        for column in columns {
            guard let index = table.headers.firstIndex(of: column) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "missing candidate foreign-key column \(column)"
                )
            }
            for row in table.rows {
                let value = row[index]
                if !value.isEmpty, !candidates.contains(value) {
                    throw STPDResultPackageError.foreignKeyViolation(
                        table: table.contract.table.rawValue,
                        column: column,
                        value: value
                    )
                }
            }
        }
    }

    private static func validateDelimitedCandidateForeignKeys(
        table: STPDResultTableData,
        column: String,
        candidates: Set<String>
    ) throws {
        guard let index = table.headers.firstIndex(of: column) else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "missing candidate foreign-key column \(column)"
            )
        }
        for row in table.rows {
            guard let values = STPDCanonicalValue.parseStringList(row[index]) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "\(column) is not a canonical string list"
                )
            }
            for value in values where !candidates.contains(value) {
                throw STPDResultPackageError.foreignKeyViolation(
                    table: table.contract.table.rawValue,
                    column: column,
                    value: value
                )
            }
        }
    }

    private static func validateEventDiagnosticForeignKeys(
        table: STPDResultTableData,
        events: Set<String>
    ) throws {
        let kindIndex = try columnIndex("evidence_kind", in: table)
        let eventIndex = try columnIndex("event_uid", in: table)
        for row in table.rows where row[kindIndex] == "event_source" {
            let eventUID = row[eventIndex]
            guard !eventUID.isEmpty, events.contains(eventUID) else {
                throw STPDResultPackageError.foreignKeyViolation(
                    table: table.contract.table.rawValue,
                    column: "event_uid",
                    value: eventUID
                )
            }
        }
    }

    private struct FinalISIProjectionRow {
        let uid: String
        let trainID: String
        let trainName: String
        let index: Int
        let timestampSec: Double
        let alignedTimestampSec: Double
        let isiSec: Double
        let finalPattern: String
        let finalSubtype: String
        let finalSource: String
        let evidenceUIDs: Set<String>
        let changedProjection: Bool
    }

    private static func validateFinalEventProjection(
        identity: DetectionRunIdentity,
        eventTable: STPDResultTableData,
        isiTable: STPDResultTableData
    ) throws {
        let isiUIDIndex = try columnIndex("isi_uid", in: isiTable)
        let isiTrainIndex = try columnIndex("train_id", in: isiTable)
        let isiTrainNameIndex = try columnIndex("train_name", in: isiTable)
        let isiIndexIndex = try columnIndex("isi_index", in: isiTable)
        let timestampIndex = try columnIndex("timestamp_sec", in: isiTable)
        let alignedTimestampIndex = try columnIndex(
            "aligned_timestamp_sec",
            in: isiTable
        )
        let isiSecIndex = try columnIndex("isi_sec", in: isiTable)
        let finalPatternIndex = try columnIndex("final_pattern", in: isiTable)
        let finalSubtypeIndex = try columnIndex("final_subtype", in: isiTable)
        let finalSourceIndex = try columnIndex("final_source", in: isiTable)
        let isiEvidenceIndex = try columnIndex(
            "review_evidence_uids",
            in: isiTable
        )
        let isiChangedIndex = try columnIndex(
            "review_changed_projection",
            in: isiTable
        )

        var isiByKey: [String: FinalISIProjectionRow] = [:]
        for row in isiTable.rows {
            let trainID = row[isiTrainIndex]
            let isiIndex = try parseInteger(
                row[isiIndexIndex],
                table: isiTable,
                column: "isi_index"
            )
            guard isiIndex > 0 else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "final ISI projection contains a non-positive ISI index"
                )
            }
            let projection = FinalISIProjectionRow(
                uid: row[isiUIDIndex],
                trainID: trainID,
                trainName: row[isiTrainNameIndex],
                index: isiIndex,
                timestampSec: try parseFiniteReal(
                    row[timestampIndex],
                    table: isiTable,
                    column: "timestamp_sec"
                ),
                alignedTimestampSec: try parseFiniteReal(
                    row[alignedTimestampIndex],
                    table: isiTable,
                    column: "aligned_timestamp_sec"
                ),
                isiSec: try parseFiniteReal(
                    row[isiSecIndex],
                    table: isiTable,
                    column: "isi_sec"
                ),
                finalPattern: row[finalPatternIndex],
                finalSubtype: row[finalSubtypeIndex],
                finalSource: row[finalSourceIndex],
                evidenceUIDs: delimitedValues(row[isiEvidenceIndex]),
                changedProjection: try parseBoolean(
                    row[isiChangedIndex],
                    table: isiTable,
                    column: "review_changed_projection"
                )
            )
            let key = finalProjectionKey(trainID: trainID, isiIndex: isiIndex)
            guard isiByKey.updateValue(projection, forKey: key) == nil else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "duplicate final ISI projection key \(trainID):\(isiIndex)"
                )
            }
        }

        let eventUIDIndex = try columnIndex("event_uid", in: eventTable)
        let sourceEventIDsIndex = try columnIndex(
            "source_event_ids",
            in: eventTable
        )
        let eventTrainIndex = try columnIndex("train_id", in: eventTable)
        let eventTrainNameIndex = try columnIndex("train_name", in: eventTable)
        let eventLabelIndex = try columnIndex("final_label", in: eventTable)
        let eventSubtypeIndex = try columnIndex("final_subtype", in: eventTable)
        let eventTonicSubtypeIndex = try columnIndex(
            "state_tonic_subtype",
            in: eventTable
        )
        let eventHighFrequencySubtypesIndex = try columnIndex(
            "state_high_frequency_subtypes",
            in: eventTable
        )
        let authorityOriginIndex = try columnIndex(
            "authority_origin",
            in: eventTable
        )
        let startISIIndex = try columnIndex("start_isi_index", in: eventTable)
        let endISIIndex = try columnIndex("end_isi_index", in: eventTable)
        let startSpikeIndex = try columnIndex(
            "start_spike_ordinal",
            in: eventTable
        )
        let endSpikeIndex = try columnIndex(
            "end_spike_ordinal",
            in: eventTable
        )
        let rawStartIndex = try columnIndex("raw_start_sec", in: eventTable)
        let rawEndIndex = try columnIndex("raw_end_sec", in: eventTable)
        let alignedStartIndex = try columnIndex(
            "aligned_start_sec",
            in: eventTable
        )
        let alignedEndIndex = try columnIndex(
            "aligned_end_sec",
            in: eventTable
        )
        let durationIndex = try columnIndex("duration_sec", in: eventTable)
        let eventEvidenceIndex = try columnIndex(
            "review_evidence_uids",
            in: eventTable
        )
        let eventPresentIndex = try columnIndex(
            "review_evidence_present",
            in: eventTable
        )
        let eventChangedIndex = try columnIndex(
            "review_changed_projection",
            in: eventTable
        )
        let auditSubtypeIndex = try columnIndex(
            "audit_recommended_subtype",
            in: eventTable
        )

        var coveredISIKeys = Set<String>()
        for eventRow in eventTable.rows {
            let eventUID = eventRow[eventUIDIndex]
            let trainID = eventRow[eventTrainIndex]
            let trainName = eventRow[eventTrainNameIndex]
            let finalLabel = eventRow[eventLabelIndex]
            let finalSubtype = eventRow[eventSubtypeIndex]
            let lower = try parseInteger(
                eventRow[startISIIndex],
                table: eventTable,
                column: "start_isi_index"
            )
            let upper = try parseInteger(
                eventRow[endISIIndex],
                table: eventTable,
                column: "end_isi_index"
            )
            guard !finalLabel.isEmpty,
                  finalLabel != ClassicAnchorLabel.profile.rawValue,
                  finalLabel != ClassicAnchorLabel.reject.rawValue,
                  lower > 0,
                  lower <= upper else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) has invalid public label or ISI bounds"
                )
            }

            let expectedEventUID = STPDStableIdentifier.make(
                prefix: "event",
                domain: "stpd_normalized_public_event_uid_v5",
                components: [
                    identity.datasetDigest,
                    trainID,
                    finalLabel,
                    finalSubtype,
                    eventRow[eventTonicSubtypeIndex],
                    eventRow[eventHighFrequencySubtypesIndex],
                    eventRow[startISIIndex],
                    eventRow[endISIIndex],
                    eventRow[startSpikeIndex],
                    eventRow[endSpikeIndex],
                    eventRow[rawStartIndex],
                    eventRow[rawEndIndex],
                    eventRow[alignedStartIndex],
                    eventRow[alignedEndIndex],
                ]
            )
            guard eventUID == expectedEventUID else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) does not match its deterministic scientific identity"
                )
            }

            let startSpike = try parseInteger(
                eventRow[startSpikeIndex],
                table: eventTable,
                column: "start_spike_ordinal"
            )
            let endSpike = try parseInteger(
                eventRow[endSpikeIndex],
                table: eventTable,
                column: "end_spike_ordinal"
            )
            let (expectedEndSpike, endSpikeOverflow) =
                upper.addingReportingOverflow(1)
            guard !endSpikeOverflow else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) end ISI index cannot be represented as a spike ordinal"
                )
            }
            guard startSpike == lower, endSpike == expectedEndSpike else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) spike ordinals contradict its ISI bounds"
                )
            }
            let (spanDelta, spanDeltaOverflow) =
                upper.subtractingReportingOverflow(lower)
            let (spanCount, spanCountOverflow) =
                spanDelta.addingReportingOverflow(1)
            guard !spanDeltaOverflow,
                  !spanCountOverflow,
                  spanCount <= isiByKey.count else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) ISI span exceeds the available final projection"
                )
            }

            var coveredRows: [FinalISIProjectionRow] = []
            for isiIndex in lower...upper {
                let key = finalProjectionKey(
                    trainID: trainID,
                    isiIndex: isiIndex
                )
                guard let isi = isiByKey[key] else {
                    throw STPDResultPackageError.invalidTable(
                        table: eventTable.contract.table.rawValue,
                        reason: "event \(eventUID) does not have contiguous ISI coverage"
                    )
                }
                guard coveredISIKeys.insert(key).inserted else {
                    throw STPDResultPackageError.invalidTable(
                        table: eventTable.contract.table.rawValue,
                        reason: "final events overlap at \(trainID):\(isiIndex)"
                    )
                }
                coveredRows.append(isi)
            }
            guard coveredRows.allSatisfy({
                $0.trainName == trainName && $0.finalPattern == finalLabel
            }) else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) contradicts its covered ISI train or label"
                )
            }

            let auditSubtype = eventRow[auditSubtypeIndex]
            for isi in coveredRows {
                if isi.finalSource == ReviewedISIExportBuilder.sourceManualPositive {
                    guard isi.finalSubtype.isEmpty else {
                        throw STPDResultPackageError.invalidTable(
                            table: eventTable.contract.table.rawValue,
                            reason: "event \(eventUID) gives a manual-positive ISI a detector subtype"
                        )
                    }
                } else if isi.finalSubtype != finalSubtype {
                    throw STPDResultPackageError.invalidTable(
                        table: eventTable.contract.table.rawValue,
                        reason: "event \(eventUID) authoritative subtype does not match its automatic ISI projection"
                    )
                }
            }

            let sourceEventIDs = delimitedValues(eventRow[sourceEventIDsIndex])
            let expectedAuthorityOrigin: String
            if sourceEventIDs.isEmpty {
                guard coveredRows.allSatisfy({
                    $0.finalSource ==
                        ReviewedISIExportBuilder.sourceManualPositive
                }),
                finalSubtype.isEmpty,
                auditSubtype == "manual_positive" else {
                    throw STPDResultPackageError.invalidTable(
                        table: eventTable.contract.table.rawValue,
                        reason: "manual-only event \(eventUID) is not composed solely of manual-positive ISIs"
                    )
                }
                expectedAuthorityOrigin = "manual_only"
            } else {
                expectedAuthorityOrigin = coveredRows.contains {
                    $0.changedProjection ||
                        $0.finalSource ==
                            ReviewedISIExportBuilder.sourceManualPositive
                } ? "manual_augmented" : "automatic"
            }
            guard eventRow[authorityOriginIndex] == expectedAuthorityOrigin else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) authority origin contradicts its ISI projection"
                )
            }

            let first = coveredRows[0]
            let last = coveredRows[coveredRows.count - 1]

            let expectedRawStart = first.timestampSec - first.isiSec
            let expectedRawEnd = last.timestampSec
            let expectedAlignedStart =
                first.alignedTimestampSec - first.isiSec
            let expectedAlignedEnd = last.alignedTimestampSec
            let expectedDuration = expectedRawEnd - expectedRawStart
            let actualRawStart = try parseFiniteReal(
                eventRow[rawStartIndex],
                table: eventTable,
                column: "raw_start_sec"
            )
            let actualRawEnd = try parseFiniteReal(
                eventRow[rawEndIndex],
                table: eventTable,
                column: "raw_end_sec"
            )
            let actualAlignedStart = try parseFiniteReal(
                eventRow[alignedStartIndex],
                table: eventTable,
                column: "aligned_start_sec"
            )
            let actualAlignedEnd = try parseFiniteReal(
                eventRow[alignedEndIndex],
                table: eventTable,
                column: "aligned_end_sec"
            )
            let actualDuration = try parseFiniteReal(
                eventRow[durationIndex],
                table: eventTable,
                column: "duration_sec"
            )
            guard nearlyEqual(actualRawStart, expectedRawStart),
                  nearlyEqual(actualRawEnd, expectedRawEnd),
                  nearlyEqual(actualAlignedStart, expectedAlignedStart),
                  nearlyEqual(actualAlignedEnd, expectedAlignedEnd),
                  nearlyEqual(actualDuration, expectedDuration) else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) geometry contradicts its covered ISIs"
                )
            }

            let expectedEvidence = coveredRows.reduce(into: Set<String>()) {
                $0.formUnion($1.evidenceUIDs)
            }
            let actualEvidence = delimitedValues(
                eventRow[eventEvidenceIndex]
            )
            let expectedChanged = coveredRows.contains {
                $0.changedProjection
            }
            let actualPresent = try parseBoolean(
                eventRow[eventPresentIndex],
                table: eventTable,
                column: "review_evidence_present"
            )
            let actualChanged = try parseBoolean(
                eventRow[eventChangedIndex],
                table: eventTable,
                column: "review_changed_projection"
            )
            guard actualEvidence == expectedEvidence,
                  actualPresent == !expectedEvidence.isEmpty,
                  actualChanged == expectedChanged else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event \(eventUID) review evidence is not the exact union of its ISIs"
                )
            }
        }

        for isi in isiByKey.values {
            let key = finalProjectionKey(
                trainID: isi.trainID,
                isiIndex: isi.index
            )
            guard coveredISIKeys.contains(key) == !isi.finalPattern.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: "final event/ISI projection",
                    reason: "ISI \(isi.uid) event coverage contradicts its final label"
                )
            }
        }
    }

    private static func validatedCandidateRecords(
        _ table: STPDResultTableData
    ) throws -> [String: ValidatedCandidateRecord] {
        let uidIndex = try columnIndex("candidate_uid", in: table)
        let sourceIDIndex = try columnIndex("source_candidate_id", in: table)
        let trainIDIndex = try columnIndex("train_id", in: table)
        let candidateClassIndex = try columnIndex("candidate_class", in: table)
        let finalLabelIndex = try columnIndex("final_label", in: table)
        let startIndex = try columnIndex("start_isi_index", in: table)
        let endIndex = try columnIndex("end_isi_index", in: table)
        var recordsByUID: [String: ValidatedCandidateRecord] = [:]
        var seenSourceIDs = Set<String>()
        let profileClasses = Set(["train_profile", "dataset_profile"])

        for row in table.rows {
            let uid = row[uidIndex]
            let sourceID = row[sourceIDIndex]
            let trainID = row[trainIDIndex]
            let candidateClass = row[candidateClassIndex]
            let finalLabel = row[finalLabelIndex]
            let start = try parseInteger(
                row[startIndex],
                table: table,
                column: "start_isi_index"
            )
            let end = try parseInteger(
                row[endIndex],
                table: table,
                column: "end_isi_index"
            )
            let isProfile = finalLabel == ClassicAnchorLabel.profile.rawValue
            let validProfileOwnership =
                candidateClass == "dataset_profile"
                    ? trainID == "__dataset__"
                    : candidateClass == "train_profile" &&
                        trainID != "__dataset__"
            let validGeometry = isProfile
                ? profileClasses.contains(candidateClass) &&
                    validProfileOwnership &&
                    start == 0 &&
                    end == 0
                : !profileClasses.contains(candidateClass) &&
                    trainID != "__dataset__" &&
                    start > 0 &&
                    end >= start
            guard !uid.isEmpty,
                  !sourceID.isEmpty,
                  !trainID.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "candidate identity or train is empty"
                )
            }
            guard validGeometry else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason:
                        "candidate \(sourceID) has invalid \(trainID) geometry " +
                        "\(start)...\(end)"
                )
            }
            guard seenSourceIDs.insert(sourceID).inserted else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "candidate source ID \(sourceID) is duplicated"
                )
            }
            let record = ValidatedCandidateRecord(
                uid: uid,
                sourceID: sourceID,
                trainID: trainID,
                startISIIndex: start,
                endISIIndex: end
            )
            guard recordsByUID.updateValue(record, forKey: uid) == nil else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "candidate \(uid) is duplicated"
                )
            }
        }
        return recordsByUID
    }

    private static func validatedISIRecords(
        identity: DetectionRunIdentity,
        _ table: STPDResultTableData
    ) throws -> [String: ValidatedISIRecord] {
        let uidIndex = try columnIndex("isi_uid", in: table)
        let trainIDIndex = try columnIndex("train_id", in: table)
        let trainNameIndex = try columnIndex("train_name", in: table)
        let isiIndex = try columnIndex("isi_index", in: table)
        let automaticPatternIndex = try columnIndex(
            "auto_pattern",
            in: table
        )
        let automaticSubtypeIndex = try columnIndex(
            "auto_subtype",
            in: table
        )
        let candidateUIDIndex = try columnIndex(
            "auto_candidate_uid",
            in: table
        )
        let sourceCandidateIDIndex = try columnIndex(
            "auto_source_candidate_id",
            in: table
        )
        let finalPatternIndex = try columnIndex("final_pattern", in: table)
        let finalSubtypeIndex = try columnIndex("final_subtype", in: table)
        let finalSourceIndex = try columnIndex("final_source", in: table)
        let manualVetoIndex = try columnIndex(
            "manual_veto_suppressed",
            in: table
        )
        let reviewNoteIndex = try columnIndex("review_note", in: table)
        var recordsByUID: [String: ValidatedISIRecord] = [:]

        for row in table.rows {
            let uid = row[uidIndex]
            let trainID = row[trainIDIndex]
            let index = try parseInteger(
                row[isiIndex],
                table: table,
                column: "isi_index"
            )
            let expectedUID = STPDResultPackageBuilder.stableISIUID(
                datasetDigest: identity.datasetDigest,
                trainID: trainID,
                isiIndex: index
            )
            guard index > 0, uid == expectedUID else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "ISI \(uid) does not match its stable train/index identity"
                )
            }
            let record = ValidatedISIRecord(
                uid: uid,
                trainID: trainID,
                trainName: row[trainNameIndex],
                index: index,
                automaticPattern: row[automaticPatternIndex],
                automaticSubtype: row[automaticSubtypeIndex],
                automaticCandidateUID: row[candidateUIDIndex],
                automaticSourceCandidateID: row[sourceCandidateIDIndex],
                finalPattern: row[finalPatternIndex],
                finalSubtype: row[finalSubtypeIndex],
                finalSource: row[finalSourceIndex],
                manualVetoSuppressed: try parseBoolean(
                    row[manualVetoIndex],
                    table: table,
                    column: "manual_veto_suppressed"
                ),
                reviewNote: row[reviewNoteIndex]
            )
            guard recordsByUID.updateValue(record, forKey: uid) == nil else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "ISI \(uid) is duplicated"
                )
            }
        }
        return recordsByUID
    }

    private static func validateReviewRows(
        identity: DetectionRunIdentity,
        table: STPDResultTableData,
        candidateTable: STPDResultTableData
    ) throws -> [String: ValidatedReviewRecord] {
        let reviewUIDIndex = try columnIndex("review_uid", in: table)
        let candidateUIDIndex = try columnIndex("candidate_uid", in: table)
        let sourceCandidateIDIndex = try columnIndex(
            "source_candidate_id",
            in: table
        )
        let statusIndex = try columnIndex("status", in: table)
        let reviewerIndex = try columnIndex("reviewer", in: table)
        let noteIndex = try columnIndex("note", in: table)
        let reviewedRunIndex = try columnIndex("reviewed_run_id", in: table)
        let linksIndex = try columnIndex("linked_isi_uids", in: table)
        let scopeIndex = try columnIndex("link_scope", in: table)
        let reviewedAtIndex = try columnIndex("reviewed_at", in: table)
        let reviewedAtExactIndex = try columnIndex(
            "reviewed_at_unix_sec",
            in: table
        )
        let candidatesByUID = try validatedCandidateRecords(candidateTable)
        var reviewsByUID: [String: ValidatedReviewRecord] = [:]

        for row in table.rows {
            let reviewUID = row[reviewUIDIndex]
            guard let status = STPDCandidateReviewStatus(
                rawValue: row[statusIndex]
            ) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "review \(reviewUID) has an unknown status"
                )
            }
            let reviewedAt = row[reviewedAtIndex]
            let reviewedAtExact = row[reviewedAtExactIndex]
            var reviewedAtDate: Date? = nil
            if reviewedAt.isEmpty != reviewedAtExact.isEmpty {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "review \(reviewUID) has only one of its two review-time representations"
                )
            }
            if !reviewedAt.isEmpty {
                guard STPDResultTimestamp.exactSeconds(reviewedAt) != nil else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "review \(reviewUID) has an invalid review time"
                    )
                }
                let exact = try parseFiniteReal(
                    reviewedAtExact,
                    table: table,
                    column: "reviewed_at_unix_sec"
                )
                guard canonicalTimestamp(
                    reviewedAt,
                    matchesExactSeconds: exact
                ) else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "review \(reviewUID) has contradictory review timestamps"
                    )
                }
                reviewedAtDate = Date(timeIntervalSince1970: exact)
            }
            let expectedReviewUID = STPDCandidateReviewIdentity.make(
                candidateUID: row[candidateUIDIndex],
                status: status,
                reviewer: row[reviewerIndex],
                note: row[noteIndex],
                reviewedAtUnixSec: reviewedAtExact,
                reviewedRunID: row[reviewedRunIndex]
            )
            guard reviewUID == expectedReviewUID else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "review \(reviewUID) does not match its semantic identity"
                )
            }
            let candidateUID = row[candidateUIDIndex]
            let sourceCandidateID = row[sourceCandidateIDIndex]
            guard let candidate = candidatesByUID[candidateUID],
                  candidate.sourceID == sourceCandidateID else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "review \(reviewUID) candidate UID/source ID pair is inconsistent"
                )
            }

            let linkedUIDs = try canonicalDelimitedValues(
                row[linksIndex],
                table: table,
                column: "linked_isi_uids",
                requireSorted: true
            )
            let reviewRecord = ValidatedReviewRecord(
                uid: reviewUID,
                candidateUID: candidateUID,
                sourceCandidateID: sourceCandidateID,
                status: status,
                reviewer: row[reviewerIndex],
                note: row[noteIndex],
                reviewedAt: reviewedAtDate,
                reviewedRunID: row[reviewedRunIndex],
                linkedISIUIDs: linkedUIDs,
                linkScope: row[scopeIndex]
            )
            guard reviewsByUID.updateValue(
                reviewRecord,
                forKey: reviewUID
            ) == nil else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "review \(reviewUID) is duplicated"
                )
            }

            if status.grantsReviewAuthority {
                guard row[reviewedRunIndex] == identity.runID,
                      !row[reviewerIndex]
                        .trimmingCharacters(in: .whitespacesAndNewlines).isEmpty,
                      !reviewedAt.isEmpty else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "authority-bearing review \(reviewUID) lacks exact run, reviewer, or time"
                    )
                }
            }

            if linkedUIDs.isEmpty {
                guard row[scopeIndex] == "non_authoritative" else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "unlinked review \(reviewUID) claims public authority"
                    )
                }
                continue
            }

            guard status.grantsReviewAuthority,
                  row[scopeIndex] == "public_projection" else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "linked review \(reviewUID) lacks public status authority"
                )
            }
        }
        return reviewsByUID
    }

    private static func validateExpectedReviewSet(
        identity: DetectionRunIdentity,
        expected: [STPDCandidateReviewInput],
        actual: [String: ValidatedReviewRecord],
        candidatesByUID: [String: ValidatedCandidateRecord]
    ) throws {
        let candidateUIDBySourceID = Dictionary(
            uniqueKeysWithValues: candidatesByUID.values.map {
                ($0.sourceID, $0.uid)
            }
        )
        var expectedUIDs = Set<String>()
        for review in expected {
            guard let candidateUID =
                    candidateUIDBySourceID[review.sourceCandidateID] else {
                throw STPDResultPackageError.invalidInput(
                    "sealed review references unknown candidate " +
                        review.sourceCandidateID
                )
            }
            let uid = STPDCandidateReviewIdentity.make(
                candidateUID: candidateUID,
                status: review.status,
                reviewer: review.reviewer,
                note: review.note,
                reviewedAtUnixSec: STPDCanonicalValue.double(
                    review.reviewedAt?.timeIntervalSince1970
                ),
                reviewedRunID: review.reviewedRunID
            )
            guard expectedUIDs.insert(uid).inserted else {
                throw STPDResultPackageError.invalidInput(
                    "sealed review set contains duplicate semantic review \(uid)"
                )
            }
            if review.status.grantsReviewAuthority {
                guard review.reviewedRunID == identity.runID else {
                    throw STPDResultPackageError.invalidInput(
                        "sealed authority review \(uid) targets a different run"
                    )
                }
            }
        }
        guard Set(actual.keys) == expectedUIDs else {
            throw STPDResultPackageError.invalidTable(
                table: STPDResultTable.reviewStatus.rawValue,
                reason:
                    "review rows do not exactly match the sealed review set; " +
                    "missing=\(expectedUIDs.subtracting(actual.keys).sorted().joined(separator: "|")); " +
                    "unexpected=\(Set(actual.keys).subtracting(expectedUIDs).sorted().joined(separator: "|"))"
            )
        }
    }

    private static func validateExpectedManualSet(
        identity: DetectionRunIdentity,
        expected: [ManualAnnotation],
        actual: ValidatedManualAuthority,
        dataset: SpikeDataset
    ) throws {
        let resolvedExpected = try STPDResultPackageBuilder
            .resolvedManualAnnotations(expected, dataset: dataset)
        let expectedByUID = Dictionary(
            uniqueKeysWithValues: resolvedExpected.map { annotation in
                let sourceUUID = annotation.id.uuidString.lowercased()
                let uid = STPDStableIdentifier.make(
                    prefix: "manual",
                    domain: "stpd_manual_annotation_uid_v1",
                    components: [identity.datasetDigest, sourceUUID]
                )
                return (uid, manualSnapshotComponents(annotation))
            }
        )
        let actualByUID = actual.recordsByUID.mapValues {
            manualSnapshotComponents($0.annotation)
        }
        guard expectedByUID == actualByUID else {
            let expectedUIDs = Set(expectedByUID.keys)
            let actualUIDs = Set(actualByUID.keys)
            let changed = expectedUIDs.intersection(actualUIDs)
                .filter { expectedByUID[$0] != actualByUID[$0] }
                .sorted()
            throw STPDResultPackageError.invalidTable(
                table: STPDResultTable.manualAnnotations.rawValue,
                reason:
                    "manual annotation snapshot does not exactly match the " +
                    "sealed authoring input; missing=" +
                    expectedUIDs.subtracting(actualUIDs).sorted()
                        .joined(separator: "|") +
                    "; unexpected=" +
                    actualUIDs.subtracting(expectedUIDs).sorted()
                        .joined(separator: "|") +
                    "; changed=" + changed.joined(separator: "|")
            )
        }
    }

    private static func manualSnapshotComponents(
        _ annotation: ManualAnnotation
    ) -> [String] {
        [
            annotation.id.uuidString.lowercased(),
            annotation.trainID,
            annotation.label.rawValue,
            annotation.polarity.rawValue,
            STPDCanonicalValue.double(annotation.normalizedStartSec),
            STPDCanonicalValue.double(annotation.normalizedEndSec),
            annotation.startISIIndex.map(String.init) ?? "",
            annotation.endISIIndex.map(String.init) ?? "",
            annotation.startSpikeIndex.map(String.init) ?? "",
            annotation.endSpikeIndex.map(String.init) ?? "",
            annotation.note ?? "",
            annotation.annotator ?? "",
            annotation.annotatorIdentitySource?.rawValue ?? "",
            STPDCanonicalValue.double(
                annotation.createdAt.timeIntervalSince1970
            ),
            STPDCanonicalValue.double(
                annotation.updatedAt.timeIntervalSince1970
            ),
        ]
    }

    private static func validateManualRows(
        identity: DetectionRunIdentity,
        table: STPDResultTableData,
        isiRecordsByUID: [String: ValidatedISIRecord],
        expectedTrainIDs: Set<String>?,
        expectedDataset: SpikeDataset?
    ) throws -> ValidatedManualAuthority {
        let annotationIndex = try columnIndex("annotation_id", in: table)
        let semanticDigestIndex = try columnIndex(
            "annotation_semantic_digest",
            in: table
        )
        let sourceUUIDIndex = try columnIndex(
            "source_annotation_uuid",
            in: table
        )
        let trainIDIndex = try columnIndex("train_id", in: table)
        let labelIndex = try columnIndex("label", in: table)
        let polarityIndex = try columnIndex("polarity", in: table)
        let startSecIndex = try columnIndex("start_sec", in: table)
        let endSecIndex = try columnIndex("end_sec", in: table)
        let linksIndex = try columnIndex("linked_isi_uids", in: table)
        let scopeIndex = try columnIndex("link_scope", in: table)
        let noteIndex = try columnIndex("note", in: table)
        let annotatorIndex = try columnIndex("annotator", in: table)
        let identitySourceIndex = try columnIndex(
            "annotator_identity_source",
            in: table
        )
        let startISIIndex = try columnIndex("start_isi_index", in: table)
        let endISIIndex = try columnIndex("end_isi_index", in: table)
        let startSpikeArrayIndex = try columnIndex(
            "start_spike_array_index",
            in: table
        )
        let endSpikeArrayIndex = try columnIndex(
            "end_spike_array_index",
            in: table
        )
        let startSpikeOrdinalIndex = try columnIndex(
            "start_spike_ordinal",
            in: table
        )
        let endSpikeOrdinalIndex = try columnIndex(
            "end_spike_ordinal",
            in: table
        )
        let createdIndex = try columnIndex("created_at", in: table)
        let updatedIndex = try columnIndex("updated_at", in: table)
        let createdExactIndex = try columnIndex(
            "created_at_unix_sec",
            in: table
        )
        let updatedExactIndex = try columnIndex(
            "updated_at_unix_sec",
            in: table
        )

        var activeSpikeOnly = Set<String>()
        var recordsByUID: [String: ValidatedManualRecord] = [:]
        var annotations: [ManualAnnotation] = []
        var seenAnnotationUUIDs = Set<UUID>()
        for row in table.rows {
            let annotationUID = row[annotationIndex]
            let sourceUUID = row[sourceUUIDIndex]
            guard let uuid = UUID(uuidString: sourceUUID),
                  uuid.uuidString.lowercased() == sourceUUID,
                  annotationUID == STPDStableIdentifier.make(
                    prefix: "manual",
                    domain: "stpd_manual_annotation_uid_v1",
                    components: [
                        identity.datasetDigest,
                        sourceUUID,
                    ]
                  ) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) does not match its stable source identity"
                )
            }
            let semanticValues = Dictionary(
                uniqueKeysWithValues: zip(table.headers, row)
            )
            guard row[semanticDigestIndex] ==
                    STPDResultPackageBuilder.manualAnnotationSemanticDigest(
                        values: semanticValues
                    ) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) does not match its semantic digest"
                )
            }
            let trainID = row[trainIDIndex]
            if let expectedTrainIDs, !expectedTrainIDs.contains(trainID) {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) references unknown train \(trainID)"
                )
            }
            guard let label = ManualAnnotationLabel(rawValue: row[labelIndex]),
                  row[polarityIndex] == label.polarity.rawValue else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) has contradictory label polarity"
                )
            }
            let startSec = try parseFiniteReal(
                row[startSecIndex],
                table: table,
                column: "start_sec"
            )
            let endSec = try parseFiniteReal(
                row[endSecIndex],
                table: table,
                column: "end_sec"
            )
            guard startSec <= endSec else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) has reversed time geometry"
                )
            }
            let startISI = try parseOptionalInteger(
                row[startISIIndex],
                table: table,
                column: "start_isi_index"
            )
            let endISI = try parseOptionalInteger(
                row[endISIIndex],
                table: table,
                column: "end_isi_index"
            )
            guard (startISI == nil) == (endISI == nil),
                  (startISI.map { $0 > 0 }) ?? true,
                  (endISI.map { $0 > 0 }) ?? true,
                  orderedOptionalPair(startISI, endISI) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) has inconsistent ISI geometry"
                )
            }
            let startSpike = try parseOptionalInteger(
                row[startSpikeArrayIndex],
                table: table,
                column: "start_spike_array_index"
            )
            let endSpike = try parseOptionalInteger(
                row[endSpikeArrayIndex],
                table: table,
                column: "end_spike_array_index"
            )
            let startOrdinal = try parseOptionalInteger(
                row[startSpikeOrdinalIndex],
                table: table,
                column: "start_spike_ordinal"
            )
            let endOrdinal = try parseOptionalInteger(
                row[endSpikeOrdinalIndex],
                table: table,
                column: "end_spike_ordinal"
            )
            let expectedStartOrdinal = incrementedWithoutOverflow(startSpike)
            let expectedEndOrdinal = incrementedWithoutOverflow(endSpike)
            guard (startSpike == nil) == (endSpike == nil),
                  (startSpike == nil) == (startOrdinal == nil),
                  (endSpike == nil) == (endOrdinal == nil),
                  (startSpike.map { $0 >= 0 }) ?? true,
                  (endSpike.map { $0 >= 0 }) ?? true,
                  orderedOptionalPair(startSpike, endSpike),
                  startOrdinal == expectedStartOrdinal,
                  endOrdinal == expectedEndOrdinal else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) has inconsistent spike geometry"
                )
            }
            guard STPDResultTimestamp.exactSeconds(row[createdIndex]) != nil,
                  STPDResultTimestamp.exactSeconds(row[updatedIndex]) != nil else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) has an invalid timestamp"
                )
            }
            let createdExact = try parseFiniteReal(
                row[createdExactIndex],
                table: table,
                column: "created_at_unix_sec"
            )
            let updatedExact = try parseFiniteReal(
                row[updatedExactIndex],
                table: table,
                column: "updated_at_unix_sec"
            )
            guard canonicalTimestamp(
                    row[createdIndex],
                    matchesExactSeconds: createdExact
                  ),
                  canonicalTimestamp(
                    row[updatedIndex],
                    matchesExactSeconds: updatedExact
                  ),
                  updatedExact >= createdExact else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) has contradictory or reversed timestamps"
                )
            }

            let identitySource: ManualAnnotationIdentitySource?
            if row[identitySourceIndex].isEmpty {
                identitySource = nil
            } else {
                guard let parsed = ManualAnnotationIdentitySource(
                    rawValue: row[identitySourceIndex]
                ) else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "manual annotation \(annotationUID) has an unknown annotator identity source"
                    )
                }
                identitySource = parsed
            }
            let annotation = ManualAnnotation(
                id: uuid,
                trainID: trainID,
                label: label,
                startSec: startSec,
                endSec: endSec,
                startISIIndex: startISI,
                endISIIndex: endISI,
                startSpikeIndex: startSpike,
                endSpikeIndex: endSpike,
                note: row[noteIndex].isEmpty ? nil : row[noteIndex],
                annotator: row[annotatorIndex].isEmpty
                    ? nil
                    : row[annotatorIndex],
                annotatorIdentitySource: identitySource,
                createdAt: Date(timeIntervalSince1970: createdExact),
                updatedAt: Date(timeIntervalSince1970: updatedExact)
            )
            guard seenAnnotationUUIDs.insert(uuid).inserted else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation source UUID \(sourceUUID) is duplicated"
                )
            }

            let linkedUIDs = try canonicalDelimitedValues(
                row[linksIndex],
                table: table,
                column: "linked_isi_uids",
                requireSorted: true
            )
            let linkedISIs = try linkedUIDs.map { linkedUID in
                guard let isi = isiRecordsByUID[linkedUID],
                      isi.trainID == trainID else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "manual annotation \(annotationUID) links an ISI outside its train"
                    )
                }
                return isi
            }
            if let lower = startISI, let upper = endISI {
                let directLinks = linkedISIs.filter {
                    lower ... upper ~= $0.index
                }
                let structurallyInducedLinks = linkedISIs.filter {
                    !(lower ... upper ~= $0.index)
                }
                if !structurallyInducedLinks.isEmpty {
                    let directCandidatePairs = Set(directLinks.compactMap {
                        isi -> String? in
                        guard !isi.automaticCandidateUID.isEmpty,
                              !isi.automaticSourceCandidateID.isEmpty else {
                            return nil
                        }
                        return "\(isi.automaticCandidateUID)\u{1f}" +
                            isi.automaticSourceCandidateID
                    })
                    guard label.polarity == .negative,
                          !directLinks.isEmpty,
                          directCandidatePairs.count == 1,
                          let governingPair = directCandidatePairs.first,
                          linkedISIs.allSatisfy({
                              "\($0.automaticCandidateUID)\u{1f}" +
                                $0.automaticSourceCandidateID ==
                                governingPair
                          }) else {
                        throw STPDResultPackageError.invalidTable(
                            table: table.contract.table.rawValue,
                            reason:
                                "manual annotation \(annotationUID) has an " +
                                "unbound structurally induced ISI link"
                        )
                    }
                }
            }
            let scope = row[scopeIndex]
            let authorityBearing =
                scope == "public_projection" || scope == "active_spike_only"
            if authorityBearing {
                let annotator = row[annotatorIndex]
                    .trimmingCharacters(in: .whitespacesAndNewlines)
                guard !annotator.isEmpty,
                      let source = ManualAnnotationIdentitySource(
                        rawValue: row[identitySourceIndex]
                      ),
                      source != .unknown else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "authority-bearing manual annotation \(annotationUID) lacks a known annotator identity"
                    )
                }
            }

            switch scope {
            case "public_projection":
                guard !linkedUIDs.isEmpty else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "manual annotation \(annotationUID) claims public projection without an ISI link"
                    )
                }
            case "active_spike_only":
                guard linkedUIDs.isEmpty,
                      startISI == nil,
                      endISI == nil,
                      let startSpike,
                      let endSpike,
                      startSpike == endSpike else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "active spike-only annotation \(annotationUID) has inconsistent spike geometry or an ISI link"
                    )
                }
                activeSpikeOnly.insert(annotationUID)
            case "inactive_or_superseded":
                guard linkedUIDs.isEmpty else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "inactive manual annotation \(annotationUID) retains an ISI link"
                    )
                }
            default:
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) has unknown link scope \(scope)"
                )
            }
            let record = ValidatedManualRecord(
                uid: annotationUID,
                annotation: annotation,
                linkedISIUIDs: linkedUIDs,
                linkScope: scope
            )
            guard recordsByUID.updateValue(
                record,
                forKey: annotationUID
            ) == nil else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "manual annotation \(annotationUID) is duplicated"
                )
            }
            annotations.append(annotation)
        }

        if let expectedDataset {
            let resolved = try STPDResultPackageBuilder.resolvedManualAnnotations(
                annotations,
                dataset: expectedDataset
            )
            let resolvedByID: [UUID: ManualAnnotation] = Dictionary(
                uniqueKeysWithValues: resolved.map { ($0.id, $0) }
            )
            for annotation in annotations {
                guard let expected = resolvedByID[annotation.id],
                      annotation.startISIIndex == expected.startISIIndex,
                      annotation.endISIIndex == expected.endISIIndex,
                      annotation.startSpikeIndex == expected.startSpikeIndex,
                      annotation.endSpikeIndex == expected.endSpikeIndex else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason:
                            "manual annotation \(annotation.id.uuidString.lowercased()) " +
                            "cached geometry does not match the dataset-resolved geometry"
                    )
                }
            }
        } else {
            try STPDResultPackageInput.validateNoAmbiguousManualEditTies(
                annotations
            )
        }

        return ValidatedManualAuthority(
            recordsByUID: recordsByUID,
            activeSpikeOnlyUIDs: activeSpikeOnly
        )
    }

    private static func validateReviewEvidence(
        identity: DetectionRunIdentity,
        sourceMode: STPDResultPackageSourceMode,
        manualTable: STPDResultTableData,
        reviewTable: STPDResultTableData,
        candidateTable: STPDResultTableData,
        eventTable: STPDResultTableData,
        isiTable: STPDResultTableData,
        expectedTrainIDs: Set<String>?,
        expectedDataset: SpikeDataset?,
        expectedQualitySettings: SpikeQualitySettings?,
        expectedCandidateReviews: [STPDCandidateReviewInput]?,
        expectedManualAnnotations: [ManualAnnotation]?,
        publicCandidateUIDs: Set<String>
    ) throws {
        let isiRecordsByUID = try validatedISIRecords(
            identity: identity,
            isiTable
        )
        let candidatesByUID = try validatedCandidateRecords(candidateTable)
        let manualAuthority = try validateManualRows(
            identity: identity,
            table: manualTable,
            isiRecordsByUID: isiRecordsByUID,
            expectedTrainIDs: expectedTrainIDs,
            expectedDataset: expectedDataset
        )
        let reviewsByUID = try validateReviewRows(
            identity: identity,
            table: reviewTable,
            candidateTable: candidateTable
        )
        if let expectedManualAnnotations, let expectedDataset {
            try validateExpectedManualSet(
                identity: identity,
                expected: expectedManualAnnotations,
                actual: manualAuthority,
                dataset: expectedDataset
            )
        }
        if let expectedCandidateReviews {
            try validateExpectedReviewSet(
                identity: identity,
                expected: expectedCandidateReviews,
                actual: reviewsByUID,
                candidatesByUID: candidatesByUID
            )
        }
        for isi in isiRecordsByUID.values {
            let candidateUID = isi.automaticCandidateUID
            let sourceCandidateID = isi.automaticSourceCandidateID
            if candidateUID.isEmpty && sourceCandidateID.isEmpty {
                continue
            }
            guard !candidateUID.isEmpty,
                  !sourceCandidateID.isEmpty,
                  let candidate = candidatesByUID[candidateUID],
                  candidate.sourceID == sourceCandidateID,
                  candidate.trainID == isi.trainID,
                  candidate.contains(isiIndex: isi.index) else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "ISI \(isi.uid) automatic candidate UID/source identity is inconsistent"
                )
            }
        }

        let manualUIDs = try nonemptyValues(
            table: manualTable,
            column: "annotation_id"
        )
        let reviewUIDs = try nonemptyValues(
            table: reviewTable,
            column: "review_uid"
        )
        let evidenceUIDs = manualUIDs.union(reviewUIDs)
        let isiUIDs = try nonemptyValues(table: isiTable, column: "isi_uid")
        var evidenceByISIUID: [String: Set<String>] = [:]
        var changedProjectionByISIUID: [String: Bool] = [:]
        var anyChangedProjection = false

        let isiUIDIndex = try columnIndex("isi_uid", in: isiTable)
        let isiEvidenceIndex = try columnIndex("review_evidence_uids", in: isiTable)
        let isiPresentIndex = try columnIndex("review_evidence_present", in: isiTable)
        let isiChangedIndex = try columnIndex("review_changed_projection", in: isiTable)
        for row in isiTable.rows {
            let isiUID = row[isiUIDIndex]
            let rowEvidence = try canonicalDelimitedValues(
                row[isiEvidenceIndex],
                table: isiTable,
                column: "review_evidence_uids",
                requireSorted: true
            )
            let evidencePresent = try parseBoolean(
                row[isiPresentIndex],
                table: isiTable,
                column: "review_evidence_present"
            )
            let changedProjection = try parseBoolean(
                row[isiChangedIndex],
                table: isiTable,
                column: "review_changed_projection"
            )
            let unresolvedEvidence = rowEvidence.subtracting(evidenceUIDs)
            guard unresolvedEvidence.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "ISI \(isiUID) references unresolved review evidence " +
                        unresolvedEvidence.sorted().joined(separator: "|")
                )
            }
            guard evidencePresent == !rowEvidence.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "ISI \(isiUID) review_evidence_present contradicts its evidence list"
                )
            }
            guard !changedProjection || evidencePresent else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "ISI \(isiUID) changes the public projection without review evidence"
                )
            }
            anyChangedProjection = anyChangedProjection || changedProjection
            evidenceByISIUID[isiUID] = rowEvidence
            changedProjectionByISIUID[isiUID] = changedProjection
        }

        let eventEvidenceIndex = try columnIndex("review_evidence_uids", in: eventTable)
        let eventPresentIndex = try columnIndex("review_evidence_present", in: eventTable)
        let eventChangedIndex = try columnIndex("review_changed_projection", in: eventTable)
        for row in eventTable.rows {
            let rowEvidence = try canonicalDelimitedValues(
                row[eventEvidenceIndex],
                table: eventTable,
                column: "review_evidence_uids",
                requireSorted: true
            )
            let evidencePresent = try parseBoolean(
                row[eventPresentIndex],
                table: eventTable,
                column: "review_evidence_present"
            )
            let changedProjection = try parseBoolean(
                row[eventChangedIndex],
                table: eventTable,
                column: "review_changed_projection"
            )
            guard rowEvidence.isSubset(of: evidenceUIDs),
                  evidencePresent == !rowEvidence.isEmpty,
                  !changedProjection || evidencePresent else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event has unresolved evidence or contradictory review flags"
                )
            }
            anyChangedProjection = anyChangedProjection || changedProjection
        }

        try validateEvidenceLinks(
            table: manualTable,
            evidenceUIDColumn: "annotation_id",
            isiUIDs: isiUIDs,
            evidenceByISIUID: evidenceByISIUID
        )
        try validateEvidenceLinks(
            table: reviewTable,
            evidenceUIDColumn: "review_uid",
            isiUIDs: isiUIDs,
            evidenceByISIUID: evidenceByISIUID
        )
        if let expectedDataset, let expectedQualitySettings {
            try validateDatasetBackedReviewCausality(
                identity: identity,
                dataset: expectedDataset,
                qualitySettings: expectedQualitySettings,
                manualAuthority: manualAuthority,
                reviewsByUID: reviewsByUID,
                candidatesByUID: candidatesByUID,
                publicCandidateUIDs: publicCandidateUIDs,
                isiRecordsByUID: isiRecordsByUID,
                evidenceByISIUID: evidenceByISIUID,
                changedProjectionByISIUID: changedProjectionByISIUID
            )
        } else {
            try validateReviewCausality(
                reviewsByUID: reviewsByUID,
                candidatesByUID: candidatesByUID,
                isiRecordsByUID: isiRecordsByUID,
                manualUIDs: manualUIDs,
                evidenceByISIUID: evidenceByISIUID,
                changedProjectionByISIUID: changedProjectionByISIUID
            )
        }

        let linkedEvidenceUIDs = evidenceByISIUID.values.reduce(into: Set<String>()) {
            $0.formUnion($1)
        }
        let linkedManualUIDs = linkedEvidenceUIDs.intersection(manualUIDs)
        let linkedReviewUIDs = linkedEvidenceUIDs.intersection(reviewUIDs)

        switch sourceMode {
        case .automatic:
            guard evidenceByISIUID.values.allSatisfy(\.isEmpty),
                  !anyChangedProjection,
                  manualAuthority.activeSpikeOnlyUIDs.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: "review authority",
                    reason: "automatic mode contains effective review authority"
                )
            }
        case .manual:
            guard !linkedManualUIDs.isEmpty
                    || !manualAuthority.activeSpikeOnlyUIDs.isEmpty,
                  linkedReviewUIDs.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: "review authority",
                    reason: "manual mode requires manual ISI authority or an active spike-only mark"
                )
            }
        case .reviewed:
            guard !linkedReviewUIDs.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: "review authority",
                    reason: "reviewed mode has no candidate-review evidence-bearing ISI"
                )
            }
        }
    }

    private static func validateReviewCausality(
        reviewsByUID: [String: ValidatedReviewRecord],
        candidatesByUID: [String: ValidatedCandidateRecord],
        isiRecordsByUID: [String: ValidatedISIRecord],
        manualUIDs: Set<String>,
        evidenceByISIUID: [String: Set<String>],
        changedProjectionByISIUID: [String: Bool]
    ) throws {
        for (reviewUID, review) in reviewsByUID {
            let reverseLinkedISIUIDs = Set(evidenceByISIUID.compactMap {
                isiUID, evidenceUIDs -> String? in
                evidenceUIDs.contains(reviewUID) ? isiUID : nil
            })
            guard reverseLinkedISIUIDs == review.linkedISIUIDs else {
                throw STPDResultPackageError.invalidTable(
                    table: "review authority",
                    reason: "review \(reviewUID) does not exactly match its declared ISI links"
                )
            }
            let linkedISIUIDs = reverseLinkedISIUIDs.sorted()
            guard !linkedISIUIDs.isEmpty else {
                continue
            }
            guard let candidate = candidatesByUID[review.candidateUID],
                  candidate.sourceID == review.sourceCandidateID else {
                throw STPDResultPackageError.invalidTable(
                    table: "review authority",
                    reason: "review \(reviewUID) references an unresolved candidate identity"
                )
            }

            for isiUID in linkedISIUIDs {
                guard let changedProjection =
                        changedProjectionByISIUID[isiUID],
                      let evidenceUIDs = evidenceByISIUID[isiUID],
                      let isi = isiRecordsByUID[isiUID] else {
                    throw STPDResultPackageError.invalidTable(
                        table: "review authority",
                        reason: "review \(reviewUID) links to an unresolved ISI"
                    )
                }
                guard isi.trainID == candidate.trainID,
                      candidate.contains(isiIndex: isi.index),
                      isi.automaticCandidateUID == review.candidateUID,
                      isi.automaticSourceCandidateID ==
                        review.sourceCandidateID else {
                    throw STPDResultPackageError.invalidTable(
                        table: "review authority",
                        reason: "review \(reviewUID) links an ISI outside its reviewed candidate"
                    )
                }

                switch review.status {
                case .accepted:
                    guard !changedProjection else {
                        throw STPDResultPackageError.invalidTable(
                            table: "review authority",
                            reason: "accepted review \(reviewUID) changes ISI \(isiUID)"
                        )
                    }
                case .rejected:
                    guard changedProjection else {
                        throw STPDResultPackageError.invalidTable(
                            table: "review authority",
                            reason: "rejected review \(reviewUID) does not causally change ISI \(isiUID)"
                        )
                    }
                case .modified:
                    guard changedProjection,
                          !evidenceUIDs.intersection(manualUIDs).isEmpty else {
                        throw STPDResultPackageError.invalidTable(
                            table: "review authority",
                            reason: "modified review \(reviewUID) lacks changed manual replacement evidence for ISI \(isiUID)"
                        )
                    }
                case .needsReview:
                    throw STPDResultPackageError.invalidTable(
                        table: "review authority",
                        reason: "needs-review record \(reviewUID) cannot govern an ISI"
                    )
                }
            }
        }
    }

    private static func validateDatasetBackedReviewCausality(
        identity: DetectionRunIdentity,
        dataset: SpikeDataset,
        qualitySettings: SpikeQualitySettings,
        manualAuthority: ValidatedManualAuthority,
        reviewsByUID: [String: ValidatedReviewRecord],
        candidatesByUID: [String: ValidatedCandidateRecord],
        publicCandidateUIDs: Set<String>,
        isiRecordsByUID: [String: ValidatedISIRecord],
        evidenceByISIUID: [String: Set<String>],
        changedProjectionByISIUID: [String: Bool]
    ) throws {
        let annotations = manualAuthority.recordsByUID.values
            .map(\.annotation)
        let annotationsByTrain = Dictionary(
            grouping: annotations,
            by: \.trainID
        )
        let projectionsByTrain = manualProjections(
            dataset: dataset,
            qualitySettings: qualitySettings,
            annotationsByTrain: annotationsByTrain,
            isiRecordsByUID: isiRecordsByUID
        )
        let authoritativeRejectedSourceIDs = Set(
            reviewsByUID.values.compactMap { review -> String? in
                review.status == .rejected
                    && publicCandidateUIDs.contains(review.candidateUID)
                    ? review.sourceCandidateID
                    : nil
            }
        )
        let fullProjection = recomputedISIProjection(
            isiRecordsByUID: isiRecordsByUID,
            projectionsByTrain: projectionsByTrain,
            rejectedSourceCandidateIDs: authoritativeRejectedSourceIDs
        )
        let baselineProjection = recomputedISIProjection(
            isiRecordsByUID: isiRecordsByUID,
            projectionsByTrain: manualProjections(
                dataset: dataset,
                qualitySettings: qualitySettings,
                annotationsByTrain: [:],
                isiRecordsByUID: isiRecordsByUID
            ),
            rejectedSourceCandidateIDs: []
        )

        for (isiUID, record) in isiRecordsByUID {
            guard let recomputed = fullProjection[isiUID],
                  let baseline = baselineProjection[isiUID] else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.isiLabelsFinal.rawValue,
                    reason: "ISI \(isiUID) is absent from a recomputed public projection"
                )
            }
            let exported = RecomputedISIProjection(
                finalPattern: record.finalPattern,
                finalSubtype: record.finalSubtype,
                finalSource: record.finalSource,
                manualVetoSuppressed: record.manualVetoSuppressed,
                reviewNote: record.reviewNote
            )
            guard exported == recomputed else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.isiLabelsFinal.rawValue,
                    reason:
                        "ISI \(isiUID) final label/source tuple does not match " +
                        "the dataset-recomputed manual/review projection"
                )
            }
            guard changedProjectionByISIUID[isiUID]
                    == (recomputed != baseline) else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.isiLabelsFinal.rawValue,
                    reason:
                        "ISI \(isiUID) review_changed_projection does not " +
                        "match the recomputed automatic baseline delta"
                )
            }
        }

        let manualOwnerUIDsByISI = recomputedManualEvidenceOwners(
            identity: identity,
            dataset: dataset,
            qualitySettings: qualitySettings,
            manualAuthority: manualAuthority,
            annotationsByTrain: annotationsByTrain,
            projectionsByTrain: projectionsByTrain,
            isiRecordsByUID: isiRecordsByUID,
            fullProjection: fullProjection,
            baselineProjection: baselineProjection,
            rejectedSourceCandidateIDs: authoritativeRejectedSourceIDs
        )
        let reviewOwnerUIDsByISI = try recomputedReviewEvidenceOwners(
            reviewsByUID: reviewsByUID,
            candidatesByUID: candidatesByUID,
            publicCandidateUIDs: publicCandidateUIDs,
            isiRecordsByUID: isiRecordsByUID,
            projectionsByTrain: projectionsByTrain,
            fullProjection: fullProjection,
            baselineProjection: baselineProjection,
            manualOwnerUIDsByISI: manualOwnerUIDsByISI,
            rejectedSourceCandidateIDs: authoritativeRejectedSourceIDs
        )

        for (manualUID, record) in manualAuthority.recordsByUID {
            let expected = Set(manualOwnerUIDsByISI.compactMap {
                isiUID, ownerUIDs -> String? in
                ownerUIDs.contains(manualUID) ? isiUID : nil
            })
            guard record.linkedISIUIDs == expected else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.manualAnnotations.rawValue,
                    reason:
                        "manual annotation \(manualUID) does not link exactly " +
                        "to its complete recomputed causal ISI set"
                )
            }
            let expectedScope: String
            if !expected.isEmpty {
                expectedScope = "public_projection"
            } else if manualAuthority.activeSpikeOnlyUIDs.contains(manualUID) {
                expectedScope = "active_spike_only"
            } else {
                expectedScope = "inactive_or_superseded"
            }
            guard record.linkScope == expectedScope else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.manualAnnotations.rawValue,
                    reason:
                        "manual annotation \(manualUID) scope does not match " +
                        "its recomputed public authority"
                )
            }
        }

        let expectedEvidenceByISI = isiRecordsByUID.keys.reduce(
            into: [String: Set<String>]()
        ) { result, isiUID in
            result[isiUID] =
                (manualOwnerUIDsByISI[isiUID] ?? [])
                .union(reviewOwnerUIDsByISI[isiUID] ?? [])
        }
        for isiUID in isiRecordsByUID.keys {
            guard evidenceByISIUID[isiUID] == expectedEvidenceByISI[isiUID] else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.isiLabelsFinal.rawValue,
                    reason:
                        "ISI \(isiUID) review evidence is not the complete " +
                        "recomputed causal owner set"
                )
            }
        }
    }

    private static func manualProjections(
        dataset: SpikeDataset,
        qualitySettings: SpikeQualitySettings,
        annotationsByTrain: [String: [ManualAnnotation]],
        isiRecordsByUID: [String: ValidatedISIRecord]
    ) -> [String: ManualAnnotationProjection] {
        let recordsByTrain = Dictionary(
            grouping: isiRecordsByUID.values,
            by: \.trainID
        )
        return Dictionary(
            uniqueKeysWithValues: dataset.trains.map { train in
                let autoLabels = Dictionary(
                    uniqueKeysWithValues: (recordsByTrain[train.id] ?? [])
                        .filter { !$0.automaticPattern.isEmpty }
                        .map { ($0.index, $0.automaticPattern) }
                )
                return (
                    train.id,
                    ManualAnnotationProjector.project(
                        train: train,
                        autoLabelsByISI: autoLabels,
                        annotations: annotationsByTrain[train.id] ?? [],
                        honorManualLock: true,
                        manualNegativeLabelsEnabled: true,
                        minValidISISeconds:
                            qualitySettings.artifactThresholdSec
                    )
                )
            }
        )
    }

    private static func recomputedISIProjection(
        isiRecordsByUID: [String: ValidatedISIRecord],
        projectionsByTrain: [String: ManualAnnotationProjection],
        rejectedSourceCandidateIDs: Set<String>
    ) -> [String: RecomputedISIProjection] {
        var result: [String: RecomputedISIProjection] = [:]
        for (isiUID, record) in isiRecordsByUID {
            let projection = projectionsByTrain[record.trainID]
            let positive = projection?
                .manualPositiveLabelByISI[record.index]
            let vetoed = projection?
                .autoBurstBlockedByVetoISIs.contains(record.index) == true
            let projected: RecomputedISIProjection
            if let positive {
                projected = RecomputedISIProjection(
                    finalPattern: positive,
                    finalSubtype: "",
                    finalSource:
                        ReviewedISIExportBuilder.sourceManualPositive,
                    manualVetoSuppressed: false,
                    reviewNote: ""
                )
            } else if !record.automaticSourceCandidateID.isEmpty,
                      rejectedSourceCandidateIDs.contains(
                        record.automaticSourceCandidateID
                      ) {
                projected = RecomputedISIProjection(
                    finalPattern: "",
                    finalSubtype: "",
                    finalSource:
                        ReviewedISIExportBuilder.sourceManualReviewRejected,
                    manualVetoSuppressed: false,
                    reviewNote: ""
                )
            } else if ManualAnnotationProjector.burstFamilyLabels.contains(
                        record.automaticPattern
                      ),
                      vetoed {
                projected = RecomputedISIProjection(
                    finalPattern: "",
                    finalSubtype: "",
                    finalSource:
                        ReviewedISIExportBuilder.sourceManualVetoRemoved,
                    manualVetoSuppressed: true,
                    reviewNote: ""
                )
            } else if !record.automaticPattern.isEmpty {
                projected = RecomputedISIProjection(
                    finalPattern: record.automaticPattern,
                    finalSubtype: record.automaticSubtype,
                    finalSource:
                        ReviewedISIExportBuilder.sourceAutoProjected,
                    manualVetoSuppressed: false,
                    reviewNote: ""
                )
            } else {
                projected = RecomputedISIProjection(
                    finalPattern: "",
                    finalSubtype: "",
                    finalSource: ReviewedISIExportBuilder.sourceNone,
                    manualVetoSuppressed: false,
                    reviewNote: ""
                )
            }
            result[isiUID] = projected
        }

        let recordsByTrain = Dictionary(
            grouping: isiRecordsByUID.values,
            by: \.trainID
        )
        for records in recordsByTrain.values {
            let uidByIndex = Dictionary(
                uniqueKeysWithValues: records.map { ($0.index, $0.uid) }
            )
            let finalLabels = Dictionary(
                uniqueKeysWithValues: records.compactMap { record
                    -> (Int, String)? in
                    guard let label = result[record.uid]?.finalPattern,
                          !label.isEmpty else {
                        return nil
                    }
                    return (record.index, label)
                }
            )
            let invalid = ManualAnnotationProjector
                .invalidBurstFragmentISIs(
                    in: finalLabels,
                    minimumRunLength:
                        ReviewedISIExportBuilder.burstMinimumISIRunLength,
                    burstLabels:
                        ManualAnnotationProjector.burstFamilyLabels
                )
            for index in invalid {
                guard let uid = uidByIndex[index] else { continue }
                result[uid] = RecomputedISIProjection(
                    finalPattern: "",
                    finalSubtype: "",
                    finalSource:
                        ReviewedISIExportBuilder.sourceInvalidBurstFragment,
                    manualVetoSuppressed: false,
                    reviewNote: "burst_run_below_min_isi"
                )
            }
        }
        return result
    }

    private static func recomputedManualEvidenceOwners(
        identity: DetectionRunIdentity,
        dataset: SpikeDataset,
        qualitySettings: SpikeQualitySettings,
        manualAuthority: ValidatedManualAuthority,
        annotationsByTrain: [String: [ManualAnnotation]],
        projectionsByTrain: [String: ManualAnnotationProjection],
        isiRecordsByUID: [String: ValidatedISIRecord],
        fullProjection: [String: RecomputedISIProjection],
        baselineProjection: [String: RecomputedISIProjection],
        rejectedSourceCandidateIDs: Set<String>
    ) -> [String: Set<String>] {
        let recordsByTrain = Dictionary(
            grouping: isiRecordsByUID.values,
            by: \.trainID
        )
        let manualUIDByUUID = Dictionary(
            uniqueKeysWithValues: manualAuthority.recordsByUID.values.map {
                ($0.annotation.id, $0.uid)
            }
        )
        var ownersByISIUID: [String: Set<String>] = [:]

        for train in dataset.trains {
            let trainAnnotations = annotationsByTrain[train.id] ?? []
            guard !trainAnnotations.isEmpty,
                  let fullTrainProjection = projectionsByTrain[train.id] else {
                continue
            }

            func projection(removing annotationIDs: Set<UUID>)
                -> ManualAnnotationProjection {
                let autoLabels = Dictionary(
                    uniqueKeysWithValues: (recordsByTrain[train.id] ?? [])
                        .filter { !$0.automaticPattern.isEmpty }
                        .map { ($0.index, $0.automaticPattern) }
                )
                return ManualAnnotationProjector.project(
                    train: train,
                    autoLabelsByISI: autoLabels,
                    annotations: trainAnnotations.filter {
                        !annotationIDs.contains($0.id)
                    },
                    honorManualLock: true,
                    manualNegativeLabelsEnabled: true,
                    minValidISISeconds:
                        qualitySettings.artifactThresholdSec
                )
            }

            func recordChanges(
                alternateTrainProjection: ManualAnnotationProjection,
                ownerID: UUID
            ) {
                guard let ownerUID = manualUIDByUUID[ownerID] else { return }
                var alternateProjections = projectionsByTrain
                alternateProjections[train.id] =
                    alternateTrainProjection
                let alternate = recomputedISIProjection(
                    isiRecordsByUID: isiRecordsByUID,
                    projectionsByTrain: alternateProjections,
                    rejectedSourceCandidateIDs:
                        rejectedSourceCandidateIDs
                )
                for record in recordsByTrain[train.id] ?? []
                where fullProjection[record.uid] != alternate[record.uid] {
                    ownersByISIUID[record.uid, default: []].insert(
                        ownerUID
                    )
                }
            }

            for annotation in trainAnnotations {
                recordChanges(
                    alternateTrainProjection: projection(
                        removing: [annotation.id]
                    ),
                    ownerID: annotation.id
                )
            }

            func effectiveIndices(
                _ annotation: ManualAnnotation
            ) -> Set<Int> {
                guard let covered = ManualAnnotationGeometryResolver
                    .resolve(annotation: annotation, in: train)
                    .coveredISIIndices else {
                    return []
                }
                return Set(covered.filter { index in
                    guard train.isiSec.indices.contains(index),
                          let value = train.isiSec[index] else {
                        return false
                    }
                    return value.isFinite
                        && value >= qualitySettings.artifactThresholdSec
                })
            }
            let annotationsByID = Dictionary(
                uniqueKeysWithValues: trainAnnotations.map { ($0.id, $0) }
            )
            let activeOwnerIDs = Set(
                fullTrainProjection.manualPositiveOwnerByISI.values
            )
            .union(fullTrainProjection.manualNegativeVetoOwnerByISI.values)
            for ownerID in activeOwnerIDs.sorted(by: {
                $0.uuidString < $1.uuidString
            }) {
                guard let owner = annotationsByID[ownerID] else { continue }
                let ownerIndices = effectiveIndices(owner)
                let equivalentIDs = Set(trainAnnotations.compactMap {
                    annotation -> UUID? in
                    annotation.label == owner.label
                        && effectiveIndices(annotation) == ownerIndices
                        ? annotation.id
                        : nil
                })
                guard equivalentIDs.count > 1 else { continue }
                recordChanges(
                    alternateTrainProjection: projection(
                        removing: equivalentIDs
                    ),
                    ownerID: ownerID
                )
            }

            for record in recordsByTrain[train.id] ?? []
            where fullProjection[record.uid]
                    != baselineProjection[record.uid] {
                let ownerID: UUID?
                switch fullProjection[record.uid]?.finalSource {
                case ReviewedISIExportBuilder.sourceManualPositive:
                    ownerID = fullTrainProjection
                        .manualPositiveOwnerByISI[record.index]
                case ReviewedISIExportBuilder.sourceManualVetoRemoved:
                    ownerID = fullTrainProjection
                        .manualNegativeVetoOwnerByISI[record.index]
                default:
                    ownerID = nil
                }
                if let ownerID, let ownerUID = manualUIDByUUID[ownerID] {
                    ownersByISIUID[record.uid, default: []].insert(
                        ownerUID
                    )
                }
            }
        }
        _ = identity
        return ownersByISIUID
    }

    private static func recomputedReviewEvidenceOwners(
        reviewsByUID: [String: ValidatedReviewRecord],
        candidatesByUID: [String: ValidatedCandidateRecord],
        publicCandidateUIDs: Set<String>,
        isiRecordsByUID: [String: ValidatedISIRecord],
        projectionsByTrain: [String: ManualAnnotationProjection],
        fullProjection: [String: RecomputedISIProjection],
        baselineProjection: [String: RecomputedISIProjection],
        manualOwnerUIDsByISI: [String: Set<String>],
        rejectedSourceCandidateIDs: Set<String>
    ) throws -> [String: Set<String>] {
        var ownersByISIUID: [String: Set<String>] = [:]
        for review in reviewsByUID.values {
            guard let candidate = candidatesByUID[review.candidateUID],
                  candidate.sourceID == review.sourceCandidateID else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.reviewStatus.rawValue,
                    reason: "review \(review.uid) has no resolvable candidate"
                )
            }
            let isPublicCandidate = publicCandidateUIDs.contains(
                review.candidateUID
            )
            let candidateRecords = isiRecordsByUID.values.filter {
                $0.automaticSourceCandidateID
                    == review.sourceCandidateID
            }
            let expected: Set<String>
            if !isPublicCandidate {
                expected = []
            } else {
                switch review.status {
                case .accepted:
                    let whollyUnchanged = !candidateRecords.isEmpty
                        && candidateRecords.allSatisfy { record in
                            fullProjection[record.uid]?.finalSource
                                    == ReviewedISIExportBuilder
                                        .sourceAutoProjected
                                && fullProjection[record.uid]
                                    == baselineProjection[record.uid]
                        }
                    expected = whollyUnchanged
                        ? Set(candidateRecords.map(\.uid))
                        : []
                case .rejected:
                    let hasPublicBaseline = candidateRecords.contains {
                        baselineProjection[$0.uid]?.finalSource
                            == ReviewedISIExportBuilder.sourceAutoProjected
                    }
                    if hasPublicBaseline {
                        let counterfactual = recomputedISIProjection(
                            isiRecordsByUID: isiRecordsByUID,
                            projectionsByTrain: projectionsByTrain,
                            rejectedSourceCandidateIDs:
                                rejectedSourceCandidateIDs.subtracting([
                                    review.sourceCandidateID,
                                ])
                        )
                        expected = Set(isiRecordsByUID.values.compactMap {
                            record -> String? in
                            fullProjection[record.uid]
                                != counterfactual[record.uid]
                                ? record.uid
                                : nil
                        })
                    } else {
                        expected = []
                    }
                case .modified:
                    let changed = Set(candidateRecords.compactMap {
                        record -> String? in
                        fullProjection[record.uid]
                            != baselineProjection[record.uid]
                            ? record.uid
                            : nil
                    })
                    expected = !changed.isEmpty
                        && changed.allSatisfy {
                            !(manualOwnerUIDsByISI[$0] ?? []).isEmpty
                        }
                        ? changed
                        : []
                case .needsReview:
                    expected = []
                }
            }

            guard review.linkedISIUIDs == expected else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.reviewStatus.rawValue,
                    reason:
                        "candidate review \(review.uid) does not link exactly " +
                        "to its complete recomputed causal public ISI set"
                )
            }
            let expectedScope = expected.isEmpty
                ? "non_authoritative"
                : "public_projection"
            guard review.linkScope == expectedScope else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.reviewStatus.rawValue,
                    reason:
                        "candidate review \(review.uid) scope does not match " +
                        "its recomputed public authority"
                )
            }
            for isiUID in expected {
                ownersByISIUID[isiUID, default: []].insert(review.uid)
            }
        }
        return ownersByISIUID
    }

    private static func validateEvidenceLinks(
        table: STPDResultTableData,
        evidenceUIDColumn: String,
        isiUIDs: Set<String>,
        evidenceByISIUID: [String: Set<String>]
    ) throws {
        let evidenceIndex = try columnIndex(evidenceUIDColumn, in: table)
        let linksIndex = try columnIndex("linked_isi_uids", in: table)
        let scopeIndex = try columnIndex("link_scope", in: table)
        let unlinkedScope: String
        switch table.contract.table {
        case .manualAnnotations:
            unlinkedScope = ""
        case .reviewStatus:
            unlinkedScope = "non_authoritative"
        default:
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "evidence-link validation does not support this table"
            )
        }
        for row in table.rows {
            let evidenceUID = row[evidenceIndex]
            let linkedUIDs = delimitedValues(row[linksIndex])
            if linkedUIDs.isEmpty {
                let allowedUnlinkedScopes: Set<String>
                if table.contract.table == .manualAnnotations {
                    allowedUnlinkedScopes = [
                        "inactive_or_superseded",
                        "active_spike_only",
                    ]
                } else {
                    allowedUnlinkedScopes = [unlinkedScope]
                }
                guard allowedUnlinkedScopes.contains(row[scopeIndex]),
                      evidenceByISIUID.values.allSatisfy({
                          !$0.contains(evidenceUID)
                      }) else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "\(evidenceUIDColumn) \(evidenceUID) has inconsistent inactive authority"
                    )
                }
                continue
            }
            guard row[scopeIndex] == "public_projection",
                  linkedUIDs.isSubset(of: isiUIDs) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "\(evidenceUIDColumn) \(evidenceUID) links to an unknown ISI"
                )
            }
            let reverseLinkedUIDs = Set(evidenceByISIUID.compactMap {
                isiUID, evidenceUIDs -> String? in
                evidenceUIDs.contains(evidenceUID) ? isiUID : nil
            })
            guard linkedUIDs == reverseLinkedUIDs else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "\(evidenceUIDColumn) \(evidenceUID) must match its ISI links exactly"
                )
            }
        }
    }

    private static func canonicalTimestamp(
        _ encodedTimestamp: String,
        matchesExactSeconds exact: Double
    ) -> Bool {
        guard exact.isFinite,
              encodedTimestamp == STPDCanonicalValue.date(
                Date(timeIntervalSince1970: exact)
              ),
              let parsedSeconds =
                STPDResultTimestamp.exactSeconds(encodedTimestamp) else {
            return false
        }
        return parsedSeconds.bitPattern == exact.bitPattern
    }

    private static func validateRunMetadata(
        identity: DetectionRunIdentity,
        sourceMode: STPDResultPackageSourceMode,
        metadata: STPDResultTableData,
        expectedDatasetMetadata: DetectionDatasetMetadataSnapshot?,
        tables: [STPDResultTable: STPDResultTableData]
    ) throws {
        guard metadata.rowCount == 1 else {
            throw STPDResultPackageError.invalidTable(
                table: metadata.contract.table.rawValue,
                reason: "run metadata must contain exactly one row"
            )
        }
        let row = metadata.rows[0]
        func value(_ column: String) throws -> String {
            row[try columnIndex(column, in: metadata)]
        }
        func requireEqual(
            _ column: String,
            _ expected: String
        ) throws {
            let actual = try value(column)
            guard actual == expected else {
                throw STPDResultPackageError.invalidTable(
                    table: metadata.contract.table.rawValue,
                    reason: "run metadata \(column) does not match the sealed run"
                )
            }
        }
        func requireCount(
            _ column: String,
            _ expected: Int
        ) throws {
            let actual = try parseInteger(
                value(column),
                table: metadata,
                column: column
            )
            guard actual == expected else {
                throw STPDResultPackageError.invalidTable(
                    table: metadata.contract.table.rawValue,
                    reason: "run metadata \(column) contradicts its materialized population"
                )
            }
        }

        try requireEqual("run_id", identity.runID)
        try requireEqual("settings_digest", identity.settingsDigest)
        try requireEqual("dataset_digest", identity.datasetDigest)
        try requireEqual(
            "result_schema_version",
            identity.resultSchemaVersion
        )
        try requireEqual("detector_version", identity.detectorVersion)
        try requireEqual("build_identifier", identity.buildCommit)
        try requireEqual(
            "build_identifier_kind",
            "caller_supplied_unattested"
        )
        guard try !parseBoolean(
            value("build_reproducibility_attested"),
            table: metadata,
            column: "build_reproducibility_attested"
        ) else {
            throw STPDResultPackageError.invalidTable(
                table: metadata.contract.table.rawValue,
                reason: "caller-supplied build identifier cannot claim reproducibility attestation"
            )
        }
        try requireEqual(
            "owner_name",
            STPDResultPackageOwnership.ownerName
        )
        try requireEqual(
            "owner_email",
            STPDResultPackageOwnership.ownerEmail
        )
        try requireEqual("source_mode", sourceMode.rawValue)

        if let expectedDatasetMetadata {
            try requireEqual("dataset_name", expectedDatasetMetadata.name)
            try requireEqual(
                "dataset_source",
                expectedDatasetMetadata.sourceDescription
            )
            try requireEqual(
                "task_event_source_digest",
                expectedDatasetMetadata.taskEventSourceDigest
            )
        }

        try requireCount("train_count", identity.trainCount)
        try requireCount("spike_count", identity.spikeCount)
        try requireCount("task_event_count", identity.taskEventCount)
        try requireCount(
            "candidate_count",
            try required(.candidateLedger, in: tables).rowCount
        )
        try requireCount(
            "diagnostic_candidate_count",
            try required(.candidateLedgerDiagnostic, in: tables).rowCount
        )
        try requireCount(
            "final_event_count",
            try required(.eventsFinal, in: tables).rowCount
        )
        try requireCount(
            "final_isi_count",
            try required(.isiLabelsFinal, in: tables).rowCount
        )
    }

    private static func validateExactISICoverage(
        identity: DetectionRunIdentity,
        table: STPDResultTableData,
        dataset: SpikeDataset
    ) throws {
        let uidIndex = try columnIndex("isi_uid", in: table)
        let trainIndex = try columnIndex("train_id", in: table)
        let trainNameIndex = try columnIndex("train_name", in: table)
        let isiIndex = try columnIndex("isi_index", in: table)
        let leftArrayIndex = try columnIndex("left_spike_array_index", in: table)
        let rightArrayIndex = try columnIndex("right_spike_array_index", in: table)
        let leftOrdinalIndex = try columnIndex("left_spike_ordinal", in: table)
        let rightOrdinalIndex = try columnIndex("right_spike_ordinal", in: table)
        let timestampIndex = try columnIndex("timestamp_sec", in: table)
        let alignedTimestampIndex = try columnIndex(
            "aligned_timestamp_sec",
            in: table
        )
        let isiSecIndex = try columnIndex("isi_sec", in: table)

        var actualRowsByKey: [String: [String]] = [:]
        for row in table.rows {
            let index = try parseInteger(
                row[isiIndex],
                table: table,
                column: "isi_index"
            )
            let key = finalProjectionKey(
                trainID: row[trainIndex],
                isiIndex: index
            )
            guard actualRowsByKey.updateValue(row, forKey: key) == nil else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "duplicate dataset ISI key \(row[trainIndex]):\(index)"
                )
            }
        }

        var expectedKeys = Set<String>()
        for train in dataset.trains {
            let aligned = train.alignedTimestampsSec.count == train.spikeCount
                ? train.alignedTimestampsSec
                : train.timestampsSec
            guard aligned.count == train.spikeCount else {
                throw STPDResultPackageError.invalidInput(
                    "train \(train.id) cannot provide authoritative aligned timestamps"
                )
            }
            guard train.spikeCount >= 2 else {
                continue
            }
            for index in 1..<train.spikeCount {
                let key = finalProjectionKey(trainID: train.id, isiIndex: index)
                expectedKeys.insert(key)
                guard let row = actualRowsByKey[key] else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "missing dataset ISI \(train.id):\(index)"
                    )
                }
                let expectedUID = STPDResultPackageBuilder.stableISIUID(
                    datasetDigest: identity.datasetDigest,
                    trainID: train.id,
                    isiIndex: index
                )
                let expectedISI =
                    train.timestampsSec[index] - train.timestampsSec[index - 1]
                guard row[uidIndex] == expectedUID,
                      row[trainNameIndex] == train.name,
                      row[leftArrayIndex] == String(index - 1),
                      row[rightArrayIndex] == String(index),
                      row[leftOrdinalIndex] == String(index),
                      row[rightOrdinalIndex] == String(index + 1),
                      row[timestampIndex] ==
                        STPDCanonicalValue.double(train.timestampsSec[index]),
                      row[alignedTimestampIndex] ==
                        STPDCanonicalValue.double(aligned[index]),
                      row[isiSecIndex] == STPDCanonicalValue.double(expectedISI)
                else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "dataset ISI \(train.id):\(index) has forged UID, train name, spike geometry, timestamp, or duration"
                    )
                }
            }
        }
        guard Set(actualRowsByKey.keys) == expectedKeys else {
            let unexpected = Set(actualRowsByKey.keys).subtracting(expectedKeys)
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "ISI projection contains \(unexpected.count) row(s) outside the sealed dataset population"
            )
        }
    }

    private static func validateQCColumns(
        _ table: STPDResultTableData,
        resolvedParameters: STPDResultTableData,
        expectedDataset: SpikeDataset?,
        expectedSettings: SpikeQualitySettings?
    ) throws {
        let classIndex = try columnIndex("isi_qc_class", in: table)
        let floorStatusIndex = try columnIndex("artifact_floor_status", in: table)
        let suspectIndex = try columnIndex("qc_refractory_suspect", in: table)
        let artifactThresholdIndex = try columnIndex(
            "qc_artifact_threshold_sec",
            in: table
        )
        let refractoryThresholdIndex = try columnIndex(
            "qc_refractory_threshold_sec",
            in: table
        )
        let isiIndex = try columnIndex("isi_sec", in: table)
        let trainIndex = try columnIndex("train_id", in: table)
        let snapshotColumns = table.headers.filter {
            $0.hasPrefix("train_qc_")
        }
        let snapshotIndices = try snapshotColumns.map {
            try columnIndex($0, in: table)
        }
        var snapshotByTrain: [String: [String]] = [:]
        var rowCountByTrain: [String: Int] = [:]
        var artifactCountByTrain: [String: Int] = [:]
        var refractoryCountByTrain: [String: Int] = [:]
        for row in table.rows {
            guard let isi = Double(row[isiIndex]),
                  isi.isFinite,
                  let artifactThreshold = Double(row[artifactThresholdIndex]),
                  artifactThreshold.isFinite,
                  artifactThreshold >= 0,
                  let refractoryThreshold = Double(row[refractoryThresholdIndex]),
                  refractoryThreshold.isFinite,
                  refractoryThreshold >= 0 else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "ISI QC row contains an invalid ISI or QC threshold"
                )
            }
            let artifactTolerance = max(1e-12, abs(artifactThreshold) * 1e-6)
            let refractoryTolerance = max(
                1e-12,
                abs(refractoryThreshold) * 1e-6
            )
            let expectedClass: String
            if isi < artifactThreshold - artifactTolerance {
                expectedClass = "artifact_below_floor"
            } else if refractoryThreshold > artifactThreshold,
                      isi < refractoryThreshold - refractoryTolerance {
                expectedClass = "refractory_suspect"
            } else {
                expectedClass = "valid"
            }
            let suspect = try parseBoolean(
                row[suspectIndex],
                table: table,
                column: "qc_refractory_suspect"
            )
            guard row[classIndex] == expectedClass,
                  row[floorStatusIndex] ==
                    (expectedClass == "artifact_below_floor"
                        ? "below_artifact_floor"
                        : "at_or_above_artifact_floor"),
                  suspect == (expectedClass == "refractory_suspect") else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "ISI QC row has contradictory classification fields"
                )
            }
            let trainID = row[trainIndex]
            let snapshot = snapshotIndices.map { row[$0] }
            if let prior = snapshotByTrain[trainID], prior != snapshot {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "train \(trainID) has inconsistent repeated QC snapshots"
                )
            }
            snapshotByTrain[trainID] = snapshot
            rowCountByTrain[trainID, default: 0] += 1
            artifactCountByTrain[trainID, default: 0] +=
                expectedClass == "artifact_below_floor" ? 1 : 0
            refractoryCountByTrain[trainID, default: 0] +=
                expectedClass == "refractory_suspect" ? 1 : 0
        }

        let countColumns = [
            "train_qc_duplicate_timestamp_count",
            "train_qc_zero_or_negative_isi_count",
            "train_qc_zero_or_negative_timestamp_step_count",
            "train_qc_input_nonmonotonic_step_count",
            "train_qc_input_duplicate_timestamp_step_count",
            "train_qc_input_zero_or_negative_step_count",
            "train_qc_dropped_duplicate_timestamp_count",
            "train_qc_artifact_isi_count",
            "train_qc_refractory_suspect_isi_count",
            "train_qc_valid_isi_count",
        ]
        let fractionColumns = [
            "train_qc_artifact_fraction",
            "train_qc_refractory_suspect_fraction",
        ]
        for (trainID, snapshot) in snapshotByTrain {
            let values = Dictionary(
                uniqueKeysWithValues: zip(snapshotColumns, snapshot)
            )
            for column in countColumns {
                guard let value = values[column],
                      let count = Int(value),
                      count >= 0 else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "train \(trainID) has invalid QC count \(column)"
                    )
                }
            }
            for column in fractionColumns {
                guard let value = values[column], !value.isEmpty else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "train \(trainID) is missing QC fraction \(column)"
                    )
                }
                guard let fraction = Double(value),
                      fraction.isFinite,
                      (0...1).contains(fraction) else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "train \(trainID) has invalid QC fraction \(column)"
                    )
                }
            }
            let rowCount = rowCountByTrain[trainID] ?? 0
            let artifactCount = artifactCountByTrain[trainID] ?? 0
            let refractoryCount = refractoryCountByTrain[trainID] ?? 0
            let validCount = rowCount - artifactCount
            let expectedArtifactFraction =
                Double(artifactCount) / Double(rowCount)
            let expectedRefractoryFraction =
                Double(refractoryCount) / Double(rowCount)
            guard Int(values["train_qc_artifact_isi_count"] ?? "") ==
                    artifactCount,
                  Int(values["train_qc_refractory_suspect_isi_count"] ?? "") ==
                    refractoryCount,
                  Int(values["train_qc_valid_isi_count"] ?? "") ==
                    validCount,
                  let artifactFraction = Double(
                      values["train_qc_artifact_fraction"] ?? ""
                  ),
                  let refractoryFraction = Double(
                      values["train_qc_refractory_suspect_fraction"] ?? ""
                  ),
                  nearlyEqual(
                      artifactFraction,
                      expectedArtifactFraction
                  ),
                  nearlyEqual(
                      refractoryFraction,
                      expectedRefractoryFraction
                  ) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "train \(trainID) QC counts or fractions contradict its exported ISI rows"
                )
            }
        }

        guard let expectedDataset, let expectedSettings else {
            return
        }
        let expectedQualities = Dictionary(
            uniqueKeysWithValues: expectedDataset.trains.map { train in
                (
                    train.id,
                    STPDResultPackageBuilder.qualitySnapshotValues(
                        SpikeQualityAnalyzer.quality(
                            for: train,
                            settings: expectedSettings
                        )
                    )
                )
            }
        )
        let expectedTrainNames = Dictionary(
            uniqueKeysWithValues: expectedDataset.trains.map { ($0.id, $0.name) }
        )
        let artifactThreshold = STPDCanonicalValue.double(
            expectedSettings.artifactThresholdSec
        )
        let refractoryThreshold = STPDCanonicalValue.double(
            expectedSettings.refractorySuspectThresholdSec
        )
        for row in table.rows {
            let trainID = row[trainIndex]
            guard let expected = expectedQualities[trainID] else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "ISI QC row references train \(trainID) outside the sealed dataset"
                )
            }
            guard row[artifactThresholdIndex] == artifactThreshold,
                  row[refractoryThresholdIndex] == refractoryThreshold else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "train \(trainID) exports QC thresholds that differ from the sealed settings"
                )
            }
            for column in snapshotColumns {
                let qualityKey = String(column.dropFirst("train_qc_".count))
                guard let expectedValue = expected[qualityKey],
                      row[try columnIndex(column, in: table)] == expectedValue else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "train \(trainID) QC field \(column) differs from a fresh analyzer result"
                    )
                }
            }
        }

        let scopeTypeIndex = try columnIndex(
            "scope_type",
            in: resolvedParameters
        )
        let scopeIDIndex = try columnIndex("scope_id", in: resolvedParameters)
        let scopeNameIndex = try columnIndex(
            "scope_name",
            in: resolvedParameters
        )
        let keyIndex = try columnIndex(
            "parameter_key",
            in: resolvedParameters
        )
        let effectiveIndex = try columnIndex(
            "effective_value",
            in: resolvedParameters
        )
        let sourceIndex = try columnIndex("source", in: resolvedParameters)
        var actualQualityRows: [String: [String]] = [:]
        for row in resolvedParameters.rows
        where row[scopeTypeIndex] == "train"
            && row[keyIndex].hasPrefix("quality.") {
            let trainID = row[scopeIDIndex]
            let qualityKey = String(row[keyIndex].dropFirst("quality.".count))
            let compoundKey = "\(trainID)\u{1}\(qualityKey)"
            guard actualQualityRows.updateValue(row, forKey: compoundKey) == nil else {
                throw STPDResultPackageError.invalidTable(
                    table: resolvedParameters.contract.table.rawValue,
                    reason: "duplicate resolved QC field \(trainID):\(qualityKey)"
                )
            }
        }
        var expectedResolvedKeys = Set<String>()
        for train in expectedDataset.trains {
            guard let values = expectedQualities[train.id] else {
                continue
            }
            for key in values.keys {
                let compoundKey = "\(train.id)\u{1}\(key)"
                expectedResolvedKeys.insert(compoundKey)
                guard let row = actualQualityRows[compoundKey],
                      row[scopeTypeIndex] == "train",
                      row[scopeNameIndex] == (expectedTrainNames[train.id] ?? ""),
                      row[sourceIndex] == "quality_analyzer",
                      row[effectiveIndex] == values[key] else {
                    throw STPDResultPackageError.invalidTable(
                        table: resolvedParameters.contract.table.rawValue,
                        reason: "resolved QC field \(train.id):\(key) is missing or differs from a fresh analyzer result"
                    )
                }
            }
        }
        guard Set(actualQualityRows.keys) == expectedResolvedKeys else {
            throw STPDResultPackageError.invalidTable(
                table: resolvedParameters.contract.table.rawValue,
                reason: "resolved QC rows do not exactly cover every sealed dataset train and analyzer field"
            )
        }
    }

    private static func validateConsistencyTable(
        _ table: STPDResultTableData,
        expected: [STPDConsistencyCheck]
    ) throws {
        let idIndex = try columnIndex("check_id", in: table)
        let statusIndex = try columnIndex("status", in: table)
        let severityIndex = try columnIndex("severity", in: table)
        let detailsIndex = try columnIndex("details", in: table)
        let actual = table.rows.map {
            [$0[idIndex], $0[statusIndex], $0[severityIndex], $0[detailsIndex]]
        }
        let required = expected.map { [$0.id, $0.status, $0.severity, $0.details] }
        guard actual == required else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "consistency rows do not exactly match the checks that were executed"
            )
        }
    }

    private static func nonemptyValues(
        table: STPDResultTableData,
        column: String
    ) throws -> Set<String> {
        Set(try values(table: table, column: column).filter { !$0.isEmpty })
    }

    private static func delimitedValues(_ value: String) -> Set<String> {
        Set(STPDCanonicalValue.parseStringList(value) ?? [])
    }

    private static func canonicalDelimitedValues(
        _ value: String,
        table: STPDResultTableData,
        column: String,
        requireSorted: Bool
    ) throws -> Set<String> {
        guard let values = STPDCanonicalValue.parseStringList(value),
              values.allSatisfy({ !$0.isEmpty }),
              Set(values).count == values.count,
              STPDCanonicalValue.stringList(values) == value,
              !requireSorted || values == values.sorted() else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "column \(column) contains a noncanonical list"
            )
        }
        return Set(values)
    }

    private static func finalProjectionKey(
        trainID: String,
        isiIndex: Int
    ) -> String {
        "\(trainID)\u{1}\(isiIndex)"
    }

    private static func parseInteger(
        _ value: String,
        table: STPDResultTableData,
        column: String
    ) throws -> Int {
        guard let parsed = Int(value) else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "column \(column) contains non-integer value \(value)"
            )
        }
        return parsed
    }

    private static func parseOptionalInteger(
        _ value: String,
        table: STPDResultTableData,
        column: String
    ) throws -> Int? {
        guard !value.isEmpty else { return nil }
        return try parseInteger(value, table: table, column: column)
    }

    private static func orderedOptionalPair(
        _ start: Int?,
        _ end: Int?
    ) -> Bool {
        switch (start, end) {
        case (nil, nil):
            return true
        case let (.some(start), .some(end)):
            return start <= end
        default:
            return false
        }
    }

    private static func incrementedWithoutOverflow(_ value: Int?) -> Int? {
        guard let value else { return nil }
        let (incremented, overflow) = value.addingReportingOverflow(1)
        return overflow ? nil : incremented
    }

    private static func parseFiniteReal(
        _ value: String,
        table: STPDResultTableData,
        column: String
    ) throws -> Double {
        guard let parsed = Double(value), parsed.isFinite else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "column \(column) contains non-finite real value \(value)"
            )
        }
        return parsed
    }

    private static func nearlyEqual(
        _ lhs: Double,
        _ rhs: Double
    ) -> Bool {
        guard lhs.isFinite, rhs.isFinite else { return false }
        let ulpTolerance = max(lhs.ulp, rhs.ulp) * 8
        return abs(lhs - rhs) <= max(1e-12, ulpTolerance)
    }

    private static func parseBoolean(
        _ value: String,
        table: STPDResultTableData,
        column: String
    ) throws -> Bool {
        switch value {
        case "true":
            return true
        case "false":
            return false
        default:
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "column \(column) contains non-boolean value \(value)"
            )
        }
    }

    private static func columnIndex(
        _ column: String,
        in table: STPDResultTableData
    ) throws -> Int {
        guard let index = table.headers.firstIndex(of: column) else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "missing column \(column)"
            )
        }
        return index
    }

    private static func pass(
        _ id: String,
        details: String
    ) -> STPDConsistencyCheck {
        STPDConsistencyCheck(
            id: id,
            status: "pass",
            severity: "info",
            details: details
        )
    }
}
