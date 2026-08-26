import Foundation

/// Exact, index-preserving preparation for a positive sub-floor ISI.
///
/// A positive ISI below the configured validity floor does not identify which of its two
/// boundary spikes is erroneous. The detector must therefore consider two explicit virtual
/// spike-removal hypotheses instead of sorting, deleting the ISI value, or joining its two
/// neighbours. This type constructs those two hypotheses and carries the mapping back to the
/// untouched original spike/ISI geometry.
enum PositiveTinyISIHypothesisKind: String, Sendable, CaseIterable {
    case earlierSpikeErroneous = "H_earlier"
    case laterSpikeErroneous = "H_later"
}

enum PositiveTinyISIResolutionStatus: String, Sendable {
    case eligibleForExactHypotheses = "eligible_for_exact_hypotheses"
    case requiresReview = "requires_review"
}

struct PositiveTinyISIHypothesisScenario: Sendable {
    let kind: PositiveTinyISIHypothesisKind
    let originalISIIndex: Int
    let removedOriginalSpikeIndex: Int
    let mergedVirtualISIIndex: Int
    let virtualTrain: SpikeTrain

    /// Maps a virtual inclusive ISI span back to the smallest inclusive span of original
    /// ISIs that generated it. The one virtual interval crossing the removed spike maps to
    /// two original intervals; all other virtual intervals map one-to-one.
    func originalISISpan(forVirtualStart start: Int, end: Int) -> ClosedRange<Int>? {
        guard start >= 1, end >= start, end < virtualTrain.isiSec.count else {
            return nil
        }
        let lower = originalFootprint(ofVirtualISI: start).lowerBound
        let upper = originalFootprint(ofVirtualISI: end).upperBound
        return lower...upper
    }

    func containsAmbiguitySupport(startISIIndex: Int, endISIIndex: Int) -> Bool {
        startISIIndex <= mergedVirtualISIIndex && mergedVirtualISIIndex <= endISIIndex
    }

    private func originalFootprint(ofVirtualISI index: Int) -> ClosedRange<Int> {
        let mergeIndex = removedOriginalSpikeIndex - 1
        if index < mergeIndex {
            return index...index
        }
        if index == mergeIndex {
            return mergeIndex...removedOriginalSpikeIndex
        }
        let originalIndex = index + 1
        return originalIndex...originalIndex
    }
}

struct PositiveTinyISIHypothesisPlan: Sendable {
    let originalISIIndex: Int
    let valueSec: Double
    let status: PositiveTinyISIResolutionStatus
    let reviewReason: String?
    let scenarios: [PositiveTinyISIHypothesisScenario]
}

/// One member of the exact Cartesian product used when one or two independent positive
/// sub-floor ISIs are present. With two ambiguities the detector evaluates all four joint
/// H_earlier/H_later assignments against one virtual train. Treating the ambiguities one at a
/// time is not equivalent: a state or event can legitimately span both locations.
struct PositiveTinyISIJointHypothesisScenario: Sendable {
    let kindsByOriginalISIIndex: [Int: PositiveTinyISIHypothesisKind]
    let removedOriginalSpikeIndices: [Int]
    let mergedVirtualISIIndicesByOriginalISIIndex: [Int: Int]
    let virtualTrain: SpikeTrain

    var key: String {
        kindsByOriginalISIIndex.keys.sorted().map { index in
            "\(index):\(kindsByOriginalISIIndex[index]?.rawValue ?? "unknown")"
        }.joined(separator: "|")
    }

    func originalISISpan(forVirtualStart start: Int, end: Int) -> ClosedRange<Int>? {
        guard start >= 1, end >= start, end < virtualTrain.isiSec.count,
              let lower = originalFootprint(ofVirtualISI: start)?.lowerBound,
              let upper = originalFootprint(ofVirtualISI: end)?.upperBound else {
            return nil
        }
        return lower...upper
    }

    func containsAllAmbiguitySupport(startISIIndex: Int, endISIIndex: Int) -> Bool {
        mergedVirtualISIIndicesByOriginalISIIndex.values.allSatisfy {
            startISIIndex <= $0 && $0 <= endISIIndex
        }
    }

    private func originalFootprint(ofVirtualISI index: Int) -> ClosedRange<Int>? {
        let removed = Set(removedOriginalSpikeIndices)
        let retainedOriginalSpikes = (1...(virtualTrain.spikeCount + removed.count)).filter {
            !removed.contains($0)
        }
        guard index > 0, index < retainedOriginalSpikes.count else { return nil }
        let earlier = retainedOriginalSpikes[index - 1]
        let later = retainedOriginalSpikes[index]
        guard earlier < later else { return nil }
        return earlier...(later - 1)
    }
}

enum PositiveTinyISIHypothesisResolver {
    /// At most two isolated, non-overlapping positive ambiguities are eligible for automatic
    /// two-hypothesis evaluation. Edge, consecutive, overlapping, or larger ambiguity sets are
    /// retained for batch review. Exact zero ISIs are deliberately outside this contract: their
    /// virtual-collapse policy is represented by `DuplicateTimestampPolicy`.
    static func plans(
        train: SpikeTrain,
        artifactThresholdSec: Double
    ) -> [PositiveTinyISIHypothesisPlan] {
        guard artifactThresholdSec.isFinite, artifactThresholdSec > 0 else {
            return []
        }

        let positiveTinyIndices = train.isiSec.indices.compactMap { index -> Int? in
            guard index > 0,
                  let value = train.isiSec[index],
                  value.isFinite,
                  value > 0,
                  value < artifactThresholdSec else {
                return nil
            }
            return index
        }
        guard !positiveTinyIndices.isEmpty else { return [] }

        let tooMany = positiveTinyIndices.count > 2
        return positiveTinyIndices.map { index in
            let value = train.isiSec[index] ?? .nan
            let atEdge = index <= 1 || index >= train.spikeCount - 1
            let overlapsAnother = positiveTinyIndices.contains { other in
                other != index && abs(other - index) <= 2
            }
            let reason: String? = {
                if tooMany { return "more_than_two_positive_tiny_isi" }
                if atEdge { return "edge_positive_tiny_isi" }
                if overlapsAnother { return "overlapping_positive_tiny_isi_influence" }
                return nil
            }()
            guard reason == nil,
                  let earlier = scenario(
                    train: train,
                    originalISIIndex: index,
                    kind: .earlierSpikeErroneous
                  ),
                  let later = scenario(
                    train: train,
                    originalISIIndex: index,
                    kind: .laterSpikeErroneous
                  ) else {
                return PositiveTinyISIHypothesisPlan(
                    originalISIIndex: index,
                    valueSec: value,
                    status: .requiresReview,
                    reviewReason: reason ?? "hypothesis_construction_failed",
                    scenarios: []
                )
            }
            return PositiveTinyISIHypothesisPlan(
                originalISIIndex: index,
                valueSec: value,
                status: .eligibleForExactHypotheses,
                reviewReason: nil,
                scenarios: [earlier, later]
            )
        }
    }

    /// Removes HFS/Burst conflict rows whose pre-hypothesis geometry is no longer authoritative.
    ///
    /// Phase 1B creates these diagnostic rows before the exact positive-tiny-ISI hypotheses are
    /// reconciled. Either virtual spike-removal choice can alter original ISIs `i-1...i+1`, so a
    /// row intersecting that conservative footprint must not be exported as if it described the
    /// final candidate graph. Unaffected rows remain valid and deterministic. The hypothesis
    /// consensus/review candidates carry the authoritative audit for the affected region.
    static func retainingUnaffectedHFSBurstAuditRows(
        _ rows: [HFSBurstArbitrationAuditRow],
        train: SpikeTrain,
        artifactThresholdSec: Double
    ) -> [HFSBurstArbitrationAuditRow] {
        let plans = plans(train: train, artifactThresholdSec: artifactThresholdSec)
        guard !plans.isEmpty else { return rows }

        let lastISIIndex = max(1, train.spikeCount - 1)
        let footprints = plans.map { plan in
            max(1, plan.originalISIIndex - 1)...min(lastISIIndex, plan.originalISIIndex + 1)
        }
        return rows.filter { row in
            guard row.trainID == train.id else { return true }
            let hfsSpan = ClosedRange(uncheckedBounds: (
                lower: min(row.hfsStartISIIndex, row.hfsEndISIIndex),
                upper: max(row.hfsStartISIIndex, row.hfsEndISIIndex)
            ))
            let conflictSpan = ClosedRange(uncheckedBounds: (
                lower: min(row.conflictStartISIIndex, row.conflictEndISIIndex),
                upper: max(row.conflictStartISIIndex, row.conflictEndISIIndex)
            ))
            return !footprints.contains { footprint in
                overlaps(footprint, hfsSpan) || overlaps(footprint, conflictSpan)
            }
        }
    }

    /// Replaces affected pre-hypothesis HFS/Burst rows with rows built from the final exact-hypothesis
    /// consensus graph. Only final HFS consensus candidates are admitted to the replacement set;
    /// superseded pre-hypothesis HFS proposals therefore cannot reappear as apparently authoritative
    /// audit rows. Unaffected Phase 1B rows retain their original evidence and ordering.
    static func reconcilingHFSBurstAuditRows(
        _ rows: [HFSBurstArbitrationAuditRow],
        train: SpikeTrain,
        finalCandidates: [ClassicAnchorCandidate],
        artifactThresholdSec: Double,
        settings: HFSBurstArbitrationAuditSettings
    ) -> [HFSBurstArbitrationAuditRow] {
        let retained = retainingUnaffectedHFSBurstAuditRows(
            rows,
            train: train,
            artifactThresholdSec: artifactThresholdSec
        )
        let finalConsensusHFSIDs = Set(finalCandidates.compactMap { candidate -> String? in
            guard candidate.trainID == train.id,
                  candidate.finalLabel == .highFrequencySpiking,
                  candidate.gateStatus == "positive_tiny_isi_hypothesis_consensus" else {
                return nil
            }
            return candidate.id
        })
        guard !finalConsensusHFSIDs.isEmpty else { return retained }

        let selectedBurstEvents = finalCandidates.filter {
            $0.trainID == train.id &&
                $0.selectedForAuto && $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .event && $0.finalLabel.isBurstEventFamily
        }
        let replacements = HFSBurstArbitrationAudit.build(
            train: train,
            preProtectionCandidates: finalCandidates,
            selectedEventsUsedForPacketization: selectedBurstEvents,
            protectedCandidates: finalCandidates,
            finalCandidates: finalCandidates,
            pipelineStage: "dataset_seed_aware_positive_tiny_isi_reconciliation",
            settings: settings
        ).filter { finalConsensusHFSIDs.contains($0.hfsCandidateID) }

        var seen = Set<String>()
        return (retained + replacements).filter { seen.insert($0.id).inserted }
    }

    /// Builds the exact joint hypothesis set for the already-vetted automatic plans. The maximum
    /// size is four (2^2), so runtime is explicitly bounded and independent of train length.
    static func jointScenarios(
        train: SpikeTrain,
        plans: [PositiveTinyISIHypothesisPlan]
    ) -> [PositiveTinyISIJointHypothesisScenario] {
        let eligible = plans
            .filter { $0.status == .eligibleForExactHypotheses }
            .sorted { $0.originalISIIndex < $1.originalISIIndex }
        guard !eligible.isEmpty, eligible.count <= 2,
              eligible.allSatisfy({ $0.scenarios.count == 2 }) else {
            return []
        }

        let assignments = eligible.reduce(into: [[Int: PositiveTinyISIHypothesisKind]]([[:]])) {
            combinations, plan in
            combinations = combinations.flatMap { existing in
                PositiveTinyISIHypothesisKind.allCases.map { kind in
                    var next = existing
                    next[plan.originalISIIndex] = kind
                    return next
                }
            }
        }
        return assignments.compactMap { assignment in
            jointScenario(train: train, assignment: assignment)
        }
    }

    private static func scenario(
        train: SpikeTrain,
        originalISIIndex: Int,
        kind: PositiveTinyISIHypothesisKind
    ) -> PositiveTinyISIHypothesisScenario? {
        let removedSpikeIndex: Int
        switch kind {
        case .earlierSpikeErroneous:
            removedSpikeIndex = originalISIIndex
        case .laterSpikeErroneous:
            removedSpikeIndex = originalISIIndex + 1
        }
        guard removedSpikeIndex >= 1, removedSpikeIndex <= train.spikeCount else {
            return nil
        }

        var timestamps = train.timestampsSec
        timestamps.remove(at: removedSpikeIndex - 1)
        let virtualTrain = SpikeTrain(
            name: train.name,
            timestampsSec: timestamps,
            duplicateTimestampPolicy: train.duplicateTimestampPolicy
        ).withID(train.id)
        return PositiveTinyISIHypothesisScenario(
            kind: kind,
            originalISIIndex: originalISIIndex,
            removedOriginalSpikeIndex: removedSpikeIndex,
            mergedVirtualISIIndex: removedSpikeIndex - 1,
            virtualTrain: virtualTrain
        )
    }

    private static func overlaps(_ lhs: ClosedRange<Int>, _ rhs: ClosedRange<Int>) -> Bool {
        max(lhs.lowerBound, rhs.lowerBound) <= min(lhs.upperBound, rhs.upperBound)
    }

    private static func jointScenario(
        train: SpikeTrain,
        assignment: [Int: PositiveTinyISIHypothesisKind]
    ) -> PositiveTinyISIJointHypothesisScenario? {
        let removedSpikeIndices = assignment.compactMap { index, kind -> Int? in
            switch kind {
            case .earlierSpikeErroneous: return index
            case .laterSpikeErroneous: return index + 1
            }
        }.sorted()
        guard removedSpikeIndices.count == assignment.count,
              Set(removedSpikeIndices).count == removedSpikeIndices.count,
              removedSpikeIndices.allSatisfy({ 1 <= $0 && $0 <= train.spikeCount }) else {
            return nil
        }

        let removed = Set(removedSpikeIndices)
        let retainedOriginalSpikes = (1...train.spikeCount).filter { !removed.contains($0) }
        guard retainedOriginalSpikes.count >= 2 else { return nil }
        let timestamps = retainedOriginalSpikes.map { train.timestampsSec[$0 - 1] }
        let virtualTrain = SpikeTrain(
            name: train.name,
            timestampsSec: timestamps,
            duplicateTimestampPolicy: train.duplicateTimestampPolicy
        ).withID(train.id)

        var mergedByOriginalISI: [Int: Int] = [:]
        for ambiguityIndex in assignment.keys {
            guard let virtualIndex = (1..<retainedOriginalSpikes.count).first(where: { index in
                let earlier = retainedOriginalSpikes[index - 1]
                let later = retainedOriginalSpikes[index]
                return earlier <= ambiguityIndex && ambiguityIndex < later
            }) else {
                return nil
            }
            mergedByOriginalISI[ambiguityIndex] = virtualIndex
        }

        return PositiveTinyISIJointHypothesisScenario(
            kindsByOriginalISIIndex: assignment,
            removedOriginalSpikeIndices: removedSpikeIndices,
            mergedVirtualISIIndicesByOriginalISIIndex: mergedByOriginalISI,
            virtualTrain: virtualTrain
        )
    }
}
