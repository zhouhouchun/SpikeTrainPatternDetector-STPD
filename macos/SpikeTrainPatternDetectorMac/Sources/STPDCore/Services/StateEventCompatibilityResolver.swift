import Foundation

/// Splits state candidates at selected semantic-track hard boundaries.
///
/// Phase 1B rules:
/// - selected burst events split tonic and HF-tonic;
/// - selected pause gaps split HFS into independent fragments, so pause-separated
///   packets are not interpreted as one continuous HFS. Selected burst events do
///   NOT fragment HFS — a sustained high-frequency state may contain internal
///   burst-like packets that are retained as overlays; true burst dominance is
///   decided by `HFSpikingProtection` / arbitration, not by cutting here;
/// - review candidates never split state;
/// - every child is rebuilt through `StatePatternDetector` gates.
struct StateEventCompatibilityResolution: Sendable {
    let fragments: [ClassicAnchorCandidate]
    let consumedStateCandidateIdentities: Set<StatePatternDetector.CandidateIdentity>
    let boundaryCandidateIDsByConsumedStateCandidateIdentity: [
        StatePatternDetector.CandidateIdentity: [String]
    ]
}

public enum StateEventCompatibilityResolver {
    /// Backward-compatible convenience entry point. Adaptive production code
    /// should pass the exact state settings used for candidate generation.
    public static func splitStateCandidates(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        settings: StatePatternDetectorSettings = StatePatternDetectorSettings()
    ) -> [ClassicAnchorCandidate] {
        let selectedEvents = candidates.filter {
                $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .event &&
                $0.finalLabel.isBurstEventFamily
        }
        let selectedGaps = candidates.filter {
            $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .gap &&
                $0.finalLabel == .pause
        }
        return splitStateCandidates(
            train: train,
            candidates: candidates,
            selectedEvents: selectedEvents,
            selectedGaps: selectedGaps,
            settings: settings
        )
    }

    /// Preferred Phase 1B entry point. `selectedEvents` and `selectedGaps`
    /// should come from the same event-first/gap-second arbitration pass.
    public static func splitStateCandidates(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate],
        settings: StatePatternDetectorSettings = StatePatternDetectorSettings()
    ) -> [ClassicAnchorCandidate] {
        resolveStateCandidates(
            train: train,
            candidates: candidates,
            selectedEvents: selectedEvents,
            selectedGaps: selectedGaps,
            settings: settings
        ).fragments
    }

    /// Detailed Phase 1B resolution used by the multi-track pipeline.
    ///
    /// A state parent intersected by a selected hard boundary is consumed even
    /// when neither resulting side is large enough to pass the primary state
    /// gates. This prevents an unsplittable parent from leaking back into later
    /// arbitration and swallowing the selected boundary.
    static func resolveStateCandidates(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate],
        settings: StatePatternDetectorSettings = StatePatternDetectorSettings()
    ) -> StateEventCompatibilityResolution {
        let trainStates = candidates.filter {
            $0.trainID == train.id &&
                $0.arbitrationTrack == .state &&
                $0.isEligibleForAutoSelection
        }
        guard !trainStates.isEmpty else {
            return StateEventCompatibilityResolution(
                fragments: [],
                consumedStateCandidateIdentities: [],
                boundaryCandidateIDsByConsumedStateCandidateIdentity: [:]
            )
        }

        let burstEvents = selectedEvents.filter {
            $0.trainID == train.id &&
                $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.finalLabel.isBurstEventFamily
        }
        let pauseGaps = selectedGaps.filter {
            $0.trainID == train.id &&
                $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.finalLabel == .pause
        }
        guard !burstEvents.isEmpty || !pauseGaps.isEmpty else {
            return StateEventCompatibilityResolution(
                fragments: [],
                consumedStateCandidateIdentities: [],
                boundaryCandidateIDsByConsumedStateCandidateIdentity: [:]
            )
        }

        let localContext = SpikeISILocalContextTable.build(
            for: train,
            settings: SpikeISILocalContextSettings(
                minValidISISec: settings.minValidISISec,
                halfWindow: max(3, settings.tonicMinSpikes)
            )
        )
        let stateGroups = Dictionary(grouping: trainStates, by: rootCandidateID)
        var fragmentsByID: [String: ClassicAnchorCandidate] = [:]
        var consumedStateCandidateIdentities = Set<StatePatternDetector.CandidateIdentity>()
        var boundaryIDsByConsumedStateCandidateIdentity: [
            StatePatternDetector.CandidateIdentity: [String]
        ] = [:]

        for group in stateGroups.values {
            // A first pass normally has one unsplit parent. Later passes may
            // contain only disjoint split siblings because the original parent
            // has already been frozen. Resolve the maximal non-overlapping
            // frontier so every current sibling is checked without rebuilding
            // nested descendants twice.
            for parent in sourceFrontier(in: group) {
                guard let stateRange = candidateRange(parent) else {
                    continue
                }
                let clippedCuts = cuttingCandidates(
                    for: parent,
                    selectedEvents: burstEvents,
                    selectedGaps: pauseGaps
                ).compactMap { cut -> CutInterval? in
                    let lower = max(
                        stateRange.lowerBound,
                        min(cut.startISIIndex, cut.endISIIndex)
                    )
                    let upper = min(
                        stateRange.upperBound,
                        max(cut.startISIIndex, cut.endISIIndex)
                    )
                    guard lower <= upper else {
                        return nil
                    }
                    return CutInterval(range: lower...upper, candidateID: cut.id)
                }
                guard !clippedCuts.isEmpty else {
                    continue
                }

                // Consume every lineage member crossed by this parent's hard
                // boundaries. Existing siblings wholly on another side remain
                // eligible; a spanning parent or stale spanning child cannot be
                // re-promoted when no rebuilt fragment passes its state gates.
                for candidate in group {
                    guard let candidateRange = candidateRange(candidate) else {
                        continue
                    }
                    let boundaryIDs = Array(Set(clippedCuts.compactMap { cut -> String? in
                        guard candidateRange.overlaps(cut.range) else {
                            return nil
                        }
                        return cut.candidateID
                    })).sorted()
                    guard !boundaryIDs.isEmpty else {
                        continue
                    }
                    let identity = StatePatternDetector.CandidateIdentity(candidate)
                    consumedStateCandidateIdentities.insert(identity)
                    boundaryIDsByConsumedStateCandidateIdentity[identity] = boundaryIDs
                }

                let mergedCuts = mergedIntervals(clippedCuts.map(\.range))
                let cleanFragments = cleanIntervals(
                    stateRange: stateRange,
                    cuts: mergedCuts
                )
                let cutIDs = Array(Set(clippedCuts.map(\.candidateID))).sorted()

                for range in cleanFragments {
                    guard let fragment = StatePatternDetector.rebuildSplitCandidate(
                        train: train,
                        parent: parent,
                        range: range,
                        settings: settings,
                        localContext: localContext,
                        splitByCandidateIDs: cutIDs
                    ) else {
                        continue
                    }
                    fragmentsByID[fragment.id] = fragment
                }
            }
        }

        return StateEventCompatibilityResolution(
            fragments: fragmentsByID.values.sorted(by: candidateOrder),
            consumedStateCandidateIdentities: consumedStateCandidateIdentities,
            boundaryCandidateIDsByConsumedStateCandidateIdentity:
                boundaryIDsByConsumedStateCandidateIdentity
        )
    }

    private struct CutInterval {
        let range: ClosedRange<Int>
        let candidateID: String
    }

    private static func cuttingCandidates(
        for state: ClassicAnchorCandidate,
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        switch state.finalLabel {
        case .tonic, .highFrequencyTonic:
            return selectedEvents

        case .highFrequencySpiking:
            // Only selected pause/gap evidence splits HFS, because multiple
            // pause-separated packets should not become one continuous HFS.
            // Selected burst events no longer fragment HFS: a sustained
            // high-frequency state may contain internal burst-like packets, which
            // are retained as overlays. Whether those packets are mere internal
            // packetization or true burst dominance is decided downstream by
            // HFSpikingProtection / arbitration (embedded coverage and group
            // count), not by chopping the sustained state into sub-threshold
            // fragments here.
            return selectedGaps

        default:
            return []
        }
    }

    private static func sourceFrontier(
        in candidates: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        let ordered = candidates.sorted { lhs, rhs in
            let lhsIsFragment = isSplitFragment(lhs)
            let rhsIsFragment = isSplitFragment(rhs)
            if lhsIsFragment != rhsIsFragment {
                return !lhsIsFragment
            }
            if lhs.nISI != rhs.nISI {
                return lhs.nISI > rhs.nISI
            }
            if lhs.priority != rhs.priority {
                return lhs.priority > rhs.priority
            }
            return lhs.id < rhs.id
        }

        var frontier: [ClassicAnchorCandidate] = []
        for candidate in ordered {
            guard let range = candidateRange(candidate) else {
                continue
            }
            // Generated siblings are disjoint. If malformed or stale lineage
            // members overlap, the higher-ranked maximal member owns that area,
            // preventing duplicate fragments with the same root/range identity.
            guard !frontier.contains(where: { existing in
                candidateRange(existing)?.overlaps(range) == true
            }) else {
                continue
            }
            frontier.append(candidate)
        }
        return frontier.sorted(by: candidateOrder)
    }

    private static func candidateRange(
        _ candidate: ClassicAnchorCandidate
    ) -> ClosedRange<Int>? {
        let lower = min(candidate.startISIIndex, candidate.endISIIndex)
        let upper = max(candidate.startISIIndex, candidate.endISIIndex)
        guard lower > 0, lower <= upper else {
            return nil
        }
        return lower...upper
    }

    private static func rootCandidateID(
        _ candidate: ClassicAnchorCandidate
    ) -> String {
        candidate.id.components(separatedBy: "::state-split::").first ?? candidate.id
    }

    private static func isSplitFragment(
        _ candidate: ClassicAnchorCandidate
    ) -> Bool {
        candidate.id.contains("::state-split::") ||
            candidate.candidateLayer.contains("state_track_split")
    }

    private static func mergedIntervals(
        _ intervals: [ClosedRange<Int>]
    ) -> [ClosedRange<Int>] {
        let sorted = intervals.sorted { lhs, rhs in
            if lhs.lowerBound != rhs.lowerBound {
                return lhs.lowerBound < rhs.lowerBound
            }
            return lhs.upperBound < rhs.upperBound
        }
        guard var current = sorted.first else {
            return []
        }

        var merged: [ClosedRange<Int>] = []
        for interval in sorted.dropFirst() {
            if interval.lowerBound <= current.upperBound + 1 {
                current = current.lowerBound...max(current.upperBound, interval.upperBound)
            } else {
                merged.append(current)
                current = interval
            }
        }
        merged.append(current)
        return merged
    }

    private static func cleanIntervals(
        stateRange: ClosedRange<Int>,
        cuts: [ClosedRange<Int>]
    ) -> [ClosedRange<Int>] {
        var clean: [ClosedRange<Int>] = []
        var cursor = stateRange.lowerBound

        for cut in cuts {
            let cutStart = max(stateRange.lowerBound, cut.lowerBound)
            let cutEnd = min(stateRange.upperBound, cut.upperBound)
            if cursor < cutStart {
                clean.append(cursor...(cutStart - 1))
            }
            cursor = max(cursor, cutEnd + 1)
        }

        if cursor <= stateRange.upperBound {
            clean.append(cursor...stateRange.upperBound)
        }
        return clean
    }

    private static func candidateOrder(
        _ lhs: ClassicAnchorCandidate,
        _ rhs: ClassicAnchorCandidate
    ) -> Bool {
        if lhs.trainID != rhs.trainID {
            return lhs.trainID < rhs.trainID
        }
        if lhs.startISIIndex != rhs.startISIIndex {
            return lhs.startISIIndex < rhs.startISIIndex
        }
        if lhs.endISIIndex != rhs.endISIIndex {
            return lhs.endISIIndex < rhs.endISIIndex
        }
        return lhs.id < rhs.id
    }
}
