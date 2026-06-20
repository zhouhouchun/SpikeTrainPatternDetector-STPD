import Foundation

/// Splits state candidates at selected semantic-track hard boundaries.
///
/// Phase 1B rules:
/// - selected burst events split tonic and HF-tonic;
/// - selected burst events and pause gaps split HFS into independent fragments,
///   so HFS never shares a final ISI label with burst and pause-separated
///   packets are not interpreted as one continuous HFS;
/// - review candidates never split state;
/// - every child is rebuilt through `StatePatternDetector` gates.
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
        let trainStates = candidates.filter {
            $0.trainID == train.id &&
                $0.arbitrationTrack == .state &&
                $0.isEligibleForAutoSelection
        }
        guard !trainStates.isEmpty else {
            return []
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
            return []
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

        for group in stateGroups.values {
            guard let parent = preferredParent(in: group) else {
                continue
            }
            let stateStart = min(parent.startISIIndex, parent.endISIIndex)
            let stateEnd = max(parent.startISIIndex, parent.endISIIndex)
            guard stateStart > 0, stateStart <= stateEnd else {
                continue
            }

            let cuts = cuttingCandidates(
                for: parent,
                selectedEvents: burstEvents,
                selectedGaps: pauseGaps
            )
            let clippedCuts = cuts.compactMap { cut -> CutInterval? in
                let lower = max(stateStart, min(cut.startISIIndex, cut.endISIIndex))
                let upper = min(stateEnd, max(cut.startISIIndex, cut.endISIIndex))
                guard lower <= upper else {
                    return nil
                }
                return CutInterval(
                    range: lower...upper,
                    candidateID: cut.id
                )
            }
            guard !clippedCuts.isEmpty else {
                continue
            }

            let mergedCuts = mergedIntervals(clippedCuts.map(\.range))
            let cleanFragments = cleanIntervals(
                stateRange: stateStart...stateEnd,
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

        return fragmentsByID.values.sorted(by: candidateOrder)
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
            // Burst has higher final-label priority than HFS. Cutting HFS at
            // selected burst events preserves non-overlapping HFS fragments
            // instead of rejecting a long state because of one compact packet.
            // Selected pause evidence also breaks HFS, because multiple
            // pause-separated packets should not become one continuous HFS.
            return selectedEvents + selectedGaps

        default:
            return []
        }
    }

    private static func preferredParent(
        in candidates: [ClassicAnchorCandidate]
    ) -> ClassicAnchorCandidate? {
        candidates.sorted { lhs, rhs in
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
        }.first
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
