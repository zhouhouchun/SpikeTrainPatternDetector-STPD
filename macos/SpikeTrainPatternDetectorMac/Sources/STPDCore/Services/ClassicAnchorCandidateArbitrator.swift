import Foundation

public enum ClassicAnchorCandidateArbitrator {
    /// Backward-compatible arbitration for event, gap, and review tracks.
    /// State candidates are intentionally handled by `arbitrateBySemanticTrack`.
    public static func arbitrate(_ candidates: [ClassicAnchorCandidate]) -> [ClassicAnchorCandidate] {
        guard !candidates.isEmpty else {
            return []
        }

        var selectedStatusByID: [String: String] = [:]
        var unselectedStatusByID: [String: String] = [:]
        let grouped = Dictionary(grouping: candidates, by: \.trainID)

        for trainCandidates in grouped.values {
            let eventCandidates = trainCandidates.filter { $0.arbitrationTrack == .event }
            let eventSelectionIDs = weightedSelectionIDs(for: eventCandidates)
            let selectedEvents = eventCandidates.filter { eventSelectionIDs.contains($0.id) }
            for id in eventSelectionIDs {
                selectedStatusByID[id] = "selected_by_event_core_weighted_interval_grammar"
            }

            let gapCandidates = trainCandidates.filter { $0.arbitrationTrack == .gap }
            let gapResolution = GapTrackResolver.resolve(
                gapCandidates,
                selectedEvents: selectedEvents
            )
            selectedStatusByID.merge(gapResolution.selectedStatusByID) { current, _ in current }
            unselectedStatusByID.merge(gapResolution.unselectedStatusByID) { current, _ in current }

            let reviewCandidates = trainCandidates.filter { $0.arbitrationTrack == .review }
            for id in weightedSelectionIDs(for: reviewCandidates) {
                selectedStatusByID[id] = "selected_by_review_track_weighted_interval_grammar"
            }
        }

        return candidates.map { candidate in
            if let selectionStatus = selectedStatusByID[candidate.id] {
                return candidate.withAutoSelection(
                    selectedForAuto: true,
                    selectionStatus: selectionStatus
                )
            }
            return candidate.withAutoSelection(
                selectedForAuto: false,
                selectionStatus: unselectedStatusByID[candidate.id] ?? "not_selected"
            )
        }
    }

    /// Resolve candidates independently on event, gap, state, and review tracks.
    ///
    /// Selection order is deliberate:
    /// 1. burst-family events establish hard event cores;
    /// 2. tonic / HF-tonic states outrank HFS on overlapping ISIs;
    /// 3. HFS states outrank pauses;
    /// 4. pauses remain the lowest-priority final label;
    /// 5. review candidates remain non-destructive context.
    public static func arbitrateBySemanticTrack(_ candidates: [ClassicAnchorCandidate]) -> [ClassicAnchorCandidate] {
        guard !candidates.isEmpty else {
            return []
        }

        var selectedStatusByID: [String: String] = [:]
        var unselectedStatusByID: [String: String] = [:]
        var hfBurstPacketIDs: Set<String> = []
        let grouped = Dictionary(grouping: candidates, by: \.trainID)

        for trainCandidates in grouped.values {
            let eventCandidates = trainCandidates.filter { $0.arbitrationTrack == .event }
            let gapCandidates = trainCandidates.filter { $0.arbitrationTrack == .gap }
            let stateCandidates = trainCandidates.filter { $0.arbitrationTrack == .state }
            let reviewCandidates = trainCandidates.filter { $0.arbitrationTrack == .review }

            // Strong, long, non-burst-dominated high-frequency states. Embedded classic/tonic
            // windows fully inside such an envelope are part of the HF state, not competitors,
            // so they are demoted before weighted-interval selection (otherwise many small
            // tonic fragments out-value one long HF state). Local burst-family events inside
            // the envelope stay burst events but are tagged as hf_burst_packet (audit only).
            let strongHFStates = stateCandidates.filter { isStrongHighFrequencyState($0) }

            let eventSelectionIDs = weightedSelectionIDs(for: eventCandidates)
            let selectedEvents = eventCandidates.filter { eventSelectionIDs.contains($0.id) }
            for id in eventSelectionIDs {
                selectedStatusByID[id] = "selected_by_event_track_weighted_interval_grammar"
            }

            let gapResolution = GapTrackResolver.resolve(
                gapCandidates,
                selectedEvents: selectedEvents
            )
            selectedStatusByID.merge(gapResolution.selectedStatusByID) { current, _ in current }
            unselectedStatusByID.merge(gapResolution.unselectedStatusByID) { current, _ in current }
            let selectedGaps = gapCandidates.filter {
                gapResolution.selectedIDs.contains($0.id)
            }

            let reviewSelectionIDs = weightedSelectionIDs(for: reviewCandidates)
            let selectedReviews = reviewCandidates.filter {
                reviewSelectionIDs.contains($0.id)
            }
            for id in reviewSelectionIDs {
                selectedStatusByID[id] = "selected_by_review_track_weighted_interval_grammar"
            }

            if !strongHFStates.isEmpty {
                for event in eventCandidates where event.finalLabel.isBurstEventFamily {
                    if strongHFStates.contains(where: { fullyContains($0, event) }) {
                        hfBurstPacketIDs.insert(event.id)
                    }
                }
            }

            var compatibleStateCandidates: [ClassicAnchorCandidate] = []
            var compatibleStateStatusByID: [String: String] = [:]
            for state in stateCandidates {
                guard state.isEligibleForAutoSelection else {
                    unselectedStatusByID[state.id] = "not_selected__state_candidate_ineligible"
                    continue
                }
                // An embedded tonic window fully contained inside a strong HF state belongs to
                // the HF envelope; demote it so it cannot out-vote the long HF state.
                if state.finalLabel == .tonic,
                   strongHFStates.contains(where: { $0.id != state.id && fullyContains($0, state) }) {
                    unselectedStatusByID[state.id] = "not_selected__contained_in_strong_hf_state"
                    continue
                }
                let status = stateSelectionStatus(
                    state,
                    selectedEvents: selectedEvents,
                    selectedGaps: selectedGaps,
                    selectedReviews: selectedReviews
                )
                if status.selected {
                    compatibleStateCandidates.append(state)
                    compatibleStateStatusByID[state.id] = status.selectionStatus
                } else {
                    unselectedStatusByID[state.id] = status.selectionStatus
                }
            }

            let stateSelectionIDs = weightedSelectionIDs(for: compatibleStateCandidates)
            for state in compatibleStateCandidates {
                if stateSelectionIDs.contains(state.id) {
                    selectedStatusByID[state.id] =
                        compatibleStateStatusByID[state.id] ??
                        "selected_by_state_track_weighted_interval_grammar"
                } else {
                    unselectedStatusByID[state.id] =
                        "not_selected__state_track_weighted_interval_grammar"
                }
            }

            let selectedHFSStates = compatibleStateCandidates.filter {
                stateSelectionIDs.contains($0.id) &&
                    $0.finalLabel == .highFrequencySpiking
            }
            if !selectedHFSStates.isEmpty {
                for hfs in selectedHFSStates {
                    // Only a burst-dominated HFS is unselected here. A non-dominated HFS that
                    // merely overlaps internal selected burst packets is retained as an
                    // overlay (its packetization-overlay status was set in stateSelectionStatus).
                    guard hfs.hfSpikingBurstDominated == true else {
                        continue
                    }
                    guard selectedEvents.contains(where: {
                        $0.finalLabel.isBurstEventFamily && overlapCount(hfs, $0) > 0
                    }) else {
                        continue
                    }
                    selectedStatusByID.removeValue(forKey: hfs.id)
                    unselectedStatusByID[hfs.id] = "not_selected__hfs_burst_packet_dominance"
                }
            }

            let selectedStateCandidates = compatibleStateCandidates.filter { state in
                stateSelectionIDs.contains(state.id) &&
                    selectedStatusByID[state.id] != nil
            }
            if !selectedStateCandidates.isEmpty {
                for gap in selectedGaps {
                    guard selectedStateCandidates.contains(where: { overlapCount(gap, $0) > 0 }) else {
                        continue
                    }
                    selectedStatusByID.removeValue(forKey: gap.id)
                    unselectedStatusByID[gap.id] = "not_selected__gap_track_overlapped_higher_priority_state_track"
                }
            }
        }

        return candidates.map { candidate -> ClassicAnchorCandidate in
            let resolved: ClassicAnchorCandidate
            if let selectionStatus = selectedStatusByID[candidate.id] {
                resolved = candidate.withAutoSelection(
                    selectedForAuto: true,
                    selectionStatus: selectionStatus
                )
            } else {
                resolved = candidate.withAutoSelection(
                    selectedForAuto: false,
                    selectionStatus: unselectedStatusByID[candidate.id] ?? "not_selected"
                )
            }
            // Audit-only: a burst event inside a strong HF envelope is an HF burst packet. It
            // stays a burst event (finalLabel unchanged); only the additive subtype is set.
            guard hfBurstPacketIDs.contains(candidate.id), resolved.stateHighFrequencySubtype == nil else {
                return resolved
            }
            var tagged = resolved
            tagged.stateHighFrequencySubtype = "hf_burst_packet"
            return tagged
        }
    }

    /// A high-frequency state strong and long enough to dominate embedded tonic windows.
    /// These are arbitration-selection thresholds (not detection thresholds): an eligible,
    /// non-burst-dominated, state-level HFS run that is genuinely high-frequency (absolute
    /// q90 cap, so a slow ~28 Hz irregular-tonic run is never treated as a dominating HFS)
    /// with strong short-ISI and bridge evidence.
    private static let strongHighFrequencyStateMinISI = 20
    private static let strongHighFrequencyStateQ90MaxSec = 0.030
    private static let strongHighFrequencyStateShortFractionMin = 0.7
    private static let strongHighFrequencyStateBridgeFractionMin = 0.6

    private static func isStrongHighFrequencyState(_ candidate: ClassicAnchorCandidate) -> Bool {
        candidate.finalLabel == .highFrequencySpiking &&
            candidate.isEligibleForAutoSelection &&
            candidate.hfSpikingBurstDominated != true &&
            candidate.nISI >= strongHighFrequencyStateMinISI &&
            (candidate.intraQ90Sec ?? .infinity) <= strongHighFrequencyStateQ90MaxSec &&
            (candidate.hfSpikingShortFraction ?? 0) >= strongHighFrequencyStateShortFractionMin &&
            (candidate.hfSpikingBridgeFraction ?? 0) >= strongHighFrequencyStateBridgeFractionMin
    }

    private static func fullyContains(
        _ container: ClassicAnchorCandidate,
        _ inner: ClassicAnchorCandidate
    ) -> Bool {
        let containerStart = min(container.startISIIndex, container.endISIIndex)
        let containerEnd = max(container.startISIIndex, container.endISIIndex)
        let innerStart = min(inner.startISIIndex, inner.endISIIndex)
        let innerEnd = max(inner.startISIIndex, inner.endISIIndex)
        return containerStart <= innerStart && containerEnd >= innerEnd
    }

    private struct WeightedCandidate {
        let candidate: ClassicAnchorCandidate
        let start: Int
        let end: Int
        let value: Double
    }

    private static func weightedSelectionIDs(for candidates: [ClassicAnchorCandidate]) -> Set<String> {
        let pool = candidates.compactMap { candidate -> WeightedCandidate? in
            guard candidate.isEligibleForAutoSelection else {
                return nil
            }
            let start = min(candidate.startISIIndex, candidate.endISIIndex)
            let end = max(candidate.startISIIndex, candidate.endISIIndex)
            guard start <= end else {
                return nil
            }
            return WeightedCandidate(
                candidate: candidate,
                start: start,
                end: end,
                value: candidateValue(candidate)
            )
        }
        .sorted { lhs, rhs in
            if lhs.end != rhs.end {
                return lhs.end < rhs.end
            }
            if lhs.start != rhs.start {
                return lhs.start < rhs.start
            }
            if lhs.value != rhs.value {
                return lhs.value > rhs.value
            }
            return lhs.candidate.id < rhs.candidate.id
        }

        guard !pool.isEmpty else {
            return []
        }

        let sortedEnds = pool.map(\.end)
        var previousCompatible = Array(repeating: -1, count: pool.count)
        for index in pool.indices {
            previousCompatible[index] = lastCompatibleIndex(
                endingBefore: pool[index].start,
                sortedEnds: sortedEnds,
                upperBound: index
            )
        }

        var dp = Array(repeating: 0.0, count: pool.count + 1)
        var take = Array(repeating: false, count: pool.count)

        for index in pool.indices {
            let include = pool[index].value + dp[previousCompatible[index] + 1]
            let exclude = dp[index]
            if include > exclude {
                dp[index + 1] = include
                take[index] = true
            } else {
                dp[index + 1] = exclude
            }
        }

        var chosen = Set<String>()
        var index = pool.count - 1
        while index >= 0 {
            let include = pool[index].value + dp[previousCompatible[index] + 1]
            if take[index], include >= dp[index] {
                chosen.insert(pool[index].candidate.id)
                index = previousCompatible[index]
            } else {
                index -= 1
            }
        }
        return chosen
    }

    private static func lastCompatibleIndex(
        endingBefore start: Int,
        sortedEnds: [Int],
        upperBound: Int
    ) -> Int {
        var low = 0
        var high = upperBound
        var best = -1
        while low < high {
            let mid = (low + high) / 2
            if sortedEnds[mid] < start {
                best = mid
                low = mid + 1
            } else {
                high = mid
            }
        }
        return best
    }

    private static func candidateValue(_ candidate: ClassicAnchorCandidate) -> Double {
        let score = finite(candidate.score, default: 0)
        let nISI = Double(max(0, candidate.nISI))
        let explicitPriority = Double(candidate.priority)

        if candidate.candidateLayer.hasPrefix("isi_profile_hard_threshold") {
            let priority: Double
            switch candidate.finalLabel {
            case .burst:
                priority = 1_800
            case .highFrequencyBurst:
                priority = 1_700
            case .longBurst:
                priority = 980
            case .pause:
                priority = 900
            case .tonic, .highFrequencyTonic:
                priority = 1_100
            case .highFrequencySpiking:
                priority = 1_000
            case .possibleBurst:
                priority = 800
            case .reject, .profile:
                priority = 0
            }

            let spanBonus: Double
            switch candidate.finalLabel {
            case .burst, .highFrequencyBurst:
                spanBonus = boundedSpanBonus(nISI, scale: 45_000, cap: 80)
            case .longBurst:
                spanBonus = boundedSpanBonus(nISI, scale: 32_000, cap: 160)
            case .highFrequencySpiking:
                spanBonus = boundedSpanBonus(nISI, scale: 25_000, cap: 300)
            case .reject, .profile:
                spanBonus = 0
            default:
                spanBonus = boundedSpanBonus(nISI, scale: 12_000, cap: 120)
            }
            return priority * 10_000 + 1_200 * score + spanBonus
        }

        if candidate.finalLabel == .highFrequencySpiking {
            return 1_000 * 10_000 +
                1_200 * score +
                boundedSpanBonus(nISI, scale: 25_000, cap: 300)
        }

        if candidate.finalLabel.isCanonicalBurstFamily,
           candidate.candidateLayer == "event_grammar_burst_episode" {
            let priority: Double = switch candidate.finalLabel {
            case .burst:
                2_720
            case .highFrequencyBurst:
                2_680
            case .longBurst:
                2_640
            default:
                2_640
            }
            return priority * 10_000 +
                1_200 * score +
                boundedSpanBonus(nISI, scale: 45_000, cap: 80)
        }

        if candidate.finalLabel == .burst,
           candidate.anchorLockLevel == .lockedClassic,
           candidate.candidateLayer == "structure_first_classic_burst_anchor",
           candidate.candidateClass == "structure_first_two_sided_classic_burst_i" {
            return 2_800 * 10_000 +
                1_200 * score +
                boundedSpanBonus(nISI, scale: 48_000, cap: 90)
        }

        if candidate.finalLabel == .possibleBurst {
            if candidate.arbitrationTrack == .event {
                let isStructurallyAssembledBurst =
                    candidate.candidateLayer == "train_burst_seed_run_assembly" ||
                    candidate.candidateLayer == "event_grammar_burst_episode" ||
                    candidate.decisionPath.contains("possible_burst_structural_definition=seed_run_burst_ii") ||
                    candidate.decisionPath.contains("dense_short_isi_episode_rescued_by_train_scale_compression")
                let defaultPriority: Double = isStructurallyAssembledBurst ? 2_650 : 1_020
                let priorityCap: Double = isStructurallyAssembledBurst ? 2_700 : 1_080
                let spanScale: Double = isStructurallyAssembledBurst ? 45_000 : 35_000
                let requestedPriority = explicitPriority > 0
                    ? max(explicitPriority, defaultPriority)
                    : defaultPriority
                let priority = min(requestedPriority, priorityCap)
                return priority * 10_000 +
                    1_000 * score +
                    boundedSpanBonus(nISI, scale: spanScale, cap: 80)
            } else {
                let priority = explicitPriority > 0 ? min(explicitPriority, 760) : 320
                return priority * 10_000 + 100 * score + nISI
            }
        }

        if explicitPriority > 0 {
            return explicitPriority * 10_000 + 100 * score + nISI
        }

        let fallbackPriority: Double
        switch candidate.finalLabel {
        case .burst:
            fallbackPriority = 1_250
        case .highFrequencyBurst:
            fallbackPriority = 1_220
        case .longBurst:
            fallbackPriority = 1_160
        case .highFrequencyTonic:
            fallbackPriority = 1_120
        case .tonic:
            fallbackPriority = 1_100
        case .highFrequencySpiking:
            fallbackPriority = 1_000
        case .pause:
            fallbackPriority = 320
        case .possibleBurst:
            fallbackPriority = 280
        case .reject, .profile:
            fallbackPriority = 0
        }
        return fallbackPriority * 10_000 + 100 * score + nISI
    }

    private static func boundedSpanBonus(_ nISI: Double, scale: Double, cap: Double) -> Double {
        let capped = min(max(nISI, 0), cap)
        return scale * log1p(capped)
    }

    private static func finite(_ value: Double, default fallback: Double) -> Double {
        value.isFinite ? value : fallback
    }

    private static func stateSelectionStatus(
        _ state: ClassicAnchorCandidate,
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate],
        selectedReviews: [ClassicAnchorCandidate]
    ) -> (selected: Bool, selectionStatus: String) {
        _ = selectedGaps

        let burstEventOverlaps = selectedEvents.filter {
            $0.finalLabel.isBurstEventFamily && overlapCount(state, $0) > 0
        }
        let reviewOverlap = selectedReviews.contains {
            $0.finalLabel == .possibleBurst && overlapCount(state, $0) > 0
        }

        switch state.finalLabel {
        case .tonic, .highFrequencyTonic:
            if !burstEventOverlaps.isEmpty {
                return (false, "not_selected__state_track_split_by_event_track")
            }

        case .highFrequencySpiking:
            if state.hfSpikingBurstDominated == true {
                return (false, "not_selected__hfs_burst_packet_dominance")
            }
            if !burstEventOverlaps.isEmpty {
                // Not burst-dominated, but selected burst packets overlap internally:
                // retain the sustained HFS state as a packetization overlay instead of
                // letting any overlap erase it. Dominance (above) is the only burst-driven
                // rejection; pause/gap boundaries still split HFS upstream.
                return (
                    true,
                    "selected_by_state_track_weighted_interval_grammar__hfs_retained_with_internal_burst_packet_overlay"
                )
            }
            if state.hfSpikingBurstPacketLike == true {
                return (
                    true,
                    "selected_by_state_track_weighted_interval_grammar__provisional_packetization_review"
                )
            }

        default:
            break
        }

        if reviewOverlap {
            return (
                true,
                "selected_by_state_track_weighted_interval_grammar__possible_burst_review_overlay"
            )
        }

        return (true, "selected_by_state_track_weighted_interval_grammar")
    }

    private static func overlapCount(
        _ lhs: ClassicAnchorCandidate,
        _ rhs: ClassicAnchorCandidate
    ) -> Int {
        let lhsStart = min(lhs.startISIIndex, lhs.endISIIndex)
        let lhsEnd = max(lhs.startISIIndex, lhs.endISIIndex)
        let rhsStart = min(rhs.startISIIndex, rhs.endISIIndex)
        let rhsEnd = max(rhs.startISIIndex, rhs.endISIIndex)
        let start = max(lhsStart, rhsStart)
        let end = min(lhsEnd, rhsEnd)
        guard start <= end else {
            return 0
        }
        return end - start + 1
    }
}

public extension ClassicAnchorCandidate {
    var arbitrationTrack: ClassicAnchorSemanticTrack? {
        guard !isStructuralPausePriorEvidence else {
            return nil
        }

        return switch finalLabel {
        case .burst, .highFrequencyBurst, .longBurst:
            .event
        case .pause:
            .gap
        case .possibleBurst:
            isBurstBoundaryReviewCandidate ? .review : .event
        case .tonic, .highFrequencyTonic, .highFrequencySpiking:
            .state
        case .reject, .profile:
            nil
        }
    }

    var isEligibleForAutoSelection: Bool {
        let normalizedAction = action.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let normalizedGate = gateStatus.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        guard finalLabel != .profile,
              normalizedAction != "reject",
              normalizedAction != "audit_only",
              normalizedAction != "profile",
              normalizedAction != "suppressed",
              !normalizedAction.hasPrefix("suppress"),
              !normalizedGate.contains("reject"),
              !normalizedGate.contains("suppressed") else {
            return false
        }
        return true
    }

    var failureReason: String {
        guard isRejectDiagnostic else {
            return ""
        }
        if let pathReason = latestDiagnosticReasonTag(in: decisionPath) {
            return pathReason
        }
        if let actionReason = diagnosticReasonTag(action) {
            return actionReason
        }
        return decisionPath
    }

    var auditReasonSummary: String {
        if !failureReason.isEmpty {
            return failureReason
        }
        if suppressedByHFSpikingState == true {
            return "suppressed_by_hf_spiking_state"
        }
        if finalLabel == .profile || candidateLayer.contains("profile") {
            return profileAuditSummary
        }
        if finalLabel == .possibleBurst {
            if !reviewEvidenceSummary.isEmpty {
                return reviewEvidenceSummary
            }
            return auditUncertaintyReason
        }
        if !selectedForAuto {
            return selectionStatus
        }
        if action != "accept" {
            return action
        }
        if !auditUncertaintyReason.isEmpty {
            return auditUncertaintyReason
        }
        return gateStatus
    }

    var candidateDiagnosticClass: String {
        if isRejectDiagnostic {
            let reason = failureReason.isEmpty ? "event_grammar_reject" : failureReason
            return "rejected__\(reason)"
        }
        return "\(finalLabel.rawValue)__\(gateStatus)"
    }

    private var isRejectDiagnostic: Bool {
        let normalizedAction = action.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let normalizedGate = gateStatus.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        return finalLabel == .reject ||
            normalizedAction == "reject" ||
            normalizedAction == "suppressed" ||
            normalizedGate.contains("reject") ||
            normalizedGate.contains("suppressed")
    }

    private func diagnosticReasonTag(_ text: String) -> String? {
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        let normalized = trimmed.lowercased()
        guard isSpecificDiagnosticReason(normalized) else {
            return nil
        }
        return trimmed
    }

    private func latestDiagnosticReasonTag(in path: String) -> String? {
        let parts = path
            .split(separator: ";")
            .map { String($0).trimmingCharacters(in: .whitespacesAndNewlines) }
            .filter { !$0.isEmpty }
        let specific = parts.filter { part in
            isSpecificDiagnosticReason(part.lowercased())
        }
        if !specific.isEmpty {
            return specific.joined(separator: ";")
        }
        for part in parts.reversed() {
            let normalized = part.lowercased()
            if normalized.hasPrefix("suppressor=") {
                continue
            }
            if normalized.contains("reject") ||
                normalized.contains("suppress") {
                return part
            }
        }
        return nil
    }

    private func isSpecificDiagnosticReason(_ normalized: String) -> Bool {
        guard !normalized.isEmpty,
              normalized != "event_grammar_reject",
              normalized != "hf_protected_reject",
              normalized != "hf_protected_suppressed",
              normalized != "reject",
              normalized != "suppressed" else {
            return false
        }
        if normalized.hasPrefix("reject_") ||
            normalized.hasPrefix("suppressed_") ||
            normalized.hasPrefix("suppress_") ||
            normalized.hasPrefix("compact_burst_kernel_suppressed") {
            return true
        }
        let exactBurstGateReasons: Set<String> = [
            "too_many_bridge_isis",
            "bridge_fraction_too_high",
            "intra_q90_exceeds_bridge_band",
            "intra_q95_exceeds_bridge_band_strict",
            "intra_q95_severely_exceeds_bridge_band",
            "flank_contrast_fail",
            "no_strict_or_possible_boundary_pass"
        ]
        return exactBurstGateReasons.contains(normalized)
    }

    private var profileAuditSummary: String {
        if candidateLayer == "structural_dataset_seed_profile" {
            return "dataset_structural_seed_profile"
        }
        if candidateLayer == "structural_seed_train_band_profile" {
            return "train_structural_seed_profile"
        }
        if candidateLayer == "event_core_train_isi_band_profile" {
            return "train_event_core_isi_profile"
        }
        if candidateLayer == "dataset_isi_train_seed_band_profile" {
            return "dataset_isi_seed_band_profile"
        }
        return candidateLayer.isEmpty ? "profile" : candidateLayer
    }

    var pipelineStageSummary: String {
        let stages = decisionPath
            .split(separator: ";")
            .compactMap { part -> String? in
                let text = String(part).trimmingCharacters(in: .whitespacesAndNewlines)
                guard text.hasPrefix("pipeline_stage=") else {
                    return nil
                }
                return String(text.dropFirst("pipeline_stage=".count))
            }
        var seen = Set<String>()
        let uniqueStages = stages.filter { stage in
            if seen.contains(stage) {
                return false
            }
            seen.insert(stage)
            return true
        }
        return uniqueStages.joined(separator: " -> ")
    }

    func withAutoSelection(
        selectedForAuto: Bool,
        selectionStatus: String
    ) -> ClassicAnchorCandidate {
        ClassicAnchorCandidate(
            id: id,
            trainID: trainID,
            trainName: trainName,
            candidateLayer: candidateLayer,
            candidateClass: candidateClass,
            finalLabel: finalLabel,
            gateStatus: gateStatus,
            decisionPath: decisionPath,
            action: action,
            score: score,
            priority: priority,
            selectedForAuto: selectedForAuto,
            selectionStatus: selectionStatus,
            startISIIndex: startISIIndex,
            endISIIndex: endISIIndex,
            startSpikeIndex: startSpikeIndex,
            endSpikeIndex: endSpikeIndex,
            nISI: nISI,
            nValidISI: nValidISI,
            nSpikes: nSpikes,
            durationSec: durationSec,
            intraQ10Sec: intraQ10Sec,
            intraQ40Sec: intraQ40Sec,
            intraQ50Sec: intraQ50Sec,
            intraQ90Sec: intraQ90Sec,
            intraQ95Sec: intraQ95Sec,
            maxIntraISISec: maxIntraISISec,
            meanIntraISISec: meanIntraISISec,
            cv: cv,
            cv2: cv2,
            lv: lv,
            preGapSec: preGapSec,
            postGapSec: postGapSec,
            preRatioQ90: preRatioQ90,
            postRatioQ90: postRatioQ90,
            edgeContrastMinQ90: edgeContrastMinQ90,
            edgeContrastGeomQ90: edgeContrastGeomQ90,
            anchorFamily: anchorFamily,
            anchorLockLevel: anchorLockLevel,
            anchorBandLowerSec: anchorBandLowerSec,
            anchorBandUpperSec: anchorBandUpperSec,
            anchorBandSource: anchorBandSource,
            anchorContrastMinRequired: anchorContrastMinRequired,
            anchorContrastGeomRequired: anchorContrastGeomRequired,
            refractorySuspectCount: refractorySuspectCount,
            refractorySuspectAction: refractorySuspectAction,
            profileSeedLowPercentileInTrain: profileSeedLowPercentileInTrain,
            profileSeedHighPercentileInTrain: profileSeedHighPercentileInTrain,
            profileSeedBandFraction: profileSeedBandFraction,
            profileSeedRunCount: profileSeedRunCount,
            profileMaxSeedRunLength: profileMaxSeedRunLength,
            profileMedianISISec: profileMedianISISec,
            profileQ10ISISec: profileQ10ISISec,
            profileQ25ISISec: profileQ25ISISec,
            profileQ90ISISec: profileQ90ISISec,
            profilePauseFraction: profilePauseFraction,
            profilePhenotypePrior: profilePhenotypePrior,
            profileBridgeUpperSec: profileBridgeUpperSec,
            profileBoundaryFloorSec: profileBoundaryFloorSec,
            profileBoundaryFloorHard: profileBoundaryFloorHard,
            profileBurstContrastS: profileBurstContrastS,
            profilePossibleContrastS: profilePossibleContrastS,
            hfSpikingQ80Sec: hfSpikingQ80Sec,
            hfSpikingQ80MaxSec: hfSpikingQ80MaxSec,
            hfSpikingQ90MaxSec: hfSpikingQ90MaxSec,
            hfSpikingShortUpperSec: hfSpikingShortUpperSec,
            hfSpikingEpochBridgeSec: hfSpikingEpochBridgeSec,
            hfSpikingToleratedGapSec: hfSpikingToleratedGapSec,
            hfSpikingPatternMaxISISec: hfSpikingPatternMaxISISec,
            hfSpikingPauseBreakSec: hfSpikingPauseBreakSec,
            hfSpikingShortFraction: hfSpikingShortFraction,
            hfSpikingQ90ShortFraction: hfSpikingQ90ShortFraction,
            hfSpikingBridgeFraction: hfSpikingBridgeFraction,
            hfSpikingLargeFraction: hfSpikingLargeFraction,
            hfSpikingToleratedFraction: hfSpikingToleratedFraction,
            hfSpikingMaxConsecutiveLargeISI: hfSpikingMaxConsecutiveLargeISI,
            hfSpikingMinSpikesRequired: hfSpikingMinSpikesRequired,
            hfSpikingAcceptanceRoute: hfSpikingAcceptanceRoute,
            hfSpikingBurstDominated: hfSpikingBurstDominated,
            hfSpikingEmbeddedBurstCount: hfSpikingEmbeddedBurstCount,
            hfSpikingEmbeddedBurstGroupCount: hfSpikingEmbeddedBurstGroupCount,
            hfSpikingEmbeddedBurstCoverage: hfSpikingEmbeddedBurstCoverage,
            hfSpikingBurstPacketLike: hfSpikingBurstPacketLike,
            hfSpikingBurstPacketNeighbor: hfSpikingBurstPacketNeighbor,
            suppressedByHFSpikingState: suppressedByHFSpikingState,
            suppressedOriginalLabel: suppressedOriginalLabel,
            hfSpikingSuppressorID: hfSpikingSuppressorID,
            stateRegularityScore: stateRegularityScore,
            stateBurstSeedFraction: stateBurstSeedFraction,
            stateLowTailFraction: stateLowTailFraction,
            stateLocalStabilityScore: stateLocalStabilityScore,
            stateCoreBurstRunLength: stateCoreBurstRunLength,
            stateTonicSubtype: stateTonicSubtype,
            stateHighFrequencySubtype: stateHighFrequencySubtype,
            stateTrainPercentileMedian: stateTrainPercentileMedian,
            stateLocalPercentileMedian: stateLocalPercentileMedian,
            stateLocalPercentileQ90: stateLocalPercentileQ90,
            stateLocalRobustZMedian: stateLocalRobustZMedian,
            stateLocalRobustZAbsQ80: stateLocalRobustZAbsQ80,
            stateLocalRobustZQ10: stateLocalRobustZQ10,
            burstSeedRunStartISI: burstSeedRunStartISI,
            burstSeedRunEndISI: burstSeedRunEndISI,
            burstSeedBandLowerSec: burstSeedBandLowerSec,
            burstSeedBandUpperSec: burstSeedBandUpperSec,
            burstBridgeBandUpperSec: burstBridgeBandUpperSec,
            burstContrastRequired: burstContrastRequired,
            burstPossibleContrastRequired: burstPossibleContrastRequired,
            burstRequiredGapSec: burstRequiredGapSec,
            burstPossibleRequiredGapSec: burstPossibleRequiredGapSec,
            burstBoundaryFloorSec: burstBoundaryFloorSec,
            burstBoundaryFloorHard: burstBoundaryFloorHard,
            burstStrictBoundaryPass: burstStrictBoundaryPass,
            burstPossibleBoundaryPass: burstPossibleBoundaryPass,
            burstBridgeCountPass: burstBridgeCountPass,
            burstBridgeFractionPass: burstBridgeFractionPass,
            burstQ90BridgePass: burstQ90BridgePass,
            burstSizeLabelBeforeReview: burstSizeLabelBeforeReview,
            thresholdMode: thresholdMode,
            hardThreshold: hardThreshold,
            hardThresholdPattern: hardThresholdPattern,
            hardBurstSeedUpperSec: hardBurstSeedUpperSec,
            hardBurstBridgeUpperSec: hardBurstBridgeUpperSec,
            hardBurstCoreISICount: hardBurstCoreISICount,
            hardThresholdSource: hardThresholdSource,
            localBackgroundQ75Sec: localBackgroundQ75Sec,
            localCompressionQ90Ratio: localCompressionQ90Ratio,
            eventLocalMedianSec: eventLocalMedianSec,
            eventLocalPercentileMedian: eventLocalPercentileMedian,
            eventLocalPercentileQ90: eventLocalPercentileQ90,
            eventLocalRobustZMedian: eventLocalRobustZMedian,
            eventLocalRobustZAbsQ80: eventLocalRobustZAbsQ80,
            eventLocalRobustZQ10: eventLocalRobustZQ10
        )
    }

    func withDiagnosticOverride(
        finalLabel: ClassicAnchorLabel? = nil,
        gateStatus: String? = nil,
        decisionPath: String? = nil,
        action: String? = nil,
        score: Double? = nil,
        priority: Int? = nil,
        selectedForAuto: Bool = false,
        selectionStatus: String = "not_selected"
    ) -> ClassicAnchorCandidate {
        ClassicAnchorCandidate(
            id: id,
            trainID: trainID,
            trainName: trainName,
            candidateLayer: candidateLayer,
            candidateClass: candidateClass,
            finalLabel: finalLabel ?? self.finalLabel,
            gateStatus: gateStatus ?? self.gateStatus,
            decisionPath: decisionPath ?? self.decisionPath,
            action: action ?? self.action,
            score: score ?? self.score,
            priority: priority ?? self.priority,
            selectedForAuto: selectedForAuto,
            selectionStatus: selectionStatus,
            startISIIndex: startISIIndex,
            endISIIndex: endISIIndex,
            startSpikeIndex: startSpikeIndex,
            endSpikeIndex: endSpikeIndex,
            nISI: nISI,
            nValidISI: nValidISI,
            nSpikes: nSpikes,
            durationSec: durationSec,
            intraQ10Sec: intraQ10Sec,
            intraQ40Sec: intraQ40Sec,
            intraQ50Sec: intraQ50Sec,
            intraQ90Sec: intraQ90Sec,
            intraQ95Sec: intraQ95Sec,
            maxIntraISISec: maxIntraISISec,
            meanIntraISISec: meanIntraISISec,
            cv: cv,
            cv2: cv2,
            lv: lv,
            preGapSec: preGapSec,
            postGapSec: postGapSec,
            preRatioQ90: preRatioQ90,
            postRatioQ90: postRatioQ90,
            edgeContrastMinQ90: edgeContrastMinQ90,
            edgeContrastGeomQ90: edgeContrastGeomQ90,
            anchorFamily: anchorFamily,
            anchorLockLevel: anchorLockLevel,
            anchorBandLowerSec: anchorBandLowerSec,
            anchorBandUpperSec: anchorBandUpperSec,
            anchorBandSource: anchorBandSource,
            anchorContrastMinRequired: anchorContrastMinRequired,
            anchorContrastGeomRequired: anchorContrastGeomRequired,
            refractorySuspectCount: refractorySuspectCount,
            refractorySuspectAction: refractorySuspectAction,
            profileSeedLowPercentileInTrain: profileSeedLowPercentileInTrain,
            profileSeedHighPercentileInTrain: profileSeedHighPercentileInTrain,
            profileSeedBandFraction: profileSeedBandFraction,
            profileSeedRunCount: profileSeedRunCount,
            profileMaxSeedRunLength: profileMaxSeedRunLength,
            profileMedianISISec: profileMedianISISec,
            profileQ10ISISec: profileQ10ISISec,
            profileQ25ISISec: profileQ25ISISec,
            profileQ90ISISec: profileQ90ISISec,
            profilePauseFraction: profilePauseFraction,
            profilePhenotypePrior: profilePhenotypePrior,
            profileBridgeUpperSec: profileBridgeUpperSec,
            profileBoundaryFloorSec: profileBoundaryFloorSec,
            profileBoundaryFloorHard: profileBoundaryFloorHard,
            profileBurstContrastS: profileBurstContrastS,
            profilePossibleContrastS: profilePossibleContrastS,
            hfSpikingQ80Sec: hfSpikingQ80Sec,
            hfSpikingQ80MaxSec: hfSpikingQ80MaxSec,
            hfSpikingQ90MaxSec: hfSpikingQ90MaxSec,
            hfSpikingShortUpperSec: hfSpikingShortUpperSec,
            hfSpikingEpochBridgeSec: hfSpikingEpochBridgeSec,
            hfSpikingToleratedGapSec: hfSpikingToleratedGapSec,
            hfSpikingPatternMaxISISec: hfSpikingPatternMaxISISec,
            hfSpikingPauseBreakSec: hfSpikingPauseBreakSec,
            hfSpikingShortFraction: hfSpikingShortFraction,
            hfSpikingQ90ShortFraction: hfSpikingQ90ShortFraction,
            hfSpikingBridgeFraction: hfSpikingBridgeFraction,
            hfSpikingLargeFraction: hfSpikingLargeFraction,
            hfSpikingToleratedFraction: hfSpikingToleratedFraction,
            hfSpikingMaxConsecutiveLargeISI: hfSpikingMaxConsecutiveLargeISI,
            hfSpikingMinSpikesRequired: hfSpikingMinSpikesRequired,
            hfSpikingAcceptanceRoute: hfSpikingAcceptanceRoute,
            hfSpikingBurstDominated: hfSpikingBurstDominated,
            hfSpikingEmbeddedBurstCount: hfSpikingEmbeddedBurstCount,
            hfSpikingEmbeddedBurstGroupCount: hfSpikingEmbeddedBurstGroupCount,
            hfSpikingEmbeddedBurstCoverage: hfSpikingEmbeddedBurstCoverage,
            hfSpikingBurstPacketLike: hfSpikingBurstPacketLike,
            hfSpikingBurstPacketNeighbor: hfSpikingBurstPacketNeighbor,
            suppressedByHFSpikingState: suppressedByHFSpikingState,
            suppressedOriginalLabel: suppressedOriginalLabel,
            hfSpikingSuppressorID: hfSpikingSuppressorID,
            stateRegularityScore: stateRegularityScore,
            stateBurstSeedFraction: stateBurstSeedFraction,
            stateLowTailFraction: stateLowTailFraction,
            stateLocalStabilityScore: stateLocalStabilityScore,
            stateCoreBurstRunLength: stateCoreBurstRunLength,
            stateTonicSubtype: stateTonicSubtype,
            stateHighFrequencySubtype: stateHighFrequencySubtype,
            stateTrainPercentileMedian: stateTrainPercentileMedian,
            stateLocalPercentileMedian: stateLocalPercentileMedian,
            stateLocalPercentileQ90: stateLocalPercentileQ90,
            stateLocalRobustZMedian: stateLocalRobustZMedian,
            stateLocalRobustZAbsQ80: stateLocalRobustZAbsQ80,
            stateLocalRobustZQ10: stateLocalRobustZQ10,
            burstSeedRunStartISI: burstSeedRunStartISI,
            burstSeedRunEndISI: burstSeedRunEndISI,
            burstSeedBandLowerSec: burstSeedBandLowerSec,
            burstSeedBandUpperSec: burstSeedBandUpperSec,
            burstBridgeBandUpperSec: burstBridgeBandUpperSec,
            burstContrastRequired: burstContrastRequired,
            burstPossibleContrastRequired: burstPossibleContrastRequired,
            burstRequiredGapSec: burstRequiredGapSec,
            burstPossibleRequiredGapSec: burstPossibleRequiredGapSec,
            burstBoundaryFloorSec: burstBoundaryFloorSec,
            burstBoundaryFloorHard: burstBoundaryFloorHard,
            burstStrictBoundaryPass: burstStrictBoundaryPass,
            burstPossibleBoundaryPass: burstPossibleBoundaryPass,
            burstBridgeCountPass: burstBridgeCountPass,
            burstBridgeFractionPass: burstBridgeFractionPass,
            burstQ90BridgePass: burstQ90BridgePass,
            burstSizeLabelBeforeReview: burstSizeLabelBeforeReview,
            thresholdMode: thresholdMode,
            hardThreshold: hardThreshold,
            hardThresholdPattern: hardThresholdPattern,
            hardBurstSeedUpperSec: hardBurstSeedUpperSec,
            hardBurstBridgeUpperSec: hardBurstBridgeUpperSec,
            hardBurstCoreISICount: hardBurstCoreISICount,
            hardThresholdSource: hardThresholdSource,
            localBackgroundQ75Sec: localBackgroundQ75Sec,
            localCompressionQ90Ratio: localCompressionQ90Ratio,
            eventLocalMedianSec: eventLocalMedianSec,
            eventLocalPercentileMedian: eventLocalPercentileMedian,
            eventLocalPercentileQ90: eventLocalPercentileQ90,
            eventLocalRobustZMedian: eventLocalRobustZMedian,
            eventLocalRobustZAbsQ80: eventLocalRobustZAbsQ80,
            eventLocalRobustZQ10: eventLocalRobustZQ10
        )
    }

    /// Copy this candidate with a recomputed sub-span geometry: a narrower span and the span's RECOMPUTED interval
    /// metrics, preserving every semantic/diagnostic field (id, finalLabel, score, priority, selection, locks,
    /// profile/hf/state provenance). The span-derived metric fields (quantiles, cv/lv, edge contrasts, gaps, counts)
    /// come from the new sub-span; `cv2`, local-context, and all anchor/profile fields are kept from the original (a
    /// single-edge change does not meaningfully move them, and the canonicalization verdict recomputes its own
    /// q/coverage from the slice). The caller supplies the recomputed metrics (e.g. from `spanMetrics`).
    func withGeometry(
        startISIIndex: Int, endISIIndex: Int, startSpikeIndex: Int, endSpikeIndex: Int,
        nISI: Int, nValidISI: Int, nSpikes: Int, durationSec: Double?,
        intraQ10Sec: Double?, intraQ40Sec: Double?, intraQ50Sec: Double?, intraQ90Sec: Double?, intraQ95Sec: Double?,
        maxIntraISISec: Double?, meanIntraISISec: Double?, cv: Double?, lv: Double?,
        preGapSec: Double?, postGapSec: Double?, preRatioQ90: Double?, postRatioQ90: Double?,
        edgeContrastMinQ90: Double?, edgeContrastGeomQ90: Double?,
        decisionPath: String
    ) -> ClassicAnchorCandidate {
        ClassicAnchorCandidate(
            id: id,
            trainID: trainID,
            trainName: trainName,
            candidateLayer: candidateLayer,
            candidateClass: candidateClass,
            finalLabel: finalLabel,
            gateStatus: gateStatus,
            decisionPath: decisionPath,
            action: action,
            score: score,
            priority: priority,
            selectedForAuto: selectedForAuto,
            selectionStatus: selectionStatus,
            startISIIndex: startISIIndex,
            endISIIndex: endISIIndex,
            startSpikeIndex: startSpikeIndex,
            endSpikeIndex: endSpikeIndex,
            nISI: nISI,
            nValidISI: nValidISI,
            nSpikes: nSpikes,
            durationSec: durationSec,
            intraQ10Sec: intraQ10Sec,
            intraQ40Sec: intraQ40Sec,
            intraQ50Sec: intraQ50Sec,
            intraQ90Sec: intraQ90Sec,
            intraQ95Sec: intraQ95Sec,
            maxIntraISISec: maxIntraISISec,
            meanIntraISISec: meanIntraISISec,
            cv: cv,
            cv2: cv2,
            lv: lv,
            preGapSec: preGapSec,
            postGapSec: postGapSec,
            preRatioQ90: preRatioQ90,
            postRatioQ90: postRatioQ90,
            edgeContrastMinQ90: edgeContrastMinQ90,
            edgeContrastGeomQ90: edgeContrastGeomQ90,
            anchorFamily: anchorFamily,
            anchorLockLevel: anchorLockLevel,
            anchorBandLowerSec: anchorBandLowerSec,
            anchorBandUpperSec: anchorBandUpperSec,
            anchorBandSource: anchorBandSource,
            anchorContrastMinRequired: anchorContrastMinRequired,
            anchorContrastGeomRequired: anchorContrastGeomRequired,
            refractorySuspectCount: refractorySuspectCount,
            refractorySuspectAction: refractorySuspectAction,
            profileSeedLowPercentileInTrain: profileSeedLowPercentileInTrain,
            profileSeedHighPercentileInTrain: profileSeedHighPercentileInTrain,
            profileSeedBandFraction: profileSeedBandFraction,
            profileSeedRunCount: profileSeedRunCount,
            profileMaxSeedRunLength: profileMaxSeedRunLength,
            profileMedianISISec: profileMedianISISec,
            profileQ10ISISec: profileQ10ISISec,
            profileQ25ISISec: profileQ25ISISec,
            profileQ90ISISec: profileQ90ISISec,
            profilePauseFraction: profilePauseFraction,
            profilePhenotypePrior: profilePhenotypePrior,
            profileBridgeUpperSec: profileBridgeUpperSec,
            profileBoundaryFloorSec: profileBoundaryFloorSec,
            profileBoundaryFloorHard: profileBoundaryFloorHard,
            profileBurstContrastS: profileBurstContrastS,
            profilePossibleContrastS: profilePossibleContrastS,
            hfSpikingQ80Sec: hfSpikingQ80Sec,
            hfSpikingQ80MaxSec: hfSpikingQ80MaxSec,
            hfSpikingQ90MaxSec: hfSpikingQ90MaxSec,
            hfSpikingShortUpperSec: hfSpikingShortUpperSec,
            hfSpikingEpochBridgeSec: hfSpikingEpochBridgeSec,
            hfSpikingToleratedGapSec: hfSpikingToleratedGapSec,
            hfSpikingPatternMaxISISec: hfSpikingPatternMaxISISec,
            hfSpikingPauseBreakSec: hfSpikingPauseBreakSec,
            hfSpikingShortFraction: hfSpikingShortFraction,
            hfSpikingQ90ShortFraction: hfSpikingQ90ShortFraction,
            hfSpikingBridgeFraction: hfSpikingBridgeFraction,
            hfSpikingLargeFraction: hfSpikingLargeFraction,
            hfSpikingToleratedFraction: hfSpikingToleratedFraction,
            hfSpikingMaxConsecutiveLargeISI: hfSpikingMaxConsecutiveLargeISI,
            hfSpikingMinSpikesRequired: hfSpikingMinSpikesRequired,
            hfSpikingAcceptanceRoute: hfSpikingAcceptanceRoute,
            hfSpikingBurstDominated: hfSpikingBurstDominated,
            hfSpikingEmbeddedBurstCount: hfSpikingEmbeddedBurstCount,
            hfSpikingEmbeddedBurstGroupCount: hfSpikingEmbeddedBurstGroupCount,
            hfSpikingEmbeddedBurstCoverage: hfSpikingEmbeddedBurstCoverage,
            hfSpikingBurstPacketLike: hfSpikingBurstPacketLike,
            hfSpikingBurstPacketNeighbor: hfSpikingBurstPacketNeighbor,
            suppressedByHFSpikingState: suppressedByHFSpikingState,
            suppressedOriginalLabel: suppressedOriginalLabel,
            hfSpikingSuppressorID: hfSpikingSuppressorID,
            stateRegularityScore: stateRegularityScore,
            stateBurstSeedFraction: stateBurstSeedFraction,
            stateLowTailFraction: stateLowTailFraction,
            stateLocalStabilityScore: stateLocalStabilityScore,
            stateCoreBurstRunLength: stateCoreBurstRunLength,
            stateTonicSubtype: stateTonicSubtype,
            stateHighFrequencySubtype: stateHighFrequencySubtype,
            stateTrainPercentileMedian: stateTrainPercentileMedian,
            stateLocalPercentileMedian: stateLocalPercentileMedian,
            stateLocalPercentileQ90: stateLocalPercentileQ90,
            stateLocalRobustZMedian: stateLocalRobustZMedian,
            stateLocalRobustZAbsQ80: stateLocalRobustZAbsQ80,
            stateLocalRobustZQ10: stateLocalRobustZQ10,
            burstSeedRunStartISI: burstSeedRunStartISI,
            burstSeedRunEndISI: burstSeedRunEndISI,
            burstSeedBandLowerSec: burstSeedBandLowerSec,
            burstSeedBandUpperSec: burstSeedBandUpperSec,
            burstBridgeBandUpperSec: burstBridgeBandUpperSec,
            burstContrastRequired: burstContrastRequired,
            burstPossibleContrastRequired: burstPossibleContrastRequired,
            burstRequiredGapSec: burstRequiredGapSec,
            burstPossibleRequiredGapSec: burstPossibleRequiredGapSec,
            burstBoundaryFloorSec: burstBoundaryFloorSec,
            burstBoundaryFloorHard: burstBoundaryFloorHard,
            burstStrictBoundaryPass: burstStrictBoundaryPass,
            burstPossibleBoundaryPass: burstPossibleBoundaryPass,
            burstBridgeCountPass: burstBridgeCountPass,
            burstBridgeFractionPass: burstBridgeFractionPass,
            burstQ90BridgePass: burstQ90BridgePass,
            burstSizeLabelBeforeReview: burstSizeLabelBeforeReview,
            thresholdMode: thresholdMode,
            hardThreshold: hardThreshold,
            hardThresholdPattern: hardThresholdPattern,
            hardBurstSeedUpperSec: hardBurstSeedUpperSec,
            hardBurstBridgeUpperSec: hardBurstBridgeUpperSec,
            hardBurstCoreISICount: hardBurstCoreISICount,
            hardThresholdSource: hardThresholdSource,
            localBackgroundQ75Sec: localBackgroundQ75Sec,
            localCompressionQ90Ratio: localCompressionQ90Ratio,
            eventLocalMedianSec: eventLocalMedianSec,
            eventLocalPercentileMedian: eventLocalPercentileMedian,
            eventLocalPercentileQ90: eventLocalPercentileQ90,
            eventLocalRobustZMedian: eventLocalRobustZMedian,
            eventLocalRobustZAbsQ80: eventLocalRobustZAbsQ80,
            eventLocalRobustZQ10: eventLocalRobustZQ10
        )
    }
}
