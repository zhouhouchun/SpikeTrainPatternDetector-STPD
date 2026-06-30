import Foundation

/// Controls whether HFS protection follows the historical single-label behavior
/// or the Phase 1B multi-track event/state semantics.
public enum HFSpikingProtectionMode: String, CaseIterable, Hashable, Sendable {
    /// Historical behavior: packet-like HFS states may reject neighboring HFS
    /// states and strong HFS states may suppress embedded compact bursts.
    case legacySingleLabel = "legacy_single_label"

    /// Multi-track behavior: HFS is a state. This pass only annotates HFS
    /// packetization and burst-dominance evidence; final HFS-vs-burst mutual
    /// exclusion is applied by the semantic-track arbitrator after state
    /// selection.
    case multiTrack = "multi_track"
}

public enum HFSpikingProtection {
    /// Backward-compatible entry point. The production default is now the
    /// non-destructive multi-track policy.
    public static func apply(
        to candidates: [ClassicAnchorCandidate],
        mode: HFSpikingProtectionMode = .multiTrack
    ) -> [ClassicAnchorCandidate] {
        applyInternal(
            to: candidates,
            explicitlySelectedEvents: nil,
            mode: mode
        )
    }

    /// Preferred Phase 1B entry point.
    ///
    /// `selectedEvents` must be the canonical event-track winners from the same
    /// arbitration pass. Using selected events prevents duplicate, weak, or
    /// mutually exclusive burst proposals from inflating HFS packetization.
    public static func apply(
        to candidates: [ClassicAnchorCandidate],
        selectedEvents: [ClassicAnchorCandidate],
        mode: HFSpikingProtectionMode = .multiTrack
    ) -> [ClassicAnchorCandidate] {
        applyInternal(
            to: candidates,
            explicitlySelectedEvents: selectedEvents,
            mode: mode
        )
    }

    private static func applyInternal(
        to candidates: [ClassicAnchorCandidate],
        explicitlySelectedEvents: [ClassicAnchorCandidate]?,
        mode: HFSpikingProtectionMode
    ) -> [ClassicAnchorCandidate] {
        guard !candidates.isEmpty else {
            return []
        }

        var overrides: [String: ClassicAnchorCandidate] = [:]
        let grouped = Dictionary(grouping: candidates, by: \.trainID)
        let explicitEventsByTrain = explicitlySelectedEvents.map {
            Dictionary(grouping: $0, by: \.trainID)
        }

        for (trainID, trainCandidates) in grouped {
            let highFrequencyStates = trainCandidates.filter {
                $0.finalLabel == .highFrequencySpiking && $0.isEligibleForAutoSelection
            }
            guard !highFrequencyStates.isEmpty else {
                continue
            }

            let canonicalBurstFamily = canonicalEvents(
                forTrainID: trainID,
                trainCandidates: trainCandidates,
                explicitEventsByTrain: explicitEventsByTrain,
                mode: mode
            )
            for state in highFrequencyStates {
                let overlappingBursts = canonicalBurstFamily.filter { state.overlaps($0) }
                let embedded = intervalUnionStats(overlappingBursts, container: state)
                let minGroups = max(6, Int(ceil(0.055 * Double(max(1, state.nISI)))))
                let burstDominated = embedded.groupCount >= minGroups && embedded.coverage >= 0.25
                let selectedEventPacketLike = state.isVariableHFSpikingState && (
                    (embedded.groupCount >= 2 && embedded.coverage >= 0.08) ||
                        (embedded.groupCount >= 1 && embedded.coverage >= 0.18)
                )
                let provisionalSelfPacketLike = state.hfSpikingBurstPacketLike == true
                let packetLike = selectedEventPacketLike || provisionalSelfPacketLike

                var annotated = state.withHFProtectionStats(
                    embeddedBurstCount: overlappingBursts.count,
                    embeddedBurstGroupCount: embedded.groupCount,
                    embeddedBurstCoverage: embedded.coverage,
                    burstDominated: burstDominated,
                    packetLike: packetLike,
                    packetNeighbor: false
                )
                annotated.hfSpikingAcceptanceRoute = appendRoute(
                    annotated.hfSpikingAcceptanceRoute,
                    "selected_event_packetization"
                )

                if burstDominated {
                    overrides[state.id] = annotated.rejectedByHFProtection(
                        reason: "reject_burst_dominated_hf_spiking_state"
                    )
                    continue
                }

                if mode == .legacySingleLabel, packetLike {
                    overrides[state.id] = annotated.rejectedByHFProtection(
                        reason: "reject_burst_packet_like_hf_spiking_state"
                    )
                    continue
                }

                let policyReason = packetLike
                    ? [
                        "hfs_packet_like_but_not_dominated__retain_state_for_mutual_exclusion_arbitration",
                        "selected_event_packet_like=\(selectedEventPacketLike)",
                        "selected_event_group_count=\(embedded.groupCount)",
                        "selected_event_coverage=\(formatHFDiagnostic(embedded.coverage))",
                        "provisional_self_packet_like=\(provisionalSelfPacketLike)",
                        "provisional_self_group_count=\(state.hfSpikingEmbeddedBurstGroupCount.map(String.init) ?? "NA")",
                        "provisional_self_coverage=\(formatHFDiagnostic(state.hfSpikingEmbeddedBurstCoverage))"
                    ].joined(separator: ";")
                    : "hfs_not_burst_dominated__retain_state"
                annotated = annotated.annotatedByHFProtection(reason: policyReason)
                overrides[state.id] = annotated
            }

            let packetStates = highFrequencyStates.compactMap { state -> ClassicAnchorCandidate? in
                let candidate = overrides[state.id] ?? state
                return candidate.hfSpikingBurstPacketLike == true ? candidate : nil
            }

            if !packetStates.isEmpty {
                for state in highFrequencyStates {
                    let current = overrides[state.id] ?? state
                    guard current.isEligibleForAutoSelection,
                          current.isVariableHFSpikingState,
                          packetStates.contains(where: { current.isPacketNeighbor(of: $0) }) else {
                        continue
                    }

                    let annotated = current.withHFProtectionStats(
                        embeddedBurstCount: current.hfSpikingEmbeddedBurstCount,
                        embeddedBurstGroupCount: current.hfSpikingEmbeddedBurstGroupCount,
                        embeddedBurstCoverage: current.hfSpikingEmbeddedBurstCoverage,
                        burstDominated: current.hfSpikingBurstDominated ?? false,
                        packetLike: current.hfSpikingBurstPacketLike ?? false,
                        packetNeighbor: true
                    )

                    if mode == .legacySingleLabel {
                        overrides[state.id] = annotated.rejectedByHFProtection(
                            reason: "reject_burst_packet_neighbor_hf_spiking_state"
                        )
                    } else {
                        overrides[state.id] = annotated.annotatedByHFProtection(
                            reason: "hfs_packet_neighbor__retain_for_state_track_arbitration"
                        )
                    }
                }
            }

            // Legacy mode preserves the old destructive single-label policy.
            // Multi-track mode leaves all other semantic tracks intact and lets
            // the state-track selector arbitrate HFS versus tonic/HFT.
            guard mode == .legacySingleLabel else {
                continue
            }

            for state in highFrequencyStates {
                let currentState = overrides[state.id] ?? state
                guard currentState.isEligibleForAutoSelection else {
                    continue
                }

                let suppressibleLabels: Set<ClassicAnchorLabel> = [.highFrequencyTonic, .tonic]
                for candidate in trainCandidates where candidate.id != currentState.id &&
                    candidate.isEligibleForAutoSelection &&
                    suppressibleLabels.contains(candidate.finalLabel) &&
                    candidate.overlaps(currentState) {
                    let existing = overrides[candidate.id] ?? candidate
                    guard existing.isEligibleForAutoSelection else {
                        continue
                    }
                    overrides[candidate.id] = existing.suppressedByHFProtection(
                        suppressorID: currentState.id,
                        reason: "suppressed_by_long_hf_spiking_state",
                        action: "suppress_for_hf_spiking_state"
                    )
                }

                guard currentState.isStrongHFSpikingState else {
                    continue
                }

                let maxEmbeddedN = max(8, Int(ceil(0.06 * Double(max(1, currentState.nISI)))))
                let compactEmbeddedBursts = canonicalBurstFamily.filter {
                    currentState.fullyContains($0) &&
                        $0.nISI <= maxEmbeddedN &&
                        ($0.durationSec == nil || ($0.durationSec ?? .infinity) <= 0.25)
                }
                for candidate in compactEmbeddedBursts {
                    let existing = overrides[candidate.id] ?? candidate
                    guard existing.isEligibleForAutoSelection else {
                        continue
                    }
                    overrides[candidate.id] = existing.suppressedByHFProtection(
                        suppressorID: currentState.id,
                        reason: "compact_burst_kernel_suppressed_inside_long_hf_spiking_state",
                        action: "suppress_embedded_burst_for_hf_spiking_state"
                    )
                }
            }
        }

        guard !overrides.isEmpty else {
            return candidates
        }
        return candidates.map { overrides[$0.id] ?? $0 }
    }

    private static func canonicalEvents(
        forTrainID trainID: String,
        trainCandidates: [ClassicAnchorCandidate],
        explicitEventsByTrain: [String: [ClassicAnchorCandidate]]?,
        mode: HFSpikingProtectionMode
    ) -> [ClassicAnchorCandidate] {
        if let explicitEventsByTrain {
            return (explicitEventsByTrain[trainID] ?? []).filter {
                $0.selectedForAuto &&
                    $0.finalLabel.isCanonicalBurstFamily &&
                    $0.isEligibleForAutoSelection
            }
        }

        switch mode {
        case .multiTrack:
            // Never infer packet dominance from raw or duplicate proposals.
            // Without an explicit event set, only candidates already selected
            // on the event track are admissible evidence.
            return trainCandidates.filter {
                $0.selectedForAuto &&
                    $0.finalLabel.isCanonicalBurstFamily &&
                    $0.isEligibleForAutoSelection
            }

        case .legacySingleLabel:
            // Preserve historical behavior for controlled regression runs.
            return trainCandidates.filter {
                $0.finalLabel.isCanonicalBurstFamily &&
                    $0.isEligibleForAutoSelection
            }
        }
    }

    private static func isCompactBurstPacketOverlay(_ candidate: ClassicAnchorCandidate) -> Bool {
        let duration = candidate.durationSec ?? .infinity
        if candidate.nISI <= 5 && duration <= 0.08 {
            return true
        }

        let edge = max(
            candidate.edgeContrastMinQ90 ?? 0,
            candidate.edgeContrastGeomQ90 ?? 0
        )
        guard edge >= 3.0 else {
            return false
        }

        let maxSpikes: Int
        let maxDuration: Double
        switch candidate.finalLabel {
        case .highFrequencyBurst:
            maxSpikes = 15
            maxDuration = 0.15
        case .burst, .longBurst:
            maxSpikes = 12
            maxDuration = 0.12
        case .possibleBurst:
            maxSpikes = 10
            maxDuration = 0.10
        default:
            return false
        }

        return candidate.nSpikes <= maxSpikes &&
            duration <= maxDuration
    }

    private static func formatHFDiagnostic(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.4g", value)
    }

    private static func appendRoute(_ route: String?, _ component: String) -> String {
        let trimmed = route?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        guard !trimmed.isEmpty else {
            return component
        }
        guard !trimmed.contains(component) else {
            return trimmed
        }
        return "\(trimmed)+\(component)"
    }

    private static func intervalUnionStats(
        _ candidates: [ClassicAnchorCandidate],
        container: ClassicAnchorCandidate
    ) -> (covered: Int, groupCount: Int, coverage: Double) {
        let containerStart = min(container.startISIIndex, container.endISIIndex)
        let containerEnd = max(container.startISIIndex, container.endISIIndex)
        guard containerStart <= containerEnd else {
            return (0, 0, 0)
        }

        let ranges = candidates.compactMap { candidate -> (start: Int, end: Int)? in
            let start = max(containerStart, min(candidate.startISIIndex, candidate.endISIIndex))
            let end = min(containerEnd, max(candidate.startISIIndex, candidate.endISIIndex))
            guard start <= end else {
                return nil
            }
            return (start, end)
        }
        .sorted { lhs, rhs in
            if lhs.start != rhs.start {
                return lhs.start < rhs.start
            }
            return lhs.end < rhs.end
        }

        guard var current = ranges.first else {
            return (0, 0, 0)
        }

        var covered = 0
        var groupCount = 1
        for range in ranges.dropFirst() {
            if range.start <= current.end + 1 {
                current.end = max(current.end, range.end)
            } else {
                covered += current.end - current.start + 1
                current = range
                groupCount += 1
            }
        }
        covered += current.end - current.start + 1

        let containerLength = max(1, containerEnd - containerStart + 1)
        let coverage = min(1, Double(covered) / Double(containerLength))
        return (covered, groupCount, coverage)
    }
}

private extension ClassicAnchorCandidate {
    func fullyContains(_ other: ClassicAnchorCandidate) -> Bool {
        min(startISIIndex, endISIIndex) <= min(other.startISIIndex, other.endISIIndex) &&
            max(startISIIndex, endISIIndex) >= max(other.startISIIndex, other.endISIIndex)
    }

    func overlaps(_ other: ClassicAnchorCandidate) -> Bool {
        min(startISIIndex, endISIIndex) <= max(other.startISIIndex, other.endISIIndex) &&
            max(startISIIndex, endISIIndex) >= min(other.startISIIndex, other.endISIIndex)
    }

    func rejectedByHFProtection(reason: String) -> ClassicAnchorCandidate {
        let diagnostic = [reason] + hfProtectionDiagnosticContext()
        var candidate = withDiagnosticOverride(
            finalLabel: .reject,
            gateStatus: "hf_protected_reject",
            decisionPath: appendingDiagnosticReason(diagnostic.joined(separator: ";")),
            action: reason,
            score: 0,
            priority: 0
        )
        candidate.suppressedOriginalLabel = finalLabel.rawValue
        return candidate
    }

    func suppressedByHFProtection(
        suppressorID: String,
        reason: String,
        action: String
    ) -> ClassicAnchorCandidate {
        let diagnostic = ([reason, "suppressor=\(suppressorID)"] + hfProtectionDiagnosticContext())
            .joined(separator: ";")
        var candidate = withDiagnosticOverride(
            finalLabel: .reject,
            gateStatus: "hf_protected_suppressed",
            decisionPath: appendingDiagnosticReason(diagnostic),
            action: action,
            score: 0,
            priority: 0,
            selectionStatus: "suppressed_by_hf_spiking_state_candidate"
        )
        candidate.suppressedByHFSpikingState = true
        candidate.suppressedOriginalLabel = finalLabel.rawValue
        candidate.hfSpikingSuppressorID = suppressorID
        return candidate
    }

    func annotatedByHFProtection(reason: String) -> ClassicAnchorCandidate {
        let diagnostic = ([reason] + hfProtectionDiagnosticContext()).joined(separator: ";")
        return withDiagnosticOverride(
            decisionPath: appendingDiagnosticReason(diagnostic),
            selectedForAuto: selectedForAuto,
            selectionStatus: selectionStatus
        )
    }

    func withHFProtectionStats(
        embeddedBurstCount: Int?,
        embeddedBurstGroupCount: Int?,
        embeddedBurstCoverage: Double?,
        burstDominated: Bool,
        packetLike: Bool,
        packetNeighbor: Bool
    ) -> ClassicAnchorCandidate {
        var candidate = self
        candidate.hfSpikingEmbeddedBurstCount = embeddedBurstCount
        candidate.hfSpikingEmbeddedBurstGroupCount = embeddedBurstGroupCount
        candidate.hfSpikingEmbeddedBurstCoverage = embeddedBurstCoverage
        candidate.hfSpikingBurstDominated = burstDominated
        candidate.hfSpikingBurstPacketLike = packetLike
        candidate.hfSpikingBurstPacketNeighbor = packetNeighbor
        // Keep the HF-family subtype consistent with the protection verdict: a state the
        // selected canonical events show to be burst-dominated is hf_burst_dominant; otherwise
        // it remains a sustained irregular HF state. finalLabel is untouched here.
        if candidate.finalLabel == .highFrequencySpiking {
            candidate.stateHighFrequencySubtype = burstDominated ? "hf_burst_dominant" : "hf_irregular_spiking"
        }
        return candidate
    }

    var isStrongHFSpikingState: Bool {
        let q90Pass = intraQ90Sec == nil ||
            hfSpikingQ90MaxSec == nil ||
            (intraQ90Sec ?? .infinity) <= (hfSpikingQ90MaxSec ?? .infinity)
        let sustained = nISI >= 80 &&
            (hfSpikingShortFraction == nil || (hfSpikingShortFraction ?? 0) >= 0.70) &&
            (hfSpikingBridgeFraction == nil || (hfSpikingBridgeFraction ?? 0) >= 0.90) &&
            q90Pass
        let compactPure = nISI >= 30 &&
            (hfSpikingShortFraction ?? -.infinity) >= 0.85 &&
            (hfSpikingBridgeFraction ?? -.infinity) >= 0.95 &&
            q90Pass
        return sustained || compactPure
    }

    func isPacketNeighbor(of packet: ClassicAnchorCandidate) -> Bool {
        guard id != packet.id else {
            return false
        }
        let start = min(startISIIndex, endISIIndex)
        let end = max(startISIIndex, endISIIndex)
        let packetStart = min(packet.startISIIndex, packet.endISIIndex)
        let packetEnd = max(packet.startISIIndex, packet.endISIIndex)
        let gapN: Int
        let gapSec: Double?
        if packetEnd < start {
            gapN = start - packetEnd - 1
            gapSec = preGapSec ?? packet.postGapSec
        } else if end < packetStart {
            gapN = packetStart - end - 1
            gapSec = postGapSec ?? packet.preGapSec
        } else {
            gapN = 0
            gapSec = 0
        }
        guard gapN >= 0 && gapN <= 3 else {
            return false
        }
        let tolerance = max(hfSpikingToleratedGapSec ?? 0.075, packet.hfSpikingToleratedGapSec ?? 0.075)
        return (gapSec ?? .infinity) <= tolerance
    }

    func appendingDiagnosticReason(_ reason: String) -> String {
        guard !decisionPath.contains(reason) else {
            return decisionPath
        }
        guard !decisionPath.isEmpty else {
            return reason
        }
        return "\(decisionPath);\(reason)"
    }

    private func hfProtectionDiagnosticContext() -> [String] {
        [
            "hf_embedded_burst_count=\(hfSpikingEmbeddedBurstCount.map { String($0) } ?? "NA")",
            "hf_embedded_burst_groups=\(hfSpikingEmbeddedBurstGroupCount.map { String($0) } ?? "NA")",
            "hf_embedded_burst_coverage=\(formatDiagnostic(hfSpikingEmbeddedBurstCoverage))",
            "hf_burst_dominated=\(hfSpikingBurstDominated.map { String($0) } ?? "NA")",
            "hf_packet_like=\(hfSpikingBurstPacketLike.map { String($0) } ?? "NA")",
            "hf_packet_neighbor=\(hfSpikingBurstPacketNeighbor.map { String($0) } ?? "NA")",
            "hf_short_fraction=\(formatDiagnostic(hfSpikingShortFraction))",
            "hf_bridge_fraction=\(formatDiagnostic(hfSpikingBridgeFraction))",
            "hf_large_fraction=\(formatDiagnostic(hfSpikingLargeFraction))"
        ]
    }

    private func formatDiagnostic(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.4g", value)
    }
}

// `isVariableHFSpikingState` is exposed at module-internal level (rather than fileprivate)
// solely so the MM-removal regression test can assert it is driven by CV / LV / HFS
// large-fraction evidence and no longer by the removed max/mean (MM) term.
extension ClassicAnchorCandidate {
    var isVariableHFSpikingState: Bool {
        (cv.map { $0 >= 0.65 } ?? false) ||
            (lv.map { $0 >= 0.45 } ?? false) ||
            (hfSpikingLargeFraction.map { $0 >= 0.08 } ?? false)
    }
}
