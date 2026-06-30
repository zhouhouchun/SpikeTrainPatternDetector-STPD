import Foundation

/// Diagnostic-only audit for ambiguous HFS-versus-burst regions.
///
/// This layer does not change candidate generation or arbitration. It records
/// the evidence used by the existing multi-track pipeline so UI hover, review
/// tables, and CSV export can explain why a region remained HFS, suppressed a
/// burst proposal, selected burst, or rejected HFS as packet-dominated.
public enum HFSBurstArbitrationDecision: String, CaseIterable, Codable, Hashable, Sendable {
    case hfsSelectedWithBurstConflict = "hfs_selected_with_burst_conflict"
    case hfsSelectedWithLongBurstConflict = "hfs_selected_with_long_burst_conflict"
    case hfsSelectedWithMixedBurstOutcomes = "hfs_selected_with_mixed_burst_outcomes"
    case hfsSelectedBurstProposalSuppressed = "hfs_selected_burst_proposal_suppressed"
    case hfsSelectedNoBurstWinner = "hfs_selected_no_burst_winner"
    case burstSelectedHFSRejectedPacketDominance = "burst_selected_hfs_rejected_packet_dominance"
    case longBurstSelectedHFSRejected = "long_burst_selected_hfs_rejected"
    case burstSelectedHFSNotSelected = "burst_selected_hfs_not_selected"
    case hfsRejectedNoBurstWinner = "hfs_rejected_no_burst_winner"
    case unresolvedReview = "unresolved_review"
}

public struct HFSBurstArbitrationAuditSettings: Hashable, Sendable {
    /// By default, emit only true conflict/packetization rows.
    public var includeNonConflictHFS: Bool

    /// State splitting leaves the original parent candidate in the audit pool.
    /// Excluding superseded parents avoids duplicate rows for parent and children.
    public var includeSupersededSplitParents: Bool

    /// Optional fallback when neither the HFS candidate nor selected pause gaps
    /// provide a pause-like threshold.
    public var fallbackPauseLikeThresholdSec: Double?

    /// Optional fallbacks for seed/bridge fractions when a burst proposal does
    /// not carry its adaptive band metadata.
    public var fallbackBurstSeedUpperSec: Double?
    public var fallbackBurstBridgeUpperSec: Double?

    public init(
        includeNonConflictHFS: Bool = false,
        includeSupersededSplitParents: Bool = false,
        fallbackPauseLikeThresholdSec: Double? = nil,
        fallbackBurstSeedUpperSec: Double? = nil,
        fallbackBurstBridgeUpperSec: Double? = nil
    ) {
        self.includeNonConflictHFS = includeNonConflictHFS
        self.includeSupersededSplitParents = includeSupersededSplitParents
        self.fallbackPauseLikeThresholdSec = Self.positiveOrNil(fallbackPauseLikeThresholdSec)
        self.fallbackBurstSeedUpperSec = Self.positiveOrNil(fallbackBurstSeedUpperSec)
        self.fallbackBurstBridgeUpperSec = Self.positiveOrNil(fallbackBurstBridgeUpperSec)
    }

    func fillingMissingFallbacks(
        pauseLikeThresholdSec: Double?,
        burstSeedUpperSec: Double?,
        burstBridgeUpperSec: Double?
    ) -> HFSBurstArbitrationAuditSettings {
        HFSBurstArbitrationAuditSettings(
            includeNonConflictHFS: includeNonConflictHFS,
            includeSupersededSplitParents: includeSupersededSplitParents,
            fallbackPauseLikeThresholdSec: fallbackPauseLikeThresholdSec ?? pauseLikeThresholdSec,
            fallbackBurstSeedUpperSec: fallbackBurstSeedUpperSec ?? burstSeedUpperSec,
            fallbackBurstBridgeUpperSec: fallbackBurstBridgeUpperSec ?? burstBridgeUpperSec
        )
    }

    private static func positiveOrNil(_ value: Double?) -> Double? {
        guard let value, value.isFinite, value > 0 else {
            return nil
        }
        return value
    }
}

/// One row corresponds to one terminal HFS state candidate (normally a
/// gap-bounded state fragment) and all overlapping burst-family evidence.
public struct HFSBurstArbitrationAuditRow: Identifiable, Codable, Hashable, Sendable {
    public let id: String
    public let pipelineStage: String
    public let trainID: String
    public let trainName: String

    public let hfsCandidateID: String
    public let hfsRootCandidateID: String
    public let hfsStartISIIndex: Int
    public let hfsEndISIIndex: Int
    public let conflictStartISIIndex: Int
    public let conflictEndISIIndex: Int

    /// Detector-native scores are preserved verbatim. HFS and burst detectors
    /// currently use different score scales; they must not be subtracted or
    /// interpreted as calibrated probabilities.
    public let scoreScaleNote: String
    public let hfsRawScore: Double
    public let hfsPriority: Int
    public let hfsSelectedForAuto: Bool
    public let hfsSelectionStatus: String
    public let hfsAcceptanceRoute: String?

    public let strongestBurstCandidateID: String?
    public let strongestBurstSubtype: String?
    public let strongestBurstRawScore: Double?
    public let strongestBurstPriority: Int?

    public let strongestLongBurstCandidateID: String?
    public let longBurstRawScore: Double?
    public let longBurstPriority: Int?

    /// Canonical packet evidence is the exact event set used by
    /// `HFSpikingProtection` for packet-dominance evaluation.
    public let packetEvidenceEventCount: Int
    public let packetCount: Int
    public let packetCoverage: Double
    public let allSelectedBurstEventCount: Int
    public let selectedBurstIICount: Int
    public let selectedLongBurstCount: Int
    public let suppressedBurstProposalCount: Int

    public let pauseLikeThresholdSec: Double?
    public let pauseLikeBreakCount: Int
    public let pauseLikeBreakGroupCount: Int
    public let pauseLikeBreakFraction: Double?

    public let seedBandLowerSec: Double?
    public let seedBandUpperSec: Double?
    public let bridgeBandUpperSec: Double?
    public let seedFraction: Double?
    public let bridgeFraction: Double?

    public let hfsShortFraction: Double?
    public let hfsBridgeFraction: Double?
    public let hfsLargeFraction: Double?
    public let hfsCV: Double?
    public let hfsLV: Double?
    public let burstPacketLike: Bool
    public let burstDominated: Bool

    public let finalDecision: HFSBurstArbitrationDecision
    public let finalSelectedEventSubtypes: [String]
    public let requiresReview: Bool
    public let decisionReason: String

    public var compactSummary: String {
        let hfsScore = Self.format(hfsRawScore)
        let longScore = Self.format(longBurstRawScore)
        let seed = Self.format(seedFraction)
        let bridge = Self.format(bridgeFraction)
        return [
            finalDecision.rawValue,
            "HFS score \(hfsScore)",
            "long-burst score \(longScore)",
            "packets \(packetCount)",
            "pause-like breaks \(pauseLikeBreakCount)",
            "seed fraction \(seed)",
            "bridge fraction \(bridge)"
        ].joined(separator: " | ")
    }

    public static var csvHeader: [String] {
        [
            "audit_id", "pipeline_stage", "train_id", "train_name",
            "hfs_candidate_id", "hfs_root_candidate_id",
            "hfs_start_isi", "hfs_end_isi", "conflict_start_isi", "conflict_end_isi",
            "score_scale_note", "hfs_raw_score", "hfs_priority",
            "hfs_selected_for_auto", "hfs_selection_status", "hfs_acceptance_route",
            "strongest_burst_candidate_id", "strongest_burst_subtype",
            "strongest_burst_raw_score", "strongest_burst_priority",
            "strongest_long_burst_candidate_id", "long_burst_raw_score", "long_burst_priority",
            "packet_evidence_event_count", "packet_count", "packet_coverage",
            "all_selected_burst_event_count", "selected_burst_ii_count",
            "selected_long_burst_count", "suppressed_burst_proposal_count",
            "pause_like_threshold_sec", "pause_like_break_count",
            "pause_like_break_group_count", "pause_like_break_fraction",
            "seed_band_lower_sec", "seed_band_upper_sec", "bridge_band_upper_sec",
            "seed_fraction", "bridge_fraction",
            "hfs_short_fraction", "hfs_bridge_fraction", "hfs_large_fraction",
            "hfs_cv", "hfs_lv", "burst_packet_like", "burst_dominated",
            "final_decision", "final_selected_event_subtypes", "requires_review",
            "decision_reason"
        ]
    }

    public var csvFields: [String] {
        [
            id, pipelineStage, trainID, trainName,
            hfsCandidateID, hfsRootCandidateID,
            String(hfsStartISIIndex), String(hfsEndISIIndex),
            String(conflictStartISIIndex), String(conflictEndISIIndex),
            scoreScaleNote, Self.format(hfsRawScore), String(hfsPriority),
            String(hfsSelectedForAuto), hfsSelectionStatus, hfsAcceptanceRoute ?? "",
            strongestBurstCandidateID ?? "", strongestBurstSubtype ?? "",
            Self.format(strongestBurstRawScore), strongestBurstPriority.map(String.init) ?? "",
            strongestLongBurstCandidateID ?? "", Self.format(longBurstRawScore),
            longBurstPriority.map(String.init) ?? "",
            String(packetEvidenceEventCount), String(packetCount), Self.format(packetCoverage),
            String(allSelectedBurstEventCount), String(selectedBurstIICount),
            String(selectedLongBurstCount), String(suppressedBurstProposalCount),
            Self.format(pauseLikeThresholdSec), String(pauseLikeBreakCount),
            String(pauseLikeBreakGroupCount), Self.format(pauseLikeBreakFraction),
            Self.format(seedBandLowerSec), Self.format(seedBandUpperSec),
            Self.format(bridgeBandUpperSec), Self.format(seedFraction), Self.format(bridgeFraction),
            Self.format(hfsShortFraction), Self.format(hfsBridgeFraction),
            Self.format(hfsLargeFraction), Self.format(hfsCV), Self.format(hfsLV),
            String(burstPacketLike), String(burstDominated), finalDecision.rawValue,
            finalSelectedEventSubtypes.joined(separator: "|"), String(requiresReview), decisionReason
        ]
    }

    private static func format(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return ""
        }
        return String(format: "%.10g", value)
    }

    private static func format(_ value: Double) -> String {
        value.isFinite ? String(format: "%.10g", value) : ""
    }
}

public enum HFSBurstArbitrationAudit {
    public static let scoreScaleNote = "detector_raw_scores_not_cross_calibrated"

    /// Build deterministic audit rows from the exact pre-protection and final
    /// pools used by Phase 1B.
    public static func build(
        train: SpikeTrain,
        preProtectionCandidates: [ClassicAnchorCandidate],
        selectedEventsUsedForPacketization: [ClassicAnchorCandidate],
        protectedCandidates: [ClassicAnchorCandidate],
        finalCandidates: [ClassicAnchorCandidate],
        pipelineStage: String,
        settings: HFSBurstArbitrationAuditSettings = HFSBurstArbitrationAuditSettings()
    ) -> [HFSBurstArbitrationAuditRow] {
        let preByID = Dictionary(
            uniqueKeysWithValues: uniqueCandidatesByID(
                preProtectionCandidates.filter { $0.trainID == train.id }
            ).map { ($0.id, $0) }
        )
        let protectedByID = Dictionary(
            uniqueKeysWithValues: uniqueCandidatesByID(
                protectedCandidates.filter { $0.trainID == train.id }
            ).map { ($0.id, $0) }
        )
        let finalByID = Dictionary(
            uniqueKeysWithValues: uniqueCandidatesByID(
                finalCandidates.filter { $0.trainID == train.id }
            ).map { ($0.id, $0) }
        )

        let hfsPool = terminalHFSCandidates(
            Array(preByID.values),
            includeSupersededParents: settings.includeSupersededSplitParents
        )
        let burstProposalPool = Array(preByID.values).filter {
            $0.finalLabel.isBurstEventFamily && $0.arbitrationTrack == .event
        }
        let finalSelectedEvents = Array(finalByID.values).filter {
            $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .event &&
                $0.finalLabel.isBurstEventFamily
        }
        let selectedGaps = Array(finalByID.values).filter {
            $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .gap &&
                $0.finalLabel == .pause
        }
        let selectedPauseFloor = pauseFloor(train: train, selectedGaps: selectedGaps)

        return hfsPool.compactMap { hfsBefore in
            let hfsProtected = protectedByID[hfsBefore.id] ?? hfsBefore
            let hfsFinal = finalByID[hfsBefore.id] ?? hfsProtected

            let proposals = burstProposalPool.filter { overlaps(hfsBefore, $0) }
            let packetEvents = selectedEventsUsedForPacketization.filter {
                $0.trainID == train.id &&
                    $0.finalLabel.isCanonicalBurstFamily &&
                    overlaps(hfsBefore, $0)
            }
            let finalEvents = finalSelectedEvents.filter { overlaps(hfsBefore, $0) }
            let suppressedProposals = proposals.filter { proposal in
                guard let after = protectedByID[proposal.id] ?? finalByID[proposal.id] else {
                    return false
                }
                return after.suppressedByHFSpikingState == true ||
                    after.gateStatus.lowercased().contains("suppressed")
            }

            let hasConflict = !proposals.isEmpty ||
                !packetEvents.isEmpty ||
                !finalEvents.isEmpty ||
                !suppressedProposals.isEmpty ||
                hfsProtected.hfSpikingBurstPacketLike == true ||
                hfsProtected.hfSpikingBurstDominated == true
            guard hasConflict || settings.includeNonConflictHFS else {
                return nil
            }

            let strongestBurst = strongestCandidate(in: proposals)
            let longBursts = proposals.filter { $0.finalLabel == .longBurst }
            let strongestLongBurst = strongestCandidate(in: longBursts)
            let referenceBurst = strongestCandidate(in: finalEvents) ?? strongestBurst

            let conflictSpan = boundedConflictSpan(
                hfs: hfsBefore,
                eventCandidates: proposals.isEmpty ? finalEvents : proposals
            )
            let bands = burstBands(
                reference: referenceBurst,
                settings: settings
            )
            let burstFractions = seedBridgeFractions(
                train: train,
                span: conflictSpan,
                seedLowerSec: bands.seedLowerSec,
                seedUpperSec: bands.seedUpperSec,
                bridgeUpperSec: bands.bridgeUpperSec
            )

            let pauseThreshold = firstPositive(
                hfsBefore.hfSpikingPauseBreakSec,
                selectedPauseFloor,
                settings.fallbackPauseLikeThresholdSec,
                hfsBefore.hfSpikingToleratedGapSec,
                hfsBefore.hfSpikingPatternMaxISISec
            )
            let pauseStats = pauseLikeStats(
                train: train,
                start: min(hfsBefore.startISIIndex, hfsBefore.endISIIndex),
                end: max(hfsBefore.startISIIndex, hfsBefore.endISIIndex),
                thresholdSec: pauseThreshold
            )

            let packetStats = intervalUnionStats(packetEvents, container: hfsBefore)
            let packetCount = hfsProtected.hfSpikingEmbeddedBurstGroupCount ?? packetStats.groupCount
            let packetCoverage = hfsProtected.hfSpikingEmbeddedBurstCoverage ?? packetStats.coverage
            let burstDominated = hfsProtected.hfSpikingBurstDominated ?? false
            let packetLike = hfsProtected.hfSpikingBurstPacketLike ?? false
            let hfsSelected = hfsFinal.finalLabel == .highFrequencySpiking && hfsFinal.selectedForAuto
            let finalSubtypeValues = finalEvents
                .map(burstSubtype)
                .sorted()
            let selectedLongBurstCount = finalEvents.filter { $0.finalLabel == .longBurst }.count
            let selectedBurstIICount = finalEvents.filter {
                $0.finalLabel == .possibleBurst && $0.arbitrationTrack == .event
            }.count

            let decision = finalDecision(
                hfsSelected: hfsSelected,
                hfsProtected: hfsProtected,
                finalEvents: finalEvents,
                proposalCount: proposals.count,
                suppressedProposalCount: suppressedProposals.count,
                burstDominated: burstDominated
            )
            let requiresReview = reviewRequired(
                decision: decision,
                packetLike: packetLike,
                burstDominated: burstDominated,
                finalEvents: finalEvents,
                suppressedProposalCount: suppressedProposals.count,
                strongestLongBurst: strongestLongBurst
            )
            let reason = decisionReason(
                decision: decision,
                hfsBefore: hfsBefore,
                hfsProtected: hfsProtected,
                hfsFinal: hfsFinal,
                finalEvents: finalEvents,
                suppressedProposals: suppressedProposals,
                packetCount: packetCount,
                packetCoverage: packetCoverage,
                pauseLikeBreakCount: pauseStats.count
            )

            return HFSBurstArbitrationAuditRow(
                id: "\(train.id)::hfs-burst-audit::\(hfsBefore.id)",
                pipelineStage: pipelineStage,
                trainID: train.id,
                trainName: train.name,
                hfsCandidateID: hfsBefore.id,
                hfsRootCandidateID: splitRootID(hfsBefore.id),
                hfsStartISIIndex: min(hfsBefore.startISIIndex, hfsBefore.endISIIndex),
                hfsEndISIIndex: max(hfsBefore.startISIIndex, hfsBefore.endISIIndex),
                conflictStartISIIndex: conflictSpan.start,
                conflictEndISIIndex: conflictSpan.end,
                scoreScaleNote: scoreScaleNote,
                hfsRawScore: hfsBefore.score,
                hfsPriority: hfsBefore.priority,
                hfsSelectedForAuto: hfsSelected,
                hfsSelectionStatus: hfsFinal.selectionStatus,
                hfsAcceptanceRoute: hfsBefore.hfSpikingAcceptanceRoute,
                strongestBurstCandidateID: strongestBurst?.id,
                strongestBurstSubtype: strongestBurst.map(burstSubtype),
                strongestBurstRawScore: strongestBurst?.score,
                strongestBurstPriority: strongestBurst?.priority,
                strongestLongBurstCandidateID: strongestLongBurst?.id,
                longBurstRawScore: strongestLongBurst?.score,
                longBurstPriority: strongestLongBurst?.priority,
                packetEvidenceEventCount: packetEvents.count,
                packetCount: packetCount,
                packetCoverage: packetCoverage,
                allSelectedBurstEventCount: finalEvents.count,
                selectedBurstIICount: selectedBurstIICount,
                selectedLongBurstCount: selectedLongBurstCount,
                suppressedBurstProposalCount: suppressedProposals.count,
                pauseLikeThresholdSec: pauseThreshold,
                pauseLikeBreakCount: pauseStats.count,
                pauseLikeBreakGroupCount: pauseStats.groupCount,
                pauseLikeBreakFraction: pauseStats.fraction,
                seedBandLowerSec: bands.seedLowerSec,
                seedBandUpperSec: bands.seedUpperSec,
                bridgeBandUpperSec: bands.bridgeUpperSec,
                seedFraction: burstFractions.seedFraction,
                bridgeFraction: burstFractions.bridgeFraction,
                hfsShortFraction: hfsBefore.hfSpikingShortFraction,
                hfsBridgeFraction: hfsBefore.hfSpikingBridgeFraction,
                hfsLargeFraction: hfsBefore.hfSpikingLargeFraction,
                hfsCV: hfsBefore.cv,
                hfsLV: hfsBefore.lv,
                burstPacketLike: packetLike,
                burstDominated: burstDominated,
                finalDecision: decision,
                finalSelectedEventSubtypes: finalSubtypeValues,
                requiresReview: requiresReview,
                decisionReason: reason
            )
        }
        .sorted { lhs, rhs in
            if lhs.trainID != rhs.trainID {
                return lhs.trainID < rhs.trainID
            }
            if lhs.hfsStartISIIndex != rhs.hfsStartISIIndex {
                return lhs.hfsStartISIIndex < rhs.hfsStartISIIndex
            }
            if lhs.hfsEndISIIndex != rhs.hfsEndISIIndex {
                return lhs.hfsEndISIIndex < rhs.hfsEndISIIndex
            }
            return lhs.hfsCandidateID < rhs.hfsCandidateID
        }
    }

    public static func csvString(
        rows: [HFSBurstArbitrationAuditRow],
        includeHeader: Bool = true
    ) -> String {
        var lines: [String] = []
        if includeHeader {
            lines.append(HFSBurstArbitrationAuditRow.csvHeader.map(csvEscape).joined(separator: ","))
        }
        lines.append(contentsOf: rows.map { row in
            row.csvFields.map(csvEscape).joined(separator: ",")
        })
        return lines.joined(separator: "\n")
    }

    private static func terminalHFSCandidates(
        _ candidates: [ClassicAnchorCandidate],
        includeSupersededParents: Bool
    ) -> [ClassicAnchorCandidate] {
        let hfs = candidates.filter { $0.finalLabel == .highFrequencySpiking }
        guard !includeSupersededParents else {
            return hfs.sorted(by: candidatePositionOrder)
        }

        let rootsWithChildren = Set(
            hfs
                .filter { $0.id.contains("::state-split::") }
                .map { splitRootID($0.id) }
        )
        return hfs.filter { candidate in
            candidate.id.contains("::state-split::") ||
                !rootsWithChildren.contains(splitRootID(candidate.id))
        }
        .sorted(by: candidatePositionOrder)
    }

    private static func strongestCandidate(
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

    private static func burstSubtype(_ candidate: ClassicAnchorCandidate) -> String {
        switch candidate.finalLabel {
        case .burst:
            return "burst_i"
        case .possibleBurst:
            return candidate.arbitrationTrack == .event ? "burst_ii" : "possible_burst_review"
        case .highFrequencyBurst:
            return "hf_burst"
        case .longBurst:
            return "long_burst"
        default:
            return candidate.finalLabel.rawValue
        }
    }

    private static func boundedConflictSpan(
        hfs: ClassicAnchorCandidate,
        eventCandidates: [ClassicAnchorCandidate]
    ) -> (start: Int, end: Int) {
        let hfsStart = min(hfs.startISIIndex, hfs.endISIIndex)
        let hfsEnd = max(hfs.startISIIndex, hfs.endISIIndex)
        guard !eventCandidates.isEmpty else {
            return (hfsStart, hfsEnd)
        }
        let eventStart = eventCandidates.map { min($0.startISIIndex, $0.endISIIndex) }.min() ?? hfsStart
        let eventEnd = eventCandidates.map { max($0.startISIIndex, $0.endISIIndex) }.max() ?? hfsEnd
        let start = max(hfsStart, eventStart)
        let end = min(hfsEnd, eventEnd)
        return start <= end ? (start, end) : (hfsStart, hfsEnd)
    }

    private static func burstBands(
        reference: ClassicAnchorCandidate?,
        settings: HFSBurstArbitrationAuditSettings
    ) -> (seedLowerSec: Double?, seedUpperSec: Double?, bridgeUpperSec: Double?) {
        let seedLower = firstPositive(
            reference?.burstSeedBandLowerSec,
            reference?.anchorBandLowerSec
        )
        let seedUpper = firstPositive(
            reference?.burstSeedBandUpperSec,
            reference?.hardBurstSeedUpperSec,
            reference?.anchorBandUpperSec,
            settings.fallbackBurstSeedUpperSec
        )
        let bridgeUpperRaw = firstPositive(
            reference?.burstBridgeBandUpperSec,
            reference?.hardBurstBridgeUpperSec,
            settings.fallbackBurstBridgeUpperSec,
            seedUpper
        )
        let bridgeUpper: Double?
        if let seedUpper, let bridgeUpperRaw {
            bridgeUpper = max(seedUpper, bridgeUpperRaw)
        } else {
            bridgeUpper = bridgeUpperRaw
        }
        return (seedLower, seedUpper, bridgeUpper)
    }

    private static func seedBridgeFractions(
        train: SpikeTrain,
        span: (start: Int, end: Int),
        seedLowerSec: Double?,
        seedUpperSec: Double?,
        bridgeUpperSec: Double?
    ) -> (seedFraction: Double?, bridgeFraction: Double?) {
        guard let seedUpperSec, seedUpperSec.isFinite, seedUpperSec > 0 else {
            return (nil, nil)
        }
        let lower = seedLowerSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil } ?? 0
        let bridgeUpper = bridgeUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil } ?? seedUpperSec
        let values = isiValues(train: train, start: span.start, end: span.end).filter {
            $0 >= lower
        }
        guard !values.isEmpty else {
            return (nil, nil)
        }
        let seedCount = values.filter { $0 <= seedUpperSec }.count
        let bridgeCount = values.filter { $0 > seedUpperSec && $0 <= bridgeUpper }.count
        let denominator = Double(values.count)
        return (
            Double(seedCount) / denominator,
            Double(bridgeCount) / denominator
        )
    }

    private static func pauseLikeStats(
        train: SpikeTrain,
        start: Int,
        end: Int,
        thresholdSec: Double?
    ) -> (count: Int, groupCount: Int, fraction: Double?) {
        guard let thresholdSec, thresholdSec.isFinite, thresholdSec > 0 else {
            return (0, 0, nil)
        }
        let indexed = isiIndexedValues(train: train, start: start, end: end)
        guard !indexed.isEmpty else {
            return (0, 0, nil)
        }
        let hits = indexed.filter { $0.value >= thresholdSec }
        var groupCount = 0
        var previousIndex: Int?
        for hit in hits {
            if previousIndex == nil || hit.index != (previousIndex ?? hit.index) + 1 {
                groupCount += 1
            }
            previousIndex = hit.index
        }
        return (
            hits.count,
            groupCount,
            Double(hits.count) / Double(indexed.count)
        )
    }

    private static func pauseFloor(
        train: SpikeTrain,
        selectedGaps: [ClassicAnchorCandidate]
    ) -> Double? {
        selectedGaps.compactMap { gap -> Double? in
            let start = min(gap.startISIIndex, gap.endISIIndex)
            let end = max(gap.startISIIndex, gap.endISIIndex)
            let values = isiValues(train: train, start: start, end: end)
            return values.min() ?? gap.intraQ50Sec
        }.filter { $0.isFinite && $0 > 0 }.min()
    }

    private static func finalDecision(
        hfsSelected: Bool,
        hfsProtected: ClassicAnchorCandidate,
        finalEvents: [ClassicAnchorCandidate],
        proposalCount: Int,
        suppressedProposalCount: Int,
        burstDominated: Bool
    ) -> HFSBurstArbitrationDecision {
        let hasLongBurst = finalEvents.contains { $0.finalLabel == .longBurst }
        if hfsSelected {
            if !finalEvents.isEmpty && suppressedProposalCount > 0 {
                return .hfsSelectedWithMixedBurstOutcomes
            }
            if hasLongBurst {
                return .hfsSelectedWithLongBurstConflict
            }
            if !finalEvents.isEmpty {
                return .hfsSelectedWithBurstConflict
            }
            if suppressedProposalCount > 0 {
                return .hfsSelectedBurstProposalSuppressed
            }
            if proposalCount > 0 {
                return .hfsSelectedWithBurstConflict
            }
            return .hfsSelectedNoBurstWinner
        }

        if burstDominated && !finalEvents.isEmpty {
            return .burstSelectedHFSRejectedPacketDominance
        }
        if hasLongBurst {
            return .longBurstSelectedHFSRejected
        }
        if !finalEvents.isEmpty {
            return .burstSelectedHFSNotSelected
        }
        if hfsProtected.finalLabel == .reject ||
            hfsProtected.gateStatus.lowercased().contains("reject") {
            return .hfsRejectedNoBurstWinner
        }
        return .unresolvedReview
    }

    private static func reviewRequired(
        decision: HFSBurstArbitrationDecision,
        packetLike: Bool,
        burstDominated: Bool,
        finalEvents: [ClassicAnchorCandidate],
        suppressedProposalCount: Int,
        strongestLongBurst: ClassicAnchorCandidate?
    ) -> Bool {
        if packetLike && !burstDominated {
            return true
        }
        if suppressedProposalCount > 0 || strongestLongBurst != nil {
            return true
        }
        if finalEvents.contains(where: { $0.finalLabel == .longBurst }) {
            return true
        }
        switch decision {
        case .hfsSelectedWithMixedBurstOutcomes,
             .hfsSelectedWithLongBurstConflict,
             .hfsSelectedWithBurstConflict,
             .hfsSelectedBurstProposalSuppressed,
             .hfsRejectedNoBurstWinner,
             .unresolvedReview:
            return true
        default:
            return false
        }
    }

    private static func decisionReason(
        decision: HFSBurstArbitrationDecision,
        hfsBefore: ClassicAnchorCandidate,
        hfsProtected: ClassicAnchorCandidate,
        hfsFinal: ClassicAnchorCandidate,
        finalEvents: [ClassicAnchorCandidate],
        suppressedProposals: [ClassicAnchorCandidate],
        packetCount: Int,
        packetCoverage: Double,
        pauseLikeBreakCount: Int
    ) -> String {
        let eventLabels = finalEvents.map(burstSubtype).sorted().joined(separator: "|")
        let suppressedIDs = suppressedProposals.map(\.id).sorted().joined(separator: "|")
        let packetLikeText = hfsProtected.hfSpikingBurstPacketLike.map(String.init) ?? "NA"
        let dominatedText = hfsProtected.hfSpikingBurstDominated.map(String.init) ?? "NA"
        let eventText = eventLabels.isEmpty ? "none" : eventLabels
        let suppressedText = suppressedIDs.isEmpty ? "none" : suppressedIDs
        return [
            "decision=\(decision.rawValue)",
            "hfs_candidate=\(hfsBefore.id)",
            "hfs_final_label=\(hfsFinal.finalLabel.rawValue)",
            "hfs_selected=\(hfsFinal.selectedForAuto)",
            "hfs_selection_status=\(hfsFinal.selectionStatus)",
            "hfs_packet_like=\(packetLikeText)",
            "hfs_burst_dominated=\(dominatedText)",
            "packet_count=\(packetCount)",
            "packet_coverage=\(formatDiagnostic(packetCoverage))",
            "pause_like_break_count=\(pauseLikeBreakCount)",
            "selected_event_subtypes=\(eventText)",
            "suppressed_event_ids=\(suppressedText)"
        ].joined(separator: ";")
    }

    private static func intervalUnionStats(
        _ candidates: [ClassicAnchorCandidate],
        container: ClassicAnchorCandidate
    ) -> (groupCount: Int, coverage: Double) {
        let containerStart = min(container.startISIIndex, container.endISIIndex)
        let containerEnd = max(container.startISIIndex, container.endISIIndex)
        let ranges = candidates.compactMap { candidate -> (start: Int, end: Int)? in
            let start = max(containerStart, min(candidate.startISIIndex, candidate.endISIIndex))
            let end = min(containerEnd, max(candidate.startISIIndex, candidate.endISIIndex))
            return start <= end ? (start, end) : nil
        }.sorted { lhs, rhs in
            lhs.start == rhs.start ? lhs.end < rhs.end : lhs.start < rhs.start
        }
        guard var current = ranges.first else {
            return (0, 0)
        }
        var covered = 0
        var groups = 1
        for range in ranges.dropFirst() {
            if range.start <= current.end + 1 {
                current.end = max(current.end, range.end)
            } else {
                covered += current.end - current.start + 1
                current = range
                groups += 1
            }
        }
        covered += current.end - current.start + 1
        let length = max(1, containerEnd - containerStart + 1)
        return (groups, min(1, Double(covered) / Double(length)))
    }

    private static func isiValues(
        train: SpikeTrain,
        start: Int,
        end: Int
    ) -> [Double] {
        isiIndexedValues(train: train, start: start, end: end).map(\.value)
    }

    private static func isiIndexedValues(
        train: SpikeTrain,
        start: Int,
        end: Int
    ) -> [(index: Int, value: Double)] {
        guard !train.isiSec.isEmpty else {
            return []
        }
        let lower = max(1, min(start, end))
        let upper = min(train.isiSec.count - 1, max(start, end))
        guard lower <= upper else {
            return []
        }
        return (lower...upper).compactMap { index in
            guard let value = train.isiSec[index], value.isFinite, value > 0 else {
                return nil
            }
            return (index, value)
        }
    }

    private static func overlaps(
        _ lhs: ClassicAnchorCandidate,
        _ rhs: ClassicAnchorCandidate
    ) -> Bool {
        min(lhs.startISIIndex, lhs.endISIIndex) <= max(rhs.startISIIndex, rhs.endISIIndex) &&
            max(lhs.startISIIndex, lhs.endISIIndex) >= min(rhs.startISIIndex, rhs.endISIIndex)
    }

    private static func splitRootID(_ id: String) -> String {
        id.components(separatedBy: "::state-split::").first ?? id
    }

    private static func candidatePositionOrder(
        _ lhs: ClassicAnchorCandidate,
        _ rhs: ClassicAnchorCandidate
    ) -> Bool {
        let lhsStart = min(lhs.startISIIndex, lhs.endISIIndex)
        let rhsStart = min(rhs.startISIIndex, rhs.endISIIndex)
        if lhsStart != rhsStart {
            return lhsStart < rhsStart
        }
        let lhsEnd = max(lhs.startISIIndex, lhs.endISIIndex)
        let rhsEnd = max(rhs.startISIIndex, rhs.endISIIndex)
        if lhsEnd != rhsEnd {
            return lhsEnd < rhsEnd
        }
        return lhs.id < rhs.id
    }

    private static func uniqueCandidatesByID(
        _ candidates: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        var latestByID: [String: ClassicAnchorCandidate] = [:]
        var order: [String] = []
        for candidate in candidates {
            if latestByID[candidate.id] == nil {
                order.append(candidate.id)
            }
            latestByID[candidate.id] = candidate
        }
        return order.compactMap { latestByID[$0] }
    }

    private static func firstPositive(_ values: Double?...) -> Double? {
        values.compactMap { value -> Double? in
            guard let value, value.isFinite, value > 0 else {
                return nil
            }
            return value
        }.first
    }

    private static func formatDiagnostic(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.6g", value)
    }

    private static func csvEscape(_ field: String) -> String {
        guard field.contains(",") || field.contains("\"") || field.contains("\n") || field.contains("\r") else {
            return field
        }
        let escaped = field.replacingOccurrences(of: "\"", with: "\"\"")
        return "\"\(escaped)\""
    }
}
