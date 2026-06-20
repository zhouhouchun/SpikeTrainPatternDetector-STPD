import Foundation

public enum ClassicAnchorSemanticTrack: String, CaseIterable, Hashable, Sendable {
    case event
    case state
    case gap
    case review
    case diagnostic
    case profile
}

public struct ClassicAnchorEventGrammarAudit: Hashable, Sendable {
    public let recommendedTrack: ClassicAnchorSemanticTrack
    public let recommendedFamily: String
    public let recommendedSubtype: String
    public let recommendedFinalClass: String
    public let recommendedEventTrackClass: String
    public let reviewStatus: String
    public let reviewRequired: Bool
    public let confidenceTier: String
    public let uncertaintyReason: String
    public let longBurstDefinitionStatus: String
}

public extension ClassicAnchorCandidate {
    var isStructuralPausePriorEvidence: Bool {
        let normalizedAction = action.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let normalizedPath = decisionPath.lowercased()
        return normalizedAction == "audit_only" &&
            (
                candidateLayer == "classic_burst_flank_pause" ||
                    normalizedPath.contains("role=pause_prior_pool_anchor")
            )
    }

    var isStructurallySupportedPossibleBurst: Bool {
        guard finalLabel == .possibleBurst,
              isEligibleForAutoSelection else {
            return false
        }

        let normalizedPath = decisionPath.lowercased()
        if normalizedPath.contains("reject") || normalizedPath.contains("suppressed") {
            return false
        }

        if candidateLayer == "structural_seed_bridge_expansion_burst" {
            return normalizedPath.contains("post_merge_revalidation=") &&
                (
                    normalizedPath.contains("selected_final_label=possible_burst") ||
                        normalizedPath.contains("structural_seed_bridge_expansion_possible_review")
                )
        }

        if decisionPathValue("possible_burst_structural_definition") != nil {
            return true
        }
        if decisionPathValue("endpoint_missing_flank") == "true" {
            return true
        }
        if decisionPathValue("weak_flank_not_absorbable_as_burst_bridge") == "true" {
            return true
        }
        if decisionPathValue("weak_flank_role") == "bridgeable_bridge_candidate" {
            return false
        }

        if normalizedPath.contains("dense_short_isi_episode_rescued_by_train_scale_compression") {
            return true
        }

        let structuralEvidenceClasses: Set<String> = [
            "endpoint_single_flank_possible",
            "endpoint_single_flank_rescue",
            "endpoint_structural_compression_rescue",
            "two_sided_single_flank_possible",
            "two_sided_single_flank_rescue",
            "two_sided_structural_compression_rescue",
            "clean_one_sided_strong_contrast",
            "two_sided_possible_contrast",
            "train_scale_compression_rescue"
        ]
        if let evidenceClass = decisionPathValue("evidence_class")?.lowercased(),
           structuralEvidenceClasses.contains(evidenceClass) {
            return true
        }

        let structuralRoutes: Set<String> = [
            "endpoint",
            "single_flank",
            "clean_one_sided",
            "structural_rescue",
            "rescue",
            "two_sided"
        ]
        if let route = decisionPathValue("possible_burst_review_route")?.lowercased(),
           structuralRoutes.contains(route) {
            return true
        }

        return false
    }

    var isWeakBurstStructuralSeedEvidence: Bool {
        selectedForAuto && isStructurallySupportedPossibleBurst
    }

    var isBurstBoundaryReviewCandidate: Bool {
        guard finalLabel == .possibleBurst else {
            return false
        }

        let normalizedLayer = candidateLayer.lowercased()
        let normalizedAction = action.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let normalizedGate = gateStatus.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let normalizedPath = decisionPath.lowercased()

        if selectedForAuto,
           normalizedLayer == "event_grammar_burst_episode",
           normalizedPath.contains("dense_short_isi_episode_rescued_by_train_scale_compression") {
            return false
        }

        if normalizedAction.contains("review") ||
            normalizedGate.contains("review") ||
            normalizedPath.contains("structural_seed_bridge_expansion_possible_review") {
            return true
        }

        if normalizedLayer == "structural_seed_bridge_expansion_burst",
           normalizedPath.contains("post_merge_revalidation="),
           normalizedPath.contains("selected_final_label=possible_burst") {
            return true
        }

        if normalizedLayer == "event_grammar_burst_episode",
           normalizedGate.contains("burst_family_merge_possible") ||
            normalizedPath.contains("burst_family_merge_bridge_gap_pass") ||
            normalizedPath.contains("post_merge_revalidation=") {
            return true
        }

        return false
    }

    var isBurstBridgeExpansionAnchor: Bool {
        if finalLabel.isCanonicalBurstFamily {
            return selectedForAuto && isEligibleForAutoSelection
        }
        guard isWeakBurstStructuralSeedEvidence else {
            return false
        }
        return candidateLayer != "structural_seed_bridge_expansion_burst"
    }

    var isPossibleBurstPausePoolPriorEvidence: Bool {
        guard isWeakBurstStructuralSeedEvidence else {
            return false
        }
        if decisionPathValue("endpoint_missing_flank") == "true" ||
            decisionPathValue("weak_flank_not_absorbable_as_burst_bridge") == "true" {
            return true
        }
        guard let evidenceClass = decisionPathValue("evidence_class")?.lowercased() else {
            return false
        }
        return evidenceClass.contains("endpoint") ||
            evidenceClass.contains("single_flank") ||
            evidenceClass.contains("clean_one_sided")
    }

    var isClassicTonicStructuralSeedEvidence: Bool {
        guard finalLabel == .tonic,
              selectedForAuto,
              isEligibleForAutoSelection else {
            return false
        }

        let normalizedLayer = candidateLayer.lowercased()
        let normalizedPath = decisionPath.lowercased()
        if normalizedPath.contains("reject") || normalizedPath.contains("suppressed") {
            return false
        }

        let isCoreTonicState =
            normalizedLayer == "event_core_tonic_state" ||
            normalizedLayer == "event_core_tonic_bridge_state" ||
            normalizedLayer.contains("tonic_state")
        guard isCoreTonicState else {
            return false
        }

        return normalizedPath.contains("state_burst_packet_guard=pass")
    }

    var eventGrammarAudit: ClassicAnchorEventGrammarAudit {
        if finalLabel == .profile || candidateLayer.contains("profile") {
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .profile,
                recommendedFamily: "profile",
                recommendedSubtype: "train_isi_profile",
                recommendedFinalClass: finalLabel.rawValue,
                recommendedEventTrackClass: "profile",
                reviewStatus: "audit_context",
                reviewRequired: false,
                confidenceTier: "audit_contextual_profile",
                uncertaintyReason: "",
                longBurstDefinitionStatus: ""
            )
        }

        if suppressedByHFSpikingState == true || gateStatus.lowercased().contains("suppressed") {
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .diagnostic,
                recommendedFamily: "suppressed_candidate",
                recommendedSubtype: suppressedOriginalLabel ?? finalLabel.rawValue,
                recommendedFinalClass: "reject",
                recommendedEventTrackClass: "reject",
                reviewStatus: "suppressed",
                reviewRequired: true,
                confidenceTier: "audit_suppressed_by_hf_spiking",
                uncertaintyReason: failureReason.isEmpty ? "suppressed_by_hf_spiking_state" : failureReason,
                longBurstDefinitionStatus: longBurstStrictnessStatus
            )
        }

        if finalLabel == .reject || action == "reject" || gateStatus.lowercased().contains("reject") {
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .diagnostic,
                recommendedFamily: candidateLayer.contains("burst") ? "burst_event" : "other_or_ambiguous",
                recommendedSubtype: "rejected_candidate",
                recommendedFinalClass: "reject",
                recommendedEventTrackClass: "reject",
                reviewStatus: "rejected",
                reviewRequired: true,
                confidenceTier: "audit_reject",
                uncertaintyReason: failureReason.isEmpty ? decisionPath : failureReason,
                longBurstDefinitionStatus: longBurstStrictnessStatus
            )
        }

        switch finalLabel {
        case .burst:
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .event,
                recommendedFamily: "burst_event",
                recommendedSubtype: burstAuditSubtype(defaultSubtype: "classic_burst"),
                recommendedFinalClass: "burst",
                recommendedEventTrackClass: "burst",
                reviewStatus: selectedForAuto ? "accepted" : "review",
                reviewRequired: !selectedForAuto,
                confidenceTier: anchorLockLevel == .lockedClassic ? "audit_high_confidence_event_like" : "audit_review_event_like",
                uncertaintyReason: selectedForAuto ? burstSelectedEvidenceReason : selectionStatus,
                longBurstDefinitionStatus: ""
            )
        case .highFrequencyBurst:
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .event,
                recommendedFamily: "burst_event",
                recommendedSubtype: burstAuditSubtype(defaultSubtype: "high_frequency_burst"),
                recommendedFinalClass: "high_frequency_burst",
                recommendedEventTrackClass: "high_frequency_burst",
                reviewStatus: selectedForAuto ? "accepted" : "review",
                reviewRequired: !selectedForAuto,
                confidenceTier: anchorLockLevel == .lockedClassic ? "audit_high_confidence_event_like" : "audit_review_event_like",
                uncertaintyReason: selectedForAuto ? burstSelectedEvidenceReason : selectionStatus,
                longBurstDefinitionStatus: ""
            )
        case .longBurst:
            let status = longBurstStrictnessStatus
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .event,
                recommendedFamily: "burst_event",
                recommendedSubtype: burstAuditSubtype(defaultSubtype: "long_burst"),
                recommendedFinalClass: "long_burst",
                recommendedEventTrackClass: "long_burst",
                reviewStatus: (!selectedForAuto || status != "strict_pass") ? "review" : "accepted",
                reviewRequired: !selectedForAuto || status != "strict_pass",
                confidenceTier: status == "strict_pass" ? "audit_high_confidence_structural" : "audit_review_structural_long_burst",
                uncertaintyReason: !selectedForAuto ? selectionStatus : (status == "strict_pass" ? burstSelectedEvidenceReason : status),
                longBurstDefinitionStatus: status
            )
        case .possibleBurst:
            if isBurstBoundaryReviewCandidate {
                return ClassicAnchorEventGrammarAudit(
                    recommendedTrack: .review,
                    recommendedFamily: "burst_review",
                    recommendedSubtype: possibleBurstAuditSubtype,
                    recommendedFinalClass: "possible_burst_review",
                    recommendedEventTrackClass: "none",
                    reviewStatus: "review",
                    reviewRequired: true,
                    confidenceTier: "audit_uncertainty_review",
                    uncertaintyReason: possibleBurstUncertaintyReason,
                    longBurstDefinitionStatus: longBurstStrictnessStatus
                )
            }
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .event,
                recommendedFamily: "burst_event",
                recommendedSubtype: burstAuditSubtype(defaultSubtype: "burst_ii"),
                recommendedFinalClass: "burst_ii",
                recommendedEventTrackClass: "burst",
                reviewStatus: selectedForAuto ? "accepted" : "auto_candidate",
                reviewRequired: false,
                confidenceTier: selectedForAuto ? "audit_burst_ii_auto_event" : "audit_burst_ii_candidate_event",
                uncertaintyReason: selectedForAuto ? burstSelectedEvidenceReason : possibleBurstUncertaintyReason,
                longBurstDefinitionStatus: longBurstStrictnessStatus
            )
        case .pause:
            if isStructuralPausePriorEvidence {
                return ClassicAnchorEventGrammarAudit(
                    recommendedTrack: .diagnostic,
                    recommendedFamily: "pause_evidence",
                    recommendedSubtype: "classic_burst_flank_pause_prior",
                    recommendedFinalClass: "pause_prior",
                    recommendedEventTrackClass: "none",
                    reviewStatus: "audit_context",
                    reviewRequired: false,
                    confidenceTier: "audit_structural_pause_prior_pool",
                    uncertaintyReason: "evidence_only__pause_pool_anchor_from_selected_classic_burst_flank",
                    longBurstDefinitionStatus: ""
                )
            } else if candidateLayer == "classic_burst_flank_pause" {
                return ClassicAnchorEventGrammarAudit(
                    recommendedTrack: .gap,
                    recommendedFamily: "pause_gap",
                    recommendedSubtype: classicBurstFlankPauseSubtype,
                    recommendedFinalClass: "pause",
                    recommendedEventTrackClass: "none",
                    reviewStatus: selectedForAuto ? "accepted" : "review",
                    reviewRequired: !selectedForAuto,
                    confidenceTier: anchorLockLevel == .lockedClassic ? "audit_high_confidence_structural_pause" : "audit_review_structural_pause",
                    uncertaintyReason: selectedForAuto ? "selected_classic_burst_flank_pause_gap" : selectionStatus,
                    longBurstDefinitionStatus: ""
                )
            }
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .gap,
                recommendedFamily: "pause_gap",
                recommendedSubtype: "pause",
                recommendedFinalClass: "pause",
                recommendedEventTrackClass: "none",
                reviewStatus: selectedForAuto ? "accepted" : "review",
                reviewRequired: !selectedForAuto,
                confidenceTier: anchorLockLevel == .lockedClassic ? "audit_high_confidence" : "audit_review",
                uncertaintyReason: selectedForAuto ? "" : selectionStatus,
                longBurstDefinitionStatus: ""
            )
        case .tonic:
            return stateAudit(subtype: "classic_tonic", finalClass: "tonic")
        case .highFrequencyTonic:
            return stateAudit(subtype: "high_frequency_tonic", finalClass: "high_frequency_tonic")
        case .highFrequencySpiking:
            return stateAudit(subtype: "high_frequency_spiking", finalClass: "high_frequency_spiking")
        case .reject, .profile:
            return ClassicAnchorEventGrammarAudit(
                recommendedTrack: .diagnostic,
                recommendedFamily: "other_or_ambiguous",
                recommendedSubtype: finalLabel.rawValue,
                recommendedFinalClass: finalLabel.rawValue,
                recommendedEventTrackClass: finalLabel.rawValue,
                reviewStatus: "review",
                reviewRequired: true,
                confidenceTier: "audit_contextual",
                uncertaintyReason: failureReason,
                longBurstDefinitionStatus: ""
            )
        }
    }

    var auditRecommendedTrack: ClassicAnchorSemanticTrack { eventGrammarAudit.recommendedTrack }
    var auditRecommendedTrackRawValue: String { eventGrammarAudit.recommendedTrack.rawValue }
    var auditRecommendedFamily: String { eventGrammarAudit.recommendedFamily }
    var auditRecommendedSubtype: String { eventGrammarAudit.recommendedSubtype }
    var auditRecommendedFinalClass: String { eventGrammarAudit.recommendedFinalClass }
    var auditRecommendedEventTrackClass: String { eventGrammarAudit.recommendedEventTrackClass }
    var auditReviewStatus: String { eventGrammarAudit.reviewStatus }
    var auditReviewRequired: Bool { eventGrammarAudit.reviewRequired }
    var auditConfidenceTier: String { eventGrammarAudit.confidenceTier }
    var auditUncertaintyReason: String { eventGrammarAudit.uncertaintyReason }
    var auditLongBurstDefinitionStatus: String { eventGrammarAudit.longBurstDefinitionStatus }

    var possibleBurstStructureSummary: String {
        guard finalLabel == .possibleBurst else {
            return ""
        }
        return possibleBurstStructureSummaryText
    }

    var reviewEvidenceClass: String {
        guard finalLabel == .possibleBurst else {
            return ""
        }
        return possibleBurstReviewEvidenceClassText
    }

    var reviewEvidenceStrength: String {
        guard finalLabel == .possibleBurst else {
            return ""
        }
        return possibleBurstReviewEvidenceStrengthText
    }

    var reviewEvidenceSummary: String {
        guard finalLabel == .possibleBurst else {
            return ""
        }
        return possibleBurstReviewEvidenceSummaryText
    }
}

private extension ClassicAnchorCandidate {
    func burstAuditSubtype(defaultSubtype: String) -> String {
        if candidateLayer == "structural_seed_bridge_expansion_burst" {
            if decisionPath.contains("post_merge_revalidation=") {
                return "\(defaultSubtype)_bridge_expansion_post_merge_validated"
            }
            return "\(defaultSubtype)_bridge_expansion"
        }
        if candidateLayer.hasPrefix("isi_profile_hard_threshold") {
            return "\(defaultSubtype)_hard_threshold_profile"
        }
        if candidateLayer == "event_grammar_burst_episode" {
            return "\(defaultSubtype)_episode_merge"
        }
        return defaultSubtype
    }

    var classicBurstFlankPauseSubtype: String {
        if decisionPath.contains("source_burst_layer=structural_seed_bridge_expansion_burst") {
            return "structural_bridge_burst_flank_pause"
        }
        return "classic_burst_flank_pause"
    }

    var burstSelectedEvidenceReason: String {
        if candidateLayer == "structural_seed_bridge_expansion_burst" {
            return "selected_structural_bridge_expansion_after_post_merge_revalidation"
        }
        if candidateLayer.hasPrefix("isi_profile_hard_threshold") {
            return "selected_hard_threshold_profile_candidate"
        }
        if candidateLayer == "event_grammar_burst_episode" {
            return "selected_event_grammar_episode_merge"
        }
        return ""
    }

    func stateAudit(subtype: String, finalClass: String) -> ClassicAnchorEventGrammarAudit {
        let mediumEventness = anchorLockLevel != .lockedClassic || !selectedForAuto
        return ClassicAnchorEventGrammarAudit(
            recommendedTrack: .state,
            recommendedFamily: "state_epoch",
            recommendedSubtype: subtype,
            recommendedFinalClass: finalClass,
            recommendedEventTrackClass: "none",
            reviewStatus: mediumEventness ? "review" : "accepted",
            reviewRequired: mediumEventness,
            confidenceTier: mediumEventness ? "audit_review_state" : "audit_high_confidence_state",
            uncertaintyReason: mediumEventness ? stateReviewReason : stateAcceptedEvidenceReason(subtype: subtype),
            longBurstDefinitionStatus: ""
        )
    }

    var stateReviewReason: String {
        if !selectionStatus.isEmpty, selectionStatus != "not_selected" {
            return selectionStatus
        }
        return [
            "state_candidate_review",
            "cv=\(formatAuditNumber(cv))",
            "cv2=\(formatAuditNumber(cv2))",
            "lv=\(formatAuditNumber(lv))",
            "mm=\(formatAuditNumber(mm))",
            "state_regularity=\(formatAuditNumber(stateRegularityScore))",
            "local_stability=\(formatAuditNumber(stateLocalStabilityScore))"
        ].joined(separator: ";")
    }

    func stateAcceptedEvidenceReason(subtype: String) -> String {
        var parts = [
            "selected_\(subtype)_state_epoch",
            "cv=\(formatAuditNumber(cv))",
            "cv2=\(formatAuditNumber(cv2))",
            "lv=\(formatAuditNumber(lv))",
            "mm=\(formatAuditNumber(mm))",
            "state_regularity=\(formatAuditNumber(stateRegularityScore))",
            "local_stability=\(formatAuditNumber(stateLocalStabilityScore))",
            "train_percentile_median=\(formatAuditNumber(stateTrainPercentileMedian))",
            "local_percentile_median=\(formatAuditNumber(stateLocalPercentileMedian))"
        ]
        if let stateBurstSeedFraction {
            parts.append("burst_seed_fraction=\(formatAuditNumber(stateBurstSeedFraction))")
        }
        if let stateLowTailFraction {
            parts.append("low_tail_fraction=\(formatAuditNumber(stateLowTailFraction))")
        }
        if let stateCoreBurstRunLength {
            parts.append("core_burst_run_length=\(stateCoreBurstRunLength)")
        }
        if finalLabel == .highFrequencySpiking {
            parts.append("hf_short_fraction=\(formatAuditNumber(hfSpikingShortFraction))")
            parts.append("hf_q90_short_fraction=\(formatAuditNumber(hfSpikingQ90ShortFraction))")
            parts.append("hf_bridge_fraction=\(formatAuditNumber(hfSpikingBridgeFraction))")
            parts.append("hf_large_fraction=\(formatAuditNumber(hfSpikingLargeFraction))")
            parts.append("hf_tolerated_fraction=\(formatAuditNumber(hfSpikingToleratedFraction))")
            parts.append("hf_packet_groups=\(hfSpikingEmbeddedBurstGroupCount.map { String($0) } ?? "NA")")
            parts.append("hf_packet_coverage=\(formatAuditNumber(hfSpikingEmbeddedBurstCoverage))")
            parts.append("hf_packet_like=\(hfSpikingBurstPacketLike.map { String($0) } ?? "NA")")
            parts.append("hf_burst_dominated=\(hfSpikingBurstDominated.map { String($0) } ?? "NA")")
            if let hfSpikingMaxConsecutiveLargeISI {
                parts.append("hf_max_consecutive_large_isi=\(hfSpikingMaxConsecutiveLargeISI)")
            }
            if let hfSpikingAcceptanceRoute {
                parts.append("hf_acceptance_route=\(hfSpikingAcceptanceRoute)")
            }
        }
        return parts.joined(separator: ";")
    }

    var possibleBurstUncertaintyReason: String {
        if !failureReason.isEmpty {
            return failureReason
        }
        if candidateLayer == "structural_seed_bridge_expansion_burst" {
            if decisionPath.contains("post_merge_revalidation=") {
                return "bridge_expansion_possible_burst_review__post_merge_possible_pass_but_strict_revalidation_failed"
            }
            return "bridge_expansion_possible_burst_review"
        }
        if decisionPath.contains("evidence_class=endpoint_single_flank_possible") ||
            decisionPath.contains("evidence_class=endpoint_single_flank_rescue") {
            return "endpoint_single_flank_possible_burst_review__missing_one_boundary"
        }
        if decisionPath.contains("evidence_class=endpoint_structural_compression_rescue") {
            return "endpoint_structural_compression_possible_burst_review__missing_boundary_but_local_compression_supports_cluster"
        }
        if decisionPath.contains("evidence_class=two_sided_single_flank_possible") ||
            decisionPath.contains("evidence_class=two_sided_single_flank_rescue") {
            return "two_sided_single_flank_possible_burst_review__opposite_boundary_not_classic"
        }
        if decisionPath.contains("evidence_class=two_sided_structural_compression_rescue") {
            return "two_sided_structural_compression_possible_burst_review__local_compression_supports_cluster"
        }
        if decisionPath.contains("evidence_class=clean_one_sided_strong_contrast") {
            return "clean_one_sided_possible_burst_review__core_compact_but_only_one_boundary_locks"
        }
        if decisionPath.contains("evidence_class=two_sided_possible_contrast") {
            return "two_sided_possible_burst_review__both_boundaries_pass_possible_not_classic"
        }
        if decisionPath.contains("evidence_class=train_scale_compression_rescue") {
            return "train_scale_compression_possible_burst_review__local_background_supports_compact_cluster"
        }
        if candidateLayer == "event_grammar_one_sided_possible_burst_rescue" ||
            gateStatus.contains("one_sided_possible_burst_rescue") {
            if decisionPath.contains("endpoint_missing_flank=true") {
                return "endpoint_compact_burst_candidate_review"
            }
            if decisionPath.contains("weak_flank_not_absorbable_as_burst_bridge=true") {
                return "single_flank_compact_burst_candidate_with_nonbridgeable_weak_boundary_review"
            }
            return "one_sided_compact_burst_candidate_review"
        }
        if let size = burstSizeLabelBeforeReview, size == ClassicAnchorLabel.longBurst.rawValue {
            return longBurstStrictnessStatus
        }
        if gateStatus.contains("boundary") {
            return "boundary_single_flank_possible_burst_review"
        }
        if gateStatus.contains("clean_one_sided") {
            return "clean_one_sided_flank_possible_burst_review"
        }
        if gateStatus.contains("one_sided") {
            return "one_sided_flank_contrast_review"
        }
        if gateStatus.contains("possible") {
            return "burst_family_subtype_unresolved"
        }
        if !selectedForAuto {
            return selectionStatus
        }
        return "burst_family_review"
    }

    var possibleBurstStructureSummaryText: String {
        let route = decisionPathValue("possible_burst_review_route")
        let evidence = decisionPathValue("evidence_class")
        let weakRole = decisionPathValue("weak_flank_role")
        let dominantFlank = decisionPathValue("dominant_flank_side")
        let endpointMissing = decisionPathValue("endpoint_missing_flank") == "true"
        let weakBridgeable = decisionPathValue("weak_flank_bridgeable")
        let q90Pass = decisionPathValue("q90_bridge_pass")
        let structuralCompression = decisionPathValue("structural_compression_pass")

        var parts: [String] = []
        if let route {
            parts.append("route \(route.readableAuditToken)")
        } else if let evidence {
            parts.append(evidence.readableAuditToken)
        } else {
            parts.append("possible burst review")
        }
        if endpointMissing {
            parts.append("endpoint missing flank")
        }
        if let dominantFlank, dominantFlank != "none" {
            parts.append("dominant \(dominantFlank) flank")
        }
        if let weakRole {
            parts.append("weak \(weakRole.readableAuditToken)")
        } else if let weakBridgeable {
            parts.append("weak flank bridgeable \(weakBridgeable)")
        }
        if let q90Pass {
            parts.append("q90 bridge \(q90Pass)")
        }
        if let structuralCompression {
            parts.append("compression \(structuralCompression)")
        }
        return parts.joined(separator: "; ")
    }

    var possibleBurstReviewEvidenceClassText: String {
        if let evidenceClass = decisionPathValue("evidence_class"),
           !evidenceClass.isEmpty {
            return evidenceClass
        }
        if candidateLayer == "structural_seed_bridge_expansion_burst" {
            if decisionPath.contains("post_merge_revalidation=") {
                return "bridge_expansion_post_merge_review"
            }
            return "bridge_expansion_review"
        }
        if decisionPathValue("endpoint_missing_flank") == "true" {
            return "endpoint_missing_flank"
        }
        if decisionPathValue("weak_flank_not_absorbable_as_burst_bridge") == "true" {
            return "single_flank_nonbridgeable_boundary"
        }
        if let route = decisionPathValue("possible_burst_review_route"),
           !route.isEmpty {
            return "route_\(route)"
        }
        if gateStatus.contains("clean_one_sided") {
            return "clean_one_sided"
        }
        if gateStatus.contains("one_sided") {
            return "one_sided"
        }
        if gateStatus.contains("boundary") {
            return "boundary_single_flank"
        }
        return possibleBurstAuditSubtype
    }

    var possibleBurstReviewEvidenceStrengthText: String {
        guard isStructurallySupportedPossibleBurst else {
            return "weak_review_evidence"
        }

        let evidenceClass = possibleBurstReviewEvidenceClassText.lowercased()
        if evidenceClass.contains("clean_one_sided_strong_contrast") ||
            evidenceClass.contains("two_sided_possible_contrast") {
            return "strong_review_evidence"
        }
        if evidenceClass.contains("endpoint") ||
            evidenceClass.contains("single_flank") ||
            evidenceClass.contains("compression") ||
            evidenceClass.contains("bridge_expansion") ||
            evidenceClass.contains("train_scale") ||
            evidenceClass.contains("one_sided") ||
            evidenceClass.contains("boundary") {
            return "moderate_review_evidence"
        }
        return "weak_review_evidence"
    }

    var possibleBurstReviewEvidenceSummaryText: String {
        let strength = possibleBurstReviewEvidenceStrengthText.readableAuditToken
        let evidenceClass = possibleBurstReviewEvidenceClassText.readableAuditToken
        let structure = possibleBurstStructureSummaryText
        if structure.isEmpty {
            return "\(strength); \(evidenceClass)"
        }
        return "\(strength); \(evidenceClass); \(structure)"
    }

    var possibleBurstAuditSubtype: String {
        if decisionPath.contains("evidence_class=endpoint_single_flank_possible") {
            return "possible_burst_endpoint_single_flank"
        }
        if decisionPath.contains("evidence_class=endpoint_single_flank_rescue") {
            return "possible_burst_endpoint_single_flank_rescue"
        }
        if decisionPath.contains("evidence_class=endpoint_structural_compression_rescue") {
            return "possible_burst_endpoint_structural_compression_rescue"
        }
        if decisionPath.contains("evidence_class=two_sided_single_flank_possible") {
            return "possible_burst_two_sided_single_flank"
        }
        if decisionPath.contains("evidence_class=two_sided_single_flank_rescue") {
            return "possible_burst_two_sided_single_flank_rescue"
        }
        if decisionPath.contains("evidence_class=two_sided_structural_compression_rescue") {
            return "possible_burst_two_sided_structural_compression_rescue"
        }
        if decisionPath.contains("evidence_class=clean_one_sided_strong_contrast") {
            return "possible_burst_clean_one_sided_strong_contrast"
        }
        if decisionPath.contains("evidence_class=two_sided_possible_contrast") {
            return "possible_burst_two_sided_possible_contrast"
        }
        if decisionPath.contains("evidence_class=train_scale_compression_rescue") {
            return "possible_burst_train_scale_compression_rescue"
        }
        if candidateLayer == "event_grammar_one_sided_possible_burst_rescue" ||
            gateStatus.contains("one_sided_possible_burst_rescue") {
            if decisionPath.contains("endpoint_missing_flank=true") {
                return "possible_burst_endpoint_rescue"
            }
            if decisionPath.contains("weak_flank_not_absorbable_as_burst_bridge=true") {
                return "possible_burst_single_flank_nonbridgeable_boundary_rescue"
            }
            return "possible_burst_one_sided_rescue"
        }
        if candidateLayer == "event_grammar_burst_episode" {
            if candidateClass == "event_grammar_burst_family_merge" ||
                gateStatus.contains("burst_family_merge") {
                return "possible_burst_episode_merge_review"
            }
            return "possible_burst_episode_review"
        }
        if let size = burstSizeLabelBeforeReview, size == ClassicAnchorLabel.longBurst.rawValue {
            return "possible_long_burst_review"
        }
        if gateStatus.contains("boundary") {
            return "possible_burst_boundary_single_flank"
        }
        if gateStatus.contains("clean_one_sided") {
            return "possible_burst_clean_one_sided"
        }
        if gateStatus.contains("one_sided") {
            return "possible_burst_one_sided"
        }
        if candidateLayer == "structural_seed_bridge_expansion_burst" {
            if decisionPath.contains("post_merge_revalidation=") {
                return "possible_burst_bridge_expansion_post_merge_review"
            }
            return "possible_burst_bridge_expansion_review"
        }
        return "possible_burst"
    }

    var longBurstStrictnessStatus: String {
        guard finalLabel == .longBurst ||
            burstSizeLabelBeforeReview == ClassicAnchorLabel.longBurst.rawValue ||
            nSpikes >= 10 else {
            return ""
        }

        var weak: [String] = []
        if let contrast = edgeContrastMinQ90, contrast < 3.5 {
            weak.append("context_weak")
        }
        if let bridgePass = burstBridgeFractionPass, !bridgePass {
            weak.append("short_fraction_weak")
        }
        if let q90Pass = burstQ90BridgePass, !q90Pass {
            weak.append("internal_outlier_excess")
        }
        if nSpikes >= 17 {
            weak.append("duration_or_extent_too_long")
        }

        return weak.isEmpty ? "strict_pass" : weak.joined(separator: "__")
    }

    func formatAuditNumber(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.5g", locale: Locale(identifier: "en_US_POSIX"), value)
    }

    func decisionPathValue(_ key: String) -> String? {
        let prefix = "\(key)="
        return decisionPath
            .split(separator: ";")
            .map { String($0).trimmingCharacters(in: .whitespacesAndNewlines) }
            .first { $0.hasPrefix(prefix) }
            .map { String($0.dropFirst(prefix.count)) }
    }
}

private extension String {
    var readableAuditToken: String {
        replacingOccurrences(of: "_", with: " ")
    }
}
