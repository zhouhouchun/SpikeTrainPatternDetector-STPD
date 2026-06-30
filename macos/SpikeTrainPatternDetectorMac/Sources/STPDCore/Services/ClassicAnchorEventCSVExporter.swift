import Foundation

public enum ClassicAnchorEventCSVExporter {
    public static func csv(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        annotations: [ClassicAnchorEventAnnotation],
        reviewStatuses: [String: String],
        includeUnselectedCandidates: Bool = false,
        includeEvidenceOnlyCandidates: Bool = false,
        exportedAt: Date = Date()
    ) -> String {
        let annotationsByCandidateID = annotations.reduce(into: [String: ClassicAnchorEventAnnotation]()) { result, annotation in
            result[annotation.candidateID] = result[annotation.candidateID] ?? annotation
        }
        let resolutionsByTrainID = Dictionary(uniqueKeysWithValues: run.resolutions.map { ($0.trainID, $0) })
        // Phase 2B: per-candidate diagnostic ISI temporal-profile evidence (read-only; never gates).
        let isiEvidenceByCandidateID = ISITemporalProfileEvidenceBuilder.evidenceByCandidateID(run: run, dataset: dataset)
        // Phase 2C: per-candidate eventness audit (read-only; never gates).
        let eventnessByCandidateID = ISICandidateEventnessAuditor.auditByCandidateID(run: run, dataset: dataset)
        // Phase 2D: per-candidate near-miss audit of existing candidates (read-only; never gates).
        let nearMissByCandidateID = ISINearMissAuditor.auditByCandidateID(run: run, dataset: dataset)
        let exportedAtText = ISO8601DateFormatter().string(from: exportedAt)
        let exportCandidates = run.candidates.filter { candidate in
            guard includeEvidenceOnlyCandidates || !candidate.isStructuralPausePriorEvidence else {
                return false
            }
            guard includeUnselectedCandidates || candidate.selectedForAuto else {
                return false
            }
            guard includeUnselectedCandidates || annotationsByCandidateID[candidate.id] != nil else {
                return false
            }
            guard includeUnselectedCandidates || normalizedReviewStatus(reviewStatuses[candidate.id]) != "rejected" else {
                return false
            }
            return includeUnselectedCandidates || isPrimaryExportTrack(candidate.auditRecommendedTrack)
        }

        let rows = [headers] + sortedCandidates(exportCandidates).map { candidate in
            row(
                candidate: candidate,
                annotation: annotationsByCandidateID[candidate.id],
                resolution: resolutionsByTrainID[candidate.trainID],
                datasetSummary: run.datasetStructuralSeedSummary,
                dataset: dataset,
                reviewStatus: reviewStatuses[candidate.id] ?? "unreviewed",
                isiEvidence: isiEvidenceByCandidateID[candidate.id] ?? .undefined(),
                eventness: eventnessByCandidateID[candidate.id] ?? .undefined(),
                nearMiss: nearMissByCandidateID[candidate.id] ?? .ineligible(candidateRef: "candidate:\(candidate.id)"),
                exportedAtText: exportedAtText
            )
        }

        return rows.map { $0.map(csvEscaped).joined(separator: ",") }.joined(separator: "\n") + "\n"
    }

    private static func isPrimaryExportTrack(_ track: ClassicAnchorSemanticTrack) -> Bool {
        switch track {
        case .event, .gap, .state, .review:
            return true
        case .diagnostic, .profile:
            return false
        }
    }

    private static func normalizedReviewStatus(_ status: String?) -> String {
        status?.trimmingCharacters(in: .whitespacesAndNewlines).lowercased() ?? ""
    }

    private static let headers = [
        "exported_at",
        "dataset_name",
        "source",
        "candidate_id",
        "train_id",
        "train_name",
        "review_status",
        "label",
        "lock_level",
        "selected_for_auto",
        "selection_status",
        "gate_status",
        "action",
        "candidate_layer",
        "candidate_class",
        "candidate_diagnostic_class",
        "failure_reason",
        "recommended_track",
        "recommended_family",
        "recommended_subtype",
        "recommended_final_class",
        "recommended_event_track_class",
        "audit_review_status",
        "recommended_review_required",
        "recommendation_confidence_tier",
        "recommended_uncertainty_reason",
        "long_burst_definition_status",
        "review_evidence_class",
        "review_evidence_strength",
        "review_evidence_summary",
        "anchor_family",
        "raw_start_sec",
        "raw_end_sec",
        "aligned_start_sec",
        "aligned_end_sec",
        "event_duration_sec",
        "start_spike_index",
        "end_spike_index",
        "n_spikes",
        "start_isi_index",
        "end_isi_index",
        "n_isi",
        "n_valid_isi",
        "anchor_band_lower_sec",
        "anchor_band_upper_sec",
        "anchor_band_source",
        "adaptive_valid_isi_count",
        "adaptive_seed_lower_sec",
        "adaptive_seed_upper_sec",
        "adaptive_bridge_upper_sec",
        "adaptive_contrast_s",
        "structural_seed_source",
        "structural_burst_anchor_count",
        "structural_burst_support_weight",
        "structural_burst_seed_upper_sec",
        "structural_burst_bridge_upper_sec",
        "structural_tonic_anchor_count",
        "structural_tonic_support_weight",
        "structural_tonic_seed_lower_sec",
        "structural_tonic_seed_upper_sec",
        "structural_pause_anchor_count",
        "structural_pause_pool_anchor_count",
        "structural_pause_support_weight",
        "structural_pause_pool_support_weight",
        "structural_pause_pool_source",
        "structural_pause_seed_lower_sec",
        "structural_pause_seed_upper_sec",
        "dataset_structural_seed_source",
        "dataset_structural_train_count",
        "dataset_structural_seeded_train_count",
        "dataset_structural_burst_anchor_count",
        "dataset_structural_burst_support_weight",
        "dataset_structural_burst_seed_upper_sec",
        "dataset_structural_burst_bridge_upper_sec",
        "dataset_structural_tonic_anchor_count",
        "dataset_structural_tonic_support_weight",
        "dataset_structural_tonic_seed_lower_sec",
        "dataset_structural_tonic_seed_upper_sec",
        "dataset_structural_pause_anchor_count",
        "dataset_structural_pause_pool_anchor_count",
        "dataset_structural_pause_support_weight",
        "dataset_structural_pause_pool_support_weight",
        "dataset_structural_pause_pool_source",
        "dataset_structural_pause_seed_lower_sec",
        "dataset_structural_pause_seed_upper_sec",
        "profile_seed_low_percentile_in_train",
        "profile_seed_high_percentile_in_train",
        "profile_seed_band_fraction",
        "profile_seed_run_count",
        "profile_max_seed_run_length",
        "profile_median_isi_sec",
        "q10_ISI_sec",
        "q25_ISI_sec",
        "q90_ISI_sec",
        "profile_pause_fraction",
        "profile_phenotype_prior",
        "profile_bridge_upper_sec",
        "profile_boundary_floor_sec",
        "event_core_seed_low_sec",
        "event_core_seed_high_sec",
        "event_core_bridge_high_sec",
        "event_core_boundary_floor_sec",
        "event_core_boundary_floor_hard",
        "burst_contrast_S",
        "possible_contrast_S",
        "local_background_q75_sec",
        "local_compression_q90_ratio",
        "event_local_median_sec",
        "event_local_percentile_median",
        "event_local_percentile_q90",
        "event_local_robust_z_median",
        "event_local_robust_z_abs_q80",
        "event_local_robust_z_q10",
        "state_regularity_score",
        "state_burst_seed_fraction",
        "state_low_tail_fraction",
        "state_local_stability_score",
        "state_core_burst_run_length",
        "state_tonic_subtype",
        "state_train_percentile_median",
        "state_local_percentile_median",
        "state_local_percentile_q90",
        "state_local_robust_z_median",
        "state_local_robust_z_abs_q80",
        "state_local_robust_z_q10",
        "hf_spiking_q80_sec",
        "hf_spiking_q80_max_sec",
        "hf_spiking_q90_max_sec",
        "hf_spiking_short_upper_sec",
        "hf_spiking_epoch_bridge_sec",
        "hf_spiking_tolerated_gap_sec",
        "hf_spiking_pattern_max_ISI_sec",
        "hf_spiking_pause_break_sec",
        "hf_spiking_short_fraction",
        "hf_spiking_q90_short_fraction",
        "hf_spiking_bridge_fraction",
        "hf_spiking_large_fraction",
        "hf_spiking_tolerated_fraction",
        "hf_spiking_max_consecutive_large_isi",
        "hf_spiking_min_spikes_required",
        "hf_spiking_acceptance_route",
        "hf_spiking_embedded_burst_count",
        "hf_spiking_embedded_burst_group_count",
        "hf_spiking_embedded_burst_coverage",
        "hf_spiking_burst_dominated",
        "hf_spiking_burst_packet_like",
        "hf_spiking_burst_packet_neighbor",
        "suppressed_by_hf_spiking_state",
        "suppressed_original_label",
        "hf_spiking_suppressor_id",
        "threshold_mode",
        "hard_threshold",
        "hard_threshold_pattern",
        "hard_burst_seed_upper_sec",
        "hard_burst_bridge_upper_sec",
        "hard_burst_core_isi_count",
        "hard_threshold_source",
        "burst_seed_run_start_isi",
        "burst_seed_run_end_isi",
        "seed_band_lower_sec",
        "seed_band_upper_sec",
        "bridge_band_upper_sec",
        "burst_contrast_required",
        "possible_contrast_required",
        "required_gap_sec",
        "possible_required_gap_sec",
        "boundary_floor_sec",
        "boundary_floor_hard",
        "strict_boundary_pass",
        "possible_boundary_pass",
        "bridge_count_pass",
        "bridge_fraction_pass",
        "q90_bridge_pass",
        "size_label_before_review",
        "score",
        "priority",
        "intra_q10_sec",
        "intra_q40_sec",
        "intra_q50_sec",
        "intra_q90_sec",
        "intra_q95_sec",
        "max_intra_isi_sec",
        "mean_intra_isi_sec",
        "cv",
        "cv2",
        "lv",
        "pre_gap_sec",
        "post_gap_sec",
        "pre_ratio_q90",
        "post_ratio_q90",
        "edge_contrast_min_q90",
        "edge_contrast_geom_q90",
        "contrast_min_required",
        "contrast_geom_required",
        "refractory_suspect_count",
        "refractory_suspect_action",
        "pipeline_stages",
        "decision_path",
        // Additive, audit-only HF-family subtype (one of hf_tonic_spiking, hf_irregular_spiking,
        // hf_burst_dominant, hf_burst_packet; empty when not in the HF family). Appended at the
        // end so no existing column position shifts; finalLabel and selection are unchanged.
        "state_high_frequency_subtype",
        // Phase 2B: diagnostic ISI temporal-profile evidence (ISITemporalProfileEvidence). Evidence
        // only — these never participate in detection. Appended last so no existing column shifts.
        "isi_edge_contrast_min",
        "isi_edge_contrast_geom",
        "isi_pre_edge_ratio",
        "isi_post_edge_ratio",
        "isi_flank_count",
        "isi_core_q_pct",
        "isi_percentile_reliable",
        "isi_local_median_sec",
        "isi_local_compression_ratio",
        // Phase 2C: candidate eventness audit (ISICandidateEventnessAudit). Audit only — never gates
        // detection. Appended after the Phase 2B isi_* block so no existing column shifts.
        "eventness_q10_isi_sec",
        "eventness_q50_isi_sec",
        "eventness_q90_isi_sec",
        "eventness_q90_q10_ratio",
        "eventness_distant_context_median_sec",
        "eventness_context_contrast",
        "eventness_return_to_baseline_score",
        "eventness_edge_component",
        "eventness_context_component",
        "eventness_score",
        "eventness_regularity_score",
        "eventness_zone",
        "eventness_medium_review",
        "eventness_audit_recommendation",
        "eventness_audit_note",
        // Phase 2D: near-miss review of existing candidates (ISINearMissAudit). Audit only — never
        // gates detection. Appended after the Phase 2C eventness_* block so no existing column shifts.
        "near_miss_eligible",
        "near_miss_is_near_miss",
        "near_miss_category",
        "near_miss_parameter",
        "near_miss_direction",
        "near_miss_current_value",
        "near_miss_required_value",
        "near_miss_absolute_change",
        "near_miss_relative_change",
        "near_miss_failure_count",
        "near_miss_score",
        "near_miss_eventness_score",
        "near_miss_eventness_zone",
        "near_miss_candidate_ref",
        "near_miss_reason",
        "near_miss_details"
    ]

    private static func row(
        candidate: ClassicAnchorCandidate,
        annotation: ClassicAnchorEventAnnotation?,
        resolution: TrainAdaptiveBandResolution?,
        datasetSummary: StructuralDatasetSeedSummary,
        dataset: SpikeDataset,
        reviewStatus: String,
        isiEvidence: ISITemporalProfileEvidence,
        eventness: ISICandidateEventnessAudit,
        nearMiss: ISINearMissAudit,
        exportedAtText: String
    ) -> [String] {
        let candidateBand = resolution?.band(for: adaptiveBandPattern(for: candidate.finalLabel))

        return [
            exportedAtText,
            dataset.name,
            dataset.sourceDescription,
            candidate.id,
            candidate.trainID,
            candidate.trainName,
            reviewStatus,
            candidate.finalLabel.rawValue,
            candidate.anchorLockLevel.rawValue,
            bool(candidate.selectedForAuto),
            candidate.selectionStatus,
            candidate.gateStatus,
            candidate.action,
            candidate.candidateLayer,
            candidate.candidateClass,
            candidate.candidateDiagnosticClass,
            candidate.failureReason,
            candidate.auditRecommendedTrackRawValue,
            candidate.auditRecommendedFamily,
            candidate.auditRecommendedSubtype,
            candidate.auditRecommendedFinalClass,
            candidate.auditRecommendedEventTrackClass,
            candidate.auditReviewStatus,
            bool(candidate.auditReviewRequired),
            candidate.auditConfidenceTier,
            candidate.auditUncertaintyReason,
            candidate.auditLongBurstDefinitionStatus,
            candidate.reviewEvidenceClass,
            candidate.reviewEvidenceStrength,
            candidate.reviewEvidenceSummary,
            candidate.anchorFamily,
            number(annotation?.rawStartSec),
            number(annotation?.rawEndSec),
            number(annotation?.alignedStartSec),
            number(annotation?.alignedEndSec),
            number(candidate.durationSec ?? annotation?.durationSec),
            integer(candidate.startSpikeIndex),
            integer(candidate.endSpikeIndex),
            integer(candidate.nSpikes),
            integer(candidate.startISIIndex),
            integer(candidate.endISIIndex),
            integer(candidate.nISI),
            integer(candidate.nValidISI),
            number(candidate.anchorBandLowerSec),
            number(candidate.anchorBandUpperSec),
            candidate.anchorBandSource.rawValue,
            integer(resolution?.validISICount),
            number(candidateBand?.seedLowerSec),
            number(candidateBand?.seedUpperSec),
            number(candidateBand?.bridgeUpperSec),
            number(candidateBand?.contrastS),
            resolution?.structuralSeedSummary.source ?? "",
            integer(resolution?.structuralSeedSummary.burstAnchorCount),
            number(resolution?.structuralSeedSummary.burstSupportWeight),
            number(resolution?.structuralSeedSummary.burstSeedUpperSec),
            number(resolution?.structuralSeedSummary.burstBridgeUpperSec),
            integer(resolution?.structuralSeedSummary.tonicAnchorCount),
            number(resolution?.structuralSeedSummary.tonicSupportWeight),
            number(resolution?.structuralSeedSummary.tonicSeedLowerSec),
            number(resolution?.structuralSeedSummary.tonicSeedUpperSec),
            integer(resolution?.structuralSeedSummary.pauseAnchorCount),
            integer(resolution?.structuralSeedSummary.pausePoolAnchorCount),
            number(resolution?.structuralSeedSummary.pauseSupportWeight),
            number(resolution?.structuralSeedSummary.pausePoolSupportWeight),
            resolution?.structuralSeedSummary.pausePoolSource ?? "",
            number(resolution?.structuralSeedSummary.pauseSeedLowerSec),
            number(resolution?.structuralSeedSummary.pauseSeedUpperSec),
            datasetSummary.source,
            integer(datasetSummary.trainCount),
            integer(datasetSummary.seededTrainCount),
            integer(datasetSummary.burstAnchorCount),
            number(datasetSummary.burstSupportWeight),
            number(datasetSummary.burstSeedUpperSec),
            number(datasetSummary.burstBridgeUpperSec),
            integer(datasetSummary.tonicAnchorCount),
            number(datasetSummary.tonicSupportWeight),
            number(datasetSummary.tonicSeedLowerSec),
            number(datasetSummary.tonicSeedUpperSec),
            integer(datasetSummary.pauseAnchorCount),
            integer(datasetSummary.pausePoolAnchorCount),
            number(datasetSummary.pauseSupportWeight),
            number(datasetSummary.pausePoolSupportWeight),
            datasetSummary.pausePoolSource,
            number(datasetSummary.pauseSeedLowerSec),
            number(datasetSummary.pauseSeedUpperSec),
            number(candidate.profileSeedLowPercentileInTrain),
            number(candidate.profileSeedHighPercentileInTrain),
            number(candidate.profileSeedBandFraction),
            integer(candidate.profileSeedRunCount),
            integer(candidate.profileMaxSeedRunLength),
            number(candidate.profileMedianISISec),
            number(candidate.profileQ10ISISec),
            number(candidate.profileQ25ISISec),
            number(candidate.profileQ90ISISec),
            number(candidate.profilePauseFraction),
            candidate.profilePhenotypePrior ?? "",
            number(candidate.profileBridgeUpperSec),
            number(candidate.profileBoundaryFloorSec),
            eventCoreProfileValue(candidate, candidate.anchorBandLowerSec),
            eventCoreProfileValue(candidate, candidate.anchorBandUpperSec),
            eventCoreProfileValue(candidate, candidate.profileBridgeUpperSec),
            eventCoreProfileValue(candidate, candidate.profileBoundaryFloorSec),
            candidate.candidateLayer == "event_core_train_isi_band_profile" ? bool(candidate.profileBoundaryFloorHard ?? false) : "",
            eventCoreProfileValue(candidate, candidate.profileBurstContrastS),
            eventCoreProfileValue(candidate, candidate.profilePossibleContrastS),
            number(candidate.localBackgroundQ75Sec),
            number(candidate.localCompressionQ90Ratio),
            number(candidate.eventLocalMedianSec),
            number(candidate.eventLocalPercentileMedian),
            number(candidate.eventLocalPercentileQ90),
            number(candidate.eventLocalRobustZMedian),
            number(candidate.eventLocalRobustZAbsQ80),
            number(candidate.eventLocalRobustZQ10),
            number(candidate.stateRegularityScore),
            number(candidate.stateBurstSeedFraction),
            number(candidate.stateLowTailFraction),
            number(candidate.stateLocalStabilityScore),
            integer(candidate.stateCoreBurstRunLength),
            candidate.stateTonicSubtype ?? "",
            number(candidate.stateTrainPercentileMedian),
            number(candidate.stateLocalPercentileMedian),
            number(candidate.stateLocalPercentileQ90),
            number(candidate.stateLocalRobustZMedian),
            number(candidate.stateLocalRobustZAbsQ80),
            number(candidate.stateLocalRobustZQ10),
            number(candidate.hfSpikingQ80Sec),
            number(candidate.hfSpikingQ80MaxSec),
            number(candidate.hfSpikingQ90MaxSec),
            number(candidate.hfSpikingShortUpperSec),
            number(candidate.hfSpikingEpochBridgeSec),
            number(candidate.hfSpikingToleratedGapSec),
            number(candidate.hfSpikingPatternMaxISISec),
            number(candidate.hfSpikingPauseBreakSec),
            number(candidate.hfSpikingShortFraction),
            number(candidate.hfSpikingQ90ShortFraction),
            number(candidate.hfSpikingBridgeFraction),
            number(candidate.hfSpikingLargeFraction),
            number(candidate.hfSpikingToleratedFraction),
            integer(candidate.hfSpikingMaxConsecutiveLargeISI),
            integer(candidate.hfSpikingMinSpikesRequired),
            candidate.hfSpikingAcceptanceRoute ?? "",
            integer(candidate.hfSpikingEmbeddedBurstCount),
            integer(candidate.hfSpikingEmbeddedBurstGroupCount),
            number(candidate.hfSpikingEmbeddedBurstCoverage),
            optionalBool(candidate.hfSpikingBurstDominated),
            optionalBool(candidate.hfSpikingBurstPacketLike),
            optionalBool(candidate.hfSpikingBurstPacketNeighbor),
            optionalBool(candidate.suppressedByHFSpikingState),
            candidate.suppressedOriginalLabel ?? "",
            candidate.hfSpikingSuppressorID ?? "",
            candidate.thresholdMode ?? "",
            optionalBool(candidate.hardThreshold),
            candidate.hardThresholdPattern ?? "",
            number(candidate.hardBurstSeedUpperSec),
            number(candidate.hardBurstBridgeUpperSec),
            integer(candidate.hardBurstCoreISICount),
            candidate.hardThresholdSource ?? "",
            integer(candidate.burstSeedRunStartISI),
            integer(candidate.burstSeedRunEndISI),
            number(candidate.burstSeedBandLowerSec),
            number(candidate.burstSeedBandUpperSec),
            number(candidate.burstBridgeBandUpperSec),
            number(candidate.burstContrastRequired),
            number(candidate.burstPossibleContrastRequired),
            number(candidate.burstRequiredGapSec),
            number(candidate.burstPossibleRequiredGapSec),
            number(candidate.burstBoundaryFloorSec),
            optionalBool(candidate.burstBoundaryFloorHard),
            optionalBool(candidate.burstStrictBoundaryPass),
            optionalBool(candidate.burstPossibleBoundaryPass),
            optionalBool(candidate.burstBridgeCountPass),
            optionalBool(candidate.burstBridgeFractionPass),
            optionalBool(candidate.burstQ90BridgePass),
            candidate.burstSizeLabelBeforeReview ?? "",
            number(candidate.score),
            integer(candidate.priority),
            number(candidate.intraQ10Sec),
            number(candidate.intraQ40Sec),
            number(candidate.intraQ50Sec),
            number(candidate.intraQ90Sec),
            number(candidate.intraQ95Sec),
            number(candidate.maxIntraISISec),
            number(candidate.meanIntraISISec),
            number(candidate.cv),
            number(candidate.cv2),
            number(candidate.lv),
            number(candidate.preGapSec),
            number(candidate.postGapSec),
            number(candidate.preRatioQ90),
            number(candidate.postRatioQ90),
            number(candidate.edgeContrastMinQ90),
            number(candidate.edgeContrastGeomQ90),
            number(candidate.anchorContrastMinRequired),
            number(candidate.anchorContrastGeomRequired),
            integer(candidate.refractorySuspectCount),
            candidate.refractorySuspectAction?.rawValue ?? "",
            candidate.pipelineStageSummary,
            candidate.decisionPath,
            candidate.stateHighFrequencySubtype ?? "",
            number(isiEvidence.edgeContrastMin),
            number(isiEvidence.edgeContrastGeom),
            number(isiEvidence.preEdgeRatio),
            number(isiEvidence.postEdgeRatio),
            integer(isiEvidence.flankCount),
            number(isiEvidence.coreQPct),
            bool(isiEvidence.percentileReliable),
            number(isiEvidence.localMedianISISec),
            number(isiEvidence.localCompressionRatio),
            number(eventness.q10ISISec),
            number(eventness.q50ISISec),
            number(eventness.q90ISISec),
            number(eventness.q90Q10Ratio),
            number(eventness.distantContextMedianSec),
            number(eventness.contextContrast),
            number(eventness.returnToBaselineScore),
            number(eventness.eventnessEdgeComponent),
            number(eventness.eventnessContextComponent),
            number(eventness.eventnessScore),
            number(eventness.regularityScore),
            eventness.eventnessZone,
            bool(eventness.mediumEventnessReview),
            eventness.auditRecommendation,
            eventness.auditNote,
            bool(nearMiss.eligible),
            bool(nearMiss.isNearMiss),
            nearMiss.bestCategory,
            nearMiss.bestParameter ?? "",
            nearMiss.bestDirection,
            number(nearMiss.bestCurrentValue),
            number(nearMiss.bestRequiredValue),
            number(nearMiss.bestAbsoluteChange),
            number(nearMiss.bestRelativeChange),
            integer(nearMiss.failureCount),
            number(nearMiss.nearMissScore),
            number(nearMiss.eventnessScore),
            nearMiss.eventnessZone,
            nearMiss.candidateRef,
            nearMiss.reason,
            nearMiss.details
        ]
    }

    private static func sortedCandidates(_ candidates: [ClassicAnchorCandidate]) -> [ClassicAnchorCandidate] {
        candidates.sorted { lhs, rhs in
            if lhs.trainName != rhs.trainName {
                return lhs.trainName < rhs.trainName
            }
            if lhs.startSpikeIndex != rhs.startSpikeIndex {
                return lhs.startSpikeIndex < rhs.startSpikeIndex
            }
            if lhs.priority != rhs.priority {
                return lhs.priority > rhs.priority
            }
            return lhs.id < rhs.id
        }
    }

    private static func adaptiveBandPattern(for label: ClassicAnchorLabel) -> AdaptiveBandPattern {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst, .reject, .profile:
            return .burst
        case .highFrequencySpiking:
            return .highFrequencySpiking
        case .highFrequencyTonic:
            return .highFrequencyTonic
        case .tonic:
            return .tonic
        case .pause:
            return .pause
        }
    }

    private static func csvEscaped(_ value: String) -> String {
        guard value.contains(",") || value.contains("\"") || value.contains("\n") || value.contains("\r") else {
            return value
        }
        return "\"\(value.replacingOccurrences(of: "\"", with: "\"\""))\""
    }

    private static func bool(_ value: Bool) -> String {
        value ? "true" : "false"
    }

    private static func optionalBool(_ value: Bool?) -> String {
        guard let value else {
            return ""
        }
        return bool(value)
    }

    private static func eventCoreProfileValue(_ candidate: ClassicAnchorCandidate, _ value: Double?) -> String {
        guard candidate.candidateLayer == "event_core_train_isi_band_profile" else {
            return ""
        }
        return number(value)
    }

    private static func integer(_ value: Int?) -> String {
        guard let value else {
            return ""
        }
        return "\(value)"
    }

    private static func number(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return ""
        }
        return String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), value)
    }
}
