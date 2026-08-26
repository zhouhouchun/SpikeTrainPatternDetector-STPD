import Foundation

public enum ClassicBurstFlankPauseDetector {
    public static func detect(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        settings: PauseDetectorSettings,
        existingCandidates: [ClassicAnchorCandidate] = []
    ) -> [ClassicAnchorCandidate] {
        guard settings.isEnabled, train.spikeCount >= 2 else {
            return []
        }

        let structuralClassicBursts = candidates.filter { candidate in
            candidate.selectedForAuto &&
                candidate.isEligibleForAutoSelection &&
                isBurstAllowedToSeedFlankPause(candidate)
        }

        var emittedIndices = Set(
            existingCandidates
                .filter {
                    ($0.finalLabel == .pause && $0.isEligibleForAutoSelection) ||
                        $0.isStructuralPausePriorEvidence
                }
                .filter {
                    $0.selectedForAuto ||
                        $0.candidateLayer == "classic_burst_flank_pause" ||
                        $0.candidateLayer == "possible_burst_flank_pause_prior" ||
                        $0.isStructuralPausePriorEvidence
                }
                .map { min($0.startISIIndex, $0.endISIIndex) }
        )
        var flankCandidates: [ClassicAnchorCandidate] = []
        for burst in structuralClassicBursts {
            let flankIndices = [
                burst.startISIIndex - 1,
                burst.endISIIndex + 1
            ]

            for isiIndex in flankIndices {
                guard !emittedIndices.contains(isiIndex),
                      let candidate = flankPauseCandidate(
                        train: train,
                        burst: burst,
                        isiIndex: isiIndex,
                        settings: settings,
                        candidateIndex: existingCandidates.count + flankCandidates.count + 1
                      ) else {
                    continue
                }
                emittedIndices.insert(isiIndex)
                flankCandidates.append(candidate)
            }
        }

        return flankCandidates
    }

    private static func flankPauseCandidate(
        train: SpikeTrain,
        burst: ClassicAnchorCandidate,
        isiIndex: Int,
        settings: PauseDetectorSettings,
        candidateIndex: Int
    ) -> ClassicAnchorCandidate? {
        guard train.isiSec.indices.contains(isiIndex),
              isiIndex > 0,
              let gap = finiteValidISI(train.isiSec[isiIndex], settings: settings),
              train.timestampsSec.indices.contains(isiIndex - 1),
              train.timestampsSec.indices.contains(isiIndex) else {
            return nil
        }

        let burstReference = [
            burst.intraQ90Sec,
            burst.intraQ95Sec,
            burst.maxIntraISISec,
            burst.meanIntraISISec
        ]
            .compactMap { $0 }
            .filter { $0.isFinite && $0 > 0 }
            .first
        guard let burstReference else {
            return nil
        }

        let contrast = gap / burstReference
        let contrastRequired = settings.classicBurstFlankPauseContrastMin
        let priorContrastRequired = contrastRequired
        let contrastEventPass = contrast >= contrastRequired - tolerance(for: contrastRequired)
        let contrastPriorPass = contrast >= priorContrastRequired - tolerance(for: priorContrastRequired)
        let structuralPauseSeedPass = contrastEventPass
        let structuralPauseEventPass = contrastEventPass
        let sourceIsPossibleBurst = burst.finalLabel == .possibleBurst
        let classicBurstBoundaryEventPass = !sourceIsPossibleBurst && contrastEventPass
        let eventPass = sourceIsPossibleBurst
            ? false
            : contrastEventPass
        let priorPass = contrastPriorPass
        guard eventPass || priorPass else {
            return nil
        }

        let isStrong = eventPass &&
            gap >= max(settings.strongThresholdSec, settings.displayUpperSec) -
            tolerance(for: max(settings.strongThresholdSec, settings.displayUpperSec))
        let duration = train.timestampsSec[isiIndex] - train.timestampsSec[isiIndex - 1]
        let score = log1p(max(gap, 0)) + 0.65 * log(max(contrast, 1))
        let side = isiIndex < burst.startISIIndex ? "pre" : "post"
        let role = eventPass ? "pause_pool_anchor_and_event_candidate" : "pause_prior_pool_anchor"
        let eventEvidenceTag = contrastEventPass
            ? "selected_classic_burst_flank_isi_has_structural_pause_contrast"
            : "selected_classic_burst_flank_isi_has_pause_pool_seed_support"
        let sourceStrength = sourceIsPossibleBurst
            ? "weak_possible_burst_flank_pause_prior"
            : "classic_burst_flank_pause"
        let pauseBoundaryRole: PauseBoundaryRole = isStrong
            ? .canonicalPauseAnchor
            : .briefStateInterruption
        let priorEvidenceTag = structuralPauseSeedPass
            ? "structural_pause_prior_seed_support_only__below_event_support"
            : "structural_pause_prior_only__below_event_contrast"
        let decisionPath = [
            eventPass ? "classic_burst_flank_pause_event" : "classic_burst_flank_pause_prior",
            "pause_boundary_role=\(pauseBoundaryRole.rawValue)",
            "role=\(role)",
            "source_burst=\(burst.id)",
            "source_burst_layer=\(burst.candidateLayer)",
            "source_burst_lock_level=\(burst.anchorLockLevel.rawValue)",
            "source_burst_selection_status=\(burst.selectionStatus)",
            "source_burst_label=\(burst.finalLabel.rawValue)",
            "source_burst_pause_seed_strength=\(sourceStrength)",
            "source_burst_allowed_to_seed_flank_pause=true",
            "possible_burst_flank_allowed_as_pause_prior_only=\(sourceIsPossibleBurst)",
            "side=\(side)",
            "flank_isi_index=\(isiIndex)",
            "ordinary_pause_candidate_must_be_auto_selected_to_block_flank_pause=true",
            "existing_flank_pause_candidate_blocks_duplicate=true",
            "flank_gap_sec=\(format(gap))",
            "burst_reference_sec=\(format(burstReference))",
            "contrast=\(formatRatio(contrast))",
            "required=\(formatRatio(contrastRequired))",
            "prior_required=\(formatRatio(priorContrastRequired))",
            "contrast_event_pass=\(contrastEventPass)",
            "contrast_prior_pass=\(contrastPriorPass)",
            "pause_seed_baseline_sec=\(format(settings.seedBaselineSec))",
            "structural_pause_seed_pass=\(structuralPauseSeedPass)",
            "structural_pause_support_weight=\(formatRatio(settings.structuralPauseSupportWeight))",
            "structural_pause_pool_support_weight=\(formatRatio(settings.structuralPausePoolSupportWeight))",
            "structural_pause_event_pass=\(structuralPauseEventPass)",
            "classic_burst_boundary_event_pass=\(classicBurstBoundaryEventPass)",
            "classic_burst_flank_pause_requires_contrast_ii=true",
            eventPass ? eventEvidenceTag : priorEvidenceTag,
            "classic_burst_boundary_isi_allowed_as_pause_seed",
            eventPass
                ? "classic_burst_boundary_isi_allowed_as_pause_event"
                : "not_event_track__used_for_structural_pause_seed_pool",
            sourceIsPossibleBurst
                ? "possible_burst_boundary_isi_used_as_weak_pause_pool_prior"
                : "classic_burst_boundary_isi_used_as_pause_pool_prior"
        ].joined(separator: ";")

        return ClassicAnchorCandidate(
            id: "\(train.id)-\(sourceIsPossibleBurst ? "possible" : "classic")-anchor-burst-flank-pause-\(candidateIndex)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: sourceIsPossibleBurst
                ? "possible_burst_flank_pause_prior"
                : "classic_burst_flank_pause",
            candidateClass: eventPass
                ? "\(side)_classic_burst_flank_pause_event"
                : "\(side)_\(sourceIsPossibleBurst ? "possible_burst" : "classic_burst")_flank_pause_prior",
            finalLabel: .pause,
            gateStatus: eventPass
                ? (isStrong ? "classic_burst_flank_strong_pause_pass" : "classic_burst_flank_structural_pause_pass")
                : "classic_burst_flank_pause_prior_seed_only",
            decisionPath: decisionPath,
            action: eventPass ? "accept" : "audit_only",
            score: score,
            priority: eventPass ? (isStrong ? 1_080 : 1_040) : 0,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: isiIndex,
            endISIIndex: isiIndex,
            startSpikeIndex: isiIndex,
            endSpikeIndex: isiIndex + 1,
            nISI: 1,
            nValidISI: 1,
            nSpikes: 2,
            durationSec: duration.isFinite ? duration : gap,
            intraQ10Sec: gap,
            intraQ40Sec: gap,
            intraQ50Sec: gap,
            intraQ90Sec: gap,
            intraQ95Sec: gap,
            maxIntraISISec: gap,
            meanIntraISISec: gap,
            cv: nil,
            lv: nil,
            preGapSec: finiteValidISI(isiIndex > 1 ? train.isiSec[isiIndex - 1] : nil, settings: settings),
            postGapSec: finiteValidISI(isiIndex < train.isiSec.count - 1 ? train.isiSec[isiIndex + 1] : nil, settings: settings),
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: contrast,
            edgeContrastGeomQ90: contrast,
            anchorFamily: "pause",
            anchorLockLevel: eventPass
                ? (isStrong ? .lockedClassic : .strongCandidate)
                : .auditOnly,
            anchorBandLowerSec: min(gap, settings.seedBaselineSec),
            anchorBandUpperSec: settings.displayUpperSec,
            anchorBandSource: .structure,
            anchorContrastMinRequired: contrastRequired,
            anchorContrastGeomRequired: burst.anchorContrastGeomRequired,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            pauseBoundaryRole: pauseBoundaryRole
        )
    }

    private static func isBurstAllowedToSeedFlankPause(_ candidate: ClassicAnchorCandidate) -> Bool {
        if candidate.finalLabel == .possibleBurst {
            return candidate.isPossibleBurstPausePoolPriorEvidence
        }
        guard candidate.finalLabel.isCanonicalBurstFamily else {
            return false
        }
        if candidate.anchorLockLevel == .lockedClassic {
            return true
        }
        if candidate.candidateLayer == "structural_seed_bridge_expansion_burst" {
            return true
        }
        let normalizedGate = candidate.gateStatus.lowercased()
        let normalizedPath = candidate.decisionPath.lowercased()
        return normalizedGate.contains("pass") ||
            normalizedPath.contains("strict_pass") ||
            normalizedPath.contains("post_merge_revalidation")
    }

    private static func finiteValidISI(_ value: Double?, settings: PauseDetectorSettings) -> Double? {
        guard let value,
              value.isFinite,
              value >= settings.minValidISISec - tolerance(for: settings.minValidISISec) else {
            return nil
        }
        return value
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }

    private static func formatRatio(_ value: Double) -> String {
        String(format: "%.2f", value)
    }

    private static func format(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.6g", value)
    }
}
