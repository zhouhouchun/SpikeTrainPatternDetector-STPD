import Foundation

public struct StructuralBridgeExpansionSettings: Hashable, Sendable {
    public var maxBridgeISI: Int
    public var localBridgeExpansionFactor: Double
    public var datasetBridgeExpansionFactor: Double
    public var maxLongBurstSpikes: Int

    public init(
        maxBridgeISI: Int = 2,
        localBridgeExpansionFactor: Double = 1.25,
        datasetBridgeExpansionFactor: Double = 1.15,
        maxLongBurstSpikes: Int = 0
    ) {
        self.maxBridgeISI = max(1, maxBridgeISI)
        self.localBridgeExpansionFactor = max(1, localBridgeExpansionFactor)
        self.datasetBridgeExpansionFactor = max(1, datasetBridgeExpansionFactor)
        self.maxLongBurstSpikes = max(0, maxLongBurstSpikes)
    }
}

public enum StructuralBridgeExpansionResolver {
    public static func detect(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        resolution: TrainAdaptiveBandResolution,
        datasetSummary: StructuralDatasetSeedSummary,
        detectorSettings: ClassicAnchorSettings,
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        settings: StructuralBridgeExpansionSettings = StructuralBridgeExpansionSettings()
    ) -> [ClassicAnchorCandidate] {
        let anchors = candidates
            .filter { candidate in
                candidate.isBurstBridgeExpansionAnchor
            }
            .sorted { lhs, rhs in
                if lhs.startISIIndex != rhs.startISIIndex {
                    return lhs.startISIIndex < rhs.startISIIndex
                }
                return lhs.endISIIndex < rhs.endISIIndex
            }
        guard anchors.count >= 2 else {
            return []
        }

        var bridgeCandidates: [ClassicAnchorCandidate] = []
        var seen = Set<String>()
        for index in anchors.indices.dropLast() {
            let left = anchors[index]
            let right = anchors[index + 1]
            guard right.startISIIndex > left.endISIIndex else {
                continue
            }
            let gapRange = (left.endISIIndex + 1)..<right.startISIIndex
            guard !gapRange.isEmpty,
                  gapRange.count <= settings.maxBridgeISI,
                  gapRange.allSatisfy({ gapIndex in
                      isBridgeableGap(
                          train: train,
                          index: gapIndex,
                          resolution: resolution,
                          datasetSummary: datasetSummary,
                          detectorSettings: detectorSettings,
                          qualitySettings: qualitySettings,
                          settings: settings
                      )
                  }) else {
                continue
            }

            let start = left.startISIIndex
            let end = right.endISIIndex
            let key = "\(start)-\(end)"
            guard seen.insert(key).inserted,
                  let candidate = bridgeCandidate(
                      train: train,
                      start: start,
                      end: end,
                      left: left,
                      right: right,
                      gapIndices: Array(gapRange),
                      resolution: resolution,
                      datasetSummary: datasetSummary,
                      detectorSettings: detectorSettings,
                      qualitySettings: qualitySettings,
                      settings: settings,
                      index: bridgeCandidates.count + 1
                  ) else {
                continue
            }
            bridgeCandidates.append(candidate)
        }
        return bridgeCandidates
    }

    private struct BridgeMetrics {
        let values: [Double]
        let nISI: Int
        let nValidISI: Int
        let nSpikes: Int
        let durationSec: Double?
        let q10: Double?
        let q40: Double?
        let q50: Double?
        let q90: Double?
        let q95: Double?
        let max: Double?
        let mean: Double?
        let cv: Double?
        let lv: Double?
        let preGap: Double?
        let postGap: Double?
        let preRatioQ90: Double?
        let postRatioQ90: Double?
        let edgeContrastMinQ90: Double?
        let edgeContrastGeomQ90: Double?
        let refractorySuspectCount: Int
    }

    private struct BridgeGuard {
        let localBridge: Double
        let datasetBridge: Double?
        let bridgeLimit: Double
        let pauseFloor: Double?
        let pauseFloorSource: String
        let effectiveBridgeUpper: Double
    }

    private struct BridgeSupport {
        let localBurstSupport: Double
        let datasetBurstSupport: Double
        let datasetCoverage: Double
        let combinedBurstSupport: Double
        let pauseBoundarySupport: Double
    }

    private struct BridgeRevalidation {
        let seedPurity: Double
        let seedPurityMin: Double
        let bridgeCount: Int
        let bridgeFraction: Double
        let bridgeFractionStrictMax: Double
        let bridgeFractionPossibleMax: Double
        let q90StrictPass: Bool
        let q90PossiblePass: Bool
        let q95StrictPass: Bool
        let q95PossiblePass: Bool
        let q95ExcessRatio: Double?
        let externalFlankStrictPass: Bool
        let externalFlankPossiblePass: Bool
        let externalFlankMode: String
        let strictPass: Bool
        let possiblePass: Bool
    }

    private static func isBridgeableGap(
        train: SpikeTrain,
        index: Int,
        resolution: TrainAdaptiveBandResolution,
        datasetSummary: StructuralDatasetSeedSummary,
        detectorSettings: ClassicAnchorSettings,
        qualitySettings: SpikeQualitySettings,
        settings: StructuralBridgeExpansionSettings
    ) -> Bool {
        guard let value = validISI(train: train, index: index, minValidISISec: detectorSettings.minValidISISec) else {
            return false
        }
        let guardrail = bridgeGuard(
            resolution: resolution,
            datasetSummary: datasetSummary,
            detectorSettings: detectorSettings,
            settings: settings
        )
        guard value <= guardrail.bridgeLimit + tolerance(for: guardrail.bridgeLimit) else {
            return false
        }
        guard !crossesStructuralPauseFloor(value: value, pauseFloor: guardrail.pauseFloor) else {
            return false
        }
        return true
    }

    private static func bridgeCandidate(
        train: SpikeTrain,
        start: Int,
        end: Int,
        left: ClassicAnchorCandidate,
        right: ClassicAnchorCandidate,
        gapIndices: [Int],
        resolution: TrainAdaptiveBandResolution,
        datasetSummary: StructuralDatasetSeedSummary,
        detectorSettings: ClassicAnchorSettings,
        qualitySettings: SpikeQualitySettings,
        settings: StructuralBridgeExpansionSettings,
        index: Int
    ) -> ClassicAnchorCandidate? {
        guard let metrics = bridgeMetrics(
            train: train,
            start: start,
            end: end,
            detectorSettings: detectorSettings,
            qualitySettings: qualitySettings
        ) else {
            return nil
        }
        let maxSpikes = settings.maxLongBurstSpikes > 0 ? settings.maxLongBurstSpikes : detectorSettings.longMaxSpikes
        guard maxSpikes <= 0 || metrics.nSpikes <= maxSpikes else {
            return nil
        }

        let anchorUpper = resolution.structuralSeedSummary.burstSeedUpperSec ??
            resolution.burstBand?.seedUpperSec ??
            detectorSettings.effectiveBurstBandUpperSec
        let bridgeUpper = resolution.structuralSeedSummary.burstBridgeUpperSec ??
            datasetSummary.burstBridgeUpperSec ??
            resolution.burstBand?.bridgeUpperSec ??
            detectorSettings.effectiveBurstBridgeUpperSec
        let guardrail = bridgeGuard(
            resolution: resolution,
            datasetSummary: datasetSummary,
            detectorSettings: detectorSettings,
            settings: settings
        )
        guard let revalidation = postMergeRevalidation(
            metrics: metrics,
            anchorUpper: anchorUpper,
            guardrail: guardrail,
            detectorSettings: detectorSettings
        ) else {
            return nil
        }
        let bridgeSizeLabel = bridgeLabel(
            left: left,
            right: right,
            metrics: metrics,
            detectorSettings: detectorSettings
        )
        let label = revalidation.strictPass
            ? bridgeSizeLabel
            : ClassicAnchorLabel.possibleBurst
        guard detectorSettings.enabledLabels.contains(label) else {
            return nil
        }
        let gapText = gapIndices.map(String.init).joined(separator: "|")
        let gapValueText = gapIndices
            .compactMap { validISI(train: train, index: $0, minValidISISec: detectorSettings.minValidISISec) }
            .map(format)
            .joined(separator: "|")
        let support = bridgeSupport(
            resolution: resolution,
            datasetSummary: datasetSummary
        )
        let weakPossibleAnchorCount = [left, right].filter { $0.finalLabel == .possibleBurst }.count
        let score = 80 +
            min(20, Double(metrics.nValidISI)) +
            min(8, Double(gapIndices.count)) +
            min(12, (metrics.edgeContrastMinQ90 ?? 0)) +
            25 * support.combinedBurstSupport +
            10 * support.pauseBoundarySupport +
            (revalidation.strictPass ? 10 : -8) -
            6 * Double(weakPossibleAnchorCount)
        let priority = revalidation.strictPass
            ? bridgePriority(label: label, gapCount: gapIndices.count, support: support)
            : possibleBridgePriority(gapCount: gapIndices.count, support: support)
        let action = revalidation.strictPass ? "accept" : "review_possible"
        let gateStatus = revalidation.strictPass
            ? "structural_seed_bridge_expansion_pass"
            : "structural_seed_bridge_expansion_possible_review"
        let gapMax = gapIndices
            .compactMap { validISI(train: train, index: $0, minValidISISec: detectorSettings.minValidISISec) }
            .max()
        let decisionPath = [
            "structural_bridge_expansion",
            "post_merge_revalidation=seed_purity_bridge_fraction_q90_q95_external_flank_pause_floor",
            "left=\(left.id)",
            "right=\(right.id)",
            "left_label=\(left.finalLabel.rawValue)",
            "right_label=\(right.finalLabel.rawValue)",
            "left_anchor_strength=\(bridgeAnchorStrength(left))",
            "right_anchor_strength=\(bridgeAnchorStrength(right))",
            "uses_weak_possible_burst_anchor=\(weakPossibleAnchorCount > 0)",
            "weak_possible_burst_anchor_count=\(weakPossibleAnchorCount)",
            "gap_isi=\(gapText)",
            "gap_values=\(gapValueText.isEmpty ? "NA" : gapValueText)",
            "gap_max=\(format(gapMax))",
            "bridge_size_label_before_review=\(bridgeSizeLabel.rawValue)",
            "selected_final_label=\(label.rawValue)",
            "seed_purity=\(format(revalidation.seedPurity))",
            "seed_purity_min=\(format(revalidation.seedPurityMin))",
            "bridge_count=\(revalidation.bridgeCount)",
            "bridge_fraction=\(format(revalidation.bridgeFraction))",
            "bridge_fraction_strict_max=\(format(revalidation.bridgeFractionStrictMax))",
            "bridge_fraction_possible_max=\(format(revalidation.bridgeFractionPossibleMax))",
            "q90_strict_pass=\(revalidation.q90StrictPass)",
            "q90_possible_pass=\(revalidation.q90PossiblePass)",
            "q95_strict_pass=\(revalidation.q95StrictPass)",
            "q95_possible_pass=\(revalidation.q95PossiblePass)",
            "q95_excess_ratio=\(format(revalidation.q95ExcessRatio))",
            "external_flank_mode=\(revalidation.externalFlankMode)",
            "external_flank_strict_pass=\(revalidation.externalFlankStrictPass)",
            "external_flank_possible_pass=\(revalidation.externalFlankPossiblePass)",
            "local_burst_bridge=\(format(guardrail.localBridge))",
            "dataset_burst_bridge=\(format(guardrail.datasetBridge))",
            "bridge_limit=\(format(guardrail.bridgeLimit))",
            "pause_floor=\(format(guardrail.pauseFloor))",
            "pause_floor_source=\(guardrail.pauseFloorSource)",
            "train_structural_seed_source=\(resolution.structuralSeedSummary.source)",
            "train_pause_pool_source=\(resolution.structuralSeedSummary.pausePoolSource)",
            "dataset_structural_seed_source=\(datasetSummary.source)",
            "dataset_pause_pool_source=\(datasetSummary.pausePoolSource)",
            "train_burst_anchor_count=\(resolution.structuralSeedSummary.burstAnchorCount)",
            "dataset_burst_anchor_count=\(datasetSummary.burstAnchorCount)",
            "train_pause_pool_anchor_count=\(resolution.structuralSeedSummary.pausePoolAnchorCount)",
            "dataset_pause_pool_anchor_count=\(datasetSummary.pausePoolAnchorCount)",
            "local_burst_support_weight=\(format(support.localBurstSupport))",
            "dataset_burst_support_weight=\(format(support.datasetBurstSupport))",
            "dataset_seeded_train_fraction=\(format(support.datasetCoverage))",
            "combined_burst_support_weight=\(format(support.combinedBurstSupport))",
            "pause_boundary_support_weight=\(format(support.pauseBoundarySupport))",
            "support_adjusted_priority=\(priority)",
            "weak_possible_anchor_score_penalty=\(format(6 * Double(weakPossibleAnchorCount)))",
            "pause_boundary_guard=strict_below_structural_floor",
            "bridge_crosses_structural_pause_floor=false",
            "effective_bridge_upper=\(format(guardrail.effectiveBridgeUpper))"
        ].joined(separator: ";")

        var candidate = ClassicAnchorCandidate(
            id: "\(train.id)-structural-bridge-burst-\(start)-\(end)-\(index)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "structural_seed_bridge_expansion_burst",
            candidateClass: "structural_seed_bridge_burst_event",
            finalLabel: label,
            gateStatus: gateStatus,
            decisionPath: decisionPath,
            action: action,
            score: score,
            priority: priority,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: start,
            endISIIndex: end,
            startSpikeIndex: start,
            endSpikeIndex: end + 1,
            nISI: metrics.nISI,
            nValidISI: metrics.nValidISI,
            nSpikes: metrics.nSpikes,
            durationSec: metrics.durationSec,
            intraQ10Sec: metrics.q10,
            intraQ40Sec: metrics.q40,
            intraQ50Sec: metrics.q50,
            intraQ90Sec: metrics.q90,
            intraQ95Sec: metrics.q95,
            maxIntraISISec: metrics.max,
            meanIntraISISec: metrics.mean,
            cv: metrics.cv,
            lv: metrics.lv,
            preGapSec: metrics.preGap,
            postGapSec: metrics.postGap,
            preRatioQ90: metrics.preRatioQ90,
            postRatioQ90: metrics.postRatioQ90,
            edgeContrastMinQ90: metrics.edgeContrastMinQ90,
            edgeContrastGeomQ90: metrics.edgeContrastGeomQ90,
            anchorFamily: "structural_bridge_burst",
            anchorLockLevel: revalidation.strictPass ? .strongCandidate : .auditOnly,
            anchorBandLowerSec: resolution.burstBand?.seedLowerSec ?? detectorSettings.effectiveBurstBandLowerSec,
            anchorBandUpperSec: anchorUpper,
            anchorBandSource: .structure,
            anchorContrastMinRequired: detectorSettings.possibleBurstContrastMin,
            anchorContrastGeomRequired: detectorSettings.possibleBurstContrastGeomMin,
            refractorySuspectCount: metrics.refractorySuspectCount,
            refractorySuspectAction: STPDRefractoryEvidencePolicy.effectiveAction(
                refCount: metrics.refractorySuspectCount,
                nISI: metrics.nISI,
                requestedAction: detectorSettings.refractoryAction
            )
        )
        candidate.burstSeedRunStartISI = left.burstSeedRunStartISI ?? left.startISIIndex
        candidate.burstSeedRunEndISI = right.burstSeedRunEndISI ?? right.endISIIndex
        candidate.burstSeedBandLowerSec = resolution.burstBand?.seedLowerSec ?? detectorSettings.effectiveBurstBandLowerSec
        candidate.burstSeedBandUpperSec = anchorUpper
        candidate.burstBridgeBandUpperSec = bridgeUpper
        candidate.burstContrastRequired = detectorSettings.burstContrastMin
        candidate.burstPossibleContrastRequired = detectorSettings.possibleBurstContrastMin
        candidate.burstRequiredGapSec = metrics.q90.map { $0 * detectorSettings.burstContrastMin }
        candidate.burstPossibleRequiredGapSec = metrics.q90.map { $0 * detectorSettings.possibleBurstContrastMin }
        candidate.burstBoundaryFloorSec = guardrail.pauseFloor
        candidate.burstBoundaryFloorHard = guardrail.pauseFloor != nil
        candidate.burstStrictBoundaryPass = revalidation.externalFlankStrictPass
        candidate.burstPossibleBoundaryPass = revalidation.externalFlankPossiblePass
        candidate.burstBridgeCountPass = gapIndices.count <= settings.maxBridgeISI
        candidate.burstBridgeFractionPass = revalidation.strictPass
            ? revalidation.bridgeFraction <= revalidation.bridgeFractionStrictMax + tolerance(for: revalidation.bridgeFractionStrictMax)
            : revalidation.bridgeFraction <= revalidation.bridgeFractionPossibleMax + tolerance(for: revalidation.bridgeFractionPossibleMax)
        candidate.burstQ90BridgePass = revalidation.strictPass ? revalidation.q90StrictPass : revalidation.q90PossiblePass
        candidate.burstSizeLabelBeforeReview = bridgeSizeLabel.rawValue
        return candidate
    }

    private static func bridgeGuard(
        resolution: TrainAdaptiveBandResolution,
        datasetSummary: StructuralDatasetSeedSummary,
        detectorSettings: ClassicAnchorSettings,
        settings: StructuralBridgeExpansionSettings
    ) -> BridgeGuard {
        let localBridge = resolution.structuralSeedSummary.burstBridgeUpperSec ??
            resolution.burstBand?.bridgeUpperSec ??
            detectorSettings.effectiveBurstBridgeUpperSec
        let datasetBridge = datasetSummary.burstBridgeUpperSec
        let bridgeLimit = maxFinite(
            localBridge * settings.localBridgeExpansionFactor,
            datasetBridge.map { $0 * settings.datasetBridgeExpansionFactor }
        ) ?? localBridge * settings.localBridgeExpansionFactor
        let localPauseFloor = resolution.structuralSeedSummary.pauseSeedLowerSec
        let datasetPauseFloor = datasetSummary.pauseSeedLowerSec
        let pauseFloor = minFinite(localPauseFloor, datasetPauseFloor)
        let pauseFloorSource = structuralPauseFloorSource(
            pauseFloor: pauseFloor,
            localPauseFloor: localPauseFloor,
            datasetPauseFloor: datasetPauseFloor,
            localSummary: resolution.structuralSeedSummary,
            datasetSummary: datasetSummary
        )
        let effectiveBridgeUpper = pauseFloor.map { min(bridgeLimit, $0) } ?? bridgeLimit
        return BridgeGuard(
            localBridge: localBridge,
            datasetBridge: datasetBridge,
            bridgeLimit: bridgeLimit,
            pauseFloor: pauseFloor,
            pauseFloorSource: pauseFloorSource,
            effectiveBridgeUpper: effectiveBridgeUpper
        )
    }

    private static func bridgeAnchorStrength(_ candidate: ClassicAnchorCandidate) -> String {
        if candidate.finalLabel.isCanonicalBurstFamily {
            return "selected_canonical_burst_anchor"
        }
        if candidate.isWeakBurstStructuralSeedEvidence {
            return "selected_weak_possible_burst_anchor"
        }
        return "non_anchor"
    }

    private static func bridgeSupport(
        resolution: TrainAdaptiveBandResolution,
        datasetSummary: StructuralDatasetSeedSummary
    ) -> BridgeSupport {
        let datasetCoverage = datasetSummary.trainCount > 0
            ? Double(datasetSummary.seededTrainCount) / Double(datasetSummary.trainCount)
            : 0
        let localBurstSupport = cleanWeight(resolution.structuralSeedSummary.burstSupportWeight)
        let datasetBurstSupport = cleanWeight(datasetSummary.burstSupportWeight)
        let cleanCoverage = cleanWeight(datasetCoverage)
        let combinedBurstSupport = max(localBurstSupport, datasetBurstSupport * cleanCoverage)
        let localPauseSupport = cleanWeight(resolution.structuralSeedSummary.pauseSupportWeight)
        let datasetPauseSupport = cleanWeight(datasetSummary.pauseSupportWeight) * cleanCoverage
        let pauseBoundarySupport = max(localPauseSupport, datasetPauseSupport)
        return BridgeSupport(
            localBurstSupport: localBurstSupport,
            datasetBurstSupport: datasetBurstSupport,
            datasetCoverage: cleanCoverage,
            combinedBurstSupport: combinedBurstSupport,
            pauseBoundarySupport: pauseBoundarySupport
        )
    }

    private static func bridgePriority(
        label: ClassicAnchorLabel,
        gapCount: Int,
        support: BridgeSupport
    ) -> Int {
        let base: Double = switch label {
        case .highFrequencyBurst:
            1_235
        case .longBurst:
            1_160
        default:
            1_245
        }
        let supportBonus = 115 * support.combinedBurstSupport
        let pauseBonus = 25 * support.pauseBoundarySupport
        let gapPenalty = 35 * Double(max(0, gapCount - 1))
        let value = base + supportBonus + pauseBonus - gapPenalty
        return Int(max(1_080, min(1_390, value)).rounded())
    }

    private static func possibleBridgePriority(
        gapCount: Int,
        support: BridgeSupport
    ) -> Int {
        let supportBonus = 90 * support.combinedBurstSupport
        let pauseBonus = 20 * support.pauseBoundarySupport
        let gapPenalty = 30 * Double(max(0, gapCount - 1))
        let value = 560 + supportBonus + pauseBonus - gapPenalty
        return Int(max(500, min(760, value)).rounded())
    }

    private static func postMergeRevalidation(
        metrics: BridgeMetrics,
        anchorUpper: Double,
        guardrail: BridgeGuard,
        detectorSettings: ClassicAnchorSettings
    ) -> BridgeRevalidation? {
        guard anchorUpper.isFinite,
              anchorUpper > 0,
              !metrics.values.isEmpty else {
            return nil
        }

        let seedPurityMin = 0.45
        let seedCount = metrics.values.filter {
            $0 <= anchorUpper + tolerance(for: anchorUpper)
        }.count
        let seedPurity = Double(seedCount) / Double(max(1, metrics.values.count))
        let bridgeUpper = guardrail.effectiveBridgeUpper
        let bridgeCount = metrics.values.filter {
            $0 > anchorUpper + tolerance(for: anchorUpper) &&
                $0 <= bridgeUpper + tolerance(for: bridgeUpper)
        }.count
        let bridgeFraction = Double(bridgeCount) / Double(max(1, metrics.values.count))
        let bridgeFractionStrictMax = detectorSettings.burstBridgeFractionMax
        let bridgeFractionPossibleMax = max(0.50, detectorSettings.burstBridgeFractionMax)
        let strictBridgeUpper = min(guardrail.localBridge, bridgeUpper)
        let q90StrictPass = finiteLessThanOrEqual(metrics.q90, strictBridgeUpper)
        let q90PossiblePass = finiteLessThanOrEqual(metrics.q90, bridgeUpper)
        let q95StrictRatio = ratio(metrics.q95, over: strictBridgeUpper)
        let q95StrictPass = detectorSettings.burstStrictQ95BridgeGate
            ? finiteLessThanOrEqual(metrics.q95, strictBridgeUpper)
            : (q95StrictRatio ?? .infinity) <= detectorSettings.burstQ95SoftSevereRatio
        let q95ExcessRatio = ratio(metrics.q95, over: bridgeUpper)
        let q95PossiblePass = (q95ExcessRatio ?? .infinity) <= detectorSettings.burstQ95SoftSevereRatio + 0.20
        let flank = externalFlankRevalidation(metrics: metrics, detectorSettings: detectorSettings)

        let strictPass = seedPurity >= seedPurityMin - tolerance(for: seedPurityMin) &&
            bridgeFraction <= bridgeFractionStrictMax + tolerance(for: bridgeFractionStrictMax) &&
            q90StrictPass &&
            q95StrictPass &&
            flank.strict
        let possiblePass = seedPurity >= seedPurityMin - tolerance(for: seedPurityMin) &&
            bridgeFraction <= bridgeFractionPossibleMax + tolerance(for: bridgeFractionPossibleMax) &&
            q90PossiblePass &&
            q95PossiblePass &&
            flank.possible
        guard possiblePass else {
            return nil
        }

        return BridgeRevalidation(
            seedPurity: seedPurity,
            seedPurityMin: seedPurityMin,
            bridgeCount: bridgeCount,
            bridgeFraction: bridgeFraction,
            bridgeFractionStrictMax: bridgeFractionStrictMax,
            bridgeFractionPossibleMax: bridgeFractionPossibleMax,
            q90StrictPass: q90StrictPass,
            q90PossiblePass: q90PossiblePass,
            q95StrictPass: q95StrictPass,
            q95PossiblePass: q95PossiblePass,
            q95ExcessRatio: q95ExcessRatio,
            externalFlankStrictPass: flank.strict,
            externalFlankPossiblePass: flank.possible,
            externalFlankMode: flank.mode,
            strictPass: strictPass,
            possiblePass: possiblePass
        )
    }

    private static func externalFlankRevalidation(
        metrics: BridgeMetrics,
        detectorSettings: ClassicAnchorSettings
    ) -> (strict: Bool, possible: Bool, mode: String) {
        let left = metrics.preRatioQ90
        let right = metrics.postRatioQ90
        if let left, left.isFinite,
           let right, right.isFinite {
            let minRatio = min(left, right)
            let geomRatio = sqrt(max(left, 0) * max(right, 0))
            return (
                minRatio >= detectorSettings.burstContrastMin - tolerance(for: detectorSettings.burstContrastMin) &&
                    geomRatio >= detectorSettings.burstContrastGeomMin - tolerance(for: detectorSettings.burstContrastGeomMin),
                minRatio >= detectorSettings.possibleBurstContrastMin - tolerance(for: detectorSettings.possibleBurstContrastMin) &&
                    geomRatio >= detectorSettings.possibleBurstContrastGeomMin - tolerance(for: detectorSettings.possibleBurstContrastGeomMin),
                "two_sided"
            )
        }

        if let oneSided = left ?? right,
           oneSided.isFinite {
            let oneSidedStrict = detectorSettings.burstOneSidedAsCanonical &&
                oneSided >= (detectorSettings.burstOneSidedContrastMin ?? detectorSettings.burstContrastMin + 0.5) -
                tolerance(for: detectorSettings.burstContrastMin)
            let oneSidedPossible = oneSided >= detectorSettings.possibleBurstContrastMin -
                tolerance(for: detectorSettings.possibleBurstContrastMin)
            return (
                oneSidedStrict,
                oneSidedPossible,
                left == nil ? "one_sided_post_endpoint" : "one_sided_pre_endpoint"
            )
        }

        return (false, false, "missing_external_flanks")
    }

    private static func structuralPauseFloorSource(
        pauseFloor: Double?,
        localPauseFloor: Double?,
        datasetPauseFloor: Double?,
        localSummary: StructuralSeedBandSummary,
        datasetSummary: StructuralDatasetSeedSummary
    ) -> String {
        guard let pauseFloor, pauseFloor.isFinite else {
            return "none"
        }
        if let localPauseFloor,
           abs(localPauseFloor - pauseFloor) <= tolerance(for: pauseFloor) {
            return localSummary.pausePoolAnchorCount > 0
                ? "train_classic_burst_flank_pause_pool"
                : "train_structural_pause"
        }
        if let datasetPauseFloor,
           abs(datasetPauseFloor - pauseFloor) <= tolerance(for: pauseFloor) {
            return datasetSummary.pausePoolAnchorCount > 0
                ? "dataset_classic_burst_flank_pause_pool"
                : "dataset_structural_pause"
        }
        return "structural_pause"
    }

    private static func crossesStructuralPauseFloor(value: Double, pauseFloor: Double?) -> Bool {
        guard let pauseFloor, pauseFloor.isFinite, pauseFloor > 0 else {
            return false
        }
        return value >= pauseFloor - tolerance(for: pauseFloor)
    }

    private static func bridgeLabel(
        left: ClassicAnchorCandidate,
        right: ClassicAnchorCandidate,
        metrics: BridgeMetrics,
        detectorSettings: ClassicAnchorSettings
    ) -> ClassicAnchorLabel {
        if left.finalLabel == .highFrequencyBurst || right.finalLabel == .highFrequencyBurst {
            return .highFrequencyBurst
        }
        if metrics.nSpikes >= detectorSettings.longMinSpikes &&
            (detectorSettings.longMaxSpikes <= 0 || metrics.nSpikes <= detectorSettings.longMaxSpikes) &&
            (left.finalLabel == .longBurst || right.finalLabel == .longBurst) &&
            longBurstUpperBandPass(metrics: metrics, detectorSettings: detectorSettings) {
            return .longBurst
        }
        if metrics.nSpikes >= detectorSettings.longMinSpikes &&
            (detectorSettings.longMaxSpikes <= 0 || metrics.nSpikes <= detectorSettings.longMaxSpikes) &&
            longBurstUpperBandPass(metrics: metrics, detectorSettings: detectorSettings) {
            return .longBurst
        }
        if metrics.nSpikes > detectorSettings.classicMaxSpikes {
            return .possibleBurst
        }
        return .burst
    }

    private static func longBurstUpperBandPass(
        metrics: BridgeMetrics,
        detectorSettings: ClassicAnchorSettings
    ) -> Bool {
        let lower = detectorSettings.effectiveBurstBandLowerSec
        let upper = detectorSettings.effectiveBurstBandUpperSec
        let upperBandFloor = lower + 0.60 * max(0, upper - lower)
        return (metrics.q10 ?? -.infinity) >= upperBandFloor - tolerance(for: upperBandFloor) &&
            (metrics.q90 ?? .infinity) <= upper + tolerance(for: upper)
    }

    private static func bridgeMetrics(
        train: SpikeTrain,
        start: Int,
        end: Int,
        detectorSettings: ClassicAnchorSettings,
        qualitySettings: SpikeQualitySettings
    ) -> BridgeMetrics? {
        guard start > 0,
              end >= start,
              end < train.isiSec.count,
              start - 1 < train.timestampsSec.count,
              end < train.timestampsSec.count else {
            return nil
        }
        let values = (start...end).compactMap { index in
            validISI(train: train, index: index, minValidISISec: detectorSettings.minValidISISec)
        }
        guard !values.isEmpty else {
            return nil
        }
        let sample = SortedFiniteSample(values)
        let q10 = sample.quantile(0.10)
        let q40 = sample.quantile(0.40)
        let q50 = sample.quantile(0.50)
        let q90 = sample.quantile(0.90)
        let q95 = sample.quantile(0.95)
        let mean = values.reduce(0, +) / Double(values.count)
        let cv = STPDStatistics.coefficientOfVariation(values)
        let lv = STPDStatistics.localVariation(values)
        let preGap = validISI(train: train, index: start - 1, minValidISISec: detectorSettings.minValidISISec)
        let postGap = validISI(train: train, index: end + 1, minValidISISec: detectorSettings.minValidISISec)
        let preRatio = ratio(preGap, over: q90)
        let postRatio = ratio(postGap, over: q90)
        let edgeValues = [preRatio, postRatio].compactMap { $0 }.filter(\.isFinite)
        let edgeMin = edgeValues.min()
        let edgeGeom = geometricMean(edgeValues)
        let duration = train.timestampsSec[end] - train.timestampsSec[start - 1]
        let refThreshold = max(qualitySettings.refractorySuspectThresholdSec, qualitySettings.artifactThresholdSec)
        let refractoryCount = values.filter {
            $0 >= qualitySettings.artifactThresholdSec - tolerance(for: qualitySettings.artifactThresholdSec) &&
                $0 < refThreshold - tolerance(for: refThreshold)
        }.count

        return BridgeMetrics(
            values: values,
            nISI: end - start + 1,
            nValidISI: values.count,
            nSpikes: end - start + 2,
            durationSec: duration.isFinite ? max(0, duration) : nil,
            q10: q10,
            q40: q40,
            q50: q50,
            q90: q90,
            q95: q95,
            max: values.max(),
            mean: mean,
            cv: cv,
            lv: lv,
            preGap: preGap,
            postGap: postGap,
            preRatioQ90: preRatio,
            postRatioQ90: postRatio,
            edgeContrastMinQ90: edgeMin,
            edgeContrastGeomQ90: edgeGeom,
            refractorySuspectCount: refractoryCount
        )
    }

    private static func validISI(
        train: SpikeTrain,
        index: Int,
        minValidISISec: Double
    ) -> Double? {
        guard index > 0,
              train.isiSec.indices.contains(index),
              let value = train.isiSec[index],
              value.isFinite,
              value >= minValidISISec - tolerance(for: minValidISISec) else {
            return nil
        }
        return value
    }

    private static func quantile(_ values: [Double], probability: Double) -> Double? {
        let sorted = values.filter(\.isFinite).filter { $0 > 0 }.sorted()
        guard !sorted.isEmpty else {
            return nil
        }
        guard sorted.count > 1 else {
            return sorted[0]
        }
        let p = min(max(probability, 0), 1)
        let h = 1 + (Double(sorted.count) - 1) * p
        let lowerIndex = max(0, Int(floor(h)) - 1)
        let upperIndex = max(0, Int(ceil(h)) - 1)
        let lowerValue = sorted[min(lowerIndex, sorted.count - 1)]
        let upperValue = sorted[min(upperIndex, sorted.count - 1)]
        return lowerValue + (h - floor(h)) * (upperValue - lowerValue)
    }

    private static func ratio(_ numerator: Double?, over denominator: Double?) -> Double? {
        guard let numerator,
              let denominator,
              numerator.isFinite,
              denominator.isFinite,
              denominator > 0 else {
            return nil
        }
        return numerator / denominator
    }

    private static func finiteLessThanOrEqual(_ value: Double?, _ upper: Double?) -> Bool {
        guard let value,
              let upper,
              value.isFinite,
              upper.isFinite else {
            return false
        }
        return value <= upper + tolerance(for: upper)
    }

    private static func geometricMean(_ values: [Double]) -> Double? {
        let positive = values.filter { $0.isFinite && $0 > 0 }
        guard !positive.isEmpty else {
            return nil
        }
        return exp(positive.map(log).reduce(0, +) / Double(positive.count))
    }

    private static func maxFinite(_ values: Double?...) -> Double? {
        values.compactMap { value -> Double? in
            guard let value, value.isFinite, value > 0 else {
                return nil
            }
            return value
        }
        .max()
    }

    private static func minFinite(_ values: Double?...) -> Double? {
        values.compactMap { value -> Double? in
            guard let value, value.isFinite, value > 0 else {
                return nil
            }
            return value
        }
        .min()
    }

    private static func format(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), value)
    }

    private static func cleanWeight(_ value: Double) -> Double {
        guard value.isFinite else {
            return 0
        }
        return min(1, max(0, value))
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }
}
