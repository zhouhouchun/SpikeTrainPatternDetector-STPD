import Foundation

public enum HybridPatternDetectionStage: String, CaseIterable, Hashable, Sendable {
    case trainStructuralBurstScreen = "train_structural_burst_screen"
    case trainStructuralSeedBandRefinement = "train_structural_seed_band_refinement"
    case trainCoreEventStateDetection = "train_core_event_state_detection"
    case burstFlankPausePool = "burst_flank_pause_pool"
    case stateEventCompatibilitySplit = "state_event_compatibility_split"
    case trainStructuralSeedSummary = "train_structural_seed_summary"
    case datasetSeedAggregation = "dataset_seed_aggregation"
    case datasetBridgeExpansion = "dataset_bridge_expansion"
    case datasetSeedAwareRerun = "dataset_seed_aware_rerun"
    case hfsProtection = "hfs_protection"
    case semanticArbitration = "semantic_arbitration"
    case auditAndExport = "audit_and_export"

    public var shortDescription: String {
        switch self {
        case .trainStructuralBurstScreen:
            "Find structural burst anchors from local run shape before trusting global ISI bands."
        case .trainStructuralSeedBandRefinement:
            "Use accepted structural anchors, including classic burst flanks, to refine train-local burst/pause/tonic bands."
        case .trainCoreEventStateDetection:
            "Run burst, pause, tonic, HF tonic, and HFS candidate generators with train-adaptive bands."
        case .burstFlankPausePool:
            "Promote classic burst boundary ISIs into the pause seed/event pool while keeping weak possible-burst flanks as priors."
        case .stateEventCompatibilitySplit:
            "Split tonic/HF state candidates around selected event candidates instead of forcing one global non-overlap rule."
        case .trainStructuralSeedSummary:
            "Summarize per-train structural seeds for later dataset-level distribution reasoning."
        case .datasetSeedAggregation:
            "Merge train-local structural seeds into dataset-level seed bands."
        case .datasetBridgeExpansion:
            "Bridge nearby burst/tonic fragments using local and dataset seed support, with pause-floor guardrails."
        case .datasetSeedAwareRerun:
            "Rerun detectors with dataset-aware bands after bridge expansion."
        case .hfsProtection:
            "Protect broad HFS states from being shredded by embedded burst-like packets."
        case .semanticArbitration:
            "Select final event/state tracks by semantic track, not by one flat interval class."
        case .auditAndExport:
            "Keep rejected, suppressed, profile, and selected candidates in the audit trail for UI/export."
        }
    }
}

public struct HybridPatternDetectionPolicy: Hashable, Sendable {
    public let name: String
    public let classicBurstContrastMin: Double
    public let classicBurstContrastGeomMin: Double
    public let classicBurstFlankPauseContrastMin: Double
    public let possibleBurstContrastMin: Double
    public let possibleBurstContrastGeomMin: Double
    public let oneSidedBurstAsCanonical: Bool

    public init(
        name: String,
        classicBurstContrastMin: Double,
        classicBurstContrastGeomMin: Double,
        classicBurstFlankPauseContrastMin: Double = 5.0,
        possibleBurstContrastMin: Double,
        possibleBurstContrastGeomMin: Double,
        oneSidedBurstAsCanonical: Bool
    ) {
        self.name = name
        self.classicBurstContrastMin = max(1, classicBurstContrastMin)
        self.classicBurstContrastGeomMin = max(1, classicBurstContrastGeomMin)
        self.classicBurstFlankPauseContrastMin = max(
            self.classicBurstContrastMin,
            classicBurstFlankPauseContrastMin
        )
        self.possibleBurstContrastMin = max(1, possibleBurstContrastMin)
        self.possibleBurstContrastGeomMin = max(1, possibleBurstContrastGeomMin)
        self.oneSidedBurstAsCanonical = oneSidedBurstAsCanonical
    }

    public static let legacyCompatible = HybridPatternDetectionPolicy(
        name: "legacy_compatible",
        classicBurstContrastMin: 2.5,
        classicBurstContrastGeomMin: 2.5,
        classicBurstFlankPauseContrastMin: 5.0,
        possibleBurstContrastMin: 2.0,
        possibleBurstContrastGeomMin: 2.0,
        oneSidedBurstAsCanonical: false
    )

    public static let structureFirstAdaptiveISI = HybridPatternDetectionPolicy(
        name: "structure_first_adaptive_isi",
        classicBurstContrastMin: 3.0,
        classicBurstContrastGeomMin: 3.0,
        classicBurstFlankPauseContrastMin: 5.0,
        possibleBurstContrastMin: 2.0,
        possibleBurstContrastGeomMin: 2.0,
        oneSidedBurstAsCanonical: true
    )

    public var decisionPathTag: String {
        [
            "hybrid_policy=\(name)",
            "classic_burst_contrast_min=\(formatRatio(classicBurstContrastMin))",
            "classic_burst_geom_contrast_min=\(formatRatio(classicBurstContrastGeomMin))",
            "classic_burst_flank_pause_contrast_min=\(formatRatio(classicBurstFlankPauseContrastMin))",
            "possible_burst_contrast_min=\(formatRatio(possibleBurstContrastMin))",
            "one_sided_burst_as_canonical=\(oneSidedBurstAsCanonical ? "true" : "false")"
        ].joined(separator: ";")
    }

    private func formatRatio(_ value: Double) -> String {
        String(format: "%.3g", value)
    }
}

public struct PatternDetectionParameterSettings: Hashable, Sendable {
    public var classicBurstContrastMin: Double
    public var classicBurstFlankPauseContrastMin: Double
    public var classicBurstMinSpikes: Int
    public var classicBurstMaxSpikes: Int
    public var pauseMinISISecOverride: Double?

    public init(
        classicBurstContrastMin: Double = 3.0,
        classicBurstFlankPauseContrastMin: Double = 5.0,
        classicBurstMinSpikes: Int = 3,
        classicBurstMaxSpikes: Int = 9,
        pauseMinISISecOverride: Double? = nil
    ) {
        self.classicBurstContrastMin = classicBurstContrastMin.isFinite && classicBurstContrastMin > 0
            ? max(1, classicBurstContrastMin)
            : 3.0
        self.classicBurstFlankPauseContrastMin = classicBurstFlankPauseContrastMin.isFinite && classicBurstFlankPauseContrastMin > 0
            ? max(self.classicBurstContrastMin, classicBurstFlankPauseContrastMin)
            : 5.0
        self.classicBurstMinSpikes = max(3, classicBurstMinSpikes)
        self.classicBurstMaxSpikes = max(self.classicBurstMinSpikes, classicBurstMaxSpikes)
        self.pauseMinISISecOverride = pauseMinISISecOverride.flatMap { value in
            value.isFinite && value > 0 ? value : nil
        }
    }

    public static let defaults = PatternDetectionParameterSettings()
}

public struct HybridPatternDetectionFrameworkPlan: Hashable, Sendable {
    public let name: String
    public let version: String
    public let stages: [HybridPatternDetectionStage]
    public let policy: HybridPatternDetectionPolicy

    public init(
        name: String = "Structure-first adaptive ISI framework",
        version: String = HybridPatternDetectionFramework.version,
        stages: [HybridPatternDetectionStage] = HybridPatternDetectionStage.allCases,
        policy: HybridPatternDetectionPolicy = .structureFirstAdaptiveISI
    ) {
        self.name = name
        self.version = version
        self.stages = stages
        self.policy = policy
    }

    public var decisionPathTag: String {
        let stageText = stages.map(\.rawValue).joined(separator: "|")
        return "hybrid_framework=\(version);\(policy.decisionPathTag);framework_stages=\(stageText)"
    }
}

public enum HybridPatternDetectionFramework {
    public static let version = "structure_first_adaptive_isi_v1"

    public static var plan: HybridPatternDetectionFrameworkPlan {
        HybridPatternDetectionFrameworkPlan()
    }

    public static func run(
        dataset: SpikeDataset,
        bandSettings: TrainAdaptiveBandSettings = TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        refractoryAction: ClassicAnchorRefractoryAction = .warnOnly,
        stateTuning: StatePatternDetectorTuning = StatePatternDetectorTuning(),
        detectorParameters: PatternDetectionParameterSettings = .defaults
    ) -> ClassicAnchorDetectionRun {
        let run = ClassicAnchorDetectionPipeline.run(
            dataset: dataset,
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            refractoryAction: refractoryAction,
            stateTuning: stateTuning,
            detectorParameters: detectorParameters,
            frameworkPolicy: plan.policy
        )
        return tagRun(run, plan: plan)
    }

    private static func tagRun(
        _ run: ClassicAnchorDetectionRun,
        plan: HybridPatternDetectionFrameworkPlan
    ) -> ClassicAnchorDetectionRun {
        let taggedResults = run.results.map { result in
            ClassicAnchorDetectionResult(
                trainID: result.trainID,
                trainName: result.trainName,
                candidates: result.candidates.map { tagCandidate($0, plan: plan) },
                hfsBurstArbitrationAuditRows: result.hfsBurstArbitrationAuditRows
            )
        }

        return ClassicAnchorDetectionRun(
            bandSettings: run.bandSettings,
            qualitySettings: run.qualitySettings,
            resolutions: run.resolutions,
            results: taggedResults,
            datasetStructuralSeedSummary: run.datasetStructuralSeedSummary,
            performanceReport: run.performanceReport
        )
    }

    private static func tagCandidate(
        _ candidate: ClassicAnchorCandidate,
        plan: HybridPatternDetectionFrameworkPlan
    ) -> ClassicAnchorCandidate {
        guard !candidate.decisionPath.contains("hybrid_framework=") else {
            return candidate
        }
        let trimmed = candidate.decisionPath.trimmingCharacters(in: .whitespacesAndNewlines)
        let decisionPath = trimmed.isEmpty ? plan.decisionPathTag : "\(candidate.decisionPath);\(plan.decisionPathTag)"
        return candidate.withDiagnosticOverride(
            decisionPath: decisionPath,
            selectedForAuto: candidate.selectedForAuto,
            selectionStatus: candidate.selectionStatus
        )
    }
}

public extension ClassicAnchorSettings {
    func applyingHybridPolicy(_ policy: HybridPatternDetectionPolicy) -> ClassicAnchorSettings {
        var tuned = self
        tuned.burstContrastMin = policy.classicBurstContrastMin
        tuned.burstContrastGeomMin = policy.classicBurstContrastGeomMin
        tuned.classicBurstFlankPauseContrastMin = policy.classicBurstFlankPauseContrastMin
        tuned.possibleBurstContrastMin = policy.possibleBurstContrastMin
        tuned.possibleBurstContrastGeomMin = policy.possibleBurstContrastGeomMin
        tuned.burstOneSidedAsCanonical = policy.oneSidedBurstAsCanonical
        return tuned
    }

    func applyingDetectorParameters(_ parameters: PatternDetectionParameterSettings) -> ClassicAnchorSettings {
        var tuned = self
        tuned.burstContrastMin = parameters.classicBurstContrastMin
        tuned.burstContrastGeomMin = parameters.classicBurstContrastMin
        tuned.classicBurstFlankPauseContrastMin = parameters.classicBurstFlankPauseContrastMin
        tuned.minSpikes = parameters.classicBurstMinSpikes
        tuned.classicMaxSpikes = max(parameters.classicBurstMinSpikes, parameters.classicBurstMaxSpikes)
        tuned.longMinSpikes = max(tuned.classicMaxSpikes + 1, tuned.longMinSpikes)
        tuned.burstCoreMinISI = max(1, parameters.classicBurstMinSpikes - 1)
        return tuned
    }
}
