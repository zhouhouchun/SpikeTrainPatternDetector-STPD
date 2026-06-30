import Foundation

public enum ClassicAnchorLabel: String, CaseIterable, Sendable {
    case burst
    case highFrequencyBurst = "high_frequency_burst"
    case longBurst = "long_burst"
    case possibleBurst = "possible_burst"
    case tonic
    case highFrequencyTonic = "high_frequency_tonic"
    case highFrequencySpiking = "high_frequency_spiking"
    case pause
    case reject
    case profile

    public static let allCases: [ClassicAnchorLabel] = [
        .burst,
        .highFrequencyBurst,
        .longBurst,
        .possibleBurst,
        .tonic,
        .highFrequencyTonic,
        .highFrequencySpiking,
        .pause,
        .reject
    ]
}

public enum ClassicAnchorBandSource: String, Sendable {
    case userPatternISILimit = "user_pattern_isi_limit"
    case eventGrammarSeedBand = "event_grammar_seed_band"
    case structure = "structure"
    case none
}

public extension ClassicAnchorLabel {
    var isCanonicalBurstFamily: Bool {
        self == .burst || self == .highFrequencyBurst || self == .longBurst
    }

    var isBurstEventFamily: Bool {
        isCanonicalBurstFamily || self == .possibleBurst
    }
}

public enum ClassicAnchorLockLevel: String, Sendable {
    case lockedClassic = "locked_classic"
    case strongCandidate = "strong_candidate"
    case auditOnly = "audit_only"
}

public enum ClassicAnchorRefractoryAction: String, Sendable {
    case warnOnly = "warn_only"
    case excludeCandidate = "exclude_candidate"
    case reject
    case demoteToPossible = "demote_to_possible"
    case review
    case markMultiunitContamination = "mark_multiunit_contamination"
}

public struct ClassicAnchorSettings: Hashable, Sendable {
    public var isEnabled: Bool
    public var isBurstEnabled: Bool
    public var enabledLabels: Set<ClassicAnchorLabel>
    public var minValidISISec: Double
    public var burstBandLowerSec: Double
    public var burstBandUpperSec: Double
    public var burstBridgeUpperSec: Double?
    public var burstBandSource: ClassicAnchorBandSource
    public var burstBandIsStructureDerived: Bool
    public var burstCoreMinISI: Int
    public var burstBridgeMaxCount: Int
    public var burstBridgeFractionMax: Double
    public var minSpikes: Int
    public var classicMaxSpikes: Int
    public var longMinSpikes: Int
    public var longMaxSpikes: Int
    public var highFrequencyBurstMinSpikes: Int
    public var highFrequencyBurstMaxSpikes: Int
    public var highFrequencyBurstEdgeMin: Double
    public var highFrequencyBurstContextContrastMin: Double
    public var highFrequencyBurstCoreQ90MaxSec: Double
    public var burstContrastMin: Double
    public var burstContrastGeomMin: Double
    public var classicBurstFlankPauseContrastMin: Double
    public var possibleBurstContrastMin: Double
    public var possibleBurstContrastGeomMin: Double
    public var burstMaxExpansionSteps: Int
    public var burstOneSidedAsCanonical: Bool
    public var burstOneSidedContrastMin: Double?
    public var burstOneSidedSeedPurityMin: Double
    public var burstOneSidedBridgeFractionMax: Double
    public var burstStructuralRescueCompressionMin: Double
    public var burstStructuralRescueStrongCompressionMin: Double
    public var burstStructuralRescueSeedPurityMin: Double
    public var burstStructuralRescueCVMax: Double
    public var burstEpisodeBridgeFactor: Double
    public var burstEpisodeBackgroundFraction: Double
    public var burstEpisodeMinISI: Int
    public var burstEpisodeMinSeedISI: Int
    public var burstEpisodeSeedFractionMin: Double
    public var burstEpisodeBridgeFractionMin: Double
    public var burstEpisodeLowISIFractionMin: Double
    public var burstEpisodeCVMax: Double
    public var burstEpisodeClassicMaxSpikes: Int
    public var burstStrictQ95BridgeGate: Bool
    public var burstQ95SoftSevereRatio: Double
    public var burstHardThresholdEnabled: Bool
    public var burstHardThresholdSource: String
    /// Automatic/structural seed upper retained as the compact-core reference even when a manual
    /// hard gate widens the legal burst ceiling. `nil` means use `effectiveBurstBandUpperSec`.
    public var burstCoreReferenceUpperSec: Double?
    public var structuralBurstSupportWeight: Double
    public var refractorySuspectSec: Double
    public var refractoryAction: ClassicAnchorRefractoryAction

    public init(
        isEnabled: Bool = true,
        isBurstEnabled: Bool = true,
        enabledLabels: Set<ClassicAnchorLabel> = Set(ClassicAnchorLabel.allCases),
        minValidISISec: Double = 0.0009,
        burstBandLowerSec: Double = 0.001,
        burstBandUpperSec: Double = 0.010,
        burstBridgeUpperSec: Double? = nil,
        burstBandSource: ClassicAnchorBandSource = .none,
        burstBandIsStructureDerived: Bool = false,
        burstCoreMinISI: Int = 2,
        burstBridgeMaxCount: Int = 4,
        burstBridgeFractionMax: Double = 0.60,
        minSpikes: Int = 3,
        classicMaxSpikes: Int = 9,
        longMinSpikes: Int = 10,
        longMaxSpikes: Int = 16,
        highFrequencyBurstMinSpikes: Int = 3,
        highFrequencyBurstMaxSpikes: Int = 15,
        highFrequencyBurstEdgeMin: Double = 3.0,
        highFrequencyBurstContextContrastMin: Double = 3.0,
        highFrequencyBurstCoreQ90MaxSec: Double = 0.015,
        burstContrastMin: Double = 3.0,
        burstContrastGeomMin: Double? = nil,
        classicBurstFlankPauseContrastMin: Double = 5.0,
        possibleBurstContrastMin: Double = 2.0,
        possibleBurstContrastGeomMin: Double? = nil,
        burstMaxExpansionSteps: Int = 4,
        burstOneSidedAsCanonical: Bool = true,
        burstOneSidedContrastMin: Double? = 3.0,
        burstOneSidedSeedPurityMin: Double = 0.65,
        burstOneSidedBridgeFractionMax: Double? = nil,
        burstStructuralRescueCompressionMin: Double = 3.0,
        burstStructuralRescueStrongCompressionMin: Double = 4.0,
        burstStructuralRescueSeedPurityMin: Double = 0.70,
        burstStructuralRescueCVMax: Double = 0.80,
        burstEpisodeBridgeFactor: Double = 1.75,
        burstEpisodeBackgroundFraction: Double = 0.35,
        burstEpisodeMinISI: Int = 3,
        burstEpisodeMinSeedISI: Int = 1,
        burstEpisodeSeedFractionMin: Double = 0.18,
        burstEpisodeBridgeFractionMin: Double = 0.55,
        burstEpisodeLowISIFractionMin: Double = 0.85,
        burstEpisodeCVMax: Double = 0.55,
        burstEpisodeClassicMaxSpikes: Int = 9,
        burstStrictQ95BridgeGate: Bool = false,
        burstQ95SoftSevereRatio: Double = 1.35,
        burstHardThresholdEnabled: Bool = false,
        burstHardThresholdSource: String = "ui_isi_profile_threshold_line",
        burstCoreReferenceUpperSec: Double? = nil,
        structuralBurstSupportWeight: Double = 0,
        refractorySuspectSec: Double = 0.001,
        refractoryAction: ClassicAnchorRefractoryAction = .warnOnly
    ) {
        self.isEnabled = isEnabled
        self.isBurstEnabled = isBurstEnabled
        self.enabledLabels = enabledLabels
        self.minValidISISec = minValidISISec
        self.burstBandLowerSec = burstBandLowerSec
        self.burstBandUpperSec = burstBandUpperSec
        self.burstBridgeUpperSec = burstBridgeUpperSec
        self.burstBandSource = burstBandSource
        self.burstBandIsStructureDerived = burstBandIsStructureDerived
        self.burstCoreMinISI = max(1, burstCoreMinISI)
        self.burstBridgeMaxCount = max(0, burstBridgeMaxCount)
        self.burstBridgeFractionMax = min(max(0, burstBridgeFractionMax), 1)
        self.minSpikes = max(3, minSpikes)
        self.classicMaxSpikes = max(max(3, minSpikes), classicMaxSpikes)
        self.longMinSpikes = max(self.classicMaxSpikes + 1, longMinSpikes)
        let canonicalLongBurstMaxSpikes = 16
        let requestedLongMaxSpikes = longMaxSpikes > 0 ? longMaxSpikes : canonicalLongBurstMaxSpikes
        self.longMaxSpikes = max(
            self.longMinSpikes,
            min(requestedLongMaxSpikes, canonicalLongBurstMaxSpikes)
        )
        self.highFrequencyBurstMinSpikes = max(self.minSpikes, highFrequencyBurstMinSpikes)
        self.highFrequencyBurstMaxSpikes = max(self.highFrequencyBurstMinSpikes, highFrequencyBurstMaxSpikes)
        self.highFrequencyBurstEdgeMin = max(1, highFrequencyBurstEdgeMin)
        self.highFrequencyBurstContextContrastMin = max(1, highFrequencyBurstContextContrastMin)
        self.highFrequencyBurstCoreQ90MaxSec = max(self.minValidISISec, highFrequencyBurstCoreQ90MaxSec)
        self.burstContrastMin = max(1, burstContrastMin)
        self.burstContrastGeomMin = max(1, burstContrastGeomMin ?? burstContrastMin)
        self.classicBurstFlankPauseContrastMin = max(self.burstContrastMin, classicBurstFlankPauseContrastMin)
        self.possibleBurstContrastMin = max(1, possibleBurstContrastMin)
        self.possibleBurstContrastGeomMin = max(1, possibleBurstContrastGeomMin ?? possibleBurstContrastMin)
        self.burstMaxExpansionSteps = max(0, burstMaxExpansionSteps)
        self.burstOneSidedAsCanonical = burstOneSidedAsCanonical
        self.burstOneSidedContrastMin = burstOneSidedContrastMin.map { max(1, $0) }
        self.burstOneSidedSeedPurityMin = min(max(0, burstOneSidedSeedPurityMin), 1)
        self.burstOneSidedBridgeFractionMax = min(
            max(0, burstOneSidedBridgeFractionMax ?? min(0.35, self.burstBridgeFractionMax)),
            1
        )
        self.burstStructuralRescueCompressionMin = max(1, burstStructuralRescueCompressionMin)
        self.burstStructuralRescueStrongCompressionMin = max(
            self.burstStructuralRescueCompressionMin,
            burstStructuralRescueStrongCompressionMin
        )
        self.burstStructuralRescueSeedPurityMin = min(max(0, burstStructuralRescueSeedPurityMin), 1)
        self.burstStructuralRescueCVMax = max(0, burstStructuralRescueCVMax)
        self.burstEpisodeBridgeFactor = max(1, burstEpisodeBridgeFactor)
        self.burstEpisodeBackgroundFraction = min(max(0, burstEpisodeBackgroundFraction), 1)
        self.burstEpisodeMinISI = max(3, burstEpisodeMinISI)
        self.burstEpisodeMinSeedISI = max(1, burstEpisodeMinSeedISI)
        self.burstEpisodeSeedFractionMin = min(max(0, burstEpisodeSeedFractionMin), 1)
        self.burstEpisodeBridgeFractionMin = min(max(0, burstEpisodeBridgeFractionMin), 1)
        self.burstEpisodeLowISIFractionMin = max(
            self.burstEpisodeBridgeFractionMin,
            min(max(0, burstEpisodeLowISIFractionMin), 1)
        )
        self.burstEpisodeCVMax = max(0, burstEpisodeCVMax)
        self.burstEpisodeClassicMaxSpikes = max(self.classicMaxSpikes, burstEpisodeClassicMaxSpikes)
        self.burstStrictQ95BridgeGate = burstStrictQ95BridgeGate
        self.burstQ95SoftSevereRatio = max(1, burstQ95SoftSevereRatio)
        self.burstHardThresholdEnabled = burstHardThresholdEnabled
        let trimmedHardThresholdSource = burstHardThresholdSource.trimmingCharacters(in: .whitespacesAndNewlines)
        self.burstHardThresholdSource = trimmedHardThresholdSource.isEmpty
            ? "ui_isi_profile_threshold_line"
            : trimmedHardThresholdSource
        if let burstCoreReferenceUpperSec,
           burstCoreReferenceUpperSec.isFinite,
           burstCoreReferenceUpperSec > 0 {
            self.burstCoreReferenceUpperSec = max(self.minValidISISec, burstCoreReferenceUpperSec)
        } else {
            self.burstCoreReferenceUpperSec = nil
        }
        self.structuralBurstSupportWeight = min(
            1,
            max(0, structuralBurstSupportWeight.isFinite ? structuralBurstSupportWeight : 0)
        )
        self.refractorySuspectSec = refractorySuspectSec
        self.refractoryAction = refractoryAction
    }

    public init(
        qualitySettings: SpikeQualitySettings,
        burstBandLowerSec: Double = 0.001,
        burstBandUpperSec: Double = 0.010,
        burstBridgeUpperSec: Double? = nil,
        burstBandSource: ClassicAnchorBandSource = .none,
        burstBandIsStructureDerived: Bool = false
    ) {
        self.init(
            minValidISISec: qualitySettings.artifactThresholdSec,
            burstBandLowerSec: burstBandLowerSec,
            burstBandUpperSec: burstBandUpperSec,
            burstBridgeUpperSec: burstBridgeUpperSec,
            burstBandSource: burstBandSource,
            burstBandIsStructureDerived: burstBandIsStructureDerived,
            refractorySuspectSec: qualitySettings.refractorySuspectThresholdSec
        )
    }

    public var effectiveBurstBandLowerSec: Double {
        max(minValidISISec, burstBandLowerSec)
    }

    public var effectiveBurstBandUpperSec: Double {
        max(effectiveBurstBandLowerSec, burstBandUpperSec)
    }

    public var effectiveBurstBridgeUpperSec: Double {
        max(effectiveBurstBandUpperSec, burstBridgeUpperSec ?? burstBandUpperSec)
    }

    public var canUseBurstSeedBandForDetection: Bool {
        burstBandIsStructureDerived || burstBandSource == .userPatternISILimit
    }
}

public struct ClassicAnchorCandidate: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainID: String
    public let trainName: String
    public let candidateLayer: String
    public let candidateClass: String
    public let finalLabel: ClassicAnchorLabel
    public let gateStatus: String
    public let decisionPath: String
    public let action: String
    public let score: Double
    public let priority: Int
    public let selectedForAuto: Bool
    public let selectionStatus: String
    // ISI index i denotes the interval spanning 1-based spikes i...(i + 1).
    public let startISIIndex: Int
    public let endISIIndex: Int
    public let startSpikeIndex: Int
    public let endSpikeIndex: Int
    public let nISI: Int
    public let nValidISI: Int
    public let nSpikes: Int
    public let durationSec: Double?
    public let intraQ10Sec: Double?
    public let intraQ40Sec: Double?
    public let intraQ50Sec: Double?
    public let intraQ90Sec: Double?
    public let intraQ95Sec: Double?
    public let maxIntraISISec: Double?
    public let meanIntraISISec: Double?
    public let cv: Double?
    public var cv2: Double? = nil
    public let lv: Double?
    public let preGapSec: Double?
    public let postGapSec: Double?
    public let preRatioQ90: Double?
    public let postRatioQ90: Double?
    public let edgeContrastMinQ90: Double?
    public let edgeContrastGeomQ90: Double?
    public let anchorFamily: String
    public let anchorLockLevel: ClassicAnchorLockLevel
    public let anchorBandLowerSec: Double
    public let anchorBandUpperSec: Double
    public let anchorBandSource: ClassicAnchorBandSource
    public let anchorContrastMinRequired: Double
    public let anchorContrastGeomRequired: Double
    public let refractorySuspectCount: Int
    public let refractorySuspectAction: ClassicAnchorRefractoryAction?
    public var profileSeedLowPercentileInTrain: Double? = nil
    public var profileSeedHighPercentileInTrain: Double? = nil
    public var profileSeedBandFraction: Double? = nil
    public var profileSeedRunCount: Int? = nil
    public var profileMaxSeedRunLength: Int? = nil
    public var profileMedianISISec: Double? = nil
    public var profileQ10ISISec: Double? = nil
    public var profileQ25ISISec: Double? = nil
    public var profileQ90ISISec: Double? = nil
    public var profilePauseFraction: Double? = nil
    public var profilePhenotypePrior: String? = nil
    public var profileBridgeUpperSec: Double? = nil
    public var profileBoundaryFloorSec: Double? = nil
    public var profileBoundaryFloorHard: Bool? = nil
    public var profileBurstContrastS: Double? = nil
    public var profilePossibleContrastS: Double? = nil
    public var hfSpikingQ80Sec: Double? = nil
    public var hfSpikingQ80MaxSec: Double? = nil
    public var hfSpikingQ90MaxSec: Double? = nil
    public var hfSpikingShortUpperSec: Double? = nil
    public var hfSpikingEpochBridgeSec: Double? = nil
    public var hfSpikingToleratedGapSec: Double? = nil
    public var hfSpikingPatternMaxISISec: Double? = nil
    public var hfSpikingPauseBreakSec: Double? = nil
    public var hfSpikingShortFraction: Double? = nil
    public var hfSpikingQ90ShortFraction: Double? = nil
    public var hfSpikingBridgeFraction: Double? = nil
    public var hfSpikingLargeFraction: Double? = nil
    public var hfSpikingToleratedFraction: Double? = nil
    public var hfSpikingMaxConsecutiveLargeISI: Int? = nil
    public var hfSpikingMinSpikesRequired: Int? = nil
    public var hfSpikingAcceptanceRoute: String? = nil
    public var hfSpikingBurstDominated: Bool? = nil
    public var hfSpikingEmbeddedBurstCount: Int? = nil
    public var hfSpikingEmbeddedBurstGroupCount: Int? = nil
    public var hfSpikingEmbeddedBurstCoverage: Double? = nil
    public var hfSpikingBurstPacketLike: Bool? = nil
    public var hfSpikingBurstPacketNeighbor: Bool? = nil
    public var suppressedByHFSpikingState: Bool? = nil
    public var suppressedOriginalLabel: String? = nil
    public var hfSpikingSuppressorID: String? = nil
    public var stateRegularityScore: Double? = nil
    public var stateBurstSeedFraction: Double? = nil
    public var stateLowTailFraction: Double? = nil
    public var stateLocalStabilityScore: Double? = nil
    public var stateCoreBurstRunLength: Int? = nil
    /// Tonic-family subtype for state candidates whose `finalLabel` is `.tonic` or
    /// `.highFrequencyTonic`: one of `"classic"`, `"irregular"`, `"high_frequency"`.
    /// Auditable only — `finalLabel` is unchanged, so arbitration/UI/CSV consumers keep
    /// working; this field exposes the biological tonic-family distinction.
    public var stateTonicSubtype: String? = nil
    /// High-frequency family subtype, additive and `finalLabel`-preserving. One of:
    /// `"hf_tonic_spiking"` (sustained regular HF, i.e. `.highFrequencyTonic`),
    /// `"hf_irregular_spiking"` (sustained irregular HF state, i.e. non-burst-dominated
    /// `.highFrequencySpiking`), `"hf_burst_dominant"` (HF state dominated by burst packets),
    /// or `"hf_burst_packet"` (a local burst-family event inside a high-frequency envelope —
    /// it stays a burst event; this is audit/provenance only). `nil` for non-HF candidates.
    public var stateHighFrequencySubtype: String? = nil
    public var stateTrainPercentileMedian: Double? = nil
    public var stateLocalPercentileMedian: Double? = nil
    public var stateLocalPercentileQ90: Double? = nil
    public var stateLocalRobustZMedian: Double? = nil
    public var stateLocalRobustZAbsQ80: Double? = nil
    public var stateLocalRobustZQ10: Double? = nil
    public var burstSeedRunStartISI: Int? = nil
    public var burstSeedRunEndISI: Int? = nil
    public var burstSeedBandLowerSec: Double? = nil
    public var burstSeedBandUpperSec: Double? = nil
    public var burstBridgeBandUpperSec: Double? = nil
    public var burstContrastRequired: Double? = nil
    public var burstPossibleContrastRequired: Double? = nil
    public var burstRequiredGapSec: Double? = nil
    public var burstPossibleRequiredGapSec: Double? = nil
    public var burstBoundaryFloorSec: Double? = nil
    public var burstBoundaryFloorHard: Bool? = nil
    public var burstStrictBoundaryPass: Bool? = nil
    public var burstPossibleBoundaryPass: Bool? = nil
    public var burstBridgeCountPass: Bool? = nil
    public var burstBridgeFractionPass: Bool? = nil
    public var burstQ90BridgePass: Bool? = nil
    public var burstSizeLabelBeforeReview: String? = nil
    public var thresholdMode: String? = nil
    public var hardThreshold: Bool? = nil
    public var hardThresholdPattern: String? = nil
    public var hardBurstSeedUpperSec: Double? = nil
    public var hardBurstBridgeUpperSec: Double? = nil
    public var hardBurstCoreISICount: Int? = nil
    public var hardThresholdSource: String? = nil
    public var localBackgroundQ75Sec: Double? = nil
    public var localCompressionQ90Ratio: Double? = nil
    public var eventLocalMedianSec: Double? = nil
    public var eventLocalPercentileMedian: Double? = nil
    public var eventLocalPercentileQ90: Double? = nil
    public var eventLocalRobustZMedian: Double? = nil
    public var eventLocalRobustZAbsQ80: Double? = nil
    public var eventLocalRobustZQ10: Double? = nil
}

public struct ClassicAnchorDetectionResult: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let candidates: [ClassicAnchorCandidate]
    public let hfsBurstArbitrationAuditRows: [HFSBurstArbitrationAuditRow]

    public init(
        trainID: String,
        trainName: String,
        candidates: [ClassicAnchorCandidate],
        hfsBurstArbitrationAuditRows: [HFSBurstArbitrationAuditRow] = []
    ) {
        self.trainID = trainID
        self.trainName = trainName
        self.candidates = candidates
        self.hfsBurstArbitrationAuditRows = hfsBurstArbitrationAuditRows
    }
}

public enum ClassicAnchorDetector {
    public static func detect(
        train: SpikeTrain,
        settings: ClassicAnchorSettings = ClassicAnchorSettings()
    ) -> ClassicAnchorDetectionResult {
        ClassicAnchorDetectionResult(
            trainID: train.id,
            trainName: train.name,
            // BCB-1: core-first boundary trim runs on every generated burst candidate before arbitration (default path).
            candidates: coreFirstBoundaryTrimmedCandidates(
                detectBurstCandidates(train: train, settings: settings),
                train: train, settings: settings)
        )
    }

    public static func detect(
        dataset: SpikeDataset,
        settings: ClassicAnchorSettings = ClassicAnchorSettings()
    ) -> [ClassicAnchorDetectionResult] {
        dataset.trains.map { detect(train: $0, settings: settings) }
    }

    public static func auditRows(
        dataset: SpikeDataset,
        settings: ClassicAnchorSettings = ClassicAnchorSettings()
    ) -> [ClassicAnchorCandidate] {
        detect(dataset: dataset, settings: settings).flatMap(\.candidates)
    }

    private static func detectBurstCandidates(
        train: SpikeTrain,
        settings: ClassicAnchorSettings
    ) -> [ClassicAnchorCandidate] {
        guard settings.isEnabled,
              settings.isBurstEnabled,
              settings.enabledLabels.contains(.burst) ||
                  settings.enabledLabels.contains(.highFrequencyBurst) ||
                  settings.enabledLabels.contains(.longBurst) ||
                  settings.enabledLabels.contains(.possibleBurst),
              train.spikeCount > 3 else {
            return []
        }

        let lower = settings.effectiveBurstBandLowerSec
        let upper = settings.effectiveBurstBandUpperSec
        let bridgeUpper = settings.effectiveBurstBridgeUpperSec
        let validFlag = validBurstFlag(train: train, minValidISISec: settings.minValidISISec)
        let canUseSeedBand = settings.canUseBurstSeedBandForDetection
        var seedFlag = Array(repeating: false, count: train.isiSec.count)
        var bridgeFlag = Array(repeating: false, count: train.isiSec.count)

        if canUseSeedBand {
            for index in train.isiSec.indices where index > 0 {
                guard let isi = train.isiSec[index],
                      isi.isFinite,
                      !isArtifactISI(isi, threshold: settings.minValidISISec) else {
                    continue
                }
                seedFlag[index] = isi >= lower && isi <= upper
                bridgeFlag[index] = isi <= bridgeUpper
            }
        }

        let seedRuns = boolRuns(seedFlag)
        var candidates = structureFirstClassicBurstCandidates(
            train: train,
            settings: settings,
            validFlag: validFlag,
            startingCandidateIndex: 1
        )
        candidates.append(contentsOf: classicAnchorBurstCandidates(
            train: train,
            settings: settings,
            seedFlag: seedFlag,
            lower: lower,
            upper: upper
        ))
        candidates.append(
            contentsOf: hardThresholdBurstCandidates(
                train: train,
                settings: settings,
                startingCandidateIndex: candidates.count + 1
            )
        )
        var seenRuns = Set(candidates.map { "\($0.startISIIndex)_\($0.endISIIndex)" })
        var runs: [BurstRun] = []
        runs.reserveCapacity(seedRuns.count)

        guard canUseSeedBand else {
            // No usable burst seed band (source=none / collapsed band). The seed-centred routes
            // cannot run, but the adaptive local HF burst-packet route still can: it falls back to
            // a local-background-derived core ceiling (see adaptiveLocalHFBurstPacketCandidates), so
            // the screenshot false negatives in no-structure trains are recovered.
            candidates.append(
                contentsOf: adaptiveLocalHFBurstPacketCandidates(
                    train: train,
                    settings: settings,
                    validFlag: validFlag,
                    seenRuns: &seenRuns,
                    lower: lower,
                    upper: upper,
                    bridgeUpper: bridgeUpper,
                    startingCandidateIndex: candidates.count + 1
                )
            )
            return candidates
        }

        for seedRun in seedRuns {
            let seedCount = (seedRun.start...seedRun.end).filter { seedFlag[$0] }.count
            guard seedCount >= settings.burstCoreMinISI else {
                continue
            }
            let lefts = leftExtensions(
                seedStart: seedRun.start,
                bridgeFlag: bridgeFlag,
                maxSteps: settings.burstMaxExpansionSteps
            )
            let rights = rightExtensions(
                seedEnd: seedRun.end,
                bridgeFlag: bridgeFlag,
                maxSteps: settings.burstMaxExpansionSteps
            )

            for start in lefts {
                for end in rights where start <= end {
                    let key = "\(start)_\(end)"
                    guard seenRuns.insert(key).inserted else {
                        continue
                    }
                    let spanSeedCount = (start...end).filter { seedFlag[$0] }.count
                    guard spanSeedCount >= settings.burstCoreMinISI else {
                        continue
                    }
                    let bridgeCount = (start...end).filter { bridgeFlag[$0] && !seedFlag[$0] }.count
                    runs.append(
                        BurstRun(
                            start: start,
                            end: end,
                            seedStart: seedRun.start,
                            seedEnd: seedRun.end,
                            seedCount: spanSeedCount,
                            bridgeCount: bridgeCount
                        )
                    )
                }
            }
        }
        let trainBackgroundQ75 = quantile(
            train.isiSec.indices.compactMap { index -> Double? in
                guard index > 0,
                      let isi = train.isiSec[index],
                      isi.isFinite,
                      !isArtifactISI(isi, threshold: settings.minValidISISec) else {
                    return nil
                }
                return isi
            },
            probability: 0.75
        )

        candidates.reserveCapacity(candidates.count + runs.count)
        for run in runs {
            let start = run.start
            let end = run.end
            let seedCount = run.seedCount
            let bridgeCount = run.bridgeCount
            let spanISI = end - start + 1
            let bridgeFraction = Double(bridgeCount) / Double(max(1, spanISI))
            guard seedCount >= settings.burstCoreMinISI else {
                continue
            }
            let spikeCount = end - start + 2
            guard spikeCount >= settings.minSpikes else {
                continue
            }
            if spikeCount > settings.classicMaxSpikes {
                let longSized = spikeCount >= settings.longMinSpikes &&
                    (settings.longMaxSpikes <= 0 || spikeCount <= settings.longMaxSpikes)
                guard longSized else {
                    continue
                }
            }

            guard let metrics = spanMetrics(train: train, start: start, end: end, settings: settings) else {
                continue
            }
            let seedPurity = Double(seedCount) / Double(max(1, metrics.nValidISI))
            let edgeMin = metrics.edgeContrastMinQ90
            let decisionEdgeGeom = metrics.edgeContrastGeomQ90 ?? metrics.edgeContrastMinQ90
            let q90Pass = finiteLessThanOrEqual(metrics.intraQ90Sec, bridgeUpper)
            let q95RawPass = finiteLessThanOrEqual(metrics.intraQ95Sec, bridgeUpper)
            let q95ExcessRatio = ratio(metrics.intraQ95Sec, over: bridgeUpper)
            let q95Severe = (q95ExcessRatio ?? -.infinity) > settings.burstQ95SoftSevereRatio
            let q95GatePass = q95RawPass || (!settings.burstStrictQ95BridgeGate && !q95Severe)
            let q95Penalty = (q95ExcessRatio ?? 0) > 1
                ? 0.45 * min(2, (q95ExcessRatio ?? 1) - 1)
                : 0

            let hasPreGap = metrics.preGapSec?.isFinite == true
            let hasPostGap = metrics.postGapSec?.isFinite == true
            let isTwoSided = hasPreGap && hasPostGap
            let isStartBoundary = !hasPreGap && hasPostGap && start == 1
            let isEndBoundary = hasPreGap && !hasPostGap && end == train.isiSec.count - 1
            let isBoundaryBurst = isStartBoundary || isEndBoundary
            let boundarySide = isStartBoundary ? "start" : (isEndBoundary ? "end" : "none")
            let hasEligibleBoundary = isTwoSided || isBoundaryBurst

            let strict = (edgeMin ?? -.infinity) >= settings.burstContrastMin &&
                (decisionEdgeGeom ?? -.infinity) >= settings.burstContrastGeomMin
            let possible = (edgeMin ?? -.infinity) >= settings.possibleBurstContrastMin &&
                (decisionEdgeGeom ?? -.infinity) >= settings.possibleBurstContrastGeomMin
            let oneSidedContrastMin = settings.burstOneSidedContrastMin ?? (settings.burstContrastMin + 0.5)
            let oneSided = (metrics.preRatioQ90 ?? -.infinity) >= oneSidedContrastMin ||
                (metrics.postRatioQ90 ?? -.infinity) >= oneSidedContrastMin
            let oneSidedPossibleContrast = finiteMax(metrics.preRatioQ90, metrics.postRatioQ90)
            let oneSidedPossible = (oneSidedPossibleContrast ?? -.infinity) >= settings.possibleBurstContrastMin
            let strictBoundaryAuditPass = (isTwoSided || isBoundaryBurst) && strict
            let possibleBoundaryAuditPass = (isTwoSided && possible) ||
                (!isTwoSided && isBoundaryBurst && possible) ||
                (isTwoSided && oneSidedPossible)
            let cleanOneSided = oneSided &&
                seedPurity >= settings.burstOneSidedSeedPurityMin &&
                bridgeFraction <= settings.burstOneSidedBridgeFractionMax &&
                q90Pass &&
                q95GatePass
            let corePass = bridgeCount <= settings.burstBridgeMaxCount &&
                bridgeFraction <= settings.burstBridgeFractionMax &&
                q90Pass &&
                q95GatePass
            let localBackground = localBackgroundQ75(
                train: train,
                validFlag: validFlag,
                start: start,
                end: end
            )
            let structuralBackground = finiteMax(
                localBackground,
                metrics.preGapSec,
                metrics.postGapSec
            ) ?? trainBackgroundQ75
            let structuralRatio = ratio(structuralBackground, over: metrics.intraQ90Sec)
            let cvRescuePass = metrics.cv.map { !$0.isFinite || $0 <= settings.burstStructuralRescueCVMax } ?? true
            let structuralRescue = corePass &&
                seedPurity >= settings.burstStructuralRescueSeedPurityMin &&
                cvRescuePass &&
                (structuralRatio ?? -.infinity) >= settings.burstStructuralRescueCompressionMin
            let structuralRescueStrong = structuralRescue &&
                (structuralRatio ?? -.infinity) >= settings.burstStructuralRescueStrongCompressionMin

            var rejectReasons: [String] = []
            if bridgeCount > settings.burstBridgeMaxCount {
                rejectReasons.append("too_many_bridge_isis")
            }
            if bridgeFraction > settings.burstBridgeFractionMax {
                rejectReasons.append("bridge_fraction_too_high")
            }
            if !q90Pass {
                rejectReasons.append("intra_q90_exceeds_bridge_band")
            }
            if settings.burstStrictQ95BridgeGate && !q95RawPass {
                rejectReasons.append("intra_q95_exceeds_bridge_band_strict")
            }
            if !settings.burstStrictQ95BridgeGate && q95Severe {
                rejectReasons.append("intra_q95_severely_exceeds_bridge_band")
            }
            if !structuralRescue && (!hasEligibleBoundary || (!strict && !possible && !oneSided && !oneSidedPossible)) {
                rejectReasons.append("flank_contrast_fail")
            }
            if !rejectReasons.isEmpty {
                let rejectDecisionPath = (
                    rejectReasons +
                    burstRejectionContext(
                        metrics: metrics,
                        settings: settings,
                        seedPurity: seedPurity,
                        bridgeFraction: bridgeFraction,
                        localBackground: localBackground,
                        structuralRatio: structuralRatio,
                        q95ExcessRatio: q95ExcessRatio
                    )
                ).joined(separator: ";")
                candidates.append(
                    burstCandidate(
                        train: train,
                        metrics: metrics,
                        settings: settings,
                        label: .reject,
                        gateStatus: "event_grammar_reject",
                        decisionPath: rejectDecisionPath,
                        action: "reject",
                        score: 0,
                        priority: 0,
                        start: start,
                        end: end,
                        bridgeCount: bridgeCount,
                        lower: lower,
                        upper: upper,
                        lockLevel: .strongCandidate,
                        edgeGeom: decisionEdgeGeom,
                        candidateIndex: candidates.count + 1,
                        refCount: 0,
                        candidateLayer: "event_grammar_burst_event",
                        candidateClass: "event_grammar_seed_centered_burst",
                        seedRunStart: run.seedStart,
                        seedRunEnd: run.seedEnd,
                        bridgeBandUpper: bridgeUpper,
                        requiredGapSec: requiredGapSec(
                            intraQ90Sec: metrics.intraQ90Sec,
                            contrast: settings.burstContrastMin
                        ),
                        possibleRequiredGapSec: requiredGapSec(
                            intraQ90Sec: metrics.intraQ90Sec,
                            contrast: settings.possibleBurstContrastMin
                        ),
                        strictBoundaryPass: strictBoundaryAuditPass,
                        possibleBoundaryPass: possibleBoundaryAuditPass,
                        bridgeCountPass: bridgeCount <= settings.burstBridgeMaxCount,
                        bridgeFractionPass: bridgeFraction <= settings.burstBridgeFractionMax,
                        q90BridgePass: q90Pass,
                        sizeLabelBeforeReview: sizeLabelBeforeReview(
                            spikeCount: spikeCount,
                            settings: settings
                        ),
                        localBackgroundQ75Sec: localBackground,
                        localCompressionQ90Ratio: structuralRatio
                    )
                )
                continue
            }
            let acceptedEdgeMin = edgeMin ?? 1
            let acceptedEdgeGeom = decisionEdgeGeom ?? acceptedEdgeMin
            guard q90Pass && q95GatePass else {
                continue
            }

            var label = burstFamilyLabel(spikeCount: spikeCount, metrics: metrics, settings: settings)
            var candidateGateStatus = gateStatus(strict: strict, boundary: isBoundaryBurst, bridged: bridgeCount > 0)
            var candidateDecisionPath = decisionPath(strict: strict, boundary: isBoundaryBurst, bridged: bridgeCount > 0)
            var action = "accept"
            var priority: Int
            var lockLevel: ClassicAnchorLockLevel
            var candidateClass: String? = "event_grammar_seed_centered_burst"

            if corePass && strict && (isTwoSided || isBoundaryBurst) {
                if isTwoSided {
                    candidateGateStatus = "event_grammar_two_sided_burst_event_pass"
                    candidateDecisionPath = "event_grammar_two_sided_event_grammar_pass__\(label.rawValue)"
                    priority = burstFamilyPriority(label: label, classic: 1_250, highFrequency: 1_220, long: 1_160)
                } else {
                    priority = burstFamilyPriority(label: label, classic: 1_010, highFrequency: 990, long: 960)
                }
                lockLevel = .lockedClassic
            } else if corePass && cleanOneSided {
                if settings.burstOneSidedAsCanonical {
                    candidateGateStatus = "event_grammar_clean_one_sided_burst_event_pass_user_allowed"
                    candidateDecisionPath = "event_grammar_clean_one_sided_event_grammar_pass__\(label.rawValue)"
                    priority = burstFamilyPriority(label: label, classic: 980, highFrequency: 955, long: 930)
                    lockLevel = .lockedClassic
                } else {
                    label = .possibleBurst
                    candidateGateStatus = "event_grammar_clean_one_sided_possible_burst"
                    let possiblePriority = possibleBurstPriority(
                        metrics: metrics,
                        settings: settings,
                        coreCount: seedCount,
                        seedPurity: seedPurity,
                        bridgeFraction: bridgeFraction,
                        twoPossible: false,
                        oneSided: true,
                        q95ExcessRatio: q95ExcessRatio,
                        oneSidedContrast: oneSidedPossibleContrast
                    )
                    let possibleDecisionFields = [
                        "clean_one_sided_flank_contrast_pass_core_compact",
                        "evidence_class=clean_one_sided_strong_contrast",
                        "possible_burst_review_route=clean_one_sided",
                        "other_side_does_not_lock_classic_boundary",
                        "interpretation=possible_burst_by_clean_asymmetric_structure",
                        "pre_ratio=\(formatRatio(metrics.preRatioQ90))",
                        "post_ratio=\(formatRatio(metrics.postRatioQ90))",
                        "required=\(formatRatio(settings.possibleBurstContrastMin))",
                        "structural_burst_support_weight=\(formatRatio(settings.structuralBurstSupportWeight))",
                        "support_adjusted_priority=\(possiblePriority)"
                    ] + possibleBurstBoundaryAuditFields(
                        metrics: metrics,
                        bridgeUpper: bridgeUpper
                    )
                    candidateDecisionPath = possibleDecisionFields.joined(separator: ";")
                    action = "demote_to_possible"
                    priority = possiblePriority
                    lockLevel = .strongCandidate
                }
            } else if corePass && isTwoSided && !possible && oneSidedPossible {
                label = .possibleBurst
                candidateGateStatus = "event_grammar_one_sided_possible_burst"
                let possiblePriority = possibleBurstPriority(
                    metrics: metrics,
                    settings: settings,
                    coreCount: seedCount,
                    seedPurity: seedPurity,
                    bridgeFraction: bridgeFraction,
                    twoPossible: false,
                    oneSided: true,
                    q95ExcessRatio: q95ExcessRatio,
                    oneSidedContrast: oneSidedPossibleContrast
                )
                let possibleDecisionFields = [
                    "single_flank_possible_contrast_pass",
                    "evidence_class=two_sided_single_flank_possible",
                    "possible_burst_review_route=single_flank",
                    "other_side_does_not_lock_classic_boundary",
                    "interpretation=possible_burst_by_asymmetric_structure",
                    "pre_ratio=\(formatRatio(metrics.preRatioQ90))",
                    "post_ratio=\(formatRatio(metrics.postRatioQ90))",
                    "required=\(formatRatio(settings.possibleBurstContrastMin))",
                    "structural_burst_support_weight=\(formatRatio(settings.structuralBurstSupportWeight))",
                    "support_adjusted_priority=\(possiblePriority)"
                ] + possibleBurstBoundaryAuditFields(
                    metrics: metrics,
                    bridgeUpper: bridgeUpper
                )
                candidateDecisionPath = possibleDecisionFields.joined(separator: ";")
                action = "demote_to_possible"
                priority = possiblePriority
                lockLevel = .strongCandidate
            } else if corePass && possible {
                label = .possibleBurst
                if isBoundaryBurst {
                    candidateGateStatus = "event_grammar_possible_boundary_burst"
                    let possiblePriority = possibleBurstPriority(
                        metrics: metrics,
                        settings: settings,
                        coreCount: seedCount,
                        seedPurity: seedPurity,
                        bridgeFraction: bridgeFraction,
                        twoPossible: false,
                        oneSided: true,
                        q95ExcessRatio: q95ExcessRatio,
                        oneSidedContrast: oneSidedPossibleContrast
                    )
                    let possibleDecisionFields = [
                        "single_flank_boundary_possible_contrast_pass",
                        "evidence_class=endpoint_single_flank_possible",
                        "possible_burst_review_route=endpoint",
                        "boundary_side=\(boundarySide)",
                        "endpoint_missing_flank=true",
                        "missing_flank_allowed_for_endpoint_possible_burst=true",
                        "interpretation=possible_burst_by_endpoint_structure",
                        "pre_ratio=\(formatRatio(metrics.preRatioQ90))",
                        "post_ratio=\(formatRatio(metrics.postRatioQ90))",
                        "required=\(formatRatio(settings.possibleBurstContrastMin))",
                        "structural_burst_support_weight=\(formatRatio(settings.structuralBurstSupportWeight))",
                        "support_adjusted_priority=\(possiblePriority)"
                    ] + possibleBurstBoundaryAuditFields(
                        metrics: metrics,
                        bridgeUpper: bridgeUpper
                    )
                    candidateDecisionPath = possibleDecisionFields.joined(separator: ";")
                    priority = possiblePriority
                } else {
                    candidateGateStatus = "event_grammar_possible_two_sided_burst"
                    let possiblePriority = possibleBurstPriority(
                        metrics: metrics,
                        settings: settings,
                        coreCount: seedCount,
                        seedPurity: seedPurity,
                        bridgeFraction: bridgeFraction,
                        twoPossible: true,
                        oneSided: false,
                        q95ExcessRatio: q95ExcessRatio
                    )
                    candidateDecisionPath = [
                        "two_sided_possible_contrast_pass",
                        "evidence_class=two_sided_possible_contrast",
                        "possible_burst_review_route=two_sided",
                        "pre_ratio=\(formatRatio(metrics.preRatioQ90))",
                        "post_ratio=\(formatRatio(metrics.postRatioQ90))",
                        "required=\(formatRatio(settings.possibleBurstContrastMin))",
                        "structural_burst_support_weight=\(formatRatio(settings.structuralBurstSupportWeight))",
                        "support_adjusted_priority=\(possiblePriority)"
                    ].joined(separator: ";")
                    priority = possiblePriority
                }
                action = "demote_to_possible"
                lockLevel = .strongCandidate
            } else if structuralRescue {
                let strongFamilyLabel = burstFamilyLabel(spikeCount: spikeCount, metrics: metrics, settings: settings)
                if structuralRescueStrong && strongFamilyLabel.isCanonicalBurstFamily {
                    label = strongFamilyLabel
                    candidateGateStatus = "event_grammar_structural_burst_rescue_pass"
                    candidateDecisionPath = "compact_short_isi_cluster_rescued_by_train_scale_compression__\(label.rawValue)"
                    action = "accept"
                    priority = burstFamilyPriority(label: label, classic: 1_010, highFrequency: 990, long: 960)
                    lockLevel = .lockedClassic
                } else {
                    label = .possibleBurst
                    candidateGateStatus = "event_grammar_structural_possible_burst_rescue"
                    let possiblePriority = possibleBurstPriority(
                        metrics: metrics,
                        settings: settings,
                        coreCount: seedCount,
                        seedPurity: seedPurity,
                        bridgeFraction: bridgeFraction,
                        twoPossible: false,
                        oneSided: false,
                        q95ExcessRatio: q95ExcessRatio
                    )
                    candidateDecisionPath = [
                        "compact_short_isi_cluster_rescued_for_review_by_train_scale_compression",
                        "evidence_class=train_scale_compression_rescue",
                        "possible_burst_review_route=structural_rescue",
                        "local_background_q75=\(formatRatio(localBackground))",
                        "local_compression=\(formatRatio(structuralRatio))",
                        "required=\(formatRatio(settings.burstStructuralRescueCompressionMin))",
                        "structural_burst_support_weight=\(formatRatio(settings.structuralBurstSupportWeight))",
                        "support_adjusted_priority=\(possiblePriority)"
                    ].joined(separator: ";")
                    action = "demote_to_possible"
                    priority = possiblePriority
                    lockLevel = .strongCandidate
                }
                candidateClass = "event_grammar_seed_centered_burst"
            } else {
                let rejectDecisionPath = (
                    [
                        "event_grammar_reject",
                        "no_strict_or_possible_boundary_pass"
                    ] +
                    burstRejectionContext(
                        metrics: metrics,
                        settings: settings,
                        seedPurity: seedPurity,
                        bridgeFraction: bridgeFraction,
                        localBackground: localBackground,
                        structuralRatio: structuralRatio,
                        q95ExcessRatio: q95ExcessRatio,
                        boundarySide: boundarySide
                    )
                ).joined(separator: ";")
                candidates.append(
                    burstCandidate(
                        train: train,
                        metrics: metrics,
                        settings: settings,
                        label: .reject,
                        gateStatus: "event_grammar_reject",
                        decisionPath: rejectDecisionPath,
                        action: "reject",
                        score: 0,
                        priority: 0,
                        start: start,
                        end: end,
                        bridgeCount: bridgeCount,
                        lower: lower,
                        upper: upper,
                        lockLevel: .strongCandidate,
                        edgeGeom: decisionEdgeGeom,
                        candidateIndex: candidates.count + 1,
                        refCount: 0,
                        candidateLayer: "event_grammar_burst_event",
                        candidateClass: "event_grammar_seed_centered_burst",
                        seedRunStart: run.seedStart,
                        seedRunEnd: run.seedEnd,
                        bridgeBandUpper: bridgeUpper,
                        requiredGapSec: requiredGapSec(
                            intraQ90Sec: metrics.intraQ90Sec,
                            contrast: settings.burstContrastMin
                        ),
                        possibleRequiredGapSec: requiredGapSec(
                            intraQ90Sec: metrics.intraQ90Sec,
                            contrast: settings.possibleBurstContrastMin
                        ),
                        strictBoundaryPass: strictBoundaryAuditPass,
                        possibleBoundaryPass: possibleBoundaryAuditPass,
                        bridgeCountPass: bridgeCount <= settings.burstBridgeMaxCount,
                        bridgeFractionPass: bridgeFraction <= settings.burstBridgeFractionMax,
                        q90BridgePass: q90Pass,
                        sizeLabelBeforeReview: sizeLabelBeforeReview(
                            spikeCount: spikeCount,
                            settings: settings
                        ),
                        localBackgroundQ75Sec: localBackground,
                        localCompressionQ90Ratio: structuralRatio
                    )
                )
                continue
            }
            label = enabledBurstFamilyLabel(label, settings: settings)
            if !settings.enabledLabels.contains(label) {
                continue
            }

            let refCount = refractorySuspectCount(train: train, start: start, end: end, settings: settings)
            let effectiveRefAction = STPDRefractoryEvidencePolicy.effectiveAction(
                refCount: refCount,
                nISI: metrics.nISI,
                requestedAction: settings.refractoryAction
            )
            if STPDRefractoryEvidencePolicy.shouldApplyAction(refCount: refCount, nISI: metrics.nISI) {
                switch settings.refractoryAction {
                case .excludeCandidate, .reject:
                    continue
                case .demoteToPossible, .review, .markMultiunitContamination:
                    label = .possibleBurst
                    action = "demote_to_possible"
                    lockLevel = .strongCandidate
                case .warnOnly:
                    break
                }
            }

            let score = log1p(Double(metrics.nISI)) +
                0.45 * log(max(acceptedEdgeGeom, 1)) +
                0.25 * log(max(acceptedEdgeMin, 1)) -
                0.04 * max(0, (metrics.cv ?? 0) - 1) -
                q95Penalty

            let candidateIndex = candidates.count + 1
            candidates.append(
                burstCandidate(
                    train: train,
                    metrics: metrics,
                    settings: settings,
                    label: label,
                    gateStatus: candidateGateStatus,
                    decisionPath: candidateDecisionPath,
                    action: action,
                    score: score,
                    priority: priority,
                    start: start,
                    end: end,
                    bridgeCount: bridgeCount,
                    lower: lower,
                    upper: upper,
                    lockLevel: lockLevel,
                    edgeGeom: acceptedEdgeGeom,
                    candidateIndex: candidateIndex,
                    refCount: refCount,
                    refractoryAction: effectiveRefAction,
                    candidateLayer: "event_grammar_burst_event",
                    candidateClass: candidateClass,
                    seedRunStart: run.seedStart,
                    seedRunEnd: run.seedEnd,
                    bridgeBandUpper: bridgeUpper,
                    requiredGapSec: requiredGapSec(
                        intraQ90Sec: metrics.intraQ90Sec,
                        contrast: settings.burstContrastMin
                    ),
                    possibleRequiredGapSec: requiredGapSec(
                        intraQ90Sec: metrics.intraQ90Sec,
                        contrast: settings.possibleBurstContrastMin
                    ),
                    strictBoundaryPass: strictBoundaryAuditPass,
                    possibleBoundaryPass: possibleBoundaryAuditPass,
                    bridgeCountPass: bridgeCount <= settings.burstBridgeMaxCount,
                    bridgeFractionPass: bridgeFraction <= settings.burstBridgeFractionMax,
                    q90BridgePass: q90Pass,
                    sizeLabelBeforeReview: sizeLabelBeforeReview(
                        spikeCount: spikeCount,
                        settings: settings
                    ),
                    localBackgroundQ75Sec: localBackground,
                    localCompressionQ90Ratio: structuralRatio
                )
            )
        }

        candidates.append(
            contentsOf: adaptiveLocalHFBurstPacketCandidates(
                train: train,
                settings: settings,
                validFlag: validFlag,
                seenRuns: &seenRuns,
                lower: lower,
                upper: upper,
                bridgeUpper: bridgeUpper,
                startingCandidateIndex: candidates.count + 1
            )
        )
        candidates.append(
            contentsOf: oneSidedPossibleBurstRescueCandidates(
                train: train,
                settings: settings,
                validFlag: validFlag,
                seedFlag: seedFlag,
                bridgeFlag: bridgeFlag,
                seenRuns: &seenRuns,
                lower: lower,
                upper: upper,
                bridgeUpper: bridgeUpper,
                startingCandidateIndex: candidates.count + 1
            )
        )
        candidates.append(
            contentsOf: burstEpisodeCandidates(
                train: train,
                settings: settings,
                validFlag: validFlag,
                seedFlag: seedFlag,
                seenRuns: &seenRuns,
                trainBackgroundQ75: trainBackgroundQ75,
                lower: lower,
                upper: upper,
                bridgeUpper: bridgeUpper,
                startingCandidateIndex: candidates.count + 1
            )
        )
        candidates.append(
            contentsOf: burstFamilyMergeCandidates(
                train: train,
                settings: settings,
                existingCandidates: candidates,
                validFlag: validFlag,
                lower: lower,
                upper: upper,
                bridgeUpper: bridgeUpper,
                startingCandidateIndex: candidates.count + 1
            )
        )

        return candidates
    }

    /// Band-relative headroom for the adaptive HF burst-packet core ceiling: in the structure-band
    /// path the packet core (intra-q90) may sit at most this multiple of the learned burst band. It
    /// is a relative factor on an adaptive band (in the spirit of `burstEpisodeBridgeFactor`), never
    /// a hard ms constant. `band * headroom` is used directly (rather than `min(bridge, band *
    /// headroom)`): the bridge is not a useful lower cap here — when a structure bridge collapses to
    /// ≈ the band it wrongly rejects genuine ~1.2-1.5x-band packets such as the real screenshot
    /// packets. NOTE: `band * headroom` alone does NOT reject a slow-tonic cluster when the learned
    /// band itself is slow (≥ ~15 ms, so band*2 ≥ ~30 ms); the slow-tonic guard is held by the
    /// MANDATORY local-compression self-gate (see `adaptiveLocalHFBurstPacketLocalCeilingFraction`),
    /// which applies in this path too, not by this headroom.
    private static let adaptiveLocalHFBurstPacketCoreBandHeadroom = 2.0

    /// Fraction of the packet's local-background q75 used as a MANDATORY local-compression self-gate
    /// (and, when the burst band has collapsed, as the sole HF core ceiling). It is a relative factor
    /// on a measured local quantile (in the spirit of `burstEpisodeBackgroundFraction`), never a hard
    /// ms constant: a packet is admitted only when its core q90 is at most this fraction of its own
    /// neighbourhood background, i.e. at least `1 / fraction ≈ 1.47` times faster than the background.
    /// That self-gates out slow tonic-like clusters (core ≈ background, ratio ≈ 1) in BOTH the
    /// structure-band and collapsed-band paths, even when their boundary contrast is high, while
    /// admitting genuinely fast packets in a slower local background (the real screenshot packets sit
    /// at local-background/q90 ≈ 1.57-1.67, comfortably above the ≈1.47 threshold).
    private static let adaptiveLocalHFBurstPacketLocalCeilingFraction = 0.68

    /// Adaptive local high-frequency burst packets.
    ///
    /// A short, locally compressed run bounded on BOTH sides by ISIs several-fold larger than its
    /// own core is a visually burst-like high-frequency packet. In a high-frequency background the
    /// train-global compactness ceiling (`burstCompactnessUpper`, a fraction of the train q80)
    /// collapses below such a packet's intra-q90, so `structureFirstClassicBurstCandidates`
    /// rejects it; and once the learned burst band falls below the packet ISIs the seed-centred
    /// routes never seed it. This recovers those false negatives WITHOUT lowering any classic
    /// burst threshold, using a high-frequency core ceiling learned adaptively (never a hard
    /// 2-4 / 1-10 / 1-15 ms constant). The ceiling combines:
    ///   - a MANDATORY local-compression self-gate (both paths): intra-q90 <= a fraction of the
    ///     packet's own radius-12 local-background q75, so the core must be several-fold faster than
    ///     its neighbourhood. This rejects slow tonic-like clusters (core ≈ background) regardless of
    ///     band width, and is what recovers the `source=none` screenshot trains (where it is the sole
    ///     ceiling, the collapsed band being ≈ minValidISISec);
    ///   - plus, when the train HAS a usable burst band, an additional `band * headroom` cap keeping
    ///     the core within the learned burst regime.
    /// A span is admitted only on strong, LOCAL, two-sided evidence:
    ///   - short enough to be a packet, not a sustained state (<= highFrequencyBurstMaxSpikes);
    ///   - intra-q90 within the resolved adaptive HF ceiling, so a genuinely slow run is excluded;
    ///   - both immediate boundary ISIs >= `burstContrastMin` * the packet core median (median
    ///     scaled so a tight cluster whose q90 understates its compression is not lost) AND
    ///     >= the existing possible-burst q90 contrast floor;
    ///   - the structural background (max of the local q75 background and the two boundary gaps)
    ///     >= `burstStructuralRescueCompressionMin` * the packet core median.
    /// Spans already claimed by a stronger route are skipped via the shared `seenRuns`. The
    /// emitted candidate keeps a canonical burst-family `finalLabel` and additionally carries the
    /// additive, audit-only `stateHighFrequencySubtype = "hf_burst_packet"`; `finalLabel`, export
    /// columns and existing routes are otherwise unchanged. The decision path records which adaptive
    /// ceiling source admitted each packet.
    private static func adaptiveLocalHFBurstPacketCandidates(
        train: SpikeTrain,
        settings: ClassicAnchorSettings,
        validFlag: [Bool],
        seenRuns: inout Set<String>,
        lower: Double,
        upper: Double,
        bridgeUpper: Double,
        startingCandidateIndex: Int
    ) -> [ClassicAnchorCandidate] {
        guard settings.enabledLabels.contains(.burst) ||
                settings.enabledLabels.contains(.highFrequencyBurst),
              train.isiSec.count == validFlag.count,
              train.isiSec.count > 2 else {
            return []
        }

        let minSpanISI = max(settings.burstCoreMinISI, settings.minSpikes - 1)
        let maxSpanISI = max(minSpanISI, settings.highFrequencyBurstMaxSpikes - 1)
        guard minSpanISI > 0, maxSpanISI >= minSpanISI else {
            return []
        }

        // The adaptive HF core ceiling combines two relative gates (never a hard ms constant):
        //
        //  1. Local-compression self-gate (MANDATORY, both paths): intra-q90 <= a fraction of the
        //     packet's own radius-12 local-background q75. A packet is admitted only when its core is
        //     ≈1.47x faster than its neighbourhood, so a slow tonic-like cluster (core ≈ background,
        //     ratio ≈ 1) is rejected even with high boundary contrast — independently of band width.
        //     This is what keeps the slow-tonic guard intact in the STRUCTURE path (where the band
        //     alone could be slow), not just the collapsed path.
        //  2. Band ceiling (structure path only): intra-q90 <= learned band * headroom. When the band
        //     has collapsed (source=none, ceiling ≈ minValidISISec) it is dropped and the local
        //     self-gate is the sole ceiling, so genuine 5-10 ms packets in no-structure trains are
        //     recovered. The bridge is deliberately not used (a structure bridge that collapses to ≈
        //     the band would reject genuine ~1.2-1.5x-band packets; band * headroom plus the local
        //     self-gate already bound the core safely).
        let bandCeiling = settings.effectiveBurstBandUpperSec * adaptiveLocalHFBurstPacketCoreBandHeadroom
        let bandIsCollapsed = !settings.canUseBurstSeedBandForDetection
        let compressionMin = settings.burstStructuralRescueCompressionMin

        var candidates: [ClassicAnchorCandidate] = []
        for start in train.isiSec.indices where start > 0 {
            for spanLength in minSpanISI...maxSpanISI {
                let end = start + spanLength - 1
                guard train.isiSec.indices.contains(end) else {
                    break
                }
                let key = "\(start)_\(end)"
                guard !seenRuns.contains(key) else {
                    continue
                }
                guard (start...end).allSatisfy({ index in
                    validFlag.indices.contains(index) && validFlag[index]
                }) else {
                    continue
                }
                guard let metrics = spanMetrics(train: train, start: start, end: end, settings: settings),
                      let intraQ90 = metrics.intraQ90Sec, intraQ90.isFinite, intraQ90 > 0,
                      let intraMedian = metrics.intraQ50Sec, intraMedian.isFinite, intraMedian > 0,
                      let preGap = metrics.preGapSec, preGap.isFinite,
                      let postGap = metrics.postGapSec, postGap.isFinite else {
                    continue
                }

                let spikeCount = end - start + 2
                guard spikeCount >= settings.minSpikes,
                      spikeCount <= settings.highFrequencyBurstMaxSpikes else {
                    continue
                }
                // Local background scale: radius-12 q75 of the flanking ISIs (excludes the packet).
                let localBackground = localBackgroundQ75(
                    train: train,
                    validFlag: validFlag,
                    start: start,
                    end: end
                )
                // Resolve the adaptive HF core ceiling and record which adaptive source admitted it.
                // The local-compression self-gate (intra-q90 <= localBackground q75 * fraction) is
                // MANDATORY in both paths (a packet must be faster than its own neighbourhood); the
                // band ceiling additionally applies only when the band has not collapsed.
                guard let localBackground, localBackground.isFinite, localBackground > 0 else {
                    continue
                }
                let localCeiling = localBackground * adaptiveLocalHFBurstPacketLocalCeilingFraction
                let coreCeiling: Double
                let coreCeilingSource: String
                if bandIsCollapsed {
                    coreCeiling = localCeiling
                    coreCeilingSource = "local_background_q75_fraction"
                } else {
                    coreCeiling = min(bandCeiling, localCeiling)
                    coreCeilingSource = "min_burst_band_headroom_and_local_background"
                }
                // Core regime: the packet core sits within the adaptive HF ceiling. The local term
                // requires the core to be several-fold faster than its own neighbourhood, excluding
                // slow tonic-like clusters; the band term keeps it within the learned burst regime.
                guard intraQ90 <= coreCeiling + tolerance(for: coreCeiling) else {
                    continue
                }
                // Two-sided q90 boundary floor (not weaker than the existing possible-burst gate).
                let preRatioQ90 = metrics.preRatioQ90 ?? -.infinity
                let postRatioQ90 = metrics.postRatioQ90 ?? -.infinity
                guard preRatioQ90 >= settings.possibleBurstContrastMin,
                      postRatioQ90 >= settings.possibleBurstContrastMin else {
                    continue
                }
                // Discrete-packet evidence: both boundaries are several-fold the packet core median.
                let preMedianRatio = preGap / intraMedian
                let postMedianRatio = postGap / intraMedian
                guard preMedianRatio >= settings.burstContrastMin,
                      postMedianRatio >= settings.burstContrastMin else {
                    continue
                }
                // Local compression: structural background (local q75 OR the boundary gaps) is
                // several-fold the packet core median.
                let structuralBackground = finiteMax(localBackground, preGap, postGap)
                    ?? max(preGap, postGap)
                let structuralMedianRatio = structuralBackground / intraMedian
                guard structuralMedianRatio.isFinite,
                      structuralMedianRatio >= compressionMin else {
                    continue
                }
                let structuralQ90Ratio = ratio(structuralBackground, over: intraQ90)
                let localBackgroundQ90Ratio = ratio(localBackground, over: intraQ90)

                guard seenRuns.insert(key).inserted else {
                    continue
                }

                // Strong two-sided + local-compression evidence: a confirmed discrete packet, not a
                // merely "possible" one, so a possible-burst fallback is promoted to canonical burst.
                var label = burstFamilyLabel(spikeCount: spikeCount, metrics: metrics, settings: settings)
                if label == .possibleBurst {
                    label = .burst
                }
                label = enabledBurstFamilyLabel(label, settings: settings)

                let decisionPath = [
                    "adaptive_local_hf_burst_packet",
                    "source=local_flank_compression",
                    "band_collapsed=\(bandIsCollapsed)",
                    "core_ceiling_source=\(coreCeilingSource)",
                    "core_ceiling_sec=\(formatRatio(coreCeiling))",
                    "intra_q90_sec=\(formatRatio(intraQ90))",
                    "intra_median_sec=\(formatRatio(intraMedian))",
                    "local_background_q75_sec=\(formatRatio(localBackground))",
                    "local_background_q90_ratio=\(formatRatio(localBackgroundQ90Ratio))",
                    "pre_boundary_median_ratio=\(formatRatio(preMedianRatio))",
                    "post_boundary_median_ratio=\(formatRatio(postMedianRatio))",
                    "structural_background_median_ratio=\(formatRatio(structuralMedianRatio))",
                    "structural_compression_required=\(formatRatio(compressionMin))",
                    "two_sided=true",
                    "hf_burst_packet=true"
                ].joined(separator: ";")

                var candidate = burstCandidate(
                    train: train,
                    metrics: metrics,
                    settings: settings,
                    label: label,
                    gateStatus: "adaptive_local_hf_burst_packet_local_compression_pass",
                    decisionPath: decisionPath,
                    action: "accept",
                    score: 1 + log1p(structuralMedianRatio),
                    priority: 2_680,
                    start: start,
                    end: end,
                    bridgeCount: 0,
                    lower: lower,
                    upper: upper,
                    lockLevel: .strongCandidate,
                    edgeGeom: metrics.edgeContrastGeomQ90,
                    candidateIndex: startingCandidateIndex + candidates.count,
                    refCount: 0,
                    candidateLayer: "adaptive_local_hf_burst_packet",
                    candidateClass: "adaptive_two_sided_local_compression_hf_packet",
                    bridgeBandUpper: bridgeUpper,
                    requiredGapSec: requiredGapSec(
                        intraQ90Sec: intraQ90,
                        contrast: settings.burstContrastMin
                    ),
                    possibleRequiredGapSec: requiredGapSec(
                        intraQ90Sec: intraQ90,
                        contrast: settings.possibleBurstContrastMin
                    ),
                    strictBoundaryPass: true,
                    possibleBoundaryPass: true,
                    bridgeCountPass: true,
                    bridgeFractionPass: true,
                    q90BridgePass: true,
                    sizeLabelBeforeReview: sizeLabelBeforeReview(
                        spikeCount: spikeCount,
                        settings: settings
                    ),
                    localBackgroundQ75Sec: localBackground,
                    localCompressionQ90Ratio: structuralQ90Ratio
                )
                candidate.stateHighFrequencySubtype = "hf_burst_packet"
                candidates.append(candidate)
                // One packet per start: emit the smallest qualifying two-sided span (the tightest
                // bilateral boundary match) and stop, so a start cannot spawn nested duplicates.
                break
            }
        }
        return candidates
    }

    private static func structureFirstClassicBurstCandidates(
        train: SpikeTrain,
        settings: ClassicAnchorSettings,
        validFlag: [Bool],
        startingCandidateIndex: Int
    ) -> [ClassicAnchorCandidate] {
        guard settings.enabledLabels.contains(.burst),
              train.isiSec.count > 2 else {
            return []
        }

        let minSpanISI = max(settings.burstCoreMinISI, settings.minSpikes - 1)
        let maxSpanISI = max(minSpanISI, settings.classicMaxSpikes - 1)
        guard minSpanISI > 0, maxSpanISI >= minSpanISI else {
            return []
        }

        let trainCompactUpper = burstCompactnessUpper(
            train: train,
            validFlag: validFlag,
            settings: settings
        )

        var candidates: [ClassicAnchorCandidate] = []
        for start in train.isiSec.indices where start > 0 {
            for spanLength in minSpanISI...maxSpanISI {
                let end = start + spanLength - 1
                guard train.isiSec.indices.contains(end) else {
                    break
                }
                guard (start...end).allSatisfy({ index in
                    validFlag.indices.contains(index) && validFlag[index]
                }) else {
                    continue
                }
                guard let metrics = spanMetrics(train: train, start: start, end: end, settings: settings),
                      let intraQ90 = metrics.intraQ90Sec,
                      intraQ90.isFinite,
                      intraQ90 > 0 else {
                    continue
                }
                guard burstCompactnessPass(
                    intraQ90Sec: intraQ90,
                    compactUpperSec: trainCompactUpper
                ) else {
                    continue
                }
                // Flank contrast is computed first so the max-internal-ISI compactness gate can
                // tolerate a fractional tail when the cluster is strongly two-sided.
                let hasPreGap = metrics.preGapSec?.isFinite == true
                let hasPostGap = metrics.postGapSec?.isFinite == true
                let prePass = (metrics.preRatioQ90 ?? -.infinity) >= settings.burstContrastMin
                let postPass = (metrics.postRatioQ90 ?? -.infinity) >= settings.burstContrastMin
                let geomPass = (metrics.edgeContrastGeomQ90 ?? -.infinity) >= settings.burstContrastGeomMin
                let twoSidedPass = hasPreGap && hasPostGap && prePass && postPass && geomPass
                let startBoundaryPass = !hasPreGap && hasPostGap && start == 1 && postPass
                let endBoundaryPass = hasPreGap && !hasPostGap && end == train.isiSec.count - 1 && prePass
                let boundaryPass = startBoundaryPass || endBoundaryPass

                // Max-internal-ISI compactness, with a tolerated-tail rescue for strongly
                // two-sided packets. A compact cluster must not be erased because one internal
                // ISI is fractionally above the adaptive compactness upper, provided the bulk
                // (intraQ90) is compact (already gated above) AND even the largest internal ISI
                // stays >= the burst flank contrast below BOTH boundary gaps. Boundary/one-sided
                // clusters get no tail tolerance, so a weak/missing opposite boundary cannot be
                // rescued.
                let maxIntraStrictPass = burstCompactnessPass(
                    intraQ90Sec: metrics.maxIntraISISec,
                    compactUpperSec: trainCompactUpper
                )
                let maxIntraSec = metrics.maxIntraISISec ?? .infinity
                let maxIntraExcessRatio: Double = {
                    guard let upper = trainCompactUpper, upper > 0, maxIntraSec.isFinite else {
                        return .infinity
                    }
                    return maxIntraSec / upper
                }()
                let preFlankToMaxIntra: Double? = metrics.preGapSec.flatMap { gap in
                    maxIntraSec > 0 && maxIntraSec.isFinite ? gap / maxIntraSec : nil
                }
                let postFlankToMaxIntra: Double? = metrics.postGapSec.flatMap { gap in
                    maxIntraSec > 0 && maxIntraSec.isFinite ? gap / maxIntraSec : nil
                }
                let maxIntraToleratedTailPass = !maxIntraStrictPass &&
                    twoSidedPass &&
                    maxIntraExcessRatio <= maxIntraToleratedTailRatio &&
                    (preFlankToMaxIntra ?? -.infinity) >= settings.burstContrastMin &&
                    (postFlankToMaxIntra ?? -.infinity) >= settings.burstContrastMin
                guard maxIntraStrictPass || maxIntraToleratedTailPass else {
                    continue
                }

                guard twoSidedPass || boundaryPass else {
                    continue
                }

                let boundarySide = startBoundaryPass ? "start" : (endBoundaryPass ? "end" : "none")
                let edgeMin = twoSidedPass
                    ? metrics.edgeContrastMinQ90
                    : finiteMax(metrics.preRatioQ90, metrics.postRatioQ90)
                let edgeGeom = twoSidedPass ? metrics.edgeContrastGeomQ90 : edgeMin
                let contrastScore = log(max(edgeMin ?? 1, 1))
                let compactnessScore = log1p(Double(metrics.nValidISI))
                let score = compactnessScore + contrastScore
                var label: ClassicAnchorLabel = .burst
                var action = "accept"
                var lockLevel: ClassicAnchorLockLevel = .lockedClassic
                var priority = boundaryPass ? 1_030 : 1_260
                let refCount = refractorySuspectCount(
                    train: train,
                    start: start,
                    end: end,
                    settings: settings
                )
                let effectiveRefAction = STPDRefractoryEvidencePolicy.effectiveAction(
                    refCount: refCount,
                    nISI: metrics.nISI,
                    requestedAction: settings.refractoryAction
                )
                if STPDRefractoryEvidencePolicy.shouldApplyAction(refCount: refCount, nISI: metrics.nISI) {
                    switch settings.refractoryAction {
                    case .excludeCandidate, .reject:
                        continue
                    case .demoteToPossible, .review, .markMultiunitContamination:
                        label = .possibleBurst
                        action = "demote_to_possible"
                        lockLevel = .strongCandidate
                        priority = min(priority, 620)
                    case .warnOnly:
                        break
                    }
                }
                guard settings.enabledLabels.contains(label) else {
                    continue
                }
                let gateStatus: String
                if maxIntraToleratedTailPass {
                    gateStatus = "structure_first_two_sided_classic_burst_i_pass_with_tolerated_internal_tail"
                } else if boundaryPass {
                    gateStatus = "structure_first_endpoint_classic_burst_i_pass"
                } else {
                    gateStatus = "structure_first_two_sided_classic_burst_i_pass"
                }
                let decisionPath = [
                    "structure_first_classic_burst_i",
                    "no_default_burst_seed_band_used=true",
                    "source=local_flank_contrast",
                    "boundary_side=\(boundarySide)",
                    "pre_ratio=\(formatRatio(metrics.preRatioQ90))",
                    "post_ratio=\(formatRatio(metrics.postRatioQ90))",
                    "edge_min_required=\(formatRatio(settings.burstContrastMin))",
                    "edge_geom_required=\(formatRatio(settings.burstContrastGeomMin))",
                    "intra_q90_sec=\(formatRatio(intraQ90))",
                    "max_intra_isi_sec=\(formatRatio(metrics.maxIntraISISec))",
                    "train_compact_upper_sec=\(formatRatio(trainCompactUpper))",
                    "train_adaptive_compactness_pass=true",
                    "max_intra_strict_compactness_pass=\(maxIntraStrictPass)",
                    "max_intra_tolerated_tail=\(maxIntraToleratedTailPass)",
                    "max_intra_excess_ratio=\(formatRatio(maxIntraExcessRatio))",
                    "max_intra_excess_ratio_max=\(formatRatio(maxIntraToleratedTailRatio))",
                    "max_intra_pre_flank_ratio=\(formatRatio(preFlankToMaxIntra))",
                    "max_intra_post_flank_ratio=\(formatRatio(postFlankToMaxIntra))"
                ].joined(separator: ";")

                candidates.append(
                    burstCandidate(
                        train: train,
                        metrics: metrics,
                        settings: settings,
                        label: label,
                        gateStatus: gateStatus,
                        decisionPath: decisionPath,
                        action: action,
                        score: score,
                        priority: priority,
                        start: start,
                        end: end,
                        bridgeCount: 0,
                        lower: settings.minValidISISec,
                        upper: intraQ90,
                        lockLevel: lockLevel,
                        edgeGeom: edgeGeom,
                        candidateIndex: startingCandidateIndex + candidates.count,
                        refCount: refCount,
                        refractoryAction: effectiveRefAction,
                        candidateLayer: "structure_first_classic_burst_anchor",
                        candidateClass: boundaryPass
                            ? "structure_first_endpoint_classic_burst_i"
                            : "structure_first_two_sided_classic_burst_i",
                        seedRunStart: start,
                        seedRunEnd: end,
                        bridgeBandUpper: intraQ90,
                        requiredGapSec: requiredGapSec(
                            intraQ90Sec: metrics.intraQ90Sec,
                            contrast: settings.burstContrastMin
                        ),
                        possibleRequiredGapSec: requiredGapSec(
                            intraQ90Sec: metrics.intraQ90Sec,
                            contrast: settings.possibleBurstContrastMin
                        ),
                        strictBoundaryPass: true,
                        possibleBoundaryPass: true,
                        bridgeCountPass: true,
                        bridgeFractionPass: true,
                        q90BridgePass: true,
                        sizeLabelBeforeReview: ClassicAnchorLabel.burst.rawValue,
                        anchorBandSourceOverride: .structure
                    )
                )
            }
        }

        return candidates
    }

    private static func classicAnchorBurstCandidates(
        train: SpikeTrain,
        settings: ClassicAnchorSettings,
        seedFlag: [Bool],
        lower: Double,
        upper: Double
    ) -> [ClassicAnchorCandidate] {
        let runs = boolRuns(seedFlag)
        guard !runs.isEmpty else {
            return []
        }

        var candidates: [ClassicAnchorCandidate] = []
        candidates.reserveCapacity(runs.count)

        for run in runs {
            let start = run.start
            let end = run.end
            let spikeCount = end - start + 2
            guard spikeCount >= settings.minSpikes else {
                continue
            }
            if spikeCount > settings.classicMaxSpikes {
                let longSized = spikeCount >= settings.longMinSpikes &&
                    (settings.longMaxSpikes <= 0 || spikeCount <= settings.longMaxSpikes)
                guard longSized else {
                    continue
                }
            }
            guard let metrics = spanMetrics(train: train, start: start, end: end, settings: settings) else {
                continue
            }

            let hasPreGap = metrics.preGapSec?.isFinite == true
            let hasPostGap = metrics.postGapSec?.isFinite == true
            let isTwoSided = hasPreGap && hasPostGap
            let isStartBoundary = !hasPreGap && hasPostGap && start == 1
            let isEndBoundary = hasPreGap && !hasPostGap && end == train.isiSec.count - 1
            let isBoundaryBurst = isStartBoundary || isEndBoundary
            guard isTwoSided || isBoundaryBurst else {
                continue
            }

            let edgeMin = metrics.edgeContrastMinQ90
            let edgeGeom = metrics.edgeContrastGeomQ90 ?? edgeMin
            let strict = (edgeMin ?? -.infinity) >= settings.burstContrastMin &&
                (edgeGeom ?? -.infinity) >= settings.burstContrastGeomMin
            let possible = (edgeMin ?? -.infinity) >= settings.possibleBurstContrastMin &&
                (edgeGeom ?? -.infinity) >= settings.possibleBurstContrastGeomMin
            guard strict || possible else {
                continue
            }

            var label = burstFamilyLabel(spikeCount: spikeCount, metrics: metrics, settings: settings)
            if !strict {
                label = .possibleBurst
            }
            label = enabledBurstFamilyLabel(label, settings: settings)
            if !settings.enabledLabels.contains(label) {
                continue
            }

            let refCount = refractorySuspectCount(train: train, start: start, end: end, settings: settings)
            let effectiveRefAction = STPDRefractoryEvidencePolicy.effectiveAction(
                refCount: refCount,
                nISI: metrics.nISI,
                requestedAction: settings.refractoryAction
            )
            var action = label == .possibleBurst ? "demote_to_possible" : "accept"
            var lockLevel: ClassicAnchorLockLevel = strict && label != .possibleBurst ? .lockedClassic : .strongCandidate
            if STPDRefractoryEvidencePolicy.shouldApplyAction(refCount: refCount, nISI: metrics.nISI) {
                switch settings.refractoryAction {
                case .excludeCandidate, .reject:
                    continue
                case .demoteToPossible, .review, .markMultiunitContamination:
                    label = .possibleBurst
                    action = "demote_to_possible"
                    lockLevel = .strongCandidate
                case .warnOnly:
                    break
                }
            }

            let acceptedEdgeMin = edgeMin ?? 1
            let acceptedEdgeGeom = edgeGeom ?? acceptedEdgeMin
            let score = log1p(Double(metrics.nISI)) +
                0.45 * log(max(acceptedEdgeGeom, 1)) +
                0.25 * log(max(acceptedEdgeMin, 1)) -
                0.04 * max(0, (metrics.cv ?? 0) - 1)
            let priority = switch label {
            case .burst:
                1_375
            case .highFrequencyBurst:
                1_350
            case .longBurst:
                1_330
            default:
                520
            }

            candidates.append(
                burstCandidate(
                    train: train,
                    metrics: metrics,
                    settings: settings,
                    label: label,
                    gateStatus: gateStatus(strict: strict, boundary: isBoundaryBurst, bridged: false),
                    decisionPath: decisionPath(strict: strict, boundary: isBoundaryBurst, bridged: false),
                    action: action,
                    score: score,
                    priority: priority,
                    start: start,
                    end: end,
                    bridgeCount: 0,
                    lower: lower,
                    upper: upper,
                    lockLevel: lockLevel,
                    edgeGeom: acceptedEdgeGeom,
                    candidateIndex: candidates.count + 1,
                    refCount: refCount,
                    refractoryAction: effectiveRefAction
                )
            )
        }

        return candidates
    }

    private static func oneSidedPossibleBurstRescueCandidates(
        train: SpikeTrain,
        settings: ClassicAnchorSettings,
        validFlag: [Bool],
        seedFlag: [Bool],
        bridgeFlag: [Bool],
        seenRuns: inout Set<String>,
        lower: Double,
        upper: Double,
        bridgeUpper: Double,
        startingCandidateIndex: Int
    ) -> [ClassicAnchorCandidate] {
        guard settings.enabledLabels.contains(.possibleBurst),
              validFlag.count == bridgeFlag.count,
              seedFlag.count == bridgeFlag.count,
              train.isiSec.count == bridgeFlag.count else {
            return []
        }

        let bridgeRuns = boolRuns(bridgeFlag)
        let structuralSupport = min(1, max(0, settings.structuralBurstSupportWeight))
        let possibleSeedPurityMinBase = min(settings.burstStructuralRescueSeedPurityMin, 0.50)
        let possibleSeedPurityMin = max(0.35, possibleSeedPurityMinBase - 0.15 * structuralSupport)
        let possibleBridgeFractionMax = min(
            0.90,
            max(settings.burstBridgeFractionMax, 0.75) + 0.10 * structuralSupport
        )
        let possibleContrastMin = max(
            1.35,
            settings.possibleBurstContrastMin - 0.30 * structuralSupport
        )
        let possibleCompressionMin = max(
            1.50,
            settings.burstStructuralRescueCompressionMin - 0.75 * structuralSupport
        )
        let q95SevereRatioMax = settings.burstQ95SoftSevereRatio + 0.20 * structuralSupport
        var candidates: [ClassicAnchorCandidate] = []
        candidates.reserveCapacity(bridgeRuns.count)

        for run in bridgeRuns {
            let start = run.start
            let end = run.end
            let key = "\(start)_\(end)"
            guard !seenRuns.contains(key) else {
                continue
            }

            let spikeCount = end - start + 2
            guard spikeCount >= settings.minSpikes else {
                continue
            }
            if spikeCount > settings.classicMaxSpikes {
                let longSized = spikeCount >= settings.longMinSpikes &&
                    (settings.longMaxSpikes <= 0 || spikeCount <= settings.longMaxSpikes)
                guard longSized else {
                    continue
                }
            }
            guard let metrics = spanMetrics(train: train, start: start, end: end, settings: settings) else {
                continue
            }

            let seedIndices = (start...end).filter { index in
                seedFlag.indices.contains(index) && seedFlag[index]
            }
            let seedCount = seedIndices.count
            guard seedCount >= max(1, settings.burstCoreMinISI - 1) else {
                continue
            }

            let spanISI = end - start + 1
            let bridgeCount = (start...end).filter { index in
                bridgeFlag.indices.contains(index) && bridgeFlag[index] &&
                    !(seedFlag.indices.contains(index) && seedFlag[index])
            }.count
            let seedPurity = Double(seedCount) / Double(max(1, metrics.nValidISI))
            let bridgeFraction = Double(bridgeCount) / Double(max(1, spanISI))
            guard seedPurity >= possibleSeedPurityMin,
                  bridgeFraction <= possibleBridgeFractionMax else {
                continue
            }

            let q90Pass = finiteLessThanOrEqual(metrics.intraQ90Sec, bridgeUpper)
            let q95ExcessRatio = ratio(metrics.intraQ95Sec, over: bridgeUpper)
            let q95Severe = (q95ExcessRatio ?? -.infinity) > q95SevereRatioMax
            guard q90Pass, !q95Severe else {
                continue
            }

            let localBackground = localBackgroundQ75(
                train: train,
                validFlag: validFlag,
                start: start,
                end: end
            )
            let localCompression = ratio(localBackground, over: metrics.intraQ90Sec)
            let structuralCompressionPass = (localCompression ?? -.infinity) >= possibleCompressionMin

            let prePass = (metrics.preRatioQ90 ?? -.infinity) >= possibleContrastMin
            let postPass = (metrics.postRatioQ90 ?? -.infinity) >= possibleContrastMin
            let hasPreGap = metrics.preGapSec?.isFinite == true
            let hasPostGap = metrics.postGapSec?.isFinite == true
            let isStartBoundary = !hasPreGap && hasPostGap && start == 1
            let isEndBoundary = hasPreGap && !hasPostGap && end == train.isiSec.count - 1
            let isTwoSided = hasPreGap && hasPostGap
            let oneSidedContrast = finiteMax(metrics.preRatioQ90, metrics.postRatioQ90)
            let modestFlankPass = (oneSidedContrast ?? -.infinity) >= max(1.15, possibleContrastMin * 0.70)

            let rescueKind: String?
            let boundarySide: String
            let evidenceClass: String
            let dominantFlankSide: String?
            if isStartBoundary, postPass {
                rescueKind = "start_boundary_single_flank"
                boundarySide = "start"
                evidenceClass = "endpoint_single_flank_rescue"
                dominantFlankSide = "post"
            } else if isEndBoundary, prePass {
                rescueKind = "end_boundary_single_flank"
                boundarySide = "end"
                evidenceClass = "endpoint_single_flank_rescue"
                dominantFlankSide = "pre"
            } else if isStartBoundary, structuralCompressionPass, modestFlankPass {
                rescueKind = "start_boundary_structural_compression"
                boundarySide = "start"
                evidenceClass = "endpoint_structural_compression_rescue"
                dominantFlankSide = "post"
            } else if isEndBoundary, structuralCompressionPass, modestFlankPass {
                rescueKind = "end_boundary_structural_compression"
                boundarySide = "end"
                evidenceClass = "endpoint_structural_compression_rescue"
                dominantFlankSide = "pre"
            } else if isTwoSided, prePass != postPass {
                rescueKind = prePass ? "pre_flank_only" : "post_flank_only"
                boundarySide = "none"
                evidenceClass = "two_sided_single_flank_rescue"
                dominantFlankSide = prePass ? "pre" : "post"
            } else if isTwoSided, structuralCompressionPass, modestFlankPass {
                let preRatio = metrics.preRatioQ90 ?? -.infinity
                let postRatio = metrics.postRatioQ90 ?? -.infinity
                rescueKind = preRatio >= postRatio
                    ? "pre_flank_structural_compression"
                    : "post_flank_structural_compression"
                boundarySide = "none"
                evidenceClass = "two_sided_structural_compression_rescue"
                dominantFlankSide = preRatio >= postRatio ? "pre" : "post"
            } else {
                rescueKind = nil
                boundarySide = "none"
                evidenceClass = "none"
                dominantFlankSide = nil
            }
            guard let rescueKind else {
                continue
            }

            let weakFlankSec: Double?
            if dominantFlankSide == "pre" {
                weakFlankSec = metrics.postGapSec
            } else if dominantFlankSide == "post" {
                weakFlankSec = metrics.preGapSec
            } else {
                weakFlankSec = nil
            }
            let weakFlankBridgeable = weakFlankSec.map {
                $0 <= bridgeUpper + tolerance(for: bridgeUpper)
            } ?? false
            let weakFlankRole = possibleBurstWeakFlankRole(
                weakFlankSec: weakFlankSec,
                bridgeable: weakFlankBridgeable
            )
            if isTwoSided, weakFlankBridgeable {
                continue
            }

            let cvPass = metrics.cv.map {
                !$0.isFinite || $0 <= settings.burstStructuralRescueCVMax * 1.25
            } ?? true
            guard cvPass else {
                continue
            }

            let priority = possibleBurstPriority(
                metrics: metrics,
                settings: settings,
                coreCount: seedCount,
                seedPurity: seedPurity,
                bridgeFraction: bridgeFraction,
                twoPossible: false,
                oneSided: true,
                q95ExcessRatio: q95ExcessRatio,
                oneSidedContrast: oneSidedContrast
            )
            let decisionPath = [
                "one_sided_possible_burst_rescue",
                "evidence_class=\(evidenceClass)",
                "possible_burst_review_route=rescue",
                "kind=\(rescueKind)",
                "boundary_side=\(boundarySide)",
                "possible_burst_structural_definition=compact_short_isi_run_with_single_strong_flank_and_missing_or_nonbridgeable_opposite_boundary",
                "missing_or_weak_flank_allowed_for_possible_burst=true",
                "endpoint_missing_flank=\(isStartBoundary || isEndBoundary)",
                "weak_flank_sec=\(formatRatio(weakFlankSec))",
                "weak_flank_role=\(weakFlankRole)",
                "weak_flank_bridgeable=\(weakFlankBridgeable)",
                "weak_flank_not_absorbable_as_burst_bridge=\(!weakFlankBridgeable)",
                "interpretation=possible_burst_by_single_flank_or_endpoint_structure",
                "pre_ratio=\(formatRatio(metrics.preRatioQ90))",
                "post_ratio=\(formatRatio(metrics.postRatioQ90))",
                "required=\(formatRatio(settings.possibleBurstContrastMin))",
                "effective_required=\(formatRatio(possibleContrastMin))",
                "seed_count=\(seedCount)",
                "seed_purity=\(formatRatio(seedPurity))",
                "seed_purity_min_effective=\(formatRatio(possibleSeedPurityMin))",
                "bridge_fraction=\(formatRatio(bridgeFraction))",
                "bridge_fraction_max_effective=\(formatRatio(possibleBridgeFractionMax))",
                "local_background_q75=\(formatRatio(localBackground))",
                "local_compression=\(formatRatio(localCompression))",
                "local_compression_min_effective=\(formatRatio(possibleCompressionMin))",
                "structural_compression_pass=\(structuralCompressionPass)",
                "modest_flank_pass=\(modestFlankPass)",
                "q95_excess_ratio=\(formatRatio(q95ExcessRatio))",
                "q95_severe_ratio_max_effective=\(formatRatio(q95SevereRatioMax))",
                "q90_bridge_pass=\(q90Pass)",
                "structural_burst_support_weight=\(formatRatio(settings.structuralBurstSupportWeight))",
                "support_adjusted_priority=\(priority)"
            ].joined(separator: ";")

            let score = log1p(Double(metrics.nISI)) +
                0.35 * log(max(oneSidedContrast ?? 1, 1)) +
                0.15 * log(max(localCompression ?? 1, 1)) +
                0.25 * structuralSupport +
                0.20 * seedPurity -
                0.04 * max(0, (metrics.cv ?? 0) - 1)
            let refCount = refractorySuspectCount(train: train, start: start, end: end, settings: settings)
            let effectiveRefAction = STPDRefractoryEvidencePolicy.effectiveAction(
                refCount: refCount,
                nISI: metrics.nISI,
                requestedAction: settings.refractoryAction
            )
            var action = "demote_to_possible"
            if STPDRefractoryEvidencePolicy.shouldApplyAction(refCount: refCount, nISI: metrics.nISI) {
                switch settings.refractoryAction {
                case .excludeCandidate, .reject:
                    continue
                case .demoteToPossible, .review, .markMultiunitContamination, .warnOnly:
                    action = "demote_to_possible"
                }
            }

            seenRuns.insert(key)
            candidates.append(
                burstCandidate(
                    train: train,
                    metrics: metrics,
                    settings: settings,
                    label: .possibleBurst,
                    gateStatus: "event_grammar_one_sided_possible_burst_rescue",
                    decisionPath: decisionPath,
                    action: action,
                    score: score,
                    priority: priority,
                    start: start,
                    end: end,
                    bridgeCount: bridgeCount,
                    lower: lower,
                    upper: upper,
                    lockLevel: .strongCandidate,
                    edgeGeom: oneSidedContrast,
                    candidateIndex: startingCandidateIndex + candidates.count,
                    refCount: refCount,
                    refractoryAction: effectiveRefAction,
                    candidateLayer: "event_grammar_one_sided_possible_burst_rescue",
                    candidateClass: "event_grammar_bridge_run_possible_burst",
                    seedRunStart: seedIndices.first,
                    seedRunEnd: seedIndices.last,
                    bridgeBandUpper: bridgeUpper,
                    requiredGapSec: requiredGapSec(
                        intraQ90Sec: metrics.intraQ90Sec,
                        contrast: settings.burstContrastMin
                    ),
                    possibleRequiredGapSec: requiredGapSec(
                        intraQ90Sec: metrics.intraQ90Sec,
                        contrast: settings.possibleBurstContrastMin
                    ),
                    strictBoundaryPass: false,
                    possibleBoundaryPass: true,
                    bridgeCountPass: bridgeCount <= settings.burstBridgeMaxCount,
                    bridgeFractionPass: bridgeFraction <= settings.burstBridgeFractionMax,
                    q90BridgePass: q90Pass,
                    sizeLabelBeforeReview: sizeLabelBeforeReview(
                        spikeCount: spikeCount,
                        settings: settings
                    ),
                    localBackgroundQ75Sec: localBackground,
                    localCompressionQ90Ratio: localCompression
                )
            )
        }

        return candidates
    }

    private static func hardThresholdBurstCandidates(
        train: SpikeTrain,
        settings: ClassicAnchorSettings,
        startingCandidateIndex: Int
    ) -> [ClassicAnchorCandidate] {
        guard settings.burstHardThresholdEnabled else {
            return []
        }

        let burstSeedUpper = settings.effectiveBurstBandUpperSec
        guard burstSeedUpper.isFinite, burstSeedUpper > 0 else {
            return []
        }
        // Mirrors R `bridge_high = max(bmax, vp$bridge_high, bmax * 1.25)` (R/38_event_grammar_core.R).
        let bridgeUpper = max(
            burstSeedUpper,
            settings.effectiveBurstBridgeUpperSec,
            burstSeedUpper * 1.25
        )
        var seedFlag = Array(repeating: false, count: train.isiSec.count)
        var bridgeFlag = Array(repeating: false, count: train.isiSec.count)

        for index in train.isiSec.indices where index > 0 {
            guard let isi = train.isiSec[index],
                  isi.isFinite,
                  !isArtifactISI(isi, threshold: settings.minValidISISec) else {
                continue
            }
            seedFlag[index] = isi <= burstSeedUpper
            bridgeFlag[index] = isi <= bridgeUpper
        }

        let runs = boolRuns(bridgeFlag)
        guard !runs.isEmpty else {
            return []
        }

        var candidates: [ClassicAnchorCandidate] = []
        candidates.reserveCapacity(runs.count)
        let minCore = max(1, settings.burstCoreMinISI)
        let minSpikes = max(2, settings.minSpikes)

        for run in runs {
            let runStart = run.start
            let runEnd = run.end
            // R `bridge_isi_count` / `bridge_fraction` validation (`burstBridgeMaxCount` = R `n_count`,
            // `burstBridgeFractionMax` = R `n_fraction`; R/38_event_grammar_core.R). Every other burst
            // route applies these limits; this hard-threshold route did not. A compact seed cluster
            // embedded in a moderate tonic baseline whose ISIs still fall under `bmax * 1.25` (e.g. a
            // 40 ms cluster flanked by a 120 ms baseline under a 100 ms seed max) was therefore emitted
            // as one oversized whole-train span whose bridge fraction is far above the limit. That span
            // exceeds the band and is rejected, leaving the real cluster labeled "others". When a run
            // violates the bridge limits, contract it to its continuous seed-bounded cores so each
            // emitted burst is tight to a cluster; runs within the limits (the R event-core parity case)
            // are emitted exactly as R emits them, including their adjacent in-band bridge ISIs.
            let runBridgeCount = (runStart...runEnd).filter { bridgeFlag[$0] && !seedFlag[$0] }.count
            let runSpan = runEnd - runStart + 1
            let runBridgeFraction = runSpan > 0 ? Double(runBridgeCount) / Double(runSpan) : 0
            let candidateSpans: [(start: Int, end: Int)]
            if runBridgeCount > settings.burstBridgeMaxCount || runBridgeFraction > settings.burstBridgeFractionMax {
                var seedOnlyFlag = Array(repeating: false, count: seedFlag.count)
                for index in runStart...runEnd {
                    seedOnlyFlag[index] = seedFlag[index]
                }
                candidateSpans = boolRuns(seedOnlyFlag).map { (start: $0.start, end: $0.end) }
            } else {
                candidateSpans = [(start: runStart, end: runEnd)]
            }

            for span in candidateSpans {
                let start = span.start
                let end = span.end
                let coreCount = (start...end).filter { seedFlag[$0] }.count
                let spikeCount = end - start + 2
                guard coreCount >= minCore, spikeCount >= minSpikes else {
                    continue
                }
                guard let metrics = spanMetrics(train: train, start: start, end: end, settings: settings) else {
                    continue
                }

                var label = burstFamilyLabel(spikeCount: spikeCount, metrics: metrics, settings: settings)
                label = enabledBurstFamilyLabel(label, settings: settings)
                if !settings.enabledLabels.contains(label) {
                    continue
                }

                let status = switch label {
                case .burst:
                    "isi_profile_hard_threshold_burst_pass"
                case .highFrequencyBurst:
                    "isi_profile_hard_threshold_high_frequency_burst_pass"
                case .longBurst:
                    "isi_profile_hard_threshold_long_burst_review"
                default:
                    "isi_profile_hard_threshold_oversized_burst_review"
                }
                let decision = switch label {
                case .burst:
                    "hard_threshold_direct_seed_bridge_without_flank_contrast_gate"
                case .highFrequencyBurst:
                    "hard_threshold_core_fast_limited_extent_high_frequency_burst"
                case .longBurst:
                    "hard_threshold_long_burst_requires_flank_contrast_gate"
                default:
                    "hard_threshold_burst_family_size_review"
                }
                let bridgeNonSeedCount = (start...end).filter { bridgeFlag[$0] && !seedFlag[$0] }.count
                let score = 30 +
                    Double(coreCount) +
                    0.05 * Double(spikeCount) -
                    0.25 * Double(bridgeNonSeedCount)

                let refCount = refractorySuspectCount(train: train, start: start, end: end, settings: settings)
                var candidate = burstCandidate(
                    train: train,
                    metrics: metrics,
                    settings: settings,
                    label: label,
                    gateStatus: status,
                    decisionPath: decision,
                    action: "accept",
                    score: score,
                    priority: burstFamilyPriority(label: label, classic: 1_450, highFrequency: 1_420, long: 1_310),
                    start: start,
                    end: end,
                    bridgeCount: bridgeNonSeedCount,
                    lower: settings.effectiveBurstBandLowerSec,
                    upper: burstSeedUpper,
                    lockLevel: label.isCanonicalBurstFamily ? .lockedClassic : .strongCandidate,
                    edgeGeom: metrics.edgeContrastGeomQ90,
                    candidateIndex: startingCandidateIndex + candidates.count,
                    refCount: refCount,
                    refractoryAction: STPDRefractoryEvidencePolicy.effectiveAction(
                        refCount: refCount,
                        nISI: metrics.nISI,
                        requestedAction: settings.refractoryAction
                    ),
                    candidateLayer: "isi_profile_hard_threshold_burst",
                    candidateClass: "isi_profile_hard_threshold_burst",
                    bridgeBandUpper: bridgeUpper,
                    sizeLabelBeforeReview: label.rawValue
                )
                candidate.thresholdMode = "hard_threshold"
                candidate.hardThreshold = true
                candidate.hardThresholdPattern = "burst"
                candidate.hardBurstSeedUpperSec = burstSeedUpper
                candidate.hardBurstBridgeUpperSec = bridgeUpper
                candidate.hardBurstCoreISICount = coreCount
                candidate.hardThresholdSource = settings.burstHardThresholdSource
                candidates.append(candidate)
            }
        }

        return candidates
    }

    private static func burstEpisodeCandidates(
        train: SpikeTrain,
        settings: ClassicAnchorSettings,
        validFlag: [Bool],
        seedFlag: [Bool],
        seenRuns: inout Set<String>,
        trainBackgroundQ75: Double?,
        lower: Double,
        upper: Double,
        bridgeUpper: Double,
        startingCandidateIndex: Int
    ) -> [ClassicAnchorCandidate] {
        guard train.isiSec.count == validFlag.count,
              train.isiSec.count == seedFlag.count else {
            return []
        }

        let episodeUpperCandidate = min(
            bridgeUpper * settings.burstEpisodeBridgeFactor,
            (trainBackgroundQ75 ?? .infinity) * settings.burstEpisodeBackgroundFraction
        )
        var episodeUpper = max(bridgeUpper, episodeUpperCandidate)
        if !episodeUpper.isFinite || episodeUpper <= bridgeUpper {
            episodeUpper = bridgeUpper
        }

        let episodeFlag = train.isiSec.indices.map { index -> Bool in
            guard validFlag[index],
                  let isi = train.isiSec[index],
                  isi.isFinite else {
                return false
            }
            return isi <= episodeUpper
        }
        let episodeRuns = boolRuns(episodeFlag)
        guard !episodeRuns.isEmpty else {
            return []
        }

        var candidates: [ClassicAnchorCandidate] = []
        candidates.reserveCapacity(episodeRuns.count)

        for run in episodeRuns {
            var start = run.start
            var end = run.end
            let originalStart = start
            let originalEnd = end
            let originalValues = validISIValues(
                train: train,
                validFlag: validFlag,
                start: originalStart,
                end: originalEnd
            )
            let edgeQ90 = quantile(originalValues, probability: 0.90)
            var edgeUpper = max(bridgeUpper, (edgeQ90 ?? 0) * 1.10)
            edgeUpper = min(edgeUpper, episodeUpper)
            if !edgeUpper.isFinite || edgeUpper <= 0 {
                edgeUpper = episodeUpper
            }

            while start <= end,
                  let isi = train.isiSec[start],
                  isi.isFinite,
                  isi > edgeUpper {
                start += 1
            }
            while end >= start,
                  let isi = train.isiSec[end],
                  isi.isFinite,
                  isi > edgeUpper {
                end -= 1
            }

            guard start <= end,
                  end - start + 1 >= settings.burstEpisodeMinISI else {
                continue
            }
            let key = "\(start)_\(end)"
            guard seenRuns.insert(key).inserted else {
                continue
            }
            let values = validISIValues(
                train: train,
                validFlag: validFlag,
                start: start,
                end: end
            )
            guard values.count >= settings.burstEpisodeMinISI else {
                continue
            }

            let seedCount = (start...end).filter { index in
                validFlag[index] &&
                    (train.isiSec[index] ?? .infinity) >= lower &&
                    (train.isiSec[index] ?? -.infinity) <= upper
            }.count
            let seedFraction = Double(seedCount) / Double(max(1, values.count))
            let bridgeFraction = fraction(values, matching: { $0 <= bridgeUpper })
            let lowISIFraction = fraction(values, matching: { $0 <= episodeUpper })
            let q90 = quantile(values, probability: 0.90)
            let cv = STPDStatistics.coefficientOfVariation(values)
            let compression = ratio(trainBackgroundQ75, over: q90)

            let seedEntryPass = seedCount >= settings.burstEpisodeMinSeedISI &&
                seedFraction >= settings.burstEpisodeSeedFractionMin
            let lowISIEpisodePass = lowISIFraction >= settings.burstEpisodeLowISIFractionMin &&
                finiteLessThanOrEqual(q90, episodeUpper) &&
                (compression ?? -.infinity) >= settings.burstStructuralRescueCompressionMin
            guard seedEntryPass || lowISIEpisodePass,
                  finiteLessThanOrEqual(q90, episodeUpper),
                  (cv ?? 0) <= settings.burstEpisodeCVMax,
                  (compression ?? -.infinity) >= settings.burstStructuralRescueCompressionMin else {
                continue
            }

            let spikeCount = end - start + 2
            guard spikeCount >= settings.minSpikes,
                  let metrics = spanMetrics(train: train, start: start, end: end, settings: settings) else {
                continue
            }

            var label: ClassicAnchorLabel
            if highFrequencyBurstPass(metrics: metrics, spikeCount: spikeCount, settings: settings) {
                label = .highFrequencyBurst
            } else if spikeCount <= settings.burstEpisodeClassicMaxSpikes {
                label = .burst
            } else if spikeCount >= settings.longMinSpikes &&
                (settings.longMaxSpikes <= 0 || spikeCount <= settings.longMaxSpikes) &&
                longBurstUpperBandPass(metrics: metrics, settings: settings) {
                label = .longBurst
            } else {
                label = .possibleBurst
            }
            label = enabledBurstFamilyLabel(label, settings: settings)
            if !settings.enabledLabels.contains(label) {
                continue
            }

            let action = label == .possibleBurst ? "demote_to_possible" : "accept"
            let lockLevel: ClassicAnchorLockLevel = label == .possibleBurst ? .strongCandidate : .lockedClassic
            let gateStatus = label == .possibleBurst
                ? "event_grammar_structural_burst_episode_review"
                : "event_grammar_structural_burst_episode_pass"
            let decisionPath = "dense_short_isi_episode_rescued_by_train_scale_compression__\(label.rawValue)"
            let priority: Int = switch label {
            case .burst:
                1_035
            case .highFrequencyBurst:
                1_020
            case .longBurst:
                985
            case .possibleBurst:
                520
            default:
                0
            }
            let score = 4 +
                0.12 * Double(values.count) +
                0.6 * seedFraction +
                0.5 * bridgeFraction +
                min(4, 0.2 * max(0, compression ?? 0)) -
                0.4 * max(0, cv ?? 0)
            let bridgeCount = (start...end).filter { index in
                validFlag[index] &&
                    !seedFlag[index] &&
                    (train.isiSec[index] ?? .infinity) <= bridgeUpper
            }.count
            let refCount = refractorySuspectCount(train: train, start: start, end: end, settings: settings)

            candidates.append(
                burstCandidate(
                    train: train,
                    metrics: metrics,
                    settings: settings,
                    label: label,
                    gateStatus: gateStatus,
                    decisionPath: decisionPath,
                    action: action,
                    score: score,
                    priority: priority,
                    start: start,
                    end: end,
                    bridgeCount: bridgeCount,
                    lower: lower,
                    upper: episodeUpper,
                    lockLevel: lockLevel,
                    edgeGeom: metrics.edgeContrastGeomQ90,
                    candidateIndex: startingCandidateIndex + candidates.count,
                    refCount: refCount,
                    refractoryAction: STPDRefractoryEvidencePolicy.effectiveAction(
                        refCount: refCount,
                        nISI: metrics.nISI,
                        requestedAction: settings.refractoryAction
                    ),
                    candidateLayer: "event_grammar_burst_episode",
                    candidateClass: "event_grammar_dense_short_isi_episode"
                )
            )
        }

        return candidates
    }

    private static func burstFamilyMergeCandidates(
        train: SpikeTrain,
        settings: ClassicAnchorSettings,
        existingCandidates: [ClassicAnchorCandidate],
        validFlag: [Bool],
        lower: Double,
        upper: Double,
        bridgeUpper: Double,
        startingCandidateIndex: Int
    ) -> [ClassicAnchorCandidate] {
        guard train.isiSec.count == validFlag.count else {
            return []
        }
        let pool = existingCandidates
            .filter { candidate in
                candidate.isEligibleForAutoSelection &&
                    (candidate.finalLabel.isCanonicalBurstFamily ||
                        candidate.finalLabel == .possibleBurst)
            }
            .sorted { lhs, rhs in
                if lhs.startISIIndex != rhs.startISIIndex {
                    return lhs.startISIIndex < rhs.startISIIndex
                }
                if lhs.endISIIndex != rhs.endISIIndex {
                    return lhs.endISIIndex < rhs.endISIIndex
                }
                return lhs.priority > rhs.priority
            }
        guard pool.count >= 2 else {
            return []
        }

        var seenIntervals = Set(
            existingCandidates
                .filter { $0.isEligibleForAutoSelection }
                .map {
                    "\(min($0.startISIIndex, $0.endISIIndex))_\(max($0.startISIIndex, $0.endISIIndex))"
                }
        )
        var mergedCandidates: [ClassicAnchorCandidate] = []
        let maxGapCount = 2
        let mergeBridgeUpper = min(
            0.030,
            max(bridgeUpper * 2.0, upper * 3.0, bridgeUpper)
        )
        let robustFlankCount = 3
        let seedPurityMin = 0.45
        let bridgeFractionMax = max(0.50, settings.burstBridgeFractionMax)
        let q95PossibleRatioMax = 1.25

        for index in 0..<(pool.count - 1) {
            let first = pool[index]
            let second = pool[index + 1]
            let firstStart = min(first.startISIIndex, first.endISIIndex)
            let firstEnd = max(first.startISIIndex, first.endISIIndex)
            let secondStart = min(second.startISIIndex, second.endISIIndex)
            let secondEnd = max(second.startISIIndex, second.endISIIndex)
            guard first.trainID == second.trainID,
                  secondStart > firstEnd else {
                continue
            }

            let gapStart = firstEnd + 1
            let gapEnd = secondStart - 1
            let gapCount = max(0, gapEnd - gapStart + 1)
            guard gapCount <= maxGapCount else {
                continue
            }
            if gapCount > 0 {
                let gapValues = validISIValues(
                    train: train,
                    validFlag: validFlag,
                    start: gapStart,
                    end: gapEnd
                )
                guard gapValues.count == gapCount,
                      gapValues.allSatisfy({ $0 <= mergeBridgeUpper + tolerance(for: mergeBridgeUpper) }) else {
                    continue
                }
            }

            let start = firstStart
            let end = secondEnd
            let key = "\(start)_\(end)"
            guard seenIntervals.insert(key).inserted,
                  start > 0,
                  end < train.isiSec.count,
                  end > start else {
                continue
            }

            let values = validISIValues(
                train: train,
                validFlag: validFlag,
                start: start,
                end: end
            )
            guard !values.isEmpty,
                  let metrics = spanMetrics(train: train, start: start, end: end, settings: settings) else {
                continue
            }

            let spikeCount = end - start + 2
            guard spikeCount >= settings.minSpikes,
                  settings.longMaxSpikes <= 0 || spikeCount <= settings.longMaxSpikes else {
                continue
            }

            let coreCount = (start...end).filter { index in
                validFlag[index] &&
                    (train.isiSec[index] ?? .infinity) >= lower &&
                    (train.isiSec[index] ?? -.infinity) <= upper
            }.count
            let seedPurity = Double(coreCount) / Double(max(1, values.count))
            let bridgeCount = (start...end).filter { index in
                validFlag[index] &&
                    (train.isiSec[index] ?? -.infinity) > upper &&
                    (train.isiSec[index] ?? .infinity) <= mergeBridgeUpper
            }.count
            let bridgeFraction = Double(bridgeCount) / Double(max(1, values.count))
            let q90 = metrics.intraQ90Sec
            let q95 = metrics.intraQ95Sec
            let q90Possible = finiteLessThanOrEqual(q90, mergeBridgeUpper)
            let q90Strict = finiteLessThanOrEqual(q90, bridgeUpper)
            let q95Possible = (ratio(q95, over: mergeBridgeUpper) ?? .infinity) <= q95PossibleRatioMax
            let q95Strict = finiteLessThanOrEqual(q95, bridgeUpper)
            let leftRatio = robustFlankRatio(
                train: train,
                validFlag: validFlag,
                start: max(1, start - robustFlankCount),
                end: start - 1,
                reference: q90
            )
            let rightRatio = robustFlankRatio(
                train: train,
                validFlag: validFlag,
                start: end + 1,
                end: min(train.isiSec.count - 1, end + robustFlankCount),
                reference: q90
            )
            let robustTwoPossible = (leftRatio ?? -.infinity) >= settings.possibleBurstContrastMin &&
                (rightRatio ?? -.infinity) >= settings.possibleBurstContrastMin
            let robustTwoStrict = (leftRatio ?? -.infinity) >= settings.burstContrastMin &&
                (rightRatio ?? -.infinity) >= settings.burstContrastMin

            guard seedPurity >= seedPurityMin,
                  bridgeFraction <= bridgeFractionMax + 1e-12,
                  q90Possible,
                  q95Possible,
                  robustTwoPossible else {
                continue
            }

            let sizeLabel = burstFamilyLabel(spikeCount: spikeCount, metrics: metrics, settings: settings)
            var finalLabel: ClassicAnchorLabel
            if sizeLabel.isCanonicalBurstFamily,
               q90Strict,
               q95Strict,
               bridgeFraction <= settings.burstBridgeFractionMax + 1e-12,
               robustTwoStrict {
                finalLabel = sizeLabel
            } else {
                finalLabel = .possibleBurst
            }
            finalLabel = enabledBurstFamilyLabel(finalLabel, settings: settings)
            if !settings.enabledLabels.contains(finalLabel) {
                continue
            }

            let possibleMergePriority: Int? = finalLabel == .possibleBurst
                ? max(
                    510,
                    possibleBurstPriority(
                        metrics: metrics,
                        settings: settings,
                        coreCount: coreCount,
                        seedPurity: seedPurity,
                        bridgeFraction: bridgeFraction,
                        twoPossible: true,
                        oneSided: false,
                        q95ExcessRatio: ratio(q95, over: bridgeUpper)
                    )
                )
                : nil
            let decisionPath = [
                "burst_family_merge_bridge_gap_pass",
                "post_merge_revalidation=seed_purity_bridge_fraction_q90_q95_external_flank",
                "gap_isi_n=\(gapCount)",
                "max_gap_sec=\(formatRatio(maxGapSec(train: train, start: gapStart, end: gapEnd)))",
                "seed_purity=\(formatRatio(seedPurity))",
                "bridge_count=\(bridgeCount)",
                "bridge_fraction=\(formatRatio(bridgeFraction))",
                "q90_possible_pass=\(q90Possible)",
                "q90_strict_pass=\(q90Strict)",
                "q95_possible_pass=\(q95Possible)",
                "q95_strict_pass=\(q95Strict)",
                "robust_two_flank_possible_pass=\(robustTwoPossible)",
                "robust_two_flank_strict_pass=\(robustTwoStrict)",
                "robust_left_ratio=\(formatRatio(leftRatio))",
                "robust_right_ratio=\(formatRatio(rightRatio))",
                "structural_burst_support_weight=\(formatRatio(settings.structuralBurstSupportWeight))",
                "support_adjusted_priority=\(possibleMergePriority.map { String($0) } ?? "NA")"
            ].joined(separator: ";")
            let action = finalLabel == .possibleBurst ? "merge_to_possible" : "merge_accept"
            let score = (metrics.edgeContrastMinQ90 ?? 0) +
                0.20 * Double(coreCount) +
                0.40 * seedPurity -
                0.25 * Double(bridgeCount) -
                0.55 * bridgeFraction +
                (finalLabel == .possibleBurst ? 0 : 0.40)
            let priority: Int
            switch finalLabel {
            case .burst:
                priority = 1_265
            case .highFrequencyBurst:
                priority = 1_240
            case .longBurst:
                priority = 1_175
            case .possibleBurst:
                priority = possibleMergePriority ?? 510
            default:
                priority = 0
            }

            let refCount = refractorySuspectCount(train: train, start: start, end: end, settings: settings)
            mergedCandidates.append(
                burstCandidate(
                    train: train,
                    metrics: metrics,
                    settings: settings,
                    label: finalLabel,
                    gateStatus: finalLabel == .possibleBurst
                        ? "event_grammar_burst_family_merge_possible"
                        : "event_grammar_burst_family_merge_pass",
                    decisionPath: decisionPath,
                    action: action,
                    score: score,
                    priority: priority,
                    start: start,
                    end: end,
                    bridgeCount: bridgeCount,
                    lower: lower,
                    upper: upper,
                    lockLevel: finalLabel == .possibleBurst ? .strongCandidate : .lockedClassic,
                    edgeGeom: metrics.edgeContrastGeomQ90,
                    candidateIndex: startingCandidateIndex + mergedCandidates.count,
                    refCount: refCount,
                    refractoryAction: STPDRefractoryEvidencePolicy.effectiveAction(
                        refCount: refCount,
                        nISI: metrics.nISI,
                        requestedAction: settings.refractoryAction
                    ),
                    candidateLayer: "event_grammar_burst_episode",
                    candidateClass: "event_grammar_burst_family_merge",
                    seedRunStart: firstStart,
                    seedRunEnd: secondEnd,
                    bridgeBandUpper: mergeBridgeUpper,
                    requiredGapSec: requiredGapSec(
                        intraQ90Sec: metrics.intraQ90Sec,
                        contrast: settings.burstContrastMin
                    ),
                    possibleRequiredGapSec: requiredGapSec(
                        intraQ90Sec: metrics.intraQ90Sec,
                        contrast: settings.possibleBurstContrastMin
                    ),
                    strictBoundaryPass: robustTwoStrict,
                    possibleBoundaryPass: robustTwoPossible,
                    bridgeCountPass: bridgeCount <= settings.burstBridgeMaxCount,
                    bridgeFractionPass: bridgeFraction <= settings.burstBridgeFractionMax,
                    q90BridgePass: q90Strict,
                    sizeLabelBeforeReview: sizeLabel.rawValue
                )
            )
        }

        return mergedCandidates
    }

    private static func burstCandidate(
        train: SpikeTrain,
        metrics: SpanMetrics,
        settings: ClassicAnchorSettings,
        label: ClassicAnchorLabel,
        gateStatus: String,
        decisionPath: String,
        action: String,
        score: Double,
        priority: Int,
        start: Int,
        end: Int,
        bridgeCount: Int,
        lower: Double,
        upper: Double,
        lockLevel: ClassicAnchorLockLevel,
        edgeGeom: Double?,
        candidateIndex: Int,
        refCount: Int,
        refractoryAction: ClassicAnchorRefractoryAction? = nil,
        candidateLayer: String = "classic_anchor_burst",
        candidateClass: String? = nil,
        seedRunStart: Int? = nil,
        seedRunEnd: Int? = nil,
        bridgeBandUpper: Double? = nil,
        requiredGapSec: Double? = nil,
        possibleRequiredGapSec: Double? = nil,
        boundaryFloorSec: Double? = nil,
        boundaryFloorHard: Bool? = nil,
        strictBoundaryPass: Bool? = nil,
        possibleBoundaryPass: Bool? = nil,
        bridgeCountPass: Bool? = nil,
        bridgeFractionPass: Bool? = nil,
        q90BridgePass: Bool? = nil,
        sizeLabelBeforeReview: String? = nil,
        localBackgroundQ75Sec: Double? = nil,
        localCompressionQ90Ratio: Double? = nil,
        anchorBandSourceOverride: ClassicAnchorBandSource? = nil
    ) -> ClassicAnchorCandidate {
        let stableLayer = candidateLayer
            .replacingOccurrences(of: " ", with: "_")
            .replacingOccurrences(of: "/", with: "_")
        var candidate = ClassicAnchorCandidate(
            id: "\(train.id)-classic-anchor-burst-\(stableLayer)-\(label.rawValue)-isi-\(start)-\(end)-\(candidateIndex)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: candidateLayer,
            candidateClass: candidateClass ?? (
                bridgeCount > 0 ? "classic_anchor_seed_bridge_packet" : "classic_anchor_short_isi_packet"
            ),
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
            intraQ10Sec: metrics.intraQ10Sec,
            intraQ40Sec: metrics.intraQ40Sec,
            intraQ50Sec: metrics.intraQ50Sec,
            intraQ90Sec: metrics.intraQ90Sec,
            intraQ95Sec: metrics.intraQ95Sec,
            maxIntraISISec: metrics.maxIntraISISec,
            meanIntraISISec: metrics.meanIntraISISec,
            cv: metrics.cv,
            lv: metrics.lv,
            preGapSec: metrics.preGapSec,
            postGapSec: metrics.postGapSec,
            preRatioQ90: metrics.preRatioQ90,
            postRatioQ90: metrics.postRatioQ90,
            edgeContrastMinQ90: metrics.edgeContrastMinQ90,
            edgeContrastGeomQ90: metrics.edgeContrastGeomQ90 ?? edgeGeom,
            anchorFamily: "classic_burst",
            anchorLockLevel: lockLevel,
            anchorBandLowerSec: lower,
            anchorBandUpperSec: upper,
            anchorBandSource: anchorBandSourceOverride ?? settings.burstBandSource,
            anchorContrastMinRequired: settings.burstContrastMin,
            anchorContrastGeomRequired: settings.burstContrastGeomMin,
            refractorySuspectCount: refCount,
            refractorySuspectAction: refractoryAction
        )
        candidate.burstSeedRunStartISI = seedRunStart
        candidate.burstSeedRunEndISI = seedRunEnd
        candidate.burstSeedBandLowerSec = lower
        candidate.burstSeedBandUpperSec = upper
        candidate.burstBridgeBandUpperSec = bridgeBandUpper
        candidate.burstContrastRequired = settings.burstContrastMin
        candidate.burstPossibleContrastRequired = settings.possibleBurstContrastMin
        candidate.burstRequiredGapSec = requiredGapSec
        candidate.burstPossibleRequiredGapSec = possibleRequiredGapSec
        candidate.burstBoundaryFloorSec = boundaryFloorSec
        candidate.burstBoundaryFloorHard = boundaryFloorHard
        candidate.burstStrictBoundaryPass = strictBoundaryPass
        candidate.burstPossibleBoundaryPass = possibleBoundaryPass
        candidate.burstBridgeCountPass = bridgeCountPass
        candidate.burstBridgeFractionPass = bridgeFractionPass
        candidate.burstQ90BridgePass = q90BridgePass
        candidate.burstSizeLabelBeforeReview = sizeLabelBeforeReview
        candidate.localBackgroundQ75Sec = localBackgroundQ75Sec
        candidate.localCompressionQ90Ratio = localCompressionQ90Ratio
        candidate.eventLocalMedianSec = metrics.localMedianSec
        candidate.eventLocalPercentileMedian = metrics.localPercentileMedian
        candidate.eventLocalPercentileQ90 = metrics.localPercentileQ90
        candidate.eventLocalRobustZMedian = metrics.localRobustZMedian
        candidate.eventLocalRobustZAbsQ80 = metrics.localRobustZAbsQ80
        candidate.eventLocalRobustZQ10 = metrics.localRobustZQ10
        return candidate
    }

    private static func requiredGapSec(intraQ90Sec: Double?, contrast: Double) -> Double? {
        guard let intraQ90Sec, intraQ90Sec.isFinite else {
            return nil
        }
        return intraQ90Sec * contrast
    }

    private static func sizeLabelBeforeReview(
        spikeCount: Int,
        settings: ClassicAnchorSettings
    ) -> String {
        if spikeCount <= settings.classicMaxSpikes {
            return ClassicAnchorLabel.burst.rawValue
        }
        if spikeCount >= settings.longMinSpikes &&
            (settings.longMaxSpikes <= 0 || spikeCount <= settings.longMaxSpikes) {
            return ClassicAnchorLabel.longBurst.rawValue
        }
        return "prolonged_burst_like"
    }

    private static func burstFamilyLabel(
        spikeCount: Int,
        metrics: SpanMetrics,
        settings: ClassicAnchorSettings
    ) -> ClassicAnchorLabel {
        if highFrequencyBurstPass(metrics: metrics, spikeCount: spikeCount, settings: settings) {
            return .highFrequencyBurst
        }
        if spikeCount <= settings.classicMaxSpikes {
            return .burst
        }
        if spikeCount >= settings.longMinSpikes &&
            (settings.longMaxSpikes <= 0 || spikeCount <= settings.longMaxSpikes) &&
            longBurstUpperBandPass(metrics: metrics, settings: settings) {
            return .longBurst
        }
        return .possibleBurst
    }

    private static func highFrequencyBurstPass(
        metrics: SpanMetrics,
        spikeCount: Int,
        settings: ClassicAnchorSettings
    ) -> Bool {
        guard spikeCount >= settings.highFrequencyBurstMinSpikes,
              spikeCount <= settings.highFrequencyBurstMaxSpikes else {
            return false
        }
        let q90Limit = min(
            settings.highFrequencyBurstCoreQ90MaxSec,
            settings.effectiveBurstBandUpperSec * 0.50
        )
        return (metrics.intraQ90Sec ?? .infinity) <= q90Limit + tolerance(for: q90Limit) &&
            (metrics.edgeContrastMinQ90 ?? -.infinity) >= settings.highFrequencyBurstEdgeMin &&
            (metrics.edgeContrastGeomQ90 ?? -.infinity) >= settings.highFrequencyBurstContextContrastMin
    }

    private static func longBurstUpperBandPass(
        metrics: SpanMetrics,
        settings: ClassicAnchorSettings
    ) -> Bool {
        let lower = settings.effectiveBurstBandLowerSec
        let upper = settings.effectiveBurstBandUpperSec
        let upperBandFloor = lower + 0.60 * max(0, upper - lower)
        return (metrics.intraQ10Sec ?? -.infinity) >= upperBandFloor - tolerance(for: upperBandFloor) &&
            (metrics.intraQ90Sec ?? .infinity) <= upper + tolerance(for: upper)
    }

    private static func enabledBurstFamilyLabel(
        _ label: ClassicAnchorLabel,
        settings: ClassicAnchorSettings
    ) -> ClassicAnchorLabel {
        if settings.enabledLabels.contains(label) {
            return label
        }
        switch label {
        case .highFrequencyBurst:
            return settings.enabledLabels.contains(.burst) ? .burst : .possibleBurst
        case .longBurst:
            return settings.enabledLabels.contains(.burst) ? .burst : .possibleBurst
        default:
            return label
        }
    }

    private static func burstFamilyPriority(
        label: ClassicAnchorLabel,
        classic: Int,
        highFrequency: Int,
        long: Int
    ) -> Int {
        switch label {
        case .highFrequencyBurst:
            return highFrequency
        case .longBurst:
            return long
        default:
            return classic
        }
    }

    private static func gateStatus(strict: Bool, boundary: Bool, bridged: Bool) -> String {
        if boundary {
            return strict ? "classic_anchor_one_sided_boundary_burst_pass" : "classic_anchor_possible_boundary_burst"
        }
        if bridged {
            return strict ? "classic_anchor_seed_bridge_burst_pass" : "classic_anchor_possible_seed_bridge_burst"
        }
        return strict ? "classic_anchor_two_sided_burst_pass" : "classic_anchor_possible_burst"
    }

    private static func decisionPath(strict: Bool, boundary: Bool, bridged: Bool) -> String {
        if boundary {
            return strict
                ? "classic_anchor_boundary_burst_band_with_single_flank_q90_contrast"
                : "classic_anchor_boundary_burst_band_with_partial_single_flank_contrast"
        }
        if bridged {
            return strict
                ? "classic_anchor_seed_bridge_band_with_clear_pre_post_q90_contrast"
                : "classic_anchor_seed_bridge_band_with_partial_contrast"
        }
        return strict
            ? "classic_anchor_continuous_burst_band_with_clear_pre_post_q90_contrast"
            : "classic_anchor_continuous_burst_band_with_partial_contrast"
    }

    /// Maximum fraction by which the largest internal ISI of a strongly two-sided compact
    /// cluster may exceed the adaptive compactness upper and still be rescued as a classic
    /// burst (the "tolerated internal tail"). This is NOT a global burst threshold: it only
    /// loosens the per-span max-internal-ISI compactness gate, and only when the bulk
    /// (intraQ90) is already compact AND even the largest internal ISI stays >= the burst
    /// flank contrast below both boundary gaps. It exists so a clearly two-sided packet is not
    /// erased because one internal ISI is fractionally above the adaptive compactness upper,
    /// without admitting moderate tonic runs (which lack strong two-sided gaps).
    private static let maxIntraToleratedTailRatio = 1.25

    private static func burstCompactnessUpper(
        train: SpikeTrain,
        validFlag: [Bool],
        settings: ClassicAnchorSettings
    ) -> Double? {
        guard train.isiSec.count == validFlag.count else {
            return nil
        }
        let values = train.isiSec.indices.compactMap { index -> Double? in
            guard index > 0,
                  validFlag[index],
                  let isi = train.isiSec[index],
                  isi.isFinite else {
                return nil
            }
            return isi
        }
        guard values.count >= 8,
              let q80 = quantile(values, probability: 0.80),
              q80.isFinite,
              q80 > settings.minValidISISec else {
            return nil
        }
        return max(
            settings.minValidISISec,
            q80 * settings.burstEpisodeBackgroundFraction
        )
    }

    private static func burstCompactnessPass(
        intraQ90Sec: Double?,
        compactUpperSec: Double?
    ) -> Bool {
        guard let intraQ90Sec,
              intraQ90Sec.isFinite,
              let compactUpperSec,
              compactUpperSec.isFinite,
              compactUpperSec > 0 else {
            return true
        }
        return intraQ90Sec <= compactUpperSec + tolerance(for: compactUpperSec)
    }

    private static func burstRejectionContext(
        metrics: SpanMetrics,
        settings: ClassicAnchorSettings,
        seedPurity: Double,
        bridgeFraction: Double,
        localBackground: Double?,
        structuralRatio: Double?,
        q95ExcessRatio: Double?,
        boundarySide: String? = nil
    ) -> [String] {
        var context = [
            "pre_ratio=\(formatRatio(metrics.preRatioQ90))",
            "post_ratio=\(formatRatio(metrics.postRatioQ90))",
            "strict_required=\(formatRatio(settings.burstContrastMin))",
            "possible_required=\(formatRatio(settings.possibleBurstContrastMin))",
            "seed_purity=\(formatRatio(seedPurity))",
            "bridge_fraction=\(formatRatio(bridgeFraction))",
            "local_background_q75=\(formatRatio(localBackground))",
            "local_compression_q90=\(formatRatio(structuralRatio))",
            "structural_rescue_required=\(formatRatio(settings.burstStructuralRescueCompressionMin))"
        ]
        if let q95ExcessRatio, q95ExcessRatio.isFinite {
            context.append("q95_excess_ratio=\(formatRatio(q95ExcessRatio))")
        }
        if let boundarySide, !boundarySide.isEmpty {
            context.append("boundary_side=\(boundarySide)")
        }
        return context
    }

    private struct SpanMetrics {
        let nISI: Int
        let nValidISI: Int
        let nSpikes: Int
        let durationSec: Double?
        let intraQ10Sec: Double?
        let intraQ40Sec: Double?
        let intraQ50Sec: Double?
        let intraQ90Sec: Double?
        let intraQ95Sec: Double?
        let maxIntraISISec: Double?
        let meanIntraISISec: Double?
        let cv: Double?
        let lv: Double?
        let preGapSec: Double?
        let postGapSec: Double?
        let preRatioQ90: Double?
        let postRatioQ90: Double?
        let edgeContrastMinQ90: Double?
        let edgeContrastGeomQ90: Double?
        let localMedianSec: Double?
        let localPercentileMedian: Double?
        let localPercentileQ90: Double?
        let localRobustZMedian: Double?
        let localRobustZAbsQ80: Double?
        let localRobustZQ10: Double?
    }

    private struct BurstRun {
        let start: Int
        let end: Int
        let seedStart: Int
        let seedEnd: Int
        let seedCount: Int
        let bridgeCount: Int
    }

    private static func spanMetrics(
        train: SpikeTrain,
        start: Int,
        end: Int,
        settings: ClassicAnchorSettings
    ) -> SpanMetrics? {
        guard start > 0,
              end >= start,
              end < train.isiSec.count,
              train.timestampsSec.indices.contains(start - 1),
              train.timestampsSec.indices.contains(end) else {
            return nil
        }

        let values = (start...end).compactMap { index -> Double? in
            guard let isi = train.isiSec[index],
                  isi.isFinite,
                  !isArtifactISI(isi, threshold: settings.minValidISISec) else {
                return nil
            }
            return isi
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
        let pre = start > 1 ? finiteISI(train.isiSec[start - 1]) : nil
        let post = end < train.isiSec.count - 1 ? finiteISI(train.isiSec[end + 1]) : nil
        let preRatio = ratio(pre, over: q90)
        let postRatio = ratio(post, over: q90)
        let edgeMin = finiteMin(preRatio, postRatio)
        let edgeGeom = edgeGeomRatio(preRatio, postRatio)
        let duration = train.timestampsSec[end] - train.timestampsSec[start - 1]
        let localContext = localContextMetrics(
            train: train,
            start: start,
            end: end,
            settings: settings
        )

        return SpanMetrics(
            nISI: end - start + 1,
            nValidISI: values.count,
            nSpikes: end - start + 2,
            durationSec: duration.isFinite ? duration : nil,
            intraQ10Sec: q10,
            intraQ40Sec: q40,
            intraQ50Sec: q50,
            intraQ90Sec: q90,
            intraQ95Sec: q95,
            maxIntraISISec: values.max(),
            meanIntraISISec: mean(values),
            cv: STPDStatistics.coefficientOfVariation(values),
            lv: STPDStatistics.localVariation(values),
            preGapSec: pre,
            postGapSec: post,
            preRatioQ90: preRatio,
            postRatioQ90: postRatio,
            edgeContrastMinQ90: edgeMin,
            edgeContrastGeomQ90: edgeGeom,
            localMedianSec: localContext.localMedianSec,
            localPercentileMedian: localContext.localPercentileMedian,
            localPercentileQ90: localContext.localPercentileQ90,
            localRobustZMedian: localContext.localRobustZMedian,
            localRobustZAbsQ80: localContext.localRobustZAbsQ80,
            localRobustZQ10: localContext.localRobustZQ10
        )
    }

    // BCB-1: an edge ISI outside the compact core, more than this multiple of the candidate's own CORE median, is an
    // incompatible boundary and is trimmed. Scale-free ratio-to-core (profile-relative), NOT a fixed-ms cutoff. The two
    // ratios are ASYMMETRIC because a burst has an abrupt onset but a gradually lengthening tail: a slow LEADING ISI
    // (> 2× core) is almost never part of a fast-onset burst, whereas a modestly longer TRAILING ISI is usually a
    // natural tail — only a SEVERAL-times-core trailing ISI (>= the burst flank-contrast multiple) is a pulled-in gap.
    // A single symmetric threshold cannot do both: the 5x5 burst_response_2_s regression case needs a 2.3× leading edge trimmed while a clean
    // simulated burst needs its ~2.5× natural trailing tail kept.
    private static let coreFirstBoundaryLeadingEdgeRatioMax = 2.0
    private static let coreFirstBoundaryTrailingEdgeRatioMax = 3.0
    private static let coreFirstBoundaryMaxTrimPerSide = 2

    /// BCB-1 — CORE-FIRST burst boundary trim (default path; runs regardless of the Adaptive-v2 flag). For each CANONICAL
    /// burst candidate the COMPACT CORE — ISIs inside the automatic/structural core reference band — is the anchor; edge
    /// ISIs outside that core are optional extensions. A leading extension > `coreFirstBoundaryLeadingEdgeRatioMax ×
    /// coreMedian` or a trailing extension > `coreFirstBoundaryTrailingEdgeRatioMax × coreMedian` is INCOMPATIBLE and is
    /// trimmed (≤ `coreFirstBoundaryMaxTrimPerSide` per side), walking inward from each side but NEVER crossing into the
    /// core. Compatible extensions (inside the core reference band, or only modestly above the core) are preserved; the
    /// core is never erased.
    ///
    /// Profile-relative (no fixed-ms window) and flank-agnostic. Skipped under a user hard gate or an explicit pattern-ISI
    /// band (those express verbatim user intent and have their own boundary handling, so BCB-1 stays byte-identical there),
    /// and for tolerated-internal-tail rescues (which deliberately accept a tail under strong two-sided boundary evidence).
    /// The trimmed candidate keeps the original candidate's score (via `withGeometry`), so it wins de-duplication over a
    /// narrower sibling, preserving the `core_first_boundary_trim(...)` provenance.
    private static func coreFirstBoundaryTrimmedCandidates(
        _ candidates: [ClassicAnchorCandidate],
        train: SpikeTrain,
        settings: ClassicAnchorSettings
    ) -> [ClassicAnchorCandidate] {
        guard !settings.burstHardThresholdEnabled,
              settings.burstBandSource != .userPatternISILimit else { return candidates }
        let lower = settings.effectiveBurstBandLowerSec
        let coreReferenceUpper = settings.burstCoreReferenceUpperSec ?? settings.effectiveBurstBandUpperSec
        let upper = min(settings.effectiveBurstBandUpperSec, coreReferenceUpper)
        guard lower.isFinite, upper.isFinite, upper > 0, lower >= 0, upper >= lower else { return candidates }
        func isSeed(_ index: Int) -> Bool {
            guard train.isiSec.indices.contains(index), let v = train.isiSec[index], v.isFinite,
                  !isArtifactISI(v, threshold: settings.minValidISISec) else { return false }
            return v >= lower && v <= upper
        }
        return candidates.map { candidate in
            guard candidate.finalLabel.isCanonicalBurstFamily,
                  candidate.startISIIndex >= 0,
                  candidate.endISIIndex < train.isiSec.count,
                  candidate.endISIIndex > candidate.startISIIndex,
                  // Do not undo the tolerated-internal-tail rescue (it accepted a tail under strong two-sided boundaries).
                  !candidate.gateStatus.contains("tolerated_internal_tail") else { return candidate }
            let start = candidate.startISIIndex, end = candidate.endISIIndex
            // Core = the candidate's seed-band ISIs; we need a real core to anchor (never erase it).
            let coreISIs: [Double] = (start...end).compactMap { isSeed($0) ? train.isiSec[$0] ?? nil : nil }
            guard coreISIs.count >= Swift.max(1, settings.burstCoreMinISI) else { return candidate }
            let sortedCore = coreISIs.sorted()
            let coreMedian = sortedCore[sortedCore.count / 2]
            guard coreMedian > 0 else { return candidate }
            let leadingRatioLimit = coreMedian * coreFirstBoundaryLeadingEdgeRatioMax
            let trailingRatioLimit = coreMedian * coreFirstBoundaryTrailingEdgeRatioMax
            // Walk inward from both edges, trimming only INCOMPATIBLE non-core extensions; stop at the core or a
            // compatible bridge. This keeps the compact core and any compatible extension intact. The leading edge uses
            // the strict onset ratio; the trailing edge uses the lenient offset ratio so natural burst tails survive.
            var newStart = start
            var newEnd = end
            var dropLeft = 0
            var dropRight = 0
            while newStart < end, dropLeft < coreFirstBoundaryMaxTrimPerSide, !isSeed(newStart),
                  let v = train.isiSec[newStart], v.isFinite, v > leadingRatioLimit {
                newStart += 1
                dropLeft += 1
            }
            while newEnd > newStart, dropRight < coreFirstBoundaryMaxTrimPerSide, !isSeed(newEnd),
                  let v = train.isiSec[newEnd], v.isFinite, v > trailingRatioLimit {
                newEnd -= 1
                dropRight += 1
            }
            guard dropLeft > 0 || dropRight > 0,
                  newEnd >= newStart,
                  let m = spanMetrics(train: train, start: newStart, end: newEnd, settings: settings) else { return candidate }
            let note = ";core_first_boundary_trim(left=\(dropLeft),right=\(dropRight),reason=edge_ratio_to_core)"
            return candidate.withGeometry(
                startISIIndex: newStart, endISIIndex: newEnd,
                startSpikeIndex: candidate.startSpikeIndex + dropLeft, endSpikeIndex: candidate.endSpikeIndex - dropRight,
                nISI: m.nISI, nValidISI: m.nValidISI, nSpikes: m.nSpikes, durationSec: m.durationSec,
                intraQ10Sec: m.intraQ10Sec, intraQ40Sec: m.intraQ40Sec, intraQ50Sec: m.intraQ50Sec,
                intraQ90Sec: m.intraQ90Sec, intraQ95Sec: m.intraQ95Sec,
                maxIntraISISec: m.maxIntraISISec, meanIntraISISec: m.meanIntraISISec, cv: m.cv, lv: m.lv,
                preGapSec: m.preGapSec, postGapSec: m.postGapSec, preRatioQ90: m.preRatioQ90, postRatioQ90: m.postRatioQ90,
                edgeContrastMinQ90: m.edgeContrastMinQ90, edgeContrastGeomQ90: m.edgeContrastGeomQ90,
                decisionPath: candidate.decisionPath + note)
        }
    }

    /// BCB-1 core-first boundary trimming: produce a burst candidate for the sub-span obtained by dropping at most ONE ISI from the
    /// left and/or right of `original`, with all span metrics recomputed via `spanMetrics`. Returns nil if the trimmed
    /// span is empty or its metrics cannot be computed. The caller re-evaluates the canonicalization verdict on the
    /// result; this function performs no eligibility decision of its own. Interior ISIs are never removed.
    static func boundaryTrimmedBurstCandidate(
        from original: ClassicAnchorCandidate,
        train: SpikeTrain,
        dropLeft: Bool,
        dropRight: Bool,
        settings: ClassicAnchorSettings,
        note: String
    ) -> ClassicAnchorCandidate? {
        guard dropLeft || dropRight else { return nil }
        let newStart = original.startISIIndex + (dropLeft ? 1 : 0)
        let newEnd = original.endISIIndex - (dropRight ? 1 : 0)
        guard newEnd >= newStart else { return nil }
        guard let m = spanMetrics(train: train, start: newStart, end: newEnd, settings: settings) else { return nil }
        let newStartSpike = original.startSpikeIndex + (dropLeft ? 1 : 0)
        let newEndSpike = original.endSpikeIndex - (dropRight ? 1 : 0)
        return original.withGeometry(
            startISIIndex: newStart, endISIIndex: newEnd,
            startSpikeIndex: newStartSpike, endSpikeIndex: newEndSpike,
            nISI: m.nISI, nValidISI: m.nValidISI, nSpikes: m.nSpikes, durationSec: m.durationSec,
            intraQ10Sec: m.intraQ10Sec, intraQ40Sec: m.intraQ40Sec, intraQ50Sec: m.intraQ50Sec,
            intraQ90Sec: m.intraQ90Sec, intraQ95Sec: m.intraQ95Sec,
            maxIntraISISec: m.maxIntraISISec, meanIntraISISec: m.meanIntraISISec, cv: m.cv, lv: m.lv,
            preGapSec: m.preGapSec, postGapSec: m.postGapSec, preRatioQ90: m.preRatioQ90, postRatioQ90: m.postRatioQ90,
            edgeContrastMinQ90: m.edgeContrastMinQ90, edgeContrastGeomQ90: m.edgeContrastGeomQ90,
            decisionPath: original.decisionPath + note
        )
    }

    private static func refractorySuspectCount(
        train: SpikeTrain,
        start: Int,
        end: Int,
        settings: ClassicAnchorSettings
    ) -> Int {
        guard settings.refractorySuspectSec > settings.minValidISISec else {
            return 0
        }

        return (start...end).filter { index in
            guard let value = train.isiSec[index], value.isFinite else {
                return false
            }
            return isRefractorySuspectISI(
                value,
                minValidISISec: settings.minValidISISec,
                refractorySuspectSec: settings.refractorySuspectSec
            )
        }.count
    }

    private static func boolRuns(_ flag: [Bool]) -> [(start: Int, end: Int)] {
        var runs: [(start: Int, end: Int)] = []
        var start: Int?

        for index in flag.indices {
            if flag[index] {
                if start == nil {
                    start = index
                }
            } else if let openStart = start {
                runs.append((openStart, index - 1))
                start = nil
            }
        }

        if let openStart = start {
            runs.append((openStart, flag.count - 1))
        }

        return runs
    }

    private static func validBurstFlag(
        train: SpikeTrain,
        minValidISISec: Double
    ) -> [Bool] {
        train.isiSec.indices.map { index in
            guard index > 0,
                  let isi = train.isiSec[index],
                  isi.isFinite,
                  !isArtifactISI(isi, threshold: minValidISISec) else {
                return false
            }
            return true
        }
    }

    private static func validISIValues(
        train: SpikeTrain,
        validFlag: [Bool],
        start: Int,
        end: Int
    ) -> [Double] {
        guard start <= end,
              train.isiSec.count == validFlag.count else {
            return []
        }
        return (start...end).compactMap { index -> Double? in
            guard train.isiSec.indices.contains(index),
                  validFlag[index],
                  let isi = train.isiSec[index],
                  isi.isFinite else {
                return nil
            }
            return isi
        }
    }

    private static func validISIValues(
        train: SpikeTrain,
        start: Int,
        end: Int,
        minValidISISec: Double
    ) -> [Double] {
        guard start <= end else {
            return []
        }
        return (start...end).compactMap { index -> Double? in
            guard train.isiSec.indices.contains(index),
                  index > 0,
                  let isi = train.isiSec[index],
                  isi.isFinite,
                  !isArtifactISI(isi, threshold: minValidISISec) else {
                return nil
            }
            return isi
        }
    }

    private static func localBackgroundQ75(
        train: SpikeTrain,
        validFlag: [Bool],
        start: Int,
        end: Int,
        radius: Int = 12
    ) -> Double? {
        guard radius > 0,
              start <= end,
              train.isiSec.count == validFlag.count else {
            return nil
        }

        let leftStart = max(1, start - radius)
        let leftEnd = start - 1
        let rightStart = end + 1
        let rightEnd = min(train.isiSec.count - 1, end + radius)
        var values: [Double] = []

        if leftStart <= leftEnd {
            values.append(
                contentsOf: validISIValues(
                    train: train,
                    validFlag: validFlag,
                    start: leftStart,
                    end: leftEnd
                )
            )
        }
        if rightStart <= rightEnd {
            values.append(
                contentsOf: validISIValues(
                    train: train,
                    validFlag: validFlag,
                    start: rightStart,
                    end: rightEnd
                )
            )
        }

        return quantile(values, probability: 0.75)
    }

    private struct SpanLocalContextMetrics {
        let localMedianSec: Double?
        let localPercentileMedian: Double?
        let localPercentileQ90: Double?
        let localRobustZMedian: Double?
        let localRobustZAbsQ80: Double?
        let localRobustZQ10: Double?

        static let empty = SpanLocalContextMetrics(
            localMedianSec: nil,
            localPercentileMedian: nil,
            localPercentileQ90: nil,
            localRobustZMedian: nil,
            localRobustZAbsQ80: nil,
            localRobustZQ10: nil
        )
    }

    private static func localContextMetrics(
        train: SpikeTrain,
        start: Int,
        end: Int,
        settings: ClassicAnchorSettings,
        radius: Int = 12
    ) -> SpanLocalContextMetrics {
        guard radius > 0,
              start <= end else {
            return SpanLocalContextMetrics.empty
        }

        let eventValues = validISIValues(
            train: train,
            start: start,
            end: end,
            minValidISISec: settings.minValidISISec
        )
        guard !eventValues.isEmpty else {
            return SpanLocalContextMetrics.empty
        }

        var localValues: [Double] = []
        let leftStart = max(1, start - radius)
        let leftEnd = start - 1
        let rightStart = end + 1
        let rightEnd = min(train.isiSec.count - 1, end + radius)
        if leftStart <= leftEnd {
            localValues.append(
                contentsOf: validISIValues(
                    train: train,
                    start: leftStart,
                    end: leftEnd,
                    minValidISISec: settings.minValidISISec
                )
            )
        }
        if rightStart <= rightEnd {
            localValues.append(
                contentsOf: validISIValues(
                    train: train,
                    start: rightStart,
                    end: rightEnd,
                    minValidISISec: settings.minValidISISec
                )
            )
        }

        let cleanLocal = localValues.filter { $0.isFinite && $0 > 0 }.sorted()
        guard !cleanLocal.isEmpty else {
            return SpanLocalContextMetrics.empty
        }

        let localSample = SortedFiniteSample(sortedFiniteValues: cleanLocal)
        let localMedian = localSample.quantile(0.50)
        let localPercentiles = eventValues.map { percentileRank($0, in: cleanLocal) }
        let localLogs = cleanLocal.map(log)
        let localLogSample = SortedFiniteSample(localLogs)
        let localMedianLog = localLogSample.quantile(0.50)
        let localMADLog = localMedianLog.flatMap { medianLog in
            SortedFiniteSample(localLogs.map { abs($0 - medianLog) }).quantile(0.50)
        }
        let robustZValues: [Double] = eventValues.compactMap { value in
            guard value.isFinite,
                  value > 0,
                  let localMedianLog,
                  let localMADLog else {
                return nil
            }
            let scale = max(localMADLog * 1.4826, 1e-12)
            return (log(value) - localMedianLog) / scale
        }

        let percentileSample = SortedFiniteSample(localPercentiles)
        let robustZSample = SortedFiniteSample(robustZValues)
        let robustZAbsSample = SortedFiniteSample(robustZValues.map(abs))
        return SpanLocalContextMetrics(
            localMedianSec: localMedian,
            localPercentileMedian: percentileSample.quantile(0.50),
            localPercentileQ90: percentileSample.quantile(0.90),
            localRobustZMedian: robustZSample.quantile(0.50),
            localRobustZAbsQ80: robustZAbsSample.quantile(0.80),
            localRobustZQ10: robustZSample.quantile(0.10)
        )
    }

    private static func maxGapSec(train: SpikeTrain, start: Int, end: Int) -> Double? {
        guard start <= end else {
            return 0
        }
        let values = (start...end).compactMap { index -> Double? in
            guard train.isiSec.indices.contains(index),
                  let value = train.isiSec[index],
                  value.isFinite else {
                return nil
            }
            return value
        }
        return values.max()
    }

    private static func fraction(
        _ values: [Double],
        matching predicate: (Double) -> Bool
    ) -> Double {
        guard !values.isEmpty else {
            return .nan
        }
        let count = values.filter(predicate).count
        return Double(count) / Double(values.count)
    }

    private static func leftExtensions(seedStart: Int, bridgeFlag: [Bool], maxSteps: Int) -> [Int] {
        var starts = [seedStart]
        var current = seedStart
        var steps = 0

        while current > 1 && steps < maxSteps {
            let candidate = current - 1
            guard bridgeFlag.indices.contains(candidate), bridgeFlag[candidate] else {
                break
            }
            starts.append(candidate)
            current = candidate
            steps += 1
        }

        return starts
    }

    private static func rightExtensions(seedEnd: Int, bridgeFlag: [Bool], maxSteps: Int) -> [Int] {
        var ends = [seedEnd]
        var current = seedEnd
        var steps = 0

        while current < bridgeFlag.count - 1 && steps < maxSteps {
            let candidate = current + 1
            guard bridgeFlag.indices.contains(candidate), bridgeFlag[candidate] else {
                break
            }
            ends.append(candidate)
            current = candidate
            steps += 1
        }

        return ends
    }

    private static func isArtifactISI(_ value: Double, threshold: Double) -> Bool {
        let tolerance = max(1e-12, abs(threshold) * 1e-6)
        return value < threshold - tolerance
    }

    private static func isRefractorySuspectISI(
        _ value: Double,
        minValidISISec: Double,
        refractorySuspectSec: Double
    ) -> Bool {
        let minTolerance = max(1e-12, abs(minValidISISec) * 1e-6)
        let refractoryTolerance = max(1e-12, abs(refractorySuspectSec) * 1e-6)
        return value >= minValidISISec - minTolerance &&
            value < refractorySuspectSec - refractoryTolerance
    }

    private static func finiteISI(_ value: Double?) -> Double? {
        guard let value, value.isFinite else {
            return nil
        }
        return value
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

    private static func finiteLessThanOrEqual(_ value: Double?, _ limit: Double) -> Bool {
        guard let value, value.isFinite, limit.isFinite else {
            return false
        }
        return value <= limit
    }

    private static func finiteMin(_ first: Double?, _ second: Double?) -> Double? {
        let values = [first, second].compactMap { value in
            value.flatMap { $0.isFinite ? $0 : nil }
        }
        return values.min()
    }

    private static func finiteMax(_ values: Double?...) -> Double? {
        values.compactMap { value in
            value.flatMap { $0.isFinite ? $0 : nil }
        }
        .max()
    }

    private static func robustFlankRatio(
        train: SpikeTrain,
        validFlag: [Bool],
        start: Int,
        end: Int,
        reference: Double?
    ) -> Double? {
        guard let reference,
              reference.isFinite,
              reference > 0,
              start <= end,
              train.isiSec.count == validFlag.count else {
            return nil
        }
        let lower = max(1, start)
        let upper = min(train.isiSec.count - 1, end)
        guard lower <= upper else {
            return nil
        }
        let values = validISIValues(
            train: train,
            validFlag: validFlag,
            start: lower,
            end: upper
        )
        guard let maxValue = values.max(), maxValue.isFinite else {
            return nil
        }
        return maxValue / reference
    }

    private static func possibleBurstPriority(
        metrics: SpanMetrics,
        settings: ClassicAnchorSettings,
        coreCount: Int,
        seedPurity: Double,
        bridgeFraction: Double,
        twoPossible: Bool,
        oneSided: Bool,
        q95ExcessRatio: Double?,
        oneSidedContrast: Double? = nil
    ) -> Int {
        let contrast = oneSided
            ? max(metrics.edgeContrastMinQ90 ?? 0, oneSidedContrast ?? 0)
            : (metrics.edgeContrastMinQ90 ?? 0)
        let closeness = settings.burstContrastMin > 0
            ? min(1.5, contrast / settings.burstContrastMin)
            : 0
        let structuralSupport = min(1, max(0, settings.structuralBurstSupportWeight))
        var priority = 260 +
            180 * closeness +
            120 * min(1, max(0, seedPurity)) +
            20 * min(5, Double(max(0, coreCount))) +
            70 * structuralSupport
        if twoPossible {
            priority += 80
        }
        if oneSided {
            priority += 50
        }
        if let q95ExcessRatio,
           q95ExcessRatio.isFinite,
           q95ExcessRatio > 1 {
            priority -= 60 * min(2, q95ExcessRatio - 1)
        }
        if bridgeFraction.isFinite {
            priority -= (120 - 30 * structuralSupport) * max(0, bridgeFraction - 0.35)
        }
        return Int(max(180, min(760, priority)).rounded())
    }

    private static func possibleBurstBoundaryAuditFields(
        metrics: SpanMetrics,
        bridgeUpper: Double
    ) -> [String] {
        let preRatio = metrics.preRatioQ90 ?? -.infinity
        let postRatio = metrics.postRatioQ90 ?? -.infinity
        let dominantFlankSide = preRatio >= postRatio ? "pre" : "post"
        let weakFlankSec = dominantFlankSide == "pre" ? metrics.postGapSec : metrics.preGapSec
        let weakFlankBridgeable = weakFlankSec.map {
            $0 <= bridgeUpper + tolerance(for: bridgeUpper)
        } ?? false
        let weakFlankRole = possibleBurstWeakFlankRole(
            weakFlankSec: weakFlankSec,
            bridgeable: weakFlankBridgeable
        )

        return [
            "possible_burst_structural_definition=compact_short_isi_run_with_single_or_possible_flank_and_missing_or_nonbridgeable_opposite_boundary",
            "dominant_flank_side=\(dominantFlankSide)",
            "weak_flank_sec=\(formatRatio(weakFlankSec))",
            "weak_flank_role=\(weakFlankRole)",
            "weak_flank_bridgeable=\(weakFlankBridgeable)",
            "weak_flank_not_absorbable_as_burst_bridge=\(!weakFlankBridgeable)"
        ]
    }

    private static func possibleBurstWeakFlankRole(
        weakFlankSec: Double?,
        bridgeable: Bool
    ) -> String {
        guard let weakFlankSec, weakFlankSec.isFinite else {
            return "missing_or_invalid_boundary"
        }
        return bridgeable ? "bridgeable_bridge_candidate" : "nonbridgeable_boundary"
    }

    private static func formatRatio(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.4g", value)
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }

    private static func edgeGeomRatio(_ first: Double?, _ second: Double?) -> Double? {
        guard let first,
              let second,
              first.isFinite,
              second.isFinite else {
            return nil
        }
        return sqrt(first * second)
    }

    private static func quantile(_ values: [Double], probability: Double) -> Double? {
        let sorted = values.filter(\.isFinite).sorted()
        guard !sorted.isEmpty else {
            return nil
        }
        guard sorted.count > 1 else {
            return sorted[0]
        }

        let p = min(max(probability, 0), 1)
        let h = (Double(sorted.count) - 1) * p + 1
        let lowerIndex = max(0, Int(floor(h)) - 1)
        let upperIndex = max(0, Int(ceil(h)) - 1)
        let lowerValue = sorted[min(lowerIndex, sorted.count - 1)]
        let upperValue = sorted[min(upperIndex, sorted.count - 1)]
        return lowerValue + (h - floor(h)) * (upperValue - lowerValue)
    }

    private static func percentileRank(_ value: Double, in sortedValues: [Double]) -> Double {
        guard value.isFinite,
              !sortedValues.isEmpty else {
            return 0
        }
        let count = sortedValues.filter { $0 <= value + tolerance(for: value) }.count
        return min(1, max(0, Double(count) / Double(sortedValues.count)))
    }

    private static func mean(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard !finite.isEmpty else {
            return nil
        }
        return finite.reduce(0, +) / Double(finite.count)
    }

}
