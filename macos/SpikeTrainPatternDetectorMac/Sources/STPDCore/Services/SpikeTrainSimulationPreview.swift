import Foundation

/// SIM-1C — a pure, deterministic preview MODEL (no UI): generate a ladder of simulated tonic trains at increasing
/// jitter (regularity) levels and evaluate each with the SAME `STPDStatistics` and the REAL detector
/// (`ClassicAnchorDetectionPipeline.run`) on an isolated synthetic `SpikeDataset`. It is a pure value model for a future
/// SIM-1D UI: it never touches `RasterDocument`, the live dataset/detection run, manual thresholds, annotations, review
/// state, or CSV export, and it runs no R at runtime. Verdict reasons are machine-readable tokens (UI copy is SIM-1D).

/// Why a generated tonic train did or did not read as a clean tonic that passes the whole-train regularity gates. The
/// first failing check in precedence order (spike count → CV → CV2 → LV → detector clean-tonic); `.passes` otherwise.
public enum SpikeTrainSimulationPreviewReason: String, Sendable, Equatable, CaseIterable {
    case passes
    case tooFewSpikes = "too_few_spikes"
    case cvTooHigh = "cv_too_high"
    case cv2TooHigh = "cv2_too_high"
    case lvTooHigh = "lv_too_high"
    case notCleanTonic = "not_clean_tonic"

    /// SIM-1D: Chinese-source display label, routed through `STPDLocalization` for English. The raw `rawValue` machine
    /// token is unchanged and is never localized; only this display label is.
    public var localizationSourceZH: String {
        switch self {
        case .passes: return "标准强直"
        case .tooFewSpikes: return "棘波过少"
        case .cvTooHigh: return "CV 过高"
        case .cv2TooHigh: return "CV2 过高"
        case .lvTooHigh: return "LV 过高"
        case .notCleanTonic: return "非纯强直"
        }
    }

    /// The localized display label for `language` (zh returns the source verbatim; en via the shared dictionary).
    public func localizedLabel(language: STPDLanguage) -> String {
        STPDLocalization.text(localizationSourceZH, language: language)
    }

    /// Whether this verdict is the clean-tonic pass (drives the UI accent / pass styling).
    public var isPass: Bool { self == .passes }
}

/// Input for the preview ladder. Defaults reproduce the R-reference tonic baseline and a 5-rung jitter ladder; the
/// detector gate thresholds and the pipeline state tuning default to the existing detector defaults.
public struct SpikeTrainSimulationPreviewConfig: Sendable, Equatable {
    public var baseSeed: UInt64
    public var durationSec: Double
    public var tonicMeanSec: Double
    /// Tonic ISI standard deviations (the "regularity ladder"), low → high jitter.
    public var jitterLadderSec: [Double]
    /// Safe-default truncation strategy: each rung truncates to `[max(lowerFloorSec, mean − windowSigma·sd), mean +
    /// windowSigma·sd]`, so the window WIDENS with jitter and the requested spread is actually realized (a fixed
    /// [0.38, 0.52] window would clamp high-jitter rungs and hide the pass→fail transition).
    public var windowSigma: Double
    public var lowerFloorSec: Double
    /// The state-pattern tuning. SINGLE source of truth: it is passed to the isolated detector run that produces
    /// `selectedLabels`, AND its tonic gate fields (`tonicMinSpikes/tonicCVMax/tonicCV2Max/tonicLVMax`) are the
    /// whole-train regularity-gate thresholds — so the same tuning drives the detector and the verdict (SIM-1E feeds the
    /// app's CURRENT tuning here; the default reproduces the detector defaults for tests/previews).
    public var stateTuning: StatePatternDetectorTuning

    public init(
        baseSeed: UInt64 = 20_260_624,
        durationSec: Double = 30,
        tonicMeanSec: Double = 0.45,
        jitterLadderSec: [Double] = [0.02, 0.03, 0.06, 0.12, 0.24],
        windowSigma: Double = 3.0,
        lowerFloorSec: Double = 0.005,
        stateTuning: StatePatternDetectorTuning = StatePatternDetectorTuning()
    ) {
        self.baseSeed = baseSeed
        self.durationSec = durationSec
        self.tonicMeanSec = tonicMeanSec
        self.jitterLadderSec = jitterLadderSec
        self.windowSigma = windowSigma
        self.lowerFloorSec = lowerFloorSec
        self.stateTuning = stateTuning
    }
}

/// One rung of the ladder: the generated train + its statistics + the detector's verdict.
public struct SpikeTrainSimulationPreviewRow: Sendable, Equatable {
    public let jitterSDSec: Double
    public let lowerSec: Double
    public let upperSec: Double
    public let train: SpikeTrain
    public let spikeCount: Int
    public let isiCount: Int
    public let meanISISec: Double?
    public let cv: Double?
    public let cv2: Double?
    public let lv: Double?
    /// Sorted, de-duplicated raw detector labels selected for this train (machine tokens, never localized).
    public let selectedLabels: [String]
    /// True when the detector selected ONLY tonic (`selectedLabels == ["tonic"]`).
    public let isCleanTonic: Bool
    /// True when the whole-train CV/CV2/LV are within gates AND the spike count meets `tonicMinSpikes` (does NOT require
    /// clean-tonic — that is folded into `reason`).
    public let passesTonicRegularityGates: Bool
    public let reason: SpikeTrainSimulationPreviewReason
}

/// SIM-1F — one ladder rung evaluated against TWO tunings (the detector defaults vs. the supplied/current settings) on
/// the SAME generated train, so any verdict difference is caused only by the gate/settings change, never RNG.
public struct SpikeTrainSimulationComparisonRow: Sendable, Equatable {
    /// The rung evaluated against `StatePatternDetectorTuning()` (detector defaults).
    public let `default`: SpikeTrainSimulationPreviewRow
    /// The rung evaluated against the supplied (current) tuning.
    public let current: SpikeTrainSimulationPreviewRow

    public var jitterSDSec: Double { current.jitterSDSec }
    public var train: SpikeTrain { current.train }

    /// True when the current settings change the rung's outcome relative to the defaults (verdict, clean-tonic, gate
    /// pass/fail, or detector labels). The generated train and metrics are identical between the two by construction.
    public var changed: Bool {
        current.reason != `default`.reason
            || current.isCleanTonic != `default`.isCleanTonic
            || current.passesTonicRegularityGates != `default`.passesTonicRegularityGates
            || current.selectedLabels != `default`.selectedLabels
    }
}

public enum SpikeTrainSimulationPreview {

    /// Generate and evaluate the ladder against `config.stateTuning`. Rows are returned sorted by jitter ascending. Pure
    /// and deterministic: the same config (incl. `baseSeed`) yields identical rows and train timestamps.
    public static func generate(config: SpikeTrainSimulationPreviewConfig = SpikeTrainSimulationPreviewConfig())
        -> [SpikeTrainSimulationPreviewRow] {
        sortedLadder(config).map { index, sd in
            let g = generatedRung(index: index, jitterSDSec: sd, config: config)
            return evaluate(g, tuning: config.stateTuning)
        }
    }

    /// SIM-1F — evaluate the SAME generated ladder against the detector DEFAULTS and the supplied (current) tuning. The
    /// rung train/metrics are generated ONCE and shared by both evaluations, so the only differences are gate-driven.
    public static func compare(config: SpikeTrainSimulationPreviewConfig = SpikeTrainSimulationPreviewConfig())
        -> [SpikeTrainSimulationComparisonRow] {
        let defaultTuning = StatePatternDetectorTuning()
        return sortedLadder(config).map { index, sd in
            let g = generatedRung(index: index, jitterSDSec: sd, config: config)   // generated once
            return SpikeTrainSimulationComparisonRow(
                default: evaluate(g, tuning: defaultTuning),
                current: evaluate(g, tuning: config.stateTuning))
        }
    }

    // MARK: - generation (gate-independent) + evaluation (per-tuning)

    private static func sortedLadder(_ config: SpikeTrainSimulationPreviewConfig) -> [(index: Int, sd: Double)] {
        config.jitterLadderSec.sorted().enumerated().map { ($0.offset, $0.element) }
    }

    private struct GeneratedRung {
        let jitterSDSec: Double
        let lowerSec: Double
        let upperSec: Double
        let train: SpikeTrain
    }

    /// Generate one rung's train + truncation window. Depends only on the generation parameters (seed/duration/mean/
    /// jitter/window) — NOT on any tuning — so the same rung is identical across tunings.
    private static func generatedRung(index: Int, jitterSDSec sd: Double,
                                      config: SpikeTrainSimulationPreviewConfig) -> GeneratedRung {
        // Per-rung seed derived from the base seed so each rung is independently reproducible and rungs are spread far
        // apart in seed space (changing baseSeed re-rolls every rung).
        let rowSeed = config.baseSeed &+ (UInt64(index) &* 0x9E37_79B9_7F4A_7C15)
        let lower = Swift.max(config.lowerFloorSec, config.tonicMeanSec - config.windowSigma * sd)
        let upper = config.tonicMeanSec + config.windowSigma * sd
        var simulator = SpikeTrainSimulator(seed: rowSeed)
        let train = simulator.tonicTrain(
            name: "sim_tonic_sd_\(String(format: "%.4f", sd))",
            durationSec: config.durationSec, meanSec: config.tonicMeanSec, sdSec: sd,
            lowerSec: lower, upperSec: upper)
        return GeneratedRung(jitterSDSec: sd, lowerSec: lower, upperSec: upper, train: train)
    }

    /// Evaluate a generated rung against `tuning`: compute the (gate-independent) statistics, run the isolated detector
    /// with that tuning for the labels, and apply that tuning's tonic regularity gates for the verdict.
    private static func evaluate(_ rung: GeneratedRung, tuning: StatePatternDetectorTuning)
        -> SpikeTrainSimulationPreviewRow {
        let train = rung.train
        let isis = train.isiSec.compactMap { $0 }
        let mean = STPDStatistics.mean(isis)
        let cv = STPDStatistics.coefficientOfVariation(isis)
        let cv2 = STPDStatistics.coefficientOfVariation2(isis)
        let lv = STPDStatistics.localVariation(isis)
        let spikeCount = train.timestampsSec.count

        let dataset = SpikeDataset(name: "sim_preview", sourceDescription: "sim_preview", trains: [train])
        let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, stateTuning: tuning)
        let selectedLabels = Set((run.result(for: train.id)?.candidates ?? [])
            .filter { $0.selectedForAuto }
            .map { $0.finalLabel.rawValue }).sorted()
        let isCleanTonic = selectedLabels == ["tonic"]

        let countOK = spikeCount >= tuning.tonicMinSpikes
        let cvOK = cv.map { $0 <= tuning.tonicCVMax } ?? false
        let cv2OK = cv2.map { $0 <= tuning.tonicCV2Max } ?? false
        let lvOK = lv.map { $0 <= tuning.tonicLVMax } ?? false
        let passesGates = countOK && cvOK && cv2OK && lvOK

        let reason: SpikeTrainSimulationPreviewReason
        if !countOK { reason = .tooFewSpikes }
        else if !cvOK { reason = .cvTooHigh }
        else if !cv2OK { reason = .cv2TooHigh }
        else if !lvOK { reason = .lvTooHigh }
        else if !isCleanTonic { reason = .notCleanTonic }
        else { reason = .passes }

        return SpikeTrainSimulationPreviewRow(
            jitterSDSec: rung.jitterSDSec, lowerSec: rung.lowerSec, upperSec: rung.upperSec, train: train,
            spikeCount: spikeCount, isiCount: isis.count, meanISISec: mean,
            cv: cv, cv2: cv2, lv: lv, selectedLabels: selectedLabels,
            isCleanTonic: isCleanTonic, passesTonicRegularityGates: passesGates, reason: reason)
    }
}
