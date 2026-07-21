import Foundation

/// SIM-2C — a pure, deterministic burst-response preview MODEL (no UI), the burst sibling of the tonic
/// `SpikeTrainSimulationPreview`. For each rung of a packet-size ladder it generates ONE simulated burst-response train
/// (tonic baseline + a burst packet at each intended onset, via `SpikeTrainSimulator.burstResponseTrain`) and evaluates
/// it with the REAL detector (`ClassicAnchorDetectionPipeline.run`) on an isolated synthetic `SpikeDataset`. Unlike the
/// tonic model (a single whole-train regularity gate), burst acceptance is LABEL- and ONSET-based: the verdict for each
/// intended onset is "did a burst-family event land on it?", read from the detector's selected event-track candidates.
///
/// It is a pure value model for a future SIM-2D UI: it never touches `RasterDocument`, the live dataset/detection run,
/// manual thresholds, annotations, review state, or CSV export, and it runs no R at runtime. Generator knobs (onset
/// schedule, packet-size ladder, baseline, burst kernel) are kept separate from the detector acceptance settings
/// (`StatePatternDetectorTuning` + `PatternDetectionParameterSettings`). Verdict tokens are machine-readable (UI copy is
/// deferred to the SIM-2D layer, mirroring how SIM-1C deferred its English dictionary entries to SIM-1D).

/// Per-onset structural outcome of evaluating a simulated burst packet against the detector. Onset-local and label-based:
/// derived from which selected event-track candidates land on the intended onset, NOT a single whole-train metric gate.
///
/// These are exactly the outcomes the burst-response generator can produce: the detector routes burst / possible-burst
/// to the EVENT track (the only candidates this model scores), while tonic goes to the STATE track and pause to the GAP
/// track (both excluded from onset coverage). A non-burst-edge "contamination" / "other-label" category is intentionally
/// NOT modelled yet: it requires a generator that produces non-burst events overlapping a packet AND an overlap-vs-flank
/// definition (a classic burst is normally FLANKED by a pause, which is not contamination) — both deferred to a later
/// SIM-2 phase. Every case here is reachable and exercised by the SIM-2C tests.
public enum SpikeTrainSimulationBurstVerdict: String, Sendable, Equatable, CaseIterable {
    /// Exactly one canonical burst-family event covers the onset and spans at least the intended packet (clean hit).
    case detectedAsBurst = "detected_as_burst"
    /// The onset is covered by a `possible_burst` event (and no canonical burst-family event).
    case detectedAsPossibleBurst = "detected_as_possible_burst"
    /// The packet is split across >1 burst/possible event, or the single covering burst event captured fewer spikes
    /// than the intended packet (partial capture).
    case splitOrPartialPacket = "split_or_partial_packet"
    /// No event-track burst/possible candidate covers the onset at all (the packet was not detected as a burst event).
    case missedPacket = "missed_packet"

    /// SIM-2C: Chinese-source display label, routed through `STPDLocalization` for English. The `rawValue` machine token
    /// is unchanged and never localized; only this display label is. (English dictionary entries are added in SIM-2D.)
    public var localizationSourceZH: String {
        switch self {
        case .detectedAsBurst: return "识别为爆发"
        case .detectedAsPossibleBurst: return "识别为可能爆发"
        case .splitOrPartialPacket: return "拆分或部分爆发包"
        case .missedPacket: return "漏检爆发包"
        }
    }

    /// The localized display label for `language` (zh returns the source verbatim; en via the shared dictionary).
    public func localizedLabel(language: STPDLanguage) -> String {
        STPDLocalization.text(localizationSourceZH, language: language)
    }

    /// A clean canonical burst hit on this onset (drives UI pass styling).
    public var isCleanBurst: Bool { self == .detectedAsBurst }

    /// The onset read as some burst family (canonical, possible, or a split/partial canonical capture) — the broadest
    /// "was a burst detected here" notion (everything except a missed packet).
    public var isBurstFamilyDetected: Bool { self != .missedPacket }
}

/// Coverage of ONE intended burst onset by the detector's selected event-track candidates. Onset-local: only events whose
/// time interval intersects `[onset − tolerance, onset + tolerance]` are considered. All label/status fields are raw
/// machine tokens (never localized).
public struct SpikeTrainSimulationBurstOnsetCoverage: Sendable, Equatable {
    /// The intended burst onset time (seconds), from the generator schedule.
    public let onsetSec: Double
    public let verdict: SpikeTrainSimulationBurstVerdict
    /// Number of covering canonical burst-family events (`burst`/`high_frequency_burst`/`long_burst`).
    public let coveringBurstFamilyCount: Int
    /// Number of covering `possible_burst` events.
    public let coveringPossibleCount: Int
    /// Nearest event to the onset (covering if any cover, else the globally nearest event): raw label token.
    public let nearestLabel: String?
    /// Nearest event's gate status (raw machine token).
    public let nearestGateStatus: String?
    public let nearestStartSec: Double?
    public let nearestEndSec: Double?
    /// Spike count of the nearest event's candidate (detector-reported).
    public let nearestSpikeCount: Int?

    /// True when at least one canonical burst-family event covers this onset (regardless of clean/partial/contaminated).
    public var hasBurstFamilyCoverage: Bool { coveringBurstFamilyCount > 0 }
}

/// One rung of the ladder: the generated burst-response train + descriptive metrics + per-onset detector coverage.
public struct SpikeTrainSimulationBurstPreviewRow: Sendable, Equatable {
    /// The intended (and, by construction, actual) spikes-per-burst for this rung.
    public let packetSpikeCount: Int
    public let train: SpikeTrain
    public let spikeCount: Int
    /// Mean of the baseline ISIs (those ≥ `baselineLowerSec`), a compact descriptor of the tonic background.
    public let baselineMeanISISec: Double?
    /// Range of the intra-burst (packet) ISIs (those ≤ `burstUpperSec`).
    public let burstISIMinSec: Double?
    public let burstISIMaxSec: Double?
    /// Sorted, de-duplicated raw detector labels selected for this train (machine tokens, never localized).
    public let selectedLabels: [String]
    /// Count of selected canonical burst-family candidates across the whole train.
    public let burstFamilyCandidateCount: Int
    /// Count of selected `possible_burst` candidates across the whole train.
    public let possibleBurstCandidateCount: Int
    /// Per-intended-onset coverage, in onset order.
    public let onsetCoverage: [SpikeTrainSimulationBurstOnsetCoverage]

    /// Per-onset verdicts in order (the gate-driven outcome; excludes train/metrics which are generation-only).
    public var onsetVerdicts: [SpikeTrainSimulationBurstVerdict] { onsetCoverage.map(\.verdict) }
    /// Number of onsets read as a clean canonical burst.
    public var detectedBurstOnsetCount: Int { onsetCoverage.filter { $0.verdict == .detectedAsBurst }.count }
    /// Number of onsets with any canonical burst-family coverage.
    public var burstFamilyCoveredOnsetCount: Int { onsetCoverage.filter(\.hasBurstFamilyCoverage).count }
    /// Every intended onset was read as a clean canonical burst.
    public var allOnsetsDetectedAsBurst: Bool {
        !onsetCoverage.isEmpty && onsetCoverage.allSatisfy { $0.verdict == .detectedAsBurst }
    }
}

/// SIM-2C — one rung evaluated against TWO settings snapshots (the detector defaults vs. the supplied/current
/// tuning + parameters) on the SAME generated train, so any verdict difference is caused only by the settings change,
/// never RNG.
public struct SpikeTrainSimulationBurstComparisonRow: Sendable, Equatable {
    /// The rung evaluated against `StatePatternDetectorTuning()` + `PatternDetectionParameterSettings.defaults`.
    public let `default`: SpikeTrainSimulationBurstPreviewRow
    /// The rung evaluated against the supplied (current) tuning + parameters.
    public let current: SpikeTrainSimulationBurstPreviewRow

    public var packetSpikeCount: Int { current.packetSpikeCount }
    public var train: SpikeTrain { current.train }

    /// True when the current settings change the rung's outcome relative to the defaults (per-onset verdicts, selected
    /// labels, or burst-family / possible candidate counts). The generated train and descriptive metrics are identical
    /// between the two by construction, so they are deliberately excluded from the diff.
    public var changed: Bool {
        current.onsetVerdicts != `default`.onsetVerdicts
            || current.selectedLabels != `default`.selectedLabels
            || current.burstFamilyCandidateCount != `default`.burstFamilyCandidateCount
            || current.possibleBurstCandidateCount != `default`.possibleBurstCandidateCount
    }
}

/// Input for the burst-response preview ladder. Defaults reproduce the R-reference burst-response baseline + kernel and a
/// packet-size ladder that crosses the "is it detected as a burst?" boundary. The detector acceptance settings default
/// to the existing detector defaults; SIM-2D feeds the app's CURRENT tuning + parameters here.
public struct SpikeTrainSimulationBurstPreviewConfig: Sendable, Equatable {
    public var baseSeed: UInt64
    public var durationSec: Double
    /// The intended burst onset schedule (seconds).
    public var burstOnsetsSec: [Double]
    /// The "rung" dimension: each value is a fixed spikes-per-burst packet size; small packets may be missed while larger
    /// packets are cleanly detected as bursts (the burst analogue of the tonic jitter ladder). The `possible_burst` and
    /// `split_or_partial_packet` outcomes arise under marginal burst-contrast / max-spike settings, not the default kernel.
    public var packetSpikeLadder: [Int]
    public var baselineMeanSec: Double
    public var baselineSDSec: Double
    public var baselineLowerSec: Double
    public var baselineUpperSec: Double
    public var interEventGapSec: Double
    public var burstShape: Double
    public var burstScaleSec: Double
    public var burstLowerSec: Double
    public var burstUpperSec: Double
    /// Half-width (seconds) of the onset window an event must intersect to "cover" the onset. Must stay well below the
    /// onset spacing so coverage is onset-local (no cross-onset bleed).
    public var onsetMatchToleranceSec: Double
    /// Detector state tuning — passed to the isolated detector run (drives tonic/HF labels) and the verdict.
    public var stateTuning: StatePatternDetectorTuning
    /// Detector pattern parameters — the BURST acceptance thresholds (contrast / min-max spikes) live here, so this is
    /// what makes a stricter "current" snapshot change a burst verdict.
    public var detectorParameters: PatternDetectionParameterSettings

    public init(
        baseSeed: UInt64 = 20_260_624,
        durationSec: Double = 30,
        burstOnsetsSec: [Double] = [5, 10, 15, 20, 25],
        packetSpikeLadder: [Int] = [2, 3, 4, 6, 9],
        baselineMeanSec: Double = 0.45,
        baselineSDSec: Double = 0.03,
        baselineLowerSec: Double = 0.38,
        baselineUpperSec: Double = 0.52,
        interEventGapSec: Double = 0.003,
        burstShape: Double = 2,
        burstScaleSec: Double = 0.012,
        burstLowerSec: Double = 0.006,
        burstUpperSec: Double = 0.045,
        onsetMatchToleranceSec: Double = 0.25,
        stateTuning: StatePatternDetectorTuning = StatePatternDetectorTuning(),
        detectorParameters: PatternDetectionParameterSettings = .defaults
    ) {
        self.baseSeed = baseSeed
        self.durationSec = durationSec
        self.burstOnsetsSec = burstOnsetsSec
        self.packetSpikeLadder = packetSpikeLadder
        self.baselineMeanSec = baselineMeanSec
        self.baselineSDSec = baselineSDSec
        self.baselineLowerSec = baselineLowerSec
        self.baselineUpperSec = baselineUpperSec
        self.interEventGapSec = interEventGapSec
        self.burstShape = burstShape
        self.burstScaleSec = burstScaleSec
        self.burstLowerSec = burstLowerSec
        self.burstUpperSec = burstUpperSec
        self.onsetMatchToleranceSec = onsetMatchToleranceSec
        self.stateTuning = stateTuning
        self.detectorParameters = detectorParameters
    }
}

public enum SpikeTrainSimulationBurstPreview {

    /// Generate and evaluate the ladder against `config.stateTuning` + `config.detectorParameters`. Rows are returned in
    /// ascending packet-size order. Pure and deterministic: the same config (incl. `baseSeed`) yields identical rows and
    /// train timestamps.
    public static func generate(config: SpikeTrainSimulationBurstPreviewConfig = SpikeTrainSimulationBurstPreviewConfig())
        -> [SpikeTrainSimulationBurstPreviewRow] {
        sortedLadder(config).map { index, k in
            let g = generatedRung(index: index, packetSpikeCount: k, config: config)
            return evaluate(g, config: config, tuning: config.stateTuning, parameters: config.detectorParameters)
        }
    }

    /// SIM-2C — evaluate the SAME generated ladder against the detector DEFAULTS and the supplied (current) tuning +
    /// parameters. The rung train/metrics are generated ONCE and shared by both evaluations, so the only differences are
    /// settings-driven.
    public static func compare(config: SpikeTrainSimulationBurstPreviewConfig = SpikeTrainSimulationBurstPreviewConfig())
        -> [SpikeTrainSimulationBurstComparisonRow] {
        let defaultTuning = StatePatternDetectorTuning()
        let defaultParameters = PatternDetectionParameterSettings.defaults
        return sortedLadder(config).map { index, k in
            let g = generatedRung(index: index, packetSpikeCount: k, config: config)   // generated once
            return SpikeTrainSimulationBurstComparisonRow(
                default: evaluate(g, config: config, tuning: defaultTuning, parameters: defaultParameters),
                current: evaluate(g, config: config, tuning: config.stateTuning, parameters: config.detectorParameters))
        }
    }

    // MARK: - generation (settings-independent) + evaluation (per-settings)

    private static func sortedLadder(_ config: SpikeTrainSimulationBurstPreviewConfig) -> [(index: Int, k: Int)] {
        config.packetSpikeLadder.sorted().enumerated().map { ($0.offset, $0.element) }
    }

    private struct GeneratedBurstRung {
        let packetSpikeCount: Int
        let train: SpikeTrain
    }

    /// One event-track candidate projected to seconds (via the canonical `ClassicAnchorEventAnnotation` converter), with
    /// the detector-reported spike count and machine-token status carried alongside for coverage classification.
    private struct CoverageEvent {
        let label: ClassicAnchorLabel
        let startSec: Double
        let endSec: Double
        let nSpikes: Int
        let gateStatus: String
    }

    /// Generate one rung's burst-response train. Depends only on the generation parameters (seed/duration/onsets/packet
    /// size/baseline/kernel) — NOT on any detector settings — so the same rung is identical across settings snapshots.
    private static func generatedRung(index: Int, packetSpikeCount k: Int,
                                      config: SpikeTrainSimulationBurstPreviewConfig) -> GeneratedBurstRung {
        // Per-rung seed derived from the base seed so each rung is independently reproducible and rungs are spread far
        // apart in seed space (changing baseSeed re-rolls every rung). Same scheme as the tonic SIM-1C model.
        let rowSeed = config.baseSeed &+ (UInt64(index) &* 0x9E37_79B9_7F4A_7C15)
        var simulator = SpikeTrainSimulator(seed: rowSeed)
        let train = simulator.burstResponseTrain(
            name: "sim_burst_k_\(k)",
            durationSec: config.durationSec,
            baselineMeanSec: config.baselineMeanSec,
            baselineSDSec: config.baselineSDSec,
            baselineLowerSec: config.baselineLowerSec,
            baselineUpperSec: config.baselineUpperSec,
            burstOnsetsSec: config.burstOnsetsSec,
            spikesPerBurst: k...k,
            interEventGapSec: config.interEventGapSec,
            burstShape: config.burstShape,
            burstScaleSec: config.burstScaleSec,
            burstLowerSec: config.burstLowerSec,
            burstUpperSec: config.burstUpperSec)
        return GeneratedBurstRung(packetSpikeCount: k, train: train)
    }

    /// Evaluate a generated rung against `tuning` + `parameters`: compute the (settings-independent) descriptive metrics,
    /// run the isolated detector with those settings, and classify each intended onset by its selected event-track
    /// coverage.
    private static func evaluate(_ rung: GeneratedBurstRung,
                                 config: SpikeTrainSimulationBurstPreviewConfig,
                                 tuning: StatePatternDetectorTuning,
                                 parameters: PatternDetectionParameterSettings)
        -> SpikeTrainSimulationBurstPreviewRow {
        let train = rung.train
        let isis = train.isiSec.compactMap { $0 }
        let baselineISIs = isis.filter { $0 >= config.baselineLowerSec }
        let burstISIs = isis.filter { $0 <= config.burstUpperSec }
        let spikeCount = train.timestampsSec.count

        let dataset = SpikeDataset(name: "sim_burst_preview", sourceDescription: "sim_burst_preview", trains: [train])
        let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, stateTuning: tuning, detectorParameters: parameters)
        let candidates = run.result(for: train.id)?.candidates ?? []
        // Real selected candidates only — drop the synthetic `.profile` audit candidates the pipeline prepends.
        let selected = candidates.filter { $0.selectedForAuto && $0.finalLabel != .profile }

        let selectedLabels = Set(selected.map { $0.finalLabel.rawValue }).sorted()
        let burstFamilyCandidateCount = selected.filter { $0.finalLabel.isCanonicalBurstFamily }.count
        let possibleBurstCandidateCount = selected.filter { $0.finalLabel == .possibleBurst }.count

        // Onset coverage uses only EVENT-track candidates — the localized burst / possible-burst events. The detector
        // routes the tonic baseline to the STATE track (a candidate spanning large stretches that would falsely "cover"
        // every onset) and pauses to the GAP track; both are excluded here, so coverage is burst/possible only.
        let events: [CoverageEvent] = selected
            .filter { $0.auditRecommendedTrack == .event }
            .map { candidate in
                let annotation = ClassicAnchorEventAnnotation(candidate: candidate, train: train)
                return CoverageEvent(
                    label: candidate.finalLabel,
                    startSec: annotation.rawStartSec,
                    endSec: annotation.rawEndSec,
                    nSpikes: candidate.nSpikes,
                    gateStatus: candidate.gateStatus)
            }

        let onsets = config.burstOnsetsSec.filter { $0 > 0 && $0 <= config.durationSec }.sorted()
        let coverage = onsets.map { onset in
            onsetCoverage(onset: onset, events: events,
                          intendedPacketSpikes: rung.packetSpikeCount,
                          tolerance: config.onsetMatchToleranceSec)
        }

        return SpikeTrainSimulationBurstPreviewRow(
            packetSpikeCount: rung.packetSpikeCount,
            train: train,
            spikeCount: spikeCount,
            baselineMeanISISec: STPDStatistics.mean(baselineISIs),
            burstISIMinSec: burstISIs.min(),
            burstISIMaxSec: burstISIs.max(),
            selectedLabels: selectedLabels,
            burstFamilyCandidateCount: burstFamilyCandidateCount,
            possibleBurstCandidateCount: possibleBurstCandidateCount,
            onsetCoverage: coverage)
    }

    /// Classify a single intended onset by the selected event-track events whose interval intersects the onset window.
    /// Deterministic and onset-local (only events within `tolerance` of the onset participate).
    private static func onsetCoverage(onset: Double, events: [CoverageEvent],
                                      intendedPacketSpikes: Int, tolerance: Double)
        -> SpikeTrainSimulationBurstOnsetCoverage {
        let covering = events.filter { $0.startSec <= onset + tolerance && $0.endSec >= onset - tolerance }
        let burst = covering.filter { $0.label.isCanonicalBurstFamily }
        let possible = covering.filter { $0.label == .possibleBurst }

        // Nearest event for the compact "what is here" readout: the covering events if any, else the globally nearest.
        let pool = covering.isEmpty ? events : covering
        let nearest = pool.min { gapToOnset($0, onset: onset) < gapToOnset($1, onset: onset) }

        let verdict: SpikeTrainSimulationBurstVerdict
        if burst.isEmpty && possible.isEmpty {
            verdict = .missedPacket
        } else if burst.count + possible.count > 1 {
            verdict = .splitOrPartialPacket                              // packet split across multiple events
        } else if let single = burst.first {                            // exactly one canonical burst event
            verdict = single.nSpikes < intendedPacketSpikes ? .splitOrPartialPacket : .detectedAsBurst
        } else {                                                        // exactly one possible_burst, no canonical
            verdict = .detectedAsPossibleBurst
        }

        return SpikeTrainSimulationBurstOnsetCoverage(
            onsetSec: onset,
            verdict: verdict,
            coveringBurstFamilyCount: burst.count,
            coveringPossibleCount: possible.count,
            nearestLabel: nearest?.label.rawValue,
            nearestGateStatus: nearest?.gateStatus,
            nearestStartSec: nearest?.startSec,
            nearestEndSec: nearest?.endSec,
            nearestSpikeCount: nearest?.nSpikes)
    }

    /// Distance from an event to an onset: 0 when the onset is inside `[startSec, endSec]`, else the gap to the nearer edge.
    private static func gapToOnset(_ event: CoverageEvent, onset: Double) -> Double {
        Swift.max(0, Swift.max(event.startSec - onset, onset - event.endSec))
    }
}
