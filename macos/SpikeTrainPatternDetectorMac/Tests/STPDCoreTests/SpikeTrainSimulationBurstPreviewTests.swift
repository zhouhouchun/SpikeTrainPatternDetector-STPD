import Foundation
@testable import STPDCore
import Testing

// SIM-2C — tests for the pure, deterministic burst-response preview MODEL. No document/detection state is involved
// (RasterDocument is in the app target and is never referenced). The model generates one burst-response train per
// packet-size rung and reads the REAL detector's selected event-track coverage per intended onset. Parity with R is
// distributional (the generator) and the verdicts are structural/label-based machine tokens.

private func cleanConfig(
    ladder: [Int],
    tuning: StatePatternDetectorTuning = StatePatternDetectorTuning(),
    parameters: PatternDetectionParameterSettings = .defaults
) -> SpikeTrainSimulationBurstPreviewConfig {
    SpikeTrainSimulationBurstPreviewConfig(packetSpikeLadder: ladder, stateTuning: tuning, detectorParameters: parameters)
}

// MARK: - determinism

@Test func sim_burstPreview_sameConfigProducesIdenticalGeneratedTimestamps() {
    let config = SpikeTrainSimulationBurstPreviewConfig()
    let a = SpikeTrainSimulationBurstPreview.generate(config: config)
    let b = SpikeTrainSimulationBurstPreview.generate(config: config)
    #expect(a.count == b.count)
    #expect(a.count == config.packetSpikeLadder.count)
    for (rowA, rowB) in zip(a, b) {
        #expect(rowA.packetSpikeCount == rowB.packetSpikeCount)
        #expect(rowA.train.timestampsSec == rowB.train.timestampsSec)   // identical generation
    }
    // Rows are sorted by ascending packet size.
    #expect(a.map(\.packetSpikeCount) == config.packetSpikeLadder.sorted())
}

// MARK: - generate-once, evaluate-twice (default vs current share the SAME train + descriptive metrics)

@Test func sim_burstPreview_compareUsesIdenticalTrainAndMetrics() {
    // A current snapshot that differs from defaults (strict burst gate) must NOT change the generated train/metrics.
    let strict = PatternDetectionParameterSettings(classicBurstMinSpikes: 50)
    let rows = SpikeTrainSimulationBurstPreview.compare(config: cleanConfig(ladder: [3, 6, 9], parameters: strict))
    #expect(rows.count == 3)
    for row in rows {
        #expect(row.default.packetSpikeCount == row.current.packetSpikeCount)
        #expect(row.default.train.timestampsSec == row.current.train.timestampsSec)   // generated once
        #expect(row.default.spikeCount == row.current.spikeCount)
        #expect(row.default.baselineMeanISISec == row.current.baselineMeanISISec)
        #expect(row.default.burstISIMinSec == row.current.burstISIMinSec)
        #expect(row.default.burstISIMaxSec == row.current.burstISIMaxSec)
    }
}

// MARK: - clean burst-response is detected as burst-family under defaults

@Test func sim_burstPreview_cleanBurstResponseIsDetectedAsBurstFamilyUnderDefaults() throws {
    // A generous 9-spike packet at every onset is read as a CLEAN canonical burst at every onset under defaults.
    let rows = SpikeTrainSimulationBurstPreview.generate(config: cleanConfig(ladder: [9]))
    let row = try #require(rows.first)
    let onsetCount = SpikeTrainSimulationBurstPreviewConfig().burstOnsetsSec.count
    #expect(row.onsetCoverage.count == onsetCount)
    #expect(row.burstFamilyCandidateCount >= onsetCount)            // one canonical burst per onset (at least)
    #expect(row.burstFamilyCoveredOnsetCount == onsetCount)         // every intended onset has burst-family coverage
    #expect(row.detectedBurstOnsetCount == onsetCount)              // …and each is a CLEAN detected_as_burst
    #expect(row.allOnsetsDetectedAsBurst)
    #expect(row.selectedLabels.contains("burst"))
}

// MARK: - the missed-packet verdict is reachable (small packets and/or strict gates)

@Test func sim_burstPreview_smallPacketsAndStrictGatesProduceMissedPackets() throws {
    // (a) Tiny 2-spike packets are not all cleanly captured by the GENERATOR path: at least one onset is read as missed.
    let smallRow = try #require(SpikeTrainSimulationBurstPreview.generate(config: cleanConfig(ladder: [2])).first)
    #expect(smallRow.detectedBurstOnsetCount < smallRow.onsetCoverage.count)
    #expect(smallRow.onsetVerdicts.contains(.missedPacket))         // missed via the small-packet generator path

    // (b) An impossibly strict burst gate makes EVERY onset a non-clean-burst verdict, and at least one a missed packet.
    let strict = PatternDetectionParameterSettings(classicBurstMinSpikes: 50)
    let strictRow = try #require(SpikeTrainSimulationBurstPreview.generate(config: cleanConfig(ladder: [6], parameters: strict)).first)
    #expect(strictRow.burstFamilyCandidateCount == 0)
    #expect(strictRow.onsetCoverage.allSatisfy { !$0.verdict.isCleanBurst })
    #expect(strictRow.onsetVerdicts.contains(.missedPacket))
}

@Test func sim_burstPreview_marginalContrastProducesPossibleAndSplitVerdicts() {
    // Behaviourally exercise the two non-trivial verdicts: a marginal burst-contrast gate (just above the simulated
    // packet contrast) demotes some packets to `possible_burst` and splits others, so across the ladder the model emits
    // detected_as_burst, split_or_partial_packet, AND detected_as_possible_burst — not just the clean/missed extremes.
    let marginal = PatternDetectionParameterSettings(classicBurstContrastMin: 20)
    let rows = SpikeTrainSimulationBurstPreview.generate(config: cleanConfig(ladder: [4, 6, 9], parameters: marginal))
    let verdicts = Set(rows.flatMap(\.onsetVerdicts))
    #expect(verdicts.contains(.detectedAsBurst))
    #expect(verdicts.contains(.splitOrPartialPacket))
    #expect(verdicts.contains(.detectedAsPossibleBurst))
    #expect(rows.contains { $0.possibleBurstCandidateCount > 0 })
}

// MARK: - stricter current settings flip a verdict without changing the generated train

@Test func sim_burstPreview_stricterSettingsChangeAVerdictWithoutChangingTimestamps() {
    // Default detection sees bursts; a strict burst gate (impossibly high min spikes) must change ≥1 rung's verdict,
    // while the generated train stays byte-identical between the two evaluations.
    let strict = PatternDetectionParameterSettings(classicBurstMinSpikes: 50)
    let rows = SpikeTrainSimulationBurstPreview.compare(config: cleanConfig(ladder: [3, 6, 9], parameters: strict))
    #expect(rows.contains { $0.changed })                            // at least one verdict/label/count changed
    for row in rows {
        #expect(row.default.train.timestampsSec == row.current.train.timestampsSec)   // RNG-independent of settings
    }
    // The strict current snapshot detects no canonical burst on any rung (so its verdicts differ from defaults).
    #expect(rows.allSatisfy { $0.current.burstFamilyCandidateCount == 0 })
    #expect(rows.contains { $0.default.burstFamilyCandidateCount > 0 })
}

@Test func sim_burstPreview_settingsDriveAGradedPerRungFlipNotJustCollapse() throws {
    // A mid-strength burst gate (min 7 spikes) must flip SOME rungs while leaving the largest packet a clean burst —
    // proving the comparison discriminates per rung rather than collapsing all-or-nothing. Rows are ascending by size.
    let mid = PatternDetectionParameterSettings(classicBurstMinSpikes: 7)
    let rows = SpikeTrainSimulationBurstPreview.compare(config: cleanConfig(ladder: [3, 6, 9], parameters: mid))
    #expect(rows.map(\.packetSpikeCount) == [3, 6, 9])
    let largest = try #require(rows.last)
    #expect(largest.packetSpikeCount == 9)
    #expect(!largest.changed)                                           // a 9-spike packet still clears a 7-spike gate
    #expect(largest.current.allOnsetsDetectedAsBurst)
    #expect(rows.contains { $0.changed })                              // …while at least one smaller rung flips
    #expect(rows.contains { !$0.changed })                            // graded, not a wholesale collapse
    for row in rows {
        #expect(row.default.train.timestampsSec == row.current.train.timestampsSec)
    }
}

// MARK: - the isolated detector run carries no manual / learned-threshold / scope provenance

@Test func sim_burstPreview_isolatedRunUsesNoManualOrExportProvenance() {
    // The model runs detection on an isolated synthetic dataset with NO manual/learned threshold profile, so candidate
    // decision paths must never carry resolved-threshold or manual-scope provenance tokens (mirrors the tonic sibling).
    var simulator = SpikeTrainSimulator(seed: 20_260_624)
    let train = simulator.burstResponseTrain(durationSec: 30, burstOnsetsSec: [5, 10, 15, 20, 25], spikesPerBurst: 6...6)
    let dataset = SpikeDataset(name: "sim", sourceDescription: "sim", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset)
    let decisionPaths = (run.result(for: train.id)?.candidates ?? []).map(\.decisionPath)
    #expect(!decisionPaths.isEmpty)
    #expect(decisionPaths.allSatisfy { !$0.contains("resolved_threshold[") })
    #expect(decisionPaths.allSatisfy { !$0.contains("manual_threshold_scope=") })
}

// MARK: - coverage is deterministic and onset-local

@Test func sim_burstPreview_packetCoverageIsDeterministicAndOnsetLocal() throws {
    let config = cleanConfig(ladder: [6])
    let first = try #require(SpikeTrainSimulationBurstPreview.generate(config: config).first)
    let second = try #require(SpikeTrainSimulationBurstPreview.generate(config: config).first)
    // Deterministic: identical coverage (verdicts + counts + nearest readouts) across two runs.
    #expect(first.onsetVerdicts == second.onsetVerdicts)
    #expect(first.onsetCoverage == second.onsetCoverage)

    let onsets = config.burstOnsetsSec.sorted()
    #expect(first.onsetCoverage.map(\.onsetSec) == onsets)          // one coverage entry per onset, in order
    let tol = config.onsetMatchToleranceSec
    for coverage in first.onsetCoverage where coverage.hasBurstFamilyCoverage {
        let start = try #require(coverage.nearestStartSec)
        let end = try #require(coverage.nearestEndSec)
        // The covering event intersects the onset window (coverage invariant)…
        #expect(start <= coverage.onsetSec + tol)
        #expect(end >= coverage.onsetSec - tol)
        // …and belongs to THIS onset, not a neighbour 5 s away (onset-local).
        #expect(abs(start - coverage.onsetSec) < 2.0)
    }
}

// MARK: - isolation (no document / manual / export coupling)

@Test func sim_burstPreview_isSelfContainedNoDocumentOrExportState() {
    // Pure model: two independent generations are identical and produce plain synthetic trains (no detector run /
    // document / manual thresholds / export coupling). Row is a value type with only generated + detector-derived data.
    func gen() -> [SpikeTrainSimulationBurstPreviewRow] {
        SpikeTrainSimulationBurstPreview.generate(config: cleanConfig(ladder: [2, 4]))
    }
    let a = gen()
    let b = gen()
    #expect(a == b)
    #expect(a.allSatisfy { $0.train.name.hasPrefix("sim_burst_k_") })
}

// MARK: - verdict token stability (machine tokens are never localized)

@Test func sim_burstPreview_verdictRawTokensAreStable() {
    // The four reachable structural verdicts. Every case is exercised by an evaluation test elsewhere in this file.
    #expect(SpikeTrainSimulationBurstVerdict.allCases.map(\.rawValue) == [
        "detected_as_burst", "detected_as_possible_burst", "split_or_partial_packet", "missed_packet",
    ])
    // zh returns the Chinese source verbatim; en falls back to source until SIM-2D adds the dictionary entries.
    #expect(SpikeTrainSimulationBurstVerdict.detectedAsBurst.localizedLabel(language: .zh) == "识别为爆发")
}
