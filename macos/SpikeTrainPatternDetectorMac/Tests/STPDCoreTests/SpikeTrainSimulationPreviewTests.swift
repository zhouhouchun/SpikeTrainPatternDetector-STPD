import Foundation
@testable import STPDCore
import Testing

// SIM-1C — tests for the pure preview-ladder model. STPDCore only; no app/document state. Parity is distributional;
// metrics/labels in each row must match direct STPDStatistics / pipeline calls on the generated train.

private func isis(_ train: SpikeTrain) -> [Double] { train.isiSec.compactMap { $0 } }

// MARK: - determinism + ordering

@Test func preview_sameConfigAndSeedProducesIdenticalRows() {
    let config = SpikeTrainSimulationPreviewConfig(baseSeed: 20260624)
    let a = SpikeTrainSimulationPreview.generate(config: config)
    let b = SpikeTrainSimulationPreview.generate(config: config)
    #expect(a == b)                                   // full value equality (incl. train timestamps + reasons)
    #expect(zip(a, b).allSatisfy { $0.train.timestampsSec == $1.train.timestampsSec })
    #expect(a.count == config.jitterLadderSec.count)
}

@Test func preview_differentSeedDiffersInTimestampsButSameRungOrder() {
    let a = SpikeTrainSimulationPreview.generate(config: .init(baseSeed: 1))
    let b = SpikeTrainSimulationPreview.generate(config: .init(baseSeed: 2))
    #expect(a.count == b.count)
    #expect(a.map(\.jitterSDSec) == b.map(\.jitterSDSec))                  // same ladder, same order
    for (ra, rb) in zip(a, b) {
        #expect(ra.train.timestampsSec != rb.train.timestampsSec)         // different RNG stream ⇒ different spikes
    }
}

@Test func preview_rowsSortedByJitterAscending() {
    // Even an unsorted ladder is returned ascending.
    let rows = SpikeTrainSimulationPreview.generate(config: .init(jitterLadderSec: [0.24, 0.02, 0.12, 0.06, 0.03]))
    let jitters = rows.map(\.jitterSDSec)
    #expect(jitters == jitters.sorted())
    #expect(jitters == [0.02, 0.03, 0.06, 0.12, 0.24])
}

// MARK: - ladder pass/fail semantics

@Test func preview_lowJitterRowPassesGatesAndIsCleanTonic() {
    let rows = SpikeTrainSimulationPreview.generate()
    let low = rows.first!                              // ascending ⇒ lowest jitter
    #expect(low.passesTonicRegularityGates)
    #expect(low.isCleanTonic)
    #expect(low.reason == .passes)
    #expect(low.selectedLabels == ["tonic"])
}

@Test func preview_highJitterRowFailsAGateAndIsNotCleanTonic() {
    let rows = SpikeTrainSimulationPreview.generate()
    let high = rows.last!                              // highest jitter
    #expect(!high.passesTonicRegularityGates)
    #expect(!high.isCleanTonic)
    #expect(high.reason != .passes)                   // explicit failure reason (cv_too_high for the default ladder)
    // With the widening window the high rung's whole-train CV exceeds the tonic regularity gate (the binding failure).
    #expect((high.cv ?? 0) > StatePatternDetectorTuning().tonicCVMax)
}

@Test func preview_reasonReflectsFirstFailingGate() {
    let rows = SpikeTrainSimulationPreview.generate()
    let gates = StatePatternDetectorTuning()
    for row in rows {
        switch row.reason {
        case .tooFewSpikes:  #expect(row.spikeCount < gates.tonicMinSpikes)
        case .cvTooHigh:     #expect((row.cv ?? .infinity) > gates.tonicCVMax)
        case .cv2TooHigh:    #expect((row.cv2 ?? .infinity) > gates.tonicCV2Max)
        case .lvTooHigh:     #expect((row.lv ?? .infinity) > gates.tonicLVMax)
        case .notCleanTonic: #expect(row.passesTonicRegularityGates && !row.isCleanTonic)
        case .passes:        #expect(row.passesTonicRegularityGates && row.isCleanTonic)
        }
    }
}

// MARK: - metrics + labels match direct STPDStatistics / pipeline

@Test func preview_metricsMatchDirectStpdStatistics() {
    let rows = SpikeTrainSimulationPreview.generate()
    for row in rows {
        let values = isis(row.train)
        #expect(row.isiCount == values.count)
        #expect(row.meanISISec == STPDStatistics.mean(values))
        #expect(row.cv == STPDStatistics.coefficientOfVariation(values))
        #expect(row.cv2 == STPDStatistics.coefficientOfVariation2(values))
        #expect(row.lv == STPDStatistics.localVariation(values))
        #expect(row.spikeCount == row.train.timestampsSec.count)
    }
}

@Test func preview_labelsMatchDirectIsolatedDetectorRun() {
    let config = SpikeTrainSimulationPreviewConfig()
    let rows = SpikeTrainSimulationPreview.generate(config: config)
    for row in rows {
        let dataset = SpikeDataset(name: "verify", sourceDescription: "verify", trains: [row.train])
        let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, stateTuning: config.stateTuning)
        let direct = Set((run.result(for: row.train.id)?.candidates ?? [])
            .filter { $0.selectedForAuto }.map { $0.finalLabel.rawValue }).sorted()
        #expect(row.selectedLabels == direct)
        #expect(row.isCleanTonic == (direct == ["tonic"]))
    }
}

// MARK: - SIM-1E current-settings (state tuning) reflected in the gate verdict

@Test func preview_configCarriesProvidedStateTuning() {
    let tuning = StatePatternDetectorTuning(tonicMinSpikes: 9, tonicCVMax: 0.21, tonicCV2Max: 0.22, tonicLVMax: 0.23)
    let config = SpikeTrainSimulationPreviewConfig(stateTuning: tuning)
    #expect(config.stateTuning == tuning)
    #expect(config.stateTuning.tonicCVMax == 0.21)
    #expect(config.stateTuning.tonicMinSpikes == 9)
}

@Test func preview_stricterTonicCVGateFromTuningFlipsVerdict() throws {
    // Tighten the CV gate below a rung that the complete current detector actually accepts as clean tonic. This keeps
    // the fixture valid when other independently justified state-support checks become stricter: the test still proves
    // that the supplied gate, rather than a fixed default, controls the verdict.
    let defaultRows = SpikeTrainSimulationPreview.generate(config: .init())
    let acceptedIndex = try #require(defaultRows.firstIndex {
        $0.reason == .passes && ($0.cv ?? 0) > 0
    })
    let acceptedCV = try #require(defaultRows[acceptedIndex].cv)
    let strictCVMax = acceptedCV * 0.5
    let strictRows = SpikeTrainSimulationPreview.generate(
        config: .init(stateTuning: StatePatternDetectorTuning(tonicCVMax: strictCVMax)))
    #expect(defaultRows.map(\.cv) == strictRows.map(\.cv))            // same generated trains
    #expect(defaultRows[acceptedIndex].passesTonicRegularityGates)
    #expect(!strictRows[acceptedIndex].passesTonicRegularityGates)
    #expect(strictRows[acceptedIndex].reason == .cvTooHigh)          // CV is the first failing gate
}

@Test func preview_stricterMinSpikesGateFromTuningCanFailLowRung() {
    // A min-spikes gate above the generated spike count fails every rung with `too_few_spikes`, reflecting the tuning.
    let rows = SpikeTrainSimulationPreview.generate(
        config: .init(durationSec: 30, stateTuning: StatePatternDetectorTuning(tonicMinSpikes: 5000)))
    #expect(rows.allSatisfy { $0.reason == .tooFewSpikes && !$0.passesTonicRegularityGates })
}

// MARK: - SIM-1F default-vs-current comparison

@Test func compare_defaultTuningYieldsNoDifferences() {
    // Comparing the defaults against the defaults: every rung is unchanged and the two evaluations are equal.
    let rows = SpikeTrainSimulationPreview.compare(config: .init())
    #expect(!rows.isEmpty)
    #expect(rows.allSatisfy { !$0.changed })
    #expect(rows.allSatisfy { $0.default == $0.current })
}

@Test func compare_stricterCVGateFlipsAtLeastOneRung() throws {
    let acceptedCV = try #require(
        SpikeTrainSimulationPreview.generate().first { $0.reason == .passes && ($0.cv ?? 0) > 0 }?.cv
    )
    let rows = SpikeTrainSimulationPreview.compare(
        config: .init(stateTuning: StatePatternDetectorTuning(tonicCVMax: acceptedCV * 0.5)))
    #expect(rows.contains { $0.changed })
    // The flipped rung passed under defaults but fails the stricter current CV gate.
    let flipped = try #require(rows.first { $0.changed && $0.default.reason == .passes })
    #expect(flipped.current.reason == .cvTooHigh)
    #expect(flipped.default.passesTonicRegularityGates)
    #expect(!flipped.current.passesTonicRegularityGates)
}

@Test func compare_generatedTrainsAndMetricsIdenticalAcrossTunings() {
    // The SAME generated train + metrics are shared by both evaluations — only the gate-driven verdict can differ.
    let rows = SpikeTrainSimulationPreview.compare(
        config: .init(stateTuning: StatePatternDetectorTuning(tonicMinSpikes: 7, tonicCVMax: 0.10)))
    for row in rows {
        #expect(row.default.train.timestampsSec == row.current.train.timestampsSec)
        #expect(row.default.cv == row.current.cv)
        #expect(row.default.cv2 == row.current.cv2)
        #expect(row.default.lv == row.current.lv)
        #expect(row.default.meanISISec == row.current.meanISISec)
        #expect(row.default.spikeCount == row.current.spikeCount)
        #expect(row.default.jitterSDSec == row.current.jitterSDSec)
    }
}

@Test func compare_currentSideMatchesGenerate() {
    // `compare(...).current` is exactly what `generate(...)` produces for the same config (consistency of the refactor).
    let config = SpikeTrainSimulationPreviewConfig(stateTuning: StatePatternDetectorTuning(tonicCVMax: 0.18))
    let generated = SpikeTrainSimulationPreview.generate(config: config)
    let compared = SpikeTrainSimulationPreview.compare(config: config)
    #expect(compared.map(\.current) == generated)
}

// MARK: - SIM-1D reason display labels (pure formatter/localization helper)

@Test func preview_reasonLocalizedLabelsAreBilingualAndKeepRawToken() {
    let cases: [(reason: SpikeTrainSimulationPreviewReason, raw: String, zh: String, en: String)] = [
        (.passes, "passes", "标准强直", "Clean tonic"),
        (.tooFewSpikes, "too_few_spikes", "棘波过少", "Too few spikes"),
        (.cvTooHigh, "cv_too_high", "CV 过高", "CV too high"),
        (.cv2TooHigh, "cv2_too_high", "CV2 过高", "CV2 too high"),
        (.lvTooHigh, "lv_too_high", "LV 过高", "LV too high"),
        (.notCleanTonic, "not_clean_tonic", "非纯强直", "Not clean tonic"),
    ]
    for c in cases {
        #expect(c.reason.rawValue == c.raw)                                   // raw machine token unchanged
        #expect(c.reason.localizedLabel(language: .zh) == c.zh)
        #expect(c.reason.localizedLabel(language: .en) == c.en)
    }
    #expect(SpikeTrainSimulationPreviewReason.passes.isPass)
    #expect(!SpikeTrainSimulationPreviewReason.cvTooHigh.isPass)
    #expect(SpikeTrainSimulationPreviewReason.allCases.allSatisfy { !$0.localizedLabel(language: .en).isEmpty })
}

// MARK: - isolation / no manual-threshold or export involvement

@Test func preview_usesIsolatedSyntheticTrainsAndNoManualThresholds() {
    let config = SpikeTrainSimulationPreviewConfig()
    let rows = SpikeTrainSimulationPreview.generate(config: config)
    for row in rows {
        // Synthetic, preview-only trains (never a real loaded dataset).
        #expect(row.train.name.hasPrefix("sim_tonic_sd_"))
        // The preview runs the detector with the DEFAULT (automatic) manual profile — no manual-threshold or scope
        // provenance is produced, so no manual-threshold / CSV-scope behavior is exercised.
        let dataset = SpikeDataset(name: "iso", sourceDescription: "iso", trains: [row.train])
        let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, stateTuning: config.stateTuning)
        for candidate in run.result(for: row.train.id)?.candidates ?? [] {
            #expect(!candidate.decisionPath.contains("resolved_threshold["))
            #expect(!candidate.decisionPath.contains("manual_threshold_scope="))
        }
    }
}
