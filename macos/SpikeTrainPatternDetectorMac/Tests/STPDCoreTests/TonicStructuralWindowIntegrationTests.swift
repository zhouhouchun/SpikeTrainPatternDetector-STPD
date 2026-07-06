import Foundation
import STPDCore
import Testing

// MARK: - TSW-INTEGRATION — the from-sequence expandable sliding window (TonicStructuralWindowDetector)
// wired in as the PRIMARY tonic structural candidate generator inside StatePatternDetector.detectTonic.
// These pin the behavioral goals of the wiring; the burst/pause/BCB-1 regression suite is covered
// separately (BCB1*, TonicBoundaryRescue*, PauseTonicArbitration*).

private func tswiCumulative(_ isis: [Double]) -> [Double] {
    var t = 0.0; var out = [0.0]
    for x in isis { t += x; out.append(t) }
    return out
}
private func tswiTrain(_ name: String, _ isis: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: tswiCumulative(isis))
}
private func tswiAcceptedTonic(_ train: SpikeTrain, primary: Bool = true) -> [ClassicAnchorCandidate] {
    var settings = StatePatternDetectorSettings()
    settings.tonicStructuralWindowPrimary = primary
    return StatePatternDetector.detect(train: train, settings: settings).candidates
        .filter { $0.finalLabel == .tonic && $0.action == "accept" }
        .sorted { $0.startISIIndex < $1.startISIIndex }
}
private func tswiFixture5x5() throws -> SpikeDataset {
    let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8),
                                          datasetName: "5x5", sourceDescription: url.path)
}

// 1 — a clean regular tonic run is detected as ONE long tonic candidate spanning the whole run (not
// fragmented into sub-windows), produced by the TSW structural window.
@Test
func tswIntegrationCleanRunIsOneLongCandidate() {
    let isis: [Double] = [0.040, 0.043, 0.037, 0.041, 0.039, 0.042, 0.038, 0.040,
                          0.041, 0.039, 0.043, 0.038, 0.040, 0.042, 0.037, 0.041]   // 16 clean ISIs
    let train = tswiTrain("clean_tonic_long", isis)
    let accepted = tswiAcceptedTonic(train)
    #expect(accepted.count == 1)                                       // NOT fragmented
    let c = accepted.first
    #expect(c?.startISIIndex == 1)
    #expect(c?.endISIIndex == isis.count)                             // spans the whole run
    #expect(c?.stateTonicSubtype == "classic")
    #expect(c?.decisionPath.contains("tonic_structural_window") == true)
    #expect((c?.cv ?? 1) <= 0.30)
}

// 2 — on the 5x5 dataset the pause_response trains' visually-tonic baseline segments are captured as
// selected tonic candidates produced by the TSW structural window.
@Test
func tswIntegrationPauseResponseTonicSegmentsCaptured() throws {
    let dataset = try tswiFixture5x5()
    for n in 1...5 {
        let name = "pause_response_\(n)_s"
        let train = try #require(dataset.trains.first { $0.name == name })
        let run = ClassicAnchorDetectionPipeline.run(
            dataset: SpikeDataset(name: "one", sourceDescription: dataset.sourceDescription, trains: [train]),
            bandSettings: TrainAdaptiveBandSettings()
        )
        let tonics = (run.result(for: train.id)?.candidates ?? [])
            .filter { $0.selectedForAuto && $0.finalLabel == .tonic }
        #expect(!tonics.isEmpty, "\(name): expected selected tonic baseline segments")
        #expect(tonics.contains { $0.decisionPath.contains("tonic_structural_window") },
                "\(name): tonic should be TSW-sourced")
    }
}

// 3 — a tonic run followed by a LARGE pause ISI stops BEFORE the pause: no tonic candidate covers the
// pause ISI, and it remains available for pause detection.
@Test
func tswIntegrationStopsBeforeLargePause() {
    let isis = Array(repeating: 0.050, count: 8) + [1.5] + Array(repeating: 0.050, count: 8)  // pause at ISI 9
    let train = tswiTrain("tonic_pause_tonic", isis)
    let accepted = tswiAcceptedTonic(train)
    #expect(!accepted.isEmpty)
    #expect(!accepted.contains { ($0.startISIIndex...$0.endISIIndex).contains(9) })   // never swallows the pause
    // The first tonic run ends at the last pre-pause ISI (8), not across the 1.5 s gap.
    #expect(accepted.contains { $0.startISIIndex == 1 && $0.endISIIndex == 8 })
}

// 4 — a fast, regular burst-like packet is NOT converted into classic tonic: the burst-contamination /
// magnitude guards keep the sliding window from seeding tonic on sub-refractory-fast ISIs.
@Test
func tswIntegrationFastPacketIsNotClassicTonic() {
    // Uniform 4 ms ISIs — well below the burst seed upper (10 ms) and the physiological classic-tonic floor.
    let fast = tswiTrain("fast_packet", Array(repeating: 0.004, count: 14))
    #expect(tswiAcceptedTonic(fast).isEmpty)

    // Mixed: a fast packet embedded next to a real tonic baseline — the tonic forms, but no tonic candidate
    // classifies the fast packet (ISIs 9...14) as classic tonic.
    let mixed = tswiTrain("tonic_then_fast",
                          Array(repeating: 0.050, count: 8) + Array(repeating: 0.004, count: 6))
    let accepted = tswiAcceptedTonic(mixed)
    #expect(!accepted.isEmpty)
    #expect(!accepted.contains { c in (9...14).contains { c.startISIIndex <= $0 && c.endISIIndex >= $0 } })
}

// 5 — the integration is behind a reversible flag: OFF restores the legacy band-membership path (no
// `tonic_structural_window` provenance); ON uses the sliding window. A/B / rollback safety.
@Test
func tswIntegrationFlagIsReversible() {
    let train = tswiTrain("flag_ab",
                          [0.040, 0.043, 0.037, 0.041, 0.039, 0.042, 0.038, 0.040, 0.041, 0.039, 0.043, 0.038])
    let on = tswiAcceptedTonic(train, primary: true)
    let off = tswiAcceptedTonic(train, primary: false)
    #expect(!on.isEmpty && !off.isEmpty)                                          // both still detect tonic
    #expect(on.contains { $0.decisionPath.contains("tonic_structural_window") })  // ON = sliding window
    #expect(off.allSatisfy { !$0.decisionPath.contains("tonic_structural_window") })  // OFF = legacy band scan
}
