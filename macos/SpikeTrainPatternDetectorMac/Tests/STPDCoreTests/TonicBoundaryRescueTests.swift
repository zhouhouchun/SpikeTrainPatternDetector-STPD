import Foundation
import STPDCore
import Testing

private func tonicBoundaryFixtureDataset() throws -> SpikeDataset {
    let url = URL(fileURLWithPath: #filePath)
        .deletingLastPathComponent()
        .appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
    let csv = try String(contentsOf: url, encoding: .utf8)
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "5x5", sourceDescription: url.path)
}

@Test
func tonicBoundaryRescueIncludesStableLeftNeighbors() throws {
    let dataset = try tonicBoundaryFixtureDataset()
    let train = try #require(dataset.trains.first { $0.name == "burst_response_2_s" })
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "single", sourceDescription: dataset.sourceDescription, trains: [train]),
        bandSettings: TrainAdaptiveBandSettings()
    )
    let candidates = run.result(for: train.id)?.candidates ?? []
    let rescued = try #require(candidates.first {
        $0.selectedForAuto &&
            $0.finalLabel == .tonic &&
            $0.startISIIndex <= 1 &&
            $0.endISIIndex >= 10
    })

    #expect(rescued.startISIIndex == 1)
    #expect(rescued.endISIIndex == 10)
    #expect(rescued.decisionPath.contains("tonic_boundary_rescue=true"))
    #expect(rescued.decisionPath.contains("rescued_left_isi=2"))
    #expect(rescued.cv.map { $0 <= 0.30 + 1e-12 } ?? false)
    #expect(rescued.cv2.map { $0 <= 0.30 + 1e-12 } ?? false)
    #expect(rescued.lv.map { $0 <= 0.35 + 1e-12 } ?? false)
}

@Test
func tonicBoundaryRescueDoesNotCrossBurstTransition() throws {
    let dataset = try tonicBoundaryFixtureDataset()
    let train = try #require(dataset.trains.first { $0.name == "burst_response_2_s" })
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "single", sourceDescription: dataset.sourceDescription, trains: [train]),
        bandSettings: TrainAdaptiveBandSettings()
    )
    let candidates = run.result(for: train.id)?.candidates ?? []
    let selectedTonic = candidates.filter { $0.selectedForAuto && $0.finalLabel == .tonic }

    #expect(!selectedTonic.contains { $0.startISIIndex <= 11 && $0.endISIIndex >= 11 })
    #expect(!selectedTonic.contains { $0.startISIIndex <= 12 && $0.endISIIndex >= 12 })
    #expect(!selectedTonic.contains { $0.startISIIndex <= 13 && $0.endISIIndex >= 13 })
    #expect(candidates.contains {
        $0.selectedForAuto &&
            $0.finalLabel.isBurstEventFamily &&
            $0.startISIIndex <= 12 &&
            $0.endISIIndex >= 13
    })
}

// MARK: - negative cases (conservative: do NOT over-expand)

private func cumulative(_ isis: [Double]) -> [Double] {
    var t = 0.0; var times = [0.0]
    for x in isis { t += x; times.append(t) }
    return times
}
private func singleTrainRun(_ name: String, _ isis: [Double]) -> (SpikeTrain, ClassicAnchorDetectionRun) {
    let train = SpikeTrain(name: name, timestampsSec: cumulative(isis))
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "s", sourceDescription: "s", trains: [train]), bandSettings: TrainAdaptiveBandSettings())
    return (train, run)
}
private func selectedTonic(_ run: ClassicAnchorDetectionRun, _ train: SpikeTrain) -> [ClassicAnchorCandidate] {
    (run.result(for: train.id)?.candidates ?? []).filter { $0.selectedForAuto && $0.finalLabel == .tonic }
}

@Test
func tonicBoundaryRescueRejectsRegularityBreakingNeighbor() {
    // A stable regular tonic run with a faster left-neighbor ISI (0.34 s, well below the run's own q10) that would
    // degrade tonic regularity. The stable-neighbor guard rejects it: the tonic candidate is NOT expanded to swallow it.
    // (A trailing burst packet gives the band resolver burst-vs-tonic structure so a tonic candidate forms.)
    let isis = [0.34] + Array(repeating: 0.45, count: 8) + [0.011, 0.011, 0.011]   // [1]=0.34 fast neighbor; [2...9]=tonic
    let (train, run) = singleTrainRun("regularity_negative", isis)
    let tonic = selectedTonic(run, train)
    #expect(!tonic.isEmpty)                                       // a tonic candidate formed over the regular run
    #expect(!tonic.contains { $0.startISIIndex <= 1 && $0.endISIIndex >= 1 })   // …but none swallowed the faster ISI[1]
}

@Test
func tonicBoundaryRescueDoesNotCrossPauseOrBurst() {
    // A regular tonic run flanked by a PAUSE on the left (1.5 s) and a BURST packet on the right (3× ~0.011 s). Neither
    // boundary is swallowed: the pause is far above the tonic upper, the burst ISIs are below the burst-seed/fast guard.
    let isis = [1.5] + Array(repeating: 0.45, count: 8) + [0.011, 0.011, 0.011]   // [1]=pause, [2...9]=tonic, [10...12]=burst
    let (train, run) = singleTrainRun("cross_negative", isis)
    let tonic = selectedTonic(run, train)
    #expect(!tonic.isEmpty)
    #expect(!tonic.contains { $0.startISIIndex <= 1 && $0.endISIIndex >= 1 })            // not into the pause ISI[1]
    #expect(!tonic.contains { ($0.startISIIndex...$0.endISIIndex).contains(10) })        // not into the burst packet
    #expect(!tonic.contains { ($0.startISIIndex...$0.endISIIndex).contains(11) })
}

@Test
func tonicBoundaryRescueIsInertOnBurstyGrechishnikova() throws {
    // Sanity: on a real bursty STN dataset (no tonic state), the rescue fires zero times and burst detection is
    // unchanged — the rescue is targeted (only expands EXISTING tonic candidates), not a blanket tonic fill.
    guard let ds = try grechishnikovaDataset() else { return }   // graceful skip if the repo fixture is absent
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    var rescue = 0, burst = 0, tonic = 0
    for tr in ds.trains {
        let cs = run.result(for: tr.id)?.candidates ?? []
        rescue += cs.filter { $0.decisionPath.contains("tonic_boundary_rescue=true") }.count
        burst += cs.filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }.count
        tonic += cs.filter { $0.selectedForAuto && $0.finalLabel == .tonic }.count
    }
    #expect(tonic == 0)         // bursty STN → no tonic candidates
    #expect(rescue == 0)        // …so the rescue is inert (cannot distort)
    #expect(burst >= 70)        // burst detection preserved (the rescue never touches burst candidates)
}

private func grechishnikovaDataset() throws -> SpikeDataset? {
    let repo = URL(fileURLWithPath: #filePath)
        .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
        .deletingLastPathComponent().deletingLastPathComponent()
    let url = repo.appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    guard FileManager.default.fileExists(atPath: url.path) else { return nil }
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8),
                                          datasetName: "grech", sourceDescription: url.path, allowDerivedCSV: true)
}
