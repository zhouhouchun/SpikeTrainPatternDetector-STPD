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
    // TSW-INTEGRATION: the stable left neighbors are now captured DIRECTLY by the first-stage expandable
    // sliding window (one maximal [1...10] structural window), rather than requiring the boundary-rescue
    // post-pass to add them back — exactly the fragmentation the sliding window removes. Provenance reflects
    // the structural window instead of `tonic_boundary_rescue=true` / `rescued_left_isi=2`.
    #expect(rescued.decisionPath.contains("tonic_structural_window"))
    #expect(rescued.decisionPath.contains("expanded_window"))
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
    // Sanity on a real bursty STN dataset: the rescue post-pass fires zero times and burst detection is
    // unchanged. Under the TSW primary path the first-stage sliding window may legitimately surface a small
    // number of genuinely slow, regular, burst-clean micro-epochs (e.g. a ~35 ms-median 5-spike run) that
    // the old band scan clipped away — a recall gain, not burst contamination. The invariants that matter
    // are that burst detection is untouched and any such tonic is magnitude-clean (well above the burst
    // regime), never swallowing a burst core.
    guard let ds = try grechishnikovaDataset() else { return }   // graceful skip if the repo fixture is absent
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    var rescue = 0, burst = 0, tonic = 0
    var fastTonic = 0
    for tr in ds.trains {
        let cs = run.result(for: tr.id)?.candidates ?? []
        rescue += cs.filter { $0.decisionPath.contains("tonic_boundary_rescue=true") }.count
        burst += cs.filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }.count
        for c in cs where c.selectedForAuto && c.finalLabel == .tonic {
            tonic += 1
            // Median ISI of the tonic candidate must be clearly above the burst seed regime (not burst).
            let isis = (c.startISIIndex...c.endISIIndex).compactMap { tr.isiSec[$0] }.sorted()
            if let med = isis.isEmpty ? nil : isis[isis.count / 2], med <= 0.020 { fastTonic += 1 }
        }
    }
    #expect(rescue == 0)        // the rescue is inert (cannot distort)
    #expect(burst >= 70)        // burst detection preserved (unchanged by the tonic path)
    #expect(tonic <= 3)         // only a few genuine slow micro-epochs, not a tonic flood
    #expect(fastTonic == 0)     // …and none is burst-fast (no burst-core swallowed into tonic)
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
