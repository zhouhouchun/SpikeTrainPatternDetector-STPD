import Foundation
import STPDCore
import Testing

// Phase 2B: per-candidate ISI temporal-profile evidence (diagnostic only). These tests pin the R
// parity of the evidence formulas and PROVE the evidence never changes detection output.

private func train(_ name: String, _ timestamps: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: timestamps)
}

// ISIs (0-based isiSec): nil, 0.1, 0.1, 0.010, 0.010, 0.012, 0.120, 0.100
// A compressed core at ISI indices 3..5 flanked by long ISIs.
private let coreTimestamps: [Double] = {
    let isi = [0.100, 0.100, 0.010, 0.010, 0.012, 0.120, 0.100]
    var ts = [0.0]
    for d in isi { ts.append(ts.last! + d) }
    return ts
}()
private let coreTrain = train("c", coreTimestamps)

private func evidence(
    _ t: SpikeTrain, start: Int, end: Int, minValid: Double = 0.001
) -> ISITemporalProfileEvidence {
    ISITemporalProfileEvidenceBuilder.makeEvidence(
        train: t, startISIIndex: start, endISIIndex: end, minValidISISec: minValid
    )
}

// MARK: - 1. Edge contrast / core percentile / local compression on a known train.

@Test
func edgeContrastAndCoreEvidenceMatchHandComputedValues() {
    // span = ISI indices 3...5 -> vals [0.010, 0.010, 0.012]; pre = isi[2] = 0.100, post = isi[6] = 0.120.
    let e = evidence(coreTrain, start: 3, end: 5)
    let coreQ = 0.010 + 0.8 * (0.012 - 0.010)   // type-7 quantile @0.90 of [0.010,0.010,0.012] = 0.0116
    let preRatio = 0.100 / coreQ
    let postRatio = 0.120 / coreQ
    #expect(abs((e.preEdgeRatio ?? 0) - preRatio) < 1e-9)
    #expect(abs((e.postEdgeRatio ?? 0) - postRatio) < 1e-9)
    #expect(abs((e.edgeContrastMin ?? 0) - min(preRatio, postRatio)) < 1e-9)
    #expect(abs((e.edgeContrastGeom ?? 0) - (preRatio * postRatio).squareRoot()) < 1e-9)
    #expect(e.flankCount == 2)
    // core_q percentile: valid ISIs sorted [0.010,0.010,0.012,0.100,0.100,0.100,0.120], nValid 7.
    // coreQ = 0.0116, so count(<= coreQ) = 2 (the two ~0.010 ISIs; 0.012 > 0.0116) -> 100*2/7.
    #expect(abs((e.coreQPct ?? 0) - (100.0 * 2.0 / 7.0)) < 1e-6)
    #expect(e.percentileReliable == false)   // 7 < 50
    // local median of each in-span index's window-median is 0.100; compression = 0.100 / coreQ.
    #expect(abs((e.localMedianISISec ?? 0) - 0.100) < 1e-9)
    #expect(abs((e.localCompressionRatio ?? 0) - (0.100 / coreQ)) < 1e-9)
}

// MARK: - 2. Missing flanks produce nil edge evidence, never fabricated values.

@Test
func missingBothFlanksYieldNilEdgeButStillComputesCoreAndLocal() {
    // 3 spikes, ISIs [nil, 0.01, 0.01]; span 1...2 spans the whole train: no pre (start 1), no post (end == n-1).
    let t = train("s", [0, 0.01, 0.02])
    let e = evidence(t, start: 1, end: 2)
    #expect(e.preEdgeRatio == nil)
    #expect(e.postEdgeRatio == nil)
    #expect(e.edgeContrastMin == nil)
    #expect(e.edgeContrastGeom == nil)
    #expect(e.flankCount == 0)
    // coreQ + local are still defined (not fabricated edge evidence).
    #expect(e.coreQPct != nil)
    #expect(e.localMedianISISec != nil)
}

@Test
func oneMissingFlankGivesSingleFlankEvidence() {
    // Candidate at the very start: span 1...3 -> no pre (start 1), post = isi[4].
    // ISIs: nil,0.010,0.010,0.010,0.200,0.100
    let t = train("o", [0, 0.010, 0.020, 0.030, 0.230, 0.330])
    let e = evidence(t, start: 1, end: 3)
    #expect(e.preEdgeRatio == nil)
    #expect(e.postEdgeRatio != nil)
    #expect(e.flankCount == 1)
    // with one flank, min == geom == that single ratio.
    #expect(e.edgeContrastMin == e.postEdgeRatio)
    #expect(abs((e.edgeContrastGeom ?? 0) - (e.postEdgeRatio ?? 0)) < 1e-12)
}

// MARK: - 3. Out-of-range span / short train -> fully undefined evidence.

@Test
func outOfRangeOrShortTrainIsUndefined() {
    // start 0 is invalid (isiSec[0] is nil).
    let bad = evidence(coreTrain, start: 0, end: 2)
    #expect(bad.edgeContrastMin == nil && bad.coreQPct == nil && bad.localMedianISISec == nil)
    #expect(bad.flankCount == 0)
    // train too short for any ISI.
    let tiny = evidence(train("t", [0]), start: 1, end: 1)
    #expect(tiny.coreQPct == nil && tiny.percentileReliable == false)
}

// MARK: - 4. Percentile reliability threshold + rightmost.closed max handling.

@Test
func percentileReliableTrueAtFiftyOrMoreValidISIs() {
    // 60 spikes -> 59 valid ISIs of 0.05 each; reliable.
    var ts: [Double] = [0]
    for _ in 0..<60 { ts.append(ts.last! + 0.05) }
    let e = evidence(train("r", ts), start: 5, end: 10)
    #expect(e.percentileReliable == true)
}

@Test
func coreQEqualToTrainMaxUsesRightmostClosedPercentile() {
    // All ISIs equal -> coreQ equals the train max; rightmost.closed maps the max to 100*(n-1)/n.
    // 6 spikes -> 5 valid ISIs all 0.02. span 1...5 -> coreQ = 0.02 = max.
    let t = train("m", [0, 0.02, 0.04, 0.06, 0.08, 0.10])
    let e = evidence(t, start: 1, end: 5)
    #expect(abs((e.coreQPct ?? 0) - (100.0 * 4.0 / 5.0)) < 1e-9)   // not 100
}

// MARK: - 5. Zero detector influence (the key invariant).

private func burstDataset() -> SpikeDataset {
    // Clear burst: 100 ms baseline, a run of 6 short (5 ms) ISIs, then baseline again.
    let isi = [0.1, 0.1, 0.1, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.1, 0.1, 0.1]
    var ts = [0.0]
    for d in isi { ts.append(ts.last! + d) }
    return SpikeDataset(name: "burst", sourceDescription: "synthetic", trains: [train("b", ts)])
}

private func signature(_ run: ClassicAnchorDetectionRun) -> [String] {
    run.candidates.map {
        "\($0.id)|\($0.finalLabel)|\($0.startISIIndex)|\($0.endISIIndex)|\($0.startSpikeIndex)|\($0.endSpikeIndex)|\($0.selectedForAuto)|\($0.score)"
    }.sorted()
}

@Test
func computingEvidenceDoesNotChangeDetectionOutput() {
    let dataset = burstDataset()
    let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run1 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    let before = signature(run1)
    #expect(run1.candidates.count >= 1)   // the fixture actually produces candidates

    // "Attach" evidence for every candidate (the new diagnostic step).
    let evidenceByID = ISITemporalProfileEvidenceBuilder.evidenceByCandidateID(run: run1, dataset: dataset)
    let after = signature(run1)
    #expect(before == after)                                          // evidence computation mutated nothing
    #expect(evidenceByID.count == run1.candidates.count)              // every candidate id maps to evidence
    #expect(run1.candidates.allSatisfy { evidenceByID[$0.id] != nil })

    // Re-running the pipeline is identical (evidence has no global side effect).
    let run2 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    #expect(signature(run2) == before)

    // Sanity: at least one candidate actually gets defined edge evidence (the service does compute).
    #expect(evidenceByID.values.contains { $0.edgeContrastMin != nil })
}

// MARK: - 6. CSV export carries the new diagnostic columns, appended, without disturbing existing ones.

@Test
func csvExportAppendsISIEvidenceColumnsWithoutChangingExistingColumns() throws {
    let dataset = burstDataset()
    let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: [],
        reviewStatuses: [:],
        includeUnselectedCandidates: true
    )
    let header = csv.split(separator: "\n").first.map { $0.split(separator: ",").map(String.init) } ?? []
    let newColumns = [
        "isi_edge_contrast_min", "isi_edge_contrast_geom", "isi_pre_edge_ratio", "isi_post_edge_ratio",
        "isi_flank_count", "isi_core_q_pct", "isi_percentile_reliable", "isi_local_median_sec",
        "isi_local_compression_ratio"
    ]
    for column in newColumns {
        #expect(header.contains(column))
    }
    // Existing required columns are still present (not removed/renamed).
    #expect(header.contains("candidate_id"))
    #expect(header.contains("label"))
    #expect(header.contains("resolved_thresholds"))
    // The new columns form a contiguous ordered block (append-only; Phase 2C eventness columns
    // follow this block, so it is no longer necessarily the header suffix).
    let start = try #require(header.firstIndex(of: "isi_edge_contrast_min"))
    #expect(start + newColumns.count <= header.count)
    #expect(Array(header[start..<(start + newColumns.count)]) == newColumns)
}
