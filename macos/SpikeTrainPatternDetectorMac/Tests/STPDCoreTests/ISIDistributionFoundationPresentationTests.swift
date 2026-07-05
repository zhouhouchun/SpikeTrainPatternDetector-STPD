@testable import STPDCore
import Testing

// MARK: - Distribution-first foundation presentation model tests (pure; no UI).

private func presTrain(_ name: String, isis: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for isi in isis { ts.append((ts.last ?? 0) + isi) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

private let presBurst = [0.003, 0.0032, 0.0035, 0.003, 0.0033]
private let presTonic = [0.040, 0.052, 0.045, 0.058, 0.048, 0.055, 0.043, 0.050, 0.047, 0.053]

private func presDataset() -> SpikeDataset {
    SpikeDataset(name: "pres", sourceDescription: "unit-test", trains: [
        presTrain("bimodal", isis: presBurst + presTonic),
        presTrain("tonic-only", isis: presTonic),
    ])
}

// 1 — end-to-end `from(...)` with no run distribution computes the fallback and shapes rows.
@Test
func presentationFromDatasetFallbackComputesAndShapes() {
    let dataset = presDataset()
    let p = ISIDistributionFoundationPresentation.from(
        dataset: dataset, runDistribution: nil, minimumValidISISec: 0.001)

    #expect(p.source == .computedFromDataset)
    #expect(p.datasetName == "pres")
    #expect(p.floorSec == 0.001)
    #expect(p.perTrain.count == 2)                                  // one row per train
    #expect(p.contributingTrainCount == 2)
    #expect(p.pooled.validISICount == p.pooledValidISICount)
    #expect(p.pooled.q50Sec != nil)
    #expect(p.trainBalanced != nil)
    #expect(p.trainBalanced?.validISICount == 2)                   // contributing-train count
    // Per-train row carries identity + count.
    let bimodal = p.perTrain.first { $0.label == "bimodal" }
    #expect(bimodal != nil)
    #expect(bimodal?.trainID != nil)
    #expect((bimodal?.validISICount ?? 0) == presBurst.count + presTonic.count)
}

// 2 — the bimodal dataset yields D3 burst + tonic dataset-scope interval rows with dataset provenance.
@Test
func presentationSurfacesDerivedDatasetIntervals() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)

    let families = Set(p.datasetIntervals.map(\.family))
    #expect(families.contains(.burst))
    #expect(families.contains(.tonic))
    for row in p.datasetIntervals {
        #expect(row.scope == .dataset)
        #expect(row.provenanceOrigin == .datasetSupported)         // dataset-scope priors propagate-eligible
        #expect(row.mayPropagateToDataset == true)
        #expect(row.isValid == true)
        #expect(row.lowerSec <= row.upperSec)
    }
    // Burst row carries a bridge; tonic does not.
    let burst = p.datasetIntervals.first { $0.family == .burst }
    #expect(burst?.bridgeUpperSec != nil)
    let tonic = p.datasetIntervals.first { $0.family == .tonic }
    #expect(tonic?.bridgeUpperSec == nil)
}

// 3 — a supplied run distribution is preferred over recompute, and marks the source.
@Test
func presentationPrefersRunDistributionWhenProvided() {
    let dataset = presDataset()
    let runDist = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.001)
    let p = ISIDistributionFoundationPresentation.from(
        dataset: dataset, runDistribution: runDist, minimumValidISISec: 0.001)

    #expect(p.source == .detectionRun)
    #expect(p.pooledValidISICount == runDist.pooledValidISICount)
    #expect(p.perTrain.count == runDist.trainDistributions.count)
}

// 4 — `make(...)` maps quantiles and interval provenance faithfully.
@Test
func presentationMakeMapsQuantilesAndProvenance() {
    let dataset = presDataset()
    let dist = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.001)
    let derived = ModeISIIntervalDeriver.derive(datasetDistribution: dist, minimumValidISISec: 0.001)
    let p = ISIDistributionFoundationPresentation.make(
        distribution: dist, derived: derived, source: .detectionRun, floorSec: 0.001)

    #expect(p.pooled.q50Sec == dist.pooledQuantiles.q50)
    #expect(p.pooled.q90Sec == dist.pooledQuantiles.q90)
    #expect(p.pooled.minSec == dist.pooledQuantiles.minSec)
    // Interval rows preserve provenance permission flags from the derived intervals.
    if let derivedBurst = derived.dataset.burst,
       let rowBurst = p.datasetIntervals.first(where: { $0.family == .burst }) {
        #expect(rowBurst.mayPropagateToDataset == derivedBurst.provenance.mayPropagateToDataset)
        #expect(rowBurst.maySelectFinalLabel == derivedBurst.provenance.maySelectFinalLabel)
        #expect(rowBurst.isAuditOnly == derivedBurst.provenance.isAuditOnly)
        #expect(rowBurst.sourceStatistic == derivedBurst.provenance.sourceStatistic)
    }
}

// 5 — empty dataset is safe: zero counts, no intervals, no crash.
@Test
func presentationHandlesEmptyDatasetSafely() {
    let empty = SpikeDataset(name: "empty", sourceDescription: "unit-test", trains: [])
    let p = ISIDistributionFoundationPresentation.from(
        dataset: empty, runDistribution: nil, minimumValidISISec: 0.001)

    #expect(p.perTrain.isEmpty)
    #expect(p.pooledValidISICount == 0)
    #expect(p.contributingTrainCount == 0)
    #expect(p.datasetIntervals.isEmpty)
    #expect(p.pooled.q50Sec == nil)
}

// 6 — interval rows expose the tonic acceptance band (wider than the core), for UI display.
@Test
func presentationExposesTonicAcceptanceBand() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)
    guard let tonic = p.datasetIntervals.first(where: { $0.family == .tonic }) else {
        #expect(Bool(false), "expected a tonic interval row"); return
    }
    #expect(tonic.acceptanceLowerSec != nil)
    #expect(tonic.acceptanceUpperSec != nil)
    #expect((tonic.acceptanceLowerSec ?? .infinity) <= tonic.lowerSec)   // core ⊆ acceptance
    #expect((tonic.acceptanceUpperSec ?? -.infinity) >= tonic.upperSec)
}

// 7 — log-ISI histogram: counts sum to total, bins are ordered/contiguous, degenerate inputs return nil.
@Test
func isiDistributionHistogramLogScaleBinsPooledValues() {
    let values = [0.003, 0.0032, 0.05, 0.052, 0.048, 0.5, 0.55]
    guard let h = ISIDistributionHistogram.logScale(values: values, binCount: 12) else {
        #expect(Bool(false), "expected a histogram"); return
    }
    #expect(h.totalCount == 7)
    #expect(h.bins.reduce(0) { $0 + $1.count } == 7)               // every value binned exactly once
    #expect(h.bins.count == 12)
    #expect(h.minSec == 0.003 && h.maxSec == 0.55)
    #expect(h.maxCount >= 1)
    for i in 0..<(h.bins.count - 1) {
        #expect(h.bins[i].lowerSec < h.bins[i].upperSec)
        #expect(abs(h.bins[i].upperSec - h.bins[i + 1].lowerSec) <= 1e-12)   // contiguous
    }
    // Degenerate: too few / single value / min==max ⇒ nil (no crash).
    #expect(ISIDistributionHistogram.logScale(values: [0.05]) == nil)
    #expect(ISIDistributionHistogram.logScale(values: [0.05, 0.05]) == nil)
    #expect(ISIDistributionHistogram.logScale(values: []) == nil)
}

// 8 — the presentation carries the pooled histogram (total == pooled valid ISI count).
@Test
func presentationCarriesPooledHistogram() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)
    #expect(p.histogram != nil)
    #expect(p.histogram?.totalCount == p.pooledValidISICount)
    #expect((p.histogram?.maxCount ?? 0) >= 1)
}

// 9 — `counts(for:)` bins onto the SAME edges as the pooled histogram: per-train overlays are aligned
// (same length as bins) and every train's counts nest inside the pooled counts bin-for-bin.
@Test
func histogramCountsForOverlayAlignWithPooledBins() {
    let values = [0.003, 0.0032, 0.05, 0.052, 0.048, 0.5, 0.55]
    guard let h = ISIDistributionHistogram.logScale(values: values, binCount: 12) else {
        #expect(Bool(false), "expected a histogram"); return
    }
    // Re-binning the pooled values through counts(for:) reproduces the histogram's own bar counts.
    let reBinned = h.counts(for: values)
    #expect(reBinned == h.bins.map(\.count))
    #expect(reBinned.reduce(0, +) == 7)

    // A subset (one "train") bins into a subset of the bins and never exceeds the pooled per-bin count.
    let subset = [0.05, 0.052, 0.048]
    let subCounts = h.counts(for: subset)
    #expect(subCounts.count == h.bins.count)
    #expect(subCounts.reduce(0, +) == 3)
    for i in 0..<subCounts.count { #expect(subCounts[i] <= reBinned[i]) }
    // Values outside the domain are clamped into the edge bins (not dropped, not crashing).
    #expect(h.counts(for: [1e-9, 1e9]).reduce(0, +) == 2)
}

// 10 — per-train details: one per train, overlay counts aligned to the pooled histogram, and the pooled
// histogram equals the bin-wise sum of the train overlays (pooled = sum of trains).
@Test
func presentationCarriesPerTrainOverlayDetails() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)
    #expect(p.perTrainDetails.count == p.perTrain.count)                 // one detail per train
    guard let hist = p.histogram else { #expect(Bool(false), "expected pooled histogram"); return }
    // Each overlay is aligned to the pooled bins and sums to that train's valid ISI count.
    for detail in p.perTrainDetails {
        #expect(detail.histogramCounts.count == hist.bins.count)
        #expect(detail.histogramCounts.reduce(0, +) == detail.validISICount)
    }
    // Bin-wise, the train overlays sum exactly to the pooled bars.
    var summed = [Int](repeating: 0, count: hist.bins.count)
    for detail in p.perTrainDetails {
        for i in 0..<summed.count { summed[i] += detail.histogramCounts[i] }
    }
    #expect(summed == hist.bins.map(\.count))
}

// 11 — a train-local prior can exist even when the same family is absent at dataset scope: the
// per-train details expose it (the "pause_response" inspection question, in miniature).
@Test
func perTrainDetailsExposeTrainLocalPriors() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)
    // The bimodal train carries burst + tonic train-local priors.
    guard let bimodal = p.perTrainDetails.first(where: { $0.trainName == "bimodal" }) else {
        #expect(Bool(false), "expected a bimodal per-train detail"); return
    }
    let localFamilies = Set(bimodal.intervals.map(\.family))
    #expect(localFamilies.contains(.tonic))
    // Train-local priors are train-scoped, distinguishing them from dataset priors.
    for row in bimodal.intervals { #expect(row.scope == .trainLocal) }
}

// 12 — KEY QUESTION: a train can carry a train-local prior (here PAUSE) that the pooled dataset prior
// LACKS. A "pause responder" train has a tight tonic cluster + a slow pause tail (its own sorted ISIs
// show a ≥2× upper gap ⇒ train-local pause), while a second "filler" train ramps smoothly across that
// gap so the POOLED distribution has no ≥2× upper gap ⇒ the dataset scope derives no pause. This is the
// exact "does pause_response show a train-local pause the dataset does not?" inspection question, pinned
// so a regression that sourced per-train priors from `derived.dataset` would fail here.
@Test
func perTrainDetailExposesPauseAbsentFromDataset() {
    // Pause responder: tonic ~50 ms + slow tail ~0.5 s (clear upper gap → train-local pause).
    let responder = presTrain(
        "pause_responder",
        isis: [0.048, 0.050, 0.052, 0.049, 0.051, 0.047, 0.053, 0.050, 0.50, 0.55, 0.52])
    // Filler: a smooth <2× ramp bridging ~0.05 s → ~0.55 s so the POOLED upper region has no ≥2× gap.
    let filler = presTrain("filler", isis: [0.06, 0.09, 0.13, 0.18, 0.25, 0.33, 0.45, 0.55])
    let dataset = SpikeDataset(name: "pause-case", sourceDescription: "unit-test", trains: [responder, filler])

    let p = ISIDistributionFoundationPresentation.from(
        dataset: dataset, runDistribution: nil, minimumValidISISec: 0.001)

    let datasetFamilies = Set(p.datasetIntervals.map(\.family))
    // Dataset-scope prior has NO pause (the filler bridged the gap in the pooled distribution).
    #expect(!datasetFamilies.contains(.pause))

    // ...but the responder's TRAIN-LOCAL prior does carry a pause the dataset lacks.
    guard let responderDetail = p.perTrainDetails.first(where: { $0.trainName == "pause_responder" }) else {
        #expect(Bool(false), "expected a pause_responder per-train detail"); return
    }
    let localFamilies = Set(responderDetail.intervals.map(\.family))
    #expect(localFamilies.contains(.pause))
    #expect(localFamilies.subtracting(datasetFamilies).contains(.pause))   // train-local-ONLY family
    let pauseRow = responderDetail.intervals.first { $0.family == .pause }
    #expect(pauseRow?.scope == .trainLocal)
}

// 13 — selectedTrainDetails(_:) returns trains in stable perTrainDetails order and silently drops ids
// that are not in the current presentation (so a stale/partial multi-selection stays consistent).
@Test
func selectedTrainDetailsAreStableOrderedAndDropUnknownIDs() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)
    let order = p.perTrainDetails.map(\.trainID)
    #expect(order.count == 2)
    // Selecting everything preserves perTrainDetails order.
    #expect(p.selectedTrainDetails(Set(order)).map(\.trainID) == order)
    // A known subset preserves order; unknown ids are dropped, not appended.
    let firstID = order[0]
    #expect(p.selectedTrainDetails([firstID, "not-a-real-train"]).map(\.trainID) == [firstID])
    // Reversed input set still comes back in perTrainDetails order (Set has no order; the method imposes it).
    #expect(p.selectedTrainDetails(Set(order.reversed())).map(\.trainID) == order)
    #expect(p.selectedTrainDetails([]).isEmpty)
}

// 14 — STACKED composition invariant: within each pooled bin, a SUBSET of selected trains sums to at
// most the pooled bar (so stacked segments never exceed the gray pooled bar), and ALL trains sum to
// EXACTLY the pooled bar (full composition == pooled). This is the y-scale contract the view stacks on.
@Test
func stackedSelectedTrainCountsComposeWithinPooledPerBin() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)
    guard let hist = p.histogram else { #expect(Bool(false), "expected pooled histogram"); return }
    let pooled = hist.bins.map(\.count)

    func binSums(_ details: [ISIDistributionTrainDetail]) -> [Int] {
        var sums = [Int](repeating: 0, count: hist.bins.count)
        for detail in details {
            #expect(detail.histogramCounts.count == hist.bins.count)   // every overlay is pooled-aligned
            for i in 0..<sums.count { sums[i] += detail.histogramCounts[i] }
        }
        return sums
    }

    let allIDs = Set(p.perTrainDetails.map(\.trainID))
    let subsetIDs = Set(p.perTrainDetails.prefix(1).map(\.trainID))     // a single train
    let allSums = binSums(p.selectedTrainDetails(allIDs))
    let subsetSums = binSums(p.selectedTrainDetails(subsetIDs))

    #expect(allSums == pooled)                                          // full stack == pooled bar
    for i in 0..<pooled.count {
        #expect(subsetSums[i] <= pooled[i])                            // subset never exceeds pooled bar
        #expect(subsetSums[i] <= allSums[i])                           // subset ⊆ full stack
    }
    // At least one bin has the single-train subset STRICTLY below the pooled bar → the gray pooled bar
    // stays visible above the stacked segment there (the "pooled behind" guarantee, pt4).
    #expect(zip(subsetSums, pooled).contains { $0 < $1 })
}

// 15 — focusedTrainDetail(requested:within:) resolves the effective focus train: the requested train
// when it is selected, otherwise the first selected train (stable order), and nil when nothing is
// selected. Pure backing for the UI-2D focus picker's default + deselect-fallback behavior.
@Test
func focusedTrainDetailResolvesRequestedElseFirstSelected() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)
    let ids = p.perTrainDetails.map(\.trainID)                          // stable order
    #expect(ids.count == 2)
    let all = Set(ids)

    // No request → first selected (in perTrainDetails order).
    #expect(p.focusedTrainDetail(requested: nil, within: all)?.trainID == ids[0])
    // Requested train that IS selected → that train.
    #expect(p.focusedTrainDetail(requested: ids[1], within: all)?.trainID == ids[1])
    // Requested train NOT in the selected set → first selected (deselect fallback).
    #expect(p.focusedTrainDetail(requested: ids[0], within: [ids[1]])?.trainID == ids[1])
    // Unknown id → first selected.
    #expect(p.focusedTrainDetail(requested: "not-a-real-train", within: all)?.trainID == ids[0])
    // Nothing selected → nil.
    #expect(p.focusedTrainDetail(requested: ids[0], within: []) == nil)
}

// MARK: - TSW-2 debug candidate rows (presentation only; TSW unwired).

// 16 — a clean regular tonic train produces a TSW candidate row spanning the full run.
@Test
func presentationComputesTSWCandidatesForCleanTonicTrain() {
    let tonic = presTrain("clean_tonic", isis: [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.051, 0.050, 0.049, 0.051])
    let dataset = SpikeDataset(name: "tsw", sourceDescription: "unit-test", trains: [tonic])
    let p = ISIDistributionFoundationPresentation.from(dataset: dataset, runDistribution: nil, minimumValidISISec: 0.001)
    let rows = p.tswCandidatesByTrainID[tonic.id] ?? []
    #expect(!rows.isEmpty)
    #expect(rows.first?.startISIIndex == 1)
    #expect(rows.first?.endISIIndex == 10)
    #expect(rows.first?.isiCount == 10)
    #expect(rows.first?.trainName == "clean_tonic")
}

// 17 — a hyper-regular burst-like train (sub-refractory-floor ISIs) yields NO TSW tonic rows.
@Test
func presentationBurstLikeTrainProducesNoTSWCandidate() {
    let burst = presTrain("burst_like", isis: [Double](repeating: 0.002, count: 10))   // < refractory floor ×3
    let dataset = SpikeDataset(name: "tsw", sourceDescription: "unit-test", trains: [burst])
    let p = ISIDistributionFoundationPresentation.from(dataset: dataset, runDistribution: nil, minimumValidISISec: 0.001)
    #expect((p.tswCandidatesByTrainID[burst.id] ?? []).isEmpty)
}

// 18 — TSW row honors the span→spike index convention: endSpikeIndex == endISIIndex + 1.
@Test
func presentationTSWRowIndexConvention() {
    let tonic = presTrain("t", isis: [0.050, 0.051, 0.049, 0.050, 0.052, 0.048])
    let dataset = SpikeDataset(name: "tsw", sourceDescription: "unit-test", trains: [tonic])
    let p = ISIDistributionFoundationPresentation.from(dataset: dataset, runDistribution: nil, minimumValidISISec: 0.001)
    guard let row = p.tswCandidatesByTrainID[tonic.id]?.first else { #expect(Bool(false), "expected a row"); return }
    #expect(row.startSpikeIndex == row.startISIIndex)
    #expect(row.endSpikeIndex == row.endISIIndex + 1)
    #expect(row.spikeCount == row.isiCount + 1)
}

// 19 — the D3-mismatch flag: a candidate whose ISI value range sits outside the D3 tonic acceptance band
// is flagged; a covered range or a nil band is not.
@Test
func tswCandidateRowD3MismatchFlag() {
    let metrics = ISISpanMetrics.compute(orderedValidISISec: [0.020, 0.022, 0.021])   // range 0.020–0.022
    let span = ISISpan(trainID: "x", startISIIndex: 1, endISIIndex: 3, familyHint: .tonic)
    let candidate = TonicStructuralWindowCandidate(
        span: span, metrics: metrics, source: .seed, signals: [], reviewRequired: false,
        boundaryReason: nil, decisionPath: "test")
    // Acceptance band well above the candidate range → outside.
    #expect(TSWCandidateRow(trainName: "x", candidate: candidate,
                            d3TonicAcceptance: (lower: 0.040, upper: 0.060)).outsideD3Acceptance == true)
    // Acceptance band covering the range → not outside.
    #expect(TSWCandidateRow(trainName: "x", candidate: candidate,
                            d3TonicAcceptance: (lower: 0.010, upper: 0.030)).outsideD3Acceptance == false)
    // No D3 tonic band → not flagged.
    #expect(TSWCandidateRow(trainName: "x", candidate: candidate, d3TonicAcceptance: nil).outsideD3Acceptance == false)
}

// 20 — TSW presentation is safe for an empty dataset.
@Test
func presentationTSWSafeForEmptyDataset() {
    let empty = SpikeDataset(name: "empty", sourceDescription: "unit-test", trains: [])
    let p = ISIDistributionFoundationPresentation.from(dataset: empty, runDistribution: nil, minimumValidISISec: 0.001)
    #expect(p.tswCandidatesByTrainID.isEmpty)
}

// 21 — TSW-2A: rows carry a family route; a slow regular tonic train routes classic tonic (the D3
// tonic-core-lower fallback is threaded into the scan).
@Test
func presentationTSWRowsCarryRouteAndSlowTonicIsClassic() {
    let tonic = presTrain("slow_tonic", isis: [0.44, 0.46, 0.45, 0.47, 0.45, 0.46, 0.44, 0.46, 0.45, 0.47, 0.45, 0.46])
    let dataset = SpikeDataset(name: "tsw", sourceDescription: "unit-test", trains: [tonic])
    let p = ISIDistributionFoundationPresentation.from(dataset: dataset, runDistribution: nil, minimumValidISISec: 0.001)
    let rows = p.tswCandidatesByTrainID[tonic.id] ?? []
    #expect(!rows.isEmpty)
    #expect(rows.allSatisfy { $0.route == "classicTonic" })
}

// 22 — TSW-2A: the physiological classic-tonic floor tracks the dataset QC floor threaded through the
// presentation (refractoryFloorSec == minimumValidISISec). The SAME ~20 ms regular train routes classic at
// minimumValidISISec = 0.001 (physiological floor 15 ms) but too-fast at 0.0015 (floor 22.5 ms). Without the
// fix (refractoryFloorSec pinned to 0.001) both would route classic and the second assertion would fail.
@Test
func presentationTSWPhysiologicalFloorTracksQCFloor() {
    let train = presTrain("mid", isis: [0.020, 0.021, 0.019, 0.022, 0.020, 0.021, 0.019, 0.022, 0.020, 0.021])
    let dataset = SpikeDataset(name: "tsw", sourceDescription: "unit-test", trains: [train])
    let low = ISIDistributionFoundationPresentation.from(dataset: dataset, runDistribution: nil, minimumValidISISec: 0.001)
        .tswCandidatesByTrainID[train.id] ?? []
    let high = ISIDistributionFoundationPresentation.from(dataset: dataset, runDistribution: nil, minimumValidISISec: 0.0015)
        .tswCandidatesByTrainID[train.id] ?? []
    #expect(!low.isEmpty && !high.isEmpty)
    #expect(low.contains { $0.route == "classicTonic" })              // physiological floor 15 ms < 20 ms → classic
    #expect(high.allSatisfy { $0.route == "tooFastForClassicTonic" }) // physiological floor 22.5 ms > 20 ms → too fast
}
