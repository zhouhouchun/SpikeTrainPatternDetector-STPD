import Foundation
import STPDCore
import Testing

// SEG-PREVIEW-1: pure read-only boundary-sensitivity previewer (expansion + contraction). Hand-built ISI arrays
// with known regularity so the gate outcomes are deterministic and independent of detector behavior.

private let gates = TonicGateThresholds(cvMax: 0.30, cv2Max: 0.30, lvMax: 0.35, minSpikes: 5)

/// Build a trace-aligned `[Double?]` (index 0 is nil) from valid ISI values starting at index 1.
private func trace(_ values: [Double]) -> [Double?] {
    [nil] + values.map { Optional($0) }
}

private func row(_ p: SegmentSensitivityPreview, _ variant: SegmentSensitivityVariant) -> SegmentSensitivityRow {
    p.rows.first { $0.variant == variant }!
}

@Test
func stableTonicStaysStableUnderMildExpansionAndContraction() throws {
    // 12 near-identical ISIs (~0.45 s, tiny jitter) → very low CV/CV2/LV everywhere.
    let isi = trace([0.45, 0.44, 0.46, 0.45, 0.46, 0.44, 0.45, 0.46, 0.45, 0.44, 0.46, 0.45])
    let p = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 4...9, gates: gates))

    #expect(row(p, .current).verdict.passes)
    #expect(row(p, .current).stability == .reference)
    // Mild expansion/contraction over the regular run keeps the tonic outcome.
    for v in [SegmentSensitivityVariant.expandLeft, .expandRight, .expandBoth, .contractLeft, .contractRight, .contractBoth] {
        #expect(row(p, v).verdict.passes, "\(v) should still pass")
        #expect(row(p, v).stability == .stable, "\(v) should be stable")
    }
    // nISI / nSpikes arithmetic: current [4...9] = 6 ISIs / 7 spikes; expandBoth [3...10] = 8 / 9.
    #expect(row(p, .current).nISI == 6 && row(p, .current).nSpikes == 7)
    #expect(row(p, .expandBoth).nISI == 8 && row(p, .expandBoth).nSpikes == 9)
}

@Test
func expansionIntoLongOutlierFails() throws {
    // Regular core at indices 1...6, a large outlier (2.0 s) sitting at index 7 (the right neighbor).
    let isi = trace([0.45, 0.44, 0.46, 0.45, 0.46, 0.45, 2.00])
    let p = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 1...6, gates: gates))

    #expect(row(p, .current).verdict.passes)               // regular core is tonic
    let right = row(p, .expandRight)
    #expect(!right.verdict.passes)                          // pulling in the outlier breaks a gate
    #expect(right.failureReason != nil)
    #expect(right.stability == .changed)
    #expect(row(p, .expandBoth).stability == .changed)      // both-side expansion also includes the outlier
    // Left expansion is clipped at the train start (index 1 is the first valid ISI) → no change, still stable.
    #expect(row(p, .expandLeft).clippedAtBoundary)
    #expect(row(p, .expandLeft).stability == .stable)
}

@Test
func contractionRevealsStableCore() throws {
    // A left-edge outlier (1.5 s) makes the CURRENT span fail; trimming it reveals a tonic core.
    let isi = trace([1.50, 0.45, 0.46, 0.44, 0.45, 0.46, 0.45])
    let p = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 1...7, gates: gates))

    #expect(!row(p, .current).verdict.passes)              // edge outlier sinks the whole span
    let left = row(p, .contractLeft)                       // drop index 1 (the outlier)
    #expect(left.verdict.passes)                           // core [2...7] is tonic
    #expect(left.stability == .changed)                    // label depended on the edge ISI
    #expect(row(p, .contractLeft).nISI == 6)
}

@Test
func contractionTooShortIsHandledGracefully() throws {
    // Default gates require >=5 spikes. A 5-ISI (6-spike) tonic span: contracting BOTH edges → 3 ISIs / 4 spikes,
    // below the min-spikes floor → reported "too short" rather than failed as non-tonic.
    let isi = trace([0.45, 0.46, 0.44, 0.45, 0.46, 0.44, 0.45])
    let p = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 1...5, gates: gates))

    #expect(row(p, .current).verdict.passes)
    let both = row(p, .contractBoth)                       // [2...4] = 3 ISIs / 4 spikes
    #expect(both.nISI == 3 && both.nSpikes == 4)
    #expect(both.verdict.isTooShort)                       // below minSpikes(5) → too short, NOT a gate failure
    #expect(both.stability == .notComparable)

    // Collapsing to a single ISI (no pairwise metrics) is also handled as "too short", not a crash.
    let tiny = TonicGateThresholds(cvMax: 0.30, cv2Max: 0.30, lvMax: 0.35, minSpikes: 2)
    let q = try #require(SegmentSensitivityPreviewer.preview(isiSec: trace([0.45, 0.46, 0.44]), segment: 1...2, gates: tiny))
    #expect(row(q, .contractLeft).verdict.isTooShort)      // [2...2] = 1 ISI → cannot compute CV2/LV
}

@Test
func boundaryClippingIsSafeAtTrainEdges() throws {
    let isi = trace([0.45, 0.46, 0.44, 0.45, 0.46, 0.45])   // valid ISI indices 1...6
    // Segment hugging both train edges: expansion cannot move outward.
    let p = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 1...6, gates: gates))
    #expect(row(p, .expandLeft).clippedAtBoundary)
    #expect(row(p, .expandRight).clippedAtBoundary)
    #expect(row(p, .expandBoth).clippedAtBoundary)
    // Clipped expansions equal the current span exactly.
    #expect(row(p, .expandLeft).startISIIndex == 1 && row(p, .expandLeft).endISIIndex == 6)
    // An out-of-range segment is clamped into the valid domain rather than crashing.
    let clamped = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: (-5)...100, gates: gates))
    #expect(clamped.baseStartISIIndex == 1 && clamped.baseEndISIIndex == 6)
}

@Test
func invalidOrEmptySegmentReturnsNil() {
    #expect(SegmentSensitivityPreviewer.preview(isiSec: [nil], segment: 0...0, gates: gates) == nil)
    #expect(SegmentSensitivityPreviewer.preview(isiSec: [], segment: 0...3, gates: gates) == nil)
    // A segment entirely outside the valid domain (all-nil/non-finite values).
    #expect(SegmentSensitivityPreviewer.preview(isiSec: [nil, nil, nil], segment: 1...2, gates: gates) == nil)
}

@Test
func verdictHonorsSuppliedGatesNotHardcodedDefaults() throws {
    // Same span, moderate CV (~0.35): FAILS strict CV≤0.30 but PASSES a loosened CV≤0.50. Pins that the verdict
    // is driven by the gates passed in — so the document must supply the user-edited detector gates, not defaults.
    let isi = trace([0.45, 0.45, 0.45, 0.45, 0.45, 0.90])
    let strict = TonicGateThresholds(cvMax: 0.30, cv2Max: 0.30, lvMax: 0.35, minSpikes: 5)
    let loose = TonicGateThresholds(cvMax: 0.50, cv2Max: 0.50, lvMax: 0.80, minSpikes: 5)
    let s = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 1...6, gates: strict))
    let l = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 1...6, gates: loose))
    #expect(!row(s, .current).verdict.passes)
    #expect(row(s, .current).verdict.failedMetric == "CV")
    #expect(row(l, .current).verdict.passes)
}

@Test
func previewIsPureAndDeterministic() throws {
    // No hidden state / side effects: identical inputs always yield identical output, and the input array is a
    // value type the previewer cannot mutate. (STPDCore also cannot reference RasterDocument, so there is no
    // detector/manual-threshold mutation path by construction.)
    let isi = trace([0.45, 0.44, 0.46, 0.45, 0.46, 0.44, 0.45])
    let a = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 2...6, gates: gates))
    let b = try #require(SegmentSensitivityPreviewer.preview(isiSec: isi, segment: 2...6, gates: gates))
    #expect(a == b)
    #expect(a.rows.count == SegmentSensitivityVariant.allCases.count)
}

// MARK: - SEG-PREVIEW-3: display filtering (hide uninformative / duplicate variants)

private func eightRegular() -> [Double?] {
    trace([0.45, 0.46, 0.44, 0.45, 0.46, 0.44, 0.45, 0.46])   // valid ISI indices 1...8
}

@Test
func leftBoundaryHidesLeftPlus1AndDuplicateBoth() throws {
    // Segment hugging the train start: Left +1 is clipped (== Current) and Both +1 collapses to Right +1.
    let p = try #require(SegmentSensitivityPreviewer.preview(isiSec: eightRegular(), segment: 1...5, gates: gates))
    let shown = p.displayedExpansionRows
    #expect(shown.map(\.variant) == [.current, .expandRight])
    #expect(shown.first?.variant == .current)                       // Current always visible
    #expect(shown.contains { $0.variant == .expandRight })
    #expect(!shown.contains { $0.variant == .expandLeft || $0.variant == .expandBoth })
}

@Test
func rightBoundaryHidesRightPlus1AndDuplicateBoth() throws {
    // Symmetric at the train end: Right +1 is clipped (== Current), Both +1 collapses to Left +1.
    let p = try #require(SegmentSensitivityPreviewer.preview(isiSec: eightRegular(), segment: 4...8, gates: gates))
    let shown = p.displayedExpansionRows
    #expect(shown.map(\.variant) == [.current, .expandLeft])
    #expect(!shown.contains { $0.variant == .expandRight || $0.variant == .expandBoth })
}

@Test
func middleSegmentShowsAllExpansionVariants() throws {
    // Well inside the train: every expansion produces a distinct new range, so all four rows are informative.
    let p = try #require(SegmentSensitivityPreviewer.preview(isiSec: eightRegular(), segment: 3...6, gates: gates))
    #expect(p.displayedExpansionRows.map(\.variant) == [.current, .expandLeft, .expandRight, .expandBoth])
    // ...and a middle segment's three contraction ranges are all distinct → none hidden.
    #expect(p.displayedContractionRows.count == 3)
}

@Test
func duplicateContractionRangesAreHiddenButTooShortKept() {
    // Two contraction rows sharing a range → the later duplicate is dropped; too-short rows are still kept.
    func mk(_ v: SegmentSensitivityVariant, _ s: Int, _ e: Int) -> SegmentSensitivityRow {
        SegmentSensitivityRow(
            variant: v, startISIIndex: s, endISIIndex: e, nISI: 0, nSpikes: 0,
            meanISISec: nil, cv: nil, cv2: nil, lv: nil, verdict: .tooShort,
            clippedAtBoundary: false, stability: .notComparable
        )
    }
    let rows = [mk(.contractLeft, 3, 5), mk(.contractRight, 3, 5), mk(.contractBoth, 4, 4)]
    let shown = SegmentSensitivityPreviewer.displayedContractionRows(rows)
    #expect(shown.map(\.variant) == [.contractLeft, .contractBoth])   // contractRight (dup of 3...5) hidden
    #expect(shown.allSatisfy { $0.verdict.isTooShort })               // too-short rows are kept
}

// MARK: - SEG-PREVIEW-4: one-line summary verdict

private let passV = SegmentTonicVerdict.pass
private let failV = SegmentTonicVerdict.fail(metric: "CV", value: 0.5, threshold: 0.3)
private let shortV = SegmentTonicVerdict.tooShort

private func sRow(_ v: SegmentSensitivityVariant, _ s: Int, _ e: Int,
                  _ verdict: SegmentTonicVerdict, _ stability: SegmentStability) -> SegmentSensitivityRow {
    SegmentSensitivityRow(variant: v, startISIIndex: s, endISIIndex: e, nISI: max(0, e - s + 1),
                          nSpikes: max(0, e - s + 2), meanISISec: 0.45, cv: 0.1, cv2: 0.1, lv: 0.05,
                          verdict: verdict, clippedAtBoundary: false, stability: stability)
}

/// Build a preview with distinct per-variant ranges (so the display filters keep every row) for summary tests.
private func summaryPreview(
    current: SegmentTonicVerdict,
    expansions: [(SegmentSensitivityVariant, SegmentTonicVerdict, SegmentStability)],
    contractions: [(SegmentSensitivityVariant, SegmentTonicVerdict, SegmentStability)]
) -> SegmentSensitivityPreview {
    let expRanges: [SegmentSensitivityVariant: (Int, Int)] = [.expandLeft: (2, 8), .expandRight: (3, 9), .expandBoth: (2, 9)]
    let conRanges: [SegmentSensitivityVariant: (Int, Int)] = [.contractLeft: (4, 8), .contractRight: (3, 7), .contractBoth: (4, 7)]
    var rows = [sRow(.current, 3, 8, current, .reference)]
    for (v, verd, stab) in expansions { let r = expRanges[v]!; rows.append(sRow(v, r.0, r.1, verd, stab)) }
    for (v, verd, stab) in contractions { let r = conRanges[v]!; rows.append(sRow(v, r.0, r.1, verd, stab)) }
    return SegmentSensitivityPreview(baseStartISIIndex: 3, baseEndISIIndex: 8, gates: gates, rows: rows)
}

@Test
func summaryStableTonicCore() {
    let p = summaryPreview(current: passV,
        expansions: [(.expandLeft, passV, .stable), (.expandRight, passV, .stable), (.expandBoth, passV, .stable)],
        contractions: [(.contractLeft, passV, .stable), (.contractRight, passV, .stable), (.contractBoth, passV, .stable)])
    #expect(p.summaryCategory == .stableTonicCore)
}

@Test
func summaryBoundarySensitive() {
    let p = summaryPreview(current: passV,
        expansions: [(.expandLeft, passV, .stable), (.expandRight, failV, .changed), (.expandBoth, failV, .changed)],
        contractions: [(.contractLeft, passV, .stable), (.contractRight, passV, .stable), (.contractBoth, passV, .stable)])
    #expect(p.summaryCategory == .boundarySensitive)
}

@Test
func summaryEdgeDependent() {
    let p = summaryPreview(current: passV,
        expansions: [(.expandLeft, passV, .stable), (.expandRight, passV, .stable), (.expandBoth, passV, .stable)],
        contractions: [(.contractLeft, failV, .changed), (.contractRight, passV, .stable), (.contractBoth, passV, .stable)])
    #expect(p.summaryCategory == .edgeDependent)
}

@Test
func summaryCurrentFailsButCorePasses() {
    let p = summaryPreview(current: failV,
        expansions: [(.expandLeft, failV, .stable), (.expandRight, failV, .stable), (.expandBoth, failV, .stable)],
        contractions: [(.contractLeft, passV, .changed), (.contractRight, failV, .stable), (.contractBoth, shortV, .notComparable)])
    #expect(p.summaryCategory == .currentFailsButCorePasses)
}

@Test
func summaryInsufficientCore() {
    let p = summaryPreview(current: passV,
        expansions: [(.expandLeft, passV, .stable), (.expandRight, passV, .stable), (.expandBoth, passV, .stable)],
        contractions: [(.contractLeft, shortV, .notComparable), (.contractRight, shortV, .notComparable), (.contractBoth, shortV, .notComparable)])
    #expect(p.summaryCategory == .insufficientCore)
}

@Test
func summaryNotTonicCompatible() {
    let p = summaryPreview(current: failV,
        expansions: [(.expandLeft, failV, .stable), (.expandRight, failV, .stable), (.expandBoth, failV, .stable)],
        contractions: [(.contractLeft, failV, .stable), (.contractRight, failV, .stable), (.contractBoth, shortV, .notComparable)])
    #expect(p.summaryCategory == .notTonicCompatible)
}

@Test
func summaryFromRealPreviews() throws {
    // Cross-check the classifier end-to-end on real previews from SEG-PREVIEW-1's scenarios.
    let stable = trace([0.45, 0.44, 0.46, 0.45, 0.46, 0.44, 0.45, 0.46, 0.45, 0.44, 0.46, 0.45])
    #expect(try #require(SegmentSensitivityPreviewer.preview(isiSec: stable, segment: 4...9, gates: gates)).summaryCategory == .stableTonicCore)

    let outlier = trace([0.45, 0.44, 0.46, 0.45, 0.46, 0.45, 2.00])
    #expect(try #require(SegmentSensitivityPreviewer.preview(isiSec: outlier, segment: 1...6, gates: gates)).summaryCategory == .boundarySensitive)

    let edgeOutlier = trace([1.50, 0.45, 0.46, 0.44, 0.45, 0.46, 0.45])
    #expect(try #require(SegmentSensitivityPreviewer.preview(isiSec: edgeOutlier, segment: 1...7, gates: gates)).summaryCategory == .currentFailsButCorePasses)
}

// MARK: - SEG-PREVIEW-2: covering-tonic-candidate lookup (real pipeline candidates, no manual construction)

private func runFor(times: [Double]) throws -> (SpikeDataset, ClassicAnchorDetectionRun) {
    let csv = (["t"] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    let ds = try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "seg2", sourceDescription: "seg2")
    return (ds, ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings()))
}

@Test
func coveringSelectedTonicCandidateFindsTheTonicSpan() throws {
    var times = [0.0]; var t = 0.0
    let cyc = [0.45, 0.46, 0.44]
    for i in 0..<29 { t += cyc[i % 3]; times.append(t) }     // 30 regular spikes → a tonic candidate
    let (ds, run) = try runFor(times: times)
    let cands = run.result(for: ds.trains[0].id)?.candidates ?? []
    let tonic = try #require(cands.first { $0.finalLabel == .tonic && $0.selectedForAuto })

    let mid = (tonic.startISIIndex + tonic.endISIIndex) / 2
    let found = try #require(SegmentSensitivityPreviewer.coveringSelectedTonicCandidate(isiIndex: mid, candidates: cands))
    #expect(found.finalLabel == .tonic && found.selectedForAuto)
    // Outside any span and an empty candidate list both resolve to nil.
    #expect(SegmentSensitivityPreviewer.coveringSelectedTonicCandidate(isiIndex: 99_999, candidates: cands) == nil)
    #expect(SegmentSensitivityPreviewer.coveringSelectedTonicCandidate(isiIndex: mid, candidates: []) == nil)
}

@Test
func coveringSelectedTonicCandidateRejectsNonTonicSpans() throws {
    var times = [0.0]; var t = 0.0
    let cyc = [0.45, 0.46, 0.44]
    for i in 0..<15 { t += cyc[i % 3]; times.append(t) }     // tonic
    for _ in 0..<6 { t += 0.008; times.append(t) }            // a fast burst packet
    for i in 0..<15 { t += cyc[i % 3]; times.append(t) }     // tonic
    let (ds, run) = try runFor(times: times)
    let cands = run.result(for: ds.trains[0].id)?.candidates ?? []

    // An ISI inside a selected BURST candidate is not inside any tonic candidate → nil.
    let burst = try #require(cands.first { $0.finalLabel.isBurstEventFamily && $0.selectedForAuto })
    let midBurst = (burst.startISIIndex + burst.endISIIndex) / 2
    #expect(SegmentSensitivityPreviewer.coveringSelectedTonicCandidate(isiIndex: midBurst, candidates: cands) == nil)

    // ...but a tonic stretch still resolves to its tonic candidate.
    let tonic = try #require(cands.first { $0.finalLabel == .tonic && $0.selectedForAuto })
    let midTonic = (tonic.startISIIndex + tonic.endISIIndex) / 2
    #expect(SegmentSensitivityPreviewer.coveringSelectedTonicCandidate(isiIndex: midTonic, candidates: cands) != nil)
}
