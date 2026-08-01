import Foundation
import STPDCore
import Testing

// Phase 2A: R-compatible ISI state-space feature rows (mirror of stpd_make_isi_state_space_features).
// Identity fields are two distinct things: `rowNumber` is R `row_number` (the right spike's 1-based
// position in the sorted/chronological sequence, not a source identity), while `idx`/`leftIdx`/`rightIdx`
// are source identity values (`SpikeTrain.inputOrderIndices` by default, sequential otherwise). QC reuses
// the shared Mac SpikeISITrace tolerance rules (duplicate > artifact > refractory > ok).

private func train(_ name: String, _ timestamps: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: timestamps)
}

// Spikes 0, 0.010, 0.020, 0.040, 0.080 -> ISI Swift j=1..4 = 0.010, 0.010, 0.020, 0.040.
// R-visible idx (= j+1) for those rows is 2, 3, 4, 5.
private let cleanTrain = train("g", [0, 0.010, 0.020, 0.040, 0.080])

private func rows(
    _ t: SpikeTrain,
    k: Int = 3,
    winsorize: Bool = false,
    minValid: Double = 0.001,
    qc: SpikeQualitySettings = SpikeQualitySettings()
) -> [SpikeISIStateSpaceRow] {
    SpikeISIStateSpaceFeatureBuilder.makeRows(
        train: t,
        k: k,
        minValidISISec: minValid,
        winsorize: winsorize,
        qualitySettings: qc
    )
}

private func row(_ rows: [SpikeISIStateSpaceRow], idx: Int) -> SpikeISIStateSpaceRow? {
    rows.first { $0.idx == idx }
}

// MARK: - 1. Row count + R-visible 1-based identity (one row per valid ISI).

@Test
func oneRowPerValidISIWithRVisibleIdentity() {
    let result = rows(cleanTrain)
    #expect(result.count == 4)                          // ISI Swift j = 1..4
    #expect(result.map(\.idx) == [2, 3, 4, 5])          // R idx = j + 1
    #expect(result.map(\.rowNumber) == [2, 3, 4, 5])    // R row_number = j + 1
    #expect(result.map(\.leftIdx) == [1, 2, 3, 4])      // R left_idx = j
    #expect(result.map(\.rightIdx) == [2, 3, 4, 5])     // R right_idx = j + 1
    let first = result[0]
    #expect(first.leftIdx == 1 && first.rightIdx == 2)
    #expect(first.leftTimeSec == 0 && first.rightTimeSec == 0.010)
    #expect(abs(first.timeMidSec - 0.005) < 1e-12)
    #expect(abs(first.isiSec - 0.010) < 1e-12)
    let last = result[3]
    #expect(last.leftIdx == 4 && last.rightIdx == 5)
    #expect(abs(last.isiSec - 0.040) < 1e-12)
}

// RED-first: under the old sequential-ordinal semantics the first produced row reset to
// rowNumber == 1; R `row_number` preserves the right spike's chronological position across the skipped
// invalid first ISI (it does not reset to the produced-row counter after filtering invalid ISIs).
@Test
func belowMinValidISIsAreExcludedAndDoNotResetRowNumber() {
    // ISI Swift j=1 = 0.0005 (< minValid) is excluded; valid rows are Swift j = 2, 3.
    let result = rows(train("a", [0, 0.0005, 0.010, 0.020]))
    #expect(result.count == 2)
    #expect(result.map(\.idx) == [3, 4])                // R idx = source identity (sequential here = j + 1), NOT [2, 3]
    #expect(result.map(\.rowNumber) == [3, 4])          // R row_number = chronological position, NOT [1, 2]
    #expect(result[0].rowNumber == 3)                   // first produced row is row_number 3, not 1
    #expect(result.map(\.leftIdx) == [2, 3])
    #expect(result.map(\.rightIdx) == [3, 4])
}

// MARK: - 1b. P1: non-sequential source-index R-visible identity (idx / left_idx / right_idx).

// A normal chronological train (inputOrderIndices == [1...n]) is unaffected by the source-index
// support: idx/leftIdx/rightIdx/rowNumber keep the sequential convention (no regression).
@Test
func sequentialTrainIdentityUnchangedUnderIndexSourceSupport() {
    #expect(cleanTrain.inputOrderIndices == [1, 2, 3, 4, 5])
    let result = rows(cleanTrain)
    #expect(result.map(\.idx) == [2, 3, 4, 5])
    #expect(result.map(\.leftIdx) == [1, 2, 3, 4])
    #expect(result.map(\.rightIdx) == [2, 3, 4, 5])
    #expect(result.map(\.rowNumber) == [2, 3, 4, 5])
}

// Non-chronological input file: rows (1:0.030, 2:0.010, 3:0.020, 4:0.040). `SpikeTrain` sorts by
// time -> timestamps [0.010,0.020,0.030,0.040] with inputOrderIndices (original 1-based rows) =
// [2,3,1,4]. R `stpd_make_isi_state_space_features` reports the source `idx` (it sorts dat by idx),
// so idx/leftIdx/rightIdx must follow that permutation. ISI stays CHRONOLOGICAL (Mac never reorders
// timestamps), so all ISIs are 0.010; `rowNumber` is the chronological 1-based position (== R
// `row_number` exactly when the source idx order matches chronological order, which is the common
// case). This is the documented Mac-vs-R ordering choice.
@Test
func nonChronologicalInputReportsSourceIdentityWithChronologicalISI() {
    let t = train("perm", [0.030, 0.010, 0.020, 0.040])
    #expect(t.inputOrderIndices == [2, 3, 1, 4])        // precondition: source identity permutation
    let result = rows(t, k: 1)                           // default source = train.inputOrderIndices
    #expect(result.count == 3)
    #expect(result.map(\.idx) == [3, 1, 4])             // identity[j] for j = 1...3
    #expect(result.map(\.leftIdx) == [2, 3, 1])         // identity[j-1]
    #expect(result.map(\.rightIdx) == [3, 1, 4])        // identity[j]
    #expect(result.map(\.rowNumber) == [2, 3, 4])       // chronological position, NOT the permutation
    #expect(result.allSatisfy { abs($0.isiSec - 0.010) < 1e-12 })   // ISI not scrambled by source order
}

// An explicit per-spike source-index array overrides the default; ISI remains chronological.
@Test
func explicitSourceIndicesDriveRVisibleIdentity() {
    let result = SpikeISIStateSpaceFeatureBuilder.makeRows(
        train: cleanTrain, k: 3, winsorize: false, sourceIndices: [10, 20, 30, 40, 50]
    )
    #expect(result.count == 4)
    #expect(result.map(\.idx) == [20, 30, 40, 50])      // identity[j]
    #expect(result.map(\.leftIdx) == [10, 20, 30, 40])  // identity[j-1]
    #expect(result.map(\.rightIdx) == [20, 30, 40, 50])
    #expect(result.map(\.rowNumber) == [2, 3, 4, 5])    // chronological position, unchanged
    #expect(zip(result.map(\.isiSec), [0.010, 0.010, 0.020, 0.040]).allSatisfy { abs($0 - $1) < 1e-12 })
}

// A wrong-length explicit source index is ignored (falls back to inputOrderIndices), never crashes.
@Test
func mismatchedSourceIndexLengthFallsBackToInputOrder() {
    let result = SpikeISIStateSpaceFeatureBuilder.makeRows(
        train: cleanTrain, k: 3, winsorize: false, sourceIndices: [1, 2]   // wrong length
    )
    #expect(result.map(\.idx) == [2, 3, 4, 5])          // == inputOrderIndices-derived sequential
}

// MARK: - 1c. P1: min-ISI floor (R default min_isi_sec = 0.001) inclusion boundary.

// The builder gate is `isi >= minValid` (inclusive at the floor). The live `ISIStateSpaceView` now
// pins this to R's default 0.001; this pins the boundary semantics the view relies on.
@Test
func isiAtMinFloorIsIncludedJustBelowIsExcluded() {
    // [0, 0.001, 0.0019]: ISI j=1 = 0.001 (== floor -> included), j=2 = 0.0009 (< floor -> excluded).
    let result = rows(train("floor", [0, 0.001, 0.0019]), k: 1, minValid: 0.001)
    #expect(result.count == 1)
    #expect(result[0].idx == 2)                          // R idx = j+1 (j=1) under sequential idx
    #expect(abs(result[0].isiSec - 0.001) < 1e-12)       // the exactly-at-floor ISI is kept
}

// The shared default floor (used by the document setting, live view, and builder) is R's 0.001 s.
@Test
func defaultMinValidISISecIsRCompatible() {
    #expect(SpikeISIStateSpaceFeatureBuilder.defaultMinValidISISec == 0.001)
}

// Guardrail: the UI min-ISI floor clamps to a tiny positive minimum (never exactly 0); normal values
// pass through and the R default is unchanged.
@Test
func clampedMinValidISISecEnforcesTinyPositiveFloor() {
    let lowerBound = SpikeISIStateSpaceFeatureBuilder.minValidISISecLowerBound
    #expect(lowerBound > 0)
    #expect(SpikeISIStateSpaceFeatureBuilder.clampedMinValidISISec(0) == lowerBound)   // 0 -> tiny positive
    #expect(SpikeISIStateSpaceFeatureBuilder.clampedMinValidISISec(-1) == lowerBound)  // negative -> tiny positive
    #expect(SpikeISIStateSpaceFeatureBuilder.clampedMinValidISISec(0.001) == 0.001)    // normal value passes through
    #expect(SpikeISIStateSpaceFeatureBuilder.defaultMinValidISISec == 0.001)           // default unchanged
    // Non-finite input falls back to the R default rather than producing NaN/0.
    #expect(SpikeISIStateSpaceFeatureBuilder.clampedMinValidISISec(.nan)
        == SpikeISIStateSpaceFeatureBuilder.defaultMinValidISISec)
}

// The exposed state-space min-ISI floor is a real seam: raising it excludes shorter ISIs the R default
// would keep. [0, 0.002, 0.0035]: ISI j=1 = 0.002, j=2 = 0.0015.
@Test
func customMinFloorChangesInclusion() {
    let raised = rows(train("cf", [0, 0.002, 0.0035]), k: 1, minValid: 0.002)
    #expect(raised.count == 1)                           // 0.0015 excluded at floor 0.002
    #expect(abs(raised[0].isiSec - 0.002) < 1e-12)       // the exactly-at-floor ISI is kept
    let atDefault = rows(
        train("cf", [0, 0.002, 0.0035]), k: 1,
        minValid: SpikeISIStateSpaceFeatureBuilder.defaultMinValidISISec
    )
    #expect(atDefault.count == 2)                        // both ISIs kept at the R default 0.001
}

// MARK: - 2. Lag naming + index semantics.

@Test
func lagNamingAndOffsetSemantics() {
    #expect(SpikeISIStateSpaceFeatureBuilder.lagName(forOffset: -1) == "lag_m1")
    #expect(SpikeISIStateSpaceFeatureBuilder.lagName(forOffset: 0) == "lag_0")
    #expect(SpikeISIStateSpaceFeatureBuilder.lagName(forOffset: 2) == "lag_p2")

    let r = try! #require(row(rows(cleanTrain), idx: 3))   // R idx 3 == Swift j=2; no winsor => log feature == log10(ISI)
    #expect(r.lag(0) == r.logISIFeature)
    #expect(abs((r.lag(-1) ?? 0) - (-2.0)) < 1e-9)         // ISI at j=1 = 0.010 -> log10 = -2
    #expect(abs((r.lag(1) ?? 0) - log10(0.020)) < 1e-9)    // ISI at j=3 = 0.020
    #expect(abs((r.lag(2) ?? 0) - log10(0.040)) < 1e-9)    // ISI at j=4 = 0.040
    #expect(r.lag(-2) == nil)                              // j=0 has no ISI
    #expect(r.lag(3) == nil)                               // j=5 is out of range
}

// MARK: - 3. Local CV / CV2 / LV (canonical STPDStatistics semantics).

@Test
func localCVCV2LVMatchCanonicalSemantics() {
    // k=1 window for Swift j=2 (R idx 3) -> ISI [0.010, 0.010, 0.020].
    let r = try! #require(row(rows(cleanTrain, k: 1), idx: 3))
    // sample CV (n-1): mean 0.0133..., sd 0.005774 -> 0.4330.
    #expect(abs((r.localCV ?? 0) - 0.4330127) < 1e-5)
    // LV: mean(0, 3*(0.01^2)/(0.03^2)) = mean(0, 0.33333) = 0.16667.
    #expect(abs((r.localLV ?? 0) - (1.0 / 6.0)) < 1e-6)
    // CV2: mean(0, 2*0.010/0.030) = mean(0, 0.66667) = 0.33333.
    #expect(abs((r.localCV2 ?? 0) - (1.0 / 3.0)) < 1e-6)
    // local rate = 1 / median(ISI) = 1 / 0.010 = 100 Hz.
    #expect(abs((r.localRateHz ?? 0) - 100.0) < 1e-6)
}

// MARK: - 4. Winsorization defaults (0.01 / 0.99).

@Test
func winsorizationClampsOutlierLogFeature() {
    // ISIs 0.010 x4 then a 1.0 s outlier (logs [-2,-2,-2,-2, 0]); outlier is Swift j=5 -> R idx 6.
    let t = train("w", [0, 0.010, 0.020, 0.030, 0.040, 1.040])
    let outlierRIdx = 6
    let withWinsor = try! #require(row(rows(t, winsorize: true), idx: outlierRIdx))
    #expect(abs((withWinsor.logISI ?? 0) - 0.0) < 1e-9)             // log10(1.0) = 0
    // q99 of [-2,-2,-2,-2,0] = -0.08 -> the outlier is clamped down to it.
    #expect(abs((withWinsor.logISIFeature ?? 0) - (-0.08)) < 1e-9)
    let withoutWinsor = try! #require(row(rows(t, winsorize: false), idx: outlierRIdx))
    #expect(abs((withoutWinsor.logISIFeature ?? 0) - 0.0) < 1e-9)   // unchanged
}

// MARK: - 5. prepostRatio.

@Test
func prepostRatioIsMedianOfFlankOverCurrent() {
    // Swift j=2 (R idx 3): prev ISI 0.010, next ISI 0.020 -> median 0.015; current 0.010 -> 1.5.
    let r = try! #require(row(rows(cleanTrain), idx: 3))
    #expect(abs((r.prepostRatio ?? 0) - 1.5) < 1e-9)
}

// MARK: - 6. delta / nextDelta of the log feature.

@Test
func deltaAndNextDeltaUsePreviousAndNextLogFeature() {
    let result = rows(cleanTrain)   // winsor off: log feature == log10(ISI)
    // First ISI row (Swift j=1, R idx 2) has no previous ISI -> delta nil.
    #expect(row(result, idx: 2)?.deltaLogISI == nil)
    // Swift j=2 (R idx 3): log10(0.010) - log10(0.010) = 0.
    #expect(abs((row(result, idx: 3)?.deltaLogISI ?? .nan) - 0.0) < 1e-9)
    // Swift j=3 (R idx 4): log10(0.020) - log10(0.010) = 0.30103.
    #expect(abs((row(result, idx: 4)?.deltaLogISI ?? 0) - (log10(0.020) - log10(0.010))) < 1e-9)
    // nextDelta at Swift j=2 (R idx 3) = log10(0.020) - log10(0.010).
    #expect(abs((row(result, idx: 3)?.nextDeltaLogISI ?? 0) - (log10(0.020) - log10(0.010))) < 1e-9)
    // Last ISI row (Swift j=4, R idx 5) has no next ISI -> nextDelta nil.
    #expect(row(result, idx: 5)?.nextDeltaLogISI == nil)
}

// MARK: - 7. QC status (shared SpikeISITrace semantics) + k clamp.

@Test
func refractoryRowsCarryQCStatusAndKClamp() {
    // artifact 0.9 ms, refractory 12 ms: cleanTrain ISIs 0.010 -> refractory, 0.020 -> ok.
    let qc = SpikeQualitySettings(artifactThresholdMilliseconds: 0.9, refractorySuspectThresholdMilliseconds: 12.0)
    let result = rows(cleanTrain, qc: qc)
    let refractoryRow = try! #require(row(result, idx: 2))   // Swift j=1, ISI 0.010
    #expect(refractoryRow.qcStatus == .refractory)
    #expect(refractoryRow.isRefractorySuspect == true)
    let okRow = try! #require(row(result, idx: 4))           // Swift j=3, ISI 0.020
    #expect(okRow.qcStatus == .ok)
    #expect(okRow.isRefractorySuspect == false)

    // k is clamped to 1...10: k=0 behaves like k=1 (a -1 lag still exists).
    let kZero = SpikeISIStateSpaceFeatureBuilder.makeRows(train: cleanTrain, k: 0, winsorize: false)
    #expect(kZero.count == 4)
    #expect(row(kZero, idx: 3)?.lag(-1) != nil)             // Swift j=2 has a -1 lag at k>=1
}

@Test
func sharedQCClassifierPrecedenceAndBoundaries() {
    // Direct test of the shared classifier the row builder reuses.
    let qc = SpikeQualitySettings(artifactThresholdMilliseconds: 1.0, refractorySuspectThresholdMilliseconds: 2.0)
    #expect(SpikeISITrace.qcStatus(forISISec: 0.0, settings: qc) == .duplicate)     // zero-length ISI
    #expect(SpikeISITrace.qcStatus(forISISec: 0.0005, settings: qc) == .artifact)   // < 1 ms
    #expect(SpikeISITrace.qcStatus(forISISec: 0.0015, settings: qc) == .refractory) // [1 ms, 2 ms)
    #expect(SpikeISITrace.qcStatus(forISISec: 0.002, settings: qc) == .ok)          // == 2 ms boundary -> NOT refractory
    #expect(SpikeISITrace.qcStatus(forISISec: 0.010, settings: qc) == .ok)
}

@Test
func rowQCStatusFollowsArtifactPrecedenceWhenIncluded() {
    // A tiny min-valid keeps a sub-artifact ISI as a row; its QC must be artifact (precedence over ok).
    let qc = SpikeQualitySettings(artifactThresholdMilliseconds: 1.0, refractorySuspectThresholdMilliseconds: 2.0)
    let t = train("p", [0, 0.0005, 0.010])  // Swift j=1 ISI 0.0005 (< 1 ms), j=2 ISI 0.0095
    let result = SpikeISIStateSpaceFeatureBuilder.makeRows(
        train: t, k: 1, minValidISISec: 1e-9, winsorize: false, qualitySettings: qc
    )
    #expect(result.count == 2)
    #expect(row(result, idx: 2)?.qcStatus == .artifact)   // Swift j=1
    #expect(row(result, idx: 3)?.qcStatus == .ok)         // Swift j=2 (0.0095 > 2 ms)
}

@Test
func shortTrainsProduceNoRows() {
    #expect(SpikeISIStateSpaceFeatureBuilder.makeRows(train: train("s", [0, 0.01]), winsorize: false).isEmpty)
}
