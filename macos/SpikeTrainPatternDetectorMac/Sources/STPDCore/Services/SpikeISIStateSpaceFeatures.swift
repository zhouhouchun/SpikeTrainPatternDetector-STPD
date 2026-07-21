import Foundation

/// One R-compatible ISI state-space feature row — one row per VALID ISI (not per adjacent pair),
/// mirroring R's `stpd_make_isi_state_space_features` (`R/55_state_space_pca.R`). All numeric features
/// are label-free (the optional `label` is a color overlay only and never participates in any
/// coordinate/feature computation). No MM; no fixed-millisecond biological assumption.
///
/// ISI-index convention (matches `SpikeTrain.isiSec`): ISI index `i` is the interval
/// `[timestampsSec[i-1], timestampsSec[i]]`, valid for `i` in `1..<count`.
public struct SpikeISIStateSpaceRow: Hashable, Sendable {
    // Identity / time. These mirror R's `row_number` / `idx` / `left_idx` / `right_idx`, which are two
    // distinct things: `rowNumber` is a POSITION, while `idx` / `leftIdx` / `rightIdx` are SOURCE
    // IDENTITY values.
    //   - `rowNumber` = R `row_number`: the 1-based position of the right spike in the
    //     sorted/chronological spike sequence. It preserves that position across skipped invalid ISIs,
    //     so it is NOT a compact produced-row counter (and it is NOT a source/data-row identity).
    //   - `idx` / `leftIdx` / `rightIdx` = the per-spike source identity (`SpikeTrain.inputOrderIndices`
    //     by default, sequential `1...n` otherwise).
    // For the ISI between Swift `timestamps[j-1]` and `timestamps[j]`: `rowNumber = j + 1`,
    // `idx = rightIdx = identity[j]`, `leftIdx = identity[j-1]`. For an ordinary sequential train the
    // source identity is `[1...n]`, so `rowNumber = idx = rightIdx = j + 1` and `leftIdx = j`; for a
    // non-chronological input file the idx columns report the true original source rows while ISI stays
    // chronological. See `SpikeISIStateSpaceFeatureBuilder.makeRows(sourceIndices:)`.
    public let trainID: String
    public let trainName: String
    public let rowNumber: Int        // R `row_number` (1-based right-spike position, not a source identity)
    public let idx: Int              // R `idx` (right-spike source identity; always == rightIdx)
    public let leftIdx: Int          // R `left_idx` (left-spike source identity)
    public let rightIdx: Int         // R `right_idx` (right-spike source identity)
    public let leftTimeSec: Double
    public let rightTimeSec: Double
    public let timeMidSec: Double
    public let isiSec: Double

    // Log.
    public let logISI: Double?         // log10(ISI)
    public let logISIFeature: Double?  // winsorized log10(ISI) (== logISI when winsorization is off/insufficient)

    // Lag context: winsorized log feature at offsets -k...+k (only finite offsets present).
    public let lagLogFeature: [Int: Double]

    // Local context (window of ISI rows i-k...i+k).
    public let localMedianLogISI: Double?
    public let localMeanLogISI: Double?
    public let localSDLogISI: Double?
    public let localQ10LogISI: Double?
    public let localQ90LogISI: Double?
    public let localIQRLogISI: Double?
    public let localMeanISISec: Double?
    public let localMedianISISec: Double?
    public let localRateHz: Double?
    public let localCV: Double?
    public let localLV: Double?
    public let localCV2: Double?
    public let deltaLogISI: Double?
    public let nextDeltaLogISI: Double?
    public let prepostRatio: Double?

    // QC. Shared Mac semantics (duplicate > artifact > refractory > ok), derived from the same
    // tolerance helpers that produce the per-event flags on `SpikeISITrace` — see
    // `SpikeISITrace.qcStatus(forISISec:settings:)`. Not from R: `stpd_make_isi_state_space_features`
    // emits no QC column, so the Mac's own QC rules are the source of truth here.
    public let qcStatus: SpikeISIStatePointQC

    /// Backward-compatible convenience: true only when the shared QC status is refractory-suspect.
    public var isRefractorySuspect: Bool { qcStatus == .refractory }

    // Overlay only (never a feature).
    public let label: String?

    public init(
        trainID: String, trainName: String, rowNumber: Int, idx: Int, leftIdx: Int, rightIdx: Int,
        leftTimeSec: Double, rightTimeSec: Double, timeMidSec: Double, isiSec: Double,
        logISI: Double?, logISIFeature: Double?, lagLogFeature: [Int: Double],
        localMedianLogISI: Double?, localMeanLogISI: Double?, localSDLogISI: Double?,
        localQ10LogISI: Double?, localQ90LogISI: Double?, localIQRLogISI: Double?,
        localMeanISISec: Double?, localMedianISISec: Double?, localRateHz: Double?,
        localCV: Double?, localLV: Double?, localCV2: Double?,
        deltaLogISI: Double?, nextDeltaLogISI: Double?, prepostRatio: Double?,
        qcStatus: SpikeISIStatePointQC, label: String?
    ) {
        self.trainID = trainID; self.trainName = trainName; self.rowNumber = rowNumber
        self.idx = idx; self.leftIdx = leftIdx; self.rightIdx = rightIdx
        self.leftTimeSec = leftTimeSec; self.rightTimeSec = rightTimeSec; self.timeMidSec = timeMidSec
        self.isiSec = isiSec
        self.logISI = logISI; self.logISIFeature = logISIFeature; self.lagLogFeature = lagLogFeature
        self.localMedianLogISI = localMedianLogISI; self.localMeanLogISI = localMeanLogISI
        self.localSDLogISI = localSDLogISI; self.localQ10LogISI = localQ10LogISI
        self.localQ90LogISI = localQ90LogISI; self.localIQRLogISI = localIQRLogISI
        self.localMeanISISec = localMeanISISec; self.localMedianISISec = localMedianISISec
        self.localRateHz = localRateHz; self.localCV = localCV; self.localLV = localLV
        self.localCV2 = localCV2; self.deltaLogISI = deltaLogISI; self.nextDeltaLogISI = nextDeltaLogISI
        self.prepostRatio = prepostRatio; self.qcStatus = qcStatus; self.label = label
    }

    public func lag(_ offset: Int) -> Double? { lagLogFeature[offset] }
}

/// Pure builder for R-compatible ISI state-space feature rows. Mirrors
/// `stpd_make_isi_state_space_features`: log10 ISI, train-wide winsorization at `0.01/0.99`, lag
/// offsets `-k...+k` on the winsorized log feature, local windows `i-k...i+k`, sample CV / canonical
/// CV2 / LV from `STPDStatistics`, delta/next-delta of the log feature, and prepost ratio.
public enum SpikeISIStateSpaceFeatureBuilder {
    public static let defaultWinsorLow = 0.01
    public static let defaultWinsorHigh = 0.99
    /// R `stpd_make_isi_state_space_features` validity floor (`min_isi_sec`) default, in seconds.
    /// Shared so the document setting, the live view, and this builder agree on one R-compatible value.
    public static let defaultMinValidISISec = 0.001

    /// Lower bound for the user-exposed state-space min-ISI floor: a tiny positive value so the stored
    /// floor is never exactly 0 (a 0 floor would admit zero-length / duplicate-timestamp ISIs). This is a
    /// UI-setting guardrail only; it does not change the builder's own computation.
    public static let minValidISISecLowerBound = 1e-9

    /// Clamp a user-entered state-space min-ISI floor (seconds) to a tiny positive minimum. Zero and
    /// negative inputs clamp up to `minValidISISecLowerBound`; non-finite input falls back to the R
    /// default. Used by the `ISIStateSpaceView` min-ISI control so the stored value stays > 0.
    public static func clampedMinValidISISec(_ seconds: Double) -> Double {
        guard seconds.isFinite else { return defaultMinValidISISec }
        return max(minValidISISecLowerBound, seconds)
    }

    /// R-style lag column name for an offset (`lag_m1`, `lag_0`, `lag_p2`, ...).
    public static func lagName(forOffset offset: Int) -> String {
        if offset < 0 { return "lag_m\(abs(offset))" }
        if offset > 0 { return "lag_p\(offset)" }
        return "lag_0"
    }

    /// - Parameter sourceIndices: optional per-spike R-visible identity, aligned with
    ///   `train.timestampsSec` (one entry per spike). When `nil` it defaults to
    ///   `train.inputOrderIndices` (each spike's original 1-based input row). R
    ///   `stpd_make_isi_state_space_features` sorts `dat` by its `idx` column and reports those idx
    ///   values; Mac always sorts spikes chronologically and records each spike's source row, so we
    ///   route the identity columns (`idx`/`leftIdx`/`rightIdx`) through that source index. This keeps
    ///   ISI computation CHRONOLOGICAL (ISI values never change) while reporting true source-row
    ///   identity for a non-chronological input file. For an ordinary sequential train
    ///   `inputOrderIndices == [1...n]`, so the emitted identity is unchanged (`idx == j+1`, `leftIdx
    ///   == j`). `rowNumber` stays the chronological 1-based position (R `row_number`), which equals
    ///   R's value exactly when the source idx order matches chronological order (the common case).
    public static func makeRows(
        train: SpikeTrain,
        k: Int = 3,
        minValidISISec: Double = defaultMinValidISISec,
        winsorize: Bool = true,
        winsorLow: Double = defaultWinsorLow,
        winsorHigh: Double = defaultWinsorHigh,
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        labelByISI: [Int: String] = [:],
        sourceIndices: [Int]? = nil
    ) -> [SpikeISIStateSpaceRow] {
        let timestamps = train.timestampsSec
        let n = timestamps.count
        guard n >= 3 else { return [] }
        let clampedK = max(1, min(10, k))
        let minValid = minValidISISec.isFinite ? minValidISISec : defaultMinValidISISec
        let isi = train.isiSec   // [Double?], isi[0] == nil; isi[i] = timestamps[i] - timestamps[i-1]

        // R-visible per-spike identity (0-based array aligned with `timestamps`): explicit override,
        // else the train's recorded source rows, else a sequential 1-based fallback. A wrong-length
        // override is ignored so callers can never desync identity from spikes.
        let identity: [Int]
        if let sourceIndices, sourceIndices.count == n {
            identity = sourceIndices
        } else if train.inputOrderIndices.count == n {
            identity = train.inputOrderIndices
        } else {
            identity = Array(1...n)
        }

        func isValidISI(_ index: Int) -> Double? {
            guard timestamps.indices.contains(index), index >= 1,
                  let value = isi[index], value.isFinite, value >= minValid else { return nil }
            return value
        }

        // log10(ISI) for valid ISIs, then train-wide winsorization -> the log feature.
        var logISIArr: [Double?] = Array(repeating: nil, count: n)
        for index in 1..<n {
            if let value = isValidISI(index) { logISIArr[index] = log10(value) }
        }
        let logFeatureArr = winsorize ? winsorized(logISIArr, low: winsorLow, high: winsorHigh) : logISIArr

        var rows: [SpikeISIStateSpaceRow] = []
        for j in 1..<n {
            guard let isiVal = isValidISI(j), timestamps[j].isFinite, timestamps[j - 1].isFinite else { continue }
            let leftTime = timestamps[j - 1]
            let rightTime = timestamps[j]

            var lagDict: [Int: Double] = [:]
            for offset in -clampedK...clampedK {
                let pos = j + offset
                if pos >= 1, pos < n, let value = logFeatureArr[pos] { lagDict[offset] = value }
            }

            let windowLow = max(1, j - clampedK)
            let windowHigh = min(n - 1, j + clampedK)
            var localISIs: [Double] = []
            var localLogs: [Double] = []
            for w in windowLow...windowHigh {
                if let value = isValidISI(w) { localISIs.append(value) }
                if let logValue = logFeatureArr[w] { localLogs.append(logValue) }
            }

            var flank: [Double] = []
            if let prev = isValidISI(j - 1) { flank.append(prev) }
            if let next = isValidISI(j + 1) { flank.append(next) }

            let localMedianISI = median(localISIs)
            let deltaLog: Double? = (j > 1 && logFeatureArr[j - 1] != nil && logFeatureArr[j] != nil)
                ? logFeatureArr[j]! - logFeatureArr[j - 1]! : nil
            let nextDeltaLog: Double? = (j + 1 < n && logFeatureArr[j + 1] != nil && logFeatureArr[j] != nil)
                ? logFeatureArr[j + 1]! - logFeatureArr[j]! : nil
            let prepost: Double? = (!flank.isEmpty && isiVal > 0) ? (median(flank)! / isiVal) : nil

            // R-visible identity: right spike is `timestamps[j]` (source `identity[j]`), left spike is
            // `timestamps[j-1]` (source `identity[j-1]`). `rowNumber` is the chronological position.
            // `localCV`/`localLV`/`localCV2` use the canonical sample statistics from `STPDStatistics`
            // (`localCV` is sample sd (n-1) / mean, == R `stats::sd / mean` for positive ISIs).
            rows.append(SpikeISIStateSpaceRow(
                trainID: train.id,
                trainName: train.name,
                rowNumber: j + 1,
                idx: identity[j],
                leftIdx: identity[j - 1],
                rightIdx: identity[j],
                leftTimeSec: leftTime,
                rightTimeSec: rightTime,
                timeMidSec: (leftTime + rightTime) / 2,
                isiSec: isiVal,
                logISI: logISIArr[j],
                logISIFeature: logFeatureArr[j],
                lagLogFeature: lagDict,
                localMedianLogISI: median(localLogs),
                localMeanLogISI: STPDStatistics.mean(localLogs),
                localSDLogISI: localLogs.count >= 2 ? sampleStandardDeviation(localLogs) : nil,
                localQ10LogISI: SortedFiniteSample(localLogs).quantile(0.10),
                localQ90LogISI: SortedFiniteSample(localLogs).quantile(0.90),
                localIQRLogISI: interquartileRange(localLogs),
                localMeanISISec: STPDStatistics.mean(localISIs),
                localMedianISISec: localMedianISI,
                localRateHz: (localMedianISI.flatMap { $0 > 0 ? 1.0 / $0 : nil }),
                localCV: localISIs.count >= 2 ? STPDStatistics.coefficientOfVariation(localISIs) : nil,
                localLV: localISIs.count >= 2 ? STPDStatistics.localVariation(localISIs) : nil,
                localCV2: STPDStatistics.coefficientOfVariation2(localISIs),
                deltaLogISI: deltaLog,
                nextDeltaLogISI: nextDeltaLog,
                prepostRatio: prepost,
                qcStatus: SpikeISITrace.qcStatus(forISISec: isiVal, settings: qualitySettings),
                label: labelByISI[j]
            ))
        }
        return rows
    }

    // MARK: - Pure stat helpers

    /// Winsorize finite values to the [qLow, qHigh] quantile band (R `stpd_winsorize_numeric`):
    /// requires >= 4 finite values and qHigh > qLow, else returns the input unchanged.
    private static func winsorized(_ values: [Double?], low: Double, high: Double) -> [Double?] {
        let finite = values.compactMap { $0 }.filter(\.isFinite)
        guard finite.count >= 4 else { return values }
        let sample = SortedFiniteSample(finite)
        guard let qLow = sample.quantile(low), let qHigh = sample.quantile(high),
              qLow.isFinite, qHigh.isFinite, qHigh > qLow else {
            return values
        }
        return values.map { value in
            guard let value, value.isFinite else { return value }
            return Swift.min(Swift.max(value, qLow), qHigh)
        }
    }

    private static func median(_ values: [Double]) -> Double? {
        SortedFiniteSample(values).quantile(0.5)
    }

    private static func interquartileRange(_ values: [Double]) -> Double? {
        let sample = SortedFiniteSample(values)
        guard let q25 = sample.quantile(0.25), let q75 = sample.quantile(0.75) else { return nil }
        return q75 - q25
    }

    /// Sample standard deviation (n-1), matching R's `stats::sd`.
    private static func sampleStandardDeviation(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2, let mean = STPDStatistics.mean(finite) else { return nil }
        let variance = finite.reduce(0.0) { partial, value in
            let delta = value - mean
            return partial + delta * delta
        } / Double(finite.count - 1)
        return variance.squareRoot()
    }
}
