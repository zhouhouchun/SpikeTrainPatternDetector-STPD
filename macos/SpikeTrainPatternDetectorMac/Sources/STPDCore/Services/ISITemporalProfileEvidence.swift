import Foundation

/// Per-candidate ISI temporal-profile **evidence** (Phase 2B). These are diagnostic values computed
/// beside a detected candidate to help a reviewer judge how strong a candidate's ISI boundary /
/// compression support is. They mirror R's `R/05_isi_context_features.R` / `R/09_burst_seed_bridge.R`
/// helpers (`calc_edge_contrast_stats`, `calc_ratio_summary`, `isi_percentile_scalar`,
/// `get_local_median`) as used on the seed-candidate path in `R/17_critical_overrides.R:48-87`.
///
/// IMPORTANT — these are evidence only. No field here may ever enter a comparison that changes which
/// candidates are emitted, selected, or labeled (matching R's audit-first policy, `R/32:1-4`). All
/// values are optional and are `nil` when undefined (e.g. no valid flank); they are never fabricated.
/// No MM / max-mean. No fixed universal millisecond assumption.
public struct ISITemporalProfileEvidence: Hashable, Sendable {
    /// `min(pre/coreQ, post/coreQ)` over the finite, positive flank ratios — the canonical
    /// edge-contrast minimum (R `edge_contrast_min_q`). `nil` when no valid flank ratio exists.
    public let edgeContrastMin: Double?
    /// Geometric mean of the finite, positive flank ratios (R `edge_contrast_geom_q`).
    public let edgeContrastGeom: Double?
    /// `pre / coreQ` — the leading-flank ISI relative to the candidate's core quantile ISI.
    public let preEdgeRatio: Double?
    /// `post / coreQ` — the trailing-flank ISI relative to the core quantile ISI.
    public let postEdgeRatio: Double?
    /// Number of finite, positive flank ratios available (0, 1, or 2). R `n_flank`.
    public let flankCount: Int
    /// Empirical percentile (0–100) of the candidate's core quantile ISI within the whole train's
    /// valid-ISI distribution (R `isi_percentile_scalar(core_q, dat$ISI_sec)`). `nil` when undefined.
    public let coreQPct: Double?
    /// Whether the train has enough valid ISIs for the percentile to be reliable (R
    /// `train_percentile_reliable`: `nValid >= 50`).
    public let percentileReliable: Bool
    /// Median of the per-in-span-ISI local windowed median ISIs (R `local_median_sec`) — the local
    /// temporal baseline outside the candidate's own compression.
    public let localMedianISISec: Double?
    /// `localMedianISISec / coreQ` (R `local_median_core_ratio` / seed `local_comp`): how compressed
    /// the candidate's core is versus its local neighborhood. `> 1` ⇒ locally compressed.
    public let localCompressionRatio: Double?

    public init(
        edgeContrastMin: Double?,
        edgeContrastGeom: Double?,
        preEdgeRatio: Double?,
        postEdgeRatio: Double?,
        flankCount: Int,
        coreQPct: Double?,
        percentileReliable: Bool,
        localMedianISISec: Double?,
        localCompressionRatio: Double?
    ) {
        self.edgeContrastMin = edgeContrastMin
        self.edgeContrastGeom = edgeContrastGeom
        self.preEdgeRatio = preEdgeRatio
        self.postEdgeRatio = postEdgeRatio
        self.flankCount = flankCount
        self.coreQPct = coreQPct
        self.percentileReliable = percentileReliable
        self.localMedianISISec = localMedianISISec
        self.localCompressionRatio = localCompressionRatio
    }

    /// Fully-undefined evidence (used when a candidate span is out of range / has no valid ISIs).
    /// `percentileReliable` still reflects the train when known.
    public static func undefined(percentileReliable: Bool = false) -> ISITemporalProfileEvidence {
        ISITemporalProfileEvidence(
            edgeContrastMin: nil, edgeContrastGeom: nil, preEdgeRatio: nil, postEdgeRatio: nil,
            flankCount: 0, coreQPct: nil, percentileReliable: percentileReliable,
            localMedianISISec: nil, localCompressionRatio: nil
        )
    }
}

/// Pure builder for `ISITemporalProfileEvidence`. Operates on a `SpikeTrain`'s `isiSec` (0-based;
/// `isiSec[i]` is the interval ending at spike `i`, valid for `i` in `1..<count`) and a candidate's
/// ISI-index span `[startISIIndex, endISIIndex]` — the same convention as `ClassicAnchorCandidate`
/// (`ISI index i` indexes `isiSec[i]`). No detector state is touched; this is read-only over ISIs.
public enum ISITemporalProfileEvidenceBuilder {
    public static let defaultContrastQuantile = 0.90
    public static let defaultLocalWindow = 11
    /// R `adaptive_min_isi_for_percentile` default (`train_percentile_reliable`).
    public static let percentileReliableMinCount = 50

    public static func makeEvidence(
        train: SpikeTrain,
        startISIIndex: Int,
        endISIIndex: Int,
        minValidISISec: Double,
        contrastQuantile: Double = defaultContrastQuantile,
        localWindow: Int = defaultLocalWindow
    ) -> ISITemporalProfileEvidence {
        let isi = train.isiSec
        let n = isi.count
        let minValid = minValidISISec.isFinite ? minValidISISec : 0.001
        let window = max(1, localWindow)

        // Train-wide valid ISIs (for the percentile rank + reliability). `isiSec[0]` is nil.
        var validISIs: [Double] = []
        if n >= 2 {
            for j in 1..<n {
                if let value = isi[j], value.isFinite, value >= minValid { validISIs.append(value) }
            }
        }
        let nValid = validISIs.count
        let percentileReliable = nValid >= percentileReliableMinCount
        let sortedValid = validISIs.sorted()

        func validISI(at index: Int) -> Double? {
            guard index >= 1, index < n, let value = isi[index], value.isFinite, value >= minValid else {
                return nil
            }
            return value
        }

        // Candidate span guard (mirrors R `s_isi >= 2 (post shift) & e_isi <= n & e_isi >= s_isi`,
        // expressed in the 0-based isiSec valid range `1...n-1`).
        guard startISIIndex >= 1, endISIIndex <= n - 1, endISIIndex >= startISIIndex else {
            return .undefined(percentileReliable: percentileReliable)
        }

        var spanVals: [Double] = []
        for i in startISIIndex...endISIIndex {
            if let value = validISI(at: i) { spanVals.append(value) }
        }
        guard !spanVals.isEmpty else {
            return .undefined(percentileReliable: percentileReliable)
        }

        let q = Swift.min(Swift.max(contrastQuantile, 0.50), 1.00)
        let coreQ = SortedFiniteSample(spanVals).quantile(q)   // R type-7 quantile

        // Immediate flanks: the ISI just before the span start and just after the span end.
        let pre = (startISIIndex >= 2) ? validISI(at: startISIIndex - 1) : nil
        let post = (endISIIndex + 1 <= n - 1) ? validISI(at: endISIIndex + 1) : nil

        // Ratio summary against coreQ (R `calc_ratio_summary`): undefined unless coreQ is finite > 0.
        var preEdgeRatio: Double?
        var postEdgeRatio: Double?
        var ratios: [Double] = []
        if let coreQ, coreQ.isFinite, coreQ > 0 {
            if let pre, pre > 0 {
                let r = pre / coreQ
                preEdgeRatio = r
                if r.isFinite, r > 0 { ratios.append(r) }
            }
            if let post, post > 0 {
                let r = post / coreQ
                postEdgeRatio = r
                if r.isFinite, r > 0 { ratios.append(r) }
            }
        }
        let edgeContrastMin = ratios.min()
        let edgeContrastGeom = ratios.isEmpty
            ? nil
            : exp(ratios.reduce(0.0) { $0 + log($1) } / Double(ratios.count))

        let coreQPct = coreQ.flatMap { percentileRank(of: $0, inSorted: sortedValid, nValid: nValid) }

        // Local median baseline over the in-span ISI indices (each excluding itself), then median.
        var localMedians: [Double] = []
        for i in startISIIndex...endISIIndex {
            if let m = localMedian(isi: isi, index: i, window: window, minValid: minValid, count: n) {
                localMedians.append(m)
            }
        }
        let localMedianISISec = localMedians.isEmpty ? nil : SortedFiniteSample(localMedians).quantile(0.5)
        let localCompressionRatio: Double? = {
            guard let lm = localMedianISISec, let cq = coreQ, cq.isFinite, cq > 0 else { return nil }
            return lm / cq
        }()

        return ISITemporalProfileEvidence(
            edgeContrastMin: edgeContrastMin,
            edgeContrastGeom: edgeContrastGeom,
            preEdgeRatio: preEdgeRatio,
            postEdgeRatio: postEdgeRatio,
            flankCount: ratios.count,
            coreQPct: coreQPct,
            percentileReliable: percentileReliable,
            localMedianISISec: localMedianISISec,
            localCompressionRatio: localCompressionRatio
        )
    }

    /// Convenience: evidence for a detected candidate using its ISI-index span.
    public static func makeEvidence(
        for candidate: ClassicAnchorCandidate,
        train: SpikeTrain,
        minValidISISec: Double,
        contrastQuantile: Double = defaultContrastQuantile,
        localWindow: Int = defaultLocalWindow
    ) -> ISITemporalProfileEvidence {
        makeEvidence(
            train: train,
            startISIIndex: candidate.startISIIndex,
            endISIIndex: candidate.endISIIndex,
            minValidISISec: minValidISISec,
            contrastQuantile: contrastQuantile,
            localWindow: localWindow
        )
    }

    /// Evidence for every candidate in a detection run, keyed by candidate id. Uses the run's
    /// dataset-level `bandSettings.minValidISISec` (the same value the detector derived from the
    /// artifact threshold). The run does not retain the source dataset, so the caller passes it. A
    /// candidate with no matching train (e.g. the dataset-level `__dataset__` profile pseudo-candidate)
    /// maps to `.undefined()`, so the result covers every candidate id. Pure, read-only — never
    /// mutates the run.
    public static func evidenceByCandidateID(
        run: ClassicAnchorDetectionRun,
        dataset: SpikeDataset,
        contrastQuantile: Double = defaultContrastQuantile,
        localWindow: Int = defaultLocalWindow
    ) -> [String: ISITemporalProfileEvidence] {
        let minValid = max(run.bandSettings.minValidISISec, 1e-9)
        let trainsByID = Dictionary(dataset.trains.map { ($0.id, $0) }, uniquingKeysWith: { first, _ in first })
        var result: [String: ISITemporalProfileEvidence] = [:]
        result.reserveCapacity(run.candidates.count)
        for candidate in run.candidates {
            if let train = trainsByID[candidate.trainID] {
                result[candidate.id] = makeEvidence(
                    for: candidate,
                    train: train,
                    minValidISISec: minValid,
                    contrastQuantile: contrastQuantile,
                    localWindow: localWindow
                )
            } else {
                result[candidate.id] = .undefined()
            }
        }
        return result
    }

    // MARK: - Pure helpers

    /// Empirical percentile rank (0–100) of `value` in a train's sorted valid ISIs, matching R's
    /// `isi_percentile_from_cache`: `100 * findInterval(value, sorted, rightmost.closed = TRUE) /
    /// nValid` (count of sorted ≤ value, with the maximum mapped to the closed rightmost interval).
    static func percentileRank(of value: Double, inSorted sorted: [Double], nValid: Int) -> Double? {
        guard value.isFinite, nValid > 0, !sorted.isEmpty else { return nil }
        var count = countLessThanOrEqual(value, in: sorted)
        if let last = sorted.last, value == last, count == nValid {
            count = nValid - 1   // rightmost.closed = TRUE
        }
        return 100.0 * Double(count) / Double(nValid)
    }

    /// Number of elements `<= value` in an ascending-sorted array (binary search upper bound).
    private static func countLessThanOrEqual(_ value: Double, in sorted: [Double]) -> Int {
        var low = 0
        var high = sorted.count
        while low < high {
            let mid = (low + high) / 2
            if sorted[mid] <= value { low = mid + 1 } else { high = mid }
        }
        return low
    }

    /// Local windowed median ISI around ISI index `i` (window `±window`, self-excluded), over valid
    /// ISIs only. Mirrors R `get_local_median(isi, i, window, exclude_idx = i)` in the 0-based isiSec
    /// space (R's `max(2, ...)` lower clamp ⇒ `max(1, ...)` here, since R ISI index 2 == isiSec[1]).
    static func localMedian(isi: [Double?], index i: Int, window: Int, minValid: Double, count n: Int) -> Double? {
        let low = max(1, i - window)
        let high = min(n - 1, i + window)
        guard low <= high else { return nil }
        var xs: [Double] = []
        for j in low...high where j != i {
            if let value = isi[j], value.isFinite, value >= minValid { xs.append(value) }
        }
        guard !xs.isEmpty else { return nil }
        return SortedFiniteSample(xs).quantile(0.5)
    }
}
