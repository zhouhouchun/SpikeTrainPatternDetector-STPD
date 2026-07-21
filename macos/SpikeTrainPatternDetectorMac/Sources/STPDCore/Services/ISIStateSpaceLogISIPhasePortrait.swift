import Foundation

/// One logISI phase-portrait transition row (mirrors a row of `stpd_make_logisi_phase_portrait`,
/// `R/55_state_space_pca.R`). A row is the transition from the ISI ending at spike `idx` to the ISI ending
/// at spike `nextIdx` (= `idx + lag` in spike order), carrying the log10-ISI of each so the view can plot
/// `logISI_i` vs `logISI_next` dynamics. Display / annotation layer only — never a detector input.
public struct ISIStateSpacePhasePortraitRow: Hashable, Sendable {
    public let train: String
    /// R `row_number` — the 1-based spike position of the current ISI (a position, not a source identity).
    public let rowNumber: Int
    public let idx: Int          // R `idx` (current right-spike source identity)
    public let nextIdx: Int      // R `next_idx`
    public let leftTimeSec: Double
    public let rightTimeSec: Double
    public let timeMidSec: Double
    public let isiSec: Double
    public let nextISISec: Double
    public let logISIi: Double    // R `logISI_i`
    public let logISINext: Double // R `logISI_next`
    public let label: String
    public let nextLabel: String
    public let transition: String // R `transition` = "label -> next_label"
    public let labelSource: String

    public init(
        train: String, rowNumber: Int, idx: Int, nextIdx: Int, leftTimeSec: Double, rightTimeSec: Double,
        timeMidSec: Double, isiSec: Double, nextISISec: Double, logISIi: Double, logISINext: Double,
        label: String, nextLabel: String, transition: String, labelSource: String
    ) {
        self.train = train; self.rowNumber = rowNumber; self.idx = idx; self.nextIdx = nextIdx
        self.leftTimeSec = leftTimeSec; self.rightTimeSec = rightTimeSec; self.timeMidSec = timeMidSec
        self.isiSec = isiSec; self.nextISISec = nextISISec; self.logISIi = logISIi; self.logISINext = logISINext
        self.label = label; self.nextLabel = nextLabel; self.transition = transition; self.labelSource = labelSource
    }
}

/// Pure mirror of `stpd_make_logisi_phase_portrait` (`R/55_state_space_pca.R`). Given a single train's spike
/// timestamps (plus optional source indices + per-spike labels), it computes per-spike ISIs and their
/// winsorizable log10 values, then emits one transition row per valid `(spike, spike+lag)` pair. Labels are an
/// optional pass-through (R resolves them via `stpd_state_space_pattern_labels`; the Mac supplies whichever
/// per-spike labels it has, defaulting to `"unlabeled"`). No detector behavior, no UI.
public enum ISIStateSpaceLogISIPhasePortrait {

    public static func build(
        timestampsSec: [Double],
        idx: [Int]? = nil,
        labels: [String]? = nil,
        train: String = "",
        labelSource: String = "auto",
        minValidISISec: Double = 0.001,
        lag: Int = 1,
        winsorize: Bool = false,
        winsorProbs: (low: Double, high: Double) = (0.01, 0.99)
    ) -> [ISIStateSpacePhasePortraitRow] {
        // R: `if (nrow(dat) < 4) return(data.frame())`.
        let n = timestampsSec.count
        guard n >= 4 else { return [] }
        let lag = Swift.max(1, lag)

        // R: `dat <- dat[order(dat$idx %||% seq_len(n)), ]` — order parallel arrays by source idx.
        let sourceIdx: [Int] = (idx?.count == n) ? idx! : Array(1...n)   // R default `seq_len(n)`
        let order = (0..<n).sorted { lhs, rhs in
            sourceIdx[lhs] != sourceIdx[rhs] ? sourceIdx[lhs] < sourceIdx[rhs] : lhs < rhs
        }
        let ts = order.map { timestampsSec[$0] }
        let idxValues = order.map { sourceIdx[$0] }
        let resolvedLabels: [String] = {
            guard let labels, labels.count == n else { return Array(repeating: "unlabeled", count: n) }
            return order.map { rawLabel(labels[$0]) }
        }()

        // R: `isi <- ISI_sec %||% c(NA, diff(ts))`; `valid <- is.finite(isi) & isi >= min_isi_sec`.
        var isi = [Double](repeating: .nan, count: n)
        for i in 1..<n { isi[i] = ts[i] - ts[i - 1] }
        let valid = isi.map { $0.isFinite && $0 >= minValidISISec }

        // R: `log_isi[valid] <- log10(isi[valid])`; optional winsorization of the whole logISI vector.
        var logISI = [Double](repeating: .nan, count: n)
        for i in 0..<n where valid[i] { logISI[i] = log10(isi[i]) }
        if winsorize { logISI = winsorizeNumeric(logISI, low: winsorProbs.low, high: winsorProbs.high) }

        // R: `rows <- which(seq >= 2 & seq + lag <= n & valid & valid[seq + lag])`, with finite endpoints.
        // 0-based: position p with p >= 1, p + lag <= n - 1, valid[p], valid[p + lag], finite ts[p], ts[p-1].
        var output: [ISIStateSpacePhasePortraitRow] = []
        for p in 1..<n {
            let next = p + lag
            guard next <= n - 1, valid[p], valid[next], ts[p].isFinite, ts[p - 1].isFinite else { continue }
            output.append(ISIStateSpacePhasePortraitRow(
                train: train,
                rowNumber: p + 1,                 // R 1-based `row_number`
                idx: idxValues[p],
                nextIdx: idxValues[next],
                leftTimeSec: ts[p - 1],
                rightTimeSec: ts[p],
                timeMidSec: (ts[p - 1] + ts[p]) / 2,
                isiSec: isi[p],
                nextISISec: isi[next],
                logISIi: logISI[p],
                logISINext: logISI[next],
                label: resolvedLabels[p],
                nextLabel: resolvedLabels[next],
                transition: "\(resolvedLabels[p]) -> \(resolvedLabels[next])",
                labelSource: labelSource
            ))
        }
        return output
    }

    /// R: empty / `NA` labels collapse to `"unlabeled"`.
    private static func rawLabel(_ value: String) -> String {
        let trimmed = value.trimmingCharacters(in: .whitespaces)
        return trimmed.isEmpty ? "unlabeled" : trimmed
    }

    /// Mirror of `stpd_winsorize_numeric`: with `< 4` finite values, or degenerate quantiles, the vector is
    /// returned unchanged; otherwise finite entries are clamped to the type-7 `[low, high]` quantiles.
    static func winsorizeNumeric(_ x: [Double], low: Double, high: Double) -> [Double] {
        let finite = x.filter(\.isFinite)
        guard finite.count >= 4 else { return x }
        let sample = SortedFiniteSample(finite)
        guard let q1 = sample.quantile(low), let q2 = sample.quantile(high),
              q1.isFinite, q2.isFinite, q2 > q1 else { return x }
        return x.map { $0.isFinite ? Swift.min(Swift.max($0, q1), q2) : $0 }
    }
}
