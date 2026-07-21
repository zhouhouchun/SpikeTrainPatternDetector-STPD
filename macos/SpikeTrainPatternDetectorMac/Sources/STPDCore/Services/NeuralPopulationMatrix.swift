import Foundation

/// NM-1A: a pure, R-compatible population activity matrix builder.
///
/// Mirrors the matrix-construction core of R `stpd_make_neural_population_matrix`
/// (`R/61_neural_manifold.R`): it bins several spike trains onto a shared time grid and produces a
/// `bins × trains` matrix of spike counts, firing rates, a transformed "signal" matrix
/// (`X_raw` in R), and an optionally smoothed + R-compatible-scaled matrix (`X` in R).
///
/// Scope is deliberately limited to the matrix itself. The behavior / task-event annotation layers
/// (`stpd_neural_attach_behavior`, `stpd_neural_attach_task_events`) and every embedding/validation
/// method (PCA/FA/Isomap/PHATE/UMAP/t-SNE/CEBRA/sliceTCA/NF geometry) are out of scope for NM-1A.
///
/// Unlike R's manifold entry point — which requires `>= 2` neurons because a manifold needs at least
/// two dimensions — this builder treats the matrix as a reusable primitive and accepts `>= 1` valid
/// train. The `>= 2` requirement belongs to the embedding layer (a later NM step), not here.

// MARK: - Parameter enums

/// Time base for binning. `raw` uses absolute timestamps; `aligned` shifts each train to its own first
/// spike (mirrors R `ts - min(ts)`, which the model already exposes as `alignedTimestampsSec`).
public enum NeuralPopulationTimeOrigin: String, CaseIterable, Sendable, Hashable {
    case raw
    case aligned
}

/// Per-bin signal transform, mirroring R `transform = c("sqrt_count", "log1p_rate", "rate", "count")`.
public enum NeuralPopulationTransform: String, CaseIterable, Sendable, Hashable {
    /// Raw spike count.
    case count
    /// Firing rate in Hz (`count / bin_width_sec`).
    case rate
    /// Anscombe-style `sqrt(count + 3/8)`.
    case sqrtCount = "sqrt_count"
    /// `log1p(max(rate, 0))`.
    case log1pRate = "log1p_rate"
}

/// Column scaling for the `scaled` matrix, mirroring R `scaling = c("zscore", "robust", "none")`.
public enum NeuralPopulationScaling: String, CaseIterable, Sendable, Hashable {
    case zscore
    case robust
    case none
}

// MARK: - Parameters

/// Pure parameters for the population matrix builder. Defaults mirror R
/// `stpd_make_neural_population_matrix` (`bin_sec = 0.05`, `time_origin = "raw"`,
/// `transform = "sqrt_count"`, `smoothing_sigma_bins = 1`, `scaling = "zscore"`).
public struct NeuralPopulationParameters: Sendable, Hashable {
    /// Bin width in seconds. Non-finite or `<= 0` falls back to `0.05` (R guard).
    public var binSec: Double
    /// Window start in seconds; `nil` uses the earliest spike across selected trains (R `%||% auto_start`).
    public var startSec: Double?
    /// Window end in seconds; `nil` uses the latest spike across selected trains (R `%||% auto_end`).
    public var endSec: Double?
    public var timeOrigin: NeuralPopulationTimeOrigin
    public var transform: NeuralPopulationTransform
    /// Gaussian smoothing sigma in bins; `0` disables. Non-finite or `< 0` falls back to `0` (R guard).
    public var smoothingSigmaBins: Double
    public var scaling: NeuralPopulationScaling

    public init(
        binSec: Double = 0.05,
        startSec: Double? = nil,
        endSec: Double? = nil,
        timeOrigin: NeuralPopulationTimeOrigin = .raw,
        transform: NeuralPopulationTransform = .sqrtCount,
        smoothingSigmaBins: Double = 1,
        scaling: NeuralPopulationScaling = .zscore
    ) {
        self.binSec = binSec
        self.startSec = startSec
        self.endSec = endSec
        self.timeOrigin = timeOrigin
        self.transform = transform
        self.smoothingSigmaBins = smoothingSigmaBins
        self.scaling = scaling
    }
}

// MARK: - Result types

/// One time bin (mirrors a row of R `stpd_state_trajectory_make_bin_table`). `endSec`/`widthSec` reflect
/// the clamp of the final bin to the window end, so the last bin can be narrower.
public struct NeuralPopulationBin: Sendable, Hashable {
    /// 1-based bin id, matching R `bin_id`.
    public let binID: Int
    public let startSec: Double
    public let endSec: Double
    public let midSec: Double
    public let widthSec: Double

    public init(binID: Int, startSec: Double, endSec: Double, midSec: Double, widthSec: Double) {
        self.binID = binID
        self.startSec = startSec
        self.endSec = endSec
        self.midSec = midSec
        self.widthSec = widthSec
    }
}

/// The built population matrix. All matrices are row-major `bins × trains` (row = bin, column = train),
/// with columns ordered as `trainIDs`.
public struct NeuralPopulationMatrix: Sendable, Hashable {
    public let bins: [NeuralPopulationBin]
    /// Column order: the valid selected trains actually included (those with `>= 2` spikes), in request order.
    public let trainIDs: [String]
    public let trainNames: [String]
    /// Raw spike counts per bin × train.
    public let counts: [[Double]]
    /// Firing rates (Hz) per bin × train (`count / max(bin_width, eps)`).
    public let rates: [[Double]]
    /// Transformed (and optionally smoothed) signal — R `X_raw`.
    public let signal: [[Double]]
    /// R-compatible-scaled signal — R `X`. Equals the median-filled `signal` when `scaling == .none`.
    public let scaled: [[Double]]
    public let transform: NeuralPopulationTransform
    public let scaling: NeuralPopulationScaling
    public let timeOrigin: NeuralPopulationTimeOrigin
    public let binSec: Double
    public let smoothingSigmaBins: Double
    public let windowStartSec: Double
    public let windowEndSec: Double

    public var binCount: Int { bins.count }
    public var trainCount: Int { trainIDs.count }

    public init(
        bins: [NeuralPopulationBin],
        trainIDs: [String],
        trainNames: [String],
        counts: [[Double]],
        rates: [[Double]],
        signal: [[Double]],
        scaled: [[Double]],
        transform: NeuralPopulationTransform,
        scaling: NeuralPopulationScaling,
        timeOrigin: NeuralPopulationTimeOrigin,
        binSec: Double,
        smoothingSigmaBins: Double,
        windowStartSec: Double,
        windowEndSec: Double
    ) {
        self.bins = bins
        self.trainIDs = trainIDs
        self.trainNames = trainNames
        self.counts = counts
        self.rates = rates
        self.signal = signal
        self.scaled = scaled
        self.transform = transform
        self.scaling = scaling
        self.timeOrigin = timeOrigin
        self.binSec = binSec
        self.smoothingSigmaBins = smoothingSigmaBins
        self.windowStartSec = windowStartSec
        self.windowEndSec = windowEndSec
    }
}

/// Guard failures, mirroring R's empty-result early returns.
public enum NeuralPopulationMatrixError: Error, Equatable, Sendable {
    /// No selected train ids (empty selection, or none present in the dataset).
    case noTrainsSelected
    /// No selected train has at least two finite spikes.
    case noValidTrains
}

// MARK: - Builder

public enum NeuralPopulationMatrixBuilder {
    /// `.Machine$double.eps`, used as the rate denominator floor (R `pmax(bin_width_sec, eps)`).
    public static let widthEpsilon = Double.ulpOfOne

    /// Build the `bins × trains` population matrix from a dataset.
    ///
    /// - Parameters:
    ///   - dataset: source spike trains.
    ///   - selectedTrainIDs: train ids to include, in the desired column order; `nil` uses all trains in
    ///     dataset order. Unknown ids are ignored and duplicates are de-duplicated (first occurrence wins),
    ///     mirroring R `intersect(selected_trains, names(trains))`.
    ///   - parameters: pure binning / transform / smoothing / scaling parameters.
    public static func build(
        dataset: SpikeDataset,
        selectedTrainIDs: [String]? = nil,
        parameters: NeuralPopulationParameters = NeuralPopulationParameters()
    ) throws -> NeuralPopulationMatrix {
        // 1. Resolve selected trains: keep request order, drop unknowns + duplicates.
        let trainsByID = Dictionary(dataset.trains.map { ($0.id, $0) }, uniquingKeysWith: { first, _ in first })
        let requestedIDs: [String]
        if let selectedTrainIDs {
            var seen = Set<String>()
            requestedIDs = selectedTrainIDs.filter { trainsByID[$0] != nil && seen.insert($0).inserted }
        } else {
            requestedIDs = dataset.trains.map(\.id)
        }
        guard !requestedIDs.isEmpty else { throw NeuralPopulationMatrixError.noTrainsSelected }

        // 2. Validate bin width + smoothing (R guards).
        var binSec = parameters.binSec
        if !binSec.isFinite || binSec <= 0 { binSec = 0.05 }
        var smoothingSigmaBins = parameters.smoothingSigmaBins
        if !smoothingSigmaBins.isFinite || smoothingSigmaBins < 0 { smoothingSigmaBins = 0 }

        // 3. Prepare per-train analysis timestamps (>= 2 finite spikes; `timestampsSec` is already
        //    sorted + finite, and `alignedTimestampsSec` already encodes R's `ts - min(ts)`).
        struct PreparedTrain { let id: String; let name: String; let analysis: [Double] }
        var prepared: [PreparedTrain] = []
        for id in requestedIDs {
            guard let train = trainsByID[id], train.timestampsSec.count >= 2 else { continue }
            let analysis = parameters.timeOrigin == .aligned ? train.alignedTimestampsSec : train.timestampsSec
            prepared.append(PreparedTrain(id: id, name: train.name, analysis: analysis))
        }
        guard !prepared.isEmpty else { throw NeuralPopulationMatrixError.noValidTrains }

        // 4. Window (R `%||% auto_*` + finiteness guards).
        let allTimes = prepared.flatMap(\.analysis)
        let autoStart = allTimes.min() ?? 0
        let autoEnd = allTimes.max() ?? (autoStart + binSec)
        var startUse = parameters.startSec ?? autoStart
        if !startUse.isFinite { startUse = autoStart }
        var endUse = parameters.endSec ?? autoEnd
        if !endUse.isFinite || endUse <= startUse { endUse = autoEnd }
        if !endUse.isFinite || endUse <= startUse { endUse = startUse + binSec }

        // 5. Bin table.
        let bins = makeBinTable(startSec: startUse, endSec: endUse, binSec: binSec)
        let nBins = bins.count
        let nTrains = prepared.count

        // 6. Counts (interior bins left-closed `[b0, b1)`, last bin inclusive `[b0, b1]`).
        var counts = [[Double]](repeating: [Double](repeating: 0, count: nTrains), count: nBins)
        for (j, train) in prepared.enumerated() {
            let timestamps = train.analysis
            for (b, bin) in bins.enumerated() {
                let lower = bin.startSec
                let upper = bin.endSec
                let isLast = (b == nBins - 1)
                var hits = 0
                for time in timestamps where time >= lower && (isLast ? time <= upper : time < upper) {
                    hits += 1
                }
                counts[b][j] = Double(hits)
            }
        }

        // 7. Rates.
        var rates = counts
        for b in 0..<nBins {
            let width = Swift.max(bins[b].widthSec, widthEpsilon)
            for j in 0..<nTrains { rates[b][j] = counts[b][j] / width }
        }

        // 8. Transform → signal (R `X_raw`).
        var signal = [[Double]](repeating: [Double](repeating: 0, count: nTrains), count: nBins)
        for b in 0..<nBins {
            for j in 0..<nTrains {
                let count = counts[b][j]
                let rate = rates[b][j]
                switch parameters.transform {
                case .count:
                    signal[b][j] = count
                case .rate:
                    signal[b][j] = rate
                case .sqrtCount:
                    signal[b][j] = (Swift.max(count, 0) + 0.375).squareRoot()
                case .log1pRate:
                    signal[b][j] = log1p(Swift.max(rate, 0))
                }
            }
        }

        // 9. Optional per-column Gaussian smoothing (R `smoothing_sigma_bins > 0 && nrow >= 3`).
        if smoothingSigmaBins > 0, nBins >= 3 {
            for j in 0..<nTrains {
                let column = (0..<nBins).map { signal[$0][j] }
                let smoothed = gaussianSmooth(column, sigmaBins: smoothingSigmaBins)
                for b in 0..<nBins { signal[b][j] = smoothed[b] }
            }
        }

        // 10. Scale → scaled (R `X = stpd_neural_scale_matrix(signal, scaling)`).
        let scaled: [[Double]]
        switch parameters.scaling {
        case .none:
            scaled = fillColumnMedians(signal)
        case .zscore, .robust:
            let pcaScaling: ISIStatePCAScaling = (parameters.scaling == .robust) ? .robust : .zscore
            var scaledMatrix = ISIStateSpacePCA.scaleMatrixForPCA(signal, scaling: pcaScaling)
            for b in 0..<nBins {
                for j in 0..<nTrains where !scaledMatrix[b][j].isFinite { scaledMatrix[b][j] = 0 }
            }
            scaled = scaledMatrix
        }

        return NeuralPopulationMatrix(
            bins: bins,
            trainIDs: prepared.map(\.id),
            trainNames: prepared.map(\.name),
            counts: counts,
            rates: rates,
            signal: signal,
            scaled: scaled,
            transform: parameters.transform,
            scaling: parameters.scaling,
            timeOrigin: parameters.timeOrigin,
            binSec: binSec,
            smoothingSigmaBins: smoothingSigmaBins,
            windowStartSec: startUse,
            windowEndSec: endUse
        )
    }

    // MARK: - Pure helpers (mirror of R)

    /// Mirror of R `stpd_state_trajectory_make_bin_table`: an evenly spaced grid whose final bin is clamped
    /// to `endSec` (so it can be narrower). `n = max(1, ceil((end - start) / bin))`.
    static func makeBinTable(startSec: Double, endSec: Double, binSec: Double) -> [NeuralPopulationBin] {
        var start = startSec
        var end = endSec
        var bin = binSec
        if !start.isFinite { start = 0 }
        if !bin.isFinite || bin <= 0 { bin = 0.1 }
        if !end.isFinite || end <= start { end = start + bin }
        let nBins = Swift.max(1, Int(((end - start) / bin).rounded(.up)))
        var table: [NeuralPopulationBin] = []
        table.reserveCapacity(nBins)
        for k in 0..<nBins {
            let s = start + Double(k) * bin
            let e = Swift.min(s + bin, end)
            table.append(NeuralPopulationBin(binID: k + 1, startSec: s, endSec: e, midSec: (s + e) / 2, widthSec: e - s))
        }
        return table
    }

    /// Mirror of R `stpd_state_trajectory_gaussian_smooth`: truncated Gaussian kernel (radius
    /// `ceil(3*sigma)`) whose weights are renormalized over the finite values in the window. Bins with no
    /// finite neighbor become `NaN`. Returns the input unchanged for `sigma <= 0` or fewer than 3 points.
    static func gaussianSmooth(_ values: [Double], sigmaBins: Double) -> [Double] {
        guard sigmaBins.isFinite, sigmaBins > 0, values.count >= 3 else { return values }
        let radius = Swift.max(1, Int((3 * sigmaBins).rounded(.up)))
        var out = values
        for i in 0..<values.count {
            let lower = Swift.max(0, i - radius)
            let upper = Swift.min(values.count - 1, i + radius)
            var weightSum = 0.0
            var valueSum = 0.0
            for j in lower...upper where values[j].isFinite {
                let distance = Double(j - i) / sigmaBins
                let weight = exp(-0.5 * distance * distance)
                weightSum += weight
                valueSum += values[j] * weight
            }
            out[i] = weightSum > 0 ? valueSum / weightSum : .nan
        }
        return out
    }

    /// Mirror of R `stpd_neural_fill_matrix`: per-column impute of non-finite cells by the column median of
    /// the finite values (fallback `0` when a column has no finite value). Used for the `scaling == .none`
    /// path (the scaled paths impute inside `scaleMatrixForPCA`).
    static func fillColumnMedians(_ matrix: [[Double]]) -> [[Double]] {
        let n = matrix.count
        guard n > 0 else { return matrix }
        let p = matrix[0].count
        guard p > 0 else { return matrix }
        var out = matrix
        for j in 0..<p {
            let finite = (0..<n).compactMap { matrix[$0][j].isFinite ? matrix[$0][j] : nil }
            let median = SortedFiniteSample(finite).quantile(0.5)
            let fill = (median?.isFinite ?? false) ? median! : 0
            for i in 0..<n where !out[i][j].isFinite { out[i][j] = fill }
        }
        return out
    }
}
