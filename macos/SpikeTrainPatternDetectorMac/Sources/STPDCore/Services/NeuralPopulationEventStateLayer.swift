import Foundation

/// Which detector annotation source an event-state layer was built from. Mirrors R `label_source`: `auto` is the
/// raw selected auto annotations, `final` is the reviewed public projection. Kept separate so the UI can show
/// auto vs final dominant states side by side and never lets review mutate the raw auto layer.
public enum NeuralPopulationEventLabelSource: String, Sendable, Hashable {
    case auto
    case final
}

/// One population time bin's event-state annotation. The `dominantState`/`fractions`/`labeledFraction` are the
/// interval-overlap aggregation (mirroring R `*_fraction` + `dominant_state`); the spike counts/rates mirror R's
/// per-state `*_spike_count` / `*_rate_hz` + `firing_rate_hz` (per-train mean over the contributing-train
/// denominator). Annotation layer only — never a PCA / matrix input.
public struct NeuralPopulationEventStateRow: Sendable, Hashable {
    public let binID: Int
    public let startSec: Double
    public let endSec: Double
    public let midSec: Double
    public let widthSec: Double
    public let dominantState: SpikePatternState
    /// Per-state covered fraction of the bin (overlap / bin width, clamped 0...1), from the reused
    /// `NeuralPopulationPatternStateAnnotator`. NUANCE vs R `*_fraction`: this sums overlap across trains and
    /// clamps, whereas R divides by `n_trains` (per-train mean). They are identical for a single train; for
    /// multiple trains only the reported VALUE differs — the `dominantState` is unaffected because it is the
    /// argmax of the RAW summed overlap (the same scalar denominator cancels), so it matches R's `max.col`.
    /// `hfTonic` is kept as its own state here (R folds high-frequency tonic into the `tonic` group) — a
    /// deliberate finer-grained split, consistent with the rest of the Mac `SpikePatternState` model.
    public let fractions: [SpikePatternState: Double]
    /// Total labeled fraction; `unlabeledFraction = 1 - labeledFraction`.
    public let labeledFraction: Double
    public let unlabeledFraction: Double
    /// Total spikes in the bin across contributing trains, and the per-train mean rate (R `firing_rate_hz`).
    public let spikeCount: Int
    public let firingRateHz: Double
    /// Per-state spike counts (a spike's state is the interval covering its time) and per-train mean rates.
    public let stateSpikeCounts: [SpikePatternState: Int]
    public let stateRatesHz: [SpikePatternState: Double]
    /// Trains with at least one spike in this bin, and the rate denominator (R `n_trains`).
    public let contributingTrainCount: Int
    public let nTrains: Int
    public let labelSource: NeuralPopulationEventLabelSource

    public func fraction(_ state: SpikePatternState) -> Double { fractions[state] ?? 0 }
    public func rateHz(_ state: SpikePatternState) -> Double { stateRatesHz[state] ?? 0 }
    public func spikeCount(_ state: SpikePatternState) -> Int { stateSpikeCounts[state] ?? 0 }
}

/// A full per-bin event-state layer for one label source, plus dataset-level occupancy summaries.
public struct NeuralPopulationEventStateResult: Sendable {
    public let labelSource: NeuralPopulationEventLabelSource
    public let rows: [NeuralPopulationEventStateRow]
    /// Number of bins whose dominant state is each state (`.unlabeled` included).
    public let binCountByDominantState: [SpikePatternState: Int]
    /// Fraction of bins whose dominant state is each state (bin-count occupancy; sums to ~1 over all states).
    public let occupancyByDominantState: [SpikePatternState: Double]

    private let rowsByBinID: [Int: NeuralPopulationEventStateRow]

    public init(
        labelSource: NeuralPopulationEventLabelSource,
        rows: [NeuralPopulationEventStateRow],
        binCountByDominantState: [SpikePatternState: Int],
        occupancyByDominantState: [SpikePatternState: Double]
    ) {
        self.labelSource = labelSource
        self.rows = rows
        self.binCountByDominantState = binCountByDominantState
        self.occupancyByDominantState = occupancyByDominantState
        self.rowsByBinID = Dictionary(rows.map { ($0.binID, $0) }, uniquingKeysWith: { first, _ in first })
    }

    public func row(forBinID binID: Int) -> NeuralPopulationEventStateRow? { rowsByBinID[binID] }
}

/// Pure R-style event-state layer for the Neural Manifold (mirrors `R/61` `stpd_neural_fast_event_state_result`
/// / `stpd_neural_attach_event_states`). For each population bin it reports the dominant pattern state and per-
/// state fractions (by interval overlap, reusing `NeuralPopulationPatternStateAnnotator`), plus per-state spike
/// counts/rates and the overall firing rate (by binning spikes the same way `NeuralPopulationMatrixBuilder`
/// does and tagging each spike with the interval covering its time). Pattern states are an annotation layer
/// only — PCA coordinates and the population matrix are never touched.
public enum NeuralPopulationEventStateLayer {
    /// Convenience: build directly from detector annotations (auto = raw selected; final = reviewed public).
    public static func build(
        matrix: NeuralPopulationMatrix,
        trains: [SpikeTrain],
        annotations: [ClassicAnchorEventAnnotation],
        labelSource: NeuralPopulationEventLabelSource
    ) -> NeuralPopulationEventStateResult {
        build(
            matrix: matrix,
            trains: trains,
            intervals: annotations.map(SpikePatternInterval.init(annotation:)),
            labelSource: labelSource
        )
    }

    public static func build(
        matrix: NeuralPopulationMatrix,
        trains: [SpikeTrain],
        intervals: [SpikePatternInterval],
        labelSource: NeuralPopulationEventLabelSource
    ) -> NeuralPopulationEventStateResult {
        let bins = matrix.bins
        let useAligned = matrix.timeOrigin == .aligned
        let selectedIDs = Set(matrix.trainIDs)
        let selectedTrains = trains.filter { selectedIDs.contains($0.id) }

        // 1. Overlap fractions + dominant state (reuse the existing annotator — same labeling as color-by/hover).
        let summaries = NeuralPopulationPatternStateAnnotator.annotate(
            bins: bins, selectedTrainIDs: selectedIDs, useAligned: useAligned, intervals: intervals
        )
        let summaryByBin = Dictionary(summaries.map { ($0.binID, $0) }, uniquingKeysWith: { first, _ in first })

        // 2. Per-bin spike counts (total + per state) by binning each spike like the matrix builder and tagging
        //    it with the interval covering its time. Sorted bin starts give the same [start, end) assignment
        //    (last bin closed) the builder uses for contiguous bins.
        let patternIntervals = SpikePatternIntervals(intervals, useAligned: useAligned)
        let starts = bins.map(\.startSec)
        var total = [Int](repeating: 0, count: bins.count)
        var stateCounts = [[SpikePatternState: Int]](repeating: [:], count: bins.count)
        var contributing = [Set<String>](repeating: [], count: bins.count)

        for train in selectedTrains {
            let times = useAligned ? train.alignedTimestampsSec : train.timestampsSec
            for time in times {
                guard time.isFinite, let b = binIndex(for: time, bins: bins, starts: starts) else { continue }
                total[b] += 1
                let state = patternIntervals.state(forTrainID: train.id, atSec: time)
                stateCounts[b][state, default: 0] += 1
                contributing[b].insert(train.id)
            }
        }
        // R `stpd_neural_fast_event_state_result` divides rates by the number of valid selected trains
        // (`prepared_names`), not by the number that happened to spike inside a given/custom window. Use the
        // matrix column count for exact parity with the binned population matrix, including zero-count columns.
        let denom = Double(max(1, matrix.trainCount))

        // 3. Assemble rows.
        var rows: [NeuralPopulationEventStateRow] = []
        rows.reserveCapacity(bins.count)
        for (index, bin) in bins.enumerated() {
            let summary = summaryByBin[bin.binID]
            let width = max(bin.widthSec, 0)
            let rateDenom = width > 0 ? width * denom : 0

            var stateRates: [SpikePatternState: Double] = [:]
            for (state, count) in stateCounts[index] where state != .unlabeled {
                stateRates[state] = rateDenom > 0 ? Double(count) / rateDenom : 0
            }
            let labeled = summary?.labeledFraction ?? 0

            rows.append(NeuralPopulationEventStateRow(
                binID: bin.binID,
                startSec: bin.startSec,
                endSec: bin.endSec,
                midSec: bin.midSec,
                widthSec: bin.widthSec,
                dominantState: summary?.dominant ?? .unlabeled,
                fractions: summary?.fractions ?? [:],
                labeledFraction: labeled,
                unlabeledFraction: max(0, 1 - labeled),
                spikeCount: total[index],
                firingRateHz: rateDenom > 0 ? Double(total[index]) / rateDenom : 0,
                stateSpikeCounts: stateCounts[index].filter { $0.key != .unlabeled },
                stateRatesHz: stateRates,
                contributingTrainCount: contributing[index].count,
                nTrains: Int(denom),
                labelSource: labelSource
            ))
        }

        // 4. Dataset-level occupancy summary (by dominant state, bin-count based).
        var binCountByState: [SpikePatternState: Int] = [:]
        for row in rows { binCountByState[row.dominantState, default: 0] += 1 }
        let totalBins = max(1, rows.count)
        let occupancy = binCountByState.mapValues { Double($0) / Double(totalBins) }

        return NeuralPopulationEventStateResult(
            labelSource: labelSource,
            rows: rows,
            binCountByDominantState: binCountByState,
            occupancyByDominantState: occupancy
        )
    }

    /// The bin a spike at `time` falls in, matching `NeuralPopulationMatrixBuilder` (`time >= start` and, for
    /// every bin but the last, `time < end`; the last bin is right-closed). Returns `nil` outside all bins.
    private static func binIndex(for time: Double, bins: [NeuralPopulationBin], starts: [Double]) -> Int? {
        guard let first = bins.first, let last = bins.last, time >= first.startSec, time <= last.endSec else {
            return nil
        }
        // Rightmost bin whose start <= time (binary search over the sorted starts).
        var lo = 0
        var hi = starts.count - 1
        var candidate = 0
        while lo <= hi {
            let mid = (lo + hi) / 2
            if starts[mid] <= time {
                candidate = mid
                lo = mid + 1
            } else {
                hi = mid - 1
            }
        }
        let bin = bins[candidate]
        let isLast = candidate == bins.count - 1
        if time >= bin.startSec, isLast ? time <= bin.endSec : time < bin.endSec {
            return candidate
        }
        return nil
    }
}
