import Foundation

// MARK: - Distribution-first foundation (D1-D3) — UI presentation model
//
// A PURE, view-agnostic data-shaping layer that turns the distribution-first foundation
// (`DatasetISIDistribution` from D1/D2 + `ModeISIIntervalDeriver` priors from D3) into flat rows a
// SwiftUI view can render directly. Lives in STPDCore (not the app target) so it is unit-testable in
// isolation. It does NOT touch detectors, the pipeline, or the old `DatasetISIHistogram` — it is the
// distribution-first counterpart, clearly separate.

/// One quantile summary row (pooled, train-balanced, or a single train). Seconds are raw; the view
/// formats units.
public struct ISIDistributionQuantileRow: Hashable, Sendable {
    public let label: String
    public let trainID: String?
    public let validISICount: Int
    public let minSec: Double?
    public let q10Sec: Double?
    public let q25Sec: Double?
    public let q50Sec: Double?
    public let q75Sec: Double?
    public let q90Sec: Double?
    public let maxSec: Double?
    public let meanSec: Double?

    public init(
        label: String, trainID: String?, validISICount: Int,
        minSec: Double?, q10Sec: Double?, q25Sec: Double?, q50Sec: Double?,
        q75Sec: Double?, q90Sec: Double?, maxSec: Double?, meanSec: Double?
    ) {
        self.label = label; self.trainID = trainID; self.validISICount = validISICount
        self.minSec = minSec; self.q10Sec = q10Sec; self.q25Sec = q25Sec; self.q50Sec = q50Sec
        self.q75Sec = q75Sec; self.q90Sec = q90Sec; self.maxSec = maxSec; self.meanSec = meanSec
    }

    /// Build a row from a D1 `ISIQuantiles` bundle.
    public static func from(label: String, trainID: String?, count: Int, quantiles: ISIQuantiles) -> ISIDistributionQuantileRow {
        ISIDistributionQuantileRow(
            label: label, trainID: trainID, validISICount: count,
            minSec: quantiles.minSec, q10Sec: quantiles.q10, q25Sec: quantiles.q25, q50Sec: quantiles.q50,
            q75Sec: quantiles.q75, q90Sec: quantiles.q90, maxSec: quantiles.maxSec, meanSec: quantiles.meanSec)
    }
}

/// One D3-derived `ModeISIInterval` prior, flattened with its provenance permissions for display.
public struct ISIModeIntervalRow: Hashable, Sendable {
    public let family: ISIPatternFamily
    public let scope: ISIDistributionScope
    /// Core interval bounds (e.g. tonic q25-q75).
    public let lowerSec: Double
    public let upperSec: Double
    public let bridgeUpperSec: Double?
    /// Wider acceptance-band bounds (membership band); nil when equal to the core.
    public let acceptanceLowerSec: Double?
    public let acceptanceUpperSec: Double?
    public let isValid: Bool
    public let provenanceOrigin: IntervalOrigin
    public let sourceStatistic: String
    public let mayPropagateToDataset: Bool
    public let maySelectFinalLabel: Bool
    public let isAuditOnly: Bool

    public init(_ interval: ModeISIInterval) {
        self.family = interval.family
        self.scope = interval.scope
        self.lowerSec = interval.lowerSec
        self.upperSec = interval.upperSec
        self.bridgeUpperSec = interval.bridgeUpperSec
        self.acceptanceLowerSec = interval.acceptanceLowerSec
        self.acceptanceUpperSec = interval.acceptanceUpperSec
        self.isValid = interval.isValid
        self.provenanceOrigin = interval.provenance.origin
        self.sourceStatistic = interval.provenance.sourceStatistic
        self.mayPropagateToDataset = interval.provenance.mayPropagateToDataset
        self.maySelectFinalLabel = interval.provenance.maySelectFinalLabel
        self.isAuditOnly = interval.provenance.isAuditOnly
    }
}

/// One histogram bar over an ISI range `[lowerSec, upperSec)`.
public struct ISIHistogramBin: Hashable, Sendable {
    public let lowerSec: Double
    public let upperSec: Double
    public let count: Int

    public init(lowerSec: Double, upperSec: Double, count: Int) {
        self.lowerSec = lowerSec
        self.upperSec = upperSec
        self.count = count
    }
}

/// A LOG-spaced histogram of pooled valid ISIs — the drawable distribution for the foundation chart
/// (ISIs span burst≈ms to pause≈100s of ms, so a log ISI axis is the natural view).
public struct ISIDistributionHistogram: Hashable, Sendable {
    public let bins: [ISIHistogramBin]
    public let totalCount: Int
    public let maxCount: Int
    public let minSec: Double   // domain lower (first bin lower)
    public let maxSec: Double   // domain upper (last bin upper)

    public init(bins: [ISIHistogramBin], totalCount: Int, maxCount: Int, minSec: Double, maxSec: Double) {
        self.bins = bins
        self.totalCount = totalCount
        self.maxCount = maxCount
        self.minSec = minSec
        self.maxSec = maxSec
    }

    /// Build a log-spaced histogram from valid (finite, positive) ISI values. Returns nil when there
    /// are fewer than two positive values or the range is degenerate (min == max).
    public static func logScale(values: [Double], binCount: Int = 40) -> ISIDistributionHistogram? {
        let positive = values.filter { $0.isFinite && $0 > 0 }
        guard positive.count >= 2, let minSec = positive.min(), let maxSec = positive.max(), maxSec > minSec else {
            return nil
        }
        let bins = max(1, binCount)
        let logMin = log10(minSec)
        let logMax = log10(maxSec)
        let step = (logMax - logMin) / Double(bins)
        var counts = [Int](repeating: 0, count: bins)
        for value in positive {
            let idx = min(bins - 1, max(0, Int((log10(value) - logMin) / step)))
            counts[idx] += 1
        }
        let histBins = (0..<bins).map { index in
            ISIHistogramBin(
                lowerSec: pow(10, logMin + Double(index) * step),
                upperSec: pow(10, logMin + Double(index + 1) * step),
                count: counts[index])
        }
        return ISIDistributionHistogram(
            bins: histBins, totalCount: positive.count, maxCount: counts.max() ?? 0,
            minSec: minSec, maxSec: maxSec)
    }

    /// Bin `values` into THIS histogram's existing (log) bin edges — for overlaying a single train's
    /// distribution on the pooled histogram with aligned bars. Returns one count per bin.
    public func counts(for values: [Double]) -> [Int] {
        let binCount = bins.count
        guard binCount > 0, maxSec > minSec else { return [Int](repeating: 0, count: binCount) }
        let logMin = log10(minSec)
        let step = (log10(maxSec) - logMin) / Double(binCount)
        var result = [Int](repeating: 0, count: binCount)
        for value in values where value.isFinite && value > 0 {
            let idx = min(binCount - 1, max(0, Int((log10(value) - logMin) / step)))
            result[idx] += 1
        }
        return result
    }
}

/// Per-train detail for the selected-train overlay: identity, counts binned onto the pooled histogram,
/// and the train-local D3 interval priors.
public struct ISIDistributionTrainDetail: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let validISICount: Int
    /// Per-train counts aligned to the POOLED histogram bins (same edges → overlay-aligned); empty when
    /// there is no pooled histogram.
    public let histogramCounts: [Int]
    /// Train-local D3 interval priors (burst, tonic, pause — only those present).
    public let intervals: [ISIModeIntervalRow]

    public init(trainID: String, trainName: String, validISICount: Int, histogramCounts: [Int], intervals: [ISIModeIntervalRow]) {
        self.trainID = trainID
        self.trainName = trainName
        self.validISICount = validISICount
        self.histogramCounts = histogramCounts
        self.intervals = intervals
    }
}

/// One TSW-2 tonic STRUCTURAL window candidate, flattened for display. TSW candidates are SEQUENCE-LOCAL
/// (ordered runs of ISIs), NOT ISI-distribution bands like the D3 priors — `startISIIndex/endISIIndex`
/// are the ordered sequence span, while `lowerSec/upperSec` are the candidate's ISI VALUE range (min–max)
/// used only to place it on the log-ISI magnitude axis. This is a debug/inspection row; TSW is unwired.
public struct TSWCandidateRow: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let startISIIndex: Int
    public let endISIIndex: Int
    public let startSpikeIndex: Int
    public let endSpikeIndex: Int
    public let isiCount: Int
    public let spikeCount: Int
    public let source: String            // TonicWindowSource raw value (seed / merged)
    public let route: String             // TSW-2A TonicStructuralWindowRoute raw value (classicTonic / HF / review)
    public let reviewRequired: Bool
    public let boundaryReason: String?   // TonicWindowBoundaryReason raw value, when expansion stopped
    public let decisionPath: String
    public let cv: Double?
    public let cv2: Double?
    public let lv: Double?
    public let medianSec: Double?
    /// Candidate ISI VALUE range (min / max within the span) — for the log-ISI magnitude axis only.
    public let lowerSec: Double?
    public let upperSec: Double?
    /// True when the candidate's ISI value range extends outside the D3 tonic ACCEPTANCE band (a
    /// distribution-vs-sequence mismatch worth review). False when there is no D3 tonic band to compare.
    public let outsideD3Acceptance: Bool

    public init(
        trainID: String, trainName: String, startISIIndex: Int, endISIIndex: Int,
        startSpikeIndex: Int, endSpikeIndex: Int, isiCount: Int, spikeCount: Int,
        source: String, route: String, reviewRequired: Bool, boundaryReason: String?, decisionPath: String,
        cv: Double?, cv2: Double?, lv: Double?, medianSec: Double?,
        lowerSec: Double?, upperSec: Double?, outsideD3Acceptance: Bool
    ) {
        self.trainID = trainID; self.trainName = trainName
        self.startISIIndex = startISIIndex; self.endISIIndex = endISIIndex
        self.startSpikeIndex = startSpikeIndex; self.endSpikeIndex = endSpikeIndex
        self.isiCount = isiCount; self.spikeCount = spikeCount
        self.source = source; self.route = route; self.reviewRequired = reviewRequired
        self.boundaryReason = boundaryReason; self.decisionPath = decisionPath
        self.cv = cv; self.cv2 = cv2; self.lv = lv; self.medianSec = medianSec
        self.lowerSec = lowerSec; self.upperSec = upperSec
        self.outsideD3Acceptance = outsideD3Acceptance
    }

    /// Flatten a TSW candidate for display, flagging a mismatch when its ISI value range extends outside
    /// the supplied D3 tonic acceptance band.
    public init(
        trainName: String, candidate: TonicStructuralWindowCandidate,
        d3TonicAcceptance: (lower: Double, upper: Double)?
    ) {
        let m = candidate.metrics
        var outside = false
        if let acc = d3TonicAcceptance, let lo = m.minSec, let hi = m.maxSec {
            outside = lo < acc.lower - 1e-12 || hi > acc.upper + 1e-12
        }
        self.init(
            trainID: candidate.span.trainID, trainName: trainName,
            startISIIndex: candidate.span.startISIIndex, endISIIndex: candidate.span.endISIIndex,
            startSpikeIndex: candidate.startSpikeIndex, endSpikeIndex: candidate.endSpikeIndex,
            isiCount: m.nISI, spikeCount: m.nSpikes,
            source: candidate.source.rawValue, route: candidate.route.rawValue, reviewRequired: candidate.reviewRequired,
            boundaryReason: candidate.boundaryReason?.rawValue, decisionPath: candidate.decisionPath,
            cv: m.cv, cv2: m.cv2, lv: m.lv, medianSec: m.medianSec,
            lowerSec: m.minSec, upperSec: m.maxSec, outsideD3Acceptance: outside)
    }
}

/// Everything the "Distribution-first foundation (D1-D3)" view needs, already shaped.
public struct ISIDistributionFoundationPresentation: Hashable, Sendable {
    /// Whether the distribution came from the detection run (D2-wired) or a fallback compute.
    public enum Source: String, Hashable, Sendable {
        case detectionRun          // read from run.datasetISIDistribution (band floor)
        case computedFromDataset   // fallback: DatasetISIDistributionService.compute
    }

    public let datasetName: String
    public let source: Source
    public let floorSec: Double
    public let contributingTrainCount: Int
    public let pooledValidISICount: Int
    public let pooled: ISIDistributionQuantileRow
    public let trainBalanced: ISIDistributionQuantileRow?
    public let perTrain: [ISIDistributionQuantileRow]
    /// Dataset-scope D3 priors (burst, then tonic, then pause — only those present).
    public let datasetIntervals: [ISIModeIntervalRow]
    /// Log-ISI histogram of the pooled valid ISIs (nil when too few / degenerate).
    public let histogram: ISIDistributionHistogram?
    /// Per-train overlay details (counts aligned to the pooled histogram + train-local D3 priors), one
    /// per contributing train, in `perTrain` order.
    public let perTrainDetails: [ISIDistributionTrainDetail]
    /// TSW-2 tonic structural window candidates per train (keyed by `SpikeTrain.id` == the detail
    /// `trainID`). Debug-only + UNWIRED — nothing consumes these; empty when computed without the dataset
    /// (e.g. via `make` directly) or when a train has no candidate.
    public let tswCandidatesByTrainID: [String: [TSWCandidateRow]]

    public init(
        datasetName: String, source: Source, floorSec: Double,
        contributingTrainCount: Int, pooledValidISICount: Int,
        pooled: ISIDistributionQuantileRow, trainBalanced: ISIDistributionQuantileRow?,
        perTrain: [ISIDistributionQuantileRow], datasetIntervals: [ISIModeIntervalRow],
        histogram: ISIDistributionHistogram? = nil,
        perTrainDetails: [ISIDistributionTrainDetail] = [],
        tswCandidatesByTrainID: [String: [TSWCandidateRow]] = [:]
    ) {
        self.datasetName = datasetName; self.source = source; self.floorSec = floorSec
        self.contributingTrainCount = contributingTrainCount; self.pooledValidISICount = pooledValidISICount
        self.pooled = pooled; self.trainBalanced = trainBalanced
        self.perTrain = perTrain; self.datasetIntervals = datasetIntervals
        self.histogram = histogram
        self.perTrainDetails = perTrainDetails
        self.tswCandidatesByTrainID = tswCandidatesByTrainID
    }

    /// Flatten a family-interval bundle into display rows (burst, then tonic, then pause — present only).
    private static func intervalRows(_ families: FamilyModeIntervals?) -> [ISIModeIntervalRow] {
        guard let families else { return [] }
        return [families.burst, families.tonic, families.pause].compactMap { $0 }.map(ISIModeIntervalRow.init)
    }

    /// The given trains in stable `perTrainDetails` order (i.e. stacking order + color index for a
    /// multi-train composition), silently dropping ids not present in this presentation. Pure, so a
    /// stacked-overlay control can rely on a deterministic order and stay consistent with a stale or
    /// partial selection after a dataset swap.
    public func selectedTrainDetails(_ trainIDs: Set<String>) -> [ISIDistributionTrainDetail] {
        perTrainDetails.filter { trainIDs.contains($0.trainID) }
    }

    /// Resolve the effective FOCUS train (for detailed prior inspection) from a `requested` id,
    /// constrained to the selected set: the requested train if it is currently selected, otherwise the
    /// first selected train (stable `perTrainDetails` order), or nil when nothing is selected. Pure so
    /// the "focus defaults to first selected, and falls back when its train is deselected" rule is
    /// unit-testable independent of the view.
    public func focusedTrainDetail(requested: String?, within selectedIDs: Set<String>) -> ISIDistributionTrainDetail? {
        let selected = selectedTrainDetails(selectedIDs)
        if let requested, let match = selected.first(where: { $0.trainID == requested }) { return match }
        return selected.first
    }

    /// Shape an already-computed distribution + derived intervals into presentation rows.
    public static func make(
        distribution: DatasetISIDistribution,
        derived: DerivedModeISIIntervals,
        source: Source,
        floorSec: Double,
        tswCandidatesByTrainID: [String: [TSWCandidateRow]] = [:]
    ) -> ISIDistributionFoundationPresentation {
        let pooled = ISIDistributionQuantileRow.from(
            label: "Pooled", trainID: nil, count: distribution.pooledValidISICount,
            quantiles: distribution.pooledQuantiles)
        let balanced = distribution.trainBalancedQuantiles.map {
            ISIDistributionQuantileRow.from(
                label: "Train-balanced", trainID: nil, count: $0.count, quantiles: $0)
        }
        let perTrain = distribution.trainDistributions.map { train in
            ISIDistributionQuantileRow.from(
                label: train.trainName, trainID: train.trainID, count: train.validISICount,
                quantiles: train.quantiles)
        }
        let intervals = intervalRows(derived.dataset)

        let pooledValues = distribution.trainDistributions.flatMap(\.validISIValuesSec)
        let histogram = ISIDistributionHistogram.logScale(values: pooledValues)

        // Per-train overlay details: bin each train's ISIs onto the pooled histogram edges (so bars align)
        // and carry its train-local D3 priors — answering "does this train have a prior the dataset lacks?"
        let perTrainDetails = distribution.trainDistributions.map { train in
            ISIDistributionTrainDetail(
                trainID: train.trainID, trainName: train.trainName, validISICount: train.validISICount,
                histogramCounts: histogram?.counts(for: train.validISIValuesSec) ?? [],
                intervals: intervalRows(derived.perTrain[train.trainID]))
        }

        return ISIDistributionFoundationPresentation(
            datasetName: distribution.datasetName, source: source, floorSec: floorSec,
            contributingTrainCount: distribution.contributingTrainCount,
            pooledValidISICount: distribution.pooledValidISICount,
            pooled: pooled, trainBalanced: balanced, perTrain: perTrain, datasetIntervals: intervals,
            histogram: histogram, perTrainDetails: perTrainDetails,
            tswCandidatesByTrainID: tswCandidatesByTrainID)
    }

    /// Run the TSW-2 tonic structural window scan per train (debug-only, UNWIRED). Uses the D3 train-local
    /// burst bridge valley as the contamination floor when present, else TSW's refractory fallback. Does
    /// NOT seed from any global tonic band. Flags each candidate whose ISI value range extends outside the
    /// train's D3 tonic acceptance band.
    public static func tswCandidates(
        dataset: SpikeDataset, derived: DerivedModeISIIntervals, minimumValidISISec: Double
    ) -> [String: [TSWCandidateRow]] {
        // Use the dataset's QC floor as BOTH the metric floor and the TSW refractory floor, so the
        // physiological classic-tonic floor (classicTonicMinRefractoryMultiple × refractoryFloorSec) is
        // computed from the same QC floor shown in the distribution-first debug view (not the 0.001 default).
        let config = TonicStructuralWindowConfig(
            thresholds: StructuralEvidenceThresholds(minimumValidISISec: minimumValidISISec),
            refractoryFloorSec: minimumValidISISec)
        var byTrainID: [String: [TSWCandidateRow]] = [:]
        for train in dataset.trains {
            let family = derived.perTrain[train.id]
            let burstValley = family?.burst?.bridgeUpperSec
            // TSW-2A magnitude-guard inputs (train-local D3 priors): the burst valley's support count (so a
            // degenerate single-point valley is not trusted) and the tonic CORE lower (q25) as the fallback
            // classic-tonic floor, else the dataset tonic core lower. Scale-free; no absolute-ms literal.
            let burstSupport = family?.burst?.supportCount
            let tonicCoreLower = family?.tonic?.lowerSec ?? derived.dataset.tonic?.lowerSec
            let candidates = TonicStructuralWindowDetector.scan(
                train: train, config: config, burstValleySec: burstValley,
                burstValleySupportCount: burstSupport, fallbackFloorSec: tonicCoreLower)
            guard !candidates.isEmpty else { continue }
            let tonic = family?.tonic ?? derived.dataset.tonic
            let acceptance = tonic.map { (lower: $0.effectiveAcceptanceLowerSec, upper: $0.effectiveAcceptanceUpperSec) }
            byTrainID[train.id] = candidates.map {
                TSWCandidateRow(trainName: train.name, candidate: $0, d3TonicAcceptance: acceptance)
            }
        }
        return byTrainID
    }

    /// End-to-end convenience for the view: prefer the D2-wired `runDistribution`; otherwise compute
    /// from the dataset at `minimumValidISISec`. Then derive D3 priors and shape everything.
    public static func from(
        dataset: SpikeDataset,
        runDistribution: DatasetISIDistribution?,
        minimumValidISISec: Double
    ) -> ISIDistributionFoundationPresentation {
        let distribution = runDistribution
            ?? DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: minimumValidISISec)
        let derived = ModeISIIntervalDeriver.derive(datasetDistribution: distribution, minimumValidISISec: minimumValidISISec)
        let source: Source = runDistribution != nil ? .detectionRun : .computedFromDataset
        let tsw = tswCandidates(dataset: dataset, derived: derived, minimumValidISISec: minimumValidISISec)
        return make(distribution: distribution, derived: derived, source: source, floorSec: minimumValidISISec,
                    tswCandidatesByTrainID: tsw)
    }
}
