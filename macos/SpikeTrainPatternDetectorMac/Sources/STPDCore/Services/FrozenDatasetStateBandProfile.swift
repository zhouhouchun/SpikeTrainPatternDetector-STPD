import Foundation

/// A contiguous, scale-free stable run found directly in a train's raw ISI order.
///
/// ISI indices are inclusive indices into `SpikeTrain.isiSec`; index zero is never
/// eligible because it is the structural leading placeholder. The run never spans
/// an invalid or sub-QC-floor ISI slot.
public struct FrozenStableISIRun: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let startISIIndex: Int
    public let endISIIndex: Int
    public let isiCount: Int
    public let spikeCount: Int
    public let medianISISec: Double
    public let q10ISISec: Double
    public let q90ISISec: Double
    public let cv: Double
    public let lv: Double
    public let medianBandFraction: Double
    public let adjacentRatioPassFraction: Double
    /// Number of qualifying minimum-length windows merged into this run.
    public let sourceWindowCount: Int

    public var span: ISISpan {
        ISISpan(trainID: trainID, startISIIndex: startISIIndex, endISIIndex: endISIIndex)
    }
}

/// Per-train raw evidence used by the frozen dataset summary.
public struct FrozenTrainStateBandProfile: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    /// Number of first-pass >=29-ISI stable runs before frozen-band assignment.
    public let rawStableRunCount: Int
    /// This train's one train-balanced contribution to the dataset freeze: the
    /// lowest log-median among its first-pass stable runs.
    public let baseCenterLogContribution: Double?
    /// First-pass stable runs retained as base support because their medians lie
    /// within the frozen dataset center divided/multiplied by 1.5.
    public let sustainedStableRuns: [FrozenStableISIRun]
    /// Descriptive center of this train's retained base-support runs. It does not
    /// feed back into the already frozen dataset center.
    public let sustainedBandLogCenter: Double?
    /// Non-overlapping stable runs whose median is at least twice the frozen
    /// dataset base center. Base-support spans are removed before this scan, so
    /// sustained and slower roles cannot reuse an ISI.
    public let slowerStableSupport: [FrozenStableISIRun]
    /// The median of this train's slower-support log medians.
    public let slowerStableBandLogCenter: Double?

    public var sustainedBandCenterSec: Double? {
        sustainedBandLogCenter.map(Foundation.exp)
    }

    public var slowerStableBandCenterSec: Double? {
        slowerStableBandLogCenter.map(Foundation.exp)
    }
}

/// Necessary relative-scale evidence for later HFS adjudication. Eligibility is
/// never sufficient to assign an HFS label: duration, independent non-burst
/// support, interruption budgets, and arbitration remain downstream obligations.
public enum RelativeHFSAdjudicationEligibility: String, Hashable, Sendable {
    case eligibleWithDatasetScaleSeparation
    case abstainNoFrozenBaseBand
    case abstainNoDatasetBaseSupport
    case abstainNoDatasetSlowerStableSupport
}

/// Once-computed, raw-data-only dataset time-scale evidence.
///
/// The profile reads no truth labels, classes, detector candidates, or arbitration
/// results. Dataset summaries are train-balanced and order-independent: one robust
/// center per contributing train is used, regardless of train length or run count.
public struct FrozenDatasetStateBandProfile: Hashable, Sendable {
    public let minimumValidISISec: Double
    /// Sorted by the unique IDs already present in this `SpikeDataset`. This makes
    /// permutations of authoritative unique IDs order-independent. It deliberately
    /// does not claim to repair identity across separately rebuilt datasets whose
    /// duplicate input names receive order-dependent `#N` suffixes upstream.
    public let trains: [FrozenTrainStateBandProfile]
    /// Frozen exactly once from one lowest-run log-median per contributing train.
    public let sustainedBandLogCenter: Double?
    /// Scaled median absolute deviation (1.4826 * MAD) in natural-log seconds.
    public let sustainedBandLogDispersion: Double?
    public let baseCenterContributingTrainCount: Int
    public let sustainedBaseSupportTrainCount: Int
    public let slowerStableBandLogCenter: Double?
    /// Scaled median absolute deviation (1.4826 * MAD) in natural-log seconds.
    public let slowerStableBandLogDispersion: Double?
    public let slowerStableContributingTrainCount: Int
    public let relativeHFSAdjudicationEligibility: RelativeHFSAdjudicationEligibility

    public var sustainedBandCenterSec: Double? {
        sustainedBandLogCenter.map(Foundation.exp)
    }

    public var slowerStableBandCenterSec: Double? {
        slowerStableBandLogCenter.map(Foundation.exp)
    }

    /// Pure construction from original train-order ISIs and one explicit QC floor.
    /// The dimensionless scientific gates are fixed constants documented on the
    /// profiler, so results cannot silently depend on detector settings. Positive
    /// scale equivariance holds only while scaling leaves the fixed-floor valid/
    /// invalid ISI mask unchanged; crossing an absolute QC floor must change it.
    public static func compute(
        dataset: SpikeDataset,
        minimumValidISISec: Double
    ) -> FrozenDatasetStateBandProfile {
        FrozenDatasetStateBandProfiler.compute(
            dataset: dataset,
            minimumValidISISec: minimumValidISISec
        )
    }
}

/// Raw, scale-free state-band evidence extraction. The canonical pipeline computes
/// this service once before detector candidates exist and passes the immutable
/// snapshot only to the separately gated relative-HFS route.
public enum FrozenDatasetStateBandProfiler {
    public static let sustainedMinimumISICount = 29
    public static let slowerMinimumISICount = 3
    public static let maximumCV = 0.30
    public static let maximumLV = 0.12
    public static let medianBandRatio = 1.5
    public static let minimumMedianBandFraction = 0.80
    public static let maximumAdjacentRatio = 1.5
    public static let minimumAdjacentRatioPassFraction = 0.70
    public static let minimumSlowerCenterRatio = 2.0

    public static func compute(
        dataset: SpikeDataset,
        minimumValidISISec: Double
    ) -> FrozenDatasetStateBandProfile {
        precondition(
            minimumValidISISec.isFinite && minimumValidISISec >= 0,
            "minimumValidISISec must be finite and non-negative"
        )

        // Pass 1: raw evidence only. Each train contributes its lowest stable-run
        // log median exactly once; no slower classification exists yet.
        let firstPass = dataset.trains.map {
            firstPassTrain(train: $0, minimumValidISISec: minimumValidISISec)
        }
        let baseContributions = firstPass.compactMap(\.baseCenterLogContribution)
        let frozenBaseSummary = robustLogSummary(baseContributions)

        // Pass 2: use the once-frozen dataset center without feedback. Base and
        // slower roles are materialized on mutually exclusive ISI spans.
        let profiles = firstPass
            .map { secondPassTrain(firstPass: $0, frozenBaseLogCenter: frozenBaseSummary?.center) }
            .sorted {
                if $0.trainID == $1.trainID {
                    return $0.trainName < $1.trainName
                }
                return $0.trainID < $1.trainID
            }

        let slowerTrainCenters = profiles.compactMap(\.slowerStableBandLogCenter)
        let slowerSummary = robustLogSummary(slowerTrainCenters)
        let baseSupportTrainCount = profiles.filter { !$0.sustainedStableRuns.isEmpty }.count

        let eligibility: RelativeHFSAdjudicationEligibility
        if frozenBaseSummary == nil {
            eligibility = .abstainNoFrozenBaseBand
        } else if baseSupportTrainCount == 0 {
            eligibility = .abstainNoDatasetBaseSupport
        } else if slowerTrainCenters.isEmpty {
            eligibility = .abstainNoDatasetSlowerStableSupport
        } else {
            eligibility = .eligibleWithDatasetScaleSeparation
        }

        return FrozenDatasetStateBandProfile(
            minimumValidISISec: minimumValidISISec,
            trains: profiles,
            sustainedBandLogCenter: frozenBaseSummary?.center,
            sustainedBandLogDispersion: frozenBaseSummary?.dispersion,
            baseCenterContributingTrainCount: baseContributions.count,
            sustainedBaseSupportTrainCount: baseSupportTrainCount,
            slowerStableBandLogCenter: slowerSummary?.center,
            slowerStableBandLogDispersion: slowerSummary?.dispersion,
            slowerStableContributingTrainCount: slowerTrainCenters.count,
            relativeHFSAdjudicationEligibility: eligibility
        )
    }

    // MARK: - Per-train extraction

    private struct IndexedISI {
        let index: Int
        let value: Double
    }

    private struct RunMetrics {
        let median: Double
        let q10: Double
        let q90: Double
        let cv: Double
        let lv: Double
        let medianBandFraction: Double
        let adjacentRatioPassFraction: Double
    }

    private struct QualifiedSeed {
        let start: Int
        let end: Int
        let metrics: RunMetrics
    }

    private struct FirstPassTrain {
        let train: SpikeTrain
        let validBlocks: [[IndexedISI]]
        let rawStableRuns: [FrozenStableISIRun]
        let baseCenterLogContribution: Double?
    }

    private static func firstPassTrain(
        train: SpikeTrain,
        minimumValidISISec: Double
    ) -> FirstPassTrain {
        let blocks = validBlocks(train: train, minimumValidISISec: minimumValidISISec)
        let rawStableRuns = stableRuns(
            in: blocks,
            train: train,
            minimumCount: sustainedMinimumISICount,
            minimumMedianSec: nil
        )
        return FirstPassTrain(
            train: train,
            validBlocks: blocks,
            rawStableRuns: rawStableRuns,
            baseCenterLogContribution: rawStableRuns
                .map { Foundation.log($0.medianISISec) }
                .min()
        )
    }

    private static func secondPassTrain(
        firstPass: FirstPassTrain,
        frozenBaseLogCenter: Double?
    ) -> FrozenTrainStateBandProfile {
        let baseRuns: [FrozenStableISIRun]
        let slower: [FrozenStableISIRun]
        if let frozenBaseLogCenter {
            let frozenBaseCenterSec = Foundation.exp(frozenBaseLogCenter)
            let lower = frozenBaseCenterSec / medianBandRatio
            let upper = frozenBaseCenterSec * medianBandRatio
            baseRuns = firstPass.rawStableRuns.filter {
                $0.medianISISec >= lower && $0.medianISISec <= upper
            }
            let slowerBlocks = excluding(
                spans: baseRuns,
                from: firstPass.validBlocks
            )
            slower = stableRuns(
                in: slowerBlocks,
                train: firstPass.train,
                minimumCount: slowerMinimumISICount,
                minimumMedianSec: frozenBaseCenterSec * minimumSlowerCenterRatio
            )
        } else {
            baseRuns = []
            slower = []
        }

        return FrozenTrainStateBandProfile(
            trainID: firstPass.train.id,
            trainName: firstPass.train.name,
            rawStableRunCount: firstPass.rawStableRuns.count,
            baseCenterLogContribution: firstPass.baseCenterLogContribution,
            sustainedStableRuns: baseRuns,
            sustainedBandLogCenter: median(baseRuns.map { Foundation.log($0.medianISISec) }),
            slowerStableSupport: slower,
            slowerStableBandLogCenter: median(slower.map { Foundation.log($0.medianISISec) })
        )
    }

    /// Split at every invalid slot. In addition to the explicit QC floor, zero
    /// and negative intervals are structurally invalid even when the floor is 0.
    private static func validBlocks(
        train: SpikeTrain,
        minimumValidISISec: Double
    ) -> [[IndexedISI]] {
        guard train.isiSec.count > 1 else {
            return []
        }

        var blocks: [[IndexedISI]] = []
        var current: [IndexedISI] = []
        for index in 1..<train.isiSec.count {
            if let value = train.isiSec[index],
               value.isFinite,
               value > 0,
               value >= minimumValidISISec {
                current.append(IndexedISI(index: index, value: value))
            } else if !current.isEmpty {
                blocks.append(current)
                current = []
            }
        }
        if !current.isEmpty {
            blocks.append(current)
        }
        return blocks
    }

    /// Remove every retained base-support ISI before the independent slower scan.
    /// The spans are non-overlapping, but sorting here keeps the helper defensive
    /// and makes membership linear in block length plus span count.
    private static func excluding(
        spans: [FrozenStableISIRun],
        from blocks: [[IndexedISI]]
    ) -> [[IndexedISI]] {
        let excluded = spans
            .map { $0.startISIIndex...$0.endISIIndex }
            .sorted { $0.lowerBound < $1.lowerBound }
        guard !excluded.isEmpty else {
            return blocks
        }

        var result: [[IndexedISI]] = []
        var current: [IndexedISI] = []
        var rangeIndex = 0

        for block in blocks {
            for isi in block {
                while rangeIndex < excluded.count,
                      excluded[rangeIndex].upperBound < isi.index {
                    rangeIndex += 1
                }
                let isExcluded = rangeIndex < excluded.count
                    && excluded[rangeIndex].contains(isi.index)
                if isExcluded {
                    if !current.isEmpty {
                        result.append(current)
                        current = []
                    }
                } else {
                    current.append(isi)
                }
            }
            if !current.isEmpty {
                result.append(current)
                current = []
            }
        }
        return result
    }

    /// Find qualifying minimum-length windows and merge each connected overlap
    /// cluster when the cluster's complete envelope remains qualified. If a
    /// gradually drifting cluster has a non-qualified envelope, retain conservative
    /// non-overlapping qualifying seeds instead. The fixed-size seed scan is linear
    /// in train length; each cluster envelope is evaluated only once.
    private static func stableRuns(
        in blocks: [[IndexedISI]],
        train: SpikeTrain,
        minimumCount: Int,
        minimumMedianSec: Double?
    ) -> [FrozenStableISIRun] {
        var result: [FrozenStableISIRun] = []

        for block in blocks where block.count >= minimumCount {
            var seeds: [QualifiedSeed] = []
            seeds.reserveCapacity(block.count - minimumCount + 1)
            for seedStart in 0...(block.count - minimumCount) {
                let seedEnd = seedStart + minimumCount - 1
                let seedValues = values(in: block, from: seedStart, through: seedEnd)
                if let seedMetrics = qualifiedMetrics(
                    seedValues,
                    minimumMedianSec: minimumMedianSec
                ) {
                    seeds.append(QualifiedSeed(start: seedStart, end: seedEnd, metrics: seedMetrics))
                }
            }

            var clusterStart = 0
            while clusterStart < seeds.count {
                var clusterEnd = clusterStart
                var envelopeEnd = seeds[clusterStart].end
                while clusterEnd + 1 < seeds.count,
                      seeds[clusterEnd + 1].start <= envelopeEnd {
                    clusterEnd += 1
                    envelopeEnd = max(envelopeEnd, seeds[clusterEnd].end)
                }

                let envelopeStart = seeds[clusterStart].start
                let envelopeValues = values(
                    in: block,
                    from: envelopeStart,
                    through: envelopeEnd
                )
                if let envelopeMetrics = qualifiedMetrics(
                    envelopeValues,
                    minimumMedianSec: minimumMedianSec
                ) {
                    result.append(
                        makeRun(
                            train: train,
                            block: block,
                            startOffset: envelopeStart,
                            endOffset: envelopeEnd,
                            metrics: envelopeMetrics,
                            sourceWindowCount: clusterEnd - clusterStart + 1
                        )
                    )
                } else {
                    // A connected chain of individually stable windows can drift
                    // enough that the complete envelope is not a stable state.
                    // Preserve only disjoint qualified seeds; never double-count
                    // overlapping evidence or manufacture a drifting run.
                    var lastEmittedEnd = -1
                    for seed in seeds[clusterStart...clusterEnd] where seed.start > lastEmittedEnd {
                        result.append(
                            makeRun(
                                train: train,
                                block: block,
                                startOffset: seed.start,
                                endOffset: seed.end,
                                metrics: seed.metrics,
                                sourceWindowCount: 1
                            )
                        )
                        lastEmittedEnd = seed.end
                    }
                }
                clusterStart = clusterEnd + 1
            }
        }

        return result
    }

    private static func values(
        in block: [IndexedISI],
        from start: Int,
        through end: Int
    ) -> [Double] {
        block[start...end].map(\.value)
    }

    private static func qualifiedMetrics(
        _ values: [Double],
        minimumMedianSec: Double?
    ) -> RunMetrics? {
        guard values.count >= 2 else {
            return nil
        }
        let sample = SortedFiniteSample(values, positiveOnly: true)
        guard sample.count == values.count,
              let median = sample.quantile(0.5),
              let q10 = sample.quantile(0.1),
              let q90 = sample.quantile(0.9),
              let cv = STPDStatistics.coefficientOfVariation(values),
              let lv = STPDStatistics.localVariation(values) else {
            return nil
        }
        if let minimumMedianSec, median < minimumMedianSec {
            return nil
        }

        let lower = median / medianBandRatio
        let upper = median * medianBandRatio
        let medianBandFraction = Double(values.filter { $0 >= lower && $0 <= upper }.count)
            / Double(values.count)
        let adjacentPairs = zip(values, values.dropFirst())
        let adjacentPairCount = values.count - 1
        let adjacentPassCount = adjacentPairs.filter { previous, next in
            max(previous, next) / min(previous, next) <= maximumAdjacentRatio
        }.count
        let adjacentRatioPassFraction = Double(adjacentPassCount) / Double(adjacentPairCount)

        guard cv <= maximumCV,
              lv <= maximumLV,
              medianBandFraction >= minimumMedianBandFraction,
              adjacentRatioPassFraction >= minimumAdjacentRatioPassFraction else {
            return nil
        }

        return RunMetrics(
            median: median,
            q10: q10,
            q90: q90,
            cv: cv,
            lv: lv,
            medianBandFraction: medianBandFraction,
            adjacentRatioPassFraction: adjacentRatioPassFraction
        )
    }

    private static func makeRun(
        train: SpikeTrain,
        block: [IndexedISI],
        startOffset: Int,
        endOffset: Int,
        metrics: RunMetrics,
        sourceWindowCount: Int
    ) -> FrozenStableISIRun {
        let count = endOffset - startOffset + 1
        return FrozenStableISIRun(
            trainID: train.id,
            trainName: train.name,
            startISIIndex: block[startOffset].index,
            endISIIndex: block[endOffset].index,
            isiCount: count,
            spikeCount: count + 1,
            medianISISec: metrics.median,
            q10ISISec: metrics.q10,
            q90ISISec: metrics.q90,
            cv: metrics.cv,
            lv: metrics.lv,
            medianBandFraction: metrics.medianBandFraction,
            adjacentRatioPassFraction: metrics.adjacentRatioPassFraction,
            sourceWindowCount: sourceWindowCount
        )
    }

    // MARK: - Train-balanced frozen summaries

    private static func robustLogSummary(
        _ oneCenterPerTrain: [Double]
    ) -> (center: Double, dispersion: Double)? {
        guard let center = median(oneCenterPerTrain) else {
            return nil
        }
        let absoluteDeviations = oneCenterPerTrain.map { abs($0 - center) }
        let mad = median(absoluteDeviations) ?? 0
        return (center, 1.4826 * mad)
    }

    private static func median(_ values: [Double]) -> Double? {
        SortedFiniteSample(values).quantile(0.5)
    }
}
