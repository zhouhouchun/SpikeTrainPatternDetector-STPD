import Foundation

public enum AdaptiveBandPattern: String, CaseIterable, Sendable {
    case burst
    case highFrequencySpiking = "high_frequency_spiking"
    case highFrequencyTonic = "high_frequency_tonic"
    case tonic
    case pause
}

public enum AdaptiveBandField: String, CaseIterable, Sendable {
    case seedLowerSec = "seed_lower_sec"
    case seedUpperSec = "seed_upper_sec"
    case bridgeUpperSec = "bridge_upper_sec"
    case contrastS = "contrast_S"
}

public enum AdaptiveBandSource: String, Sendable {
    case histogram
    case structure
    case `default`
    case none
}

public struct AdaptiveBand: Sendable {
    public let pattern: AdaptiveBandPattern
    public let seedLowerSec: Double
    public let seedUpperSec: Double
    public let bridgeUpperSec: Double
    public let contrastS: Double?
    public let primarySource: AdaptiveBandSource

    public init(
        pattern: AdaptiveBandPattern,
        seedLowerSec: Double,
        seedUpperSec: Double,
        bridgeUpperSec: Double,
        contrastS: Double?,
        primarySource: AdaptiveBandSource
    ) {
        self.pattern = pattern
        self.seedLowerSec = seedLowerSec
        self.seedUpperSec = seedUpperSec
        self.bridgeUpperSec = bridgeUpperSec
        self.contrastS = contrastS
        self.primarySource = primarySource
    }
}

public struct AdaptiveBandThresholdRow: Identifiable, Sendable {
    public let id: String
    public let pattern: AdaptiveBandPattern
    public let field: AdaptiveBandField
    public let histogramSec: Double?
    public let defaultSec: Double?
    public let effectiveSec: Double?
    public let source: AdaptiveBandSource

    public init(
        pattern: AdaptiveBandPattern,
        field: AdaptiveBandField,
        histogramSec: Double?,
        defaultSec: Double?,
        effectiveSec: Double?,
        source: AdaptiveBandSource
    ) {
        self.id = "\(pattern.rawValue)-\(field.rawValue)"
        self.pattern = pattern
        self.field = field
        self.histogramSec = histogramSec
        self.defaultSec = defaultSec
        self.effectiveSec = effectiveSec
        self.source = source
    }
}

public struct TrainSeedBandProfile: Hashable, Sendable {
    public let nValidISI: Int
    public let datasetISISeedLowSec: Double
    public let datasetISISeedHighSec: Double
    public let datasetISIBridgeHighSec: Double
    public let datasetISIBoundaryFloorSec: Double
    public let seedLowPercentileInTrain: Double?
    public let seedHighPercentileInTrain: Double?
    public let seedBandFraction: Double?
    public let seedRunCount: Int
    public let maxSeedRunLength: Int
    public let medianISISec: Double?
    public let pauseFraction: Double?
    public let phenotypePrior: String

    public init(
        nValidISI: Int,
        datasetISISeedLowSec: Double,
        datasetISISeedHighSec: Double,
        datasetISIBridgeHighSec: Double,
        datasetISIBoundaryFloorSec: Double,
        seedLowPercentileInTrain: Double?,
        seedHighPercentileInTrain: Double?,
        seedBandFraction: Double?,
        seedRunCount: Int,
        maxSeedRunLength: Int,
        medianISISec: Double?,
        pauseFraction: Double?,
        phenotypePrior: String
    ) {
        self.nValidISI = nValidISI
        self.datasetISISeedLowSec = datasetISISeedLowSec
        self.datasetISISeedHighSec = datasetISISeedHighSec
        self.datasetISIBridgeHighSec = datasetISIBridgeHighSec
        self.datasetISIBoundaryFloorSec = datasetISIBoundaryFloorSec
        self.seedLowPercentileInTrain = seedLowPercentileInTrain
        self.seedHighPercentileInTrain = seedHighPercentileInTrain
        self.seedBandFraction = seedBandFraction
        self.seedRunCount = seedRunCount
        self.maxSeedRunLength = maxSeedRunLength
        self.medianISISec = medianISISec
        self.pauseFraction = pauseFraction
        self.phenotypePrior = phenotypePrior
    }
}

public enum StructuralSeedSummaryOrigin: String, Hashable, Sendable {
    /// Structural seed prior derived solely from this train's own detected candidates.
    case trainLocal = "train_local_structural_seed"
    /// This train's structural seed prior after a dataset-level summary was blended in.
    case datasetApplied = "dataset_applied_structural_seed"
}

public struct StructuralSeedBandSummary: Hashable, Sendable {
    public let burstAnchorCount: Int
    public let burstSupportWeight: Double
    public let burstSeedUpperSec: Double?
    public let burstBridgeUpperSec: Double?
    public let tonicAnchorCount: Int
    public let tonicSupportWeight: Double
    public let tonicSeedLowerSec: Double?
    public let tonicSeedUpperSec: Double?
    public let pauseAnchorCount: Int
    public let pausePoolAnchorCount: Int
    public let pauseSupportWeight: Double
    public let pausePoolSupportWeight: Double
    public let pauseSeedLowerSec: Double?
    public let pauseSeedUpperSec: Double?
    public let pausePoolSource: String
    public let source: String
    /// Whether this summary is purely train-local or the result of applying a
    /// dataset-level summary. Provenance only; not read by detection logic.
    public let origin: StructuralSeedSummaryOrigin
    /// True when the dataset summary blended into this train's prior was computed over
    /// a set that included this same train (self-inclusion). Always false for a
    /// purely train-local summary. Provenance only.
    public let datasetSummaryIncludedTargetTrain: Bool
    /// Phase 2A dataset-aggregation firewall. Per-family eligibility of this train's
    /// train-local band to enter the DATASET seed aggregate. Defaults to `true`, so
    /// train-local behavior is unchanged; only `StructuralSeedBandResolver.summarize()`
    /// sets a family `false` when its band came from non-conserved / weak evidence
    /// (weak possibleBurst fallback, structural-window tonic fill, audit-only pause prior).
    /// Read only by `StructuralDatasetSeedAggregator.aggregate()`; never affects the
    /// train-local band, `refineBands`, or `applyingDatasetSummary`.
    public let isBurstSeedDatasetAggregatable: Bool
    public let isTonicSeedDatasetAggregatable: Bool
    public let isPauseSeedDatasetAggregatable: Bool

    public init(
        burstAnchorCount: Int = 0,
        burstSupportWeight: Double? = nil,
        burstSeedUpperSec: Double? = nil,
        burstBridgeUpperSec: Double? = nil,
        tonicAnchorCount: Int = 0,
        tonicSupportWeight: Double? = nil,
        tonicSeedLowerSec: Double? = nil,
        tonicSeedUpperSec: Double? = nil,
        pauseAnchorCount: Int = 0,
        pausePoolAnchorCount: Int = 0,
        pauseSupportWeight: Double? = nil,
        pausePoolSupportWeight: Double? = nil,
        pauseSeedLowerSec: Double? = nil,
        pauseSeedUpperSec: Double? = nil,
        pausePoolSource: String = "none",
        source: String = "none",
        origin: StructuralSeedSummaryOrigin = .trainLocal,
        datasetSummaryIncludedTargetTrain: Bool = false,
        isBurstSeedDatasetAggregatable: Bool = true,
        isTonicSeedDatasetAggregatable: Bool = true,
        isPauseSeedDatasetAggregatable: Bool = true
    ) {
        self.burstAnchorCount = max(0, burstAnchorCount)
        self.burstSupportWeight = Self.cleanWeight(
            burstSupportWeight ?? Self.supportWeight(anchorCount: burstAnchorCount, shrinkage: 4)
        )
        self.burstSeedUpperSec = burstSeedUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.burstBridgeUpperSec = burstBridgeUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.tonicAnchorCount = max(0, tonicAnchorCount)
        self.tonicSupportWeight = Self.cleanWeight(
            tonicSupportWeight ?? Self.supportWeight(anchorCount: tonicAnchorCount, shrinkage: 4)
        )
        self.tonicSeedLowerSec = tonicSeedLowerSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.tonicSeedUpperSec = tonicSeedUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.pauseAnchorCount = max(0, pauseAnchorCount)
        self.pausePoolAnchorCount = max(0, pausePoolAnchorCount)
        self.pauseSupportWeight = Self.cleanWeight(
            pauseSupportWeight ?? Self.supportWeight(
                anchorCount: pauseAnchorCount + pausePoolAnchorCount,
                shrinkage: 3
            )
        )
        self.pausePoolSupportWeight = Self.cleanWeight(
            pausePoolSupportWeight ?? Self.supportWeight(anchorCount: pausePoolAnchorCount, shrinkage: 3)
        )
        self.pauseSeedLowerSec = pauseSeedLowerSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.pauseSeedUpperSec = pauseSeedUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.pausePoolSource = pausePoolSource
        self.source = source
        self.origin = origin
        self.datasetSummaryIncludedTargetTrain = datasetSummaryIncludedTargetTrain
        self.isBurstSeedDatasetAggregatable = isBurstSeedDatasetAggregatable
        self.isTonicSeedDatasetAggregatable = isTonicSeedDatasetAggregatable
        self.isPauseSeedDatasetAggregatable = isPauseSeedDatasetAggregatable
    }

    public static let empty = StructuralSeedBandSummary()

    public var hasAnyAnchor: Bool {
        burstAnchorCount > 0 || tonicAnchorCount > 0 || pauseAnchorCount > 0 || pausePoolAnchorCount > 0
    }

    private static func supportWeight(anchorCount: Int, shrinkage: Double) -> Double {
        let count = Double(max(0, anchorCount))
        let lambda = max(shrinkage, 1)
        return count / (count + lambda)
    }

    private static func cleanWeight(_ value: Double) -> Double {
        guard value.isFinite else {
            return 0
        }
        return min(1, max(0, value))
    }
}

public struct TrainAdaptiveBandSettings: Hashable, Sendable {
    public var minValidISISec: Double
    public var histogramBinWidthSec: Double
    public var datasetISIBoundaryFloorSec: Double

    public init(
        minValidISISec: Double = 0.001,
        histogramBinWidthSec: Double = 0.005,
        datasetISIBoundaryFloorSec: Double = 0.025
    ) {
        self.minValidISISec = minValidISISec.isFinite && minValidISISec > 0 ? minValidISISec : 0.001
        self.histogramBinWidthSec = histogramBinWidthSec.isFinite && histogramBinWidthSec > 0
            ? histogramBinWidthSec
            : 0.005
        self.datasetISIBoundaryFloorSec = datasetISIBoundaryFloorSec.isFinite && datasetISIBoundaryFloorSec >= 0
            ? datasetISIBoundaryFloorSec
            : 0.025
    }
}

public struct TrainAdaptiveBandResolution: Sendable {
    public let trainID: String
    public let trainName: String
    public let minValidISISec: Double
    public let histogramBinWidthSec: Double
    public let validISICount: Int
    public let bands: [AdaptiveBandPattern: AdaptiveBand]
    public let thresholdRows: [AdaptiveBandThresholdRow]
    public let seedBandProfile: TrainSeedBandProfile
    public let structuralSeedSummary: StructuralSeedBandSummary

    public init(
        trainID: String,
        trainName: String,
        minValidISISec: Double,
        histogramBinWidthSec: Double,
        validISICount: Int,
        bands: [AdaptiveBandPattern: AdaptiveBand],
        thresholdRows: [AdaptiveBandThresholdRow],
        seedBandProfile: TrainSeedBandProfile,
        structuralSeedSummary: StructuralSeedBandSummary = .empty
    ) {
        self.trainID = trainID
        self.trainName = trainName
        self.minValidISISec = minValidISISec
        self.histogramBinWidthSec = histogramBinWidthSec
        self.validISICount = validISICount
        self.bands = bands
        self.thresholdRows = thresholdRows
        self.seedBandProfile = seedBandProfile
        self.structuralSeedSummary = structuralSeedSummary
    }

    public var burstBand: AdaptiveBand? {
        bands[.burst]
    }

    public func band(for pattern: AdaptiveBandPattern) -> AdaptiveBand? {
        bands[pattern]
    }
}

public enum TrainAdaptiveBandResolver {
    public static func resolve(
        train: SpikeTrain,
        settings: TrainAdaptiveBandSettings = TrainAdaptiveBandSettings()
    ) -> TrainAdaptiveBandResolution {
        let validISI = validISIs(train: train, minValidISISec: settings.minValidISISec)
        let resolved = resolveBands(
            validISISec: validISI,
            settings: settings
        )
        return TrainAdaptiveBandResolution(
            trainID: train.id,
            trainName: train.name,
            minValidISISec: settings.minValidISISec,
            histogramBinWidthSec: settings.histogramBinWidthSec,
            validISICount: validISI.count,
            bands: resolved.bands,
            thresholdRows: resolved.rows,
            seedBandProfile: seedBandProfile(
                train: train,
                bands: resolved.bands,
                settings: settings
            )
        )
    }

    public static func resolve(
        dataset: SpikeDataset,
        settings: TrainAdaptiveBandSettings = TrainAdaptiveBandSettings()
    ) -> [TrainAdaptiveBandResolution] {
        dataset.trains.map { train in
            resolve(train: train, settings: settings)
        }
    }

    public static func resolvePooled(
        dataset: SpikeDataset,
        settings: TrainAdaptiveBandSettings = TrainAdaptiveBandSettings()
    ) -> TrainAdaptiveBandResolution {
        let values = dataset.trains.flatMap { train in
            validISIs(train: train, minValidISISec: settings.minValidISISec)
        }
        let resolved = resolveBands(validISISec: values, settings: settings)
        return TrainAdaptiveBandResolution(
            trainID: dataset.name,
            trainName: dataset.name,
            minValidISISec: settings.minValidISISec,
            histogramBinWidthSec: settings.histogramBinWidthSec,
            validISICount: values.count,
            bands: resolved.bands,
            thresholdRows: resolved.rows,
            seedBandProfile: seedBandProfile(
                isiSec: values,
                bands: resolved.bands,
                settings: settings
            )
        )
    }

    public static func validISIs(
        train: SpikeTrain,
        minValidISISec: Double = 0.001
    ) -> [Double] {
        train.isiSec.enumerated().compactMap { index, value in
            guard index > 0,
                  let value,
                  value.isFinite,
                  value >= minValidISISec else {
                return nil
            }
            return value
        }
    }

    private struct PartialBand {
        var seedLowerSec: Double?
        var seedUpperSec: Double?
        var bridgeUpperSec: Double?
        var contrastS: Double?

        func value(for field: AdaptiveBandField) -> Double? {
            switch field {
            case .seedLowerSec:
                return seedLowerSec
            case .seedUpperSec:
                return seedUpperSec
            case .bridgeUpperSec:
                return bridgeUpperSec
            case .contrastS:
                return contrastS
            }
        }
    }

    private struct ResolvedBands {
        var bands: [AdaptiveBandPattern: AdaptiveBand]
        var rows: [AdaptiveBandThresholdRow]
    }

    private static func seedBandProfile(
        train: SpikeTrain,
        bands: [AdaptiveBandPattern: AdaptiveBand],
        settings: TrainAdaptiveBandSettings
    ) -> TrainSeedBandProfile {
        let validFlags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let isi = train.isiSec[index],
                  isi.isFinite,
                  isi >= settings.minValidISISec else {
                return false
            }
            return true
        }
        let validValues = train.isiSec.indices.compactMap { index -> Double? in
            guard validFlags[index] else {
                return nil
            }
            return train.isiSec[index]
        }
        return seedBandProfile(
            isiSec: train.isiSec.map { $0 ?? .nan },
            validFlags: validFlags,
            validValues: validValues,
            bands: bands,
            settings: settings
        )
    }

    private static func seedBandProfile(
        isiSec values: [Double],
        bands: [AdaptiveBandPattern: AdaptiveBand],
        settings: TrainAdaptiveBandSettings
    ) -> TrainSeedBandProfile {
        let validValues = values.filter { $0.isFinite && $0 >= settings.minValidISISec }
        return seedBandProfile(
            isiSec: values,
            validFlags: values.map { $0.isFinite && $0 >= settings.minValidISISec },
            validValues: validValues,
            bands: bands,
            settings: settings
        )
    }

    private static func seedBandProfile(
        isiSec values: [Double],
        validFlags: [Bool],
        validValues: [Double],
        bands: [AdaptiveBandPattern: AdaptiveBand],
        settings: TrainAdaptiveBandSettings
    ) -> TrainSeedBandProfile {
        let structuralBurstBand = bands[.burst].flatMap { band -> AdaptiveBand? in
            band.primarySource == .structure ? band : nil
        }
        let pauseThreshold = bands[.pause]?.seedLowerSec ?? 0.100
        let seedFlags = values.indices.map { index -> Bool in
            guard let burst = structuralBurstBand else {
                return false
            }
            guard validFlags.indices.contains(index),
                  validFlags[index],
                  values[index].isFinite else {
                return false
            }
            return values[index] >= burst.seedLowerSec && values[index] <= burst.seedUpperSec
        }
        let runs = boolRuns(seedFlags)
        let maxRun = runs.map { $0.end - $0.start + 1 }.max() ?? 0
        let nValid = validValues.count
        let seedBandFraction: Double?
        if nValid > 0, let burst = structuralBurstBand {
            seedBandFraction = fraction(validValues) { $0 >= burst.seedLowerSec && $0 <= burst.seedUpperSec }
        } else {
            seedBandFraction = nil
        }
        let pauseFraction = nValid > 0
            ? fraction(validValues) { $0 >= pauseThreshold }
            : nil
        let phenotype: String
        if nValid < 10 {
            phenotype = "low_spike_count_unreliable"
        } else if let seedBandFraction, seedBandFraction >= 0.40, maxRun >= 10 {
            phenotype = "hf_spiking_like_seed_dominant"
        } else if let seedBandFraction, seedBandFraction <= 0.02 {
            phenotype = "tonic_or_slow_seed_sparse"
        } else if let pauseFraction, pauseFraction >= 0.25 {
            phenotype = "pause_dominant"
        } else if runs.count >= 2, let seedBandFraction, seedBandFraction > 0.02 {
            phenotype = "burst_capable"
        } else {
            phenotype = "mixed"
        }
        let profileBurstLower = structuralBurstBand?.seedLowerSec ?? settings.minValidISISec
        let profileBurstUpper = structuralBurstBand?.seedUpperSec ?? settings.minValidISISec
        let profileBridgeUpper = structuralBurstBand?.bridgeUpperSec ?? settings.minValidISISec
        let finalPhenotype = structuralBurstBand == nil
            ? "no_structural_burst_seed"
            : phenotype

        return TrainSeedBandProfile(
            nValidISI: nValid,
            datasetISISeedLowSec: profileBurstLower,
            datasetISISeedHighSec: profileBurstUpper,
            datasetISIBridgeHighSec: profileBridgeUpper,
            datasetISIBoundaryFloorSec: settings.datasetISIBoundaryFloorSec,
            seedLowPercentileInTrain: nValid > 0 && structuralBurstBand != nil ? fraction(validValues) { $0 <= profileBurstLower } * 100 : nil,
            seedHighPercentileInTrain: nValid > 0 && structuralBurstBand != nil ? fraction(validValues) { $0 <= profileBurstUpper } * 100 : nil,
            seedBandFraction: seedBandFraction,
            seedRunCount: runs.count,
            maxSeedRunLength: maxRun,
            medianISISec: quantile(validValues, probability: 0.5),
            pauseFraction: pauseFraction,
            phenotypePrior: finalPhenotype
        )
    }

    private static func resolveBands(
        validISISec rawValues: [Double],
        settings: TrainAdaptiveBandSettings
    ) -> ResolvedBands {
        let values = rawValues.filter { $0.isFinite && $0 >= settings.minValidISISec }
        let histogram = histogramSuggestion(
            validISISec: values,
            minValidISISec: settings.minValidISISec,
            binWidthSec: settings.histogramBinWidthSec
        )
        let defaults = defaultSuggestion(minValidISISec: settings.minValidISISec)

        var partialBands: [AdaptiveBandPattern: PartialBand] = [:]
        var sources: [AdaptiveBandPattern: [AdaptiveBandField: AdaptiveBandSource]] = [:]

        for pattern in AdaptiveBandPattern.allCases {
            let histogramBand = histogram[pattern] ?? PartialBand()
            let defaultBand = defaults[pattern] ?? PartialBand()
            var partial = PartialBand()
            var fieldSources: [AdaptiveBandField: AdaptiveBandSource] = [:]

            for field in AdaptiveBandField.allCases {
                let choice = choose(
                    histogramValue: histogramBand.value(for: field),
                    defaultValue: defaultBand.value(for: field),
                    field: field
                )
                switch field {
                case .seedLowerSec:
                    partial.seedLowerSec = choice.value
                case .seedUpperSec:
                    partial.seedUpperSec = choice.value
                case .bridgeUpperSec:
                    partial.bridgeUpperSec = choice.value
                case .contrastS:
                    partial.contrastS = choice.value
                }
                fieldSources[field] = choice.source
            }

            partialBands[pattern] = enforceGeometry(
                partial,
                defaultBand: defaultBand,
                minValidISISec: settings.minValidISISec
            )
            sources[pattern] = fieldSources
        }

        enforceHighFrequencyTonicFloor(
            partialBands: &partialBands,
            minValidISISec: settings.minValidISISec
        )

        var bands: [AdaptiveBandPattern: AdaptiveBand] = [:]
        var rows: [AdaptiveBandThresholdRow] = []
        rows.reserveCapacity(AdaptiveBandPattern.allCases.count * AdaptiveBandField.allCases.count)

        for pattern in AdaptiveBandPattern.allCases {
            let effective = partialBands[pattern] ?? enforceGeometry(
                PartialBand(),
                defaultBand: defaults[pattern] ?? PartialBand(),
                minValidISISec: settings.minValidISISec
            )
            let primarySource = sources[pattern]?[.seedUpperSec] ?? .none
            bands[pattern] = AdaptiveBand(
                pattern: pattern,
                seedLowerSec: effective.seedLowerSec ?? settings.minValidISISec,
                seedUpperSec: effective.seedUpperSec ?? settings.minValidISISec * 2,
                bridgeUpperSec: effective.bridgeUpperSec ?? effective.seedUpperSec ?? settings.minValidISISec * 2,
                contrastS: effective.contrastS,
                primarySource: primarySource
            )

            for field in AdaptiveBandField.allCases {
                rows.append(
                    AdaptiveBandThresholdRow(
                        pattern: pattern,
                        field: field,
                        histogramSec: histogram[pattern]?.value(for: field),
                        defaultSec: defaults[pattern]?.value(for: field),
                        effectiveSec: effective.value(for: field),
                        source: sources[pattern]?[field] ?? .none
                    )
                )
            }
        }

        return ResolvedBands(bands: bands, rows: rows)
    }

    private static func choose(
        histogramValue: Double?,
        defaultValue: Double?,
        field: AdaptiveBandField
    ) -> (value: Double?, source: AdaptiveBandSource) {
        if isUsable(histogramValue, field: field) {
            return (histogramValue, .histogram)
        }
        if isUsable(defaultValue, field: field) {
            return (defaultValue, .default)
        }
        return (nil, .none)
    }

    private static func isUsable(_ value: Double?, field: AdaptiveBandField) -> Bool {
        guard let value, value.isFinite else {
            return false
        }
        return field == .contrastS || value > 0
    }

    private static func histogramSuggestion(
        validISISec values: [Double],
        minValidISISec: Double,
        binWidthSec rawBinWidth: Double
    ) -> [AdaptiveBandPattern: PartialBand] {
        guard values.count >= 5 else {
            let lowFloor = max(minValidISISec, 0.001)
            return [
                .highFrequencySpiking: PartialBand(
                    seedLowerSec: lowFloor,
                    seedUpperSec: 0.020,
                    bridgeUpperSec: 0.030,
                    contrastS: nil
                ),
                .highFrequencyTonic: PartialBand(
                    seedLowerSec: 0.010,
                    seedUpperSec: 0.030,
                    bridgeUpperSec: 0.035,
                    contrastS: nil
                ),
                .tonic: PartialBand(
                    seedLowerSec: 0.020,
                    seedUpperSec: 0.060,
                    bridgeUpperSec: 0.080,
                    contrastS: nil
                ),
                .pause: PartialBand(
                    seedLowerSec: 0.100,
                    seedUpperSec: 0.250,
                    bridgeUpperSec: 0.250,
                    contrastS: nil
                )
            ]
        }

        let binWidth = rawBinWidth.isFinite && rawBinWidth > 0 ? rawBinWidth : 0.005
        let sample = SortedFiniteSample(values)
        let q005 = sample.quantile(0.005) ?? minValidISISec
        let q10 = sample.quantile(0.10) ?? 0.010
        let q15 = sample.quantile(0.15) ?? q10
        let q20 = sample.quantile(0.20) ?? q15
        let q25 = sample.quantile(0.25) ?? 0.030
        let q35 = sample.quantile(0.35) ?? 0.050
        let q50 = sample.quantile(0.50) ?? 0.060
        let q75 = sample.quantile(0.75) ?? 0.100
        let q90 = sample.quantile(0.90) ?? 0.150
        let q95 = sample.quantile(0.95) ?? 0.250
        let minObserved = values.min() ?? minValidISISec
        let slowStructuralTail = minObserved.isFinite &&
            minObserved > 0.015 &&
            q50.isFinite &&
            q50 > 0 &&
            minObserved <= q50 * 0.45

        let lowMax = min(max(q35, 0.030), 0.080)
        let lowValues = values.filter { $0 <= lowMax }
        var burstHigh: Double?

        if lowValues.count >= 8 {
            let upperBreak = ceil(lowMax / binWidth) * binWidth + binWidth
            let breaks = regularBreaks(from: 0, through: upperBreak, by: binWidth)
            if breaks.count >= 4 {
                let counts = histogramCounts(lowValues, breaks: breaks)
                let mids = zip(breaks, breaks.dropFirst()).map { ($0 + $1) / 2 }
                let searchMax = min(
                    lowMax,
                    finiteMax(0.040, q15, minObserved * 1.10) ?? lowMax
                )
                let searchIndices = mids.indices.filter { index in
                    mids[index] >= minValidISISec && mids[index] <= searchMax
                }
                if let peakIndex = firstMaximumIndex(in: searchIndices, counts: counts),
                   counts[peakIndex] > 0 {
                    let afterStart = peakIndex + 1
                    let afterEnd = min(counts.count - 1, peakIndex + 10)
                    if afterStart <= afterEnd {
                        let after = Array(afterStart...afterEnd)
                        var localMinimum: Int?
                        for index in after where index > 0 && index < counts.count - 1 {
                            if counts[index] <= counts[index - 1],
                               counts[index] <= counts[index + 1] {
                                localMinimum = index
                                break
                            }
                        }
                        if localMinimum == nil {
                            localMinimum = after.first { counts[$0] <= Int(floor(0.55 * Double(counts[peakIndex]))) }
                        }
                        if let localMinimum, breaks.indices.contains(localMinimum + 1) {
                            burstHigh = breaks[localMinimum + 1]
                        }
                    }
                }
            }
        }

        if burstHigh == nil || !(burstHigh?.isFinite ?? false) || (burstHigh ?? 0) <= minValidISISec {
            if slowStructuralTail {
                var fallback = finiteMax(q10, q15, minObserved * 1.05, minObserved) ?? minObserved
                fallback = min(fallback, max(q25, minObserved))
                burstHigh = fallback
            } else {
                burstHigh = min(max(q10, 0.006), 0.015)
            }
        }

        var resolvedBurstHigh = burstHigh ?? 0.010
        if slowStructuralTail, minObserved.isFinite, resolvedBurstHigh < minObserved {
            resolvedBurstHigh = minObserved
        }

        let burstLower = max(
            minValidISISec,
            min(q005, resolvedBurstHigh * 0.25)
        )

        var bridgeHigh = max(resolvedBurstHigh * 1.5, resolvedBurstHigh + binWidth)
        if slowStructuralTail {
            let bridgeCap = finiteMax(
                resolvedBurstHigh * 1.8,
                q20,
                finiteMin(q25, q50 * 0.85) ?? q25,
                resolvedBurstHigh
            ) ?? resolvedBurstHigh
            bridgeHigh = min(max(bridgeHigh, resolvedBurstHigh), bridgeCap)
            if !bridgeHigh.isFinite || bridgeHigh <= resolvedBurstHigh {
                bridgeHigh = max(resolvedBurstHigh * 1.25, resolvedBurstHigh + binWidth)
            }
        } else {
            bridgeHigh = min(max(bridgeHigh, resolvedBurstHigh), 0.050)
        }

        return [
            .highFrequencySpiking: PartialBand(
                seedLowerSec: burstLower,
                seedUpperSec: max(bridgeHigh, resolvedBurstHigh),
                bridgeUpperSec: max(bridgeHigh * 2, 0.030),
                contrastS: nil
            ),
            .highFrequencyTonic: PartialBand(
                seedLowerSec: max(resolvedBurstHigh, min(q25, 0.030)),
                seedUpperSec: finiteMax(q25, bridgeHigh, 0.020) ?? 0.020,
                bridgeUpperSec: finiteMax(q25, bridgeHigh * 1.3, 0.030) ?? 0.030,
                contrastS: nil
            ),
            .tonic: PartialBand(
                seedLowerSec: finiteMax(q25, bridgeHigh, 0.020) ?? 0.020,
                seedUpperSec: finiteMax(q75, q50, 0.050) ?? 0.050,
                bridgeUpperSec: finiteMax(q90, q75, 0.080) ?? 0.080,
                contrastS: nil
            ),
            .pause: PartialBand(
                seedLowerSec: max(q90, 0.080),
                seedUpperSec: finiteMax(q95, q90, 0.150) ?? 0.150,
                bridgeUpperSec: finiteMax(q95, q90, 0.150) ?? 0.150,
                contrastS: nil
            )
        ]
    }

    private static func defaultSuggestion(minValidISISec: Double) -> [AdaptiveBandPattern: PartialBand] {
        [
            .highFrequencySpiking: PartialBand(
                seedLowerSec: max(minValidISISec, 0.001),
                seedUpperSec: 0.020,
                bridgeUpperSec: 0.030,
                contrastS: nil
            ),
            .highFrequencyTonic: PartialBand(
                seedLowerSec: 0.010,
                seedUpperSec: 0.020,
                bridgeUpperSec: 0.025,
                contrastS: nil
            ),
            .tonic: PartialBand(
                seedLowerSec: 0.020,
                seedUpperSec: 0.060,
                bridgeUpperSec: 0.075,
                contrastS: nil
            ),
            .pause: PartialBand(
                seedLowerSec: 0.100,
                seedUpperSec: 0.150,
                bridgeUpperSec: 0.150,
                contrastS: nil
            )
        ]
    }

    private static func enforceGeometry(
        _ band: PartialBand,
        defaultBand: PartialBand,
        minValidISISec: Double
    ) -> PartialBand {
        var lower = band.seedLowerSec ?? minValidISISec
        if !lower.isFinite || lower < 0 {
            lower = minValidISISec
        }

        var upper = band.seedUpperSec ?? max(lower + minValidISISec, defaultBand.seedUpperSec ?? lower + minValidISISec)
        if !upper.isFinite || upper <= lower {
            upper = max(lower + minValidISISec, defaultBand.seedUpperSec ?? lower + minValidISISec)
        }

        var bridge = band.bridgeUpperSec ?? upper
        if !bridge.isFinite || bridge < upper {
            bridge = upper
        }

        return PartialBand(
            seedLowerSec: lower,
            seedUpperSec: upper,
            bridgeUpperSec: bridge,
            contrastS: band.contrastS
        )
    }

    private static func enforceHighFrequencyTonicFloor(
        partialBands: inout [AdaptiveBandPattern: PartialBand],
        minValidISISec: Double
    ) {
        guard let burstBand = partialBands[.burst],
              burstBand.contrastS != nil,
              let burstUpper = burstBand.seedUpperSec,
              burstUpper.isFinite,
              var highFrequencyTonic = partialBands[.highFrequencyTonic] else {
            return
        }

        let lower = max(highFrequencyTonic.seedLowerSec ?? burstUpper, burstUpper)
        highFrequencyTonic.seedLowerSec = lower
        if (highFrequencyTonic.seedUpperSec ?? 0) <= lower {
            highFrequencyTonic.seedUpperSec = max(lower * 1.5, lower + minValidISISec)
        }
        if (highFrequencyTonic.bridgeUpperSec ?? 0) < (highFrequencyTonic.seedUpperSec ?? lower) {
            highFrequencyTonic.bridgeUpperSec = highFrequencyTonic.seedUpperSec
        }
        partialBands[.highFrequencyTonic] = highFrequencyTonic
    }

    private static func regularBreaks(from lower: Double, through upper: Double, by step: Double) -> [Double] {
        guard lower.isFinite, upper.isFinite, step.isFinite, step > 0, upper >= lower else {
            return []
        }

        var breaks: [Double] = []
        var value = lower
        let epsilon = step * 1e-9
        while value <= upper + epsilon {
            breaks.append(value)
            value += step
        }
        if breaks.last.map({ $0 < upper }) ?? true {
            breaks.append(upper)
        }
        return breaks
    }

    private static func histogramCounts(_ values: [Double], breaks: [Double]) -> [Int] {
        guard breaks.count >= 2 else {
            return []
        }

        let binCount = breaks.count - 1
        let lower = breaks[0]
        let width = breaks[1] - breaks[0]
        guard width > 0 else {
            return Array(repeating: 0, count: binCount)
        }

        var counts = Array(repeating: 0, count: binCount)
        for value in values where value.isFinite && value >= lower {
            let rawIndex = Int(floor((value - lower) / width))
            let index = min(max(rawIndex, 0), binCount - 1)
            counts[index] += 1
        }
        return counts
    }

    private static func firstMaximumIndex(in indices: [Int], counts: [Int]) -> Int? {
        var bestIndex: Int?
        var bestCount = Int.min
        for index in indices where counts.indices.contains(index) {
            if counts[index] > bestCount {
                bestIndex = index
                bestCount = counts[index]
            }
        }
        return bestIndex
    }

    private static func boolRuns(_ flags: [Bool]) -> [(start: Int, end: Int)] {
        var runs: [(start: Int, end: Int)] = []
        var start: Int?
        for index in flags.indices {
            if flags[index] {
                if start == nil {
                    start = index
                }
            } else if let s = start {
                runs.append((s, index - 1))
                start = nil
            }
        }
        if let s = start {
            runs.append((s, max(s, flags.count - 1)))
        }
        return runs
    }

    private static func fraction(_ values: [Double], where predicate: (Double) -> Bool) -> Double {
        guard !values.isEmpty else {
            return .nan
        }
        let count = values.reduce(0) { partial, value in
            partial + (predicate(value) ? 1 : 0)
        }
        return Double(count) / Double(values.count)
    }

    private static func quantile(_ values: [Double], probability: Double) -> Double? {
        let sorted = values.filter(\.isFinite).sorted()
        guard !sorted.isEmpty else {
            return nil
        }
        guard sorted.count > 1 else {
            return sorted[0]
        }

        let p = min(max(probability, 0), 1)
        let h = (Double(sorted.count) - 1) * p + 1
        let lowerIndex = max(0, Int(floor(h)) - 1)
        let upperIndex = max(0, Int(ceil(h)) - 1)
        let lowerValue = sorted[min(lowerIndex, sorted.count - 1)]
        let upperValue = sorted[min(upperIndex, sorted.count - 1)]
        return lowerValue + (h - floor(h)) * (upperValue - lowerValue)
    }

    private static func finiteMax(_ values: Double...) -> Double? {
        values.filter(\.isFinite).max()
    }

    private static func finiteMin(_ values: Double...) -> Double? {
        values.filter(\.isFinite).min()
    }
}

public extension ClassicAnchorSettings {
    init(
        adaptiveResolution resolution: TrainAdaptiveBandResolution,
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        refractoryAction: ClassicAnchorRefractoryAction = .warnOnly
    ) {
        let burst = resolution.burstBand
        let hasStructureBurstBand = resolution.structuralSeedSummary.burstAnchorCount > 0 &&
            burst?.primarySource == .structure
        let burstLower = hasStructureBurstBand ? (burst?.seedLowerSec ?? resolution.minValidISISec) : resolution.minValidISISec
        let burstUpper = hasStructureBurstBand ? (burst?.seedUpperSec ?? resolution.minValidISISec) : resolution.minValidISISec
        let burstBridge = hasStructureBurstBand ? burst?.bridgeUpperSec : nil

        self.init(
            minValidISISec: resolution.minValidISISec,
            burstBandLowerSec: burstLower,
            burstBandUpperSec: burstUpper,
            burstBridgeUpperSec: burstBridge,
            burstBandSource: hasStructureBurstBand ? .structure : .none,
            burstBandIsStructureDerived: hasStructureBurstBand,
            burstContrastMin: burst?.contrastS ?? 3.0,
            burstCoreReferenceUpperSec: hasStructureBurstBand ? burstUpper : nil,
            structuralBurstSupportWeight: resolution.structuralSeedSummary.burstSupportWeight,
            refractorySuspectSec: max(
                qualitySettings.refractorySuspectThresholdSec,
                resolution.minValidISISec
            ),
            refractoryAction: refractoryAction
        )
    }
}
