import Foundation

/// A detector-independent, user-authored rule used only to accelerate manual ISI labeling.
/// It has no detector authority and does not learn or mutate thresholds.
public enum ManualISIThresholdPattern: String, CaseIterable, Hashable, Sendable {
    case burst
    case pause
    case tonic
}

public enum ManualISITonicMetric: String, CaseIterable, Hashable, Sendable {
    /// Short-run compactness: maximum ISI divided by minimum ISI over the candidate.
    /// This user-facing MM is not the retired detector-side max/mean metric.
    case mm
    case cv
    case cv2
    case lv
}

public struct ManualISIThresholdClosedRange: Hashable, Sendable {
    public let lowerBound: Double
    public let upperBound: Double

    public init(lowerBound: Double, upperBound: Double) {
        self.lowerBound = lowerBound
        self.upperBound = upperBound
    }

    public func contains(_ value: Double) -> Bool {
        value >= lowerBound && value <= upperBound
    }
}

public struct ManualISIThresholdSample: Hashable, Sendable {
    public let isiIndex: Int
    public let isiSeconds: Double

    public init(isiIndex: Int, isiSeconds: Double) {
        self.isiIndex = isiIndex
        self.isiSeconds = isiSeconds
    }
}

public struct ManualISIThresholdRule: Hashable, Sendable {
    public let pattern: ManualISIThresholdPattern
    public let isiRangeSeconds: ManualISIThresholdClosedRange
    public let minimumSpikeCount: Int
    public let maximumSpikeCount: Int?
    public let tonicMetric: ManualISITonicMetric?
    public let tonicMetricRange: ManualISIThresholdClosedRange?

    public init(
        pattern: ManualISIThresholdPattern,
        isiRangeSeconds: ManualISIThresholdClosedRange,
        minimumSpikeCount: Int = 2,
        maximumSpikeCount: Int? = nil,
        tonicMetric: ManualISITonicMetric? = nil,
        tonicMetricRange: ManualISIThresholdClosedRange? = nil
    ) {
        self.pattern = pattern
        self.isiRangeSeconds = isiRangeSeconds
        self.minimumSpikeCount = minimumSpikeCount
        self.maximumSpikeCount = maximumSpikeCount
        self.tonicMetric = tonicMetric
        self.tonicMetricRange = tonicMetricRange
    }
}

public struct ManualISIThresholdCandidate: Hashable, Sendable {
    public let pattern: ManualISIThresholdPattern
    public let isiIndices: [Int]
    public let spikeCount: Int
    public let regularityMetric: ManualISITonicMetric?
    public let regularityValue: Double?

    public init(
        pattern: ManualISIThresholdPattern,
        isiIndices: [Int],
        spikeCount: Int,
        regularityMetric: ManualISITonicMetric? = nil,
        regularityValue: Double? = nil
    ) {
        self.pattern = pattern
        self.isiIndices = isiIndices
        self.spikeCount = spikeCount
        self.regularityMetric = regularityMetric
        self.regularityValue = regularityValue
    }
}

public enum ManualISIThresholdMarkerError: Error, Equatable, Sendable, LocalizedError {
    case invalidISIRange
    case invalidSpikeCountRange
    case missingTonicMetric
    case invalidTonicMetricRange
    case invalidSampleOrder

    public var errorDescription: String? {
        switch self {
        case .invalidISIRange:
            return "ISI range must be finite, nonnegative, and ordered as a closed interval."
        case .invalidSpikeCountRange:
            return "Spike-count bounds must be positive and ordered."
        case .missingTonicMetric:
            return "Tonic threshold labeling requires MM, CV, CV2, or LV."
        case .invalidTonicMetricRange:
            return "The tonic metric range must be finite, nonnegative, and ordered."
        case .invalidSampleOrder:
            return "Manual ISI samples must have strictly increasing positive ISI indices."
        }
    }
}

/// Shared display/quick-label QC predicate. Both endpoint spikes are considered involved in an
/// invalid interval; this predicate never claims which endpoint is the erroneous spike.
public enum ManualISIQualityRule {
    public static func isAbsolutelyInvalid(
        isiSeconds: Double,
        thresholdSeconds: Double
    ) -> Bool {
        guard isiSeconds.isFinite else { return true }
        guard isiSeconds > 0 else { return true }
        let threshold = thresholdSeconds.isFinite ? max(0, thresholdSeconds) : 0
        let tolerance = max(1e-12, abs(threshold) * 1e-6)
        return isiSeconds < threshold - tolerance
    }
}

public enum ManualISIThresholdMarker {
    /// Proposes maximal contiguous runs only. A Burst run that exceeds the user-specified maximum
    /// is rejected as a whole rather than chopped into artificial Burst-sized packets.
    public static func propose(
        samples: [ManualISIThresholdSample],
        rule: ManualISIThresholdRule,
        minimumValidISISeconds: Double
    ) throws -> [ManualISIThresholdCandidate] {
        try validate(rule: rule, samples: samples)
        guard !samples.isEmpty else { return [] }

        switch rule.pattern {
        case .pause:
            return samples.compactMap { sample in
                guard isEligible(
                    sample,
                    range: rule.isiRangeSeconds,
                    minimumValidISISeconds: minimumValidISISeconds
                ) else { return nil }
                return ManualISIThresholdCandidate(
                    pattern: .pause,
                    isiIndices: [sample.isiIndex],
                    spikeCount: 2
                )
            }

        case .burst, .tonic:
            let runs = contiguousEligibleRuns(
                samples: samples,
                range: rule.isiRangeSeconds,
                minimumValidISISeconds: minimumValidISISeconds
            )
            return runs.compactMap { run in
                candidate(for: run, rule: rule)
            }
        }
    }

    private static func candidate(
        for run: [ManualISIThresholdSample],
        rule: ManualISIThresholdRule
    ) -> ManualISIThresholdCandidate? {
        let spikeCount = run.count + 1
        guard spikeCount >= rule.minimumSpikeCount,
              rule.maximumSpikeCount.map({ spikeCount <= $0 }) ?? true else {
            return nil
        }

        if rule.pattern == .tonic {
            guard let metric = rule.tonicMetric,
                  let metricRange = rule.tonicMetricRange else { return nil }
            let values = run.map(\.isiSeconds)
            let metricValue: Double?
            switch metric {
            case .mm:
                guard let minimum = values.min(), minimum > 0,
                      let maximum = values.max() else { return nil }
                metricValue = maximum / minimum
            case .cv:
                metricValue = STPDStatistics.coefficientOfVariation(values)
            case .cv2:
                metricValue = STPDStatistics.coefficientOfVariation2(values)
            case .lv:
                metricValue = STPDStatistics.localVariation(values)
            }
            guard let metricValue, metricRange.contains(metricValue) else { return nil }
            return ManualISIThresholdCandidate(
                pattern: .tonic,
                isiIndices: run.map(\.isiIndex),
                spikeCount: spikeCount,
                regularityMetric: metric,
                regularityValue: metricValue
            )
        }

        return ManualISIThresholdCandidate(
            pattern: rule.pattern,
            isiIndices: run.map(\.isiIndex),
            spikeCount: spikeCount
        )
    }

    private static func contiguousEligibleRuns(
        samples: [ManualISIThresholdSample],
        range: ManualISIThresholdClosedRange,
        minimumValidISISeconds: Double
    ) -> [[ManualISIThresholdSample]] {
        var runs: [[ManualISIThresholdSample]] = []
        var current: [ManualISIThresholdSample] = []

        func finishCurrent() {
            if !current.isEmpty { runs.append(current) }
            current.removeAll(keepingCapacity: true)
        }

        for sample in samples {
            guard isEligible(
                sample,
                range: range,
                minimumValidISISeconds: minimumValidISISeconds
            ) else {
                finishCurrent()
                continue
            }
            if let previous = current.last, sample.isiIndex != previous.isiIndex + 1 {
                finishCurrent()
            }
            current.append(sample)
        }
        finishCurrent()
        return runs
    }

    private static func isEligible(
        _ sample: ManualISIThresholdSample,
        range: ManualISIThresholdClosedRange,
        minimumValidISISeconds: Double
    ) -> Bool {
        !ManualISIQualityRule.isAbsolutelyInvalid(
            isiSeconds: sample.isiSeconds,
            thresholdSeconds: minimumValidISISeconds
        ) && range.contains(sample.isiSeconds)
    }

    private static func validate(
        rule: ManualISIThresholdRule,
        samples: [ManualISIThresholdSample]
    ) throws {
        let isiRange = rule.isiRangeSeconds
        guard isiRange.lowerBound.isFinite,
              isiRange.upperBound.isFinite,
              isiRange.lowerBound >= 0,
              isiRange.lowerBound <= isiRange.upperBound else {
            throw ManualISIThresholdMarkerError.invalidISIRange
        }

        let structuralMinimum: Int
        switch rule.pattern {
        case .pause:
            structuralMinimum = 2
        case .burst:
            structuralMinimum = 3
        case .tonic:
            guard let metric = rule.tonicMetric else {
                throw ManualISIThresholdMarkerError.missingTonicMetric
            }
            switch metric {
            case .mm:
                // Owner-approved short path: 3–5 spikes (2–4 ISIs), using max(ISI)/min(ISI).
                structuralMinimum = 3
                guard let maximum = rule.maximumSpikeCount, maximum <= 5 else {
                    throw ManualISIThresholdMarkerError.invalidSpikeCountRange
                }
            case .cv, .cv2, .lv:
                // These metrics are unstable as stand-alone evidence on the short path.
                // Require at least five ISIs (six spikes).
                structuralMinimum = 6
            }
        }
        guard rule.minimumSpikeCount >= structuralMinimum,
              rule.maximumSpikeCount.map({ $0 >= rule.minimumSpikeCount }) ?? true else {
            throw ManualISIThresholdMarkerError.invalidSpikeCountRange
        }

        if rule.pattern == .tonic {
            guard let metric = rule.tonicMetric, let range = rule.tonicMetricRange else {
                throw ManualISIThresholdMarkerError.missingTonicMetric
            }
            guard range.lowerBound.isFinite,
                  range.upperBound.isFinite,
                  range.lowerBound >= (metric == .mm ? 1 : 0),
                  range.lowerBound <= range.upperBound else {
                throw ManualISIThresholdMarkerError.invalidTonicMetricRange
            }
        }

        var previousIndex = 0
        for sample in samples {
            guard sample.isiIndex > previousIndex else {
                throw ManualISIThresholdMarkerError.invalidSampleOrder
            }
            previousIndex = sample.isiIndex
        }
    }
}
