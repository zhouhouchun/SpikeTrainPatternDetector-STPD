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
    /// Optional manual Burst boundary gate. This is a reviewer-authored quick-label rule, not a
    /// detector parameter. The observed value is the weaker available adjacent-boundary ISI
    /// divided by the candidate's intra-run Q90 ISI.
    public let burstMinimumEdgeContrast: Double?
    public let tonicMetric: ManualISITonicMetric?
    public let tonicMetricRange: ManualISIThresholdClosedRange?

    public init(
        pattern: ManualISIThresholdPattern,
        isiRangeSeconds: ManualISIThresholdClosedRange,
        minimumSpikeCount: Int = 2,
        maximumSpikeCount: Int? = nil,
        burstMinimumEdgeContrast: Double? = nil,
        tonicMetric: ManualISITonicMetric? = nil,
        tonicMetricRange: ManualISIThresholdClosedRange? = nil
    ) {
        self.pattern = pattern
        self.isiRangeSeconds = isiRangeSeconds
        self.minimumSpikeCount = minimumSpikeCount
        self.maximumSpikeCount = maximumSpikeCount
        self.burstMinimumEdgeContrast = burstMinimumEdgeContrast
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
    public let burstIntraQ90Seconds: Double?
    public let burstLeftEdgeContrast: Double?
    public let burstRightEdgeContrast: Double?
    public let burstMinimumEdgeContrast: Double?

    public init(
        pattern: ManualISIThresholdPattern,
        isiIndices: [Int],
        spikeCount: Int,
        regularityMetric: ManualISITonicMetric? = nil,
        regularityValue: Double? = nil,
        burstIntraQ90Seconds: Double? = nil,
        burstLeftEdgeContrast: Double? = nil,
        burstRightEdgeContrast: Double? = nil,
        burstMinimumEdgeContrast: Double? = nil
    ) {
        self.pattern = pattern
        self.isiIndices = isiIndices
        self.spikeCount = spikeCount
        self.regularityMetric = regularityMetric
        self.regularityValue = regularityValue
        self.burstIntraQ90Seconds = burstIntraQ90Seconds
        self.burstLeftEdgeContrast = burstLeftEdgeContrast
        self.burstRightEdgeContrast = burstRightEdgeContrast
        self.burstMinimumEdgeContrast = burstMinimumEdgeContrast
    }
}

public enum ManualISIThresholdMarkerError: Error, Equatable, Sendable, LocalizedError {
    case invalidISIRange
    case invalidSpikeCountRange
    case invalidBurstContrast
    case missingTonicMetric
    case invalidTonicMetricRange
    case invalidSampleOrder

    public var errorDescription: String? {
        switch self {
        case .invalidISIRange:
            return "ISI range must be finite, nonnegative, and ordered as a closed interval."
        case .invalidSpikeCountRange:
            return "Spike-count bounds must be positive and ordered."
        case .invalidBurstContrast:
            return "Burst edge contrast must be finite and at least 1."
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
            let samplePositions: [Int: Int] = rule.pattern == .burst
                ? Dictionary(uniqueKeysWithValues: samples.indices.map { (samples[$0].isiIndex, $0) })
                : [:]
            return runs.compactMap { run in
                candidate(
                    for: run,
                    in: samples,
                    samplePositions: samplePositions,
                    rule: rule
                )
            }
        }
    }

    private static func candidate(
        for run: [ManualISIThresholdSample],
        in samples: [ManualISIThresholdSample],
        samplePositions: [Int: Int],
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

        if rule.pattern == .burst {
            let contrast = burstContrast(
                for: run,
                in: samples,
                samplePositions: samplePositions
            )
            if let required = rule.burstMinimumEdgeContrast {
                guard let observed = contrast.minimum, observed >= required else { return nil }
            }
            return ManualISIThresholdCandidate(
                pattern: .burst,
                isiIndices: run.map(\.isiIndex),
                spikeCount: spikeCount,
                burstIntraQ90Seconds: contrast.intraQ90Seconds,
                burstLeftEdgeContrast: contrast.left,
                burstRightEdgeContrast: contrast.right,
                burstMinimumEdgeContrast: contrast.minimum
            )
        }

        return ManualISIThresholdCandidate(
            pattern: rule.pattern,
            isiIndices: run.map(\.isiIndex),
            spikeCount: spikeCount
        )
    }

    /// Matches the formal detector's interpretable Q90 boundary ratio while remaining an
    /// independent, reviewer-authored quick-label calculation. When both flanks exist, both must
    /// be valid and `minimum` is the weaker side. At a true train edge, the single available side
    /// is used. A run spanning the whole train has no measurable contrast.
    private static func burstContrast(
        for run: [ManualISIThresholdSample],
        in samples: [ManualISIThresholdSample],
        samplePositions: [Int: Int]
    ) -> (intraQ90Seconds: Double?, left: Double?, right: Double?, minimum: Double?) {
        guard let first = run.first,
              let last = run.last,
              let firstPosition = samplePositions[first.isiIndex],
              let lastPosition = samplePositions[last.isiIndex],
              let q90 = SortedFiniteSample(run.map(\.isiSeconds), positiveOnly: true).quantile(0.90),
              q90 > 0 else {
            return (nil, nil, nil, nil)
        }

        let hasLeftBoundary = firstPosition > samples.startIndex
        let hasRightBoundary = lastPosition < samples.index(before: samples.endIndex)
        let leftSample = hasLeftBoundary ? samples[samples.index(before: firstPosition)] : nil
        let rightSample = hasRightBoundary ? samples[samples.index(after: lastPosition)] : nil

        // A discontinuity in the supplied sample sequence is not a train edge and must not be
        // silently treated as usable one-sided evidence.
        let leftIsAdjacent = leftSample.map { $0.isiIndex + 1 == first.isiIndex } ?? !hasLeftBoundary
        let rightIsAdjacent = rightSample.map { last.isiIndex + 1 == $0.isiIndex } ?? !hasRightBoundary
        let left = leftSample.flatMap { ratio($0.isiSeconds, over: q90) }
        let right = rightSample.flatMap { ratio($0.isiSeconds, over: q90) }

        guard leftIsAdjacent, rightIsAdjacent else {
            return (q90, left, right, nil)
        }
        if hasLeftBoundary && left == nil { return (q90, nil, right, nil) }
        if hasRightBoundary && right == nil { return (q90, left, nil, nil) }

        let available = [left, right].compactMap { $0 }
        return (q90, left, right, available.min())
    }

    private static func ratio(_ numerator: Double, over denominator: Double) -> Double? {
        guard numerator.isFinite, numerator > 0,
              denominator.isFinite, denominator > 0 else { return nil }
        let value = numerator / denominator
        return value.isFinite ? value : nil
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

        if let contrast = rule.burstMinimumEdgeContrast {
            guard rule.pattern == .burst, contrast.isFinite, contrast >= 1 else {
                throw ManualISIThresholdMarkerError.invalidBurstContrast
            }
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
