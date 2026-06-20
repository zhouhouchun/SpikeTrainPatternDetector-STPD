import Foundation

public enum SpikeISIStatePointQC: String, CaseIterable, Sendable {
    case ok
    case refractory
    case artifact
    case duplicate

    public var title: String {
        switch self {
        case .ok:
            return "OK"
        case .refractory:
            return "Refractory suspect"
        case .artifact:
            return "Artifact"
        case .duplicate:
            return "Duplicate timestamp"
        }
    }
}

public struct SpikeISIStatePoint: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainID: String
    public let trainName: String
    public let pointIndex: Int
    public let leftEvent: SpikeISIEvent
    public let rightEvent: SpikeISIEvent
    public let qcStatus: SpikeISIStatePointQC

    public init(
        trainID: String,
        trainName: String,
        pointIndex: Int,
        leftEvent: SpikeISIEvent,
        rightEvent: SpikeISIEvent,
        qcStatus: SpikeISIStatePointQC
    ) {
        self.id = "\(trainID)-state-\(pointIndex)"
        self.trainID = trainID
        self.trainName = trainName
        self.pointIndex = pointIndex
        self.leftEvent = leftEvent
        self.rightEvent = rightEvent
        self.qcStatus = qcStatus
    }

    public var leftISISec: Double {
        leftEvent.isiSec
    }

    public var rightISISec: Double {
        rightEvent.isiSec
    }

    public var firstSpikeTimeSec: Double {
        leftEvent.leftSpikeTimeSec
    }

    public var middleSpikeTimeSec: Double {
        leftEvent.rightSpikeTimeSec
    }

    public var lastSpikeTimeSec: Double {
        rightEvent.rightSpikeTimeSec
    }

    public var alignedFirstSpikeTimeSec: Double {
        leftEvent.alignedLeftSpikeTimeSec
    }

    public var alignedMiddleSpikeTimeSec: Double {
        leftEvent.alignedRightSpikeTimeSec
    }

    public var alignedLastSpikeTimeSec: Double {
        rightEvent.alignedRightSpikeTimeSec
    }

    public var isDuplicateTimestamp: Bool {
        qcStatus == .duplicate
    }

    public var isArtifact: Bool {
        qcStatus == .artifact || qcStatus == .duplicate
    }

    public var isRefractorySuspect: Bool {
        qcStatus == .refractory
    }
}

public struct SpikeISIStateTrace: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainID: String
    public let trainName: String
    public let points: [SpikeISIStatePoint]

    public init(trainID: String, trainName: String, points: [SpikeISIStatePoint]) {
        self.id = trainID
        self.trainID = trainID
        self.trainName = trainName
        self.points = points
    }

    public static func build(
        for trace: SpikeISITrace,
        duplicateTolerance: Double = 1e-12
    ) -> SpikeISIStateTrace {
        let pairs = zip(trace.events, trace.events.dropFirst())
        let points = pairs.enumerated().map { offset, pair in
            let qcStatus = qcStatus(
                left: pair.0,
                right: pair.1,
                duplicateTolerance: duplicateTolerance
            )
            return SpikeISIStatePoint(
                trainID: trace.trainID,
                trainName: trace.trainName,
                pointIndex: offset + 1,
                leftEvent: pair.0,
                rightEvent: pair.1,
                qcStatus: qcStatus
            )
        }

        return SpikeISIStateTrace(
            trainID: trace.trainID,
            trainName: trace.trainName,
            points: points
        )
    }

    public static func build(
        for train: SpikeTrain,
        settings: SpikeQualitySettings = SpikeQualitySettings(),
        duplicateTolerance: Double = 1e-12
    ) -> SpikeISIStateTrace {
        build(
            for: SpikeISITrace.build(for: train, settings: settings),
            duplicateTolerance: duplicateTolerance
        )
    }

    public static func build(
        for dataset: SpikeDataset,
        settings: SpikeQualitySettings = SpikeQualitySettings(),
        duplicateTolerance: Double = 1e-12
    ) -> [SpikeISIStateTrace] {
        dataset.trains.map {
            build(
                for: $0,
                settings: settings,
                duplicateTolerance: duplicateTolerance
            )
        }
    }

    private static func qcStatus(
        left: SpikeISIEvent,
        right: SpikeISIEvent,
        duplicateTolerance: Double
    ) -> SpikeISIStatePointQC {
        if isDuplicate(left.isiSec, tolerance: duplicateTolerance) ||
            isDuplicate(right.isiSec, tolerance: duplicateTolerance) {
            return .duplicate
        }

        if left.isArtifact || right.isArtifact {
            return .artifact
        }

        if left.isRefractorySuspect || right.isRefractorySuspect {
            return .refractory
        }

        return .ok
    }

    private static func isDuplicate(_ isi: Double, tolerance: Double) -> Bool {
        isi.isFinite && abs(isi) <= max(tolerance, 0)
    }
}

public enum SpikeISIStateFeatureScaling: String, CaseIterable, Sendable {
    case robust
    case zScore
}

public struct SpikeISIStateFeatureOptions: Hashable, Sendable {
    public var halfWindowK: Int
    public var scaling: SpikeISIStateFeatureScaling
    public var winsorizeExtremeLogISI: Bool
    public var breakLongISI: Bool
    public var breakThresholdSec: Double

    public init(
        halfWindowK: Int = 3,
        scaling: SpikeISIStateFeatureScaling = .robust,
        winsorizeExtremeLogISI: Bool = true,
        breakLongISI: Bool = true,
        breakThresholdSec: Double = 0.150
    ) {
        self.halfWindowK = min(max(halfWindowK, 1), 10)
        self.scaling = scaling
        self.winsorizeExtremeLogISI = winsorizeExtremeLogISI
        self.breakLongISI = breakLongISI
        self.breakThresholdSec = max(0, breakThresholdSec)
    }
}

public struct SpikeISIStateFeature: Identifiable, Hashable, Sendable {
    public let id: String
    public let point: SpikeISIStatePoint
    public let rawLog10LeftISISec: Double
    public let rawLog10RightISISec: Double
    public let log10LeftISISec: Double
    public let log10RightISISec: Double
    public let deltaLog10ISI: Double
    public let localMedianLog10ISI: Double
    public let localMADLog10ISI: Double
    public let scaledLeft: Double
    public let scaledRight: Double
    public let leftLogWasFloored: Bool
    public let rightLogWasFloored: Bool
    public let leftLogWasWinsorized: Bool
    public let rightLogWasWinsorized: Bool
    public let isLongISIBreak: Bool

    public var trainID: String { point.trainID }
    public var trainName: String { point.trainName }
    public var qcStatus: SpikeISIStatePointQC { point.qcStatus }
    public var leftISISec: Double { point.leftISISec }
    public var rightISISec: Double { point.rightISISec }

    public init(
        point: SpikeISIStatePoint,
        rawLog10LeftISISec: Double,
        rawLog10RightISISec: Double,
        log10LeftISISec: Double,
        log10RightISISec: Double,
        localMedianLog10ISI: Double,
        localMADLog10ISI: Double,
        scaledLeft: Double,
        scaledRight: Double,
        leftLogWasFloored: Bool,
        rightLogWasFloored: Bool,
        leftLogWasWinsorized: Bool,
        rightLogWasWinsorized: Bool,
        isLongISIBreak: Bool
    ) {
        self.id = point.id
        self.point = point
        self.rawLog10LeftISISec = rawLog10LeftISISec
        self.rawLog10RightISISec = rawLog10RightISISec
        self.log10LeftISISec = log10LeftISISec
        self.log10RightISISec = log10RightISISec
        self.deltaLog10ISI = log10RightISISec - log10LeftISISec
        self.localMedianLog10ISI = localMedianLog10ISI
        self.localMADLog10ISI = localMADLog10ISI
        self.scaledLeft = scaledLeft
        self.scaledRight = scaledRight
        self.leftLogWasFloored = leftLogWasFloored
        self.rightLogWasFloored = rightLogWasFloored
        self.leftLogWasWinsorized = leftLogWasWinsorized
        self.rightLogWasWinsorized = rightLogWasWinsorized
        self.isLongISIBreak = isLongISIBreak
    }
}

public struct SpikeISIStateFeatureTrace: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainID: String
    public let trainName: String
    public let features: [SpikeISIStateFeature]

    public init(trainID: String, trainName: String, features: [SpikeISIStateFeature]) {
        self.id = trainID
        self.trainID = trainID
        self.trainName = trainName
        self.features = features
    }
}

public enum SpikeISIStateFeatureTable {
    public static func build(
        for trace: SpikeISIStateTrace,
        options: SpikeISIStateFeatureOptions = SpikeISIStateFeatureOptions()
    ) -> SpikeISIStateFeatureTrace {
        guard !trace.points.isEmpty else {
            return SpikeISIStateFeatureTrace(
                trainID: trace.trainID,
                trainName: trace.trainName,
                features: []
            )
        }

        let logPreparation = prepareLogValues(
            for: trace.points,
            winsorizeExtremeLogISI: options.winsorizeExtremeLogISI
        )
        let globalStats = globalStats(for: logPreparation.preparedValues)

        let features = trace.points.indices.map { index in
            let point = trace.points[index]
            let prepared = logPreparation.preparedPairs[index]
            let localValues = localLogValues(
                around: index,
                pairs: logPreparation.preparedPairs,
                halfWindowK: options.halfWindowK
            )
            let localMedian = median(localValues) ?? 0
            let localMAD = mad(localValues, center: localMedian) ?? 0
            let scaleDenominator: Double
            let center: Double

            switch options.scaling {
            case .robust:
                center = localMedian
                scaleDenominator = max(localMAD * 1.4826, 1e-9)
            case .zScore:
                center = globalStats.mean
                scaleDenominator = max(globalStats.standardDeviation, 1e-9)
            }

            return SpikeISIStateFeature(
                point: point,
                rawLog10LeftISISec: prepared.rawLeft,
                rawLog10RightISISec: prepared.rawRight,
                log10LeftISISec: prepared.left,
                log10RightISISec: prepared.right,
                localMedianLog10ISI: localMedian,
                localMADLog10ISI: localMAD,
                scaledLeft: (prepared.left - center) / scaleDenominator,
                scaledRight: (prepared.right - center) / scaleDenominator,
                leftLogWasFloored: prepared.leftWasFloored,
                rightLogWasFloored: prepared.rightWasFloored,
                leftLogWasWinsorized: prepared.leftWasWinsorized,
                rightLogWasWinsorized: prepared.rightWasWinsorized,
                isLongISIBreak: options.breakLongISI && options.breakThresholdSec > 0 &&
                    (point.leftISISec >= options.breakThresholdSec || point.rightISISec >= options.breakThresholdSec)
            )
        }

        return SpikeISIStateFeatureTrace(
            trainID: trace.trainID,
            trainName: trace.trainName,
            features: features
        )
    }

    public static func build(
        for traces: [SpikeISIStateTrace],
        options: SpikeISIStateFeatureOptions = SpikeISIStateFeatureOptions()
    ) -> [SpikeISIStateFeatureTrace] {
        traces.map { build(for: $0, options: options) }
    }

    public static func build(
        for dataset: SpikeDataset,
        settings: SpikeQualitySettings = SpikeQualitySettings(),
        options: SpikeISIStateFeatureOptions = SpikeISIStateFeatureOptions()
    ) -> [SpikeISIStateFeatureTrace] {
        build(
            for: SpikeISIStateTrace.build(for: dataset, settings: settings),
            options: options
        )
    }

    private struct PreparedLogPair {
        let rawLeft: Double
        let rawRight: Double
        let left: Double
        let right: Double
        let leftWasFloored: Bool
        let rightWasFloored: Bool
        let leftWasWinsorized: Bool
        let rightWasWinsorized: Bool
    }

    private static func prepareLogValues(
        for points: [SpikeISIStatePoint],
        winsorizeExtremeLogISI: Bool
    ) -> (preparedPairs: [PreparedLogPair], preparedValues: [Double]) {
        let positiveLogs = points
            .flatMap { [$0.leftISISec, $0.rightISISec] }
            .filter { $0.isFinite && $0 > 0 }
            .map(log10)
            .sorted()
        let minimumPositiveLog = positiveLogs.first ?? -6
        let floorLog = minimumPositiveLog - 0.42
        let winsorBounds: ClosedRange<Double>? = {
            guard winsorizeExtremeLogISI, positiveLogs.count >= 20 else {
                return nil
            }
            let lower = percentile(positiveLogs, p: 0.02)
            let upper = percentile(positiveLogs, p: 0.98)
            return lower...max(lower, upper)
        }()

        let pairs = points.map { point in
            let left = preparedLog(
                seconds: point.leftISISec,
                floorLog: floorLog,
                winsorBounds: winsorBounds
            )
            let right = preparedLog(
                seconds: point.rightISISec,
                floorLog: floorLog,
                winsorBounds: winsorBounds
            )
            return PreparedLogPair(
                rawLeft: left.raw,
                rawRight: right.raw,
                left: left.value,
                right: right.value,
                leftWasFloored: left.wasFloored,
                rightWasFloored: right.wasFloored,
                leftWasWinsorized: left.wasWinsorized,
                rightWasWinsorized: right.wasWinsorized
            )
        }

        return (pairs, pairs.flatMap { [$0.left, $0.right] })
    }

    private static func preparedLog(
        seconds: Double,
        floorLog: Double,
        winsorBounds: ClosedRange<Double>?
    ) -> (raw: Double, value: Double, wasFloored: Bool, wasWinsorized: Bool) {
        guard seconds.isFinite, seconds > 0 else {
            return (floorLog, floorLog, true, false)
        }

        let raw = log10(seconds)
        guard let winsorBounds else {
            return (raw, raw, false, false)
        }

        let clipped = min(max(raw, winsorBounds.lowerBound), winsorBounds.upperBound)
        return (raw, clipped, false, abs(clipped - raw) > 1e-12)
    }

    private static func localLogValues(
        around index: Int,
        pairs: [PreparedLogPair],
        halfWindowK: Int
    ) -> [Double] {
        let lower = max(0, index - halfWindowK)
        let upper = min(pairs.count - 1, index + halfWindowK)
        guard lower <= upper else {
            return []
        }
        return pairs[lower...upper].flatMap { [$0.left, $0.right] }
    }

    private static func globalStats(for values: [Double]) -> (mean: Double, standardDeviation: Double) {
        guard !values.isEmpty else {
            return (0, 1)
        }
        let mean = values.reduce(0, +) / Double(values.count)
        let variance = values.reduce(0) { total, value in
            let diff = value - mean
            return total + diff * diff
        } / Double(max(values.count - 1, 1))
        return (mean, sqrt(max(variance, 0)))
    }

    private static func median(_ values: [Double]) -> Double? {
        guard !values.isEmpty else {
            return nil
        }
        let sorted = values.sorted()
        let middle = sorted.count / 2
        if sorted.count.isMultiple(of: 2) {
            return (sorted[middle - 1] + sorted[middle]) / 2
        }
        return sorted[middle]
    }

    private static func mad(_ values: [Double], center: Double) -> Double? {
        median(values.map { abs($0 - center) })
    }

    private static func percentile(_ sortedValues: [Double], p: Double) -> Double {
        guard !sortedValues.isEmpty else {
            return 0
        }
        let clamped = min(max(p, 0), 1)
        let rawIndex = clamped * Double(sortedValues.count - 1)
        let lowerIndex = Int(floor(rawIndex))
        let upperIndex = Int(ceil(rawIndex))
        guard lowerIndex != upperIndex else {
            return sortedValues[lowerIndex]
        }
        let fraction = rawIndex - Double(lowerIndex)
        return sortedValues[lowerIndex] * (1 - fraction) + sortedValues[upperIndex] * fraction
    }
}
