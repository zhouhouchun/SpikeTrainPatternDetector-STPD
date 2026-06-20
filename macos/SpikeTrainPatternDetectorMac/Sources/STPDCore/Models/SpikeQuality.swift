import Foundation

public enum QualityWarningLevel: String, CaseIterable, Sendable {
    case ok
    case warning
    case error

    public var title: String {
        switch self {
        case .ok:
            return "OK"
        case .warning:
            return "Warning"
        case .error:
            return "Error"
        }
    }
}

public enum QualityDisplayUnit: String, CaseIterable, Sendable {
    case seconds = "s"
    case milliseconds = "ms"

    public var scaleFromSeconds: Double {
        switch self {
        case .seconds:
            return 1
        case .milliseconds:
            return 1000
        }
    }
}

public struct SpikeQualitySettings: Hashable, Sendable {
    public var artifactThresholdSec: Double
    public var refractorySuspectThresholdSec: Double
    public var displayUnit: QualityDisplayUnit

    public init(
        artifactThresholdSec: Double = 0.0009,
        refractorySuspectThresholdSec: Double = 0.0010,
        displayUnit: QualityDisplayUnit = .milliseconds
    ) {
        self.artifactThresholdSec = artifactThresholdSec
        self.refractorySuspectThresholdSec = max(refractorySuspectThresholdSec, artifactThresholdSec)
        self.displayUnit = displayUnit
    }
}

public struct SpikeDatasetQualityReport: Hashable, Sendable {
    public let rows: [SpikeTrainQuality]
    public let artifactDetails: [ArtifactISIDetail]
    public let duplicateDetails: [DuplicateTimestampDetail]

    public init(
        rows: [SpikeTrainQuality],
        artifactDetails: [ArtifactISIDetail],
        duplicateDetails: [DuplicateTimestampDetail]
    ) {
        self.rows = rows
        self.artifactDetails = artifactDetails
        self.duplicateDetails = duplicateDetails
    }

    public var errorCount: Int {
        rows.filter { $0.warningLevel == .error }.count
    }

    public var warningCount: Int {
        rows.filter { $0.warningLevel == .warning }.count
    }

    public var artifactISICount: Int {
        rows.reduce(0) { $0 + $1.artifactISICount }
    }

    public var refractorySuspectISICount: Int {
        rows.reduce(0) { $0 + $1.refractorySuspectISICount }
    }

    public var duplicateTimestampCount: Int {
        rows.reduce(0) { $0 + $1.duplicateTimestampCount }
    }

    public var droppedDuplicateTimestampCount: Int {
        rows.reduce(0) { $0 + $1.droppedDuplicateTimestampCount }
    }
}

public struct SpikeTrainQuality: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainName: String
    public let warningLevel: QualityWarningLevel
    public let warningMessage: String
    public let spikeCount: Int
    public let firstSpikeSec: Double?
    public let lastSpikeSec: Double?
    public let durationSec: Double?
    public let firingRateHz: Double?
    public let rawMinISISec: Double?
    public let minValidISISec: Double?
    public let artifactMinISISec: Double?
    public let medianISISec: Double?
    public let maxISISec: Double?
    public let duplicateTimestampCount: Int
    public let zeroOrNegativeISICount: Int
    public let zeroOrNegativeTimestampStepCount: Int
    public let inputWasUnsorted: Bool
    public let inputNonmonotonicStepCount: Int
    public let inputDuplicateTimestampStepCount: Int
    public let inputZeroOrNegativeStepCount: Int
    public let droppedDuplicateTimestampCount: Int
    public let duplicateTimestampPolicy: DuplicateTimestampPolicy
    public let artifactISICount: Int
    public let artifactFraction: Double?
    public let refractorySuspectISICount: Int
    public let refractorySuspectFraction: Double?
    public let validISICount: Int
    public let percentileStatus: String

    public init(
        trainName: String,
        warningLevel: QualityWarningLevel,
        warningMessage: String,
        spikeCount: Int,
        firstSpikeSec: Double?,
        lastSpikeSec: Double?,
        durationSec: Double?,
        firingRateHz: Double?,
        rawMinISISec: Double?,
        minValidISISec: Double?,
        artifactMinISISec: Double?,
        medianISISec: Double?,
        maxISISec: Double?,
        duplicateTimestampCount: Int,
        zeroOrNegativeISICount: Int,
        zeroOrNegativeTimestampStepCount: Int,
        inputWasUnsorted: Bool,
        inputNonmonotonicStepCount: Int,
        inputDuplicateTimestampStepCount: Int,
        inputZeroOrNegativeStepCount: Int,
        droppedDuplicateTimestampCount: Int,
        duplicateTimestampPolicy: DuplicateTimestampPolicy,
        artifactISICount: Int,
        artifactFraction: Double?,
        refractorySuspectISICount: Int,
        refractorySuspectFraction: Double?,
        validISICount: Int,
        percentileStatus: String
    ) {
        self.id = trainName
        self.trainName = trainName
        self.warningLevel = warningLevel
        self.warningMessage = warningMessage
        self.spikeCount = spikeCount
        self.firstSpikeSec = firstSpikeSec
        self.lastSpikeSec = lastSpikeSec
        self.durationSec = durationSec
        self.firingRateHz = firingRateHz
        self.rawMinISISec = rawMinISISec
        self.minValidISISec = minValidISISec
        self.artifactMinISISec = artifactMinISISec
        self.medianISISec = medianISISec
        self.maxISISec = maxISISec
        self.duplicateTimestampCount = duplicateTimestampCount
        self.zeroOrNegativeISICount = zeroOrNegativeISICount
        self.zeroOrNegativeTimestampStepCount = zeroOrNegativeTimestampStepCount
        self.inputWasUnsorted = inputWasUnsorted
        self.inputNonmonotonicStepCount = inputNonmonotonicStepCount
        self.inputDuplicateTimestampStepCount = inputDuplicateTimestampStepCount
        self.inputZeroOrNegativeStepCount = inputZeroOrNegativeStepCount
        self.droppedDuplicateTimestampCount = droppedDuplicateTimestampCount
        self.duplicateTimestampPolicy = duplicateTimestampPolicy
        self.artifactISICount = artifactISICount
        self.artifactFraction = artifactFraction
        self.refractorySuspectISICount = refractorySuspectISICount
        self.refractorySuspectFraction = refractorySuspectFraction
        self.validISICount = validISICount
        self.percentileStatus = percentileStatus
    }
}

public struct ArtifactISIDetail: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainName: String
    public let isiIndex: Int
    public let leftSpikeTimeSec: Double?
    public let rightSpikeTimeSec: Double
    public let isiSec: Double
    public let thresholdSec: Double

    public init(
        trainName: String,
        isiIndex: Int,
        leftSpikeTimeSec: Double?,
        rightSpikeTimeSec: Double,
        isiSec: Double,
        thresholdSec: Double
    ) {
        self.id = "\(trainName)-artifact-\(isiIndex)"
        self.trainName = trainName
        self.isiIndex = isiIndex
        self.leftSpikeTimeSec = leftSpikeTimeSec
        self.rightSpikeTimeSec = rightSpikeTimeSec
        self.isiSec = isiSec
        self.thresholdSec = thresholdSec
    }
}

public struct DuplicateTimestampDetail: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainName: String
    public let timestampSec: Double
    public let duplicateCount: Int
    public let sortedRowIndices: [Int]
    public let inputOrderIndices: [Int]
    public let policy: DuplicateTimestampPolicy

    public init(
        trainName: String,
        timestampSec: Double,
        duplicateCount: Int,
        sortedRowIndices: [Int],
        inputOrderIndices: [Int],
        policy: DuplicateTimestampPolicy
    ) {
        self.id = "\(trainName)-duplicate-\(timestampSec)"
        self.trainName = trainName
        self.timestampSec = timestampSec
        self.duplicateCount = duplicateCount
        self.sortedRowIndices = sortedRowIndices
        self.inputOrderIndices = inputOrderIndices
        self.policy = policy
    }
}
