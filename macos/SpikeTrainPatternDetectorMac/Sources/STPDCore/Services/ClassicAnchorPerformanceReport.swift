import Foundation

public struct ClassicAnchorPerformanceStageTiming: Sendable {
    public let name: String
    public let wallTimeMs: Double

    public init(name: String, wallTimeMs: Double) {
        self.name = name
        self.wallTimeMs = wallTimeMs
    }
}

public struct ClassicAnchorTrainPerformanceSummary: Sendable, Identifiable {
    public let id: String
    public let trainName: String
    public let spikeCount: Int
    public let candidateCount: Int
    public let selectedAutoCount: Int
    public let hfsBurstAuditRowCount: Int
    public let stageTimings: [ClassicAnchorPerformanceStageTiming]

    public init(
        trainID: String,
        trainName: String,
        spikeCount: Int,
        candidateCount: Int,
        selectedAutoCount: Int,
        hfsBurstAuditRowCount: Int,
        stageTimings: [ClassicAnchorPerformanceStageTiming]
    ) {
        self.id = trainID
        self.trainName = trainName
        self.spikeCount = spikeCount
        self.candidateCount = candidateCount
        self.selectedAutoCount = selectedAutoCount
        self.hfsBurstAuditRowCount = hfsBurstAuditRowCount
        self.stageTimings = stageTimings
    }

    public var totalMeasuredTimeMs: Double {
        stageTimings.reduce(0) { $0 + $1.wallTimeMs }
    }
}

public struct ClassicAnchorDetectionPerformanceReport: Sendable {
    public let totalWallTimeMs: Double
    public let trainCount: Int
    public let spikeCount: Int
    public let stageTimings: [ClassicAnchorPerformanceStageTiming]
    public let trainSummaries: [ClassicAnchorTrainPerformanceSummary]

    public init(
        totalWallTimeMs: Double,
        trainCount: Int,
        spikeCount: Int,
        stageTimings: [ClassicAnchorPerformanceStageTiming],
        trainSummaries: [ClassicAnchorTrainPerformanceSummary]
    ) {
        self.totalWallTimeMs = totalWallTimeMs
        self.trainCount = trainCount
        self.spikeCount = spikeCount
        self.stageTimings = stageTimings
        self.trainSummaries = trainSummaries
    }

    public var slowestTrain: ClassicAnchorTrainPerformanceSummary? {
        trainSummaries.max { $0.totalMeasuredTimeMs < $1.totalMeasuredTimeMs }
    }

    public func slowestTrains(limit: Int) -> [ClassicAnchorTrainPerformanceSummary] {
        guard limit > 0 else {
            return []
        }
        return Array(
            trainSummaries
                .sorted { lhs, rhs in
                    if lhs.totalMeasuredTimeMs != rhs.totalMeasuredTimeMs {
                        return lhs.totalMeasuredTimeMs > rhs.totalMeasuredTimeMs
                    }
                    return lhs.trainName < rhs.trainName
                }
                .prefix(limit)
        )
    }
}
