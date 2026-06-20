import Foundation

public struct SpikeISIEvent: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainID: String
    public let trainName: String
    public let intervalIndex: Int
    public let leftSpikeIndex: Int
    public let rightSpikeIndex: Int
    public let leftSpikeTimeSec: Double
    public let rightSpikeTimeSec: Double
    public let alignedLeftSpikeTimeSec: Double
    public let alignedRightSpikeTimeSec: Double
    public let isiSec: Double
    public let isArtifact: Bool
    public let isRefractorySuspect: Bool

    public init(
        trainID: String,
        trainName: String,
        intervalIndex: Int,
        leftSpikeIndex: Int,
        rightSpikeIndex: Int,
        leftSpikeTimeSec: Double,
        rightSpikeTimeSec: Double,
        alignedLeftSpikeTimeSec: Double,
        alignedRightSpikeTimeSec: Double,
        isiSec: Double,
        isArtifact: Bool,
        isRefractorySuspect: Bool
    ) {
        self.id = "\(trainID)-isi-\(intervalIndex)"
        self.trainID = trainID
        self.trainName = trainName
        self.intervalIndex = intervalIndex
        self.leftSpikeIndex = leftSpikeIndex
        self.rightSpikeIndex = rightSpikeIndex
        self.leftSpikeTimeSec = leftSpikeTimeSec
        self.rightSpikeTimeSec = rightSpikeTimeSec
        self.alignedLeftSpikeTimeSec = alignedLeftSpikeTimeSec
        self.alignedRightSpikeTimeSec = alignedRightSpikeTimeSec
        self.isiSec = isiSec
        self.isArtifact = isArtifact
        self.isRefractorySuspect = isRefractorySuspect
    }
}

public struct SpikeISITrace: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainID: String
    public let trainName: String
    public let events: [SpikeISIEvent]

    public init(trainID: String, trainName: String, events: [SpikeISIEvent]) {
        self.id = trainID
        self.trainID = trainID
        self.trainName = trainName
        self.events = events
    }

    public static func build(
        for train: SpikeTrain,
        settings: SpikeQualitySettings = SpikeQualitySettings()
    ) -> SpikeISITrace {
        var events: [SpikeISIEvent] = []
        events.reserveCapacity(max(0, train.spikeCount - 1))

        for rightIndex in train.timestampsSec.indices.dropFirst() {
            guard train.isiSec.indices.contains(rightIndex),
                  let isi = train.isiSec[rightIndex],
                  isi.isFinite else {
                continue
            }

            let leftIndex = rightIndex - 1
            let isArtifact = isArtifactISI(isi, threshold: settings.artifactThresholdSec)
            events.append(
                SpikeISIEvent(
                    trainID: train.id,
                    trainName: train.name,
                    intervalIndex: rightIndex,
                    leftSpikeIndex: leftIndex + 1,
                    rightSpikeIndex: rightIndex + 1,
                    leftSpikeTimeSec: train.timestampsSec[leftIndex],
                    rightSpikeTimeSec: train.timestampsSec[rightIndex],
                    alignedLeftSpikeTimeSec: train.alignedTimestampsSec[leftIndex],
                    alignedRightSpikeTimeSec: train.alignedTimestampsSec[rightIndex],
                    isiSec: isi,
                    isArtifact: isArtifact,
                    isRefractorySuspect: isRefractorySuspectISI(
                        isi,
                        artifactThreshold: settings.artifactThresholdSec,
                        refractoryThreshold: settings.refractorySuspectThresholdSec
                    )
                )
            )
        }

        return SpikeISITrace(trainID: train.id, trainName: train.name, events: events)
    }

    public static func build(
        for dataset: SpikeDataset,
        settings: SpikeQualitySettings = SpikeQualitySettings()
    ) -> [SpikeISITrace] {
        dataset.trains.map { build(for: $0, settings: settings) }
    }

    private static func isArtifactISI(_ value: Double, threshold: Double) -> Bool {
        let tolerance = max(1e-12, abs(threshold) * 1e-6)
        return value < threshold - tolerance
    }

    private static func isRefractorySuspectISI(
        _ value: Double,
        artifactThreshold: Double,
        refractoryThreshold: Double
    ) -> Bool {
        guard refractoryThreshold > artifactThreshold else {
            return false
        }

        let artifactTolerance = max(1e-12, abs(artifactThreshold) * 1e-6)
        let refractoryTolerance = max(1e-12, abs(refractoryThreshold) * 1e-6)
        return value >= artifactThreshold - artifactTolerance &&
            value < refractoryThreshold - refractoryTolerance
    }
}
