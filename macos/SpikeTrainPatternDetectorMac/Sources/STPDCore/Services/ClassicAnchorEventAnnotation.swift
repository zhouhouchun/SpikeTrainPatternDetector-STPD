import Foundation

public struct ClassicAnchorEventAnnotation: Identifiable, Hashable, Sendable {
    public let id: String
    public let candidateID: String
    public let trainID: String
    public let trainName: String
    public let semanticTrack: ClassicAnchorSemanticTrack
    public let eventTrackClass: String
    public let auditRecommendedSubtype: String
    public let auditReviewStatus: String
    public let label: ClassicAnchorLabel
    public let lockLevel: ClassicAnchorLockLevel
    public let startSpikeIndex: Int
    public let endSpikeIndex: Int
    public let startISISecIndex: Int
    public let endISISecIndex: Int
    public let rawStartSec: Double
    public let rawEndSec: Double
    public let alignedStartSec: Double
    public let alignedEndSec: Double
    public let score: Double
    public let priority: Int
    public let decisionPath: String

    private init(
        id: String,
        candidateID: String,
        trainID: String,
        trainName: String,
        semanticTrack: ClassicAnchorSemanticTrack,
        eventTrackClass: String,
        auditRecommendedSubtype: String,
        auditReviewStatus: String,
        label: ClassicAnchorLabel,
        lockLevel: ClassicAnchorLockLevel,
        startSpikeIndex: Int,
        endSpikeIndex: Int,
        startISISecIndex: Int,
        endISISecIndex: Int,
        rawStartSec: Double,
        rawEndSec: Double,
        alignedStartSec: Double,
        alignedEndSec: Double,
        score: Double,
        priority: Int,
        decisionPath: String
    ) {
        self.id = id
        self.candidateID = candidateID
        self.trainID = trainID
        self.trainName = trainName
        self.semanticTrack = semanticTrack
        self.eventTrackClass = eventTrackClass
        self.auditRecommendedSubtype = auditRecommendedSubtype
        self.auditReviewStatus = auditReviewStatus
        self.label = label
        self.lockLevel = lockLevel
        self.startSpikeIndex = startSpikeIndex
        self.endSpikeIndex = endSpikeIndex
        self.startISISecIndex = startISISecIndex
        self.endISISecIndex = endISISecIndex
        self.rawStartSec = rawStartSec
        self.rawEndSec = rawEndSec
        self.alignedStartSec = alignedStartSec
        self.alignedEndSec = alignedEndSec
        self.score = score
        self.priority = priority
        self.decisionPath = decisionPath
    }

    public init(
        candidate: ClassicAnchorCandidate,
        train: SpikeTrain
    ) {
        let timestampBounds = Self.timestampBounds(
            startISIIndex: candidate.startISIIndex,
            endISIIndex: candidate.endISIIndex,
            fallbackStartSpikeIndex: candidate.startSpikeIndex,
            fallbackEndSpikeIndex: candidate.endSpikeIndex,
            train: train
        )

        self.id = candidate.id
        self.candidateID = candidate.id
        self.trainID = candidate.trainID
        self.trainName = candidate.trainName
        self.semanticTrack = candidate.auditRecommendedTrack
        self.eventTrackClass = candidate.auditRecommendedEventTrackClass
        self.auditRecommendedSubtype = candidate.auditRecommendedSubtype
        self.auditReviewStatus = candidate.auditReviewStatus
        self.label = candidate.finalLabel
        self.lockLevel = candidate.anchorLockLevel
        self.startSpikeIndex = candidate.startSpikeIndex
        self.endSpikeIndex = candidate.endSpikeIndex
        self.startISISecIndex = candidate.startISIIndex
        self.endISISecIndex = candidate.endISIIndex
        self.rawStartSec = train.timestampsSec[timestampBounds.lowerIndex]
        self.rawEndSec = train.timestampsSec[timestampBounds.upperIndex]
        self.alignedStartSec = train.alignedTimestampsSec[timestampBounds.lowerIndex]
        self.alignedEndSec = train.alignedTimestampsSec[timestampBounds.upperIndex]
        self.score = candidate.score
        self.priority = candidate.priority
        self.decisionPath = candidate.decisionPath
    }

    public var durationSec: Double {
        max(0, rawEndSec - rawStartSec)
    }

    public var visualLabel: ClassicAnchorLabel {
        if semanticTrack == .event,
           eventTrackClass == ClassicAnchorLabel.burst.rawValue {
            return .burst
        }
        return label.isBurstEventFamily ? .burst : label
    }

    public var displayFamilyName: String {
        switch visualLabel {
        case .burst:
            return "Burst"
        case .tonic:
            return "Tonic"
        case .highFrequencyTonic:
            return "HF tonic"
        case .highFrequencySpiking:
            return "HFS"
        case .pause:
            return "Pause"
        case .reject:
            return "Reject"
        case .profile:
            return "Profile"
        case .highFrequencyBurst, .longBurst, .possibleBurst:
            return "Burst"
        }
    }

    public var displaySubtypeName: String {
        switch label {
        case .burst:
            return "Burst I"
        case .highFrequencyBurst:
            return "HF burst"
        case .longBurst:
            return "Long burst"
        case .possibleBurst:
            return eventTrackClass == ClassicAnchorLabel.burst.rawValue ? "Burst II" : "Possible burst"
        case .tonic:
            return "Tonic"
        case .highFrequencyTonic:
            return "HF tonic"
        case .highFrequencySpiking:
            return "HFS"
        case .pause:
            return auditRecommendedSubtype.contains("burst_flank_pause") ? "Burst-flank pause" : "Pause"
        case .reject:
            return "Reject"
        case .profile:
            return "Profile"
        }
    }

    public var displayAuditSubtypeName: String {
        auditRecommendedSubtype
            .replacingOccurrences(of: "_", with: " ")
            .trimmingCharacters(in: .whitespacesAndNewlines)
    }

    public func clipped(to isiRange: ClosedRange<Int>, in train: SpikeTrain) -> ClassicAnchorEventAnnotation? {
        guard train.id == trainID,
              train.spikeCount >= 2,
              !train.timestampsSec.isEmpty,
              train.alignedTimestampsSec.count == train.timestampsSec.count else {
            return nil
        }

        let lowerISI = max(1, min(isiRange.lowerBound, isiRange.upperBound))
        let upperISI = min(train.spikeCount - 1, max(isiRange.lowerBound, isiRange.upperBound))
        guard lowerISI <= upperISI else {
            return nil
        }

        if lowerISI == min(startISISecIndex, endISISecIndex),
           upperISI == max(startISISecIndex, endISISecIndex) {
            return self
        }

        let clippedStartSpikeIndex = lowerISI
        let clippedEndSpikeIndex = upperISI + 1
        let startArrayIndex = clippedStartSpikeIndex - 1
        let endArrayIndex = clippedEndSpikeIndex - 1
        guard train.timestampsSec.indices.contains(startArrayIndex),
              train.timestampsSec.indices.contains(endArrayIndex) else {
            return nil
        }

        return ClassicAnchorEventAnnotation(
            id: "\(id)-isi-\(lowerISI)-\(upperISI)",
            candidateID: candidateID,
            trainID: trainID,
            trainName: trainName,
            semanticTrack: semanticTrack,
            eventTrackClass: eventTrackClass,
            auditRecommendedSubtype: auditRecommendedSubtype,
            auditReviewStatus: auditReviewStatus,
            label: label,
            lockLevel: lockLevel,
            startSpikeIndex: clippedStartSpikeIndex,
            endSpikeIndex: clippedEndSpikeIndex,
            startISISecIndex: lowerISI,
            endISISecIndex: upperISI,
            rawStartSec: train.timestampsSec[startArrayIndex],
            rawEndSec: train.timestampsSec[endArrayIndex],
            alignedStartSec: train.alignedTimestampsSec[startArrayIndex],
            alignedEndSec: train.alignedTimestampsSec[endArrayIndex],
            score: score,
            priority: priority,
            decisionPath: decisionPath
        )
    }

    public func coveredISIIndices(in train: SpikeTrain) -> ClosedRange<Int>? {
        guard train.id == trainID,
              train.spikeCount >= 2,
              train.isiSec.count == train.spikeCount else {
            return nil
        }

        let firstISIIndex = max(1, min(startISISecIndex, endISISecIndex))
        let lastISIIndex = min(train.isiSec.count - 1, max(startISISecIndex, endISISecIndex))
        guard firstISIIndex <= lastISIIndex else {
            return nil
        }

        return firstISIIndex...lastISIIndex
    }

    private static func clampedArrayIndex(_ index: Int, train: SpikeTrain) -> Int {
        guard !train.timestampsSec.isEmpty else {
            return 0
        }
        return min(max(index, 0), train.timestampsSec.count - 1)
    }

    private static func timestampBounds(
        startISIIndex: Int,
        endISIIndex: Int,
        fallbackStartSpikeIndex: Int,
        fallbackEndSpikeIndex: Int,
        train: SpikeTrain
    ) -> (lowerIndex: Int, upperIndex: Int) {
        let lowerISI = max(1, min(startISIIndex, endISIIndex))
        let upperISI = min(train.spikeCount - 1, max(startISIIndex, endISIIndex))
        if lowerISI <= upperISI,
           train.timestampsSec.indices.contains(lowerISI - 1),
           train.timestampsSec.indices.contains(upperISI) {
            return (lowerISI - 1, upperISI)
        }

        let startArrayIndex = clampedArrayIndex(fallbackStartSpikeIndex - 1, train: train)
        let endArrayIndex = clampedArrayIndex(fallbackEndSpikeIndex - 1, train: train)
        return (
            min(startArrayIndex, endArrayIndex),
            max(startArrayIndex, endArrayIndex)
        )
    }
}

public extension ClassicAnchorDetectionRun {
    func eventAnnotations(
        in dataset: SpikeDataset,
        selectedOnly: Bool = true,
        tracks: Set<ClassicAnchorSemanticTrack> = [.event],
        includeEvidenceOnly: Bool = false,
        exclusiveFinalProjection: Bool = true
    ) -> [ClassicAnchorEventAnnotation] {
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        let annotations: [ClassicAnchorEventAnnotation] = candidates.compactMap { candidate in
            guard !selectedOnly || candidate.selectedForAuto else {
                return nil
            }
            guard includeEvidenceOnly || !candidate.isStructuralPausePriorEvidence else {
                return nil
            }
            guard tracks.contains(candidate.auditRecommendedTrack) else {
                return nil
            }
            guard let train = trainsByID[candidate.trainID], train.spikeCount > 0 else {
                return nil
            }
            return ClassicAnchorEventAnnotation(candidate: candidate, train: train)
        }
        guard selectedOnly, exclusiveFinalProjection, !includeEvidenceOnly else {
            return annotations
        }
        return Self.exclusiveFinalProjection(annotations, trainsByID: trainsByID)
    }

    private static func exclusiveFinalProjection(
        _ annotations: [ClassicAnchorEventAnnotation],
        trainsByID: [String: SpikeTrain]
    ) -> [ClassicAnchorEventAnnotation] {
        guard !annotations.isEmpty else {
            return []
        }

        var projected: [ClassicAnchorEventAnnotation] = []
        let groupedByTrain = Dictionary(grouping: annotations, by: \.trainID)
        for (trainID, trainAnnotations) in groupedByTrain {
            guard let train = trainsByID[trainID] else {
                continue
            }

            var winnerByISI: [Int: ClassicAnchorEventAnnotation] = [:]
            for annotation in trainAnnotations {
                guard let coveredISIIndices = annotation.coveredISIIndices(in: train) else {
                    continue
                }

                for isiIndex in coveredISIIndices {
                    guard train.isiSec.indices.contains(isiIndex) else {
                        continue
                    }
                    if let current = winnerByISI[isiIndex] {
                        if shouldReplaceProjection(current: current, with: annotation) {
                            winnerByISI[isiIndex] = annotation
                        }
                    } else {
                        winnerByISI[isiIndex] = annotation
                    }
                }
            }

            let sortedISI = winnerByISI.keys.sorted()
            var runStart: Int?
            var previousISI: Int?
            var currentAnnotation: ClassicAnchorEventAnnotation?

            func flushRun() {
                guard let runStart,
                      let previousISI,
                      let currentAnnotation,
                      let clipped = currentAnnotation.clipped(to: runStart...previousISI, in: train) else {
                    return
                }
                projected.append(clipped)
            }

            for isiIndex in sortedISI {
                guard let annotation = winnerByISI[isiIndex] else {
                    continue
                }
                if let previous = previousISI,
                   let current = currentAnnotation,
                   previous + 1 == isiIndex,
                   current.candidateID == annotation.candidateID {
                    previousISI = isiIndex
                    continue
                }

                flushRun()
                runStart = isiIndex
                previousISI = isiIndex
                currentAnnotation = annotation
            }
            flushRun()
        }

        return projected.sorted { lhs, rhs in
            if lhs.trainName != rhs.trainName {
                return lhs.trainName < rhs.trainName
            }
            if lhs.startISISecIndex != rhs.startISISecIndex {
                return lhs.startISISecIndex < rhs.startISISecIndex
            }
            if lhs.projectionPriority != rhs.projectionPriority {
                return lhs.projectionPriority > rhs.projectionPriority
            }
            return lhs.id < rhs.id
        }
    }

    private static func shouldReplaceProjection(
        current: ClassicAnchorEventAnnotation,
        with candidate: ClassicAnchorEventAnnotation
    ) -> Bool {
        if current.projectionPriority != candidate.projectionPriority {
            return candidate.projectionPriority > current.projectionPriority
        }
        if current.priority != candidate.priority {
            return candidate.priority > current.priority
        }
        if current.score != candidate.score {
            return candidate.score > current.score
        }
        let currentLength = abs(current.endISISecIndex - current.startISISecIndex)
        let candidateLength = abs(candidate.endISISecIndex - candidate.startISISecIndex)
        if currentLength != candidateLength {
            return candidateLength < currentLength
        }
        return candidate.id < current.id
    }
}

private extension ClassicAnchorEventAnnotation {
    var projectionPriority: Int {
        if visualLabel == .burst {
            return 4
        }
        switch visualLabel {
        case .tonic, .highFrequencyTonic:
            return 3
        case .highFrequencySpiking:
            return 2
        case .pause:
            return 1
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst:
            return 4
        case .reject, .profile:
            return 0
        }
    }
}
