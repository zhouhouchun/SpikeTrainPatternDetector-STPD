import Foundation

/// One detector candidate's automatic evidence carried through public-event projection.
///
/// Public burst events may temporarily merge several candidates through validated manual support.
/// Keeping this source-level mapping prevents a later split from inheriting the merged event's
/// aggregate label or identity.
public struct ClassicAnchorAutomaticEventSource: Hashable, Sendable {
    public let annotationID: String
    public let candidateID: String
    public let trainID: String
    /// Automatic support established before manual projection. Automatic exclusive projection may
    /// narrow this baseline, but manual lock/veto projection must never mutate it: removing a manual
    /// action must be able to reconstruct the same automatic evidence that entered the manual layer.
    public let supportISIIndices: [Int]
    public let semanticTrack: ClassicAnchorSemanticTrack
    public let eventTrackClass: String
    public let auditRecommendedSubtype: String
    public let auditReviewStatus: String
    public let label: ClassicAnchorLabel
    public let lockLevel: ClassicAnchorLockLevel
    public let stateTonicSubtype: String?
    public let score: Double
    public let priority: Int
    public let decisionPath: String

    init(
        annotationID: String,
        candidateID: String,
        trainID: String,
        supportISIIndices: [Int],
        semanticTrack: ClassicAnchorSemanticTrack,
        eventTrackClass: String,
        auditRecommendedSubtype: String,
        auditReviewStatus: String,
        label: ClassicAnchorLabel,
        lockLevel: ClassicAnchorLockLevel,
        stateTonicSubtype: String?,
        score: Double,
        priority: Int,
        decisionPath: String
    ) {
        self.annotationID = annotationID
        self.candidateID = candidateID
        self.trainID = trainID
        self.supportISIIndices = Array(Set(supportISIIndices)).sorted()
        self.semanticTrack = semanticTrack
        self.eventTrackClass = eventTrackClass
        self.auditRecommendedSubtype = auditRecommendedSubtype
        self.auditReviewStatus = auditReviewStatus
        self.label = label
        self.lockLevel = lockLevel
        self.stateTonicSubtype = stateTonicSubtype
        self.score = score
        self.priority = priority
        self.decisionPath = decisionPath
    }

    /// Narrow the automatic baseline itself. This is reserved for automatic projection (for example,
    /// exclusive winner clipping) before the event enters the manual-annotation layer.
    func retainingSupport(_ retainedISIs: Set<Int>) -> ClassicAnchorAutomaticEventSource? {
        let retained = Set(supportISIIndices).intersection(retainedISIs)
        guard !retained.isEmpty else { return nil }
        return ClassicAnchorAutomaticEventSource(
            annotationID: annotationID,
            candidateID: candidateID,
            trainID: trainID,
            supportISIIndices: retained.sorted(),
            semanticTrack: semanticTrack,
            eventTrackClass: eventTrackClass,
            auditRecommendedSubtype: auditRecommendedSubtype,
            auditReviewStatus: auditReviewStatus,
            label: label,
            lockLevel: lockLevel,
            stateTonicSubtype: stateTonicSubtype,
            score: score,
            priority: priority,
            decisionPath: decisionPath
        )
    }
}

public struct ClassicAnchorEventAnnotation: Identifiable, Hashable, Sendable {
    public let id: String
    public let candidateID: String
    /// All raw detector candidates that contribute automatic evidence to this public event.
    /// `candidateID` remains the deterministic primary source for backward-compatible review links.
    public let sourceCandidateIDs: [String]
    /// Automatic detector support retained inside this public event. Manual burst support may extend
    /// the displayed geometry, but is intentionally excluded here so removing a manual annotation can
    /// reconstruct the automatic component without treating prior manual coverage as detector evidence.
    public let automaticSupportISIIndices: [Int]
    /// Source-level automatic evidence. Unlike the aggregate fields above, this preserves which
    /// candidate supplied each ISI and that candidate's original authority across merge/split cycles.
    public let automaticEventSources: [ClassicAnchorAutomaticEventSource]
    public let trainID: String
    public let trainName: String
    public let semanticTrack: ClassicAnchorSemanticTrack
    public let eventTrackClass: String
    public let auditRecommendedSubtype: String
    public let auditReviewStatus: String
    public let label: ClassicAnchorLabel
    public let lockLevel: ClassicAnchorLockLevel
    /// Tonic-family subtype carried from the source candidate (`classic`, `irregular`,
    /// `high_frequency`), or `nil` for non-tonic annotations. Auditable display only.
    public let stateTonicSubtype: String?
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
        automaticEventSources: [ClassicAnchorAutomaticEventSource],
        trainID: String,
        trainName: String,
        semanticTrack: ClassicAnchorSemanticTrack,
        eventTrackClass: String,
        auditRecommendedSubtype: String,
        auditReviewStatus: String,
        label: ClassicAnchorLabel,
        lockLevel: ClassicAnchorLockLevel,
        stateTonicSubtype: String?,
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
        let normalizedSources = Self.normalizedAutomaticSources(automaticEventSources)
        self.sourceCandidateIDs = Array(Set(normalizedSources.map(\.candidateID))).sorted()
        let visibleLowerISI = max(1, min(startISISecIndex, endISISecIndex))
        let visibleUpperISI = max(visibleLowerISI, max(startISISecIndex, endISISecIndex))
        let visibleISIs = Set(visibleLowerISI...visibleUpperISI)
        self.automaticSupportISIIndices = Array(
            Set(normalizedSources.flatMap(\.supportISIIndices)).intersection(visibleISIs)
        ).sorted()
        self.automaticEventSources = normalizedSources
        self.trainID = trainID
        self.trainName = trainName
        self.semanticTrack = semanticTrack
        self.eventTrackClass = eventTrackClass
        self.auditRecommendedSubtype = auditRecommendedSubtype
        self.auditReviewStatus = auditReviewStatus
        self.label = label
        self.lockLevel = lockLevel
        self.stateTonicSubtype = Self.normalizedTonicSubtype(stateTonicSubtype, for: label)
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
        let automaticLower = max(1, min(candidate.startISIIndex, candidate.endISIIndex))
        let automaticUpper = min(train.spikeCount - 1, max(candidate.startISIIndex, candidate.endISIIndex))
        let automaticSupport = automaticLower <= automaticUpper
            ? Array(automaticLower...automaticUpper)
            : []
        self.sourceCandidateIDs = [candidate.id]
        self.automaticSupportISIIndices = automaticSupport
        self.automaticEventSources = [
            ClassicAnchorAutomaticEventSource(
                annotationID: candidate.id,
                candidateID: candidate.id,
                trainID: candidate.trainID,
                supportISIIndices: automaticSupport,
                semanticTrack: candidate.auditRecommendedTrack,
                eventTrackClass: candidate.auditRecommendedEventTrackClass,
                auditRecommendedSubtype: candidate.auditRecommendedSubtype,
                auditReviewStatus: candidate.auditReviewStatus,
                label: candidate.finalLabel,
                lockLevel: candidate.anchorLockLevel,
                stateTonicSubtype: Self.normalizedTonicSubtype(
                    candidate.stateTonicSubtype,
                    for: candidate.finalLabel
                ),
                score: candidate.score,
                priority: candidate.priority,
                decisionPath: candidate.decisionPath
            )
        ]
        self.trainID = candidate.trainID
        self.trainName = candidate.trainName
        self.semanticTrack = candidate.auditRecommendedTrack
        self.eventTrackClass = candidate.auditRecommendedEventTrackClass
        self.auditRecommendedSubtype = candidate.auditRecommendedSubtype
        self.auditReviewStatus = candidate.auditReviewStatus
        self.label = candidate.finalLabel
        self.lockLevel = candidate.anchorLockLevel
        self.stateTonicSubtype = Self.normalizedTonicSubtype(
            candidate.stateTonicSubtype,
            for: candidate.finalLabel
        )
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
            switch stateTonicSubtype {
            case "irregular":
                return "Irregular tonic"
            case "classic":
                return "Classic tonic"
            default:
                return "Tonic"
            }
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
            automaticEventSources: automaticEventSources.compactMap {
                $0.retainingSupport(Set(lowerISI...upperISI))
            },
            trainID: trainID,
            trainName: trainName,
            semanticTrack: semanticTrack,
            eventTrackClass: eventTrackClass,
            auditRecommendedSubtype: auditRecommendedSubtype,
            auditReviewStatus: auditReviewStatus,
            label: label,
            lockLevel: lockLevel,
            stateTonicSubtype: stateTonicSubtype,
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

    /// Reframe one non-burst automatic source after a positive-manual lock. The source's automatic
    /// baseline is intentionally preserved even when the visible component is clipped, so a later
    /// projection without that lock can restore the original automatic geometry.
    func projectedAutomaticComponent(
        source: ClassicAnchorAutomaticEventSource,
        to isiRange: ClosedRange<Int>,
        in train: SpikeTrain
    ) -> ClassicAnchorEventAnnotation? {
        guard source.trainID == trainID, train.id == trainID else {
            return nil
        }

        return Self.materializedAutomaticComponent(
            source: source,
            to: isiRange,
            in: train,
            manualLockApplied: true
        )
    }

    /// Materialize one automatic source without borrowing mutable public-event geometry or
    /// provenance. The explicit lock flag lets stateful re-projection remove an obsolete manual
    /// marker when a positive lock is deleted.
    static func materializedAutomaticComponent(
        source: ClassicAnchorAutomaticEventSource,
        to isiRange: ClosedRange<Int>,
        in train: SpikeTrain,
        manualLockApplied: Bool
    ) -> ClassicAnchorEventAnnotation? {
        guard source.trainID == train.id,
              train.spikeCount >= 2,
              !train.timestampsSec.isEmpty,
              train.alignedTimestampsSec.count == train.timestampsSec.count else {
            return nil
        }

        let lowerISI = max(1, min(isiRange.lowerBound, isiRange.upperBound))
        let upperISI = min(train.spikeCount - 1, max(isiRange.lowerBound, isiRange.upperBound))
        guard lowerISI <= upperISI else { return nil }

        let startSpike = lowerISI
        let endSpike = upperISI + 1
        let startArrayIndex = startSpike - 1
        let endArrayIndex = endSpike - 1
        guard train.timestampsSec.indices.contains(startArrayIndex),
              train.timestampsSec.indices.contains(endArrayIndex) else {
            return nil
        }

        let existingTokens = source.decisionPath.split(separator: ";").map(String.init)
        var normalizedTokens = existingTokens.filter { !$0.hasPrefix("manual_projection_") }
        if manualLockApplied {
            normalizedTokens.append("manual_projection_non_burst_lock=true")
        }
        let normalizedPath = normalizedTokens.joined(separator: ";")

        return ClassicAnchorEventAnnotation(
            id: "\(source.candidateID)-public-\(source.label.rawValue)-isi-\(lowerISI)-\(upperISI)",
            candidateID: source.candidateID,
            automaticEventSources: [source],
            trainID: source.trainID,
            trainName: train.name,
            semanticTrack: source.semanticTrack,
            eventTrackClass: source.eventTrackClass,
            auditRecommendedSubtype: source.auditRecommendedSubtype,
            auditReviewStatus: source.auditReviewStatus,
            label: source.label,
            lockLevel: source.lockLevel,
            stateTonicSubtype: source.stateTonicSubtype,
            startSpikeIndex: startSpike,
            endSpikeIndex: endSpike,
            startISISecIndex: lowerISI,
            endISISecIndex: upperISI,
            rawStartSec: train.timestampsSec[startArrayIndex],
            rawEndSec: train.timestampsSec[endArrayIndex],
            alignedStartSec: train.alignedTimestampsSec[startArrayIndex],
            alignedEndSec: train.alignedTimestampsSec[endArrayIndex],
            score: source.score,
            priority: source.priority,
            decisionPath: normalizedPath
        )
    }

    /// Reframe one final, train-level burst component after manual lock/veto projection. The event
    /// may combine automatic evidence from several candidates and may include validated manual burst
    /// support. Raw candidates remain unchanged; this method creates only the public representation.
    public func projectedBurstComponent(
        to isiRange: ClosedRange<Int>,
        in train: SpikeTrain,
        automaticEventSources contributingSources: [ClassicAnchorAutomaticEventSource],
        includesManualSupport: Bool
    ) -> ClassicAnchorEventAnnotation? {
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

        let componentISIs = Set(lowerISI...upperISI)
        // Contributing sources carry the immutable automatic baseline. Current visible automatic
        // support is derived from the component geometry by the event initializer; do not destroy the
        // baseline here, because a later manual re-projection may need to restore suppressed ISIs.
        let normalizedSources = Self.normalizedAutomaticSources(contributingSources)
        guard let primarySource = normalizedSources.sorted(by: Self.automaticSourceIsOrderedBefore).first else {
            return nil
        }
        let authoritySource = normalizedSources
            .filter { $0.label == .possibleBurst }
            .sorted(by: Self.automaticSourceIdentityIsOrderedBefore)
            .first ?? primarySource
        let normalizedSourceIDs = Array(Set(normalizedSources.map(\.candidateID))).sorted()
        let normalizedAutomaticSupport = Array(
            Set(normalizedSources.flatMap(\.supportISIIndices)).intersection(componentISIs)
        ).sorted()
        let unchangedRange = lowerISI == min(startISISecIndex, endISISecIndex)
            && upperISI == max(startISISecIndex, endISISecIndex)
        let projectionTokens = [
            "manual_projection_source_candidate_count=\(normalizedSourceIDs.count)",
            "manual_projection_includes_manual_burst_support=\(includesManualSupport)",
            "manual_projection_burst_authority=possible_if_any_source_possible"
        ]
        let existingTokens = primarySource.decisionPath.split(separator: ";").map(String.init)
        let currentTokens = decisionPath.split(separator: ";").map(String.init)
        let wasProjected = currentTokens.contains { $0.hasPrefix("manual_projection_") }
        let normalizedPath = (
            existingTokens.filter { !$0.hasPrefix("manual_projection_") } + projectionTokens
        ).joined(separator: ";")
        let projectedID = "\(primarySource.candidateID)-public-burst-isi-\(lowerISI)-\(upperISI)"
        let authorityUnchanged = label == authoritySource.label
            && semanticTrack == authoritySource.semanticTrack
            && eventTrackClass == authoritySource.eventTrackClass
            && auditRecommendedSubtype == authoritySource.auditRecommendedSubtype
            && auditReviewStatus == authoritySource.auditReviewStatus
            && lockLevel == authoritySource.lockLevel
            && stateTonicSubtype == authoritySource.stateTonicSubtype
        if unchangedRange,
           normalizedSourceIDs == sourceCandidateIDs,
           normalizedAutomaticSupport == automaticSupportISIIndices,
           normalizedSources == automaticEventSources,
           candidateID == primarySource.candidateID,
           score == primarySource.score,
           priority == primarySource.priority,
           authorityUnchanged {
            if !wasProjected && !includesManualSupport {
                return self
            }
            if wasProjected,
               id == projectedID,
               decisionPath == normalizedPath {
                return self
            }
        }

        let startSpike = lowerISI
        let endSpike = upperISI + 1
        let startArrayIndex = startSpike - 1
        let endArrayIndex = endSpike - 1
        guard train.timestampsSec.indices.contains(startArrayIndex),
              train.timestampsSec.indices.contains(endArrayIndex) else {
            return nil
        }

        return ClassicAnchorEventAnnotation(
            id: projectedID,
            candidateID: primarySource.candidateID,
            automaticEventSources: normalizedSources,
            trainID: trainID,
            trainName: trainName,
            semanticTrack: authoritySource.semanticTrack,
            eventTrackClass: authoritySource.eventTrackClass,
            auditRecommendedSubtype: authoritySource.auditRecommendedSubtype,
            auditReviewStatus: authoritySource.auditReviewStatus,
            label: authoritySource.label,
            lockLevel: authoritySource.lockLevel,
            stateTonicSubtype: authoritySource.stateTonicSubtype,
            startSpikeIndex: startSpike,
            endSpikeIndex: endSpike,
            startISISecIndex: lowerISI,
            endISISecIndex: upperISI,
            rawStartSec: train.timestampsSec[startArrayIndex],
            rawEndSec: train.timestampsSec[endArrayIndex],
            alignedStartSec: train.alignedTimestampsSec[startArrayIndex],
            alignedEndSec: train.alignedTimestampsSec[endArrayIndex],
            score: primarySource.score,
            priority: primarySource.priority,
            decisionPath: normalizedPath
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

    /// Canonicalize automatic sources by stable source identity while unioning their support.
    ///
    /// Manual projection uses the same canonicalization for its hidden source archive so visible
    /// fragments and their later merged representation cannot accumulate as distinct archive values.
    static func normalizedAutomaticSources(
        _ sources: [ClassicAnchorAutomaticEventSource]
    ) -> [ClassicAnchorAutomaticEventSource] {
        let ordered = sources
            .filter { !$0.supportISIIndices.isEmpty }
            .sorted(by: automaticSourceIdentityIsOrderedBefore)
        var normalized: [ClassicAnchorAutomaticEventSource] = []
        for source in ordered {
            if let index = normalized.firstIndex(where: { automaticSourceIdentityMatches($0, source) }) {
                let existing = normalized[index]
                normalized[index] = ClassicAnchorAutomaticEventSource(
                    annotationID: existing.annotationID,
                    candidateID: existing.candidateID,
                    trainID: existing.trainID,
                    supportISIIndices: Array(
                        Set(existing.supportISIIndices).union(source.supportISIIndices)
                    ).sorted(),
                    semanticTrack: existing.semanticTrack,
                    eventTrackClass: existing.eventTrackClass,
                    auditRecommendedSubtype: existing.auditRecommendedSubtype,
                    auditReviewStatus: existing.auditReviewStatus,
                    label: existing.label,
                    lockLevel: existing.lockLevel,
                    stateTonicSubtype: existing.stateTonicSubtype,
                    score: existing.score,
                    priority: existing.priority,
                    decisionPath: existing.decisionPath
                )
            } else {
                normalized.append(source)
            }
        }
        return normalized.sorted(by: automaticSourceIdentityIsOrderedBefore)
    }

    private static func automaticSourceIdentityMatches(
        _ lhs: ClassicAnchorAutomaticEventSource,
        _ rhs: ClassicAnchorAutomaticEventSource
    ) -> Bool {
        lhs.annotationID == rhs.annotationID
            && lhs.candidateID == rhs.candidateID
            && lhs.trainID == rhs.trainID
            && lhs.semanticTrack == rhs.semanticTrack
            && lhs.eventTrackClass == rhs.eventTrackClass
            && lhs.auditRecommendedSubtype == rhs.auditRecommendedSubtype
            && lhs.auditReviewStatus == rhs.auditReviewStatus
            && lhs.label == rhs.label
            && lhs.lockLevel == rhs.lockLevel
            && lhs.stateTonicSubtype == rhs.stateTonicSubtype
            && lhs.score == rhs.score
            && lhs.priority == rhs.priority
            && lhs.decisionPath == rhs.decisionPath
    }

    private static func automaticSourceIsOrderedBefore(
        _ lhs: ClassicAnchorAutomaticEventSource,
        _ rhs: ClassicAnchorAutomaticEventSource
    ) -> Bool {
        if lhs.priority != rhs.priority { return lhs.priority > rhs.priority }
        if lhs.score != rhs.score { return lhs.score > rhs.score }
        return automaticSourceIdentityIsOrderedBefore(lhs, rhs)
    }

    private static func automaticSourceIdentityIsOrderedBefore(
        _ lhs: ClassicAnchorAutomaticEventSource,
        _ rhs: ClassicAnchorAutomaticEventSource
    ) -> Bool {
        if lhs.candidateID != rhs.candidateID { return lhs.candidateID < rhs.candidateID }
        if lhs.supportISIIndices.first != rhs.supportISIIndices.first {
            return (lhs.supportISIIndices.first ?? Int.max) < (rhs.supportISIIndices.first ?? Int.max)
        }
        if lhs.supportISIIndices.last != rhs.supportISIIndices.last {
            return (lhs.supportISIIndices.last ?? Int.max) < (rhs.supportISIIndices.last ?? Int.max)
        }
        return lhs.annotationID < rhs.annotationID
    }

    private static func normalizedTonicSubtype(
        _ subtype: String?,
        for label: ClassicAnchorLabel
    ) -> String? {
        switch label {
        case .tonic, .highFrequencyTonic:
            return subtype
        default:
            return nil
        }
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
