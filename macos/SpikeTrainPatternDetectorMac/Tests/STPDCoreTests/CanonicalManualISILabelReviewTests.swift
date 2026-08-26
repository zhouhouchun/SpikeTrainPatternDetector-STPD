import Foundation
@testable import STPDCore
import Testing

@Suite("Canonical manual ISI review")
struct CanonicalManualISILabelReviewTests {
    @Test("Exact microsecond rows preserve duplicate ticks and dual-track coexistence")
    func exactRowsPreserveTicksAndCoexistence() throws {
        let context = try makeContext(ticks: [0, 10_000, 10_000, 20_000])
        var draft = CanonicalManualISILabelDraft(canonicalFingerprint: context.fingerprint)
        draft = draft.applying(
            label: .highFrequencySpiking,
            trainID: context.trainID,
            isiIndices: [1, 2, 3]
        )
        draft = draft.applying(
            label: .highFrequencyBurst,
            trainID: context.trainID,
            isiIndices: [1, 2]
        )

        let rows = try CanonicalManualISILabelProjector.rows(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: draft
        )
        #expect(rows.map(\.intervalMicroseconds) == [10_000, 0, 10_000])
        #expect(rows[0].stateLabel == .highFrequencySpiking)
        #expect(rows[0].eventLabel == .highFrequencyBurst)
        #expect(rows[2].eventLabel == nil)
    }

    @Test("Complete confirmation rejects every unreviewed canonical ISI")
    func confirmationRequiresCompleteCoverage() throws {
        let context = try makeContext(ticks: [0, 100_000, 200_000])
        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: context.fingerprint
        ).applying(label: .tonic, trainID: context.trainID, isiIndices: [1])

        #expect(throws: CanonicalManualISILabelReviewError.incompleteReview(
            unreviewedISICount: 1
        )) {
            try ConfirmedCanonicalManualISILabels.confirmComplete(
                draft: draft,
                dataset: context.dataset,
                fingerprint: context.fingerprint,
                reviewer: "Reviewer",
                confirmedAt: Date(timeIntervalSince1970: 100)
            )
        }
    }

    @Test("Complete confirmation is deterministic and bound to canonical identity")
    func confirmationIsDeterministicAndIdentityBound() throws {
        let context = try makeContext(ticks: [0, 100_000, 200_000])
        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: context.fingerprint
        ).applying(label: .other, trainID: context.trainID, isiIndices: [1, 2])
        let date = Date(timeIntervalSince1970: 123.5)
        let first = try ConfirmedCanonicalManualISILabels.confirmComplete(
            draft: draft,
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            reviewer: " Reviewer ",
            confirmedAt: date
        )
        let second = try ConfirmedCanonicalManualISILabels.confirmComplete(
            draft: draft,
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            reviewer: "Reviewer",
            confirmedAt: date
        )
        #expect(first.reviewer == "Reviewer")
        #expect(first.decisionDigest == second.decisionDigest)
        #expect(first.decisionDigest.count == 64)

        let other = try makeContext(ticks: [0, 100_001, 200_000])
        #expect(throws: CanonicalManualISILabelReviewError.canonicalFingerprintMismatch) {
            try CanonicalManualISILabelProjector.rows(
                dataset: other.dataset,
                fingerprint: other.fingerprint,
                draft: draft
            )
        }
    }

    @Test("Other removes biological tracks and later state removes Other")
    func otherIsExclusive() throws {
        let context = try makeContext(ticks: [0, 100_000])
        var draft = CanonicalManualISILabelDraft(canonicalFingerprint: context.fingerprint)
        draft = draft.applying(label: .tonic, trainID: context.trainID, isiIndices: [1])
        draft = draft.applying(label: .pause, trainID: context.trainID, isiIndices: [1])
        draft = draft.applying(label: .other, trainID: context.trainID, isiIndices: [1])
        var row = try #require(CanonicalManualISILabelProjector.rows(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: draft
        ).first)
        #expect(row.otherLabel == .other)
        #expect(row.stateLabel == nil)
        #expect(row.eventLabel == nil)

        draft = draft.applying(label: .tonic, trainID: context.trainID, isiIndices: [1])
        row = try #require(CanonicalManualISILabelProjector.rows(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: draft
        ).first)
        #expect(row.otherLabel == nil)
        #expect(row.stateLabel == .tonic)
    }

    private func makeContext(
        ticks: [Int64]
    ) throws -> (
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        trainID: ScientificSpikeTrainID
    ) {
        let trainID = ScientificSpikeTrainID(try ScientificSemanticID(validating: "unit_a"))
        let groupID = ScientificEventScopeGroupID(try ScientificSemanticID(validating: "group_a"))
        let segmentID = ScientificRecordingSegmentID(try ScientificSemanticID(validating: "recording_a"))
        let dataset = CanonicalScientificDataset(
            recordingSegment: ConfirmedRecordingSegment(
                semanticID: segmentID,
                regime: .continuousUntrialed,
                importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
                observationBounds: .unknownOrUnavailable
            ),
            activityMode: .putativeSingleUnit,
            spikeTrains: [CanonicalSpikeTrain(
                semanticID: trainID,
                rawTimestamps: ticks.map(MicrosecondTick.init(microseconds:))
            )],
            eventScopeGroups: [CanonicalEventScopeGroup(
                semanticID: groupID,
                timeBasis: .recordingElapsed,
                spikeTrainReferences: [trainID],
                eventDefinitions: []
            )],
            scientificAttributeDefinitions: []
        )
        return (
            dataset,
            try CanonicalScientificDatasetFingerprinter.fingerprint(dataset),
            trainID
        )
    }
}
