import Foundation
@testable import STPDCore
@testable import SpikeTrainPatternDetectorMac
import Testing

@Suite("Manual pattern learning workflow")
@MainActor
struct ManualPatternLearningWorkflowTests {
    @Test("Artifact boundary conversion is exact and never silently rounds")
    func artifactBoundaryConversionIsExact() throws {
        #expect(try RasterDocument.exactArtifactThresholdMicroseconds(milliseconds: 0.9) == 900)
        #expect(try RasterDocument.exactArtifactThresholdMicroseconds(milliseconds: 1.234) == 1_234)
        #expect(throws: ManualPatternLearningBuildError.artifactThresholdNotRepresentableInMicroseconds) {
            try RasterDocument.exactArtifactThresholdMicroseconds(milliseconds: 0.9005)
        }
        #expect(throws: ManualPatternLearningBuildError.invalidArtifactThreshold) {
            try RasterDocument.exactArtifactThresholdMicroseconds(milliseconds: -.infinity)
        }
    }

    @Test("Applying a proposal is explicit, provenance-bearing, reversible, and does not run the detector")
    func applyAndRollbackPreserveAuthorityBoundary() throws {
        let document = RasterDocument()
        let proposal = try makeBurstProposal()
        document.manualPatternLearningProposal = proposal

        #expect(document.classicAnchorDetectionRun == nil)
        #expect(document.manualBurstMode == .automatic)

        document.applyLearnedThresholds()

        #expect(document.manualBurstMode == .softAnchor)
        #expect(document.manualBurstSeedMaxISIMs > 0)
        #expect(document.manualBurstBridgeMaxISIMs > 0)
        #expect(document.lastLearnedApplyResult?.appliedFamilies == ["burst"])
        #expect(document.canUndoLastLearnedThresholdApplication)
        #expect(document.classicAnchorDetectionRun == nil)
        #expect(!document.activeLearnedThresholdProvenanceByKey.isEmpty)
        #expect(document.activeLearnedThresholdProvenanceByKey.values.allSatisfy {
            $0.contains("source_identity=\(proposal.sourceIdentityDigest)")
                && $0.contains("proposal_digest=\(proposal.proposalComputationDigest)")
        })
        #expect(
            document.manualThresholdProfile.learnedProvenanceByKey
                == document.activeLearnedThresholdProvenanceByKey
        )

        document.undoLastLearnedThresholdApplication()

        #expect(document.manualBurstMode == .automatic)
        #expect(document.manualBurstSeedMaxISIMs == 0)
        #expect(document.manualBurstBridgeMaxISIMs == 0)
        #expect(document.activeLearnedThresholdProvenanceByKey.isEmpty)
        #expect(document.manualLearnedThresholdRollback == nil)
        #expect(document.classicAnchorDetectionRun == nil)
    }

    @Test("Confirmed HFS learning applies typed lower gates with provenance and rollback")
    func hfsApplyAndRollbackPreserveAuthorityBoundary() throws {
        let document = RasterDocument()
        let proposal = try makeHFSProposal()
        document.manualPatternLearningProposal = proposal

        #expect(proposal.applicableFamilies == [.highFrequencySpiking])
        #expect(document.manualHFSMode == .automatic)
        #expect(document.classicAnchorDetectionRun == nil)

        document.applyLearnedThresholds(selecting: [.highFrequencySpiking])

        #expect(document.manualHFSMode == .hardGate)
        #expect(document.manualHFSMinSpikes == 7)
        #expect(document.manualHFSMinDurationMs == 55.5)
        #expect(document.lastLearnedApplyResult?.appliedFamilies == ["hfs"])
        #expect(document.activeLearnedThresholdProvenanceByKey.keys.sorted()
                == ["hfs.min_duration_sec", "hfs.min_spikes"])
        #expect(document.activeLearnedThresholdProvenanceByKey.values.allSatisfy {
            $0.contains("source_identity=\(proposal.sourceIdentityDigest)")
                && $0.contains("proposal_digest=\(proposal.proposalComputationDigest)")
        })
        #expect(document.canUndoLastLearnedThresholdApplication)
        #expect(document.classicAnchorDetectionRun == nil)

        document.undoLastLearnedThresholdApplication()

        #expect(document.manualHFSMode == .automatic)
        #expect(document.manualHFSMinSpikes == 0)
        #expect(document.manualHFSMinDurationMs == 0)
        #expect(document.activeLearnedThresholdProvenanceByKey.isEmpty)
        #expect(document.classicAnchorDetectionRun == nil)
    }

    @Test("Applying a later partial proposal preserves provenance for untouched families")
    func partialApplicationsPreserveLayeredProvenance() throws {
        let document = RasterDocument()
        let burst = try makeBurstProposal()
        document.manualPatternLearningProposal = burst
        document.applyLearnedThresholds()
        let burstNote = try #require(
            document.activeLearnedThresholdProvenanceByKey["burst.seed_upper_sec"]
        )

        let tonic = try makeTonicProposal()
        document.manualPatternLearningProposal = tonic
        document.applyLearnedThresholds()
        let notes = document.activeLearnedThresholdProvenanceByKey

        #expect(notes["burst.seed_upper_sec"] == burstNote)
        #expect(notes["burst.seed_upper_sec"]?.contains(burst.sourceIdentityDigest) == true)
        #expect(notes["tonic.isi_lower_sec"]?.contains(tonic.sourceIdentityDigest) == true)
        #expect(document.manualBurstMode == .softAnchor)
        #expect(document.manualTonicMode == .softAnchor)
        #expect(document.lastLearnedApplyResult?.appliedFamilies == ["tonic"])
        #expect(document.classicAnchorDetectionRun == nil)
    }

    @Test("Explicit family selection applies only the chosen compatible family")
    func selectiveFamilyApplicationIsNarrowAndAuditable() throws {
        let document = RasterDocument()
        let proposal = try makeBurstAndTonicProposal()
        document.manualPatternLearningProposal = proposal

        document.applyLearnedThresholds(selecting: [.tonic])

        #expect(document.manualBurstMode == .automatic)
        #expect(document.manualBurstSeedMaxISIMs == 0)
        #expect(document.manualBurstBridgeMaxISIMs == 0)
        #expect(document.manualTonicMode == .softAnchor)
        #expect(document.manualTonicMinISIMs > 0)
        #expect(document.manualTonicMaxISIMs > document.manualTonicMinISIMs)
        #expect(document.lastLearnedApplyResult?.appliedFamilies == ["tonic"])
        #expect(document.lastLearnedApplyResult?.noValueFamilies.isEmpty == true)
        #expect(document.activeLearnedThresholdProvenanceByKey.keys.allSatisfy {
            $0.hasPrefix("tonic.")
        })
        #expect(document.classicAnchorDetectionRun == nil)
    }

    @Test("Holdout admission applies only admitted calibration fields, never the full-data preview")
    func holdoutAdmissionUsesTheCalibrationProposal() throws {
        let document = RasterDocument()
        let calibrationProposal = try makeBurstProposal()
        document.manualPatternLearningProposal = try makeTonicProposal()
        document.manualLearningHoldoutReport = ManualLearningHoldoutValidationReport(
            schemaContractID: ManualLearningHoldoutValidator.schemaContractID,
            schemaContractDigest: ManualLearningHoldoutValidator.schemaContractDigest,
            admissionContractDigest: ManualLearningHoldoutValidator.admissionContractDigest,
            sourceDatasetDigest: "dataset",
            heldOutEvidenceDigest: "held_out_evidence",
            heldOutEvaluationDigest: "held_out_evaluation",
            calibrationTrainIDs: ["calibration_a", "calibration_b"],
            heldOutTrainIDs: ["held_out"],
            excludedTrainIDs: [],
            calibrationProposal: calibrationProposal,
            baselineSettingsDigest: "baseline",
            baselineSettingsEntries: [],
            comparisons: [],
            admissions: [ManualLearningFamilyAdmission(
                family: .burstFamily,
                disposition: .eligibleForExplicitApplication,
                reasons: [.observedImprovementWithoutMeasuredRegression]
            )],
            reportDigest: "report"
        )
        #expect(document.manualLearningHoldoutReportIsStale)
        document.applyHoldoutAdmittedThresholds(selecting: [.burstFamily])
        #expect(document.manualBurstMode == .automatic)

        document.manualLearningHoldoutConfigurationSnapshot =
            document.manualLearningHoldoutValidationConfiguration

        document.applyHoldoutAdmittedThresholds(selecting: [.burstFamily, .tonic])
        #expect(document.manualBurstMode == .automatic)

        document.applyHoldoutAdmittedThresholds(selecting: [.burstFamily])

        #expect(document.manualBurstMode == .softAnchor)
        #expect(document.manualTonicMode == .automatic)
        #expect(document.lastLearnedApplyResult?.appliedFamilies == ["burst"])
        #expect(document.appliedManualPatternLearningProposal == calibrationProposal)
        #expect(document.classicAnchorDetectionRun == nil)
    }

    @Test("Rollback refuses to overwrite a later manual parameter edit")
    func rollbackDoesNotOverwriteLaterManualEdit() throws {
        let document = RasterDocument()
        document.manualPatternLearningProposal = try makeBurstProposal()
        document.applyLearnedThresholds()
        let learnedSeed = document.manualBurstSeedMaxISIMs

        document.manualBurstSeedMaxISIMs = learnedSeed + 1
        #expect(!document.canUndoLastLearnedThresholdApplication)

        document.undoLastLearnedThresholdApplication()

        #expect(document.manualBurstSeedMaxISIMs == learnedSeed + 1)
        #expect(document.manualBurstMode == .softAnchor)
        #expect(document.manualLearnedThresholdRollback != nil)
    }

    @Test("Changing canonical manual evidence invalidates only the preview")
    func annotationEditInvalidatesPreview() throws {
        let context = try makeContext()
        let document = RasterDocument()
        document.canonicalManualDataset = context.dataset
        document.canonicalManualISILabelDraft = CanonicalManualISILabelDraft(
            canonicalFingerprint: context.fingerprint
        )
        document.manualPatternLearningProposal = try makeBurstProposal()

        let changed = document.applyCanonicalManualISILabel(
            .tonic,
            trainID: context.trainID.semanticID.canonicalText,
            isiIndices: [1, 2]
        )

        #expect(changed)
        #expect(document.manualPatternLearningProposal == nil)
        #expect(document.canonicalManualISILabelDraft?.decisions.count == 2)
        #expect(document.classicAnchorDetectionRun == nil)
    }

    @Test("Changing the exact QC boundary invalidates an unapplied learning preview")
    func qcBoundaryChangeInvalidatesPreview() throws {
        let document = RasterDocument()
        document.manualPatternLearningProposal = try makeBurstProposal()
        document.manualPatternLearningErrorMessage = "old"
        document.isManualPatternLearning = true
        let generation = document.manualPatternLearningGeneration

        document.artifactThresholdMs = 1.0

        #expect(document.manualPatternLearningProposal == nil)
        #expect(document.manualPatternLearningErrorMessage == nil)
        #expect(!document.isManualPatternLearning)
        #expect(document.manualPatternLearningGeneration == generation + 1)
        #expect(document.classicAnchorDetectionRun == nil)
    }

    @Test("Held-out train roles are explicit, canonical, and never accept unknown trains")
    func heldOutTrainRoleSelectionIsExplicit() throws {
        let unitA = ScientificSpikeTrainID(
            try ScientificSemanticID(validating: "unit_a")
        )
        let unitB = ScientificSpikeTrainID(
            try ScientificSemanticID(validating: "unit_b")
        )
        let dataset = CanonicalScientificDataset(
            recordingSegment: ConfirmedRecordingSegment(
                semanticID: ScientificRecordingSegmentID(
                    try ScientificSemanticID(validating: "recording_holdout")
                ),
                regime: .continuousUntrialed,
                importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
                observationBounds: .unknownOrUnavailable
            ),
            activityMode: .putativeSingleUnit,
            spikeTrains: [
                CanonicalSpikeTrain(
                    semanticID: unitB,
                    rawTimestamps: [0, 10_000, 20_000].map(
                        MicrosecondTick.init(microseconds:)
                    )
                ),
                CanonicalSpikeTrain(
                    semanticID: unitA,
                    rawTimestamps: [0, 20_000, 40_000].map(
                        MicrosecondTick.init(microseconds:)
                    )
                ),
            ],
            eventScopeGroups: [CanonicalEventScopeGroup(
                semanticID: ScientificEventScopeGroupID(
                    try ScientificSemanticID(validating: "group_holdout")
                ),
                timeBasis: .recordingElapsed,
                spikeTrainReferences: [unitA, unitB],
                eventDefinitions: []
            )],
            scientificAttributeDefinitions: []
        )
        let document = RasterDocument()
        document.canonicalManualDataset = dataset

        #expect(document.manualLearningAvailableTrainIDs == ["unit_a", "unit_b"])
        #expect(document.manualLearningCalibrationTrainIDs == ["unit_a", "unit_b"])

        document.setManualLearningHeldOut(true, trainID: "unit_b")

        #expect(document.manualLearningHeldOutTrainIDs == ["unit_b"])
        #expect(document.manualLearningCalibrationTrainIDs == ["unit_a"])

        document.setManualLearningHeldOut(true, trainID: "not_in_dataset")
        #expect(document.manualLearningHeldOutTrainIDs == ["unit_b"])

        document.setManualLearningHeldOut(false, trainID: "unit_b")
        #expect(document.manualLearningHeldOutTrainIDs.isEmpty)
        #expect(document.manualLearningCalibrationTrainIDs == ["unit_a", "unit_b"])
    }

    private func makeBurstProposal() throws -> ManualPatternLearningProposal {
        let context = try makeContext()
        let decisions = [1, 2, 4, 5].map { index in
            CanonicalManualISILabelDecision(
                trainID: context.trainID,
                isiIndex: index,
                track: .event,
                label: .burst
            )
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        return ManualPatternLearningProposalBuilder.build(
            from: ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        )
    }

    private func makeTonicProposal() throws -> ManualPatternLearningProposal {
        let context = try makeContext(intervals: [
            40_000, 42_000, 44_000, 46_000, 48_000, 100_000,
            38_000, 40_000, 42_000, 44_000, 46_000,
        ])
        let decisions = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11].map { index in
            CanonicalManualISILabelDecision(
                trainID: context.trainID,
                isiIndex: index,
                track: .state,
                label: .tonic
            )
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        return ManualPatternLearningProposalBuilder.build(
            from: ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        )
    }

    private func makeBurstAndTonicProposal() throws -> ManualPatternLearningProposal {
        let context = try makeContext(intervals: [
            10_000, 11_000, 90_000, 12_000, 13_000, 90_000,
            40_000, 42_000, 44_000, 46_000, 48_000, 100_000,
            38_000, 40_000, 42_000, 44_000, 46_000,
        ])
        let burst = [1, 2, 4, 5].map { index in
            CanonicalManualISILabelDecision(
                trainID: context.trainID,
                isiIndex: index,
                track: .event,
                label: .burst
            )
        }
        let tonic = [7, 8, 9, 10, 11, 13, 14, 15, 16, 17].map { index in
            CanonicalManualISILabelDecision(
                trainID: context.trainID,
                isiIndex: index,
                track: .state,
                label: .tonic
            )
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: burst + tonic
            ),
            minimumValidISIMicroseconds: 900
        )
        return ManualPatternLearningProposalBuilder.build(
            from: ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        )
    }

    private func makeHFSProposal() throws -> ManualPatternLearningProposal {
        let trainA = ScientificSpikeTrainID(
            try ScientificSemanticID(validating: "unit_hfs_a")
        )
        let trainB = ScientificSpikeTrainID(
            try ScientificSemanticID(validating: "unit_hfs_b")
        )
        func timestamps(_ intervals: [Int64]) -> [MicrosecondTick] {
            intervals.reduce(into: [Int64(0)]) { ticks, interval in
                ticks.append(ticks[ticks.count - 1] + interval)
            }.map(MicrosecondTick.init(microseconds:))
        }
        let dataset = CanonicalScientificDataset(
            recordingSegment: ConfirmedRecordingSegment(
                semanticID: ScientificRecordingSegmentID(
                    try ScientificSemanticID(validating: "recording_hfs")
                ),
                regime: .continuousUntrialed,
                importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
                observationBounds: .unknownOrUnavailable
            ),
            activityMode: .putativeSingleUnit,
            spikeTrains: [
                CanonicalSpikeTrain(
                    semanticID: trainA,
                    rawTimestamps: timestamps([
                        8_000, 8_000, 8_000, 8_000, 8_000, 8_000,
                        100_000,
                        9_000, 9_000, 9_000, 9_000, 9_000, 9_000, 9_000,
                    ])
                ),
                CanonicalSpikeTrain(
                    semanticID: trainB,
                    rawTimestamps: timestamps([
                        10_000, 10_000, 10_000, 10_000,
                        10_000, 10_000, 10_000, 10_000,
                    ])
                ),
            ],
            eventScopeGroups: [CanonicalEventScopeGroup(
                semanticID: ScientificEventScopeGroupID(
                    try ScientificSemanticID(validating: "group_hfs")
                ),
                timeBasis: .recordingElapsed,
                spikeTrainReferences: [trainA, trainB],
                eventDefinitions: []
            )],
            scientificAttributeDefinitions: []
        )
        let fingerprint = try CanonicalScientificDatasetFingerprinter.fingerprint(dataset)
        let decisions = [
            (trainA, 1...6),
            (trainA, 8...14),
            (trainB, 1...8),
        ].flatMap { trainID, range in
            range.map { index in
                CanonicalManualISILabelDecision(
                    trainID: trainID,
                    isiIndex: index,
                    track: .state,
                    label: .highFrequencySpiking
                )
            }
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: dataset,
            fingerprint: fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        return ManualPatternLearningProposalBuilder.build(
            from: ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        )
    }

    private func makeContext(
        intervals: [Int64] = [10_000, 10_000, 80_000, 12_000, 12_000]
    ) throws -> (
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        trainID: ScientificSpikeTrainID
    ) {
        let trainID = ScientificSpikeTrainID(
            try ScientificSemanticID(validating: "unit_a")
        )
        let dataset = CanonicalScientificDataset(
            recordingSegment: ConfirmedRecordingSegment(
                semanticID: ScientificRecordingSegmentID(
                    try ScientificSemanticID(validating: "recording")
                ),
                regime: .continuousUntrialed,
                importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
                observationBounds: .unknownOrUnavailable
            ),
            activityMode: .putativeSingleUnit,
            spikeTrains: [CanonicalSpikeTrain(
                semanticID: trainID,
                rawTimestamps: intervals.reduce(into: [Int64(0)]) { ticks, interval in
                    ticks.append(ticks[ticks.count - 1] + interval)
                }.map(MicrosecondTick.init(microseconds:))
            )],
            eventScopeGroups: [CanonicalEventScopeGroup(
                semanticID: ScientificEventScopeGroupID(
                    try ScientificSemanticID(validating: "group_a")
                ),
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
