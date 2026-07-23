import CryptoKit
import Foundation
import STPDCore
import Testing

private func identityFixtureDataset(
    source: String = "unit-test",
    firstTrainOffset: Double = 0,
    taskEvents: [TaskEvent] = []
) -> SpikeDataset {
    SpikeDataset(
        name: "identity-fixture",
        sourceDescription: source,
        trains: [
            SpikeTrain(
                name: "fast_train",
                timestampsSec: [0, 0.100 + firstTrainOffset, 0.106 + firstTrainOffset, 0.112 + firstTrainOffset, 0.200]
            ),
            SpikeTrain(
                name: "tonic_train",
                timestampsSec: [0, 0.300, 0.600, 0.900, 1.200, 1.500]
            ),
        ],
        taskEvents: taskEvents
    )
}

private func defaultSettingsSnapshot(
    bandSettings: TrainAdaptiveBandSettings = .init(),
    qualitySettings: SpikeQualitySettings = .init(),
    refractoryAction: ClassicAnchorRefractoryAction = .warnOnly,
    profile: ManualThresholdProfile = .automatic,
    stateTuning: StatePatternDetectorTuning = .init(),
    detectorParameters: PatternDetectionParameterSettings = .defaults,
    frameworkPolicy: HybridPatternDetectionPolicy = .legacyCompatible,
    useAdaptiveV2Canonicalization: Bool = false,
    manualThresholdScope: ManualThresholdScope = .allTrains
) -> DetectionRunSettingsSnapshot {
    DetectionRunSettingsSnapshot.make(
        bandSettings: bandSettings,
        qualitySettings: qualitySettings,
        refractoryAction: refractoryAction,
        stateTuning: stateTuning,
        detectorParameters: detectorParameters,
        manualThresholdProfile: profile,
        frameworkPolicy: frameworkPolicy,
        useAdaptiveV2Canonicalization: useAdaptiveV2Canonicalization,
        manualThresholdScope: manualThresholdScope
    )
}

private func publicEventSnapshot(
    run: ClassicAnchorDetectionRun,
    dataset: SpikeDataset
) -> [String] {
    run.eventAnnotations(in: dataset)
        .map {
            return [
                $0.trainID,
                $0.label.rawValue,
                String($0.startISISecIndex),
                String($0.endISISecIndex),
                $0.semanticTrack.rawValue,
            ].joined(separator: "|")
        }
        .sorted()
}

private func candidateBehaviorSnapshot(run: ClassicAnchorDetectionRun) -> [String] {
    run.results
        .flatMap(\.candidates)
        .map { candidate in
            // The dataset profile is audit-only and its current ID embeds the
            // execution-local SpikeDataset UUID. Normalize only that ID here;
            // all scientific train/event candidate IDs remain pinned verbatim.
            let snapshotCandidateID = candidate.trainID == "__dataset__"
                && candidate.candidateLayer == "structural_dataset_seed_profile"
                ? "__dataset__-structural-dataset-seed-profile"
                : candidate.id
            return [
                candidate.trainID,
                snapshotCandidateID,
                candidate.candidateLayer,
                candidate.candidateClass,
                candidate.finalLabel.rawValue,
                candidate.gateStatus,
                candidate.action,
                String(candidate.priority),
                candidate.selectedForAuto ? "selected" : "not_selected",
                candidate.selectionStatus,
                String(candidate.startISIIndex),
                String(candidate.endISIIndex),
                String(
                    format: "%.17g",
                    locale: Locale(identifier: "en_US_POSIX"),
                    candidate.score
                ),
                candidate.decisionPath,
            ].joined(separator: "|")
        }
        .sorted()
}

private func perISILabelSnapshot(
    run: ClassicAnchorDetectionRun,
    dataset: SpikeDataset
) -> [String] {
    ReviewedISIExportBuilder.build(
        dataset: dataset,
        autoAnnotations: run.eventAnnotations(in: dataset),
        projectionsByTrain: [:]
    )
    .filter { $0.isiIndex > 0 }
    .map {
        [
            $0.trainID,
            String($0.isiIndex),
            $0.autoPattern,
            $0.autoSubtype,
            $0.autoCandidateID,
            $0.finalPattern,
            $0.finalSubtype,
            $0.finalSource,
        ].joined(separator: "|")
    }
}

private func snapshotDigest(_ rows: [String]) -> String {
    SHA256.hash(data: Data(rows.joined(separator: "\n").utf8))
        .map { String(format: "%02x", $0) }
        .joined()
}

@Test
func resultSchemaContractPinsNormalizedTablesAndIdentityColumns() {
    #expect(STPDResultSchema.version == "stpd_result_package_v1")
    #expect(STPDResultSchema.manifestFileName == "manifest.json")
    #expect(STPDResultSchema.tables.map(\.table) == [
        .runMetadata,
        .parametersReport,
        .resolvedParameters,
        .candidateLedger,
        .candidateFeatures,
        .finalDecisions,
        .eventsFinal,
        .isiLabelsFinal,
        .candidateDiagnosticAudit,
        .resultConsistencyCheck,
        .manualAnnotations,
        .reviewStatus,
        .hfsBurstArbitrationAudit,
    ])
    #expect(STPDResultSchema.tables.map(\.grain) == [
        "one row per detector run",
        "one row per requested run-level detector parameter",
        "one row per effective detector parameter per resolution scope",
        "one row per detector candidate",
        "one row per detector candidate",
        "one row per candidate decision",
        "one row per final public event",
        "one row per train ISI",
        "one row per candidate diagnostic stage",
        "one row per consistency assertion",
        "one row per manual annotation",
        "one row per reviewed candidate",
        "one row per HFS/burst arbitration decision",
    ])
    #expect(STPDResultSchema.tables.map(\.primaryKey) == [
        ["run_id"],
        ["run_id", "parameter_key"],
        ["run_id", "scope_type", "scope_id", "parameter_key"],
        ["run_id", "candidate_uid"],
        ["run_id", "candidate_uid"],
        ["run_id", "candidate_uid"],
        ["run_id", "event_uid"],
        ["run_id", "train_id", "isi_index"],
        ["run_id", "candidate_uid", "stage_id"],
        ["run_id", "check_id"],
        ["run_id", "annotation_id"],
        ["run_id", "candidate_uid"],
        ["run_id", "audit_row_id"],
    ])
    #expect(STPDResultTable.candidateFeatures.rawValue == "Candidate_features_audit.csv")
    #expect(STPDResultTable.parametersReport.rawValue == "Parameters_report.csv")
    #expect(STPDResultTable.resolvedParameters.rawValue == "Resolved_parameters.csv")
    #expect(STPDResultSchema.tables.map(\.table) == STPDResultTable.allCases)
    #expect(Set(STPDResultSchema.tables.map { $0.table.rawValue }).count == STPDResultTable.allCases.count)
    #expect(STPDResultSchema.tables.allSatisfy {
        $0.requiredIdentityColumns == ["run_id", "settings_digest"]
    })
    #expect(STPDResultSchema.tables.allSatisfy { $0.primaryKey.first == "run_id" })
}

@Test
func datasetDigestIsStableAcrossReloadMetadataAndSensitiveToScientificInput() {
    let original = identityFixtureDataset(source: "/first/location.csv")
    let reloaded = identityFixtureDataset(source: "/different/location.csv")
    let changedTimestamp = identityFixtureDataset(firstTrainOffset: 0.001)
    let taskEvent = TaskEvent(
        id: "stimulus:1",
        name: "Stimulus",
        timeSec: 0.75,
        column: "event_stimulus",
        eventIndex: 1,
        trialID: "trial:1",
        source: "/location/that/is/not/scientific-identity.csv"
    )
    let changedEvents = identityFixtureDataset(taskEvents: [taskEvent])

    let originalSnapshot = DetectionDatasetSnapshot.make(dataset: original)
    let reloadedSnapshot = DetectionDatasetSnapshot.make(dataset: reloaded)
    let changedTimestampSnapshot = DetectionDatasetSnapshot.make(dataset: changedTimestamp)
    let changedEventsSnapshot = DetectionDatasetSnapshot.make(dataset: changedEvents)
    let reversed = SpikeDataset(
        name: original.name,
        sourceDescription: original.sourceDescription,
        trains: Array(original.trains.reversed()),
        taskEvents: original.taskEvents
    )

    #expect(original.id != reloaded.id)
    #expect(originalSnapshot.digest == reloadedSnapshot.digest)
    #expect(originalSnapshot.digest != changedTimestampSnapshot.digest)
    #expect(originalSnapshot.digest != changedEventsSnapshot.digest)
    #expect(originalSnapshot.digest != DetectionDatasetSnapshot.make(dataset: reversed).digest)
    #expect(originalSnapshot.digest.count == 64)
    #expect(originalSnapshot.trainCount == 2)
    #expect(originalSnapshot.spikeCount == 11)
    #expect(originalSnapshot.digest == "cf00605bfdba000ccd4f426220bc92addb089c5766a341e49ad0736dc0e8fd9c")
}

@Test
func settingsDigestIsDeterministicAndSensitiveToEachAuthorityClass() {
    let defaultsA = defaultSettingsSnapshot()
    let defaultsB = defaultSettingsSnapshot()

    let provenanceA = ManualThresholdProfile(
        learnedProvenanceByKey: ["tonic.isi_upper": "second", "burst.seed_upper": "first"]
    )
    let provenanceB = ManualThresholdProfile(
        learnedProvenanceByKey: ["burst.seed_upper": "first", "tonic.isi_upper": "second"]
    )
    let changedTonic = defaultSettingsSnapshot(
        stateTuning: StatePatternDetectorTuning(tonicCVMax: 0.25)
    )
    let scopedA = ManualThresholdScope(
        kind: .selectedTrains,
        trainIDs: ["train-b", "train-a", "train-b"]
    )
    let scopedB = ManualThresholdScope(
        kind: .selectedTrains,
        trainIDs: ["train-a", "train-b"]
    )

    #expect(defaultsA.digest == defaultsB.digest)
    #expect(defaultsA.entries == defaultsB.entries)
    #expect(defaultSettingsSnapshot(profile: provenanceA).digest == defaultSettingsSnapshot(profile: provenanceB).digest)
    #expect(defaultSettingsSnapshot(manualThresholdScope: scopedA).digest
        == defaultSettingsSnapshot(manualThresholdScope: scopedB).digest)

    let authorityChanges = [
        defaultSettingsSnapshot(bandSettings: .init(minValidISISec: 0.002)),
        defaultSettingsSnapshot(qualitySettings: .init(artifactThresholdSec: 0.0008)),
        defaultSettingsSnapshot(refractoryAction: .excludeCandidate),
        changedTonic,
        defaultSettingsSnapshot(
            detectorParameters: .init(classicBurstContrastMin: 4.0)
        ),
        defaultSettingsSnapshot(
            profile: ManualThresholdProfile(
                tonic: TonicManualThresholds(
                    minSpikes: .init(mode: .hardGate, value: 3)
                )
            )
        ),
        defaultSettingsSnapshot(frameworkPolicy: .structureFirstAdaptiveISI),
        defaultSettingsSnapshot(useAdaptiveV2Canonicalization: true),
        defaultSettingsSnapshot(manualThresholdScope: scopedA),
    ]
    #expect(authorityChanges.allSatisfy { $0.digest != defaultsA.digest })
    #expect(Set(authorityChanges.map(\.digest)).count == authorityChanges.count)
    #expect(Set(defaultsA.entries.map(\.key)).count == defaultsA.entries.count)
    #expect(defaultsA.digest.count == 64)
    #expect(defaultsA.digest == "1b5ffad395c3c1ca823ffd493c11cbbb02de6d0ef3484f3acca5f552b8f58318")
}

@Test
func pipelineAttachesAuthoritativeIdentityWithoutChangingScientificSnapshot() {
    let dataset = identityFixtureDataset()
    let first = ClassicAnchorDetectionPipeline.run(dataset: dataset)
    let second = ClassicAnchorDetectionPipeline.run(dataset: dataset)
    let buildIdentified = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        buildCommit: "0123456789abcdef0123456789abcdef01234567"
    )

    #expect(first.runIdentity.isAuthoritative)
    #expect(second.runIdentity.isAuthoritative)
    #expect(first.runIdentity.runID != second.runIdentity.runID)
    #expect(first.runIdentity.datasetDigest == second.runIdentity.datasetDigest)
    #expect(first.runIdentity.settingsDigest == second.runIdentity.settingsDigest)
    #expect(first.runIdentity.trainCount == dataset.trains.count)
    #expect(first.runIdentity.spikeCount == dataset.totalSpikeCount)
    #expect(first.runIdentity.taskEventCount == dataset.taskEvents.count)
    #expect(first.runIdentity.settingsSnapshot?.digest == first.runIdentity.settingsDigest)
    #expect(!first.runIdentity.hasBuildIdentifier)
    #expect(!first.runIdentity.hasCompleteDeclaredRunIdentity)
    #expect(buildIdentified.runIdentity.hasBuildIdentifier)
    #expect(buildIdentified.runIdentity.hasCompleteDeclaredRunIdentity)
    #expect(buildIdentified.runIdentity.settingsDigest == first.runIdentity.settingsDigest)
    #expect(publicEventSnapshot(run: first, dataset: dataset) == publicEventSnapshot(run: second, dataset: dataset))
    #expect(publicEventSnapshot(run: first, dataset: dataset)
        == publicEventSnapshot(run: buildIdentified, dataset: dataset))
    #expect(candidateBehaviorSnapshot(run: first) == candidateBehaviorSnapshot(run: second))
    #expect(candidateBehaviorSnapshot(run: first) == candidateBehaviorSnapshot(run: buildIdentified))
    #expect(perISILabelSnapshot(run: first, dataset: dataset)
        == perISILabelSnapshot(run: second, dataset: dataset))
    #expect(perISILabelSnapshot(run: first, dataset: dataset)
        == perISILabelSnapshot(run: buildIdentified, dataset: dataset))

    // Golden public-event contract for this fixture. Identity metadata must not alter detector math.
    #expect(publicEventSnapshot(run: first, dataset: dataset) == [
        "fast_train|burst|2|3|event",
    ])
    #expect(candidateBehaviorSnapshot(run: first).count == 12)
    #expect(candidateBehaviorSnapshot(run: first).contains {
        $0.hasPrefix("__dataset__|__dataset__-structural-dataset-seed-profile|")
    })
    #expect(snapshotDigest(candidateBehaviorSnapshot(run: first))
        == "65ca42dd396ee4cf448e6e2bdc72e1c02d35a1a1d261232433c7aaaef4eb11eb")
    #expect(perISILabelSnapshot(run: first, dataset: dataset).count == 9)
    #expect(snapshotDigest(perISILabelSnapshot(run: first, dataset: dataset))
        == "7d3f303548edce0c76c14fc697d14a1acacffec4d0125849469c1931d5a2638b")
}

@Test
func hybridTaggingPreservesPipelineIdentity() {
    let dataset = identityFixtureDataset()
    let run = HybridPatternDetectionFramework.run(
        dataset: dataset,
        buildCommit: "0123456789abcdef0123456789abcdef01234567"
    )

    #expect(run.runIdentity.isAuthoritative)
    #expect(run.runIdentity.hasCompleteDeclaredRunIdentity)
    #expect(run.runIdentity.datasetDigest == DetectionDatasetSnapshot.make(dataset: dataset).digest)
    #expect(run.runIdentity.settingsDigest == DetectionRunSettingsSnapshot.make(
        bandSettings: .init(),
        qualitySettings: .init(),
        refractoryAction: .warnOnly,
        stateTuning: .init(),
        detectorParameters: .defaults,
        manualThresholdProfile: .automatic,
        frameworkPolicy: .structureFirstAdaptiveISI,
        useAdaptiveV2Canonicalization: false,
        manualThresholdScope: .allTrains
    ).digest)
}

@Test
func handBuiltLegacyRunIsExplicitlyNonAuthoritative() {
    let run = ClassicAnchorDetectionRun(
        bandSettings: .init(),
        qualitySettings: .init(),
        resolutions: [],
        results: []
    )

    #expect(run.runIdentity == .legacyUnidentified)
    #expect(!run.runIdentity.isAuthoritative)
}

@Test
func legacyCandidateCSVHeaderRemainsFrozenDuringIdentityFoundationSlice() {
    let dataset = SpikeDataset(name: "empty", sourceDescription: "unit-test", trains: [])
    let run = ClassicAnchorDetectionRun(
        bandSettings: .init(),
        qualitySettings: .init(),
        resolutions: [],
        results: []
    )
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: [],
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: Date(timeIntervalSince1970: 0)
    )
    let header = String(csv.split(separator: "\n", omittingEmptySubsequences: false).first ?? "")
    let columns = header.split(separator: ",", omittingEmptySubsequences: false).map(String.init)

    #expect(columns.count == 240)
    #expect(columns.prefix(7) == [
        "exported_at",
        "dataset_name",
        "source",
        "candidate_id",
        "train_id",
        "train_name",
        "review_status",
    ])
    #expect(columns.suffix(3) == [
        "near_miss_reason",
        "near_miss_details",
        "resolved_thresholds",
    ])
}
