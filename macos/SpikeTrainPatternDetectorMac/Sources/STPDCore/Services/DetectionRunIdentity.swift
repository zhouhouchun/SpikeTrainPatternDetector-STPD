import CryptoKit
import Foundation

/// Stable, public names for the normalized scientific result package.
///
/// This file defines the contract only. Table materialization and package writing are separate
/// phases; keeping the contract in STPDCore lets tests and future exporters share one authority.
public enum STPDResultTable: String, CaseIterable, Hashable, Sendable {
    case runMetadata = "Detector_run_metadata.csv"
    case parametersReport = "Parameters_report.csv"
    case resolvedParameters = "Resolved_parameters.csv"
    case candidateLedger = "Candidate_ledger.csv"
    case candidateFeatures = "Candidate_features_audit.csv"
    case finalDecisions = "Final_decisions.csv"
    case candidateLedgerDiagnostic = "Candidate_ledger_diagnostic.csv"
    case candidateFeaturesDiagnostic = "Candidate_features_diagnostic_audit.csv"
    case finalDecisionsDiagnostic = "Final_decisions_diagnostic.csv"
    case eventsFinal = "Events_final.csv"
    case isiLabelsFinal = "ISI_labels_final.csv"
    case candidateDiagnosticAudit = "Candidate_diagnostic_audit.csv"
    case resultConsistencyCheck = "Result_consistency_check.csv"
    case manualAnnotations = "Manual_annotations.csv"
    case manualAnnotationImportApprovals = "Manual_annotation_import_approvals.csv"
    case reviewStatus = "Review_status.csv"
    case hfsBurstArbitrationAudit = "HFS_burst_arbitration_audit.csv"
    case taskEvents = "Task_events.csv"
    case dataQualityQC = "Data_quality_QC.csv"
}

public struct STPDResultTableContract: Hashable, Sendable {
    public let table: STPDResultTable
    public let grain: String
    public let primaryKey: [String]
    public let requiredIdentityColumns: [String]

    public init(
        table: STPDResultTable,
        grain: String,
        primaryKey: [String],
        requiredIdentityColumns: [String] = ["run_id", "settings_digest"]
    ) {
        self.table = table
        self.grain = grain
        self.primaryKey = primaryKey
        self.requiredIdentityColumns = requiredIdentityColumns
    }
}

public enum STPDResultSchema {
    public static let version = "stpd_result_package_v4"
    /// The immediately-preceding schema version. Retained so the schema layer can express the v2 table
    /// and v3 layouts for schema-version-aware expected-table-set validation. This is table-set
    /// recognition only, not an on-disk legacy package reader.
    public static let previousVersion = "stpd_result_package_v3"
    public static let legacyVersion = "stpd_result_package_v2"
    public static let detectorVersion = "stpd_mac_structure_first_v1"
    public static let manifestFileName = "manifest.json"

    /// The exact set of table file names expected for a given result-package schema version.
    ///
    /// v2 expects the original 17 tables (no `Data_quality_QC.csv`); v3 expects 18 tables including
    /// `Data_quality_QC.csv`; v4 adds the normalized manual-import approval ledger. An unknown version
    /// returns `nil` so callers fail closed.
    ///
    /// This helper does not implement an on-disk package reader or end-to-end backward-compatible
    /// package validation. The current full v4 build/validation path unconditionally requires both
    /// `Data_quality_QC.csv` and `Manual_annotation_import_approvals.csv`.
    public static func requiredTableFileNames(forSchemaVersion version: String) -> Set<String>? {
        switch version {
        case legacyVersion:
            return Set(STPDResultTable.allCases
                .filter {
                    $0 != .dataQualityQC &&
                        $0 != .manualAnnotationImportApprovals
                }
                .map(\.rawValue))
        case previousVersion:
            return Set(STPDResultTable.allCases
                .filter { $0 != .manualAnnotationImportApprovals }
                .map(\.rawValue))
        case Self.version:
            return Set(STPDResultTable.allCases.map(\.rawValue))
        default:
            return nil
        }
    }

    /// Name, grain, and key contracts for the normalized result package. Before a writer is added,
    /// the materialization phase must also define deterministic UID derivation, foreign-key targets,
    /// and referential-integrity checks; this declaration does not silently invent those policies.
    public static let tables: [STPDResultTableContract] = [
        .init(
            table: .runMetadata,
            grain: "one row per detector run",
            primaryKey: ["run_id"]
        ),
        .init(
            table: .parametersReport,
            grain: "one row per requested run-level detector parameter",
            primaryKey: ["run_id", "parameter_key"]
        ),
        .init(
            table: .resolvedParameters,
            grain: "one row per effective detector parameter per resolution scope",
            primaryKey: ["run_id", "scope_type", "scope_id", "parameter_key"]
        ),
        .init(
            table: .candidateLedger,
            grain: "one row per public authority-bearing detector candidate",
            primaryKey: ["run_id", "candidate_uid"]
        ),
        .init(
            table: .candidateFeatures,
            grain: "one row per public authority-bearing detector candidate",
            primaryKey: ["run_id", "candidate_uid"]
        ),
        .init(
            table: .finalDecisions,
            grain: "one row per public authority-bearing candidate decision",
            primaryKey: ["run_id", "candidate_uid"]
        ),
        .init(
            table: .candidateLedgerDiagnostic,
            grain: "one row per detector candidate, including non-public diagnostic candidates",
            primaryKey: ["run_id", "candidate_uid"]
        ),
        .init(
            table: .candidateFeaturesDiagnostic,
            grain: "one row per detector candidate, including non-public diagnostic candidates",
            primaryKey: ["run_id", "candidate_uid"]
        ),
        .init(
            table: .finalDecisionsDiagnostic,
            grain: "one row per candidate decision, including non-public diagnostic candidates",
            primaryKey: ["run_id", "candidate_uid"]
        ),
        .init(
            table: .eventsFinal,
            grain: "one row per final public event",
            primaryKey: ["run_id", "event_uid"]
        ),
        .init(
            table: .isiLabelsFinal,
            grain: "one row per train ISI",
            primaryKey: ["run_id", "train_id", "isi_index"]
        ),
        .init(
            table: .candidateDiagnosticAudit,
            grain: "one row per candidate diagnostic stage",
            primaryKey: ["run_id", "candidate_uid", "stage_id"]
        ),
        .init(
            table: .resultConsistencyCheck,
            grain: "one row per consistency assertion",
            primaryKey: ["run_id", "check_id"]
        ),
        .init(
            table: .manualAnnotations,
            grain: "one row per manual annotation",
            primaryKey: ["run_id", "annotation_id"]
        ),
        .init(
            table: .manualAnnotationImportApprovals,
            grain: "one row per explicitly approved identity-bound manual annotation import batch",
            primaryKey: ["run_id", "approval_id"]
        ),
        .init(
            table: .reviewStatus,
            grain: "one row per reviewed candidate",
            primaryKey: ["run_id", "candidate_uid"]
        ),
        .init(
            table: .hfsBurstArbitrationAudit,
            grain: "one row per HFS/burst arbitration decision",
            primaryKey: ["run_id", "audit_row_id"]
        ),
        .init(
            table: .taskEvents,
            grain: "one row per normalized task or stimulus event",
            primaryKey: ["run_id", "task_event_uid"]
        ),
        .init(
            table: .dataQualityQC,
            grain: "one row per dataset train",
            primaryKey: ["run_id", "train_id"]
        ),
    ]
}

public struct DetectionSettingEntry: Hashable, Sendable {
    public let key: String
    public let value: String

    public init(key: String, value: String) {
        self.key = key
        self.value = value
    }
}

/// A deterministic snapshot of every requested public input that can alter the primary detector path.
///
/// Entries are sorted by key and hashed with a length-prefixed encoding. This deliberately avoids
/// Swift's randomized `hashValue`, reflection output, dictionary iteration order, and locale-sensitive
/// numeric formatting. The entries later become the source for the R-compatible
/// `Parameters_report.csv`. They are invocation inputs, not per-train effective values; a future
/// materializer must write those separately to `Resolved_parameters.csv`.
public struct DetectionRunSettingsSnapshot: Hashable, Sendable {
    public static let digestContractVersion = "settings_digest_v1"

    public let entries: [DetectionSettingEntry]
    public let digest: String

    private init(entries: [DetectionSettingEntry]) {
        let sorted = entries.sorted {
            if $0.key != $1.key { return $0.key < $1.key }
            return $0.value < $1.value
        }
        precondition(Set(sorted.map(\.key)).count == sorted.count, "Detection setting keys must be unique")
        self.entries = sorted

        var encoder = StableDigestEncoder(domain: Self.digestContractVersion)
        encoder.append(sorted.count)
        for entry in sorted {
            encoder.append(entry.key)
            encoder.append(entry.value)
        }
        self.digest = encoder.finalize()
    }

    public static func make(
        bandSettings: TrainAdaptiveBandSettings,
        qualitySettings: SpikeQualitySettings,
        refractoryAction: ClassicAnchorRefractoryAction,
        stateTuning: StatePatternDetectorTuning,
        detectorParameters: PatternDetectionParameterSettings,
        manualThresholdProfile: ManualThresholdProfile,
        frameworkPolicy: HybridPatternDetectionPolicy,
        useAdaptiveV2Canonicalization: Bool,
        manualThresholdScope: ManualThresholdScope
    ) -> DetectionRunSettingsSnapshot {
        var entries: [DetectionSettingEntry] = []

        func add(_ key: String, _ value: String) {
            entries.append(.init(key: key, value: value))
        }
        func add(_ key: String, _ value: Double) {
            add(key, canonicalDouble(value))
        }
        func add(_ key: String, _ value: Double?) {
            add(key, value.map(canonicalDouble) ?? "null")
        }
        func add(_ key: String, _ value: Int) {
            add(key, String(value))
        }
        func add(_ key: String, _ value: Bool) {
            add(key, value ? "true" : "false")
        }
        func add(_ prefix: String, _ value: ManualISIThreshold) {
            add("\(prefix).mode", value.mode.rawValue)
            add("\(prefix).value_sec", value.valueSec)
        }
        func add(_ prefix: String, _ value: ManualSpikeCountThreshold) {
            add("\(prefix).mode", value.mode.rawValue)
            add("\(prefix).value", value.value.map(String.init) ?? "null")
        }

        add("band.min_valid_isi_sec", bandSettings.minValidISISec)
        add("band.histogram_bin_width_sec", bandSettings.histogramBinWidthSec)
        add("band.dataset_isi_boundary_floor_sec", bandSettings.datasetISIBoundaryFloorSec)

        add("quality.artifact_threshold_sec", qualitySettings.artifactThresholdSec)
        add("quality.refractory_suspect_threshold_sec", qualitySettings.refractorySuspectThresholdSec)
        add("quality.display_unit", qualitySettings.displayUnit.rawValue)
        add("refractory.action", refractoryAction.rawValue)

        add("state.tonic_min_spikes", stateTuning.tonicMinSpikes)
        add("state.tonic_cv_max", stateTuning.tonicCVMax)
        add("state.tonic_cv2_max", stateTuning.tonicCV2Max)
        add("state.tonic_lv_max", stateTuning.tonicLVMax)
        add("state.irregular_tonic_cv_max", stateTuning.irregularTonicCVMax)
        add("state.irregular_tonic_cv2_max", stateTuning.irregularTonicCV2Max)
        add("state.irregular_tonic_lv_max", stateTuning.irregularTonicLVMax)
        add("state.tonic_burst_seed_fraction_max", stateTuning.tonicBurstSeedFractionMax)
        add("state.hf_tonic_min_spikes", stateTuning.highFrequencyTonicMinSpikes)
        add("state.hf_tonic_low_tail_fraction_max", stateTuning.highFrequencyTonicLowTailFractionMax)
        add("state.hf_tonic_cv_max", stateTuning.highFrequencyTonicCVMax)
        add("state.hf_tonic_cv2_max", stateTuning.highFrequencyTonicCV2Max)
        add("state.hf_tonic_lv_max", stateTuning.highFrequencyTonicLVMax)
        add("state.hfs_min_spikes", stateTuning.highFrequencySpikingMinSpikes)
        add("state.hfs_short_fraction_min", stateTuning.highFrequencySpikingShortFractionMin)
        add("state.hfs_allowed_large_fraction", stateTuning.highFrequencySpikingAllowedLargeFraction)
        add("state.hfs_max_consecutive_large_isi", stateTuning.highFrequencySpikingMaxConsecutiveLargeISI)

        add("detector.classic_burst_contrast_min", detectorParameters.classicBurstContrastMin)
        add(
            "detector.classic_burst_flank_pause_contrast_min",
            detectorParameters.classicBurstFlankPauseContrastMin
        )
        add("detector.classic_burst_min_spikes", detectorParameters.classicBurstMinSpikes)
        add("detector.classic_burst_max_spikes", detectorParameters.classicBurstMaxSpikes)
        add("detector.pause_min_isi_sec_override", detectorParameters.pauseMinISISecOverride)

        add("policy.name", frameworkPolicy.name)
        add("policy.classic_burst_contrast_min", frameworkPolicy.classicBurstContrastMin)
        add("policy.classic_burst_geom_contrast_min", frameworkPolicy.classicBurstContrastGeomMin)
        add(
            "policy.classic_burst_flank_pause_contrast_min",
            frameworkPolicy.classicBurstFlankPauseContrastMin
        )
        add("policy.possible_burst_contrast_min", frameworkPolicy.possibleBurstContrastMin)
        add("policy.possible_burst_geom_contrast_min", frameworkPolicy.possibleBurstContrastGeomMin)
        add("policy.one_sided_burst_as_canonical", frameworkPolicy.oneSidedBurstAsCanonical)
        add("policy.use_adaptive_v2_canonicalization", useAdaptiveV2Canonicalization)

        add("manual.scope.kind", manualThresholdScope.kind.rawValue)
        add("manual.scope.train_id_count", manualThresholdScope.trainIDs.count)
        for (index, trainID) in manualThresholdScope.trainIDs.enumerated() {
            add(String(format: "manual.scope.train_id.%06d", index), trainID)
        }

        add("manual.burst.seed_lower_isi", manualThresholdProfile.burst.seedLowerISI)
        add("manual.burst.seed_upper_isi", manualThresholdProfile.burst.seedUpperISI)
        add("manual.burst.bridge_upper_isi", manualThresholdProfile.burst.bridgeUpperISI)
        add("manual.burst.min_spikes", manualThresholdProfile.burst.minSpikes)
        add("manual.burst.classic_max_spikes", manualThresholdProfile.burst.classicMaxSpikes)
        add("manual.burst.long_min_spikes", manualThresholdProfile.burst.longMinSpikes)
        add("manual.burst.long_max_spikes", manualThresholdProfile.burst.longMaxSpikes)
        add("manual.hfs.min_spikes", manualThresholdProfile.hfs.minSpikes)
        add("manual.hfs.min_duration_sec", manualThresholdProfile.hfs.minDurationSec)
        add("manual.hf_tonic.min_spikes", manualThresholdProfile.hfTonic.minSpikes)
        add("manual.hf_tonic.isi_floor", manualThresholdProfile.hfTonic.isiFloor)
        add("manual.hf_tonic.isi_upper", manualThresholdProfile.hfTonic.isiUpper)
        add("manual.tonic.min_spikes", manualThresholdProfile.tonic.minSpikes)
        add("manual.tonic.isi_lower", manualThresholdProfile.tonic.isiLower)
        add("manual.tonic.isi_upper", manualThresholdProfile.tonic.isiUpper)
        add("manual.pause.isi_lower", manualThresholdProfile.pause.isiLower)

        for (key, value) in manualThresholdProfile.learnedProvenanceByKey.sorted(by: { $0.key < $1.key }) {
            add("manual.learned_provenance.\(key)", value)
        }

        return DetectionRunSettingsSnapshot(entries: entries)
    }
}

public struct DetectionDatasetSnapshot: Hashable, Sendable {
    public static let digestContractVersion = "dataset_digest_v2"

    public let digest: String
    public let trainCount: Int
    public let spikeCount: Int
    public let taskEventCount: Int

    /// Produces a parsed-input fingerprint. Train order, stable train identifiers, spike timestamps,
    /// and duplicate-handling metadata are intentionally significant. Task events are canonicalized
    /// by their scientific fields: display/source metadata and input-array order are not scientific
    /// identity, while the source event index remains part of the event itself.
    public static func make(dataset: SpikeDataset) -> DetectionDatasetSnapshot {
        var encoder = StableDigestEncoder(domain: digestContractVersion)
        encoder.append(dataset.trains.count)
        for train in dataset.trains {
            encoder.append(train.id)
            encoder.append(train.name)
            encoder.append(train.duplicateTimestampPolicy.rawValue)
            encoder.append(train.inputWasUnsorted)
            encoder.append(train.droppedDuplicateTimestampCount)
            encoder.append(train.sortedDuplicateTimestampCountBeforePolicy)
            encoder.append(train.inputDuplicateTimestampStepCount)
            encoder.append(train.inputNonmonotonicStepCount)
            encoder.append(train.inputZeroOrNegativeStepCount)
            encoder.append(train.inputOrderIndices.count)
            for index in train.inputOrderIndices {
                encoder.append(index)
            }
            encoder.append(train.timestampsSec.count)
            for timestamp in train.timestampsSec {
                encoder.append(timestamp)
            }
        }

        let orderedTaskEvents = dataset.taskEvents.sorted {
            let lhs = taskEventDigestComponents($0)
            let rhs = taskEventDigestComponents($1)
            return lhs.lexicographicallyPrecedes(rhs)
        }
        encoder.append(orderedTaskEvents.count)
        for event in orderedTaskEvents {
            encoder.append(event.id)
            encoder.append(event.name)
            encoder.append(event.timeSec)
            encoder.append(event.column)
            encoder.append(event.eventIndex)
            encoder.append(event.trialID)
        }

        return DetectionDatasetSnapshot(
            digest: encoder.finalize(),
            trainCount: dataset.trains.count,
            spikeCount: dataset.totalSpikeCount,
            taskEventCount: dataset.taskEvents.count
        )
    }

    private static func taskEventDigestComponents(_ event: TaskEvent) -> [String] {
        [
            event.id,
            event.name,
            STPDCanonicalValue.double(event.timeSec),
            event.column,
            String(event.eventIndex),
            event.trialID,
        ]
    }
}

/// Human-readable dataset metadata captured at detector invocation time.
///
/// The scientific-input digest intentionally remains independent of display/source strings. This
/// separate snapshot binds those strings to the run so an exporter cannot silently relabel an
/// otherwise identical spike matrix after detection.
public struct DetectionDatasetMetadataSnapshot: Hashable, Sendable {
    public static let taskEventSourceDigestContractVersion =
        "task_event_source_digest_v1"

    public let name: String
    public let sourceDescription: String
    /// Separately seals task-event provenance without making file/source strings part of the
    /// scientific dataset identity. This prevents an exporter from rewriting event provenance
    /// after detection while preserving relocation-stable scientific run identity.
    public let taskEventSourceDigest: String

    public init(
        name: String,
        sourceDescription: String,
        taskEventSourceDigest: String
    ) {
        self.name = name
        self.sourceDescription = sourceDescription
        self.taskEventSourceDigest = taskEventSourceDigest
    }

    public static func make(dataset: SpikeDataset) -> DetectionDatasetMetadataSnapshot {
        DetectionDatasetMetadataSnapshot(
            name: dataset.name,
            sourceDescription: dataset.sourceDescription,
            taskEventSourceDigest: makeTaskEventSourceDigest(
                dataset.taskEvents
            )
        )
    }

    private static func makeTaskEventSourceDigest(
        _ events: [TaskEvent]
    ) -> String {
        let ordered = events.sorted {
            taskEventSourceComponents($0)
                .lexicographicallyPrecedes(taskEventSourceComponents($1))
        }
        var encoder = StableDigestEncoder(
            domain: taskEventSourceDigestContractVersion
        )
        encoder.append(ordered.count)
        for event in ordered {
            encoder.append(event.id)
            encoder.append(event.name)
            encoder.append(event.timeSec)
            encoder.append(event.column)
            encoder.append(event.eventIndex)
            encoder.append(event.trialID)
            encoder.append(event.source)
        }
        return encoder.finalize()
    }

    private static func taskEventSourceComponents(
        _ event: TaskEvent
    ) -> [String] {
        [
            event.id,
            event.name,
            STPDCanonicalValue.double(event.timeSec),
            event.column,
            String(event.eventIndex),
            event.trialID,
            event.source,
        ]
    }
}

/// Identity and reproducibility metadata captured at detector invocation time.
///
/// `runID` distinguishes executions; `datasetDigest` and `settingsDigest` determine whether two
/// executions used the same scientific inputs. Legacy hand-built runs remain representable but are
/// explicitly non-authoritative and must not be emitted as a normalized result package.
public struct DetectionRunIdentity: Hashable, Sendable {
    public static let unavailable = "unavailable"

    public let runID: String
    public let datasetDigest: String
    public let settingsDigest: String
    public let resultSchemaVersion: String
    public let detectorVersion: String
    public let buildCommit: String
    public let trainCount: Int
    public let spikeCount: Int
    public let taskEventCount: Int
    /// The requested invocation settings behind `settingsDigest`. This is retained so a result
    /// package can materialize the R-compatible `Parameters_report.csv` without asking mutable
    /// UI/document state to reconstruct it. Per-train effective values belong in
    /// `Resolved_parameters.csv`. Legacy or externally assembled runs may omit this snapshot, in
    /// which case they are non-authoritative.
    public let settingsSnapshot: DetectionRunSettingsSnapshot?

    public init(
        runID: String,
        datasetDigest: String,
        settingsDigest: String,
        resultSchemaVersion: String = STPDResultSchema.version,
        detectorVersion: String = STPDResultSchema.detectorVersion,
        buildCommit: String = DetectionRunIdentity.unavailable,
        trainCount: Int,
        spikeCount: Int,
        taskEventCount: Int,
        settingsSnapshot: DetectionRunSettingsSnapshot? = nil
    ) {
        self.runID = runID
        self.datasetDigest = datasetDigest
        self.settingsDigest = settingsDigest
        self.resultSchemaVersion = resultSchemaVersion
        self.detectorVersion = detectorVersion
        self.buildCommit = buildCommit
        self.trainCount = max(0, trainCount)
        self.spikeCount = max(0, spikeCount)
        self.taskEventCount = max(0, taskEventCount)
        self.settingsSnapshot = settingsSnapshot
    }

    public static func make(
        dataset: SpikeDataset,
        settings: DetectionRunSettingsSnapshot,
        buildCommit: String = unavailable,
        runUUID: UUID = UUID()
    ) -> DetectionRunIdentity {
        let datasetSnapshot = DetectionDatasetSnapshot.make(dataset: dataset)
        return DetectionRunIdentity(
            runID: "run_\(runUUID.uuidString.lowercased())",
            datasetDigest: datasetSnapshot.digest,
            settingsDigest: settings.digest,
            buildCommit: buildCommit,
            trainCount: datasetSnapshot.trainCount,
            spikeCount: datasetSnapshot.spikeCount,
            taskEventCount: datasetSnapshot.taskEventCount,
            settingsSnapshot: settings
        )
    }

    public static let legacyUnidentified = DetectionRunIdentity(
        runID: "legacy_unidentified",
        datasetDigest: unavailable,
        settingsDigest: unavailable,
        resultSchemaVersion: STPDResultSchema.version,
        detectorVersion: STPDResultSchema.detectorVersion,
        buildCommit: unavailable,
        trainCount: 0,
        spikeCount: 0,
        taskEventCount: 0,
        settingsSnapshot: nil
    )

    /// True when the run has a generated execution ID plus self-consistent declared detector inputs.
    /// This does not establish environment or build reproducibility.
    public var isAuthoritative: Bool {
        generatedRunUUID != nil
            && isSHA256Hex(datasetDigest)
            && isSHA256Hex(settingsDigest)
            && resultSchemaVersion == STPDResultSchema.version
            && !detectorVersion.isEmpty
            && detectorVersion != Self.unavailable
            && settingsSnapshot?.digest == settingsDigest
    }

    /// True when the caller supplied a nonempty build identifier. The identifier may be a commit SHA,
    /// CI build ID, or release identifier; this property does not validate a clean source tree,
    /// compiler, dependency graph, SDK, architecture, or build flags.
    public var hasBuildIdentifier: Bool {
        let trimmed = buildCommit.trimmingCharacters(in: .whitespacesAndNewlines)
        return !trimmed.isEmpty && trimmed != Self.unavailable
    }

    /// The declared run inputs are internally complete and a build identifier is present. This is a
    /// package-readiness signal only, never proof that the run is fully reproducible.
    public var hasCompleteDeclaredRunIdentity: Bool {
        isAuthoritative && hasBuildIdentifier
    }

    private var generatedRunUUID: UUID? {
        guard runID.hasPrefix("run_") else { return nil }
        return UUID(uuidString: String(runID.dropFirst("run_".count)))
    }
}

private func isSHA256Hex(_ value: String) -> Bool {
    value.count == 64 && value.utf8.allSatisfy { byte in
        (48...57).contains(byte) || (97...102).contains(byte)
    }
}

private struct StableDigestEncoder {
    private var hasher = SHA256()

    init(domain: String) {
        append(domain)
    }

    mutating func append(_ value: Bool) {
        hasher.update(data: Data([value ? 1 : 0]))
    }

    mutating func append(_ value: Int) {
        var bits = Int64(value).bigEndian
        withUnsafeBytes(of: &bits) { hasher.update(data: Data($0)) }
    }

    mutating func append(_ value: Double) {
        let normalized: UInt64
        if value.isNaN {
            normalized = 0x7ff8_0000_0000_0000
        } else if value == 0 {
            normalized = 0
        } else {
            normalized = value.bitPattern
        }
        var bits = normalized.bigEndian
        withUnsafeBytes(of: &bits) { hasher.update(data: Data($0)) }
    }

    mutating func append(_ value: String) {
        let bytes = Data(value.utf8)
        append(bytes.count)
        hasher.update(data: bytes)
    }

    mutating func finalize() -> String {
        hasher.finalize().map { String(format: "%02x", $0) }.joined()
    }
}

private func canonicalDouble(_ value: Double) -> String {
    if value.isNaN { return "nan" }
    if value == .infinity { return "inf" }
    if value == -.infinity { return "-inf" }
    if value == 0 { return "0" }
    return String(format: "%.17g", locale: Locale(identifier: "en_US_POSIX"), value)
}
