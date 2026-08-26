import Foundation
import STPDCore
import Testing

@Test
func classicAnchorEventCSVExporterIncludesRangesReviewStatusAndEscapesFields() throws {
    let train = SpikeTrain(
        name: "unit, \"alpha\"",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let dataset = SpikeDataset(
        name: "synthetic export",
        sourceDescription: "unit-test, \"source\"",
        trains: [train]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let candidate = try #require(run.candidates.first { $0.selectedForAuto && $0.finalLabel == .burst })
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))

    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(in: dataset),
        reviewStatuses: [candidate.id: "accepted"],
        exportedAt: exportedAt
    )
    let rows = parseCSV(csv)
    let header = try #require(rows.first)
    let row = try #require(rows.dropFirst().first { row in
        let values = Dictionary(uniqueKeysWithValues: zip(header, row))
        return values["label"] == "burst"
    })
    let values = Dictionary(uniqueKeysWithValues: zip(header, row))

    #expect(rows.count >= 2)
    #expect(values["exported_at"] == "2026-06-17T00:00:00Z")
    #expect(values["source"] == "unit-test, \"source\"")
    #expect(values["train_name"] == "unit, \"alpha\"")
    #expect(values["review_status"] == "accepted")
    #expect(values["raw_start_sec"] == "0.1")
    #expect(values["raw_end_sec"] == "0.112")
    #expect(values["aligned_start_sec"] == "0.1")
    #expect(values["aligned_end_sec"] == "0.112")
    #expect(values["start_spike_index"] == "2")
    #expect(values["end_spike_index"] == "4")
    #expect(values["n_spikes"] == "3")
    #expect(values["label"] == "burst")
    #expect(values["recommended_track"] == "event")
    #expect(values["recommended_event_track_class"] == "burst")
    #expect(values["audit_review_status"] == "accepted")
    #expect(values["candidate_diagnostic_class"] == "burst__structure_first_two_sided_classic_burst_i_pass")
    #expect(values["failure_reason"] == "")
    #expect(values["adaptive_valid_isi_count"] == "4")
    #expect(values["structural_seed_source"]?.hasPrefix("structural_candidate_audit") == true)
    #expect((Int(values["structural_burst_anchor_count"] ?? "") ?? 0) >= 1)
    #expect(values["structural_burst_seed_upper_sec"] != "")
    #expect(values["structural_burst_bridge_upper_sec"] != "")
    #expect(values["structural_pause_anchor_count"] != "")
    #expect(values["structural_pause_seed_lower_sec"] != "")
    #expect(values["dataset_structural_seed_source"] == "structural_dataset_seed_aggregate")
    #expect(values["dataset_structural_train_count"] == "1")
    #expect(values["dataset_structural_seeded_train_count"] == "1")
    #expect((Int(values["dataset_structural_burst_anchor_count"] ?? "") ?? 0) >= 1)
    #expect(csv.contains("\"unit-test, \"\"source\"\"\""))
    #expect(csv.contains("\"unit, \"\"alpha\"\"\""))
    // MM (max/mean ratio) was removed from the Mac detection algorithm; the CSV schema
    // intentionally no longer carries an `mm` column (CV/CV2/LV remain).
    #expect(!header.contains("mm"))
    #expect(values["cv"] != nil)
    #expect(values["lv"] != nil)
}

@Test
func candidateCSVExportIncludesTonicSubtypeColumn() throws {
    let tonicTrain = SpikeTrain(
        name: "csv_subtype_tonic",
        timestampsSec: Array(stride(from: 0.0, through: 0.480, by: 0.040))
    )
    let dataset = SpikeDataset(
        name: "tonic subtype export",
        sourceDescription: "unit-test",
        trains: [tonicTrain]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))

    // The unified header carries the new column (so default event export is unchanged in shape).
    let defaultCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(in: dataset),
        reviewStatuses: [:],
        exportedAt: exportedAt
    )
    let defaultHeader = try #require(parseCSV(defaultCSV).first)
    #expect(defaultHeader.contains("state_tonic_subtype"))

    // The full/audit export carries the subtype value for the tonic state candidate.
    let fullCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(in: dataset),
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )
    let rows = parseCSV(fullCSV)
    let header = try #require(rows.first)
    #expect(header.contains("state_tonic_subtype"))
    let tonicRow = try #require(rows.dropFirst().first { row in
        let values = Dictionary(uniqueKeysWithValues: zip(header, row))
        return values["label"] == "tonic"
    })
    let values = Dictionary(uniqueKeysWithValues: zip(header, tonicRow))
    #expect(values["state_tonic_subtype"] == "classic")
}

@Test
func candidateCSVExportCarriesHFBurstPacketSubtypeForAdaptiveLocalHFBurstPacket() throws {
    // An adaptive local HF burst packet (two-sided, locally compressed) exports its additive,
    // audit-only HF-family subtype through the appended state_high_frequency_subtype column,
    // without changing finalLabel or any existing column position.
    let background = Array(repeating: 0.022, count: 12)
    let packet = [0.0090, 0.0105, 0.0095, 0.0110, 0.0098, 0.0102, 0.0096]
    let isi = background + [0.035] + packet + [0.040] + background
    var timestamps = [0.0]
    for value in isi {
        timestamps.append((timestamps.last ?? 0) + value)
    }
    let train = SpikeTrain(name: "csv_hf_burst_packet", timestampsSec: timestamps)
    let dataset = SpikeDataset(
        name: "hf burst packet export",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.008,
        burstBridgeUpperSec: 0.008,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )
    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let packetCandidate = try #require(result.candidates.first { candidate in
        candidate.candidateLayer == "adaptive_local_hf_burst_packet"
    })
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [result]
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: [],
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )

    let rows = parseCSV(csv)
    let header = try #require(rows.first)
    #expect(header.contains("state_high_frequency_subtype"))
    let packetRow = try #require(rows.dropFirst().first { row in
        Dictionary(uniqueKeysWithValues: zip(header, row))["candidate_id"] == packetCandidate.id
    })
    let values = Dictionary(uniqueKeysWithValues: zip(header, packetRow))
    #expect(values["state_high_frequency_subtype"] == "hf_burst_packet")
    #expect(values["candidate_layer"] == "adaptive_local_hf_burst_packet")
    #expect(values["label"]?.contains("burst") == true)
}

@Test
func selectedTrackCSVExportCarriesIrregularTonicSubtypeForSelectedStateRows() throws {
    // Irregular GPi-like train: a selected irregular-tonic state row must carry the subtype
    // in the same selected-track export surface the app uses (event/gap/state/review tracks).
    let isiMs: [Double] = [
        47.9, 65.3, 26.3, 29.0, 43.6, 39.7, 59.2, 18.0, 73.5, 40.1, 42.3, 24.4, 32.0, 34.7, 57.1,
        25.9, 40.6, 60.0, 26.1, 32.5, 38.9, 35.6, 39.3, 30.1, 75.0, 20.6, 38.3, 27.2, 50.9, 39.1,
        68.7, 64.9, 22.1, 29.6, 50.0, 34.3, 25.3, 52.1, 34.4, 18.9, 60.0, 21.1, 30.0, 33.2, 75.0,
        25.6, 30.2, 25.5, 66.8, 18.2, 26.2, 55.2, 74.5, 48.3, 34.7, 37.8, 47.6, 20.6, 33.2, 26.4,
        75.0, 35.4, 43.5, 29.3, 26.3, 51.4, 71.4, 43.5, 18.5, 34.8, 51.4, 35.5, 47.7, 18.0, 29.7,
        32.3, 56.2, 25.8, 18.7, 20.4, 39.3, 49.0, 56.1, 30.3, 27.0, 54.1, 18.0, 25.5, 35.5, 53.0,
        46.6, 34.1, 49.4, 39.8, 26.3, 40.8, 44.6, 32.6, 30.7, 34.8, 18.0, 43.5, 48.6, 43.4, 37.6,
        51.0, 37.0, 42.4, 24.3, 48.7, 38.8, 37.9, 31.2, 52.5, 74.0, 43.6, 46.6, 75.0, 18.0, 26.2
    ]
    var ts = [0.0]
    for value in isiMs { ts.append((ts.last ?? 0) + value / 1000) }
    let train = SpikeTrain(name: "irregular_csv_tonic", timestampsSec: ts)
    let dataset = SpikeDataset(name: "irregular tonic csv export", sourceDescription: "unit-test", trains: [train])
    // Keep this an exporter-contract test rather than coupling it to the production detector's
    // current HFS-vs-irregular-Tonic arbitration. Generate the real irregular-Tonic candidate,
    // then place that one state alone through the ordinary state-track arbitrator.
    let irregularCandidate = try #require(StatePatternDetector.detect(train: train).candidates.first {
        $0.finalLabel == .tonic && $0.stateTonicSubtype == "irregular"
    })
    let selectedCandidates = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
        [irregularCandidate]
    )
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: selectedCandidates
            )
        ]
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))

    // Selected-track export (default selected-only), matching the app's export surface.
    let selectedAnnotations = run.eventAnnotations(in: dataset, tracks: [.event, .gap, .state, .review])
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: selectedAnnotations,
        reviewStatuses: [:],
        exportedAt: exportedAt
    )
    let rows = parseCSV(csv)
    let header = try #require(rows.first)
    let irregularRow = try #require(rows.dropFirst().first { row in
        let values = Dictionary(uniqueKeysWithValues: zip(header, row))
        return values["label"] == "tonic" && values["state_tonic_subtype"] == "irregular"
    })
    let values = Dictionary(uniqueKeysWithValues: zip(header, irregularRow))
    #expect(values["state_tonic_subtype"] == "irregular")
}

@Test
func defaultEventCSVExportOmitsManuallyRejectedCandidatesButFullAuditKeepsThem() throws {
    let train = SpikeTrain(
        name: "unit_review_reject",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let dataset = SpikeDataset(
        name: "synthetic rejected export",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let candidate = try #require(run.candidates.first { $0.selectedForAuto && $0.finalLabel == .burst })

    let defaultCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(in: dataset),
        reviewStatuses: [candidate.id: "rejected"]
    )
    let defaultRows = parseCSV(defaultCSV)
    let defaultHeader = try #require(defaultRows.first)
    let defaultCandidateIDs = Set(defaultRows.dropFirst().map {
        Dictionary(uniqueKeysWithValues: zip(defaultHeader, $0))["candidate_id"] ?? ""
    })

    let fullAuditCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(
            in: dataset,
            selectedOnly: false,
            tracks: Set(ClassicAnchorSemanticTrack.allCases),
            includeEvidenceOnly: true
        ),
        reviewStatuses: [candidate.id: "rejected"],
        includeUnselectedCandidates: true,
        includeEvidenceOnlyCandidates: true
    )
    let fullRows = parseCSV(fullAuditCSV)
    let fullHeader = try #require(fullRows.first)
    let rejectedRow = try #require(fullRows.dropFirst().first { row in
        Dictionary(uniqueKeysWithValues: zip(fullHeader, row))["candidate_id"] == candidate.id
    })
    let rejectedValues = Dictionary(uniqueKeysWithValues: zip(fullHeader, rejectedRow))

    #expect(defaultCandidateIDs.contains(candidate.id) == false)
    #expect(rejectedValues["review_status"] == "rejected")
}

@Test
func fullCandidateCSVExportIncludesProfileAuditRowsAndDefaultExportOmitsThem() throws {
    let train = SpikeTrain(
        name: "profile_train",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let dataset = SpikeDataset(
        name: "synthetic profile export",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    let defaultCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(in: dataset),
        reviewStatuses: [:]
    )
    let fullCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(
            in: dataset,
            selectedOnly: false,
            tracks: Set(ClassicAnchorSemanticTrack.allCases)
        ),
        reviewStatuses: [:],
        includeUnselectedCandidates: true
    )

    let defaultRows = parseCSV(defaultCSV)
    let defaultHeader = try #require(defaultRows.first)
    let defaultValues = defaultRows.dropFirst().map { Dictionary(uniqueKeysWithValues: zip(defaultHeader, $0)) }
    #expect(defaultValues.allSatisfy { $0["label"] != "profile" })

    let fullRows = parseCSV(fullCSV)
    let fullHeader = try #require(fullRows.first)
    let profileRow = try #require(fullRows.dropFirst().first { row in
        Dictionary(uniqueKeysWithValues: zip(fullHeader, row))["candidate_layer"] == "dataset_isi_train_seed_band_profile"
    })
    let profileValues = Dictionary(uniqueKeysWithValues: zip(fullHeader, profileRow))
    let eventCoreProfileRow = try #require(fullRows.dropFirst().first { row in
        Dictionary(uniqueKeysWithValues: zip(fullHeader, row))["candidate_layer"] == "event_core_train_isi_band_profile"
    })
    let eventCoreProfileValues = Dictionary(uniqueKeysWithValues: zip(fullHeader, eventCoreProfileRow))
    let structuralDatasetProfileRow = try #require(fullRows.dropFirst().first { row in
        Dictionary(uniqueKeysWithValues: zip(fullHeader, row))["candidate_layer"] == "structural_dataset_seed_profile"
    })
    let structuralDatasetProfileValues = Dictionary(uniqueKeysWithValues: zip(fullHeader, structuralDatasetProfileRow))

    #expect(profileValues["candidate_layer"] == "dataset_isi_train_seed_band_profile")
    #expect(profileValues["candidate_class"] == "train_profile")
    #expect(profileValues["action"] == "audit_only")
    #expect(profileValues["selected_for_auto"] == "false")
    #expect(profileValues["profile_seed_band_fraction"] == "")
    #expect(profileValues["profile_seed_run_count"] != "")
    #expect(profileValues["profile_phenotype_prior"] != "")
    #expect(eventCoreProfileValues["candidate_class"] == "train_profile")
    #expect(eventCoreProfileValues["decision_path"] == "dataset_manual_isi_band_profile_percentiles_are_outputs")
    #expect(eventCoreProfileValues["action"] == "audit_only")
    #expect(eventCoreProfileValues["selected_for_auto"] == "false")
    #expect(eventCoreProfileValues["event_core_seed_low_sec"] != "")
    #expect(eventCoreProfileValues["event_core_seed_high_sec"] != "")
    #expect(eventCoreProfileValues["event_core_bridge_high_sec"] != "")
    #expect(eventCoreProfileValues["event_core_boundary_floor_sec"] == "0")
    #expect(eventCoreProfileValues["event_core_boundary_floor_hard"] == "false")
    #expect(eventCoreProfileValues["burst_contrast_S"] != "")
    #expect(eventCoreProfileValues["possible_contrast_S"] != "")
    #expect(eventCoreProfileValues["q10_ISI_sec"] != "")
    #expect(eventCoreProfileValues["q25_ISI_sec"] != "")
    #expect(eventCoreProfileValues["q90_ISI_sec"] != "")
    #expect(structuralDatasetProfileValues["candidate_class"] == "dataset_profile")
    #expect(structuralDatasetProfileValues["decision_path"]?.hasPrefix("dataset_structural_seed_aggregate") == true)
    #expect(structuralDatasetProfileValues["action"] == "audit_only")
    #expect(structuralDatasetProfileValues["selected_for_auto"] == "false")
    #expect(structuralDatasetProfileValues["dataset_structural_seed_source"] == "structural_dataset_seed_aggregate")
    #expect(structuralDatasetProfileValues["dataset_structural_train_count"] == "1")
    #expect(structuralDatasetProfileValues["dataset_structural_seeded_train_count"] == "1")
}

@Test
func fullCandidateCSVExportIncludesBurstEventGrammarAuditColumns() throws {
    let train = SpikeTrain(
        name: "unit_bridge",
        timestampsSec: [0, 0.100, 0.111, 0.122, 0.1386, 0.144, 0.230]
    )
    let dataset = SpikeDataset(
        name: "synthetic burst audit export",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.015,
        burstBridgeUpperSec: 0.020
    )
    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let eventCandidate = try #require(result.candidates.first { candidate in
        candidate.candidateLayer == "structure_first_classic_burst_anchor" &&
            candidate.finalLabel == .burst
    })
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [result]
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: [],
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )

    let rows = parseCSV(csv)
    let header = try #require(rows.first)
    let row = try #require(rows.dropFirst().first { row in
        Dictionary(uniqueKeysWithValues: zip(header, row))["candidate_id"] == eventCandidate.id
    })
    let values = Dictionary(uniqueKeysWithValues: zip(header, row))

    #expect(values["candidate_layer"] == "structure_first_classic_burst_anchor")
    #expect(values["candidate_class"] == "structure_first_two_sided_classic_burst_i")
    #expect(values["burst_seed_run_start_isi"] == "2")
    #expect(values["burst_seed_run_end_isi"] == "5")
    #expect(values["seed_band_lower_sec"] == "0.0009")
    #expect(values["seed_band_upper_sec"] != "")
    #expect(values["bridge_band_upper_sec"] == values["seed_band_upper_sec"])
    #expect(values["burst_contrast_required"] == "3")
    #expect(values["possible_contrast_required"] == "2")
    #expect(values["required_gap_sec"] != "")
    #expect(values["possible_required_gap_sec"] != "")
    #expect(values["strict_boundary_pass"] == "true")
    #expect(values["possible_boundary_pass"] == "true")
    #expect(values["bridge_count_pass"] == "true")
    #expect(values["bridge_fraction_pass"] == "true")
    #expect(values["q90_bridge_pass"] == "true")
    #expect(values["size_label_before_review"] == "burst")
}

@Test
func fullCandidateCSVExportIncludesHardThresholdBurstAuditColumns() throws {
    let train = SpikeTrain(
        name: "unit_hard_threshold",
        timestampsSec: exportTestTimestamps(fromISI: [
            0.200,
            0.080,
            0.050,
            0.060,
            0.040,
            0.200
        ])
    )
    let dataset = SpikeDataset(
        name: "synthetic hard threshold export",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.070,
        burstHardThresholdEnabled: true
    )
    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let hardCandidate = try #require(result.candidates.first { candidate in
        candidate.candidateLayer == "isi_profile_hard_threshold_burst"
    })
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [result]
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: [],
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )

    let rows = parseCSV(csv)
    let header = try #require(rows.first)
    let row = try #require(rows.dropFirst().first { row in
        Dictionary(uniqueKeysWithValues: zip(header, row))["candidate_id"] == hardCandidate.id
    })
    let values = Dictionary(uniqueKeysWithValues: zip(header, row))

    #expect(values["candidate_layer"] == "isi_profile_hard_threshold_burst")
    #expect(values["threshold_mode"] == "hard_threshold")
    #expect(values["hard_threshold"] == "true")
    #expect(values["hard_threshold_pattern"] == "burst")
    #expect(values["hard_burst_seed_upper_sec"] == "0.07")
    #expect(values["hard_burst_bridge_upper_sec"] == "0.0875")
    #expect(values["hard_burst_core_isi_count"] == "3")
    #expect(values["hard_threshold_source"] == "ui_isi_profile_threshold_line")
}

@Test
func classicAnchorReviewStatusCSVImporterReadsExportedCandidateStatuses() throws {
    let csv = """
    exported_at,candidate_id,train_name,review_status,decision_path
    2026-06-17T00:00:00Z,"train,1-classic-anchor-burst-1","unit, one",accepted,"path, with comma"
    2026-06-17T00:00:00Z,train-2-classic-anchor-burst-1,unit two,needsReview,plain
    """

    let statuses = try ClassicAnchorReviewStatusCSVImporter.importStatuses(contents: csv)

    #expect(statuses["train,1-classic-anchor-burst-1"] == "accepted")
    #expect(statuses["train-2-classic-anchor-burst-1"] == "needsReview")
}

@Test
func defaultEventCSVExportUsesOnlyAutoSelectedSampleEvents() throws {
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    try assertDefaultEventExportUsesOnlyAutoSelectedEvents(
        csvURL: sampleURL,
        datasetName: "sample",
        selectedTrainNames: nil
    )
}

@Test
func optionalPDSTNDefaultEventCSVExportUsesOnlyAutoSelectedEvents() throws {
    guard let path = ProcessInfo.processInfo.environment["STPD_COMPLEX_CSV"], !path.isEmpty else {
        return
    }

    try assertDefaultEventExportUsesOnlyAutoSelectedEvents(
        csvURL: URL(fileURLWithPath: path),
        datasetName: "PD_STN_subset",
        selectedTrainNames: [
            "LT1D00.732F001-nw-11 (flag 1)",
            "RT2D03.535_nw-5 (flag 1)"
        ]
    )
}

private func parseCSV(_ csv: String) -> [[String]] {
    var rows: [[String]] = []
    var row: [String] = []
    var field = ""
    var isQuoted = false
    var index = csv.startIndex

    while index < csv.endIndex {
        let character = csv[index]

        if character == "\"" {
            let next = csv.index(after: index)
            if isQuoted, next < csv.endIndex, csv[next] == "\"" {
                field.append("\"")
                index = csv.index(after: next)
                continue
            }
            isQuoted.toggle()
        } else if character == ",", !isQuoted {
            row.append(field)
            field = ""
        } else if character == "\n", !isQuoted {
            row.append(field)
            if !row.allSatisfy(\.isEmpty) {
                rows.append(row)
            }
            row = []
            field = ""
        } else if character != "\r" {
            field.append(character)
        }

        index = csv.index(after: index)
    }

    if !field.isEmpty || !row.isEmpty {
        row.append(field)
        rows.append(row)
    }

    return rows
}

private func exportTestTimestamps(fromISI isi: [Double]) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(isi.count + 1)
    for value in isi {
        values.append((values.last ?? 0) + value)
    }
    return values
}

private func assertDefaultEventExportUsesOnlyAutoSelectedEvents(
    csvURL: URL,
    datasetName: String,
    selectedTrainNames: Set<String>?
) throws {
    let csv = try String(contentsOf: csvURL, encoding: .utf8)
    let parsed = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: datasetName,
        sourceDescription: csvURL.path,
        unit: .seconds,
        duplicatePolicy: .collapseExact
    )
    let dataset: SpikeDataset
    if let selectedTrainNames {
        dataset = SpikeDataset(
            name: datasetName,
            sourceDescription: csvURL.path,
            trains: parsed.trains.filter { selectedTrainNames.contains($0.name) }
        )
    } else {
        dataset = parsed
    }

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let annotations = run.eventAnnotations(in: dataset)
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))
    let eventCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: annotations,
        reviewStatuses: [:],
        exportedAt: exportedAt
    )
    let rows = parseCSV(eventCSV)
    let header = try #require(rows.first)
    let dataRows = Array(rows.dropFirst())
    let values = dataRows.map { row in
        Dictionary(uniqueKeysWithValues: zip(header, row))
    }

    #expect(!dataset.trains.isEmpty)
    #expect(run.selectedAutoCount > 0)
    #expect(run.selectedEventCount > 0)
    #expect(annotations.count == run.selectedEventCount)
    #expect(dataRows.count == run.selectedEventCount)
    #expect(values.allSatisfy { $0["selected_for_auto"] == "true" })
    #expect(values.allSatisfy { $0["recommended_track"] == "event" })
    #expect(Set(values.compactMap { $0["candidate_id"] }) == Set(annotations.map(\.candidateID)))
}

private func repositoryRoot() -> URL {
    var url = URL(fileURLWithPath: #filePath)
    for _ in 0..<5 {
        url.deleteLastPathComponent()
    }
    return url
}
