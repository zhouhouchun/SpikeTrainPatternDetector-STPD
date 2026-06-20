import Foundation
@testable import STPDCore
import Testing

@Test
func ordinaryClassicBurstDoesNotCollapseIntoHighFrequencyBurst() throws {
    let train = SpikeTrain(
        name: "classic_not_hfb",
        timestampsSec: auditTimestamps(fromISI: [0.100, 0.006, 0.006, 0.006, 0.087])
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBandSource: .userPatternISILimit
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.candidateLayer == "classic_anchor_burst" })

    #expect(candidate.finalLabel == .burst)
    #expect(candidate.auditRecommendedFamily == "burst_event")
    #expect(candidate.auditRecommendedTrack == .event)
    #expect(candidate.auditRecommendedSubtype == "classic_burst")
    #expect(candidate.auditRecommendedFinalClass == "burst")
    #expect(candidate.auditRecommendedEventTrackClass == "burst")
    #expect(candidate.auditReviewStatus == "review")
    #expect(candidate.auditReviewRequired)
    #expect(candidate.auditConfidenceTier == "audit_high_confidence_event_like")
}

@Test
func finiteVeryFastPacketIsAuditedAsHighFrequencyBurst() throws {
    let train = SpikeTrain(
        name: "hfb_packet",
        timestampsSec: auditTimestamps(fromISI: [0.120, 0.003, 0.0035, 0.004, 0.110])
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBandSource: .userPatternISILimit
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.finalLabel == .highFrequencyBurst })

    #expect(candidate.nSpikes == 4)
    #expect(candidate.auditRecommendedFamily == "burst_event")
    #expect(candidate.auditRecommendedSubtype == "high_frequency_burst")
    #expect(candidate.auditRecommendedFinalClass == "high_frequency_burst")
    #expect(candidate.auditRecommendedEventTrackClass == "high_frequency_burst")
    #expect(candidate.auditReviewStatus == "review")
    #expect(candidate.auditReviewRequired)
    #expect(candidate.auditConfidenceTier == "audit_high_confidence_event_like")
}

@Test
func longBurstSettingsRemainFiniteSoHFSpikingStartsAboveLongBurstExtent() throws {
    let settings = ClassicAnchorSettings(longMaxSpikes: 0)
    #expect(settings.longMinSpikes == 10)
    #expect(settings.longMaxSpikes == 16)

    let denseButFinitePacket = SpikeTrain(
        name: "dense_below_hfs_floor",
        timestampsSec: auditTimestamps(repeating: 0.010, count: 15)
    )
    let dataset = SpikeDataset(
        name: "dense below hfs floor",
        sourceDescription: "unit-test",
        trains: [denseButFinitePacket]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005),
        stateTuning: StatePatternDetectorTuning(highFrequencySpikingMinSpikes: 3)
    )

    #expect(run.highFrequencySpikingCount == 0)
    #expect(!run.candidates.contains { $0.finalLabel == .highFrequencySpiking })
}

@Test
func longBurstAuditCarriesStrictnessStatus() {
    var strict = auditCandidate(
        id: "strict_long",
        label: .longBurst,
        selectedForAuto: true,
        lockLevel: .lockedClassic,
        nSpikes: 16,
        nISI: 15,
        edgeContrastMinQ90: 4.2
    )
    strict.burstBridgeFractionPass = true
    strict.burstQ90BridgePass = true

    var weak = auditCandidate(
        id: "weak_long",
        label: .possibleBurst,
        selectedForAuto: false,
        lockLevel: .strongCandidate,
        nSpikes: 16,
        nISI: 15,
        edgeContrastMinQ90: 2.2
    )
    weak.burstSizeLabelBeforeReview = ClassicAnchorLabel.longBurst.rawValue
    weak.burstBridgeFractionPass = true
    weak.burstQ90BridgePass = true

    #expect(strict.auditRecommendedSubtype == "long_burst")
    #expect(strict.auditRecommendedEventTrackClass == "long_burst")
    #expect(strict.auditReviewStatus == "accepted")
    #expect(strict.auditLongBurstDefinitionStatus == "strict_pass")
    #expect(!strict.auditReviewRequired)
    #expect(strict.auditConfidenceTier == "audit_high_confidence_structural")

    #expect(weak.auditRecommendedFamily == "burst_event")
    #expect(weak.auditRecommendedTrack == .event)
    #expect(weak.auditRecommendedSubtype == "burst_ii")
    #expect(weak.auditRecommendedFinalClass == "burst_ii")
    #expect(weak.auditRecommendedEventTrackClass == "burst")
    #expect(weak.auditReviewStatus == "auto_candidate")
    #expect(weak.auditLongBurstDefinitionStatus == "context_weak")
    #expect(!weak.auditReviewRequired)
    #expect(weak.auditUncertaintyReason == "context_weak")
}

@Test
func legacySuppressedHFSpikingAuditPreservesOriginalLabelAndSuppressorReason() {
    var suppressed = auditCandidate(
        id: "suppressed_burst",
        label: .reject,
        action: "suppress_embedded_burst_for_hf_spiking_state",
        gateStatus: "hf_protected_suppressed",
        decisionPath: "compact_burst_kernel_suppressed_inside_long_hf_spiking_state; suppressor=hfs_1"
    )
    suppressed.suppressedByHFSpikingState = true
    suppressed.suppressedOriginalLabel = ClassicAnchorLabel.burst.rawValue
    suppressed.hfSpikingSuppressorID = "hfs_1"

    #expect(suppressed.auditRecommendedFamily == "suppressed_candidate")
    #expect(suppressed.auditRecommendedTrack == .diagnostic)
    #expect(suppressed.auditRecommendedSubtype == "burst")
    #expect(suppressed.auditRecommendedFinalClass == "reject")
    #expect(suppressed.auditRecommendedEventTrackClass == "reject")
    #expect(suppressed.auditReviewStatus == "suppressed")
    #expect(suppressed.auditReviewRequired)
    #expect(suppressed.auditConfidenceTier == "audit_suppressed_by_hf_spiking")
    #expect(suppressed.auditUncertaintyReason == "compact_burst_kernel_suppressed_inside_long_hf_spiking_state")
}

@Test
func eventCSVExportIncludesEventGrammarAuditColumns() throws {
    let train = SpikeTrain(
        name: "audit_export_train",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let dataset = SpikeDataset(
        name: "audit export",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let candidate = auditCandidate(
        id: "audit_export_candidate",
        trainID: train.id,
        trainName: train.name,
        label: .burst,
        selectedForAuto: true,
        lockLevel: .lockedClassic
    )
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: [candidate]
            )
        ]
    )
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: [],
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z") ?? Date()
    )
    let rows = auditParseCSV(csv)
    let header = try #require(rows.first)
    let row = try #require(rows.dropFirst().first)
    let values = Dictionary(uniqueKeysWithValues: zip(header, row))

    #expect(values["recommended_family"] == "burst_event")
    #expect(values["recommended_subtype"] == "classic_burst")
    #expect(values["recommended_final_class"] == "burst")
    #expect(values["recommended_event_track_class"] == "burst")
    #expect(values["audit_review_status"] == "accepted")
    #expect(values["recommended_review_required"] == "false")
    #expect(values["recommendation_confidence_tier"] == "audit_high_confidence_event_like")
    #expect(values["recommended_uncertainty_reason"] == "")
    #expect(values["long_burst_definition_status"] == "")
}

private func auditCandidate(
    id: String,
    trainID: String = "audit_train",
    trainName: String = "audit_train",
    label: ClassicAnchorLabel,
    action: String = "accept",
    gateStatus: String = "pass",
    decisionPath: String = "unit_test",
    selectedForAuto: Bool = false,
    lockLevel: ClassicAnchorLockLevel = .strongCandidate,
    nSpikes: Int = 4,
    nISI: Int = 3,
    edgeContrastMinQ90: Double? = 4.0
) -> ClassicAnchorCandidate {
    ClassicAnchorCandidate(
        id: id,
        trainID: trainID,
        trainName: trainName,
        candidateLayer: "unit_test",
        candidateClass: "unit_test",
        finalLabel: label,
        gateStatus: gateStatus,
        decisionPath: decisionPath,
        action: action,
        score: 1,
        priority: 1_000,
        selectedForAuto: selectedForAuto,
        selectionStatus: selectedForAuto ? "selected_by_event_core_weighted_interval_grammar" : "not_selected",
        startISIIndex: 1,
        endISIIndex: max(1, nISI),
        startSpikeIndex: 1,
        endSpikeIndex: max(2, nSpikes),
        nISI: nISI,
        nValidISI: nISI,
        nSpikes: nSpikes,
        durationSec: nil,
        intraQ10Sec: nil,
        intraQ40Sec: nil,
        intraQ50Sec: nil,
        intraQ90Sec: 0.006,
        intraQ95Sec: 0.007,
        maxIntraISISec: nil,
        meanIntraISISec: nil,
        cv: nil,
        lv: nil,
        mm: nil,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: edgeContrastMinQ90,
        edgeContrastGeomQ90: edgeContrastMinQ90,
        anchorFamily: "unit_test",
        anchorLockLevel: lockLevel,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.010,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 2.5,
        anchorContrastGeomRequired: 2.5,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil
    )
}

private func auditTimestamps(fromISI isi: [Double]) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(isi.count + 1)
    for value in isi {
        values.append((values.last ?? 0) + value)
    }
    return values
}

private func auditTimestamps(repeating isi: Double, count: Int) -> [Double] {
    auditTimestamps(fromISI: Array(repeating: isi, count: count))
}

private func auditParseCSV(_ csv: String) -> [[String]] {
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
