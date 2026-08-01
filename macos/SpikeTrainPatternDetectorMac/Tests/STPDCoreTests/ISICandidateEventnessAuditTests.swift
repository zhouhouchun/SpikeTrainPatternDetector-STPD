import Foundation
import STPDCore
import Testing

// Phase 2C: candidate eventness audit (diagnostic only). Pins R parity of the eventness formulas and
// PROVES the audit never changes detection output.

private func train(_ name: String, isi: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for d in isi { ts.append(ts.last! + d) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

private func audit(
    _ t: SpikeTrain, start: Int, end: Int, edgeContrastMin: Double?, minValid: Double = 0.001
) -> ISICandidateEventnessAudit {
    ISICandidateEventnessAuditor.makeAudit(
        train: t, startISIIndex: start, endISIIndex: end, minValidISISec: minValid,
        edgeContrastMin: edgeContrastMin
    )
}

// MARK: - 1. Event-like candidate (compressed core, strong context + edge, flanks return to baseline).

@Test
func eventLikeCandidateScoresEventLike() {
    // isiSec[1..10]=0.100, [11..15]=0.005 (candidate), [16..26]=0.100. pre=isi[10], post=isi[16]=0.100.
    let isi = Array(repeating: 0.100, count: 10) + Array(repeating: 0.005, count: 5) + Array(repeating: 0.100, count: 11)
    let t = train("e", isi: isi)
    let a = audit(t, start: 11, end: 15, edgeContrastMin: 20.0)   // Phase 2B edge = 0.100/0.005 = 20
    #expect(abs((a.q90ISISec ?? 0) - 0.005) < 1e-9)
    #expect(abs((a.q90Q10Ratio ?? 0) - 1.0) < 1e-9)
    #expect(abs((a.distantContextMedianSec ?? 0) - 0.100) < 1e-9)
    #expect(abs((a.contextContrast ?? 0) - 20.0) < 1e-6)              // 0.100 / 0.005
    #expect(abs((a.eventnessContextComponent ?? 0) - 1.0) < 1e-9)     // clamp01(20/3)
    #expect(abs((a.eventnessEdgeComponent ?? 0) - 1.0) < 1e-9)        // clamp01(20/3)
    #expect(abs((a.returnToBaselineScore ?? 0) - 1.0) < 1e-9)         // flank == context
    #expect(abs((a.eventnessScore ?? 0) - 1.0) < 1e-9)               // mean(1, 1)
    #expect(a.eventnessZone == "event_like")
    #expect(a.mediumEventnessReview == false)
    #expect(a.auditRecommendation == "review_event_like_candidate")
}

// MARK: - 2. State-like / regular candidate (weak edge, no distant context).

@Test
func regularCandidateWithoutContextScoresStateLike() {
    // 6 spikes, all ISI 0.050; candidate [2,4] -> no distant context window (short train).
    let t = train("s", isi: Array(repeating: 0.050, count: 5))
    let a = audit(t, start: 2, end: 4, edgeContrastMin: 1.0)         // regular edge ratio = 1
    #expect(a.distantContextMedianSec == nil)
    #expect(a.contextContrast == nil)
    #expect(a.eventnessContextComponent == nil)
    #expect(a.returnToBaselineScore == nil)                          // no context => no return score
    #expect(abs((a.eventnessEdgeComponent ?? 0) - (1.0 / 3.0)) < 1e-9)
    #expect(abs((a.eventnessScore ?? 0) - (1.0 / 3.0)) < 1e-9)       // boundary only = edge component
    #expect(a.eventnessZone == "state_like")                        // 0.333 <= 0.45
    #expect(abs((a.regularityScore ?? 0) - 1.0) < 1e-9)             // perfectly regular core
    #expect(a.auditRecommendation == "review_state_like_candidate")
}

// MARK: - 3. Missing evidence -> nil / unknown, never fabricated.

@Test
func outOfRangeSpanIsUndefined() {
    let t = train("u", isi: Array(repeating: 0.050, count: 5))
    let a = audit(t, start: 0, end: 2, edgeContrastMin: 10.0)        // start 0 invalid
    #expect(a.eventnessScore == nil && a.q90ISISec == nil && a.contextContrast == nil)
    #expect(a.eventnessZone == "unknown")
    #expect(a.auditRecommendation == "insufficient_evidence")
}

@Test
func missingEdgeAndContextYieldUnknownZone() {
    // Short train, no context, and no edge evidence (edgeContrastMin nil) -> no boundary -> nil score.
    let t = train("m", isi: Array(repeating: 0.050, count: 5))
    let a = audit(t, start: 2, end: 4, edgeContrastMin: nil)
    #expect(a.eventnessEdgeComponent == nil)
    #expect(a.eventnessScore == nil)
    #expect(a.eventnessZone == "unknown")
    // quantiles are still defined (not fabricated away): the core exists.
    #expect(a.q50ISISec != nil)
}

// MARK: - 4. Zone thresholds match the R defaults (>= 0.60, <= 0.45, medium between).

@Test
func eventnessZoneThresholdsMatchRDefaults() {
    // Short regular train with no context => eventnessScore == clamp01(edgeContrastMin / 3).
    let t = train("z", isi: Array(repeating: 0.050, count: 5))
    func zone(_ edgeMin: Double) -> String {
        audit(t, start: 2, end: 4, edgeContrastMin: edgeMin).eventnessZone
    }
    #expect(zone(1.80) == "event_like")               // 0.60
    #expect(zone(1.35) == "state_like")               // 0.45
    #expect(zone(1.50) == "medium_eventness_review")  // 0.50
    #expect(zone(6.00) == "event_like")               // clamps to 1.0
    #expect(audit(t, start: 2, end: 4, edgeContrastMin: 1.50).mediumEventnessReview == true)
}

// MARK: - 5. Zero detector influence.

private func burstDataset() -> SpikeDataset {
    let isi = [0.1, 0.1, 0.1, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.1, 0.1, 0.1]
    return SpikeDataset(name: "burst", sourceDescription: "synthetic", trains: [train("b", isi: isi)])
}

private func signature(_ run: ClassicAnchorDetectionRun) -> [String] {
    run.candidates.map {
        "\($0.id)|\($0.finalLabel)|\($0.startISIIndex)|\($0.endISIIndex)|\($0.startSpikeIndex)|\($0.endSpikeIndex)|\($0.selectedForAuto)|\($0.score)"
    }.sorted()
}

@Test
func computingEventnessAuditDoesNotChangeDetectionOutput() {
    let dataset = burstDataset()
    let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run1 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    let before = signature(run1)
    #expect(run1.candidates.count >= 1)

    let auditByID = ISICandidateEventnessAuditor.auditByCandidateID(run: run1, dataset: dataset)
    let after = signature(run1)
    #expect(before == after)                                       // audit mutated nothing
    #expect(auditByID.count == run1.candidates.count)              // every candidate id maps to an audit

    let run2 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    #expect(signature(run2) == before)                             // re-run identical

    #expect(auditByID.values.contains { $0.eventnessScore != nil }) // audit actually computes
    // Every zone is one of the four sanctioned strings (never a detector label).
    let zones = Set(auditByID.values.map(\.eventnessZone))
    #expect(zones.isSubset(of: ["event_like", "state_like", "medium_eventness_review", "unknown"]))
}

// MARK: - 6. CSV export appends eventness columns after the Phase 2B isi_* columns.

@Test
func csvExportAppendsEventnessColumnsAfterISIColumns() throws {
    let dataset = burstDataset()
    let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset, run: run, annotations: [], reviewStatuses: [:], includeUnselectedCandidates: true
    )
    let header = csv.split(separator: "\n").first.map { $0.split(separator: ",").map(String.init) } ?? []
    let eventnessColumns = [
        "eventness_q10_isi_sec", "eventness_q50_isi_sec", "eventness_q90_isi_sec",
        "eventness_q90_q10_ratio", "eventness_distant_context_median_sec", "eventness_context_contrast",
        "eventness_return_to_baseline_score", "eventness_edge_component", "eventness_context_component",
        "eventness_score", "eventness_regularity_score", "eventness_zone", "eventness_medium_review",
        "eventness_audit_recommendation", "eventness_audit_note"
    ]
    for column in eventnessColumns {
        #expect(header.contains(column))
    }
    // The eventness_* columns form a contiguous ordered block after the Phase 2B isi_* block
    // (Phase 2D near_miss_* columns follow, so this is no longer the header suffix).
    let start = try #require(header.firstIndex(of: "eventness_q10_isi_sec"))
    #expect(start + eventnessColumns.count <= header.count)
    #expect(Array(header[start..<(start + eventnessColumns.count)]) == eventnessColumns)
    #expect(header.contains("isi_edge_contrast_min"))   // Phase 2B columns preserved
    #expect(header.contains("candidate_id"))            // existing required columns preserved
    #expect(header.contains("resolved_thresholds"))
}
