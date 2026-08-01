import Foundation
import STPDCore
import Testing

// Phase 2D: candidate near-miss review (diagnostic only). Pins the gate-margin math and PROVES the
// audit never changes detection output.

private func train(_ name: String, isi: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for d in isi { ts.append(ts.last! + d) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

private func eligibleAudit(
    edgeMin: Double?, anchorMin: Double?,
    edgeGeom: Double? = 10.0, anchorGeom: Double? = 1.50,
    possibleMin: Double? = nil,
    eventnessZone: String = "unknown"
) -> ISINearMissAudit {
    ISINearMissAuditor.makeEligibleAudit(
        candidateRef: "candidate:test",
        edgeContrastMinQ90: edgeMin,
        edgeContrastGeomQ90: edgeGeom,
        anchorContrastMinRequired: anchorMin,
        anchorContrastGeomRequired: anchorGeom,
        burstPossibleContrastRequired: possibleMin,
        failureReason: "",
        candidateClass: "classic_anchor_short_isi_packet",
        finalLabelRawValue: "reject",
        eventnessScore: nil,
        eventnessZone: eventnessZone
    )
}

// MARK: - 1. Just-below-threshold candidate is a near miss with correct gate margin.

@Test
func justBelowFinalEdgeThresholdIsNearMiss() {
    // edge min 1.40 vs required 1.45 (geom passes, possible skipped) -> one failed gate.
    let a = eligibleAudit(edgeMin: 1.40, anchorMin: 1.45)
    #expect(a.eligible == true)
    #expect(a.isNearMiss == true)
    #expect(a.failureCount == 1)
    #expect(a.bestParameter == "burst_final_edge_min")
    #expect(a.bestCategory == "final")
    #expect(a.bestDirection == "decrease")             // a minimum gate would need to drop
    #expect(abs((a.bestCurrentValue ?? 0) - 1.45) < 1e-12)   // threshold
    #expect(abs((a.bestRequiredValue ?? 0) - 1.40) < 1e-12)  // candidate metric
    #expect(abs((a.bestAbsoluteChange ?? 0) - (-0.05)) < 1e-12)
    #expect(abs((a.bestRelativeChange ?? 0) - (0.05 / 1.45)) < 1e-9)
    #expect(abs((a.nearMissScore ?? 0) - (1 - (0.05 / 1.45) / 0.25)) < 1e-9)
}

// MARK: - 2. Far-below-threshold candidate is eligible but not a near miss.

@Test
func farBelowThresholdIsEligibleButNotNearMiss() {
    let a = eligibleAudit(edgeMin: 0.5, anchorMin: 1.45)   // relativeChange = 0.95/1.45 = 0.655 > 0.25
    #expect(a.eligible == true)
    #expect(a.isNearMiss == false)
    #expect(a.failureCount == 1)
    #expect((a.bestRelativeChange ?? 0) > 0.25)
    #expect(a.nearMissScore == 0)                          // 1 - clamp01(>1) clamps to 0
}

// MARK: - 3. Selected accepted candidates are ineligible (via the real pipeline).

private func burstDataset() -> SpikeDataset {
    let isi = [0.1, 0.1, 0.1, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.1, 0.1, 0.1]
    return SpikeDataset(name: "burst", sourceDescription: "synthetic", trains: [train("b", isi: isi)])
}

@Test
func selectedAcceptedCandidatesAreIneligible() {
    let dataset = burstDataset()
    let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    let auditByID = ISINearMissAuditor.auditByCandidateID(run: run, dataset: dataset)

    // Accepted, auto-selected, non-reject candidates must be ineligible and never a near miss.
    let acceptedSelected = run.candidates.filter {
        $0.selectedForAuto && $0.finalLabel != .reject && $0.action.lowercased() != "reject"
            && !$0.gateStatus.lowercased().contains("reject") && !$0.isStructuralPausePriorEvidence
    }
    #expect(acceptedSelected.isEmpty == false)   // the burst is detected + selected
    for candidate in acceptedSelected {
        #expect(ISINearMissAuditor.isEligible(candidate) == false)
        #expect(auditByID[candidate.id]?.eligible == false)
        #expect(auditByID[candidate.id]?.isNearMiss == false)
    }
}

// MARK: - 4. Event-like unselected candidate without a gate margin: eventness diagnostic, not near miss.

@Test
func eventLikeWithoutGateMarginIsEventnessReasonNotNearMiss() {
    // All gate metrics pass (edge well above thresholds) -> no failed gate; eventness zone event_like.
    let a = eligibleAudit(edgeMin: 5.0, anchorMin: 1.45, eventnessZone: "event_like")
    #expect(a.eligible == true)
    #expect(a.failureCount == 0)
    #expect(a.bestParameter == nil)
    #expect(a.isNearMiss == false)                         // eventness alone never sets near miss
    #expect(a.bestCategory == "eventness")
    #expect(a.reason == "eventness_without_gate_margin")
    #expect(a.nearMissScore == nil)
}

// MARK: - 5. CSV export appends near_miss_* columns after the eventness_* block.

@Test
func csvExportAppendsNearMissColumnsAfterEventness() {
    let dataset = burstDataset()
    let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset, run: run, annotations: [], reviewStatuses: [:], includeUnselectedCandidates: true
    )
    let header = csv.split(separator: "\n").first.map { $0.split(separator: ",").map(String.init) } ?? []
    let nearMissColumns = [
        "near_miss_eligible", "near_miss_is_near_miss", "near_miss_category", "near_miss_parameter",
        "near_miss_direction", "near_miss_current_value", "near_miss_required_value",
        "near_miss_absolute_change", "near_miss_relative_change", "near_miss_failure_count",
        "near_miss_score", "near_miss_eventness_score", "near_miss_eventness_zone",
        "near_miss_candidate_ref", "near_miss_reason", "near_miss_details"
    ]
    for column in nearMissColumns {
        #expect(header.contains(column))
    }
    let expectedTail = nearMissColumns + ["resolved_thresholds"]
    #expect(Array(header.suffix(expectedTail.count)) == expectedTail)
    #expect(header.contains("eventness_score"))           // Phase 2C preserved
    #expect(header.contains("isi_edge_contrast_min"))     // Phase 2B preserved
    #expect(header.contains("candidate_id"))              // existing preserved
}

// MARK: - 6. Zero detector influence.

private func signature(_ run: ClassicAnchorDetectionRun) -> [String] {
    run.candidates.map {
        "\($0.id)|\($0.finalLabel)|\($0.startISIIndex)|\($0.endISIIndex)|\($0.startSpikeIndex)|\($0.endSpikeIndex)|\($0.selectedForAuto)|\($0.score)"
    }.sorted()
}

@Test
func computingNearMissAuditDoesNotChangeDetectionOutput() {
    let dataset = burstDataset()
    let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run1 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    let before = signature(run1)
    #expect(run1.candidates.count >= 1)

    let auditByID = ISINearMissAuditor.auditByCandidateID(run: run1, dataset: dataset)
    let after = signature(run1)
    #expect(before == after)                              // audit mutated nothing
    #expect(auditByID.count == run1.candidates.count)     // every candidate id maps to an audit

    let run2 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    #expect(signature(run2) == before)                    // re-run identical

    // Ranking is a stable, pure view over near-miss candidates (no detector state).
    let ranked = ISINearMissAuditor.rankedNearMisses(run: run1, dataset: dataset)
    #expect(ranked.allSatisfy { $0.audit.isNearMiss })
}
