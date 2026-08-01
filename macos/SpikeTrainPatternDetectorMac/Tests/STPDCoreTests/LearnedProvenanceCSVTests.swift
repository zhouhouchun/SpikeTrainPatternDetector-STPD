import Foundation
@testable import STPDCore
import Testing

// Phase 1E fix: the `resolved_thresholds` CSV column must also carry the Phase-1D learned-from-annotations
// note (not only the verbose decision_path), so a learned soft anchor is auditable in the dedicated column —
// matching the Detector/Parameters note that "every applied threshold is recorded in ... the resolved_thresholds
// CSV column".

@Test
func resolvedThresholdsProjectionIncludesLearnedProvenance() {
    let decisionPath = [
        "structure_first_classic_burst_i",
        "resolved_threshold[burst.seed_upper_sec]=user_soft_anchor",
        "learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.57)",
        "pipeline_stage=dataset_seed_aware_final_arbitration",
    ].joined(separator: ";")

    let projected = ClassicAnchorEventCSVExporter.resolvedThresholdProvenance(decisionPath)

    // Both the resolved-threshold token and the learned note are projected into the column.
    #expect(projected.contains("resolved_threshold[burst.seed_upper_sec]=user_soft_anchor"))
    #expect(projected.contains("learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.57)"))
    // Non-provenance tokens are excluded.
    #expect(!projected.contains("structure_first_classic_burst_i"))
    #expect(!projected.contains("pipeline_stage"))
}

@Test
func resolvedThresholdsProjectionEmptyWhenNoProvenance() {
    let projected = ClassicAnchorEventCSVExporter.resolvedThresholdProvenance("structure_first_classic_burst_i;pipeline_stage=x")
    #expect(projected.isEmpty)
}

// P10B: the `resolved_thresholds` projection must also carry the P10 `manual_threshold_scope=...` note, so the applied
// hard-threshold scope is auditable in the dedicated column (not only by parsing the full decision_path). Raw token kept verbatim.

@Test
func resolvedThresholdsProjectionIncludesScopeNoteAlongsideResolvedThreshold() {
    let decisionPath = [
        "structure_first_classic_burst_i",
        "resolved_threshold[burst.seed_upper_sec]=user_hard_gate",
        "learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.57)",
        "manual_threshold_scope=all_trains",
        "pipeline_stage=dataset_seed_aware_final_arbitration",
    ].joined(separator: ";")
    let projected = ClassicAnchorEventCSVExporter.resolvedThresholdProvenance(decisionPath)
    // The scope note joins the existing tokens; resolved_threshold + learned tokens remain (regression).
    #expect(projected.contains("manual_threshold_scope=all_trains"))
    #expect(projected.contains("resolved_threshold[burst.seed_upper_sec]=user_hard_gate"))
    #expect(projected.contains("learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.57)"))
    #expect(!projected.contains("pipeline_stage"))
}

@Test
func resolvedThresholdsProjectionCarriesSelectedAndCurrentScopeTokensVerbatim() {
    let selected = ClassicAnchorEventCSVExporter.resolvedThresholdProvenance(
        "resolved_threshold[burst.seed_upper_sec]=user_hard_gate;manual_threshold_scope=selected_trains(count=2,train_id=t1)")
    #expect(selected.contains("manual_threshold_scope=selected_trains(count=2,train_id=t1)"))
    let current = ClassicAnchorEventCSVExporter.resolvedThresholdProvenance(
        "resolved_threshold[tonic.isi_upper_sec]=user_hard_gate;manual_threshold_scope=current_train(train_id=t3)")
    #expect(current.contains("manual_threshold_scope=current_train(train_id=t3)"))
}

@Test
func resolvedThresholdsProjectionHasNoScopeNoteWhenAbsent() {
    // A soft-only / out-of-scope candidate carries no scope note ⇒ the projection has none either.
    let softOnly = ClassicAnchorEventCSVExporter.resolvedThresholdProvenance(
        "resolved_threshold[burst.seed_upper_sec]=user_soft_anchor;pipeline_stage=x")
    #expect(softOnly.contains("resolved_threshold[burst.seed_upper_sec]=user_soft_anchor"))
    #expect(!softOnly.contains("manual_threshold_scope"))
}
