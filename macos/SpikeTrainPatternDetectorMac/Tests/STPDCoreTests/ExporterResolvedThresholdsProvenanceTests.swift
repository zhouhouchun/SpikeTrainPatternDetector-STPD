import Foundation
import Testing
@testable import STPDCore

// Exporter `resolved_thresholds` provenance: `ClassicAnchorEventCSVExporter.resolvedThresholdProvenance` projects the
// manual-threshold tokens the pipeline appends to a candidate's decisionPath (`resolved_threshold[...]`,
// `learned_from_manual_annotations(...)`, `manual_threshold_scope=...`) into the pipe-separated `resolved_thresholds`
// CSV column. decisionPath stays the single source of truth; this is a name-keyed projection, tested directly as a
// pure unit (no pipeline / no CSV rendering).

@Test
func resolvedThresholdProvenanceProjectsManualTokensInOrderJoinedByPipe() {
    let decisionPath =
        "classic_anchor=two_sided; resolved_threshold[burst_seed_upper_sec=0.030]; " +
        "manual_threshold_scope=selected_trains(count=2,train_id=a); event_grammar=pass; " +
        "learned_from_manual_annotations(n=5)"
    #expect(
        ClassicAnchorEventCSVExporter.resolvedThresholdProvenance(decisionPath) ==
            "resolved_threshold[burst_seed_upper_sec=0.030]"
            + "|manual_threshold_scope=selected_trains(count=2,train_id=a)"
            + "|learned_from_manual_annotations(n=5)"
    )
}

@Test
func resolvedThresholdProvenanceTrimsWhitespaceAndKeepsOnlyManualTokens() {
    // Leading spaces are trimmed; non-manual tokens (classic_anchor, event_grammar) are dropped.
    #expect(ClassicAnchorEventCSVExporter.resolvedThresholdProvenance("  resolved_threshold[x=1] ; noise=y")
        == "resolved_threshold[x=1]")
}

@Test
func resolvedThresholdProvenanceIsEmptyForAllAutomaticProfile() {
    #expect(ClassicAnchorEventCSVExporter.resolvedThresholdProvenance("classic_anchor=two_sided;event_grammar=pass") == "")
    #expect(ClassicAnchorEventCSVExporter.resolvedThresholdProvenance("") == "")
}
