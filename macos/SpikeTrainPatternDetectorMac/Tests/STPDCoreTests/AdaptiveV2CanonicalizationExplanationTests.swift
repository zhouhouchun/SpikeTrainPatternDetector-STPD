import Foundation
import STPDCore
import Testing

// P7A/P7B — unit tests for the pure Adaptive-v2 decisionPath explanation parser/formatter. Display-only; no detector
// behavior is exercised. `parse` is asserted to be UI-language-agnostic (structural fields only); the localized copy is
// asserted in BOTH Chinese (source) and English (translated) for boundary-trim left/right/both/neither, each demotion
// verdict, the no-marker case, malformed markers, and the ceiling label.

private func parse(_ path: String) -> AdaptiveV2CanonicalizationExplanation {
    AdaptiveV2CanonicalizationExplanation.parse(decisionPath: path)
}

// MARK: - boundary trim (structural + bilingual)

@Test func explanation_boundaryTrim_leftOnly() {
    let e = parse("event_grammar_pass;adaptive_v2_canonicalization=boundary_trim(left:1,right:0,ceiling_0.0974s)")
    #expect(e.kind == .boundaryTrim)
    #expect(e.isPresent)
    #expect(e.leftTrimmed == true)
    #expect(e.rightTrimmed == false)
    #expect(e.ceilingSec == 0.0974)
    #expect(e.tone == .info)
    #expect(e.verdict == nil)
    // en
    let en = e.plainLanguageSummary(language: .en)
    #expect(en.contains("kept this as a burst"))
    #expect(en.contains("Trimmed left edge."))
    #expect(!en.contains("both"))
    // zh
    let zh = e.plainLanguageSummary(language: .zh)
    #expect(zh.contains("保留该爆发"))
    #expect(zh.contains("去除左边界"))
    #expect(!zh.contains("两侧"))
    // short title
    #expect(e.shortTitle(language: .en) == "Kept as burst (boundary trimmed)")
    #expect(e.shortTitle(language: .zh) == "保留为爆发（已修整边界）")
}

@Test func explanation_boundaryTrim_rightOnly() {
    let e = parse("x;adaptive_v2_canonicalization=boundary_trim(left:0,right:1,ceiling_0.0391s)")
    #expect(e.kind == .boundaryTrim)
    #expect(e.leftTrimmed == false)
    #expect(e.rightTrimmed == true)
    #expect(e.ceilingSec == 0.0391)
    #expect(e.plainLanguageSummary(language: .en).contains("Trimmed right edge."))
    #expect(e.plainLanguageSummary(language: .zh).contains("去除右边界"))
}

@Test func explanation_boundaryTrim_bothEdges() {
    let e = parse("adaptive_v2_canonicalization=boundary_trim(left:1,right:1,ceiling_0.0353s)")
    #expect(e.kind == .boundaryTrim)
    #expect(e.leftTrimmed == true)
    #expect(e.rightTrimmed == true)
    #expect(e.plainLanguageSummary(language: .en).contains("Trimmed both edges."))
    #expect(e.plainLanguageSummary(language: .zh).contains("去除两侧边界"))
}

@Test func explanation_boundaryTrim_neitherEdgeFlagged_hasNoEdgeSuffix() {
    let e = parse("adaptive_v2_canonicalization=boundary_trim(left:0,right:0,ceiling_0.0458s)")
    #expect(e.kind == .boundaryTrim)
    #expect(e.leftTrimmed == false)
    #expect(e.rightTrimmed == false)
    #expect(!e.plainLanguageSummary(language: .en).contains("Trimmed"))
    #expect(e.plainLanguageSummary(language: .en).contains("kept this as a burst"))
    #expect(e.plainLanguageSummary(language: .zh).contains("保留该爆发"))
}

@Test func explanation_ceilingLabel_isBilingualAndFormatted() {
    let e = parse("adaptive_v2_canonicalization=boundary_trim(left:1,right:0,ceiling_0.0974s)")
    #expect(e.ceilingLabel(language: .en) == "Burst eligibility ceiling: 0.0974 s")
    #expect(e.ceilingLabel(language: .zh) == "爆发准入上限：0.0974 s")
    // demotions carry no ceiling
    #expect(parse("adaptive_v2_canonicalization=demote_to_possible(verdict_tonic_guard_conflict)")
        .ceilingLabel(language: .en) == nil)
}

// MARK: - demotion (one per verdict; structural + bilingual)

@Test func explanation_demote_possibleBurst() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_possible_burst_candidate)")
    #expect(e.kind == .demotedToPossible)
    #expect(e.verdict == "possible_burst_candidate")
    #expect(e.tone == .review)
    #expect(e.plainLanguageSummary(language: .en).contains("Possible burst / review"))
    #expect(e.plainLanguageSummary(language: .zh).contains("可能爆发 / 复核"))
}

@Test func explanation_demote_pauseBoundary() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_pause_boundary_driven_dense_packet)")
    #expect(e.verdict == "pause_boundary_driven_dense_packet")
    #expect(e.tone == .review)
    #expect(e.plainLanguageSummary(language: .en).contains("pause-like gaps"))
    #expect(e.plainLanguageSummary(language: .zh).contains("暂停样间隔"))
    #expect(e.shortTitle(language: .en) == "Kept for review (pause-flanked)")
    #expect(e.shortTitle(language: .zh) == "保留复核（暂停侧翼）")
}

@Test func explanation_demote_tonicGuard() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_tonic_guard_conflict)")
    #expect(e.verdict == "tonic_guard_conflict")
    #expect(e.tone == .review)
    #expect(e.plainLanguageSummary(language: .en).contains("tonic-like regular firing"))
    #expect(e.plainLanguageSummary(language: .zh).contains("规则强直放电"))
}

@Test func explanation_demote_bridgeWithoutCore() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_bridge_without_core)")
    #expect(e.verdict == "bridge_without_core")
    #expect(e.tone == .review)
    #expect(e.plainLanguageSummary(language: .en).contains("bridge/tail band"))
    #expect(e.plainLanguageSummary(language: .zh).contains("桥接 / 尾部"))
}

@Test func explanation_demote_insufficientEvidence_isWarning() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_insufficient_evidence)")
    #expect(e.verdict == "insufficient_evidence")
    #expect(e.tone == .warning)
    #expect(e.plainLanguageSummary(language: .en).contains("evidence was insufficient"))
    #expect(e.plainLanguageSummary(language: .zh).contains("证据不足"))
    #expect(e.shortTitle(language: .en) == "Insufficient evidence")
}

@Test func explanation_demote_unavailableProfileEvidence_isWarning() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_unavailable_profile_evidence)")
    #expect(e.verdict == "unavailable_profile_evidence")
    #expect(e.tone == .warning)
    #expect(e.plainLanguageSummary(language: .zh).contains("证据不足"))
}

// MARK: - none / malformed (graceful degradation; bilingual)

@Test func explanation_noAdaptiveV2Marker_isAbsent() {
    let e = parse("event_grammar_two_sided_event_grammar_pass__burst;some_other_marker=foo")
    #expect(e.kind == .none)
    #expect(!e.isPresent)
    #expect(e == .absent)
    #expect(e.plainLanguageSummary(language: .en).isEmpty)
    #expect(e.plainLanguageSummary(language: .zh).isEmpty)
    #expect(e.shortTitle(language: .en).isEmpty)
}

@Test func explanation_emptyDecisionPath_isAbsent() {
    #expect(parse("") == .absent)
}

@Test func explanation_unknownVerdict_degradesToGenericDemotion() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_some_future_reason)")
    #expect(e.kind == .demotedToPossible)
    #expect(e.verdict == "some_future_reason")     // raw machine token preserved verbatim (never localized)
    #expect(e.tone == .review)
    #expect(e.plainLanguageSummary(language: .en).contains("Possible burst / review"))
    #expect(e.plainLanguageSummary(language: .zh).contains("可能爆发 / 复核"))
    #expect(!e.plainLanguageSummary(language: .en).contains("does not match"))   // generic, no reason clause
}

@Test func explanation_malformedTrim_missingFields_stillBoundaryTrimWithNilDetails() {
    let e = parse("adaptive_v2_canonicalization=boundary_trim(garbage_content_here")
    #expect(e.kind == .boundaryTrim)
    #expect(e.leftTrimmed == nil)
    #expect(e.rightTrimmed == nil)
    #expect(e.ceilingSec == nil)
    #expect(e.plainLanguageSummary(language: .en).contains("kept this as a burst"))
    #expect(e.ceilingLabel(language: .en) == nil)
}

@Test func explanation_malformedDemote_emptyVerdict_degradesGracefully() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_)")
    #expect(e.kind == .demotedToPossible)
    #expect(e.verdict == nil)
    #expect(e.tone == .review)
    #expect(e.plainLanguageSummary(language: .zh).contains("可能爆发 / 复核"))
}

@Test func explanation_trimTakesPrecedenceOverDemote_whenBothPresent() {
    let e = parse("adaptive_v2_canonicalization=demote_to_possible(verdict_tonic_guard_conflict);adaptive_v2_canonicalization=boundary_trim(left:1,right:0,ceiling_0.05s)")
    #expect(e.kind == .boundaryTrim)
}

@Test func explanation_trailingMarkerAfterTrim_doesNotLeakIntoCeiling() {
    let e = parse("adaptive_v2_canonicalization=boundary_trim(left:1,right:0,ceiling_0.0974s);selected=true")
    #expect(e.ceilingSec == 0.0974)
    #expect(e.leftTrimmed == true)
}

// MARK: - parse is language-agnostic

@Test func explanation_parseIsStructuralAndLanguageAgnostic() {
    // The SAME parsed value formats into different languages — parse carries no prose, only structure.
    let e = parse("adaptive_v2_canonicalization=boundary_trim(left:1,right:1,ceiling_0.04s)")
    #expect(e.plainLanguageSummary(language: .en) != e.plainLanguageSummary(language: .zh))
    #expect(e.shortTitle(language: .en) != e.shortTitle(language: .zh))
}
