import Foundation

/// P7A/P7B — pure, display-only explanation of an Adaptive-v2 burst-canonicalization decision.
///
/// `parse(decisionPath:)` is UI-LANGUAGE-AGNOSTIC: it reads the diagnostic markers the detector already wrote
/// (`adaptive_v2_canonicalization=boundary_trim(...)` / `=demote_to_possible(verdict_...)`) and returns only the
/// STRUCTURAL classification — `kind`, the raw `verdict` machine token, the trim flags, the ceiling, and a display
/// `tone`. It performs NO detection, NO thresholding, and NO mutation, and stores NO user-facing prose.
///
/// User-facing copy is produced separately by the `shortTitle(language:)`, `plainLanguageSummary(language:)`, and
/// `ceilingLabel(language:)` methods, which hold the Chinese source strings and route them through `STPDLocalization`
/// (zh returns the source; en is translated by the shared dictionary). This keeps the type pure and testable, keeps the
/// machine `verdict` token un-localized, and gives the Inspector / pinned-ISI inspector / hover card one consistent,
/// bilingual source of truth. Unknown or malformed markers degrade gracefully; nothing throws.
public struct AdaptiveV2CanonicalizationExplanation: Equatable, Sendable {

    /// What Adaptive-v2 did to the candidate.
    public enum Kind: String, Sendable, Equatable {
        case none               // no Adaptive-v2 marker on this candidate
        case boundaryTrim       // kept canonical after a boundary decision (a slow edge trimmed, OR — BURST-BOUNDARY-AUTH —
                                // a BCB-vetted compatible-extension boundary KEPT in full when Adaptive-v2 deferred to BCB)
        case strongCoreRescue   // P11A: kept canonical — a strong in-band core overrode a moderate q95 inflation
        case demotedToPossible  // downgraded to possible_burst / review
    }

    /// Display tone, for the UI to map to color/icon. A styling hint, not a severity ranking of the spike train.
    public enum Tone: String, Sendable, Equatable {
        case info       // a positive/neutral outcome (e.g. kept as burst after trimming)
        case review     // downgraded; kept for human review
        case warning    // evidence was insufficient/unavailable
    }

    public let kind: Kind
    // Structural details (present only when the marker carried them) — all UI-language-agnostic:
    public let leftTrimmed: Bool?
    public let rightTrimmed: Bool?
    public let ceilingSec: Double?
    public let verdict: String?      // raw verdict machine token for a demotion (e.g. "tonic_guard_conflict"); never localized
    public let tone: Tone

    /// True when Adaptive-v2 recorded a decision worth showing.
    public var isPresent: Bool { kind != .none }

    public init(
        kind: Kind,
        leftTrimmed: Bool? = nil, rightTrimmed: Bool? = nil, ceilingSec: Double? = nil,
        verdict: String? = nil, tone: Tone
    ) {
        self.kind = kind
        self.leftTrimmed = leftTrimmed
        self.rightTrimmed = rightTrimmed
        self.ceilingSec = ceilingSec
        self.verdict = verdict
        self.tone = tone
    }

    /// The "nothing to show" value.
    public static let absent = AdaptiveV2CanonicalizationExplanation(kind: .none, tone: .info)

    private static let trimKey = "adaptive_v2_canonicalization=boundary_trim("
    private static let strongCoreKey = "adaptive_v2_canonicalization=strong_core_rescue("
    private static let compatibleBoundaryKey = "adaptive_v2_canonicalization=bcb_compatible_boundary_kept("
    private static let demoteKey = "adaptive_v2_canonicalization=demote_to_possible(verdict_"

    // MARK: - parse (structural; language-agnostic)

    /// Parse a candidate `decisionPath`. The KEEP markers (boundary-trim, strong-core rescue) take precedence over
    /// demotion; if no marker is present the result is `.absent`. (A candidate carries at most one of these markers.)
    public static func parse(decisionPath: String) -> AdaptiveV2CanonicalizationExplanation {
        if let trim = parseBoundaryTrim(decisionPath) { return trim }
        if let kept = parseCompatibleBoundaryKept(decisionPath) { return kept }
        if let rescue = parseStrongCoreRescue(decisionPath) { return rescue }
        if let demote = parseDemotion(decisionPath) { return demote }
        return .absent
    }

    private static func parseStrongCoreRescue(_ path: String) -> AdaptiveV2CanonicalizationExplanation? {
        guard path.range(of: strongCoreKey) != nil else { return nil }
        return AdaptiveV2CanonicalizationExplanation(kind: .strongCoreRescue, verdict: nil, tone: .info)
    }

    /// BURST-BOUNDARY-AUTH: Adaptive-v2 deferred to BCB's vetted compatible-extension boundary and kept the burst in FULL.
    /// It is a KEPT (info) boundary outcome, mapped onto the existing `.boundaryTrim` kind with NOTHING trimmed
    /// (`leftTrimmed`/`rightTrimmed == false`) — so no new UI outcome type is introduced.
    private static func parseCompatibleBoundaryKept(_ path: String) -> AdaptiveV2CanonicalizationExplanation? {
        guard path.range(of: compatibleBoundaryKey) != nil else { return nil }
        return AdaptiveV2CanonicalizationExplanation(
            kind: .boundaryTrim, leftTrimmed: false, rightTrimmed: false, verdict: nil, tone: .info)
    }

    private static func parseBoundaryTrim(_ path: String) -> AdaptiveV2CanonicalizationExplanation? {
        guard let range = path.range(of: trimKey) else { return nil }
        // Inner content up to the closing ')' (or the next ';' marker separator, if the ')' is missing — degrade safely).
        let inner = path[range.upperBound...].prefix { $0 != ")" && $0 != ";" }
        var left: Bool?
        var right: Bool?
        var ceiling: Double?
        for raw in inner.split(separator: ",") {
            let part = raw.trimmingCharacters(in: .whitespaces)
            if part.hasPrefix("left:") {
                left = String(part.dropFirst("left:".count)) == "1"
            } else if part.hasPrefix("right:") {
                right = String(part.dropFirst("right:".count)) == "1"
            } else if part.hasPrefix("ceiling_") {
                var value = Substring(part.dropFirst("ceiling_".count))
                if value.hasSuffix("s") { value = value.dropLast() }
                ceiling = Double(value)
            }
        }
        return AdaptiveV2CanonicalizationExplanation(
            kind: .boundaryTrim, leftTrimmed: left, rightTrimmed: right, ceilingSec: ceiling,
            verdict: nil, tone: .info)
    }

    private static func parseDemotion(_ path: String) -> AdaptiveV2CanonicalizationExplanation? {
        guard let range = path.range(of: demoteKey) else { return nil }
        let token = String(path[range.upperBound...].prefix { $0 != ")" && $0 != ";" })
        let tone: Tone
        switch token {
        case "insufficient_evidence", "unavailable_profile_evidence": tone = .warning
        default: tone = .review
        }
        return AdaptiveV2CanonicalizationExplanation(
            kind: .demotedToPossible, verdict: token.isEmpty ? nil : token, tone: tone)
    }

    // MARK: - localized display copy (pure; `language` is data, not a UI dependency)

    /// Chinese source strings. zh returns these verbatim; en comes from `STPDLocalization.exactDictionary`.
    private enum Source {
        // short titles
        static let trimTitle = "保留为爆发（已修整边界）"
        static let strongCoreTitle = "保留为爆发（强核心）"   // P11A
        static let demotePossibleTitle = "降级为可能爆发"
        static let pauseTitle = "保留复核（暂停侧翼）"
        static let tonicTitle = "更像强直而非爆发"
        static let bridgeTitle = "桥接 / 尾部无核心"
        static let insufficientTitle = "证据不足"
        // summaries
        static let trimBase = "Adaptive v2 保留该爆发：已去除一个较慢的边界 ISI，紧密核心仍符合该序列的爆发画像。"
        static let edgeLeft = "去除左边界。"
        static let edgeRight = "去除右边界。"
        static let edgeBoth = "去除两侧边界。"
        static let demotePossible = "Adaptive v2 将该爆发样候选降级为可能爆发 / 复核，因为它与该序列的爆发画像不匹配。"
        static let demoteGeneric = "Adaptive v2 将该爆发样候选降级为可能爆发 / 复核。"
        static let strongCore = "Adaptive v2 保留该爆发：其紧密核心足够强，即使尾部 ISI 略高于爆发上限，整体仍符合标准爆发。"   // P11A
        static let pause = "该密集片段位于明显暂停样间隔之间，因此保留为复核，而不是直接接受为标准爆发。"
        static let tonic = "该区间更像规则强直放电，而不是标准爆发。"
        static let bridge = "该区间只符合桥接 / 尾部范围，缺少足够的爆发核心。"
        static let insufficient = "Adaptive-v2 证据不足；未做出强标准爆发判定。"
        static let ceilingLabel = "爆发准入上限"
    }

    /// A short headline for the Adaptive-v2 decision, localized for `language` ("" when absent).
    public func shortTitle(language: STPDLanguage) -> String {
        let source: String
        switch (kind, verdict) {
        case (.none, _): return ""
        case (.boundaryTrim, _): source = Source.trimTitle
        case (.strongCoreRescue, _): source = Source.strongCoreTitle
        case (.demotedToPossible, "pause_boundary_driven_dense_packet"): source = Source.pauseTitle
        case (.demotedToPossible, "tonic_guard_conflict"): source = Source.tonicTitle
        case (.demotedToPossible, "bridge_without_core"): source = Source.bridgeTitle
        case (.demotedToPossible, "insufficient_evidence"), (.demotedToPossible, "unavailable_profile_evidence"):
            source = Source.insufficientTitle
        case (.demotedToPossible, _): source = Source.demotePossibleTitle
        }
        return STPDLocalization.text(source, language: language)
    }

    /// The full plain-language explanation, localized for `language` ("" when absent).
    public func plainLanguageSummary(language: STPDLanguage) -> String {
        switch (kind, verdict) {
        case (.none, _):
            return ""
        case (.boundaryTrim, _):
            let base = STPDLocalization.text(Source.trimBase, language: language)
            guard let edgeSource = edgeSource else { return base }
            let edge = STPDLocalization.text(edgeSource, language: language)
            // Chinese sentences need no separator; English joins the two sentences with a space.
            return language == .zh ? base + edge : base + " " + edge
        case (.strongCoreRescue, _):
            return STPDLocalization.text(Source.strongCore, language: language)
        case (.demotedToPossible, "pause_boundary_driven_dense_packet"):
            return STPDLocalization.text(Source.pause, language: language)
        case (.demotedToPossible, "tonic_guard_conflict"):
            return STPDLocalization.text(Source.tonic, language: language)
        case (.demotedToPossible, "bridge_without_core"):
            return STPDLocalization.text(Source.bridge, language: language)
        case (.demotedToPossible, "possible_burst_candidate"):
            return STPDLocalization.text(Source.demotePossible, language: language)
        case (.demotedToPossible, "insufficient_evidence"), (.demotedToPossible, "unavailable_profile_evidence"):
            return STPDLocalization.text(Source.insufficient, language: language)
        case (.demotedToPossible, _):
            // Unknown/empty verdict token — generic wording (the raw token stays available in `verdict` / decisionPath).
            return STPDLocalization.text(Source.demoteGeneric, language: language)
        }
    }

    /// A localized "Burst eligibility ceiling: 0.0974 s" label, or nil when no ceiling was recorded.
    public func ceilingLabel(language: STPDLanguage) -> String? {
        guard let ceilingSec else { return nil }
        let label = STPDLocalization.text(Source.ceilingLabel, language: language)
        let value = String(format: "%.4f s", ceilingSec)
        return language == .zh ? "\(label)：\(value)" : "\(label): \(value)"
    }

    private var edgeSource: String? {
        switch (leftTrimmed ?? false, rightTrimmed ?? false) {
        case (true, true): return Source.edgeBoth
        case (true, false): return Source.edgeLeft
        case (false, true): return Source.edgeRight
        case (false, false): return nil
        }
    }
}
