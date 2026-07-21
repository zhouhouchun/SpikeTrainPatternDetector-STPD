import Foundation

// MARK: - SEG-PREVIEW-1: read-only boundary sensitivity preview (expansion + contraction)
//
// Pure, deterministic diagnostic. For a selected ISI segment (e.g. a focused tonic candidate's span) it asks:
//   • Expansion — if a neighboring ISI is included, does the span still pass the tonic regularity gates?
//   • Contraction — does the core stay tonic without the edge ISIs, or did the label depend on an edge ISI?
//
// It recomputes CV / CV2 / LV over EXPANDED / CONTRACTED copies of the span (via `STPDStatistics`) and compares
// them to the current tonic gates. It is a pure function of (isiSec, segment, gates) → preview, with NO side
// effects: it never touches detector settings, manual thresholds, learned anchors, detection signatures, or
// candidate labels. It lives in STPDCore and cannot reference the app/store layer.
//
// Scope note: "tonic compatibility" here means the tonic REGULARITY gates (CV/CV2/LV + min-spikes), not the full
// structural state classifier. That is exactly the acceptance criterion CV/LV intuition is about.

/// A read-only snapshot of the tonic regularity acceptance gates a span is evaluated against.
public struct TonicGateThresholds: Hashable, Sendable {
    public var cvMax: Double
    public var cv2Max: Double
    public var lvMax: Double
    /// Spans with fewer spikes than this are reported "too short" rather than failed as non-tonic.
    public var minSpikes: Int

    public init(cvMax: Double, cv2Max: Double, lvMax: Double, minSpikes: Int) {
        self.cvMax = cvMax
        self.cv2Max = cv2Max
        self.lvMax = lvMax
        self.minSpikes = minSpikes
    }
}

public enum SegmentSensitivityVariant: String, Hashable, Sendable, CaseIterable {
    case current
    case expandLeft, expandRight, expandBoth
    case contractLeft, contractRight, contractBoth

    public var isExpansion: Bool { self == .expandLeft || self == .expandRight || self == .expandBoth }
    public var isContraction: Bool { self == .contractLeft || self == .contractRight || self == .contractBoth }
}

/// Tonic-gate outcome for a variant. `tooShort` (min-spikes / <2 ISIs) is distinct from `fail` (gate exceeded).
public enum SegmentTonicVerdict: Hashable, Sendable {
    case pass
    case fail(metric: String, value: Double, threshold: Double)
    case tooShort

    public var passes: Bool { if case .pass = self { return true } else { return false } }
    public var isTooShort: Bool { if case .tooShort = self { return true } else { return false } }
    /// e.g. `CV 0.420 > 0.30` — the binding (most-exceeded) gate when failing.
    public var failureReason: String? {
        if case let .fail(m, v, t) = self { return String(format: "%@ %.3f > %.2f", m, v, t) }
        return nil
    }
    public var failedMetric: String? { if case let .fail(m, _, _) = self { return m } else { return nil } }
}

/// Stability of a variant's tonic outcome relative to the current segment.
public enum SegmentStability: String, Hashable, Sendable {
    case reference        // the current segment itself
    case stable           // same pass/fail outcome as current
    case changed          // pass/fail outcome differs from current
    case notComparable    // this variant or the current segment is too short to compare
}

public struct SegmentSensitivityRow: Hashable, Sendable {
    public let variant: SegmentSensitivityVariant
    public let startISIIndex: Int
    public let endISIIndex: Int
    public let nISI: Int
    public let nSpikes: Int
    public let meanISISec: Double?
    public let cv: Double?
    public let cv2: Double?
    public let lv: Double?
    public let verdict: SegmentTonicVerdict
    /// True when the requested expand/contract could not move because it hit a train edge (range unchanged).
    public let clippedAtBoundary: Bool
    public let stability: SegmentStability

    public init(
        variant: SegmentSensitivityVariant,
        startISIIndex: Int,
        endISIIndex: Int,
        nISI: Int,
        nSpikes: Int,
        meanISISec: Double?,
        cv: Double?,
        cv2: Double?,
        lv: Double?,
        verdict: SegmentTonicVerdict,
        clippedAtBoundary: Bool,
        stability: SegmentStability
    ) {
        self.variant = variant
        self.startISIIndex = startISIIndex
        self.endISIIndex = endISIIndex
        self.nISI = nISI
        self.nSpikes = nSpikes
        self.meanISISec = meanISISec
        self.cv = cv
        self.cv2 = cv2
        self.lv = lv
        self.verdict = verdict
        self.clippedAtBoundary = clippedAtBoundary
        self.stability = stability
    }

    public var passesTonicGates: Bool { verdict.passes }
    public var failureReason: String? { verdict.failureReason }
    /// The resulting ISI range, used as the de-duplication / "informative" key for display filtering.
    public var rangeKey: [Int] { [startISIIndex, endISIIndex] }
}

/// SEG-PREVIEW-4: a one-line interpretation of a boundary-sensitivity preview. Pure category; the UI renders the
/// localized title + sentence.
public enum SegmentSensitivitySummaryCategory: String, Hashable, Sendable {
    /// Current passes; expansions still pass and contractions pass or are too short — a robust tonic core.
    case stableTonicCore
    /// Current passes, but including a neighboring ISI changes/breaks the verdict.
    case boundarySensitive
    /// Current passes, but dropping an edge ISI changes/breaks the verdict — the label depends on edge ISIs.
    case edgeDependent
    /// Current fails, but a contracted core meets the tonic gates.
    case currentFailsButCorePasses
    /// Contractions are mostly too short — core stability cannot be assessed.
    case insufficientCore
    /// Current fails and no contraction rescues it.
    case notTonicCompatible
}

public struct SegmentSensitivityPreview: Hashable, Sendable {
    public let baseStartISIIndex: Int
    public let baseEndISIIndex: Int
    public let gates: TonicGateThresholds
    public let rows: [SegmentSensitivityRow]

    public init(
        baseStartISIIndex: Int, baseEndISIIndex: Int, gates: TonicGateThresholds, rows: [SegmentSensitivityRow]
    ) {
        self.baseStartISIIndex = baseStartISIIndex
        self.baseEndISIIndex = baseEndISIIndex
        self.gates = gates
        self.rows = rows
    }

    public var current: SegmentSensitivityRow? { rows.first { $0.variant == .current } }
    /// Current + the three expansion variants (the "Expansion" UI group).
    public var expansionRows: [SegmentSensitivityRow] {
        rows.filter { $0.variant == .current || $0.variant.isExpansion }
    }
    /// The three contraction variants (the "Core stability" UI group).
    public var contractionRows: [SegmentSensitivityRow] { rows.filter { $0.variant.isContraction } }

    /// SEG-PREVIEW-3: expansion rows worth DISPLAYING — hides clipped (range == Current) and duplicate variants.
    public var displayedExpansionRows: [SegmentSensitivityRow] {
        SegmentSensitivityPreviewer.displayedExpansionRows(rows)
    }
    /// SEG-PREVIEW-3: contraction rows worth DISPLAYING — keeps too-short rows, hides exact duplicate ranges.
    public var displayedContractionRows: [SegmentSensitivityRow] {
        SegmentSensitivityPreviewer.displayedContractionRows(rows)
    }

    /// SEG-PREVIEW-4: one-line interpretation of the (displayed) rows.
    public var summaryCategory: SegmentSensitivitySummaryCategory {
        SegmentSensitivityPreviewer.summary(self)
    }
}

public enum SegmentSensitivityPreviewer {
    /// Build the read-only preview. `isiSec` is the train's per-spike ISI array (index `i` = the ISI ending at
    /// spike `i`; index 0 is `nil`); `segment` is the selected ISI index range (e.g. a candidate's
    /// `startISIIndex...endISIIndex`). Returns `nil` only when the segment does not overlap any valid ISI.
    public static func preview(
        isiSec: [Double?],
        segment: ClosedRange<Int>,
        gates: TonicGateThresholds
    ) -> SegmentSensitivityPreview? {
        // Valid ISI index domain = indices holding a finite, positive value (index 0 is nil by construction).
        let validIndices = isiSec.indices.filter { i in
            if let v = isiSec[i], v.isFinite, v > 0 { return true }
            return false
        }
        guard let domainLower = validIndices.first, let domainUpper = validIndices.last else { return nil }

        let baseStart = max(segment.lowerBound, domainLower)
        let baseEnd = min(segment.upperBound, domainUpper)
        guard baseStart <= baseEnd else { return nil }

        // Current first — it is the stability reference for every other variant.
        let currentRow = makeRow(
            .current, requestedStart: baseStart, requestedEnd: baseEnd,
            domainLower: domainLower, domainUpper: domainUpper, isiSec: isiSec, gates: gates, reference: nil
        )

        var rows = [currentRow]
        let plan: [(SegmentSensitivityVariant, Int, Int)] = [
            (.expandLeft, baseStart - 1, baseEnd),
            (.expandRight, baseStart, baseEnd + 1),
            (.expandBoth, baseStart - 1, baseEnd + 1),
            (.contractLeft, baseStart + 1, baseEnd),
            (.contractRight, baseStart, baseEnd - 1),
            (.contractBoth, baseStart + 1, baseEnd - 1),
        ]
        for (variant, reqStart, reqEnd) in plan {
            rows.append(makeRow(
                variant, requestedStart: reqStart, requestedEnd: reqEnd,
                domainLower: domainLower, domainUpper: domainUpper, isiSec: isiSec, gates: gates, reference: currentRow
            ))
        }

        return SegmentSensitivityPreview(
            baseStartISIIndex: baseStart, baseEndISIIndex: baseEnd, gates: gates, rows: rows
        )
    }

    // MARK: - Internals

    private static func makeRow(
        _ variant: SegmentSensitivityVariant,
        requestedStart: Int,
        requestedEnd: Int,
        domainLower: Int,
        domainUpper: Int,
        isiSec: [Double?],
        gates: TonicGateThresholds,
        reference: SegmentSensitivityRow?
    ) -> SegmentSensitivityRow {
        // Clip the requested range to the valid ISI domain (safe at train boundaries).
        let start = max(requestedStart, domainLower)
        let end = min(requestedEnd, domainUpper)
        let clipped = (start != requestedStart) || (end != requestedEnd)

        // Contraction can collapse the span (start > end) → a too-short, non-comparable row.
        guard start <= end else {
            return SegmentSensitivityRow(
                variant: variant, startISIIndex: min(start, end), endISIIndex: max(start, end),
                nISI: 0, nSpikes: 0, meanISISec: nil, cv: nil, cv2: nil, lv: nil,
                verdict: .tooShort, clippedAtBoundary: clipped, stability: .notComparable
            )
        }

        let values = (start...end).compactMap { i -> Double? in
            guard i >= 0, i < isiSec.count, let v = isiSec[i], v.isFinite, v > 0 else { return nil }
            return v
        }
        let nISI = values.count
        let nSpikes = nISI > 0 ? nISI + 1 : 0
        let mean = nISI > 0 ? values.reduce(0, +) / Double(nISI) : nil
        let cv = STPDStatistics.coefficientOfVariation(values)
        let cv2 = STPDStatistics.coefficientOfVariation2(values)
        let lv = STPDStatistics.localVariation(values)
        let verdict = evaluate(values: values, nSpikes: nSpikes, cv: cv, cv2: cv2, lv: lv, gates: gates)

        return SegmentSensitivityRow(
            variant: variant, startISIIndex: start, endISIIndex: end,
            nISI: nISI, nSpikes: nSpikes, meanISISec: mean, cv: cv, cv2: cv2, lv: lv,
            verdict: verdict, clippedAtBoundary: clipped,
            stability: stability(of: verdict, reference: reference)
        )
    }

    private static func evaluate(
        values: [Double], nSpikes: Int, cv: Double?, cv2: Double?, lv: Double?, gates: TonicGateThresholds
    ) -> SegmentTonicVerdict {
        // Too short to assess as tonic (min-spikes gate, or not enough ISIs to compute pairwise metrics).
        guard values.count >= 2, nSpikes >= gates.minSpikes, let cv, let cv2, let lv else { return .tooShort }

        // Report the most-exceeded gate as the binding failure reason.
        var failures: [(metric: String, value: Double, threshold: Double, ratio: Double)] = []
        if cv > gates.cvMax { failures.append(("CV", cv, gates.cvMax, gates.cvMax > 0 ? cv / gates.cvMax : .infinity)) }
        if cv2 > gates.cv2Max { failures.append(("CV2", cv2, gates.cv2Max, gates.cv2Max > 0 ? cv2 / gates.cv2Max : .infinity)) }
        if lv > gates.lvMax { failures.append(("LV", lv, gates.lvMax, gates.lvMax > 0 ? lv / gates.lvMax : .infinity)) }
        if let worst = failures.max(by: { $0.ratio < $1.ratio }) {
            return .fail(metric: worst.metric, value: worst.value, threshold: worst.threshold)
        }
        return .pass
    }

    private static func stability(of verdict: SegmentTonicVerdict, reference: SegmentSensitivityRow?) -> SegmentStability {
        guard let reference else { return .reference }   // nil reference == this is the current row
        if reference.verdict.isTooShort || verdict.isTooShort { return .notComparable }
        return verdict.passes == reference.passesTonicGates ? .stable : .changed
    }

    // MARK: - SEG-PREVIEW-2: surface the preview from a pinned ISI

    /// The selected tonic candidate (if any) whose ISI span covers `isiIndex`. Used to surface the boundary
    /// sensitivity preview from a pinned ISI: the covering tonic candidate's FULL span is the segment to evaluate,
    /// not the single pinned ISI. `isiIndex` and the candidate ISI indices share one convention (the ISI ending at
    /// that spike). Pure lookup — no side effects.
    public static func coveringSelectedTonicCandidate(
        isiIndex: Int, candidates: [ClassicAnchorCandidate]
    ) -> ClassicAnchorCandidate? {
        candidates.first {
            $0.finalLabel == .tonic && $0.selectedForAuto
                && $0.startISIIndex <= isiIndex && isiIndex <= $0.endISIIndex
        }
    }

    // MARK: - SEG-PREVIEW-3: display filtering (hide uninformative / duplicate variants)

    /// Expansion rows worth displaying. `current` is always kept; an expansion variant is shown only if its resulting
    /// ISI range is NEW — differing from `current` AND from every already-kept expansion row. This hides variants
    /// that are clipped at a train boundary (range == current) and duplicates (e.g. both-side expansion that, with the
    /// left side clipped, equals right-side expansion). Pure; preserves the input order (current, left, right, both).
    public static func displayedExpansionRows(_ rows: [SegmentSensitivityRow]) -> [SegmentSensitivityRow] {
        var kept: [SegmentSensitivityRow] = []
        var seen = Set<[Int]>()
        if let current = rows.first(where: { $0.variant == .current }) {
            kept.append(current)
            seen.insert(current.rangeKey)
        }
        for row in rows where row.variant.isExpansion {
            guard !seen.contains(row.rangeKey) else { continue }
            seen.insert(row.rangeKey)
            kept.append(row)
        }
        return kept
    }

    /// Contraction rows worth displaying. Too-short rows are KEPT (they explain core-stability limits), but exact
    /// duplicate resulting ranges are dropped (keeping the first). Pure; preserves input order (left, right, both).
    public static func displayedContractionRows(_ rows: [SegmentSensitivityRow]) -> [SegmentSensitivityRow] {
        var kept: [SegmentSensitivityRow] = []
        var seen = Set<[Int]>()
        for row in rows where row.variant.isContraction {
            guard !seen.contains(row.rangeKey) else { continue }
            seen.insert(row.rangeKey)
            kept.append(row)
        }
        return kept
    }

    // MARK: - SEG-PREVIEW-4: one-line summary verdict

    /// Classify a preview into a single summary category, using the DISPLAYED (filtered) rows so clipped/duplicate
    /// variants don't skew the verdict. Pure. Priority: when Current passes, a broken expansion (boundary) outranks
    /// a broken contraction (edge), which outranks an untestable (mostly-too-short) core, else a stable core; when
    /// Current fails, a rescuing contraction outranks "not tonic-compatible".
    public static func summary(_ preview: SegmentSensitivityPreview) -> SegmentSensitivitySummaryCategory {
        guard let current = preview.current else { return .notTonicCompatible }
        let expansions = preview.displayedExpansionRows.filter { $0.variant.isExpansion }
        let contractions = preview.displayedContractionRows

        // A variant "breaks" relative to Current when its pass/fail outcome flips (stability == .changed). Too-short
        // contractions are .notComparable, so they never count as a break.
        let anyExpansionBreaks = expansions.contains { $0.stability == .changed }
        let anyContractionBreaks = contractions.contains { $0.stability == .changed }
        let tooShortCount = contractions.filter { $0.verdict.isTooShort }.count
        let contractionsMostlyTooShort = !contractions.isEmpty && tooShortCount * 2 > contractions.count

        switch current.verdict {
        case .pass:
            if anyExpansionBreaks { return .boundarySensitive }
            if anyContractionBreaks { return .edgeDependent }
            if contractionsMostlyTooShort { return .insufficientCore }
            return .stableTonicCore
        case .fail:
            return contractions.contains { $0.verdict.passes } ? .currentFailsButCorePasses : .notTonicCompatible
        case .tooShort:
            // The selected segment itself is below the tonic floor — core stability cannot be assessed.
            return .insufficientCore
        }
    }
}
