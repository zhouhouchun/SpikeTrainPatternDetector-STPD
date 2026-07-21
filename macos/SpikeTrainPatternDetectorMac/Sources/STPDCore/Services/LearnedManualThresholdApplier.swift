import Foundation

// MARK: - Phase 1B: apply a learned proposal onto the flat manual-threshold field state
//
// Pure, testable transform that mirrors the `RasterDocument` flat manual-threshold fields (per family:
// a `ThresholdMode` + millisecond ISI values). It writes a `LearnedThresholdProposal` (Phase 1A) into
// that state as SOFT anchors, while PRESERVING any family the user has already set to Hard (those are
// left untouched and reported as skipped). It never sets Hard mode and never runs the detector — the
// document layer just copies the returned state back into its observable fields.
//
// Phase-1B learnable families: burst (seed/bridge upper), tonic (lower/upper), hf-tonic (floor/upper),
// pause (lower). HFS and all spike counts are not learned in 1A/1B, so they are not touched here.

/// The subset of `RasterDocument`'s flat manual-threshold fields that Phase 1B can learn into. Millisecond
/// ISI values, `0` meaning unset, plus the per-family mode (matches the document's field semantics).
public struct ManualThresholdFieldState: Hashable, Sendable {
    public var burstMode: ThresholdMode
    public var burstSeedMaxISIMs: Double
    public var burstBridgeMaxISIMs: Double
    public var tonicMode: ThresholdMode
    public var tonicMinISIMs: Double
    public var tonicMaxISIMs: Double
    public var hfTonicMode: ThresholdMode
    public var hfTonicMinISIMs: Double
    public var hfTonicMaxISIMs: Double
    public var pauseMode: ThresholdMode
    public var pauseMinISIMs: Double

    public init(
        burstMode: ThresholdMode = .automatic, burstSeedMaxISIMs: Double = 0, burstBridgeMaxISIMs: Double = 0,
        tonicMode: ThresholdMode = .automatic, tonicMinISIMs: Double = 0, tonicMaxISIMs: Double = 0,
        hfTonicMode: ThresholdMode = .automatic, hfTonicMinISIMs: Double = 0, hfTonicMaxISIMs: Double = 0,
        pauseMode: ThresholdMode = .automatic, pauseMinISIMs: Double = 0
    ) {
        self.burstMode = burstMode
        self.burstSeedMaxISIMs = burstSeedMaxISIMs
        self.burstBridgeMaxISIMs = burstBridgeMaxISIMs
        self.tonicMode = tonicMode
        self.tonicMinISIMs = tonicMinISIMs
        self.tonicMaxISIMs = tonicMaxISIMs
        self.hfTonicMode = hfTonicMode
        self.hfTonicMinISIMs = hfTonicMinISIMs
        self.hfTonicMaxISIMs = hfTonicMaxISIMs
        self.pauseMode = pauseMode
        self.pauseMinISIMs = pauseMinISIMs
    }
}

public struct LearnedThresholdApplyResult: Hashable, Sendable {
    /// The new field state to write back (unchanged for skipped/no-value families).
    public let state: ManualThresholdFieldState
    /// Families written as soft anchors ("burst", "tonic", "hf_tonic", "pause").
    public let appliedFamilies: [String]
    /// Families that had a learned value but were left untouched because the user already set them to Hard.
    public let skippedHardFamilies: [String]
    /// Families with no learned value to apply.
    public let noValueFamilies: [String]
    /// Phase 1C: provenance notes for the fields actually applied (one per learned field of an applied
    /// family), e.g. `learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.57)`.
    /// These are the strings that can later be threaded into candidate decisionPath / CSV resolved_thresholds.
    public let appliedProvenanceNotes: [String]

    public init(
        state: ManualThresholdFieldState,
        appliedFamilies: [String],
        skippedHardFamilies: [String],
        noValueFamilies: [String],
        appliedProvenanceNotes: [String] = []
    ) {
        self.state = state
        self.appliedFamilies = appliedFamilies
        self.skippedHardFamilies = skippedHardFamilies
        self.noValueFamilies = noValueFamilies
        self.appliedProvenanceNotes = appliedProvenanceNotes
    }

    public var didApplyAnything: Bool { !appliedFamilies.isEmpty }
}

public enum LearnedManualThresholdApplier {
    /// Write the proposal's learned values into `current` as soft anchors, preserving Hard families.
    public static func apply(
        proposal: LearnedThresholdProposal,
        to current: ManualThresholdFieldState
    ) -> LearnedThresholdApplyResult {
        var next = current
        var applied: [String] = []
        var skippedHard: [String] = []
        var noValue: [String] = []

        func ms(_ t: ManualISIThreshold) -> Double? {
            guard t.isActive, let value = t.valueSec else { return nil }
            return value * 1000
        }

        // Burst: seed upper + bridge upper.
        let burstSeed = ms(proposal.profile.burst.seedUpperISI)
        let burstBridge = ms(proposal.profile.burst.bridgeUpperISI)
        if burstSeed != nil || burstBridge != nil {
            if current.burstMode == .hardGate {
                skippedHard.append("burst")
            } else {
                if let burstSeed { next.burstSeedMaxISIMs = burstSeed }
                if let burstBridge { next.burstBridgeMaxISIMs = burstBridge }
                next.burstMode = .softAnchor
                applied.append("burst")
            }
        } else {
            noValue.append("burst")
        }

        // Tonic: lower + upper.
        let tonicLo = ms(proposal.profile.tonic.isiLower)
        let tonicHi = ms(proposal.profile.tonic.isiUpper)
        if tonicLo != nil || tonicHi != nil {
            if current.tonicMode == .hardGate {
                skippedHard.append("tonic")
            } else {
                if let tonicLo { next.tonicMinISIMs = tonicLo }
                if let tonicHi { next.tonicMaxISIMs = tonicHi }
                next.tonicMode = .softAnchor
                applied.append("tonic")
            }
        } else {
            noValue.append("tonic")
        }

        // HF-tonic: floor + upper.
        let hfLo = ms(proposal.profile.hfTonic.isiFloor)
        let hfHi = ms(proposal.profile.hfTonic.isiUpper)
        if hfLo != nil || hfHi != nil {
            if current.hfTonicMode == .hardGate {
                skippedHard.append("hf_tonic")
            } else {
                if let hfLo { next.hfTonicMinISIMs = hfLo }
                if let hfHi { next.hfTonicMaxISIMs = hfHi }
                next.hfTonicMode = .softAnchor
                applied.append("hf_tonic")
            }
        } else {
            noValue.append("hf_tonic")
        }

        // Pause: lower only.
        if let pauseLo = ms(proposal.profile.pause.isiLower) {
            if current.pauseMode == .hardGate {
                skippedHard.append("pause")
            } else {
                next.pauseMinISIMs = pauseLo
                next.pauseMode = .softAnchor
                applied.append("pause")
            }
        } else {
            noValue.append("pause")
        }

        let appliedSet = Set(applied)
        let appliedProvenanceNotes = proposal.contributions
            .filter { appliedSet.contains($0.family) }
            .map(\.provenanceNote)

        return LearnedThresholdApplyResult(
            state: next, appliedFamilies: applied, skippedHardFamilies: skippedHard,
            noValueFamilies: noValue, appliedProvenanceNotes: appliedProvenanceNotes
        )
    }

    // MARK: - Phase 1C: explainability (current → learned) — pure, no behavior change.

    /// Per learned (proposed) field: the current manual state, the learned value, and how applying would
    /// change it. One row per `proposal.contributions` entry (the proposed fields). Families with no learned
    /// value are not rows here — they are reported as `noValueFamilies` by `apply(...)`.
    public static func explain(
        proposal: LearnedThresholdProposal,
        current: ManualThresholdFieldState
    ) -> [LearnedThresholdFieldExplanation] {
        proposal.contributions.map { contribution in
            let (mode, valueMs) = currentState(family: contribution.family, field: contribution.field, in: current)
            let learnedMs = contribution.valueSec * 1000
            let change: LearnedThresholdFieldExplanation.Change
            if mode == .hardGate {
                change = .skippedHard
            } else if mode == .softAnchor, let valueMs, abs(valueMs - learnedMs) < 1e-6 {
                change = .noChange
            } else if mode == .automatic {
                change = .applied
            } else {
                change = .changed   // soft, different value
            }
            return LearnedThresholdFieldExplanation(
                family: contribution.family,
                field: contribution.field,
                currentMode: mode,
                currentValueMs: (mode == .automatic) ? nil : valueMs,
                learnedMode: .softAnchor,
                learnedValueMs: learnedMs,
                change: change,
                contribution: contribution
            )
        }
    }

    /// Phase 1D: map of resolved-threshold key (`<family>.<field>`) → learned provenance note, for fields whose
    /// CURRENT manual state is an active SOFT anchor equal to the learned value (the learned values currently in
    /// effect). Hard families, manually-changed values, and unlearned fields are excluded — so it can be threaded
    /// into the detector run to tag only the candidates a learned threshold actually shaped. Empty for an
    /// all-automatic proposal or when nothing learned is in effect.
    public static func learnedProvenanceByKey(
        proposal: LearnedThresholdProposal,
        current: ManualThresholdFieldState
    ) -> [String: String] {
        var map: [String: String] = [:]
        for explanation in explain(proposal: proposal, current: current) {
            guard explanation.currentMode == .softAnchor,
                  explanation.change == .noChange,
                  let contribution = explanation.contribution else { continue }
            map[resolvedProvenanceKey(family: explanation.family, field: explanation.field)] = contribution.provenanceNote
        }
        return map
    }

    /// The `<family>.<field>` key used to route a learned provenance note to its candidate family.
    public static func resolvedProvenanceKey(family: String, field: String) -> String {
        "\(family).\(field)"
    }

    private static func currentState(
        family: String, field: String, in s: ManualThresholdFieldState
    ) -> (ThresholdMode, Double?) {
        switch (family, field) {
        case ("burst", "seed_upper_sec"): return (s.burstMode, s.burstSeedMaxISIMs)
        case ("burst", "bridge_upper_sec"): return (s.burstMode, s.burstBridgeMaxISIMs)
        case ("tonic", "isi_lower_sec"): return (s.tonicMode, s.tonicMinISIMs)
        case ("tonic", "isi_upper_sec"): return (s.tonicMode, s.tonicMaxISIMs)
        case ("hf_tonic", "isi_floor_sec"): return (s.hfTonicMode, s.hfTonicMinISIMs)
        case ("hf_tonic", "isi_upper_sec"): return (s.hfTonicMode, s.hfTonicMaxISIMs)
        case ("pause", "isi_lower_sec"): return (s.pauseMode, s.pauseMinISIMs)
        default: return (.automatic, nil)
        }
    }
}

public extension LearnedThresholdContribution {
    /// Auditable provenance for a learned threshold, suitable for later inclusion in a candidate's
    /// decisionPath / the CSV `resolved_thresholds` column. `label` is the calibration source label
    /// (e.g. `burst_family`, `tonic`, `pause`), `n` the labelled-event count, `trains` the contributing
    /// train count, `stat` the percentile used, and `confidence` = n/(n+6).
    var provenanceNote: String {
        String(
            format: "learned_from_manual_annotations(label=%@,n=%d,trains=%d,stat=%@,confidence=%.2f)",
            sourceLabel, annotationCount, trainCount, statistic, confidence
        )
    }

    /// Phase 1F: compact, explicitly-labelled evidence summary for the preview UI, e.g.
    /// `n=8 · trains=3 · confidence=0.57`. The `n=/trains=/confidence=` keys are technical tokens (like the
    /// provenance note), not localized.
    var evidenceSummary: String {
        String(format: "n=%d · trains=%d · confidence=%.2f", annotationCount, trainCount, confidence)
    }
}

public extension LearnedManualThresholdApplier {
    /// Phase 1F: display-only normalization of a machine provenance note — shows `label=burst` instead of the
    /// internal calibration source label `label=burst_family`. The machine note threaded into candidate
    /// decisionPath / the CSV `resolved_thresholds` column is unchanged (only the on-screen preview is normalized).
    static func displayProvenanceNote(_ machineNote: String) -> String {
        machineNote.replacingOccurrences(
            of: "label=\(ManualAnnotationCalibrationSummarizer.familyBurst)",
            with: "label=burst"
        )
    }
}

/// Phase 1C: a compact "current → learned" explanation for one proposed manual-threshold field.
public struct LearnedThresholdFieldExplanation: Hashable, Sendable {
    public enum Change: String, Hashable, Sendable {
        case applied = "applied"        // current Auto → learned Soft (a new value)
        case changed = "changed"        // current Soft → a different learned Soft value
        case noChange = "no_change"     // current already equals the learned value
        case skippedHard = "skipped_hard" // current family is Hard → not overwritten
    }
    public let family: String
    public let field: String
    public let currentMode: ThresholdMode
    public let currentValueMs: Double?       // nil when the current family is automatic (inactive)
    public let learnedMode: ThresholdMode?   // .softAnchor for a learned field
    public let learnedValueMs: Double?
    public let change: Change
    public let contribution: LearnedThresholdContribution?

    public init(
        family: String, field: String, currentMode: ThresholdMode, currentValueMs: Double?,
        learnedMode: ThresholdMode?, learnedValueMs: Double?, change: Change,
        contribution: LearnedThresholdContribution?
    ) {
        self.family = family
        self.field = field
        self.currentMode = currentMode
        self.currentValueMs = currentValueMs
        self.learnedMode = learnedMode
        self.learnedValueMs = learnedValueMs
        self.change = change
        self.contribution = contribution
    }

    /// True when applying would change this field (applied or changed).
    public var isChange: Bool { change == .applied || change == .changed }
}
