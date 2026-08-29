import Foundation

// MARK: - Phase 1B: apply a learned proposal onto the flat manual-threshold field state
//
// Pure, testable transform that mirrors the `RasterDocument` flat manual-threshold fields (per family:
// a `ThresholdMode` + millisecond ISI values). It writes a `LearnedThresholdProposal` (Phase 1A) into
// that state as SOFT anchors, except for HFS minimum-support count/duration, which are explicit
// conservative lower gates. Any family the user has already set to Hard is preserved and reported as
// skipped. It never runs the detector — the document layer only copies the returned field state.
//
// Learnable families: burst (seed/bridge upper), HFS (minimum independent-support spikes/duration),
// tonic (lower/upper), hf-tonic (floor/upper), and pause (lower).

/// The subset of `RasterDocument`'s flat manual-threshold fields that Phase 1B can learn into. Millisecond
/// ISI values, `0` meaning unset, plus the per-family mode (matches the document's field semantics).
public struct ManualThresholdFieldState: Hashable, Sendable {
    public var burstMode: ThresholdMode
    public var burstSeedMaxISIMs: Double
    public var burstBridgeMaxISIMs: Double
    public var hfsMode: ThresholdMode
    public var hfsMinSpikes: Int
    public var hfsMinDurationMs: Double
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
        hfsMode: ThresholdMode = .automatic, hfsMinSpikes: Int = 0, hfsMinDurationMs: Double = 0,
        tonicMode: ThresholdMode = .automatic, tonicMinISIMs: Double = 0, tonicMaxISIMs: Double = 0,
        hfTonicMode: ThresholdMode = .automatic, hfTonicMinISIMs: Double = 0, hfTonicMaxISIMs: Double = 0,
        pauseMode: ThresholdMode = .automatic, pauseMinISIMs: Double = 0
    ) {
        self.burstMode = burstMode
        self.burstSeedMaxISIMs = burstSeedMaxISIMs
        self.burstBridgeMaxISIMs = burstBridgeMaxISIMs
        self.hfsMode = hfsMode
        self.hfsMinSpikes = hfsMinSpikes
        self.hfsMinDurationMs = hfsMinDurationMs
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
    /// Families written by this application. HFS is written as a confirmed lower support gate;
    /// the other families are written as soft anchors.
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
    /// Write the proposal's learned values into `current`, preserving existing user Hard families.
    /// Non-HFS fields are soft anchors; HFS minimum-support fields are confirmed hard lower gates.
    public static func apply(
        proposal: LearnedThresholdProposal,
        to current: ManualThresholdFieldState,
        selectingFamilies selectedFamilies: Set<String>? = nil
    ) -> LearnedThresholdApplyResult {
        var next = current
        var applied: [String] = []
        var skippedHard: [String] = []
        var noValue: [String] = []

        func ms(_ t: ManualISIThreshold) -> Double? {
            guard t.isActive, let value = t.valueSec else { return nil }
            return value * 1000
        }
        func isSelected(_ family: String) -> Bool {
            selectedFamilies?.contains(family) ?? true
        }

        // Burst: seed upper + bridge upper.
        let burstSeed = ms(proposal.profile.burst.seedUpperISI)
        let burstBridge = ms(proposal.profile.burst.bridgeUpperISI)
        if !isSelected("burst") {
            // An explicit family selection is an application choice, not missing evidence.
        } else if burstSeed != nil || burstBridge != nil {
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

        // HFS: learned state-size evidence is not an ISI band. It becomes a confirmed minimum
        // support gate only after the caller has presented the HFS warning and the user applies it.
        let hfsMinSpikes = proposal.profile.hfs.minSpikes.value
        let hfsMinDuration = ms(proposal.profile.hfs.minDurationSec)
        if !isSelected("hfs") {
            // Leave this family and its provenance untouched.
        } else if hfsMinSpikes != nil || hfsMinDuration != nil {
            if current.hfsMode == .hardGate {
                skippedHard.append("hfs")
            } else {
                if let hfsMinSpikes { next.hfsMinSpikes = hfsMinSpikes }
                if let hfsMinDuration { next.hfsMinDurationMs = hfsMinDuration }
                next.hfsMode = .hardGate
                applied.append("hfs")
            }
        } else {
            noValue.append("hfs")
        }

        // Tonic: lower + upper.
        let tonicLo = ms(proposal.profile.tonic.isiLower)
        let tonicHi = ms(proposal.profile.tonic.isiUpper)
        if !isSelected("tonic") {
            // Leave this family and its provenance untouched.
        } else if tonicLo != nil || tonicHi != nil {
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
        if !isSelected("hf_tonic") {
            // Leave this family and its provenance untouched.
        } else if hfLo != nil || hfHi != nil {
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
        if !isSelected("pause") {
            // Leave this family and its provenance untouched.
        } else if let pauseLo = ms(proposal.profile.pause.isiLower) {
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
            let (mode, currentValue) = currentState(
                family: contribution.family,
                field: contribution.field,
                in: current
            )
            let learnedValue = contribution.value
            let change: LearnedThresholdFieldExplanation.Change
            if mode == contribution.mode,
               valuesEqual(currentValue, learnedValue) {
                change = .noChange
            } else if mode == .hardGate {
                change = .skippedHard
            } else if mode == .automatic {
                change = .applied
            } else {
                change = .changed   // soft, different value
            }
            return LearnedThresholdFieldExplanation(
                family: contribution.family,
                field: contribution.field,
                currentMode: mode,
                currentValue: (mode == .automatic) ? nil : currentValue,
                learnedMode: contribution.mode,
                learnedValue: learnedValue,
                change: change,
                contribution: contribution
            )
        }
    }

    /// Phase 1D: map of resolved-threshold key (`<family>.<field>`) → learned provenance note, for fields whose
    /// CURRENT manual state has the same active mode and value as the learned proposal. This includes
    /// ordinary soft anchors and confirmed HFS hard support gates. User-hard values, manually changed
    /// values, and unlearned fields are excluded, so only thresholds actually in effect are tagged.
    public static func learnedProvenanceByKey(
        proposal: LearnedThresholdProposal,
        current: ManualThresholdFieldState
    ) -> [String: String] {
        var map: [String: String] = [:]
        for explanation in explain(proposal: proposal, current: current) {
            guard explanation.currentMode == explanation.learnedMode,
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
    ) -> (ThresholdMode, LearnedThresholdValue?) {
        switch (family, field) {
        case ("burst", "seed_upper_sec"):
            return (s.burstMode, .seconds(s.burstSeedMaxISIMs / 1_000))
        case ("burst", "bridge_upper_sec"):
            return (s.burstMode, .seconds(s.burstBridgeMaxISIMs / 1_000))
        case ("hfs", "min_spikes"):
            return (s.hfsMode, .spikeCount(s.hfsMinSpikes))
        case ("hfs", "min_duration_sec"):
            return (s.hfsMode, .seconds(s.hfsMinDurationMs / 1_000))
        case ("tonic", "isi_lower_sec"):
            return (s.tonicMode, .seconds(s.tonicMinISIMs / 1_000))
        case ("tonic", "isi_upper_sec"):
            return (s.tonicMode, .seconds(s.tonicMaxISIMs / 1_000))
        case ("hf_tonic", "isi_floor_sec"):
            return (s.hfTonicMode, .seconds(s.hfTonicMinISIMs / 1_000))
        case ("hf_tonic", "isi_upper_sec"):
            return (s.hfTonicMode, .seconds(s.hfTonicMaxISIMs / 1_000))
        case ("pause", "isi_lower_sec"):
            return (s.pauseMode, .seconds(s.pauseMinISIMs / 1_000))
        default: return (.automatic, nil)
        }
    }

    private static func valuesEqual(
        _ lhs: LearnedThresholdValue?,
        _ rhs: LearnedThresholdValue
    ) -> Bool {
        guard let lhs else { return false }
        switch (lhs, rhs) {
        case (.seconds(let left), .seconds(let right)):
            return abs(left - right) < 1e-9
        case (.spikeCount(let left), .spikeCount(let right)):
            return left == right
        default:
            return false
        }
    }
}

public extension LearnedThresholdContribution {
    /// Auditable provenance for a learned threshold, suitable for later inclusion in a candidate's
    /// decisionPath / the CSV `resolved_thresholds` column. `label` is the calibration source label
    /// (e.g. `burst_family`, `tonic`, `pause`), `n` the resolved evidence-run count, `trains` the
    /// contributing-train count, and `stat` the percentile used. The legacy `confidence` key carries the
    /// train-clustered support score `min(n, trains) / (min(n, trains) + 6)`; it is not a frequentist
    /// confidence level or a claim of independent biological replication.
    var provenanceNote: String {
        String(
            format: "learned_from_manual_annotations(label=%@,n=%d,trains=%d,stat=%@,confidence=%.2f)",
            sourceLabel, annotationCount, trainCount, statistic, confidence
        )
    }

    /// Compact evidence summary for the preview UI. This is deliberately labelled `support`, not
    /// `confidence`: it is an observational coverage score, not inferential confidence.
    var evidenceSummary: String {
        String(format: "n=%d · trains=%d · support=%.2f", annotationCount, trainCount, supportScore)
    }
}

public extension LearnedManualThresholdApplier {
    /// Display-only normalization of a machine provenance note. The persisted machine note keeps its
    /// compatibility keys; the UI calls the observational score `support` to avoid implying statistical
    /// confidence.
    static func displayProvenanceNote(_ machineNote: String) -> String {
        machineNote
            .replacingOccurrences(
                of: "label=\(ManualAnnotationCalibrationSummarizer.familyBurst)",
                with: "label=burst"
            )
            .replacingOccurrences(of: "confidence=", with: "support=")
    }
}

/// Phase 1C: a compact "current → learned" explanation for one proposed manual-threshold field.
public struct LearnedThresholdFieldExplanation: Hashable, Sendable {
    public enum Change: String, Hashable, Sendable {
        case applied = "applied"        // current Auto → learned value (a new value)
        case changed = "changed"        // current active value → a different learned value
        case noChange = "no_change"     // current already equals the learned value
        case skippedHard = "skipped_hard" // current family is Hard → not overwritten
    }
    public let family: String
    public let field: String
    public let currentMode: ThresholdMode
    public let currentValue: LearnedThresholdValue?
    public let learnedMode: ThresholdMode?
    public let learnedValue: LearnedThresholdValue?
    public let change: Change
    public let contribution: LearnedThresholdContribution?

    public init(
        family: String, field: String, currentMode: ThresholdMode,
        currentValue: LearnedThresholdValue?, learnedMode: ThresholdMode?,
        learnedValue: LearnedThresholdValue?, change: Change,
        contribution: LearnedThresholdContribution?
    ) {
        self.family = family
        self.field = field
        self.currentMode = currentMode
        self.currentValue = currentValue
        self.learnedMode = learnedMode
        self.learnedValue = learnedValue
        self.change = change
        self.contribution = contribution
    }

    /// True when applying would change this field (applied or changed).
    public var isChange: Bool { change == .applied || change == .changed }

    /// Compatibility accessors for existing ISI-duration UI/tests. Count-valued fields return nil.
    public var currentValueMs: Double? { currentValue?.seconds.map { $0 * 1_000 } }
    public var learnedValueMs: Double? { learnedValue?.seconds.map { $0 * 1_000 } }
}
