import Foundation

// MARK: - Phase 1A: manual-annotation calibration → learned soft ManualThresholdProfile
//
// Pure, deterministic transform from the existing `ManualAnnotationCalibrationSummary` (preview stats over
// the user's manual annotations) into a LEARNED `ManualThresholdProfile`. No detector run, no I/O, no UI,
// and nothing is applied to the detector here — this only PROPOSES a profile plus provenance for an explicit
// later Apply step.
//
// Mapping (positive, usable labels only — see the design audit for the R-parity rationale):
//   burst family (burst + long_burst) -> burst.seedUpperISI = q90,  burst.bridgeUpperISI = q95
//   tonic                              -> tonic.isiLower      = q10,  tonic.isiUpper       = q90
//   high_frequency_tonic               -> hfTonic.isiFloor    = q10,  hfTonic.isiUpper     = q90
//   pause                              -> pause.isiLower       = q10
//       (q10 = R's pause `T_seed`, the soft LOWER seed; as a soft anchor on `pause.isiLower` it can only
//        lower the floor — expand pause coverage toward the user's smallest marked pauses — never cap it.
//        Chosen over the median `T_strong` because `PauseManualThresholds` exposes only the lower seed.)
//
// NOT learned in Phase 1A:
//   - HFS: the calibration exposes ISI percentiles, but `HFSManualThresholds` has no ISI bound; min-duration
//     (a duration) and min-spikes (a count) are not derivable from an ISI-percentile summary -> stays automatic.
//   - All min-spike counts: the summary has no per-event spike-count statistic -> burst/tonic/etc. minSpikes
//     stay automatic ("minSpikes if supported" — it is not supported by the current summary).
//   - not_burst / negative-veto labels: never learned (they remain run-time vetoes only).
//
// All learned values are `softAnchor` by default: expand-only (widen a band / lower a floor), never force a
// candidate. A row only contributes when it is positive AND `isUsableForCalibration` (the per-label ISI-count
// gate); unusable labels produce no threshold. Cross-train evidence is retained as audit metadata. The
// proposal's support score is train-clustered: multiple resolved runs on one train can improve the quantile
// estimate but cannot increase this score beyond one observational train unit. It is not a claim of
// independent neurons, sessions, or biological replication because those identifiers are not yet modeled.

/// One learned threshold field, with the calibration evidence and statistic it came from.
public struct LearnedThresholdContribution: Hashable, Sendable {
    public let family: String        // "burst" | "tonic" | "hf_tonic" | "pause"
    public let field: String         // resolved-threshold key tail, e.g. "seed_upper_sec", "isi_lower_sec"
    public let sourceLabel: String   // calibration row label, e.g. "burst_family", "tonic", "pause"
    public let statistic: String     // "q90" | "q95" | "q10"
    public let valueSec: Double
    public let mode: ThresholdMode
    public let evidenceRunCount: Int
    /// Compatibility spelling. This is a resolved evidence-run count, not a serialized-mark count.
    public var annotationCount: Int { evidenceRunCount }
    public let trainCount: Int
    public let coveredISICount: Int
    /// Train-clustered support score in `[0,1)`: `n / (n + 6)`, where
    /// `n = min(evidenceRunCount, trainCount)`. Multiple runs from one train therefore cannot inflate the
    /// score. This is observational coverage, not independent biological replication.
    public let supportScore: Double
    /// Compatibility spelling for existing callers. This is the train-clustered `supportScore`, not a
    /// frequentist confidence level and not evidence of independent neurons or sessions.
    public var confidence: Double { supportScore }

    public init(
        family: String, field: String, sourceLabel: String, statistic: String, valueSec: Double,
        mode: ThresholdMode, annotationCount: Int, trainCount: Int, coveredISICount: Int, confidence: Double
    ) {
        self.family = family
        self.field = field
        self.sourceLabel = sourceLabel
        self.statistic = statistic
        self.valueSec = valueSec
        self.mode = mode
        self.evidenceRunCount = annotationCount
        self.trainCount = trainCount
        self.coveredISICount = coveredISICount
        self.supportScore = confidence
    }
}

/// A calibration row that was considered but did not yield a learned threshold, with why.
public struct LearnedThresholdSkip: Hashable, Sendable {
    public enum Reason: String, Hashable, Sendable {
        case negativeVetoLabel = "negative_veto_label"     // not_burst etc. — never learned
        case notUsableMinCount = "not_usable_min_count"    // below the per-label minimum evidence count
        case supersededByBurstFamily = "superseded_by_burst_family" // individual burst/long_burst (use the family row)
        case noMappableField = "no_mappable_field"         // HFS / other — no Phase-1A ManualThresholdProfile field
        case missingQuantile = "missing_quantile"          // usable row but the needed percentile was absent
    }
    public let sourceLabel: String
    public let reason: Reason

    public init(sourceLabel: String, reason: Reason) {
        self.sourceLabel = sourceLabel
        self.reason = reason
    }
}

/// The learned proposal: a soft `ManualThresholdProfile` plus the provenance explaining it. Not applied.
public struct LearnedThresholdProposal: Hashable, Sendable {
    public let profile: ManualThresholdProfile
    public let contributions: [LearnedThresholdContribution]
    public let skipped: [LearnedThresholdSkip]
    public let mode: ThresholdMode

    public init(
        profile: ManualThresholdProfile,
        contributions: [LearnedThresholdContribution],
        skipped: [LearnedThresholdSkip],
        mode: ThresholdMode
    ) {
        self.profile = profile
        self.contributions = contributions
        self.skipped = skipped
        self.mode = mode
    }

    /// True when nothing was learned (no usable positive labels) — the profile is the adaptive no-op.
    public var isAllAutomatic: Bool { profile.isAllAutomatic }
}

public enum LearnedManualThresholdBuilder {
    /// Build a learned `ManualThresholdProfile` from a calibration summary. `mode` is the application mode
    /// for the learned ISI bounds; defaults to `.softAnchor`. `.automatic` is coerced to `.softAnchor`
    /// (a "learned automatic" value is meaningless).
    public static func build(
        from summary: ManualAnnotationCalibrationSummary,
        mode: ThresholdMode = .softAnchor
    ) -> LearnedThresholdProposal {
        let learnMode: ThresholdMode = (mode == .automatic) ? .softAnchor : mode

        var burst = BurstManualThresholds()
        var tonic = TonicManualThresholds()
        var hfTonic = HFTonicManualThresholds()
        var pause = PauseManualThresholds()
        var contributions: [LearnedThresholdContribution] = []
        var skipped: [LearnedThresholdSkip] = []

        func isi(_ value: Double) -> ManualISIThreshold { ManualISIThreshold(mode: learnMode, valueSec: value) }
        func trainClusteredSupportScore(evidenceRunCount: Int, trainCount: Int) -> Double {
            let n = min(max(0, evidenceRunCount), max(0, trainCount))
            return n == 0 ? 0 : Double(n) / Double(n + 6)
        }
        func record(
            family: String, field: String, statistic: String, value: Double,
            _ row: ManualAnnotationCalibrationLabelSummary
        ) {
            contributions.append(
                LearnedThresholdContribution(
                    family: family, field: field, sourceLabel: row.label, statistic: statistic, valueSec: value,
                    mode: learnMode, annotationCount: row.evidenceRunCount, trainCount: row.trainCount,
                    coveredISICount: row.coveredISICount,
                    confidence: trainClusteredSupportScore(
                        evidenceRunCount: row.evidenceRunCount,
                        trainCount: row.trainCount
                    )
                )
            )
        }

        for row in summary.rows {
            // Negative / veto labels never learn.
            guard row.isPositive else {
                skipped.append(LearnedThresholdSkip(sourceLabel: row.label, reason: .negativeVetoLabel))
                continue
            }
            // Respect the per-label preview evidence-count gate. Cross-train evidence is reported on the
            // row, but is not a hard gate because a dataset may contain only one train with this physiological
            // pattern. The support score below is capped by train-level observational coverage; biological
            // independence remains unknown until neuron/session identifiers are modeled.
            guard row.isUsableForCalibration else {
                skipped.append(LearnedThresholdSkip(sourceLabel: row.label, reason: .notUsableMinCount))
                continue
            }

            switch row.label {
            case ManualAnnotationCalibrationSummarizer.familyBurst:
                if let q90 = positive(row.q90ISISeconds) {
                    burst.seedUpperISI = isi(q90)
                    record(family: "burst", field: "seed_upper_sec", statistic: "q90", value: q90, row)
                } else {
                    skipped.append(LearnedThresholdSkip(sourceLabel: row.label, reason: .missingQuantile))
                }
                if let q95 = positive(row.q95ISISeconds) {
                    burst.bridgeUpperISI = isi(q95)
                    record(family: "burst", field: "bridge_upper_sec", statistic: "q95", value: q95, row)
                }

            case ManualAnnotationLabel.tonic.rawValue:
                if let q10 = positive(row.q10ISISeconds) {
                    tonic.isiLower = isi(q10)
                    record(family: "tonic", field: "isi_lower_sec", statistic: "q10", value: q10, row)
                } else {
                    skipped.append(LearnedThresholdSkip(sourceLabel: row.label, reason: .missingQuantile))
                }
                if let q90 = positive(row.q90ISISeconds) {
                    tonic.isiUpper = isi(q90)
                    record(family: "tonic", field: "isi_upper_sec", statistic: "q90", value: q90, row)
                }

            case ManualAnnotationLabel.highFrequencyTonic.rawValue:
                if let q10 = positive(row.q10ISISeconds) {
                    hfTonic.isiFloor = isi(q10)
                    record(family: "hf_tonic", field: "isi_floor_sec", statistic: "q10", value: q10, row)
                } else {
                    skipped.append(LearnedThresholdSkip(sourceLabel: row.label, reason: .missingQuantile))
                }
                if let q90 = positive(row.q90ISISeconds) {
                    hfTonic.isiUpper = isi(q90)
                    record(family: "hf_tonic", field: "isi_upper_sec", statistic: "q90", value: q90, row)
                }

            case ManualAnnotationLabel.pause.rawValue:
                if let q10 = positive(row.q10ISISeconds) {
                    pause.isiLower = isi(q10)
                    record(family: "pause", field: "isi_lower_sec", statistic: "q10", value: q10, row)
                } else {
                    skipped.append(LearnedThresholdSkip(sourceLabel: row.label, reason: .missingQuantile))
                }

            case ManualAnnotationLabel.burst.rawValue, ManualAnnotationLabel.longBurst.rawValue:
                // Burst is learned from the combined `burst_family` row; the individual rows are redundant.
                skipped.append(LearnedThresholdSkip(sourceLabel: row.label, reason: .supersededByBurstFamily))

            default:
                // HFS (no ISI field in 1A), `other`, and any future label have no mappable Phase-1A field.
                skipped.append(LearnedThresholdSkip(sourceLabel: row.label, reason: .noMappableField))
            }
        }

        let profile = ManualThresholdProfile(
            burst: burst, hfs: HFSManualThresholds(), hfTonic: hfTonic, tonic: tonic, pause: pause
        )
        return LearnedThresholdProposal(
            profile: profile, contributions: contributions, skipped: skipped, mode: learnMode
        )
    }

    private static func positive(_ value: Double?) -> Double? {
        guard let value, value.isFinite, value > 0 else { return nil }
        return value
    }
}
