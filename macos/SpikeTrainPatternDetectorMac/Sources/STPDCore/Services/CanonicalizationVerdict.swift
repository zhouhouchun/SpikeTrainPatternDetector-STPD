import Foundation

// MARK: - P2: audit-only canonicality verdicts (built on the P1 evidence boundary)
//
// These are DIAGNOSTIC HINTS, not behavior. The builder is a pure function of P1 evidence + per-family reference
// distributions; NOTHING in detection consumes it. It changes no final label, arbitration outcome, export, UI,
// threshold, learned-threshold, or manual-annotation behavior. No candidate is demoted; no arbitration is
// rebalanced. (Adaptive-v2 enforcement is a later phase.)
//
// Semantics (enforced below):
//   • No fixed-ms rules — every interval test is profile-relative (candidate vs the train's resolved reference
//     distribution). The only scalar settings are SCALE-FREE: coverage fractions, regularity (CV/CV2/LV), and the
//     normalized eventness contrasts.
//   • Missing profile evidence ⇒ unavailable / inconclusive, NEVER incompatibility.
//   • A soft anchor only colors compatibility evidence; it is never a semantic override.
//   • A hard numeric gate is an eligibility/bound signal, not a semantic label.
//   • A manual semantic label (if explicitly supplied) is surfaced as a separate override hint — it does not
//     change the computed verdict here, and changes no actual label.
//   • Regularity ALONE never demotes burst evidence: a tonic-guard hint requires regularity + tonic/HF
//     compatibility + weak burst core/bridge evidence together.
//   • Bridge coverage without sufficient core ⇒ bridge_without_core / possible, never canonical.

public enum CanonicalizationVerdict: String, Hashable, Sendable {
    case canonicalCandidate = "canonical_candidate"
    /// P11A: kept canonical by the strong-core rescue — a strong in-band burst core overrides a MODERATE q95 inflation
    /// (the candidate median is still within the eligibility ceiling). A distinct verdict from `canonicalCandidate` so the
    /// decision path / explanation can show WHY it stayed canonical, and so existing `!= .canonicalCandidate` pins hold.
    case strongCoreRescue = "strong_core_rescue"
    case possibleBurstCandidate = "possible_burst_candidate"
    case tonicGuardConflict = "tonic_guard_conflict"
    case pauseBoundaryDrivenDensePacket = "pause_boundary_driven_dense_packet"
    case bridgeWithoutCore = "bridge_without_core"
    case insufficientEvidence = "insufficient_evidence"
    case unavailableProfileEvidence = "unavailable_profile_evidence"
}

public enum CanonicalizationVerdictReason: String, Hashable, Sendable {
    case sufficientBurstCore = "sufficient_burst_core"
    case weakBurstCore = "weak_burst_core"
    case bridgeCoverageWithoutCore = "bridge_coverage_without_core"
    case burstQuantilesWithinReference = "burst_quantiles_within_reference"
    case burstQuantilesOutsideReference = "burst_quantiles_outside_reference"
    case strongTonicCompatibility = "strong_tonic_compatibility"
    case regularityPass = "regularity_pass"
    case regularityInconclusive = "regularity_inconclusive"
    case pauseBoundaryDrivenContrast = "pause_boundary_driven_contrast"
    case burstReferenceUnavailable = "burst_reference_unavailable"
    case tonicReferenceUnavailable = "tonic_reference_unavailable"
    case candidateQuantilesUnavailable = "candidate_quantiles_unavailable"
    case manualSemanticOverridePresent = "manual_semantic_override_present"
    case softAnchorInvolved = "soft_anchor_involved"
    case hardNumericGateInvolved = "hard_numeric_gate_involved"
    case familyNotAuditedInP2 = "family_not_audited_in_p2"
    case strongBurstCoreRescued = "strong_burst_core_rescued"   // P11A
}

/// Scale-free audit thresholds (coverage fractions, regularity, normalized contrasts) — NO fixed-ms values.
public struct CanonicalizationVerdictSettings: Hashable, Sendable {
    public var minBurstCoreCoverage: Double
    public var minBurstCoreISICount: Int
    public var minTonicCoverage: Double
    public var tonicCVMax: Double
    public var tonicCV2Max: Double
    public var tonicLVMax: Double
    public var pauseBoundaryEdgeMin: Double
    public var pauseBoundaryContextWeakMax: Double
    /// P4: scale-relative tolerance for quantile-vs-reference compatibility. A candidate quantile counts as "within" a
    /// reference quantile if `candidate ≤ reference × (1 + tolerance)`. This is a FRACTION of the reference quantile
    /// (not a fixed ms), so it scales per train; it absorbs floating-point accumulation epsilon and minor packet-to-
    /// packet variation while staying far below the multiple-× margin that distinguishes a true non-canonical packet.
    public var quantileCompatibilityRelativeTolerance: Double
    /// P11A: minimum exact in-band burst-CORE ISI count (`burstCoreISICount`) for the strong-core rescue. A candidate with
    /// this many tight-core ISIs is treated as dispositive burst evidence and kept canonical even when its q95 is MODERATELY
    /// above the eligibility ceiling — provided the candidate MEDIAN (q50) sits comfortably WITHIN the band (see below).
    /// Scale-free count, not an ISI window; the distance test is profile-relative (vs the resolved ceiling).
    public var minStrongBurstCoreISICount: Int
    /// P11A: the candidate MEDIAN must be ≤ `eligibilityCeiling × strongCoreMedianCeilingFraction` to qualify for the
    /// strong-core rescue. A fraction BELOW 1 means the median must sit comfortably inside the band (a genuine fast core),
    /// NOT at its upper edge: a regular tonic-rate packet whose ISIs ARE the band (median == ceiling) is excluded, while a
    /// real burst (median well below the ceiling) is rescued. Scale-free (a fraction of the train's resolved ceiling).
    public var strongCoreMedianCeilingFraction: Double

    public init(
        minBurstCoreCoverage: Double = 0.5,
        minBurstCoreISICount: Int = 2,
        minTonicCoverage: Double = 0.6,
        tonicCVMax: Double = 0.30,
        tonicCV2Max: Double = 0.30,
        tonicLVMax: Double = 0.35,
        pauseBoundaryEdgeMin: Double = 0.60,
        pauseBoundaryContextWeakMax: Double = 0.45,
        quantileCompatibilityRelativeTolerance: Double = 0.05,
        minStrongBurstCoreISICount: Int = 3,
        strongCoreMedianCeilingFraction: Double = 0.95
    ) {
        self.minBurstCoreCoverage = minBurstCoreCoverage
        self.minBurstCoreISICount = minBurstCoreISICount
        self.minTonicCoverage = minTonicCoverage
        self.tonicCVMax = tonicCVMax
        self.tonicCV2Max = tonicCV2Max
        self.tonicLVMax = tonicLVMax
        self.pauseBoundaryEdgeMin = pauseBoundaryEdgeMin
        self.pauseBoundaryContextWeakMax = pauseBoundaryContextWeakMax
        self.quantileCompatibilityRelativeTolerance = quantileCompatibilityRelativeTolerance
        self.minStrongBurstCoreISICount = minStrongBurstCoreISICount
        self.strongCoreMedianCeilingFraction = strongCoreMedianCeilingFraction
    }
}

public struct CanonicalizationVerdictResult: Hashable, Sendable {
    public let candidateID: String
    public let proposedFamily: IntervalFamily?
    public let verdict: CanonicalizationVerdict
    public let reasons: [CanonicalizationVerdictReason]
    /// Separate override SIGNAL (a manual semantic burst label was supplied). It does NOT change `verdict` here,
    /// and changes no actual label — it is for later layers/UI to weigh.
    public let manualSemanticOverride: Bool
    public let softAnchorInvolved: Bool
    public let hardNumericGateInvolved: Bool
    public let burstCompatibility: IntervalCompatibility?
    public let tonicCompatibility: IntervalCompatibility?
    public let hfTonicCompatibility: IntervalCompatibility?

    public init(
        candidateID: String, proposedFamily: IntervalFamily?, verdict: CanonicalizationVerdict,
        reasons: [CanonicalizationVerdictReason], manualSemanticOverride: Bool,
        softAnchorInvolved: Bool, hardNumericGateInvolved: Bool,
        burstCompatibility: IntervalCompatibility?, tonicCompatibility: IntervalCompatibility?,
        hfTonicCompatibility: IntervalCompatibility?
    ) {
        self.candidateID = candidateID
        self.proposedFamily = proposedFamily
        self.verdict = verdict
        self.reasons = reasons
        self.manualSemanticOverride = manualSemanticOverride
        self.softAnchorInvolved = softAnchorInvolved
        self.hardNumericGateInvolved = hardNumericGateInvolved
        self.burstCompatibility = burstCompatibility
        self.tonicCompatibility = tonicCompatibility
        self.hfTonicCompatibility = hfTonicCompatibility
    }
}

public enum CanonicalizationVerdictBuilder {
    /// Compute an audit-only verdict for one candidate from its P1 evidence and the train's per-family reference
    /// distributions (any of which may be unavailable). Pure; no side effects; never consulted by detection.
    public static func verdict(
        evidence: CandidateIntervalEvidence,
        burstReference: ResolvedFamilyDistributionSummary?,
        tonicReference: ResolvedFamilyDistributionSummary?,
        hfTonicReference: ResolvedFamilyDistributionSummary?,
        settings: CanonicalizationVerdictSettings = CanonicalizationVerdictSettings(),
        // P6B-1: the train-specific burst ELIGIBILITY ceiling (resolved burst bridgeUpper, else burst upper). When
        // provided, burst q-compatibility compares the candidate quantiles against this ceiling (× tolerance) instead
        // of the much tighter in-band reference percentile — the P6A over-demotion fix. Nil ⇒ legacy reference path.
        burstEligibilityCeilingSec: Double? = nil
    ) -> CanonicalizationVerdictResult {
        let burstCompat = burstReference.map {
            IntervalCompatibility.compare(candidate: evidence, reference: $0, coverage: evidence.burstBridgeCoverage)
        }
        let tonicCompat = tonicReference.map {
            IntervalCompatibility.compare(candidate: evidence, reference: $0, coverage: evidence.tonicCoverage)
        }
        let hfCompat = hfTonicReference.map {
            IntervalCompatibility.compare(candidate: evidence, reference: $0, coverage: evidence.hfTonicCoverage)
        }

        var reasons: [CanonicalizationVerdictReason] = []
        let manualOverride = evidence.manualSemanticLabelInvolved
        if manualOverride { reasons.append(.manualSemanticOverridePresent) }
        if evidence.softAnchorInvolved { reasons.append(.softAnchorInvolved) }
        if evidence.hardNumericGateInvolved { reasons.append(.hardNumericGateInvolved) }

        func result(_ verdict: CanonicalizationVerdict) -> CanonicalizationVerdictResult {
            CanonicalizationVerdictResult(
                candidateID: evidence.candidateID, proposedFamily: evidence.proposedFamily, verdict: verdict,
                reasons: reasons, manualSemanticOverride: manualOverride,
                softAnchorInvolved: evidence.softAnchorInvolved, hardNumericGateInvolved: evidence.hardNumericGateInvolved,
                burstCompatibility: burstCompat, tonicCompatibility: tonicCompat, hfTonicCompatibility: hfCompat
            )
        }

        // Candidate-side evidence missing entirely.
        guard evidence.isiCount >= 2, evidence.q90Sec != nil else {
            reasons.append(.candidateQuantilesUnavailable)
            return result(.insufficientEvidence)
        }

        // Shared signals (all scale-free / profile-relative).
        let regularity: Bool? = {
            guard let cv = evidence.cv, let cv2 = evidence.cv2, let lv = evidence.lv else { return nil }
            return cv <= settings.tonicCVMax && cv2 <= settings.tonicCV2Max && lv <= settings.tonicLVMax
        }()
        if regularity == true { reasons.append(.regularityPass) } else if regularity == nil { reasons.append(.regularityInconclusive) }

        // P6B-1: burst SUPPORT sufficiency uses the whole burst BAND (core + bridge-extension), not the tight core
        // alone — a legitimate burst whose ISIs span core+bridge is supported. Exact P2.5 counts are preserved.
        // `bridgeWithoutCore` is still rejected: bridge-extension ISIs with NO tight-core ISI cannot be canonical.
        let coreCount = evidence.burstCoreISICount ?? 0
        let bridgeExtCount = evidence.burstBridgeExtensionISICount ?? 0
        let burstBandCount = coreCount + bridgeExtCount
        // Fraction of candidate ISIs inside the full burst band [lower, bridgeUpper] (P1 `burstBridgeCoverage`).
        let burstBandCoverage = evidence.burstBridgeCoverage
        let hasTightCore = coreCount >= 1
        let burstSupportSufficient: Bool? = {
            guard evidence.burstCoreISICount != nil, let bandCov = burstBandCoverage else { return nil }
            return hasTightCore && burstBandCount >= settings.minBurstCoreISICount && bandCov >= settings.minBurstCoreCoverage
        }()
        if burstSupportSufficient == true { reasons.append(.sufficientBurstCore) } else { reasons.append(.weakBurstCore) }
        let weakSupport = (burstSupportSufficient != true)
        let bridgeOnly = (coreCount == 0 && bridgeExtCount > 0)   // bridge extension without any tight core

        // P6B-1: scale-relative quantile compatibility against the burst ELIGIBILITY CEILING (resolved bridgeUpper,
        // else burst upper) when available — the P6A fix (the in-band reference percentile was far too tight). The
        // strict `IntervalCompatibility.withinReference` margins remain as the audit fact (diagnostic detail preserved);
        // only the DECISION uses the eligibility-ceiling form. Falls back to the reference when no ceiling is supplied.
        func withinTolerant(_ candidate: Double?, _ bound: Double?) -> Bool? {
            guard let candidate, let bound, candidate.isFinite, bound.isFinite, bound > 0 else { return nil }
            return candidate <= bound * (1 + settings.quantileCompatibilityRelativeTolerance)
        }
        let burstQCompatible: Bool? = {
            if let ceiling = burstEligibilityCeilingSec, ceiling.isFinite, ceiling > 0 {
                guard let q90Within = withinTolerant(evidence.q90Sec, ceiling),
                      let q95Within = withinTolerant(evidence.q95Sec, ceiling) else { return nil }
                return q90Within && q95Within
            }
            guard let burstReference, burstReference.isAvailable,
                  let q90Within = withinTolerant(evidence.q90Sec, burstReference.q90Sec),
                  let q95Within = withinTolerant(evidence.q95Sec, burstReference.q95Sec) else { return nil }
            return q90Within && q95Within
        }()
        if burstQCompatible == true { reasons.append(.burstQuantilesWithinReference) }
        else if burstQCompatible == false { reasons.append(.burstQuantilesOutsideReference) }
        else { reasons.append(.burstReferenceUnavailable) }

        let tonicCoverage = max(evidence.tonicCoverage ?? 0, evidence.hfTonicCoverage ?? 0)
        let tonicWithin =
            (tonicCompat?.referenceAvailable == true && tonicCompat?.q90Margin.withinReference == true)
            || (hfCompat?.referenceAvailable == true && hfCompat?.q90Margin.withinReference == true)
        // #7: weak burst core + regularity + tonic/HF compatibility together (regularity alone is insufficient).
        let tonicStrong = (regularity == true) && tonicWithin && tonicCoverage >= settings.minTonicCoverage
        if tonicStrong { reasons.append(.strongTonicCompatibility) }
        if tonicCompat?.referenceAvailable != true && hfCompat?.referenceAvailable != true {
            reasons.append(.tonicReferenceUnavailable)
        }

        let pauseDriven: Bool = {
            guard let edge = evidence.edgeContrast, edge >= settings.pauseBoundaryEdgeMin else { return false }
            if let ctx = evidence.contextContrast { return ctx <= settings.pauseBoundaryContextWeakMax }
            return true   // strong immediate-flank contrast with no active-background context ⇒ flank/pause-driven
        }()
        if pauseDriven { reasons.append(.pauseBoundaryDrivenContrast) }

        switch evidence.proposedFamily {
        case .burst:
            if burstSupportSufficient == true && burstQCompatible == true && !tonicStrong {
                return result(.canonicalCandidate)
            }
            if bridgeOnly { return result(.bridgeWithoutCore) }   // bridge extension with no tight core
            if weakSupport && tonicStrong { return result(.tonicGuardConflict) }
            if weakSupport && pauseDriven { return result(.pauseBoundaryDrivenDensePacket) }
            // P11A STRONG-CORE RESCUE: a candidate with a STRONG tight burst core stays canonical even when q95 is
            // MODERATELY above the eligibility ceiling. "Moderately above" is operationalized profile-relatively: the
            // candidate MEDIAN (q50) sits COMFORTABLY WITHIN the band (≤ ceiling × strongCoreMedianCeilingFraction, a
            // fraction below 1) — a genuine fast core, so only the upper tail spills over. This is the load-bearing
            // distinction from a regular tonic-rate packet whose ISIs ARE the band (median == ceiling): that packet is a
            // false burst and is NOT rescued. ENOUGH CORE EVIDENCE = a strong EXACT in-band core (`burstCoreISICount` ≥
            // minStrongBurstCoreISICount) WITH the median comfortably in-band. Requires an available ceiling + core
            // evidence.
            //
            // The intended demotions are NOT touched, by ORDERING: `bridge_without_core` (core 0) and the WEAK-support
            // `tonic_guard_conflict` / `pause_boundary_driven_dense_packet` verdicts are already returned above, so they
            // never reach here. No `!pauseDriven` guard: a real STN burst is an isolated event flanked by quiet (so it
            // reads `pauseDriven`) yet with a deep fast core it is the genuine-burst case — the median-depth test, not the
            // pause flag, is what separates it from a tonic-rate pause-flanked packet. `!tonicStrong` IS kept — a burst
            // better explained as regular tonic is not rescued. Profile-relative, evidence-based, no fixed-ms window.
            if let q50 = evidence.q50Sec, q50.isFinite,
               evidence.burstCoreISICount != nil,
               coreCount >= settings.minStrongBurstCoreISICount,
               !bridgeOnly, !tonicStrong,
               let ceiling = burstEligibilityCeilingSec, ceiling.isFinite, ceiling > 0,
               q50 <= ceiling * settings.strongCoreMedianCeilingFraction {
                reasons.append(.strongBurstCoreRescued)
                return result(.strongCoreRescue)
            }
            // Has burst-band support but is not eligibility-ceiling compatible (e.g. a slow edge ISI inflates q95) —
            // possible burst pending P6B-2 boundary trimming. Also covers the tonic-better / pause-driven sufficient case.
            return result(.possibleBurstCandidate)

        case .tonic, .hfTonic:
            let isHF = evidence.proposedFamily == .hfTonic
            let ref = isHF ? hfTonicReference : tonicReference
            let compat = isHF ? hfCompat : tonicCompat
            guard let ref, ref.isAvailable, compat?.referenceAvailable == true else {
                return result(.unavailableProfileEvidence)
            }
            if regularity == true && (compat?.q90Margin.withinReference == true) {
                return result(.canonicalCandidate)
            }
            return result(.insufficientEvidence)

        default:
            // Pause / HFS / non-pattern: not audited for canonicality in P2.
            reasons.append(.familyNotAuditedInP2)
            return result(.insufficientEvidence)
        }
    }
}
