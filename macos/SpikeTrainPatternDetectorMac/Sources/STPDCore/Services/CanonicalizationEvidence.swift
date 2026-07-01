import Foundation

// MARK: - P1: audit-only interval / profile evidence boundary
//
// These types unify the distributional evidence already computed per-route (candidate q50/q90/q95, CV/CV2/LV,
// eventness contrast) and the resolved-profile reference distribution into ONE explicit, profile-relative,
// audit-only model. They are NEVER consumed by detection: no final label, arbitration, export, threshold,
// learned-threshold, manual-annotation, or UI behavior reads them. They add no fixed-ms assumptions — every
// comparison is candidate-vs-train-resolved-profile, and missing evidence is reported as unavailable (nil),
// NOT as incompatibility.
//
// Separation (intentional):
//   1. ResolvedFamilyDistributionSummary — the train's *reference* interval distribution for a family, populated
//      ONLY when a true in-band sample exists (never synthesized from a single numeric bound).
//   2. CandidateIntervalEvidence — a single candidate's *observed* interval distribution + coverages + contrast.
//   3. IntervalCompatibility — audit-only margins comparing (2) against (1), with explicit availability.
//   4. CandidateDecisionAudit — the candidate's *current* arbitration outcome, kept separate from (2)/(3).

public enum IntervalFamily: String, Hashable, Sendable, CaseIterable {
    case burst
    case hfTonic = "hf_tonic"
    case tonic
    case pause
    case hfs

    /// The provenance-key prefix this family uses in `ResolvedThresholdProfile.provenance` (e.g. `burst.…`).
    public var provenanceKeyPrefix: String { rawValue }

    /// Map a detector label to its interval family (nil for non-pattern labels).
    public static func from(label: ClassicAnchorLabel) -> IntervalFamily? {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst: return .burst
        case .tonic: return .tonic
        case .highFrequencyTonic: return .hfTonic
        case .highFrequencySpiking: return .hfs
        case .pause: return .pause
        case .reject, .profile: return nil
        }
    }
}

// MARK: - 1. ResolvedFamilyDistributionSummary

/// The reference interval distribution for one family on one train: empirical quantiles of the train's ISIs that
/// fall within the family's resolved band. Quantiles are populated ONLY when the in-band sample is large enough —
/// a single numeric bound (e.g. a bare user hard gate with no in-band ISIs) yields an UNAVAILABLE summary, never
/// synthesized quantiles.
public struct ResolvedFamilyDistributionSummary: Hashable, Sendable {
    public let family: IntervalFamily
    public let q50Sec: Double?
    public let q75Sec: Double?
    public let q90Sec: Double?
    public let q95Sec: Double?
    public let sampleSize: Int
    /// Shrinkage confidence n/(n+k) (matches the learned-threshold convention); nil when no reference exists.
    public let confidence: Double?
    public let provenance: ResolvedThresholdSource

    public init(
        family: IntervalFamily, q50Sec: Double?, q75Sec: Double?, q90Sec: Double?, q95Sec: Double?,
        sampleSize: Int, confidence: Double?, provenance: ResolvedThresholdSource
    ) {
        self.family = family
        self.q50Sec = q50Sec
        self.q75Sec = q75Sec
        self.q90Sec = q90Sec
        self.q95Sec = q95Sec
        self.sampleSize = sampleSize
        self.confidence = confidence
        self.provenance = provenance
    }

    /// True when a real reference distribution exists (quantiles populated from a sufficient sample).
    public var isAvailable: Bool { q50Sec != nil }

    /// Build from an explicit in-band ISI sample. Returns an UNAVAILABLE summary when the sample is below
    /// `minSample` — it never invents quantiles.
    public static func from(
        family: IntervalFamily,
        inBandISIsSec: [Double],
        provenance: ResolvedThresholdSource,
        minSample: Int = 4,
        confidenceK: Double = 6
    ) -> ResolvedFamilyDistributionSummary {
        let sample = inBandISIsSec.filter { $0.isFinite && $0 > 0 }
        guard sample.count >= max(2, minSample) else {
            return ResolvedFamilyDistributionSummary(
                family: family, q50Sec: nil, q75Sec: nil, q90Sec: nil, q95Sec: nil,
                sampleSize: sample.count, confidence: nil, provenance: provenance
            )
        }
        let s = SortedFiniteSample(sample)
        let n = Double(sample.count)
        return ResolvedFamilyDistributionSummary(
            family: family, q50Sec: s.quantile(0.5), q75Sec: s.quantile(0.75),
            q90Sec: s.quantile(0.9), q95Sec: s.quantile(0.95),
            sampleSize: sample.count, confidence: n / (n + confidenceK), provenance: provenance
        )
    }

    /// Build the reference from a train: the empirical distribution of the train's ISIs that fall inside the
    /// family's resolved band. `upperSec` nil/non-finite ⇒ a one-sided lower band (e.g. pause: ISI ≥ lower).
    public static func fromTrain(
        train: SpikeTrain,
        family: IntervalFamily,
        lowerSec: Double,
        upperSec: Double?,
        provenance: ResolvedThresholdSource,
        minValidISISec: Double = 0.001,
        minSample: Int = 4
    ) -> ResolvedFamilyDistributionSummary {
        let inBand = train.isiSec.compactMap { value -> Double? in
            guard let value, value.isFinite, value > 0, value >= minValidISISec, value >= lowerSec else { return nil }
            if let upperSec, upperSec.isFinite, value > upperSec { return nil }
            return value
        }
        return from(family: family, inBandISIsSec: inBand, provenance: provenance, minSample: minSample)
    }
}

// MARK: - 2. CandidateIntervalEvidence

public struct CandidateIntervalEvidence: Hashable, Sendable {
    public let candidateID: String
    public let route: String            // candidateLayer (where the candidate came from)
    public let candidateClass: String
    public let proposedLabel: ClassicAnchorLabel
    public let proposedFamily: IntervalFamily?
    public let isiCount: Int
    public let spikeCount: Int

    // Observed candidate interval distribution (from the candidate's own ISI slice).
    public let q50Sec: Double?
    public let q75Sec: Double?
    public let q90Sec: Double?
    public let q95Sec: Double?
    public let cv: Double?
    public let cv2: Double?
    public let lv: Double?

    // Coverage = fraction of candidate ISIs inside each family's resolved band (nil when the band is unusable).
    public let burstCoreCoverage: Double?
    public let burstBridgeCoverage: Double?
    public let tonicCoverage: Double?
    public let hfTonicCoverage: Double?
    public let pauseCoverage: Double?

    // P2.5: EXACT integer counts (not reconstructed from coverage by rounding), nil when the band is unusable.
    // - burstCoreISICount: candidate ISIs inside the resolved burst-core band [lower, upper].
    // - burstBridgeExtensionISICount: candidate ISIs OUTSIDE the core but inside the bridge band (upper, bridgeUpper].
    public let burstCoreISICount: Int?
    public let burstBridgeExtensionISICount: Int?

    // Eventness / contrast inputs (from the existing eventness auditor; nil when unavailable).
    public let edgeContrast: Double?
    public let contextContrast: Double?
    public let eventnessScore: Double?
    public let returnToBaselineScore: Double?

    // User-intent flags (kept distinct: a numeric gate is NOT a semantic label).
    public let softAnchorInvolved: Bool
    public let hardNumericGateInvolved: Bool
    public let manualSemanticLabelInvolved: Bool

    public init(
        candidateID: String, route: String, candidateClass: String, proposedLabel: ClassicAnchorLabel,
        proposedFamily: IntervalFamily?, isiCount: Int, spikeCount: Int,
        q50Sec: Double?, q75Sec: Double?, q90Sec: Double?, q95Sec: Double?,
        cv: Double?, cv2: Double?, lv: Double?,
        burstCoreCoverage: Double?, burstBridgeCoverage: Double?, tonicCoverage: Double?,
        hfTonicCoverage: Double?, pauseCoverage: Double?,
        edgeContrast: Double?, contextContrast: Double?, eventnessScore: Double?, returnToBaselineScore: Double?,
        softAnchorInvolved: Bool, hardNumericGateInvolved: Bool, manualSemanticLabelInvolved: Bool,
        burstCoreISICount: Int? = nil, burstBridgeExtensionISICount: Int? = nil
    ) {
        self.candidateID = candidateID
        self.route = route
        self.candidateClass = candidateClass
        self.proposedLabel = proposedLabel
        self.proposedFamily = proposedFamily
        self.isiCount = isiCount
        self.spikeCount = spikeCount
        self.q50Sec = q50Sec
        self.q75Sec = q75Sec
        self.q90Sec = q90Sec
        self.q95Sec = q95Sec
        self.cv = cv
        self.cv2 = cv2
        self.lv = lv
        self.burstCoreCoverage = burstCoreCoverage
        self.burstBridgeCoverage = burstBridgeCoverage
        self.tonicCoverage = tonicCoverage
        self.hfTonicCoverage = hfTonicCoverage
        self.pauseCoverage = pauseCoverage
        self.edgeContrast = edgeContrast
        self.contextContrast = contextContrast
        self.eventnessScore = eventnessScore
        self.returnToBaselineScore = returnToBaselineScore
        self.softAnchorInvolved = softAnchorInvolved
        self.hardNumericGateInvolved = hardNumericGateInvolved
        self.manualSemanticLabelInvolved = manualSemanticLabelInvolved
        self.burstCoreISICount = burstCoreISICount
        self.burstBridgeExtensionISICount = burstBridgeExtensionISICount
    }

    /// The candidate's ISI values from its span (index `i` = the ISI ending at spike `i`).
    public static func candidateISIsSec(
        train: SpikeTrain, startISIIndex: Int, endISIIndex: Int, minValidISISec: Double = 0.001
    ) -> [Double] {
        guard startISIIndex <= endISIIndex else { return [] }
        return (startISIIndex...endISIIndex).compactMap { i in
            guard i >= 0, i < train.isiSec.count, let v = train.isiSec[i], v.isFinite, v > 0, v >= minValidISISec else {
                return nil
            }
            return v
        }
    }

    public static func build(
        candidate: ClassicAnchorCandidate,
        train: SpikeTrain,
        profile: ResolvedThresholdProfile,
        eventness: ISICandidateEventnessAudit?,
        minValidISISec: Double = 0.001,
        manualSemanticLabelInvolved: Bool = false,
        bandEdgeRelativeTolerance: Double = 1e-9
    ) -> CandidateIntervalEvidence {
        let isis = candidateISIsSec(
            train: train, startISIIndex: candidate.startISIIndex, endISIIndex: candidate.endISIIndex,
            minValidISISec: minValidISISec
        )
        let sample = isis.count >= 2 ? SortedFiniteSample(isis) : nil

        // P4.5: tiny scale-relative band-edge tolerance (a FRACTION of the bound, not ms) so a value sitting at a band
        // edge ± floating-point accumulation epsilon is counted on the intended side. Far below any real ISI
        // resolution, so it never shifts genuine membership.
        let edgeTol = max(0, bandEdgeRelativeTolerance)
        func edge(_ x: Double) -> Double { x.isFinite ? abs(x) * edgeTol : 0 }

        func inBandCount(lower: Double, upper: Double?) -> Int? {
            guard !isis.isEmpty, lower.isFinite else { return nil }
            let lo = lower - edge(lower)
            return isis.filter { v in
                v >= lo && (upper.map { u in !u.isFinite || v <= u + edge(u) } ?? true)
            }.count
        }
        func coverage(lower: Double, upper: Double?) -> Double? {
            guard let count = inBandCount(lower: lower, upper: upper), !isis.isEmpty else { return nil }
            return Double(count) / Double(isis.count)
        }
        // P2.5 exact counts. Core = [lower, upper]; bridge EXTENSION = (upper, bridgeUpper] (strictly above core).
        // P4.5: both use the SAME tolerant core upper (`upper + edge(upper)`), so an epsilon-above-core value stays
        // CORE (not bridge) — guaranteeing no double-counting and no gap between the two bands.
        let coreUpperWithTolerance = profile.burst.upperSec + edge(profile.burst.upperSec)
        let coreISICount = inBandCount(lower: profile.burst.lowerSec, upper: profile.burst.upperSec)
        let bridgeExtensionISICount: Int? = profile.burst.bridgeUpperSec.flatMap { bridgeUpper -> Int? in
            guard !isis.isEmpty, profile.burst.upperSec.isFinite else { return nil }
            return isis.filter { v in
                v > coreUpperWithTolerance && (!bridgeUpper.isFinite || v <= bridgeUpper + edge(bridgeUpper))
            }.count
        }

        let family = IntervalFamily.from(label: candidate.finalLabel)
        // Soft / hard numeric flags from the resolved-profile provenance for this family (a numeric gate is NOT a
        // semantic label; the manual semantic label flag is an explicit, separate input).
        var soft = false
        var hardNumeric = false
        if let prefix = family?.provenanceKeyPrefix {
            for resolved in profile.provenance where resolved.key.hasPrefix(prefix + ".") {
                if resolved.source == .userSoftAnchor { soft = true }
                if resolved.source == .userHardGate { hardNumeric = true }
            }
        }

        return CandidateIntervalEvidence(
            candidateID: candidate.id,
            route: candidate.candidateLayer,
            candidateClass: candidate.candidateClass,
            proposedLabel: candidate.finalLabel,
            proposedFamily: family,
            isiCount: isis.count,
            spikeCount: candidate.nSpikes,
            q50Sec: sample?.quantile(0.5),
            q75Sec: sample?.quantile(0.75),
            q90Sec: sample?.quantile(0.9),
            q95Sec: sample?.quantile(0.95),
            cv: candidate.cv,
            cv2: candidate.cv2,
            lv: candidate.lv,
            burstCoreCoverage: coverage(lower: profile.burst.lowerSec, upper: profile.burst.upperSec),
            burstBridgeCoverage: profile.burst.bridgeUpperSec.flatMap { coverage(lower: profile.burst.lowerSec, upper: $0) },
            tonicCoverage: coverage(lower: profile.tonic.lowerSec, upper: profile.tonic.upperSec),
            hfTonicCoverage: coverage(lower: profile.hfTonic.lowerSec, upper: profile.hfTonic.upperSec),
            pauseCoverage: coverage(lower: profile.pause.lowerSec, upper: nil),
            edgeContrast: eventness?.eventnessEdgeComponent,
            contextContrast: eventness?.eventnessContextComponent,
            eventnessScore: eventness?.eventnessScore,
            returnToBaselineScore: eventness?.returnToBaselineScore,
            softAnchorInvolved: soft,
            hardNumericGateInvolved: hardNumeric,
            manualSemanticLabelInvolved: manualSemanticLabelInvolved,
            burstCoreISICount: coreISICount,
            burstBridgeExtensionISICount: bridgeExtensionISICount
        )
    }
}

// MARK: - 3. IntervalCompatibility

/// One audit-only margin between a candidate quantile and a reference quantile. `available == false` means the
/// comparison could not be made (candidate or reference quantile missing) — this is UNAVAILABLE, not incompatible.
public struct ProfileMargin: Hashable, Sendable {
    public let available: Bool
    /// candidateQuantile − referenceQuantile (seconds), signed; nil when unavailable.
    public let deltaSec: Double?
    /// candidateQuantile ≤ referenceQuantile (profile-relative); nil when unavailable.
    public let withinReference: Bool?

    public init(available: Bool, deltaSec: Double?, withinReference: Bool?) {
        self.available = available
        self.deltaSec = deltaSec
        self.withinReference = withinReference
    }

    public static let unavailable = ProfileMargin(available: false, deltaSec: nil, withinReference: nil)

    fileprivate static func compare(_ candidate: Double?, _ reference: Double?) -> ProfileMargin {
        guard let candidate, let reference, candidate.isFinite, reference.isFinite else { return .unavailable }
        return ProfileMargin(available: true, deltaSec: candidate - reference, withinReference: candidate <= reference)
    }
}

public struct IntervalCompatibility: Hashable, Sendable {
    public let family: IntervalFamily
    public let referenceAvailable: Bool
    public let q90Margin: ProfileMargin
    public let q95Margin: ProfileMargin
    public let coverage: Double?

    public init(
        family: IntervalFamily, referenceAvailable: Bool,
        q90Margin: ProfileMargin, q95Margin: ProfileMargin, coverage: Double?
    ) {
        self.family = family
        self.referenceAvailable = referenceAvailable
        self.q90Margin = q90Margin
        self.q95Margin = q95Margin
        self.coverage = coverage
    }

    /// Audit-only comparison of a candidate's observed quantiles against a family reference distribution. When the
    /// reference (or the candidate quantile) is unavailable, the margins are UNAVAILABLE — never "incompatible".
    public static func compare(
        candidate: CandidateIntervalEvidence,
        reference: ResolvedFamilyDistributionSummary,
        coverage: Double?
    ) -> IntervalCompatibility {
        IntervalCompatibility(
            family: reference.family,
            referenceAvailable: reference.isAvailable,
            q90Margin: ProfileMargin.compare(candidate.q90Sec, reference.q90Sec),
            q95Margin: ProfileMargin.compare(candidate.q95Sec, reference.q95Sec),
            coverage: coverage
        )
    }
}

// MARK: - 4. CandidateDecisionAudit (current arbitration outcome — kept separate from pre-arbitration evidence)

public struct CandidateDecisionAudit: Hashable, Sendable {
    public let candidateID: String
    public let finalLabel: ClassicAnchorLabel
    public let selectedForAuto: Bool
    public let selectionStatus: String
    public let anchorLockLevel: ClassicAnchorLockLevel

    public init(
        candidateID: String, finalLabel: ClassicAnchorLabel, selectedForAuto: Bool,
        selectionStatus: String, anchorLockLevel: ClassicAnchorLockLevel
    ) {
        self.candidateID = candidateID
        self.finalLabel = finalLabel
        self.selectedForAuto = selectedForAuto
        self.selectionStatus = selectionStatus
        self.anchorLockLevel = anchorLockLevel
    }

    public static func from(_ candidate: ClassicAnchorCandidate) -> CandidateDecisionAudit {
        CandidateDecisionAudit(
            candidateID: candidate.id,
            finalLabel: candidate.finalLabel,
            selectedForAuto: candidate.selectedForAuto,
            selectionStatus: candidate.selectionStatus,
            anchorLockLevel: candidate.anchorLockLevel
        )
    }
}

// MARK: - Aggregate (convenience bundle; still audit-only)

public struct CanonicalizationEvidence: Hashable, Sendable {
    public let evidence: CandidateIntervalEvidence
    public let burstCompatibility: IntervalCompatibility?
    public let tonicCompatibility: IntervalCompatibility?
    public let hfTonicCompatibility: IntervalCompatibility?
    public let decision: CandidateDecisionAudit

    public init(
        evidence: CandidateIntervalEvidence,
        burstCompatibility: IntervalCompatibility?,
        tonicCompatibility: IntervalCompatibility?,
        hfTonicCompatibility: IntervalCompatibility?,
        decision: CandidateDecisionAudit
    ) {
        self.evidence = evidence
        self.burstCompatibility = burstCompatibility
        self.tonicCompatibility = tonicCompatibility
        self.hfTonicCompatibility = hfTonicCompatibility
        self.decision = decision
    }
}
