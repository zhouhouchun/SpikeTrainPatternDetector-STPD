import Foundation

// MARK: - P3A: central burst canonicalization decision (pure)
//
// One pure function that maps a burst candidate + its P1 evidence + its P2 verdict into a canonical-vs-possible
// DECISION. It is the single chokepoint the pipeline consults — but ONLY when the Adaptive-v2 flag is enabled.
// With the flag OFF (the default, shipped state), the pipeline never calls this, so behavior is byte-identical.
//
// Decision policy (audit-derived; demotes only on positive evidence, never on missing evidence):
//   • Non-canonical-burst candidates are left alone (nothing to demote).
//   • A manual SEMANTIC burst label is a user override → keep canonical.
//   • canonical_candidate / insufficient_evidence / unavailable_profile_evidence → keep canonical
//     (don't demote on confirmation, and don't demote when evidence is missing/inconclusive).
//   • possible_burst / pause_boundary_driven / bridge_without_core / tonic_guard_conflict → demote to possible.

public enum BurstCanonicalDecision: String, Hashable, Sendable {
    case keepCanonical
    case demoteToPossible
}

public enum BurstCanonicalizationGate {
    /// Decide whether a burst candidate should remain a canonical classic burst or be demoted to possible/review.
    /// Pure: depends only on the candidate's current label, the P2 verdict, and the override signal. The `evidence`
    /// is part of the contract (and already drives the verdict) — kept in the signature for future criteria.
    public static func decide(
        candidate: ClassicAnchorCandidate,
        evidence: CandidateIntervalEvidence,
        verdict: CanonicalizationVerdictResult
    ) -> BurstCanonicalDecision {
        // Only currently-canonical burst candidates are subject to demotion.
        guard candidate.finalLabel.isCanonicalBurstFamily else { return .keepCanonical }
        // A manual semantic burst label is the user's explicit decision — never demote.
        if verdict.manualSemanticOverride { return .keepCanonical }

        switch verdict.verdict {
        case .canonicalCandidate, .strongCoreRescue, .insufficientEvidence, .unavailableProfileEvidence:
            return .keepCanonical   // P11A: strong-core rescue is a KEEP outcome
        case .possibleBurstCandidate, .pauseBoundaryDrivenDensePacket, .bridgeWithoutCore, .tonicGuardConflict:
            return .demoteToPossible
        }
    }
}
