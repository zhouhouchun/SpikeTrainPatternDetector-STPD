/// A stable, functional reason that authoritative scientific analysis is blocked. This vocabulary is
/// deny-only: the presence of any blocker means analysis is not permitted. There is no positive
/// "ready" or "permitted" member.
public enum ScientificAnalysisReadinessBlocker: Hashable, Sendable {
    case confirmedManifestPersistenceUnavailable
    case recordingSegmentNotRepresented
    case trialContractNotRepresented
    case runContractUnavailable
    case detectorConsumerClosureUnavailable
    case authoritativeExportClosureUnavailable
    /// Authoritative biological pattern detection is not defined for this activity mode. This is a
    /// `not_evaluated` state — never zero, absent, negative, or "no pattern detected".
    case authoritativeBiologicalPatternDetectionNotDefinedForActivityMode(ScientificDatasetActivityMode)
}

/// A fail-closed, deny-only analysis-readiness assessment.
///
/// It is structurally impossible to represent a positive/ready state: an assessment always has at
/// least a `firstBlocker`. It is informational, never an authorization token — it reports only the
/// blocking reasons and never grants readiness. Construction is restricted to the colocated
/// `ScientificAnalysisReadinessEvaluator`.
public struct ScientificAnalysisReadinessAssessment: Sendable {
    public let firstBlocker: ScientificAnalysisReadinessBlocker
    public let additionalBlockers: [ScientificAnalysisReadinessBlocker]

    fileprivate init(
        firstBlocker: ScientificAnalysisReadinessBlocker,
        additionalBlockers: [ScientificAnalysisReadinessBlocker]
    ) {
        self.firstBlocker = firstBlocker
        self.additionalBlockers = additionalBlockers
    }

    /// The ordered blockers, first then additional. Always non-empty.
    public var blockers: [ScientificAnalysisReadinessBlocker] {
        [firstBlocker] + additionalBlockers
    }

    /// Deny-only: an assessment always carries at least one blocker, so authoritative analysis is
    /// never permitted. There is deliberately no positive "ready" flag.
    public var isAnalysisBlocked: Bool {
        true
    }
}

/// Produces a deny-only, fail-closed analysis-readiness assessment for a confirmed import.
///
/// For the current canonical model every import remains shadow-only. The assessment always reports
/// the fixed set of incomplete-contract blockers, and for `intentional_multi_unit` and
/// `unknown_or_uncertain` additionally reports that authoritative biological pattern detection is
/// not defined for that mode (a `not_evaluated` state, never "no pattern detected"). It never grants
/// readiness.
public enum ScientificAnalysisReadinessEvaluator {
    public static func assess(
        _ manifest: ConfirmedScientificImportManifest
    ) -> ScientificAnalysisReadinessAssessment {
        var additional: [ScientificAnalysisReadinessBlocker] = []

        // The current model is event-scope only.
        switch manifest.temporalScope {
        case .eventScopeOnly:
            additional.append(.recordingSegmentNotRepresented)
            additional.append(.trialContractNotRepresented)
        }

        // The remaining downstream contracts do not exist in this slice.
        additional.append(.runContractUnavailable)
        additional.append(.detectorConsumerClosureUnavailable)
        additional.append(.authoritativeExportClosureUnavailable)

        // Authoritative biological pattern detection is undefined outside putative single-unit data.
        switch manifest.activityMode {
        case .putativeSingleUnit:
            break
        case .intentionalMultiUnit, .unknownOrUncertain:
            additional.append(
                .authoritativeBiologicalPatternDetectionNotDefinedForActivityMode(manifest.activityMode)
            )
        }

        // The persistence blocker is always first (this slice is in-memory only). The switch keeps
        // the assessment fail-closed if a future persistence state is introduced.
        switch manifest.persistence {
        case .unavailableInMemoryOnly:
            return ScientificAnalysisReadinessAssessment(
                firstBlocker: .confirmedManifestPersistenceUnavailable,
                additionalBlockers: additional
            )
        }
    }
}
