/// A stable, functional reason that authoritative scientific analysis is blocked. This vocabulary is
/// deny-only: the presence of any blocker means analysis is not permitted. There is no positive
/// "ready" or "permitted" member.
public enum ScientificAnalysisReadinessBlocker: Hashable, Sendable {
    case confirmedManifestPersistenceUnavailable
    /// Exact acquisition bounds are unknown/unavailable, so no recording-wide rate, occupancy, or
    /// edge-Pause claim is authoritative. This censors complete recording-relative or
    /// boundary-censored episode-duration claims — not a candidate episode's internally supported
    /// duration measured wholly within the imported data.
    case observationBoundsUnavailable
    /// `trialized` regime: Trial entities/contract are not represented by this slice.
    case trialEntitiesNotRepresented
    /// Regime `unknown_or_uncertain`: the Trial contract is unavailable.
    case trialContractUnavailable
    /// Regime `unknown_or_uncertain`: the recording regime itself is unknown/uncertain.
    case recordingRegimeUnknownOrUncertain
    /// Coverage `not_all_spike_trains_full_imported_excerpt`: partial imported-excerpt coverage.
    case partialImportedExcerptCoverage
    /// Coverage `unknown_or_uncertain`.
    case importedExcerptCoverageUnknownOrUncertain
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
    /// A bare, in-memory base confirmation: the persistence blocker is always first, every contract
    /// blocker follows.
    public static func assess(
        _ manifest: ConfirmedScientificImportManifest
    ) -> ScientificAnalysisReadinessAssessment {
        ScientificAnalysisReadinessAssessment(
            firstBlocker: .confirmedManifestPersistenceUnavailable,
            additionalBlockers: contractBlockers(for: manifest)
        )
    }

    /// A persisted-and-verified confirmation removes exactly one blocker
    /// (`confirmedManifestPersistenceUnavailable`) and retains every other blocker unchanged. It never
    /// grants readiness: observation-bounds, run-contract, detector-consumer, export, Trial/coverage,
    /// and activity-mode blockers all remain, so the assessment stays deny-only and fail-closed.
    public static func assess(
        _ persisted: PersistedConfirmedScientificImportManifest
    ) -> ScientificAnalysisReadinessAssessment {
        let contract = contractBlockers(for: persisted.baseConfirmation)
        guard let first = contract.first else {
            // Unreachable in this slice: run/detector/export blockers are unconditional, so `contract`
            // is never empty. Fail closed anyway — never fabricate readiness by returning no blocker.
            return ScientificAnalysisReadinessAssessment(
                firstBlocker: .confirmedManifestPersistenceUnavailable,
                additionalBlockers: []
            )
        }
        return ScientificAnalysisReadinessAssessment(
            firstBlocker: first,
            additionalBlockers: Array(contract.dropFirst())
        )
    }

    /// Every deny-only blocker EXCEPT the persistence blocker, in canonical order. Persistence
    /// standing is layered on by the caller: absent for a bare confirmation (persistence blocker
    /// added as `firstBlocker`), satisfied for the sealed persisted wrapper (persistence blocker
    /// omitted). This is the single shared source of the non-persistence blocker set.
    private static func contractBlockers(
        for manifest: ConfirmedScientificImportManifest
    ) -> [ScientificAnalysisReadinessBlocker] {
        var additional: [ScientificAnalysisReadinessBlocker] = []
        let segment = manifest.recordingSegment

        // Exact acquisition bounds are unknown/unavailable (the only supported state this slice).
        switch segment.observationBounds {
        case .unknownOrUnavailable:
            additional.append(.observationBoundsUnavailable)
        }

        // Recording regime → Trial matrix. `continuous_untrialed` makes Trial explicitly not
        // applicable, so no Trial blocker is reported for it.
        switch segment.regime {
        case .continuousUntrialed:
            break
        case .trialized:
            additional.append(.trialEntitiesNotRepresented)
        case .unknownOrUncertain:
            additional.append(.recordingRegimeUnknownOrUncertain)
            additional.append(.trialContractUnavailable)
        }

        // Imported-excerpt coverage matrix. `all` adds no blocker but grants no positive permission.
        switch segment.importedExcerptCoverage {
        case .allSpikeTrainsFullImportedExcerpt:
            break
        case .notAllSpikeTrainsFullImportedExcerpt:
            additional.append(.partialImportedExcerptCoverage)
        case .unknownOrUncertain:
            additional.append(.importedExcerptCoverageUnknownOrUncertain)
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

        return additional
    }
}
