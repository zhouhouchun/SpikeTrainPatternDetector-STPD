/// The temporal scope a confirmed canonical model represents. The current model is event-scope
/// only: `RecordingSegment` and `Trial` are not represented.
public enum ConfirmedImportTemporalScope: Hashable, Sendable {
    /// `event_scope_only / recording_segment_and_trial_not_represented`.
    case eventScopeOnly
}

/// Whether a confirmation record has been persisted. This slice is in-memory only; persistence is
/// explicitly recorded as unavailable rather than pretended.
public enum ConfirmationPersistenceAvailability: Hashable, Sendable {
    case unavailableInMemoryOnly
}

/// An immutable, source-bound confirmation record for an independently replay-validated scientific
/// import.
///
/// This is a CONFIRMATION RECORD, not an authority receipt. It proves which exact source
/// transaction was reviewed, which explicit user decisions were confirmed, and which canonical
/// fingerprint resulted. It exposes no positive permission (`authority`, `permitsDetection`,
/// `permitsExport`, or equivalent), does not confirm that the analysis contract is complete, and
/// grants no detector, review, result-package, or export authority.
///
/// Scientific identity is solely `canonicalFingerprint` — there is no second competing scientific
/// dataset digest. `sourceTransactionBinding` and the bound source/provenance (CSV/XLSX format,
/// source unit, worksheet coordinates, Presentation choices, sort history, requested duplicate
/// policy) may distinguish confirmation records, but they do not change canonical scientific
/// identity when the resulting canonical scientific data are equivalent.
///
/// It is deliberately not `Hashable`/`Equatable`: synthesized whole-manifest hashing or equality
/// would traverse the validated import's spike-timestamp arrays and source cells. Compare the
/// specific fields that matter instead (for example `canonicalFingerprint`).
public struct ConfirmedScientificImportManifest: Sendable {
    /// The exact non-forgeable independently validated import. It binds the resolved user decisions,
    /// activity mode, dataset-global `ScientificSpikeTrainID` assignments, EventScopeGroup
    /// definitions and membership, confirmed group time bases and event-relative origins, event and
    /// attribute definitions, source time unit, timestamp ordering decisions, requested duplicate
    /// policies, and Presentation decisions. `collapseExact` remains only a requested future
    /// run-derived analysis-view policy and never removes timestamps from canonical raw data.
    public let validatedImport: CanonicalProjectionValidatedImport
    /// The sole scientific identity of the confirmed canonical dataset.
    public let canonicalFingerprint: CanonicalScientificDatasetFingerprint
    /// The exact reviewed source transaction. Source-bound provenance only; never scientific identity.
    public let sourceTransactionBinding: StagedSourceTransactionBinding
    public let temporalScope: ConfirmedImportTemporalScope
    public let persistence: ConfirmationPersistenceAvailability

    /// File-private so a manifest can be built only by the colocated factory, only from one already
    /// sealed `ConfirmableScientificImport`. There is intentionally no initializer that accepts a
    /// validated import, fingerprint, and source binding independently.
    private init(
        validatedImport: CanonicalProjectionValidatedImport,
        canonicalFingerprint: CanonicalScientificDatasetFingerprint,
        sourceTransactionBinding: StagedSourceTransactionBinding,
        temporalScope: ConfirmedImportTemporalScope,
        persistence: ConfirmationPersistenceAvailability
    ) {
        self.validatedImport = validatedImport
        self.canonicalFingerprint = canonicalFingerprint
        self.sourceTransactionBinding = sourceTransactionBinding
        self.temporalScope = temporalScope
        self.persistence = persistence
    }

    /// The sole construction path: from one already-sealed `ConfirmableScientificImport`. It reuses
    /// the canonical fingerprint the projector already produced and performs no projection,
    /// validation, normalization, or hashing.
    public static func confirmed(
        from confirmable: ConfirmableScientificImport
    ) -> ConfirmedScientificImportManifest {
        ConfirmedScientificImportManifest(
            validatedImport: confirmable.validatedImport,
            canonicalFingerprint: confirmable.shadow.fingerprint,
            sourceTransactionBinding: confirmable.shadow.sourceTransactionBinding,
            temporalScope: .eventScopeOnly,
            persistence: .unavailableInMemoryOnly
        )
    }

    /// The confirmed activity mode.
    public var activityMode: ScientificDatasetActivityMode {
        validatedImport.preparedImport.data.activityMode
    }

    /// The confirmed source time unit (retained as a decision; canonical time is integer microseconds).
    public var sourceTimeUnit: SpikeTimeUnit {
        validatedImport.preparedImport.provenance.resolvedPlan.sourceTimeUnit
    }
}
