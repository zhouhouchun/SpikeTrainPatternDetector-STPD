/// The confirmation entry point. It delegates to `ConfirmedScientificImportManifest.confirmed(from:)`,
/// whose file-private initializer makes a manifest constructible only from one already-sealed
/// `ConfirmableScientificImport`.
///
/// It reuses the canonical fingerprint already produced during validation and performs no second
/// independent validation, no restaging or renormalization, and no spike-timestamp-proportional
/// rescan or rehash — it calls no validator, normalizer, projector, or fingerprinter. Because its
/// only input is the sealed pairing (whose initializer is file-private to the projector), a caller
/// cannot combine an unrelated manifest draft, validation report, source binding, or canonical
/// fingerprint.
public enum ScientificImportConfirmationBuilder {
    public static func confirm(
        _ confirmable: ConfirmableScientificImport
    ) -> ConfirmedScientificImportManifest {
        .confirmed(from: confirmable)
    }
}
