/// The reason a persisted-confirmation verification failed. Ordered and distinct: a caller can tell a
/// changed source, a changed scientific dataset, and a changed confirmation record apart. A decoded
/// receipt that merely exists or parses is never sufficient — verification requires exact matching
/// against a freshly produced base confirmation.
public enum PersistedConfirmationVerificationError: Error, Equatable, Sendable {
    /// The receipt's exact source transaction (SHA / CSV-or-worksheet selection) differs from the
    /// freshly produced base confirmation.
    case sourceBindingMismatch
    /// The canonical fingerprint identity triple — the sole scientific identity — differs. This is a
    /// replay divergence: the reselected source replayed to a different canonical dataset.
    case canonicalFingerprintMismatch
    /// The scientific fingerprint is unchanged, but some confirmed decision (header rule, unit,
    /// activity mode, segment, groups, attributes including Presentation) or the record digest differs.
    /// This is a confirmation-record mismatch, never a changed scientific dataset.
    case confirmationRecordMismatch
}

/// A sealed, in-memory proof that a specific confirmed scientific import has a matching persisted
/// receipt on disk — either just written and read back exactly, or reconstructed by a full replay
/// after restart that reproduced the exact source binding, every confirmed decision, and the
/// canonical fingerprint identity triple.
///
/// This is the third of three deliberately distinct states:
/// 1. `ConfirmedScientificImportManifest` — a live, in-memory base confirmation.
/// 2. `ConfirmedScientificImportReceipt` — untrusted decoded disk content that cannot, by itself,
///    enter readiness or create a confirmation.
/// 3. this wrapper — mintable ONLY by `ConfirmedScientificImportManifestStore` after it has read back
///    its own app-managed final record and matched it against a freshly produced base confirmation.
///
/// The mint (`verified(…)`) is `internal` to STPDCore, so no public or package API outside the module
/// can turn a `base + in-memory receipt` into persistence standing. The store is the single caller,
/// and it passes only a receipt it decoded from its own derived final path after every durability and
/// final-readback barrier. There is no public arbitrary-file path that yields this wrapper.
///
/// It removes exactly one readiness blocker (`confirmedManifestPersistenceUnavailable`) and grants no
/// detector, active-dataset, review, result-package, or export authority. A public `.persisted = true`
/// flag is deliberately never added to the base confirmation; persistence standing lives only here.
public struct PersistedConfirmedScientificImportManifest: Sendable {
    /// The freshly produced base confirmation this persisted standing attaches to. Its own
    /// `persistence` remains `.unavailableInMemoryOnly`; persistence standing is expressed by this
    /// wrapper's existence, not by mutating the base.
    public let baseConfirmation: ConfirmedScientificImportManifest
    /// The verified stored receipt that matched the base confirmation exactly.
    public let receipt: ConfirmedScientificImportReceipt

    /// Private: a wrapper can be minted only by the colocated `verified(…)` after exact matching.
    private init(
        baseConfirmation: ConfirmedScientificImportManifest,
        receipt: ConfirmedScientificImportReceipt
    ) {
        self.baseConfirmation = baseConfirmation
        self.receipt = receipt
    }

    /// The sole minting path — `internal`, and gated by an opaque `DurableRecordCapability` whose
    /// initializer is `fileprivate` to `ConfirmedScientificImportManifestStore`. Only that store, after
    /// completing its anchored durability barrier on its own derived final record, can construct the
    /// capability, so nothing else — in any module — can promote a receipt to standing. Projects the
    /// expected receipt EXCLUSIVELY from the freshly produced base confirmation (the header rule is
    /// derived from the sealed base, never supplied), then compares it field-by-field against the
    /// `decodedReceipt` the store read back. Only complete equality mints the wrapper; any divergence
    /// returns a specific, ordered failure. `throws` only when the base's header presence is empty or
    /// mixed (a fail-closed derivation defect). The check runs on decision metadata only — no
    /// restaging, normalization, validation, projection, or fingerprinting, and never scans spike
    /// timestamps.
    static func verified(
        baseConfirmation: ConfirmedScientificImportManifest,
        decodedReceipt: ConfirmedScientificImportReceipt,
        durability: DurableRecordCapability
    ) throws -> Result<PersistedConfirmedScientificImportManifest, PersistedConfirmationVerificationError> {
        _ = durability
        let expected = try ConfirmedScientificImportManifestPersistence.project(from: baseConfirmation)

        // 1. Exact source transaction (SHA + CSV/worksheet selection).
        guard expected.sourceTransactionBinding == decodedReceipt.sourceTransactionBinding else {
            return .failure(.sourceBindingMismatch)
        }
        // 2. Canonical fingerprint identity triple — the sole scientific identity.
        guard expected.canonicalSchemaContractID == decodedReceipt.canonicalSchemaContractID,
              expected.canonicalSchemaContractDigest == decodedReceipt.canonicalSchemaContractDigest,
              expected.canonicalDatasetDigest == decodedReceipt.canonicalDatasetDigest else {
            return .failure(.canonicalFingerprintMismatch)
        }
        // 3. Every remaining confirmed decision and the record digest. Unchanged fingerprint with any
        //    other difference is a confirmation-record mismatch, never a changed scientific dataset.
        guard expected == decodedReceipt else {
            return .failure(.confirmationRecordMismatch)
        }

        return .success(
            PersistedConfirmedScientificImportManifest(
                baseConfirmation: baseConfirmation, receipt: decodedReceipt
            )
        )
    }

    /// The sole scientific identity of the persisted confirmed dataset.
    public var canonicalFingerprint: CanonicalScientificDatasetFingerprint {
        baseConfirmation.canonicalFingerprint
    }

    /// The digest identifying this source-bound saved record (never a scientific identity).
    public var confirmationRecordDigest: String {
        receipt.confirmationRecordDigest
    }
}
