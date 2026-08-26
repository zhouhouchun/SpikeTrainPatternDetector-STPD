import STPDCore

/// The durable confirmed-manifest store now lives in STPDCore as
/// `ConfirmedScientificImportManifestStore`, so the readiness-bearing wrapper's mint
/// (`PersistedConfirmedScientificImportManifest.verified`) can be `internal` to the module and
/// reachable only through the store's `save`/`restoreVerify` durable paths. There is no app-side store
/// implementation and no public arbitrary-file API that yields persistence standing.
///
/// These aliases keep the app-facing names stable for the coordinator and its tests.
typealias ScientificImportManifestFileStore = ConfirmedScientificImportManifestStore
typealias ScientificImportManifestStoreError = ConfirmedScientificImportManifestStoreError
