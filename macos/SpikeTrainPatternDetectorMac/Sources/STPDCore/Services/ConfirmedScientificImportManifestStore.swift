import Foundation

/// A defect while persisting, restoring, or discovering a confirmed-manifest receipt. Every case is
/// distinct and fail-closed; the store never overwrites, migrates, or best-effort repairs, and never
/// falls back to a temporary directory when durable storage is unavailable.
public enum ConfirmedScientificImportManifestStoreError: Error, Equatable, Sendable {
    case durabilityUnavailable
    case invalidSourceSHA
    case invalidRecordDigest
    case oversizedOrNoncanonicalEncoding
    case conflictingBytesAtTarget
    case candidateLimitExceeded(maximum: Int)
    case writeFailed
    case publishFailed
    /// A directory/file/lock open, `fstat`, `fsync`, `close`, `link`, or path-anchor check failed.
    case durabilitySyncFailed
    /// A path component was a symlink or a non-regular/non-directory object where one was required.
    case unsafePathObject
    /// A synchronized/opened descriptor no longer matches the current directory entry that named it
    /// (device/inode/type mismatch, unlink, or replacement). Fail-closed: no standing is minted.
    case pathIdentityChanged
    /// A successfully created unique temporary file could not be removed on an exit path. Fail-closed:
    /// no standing is minted and the store is not claimed clean.
    case temporaryCleanupFailed
    case readbackMismatch
    case recordNotFound
    case storeUnreadable
    /// Discovery exceeded its deterministic entry or byte-work budget.
    case discoveryBudgetExceeded
    /// The store lock could not be acquired within the bounded deadline.
    case lockTimeout
    case operationCancelled
    /// The header rule could not be derived from the sealed base (no source columns, or mixed header
    /// presence). Fail-closed: nothing is written and no standing is minted.
    case headerRuleDerivation(PersistedHeaderRuleDerivationError)
    case read(PersistedStoreReadError)
    case verification(PersistedConfirmationVerificationError)
}

/// An opaque, compiler-enforced durability capability. Its initializer is `fileprivate` to this file,
/// so ONLY `ConfirmedScientificImportManifestStore` — after it has completed the full anchored
/// durability barrier on its own derived final record — can construct it. The persisted wrapper's mint
/// (`verified`) requires it, so no other code, in any module, can promote a receipt to persistence
/// standing.
public struct DurableRecordCapability: Sendable {
    fileprivate init() {}
}

/// The outcome of publishing one receipt. It carries the sealed persisted wrapper the store minted
/// after completing every durability and final-readback barrier — the only production source of
/// persistence standing.
public struct StoredSaveResult: Sendable {
    public enum Outcome: Sendable, Equatable {
        case written
        case idempotentExisting
    }
    public let url: URL
    public let manifest: PersistedConfirmedScientificImportManifest
    public let outcome: Outcome

    public var receipt: ConfirmedScientificImportReceipt { manifest.receipt }
}

/// A safe, decision-only summary of one discovered saved record for the restore picker.
public struct StoredManifestSummary: Sendable, Equatable {
    public let url: URL
    public let receipt: ConfirmedScientificImportReceipt

    public var recordDigest: String { receipt.confirmationRecordDigest }
    public var recordDigestPrefix: String { String(receipt.confirmationRecordDigest.prefix(12)) }
    public var recordingSegmentID: String { receipt.recordingSegment.semanticID.semanticID.canonicalText }
    public var recordingRegime: ScientificRecordingRegime { receipt.recordingSegment.regime }
    public var importedExcerptCoverage: ImportedExcerptCoverage { receipt.recordingSegment.importedExcerptCoverage }
    public var headerRule: PersistedTabularHeaderRule { receipt.headerRule }
    public var activityMode: ScientificDatasetActivityMode { receipt.activityMode }
    public var sourceTimeUnit: SpikeTimeUnit { receipt.sourceTimeUnit }
    public var selection: StagedSourceTransactionSelection { receipt.sourceTransactionBinding.selection }
    public var groupCount: Int { receipt.eventScopeGroups.count }
    public var attributeCount: Int { receipt.attributeDefinitions.count }
}

/// The result of a bounded discovery pass. `workBytesCharged` is a deterministic measure of the read
/// work performed, so adversarial tests can prove the implementation never performs multi-GiB reads and
/// that a file which grows after its size probe cannot bypass the byte budget.
public struct ManifestDiscovery: Sendable, Equatable {
    public let summaries: [StoredManifestSummary]
    public let unreadableRecordCount: Int
    public let entriesInspected: Int
    public let workBytesCharged: Int
}

/// Test-only durability fault injection (`internal`, never public production API). Each hook runs at a
/// specific durability boundary and throws to exercise the SAME fail-closed branch the equivalent real
/// syscall failure takes (documented at each call site); hooks observe the real path rather than
/// replacing it with a no-op.
struct StoreFaultInjection: Sendable {
    var beforeTempWrite: (@Sendable () throws -> Void)?
    /// Runs during the temporary write/fsync/close — the write/fsync/close failure branch (equivalent to
    /// a real `write`/`fsync`/`close` failure). The temp file is left for the shared cleanup path.
    var duringTempWriteSync: (@Sendable () throws -> Void)?
    var afterTempFsync: (@Sendable () throws -> Void)?
    var beforePublishLink: (@Sendable () throws -> Void)?
    /// Runs after the publish link succeeds and the temp is cleaned, but before the durability barrier —
    /// the deterministic window for the publish/durability race and post-publication cancellation tests.
    /// Runs synchronously while the store lock is still held.
    var afterPublishBeforeBarrier: (@Sendable () -> Void)?
    var beforeLockAcquire: (@Sendable () throws -> Void)?
    /// Runs after the store lock is acquired — the lock-identity-validation branch (equivalent to the
    /// real revalidation detecting a replaced/unlinked store-owned directory).
    var afterLockAcquired: (@Sendable () throws -> Void)?
    /// Runs immediately before the final file `fsync` — the file-fsync failure branch.
    var beforeFinalFileFsync: (@Sendable () throws -> Void)?
    /// Runs after the REAL `fsync` of each directory, in the required child-to-parent order. The `Int` is
    /// the depth from the source directory (0 = source, increasing toward the anchor).
    var afterDirectorySync: (@Sendable (Int) throws -> Void)?
    var beforeFinalReadback: (@Sendable () throws -> Void)?
    /// Runs immediately before the final path/descriptor identity revalidation — the identity-rebind
    /// failure branch (equivalent to the real revalidation detecting a replaced/unlinked final record).
    var beforeIdentityRevalidation: (@Sendable () throws -> Void)?
    /// Runs during a temporary-file cleanup — the cleanup-failure branch (equivalent to a real
    /// `unlinkat` failure other than ENOENT).
    var duringTempCleanup: (@Sendable () throws -> Void)?
    /// Runs on each enumeration step — the `readdir` error branch (equivalent to a real errno failure).
    var duringEnumeration: (@Sendable () throws -> Void)?
    /// Runs after a discovery candidate's size probe but before its bounded read — exercises a file that
    /// grows after `fstat`, proving actual-bytes-read byte-work accounting rather than probed size.
    var afterCandidateSizeProbe: (@Sendable () throws -> Void)?
    /// Runs during a discovery candidate's bounded read (the `Int` is the bytes read so far) — the
    /// read-error branch, so a valid receipt prefix followed by a read failure is never listed as a
    /// valid summary (equivalent to a real `read` returning a non-EINTR error mid-file).
    var duringCandidateRead: (@Sendable (Int) throws -> Void)?
    /// Overrides the store-lock acquisition deadline so contention/timeout/cancellation can be
    /// exercised without waiting the full production timeout. Test-only, like the rest of this seam.
    var lockTimeoutOverrideNanoseconds: UInt64?

    static let none = StoreFaultInjection()
}

/// The app-managed, atomic, injectable-root durable store for confirmed-manifest receipts, and the
/// SOLE production source of `PersistedConfirmedScientificImportManifest`.
///
/// The ONLY public production factory is `productionDefault()` (the fixed Application Support store).
/// The arbitrary-root initializer and the fault seam are `internal` (test-only via `@testable`), so no
/// production code in any other module can construct an arbitrary-root store or inject faults.
///
/// Persistence standing is obtainable only through `save` (write → anchored durability barrier →
/// identity revalidation → readback of the derived final path → match → mint) or `restoreVerify` (under
/// the same store-global anchor lock: anchored durability barrier + identity revalidation on the
/// derived final path → match against a replayed base → mint). A generic decode
/// (`ConfirmedScientificImportManifestPersistence.readUntrusted`) and `readForReplay`/`discover` return
/// only bare, untrusted receipts.
public actor ConfirmedScientificImportManifestStore {
    public static let maximumCandidatesPerSource = 128
    public static let fileExtension = "stpdimportmanifest"
    static let maximumDiscoveryEntries = 4_096
    static let maximumDiscoveryBytes = 128 * 1_024 * 1_024
    static let lockTimeoutNanoseconds: UInt64 = 5_000_000_000
    static let lockRetryIntervalNanoseconds: UInt64 = 2_000_000

    /// The FIXED trusted anchor: a directory OUTSIDE every store-owned directory, opened path-based once.
    /// Production: Application Support. Test: the injected root's existing parent. `nil` → durability
    /// unavailable (fail closed; no temporary-directory fallback).
    private let anchorURL: URL?
    /// The store-owned directory components from just below the anchor down to and including the store
    /// root leaf. Every one — plus `by-source` and the per-source directory — is created/traversed and
    /// revalidated by descriptor. None is ever the trusted anchor.
    private let storeRootComponents: [String]
    private let faults: StoreFaultInjection

    /// The internal, arbitrary-root initializer (test-only). The trusted anchor is the injected root's
    /// PARENT (which must already exist and lies outside the store-owned subtree); the injected root leaf
    /// is the first store-owned directory.
    init(rootDirectory: URL, faults: StoreFaultInjection = .none) {
        self.anchorURL = rootDirectory.deletingLastPathComponent()
        self.storeRootComponents = [rootDirectory.lastPathComponent]
        self.faults = faults
    }

    private init(anchorURL: URL?, storeRootComponents: [String]) {
        self.anchorURL = anchorURL
        self.storeRootComponents = storeRootComponents
        self.faults = .none
    }

    /// The ONLY public production factory. Fails closed (no temporary-directory fallback) when
    /// Application Support cannot be resolved. Application Support is the fixed trusted anchor; both
    /// store-owned levels below it are created and revalidated by descriptor.
    public static func productionDefault() -> ConfirmedScientificImportManifestStore {
        guard let base = FileManager.default
            .urls(for: .applicationSupportDirectory, in: .userDomainMask).first else {
            return ConfirmedScientificImportManifestStore(anchorURL: nil, storeRootComponents: [])
        }
        return ConfirmedScientificImportManifestStore(
            anchorURL: base,
            storeRootComponents: ["SpikeTrainPatternDetectorMac", "confirmed-scientific-import-manifests"]
        )
    }

    /// The store root URL (anchor + store-owned components), used ONLY to build result/summary URLs.
    private var rootDirectory: URL? {
        guard let anchorURL else { return nil }
        return storeRootComponents.reduce(anchorURL) { $0.appendingPathComponent($1, isDirectory: true) }
    }

    // MARK: - Save (mints standing)

    public func save(
        baseConfirmation: ConfirmedScientificImportManifest
    ) async throws -> StoredSaveResult {
        try requireRoot()
        // Derive the receipt (header rule included) from the sealed base BEFORE creating any directory,
        // temporary file, or final artifact. A contradictory (headerful base ↔ headerless) save is
        // unrepresentable: there is no caller-supplied receipt or header rule to disagree with the base.
        let receipt: ConfirmedScientificImportReceipt
        do {
            receipt = try ConfirmedScientificImportManifestPersistence.project(from: baseConfirmation)
        } catch let error as PersistedHeaderRuleDerivationError {
            throw StoreError.headerRuleDerivation(error)
        }
        let sourceSHA = receipt.sourceTransactionBinding.sourceBytesSHA256
        let recordDigest = receipt.confirmationRecordDigest
        guard isLowercaseSHA256(sourceSHA) else { throw StoreError.invalidSourceSHA }
        guard isLowercaseSHA256(recordDigest) else { throw StoreError.invalidRecordDigest }

        let bytes = ConfirmedScientificImportManifestPersistence.encode(receipt)
        // Reject oversized/noncanonical encodings BEFORE creating any final artifact.
        guard bytes.count <= ConfirmedScientificImportManifestPersistence.maximumFileByteCount,
              (try? ConfirmedScientificImportManifestPersistence.decode(bytes)) == receipt else {
            throw StoreError.oversizedOrNoncanonicalEncoding
        }
        let targetName = "\(recordDigest).\(Self.fileExtension)"

        return try await withAnchoredSourceDirectory(sourceSHA: sourceSHA, create: true) { chain in
            try await self.withStoreLock(chain) {
                let outcome: StoredSaveResult.Outcome
                if try self.fileExists(in: chain.source, name: targetName) {
                    try self.requireExistingMatches(chain, name: targetName, receipt: receipt)
                    outcome = .idempotentExisting
                } else {
                    guard try self.candidateCount(chain.source, excluding: targetName) < Self.maximumCandidatesPerSource else {
                        throw StoreError.candidateLimitExceeded(maximum: Self.maximumCandidatesPerSource)
                    }
                    outcome = try self.writePublishAndClean(chain, targetName: targetName, receipt: receipt, bytes: bytes)
                }

                let manifest = try self.establishDurableStanding(
                    chain, targetName: targetName, receipt: receipt,
                    baseConfirmation: baseConfirmation
                )
                let url = chain.sourceURL.appendingPathComponent(targetName, isDirectory: false)
                return StoredSaveResult(url: url, manifest: manifest, outcome: outcome)
            }
        }
    }

    /// Writes a unique temporary file and publishes it with no-replace. Once `O_EXCL` creation succeeds,
    /// EVERY error path — write/fsync/close, cancellation, publish, hook throws — flows through the ONE
    /// cleanup path, which propagates `temporaryCleanupFailed` if the unlink itself fails (never ignored).
    /// A successful publish leaves the immutable final record for a safe retry.
    private func writePublishAndClean(
        _ chain: AnchoredChain, targetName: String,
        receipt: ConfirmedScientificImportReceipt, bytes: [UInt8]
    ) throws -> StoredSaveResult.Outcome {
        let tempName = ".\(receipt.confirmationRecordDigest).\(deterministicTempSuffix()).tmp"
        try faults.beforeTempWrite?()
        // Create the O_EXCL temporary. If this fails, NO temp exists — there is nothing to clean.
        let tempFD = try createExclusiveTemporary(in: chain.source, name: tempName)

        // From here the temporary file exists; every non-consuming exit runs the single cleanup path.
        let outcome: StoredSaveResult.Outcome
        do {
            try writeAllSyncAndClose(tempFD, bytes: bytes)   // write + fsync + close; throws → cleanup
            try faults.afterTempFsync?()
            // Cancellation BEFORE publication cleans the temp and returns no wrapper.
            if Task.isCancelled { throw StoreError.operationCancelled }
            try faults.beforePublishLink?()
            // Recheck cancellation AFTER the final pre-link hook and IMMEDIATELY before the link, so a
            // cancellation observed in this window leaves no final record.
            if Task.isCancelled { throw StoreError.operationCancelled }
            switch publishNoReplace(temp: tempName, target: targetName, in: chain.source) {
            case .published:
                outcome = .written
            case .targetAppearedConcurrently:
                try requireExistingMatches(chain, name: targetName, receipt: receipt)
                outcome = .idempotentExisting
            case .failed:
                throw StoreError.publishFailed
            }
        } catch {
            // Every failure runs cleanup. If cleanup itself fails, surface that rather than claiming the
            // store is clean; otherwise re-throw the original fail-closed error.
            do { try cleanUpTemporary(chain.source, name: tempName) }
            catch { throw StoreError.temporaryCleanupFailed }
            throw error
        }
        // Consumed or superseded: remove the temporary NAME (its inode lives on as the published record
        // when written). A cleanup failure aborts the mint; the visible, retryable record has not yet
        // completed the directory durability barrier and can be made durable by a clean retry.
        try cleanUpTemporary(chain.source, name: tempName)
        faults.afterPublishBeforeBarrier?()
        return outcome
    }

    // MARK: - Restore (mints standing)

    public func restoreVerify(
        sourceSHA256 sourceSHA: String,
        recordDigest: String,
        baseConfirmation: ConfirmedScientificImportManifest
    ) async throws -> PersistedConfirmedScientificImportManifest {
        try requireRoot()
        guard isLowercaseSHA256(sourceSHA) else { throw StoreError.invalidSourceSHA }
        guard isLowercaseSHA256(recordDigest) else { throw StoreError.invalidRecordDigest }
        let targetName = "\(recordDigest).\(Self.fileExtension)"

        return try await withAnchoredSourceDirectory(sourceSHA: sourceSHA, create: false) { chain in
            try await self.withStoreLock(chain) {
                // Restore runs the SAME durability barrier under the SAME lock, so it never races the
                // interval after a concurrent writer publishes a link but before that writer finishes
                // its barrier. The header rule is derived from the replayed base inside the barrier;
                // `receipt.headerRule` on disk is an untrusted replay decision, never trusted here.
                let receipt = try self.readAnchoredRecord(chain, name: targetName, sourceSHA: sourceSHA, recordDigest: recordDigest)
                return try self.establishDurableStanding(
                    chain, targetName: targetName, receipt: receipt,
                    baseConfirmation: baseConfirmation
                )
            }
        }
    }

    /// Reads a specific saved record from the store's derived path and returns an UNTRUSTED bare
    /// receipt for driving replay. It carries no capability and removes no readiness blocker.
    public func readForReplay(
        sourceSHA256 sourceSHA: String,
        recordDigest: String
    ) async throws -> ConfirmedScientificImportReceipt {
        try requireRoot()
        guard isLowercaseSHA256(sourceSHA) else { throw StoreError.invalidSourceSHA }
        guard isLowercaseSHA256(recordDigest) else { throw StoreError.invalidRecordDigest }
        let targetName = "\(recordDigest).\(Self.fileExtension)"
        return try await withAnchoredSourceDirectory(sourceSHA: sourceSHA, create: false) { chain in
            try self.readAnchoredRecord(chain, name: targetName, sourceSHA: sourceSHA, recordDigest: recordDigest)
        }
    }

    // MARK: - Discover (bounded, incremental)

    public func discover(sourceSHA256 sourceSHA: String) async throws -> ManifestDiscovery {
        try requireRoot()
        guard isLowercaseSHA256(sourceSHA) else { throw StoreError.invalidSourceSHA }
        do {
            return try await withAnchoredSourceDirectory(sourceSHA: sourceSHA, create: false) { chain in
                try self.discoverBounded(chain, sourceSHA: sourceSHA)
            }
        } catch StoreError.recordNotFound {
            // A genuinely ABSENT store-owned directory (the store subtree, `by-source`, or this source
            // has never been created) is an empty, healthy store — NOT an error. Unsafe or unreadable
            // ancestors (symlink/non-directory) surface as their own errors and are not caught here.
            return ManifestDiscovery(summaries: [], unreadableRecordCount: 0, entriesInspected: 0, workBytesCharged: 0)
        }
    }

    // MARK: - Shared durability barrier

    private func establishDurableStanding(
        _ chain: AnchoredChain,
        targetName: String,
        receipt: ConfirmedScientificImportReceipt,
        baseConfirmation: ConfirmedScientificImportManifest
    ) throws -> PersistedConfirmedScientificImportManifest {
        // 1. Open + verify the final regular non-symlink file (no-follow, non-blocking so a substituted
        //    FIFO/device cannot stall the open), then synchronize it. This descriptor is the one whose
        //    identity is rebound to the current entry immediately before minting.
        let finalFD = openUntrustedLeaf(chain.source, name: targetName)
        guard finalFD >= 0 else {
            throw errno == ELOOP ? StoreError.unsafePathObject : StoreError.recordNotFound
        }
        defer { close(finalFD) }
        guard isRegularFile(finalFD) else { throw StoreError.unsafePathObject }
        try faults.beforeFinalFileFsync?()   // exercises the same branch as a real file-fsync failure
        guard fsync(finalFD) == 0 else { throw StoreError.durabilitySyncFailed }

        // 2. Synchronize the complete FIXED directory chain child-to-parent through the anchor (always the
        //    whole chain — a previous failed attempt may have created visible-but-not-durable links, and
        //    the anchor's entry now names the highest newly created directory).
        for (depthFromSource, fd) in chain.fsyncChain {
            guard fsync(fd) == 0 else { throw StoreError.durabilitySyncFailed }
            do { try faults.afterDirectorySync?(depthFromSource) } catch { throw StoreError.durabilitySyncFailed }
        }

        // 3. Bounded readback of the final file and exact comparison.
        try faults.beforeFinalReadback?()
        let bytes = try boundedRead(finalFD, limit: ConfirmedScientificImportManifestPersistence.maximumFileByteCount)
        let decoded: ConfirmedScientificImportReceipt
        do {
            decoded = try ConfirmedScientificImportManifestPersistence.decode(bytes)
        } catch let error as PersistedReceiptDecodingError {
            throw StoreError.read(.decode(error))
        }
        guard decoded == receipt,
              decoded.sourceTransactionBinding.sourceBytesSHA256 == receipt.sourceTransactionBinding.sourceBytesSHA256,
              decoded.confirmationRecordDigest == receipt.confirmationRecordDigest else {
            throw StoreError.readbackMismatch
        }

        // 4. Rebind EVERY store-owned directory descriptor to its current entry (so a renamed/replaced
        //    root or by-source is caught), and rebind the final record name to the same regular-file
        //    inode we synchronized and read, immediately before minting. An open descriptor proves only
        //    which inode was opened, not that the pathname still names it.
        try faults.beforeIdentityRevalidation?()
        try revalidateChainIdentity(chain)
        try revalidateFinalRecord(sourceFD: chain.source, name: targetName, finalFD: finalFD)

        // 5. Post-publication cancellation: the record is now fully durable and identity-verified. If the
        //    operation was cancelled, report cancellation WITHOUT minting in-memory standing. The durable
        //    record remains for a later explicit retry.
        if Task.isCancelled { throw StoreError.operationCancelled }

        // 6. Mint only now, with the opaque capability that only this file can construct. `verified`
        //    re-derives the header rule from the base and projects the expected receipt exclusively
        //    from the base, comparing it exactly against the decoded on-disk record.
        let verification: Result<PersistedConfirmedScientificImportManifest, PersistedConfirmationVerificationError>
        do {
            verification = try PersistedConfirmedScientificImportManifest.verified(
                baseConfirmation: baseConfirmation,
                decodedReceipt: decoded, durability: DurableRecordCapability()
            )
        } catch let error as PersistedHeaderRuleDerivationError {
            throw StoreError.headerRuleDerivation(error)
        }
        switch verification {
        case .success(let manifest):
            return manifest
        case .failure(let error):
            throw StoreError.verification(error)
        }
    }

    // MARK: - Identity revalidation (rebind descriptors to the current entries)

    /// Verifies every store-owned directory descriptor (root, by-source, and the per-source directory)
    /// still matches the entry by which its parent names it (device/inode/type), and every level remains
    /// a live directory — so a renamed/replaced root or by-source directory, or a source directory that
    /// no longer belongs to the anchored chain, fails with `pathIdentityChanged`.
    private func revalidateChainIdentity(_ chain: AnchoredChain) throws {
        for level in chain.below {
            var entry = stat()
            guard level.name.withCString({ fstatat(level.parentFD, $0, &entry, AT_SYMLINK_NOFOLLOW) }) == 0 else {
                throw StoreError.pathIdentityChanged
            }
            var current = stat()
            guard fstat(level.fd, &current) == 0 else { throw StoreError.pathIdentityChanged }
            guard (entry.st_mode & S_IFMT) == S_IFDIR, (current.st_mode & S_IFMT) == S_IFDIR,
                  entry.st_dev == current.st_dev, entry.st_ino == current.st_ino,
                  current.st_nlink >= 1 else {
                throw StoreError.pathIdentityChanged
            }
        }
    }

    /// Verifies the current final-record name still identifies the same regular-file inode as the
    /// descriptor that was synchronized and read (device + inode + regular-file type, no-follow).
    private func revalidateFinalRecord(sourceFD: Int32, name: String, finalFD: Int32) throws {
        var entry = stat()
        guard name.withCString({ fstatat(sourceFD, $0, &entry, AT_SYMLINK_NOFOLLOW) }) == 0 else {
            throw StoreError.pathIdentityChanged
        }
        var current = stat()
        guard fstat(finalFD, &current) == 0 else { throw StoreError.pathIdentityChanged }
        guard (entry.st_mode & S_IFMT) == S_IFREG, (current.st_mode & S_IFMT) == S_IFREG,
              entry.st_dev == current.st_dev, entry.st_ino == current.st_ino else {
            throw StoreError.pathIdentityChanged
        }
    }

    // MARK: - Anchored directory chain (fixed trusted anchor)

    private struct Level {
        let fd: Int32
        let parentFD: Int32
        let name: String
    }

    /// The fixed trusted anchor descriptor plus every store-owned directory descriptor beneath it,
    /// ordered anchor-child first … source last. `below` always contains the store root, `by-source`,
    /// and the per-source directory (never empty), so every store-owned level is revalidated.
    private struct AnchoredChain {
        let anchorFD: Int32
        let below: [Level]
        let sourceURL: URL

        var source: Int32 { below.last?.fd ?? anchorFD }

        /// Directory descriptors in child-to-parent order with their depth from the source (0 = source),
        /// ending at the anchor — the exact synchronization order.
        var fsyncChain: [(depthFromSource: Int, fd: Int32)] {
            var result: [(Int, Int32)] = []
            var depth = 0
            for level in below.reversed() {
                result.append((depth, level.fd))
                depth += 1
            }
            result.append((depth, anchorFD))
            return result
        }

        func closeAll() {
            for level in below.reversed() { close(level.fd) }
            close(anchorFD)
        }
    }

    /// Opens the anchored chain to the per-source directory using the FIXED trusted anchor (never a
    /// dynamic deepest-existing ancestor). The anchor is opened once, path-based and no-follow; every
    /// store-owned descendant beneath it — the store root, `by-source`, and the per-source directory — is
    /// created/opened by descriptor (`mkdirat`/`openat`, `O_NOFOLLOW`), so a symlink cannot be
    /// substituted for the store subtree and no create/open error is swallowed. Runs `body`, then closes
    /// all descriptors.
    private func withAnchoredSourceDirectory<T>(
        sourceSHA: String, create: Bool, _ body: (AnchoredChain) async throws -> T
    ) async throws -> T {
        guard let anchorURL, let root = rootDirectory else { throw StoreError.durabilityUnavailable }
        let sourceDirURL = root
            .appendingPathComponent("by-source", isDirectory: true)
            .appendingPathComponent(sourceSHA, isDirectory: true)
        let components = storeRootComponents + ["by-source", sourceSHA]

        let anchorFD = openDirectoryNoFollow(anchorURL)
        guard anchorFD >= 0 else {
            throw (errno == ELOOP || errno == ENOTDIR)
                ? StoreError.unsafePathObject
                : StoreError.durabilitySyncFailed
        }
        guard isDirectory(anchorFD) else { close(anchorFD); throw StoreError.unsafePathObject }

        var below: [Level] = []
        var parent = anchorFD
        for name in components {
            let fd: Int32
            do {
                fd = try openChildDirectory(parent: parent, name: name, create: create)
            } catch {
                for level in below.reversed() { close(level.fd) }
                close(anchorFD)
                throw error
            }
            below.append(Level(fd: fd, parentFD: parent, name: name))
            parent = fd
        }

        let chain = AnchoredChain(anchorFD: anchorFD, below: below, sourceURL: sourceDirURL)
        defer { chain.closeAll() }
        return try await body(chain)
    }

    private func openDirectoryNoFollow(_ url: URL) -> Int32 {
        url.withUnsafeFileSystemRepresentation { path in
            guard let path else { return -1 }
            return open(path, O_DIRECTORY | O_RDONLY | O_NOFOLLOW)
        }
    }

    private func openChildDirectory(parent: Int32, name: String, create: Bool) throws -> Int32 {
        var fd = openAt(parent, name: name, flags: O_DIRECTORY | O_RDONLY | O_NOFOLLOW)
        if fd < 0, errno == ENOENT {
            guard create else { throw StoreError.recordNotFound }
            if mkdirAt(parent, name: name, mode: 0o700) != 0, errno != EEXIST {
                throw StoreError.durabilitySyncFailed
            }
            fd = openAt(parent, name: name, flags: O_DIRECTORY | O_RDONLY | O_NOFOLLOW)
        }
        guard fd >= 0 else {
            // A non-ENOENT failure on this no-follow directory open means an object exists at the name
            // that is not the plain directory we require: a symlink (ELOOP without O_DIRECTORY, or
            // ENOTDIR because O_DIRECTORY rejects the symlink's own type first) or any other
            // non-directory (ENOTDIR). All are unsafe path objects, never real durability faults.
            throw (errno == ELOOP || errno == ENOTDIR)
                ? StoreError.unsafePathObject
                : StoreError.durabilitySyncFailed
        }
        guard isDirectory(fd) else {
            close(fd)
            throw StoreError.unsafePathObject
        }
        return fd
    }

    // MARK: - Existence / read helpers (anchored, non-blocking untrusted leaves)

    private func fileExists(in dirFD: Int32, name: String) throws -> Bool {
        var status = stat()
        let result = name.withCString { fstatat(dirFD, $0, &status, AT_SYMLINK_NOFOLLOW) }
        if result == 0 { return true }
        if errno == ENOENT { return false }
        throw StoreError.durabilitySyncFailed
    }

    /// Opens an untrusted store leaf for reading with no-follow, non-blocking, and close-on-exec
    /// semantics, so a substituted FIFO or device special file returns immediately instead of blocking
    /// the open, letting the caller `fstat`-reject it promptly.
    private func openUntrustedLeaf(_ dirFD: Int32, name: String) -> Int32 {
        openAt(dirFD, name: name, flags: O_RDONLY | O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC)
    }

    private func readAnchoredRecord(
        _ chain: AnchoredChain, name: String, sourceSHA: String, recordDigest: String
    ) throws -> ConfirmedScientificImportReceipt {
        let fd = openUntrustedLeaf(chain.source, name: name)
        guard fd >= 0 else {
            throw errno == ELOOP ? StoreError.unsafePathObject : StoreError.recordNotFound
        }
        defer { close(fd) }
        guard isRegularFile(fd) else { throw StoreError.unsafePathObject }
        let bytes = try boundedRead(fd, limit: ConfirmedScientificImportManifestPersistence.maximumFileByteCount)
        let receipt: ConfirmedScientificImportReceipt
        do {
            receipt = try ConfirmedScientificImportManifestPersistence.decode(bytes)
        } catch let error as PersistedReceiptDecodingError {
            throw StoreError.read(.decode(error))
        }
        guard receipt.confirmationRecordDigest == recordDigest,
              receipt.sourceTransactionBinding.sourceBytesSHA256 == sourceSHA else {
            throw StoreError.recordNotFound
        }
        return receipt
    }

    private func requireExistingMatches(
        _ chain: AnchoredChain, name: String, receipt: ConfirmedScientificImportReceipt
    ) throws {
        let fd = openUntrustedLeaf(chain.source, name: name)
        guard fd >= 0 else { throw StoreError.conflictingBytesAtTarget }
        defer { close(fd) }
        guard isRegularFile(fd),
              let bytes = try? boundedRead(fd, limit: ConfirmedScientificImportManifestPersistence.maximumFileByteCount),
              let decoded = try? ConfirmedScientificImportManifestPersistence.decode(bytes),
              decoded == receipt else {
            throw StoreError.conflictingBytesAtTarget
        }
    }

    /// Counts candidate records, charging EVERY entry (relevant, irrelevant, and temporary) against the
    /// deterministic total-entry budget, so a directory full of irrelevant filenames cannot force
    /// unbounded work. Stops early once the per-source candidate cap is reached.
    private func candidateCount(_ dirFD: Int32, excluding excludeName: String) throws -> Int {
        var candidates = 0
        var entries = 0
        try enumerateEntries(dirFD) { name, _ in
            entries += 1
            if entries > Self.maximumDiscoveryEntries { throw StoreError.discoveryBudgetExceeded }
            guard name.hasSuffix(".\(Self.fileExtension)"), name != excludeName else { return true }
            candidates += 1
            return candidates < Self.maximumCandidatesPerSource
        }
        return candidates
    }

    // MARK: - Bounded incremental discovery

    private func discoverBounded(_ chain: AnchoredChain, sourceSHA: String) throws -> ManifestDiscovery {
        var summaries: [StoredManifestSummary] = []
        var unreadable = 0
        var entries = 0
        var workBytes = 0
        let byteLimit = ConfirmedScientificImportManifestPersistence.maximumFileByteCount

        try enumerateEntries(chain.source) { name, dirFD in
            entries += 1
            guard entries <= Self.maximumDiscoveryEntries else {
                throw StoreError.discoveryBudgetExceeded
            }
            guard name.hasSuffix(".\(Self.fileExtension)") else { return true }

            // Open first, no-follow AND non-blocking, so a substituted FIFO/device cannot block; reject
            // any non-regular object promptly.
            let fd = self.openUntrustedLeaf(dirFD, name: name)
            guard fd >= 0 else { unreadable += 1; return true }
            defer { close(fd) }
            var status = stat()
            guard self.isRegularFile(fd), fstat(fd, &status) == 0 else { unreadable += 1; return true }

            let remaining = Self.maximumDiscoveryBytes - workBytes
            guard remaining > 0 else { throw StoreError.discoveryBudgetExceeded }
            let probedSize = Int(clamping: status.st_size)

            // Obviously oversized by its own claimed size: charge the per-file cap WITHOUT reading it, so
            // a multi-GiB file is never read merely to reject it.
            if probedSize > byteLimit {
                let charge = Swift.min(byteLimit + 1, remaining)
                workBytes += charge
                unreadable += 1
                guard charge >= byteLimit + 1 else { throw StoreError.discoveryBudgetExceeded }
                return true
            }

            // Read bounded by BOTH the per-file limit and the remaining global budget, charging the
            // ACTUAL bytes read — so a file that GREW after its size probe cannot bypass the byte budget.
            do { try self.faults.afterCandidateSizeProbe?() } catch { unreadable += 1; return true }
            let readCap = Swift.min(byteLimit + 1, remaining)
            let bytes: [UInt8]
            do {
                // Bytes read before any failure are already charged into `workBytes`.
                bytes = try self.readUpTo(fd, maxBytes: readCap, charged: &workBytes)
            } catch {
                // A read error (e.g. EIO) after a valid prefix must NEVER be listed as a valid summary.
                unreadable += 1
                return true
            }
            if bytes.count >= readCap, readCap < byteLimit + 1 {
                // Stopped only because the global budget ran out mid-file: too large to scan safely.
                throw StoreError.discoveryBudgetExceeded
            }
            guard bytes.count <= byteLimit,
                  let receipt = try? ConfirmedScientificImportManifestPersistence.decode(bytes),
                  receipt.confirmationRecordDigest == name.replacingOccurrences(of: ".\(Self.fileExtension)", with: ""),
                  receipt.sourceTransactionBinding.sourceBytesSHA256 == sourceSHA else {
                unreadable += 1
                return true
            }
            let url = chain.sourceURL.appendingPathComponent(name, isDirectory: false)
            summaries.append(StoredManifestSummary(url: url, receipt: receipt))
            return true
        }
        return ManifestDiscovery(
            summaries: summaries.sorted { $0.recordDigest < $1.recordDigest },
            unreadableRecordCount: unreadable,
            entriesInspected: entries,
            workBytesCharged: workBytes
        )
    }

    /// Enumerates directory entries incrementally via `fdopendir`/`readdir`, never materializing the
    /// whole directory into an array. `readdir` returning `nil` is disambiguated by `errno`: a clean EOF
    /// (`errno == 0`) ends enumeration, while a non-zero `errno` is surfaced as a failure and never
    /// treated as successful completion. `visit` returns `false` to stop early.
    private func enumerateEntries(_ dirFD: Int32, _ visit: (String, Int32) throws -> Bool) throws {
        let dupFD = dup(dirFD)
        guard dupFD >= 0 else { throw StoreError.storeUnreadable }
        guard let dir = fdopendir(dupFD) else {
            close(dupFD)
            throw StoreError.storeUnreadable
        }
        defer { closedir(dir) }
        rewinddir(dir)
        while true {
            do { try faults.duringEnumeration?() } catch { throw StoreError.storeUnreadable }
            errno = 0
            guard let entry = readdir(dir) else {
                if errno != 0 { throw StoreError.storeUnreadable }   // real error, not clean EOF
                break
            }
            let name = withUnsafeBytes(of: entry.pointee.d_name) { raw -> String in
                let bytes = raw.prefix { $0 != 0 }
                return String(decoding: bytes, as: UTF8.self)
            }
            if name == "." || name == ".." { continue }
            if try !visit(name, dirFD) { return }
        }
    }

    // MARK: - Write / publish / cleanup (anchored, single cleanup path)

    /// Creates the unique `O_EXCL` temporary file and returns its descriptor. A failure here means no
    /// temp exists, so there is nothing to clean.
    private func createExclusiveTemporary(in dirFD: Int32, name: String) throws -> Int32 {
        let fd = openAt(dirFD, name: name, flags: O_CREAT | O_EXCL | O_WRONLY | O_NOFOLLOW, mode: 0o600)
        guard fd >= 0 else { throw StoreError.writeFailed }
        return fd
    }

    /// Writes all bytes, `fsync`s, and closes the temporary descriptor. Always closes the descriptor
    /// (never leaks it) and throws `writeFailed` on any write/fsync/close failure, leaving the temporary
    /// file for the caller's single cleanup path.
    private func writeAllSyncAndClose(_ fd: Int32, bytes: [UInt8]) throws {
        var ok = true
        bytes.withUnsafeBytes { raw in
            guard let base = raw.baseAddress else { return }
            var offset = 0
            while offset < raw.count {
                let written = write(fd, base.advanced(by: offset), raw.count - offset)
                if written <= 0 { ok = false; break }
                offset += written
            }
        }
        if ok { ok = (fsync(fd) == 0) }
        // Deterministically exercise the write/fsync/close failure branch without a partial write.
        let injected: Bool
        do { try faults.duringTempWriteSync?(); injected = false } catch { injected = true }
        let closed = close(fd) == 0
        guard ok, closed, !injected else { throw StoreError.writeFailed }
    }

    /// Removes a temporary file, tolerating an already-absent entry. Any other failure (or an injected
    /// cleanup fault) throws so the caller can surface `temporaryCleanupFailed` rather than leak a temp.
    /// The real `unlinkat` result is NEVER ignored.
    private func cleanUpTemporary(_ dirFD: Int32, name: String) throws {
        try faults.duringTempCleanup?()   // exercises the same branch as a real `unlinkat` failure
        if unlinkAt(dirFD, name: name) != 0, errno != ENOENT {
            throw StoreError.temporaryCleanupFailed
        }
    }

    private enum PublishResult { case published, targetAppearedConcurrently, failed }

    private func publishNoReplace(temp: String, target: String, in dirFD: Int32) -> PublishResult {
        let result = temp.withCString { tempPtr in
            target.withCString { targetPtr in
                linkat(dirFD, tempPtr, dirFD, targetPtr, 0)
            }
        }
        if result == 0 { return .published }
        return errno == EEXIST ? .targetAppearedConcurrently : .failed
    }

    // MARK: - Namespace-stable store lock (on the FIXED trusted anchor descriptor)

    /// Acquires an exclusive advisory lock on the FIXED TRUSTED ANCHOR directory descriptor — a
    /// namespace-stable inode OUTSIDE every store-owned directory that is never renamed/replaced by the
    /// store. Because the lock is rooted at the anchor rather than the per-source directory, it cannot
    /// split into distinct old/new critical sections during an ABA rename/replace/restore of the source
    /// subtree: only one operation for a given store root can hold it at a time, so two cooperating
    /// instances can never occupy the critical section concurrently.
    ///
    /// This is a STORE-GLOBAL lock (it serializes ALL manifest persistence operations for one store
    /// root, not per source); manifest records are small and correctness is favored here. It is held for
    /// the COMPLETE save/restore critical section — through candidate counting, publication, the
    /// durability barrier, readback, identity verification, and mint/no-mint completion. Acquisition is
    /// cooperative (`Task.sleep`, never a blocking `usleep` on the executor) and cancels promptly; after
    /// acquisition the whole store-owned chain's identity is revalidated.
    private func withStoreLock<T>(_ chain: AnchoredChain, _ body: () throws -> T) async throws -> T {
        try faults.beforeLockAcquire?()
        let deadline = DispatchTime.now().uptimeNanoseconds
            + (faults.lockTimeoutOverrideNanoseconds ?? Self.lockTimeoutNanoseconds)
        while true {
            if Task.isCancelled { throw StoreError.operationCancelled }
            if flock(chain.anchorFD, LOCK_EX | LOCK_NB) == 0 { break }
            guard errno == EWOULDBLOCK || errno == EAGAIN || errno == EINTR else {
                throw StoreError.durabilitySyncFailed
            }
            guard DispatchTime.now().uptimeNanoseconds < deadline else { throw StoreError.lockTimeout }
            do {
                try await Task.sleep(nanoseconds: Self.lockRetryIntervalNanoseconds)
            } catch {
                throw StoreError.operationCancelled
            }
        }
        defer { flock(chain.anchorFD, LOCK_UN) }
        // Lock identity validation: the whole store-owned chain (root, by-source, source) must still be
        // the entries their parents name — otherwise a level was rmdir'd/replaced under us.
        try faults.afterLockAcquired?()
        try revalidateChainIdentity(chain)
        return try body()
    }

    // MARK: - Low-level syscall + validation helpers

    private func requireRoot() throws {
        if anchorURL == nil { throw StoreError.durabilityUnavailable }
    }

    /// A per-invocation unique temporary suffix without `Date`/`Math.random`. `UUID()` is a fresh random
    /// v4 identifier, sufficient to make the `O_EXCL` temporary name collision-free.
    private func deterministicTempSuffix() -> String { UUID().uuidString }

    private func openAt(_ dirFD: Int32, name: String, flags: Int32, mode: mode_t = 0) -> Int32 {
        name.withCString { openat(dirFD, $0, flags, mode) }
    }

    private func mkdirAt(_ dirFD: Int32, name: String, mode: mode_t) -> Int32 {
        name.withCString { mkdirat(dirFD, $0, mode) }
    }

    private func unlinkAt(_ dirFD: Int32, name: String) -> Int32 {
        name.withCString { unlinkat(dirFD, $0, 0) }
    }

    private func isDirectory(_ fd: Int32) -> Bool {
        var status = stat()
        return fstat(fd, &status) == 0 && (status.st_mode & S_IFMT) == S_IFDIR
    }

    private func isRegularFile(_ fd: Int32) -> Bool {
        var status = stat()
        return fstat(fd, &status) == 0 && (status.st_mode & S_IFMT) == S_IFREG
    }

    /// Reads up to `maxBytes` bytes from `fd`, charging each byte actually read into `charged`, and
    /// distinguishing a clean EOF (`read == 0`) from a read error: `EINTR` retries, and any other read
    /// failure THROWS so the caller can mark the candidate unreadable rather than mistake a truncated,
    /// error-terminated stream for a valid file. It never allocates or reads more than `maxBytes`.
    private func readUpTo(_ fd: Int32, maxBytes: Int, charged: inout Int) throws -> [UInt8] {
        var buffer: [UInt8] = []
        guard maxBytes > 0 else { return buffer }
        var chunk = [UInt8](repeating: 0, count: Swift.min(64 * 1_024, maxBytes))
        while buffer.count < maxBytes {
            let want = Swift.min(chunk.count, maxBytes - buffer.count)
            let readCount = chunk.withUnsafeMutableBytes { read(fd, $0.baseAddress, want) }
            if readCount > 0 {
                buffer.append(contentsOf: chunk[0..<readCount])
                charged += readCount
                try faults.duringCandidateRead?(buffer.count)   // exercises the mid-file read-error branch
            } else if readCount == 0 {
                break   // clean EOF
            } else if errno == EINTR {
                continue   // interrupted; retry
            } else {
                throw StoreError.read(.ioError)   // a genuine read failure is never treated as EOF
            }
        }
        return buffer
    }

    private func boundedRead(_ fd: Int32, limit: Int) throws -> [UInt8] {
        var buffer: [UInt8] = []
        var chunk = [UInt8](repeating: 0, count: 64 * 1_024)
        while buffer.count <= limit {
            let want = Swift.min(chunk.count, (limit + 1) - buffer.count)
            let readCount = chunk.withUnsafeMutableBytes { read(fd, $0.baseAddress, want) }
            if readCount < 0 { throw StoreError.read(.ioError) }
            if readCount == 0 { break }
            buffer.append(contentsOf: chunk[0..<readCount])
        }
        guard buffer.count <= limit else { throw StoreError.read(.fileTooLarge(maximumBytes: limit)) }
        return buffer
    }

    private func isLowercaseSHA256(_ value: String) -> Bool {
        value.count == 64 && value.utf8.allSatisfy { byte in
            (byte >= 0x30 && byte <= 0x39) || (byte >= 0x61 && byte <= 0x66)
        }
    }
}

private typealias StoreError = ConfirmedScientificImportManifestStoreError
