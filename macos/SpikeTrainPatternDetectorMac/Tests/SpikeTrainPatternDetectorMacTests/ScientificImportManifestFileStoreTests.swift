import Foundation
@testable import SpikeTrainPatternDetectorMac
@testable import STPDCore
import Testing

// Serialized: several tests deliberately hold the real store-global anchor `flock` and let the store's bounded
// lock spin (or block a hook) to exercise timeout/cancellation/publish→durability races. Running those
// concurrently would block multiple cooperative-pool threads at once and stall the coordination that
// releases them. Intra-test `async let` concurrency (the two-store tests) is unaffected by this trait.
@Suite("Confirmed scientific import manifest store", .serialized)
struct ScientificImportManifestFileStoreTests {
    enum StoreTestError: Error { case rejected }

    /// Thread-safe recorder for the order of `afterDirectorySync` depth callbacks (a `@Sendable` sync
    /// hook). The depth is measured from the source directory (0 = source), increasing toward the anchor.
    final class RoleRecorder: @unchecked Sendable {
        private let lock = NSLock()
        private var storage: [Int] = []
        func append(_ depth: Int) {
            lock.lock(); storage.append(depth); lock.unlock()
        }
        var depths: [Int] {
            lock.lock(); defer { lock.unlock() }; return storage
        }
    }

    // MARK: - Helpers

    /// Holds an exclusive `flock` on the given directory for the duration of `body`, so the store's
    /// bounded cooperative lock (which locks that same directory descriptor — the fixed anchor) contends
    /// against a real holder. The directory must already exist.
    private func holdingExclusiveDirectoryFlock<T>(at dir: URL, _ body: () async throws -> T) async throws -> T {
        let fd = dir.path.withCString { open($0, O_DIRECTORY | O_RDONLY | O_NOFOLLOW) }
        #expect(fd >= 0)
        #expect(flock(fd, LOCK_EX) == 0)
        defer { flock(fd, LOCK_UN); close(fd) }
        return try await body()
    }

    /// Creates a sparse file of the given logical size (no data blocks allocated) so oversized-file
    /// handling can be exercised without writing gigabytes.
    private func writeSparseFile(at url: URL, logicalSize: UInt64) throws {
        FileManager.default.createFile(atPath: url.path, contents: nil)
        let handle = try FileHandle(forWritingTo: url)
        defer { try? handle.close() }
        try handle.truncate(atOffset: logicalSize)
    }

    private func sourceDirectoryURL(root: URL, sha: String) -> URL {
        root.appendingPathComponent("by-source", isDirectory: true)
            .appendingPathComponent(sha, isDirectory: true)
    }

    /// Awaits a `DispatchSemaphore` from an async context without blocking a cooperative-pool thread:
    /// the blocking wait runs on a global queue and resumes the continuation. Deterministic — no
    /// `Task.yield` or timed sleeps — used to coordinate the publish→durability race precisely.
    private func awaitSignal(_ semaphore: DispatchSemaphore) async {
        await withCheckedContinuation { continuation in
            DispatchQueue.global().async {
                semaphore.wait()
                continuation.resume()
            }
        }
    }

    /// The store's fixed trusted anchor for a store root is the root's parent directory.
    private func anchorURL(forStoreRoot root: URL) -> URL { root.deletingLastPathComponent() }

    private func withTempRoot<T>(_ body: (URL) async throws -> T) async throws -> T {
        // Give each test a UNIQUE trusted anchor (`.../ManifestStoreTests/<uuid>`) and put the store root
        // (`store`) beneath it. The store-global lock is rooted at the anchor, so a unique anchor per test
        // keeps the serialized suite's tests from sharing one lock inode. The store creates the root leaf,
        // `by-source`, and the per-source directory, giving a fixed 4-level chain (source, by-source,
        // root, anchor).
        let anchor = FileManager.default.temporaryDirectory
            .appendingPathComponent("ManifestStoreTests", isDirectory: true)
            .appendingPathComponent(UUID().uuidString, isDirectory: true)
        let root = anchor.appendingPathComponent("store", isDirectory: true)
        try FileManager.default.createDirectory(at: anchor, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: anchor) }
        return try await body(root)
    }

    /// Builds a real confirmed manifest through the import chain and its projected receipt, so the
    /// store's mint (which matches the readback against the base) has a genuine base to verify.
    private func makeConfirmed(
        bindingCharacter: Character = "a",
        segmentID: String = "segment_a"
    ) throws -> (base: ConfirmedScientificImportManifest, receipt: ConfirmedScientificImportReceipt) {
        let staged = try StagedScientificImport(
            source: .commaSeparatedValues,
            columns: [StagedScientificColumn(
                sourceColumn: try StagedSourceColumnReference(oneBasedIndex: 1),
                header: "unit",
                cells: [.text(rawText: "1"), .text(rawText: "2")]
            )],
            suggestions: .none,
            sourceTransactionBinding: try StagedSourceTransactionBinding(
                sourceBytesSHA256: String(repeating: bindingCharacter, count: 64),
                selection: .commaSeparatedValues
            )
        )
        let draft = ScientificImportManifestDraft(
            boundTo: staged,
            sourceTimeUnit: .seconds,
            activityMode: .putativeSingleUnit,
            recordingSegmentID: ScientificRecordingSegmentID(try ScientificSemanticID(validating: segmentID)),
            recordingRegime: .continuousUntrialed,
            importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
            observationBoundsAvailability: .unknownOrUnavailable,
            eventScopeGroups: [EventScopeGroupManifestDraft(
                semanticID: ScientificEventScopeGroupID(try ScientificSemanticID(validating: "group_1")),
                spikeTrains: [SpikeTrainColumnManifestDraft(
                    sourceColumn: try StagedSourceColumnReference(oneBasedIndex: 1),
                    semanticID: ScientificSpikeTrainID(try ScientificSemanticID(validating: "unit_a")),
                    orderDecision: .preserveSourceOrder,
                    duplicateDecision: .preserveMultiplicity
                )],
                eventDefinitions: [],
                timeBasis: .recordingElapsed
            )],
            eventAttributeDefinitions: []
        )
        let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
        let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
        guard case .accepted(let validated) =
            PreparedScientificImportValidator.validateForCanonicalProjection(prepared) else {
            throw StoreTestError.rejected
        }
        let base = ScientificImportConfirmationBuilder.confirm(
            try CanonicalScientificImportProjector.projectConfirmable(validated)
        )
        let receipt = try ConfirmedScientificImportManifestPersistence.project(from: base)
        return (base, receipt)
    }

    private func attemptSave(
        _ store: ScientificImportManifestFileStore,
        _ pair: (base: ConfirmedScientificImportManifest, receipt: ConfirmedScientificImportReceipt)
    ) async -> Result<StoredSaveResult, ScientificImportManifestStoreError> {
        do {
            return .success(try await store.save(baseConfirmation: pair.base))
        } catch let error as ScientificImportManifestStoreError {
            return .failure(error)
        } catch {
            return .failure(.writeFailed)
        }
    }

    // MARK: - Save / mint

    @Test("Save mints standing, lays the path out by SHA and digest, and reads back exactly")
    func savesMintsAndReadsBack() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let (base, receipt) = try makeConfirmed()
            let result = try await store.save(baseConfirmation: base)

            #expect(result.outcome == .written)
            #expect(result.manifest.confirmationRecordDigest == receipt.confirmationRecordDigest)
            #expect(result.url.lastPathComponent == "\(receipt.confirmationRecordDigest).stpdimportmanifest")
            let shaDir = result.url.deletingLastPathComponent()
            #expect(shaDir.lastPathComponent == receipt.sourceTransactionBinding.sourceBytesSHA256)
            #expect(shaDir.deletingLastPathComponent().lastPathComponent == "by-source")

            // Only the store's derived-path restore verify mints standing; a bare replay read does not.
            let replayReceipt = try await store.readForReplay(
                sourceSHA256: receipt.sourceTransactionBinding.sourceBytesSHA256,
                recordDigest: receipt.confirmationRecordDigest
            )
            #expect(replayReceipt == receipt)
            let restored = try await store.restoreVerify(
                sourceSHA256: receipt.sourceTransactionBinding.sourceBytesSHA256,
                recordDigest: receipt.confirmationRecordDigest,
                baseConfirmation: base
            )
            #expect(restored.confirmationRecordDigest == receipt.confirmationRecordDigest)
        }
    }

    @Test("An arbitrary temporary file with valid receipt bytes cannot mint persistence standing")
    func arbitraryTemporaryFileCannotMintStanding() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            // Valid receipt bytes exist in an arbitrary temp file that the store never published.
            let tmpDir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString, isDirectory: true)
            try FileManager.default.createDirectory(at: tmpDir, withIntermediateDirectories: true)
            defer { try? FileManager.default.removeItem(at: tmpDir) }
            let tmpFile = tmpDir.appendingPathComponent("receipt.stpdimportmanifest")
            try Data(ConfirmedScientificImportManifestPersistence.encode(receipt)).write(to: tmpFile)

            // They decode as an untrusted bare receipt...
            let decoded = try ConfirmedScientificImportManifestPersistence.readUntrusted(fileURL: tmpFile)
            #expect(decoded == receipt)

            // ...but the store never saved this record, so nothing under its derived path exists to
            // verify; there is no arbitrary-URL API that yields the wrapper.
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.recordNotFound) {
                _ = try await store.restoreVerify(
                    sourceSHA256: receipt.sourceTransactionBinding.sourceBytesSHA256,
                    recordDigest: receipt.confirmationRecordDigest,
                    baseConfirmation: base
                )
            }
        }
    }

    @Test("Saving the identical record again is an idempotent success")
    func idempotentIdenticalSave() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let (base, _) = try makeConfirmed()
            let first = try await store.save(baseConfirmation: base)
            let second = try await store.save(baseConfirmation: base)
            #expect(first.outcome == .written)
            #expect(second.outcome == .idempotentExisting)
        }
    }

    @Test("A different byte payload at the same target is refused and never overwrites")
    func refusesDifferentBytesAtSameTarget() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let (base, _) = try makeConfirmed()
            let result = try await store.save(baseConfirmation: base)
            try Data([0x00, 0x01, 0x02]).write(to: result.url)

            await #expect(throws: ScientificImportManifestStoreError.conflictingBytesAtTarget) {
                _ = try await store.save(baseConfirmation: base)
            }
            #expect(try Data(contentsOf: result.url) == Data([0x00, 0x01, 0x02]))
        }
    }

    @Test("Exceeding the per-source candidate limit is a clear error and deletes nothing")
    func enforcesCandidateCapWithoutDeleting() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let sha = String(repeating: "a", count: 64)
            let dir = root
                .appendingPathComponent("by-source", isDirectory: true)
                .appendingPathComponent(sha, isDirectory: true)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            for index in 0..<ScientificImportManifestFileStore.maximumCandidatesPerSource {
                try Data([0x00]).write(to: dir.appendingPathComponent("filler_\(index).stpdimportmanifest"))
            }
            let (base, _) = try makeConfirmed(bindingCharacter: "a")
            await #expect(throws: ScientificImportManifestStoreError.candidateLimitExceeded(
                maximum: ScientificImportManifestFileStore.maximumCandidatesPerSource
            )) {
                _ = try await store.save(baseConfirmation: base)
            }
            let count = try FileManager.default.contentsOfDirectory(atPath: dir.path)
                .filter { $0.hasSuffix(".stpdimportmanifest") }.count
            #expect(count == ScientificImportManifestFileStore.maximumCandidatesPerSource)
        }
    }

    // MARK: - Durability barrier

    @Test("Repeated directory-sync failure never mints standing; a clean attempt then succeeds")
    func repeatedDirectorySyncFailureNeverMintsStanding() async throws {
        try await withTempRoot { root in
            let (base, _) = try makeConfirmed()
            let faulted = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterDirectorySync: { _ in
                    throw ScientificImportManifestStoreError.durabilitySyncFailed
                })
            )
            // 1. First publication fails at the directory-sync barrier → no standing.
            await #expect(throws: ScientificImportManifestStoreError.durabilitySyncFailed) {
                _ = try await faulted.save(baseConfirmation: base)
            }
            // 2. The target now exists, but a second attempt STILL runs the barrier and STILL fails →
            //    no standing, proving the barrier was retried rather than skipped.
            await #expect(throws: ScientificImportManifestStoreError.durabilitySyncFailed) {
                _ = try await faulted.save(baseConfirmation: base)
            }
            // 3. A clean attempt completes the barrier and returns idempotent success.
            let clean = ScientificImportManifestFileStore(rootDirectory: root)
            let result = try await clean.save(baseConfirmation: base)
            #expect(result.outcome == .idempotentExisting)
        }
    }

    @Test("Partial directory creation is retried safely: only-parent-exists then a clean save succeeds")
    func partialDirectoryCreationRetriedSafely() async throws {
        try await withTempRoot { root in
            // Pre-create only `by-source` (parent), not the per-source directory.
            try FileManager.default.createDirectory(
                at: root.appendingPathComponent("by-source", isDirectory: true),
                withIntermediateDirectories: true
            )
            let (base, _) = try makeConfirmed()
            let faulted = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterDirectorySync: { _ in
                    throw ScientificImportManifestStoreError.durabilitySyncFailed
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.durabilitySyncFailed) {
                _ = try await faulted.save(baseConfirmation: base)
            }
            // The per-source directory was created during the failed attempt; a clean retry completes.
            let clean = ScientificImportManifestFileStore(rootDirectory: root)
            let result = try await clean.save(baseConfirmation: base)
            #expect(result.outcome == .idempotentExisting)
        }
    }

    @Test("A final-readback failure does not mint standing")
    func finalReadbackFailureLeavesNoStanding() async throws {
        try await withTempRoot { root in
            let (base, _) = try makeConfirmed()
            let faulted = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(beforeFinalReadback: {
                    throw ScientificImportManifestStoreError.readbackMismatch
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.readbackMismatch) {
                _ = try await faulted.save(baseConfirmation: base)
            }
        }
    }

    // MARK: - Concurrency

    @Test("Two store instances submitting different records concurrently are serialized to two records")
    func twoStoresConcurrentSaveDifferentRecords() async throws {
        try await withTempRoot { root in
            // The two saves are SUBMITTED concurrently but SERIALIZED by the store-global anchor lock;
            // both still complete and both records are persisted.
            let s1 = ScientificImportManifestFileStore(rootDirectory: root)
            let s2 = ScientificImportManifestFileStore(rootDirectory: root)
            let a = try makeConfirmed(segmentID: "segment_a")
            let b = try makeConfirmed(segmentID: "segment_b")
            async let ra = s1.save(baseConfirmation: a.base)
            async let rb = s2.save(baseConfirmation: b.base)
            let (recA, recB) = try await (ra, rb)
            #expect(recA.outcome == .written)
            #expect(recB.outcome == .written)
            let discovery = try await s1.discover(sourceSHA256: a.receipt.sourceTransactionBinding.sourceBytesSHA256)
            #expect(discovery.summaries.count == 2)
        }
    }

    @Test("Two store instances saving the same record (serialized) converge idempotently")
    func twoStoresConcurrentSaveSameRecord() async throws {
        try await withTempRoot { root in
            let s1 = ScientificImportManifestFileStore(rootDirectory: root)
            let s2 = ScientificImportManifestFileStore(rootDirectory: root)
            let pair = try makeConfirmed()
            async let ra = attemptSave(s1, pair)
            async let rb = attemptSave(s2, pair)
            let outcomes = await [ra, rb].compactMap { try? $0.get().outcome }
            #expect(outcomes.contains(.written))
            #expect(outcomes.contains(.idempotentExisting))
            let discovery = try await s1.discover(sourceSHA256: pair.receipt.sourceTransactionBinding.sourceBytesSHA256)
            #expect(discovery.summaries.count == 1)
        }
    }

    @Test("At the 128th-record boundary, one serialized save wins and the other is a clear cap error")
    func candidateBoundaryRaceAt128() async throws {
        try await withTempRoot { root in
            // Serialized by the store-global lock: the first to acquire it writes the 128th record; the
            // second sees the cap and fails cleanly, deleting nothing.
            let sha = String(repeating: "a", count: 64)
            let dir = sourceDirectoryURL(root: root, sha: sha)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            for index in 0..<(ScientificImportManifestFileStore.maximumCandidatesPerSource - 1) {
                try Data([0x00]).write(to: dir.appendingPathComponent("filler_\(index).stpdimportmanifest"))
            }
            let s1 = ScientificImportManifestFileStore(rootDirectory: root)
            let s2 = ScientificImportManifestFileStore(rootDirectory: root)
            let a = try makeConfirmed(segmentID: "segment_a")
            let b = try makeConfirmed(segmentID: "segment_b")
            async let ra = attemptSave(s1, a)
            async let rb = attemptSave(s2, b)
            let results = await [ra, rb]
            let successes = results.filter { if case .success = $0 { return true }; return false }
            let capFailures = results.filter {
                if case .failure(.candidateLimitExceeded) = $0 { return true }; return false
            }
            #expect(successes.count == 1)
            #expect(capFailures.count == 1)
        }
    }

    @Test("Across a by-source subtree replacement (ABA), the store-global lock still excludes a second store")
    func storeGlobalLockExcludesSecondStoreAcrossSubtreeReplacement() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let s1AtCriticalSection = DispatchSemaphore(value: 0)
            let s1Release = DispatchSemaphore(value: 0)

            // s1 acquires the store-global lock (rooted at the fixed anchor) and is HELD inside its
            // critical section, keeping the anchor `flock` for the whole hold.
            let s1 = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterLockAcquired: {
                    s1AtCriticalSection.signal(); s1Release.wait()
                })
            )
            let t1 = Task { await self.attemptSave(s1, (base, receipt)) }
            await awaitSignal(s1AtCriticalSection)

            // ABA: replace the by-source SUBTREE beneath s1. The per-source (sha-directory) lock the old
            // design used would now guard a DIFFERENT inode for a second store — the classic split into
            // old/new critical sections. The store-global anchor lock does not move, so it cannot split.
            // The sha directory is still empty here (s1 is held before it writes), so rmdir succeeds; s1's
            // held descriptor keeps the old inode alive but unlinked.
            let shaDir = sourceDirectoryURL(root: root, sha: sha)
            try FileManager.default.removeItem(at: shaDir)

            // s2 (same source) rebuilds the sha directory as a NEW inode inside its anchored traversal and
            // then contends for the lock with a short deadline. Under a per-source lock it would lock that
            // new inode and enter its own critical section; under the store-global anchor lock it is
            // excluded and times out — proving two stores never occupy distinct old/new critical sections.
            let s2 = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(lockTimeoutOverrideNanoseconds: 80_000_000)
            )
            let r2 = await attemptSave(s2, (base, receipt))
            guard case .failure(.lockTimeout) = r2 else {
                Issue.record("second store must be excluded by the store-global lock (lockTimeout), got \(r2)")
                s1Release.signal(); _ = await t1.value
                return
            }

            // Release s1 and drain it; its subtree was replaced under it, so its own mint fails closed at
            // the post-lock chain-identity revalidation. What matters is that s2 never got a critical
            // section while s1 held the lock.
            s1Release.signal()
            _ = await t1.value
        }
    }

    // MARK: - Discover

    @Test("Discovery returns valid records, counts mismatched/corrupt ones, and skips temp files")
    func discoversValidRecordsAndCountsUnreadable() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let a = try makeConfirmed(segmentID: "segment_a")
            let b = try makeConfirmed(segmentID: "segment_b")
            _ = try await store.save(baseConfirmation: a.base)
            _ = try await store.save(baseConfirmation: b.base)
            let sha = a.receipt.sourceTransactionBinding.sourceBytesSHA256
            let dir = root
                .appendingPathComponent("by-source", isDirectory: true)
                .appendingPathComponent(sha, isDirectory: true)
            try Data([0x00]).write(to: dir.appendingPathComponent(".inflight.tmp"))
            try Data([0x09, 0x09]).write(to: dir.appendingPathComponent("deadbeef.stpdimportmanifest"))

            let discovery = try await store.discover(sourceSHA256: sha)
            #expect(discovery.summaries.count == 2)
            #expect(Set(discovery.summaries.map(\.recordDigest))
                == Set([a.receipt.confirmationRecordDigest, b.receipt.confirmationRecordDigest]))
            #expect(discovery.unreadableRecordCount == 1)
        }
    }

    @Test("Discovery is deterministically bounded by an entry budget")
    func discoveryIsBounded() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let sha = String(repeating: "a", count: 64)
            let dir = root
                .appendingPathComponent("by-source", isDirectory: true)
                .appendingPathComponent(sha, isDirectory: true)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            for index in 0...(ScientificImportManifestFileStore.maximumDiscoveryEntries) {
                try Data([0x00]).write(to: dir.appendingPathComponent("f\(index).stpdimportmanifest"))
            }
            await #expect(throws: ScientificImportManifestStoreError.discoveryBudgetExceeded) {
                _ = try await store.discover(sourceSHA256: sha)
            }
        }
    }

    @Test("Reading an absent record fails closed; discovery of an unknown source is empty")
    func readAbsentAndDiscoverUnknown() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let unknownSHA = String(repeating: "f", count: 64)
            let discovery = try await store.discover(sourceSHA256: unknownSHA)
            #expect(discovery.summaries.isEmpty)
            await #expect(throws: ScientificImportManifestStoreError.recordNotFound) {
                _ = try await store.readForReplay(sourceSHA256: unknownSHA, recordDigest: String(repeating: "0", count: 64))
            }
        }
    }

    // MARK: - Per-point fault injection (no standing, nothing durable, on failure at each step)

    /// Asserts that a save which fails at some pre-publish point throws the injected error and leaves
    /// no durable, discoverable, restorable record behind.
    private func expectPrePublishFaultLeavesNothing(
        root: URL, faults: StoreFaultInjection, throwing expected: ScientificImportManifestStoreError
    ) async throws {
        let (base, receipt) = try makeConfirmed()
        let faulted = ScientificImportManifestFileStore(rootDirectory: root, faults: faults)
        await #expect(throws: expected) {
            _ = try await faulted.save(baseConfirmation: base)
        }
        let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
        // No task-owned temporary file is leaked in the source directory after any pre-publication fault.
        #expect(temporaryFileNames(root: root, sha: sha).isEmpty)
        let clean = ScientificImportManifestFileStore(rootDirectory: root)
        #expect(try await clean.discover(sourceSHA256: sha).summaries.isEmpty)
        await #expect(throws: ScientificImportManifestStoreError.recordNotFound) {
            _ = try await clean.restoreVerify(
                sourceSHA256: sha, recordDigest: receipt.confirmationRecordDigest,
                baseConfirmation: base
            )
        }
    }

    // MARK: - Filesystem-closure helpers

    private func sourceDirectoryEntries(root: URL, sha: String) -> [String] {
        (try? FileManager.default.contentsOfDirectory(atPath: sourceDirectoryURL(root: root, sha: sha).path)) ?? []
    }

    private func temporaryFileNames(root: URL, sha: String) -> [String] {
        sourceDirectoryEntries(root: root, sha: sha).filter { $0.hasSuffix(".tmp") }
    }

    private func derivedRecordURL(root: URL, sha: String, digest: String) -> URL {
        sourceDirectoryURL(root: root, sha: sha).appendingPathComponent("\(digest).\(ScientificImportManifestFileStore.fileExtension)")
    }

    @Test("A failure at the lock-acquire point leaves nothing durable")
    func lockAcquireFailureLeavesNothing() async throws {
        try await withTempRoot { root in
            try await expectPrePublishFaultLeavesNothing(
                root: root,
                faults: StoreFaultInjection(beforeLockAcquire: {
                    throw ScientificImportManifestStoreError.lockTimeout
                }),
                throwing: .lockTimeout
            )
        }
    }

    @Test("A failure at the temp-write point leaves nothing durable")
    func tempWriteFailureLeavesNothing() async throws {
        try await withTempRoot { root in
            try await expectPrePublishFaultLeavesNothing(
                root: root,
                faults: StoreFaultInjection(beforeTempWrite: {
                    throw ScientificImportManifestStoreError.writeFailed
                }),
                throwing: .writeFailed
            )
        }
    }

    @Test("A failure just after the temp fsync (before publish) leaves nothing durable")
    func tempFsyncFailureLeavesNothing() async throws {
        try await withTempRoot { root in
            try await expectPrePublishFaultLeavesNothing(
                root: root,
                faults: StoreFaultInjection(afterTempFsync: {
                    throw ScientificImportManifestStoreError.durabilitySyncFailed
                }),
                throwing: .durabilitySyncFailed
            )
        }
    }

    @Test("A failure at the publish-link point leaves nothing durable")
    func publishLinkFailureLeavesNothing() async throws {
        try await withTempRoot { root in
            try await expectPrePublishFaultLeavesNothing(
                root: root,
                faults: StoreFaultInjection(beforePublishLink: {
                    throw ScientificImportManifestStoreError.publishFailed
                }),
                throwing: .publishFailed
            )
        }
    }

    @Test("The durability barrier fsyncs the directory chain child-to-parent, then reads back")
    func directorySyncFollowsChildToParentOrder() async throws {
        try await withTempRoot { root in
            let (base, _) = try makeConfirmed()
            let recorder = RoleRecorder()
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterDirectorySync: { depth in recorder.append(depth) })
            )
            _ = try await store.save(baseConfirmation: base)
            // Child-to-parent: source(0) → by-source(1) → root leaf(2) → anchor(3). `withTempRoot`
            // pre-creates only the anchor, so the store creates the 3 store-owned levels beneath it.
            #expect(recorder.depths == [0, 1, 2, 3])
        }
    }

    // MARK: - Deterministic publish → durability race

    @Test("A concurrent restore serializes behind the writer's full publish→durability barrier")
    func restoreSerializesBehindPublishDurabilityWindow() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let published = DispatchSemaphore(value: 0)
            let releaseWriter = DispatchSemaphore(value: 0)
            let restoreAtLock = DispatchSemaphore(value: 0)

            let writer = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterPublishBeforeBarrier: {
                    // The link is published but the durability barrier has NOT run and the store-global
                    // anchor lock is still held. Announce the open window, then hold it until released.
                    published.signal()
                    releaseWriter.wait()
                })
            )
            let reader = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(beforeLockAcquire: { restoreAtLock.signal() })
            )

            async let writeResult = writer.save(baseConfirmation: base)
            await awaitSignal(published)

            let restoreTask = Task {
                try await reader.restoreVerify(
                    sourceSHA256: receipt.sourceTransactionBinding.sourceBytesSHA256,
                    recordDigest: receipt.confirmationRecordDigest,
                    baseConfirmation: base
                )
            }
            // The restore reaches the lock but cannot pass it — the writer holds the same store-global
            // anchor flock across its entire barrier, so restore blocks until the writer releases.
            await awaitSignal(restoreAtLock)
            releaseWriter.signal()

            let saved = try await writeResult
            let restored = try await restoreTask.value
            #expect(saved.outcome == .written)
            #expect(restored.confirmationRecordDigest == receipt.confirmationRecordDigest)
        }
    }

    // MARK: - Bounded / adversarial discovery

    @Test("Discovery is bounded by a byte-work budget independent of the entry budget")
    func discoveryBoundedByByteBudget() async throws {
        try await withTempRoot { root in
            let sha = String(repeating: "a", count: 64)
            let dir = sourceDirectoryURL(root: root, sha: sha)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            // Far below the entry budget (4096), but each sparse file charges (maxFile+1) bytes, so the
            // 128 MiB byte budget trips first. None are ever read (each exceeds the per-file cap).
            let oversized = UInt64(ConfirmedScientificImportManifestPersistence.maximumFileByteCount + 1)
            for index in 0..<20 {
                try writeSparseFile(
                    at: dir.appendingPathComponent("big_\(index).stpdimportmanifest"),
                    logicalSize: oversized
                )
            }
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.discoveryBudgetExceeded) {
                _ = try await store.discover(sourceSHA256: sha)
            }
        }
    }

    @Test("An oversized record is charged and rejected without ever being read")
    func oversizedRecordRejectedWithoutReading() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let valid = try makeConfirmed()
            _ = try await store.save(baseConfirmation: valid.base)
            let sha = valid.receipt.sourceTransactionBinding.sourceBytesSHA256
            let dir = sourceDirectoryURL(root: root, sha: sha)
            // A 2 GiB sparse file: reading it would be catastrophic. The size guard rejects it before
            // any open/read, charging only the per-file cap.
            try writeSparseFile(
                at: dir.appendingPathComponent(String(repeating: "b", count: 64) + ".stpdimportmanifest"),
                logicalSize: 2 * 1024 * 1024 * 1024
            )
            let discovery = try await store.discover(sourceSHA256: sha)
            #expect(discovery.summaries.count == 1)
            #expect(discovery.summaries.first?.recordDigest == valid.receipt.confirmationRecordDigest)
            #expect(discovery.unreadableRecordCount == 1)
            // The valid record and the oversized decoy. The lock is held on the source DIRECTORY itself,
            // so there is no leftover `.lock` file to inspect.
            #expect(discovery.entriesInspected == 2)
            // Proof of no multi-GiB read: total charged work stays within one per-file cap plus the
            // small valid record, never the 2 GiB logical size.
            #expect(discovery.workBytesCharged
                <= ConfirmedScientificImportManifestPersistence.maximumFileByteCount + 1 + 64 * 1024)
        }
    }

    // MARK: - Bounded locking: timeout + cancellation

    @Test("A held store lock makes a contending writer time out deterministically")
    func heldLockCausesLockTimeout() async throws {
        try await withTempRoot { root in
            let (base, _) = try makeConfirmed()
            // The store lock is rooted at the fixed anchor; contend against it.
            try await holdingExclusiveDirectoryFlock(at: anchorURL(forStoreRoot: root)) {
                let store = ScientificImportManifestFileStore(
                    rootDirectory: root,
                    faults: StoreFaultInjection(lockTimeoutOverrideNanoseconds: 80_000_000)
                )
                await #expect(throws: ScientificImportManifestStoreError.lockTimeout) {
                    _ = try await store.save(baseConfirmation: base)
                }
            }
        }
    }

    @Test("A save blocked on a contended lock is cancellable")
    func blockedSaveIsCancellable() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            try await holdingExclusiveDirectoryFlock(at: anchorURL(forStoreRoot: root)) {
                let reachedLock = DispatchSemaphore(value: 0)
                // A deadline far larger than any plausible scheduling delay, so cancellation — not the
                // timeout — is deterministically the exit once `cancel()` is observed in the lock loop.
                let store = ScientificImportManifestFileStore(
                    rootDirectory: root,
                    faults: StoreFaultInjection(
                        beforeLockAcquire: { reachedLock.signal() },
                        lockTimeoutOverrideNanoseconds: 600_000_000_000
                    )
                )
                let task = Task { await attemptSave(store, (base, receipt)) }
                await awaitSignal(reachedLock)
                task.cancel()
                let result = await task.value
                guard case .failure(.operationCancelled) = result else {
                    Issue.record("expected operationCancelled, got \(result)")
                    return
                }
            }
        }
    }

    // MARK: - Symlink substitution is rejected across the store subtree

    @Test("A symlink substituted for the by-source subtree is rejected")
    func rejectsSymlinkAtBySourceSubtree() async throws {
        try await withTempRoot { root in
            try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
            let decoy = root.appendingPathComponent("decoy", isDirectory: true)
            try FileManager.default.createDirectory(at: decoy, withIntermediateDirectories: true)
            try FileManager.default.createSymbolicLink(
                at: root.appendingPathComponent("by-source"), withDestinationURL: decoy
            )
            let (base, _) = try makeConfirmed()
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.unsafePathObject) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    @Test("A symlink substituted for the per-source directory is rejected")
    func rejectsSymlinkAtSourceDirectory() async throws {
        try await withTempRoot { root in
            let sha = String(repeating: "a", count: 64)
            let bySource = root.appendingPathComponent("by-source", isDirectory: true)
            try FileManager.default.createDirectory(at: bySource, withIntermediateDirectories: true)
            let decoy = root.appendingPathComponent("decoy", isDirectory: true)
            try FileManager.default.createDirectory(at: decoy, withIntermediateDirectories: true)
            try FileManager.default.createSymbolicLink(
                at: bySource.appendingPathComponent(sha), withDestinationURL: decoy
            )
            let (base, _) = try makeConfirmed()
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.unsafePathObject) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    // The store-global lock is held on the fixed trusted anchor descriptor, so it cannot split when a
    // store-owned source/by-source subtree is replaced and there is no separate `.lock` file inode to
    // substitute. Directory-substitution safety is covered below; old/new-tree lock exclusion is covered
    // by `storeGlobalLockExcludesSecondStoreAcrossSubtreeReplacement`.

    @Test("A symlink substituted for a final record is rejected on restore")
    func rejectsSymlinkAtFinalRecord() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let (base, receipt) = try makeConfirmed()
            let saved = try await store.save(baseConfirmation: base)
            // Substitute a symlink (even one pointing at valid receipt bytes) for the real final record.
            let decoy = root.appendingPathComponent("decoy.stpdimportmanifest")
            try Data(ConfirmedScientificImportManifestPersistence.encode(receipt)).write(to: decoy)
            try FileManager.default.removeItem(at: saved.url)
            try FileManager.default.createSymbolicLink(at: saved.url, withDestinationURL: decoy)
            await #expect(throws: ScientificImportManifestStoreError.unsafePathObject) {
                _ = try await store.restoreVerify(
                    sourceSHA256: receipt.sourceTransactionBinding.sourceBytesSHA256,
                    recordDigest: receipt.confirmationRecordDigest,
                    baseConfirmation: base
                )
            }
        }
    }
    // The transient temp file (`.<digest>.<uuid>.tmp`) is opened `O_CREAT | O_EXCL | O_WRONLY |
    // O_NOFOLLOW` under an unpredictable name, so a pre-planted object at its path fails the create
    // (EEXIST) and a symlink is never followed; there is no fixed name for a test to substitute.

    // MARK: - Anchored root creation + parent durability

    @Test("A regular file blocking a store-owned directory component fails closed, not swallowed")
    func regularFileBlockingStoreDirectoryFailsClosed() async throws {
        try await withTempRoot { root in
            // Plant a regular FILE where the store must create the root-leaf directory beneath the anchor.
            try Data([0x00]).write(to: root)
            let (base, _) = try makeConfirmed()
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.unsafePathObject) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    @Test("First creation builds the whole chain beneath the anchor and mints durable standing")
    func firstCreationBuildsChainBeneathAnchor() async throws {
        try await withTempRoot { root in
            // Only the anchor (root's parent) exists; the store creates root, by-source and the source dir.
            #expect(!FileManager.default.fileExists(atPath: root.path))
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let (base, receipt) = try makeConfirmed()
            let result = try await store.save(baseConfirmation: base)
            #expect(result.outcome == .written)
            #expect(FileManager.default.fileExists(atPath: derivedRecordURL(
                root: root, sha: receipt.sourceTransactionBinding.sourceBytesSHA256,
                digest: receipt.confirmationRecordDigest).path))
        }
    }

    // MARK: - Descriptor/path identity revalidation before minting

    @Test("A final record unlinked after its descriptor is opened is rejected before minting")
    func finalRecordUnlinkedAfterOpenIsRejected() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let finalURL = derivedRecordURL(root: root, sha: sha, digest: receipt.confirmationRecordDigest)
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                // After readback, before identity revalidation, unlink the final record. The real
                // revalidation (not the hook) then detects the missing entry and fails closed.
                faults: StoreFaultInjection(beforeIdentityRevalidation: {
                    try FileManager.default.removeItem(at: finalURL)
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.pathIdentityChanged) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    @Test("A final record replaced by a different inode after open is rejected before minting")
    func finalRecordReplacedAfterOpenIsRejected() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let other = try makeConfirmed(segmentID: "segment_other")
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let finalURL = derivedRecordURL(root: root, sha: sha, digest: receipt.confirmationRecordDigest)
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(beforeIdentityRevalidation: {
                    // Replace the synchronized inode with a DIFFERENT regular file at the same name.
                    try FileManager.default.removeItem(at: finalURL)
                    try Data(ConfirmedScientificImportManifestPersistence.encode(other.receipt)).write(to: finalURL)
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.pathIdentityChanged) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    @Test("A source directory component replaced after its descriptor is opened is rejected before minting")
    func directoryComponentReplacedAfterOpenIsRejected() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let sourceDir = sourceDirectoryURL(root: root, sha: sha)
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(beforeIdentityRevalidation: {
                    // Move the whole source directory away and put a fresh empty one in its place: the
                    // opened source descriptor no longer matches the entry that names it.
                    let moved = sourceDir.deletingLastPathComponent().appendingPathComponent("moved-\(UUID())")
                    try FileManager.default.moveItem(at: sourceDir, to: moved)
                    try FileManager.default.createDirectory(at: sourceDir, withIntermediateDirectories: false)
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.pathIdentityChanged) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    // MARK: - Lock inode replacement fails closed

    @Test("Replacing the anchored source directory under the lock fails closed, minting nothing")
    func sourceDirectoryReplacementUnderLockFailsClosed() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let sourceDir = sourceDirectoryURL(root: root, sha: sha)
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterLockAcquired: {
                    // After the lock is acquired on the source directory descriptor, replace the source
                    // directory inode. The lock-identity revalidation must detect it and fail closed
                    // before any mutation — the locked descriptor no longer names the current entry.
                    try FileManager.default.removeItem(at: sourceDir)
                    try FileManager.default.createDirectory(at: sourceDir, withIntermediateDirectories: false)
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.pathIdentityChanged) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    // MARK: - Temporary-file cleanup for every exit class

    @Test("A temp-cleanup failure after a pre-publication error surfaces a typed error and mints nothing")
    func temporaryCleanupFailureSurfacesTypedError() async throws {
        try await withTempRoot { root in
            let (base, _) = try makeConfirmed()
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(
                    beforePublishLink: { throw ScientificImportManifestStoreError.publishFailed },
                    duringTempCleanup: { throw ScientificImportManifestStoreError.temporaryCleanupFailed }
                )
            )
            // The publish-point failure triggers cleanup, which itself fails: surfaced as a typed error.
            await #expect(throws: ScientificImportManifestStoreError.temporaryCleanupFailed) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    @Test("A temp-cleanup failure on the success path aborts the mint but leaves a restorable record")
    func temporaryCleanupFailureOnSuccessPathAbortsMintButKeepsRecord() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(duringTempCleanup: {
                    throw ScientificImportManifestStoreError.temporaryCleanupFailed
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.temporaryCleanupFailed) {
                _ = try await store.save(baseConfirmation: base)
            }
            // The record is visible and retryable but has not completed the directory durability barrier;
            // a clean retry synchronizes the full chain and completes idempotently.
            let clean = ScientificImportManifestFileStore(rootDirectory: root)
            #expect(try await clean.discover(sourceSHA256: sha).summaries.count == 1)
            #expect(try await clean.save(baseConfirmation: base).outcome == .idempotentExisting)
        }
    }

    // MARK: - Cancellation semantics

    @Test("Cancellation after the temp fsync but before publication cleans the temp and mints nothing")
    func cancellationBeforePublicationCleansTempAndMintsNothing() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let reached = DispatchSemaphore(value: 0)
            let release = DispatchSemaphore(value: 0)
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterTempFsync: { reached.signal(); release.wait() })
            )
            let task = Task { await self.attemptSave(store, (base, receipt)) }
            await awaitSignal(reached)
            task.cancel()
            release.signal()
            let result = await task.value
            guard case .failure(.operationCancelled) = result else {
                Issue.record("expected operationCancelled, got \(result)"); return
            }
            #expect(temporaryFileNames(root: root, sha: sha).isEmpty)
            let clean = ScientificImportManifestFileStore(rootDirectory: root)
            #expect(try await clean.discover(sourceSHA256: sha).summaries.isEmpty)
        }
    }

    @Test("Cancellation after publication completes the barrier, mints nothing, and leaves a restorable record")
    func cancellationAfterPublicationLeavesDurableRecordWithoutMinting() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let reached = DispatchSemaphore(value: 0)
            let release = DispatchSemaphore(value: 0)
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterPublishBeforeBarrier: { reached.signal(); release.wait() })
            )
            let task = Task { await self.attemptSave(store, (base, receipt)) }
            await awaitSignal(reached)
            task.cancel()
            release.signal()
            let result = await task.value
            guard case .failure(.operationCancelled) = result else {
                Issue.record("expected operationCancelled, got \(result)"); return
            }
            // No temp leaked and no wrapper minted, but the published record is fully durable and a later
            // explicit retry restores it safely.
            #expect(temporaryFileNames(root: root, sha: sha).isEmpty)
            let clean = ScientificImportManifestFileStore(rootDirectory: root)
            #expect(try await clean.discover(sourceSHA256: sha).summaries.count == 1)
            let restored = try await clean.restoreVerify(
                sourceSHA256: sha, recordDigest: receipt.confirmationRecordDigest, baseConfirmation: base
            )
            #expect(restored.confirmationRecordDigest == receipt.confirmationRecordDigest)
        }
    }

    // MARK: - Enumeration bounds + fault branches

    @Test("A final-file fsync failure mints no standing; a clean retry then completes")
    func finalFileFsyncFailureMintsNothing() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let faulted = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(beforeFinalFileFsync: {
                    throw ScientificImportManifestStoreError.durabilitySyncFailed
                })
            )
            // The file fsync is part of the post-publication durability barrier, so it fails closed
            // WITHOUT minting — but the published record is retryable by a later clean attempt.
            await #expect(throws: ScientificImportManifestStoreError.durabilitySyncFailed) {
                _ = try await faulted.save(baseConfirmation: base)
            }
            #expect(temporaryFileNames(root: root, sha: sha).isEmpty)
            let clean = ScientificImportManifestFileStore(rootDirectory: root)
            #expect(try await clean.save(baseConfirmation: base).outcome == .idempotentExisting)
        }
    }

    @Test("An enumeration error during discovery is surfaced, never treated as clean completion")
    func enumerationErrorDuringDiscoveryIsSurfaced() async throws {
        try await withTempRoot { root in
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let (base, receipt) = try makeConfirmed()
            _ = try await store.save(baseConfirmation: base)
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let faulted = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(duringEnumeration: {
                    throw ScientificImportManifestStoreError.storeUnreadable
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.storeUnreadable) {
                _ = try await faulted.discover(sourceSHA256: sha)
            }
        }
    }

    @Test("Candidate counting during save is bounded by a total-entry budget over irrelevant files")
    func candidateCountingBoundedByTotalEntryBudget() async throws {
        try await withTempRoot { root in
            let sha = String(repeating: "a", count: 64)
            let dir = sourceDirectoryURL(root: root, sha: sha)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            // Fill the source directory with IRRELEVANT files past the total-entry budget. There are far
            // fewer than 128 candidate records, so only the total-entry budget can bound the count.
            for index in 0...(ScientificImportManifestFileStore.maximumDiscoveryEntries) {
                try Data([0x00]).write(to: dir.appendingPathComponent("irrelevant_\(index).txt"))
            }
            let (base, _) = try makeConfirmed(bindingCharacter: "a")
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.discoveryBudgetExceeded) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    // MARK: - Final corrective pass: fixed anchor, non-blocking leaves, actual-byte budget, single cleanup

    private func makeFIFO(at url: URL) throws {
        let result = url.path.withCString { mkfifo($0, 0o600) }
        #expect(result == 0)
    }

    @Test("A failed first directory sync followed by a clean retry still syncs the entire fixed chain")
    func failedFirstDirectorySyncThenRetrySyncsEntireChain() async throws {
        try await withTempRoot { root in
            let (base, _) = try makeConfirmed()
            // First attempt fails at the FIRST (source, depth 0) directory sync → no mint. The record was
            // already published, so it remains for a retry.
            let faulted = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterDirectorySync: { depth in
                    if depth == 0 { throw ScientificImportManifestStoreError.durabilitySyncFailed }
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.durabilitySyncFailed) {
                _ = try await faulted.save(baseConfirmation: base)
            }
            // A clean retry re-syncs the ENTIRE fixed chain child-to-parent (source, by-source, root,
            // anchor), then completes idempotently.
            let recorder = RoleRecorder()
            let clean = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterDirectorySync: { depth in recorder.append(depth) })
            )
            #expect(try await clean.save(baseConfirmation: base).outcome == .idempotentExisting)
            #expect(recorder.depths == [0, 1, 2, 3])
        }
    }

    @Test("A replaced by-source directory is rejected before minting (fixed anchor revalidates it)")
    func replacedBySourceDirectoryIsRejected() async throws {
        try await withTempRoot { root in
            let (base, _) = try makeConfirmed()
            let bySource = root.appendingPathComponent("by-source", isDirectory: true)
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(beforeIdentityRevalidation: {
                    // Replace the by-source directory inode: move its subtree aside and create a fresh
                    // empty by-source in its place. With the FIXED anchor, by-source is a store-owned
                    // level that is descriptor-traversed and revalidated, so this is caught.
                    let moved = root.appendingPathComponent("by-source-moved-\(UUID())", isDirectory: true)
                    try FileManager.default.moveItem(at: bySource, to: moved)
                    try FileManager.default.createDirectory(at: bySource, withIntermediateDirectories: false)
                })
            )
            await #expect(throws: ScientificImportManifestStoreError.pathIdentityChanged) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    @Test("A FIFO planted at the target fails a save promptly without blocking")
    func fifoTargetFailsSavePromptly() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let dir = sourceDirectoryURL(root: root, sha: sha)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            try makeFIFO(at: dir.appendingPathComponent(
                "\(receipt.confirmationRecordDigest).\(ScientificImportManifestFileStore.fileExtension)"))
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.conflictingBytesAtTarget) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    @Test("A FIFO planted at the record fails a restore promptly without blocking")
    func fifoRecordFailsRestorePromptly() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let dir = sourceDirectoryURL(root: root, sha: sha)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            try makeFIFO(at: dir.appendingPathComponent(
                "\(receipt.confirmationRecordDigest).\(ScientificImportManifestFileStore.fileExtension)"))
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.unsafePathObject) {
                _ = try await store.restoreVerify(
                    sourceSHA256: sha, recordDigest: receipt.confirmationRecordDigest, baseConfirmation: base
                )
            }
        }
    }

    @Test("A FIFO candidate is counted unreadable by discovery without blocking")
    func fifoCandidateFailsDiscoveryPromptly() async throws {
        try await withTempRoot { root in
            let sha = String(repeating: "a", count: 64)
            let dir = sourceDirectoryURL(root: root, sha: sha)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            try makeFIFO(at: dir.appendingPathComponent(String(repeating: "b", count: 64) + ".stpdimportmanifest"))
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            let discovery = try await store.discover(sourceSHA256: sha)
            #expect(discovery.summaries.isEmpty)
            #expect(discovery.unreadableRecordCount == 1)
        }
    }

    @Test("An unsafe ancestor with an absent source is an explicit error, not an empty healthy store")
    func unsafeAncestorWithAbsentSourceIsExplicitError() async throws {
        try await withTempRoot { root in
            try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
            let decoy = root.appendingPathComponent("decoy", isDirectory: true)
            try FileManager.default.createDirectory(at: decoy, withIntermediateDirectories: true)
            // by-source is a SYMLINK (unsafe ancestor); the per-source directory is absent.
            try FileManager.default.createSymbolicLink(
                at: root.appendingPathComponent("by-source"), withDestinationURL: decoy)
            let store = ScientificImportManifestFileStore(rootDirectory: root)
            await #expect(throws: ScientificImportManifestStoreError.unsafePathObject) {
                _ = try await store.discover(sourceSHA256: String(repeating: "a", count: 64))
            }
        }
    }

    @Test("A candidate that grows after its size probe is charged its actual read, not the probed size")
    func candidateGrowthAfterProbeChargedActualBytes() async throws {
        try await withTempRoot { root in
            let sha = String(repeating: "a", count: 64)
            let dir = sourceDirectoryURL(root: root, sha: sha)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            let candidate = dir.appendingPathComponent(String(repeating: "b", count: 64) + ".stpdimportmanifest")
            try Data([0x00]).write(to: candidate)   // 1-byte probed size
            let byteLimit = ConfirmedScientificImportManifestPersistence.maximumFileByteCount
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(afterCandidateSizeProbe: {
                    // Grow the file well past the per-file limit AFTER its size was probed.
                    let handle = try FileHandle(forWritingTo: candidate)
                    defer { try? handle.close() }
                    try handle.seekToEnd()
                    try handle.write(contentsOf: Data(count: byteLimit + 1_024))
                })
            )
            let discovery = try await store.discover(sourceSHA256: sha)
            #expect(discovery.summaries.isEmpty)
            #expect(discovery.unreadableRecordCount == 1)
            // Charged the ACTUAL bytes read (bounded to the per-file cap), NOT the 1-byte probe. A
            // size-probe-based charge would have recorded ~1 byte.
            #expect(discovery.workBytesCharged >= byteLimit + 1)
        }
    }

    @Test("A candidate read that fails after a valid receipt prefix is unreadable, never a valid summary")
    func candidateReadFailureAfterValidPrefixIsUnreadable() async throws {
        try await withTempRoot { root in
            // A GENUINELY valid, complete record on disk: with no read fault it lists as exactly one
            // summary, so the fault below is the only difference.
            let clean = ScientificImportManifestFileStore(rootDirectory: root)
            let (base, receipt) = try makeConfirmed()
            _ = try await clean.save(baseConfirmation: base)
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            #expect(try await clean.discover(sourceSHA256: sha).summaries.count == 1)

            // Now inject a read failure that fires AFTER the valid receipt bytes have been read (a valid
            // prefix), simulating an EIO on the next read. A read error must NOT be mistaken for a clean
            // EOF that would decode the prefix and list it: the candidate is counted unreadable instead.
            let faulted = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(duringCandidateRead: { _ in throw StoreTestError.rejected })
            )
            let discovery = try await faulted.discover(sourceSHA256: sha)
            #expect(discovery.summaries.isEmpty)
            #expect(discovery.unreadableRecordCount == 1)
        }
    }

    @Test("A write/fsync/close failure plus a cleanup failure reports temporaryCleanupFailed")
    func writeSyncFailurePlusCleanupFailureReportsTemporaryCleanupFailed() async throws {
        try await withTempRoot { root in
            let (base, _) = try makeConfirmed()
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(
                    duringTempWriteSync: { throw ScientificImportManifestStoreError.writeFailed },
                    duringTempCleanup: { throw ScientificImportManifestStoreError.temporaryCleanupFailed }
                )
            )
            // The write/fsync/close failure routes through the single cleanup path; the cleanup itself
            // fails, so the store surfaces temporaryCleanupFailed rather than claiming to be clean.
            await #expect(throws: ScientificImportManifestStoreError.temporaryCleanupFailed) {
                _ = try await store.save(baseConfirmation: base)
            }
        }
    }

    @Test("Cancellation inside the final pre-link hook leaves no published final record")
    func cancellationInsideFinalPreLinkHookLeavesNoRecord() async throws {
        try await withTempRoot { root in
            let (base, receipt) = try makeConfirmed()
            let sha = receipt.sourceTransactionBinding.sourceBytesSHA256
            let reached = DispatchSemaphore(value: 0)
            let release = DispatchSemaphore(value: 0)
            let store = ScientificImportManifestFileStore(
                rootDirectory: root,
                faults: StoreFaultInjection(beforePublishLink: { reached.signal(); release.wait() })
            )
            let task = Task { await self.attemptSave(store, (base, receipt)) }
            await awaitSignal(reached)
            task.cancel()
            release.signal()
            let result = await task.value
            guard case .failure(.operationCancelled) = result else {
                Issue.record("expected operationCancelled, got \(result)"); return
            }
            // The recheck after the final pre-link hook, immediately before linkat, prevented the link:
            // no temp leaked and no final record was published.
            #expect(temporaryFileNames(root: root, sha: sha).isEmpty)
            let clean = ScientificImportManifestFileStore(rootDirectory: root)
            #expect(try await clean.discover(sourceSHA256: sha).summaries.isEmpty)
        }
    }
}
