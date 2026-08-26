import CryptoKit
import Foundation
@testable import STPDCore
import Testing

// MARK: - Byte-mutation helpers for failure injection

private func firstIndex(of needle: [UInt8], in haystack: [UInt8]) -> Int? {
    guard !needle.isEmpty, haystack.count >= needle.count else { return nil }
    for start in 0...(haystack.count - needle.count) where Array(haystack[start..<start + needle.count]) == needle {
        return start
    }
    return nil
}

private func bytes(_ string: String) -> [UInt8] { Array(string.utf8) }

/// Overwrites `count` bytes at `offset` with `value`.
private func overwriting(_ original: [UInt8], at offset: Int, count: Int, with value: UInt8) -> [UInt8] {
    var copy = original
    for index in offset..<(offset + count) { copy[index] = value }
    return copy
}

/// Overwrites the 8-byte big-endian int64 at `offset` with `value`.
private func overwritingInt64(_ original: [UInt8], at offset: Int, with value: Int64) -> [UInt8] {
    var copy = original
    let unsigned = UInt64(bitPattern: value)
    for byte in 0..<8 { copy[offset + byte] = UInt8((unsigned >> (8 * (7 - byte))) & 0xFF) }
    return copy
}

private func decodeThrows(
    _ mutated: [UInt8],
    _ expected: PersistedReceiptDecodingError
) {
    #expect(throws: expected) {
        _ = try ConfirmedScientificImportManifestPersistence.decode(mutated)
    }
}

/// Writes `bytes` to a fresh regular file and returns its URL plus a cleanup thunk.
private func writeTempFile(_ bytes: [UInt8], name: String = "receipt.stpdimportmanifest") throws -> (url: URL, cleanup: () -> Void) {
    let dir = FileManager.default.temporaryDirectory
        .appendingPathComponent("PersistCodecTests", isDirectory: true)
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
    let url = dir.appendingPathComponent(name)
    try Data(bytes).write(to: url)
    return (url, { try? FileManager.default.removeItem(at: dir) })
}

/// Runs `body` with a fresh temp-rooted Core store (the sole minter of persistence standing).
private func withCoreStore<T>(_ body: (ConfirmedScientificImportManifestStore) async throws -> T) async throws -> T {
    try await withCoreStoreRoot { storeRoot in
        try await body(ConfirmedScientificImportManifestStore(rootDirectory: storeRoot))
    }
}

/// Creates a temporary store root and yields its URL, so a test can build MORE THAN ONE store instance
/// over the same on-disk root — e.g. to simulate a process restart that reads only what a prior store
/// instance persisted.
private func withCoreStoreRoot<T>(_ body: (URL) async throws -> T) async throws -> T {
    let root = FileManager.default.temporaryDirectory
        .appendingPathComponent("PersistCodecStore", isDirectory: true)
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: root) }
    return try await body(root.appendingPathComponent("store", isDirectory: true))
}

// MARK: - Round-trip

@Test
func persistenceCodecRoundTripsMinimalReceipt() throws {
    let receipt = makeReceipt()
    let decoded = try ConfirmedScientificImportManifestPersistence.decode(
        ConfirmedScientificImportManifestPersistence.encode(receipt)
    )
    #expect(decoded == receipt)
}

private func roundTrips(_ receipt: ConfirmedScientificImportReceipt) throws {
    let decoded = try ConfirmedScientificImportManifestPersistence.decode(
        ConfirmedScientificImportManifestPersistence.encode(receipt)
    )
    #expect(decoded == receipt)
    // A round-trippable branch fixture is also a valid receipt graph.
    #expect(throws: Never.self) { try ConfirmedScientificImportManifestPersistence.checkReceiptGraph(receipt) }
}

@Test
func roundTripsRichSingleUnitBranches() throws {
    // Valid: header rule present, Single-unit (so `.collapseExact` is permitted), two groups whose
    // flattened columns are the contiguous partition 1...5, event-relative + recording-elapsed bases,
    // both orderings, all scalar types, both roles, all unit branches, both empty-string policies.
    let receipt = makeReceipt(
        headerRule: .firstRecordIsHeader,
        sourceTimeUnit: .seconds,
        activityMode: .putativeSingleUnit,
        groups: [
            receiptGroupValue(
                id: "group_a",
                timeBasis: .eventRelative(originColumnOneBasedIndex: 3, originDataRowOneBasedIndex: 1),
                spikes: [
                    receiptSpike(1, "unit_a", order: .preserveSourceOrder, duplicate: .collapseExact),
                    receiptSpike(2, "unit_b", order: .stableAscendingSort, duplicate: .preserveMultiplicity),
                ],
                events: [receiptEvent(3, "stim", type: "stimulus", order: .stableAscendingSort)]
            ),
            receiptGroupValue(
                id: "group_b",
                timeBasis: .recordingElapsed,
                spikes: [receiptSpike(4, "unit_c", order: .preserveSourceOrder, duplicate: .preserveMultiplicity)],
                events: [receiptEvent(5, "reward", type: "reward", order: .preserveSourceOrder)]
            ),
        ],
        attributes: [
            receiptAttribute("amp", role: .scientific, type: .integer, unit: .dimensionless),
            receiptAttribute("beta", role: .presentation, type: .exactDecimal, unit: .specified(try OpaqueUnitSymbol(validating: "mV"))),
            receiptAttribute("delta", role: .scientific, type: .boolean, unit: .notApplicable),
            receiptAttribute("gamma", role: .scientific, type: .string, unit: .notApplicable, emptyStringPolicy: .allowExplicitEmptyString),
        ]
    )
    try roundTrips(receipt)
}

@Test
func roundTripsHeaderlessBranch() throws {
    // Valid headerless topology: one group, no event definitions, recording-elapsed basis.
    let receipt = makeReceipt(
        headerRule: .headerless,
        sourceTimeUnit: .milliseconds,
        activityMode: .putativeSingleUnit,
        recordingSegment: standardConfirmedRecordingSegment(regime: .unknownOrUncertain, coverage: .unknownOrUncertain),
        groups: [receiptGroupValue(id: "group_1", spikes: [receiptSpike(1, "unit_a")])]
    )
    try roundTrips(receipt)
}

@Test
func roundTripsXlsxMultiUnitBranch() throws {
    // Valid: XLSX selection, intentional multi-unit (so no `.collapseExact`), trialized regime,
    // partial coverage, two spike trains forming the partition 1...2.
    let receipt = makeReceipt(
        binding: worksheetBinding(),
        headerRule: .firstRecordIsHeader,
        sourceTimeUnit: .seconds,
        activityMode: .intentionalMultiUnit,
        recordingSegment: standardConfirmedRecordingSegment(
            id: "segment_z", regime: .trialized, coverage: .notAllSpikeTrainsFullImportedExcerpt
        ),
        groups: [receiptGroupValue(
            id: "group_1",
            spikes: [receiptSpike(1, "unit_a"), receiptSpike(2, "unit_b")]
        )]
    )
    try roundTrips(receipt)
}

// MARK: - Projection from a real confirmed manifest

@Test
func persistenceProjectsReceiptFromConfirmedManifest() throws {
    let manifest = try richConfirmedManifest()
    let receipt = try ConfirmedScientificImportManifestPersistence.project(from: manifest)
    // Carries the canonical fingerprint identity triple verbatim — the sole scientific identity.
    #expect(receipt.canonicalSchemaContractID == manifest.canonicalFingerprint.schemaContractID)
    #expect(receipt.canonicalSchemaContractDigest == manifest.canonicalFingerprint.schemaContractDigest)
    #expect(receipt.canonicalDatasetDigest == manifest.canonicalFingerprint.datasetDigest)
    #expect(receipt.sourceTransactionBinding == manifest.sourceTransactionBinding)
    #expect(receipt.activityMode == manifest.activityMode)
    #expect(receipt.recordingSegment == manifest.recordingSegment)
    // A Presentation-role attribute survives into the receipt.
    #expect(receipt.attributeDefinitions.contains { $0.role == .presentation })

    let decoded = try ConfirmedScientificImportManifestPersistence.decode(
        ConfirmedScientificImportManifestPersistence.encode(receipt)
    )
    #expect(decoded == receipt)
}

@Test
func persistenceProjectionIsDeterministic() throws {
    let manifest = try richConfirmedManifest()
    // The header rule is derived from the sealed base (headerful here); projection is deterministic.
    let first = try ConfirmedScientificImportManifestPersistence.project(from: manifest)
    let second = try ConfirmedScientificImportManifestPersistence.project(from: manifest)
    #expect(first == second)
    #expect(first.confirmationRecordDigest == second.confirmationRecordDigest)
}

// MARK: - Independent SHA-256 oracle over the record body

@Test
func persistenceRecordDigestMatchesIndependentSHA256Oracle() throws {
    let receipt = makeReceipt()
    let file = ConfirmedScientificImportManifestPersistence.encode(receipt)
    // The trailing record-digest field is [tag:1][int64 len:8][64 hex bytes] = 73 bytes.
    let bodyByteCount = file.count - 73
    let body = Array(file[0..<bodyByteCount])
    let oracle = SHA256.hash(data: Data(body)).map { String(format: "%02x", $0) }.joined()
    #expect(oracle == receipt.confirmationRecordDigest)
    // And the file's trailing field carries exactly that digest.
    let decoded = try ConfirmedScientificImportManifestPersistence.decode(file)
    #expect(decoded.confirmationRecordDigest == oracle)
}

// MARK: - Goldens (deterministic schema/record bytes and digests)

@Test
func persistenceSchemaContractIdentityIsStable() {
    #expect(ConfirmedScientificImportManifestPersistence.schemaContractID
        == "confirmed_scientific_import_manifest_source_bound_decision_receipt")
    #expect(ConfirmedScientificImportManifestPersistence.schemaContractDigest()
        == PersistenceGoldens.schemaContractDigest)
}

@Test
func persistenceFixedReceiptHasGoldenRecordDigestAndBytes() {
    let receipt = makeReceipt()
    #expect(receipt.confirmationRecordDigest == PersistenceGoldens.recordDigest)
    let hex = ConfirmedScientificImportManifestPersistence.encode(receipt)
        .map { String(format: "%02x", $0) }.joined()
    #expect(hex == PersistenceGoldens.fileHex)
}

// MARK: - No raw-source leakage

@Test
func persistenceReceiptContainsNoRawSourceValues() throws {
    // Distinctive header text and a distinctive spike timestamp lexeme that must never reach the
    // receipt (which stores confirmed decisions only, never source text/cells/timestamps).
    let staged = try stagedForPersistence(
        headers: ["RAW_HEADER_SENTINEL_ZZZ"],
        columns: [[.text(rawText: "123123123"), .text(rawText: "987654321")]],
        bindingCharacter: "c"
    )
    let group = EventScopeGroupManifestDraft(
        semanticID: psGroupID("group_1"),
        spikeTrains: [spikeDraft(1, "unit_a")],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    let manifest = ScientificImportConfirmationBuilder.confirm(
        try confirmableForPersistence(staged: staged, groups: [group])
    )
    let receipt = try ConfirmedScientificImportManifestPersistence.project(from: manifest)
    let file = ConfirmedScientificImportManifestPersistence.encode(receipt)

    #expect(firstIndex(of: bytes("RAW_HEADER_SENTINEL_ZZZ"), in: file) == nil)
    #expect(firstIndex(of: bytes("987654321"), in: file) == nil)
    #expect(firstIndex(of: bytes("123123123"), in: file) == nil)
    // The confirmed semantic ID (a decision) is legitimately present.
    #expect(firstIndex(of: bytes("unit_a"), in: file) != nil)
}

// MARK: - Failure injection (fail closed on every defect)

@Test
func persistenceRejectsFileLargerThanLimit() {
    let oversized = [UInt8](repeating: 0, count: ConfirmedScientificImportManifestPersistence.maximumFileByteCount + 1)
    decodeThrows(oversized, .fileTooLarge(maximumBytes: ConfirmedScientificImportManifestPersistence.maximumFileByteCount))
}

@Test
func persistenceRejectsTruncatedAndTrailingBytes() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // Truncation somewhere in the middle.
    #expect(throws: (any Error).self) {
        _ = try ConfirmedScientificImportManifestPersistence.decode(Array(file.prefix(file.count / 2)))
    }
    // A single trailing byte after a complete record.
    decodeThrows(file + [0x00], .trailingBytes)
}

@Test
func persistenceRejectsUnexpectedFieldTag() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // The byte immediately after the observation-bounds token is the event-scope-groups field tag.
    let boundsToken = bytes("observation_bounds.unknown_or_unavailable")
    let start = firstIndex(of: boundsToken, in: file)!
    let tagOffset = start + boundsToken.count
    decodeThrows(overwriting(file, at: tagOffset, count: 1, with: 0x7F),
                 .unexpectedFieldTag(expected: 0x20, found: 0x7F))
}

@Test
func persistenceRejectsUnknownToken() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // Flip a middle character of the activity-mode token to a different valid-UTF8 letter.
    let token = bytes("activity_mode.putative_single_unit")
    let start = firstIndex(of: token, in: file)!
    decodeThrows(overwriting(file, at: start, count: 1, with: bytes("x")[0]),
                 .unknownToken("xctivity_mode.putative_single_unit"))
}

@Test
func persistenceRejectsInvalidUTF8() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // Corrupt a byte inside the source-SHA token to an invalid UTF-8 lead byte.
    let sha = bytes(String(repeating: "a", count: 64))
    let start = firstIndex(of: sha, in: file)!
    decodeThrows(overwriting(file, at: start, count: 1, with: 0xFF), .invalidUTF8)
}

@Test
func persistenceRejectsInvalidSourceBinding() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // Uppercase the source SHA: valid UTF-8, but not the required lowercase hex.
    let sha = bytes(String(repeating: "a", count: 64))
    let start = firstIndex(of: sha, in: file)!
    decodeThrows(overwriting(file, at: start, count: 1, with: bytes("Z")[0]), .invalidSourceBinding)
}

@Test
func persistenceRejectsInvalidIdentifier() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // Put a control character into the segment-ID token: valid UTF-8, rejected as a semantic ID.
    let segment = bytes("segment_a")
    let start = firstIndex(of: segment, in: file)!
    decodeThrows(overwriting(file, at: start, count: 1, with: 0x00), .invalidIdentifier)
}

@Test
func persistenceRejectsUnsupportedSchemaAndDigestMismatch() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    let schemaID = bytes(ConfirmedScientificImportManifestPersistence.schemaContractID)
    let idStart = firstIndex(of: schemaID, in: file)!
    decodeThrows(overwriting(file, at: idStart, count: 1, with: bytes("x")[0]), .unsupportedSchemaContract)

    // The persistence schema digest is [tag:1][len:8][64 hex] immediately after the schema-ID token.
    let digestOffset = idStart + schemaID.count + 1 + 8
    decodeThrows(overwriting(file, at: digestOffset, count: 1, with: bytes("0")[0] == file[digestOffset] ? bytes("1")[0] : bytes("0")[0]),
                 .schemaContractDigestMismatch)
}

@Test
func persistenceRejectsRecordDigestMismatch() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // Flip the last byte (inside the stored record-digest hex).
    let last = file.count - 1
    let replacement: UInt8 = file[last] == bytes("0")[0] ? bytes("1")[0] : bytes("0")[0]
    decodeThrows(overwriting(file, at: last, count: 1, with: replacement), .recordDigestMismatch)
}

@Test
func persistenceRejectsTokenLengthOverflow() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // The persistence schema-ID token's 8-byte length prefix sits right after the domain token and
    // its field tag. Overwrite it with a huge length.
    let domainLen = bytes("stpd.confirmed_scientific_import_manifest_receipt").count
    let lengthPrefixOffset = 8 + domainLen + 1
    decodeThrows(overwritingInt64(file, at: lengthPrefixOffset, with: Int64.max), .tokenLengthOutOfRange)
}

@Test
func persistenceRejectsCountOverflow() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // The event-scope-groups count is the int64 right after its field tag, which follows the
    // observation-bounds token.
    let boundsToken = bytes("observation_bounds.unknown_or_unavailable")
    let start = firstIndex(of: boundsToken, in: file)!
    let countOffset = start + boundsToken.count + 1
    decodeThrows(overwritingInt64(file, at: countOffset, with: Int64.max), .countOutOfRange)
}

@Test
func persistenceRejectsInvalidSourceColumnIndex() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt())
    // The single spike train's source column int64 precedes its ID token ([tag:1][int64:8]).
    let idToken = bytes("spike_one")
    let idStart = firstIndex(of: idToken, in: file)!
    let columnOffset = idStart - 8 - 1 - 8 // ID token length prefix (8) + ID tag (1) + column int64 (8)
    decodeThrows(overwritingInt64(file, at: columnOffset, with: 0), .invalidSourceColumnIndex)
}

@Test
func persistenceRejectsNoncanonicalCollectionOrder() throws {
    // Attributes out of key order — the reader must reject noncanonical order.
    let outOfOrder = makeReceipt(attributes: [
        receiptAttribute("zeta"), receiptAttribute("alpha"),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(outOfOrder), .noncanonicalCollectionOrder)

    // Spike columns out of source-column order within a group.
    let unsortedSpikes = makeReceipt(groups: [receiptGroupValue(
        spikes: [receiptSpike(2, "unit_b"), receiptSpike(1, "unit_a")]
    )])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(unsortedSpikes), .noncanonicalCollectionOrder)
}

@Test
func persistenceRejectsInvalidWorksheetSheetID() {
    let file = ConfirmedScientificImportManifestPersistence.encode(makeReceipt(binding: worksheetBinding()))
    // The worksheet sheetID int64 immediately follows the worksheet-name token ([tag:1][int64:8]).
    let nameToken = bytes("Data")
    let nameStart = firstIndex(of: nameToken, in: file)!
    let sheetIDOffset = nameStart + nameToken.count + 1
    decodeThrows(overwritingInt64(file, at: sheetIDOffset, with: 0), .invalidWorksheetSheetID)
}

// MARK: - Draft reconstruction round-trips through the chain

@Test
func persistenceReconstructedDraftReplaysToSameFingerprint() throws {
    let manifest = try richConfirmedManifest()
    let receipt = try ConfirmedScientificImportManifestPersistence.project(from: manifest)

    // Re-stage identical source bytes (same shape) and reconstruct the draft from the receipt.
    let restaged = try stagedForPersistence(
        headers: ["unit_a", "unit_b", "event_x"],
        columns: [
            [.text(rawText: "1"), .text(rawText: "2")],
            [.text(rawText: "3"), .text(rawText: "4")],
            [.text(rawText: "5"), .blank],
        ]
    )
    let draft = try ConfirmedScientificImportManifestPersistence.reconstructDraft(from: receipt, boundTo: restaged)
    let plan = try ScientificImportPlanResolver.resolve(stagedImport: restaged, draft: draft)
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    guard case .accepted(let validated) =
        PreparedScientificImportValidator.validateForCanonicalProjection(prepared) else {
        Issue.record("Expected the reconstructed draft to validate")
        return
    }
    let replayed = ScientificImportConfirmationBuilder.confirm(
        try CanonicalScientificImportProjector.projectConfirmable(validated)
    )
    #expect(replayed.canonicalFingerprint == manifest.canonicalFingerprint)
    let replayedReceipt = try ConfirmedScientificImportManifestPersistence.project(from: replayed)
    #expect(replayedReceipt == receipt)
}

// MARK: - Bounded filesystem reader (readUntrusted — never mints standing)

@Test
func readUntrustedReturnsBareReceiptOnly() throws {
    // A generic bounded read from an arbitrary file returns only an untrusted decoded receipt. There
    // is no public/package API that turns it into a `PersistedConfirmedScientificImportManifest`:
    // `verified(…)` is internal to STPDCore, and the store is its only caller.
    let receipt = makeReceipt()
    let file = try writeTempFile(ConfirmedScientificImportManifestPersistence.encode(receipt))
    defer { file.cleanup() }
    let decoded = try ConfirmedScientificImportManifestPersistence.readUntrusted(fileURL: file.url)
    #expect(decoded == receipt)
}

@Test
func readUntrustedRejectsFileLargerThan8MiB() throws {
    let oversized = [UInt8](
        repeating: 0x41,
        count: ConfirmedScientificImportManifestPersistence.maximumFileByteCount + 1
    )
    let file = try writeTempFile(oversized, name: "big.stpdimportmanifest")
    defer { file.cleanup() }
    #expect(throws: PersistedStoreReadError.fileTooLarge(
        maximumBytes: ConfirmedScientificImportManifestPersistence.maximumFileByteCount
    )) {
        _ = try ConfirmedScientificImportManifestPersistence.readUntrusted(fileURL: file.url)
    }
}

@Test
func readUntrustedRejectsSymlink() throws {
    let file = try writeTempFile(ConfirmedScientificImportManifestPersistence.encode(makeReceipt()))
    defer { file.cleanup() }
    let link = file.url.deletingLastPathComponent().appendingPathComponent("link.stpdimportmanifest")
    try FileManager.default.createSymbolicLink(at: link, withDestinationURL: file.url)
    // The real file reads fine; a symlink to it is rejected as non-regular (O_NOFOLLOW).
    #expect(throws: PersistedStoreReadError.notRegularFile) {
        _ = try ConfirmedScientificImportManifestPersistence.readUntrusted(fileURL: link)
    }
    #expect((try? ConfirmedScientificImportManifestPersistence.readUntrusted(fileURL: file.url)) != nil)
}

@Test
func readUntrustedRejectsNonRegularFile() throws {
    // A directory is not a regular file.
    let dir = FileManager.default.temporaryDirectory
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: dir) }
    #expect(throws: PersistedStoreReadError.notRegularFile) {
        _ = try ConfirmedScientificImportManifestPersistence.readUntrusted(fileURL: dir)
    }
}

/// Box carrying the read outcome out of the worker thread the read runs on.
private final class FifoReadOutcome: @unchecked Sendable {
    var error: PersistedStoreReadError?
    var returned = false
}

@Test
func readUntrustedOnFifoReturnsPromptlyInsteadOfBlocking() async throws {
    // A FIFO substituted for a regular file must NOT block the open: the reader opens O_NONBLOCK, so the
    // open returns immediately even with no writer, and fstat then rejects it as non-regular. Bounded by
    // a watchdog so a regression (the old blocking O_RDONLY open, which waits forever for a writer) fails
    // this test deterministically instead of hanging it.
    let dir = FileManager.default.temporaryDirectory
        .appendingPathComponent("PersistCodecTests", isDirectory: true)
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: dir) }
    let fifo = dir.appendingPathComponent("receipt.stpdimportmanifest")
    #expect(fifo.path.withCString { mkfifo($0, 0o600) } == 0)

    // Run the synchronous read on a worker thread so a hypothetical blocking open cannot wedge the test
    // thread; the watchdog turns a hang into a failed assertion rather than a hung suite.
    let outcome = FifoReadOutcome()
    let done = DispatchSemaphore(value: 0)
    DispatchQueue.global().async {
        do { _ = try ConfirmedScientificImportManifestPersistence.readUntrusted(fileURL: fifo) }
        catch let error as PersistedStoreReadError { outcome.error = error }
        catch {}
        outcome.returned = true
        done.signal()
    }
    let completed = await withCheckedContinuation { (continuation: CheckedContinuation<Bool, Never>) in
        DispatchQueue.global().async {
            continuation.resume(returning: done.wait(timeout: .now() + .seconds(5)) == .success)
        }
    }
    #expect(completed, "readUntrusted must not block on a FIFO — the O_NONBLOCK open returns immediately")
    #expect(outcome.returned)
    #expect(outcome.error == .notRegularFile)
}

@Test
func storeSaveMintsStandingOnExactMatch() async throws {
    // Persistence standing can be minted only by the store, after it durably persists and reads back
    // its own record. The mint (`verified`) is capability-gated: only the store can construct the
    // opaque `DurableRecordCapability`, so no other code path can promote a receipt.
    try await withCoreStore { store in
        let manifest = try richConfirmedManifest()
        // The store derives the receipt (header rule included) from the base; no caller receipt/rule.
        let receipt = try ConfirmedScientificImportManifestPersistence.project(from: manifest)
        let result = try await store.save(baseConfirmation: manifest)
        #expect(result.outcome == .written)
        #expect(result.manifest.confirmationRecordDigest == receipt.confirmationRecordDigest)
        #expect(result.manifest.canonicalFingerprint == manifest.canonicalFingerprint)
    }
}

// MARK: - Receipt-graph invariant rejection

@Test
func decodeRejectsEmptyGroupCollection() {
    decodeThrows(
        ConfirmedScientificImportManifestPersistence.encode(makeReceipt(groups: [])),
        .emptyGroupCollection
    )
}

@Test
func decodeRejectsGroupWithoutSpikeTrain() {
    let receipt = makeReceipt(groups: [
        receiptGroupValue(spikes: [], events: [receiptEvent(1, "stim", type: "stimulus")]),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .groupWithoutSpikeTrain)
}

@Test
func decodeRejectsDuplicateSourceColumnAcrossGroups() {
    let receipt = makeReceipt(groups: [
        receiptGroupValue(id: "group_a", spikes: [receiptSpike(1, "unit_a")], events: [receiptEvent(3, "e1", type: "stim")]),
        receiptGroupValue(id: "group_b", spikes: [receiptSpike(2, "unit_b")], events: [receiptEvent(3, "e2", type: "stim")]),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .duplicateSourceColumn)
}

@Test
func decodeRejectsDuplicateGroupIdentity() {
    let receipt = makeReceipt(groups: [
        receiptGroupValue(id: "same", spikes: [receiptSpike(1, "unit_a")]),
        receiptGroupValue(id: "same", spikes: [receiptSpike(2, "unit_b")]),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .duplicateSemanticIdentity)
}

@Test
func decodeRejectsInvalidGroupPartition() {
    // A spike column must precede every event column in the same group.
    let receipt = makeReceipt(groups: [
        receiptGroupValue(spikes: [receiptSpike(5, "unit_a")], events: [receiptEvent(3, "e1", type: "stim")]),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .invalidGroupPartition)
}

@Test
func decodeRejectsEventRelativeOriginNotInGroup() {
    let receipt = makeReceipt(groups: [
        receiptGroupValue(
            timeBasis: .eventRelative(originColumnOneBasedIndex: 9, originDataRowOneBasedIndex: 1),
            spikes: [receiptSpike(1, "unit_a")],
            events: [receiptEvent(2, "e1", type: "stim")]
        ),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .eventRelativeOriginNotInGroup)
}

@Test
func decodeRejectsInvalidAttributeCombination() {
    // `allowExplicitEmptyString` is only valid for string-typed attributes.
    let receipt = makeReceipt(attributes: [
        receiptAttribute("flag", type: .boolean, emptyStringPolicy: .allowExplicitEmptyString),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .invalidAttributeCombination)
}

@Test
func decodeRejectsMissingSourceColumnGap() {
    // Columns 1 and 3, with 2 missing — not a contiguous 1...N partition.
    let receipt = makeReceipt(groups: [
        receiptGroupValue(spikes: [receiptSpike(1, "unit_a"), receiptSpike(3, "unit_b")]),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .invalidGroupPartition)
}

@Test
func decodeRejectsCrossGroupColumnInterleaving() {
    let receipt = makeReceipt(groups: [
        receiptGroupValue(id: "group_a", spikes: [receiptSpike(1, "unit_a"), receiptSpike(3, "unit_c")]),
        receiptGroupValue(id: "group_b", spikes: [receiptSpike(2, "unit_b")]),
    ])
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .invalidGroupPartition)
}

@Test
func decodeRejectsHeaderlessWithMultipleGroups() {
    let receipt = makeReceipt(
        headerRule: .headerless,
        groups: [
            receiptGroupValue(id: "group_a", spikes: [receiptSpike(1, "unit_a")]),
            receiptGroupValue(id: "group_b", spikes: [receiptSpike(2, "unit_b")]),
        ]
    )
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .headerlessTopologyInvalid)
}

@Test
func decodeRejectsHeaderlessWithEventDefinitions() {
    let receipt = makeReceipt(
        headerRule: .headerless,
        groups: [receiptGroupValue(
            id: "group_1",
            spikes: [receiptSpike(1, "unit_a")],
            events: [receiptEvent(2, "stim", type: "stimulus")]
        )]
    )
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .headerlessTopologyInvalid)
}

@Test
func decodeRejectsCollapseExactForMultiUnit() {
    let receipt = makeReceipt(
        activityMode: .intentionalMultiUnit,
        groups: [receiptGroupValue(spikes: [receiptSpike(1, "unit_a", duplicate: .collapseExact)])]
    )
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .collapseNotAllowedForActivityMode)
}

@Test
func decodeRejectsCollapseExactForUnknownMode() {
    let receipt = makeReceipt(
        activityMode: .unknownOrUncertain,
        groups: [receiptGroupValue(spikes: [receiptSpike(1, "unit_a", duplicate: .collapseExact)])]
    )
    decodeThrows(ConfirmedScientificImportManifestPersistence.encode(receipt), .collapseNotAllowedForActivityMode)
}

@Test
func decodeAcceptsEventDefinitionIDReusedAcrossGroups() throws {
    // Two groups may legitimately reuse an event-definition ID such as `stimulus`; the reuse is
    // group-local. Spike-train IDs remain dataset-unique.
    let receipt = makeReceipt(
        headerRule: .firstRecordIsHeader,
        groups: [
            receiptGroupValue(id: "group_a", spikes: [receiptSpike(1, "unit_a")], events: [receiptEvent(2, "stimulus", type: "stim")]),
            receiptGroupValue(id: "group_b", spikes: [receiptSpike(3, "unit_b")], events: [receiptEvent(4, "stimulus", type: "stim")]),
        ]
    )
    let decoded = try ConfirmedScientificImportManifestPersistence.decode(
        ConfirmedScientificImportManifestPersistence.encode(receipt)
    )
    #expect(decoded == receipt)
    // The shared event-definition ID does not collapse the two distinct spike-train identities.
    let spikeIDs = decoded.eventScopeGroups.flatMap { $0.spikeTrains.map(\.semanticID.semanticID.canonicalText) }
    #expect(Set(spikeIDs).count == spikeIDs.count)
}

@Test
func twoGroupEventReuseSavesAndRestoresEndToEnd() async throws {
    // A GENUINE restart-replay of the group-local reuse contract: two groups reuse the event-definition
    // ID `stimulus`, are saved to the store, and are then restore-verified against a base rebuilt from
    // scratch — a fresh staged import plus a draft reconstructed from the ON-DISK receipt — never the
    // original in-memory prepared/validated/confirmable/base. This proves the reuse survives a real disk
    // round-trip and replay, not merely that a value equals itself.

    // === Original session: build the base through the COMPLETE chain and project its receipt. ===
    // The header rule is DERIVED from the sealed base (headerful), never supplied.
    let originalBase = try twoGroupEventReuseConfirmedManifest()
    let originalReceipt = try ConfirmedScientificImportManifestPersistence.project(from: originalBase)
    #expect(try ConfirmedScientificImportManifestPersistence.deriveHeaderRule(from: originalBase)
        == .firstRecordIsHeader)
    #expect(originalReceipt.headerRule == .firstRecordIsHeader)
    #expect(originalReceipt.eventScopeGroups.map { $0.eventDefinitions.map(\.semanticID.semanticID.canonicalText) }
        == [["stimulus"], ["stimulus"]])

    try await withCoreStoreRoot { storeRoot in
        let sourceSHA = originalReceipt.sourceTransactionBinding.sourceBytesSHA256
        let recordDigest = originalReceipt.confirmationRecordDigest

        // Session 1 writes the record (deriving it from the base) and is then discarded.
        let session1 = ConfirmedScientificImportManifestStore(rootDirectory: storeRoot)
        #expect(try await session1.save(baseConfirmation: originalBase).outcome == .written)

        // === Simulated restart: a NEW store instance over the same root reads only from disk. ===
        let session2 = ConfirmedScientificImportManifestStore(rootDirectory: storeRoot)
        let decodedReceipt = try await session2.readForReplay(sourceSHA256: sourceSHA, recordDigest: recordDigest)

        // Rebuild an INDEPENDENT staged import from the same semantic source facts and exact source
        // binding — a brand-new value, not the original prepared/validated object. This Core test
        // reconstructs it with the KNOWN headerful source interpretation directly; it does NOT drive
        // header staging from the decoded receipt (receipt-driven header staging is exercised by the
        // application-level coordinator restart tests, including the new headerless case).
        let freshStaged = try twoGroupEventReuseStagedImport()
        #expect(freshStaged.sourceTransactionBinding?.sourceBytesSHA256 == sourceSHA)

        // Reconstruct the draft from the DECODED disk receipt (not the original draft) and run exactly
        // one fresh replay chain: resolver → normalizer → validator → projector → confirmation builder.
        let replayedDraft = try ConfirmedScientificImportManifestPersistence.reconstructDraft(
            from: decodedReceipt, boundTo: freshStaged
        )
        let plan = try ScientificImportPlanResolver.resolve(stagedImport: freshStaged, draft: replayedDraft)
        let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
        guard case .accepted(let validated) =
            PreparedScientificImportValidator.validateForCanonicalProjection(prepared) else {
            Issue.record("Fresh replay: the validator rejected the reconstructed two-group reuse import")
            return
        }
        let replayedConfirmable = try CanonicalScientificImportProjector.projectConfirmable(validated)
        let replayedBase = ScientificImportConfirmationBuilder.confirm(replayedConfirmable)

        // The decoded receipt drives DRAFT reconstruction here (not header staging, which was fixed to
        // the known headerful interpretation above). The store-DERIVED rule from the replayed base must
        // still independently match the on-disk receipt's rule before any mint.
        #expect(decodedReceipt.headerRule == .firstRecordIsHeader)
        #expect(try ConfirmedScientificImportManifestPersistence.deriveHeaderRule(from: replayedBase)
            == decodedReceipt.headerRule)

        // The fresh replay reproduces the original scientific identity and the EXACT stored receipt.
        #expect(replayedBase.canonicalFingerprint == originalBase.canonicalFingerprint)
        #expect(try ConfirmedScientificImportManifestPersistence.project(from: replayedBase)
            == decodedReceipt)

        // Mint standing by matching the on-disk record against the REPLAYED base (never the original);
        // the store derives the authoritative header rule from the replayed base — no header parameter.
        let restored = try await session2.restoreVerify(
            sourceSHA256: sourceSHA, recordDigest: recordDigest,
            baseConfirmation: replayedBase
        )
        #expect(restored.confirmationRecordDigest == decodedReceipt.confirmationRecordDigest)
        #expect(restored.canonicalFingerprint == originalBase.canonicalFingerprint)

        // --- The group-local reuse survived the disk round-trip intact. ---
        let groups = decodedReceipt.eventScopeGroups
        #expect(groups.count == 2)
        #expect(groups.map { $0.semanticID.semanticID.canonicalText } == ["group_a", "group_b"])
        // Each group still owns exactly one `stimulus` event definition...
        for group in groups {
            #expect(group.eventDefinitions.map(\.semanticID.semanticID.canonicalText) == ["stimulus"])
        }
        // ...and the two group-local definitions are DISTINCT members, not collapsed into one.
        let allEventDefinitions = groups.flatMap(\.eventDefinitions)
        #expect(allEventDefinitions.count == 2)
        #expect(Set(allEventDefinitions.map(\.sourceColumnOneBasedIndex)) == Set([2, 4]))

        // Spike-train IDs remain dataset-wide unique.
        let spikeIDs = groups.flatMap { $0.spikeTrains.map(\.semanticID.semanticID.canonicalText) }
        #expect(spikeIDs == ["unit_a", "unit_b"])
        #expect(Set(spikeIDs).count == spikeIDs.count)

        // Group membership and the ordered source-column partition are unchanged (group_a = [1,2],
        // group_b = [3,4]; the flattened partition is a contiguous 1..4).
        let partitionByGroup = groups.map { group in
            (group.spikeTrains.map(\.sourceColumnOneBasedIndex)
                + group.eventDefinitions.map(\.sourceColumnOneBasedIndex)).sorted()
        }
        #expect(partitionByGroup == [[1, 2], [3, 4]])
        #expect(partitionByGroup.flatMap { $0 } == [1, 2, 3, 4])

        // No detector/export/authority state is created: the wrapper removes ONLY the persistence
        // blocker; deny-only readiness is preserved with every other blocker retained.
        let readiness = ScientificAnalysisReadinessEvaluator.assess(restored)
        #expect(readiness.isAnalysisBlocked)
        #expect(!readiness.blockers.contains(.confirmedManifestPersistenceUnavailable))
        #expect(readiness.blockers.contains(.detectorConsumerClosureUnavailable))
        #expect(readiness.blockers.contains(.authoritativeExportClosureUnavailable))
        #expect(readiness.blockers.contains(.runContractUnavailable))
    }
}

// MARK: - Store verification mismatch kinds (through the durable store)

/// Saves `saved` (with its projected receipt), then restore-verifies the SAME derived path against a
/// deliberately different `against` base, returning the store's verification mismatch (if any).
private func restoreMismatch(
    saved: ConfirmedScientificImportManifest,
    against base: ConfirmedScientificImportManifest
) async throws -> PersistedConfirmationVerificationError? {
    try await withCoreStore { store in
        let savedReceipt = try ConfirmedScientificImportManifestPersistence.project(from: saved)
        _ = try await store.save(baseConfirmation: saved)
        do {
            _ = try await store.restoreVerify(
                sourceSHA256: savedReceipt.sourceTransactionBinding.sourceBytesSHA256,
                recordDigest: savedReceipt.confirmationRecordDigest,
                baseConfirmation: base
            )
            return nil
        } catch ConfirmedScientificImportManifestStoreError.verification(let error) {
            return error
        }
    }
}

@Test
func storeRejectsSourceBindingMismatch() async throws {
    // The stored record's source binding differs from the base being verified.
    let saved = try richConfirmedManifest(bindingCharacter: "a")
    let differentSource = try richConfirmedManifest(bindingCharacter: "f")
    #expect(try await restoreMismatch(saved: saved, against: differentSource) == .sourceBindingMismatch)
}

@Test
func storeRejectsCanonicalFingerprintMismatch() async throws {
    // Same source binding, different scientific content = replay divergence.
    let saved = try richConfirmedManifest(bindingCharacter: "a")
    let differentFingerprint = try simpleConfirmedManifest(bindingCharacter: "a")
    #expect(try await restoreMismatch(saved: saved, against: differentFingerprint) == .canonicalFingerprintMismatch)
}

@Test
func storeRejectsConfirmationRecordMismatchWhenFingerprintUnchanged() async throws {
    // A DELIBERATELY constructed fixture that isolates a confirmation-record-only mismatch: both bases
    // are built from the identical canonical cell data (the same two timestamps), so their canonical
    // dataset fingerprints are equal by construction — NOT because header presence happens to be
    // unhashed. The only difference is the derived header rule (headerful vs headerless), a
    // confirmation-record decision. Verifying the stored headerful record against the headerless base
    // makes the base-derived rule (headerless) differ from the decoded headerful record in exactly the
    // header rule and its digest. (Changing header interpretation can move first-record values into or
    // out of canonical data; the fingerprint changes when the canonical data differ, while semantically
    // equivalent canonical data can retain the same fingerprint. Here the cells are held equivalent on
    // purpose, so the fingerprint is unchanged and only the confirmation record differs.)
    let savedHeaderful = try simpleConfirmedManifest(bindingCharacter: "a", headerPresent: true)
    let sameDataHeaderless = try simpleConfirmedManifest(bindingCharacter: "a", headerPresent: false)
    // Equal canonical cell data → equal fingerprint (this fixture keeps the scientific data identical).
    #expect(savedHeaderful.canonicalFingerprint == sameDataHeaderless.canonicalFingerprint)
    #expect(try await restoreMismatch(saved: savedHeaderful, against: sameDataHeaderless)
        == .confirmationRecordMismatch)
}

// MARK: - Readiness removes only the persistence blocker

@Test
func readinessPersistedWrapperRemovesOnlyPersistenceBlocker() async throws {
    let manifest = try richConfirmedManifest()
    let wrapper = try await withCoreStore { store -> PersistedConfirmedScientificImportManifest in
        try await store.save(baseConfirmation: manifest).manifest
    }

    let base = ScientificAnalysisReadinessEvaluator.assess(manifest)
    let persisted = ScientificAnalysisReadinessEvaluator.assess(wrapper)

    #expect(base.firstBlocker == .confirmedManifestPersistenceUnavailable)
    #expect(base.blockers.contains(.confirmedManifestPersistenceUnavailable))
    // Deny-only is preserved and exactly the persistence blocker is removed.
    #expect(persisted.isAnalysisBlocked)
    #expect(!persisted.blockers.contains(.confirmedManifestPersistenceUnavailable))
    #expect(Set(persisted.blockers)
        == Set(base.blockers).subtracting([.confirmedManifestPersistenceUnavailable]))
    // Every other blocker is retained.
    #expect(persisted.blockers.contains(.observationBoundsUnavailable))
    #expect(persisted.blockers.contains(.runContractUnavailable))
    #expect(persisted.blockers.contains(.detectorConsumerClosureUnavailable))
    #expect(persisted.blockers.contains(.authoritativeExportClosureUnavailable))
}

// MARK: - Header-rule derivation (the single authoritative source)

@Test
func headerRuleDerivationKernelInspectsHeaderPresenceOnly() throws {
    typealias P = ConfirmedScientificImportManifestPersistence
    // All columns carry a header → first record is header.
    #expect(try P.deriveHeaderRule(fromColumnHeaders: ["unit_a", "unit_b"]) == .firstRecordIsHeader)
    // No column carries a header → headerless.
    #expect(try P.deriveHeaderRule(fromColumnHeaders: [nil, nil]) == .headerless)
    // An explicitly present EMPTY header string still means "header present".
    #expect(try P.deriveHeaderRule(fromColumnHeaders: [""]) == .firstRecordIsHeader)
    // An explicitly present WHITESPACE-only header still means "header present" (text never inspected).
    #expect(try P.deriveHeaderRule(fromColumnHeaders: ["   "]) == .firstRecordIsHeader)
    #expect(try P.deriveHeaderRule(fromColumnHeaders: ["", "  ", "x"]) == .firstRecordIsHeader)
    // Empty column collection fails closed — never defaulted to headerless.
    #expect(throws: PersistedHeaderRuleDerivationError.noSourceColumns) {
        _ = try P.deriveHeaderRule(fromColumnHeaders: [])
    }
    // Mixed header presence fails closed, in either order.
    #expect(throws: PersistedHeaderRuleDerivationError.mixedHeaderPresence) {
        _ = try P.deriveHeaderRule(fromColumnHeaders: ["a", nil])
    }
    #expect(throws: PersistedHeaderRuleDerivationError.mixedHeaderPresence) {
        _ = try P.deriveHeaderRule(fromColumnHeaders: [nil, "a"])
    }
}

@Test
func projectDerivesFirstRecordIsHeaderFromRealHeaderfulBase() throws {
    // A real full-chain headerful confirmation derives `.firstRecordIsHeader` with no caller input.
    let base = try simpleConfirmedManifest(headerPresent: true)
    #expect(try ConfirmedScientificImportManifestPersistence.deriveHeaderRule(from: base) == .firstRecordIsHeader)
    #expect(try ConfirmedScientificImportManifestPersistence.project(from: base).headerRule == .firstRecordIsHeader)
}

@Test
func projectDerivesHeaderlessFromRealHeaderlessBase() throws {
    // A real full-chain headerless confirmation derives `.headerless` with no caller input.
    let base = try simpleConfirmedManifest(headerPresent: false)
    #expect(try ConfirmedScientificImportManifestPersistence.deriveHeaderRule(from: base) == .headerless)
    #expect(try ConfirmedScientificImportManifestPersistence.project(from: base).headerRule == .headerless)
}

@Test
func saveDerivesHeaderfulRuleFromHeaderfulBaseWithNoCallerInput() async throws {
    // Save accepts only a base; the store derives the receipt (header rule included) internally.
    let base = try simpleConfirmedManifest(bindingCharacter: "a", headerPresent: true)
    try await withCoreStore { store in
        let result = try await store.save(baseConfirmation: base)
        #expect(result.manifest.receipt.headerRule == .firstRecordIsHeader)
        // The ON-DISK record independently carries the derived rule.
        let onDisk = try await store.readForReplay(
            sourceSHA256: result.manifest.receipt.sourceTransactionBinding.sourceBytesSHA256,
            recordDigest: result.manifest.receipt.confirmationRecordDigest
        )
        #expect(onDisk.headerRule == .firstRecordIsHeader)
    }
}

@Test
func saveDerivesHeaderlessRuleFromHeaderlessBaseWithNoCallerInput() async throws {
    let base = try simpleConfirmedManifest(bindingCharacter: "a", headerPresent: false)
    try await withCoreStore { store in
        let result = try await store.save(baseConfirmation: base)
        #expect(result.manifest.receipt.headerRule == .headerless)
        let onDisk = try await store.readForReplay(
            sourceSHA256: result.manifest.receipt.sourceTransactionBinding.sourceBytesSHA256,
            recordDigest: result.manifest.receipt.confirmationRecordDigest
        )
        #expect(onDisk.headerRule == .headerless)
    }
}

// MARK: - Original exploit regression: header rule cannot be a second truth

/// Rebuilds a receipt with its header rule flipped and its record digest recomputed — a hand-crafted
/// contradiction that only a `@testable` test can place on disk (the public save API derives from the
/// base and cannot represent this).
private func receiptWithHeaderRuleFlipped(
    _ receipt: ConfirmedScientificImportReceipt
) -> ConfirmedScientificImportReceipt {
    let flipped: PersistedTabularHeaderRule =
        receipt.headerRule == .headerless ? .firstRecordIsHeader : .headerless
    func build(_ digest: String) -> ConfirmedScientificImportReceipt {
        ConfirmedScientificImportReceipt(
            canonicalSchemaContractID: receipt.canonicalSchemaContractID,
            canonicalSchemaContractDigest: receipt.canonicalSchemaContractDigest,
            canonicalDatasetDigest: receipt.canonicalDatasetDigest,
            sourceTransactionBinding: receipt.sourceTransactionBinding,
            headerRule: flipped,
            sourceTimeUnit: receipt.sourceTimeUnit,
            activityMode: receipt.activityMode,
            recordingSegment: receipt.recordingSegment,
            eventScopeGroups: receipt.eventScopeGroups,
            attributeDefinitions: receipt.attributeDefinitions,
            confirmationRecordDigest: digest
        )
    }
    return build(ConfirmedScientificImportManifestPersistence.recordDigest(of: build("")))
}

/// Writes a receipt's encoded bytes at the store's derived final path under `storeRoot`.
private func placeReceiptAtDerivedPath(
    _ receipt: ConfirmedScientificImportReceipt,
    underStoreRoot storeRoot: URL
) throws {
    let dir = storeRoot
        .appendingPathComponent("by-source", isDirectory: true)
        .appendingPathComponent(receipt.sourceTransactionBinding.sourceBytesSHA256, isDirectory: true)
    try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
    try Data(ConfirmedScientificImportManifestPersistence.encode(receipt))
        .write(to: dir.appendingPathComponent("\(receipt.confirmationRecordDigest).stpdimportmanifest"))
}

@Test
func headerfulBaseCannotMintStandingFromHeaderlessOnDiskReceipt() async throws {
    // The historical exploit: a headerful base being verified against a headerless on-disk receipt. The
    // store DERIVES the rule from the base (headerful) and compares the full expected receipt, so the
    // headerless disk record (identical fingerprint, flipped header rule) is rejected — no wrapper.
    try await withCoreStoreRoot { storeRoot in
        let headerfulBase = try simpleConfirmedManifest(headerPresent: true)
        let headerfulReceipt = try ConfirmedScientificImportManifestPersistence.project(from: headerfulBase)
        #expect(headerfulReceipt.headerRule == .firstRecordIsHeader)
        let contradictory = receiptWithHeaderRuleFlipped(headerfulReceipt)
        #expect(contradictory.headerRule == .headerless)
        // Same source binding and fingerprint; only the header rule (and its digest) differ.
        #expect(contradictory.sourceTransactionBinding == headerfulReceipt.sourceTransactionBinding)
        #expect(contradictory.canonicalDatasetDigest == headerfulReceipt.canonicalDatasetDigest)
        try placeReceiptAtDerivedPath(contradictory, underStoreRoot: storeRoot)

        let store = ConfirmedScientificImportManifestStore(rootDirectory: storeRoot)
        await #expect(throws: ConfirmedScientificImportManifestStoreError.verification(.confirmationRecordMismatch)) {
            _ = try await store.restoreVerify(
                sourceSHA256: contradictory.sourceTransactionBinding.sourceBytesSHA256,
                recordDigest: contradictory.confirmationRecordDigest,
                baseConfirmation: headerfulBase
            )
        }
    }
}

@Test
func headerlessBaseCannotMintStandingFromHeaderfulOnDiskReceipt() async throws {
    // The mirror exploit: a headerless base verified against a headerful on-disk receipt. Rejected the
    // same way — the store-derived rule (headerless) makes the expected receipt differ from the disk
    // record, so no standing is minted.
    try await withCoreStoreRoot { storeRoot in
        let headerlessBase = try simpleConfirmedManifest(headerPresent: false)
        let headerlessReceipt = try ConfirmedScientificImportManifestPersistence.project(from: headerlessBase)
        #expect(headerlessReceipt.headerRule == .headerless)
        let contradictory = receiptWithHeaderRuleFlipped(headerlessReceipt)
        #expect(contradictory.headerRule == .firstRecordIsHeader)
        try placeReceiptAtDerivedPath(contradictory, underStoreRoot: storeRoot)

        let store = ConfirmedScientificImportManifestStore(rootDirectory: storeRoot)
        await #expect(throws: ConfirmedScientificImportManifestStoreError.verification(.confirmationRecordMismatch)) {
            _ = try await store.restoreVerify(
                sourceSHA256: contradictory.sourceTransactionBinding.sourceBytesSHA256,
                recordDigest: contradictory.confirmationRecordDigest,
                baseConfirmation: headerlessBase
            )
        }
    }
}

private enum PersistenceGoldens {
    static let schemaContractDigest = "f491e2c1319e00c1770a558d70851b8bc8fd6c1da87031f1c9dce9b149851655"
    static let recordDigest = "7130301c0c33f716852261aeddb28adc63111a824b0a644a8d45093f717961a1"
    static let fileHex = "0000000000000031737470642e636f6e6669726d65645f736369656e74696669635f696d706f72745f6d616e69666573745f72656365697074010000000000000042636f6e6669726d65645f736369656e74696669635f696d706f72745f6d616e69666573745f736f757263655f626f756e645f6465636973696f6e5f726563656970740200000000000000406634393165326331333139653030633137373061353538643730383531623862633866643663316461383730333166316339646365396231343938353136353503000000000000004263616e6f6e6963616c5f6d6963726f7365636f6e645f73696e676c655f7265636f7264696e675f7365676d656e745f6576656e745f73636f70655f64617461736574040000000000000040646464646464646464646464646464646464646464646464646464646464646464646464646464646464646464646464646464646464646464646464646464640500000000000000406565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656506000000000000004061616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161070000000000000027736f757263655f73656c656374696f6e2e636f6d6d615f7365706172617465645f76616c7565730c00000000000000226865616465725f72756c652e66697273745f7265636f72645f69735f6865616465720d0000000000000018736f757263655f74696d655f756e69742e7365636f6e64730e000000000000002261637469766974795f6d6f64652e70757461746976655f73696e676c655f756e69740f00000000000000097365676d656e745f611000000000000000257265636f7264696e675f726567696d652e636f6e74696e756f75735f756e747269616c6564110000000000000040696d706f727465645f657863657270745f636f7665726167652e616c6c5f7370696b655f747261696e735f66756c6c5f696d706f727465645f657863657270741200000000000000296f62736572766174696f6e5f626f756e64732e756e6b6e6f776e5f6f725f756e617661696c61626c6520000000000000000121000000000000000767726f75705f3122000000000000001c74696d655f62617369732e7265636f7264696e675f656c61707365642500000000000000012600000000000000012700000000000000097370696b655f6f6e6528000000000000001b6f726465722e70726573657276655f736f757263655f6f7264657229000000000000001f6475706c69636174652e70726573657276655f6d756c7469706c69636974792a000000000000000030000000000000000040000000000000004037313330333031633063333366373136383532323631616564646232386164633633313131613832346230613634346138643435303933663731373936316131"
}
