import Foundation
@testable import STPDCore
import Testing

/// Phase 2.2A regression contract: the dataset structural-seed-profile candidate carries a stable,
/// dataset-scope singleton source ID (`__dataset__-structural-dataset-seed-profile`) instead of the
/// reconstruction-random `dataset.id.uuidString`. This makes the four diagnostic result-package tables
/// byte-reproducible across separately-constructed, byte-identical datasets without touching any
/// scientific output (`candidate_uid`, geometry, labels, features, scores, decisions, public tables).
///
/// The tests deliberately construct two datasets SEPARATELY (each gets its own random `SpikeDataset.id`),
/// never rebuilding from one shared instance.
private enum ResultPackageDiagnosticDeterminismFixture {
    static let stableProfileSourceID = "__dataset__-structural-dataset-seed-profile"

    static func timestamps(_ isis: [Double]) -> [Double] {
        isis.reduce(into: [0.0]) { values, isi in values.append((values.last ?? 0) + isi) }
    }

    /// Byte-identical spike content on every call; only `SpikeDataset.id` (a fresh UUID) differs.
    static func makeDataset() -> SpikeDataset {
        let burst =
            Array(repeating: 0.100, count: 3) +
            Array(repeating: 0.006, count: 7) +
            Array(repeating: 0.100, count: 3)
        let tonic = [0.300, 0.310, 0.295, 0.305, 0.300, 0.900, 0.305, 0.300, 0.295, 0.310, 0.300]
        return SpikeDataset(
            name: "result-package-fixture",
            sourceDescription: "synthetic, \"quoted\"\nsource",
            trains: [
                SpikeTrain(name: "burst_train", timestampsSec: timestamps(burst)),
                SpikeTrain(name: "tonic_pause_train", timestampsSec: timestamps(tonic)),
            ]
        )
    }

    static func run(_ dataset: SpikeDataset) -> ClassicAnchorDetectionRun {
        ClassicAnchorDetectionPipeline.run(
            dataset: dataset,
            bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005),
            buildCommit: "phase2_2a_test"
        )
    }

    static func package(_ dataset: SpikeDataset, _ run: ClassicAnchorDetectionRun) throws -> STPDResultPackage {
        try STPDResultPackageBuilder.build(STPDResultPackageInput.automatic(dataset: dataset, run: run))
    }

    /// Table bytes with the per-execution run_id token replaced, so only run_id is normalized away.
    static func normalizedCSV(_ table: STPDResultTableData, runID: String) -> String {
        String(decoding: table.csvData, as: UTF8.self)
            .replacingOccurrences(of: runID, with: "RUN_ID_NORMALIZED")
    }

    static let publicTables: [STPDResultTable] = [
        .runMetadata, .parametersReport, .resolvedParameters, .candidateLedger, .candidateFeatures,
        .finalDecisions, .eventsFinal, .isiLabelsFinal, .resultConsistencyCheck, .manualAnnotations,
        .reviewStatus, .hfsBurstArbitrationAudit, .taskEvents,
    ]
    static let diagnosticTables: [STPDResultTable] = [
        .candidateLedgerDiagnostic, .candidateFeaturesDiagnostic, .finalDecisionsDiagnostic,
        .candidateDiagnosticAudit,
    ]

    static func column(_ table: STPDResultTableData, _ name: String) -> [String]? {
        guard let idx = table.headers.firstIndex(of: name) else { return nil }
        return table.rows.map { $0[idx] }
    }
}

@Test func datasetProfileCandidateUsesStableSingletonSourceID() throws {
    typealias F = ResultPackageDiagnosticDeterminismFixture
    let dataset = F.makeDataset()
    let run = F.run(dataset)

    // Exactly one dataset structural-seed-profile candidate exists, and it carries the stable constant ID.
    let profileCandidates = run.candidates.filter { $0.candidateLayer == "structural_dataset_seed_profile" }
    #expect(profileCandidates.count == 1)
    #expect(profileCandidates.first?.id == F.stableProfileSourceID)

    // No two candidates share a source ID within a run (also enforced fail-closed by candidateRecords).
    let ids = run.candidates.map(\.id)
    #expect(Set(ids).count == ids.count)
}

@Test func separatelyConstructedDatasetsProduceIdenticalDiagnosticTables() throws {
    typealias F = ResultPackageDiagnosticDeterminismFixture

    // Two SEPARATE constructions => two different random SpikeDataset.id values.
    let datasetA = F.makeDataset()
    let datasetB = F.makeDataset()
    #expect(datasetA.id != datasetB.id)

    let runA = F.run(datasetA)
    let runB = F.run(datasetB)
    let packageA = try F.package(datasetA, runA)
    let packageB = try F.package(datasetB, runB)

    // Scientific identity is content-derived and therefore identical.
    #expect(packageA.identity.datasetDigest == packageB.identity.datasetDigest)
    #expect(packageA.identity.settingsDigest == packageB.identity.settingsDigest)

    let ridA = packageA.identity.runID
    let ridB = packageB.identity.runID

    // Profile candidate: stable source ID in both, identical candidate_uid, exactly one per run.
    for package in [packageA, packageB] {
        let ledger = try #require(package.tables[.candidateLedgerDiagnostic])
        let layers = try #require(F.column(ledger, "candidate_layer"))
        let sourceIDs = try #require(F.column(ledger, "source_candidate_id"))
        let uids = try #require(F.column(ledger, "candidate_uid"))
        let profileRows = layers.indices.filter { layers[$0] == "structural_dataset_seed_profile" }
        #expect(profileRows.count == 1)
        #expect(profileRows.allSatisfy { sourceIDs[$0] == F.stableProfileSourceID })
        // No duplicate source_candidate_id within the run.
        #expect(Set(sourceIDs).count == sourceIDs.count)
        _ = uids
    }
    // candidate_uid set identical across the two independent runs.
    func candidateUIDs(_ package: STPDResultPackage) throws -> [String] {
        let ledger = try #require(package.tables[.candidateLedgerDiagnostic])
        return try #require(F.column(ledger, "candidate_uid"))
    }
    #expect(Set(try candidateUIDs(packageA)) == Set(try candidateUIDs(packageB)))

    // profile candidate_uid identical across the two independent runs.
    func profileUID(_ package: STPDResultPackage) throws -> String {
        let ledger = try #require(package.tables[.candidateLedgerDiagnostic])
        let layers = try #require(F.column(ledger, "candidate_layer"))
        let uids = try #require(F.column(ledger, "candidate_uid"))
        let idx = try #require(layers.firstIndex(of: "structural_dataset_seed_profile"))
        return uids[idx]
    }
    #expect(try profileUID(packageA) == profileUID(packageB))

    // All four diagnostic tables are byte-identical after normalizing only run_id.
    for table in F.diagnosticTables {
        let a = F.normalizedCSV(try #require(packageA.tables[table]), runID: ridA)
        let b = F.normalizedCSV(try #require(packageB.tables[table]), runID: ridB)
        #expect(a == b, "diagnostic table \(table.rawValue) differs across separate reconstructions")
    }

    // All thirteen public scientific tables are byte-identical after normalizing only run_id, and
    // public row counts are unchanged (guards against any label/span/feature/score/decision drift).
    for table in F.publicTables {
        let tA = try #require(packageA.tables[table])
        let tB = try #require(packageB.tables[table])
        #expect(tA.rows.count == tB.rows.count)
        #expect(F.normalizedCSV(tA, runID: ridA) == F.normalizedCSV(tB, runID: ridB),
                "public table \(table.rawValue) differs across separate reconstructions")
    }
}
