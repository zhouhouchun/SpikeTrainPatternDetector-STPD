import Foundation
@testable import STPDCore
import Testing

private func cv2ReportingTimestamps(_ isis: [Double]) -> [Double] {
    isis.reduce(into: [0.0]) { timestamps, isi in
        timestamps.append((timestamps.last ?? 0) + isi)
    }
}

private func independentlyComputedCV2(_ values: [Double]) -> Double? {
    guard values.count >= 2 else { return nil }
    let local = zip(values, values.dropFirst()).compactMap { previous, next -> Double? in
        let denominator = previous + next
        guard previous.isFinite, next.isFinite, denominator > 0 else { return nil }
        return 2 * abs(next - previous) / denominator
    }
    guard local.count == values.count - 1 else { return nil }
    return local.reduce(0, +) / Double(local.count)
}

private func candidateSpanValues(
    _ candidate: ClassicAnchorCandidate,
    train: SpikeTrain
) -> [Double] {
    (candidate.startISIIndex...candidate.endISIIndex).compactMap { train.isiSec[$0] }
}

private func cv2ReportingIsClose(
    _ lhs: Double?,
    _ rhs: Double?,
    tolerance: Double = 1e-12
) -> Bool {
    guard let lhs, let rhs else { return lhs == nil && rhs == nil }
    return abs(lhs - rhs) <= tolerance
}

private func cv2ReportingCandidate(
    _ candidate: ClassicAnchorCandidate,
    startISIIndex: Int,
    endISIIndex: Int,
    nISI: Int,
    startSpikeIndex: Int? = nil,
    endSpikeIndex: Int? = nil
) -> ClassicAnchorCandidate {
    var adjusted = candidate.withGeometry(
        startISIIndex: startISIIndex,
        endISIIndex: endISIIndex,
        startSpikeIndex: startSpikeIndex ?? startISIIndex,
        endSpikeIndex: endSpikeIndex ?? endISIIndex + 1,
        nISI: nISI,
        nValidISI: nISI,
        nSpikes: nISI + 1,
        durationSec: nil,
        intraQ10Sec: candidate.intraQ10Sec,
        intraQ40Sec: candidate.intraQ40Sec,
        intraQ50Sec: candidate.intraQ50Sec,
        intraQ90Sec: candidate.intraQ90Sec,
        intraQ95Sec: candidate.intraQ95Sec,
        maxIntraISISec: candidate.maxIntraISISec,
        meanIntraISISec: candidate.meanIntraISISec,
        cv: candidate.cv,
        lv: candidate.lv,
        preGapSec: candidate.preGapSec,
        postGapSec: candidate.postGapSec,
        preRatioQ90: candidate.preRatioQ90,
        postRatioQ90: candidate.postRatioQ90,
        edgeContrastMinQ90: candidate.edgeContrastMinQ90,
        edgeContrastGeomQ90: candidate.edgeContrastGeomQ90,
        decisionPath: candidate.decisionPath
    )
    adjusted.burstSeedRunStartISI = nil
    adjusted.burstSeedRunEndISI = nil
    return adjusted
}

@Test
func candidateCV2ReportingReachesAuthoritativeCandidateFeaturesTable() throws {
    let burstISIs =
        Array(repeating: 0.100, count: 3)
        + [0.006, 0.008, 0.010, 0.008, 0.006, 0.009, 0.007]
        + Array(repeating: 0.100, count: 3)
    let tonicISIs = [0.300, 0.310, 0.295, 0.305, 0.300, 0.900, 0.305, 0.300]
    let dataset = SpikeDataset(
        name: "cv2-package",
        sourceDescription: "synthetic CV2 result-package fixture",
        trains: [
            SpikeTrain(
                name: "cv2_burst_train",
                timestampsSec: cv2ReportingTimestamps(burstISIs)
            ),
            SpikeTrain(
                name: "cv2_tonic_train",
                timestampsSec: cv2ReportingTimestamps(tonicISIs)
            ),
        ]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        buildCommit: "candidate_cv2_reporting_test"
    )
    let train = try #require(dataset.trains.first { $0.name == "cv2_burst_train" })
    let candidate = try #require(
        run.result(for: train.id)?.candidates.first {
            $0.selectedForAuto && $0.finalLabel.isBurstEventFamily && $0.nISI >= 2
        }
    )
    let expected = independentlyComputedCV2(candidateSpanValues(candidate, train: train))
    #expect(candidate.cv2 == nil)
    #expect((expected ?? 0) > 0)
    let detectorCandidatesBeforeBuild = run.candidates

    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: dataset, run: run)
    )
    #expect(run.candidates == detectorCandidatesBeforeBuild)
    let table = try #require(package.table(STPDResultTable.candidateFeatures))
    let sourceIDColumn = try #require(table.headers.firstIndex(of: "source_candidate_id"))
    let candidateUIDColumn = try #require(table.headers.firstIndex(of: "candidate_uid"))
    let cv2Column = try #require(table.headers.firstIndex(of: "cv2"))
    let row = try #require(table.rows.first { $0[sourceIDColumn] == candidate.id })
    let tableCV2 = try #require(Double(row[cv2Column]))
    let diagnosticTable = try #require(
        package.table(STPDResultTable.candidateFeaturesDiagnostic)
    )
    let diagnosticSourceIDColumn = try #require(
        diagnosticTable.headers.firstIndex(of: "source_candidate_id")
    )
    let diagnosticCandidateUIDColumn = try #require(
        diagnosticTable.headers.firstIndex(of: "candidate_uid")
    )
    let diagnosticCV2Column = try #require(
        diagnosticTable.headers.firstIndex(of: "cv2")
    )
    let diagnosticRow = try #require(
        diagnosticTable.rows.first { $0[diagnosticSourceIDColumn] == candidate.id }
    )
    let diagnosticCV2 = try #require(Double(diagnosticRow[diagnosticCV2Column]))
    let ledger = try #require(package.table(STPDResultTable.candidateLedger))
    let ledgerSourceIDColumn = try #require(
        ledger.headers.firstIndex(of: "source_candidate_id")
    )
    let ledgerCandidateUIDColumn = try #require(
        ledger.headers.firstIndex(of: "candidate_uid")
    )
    let ledgerRow = try #require(
        ledger.rows.first { $0[ledgerSourceIDColumn] == candidate.id }
    )
    let candidateUID = row[candidateUIDColumn]
    let events = try #require(package.table(STPDResultTable.eventsFinal))
    let eventUIDColumn = try #require(events.headers.firstIndex(of: "event_uid"))
    let eventSourceUIDsColumn = try #require(
        events.headers.firstIndex(of: "source_candidate_uids")
    )
    let eventRow = try #require(
        events.rows.first {
            STPDCanonicalValue.parseStringList($0[eventSourceUIDsColumn])
                == [candidateUID]
        }
    )
    let eventUID = eventRow[eventUIDColumn]

    let semanticPresentation = try STPDResultSemanticPresentation.make(
        sourceTables: package.tables.map { table, data in
            STPDResultSemanticSourceTable(
                fileName: table.rawValue,
                columns: data.columnDefinitions,
                rows: data.rows
            )
        }
    )
    let burstEvents = try #require(
        semanticPresentation.table(
            id: STPDResultSemanticPresentation.burstFamilyTableID
        )
    )
    let semanticCV2Column = try #require(
        burstEvents.columns.firstIndex { $0.id == "cv2" }
    )
    let semanticRow = try #require(
        burstEvents.rows.first { $0.id == eventUID }
    )
    let semanticCV2 = try #require(Double(semanticRow.values[semanticCV2Column]))

    #expect(!row[cv2Column].isEmpty)
    #expect(cv2ReportingIsClose(tableCV2, expected))
    #expect(cv2ReportingIsClose(diagnosticCV2, expected))
    #expect(cv2ReportingIsClose(semanticCV2, expected))
    #expect(row[candidateUIDColumn] == ledgerRow[ledgerCandidateUIDColumn])
    #expect(diagnosticRow[diagnosticCandidateUIDColumn] == ledgerRow[ledgerCandidateUIDColumn])
}

@Test
func candidateCV2NormalizationHonorsQCUndefinedGeometryAndExistingValues() throws {
    let burstISIs =
        Array(repeating: 0.100, count: 3)
        + [0.006, 0.008, 0.010, 0.008, 0.006, 0.009, 0.007]
        + Array(repeating: 0.100, count: 3)
    let dataset = SpikeDataset(
        name: "cv2-normalization-contract",
        sourceDescription: "synthetic CV2 normalization contract fixture",
        trains: [
            SpikeTrain(
                name: "cv2_contract_train",
                timestampsSec: cv2ReportingTimestamps(burstISIs)
            ),
        ]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        buildCommit: "candidate_cv2_normalization_contract_test"
    )
    let train = try #require(dataset.trains.first)
    let candidate = try #require(
        run.candidates.first {
            $0.selectedForAuto && $0.finalLabel.isBurstEventFamily && $0.nISI >= 2
        }
    )
    let span = candidateSpanValues(candidate, train: train)
    let expected = try #require(independentlyComputedCV2(span))
    let sortedValue = try #require(independentlyComputedCV2(span.sorted()))
    #expect(!cv2ReportingIsClose(expected, sortedValue))

    let normalized = try #require(
        STPDResultPackageBuilder.candidatesWithFinalSpanCV2(
            [candidate],
            dataset: dataset,
            bandMinimumValidISISec: 0.001,
            artifactThresholdSec: 0.001
        ).first
    )
    #expect(cv2ReportingIsClose(normalized.cv2, expected))

    let minimumSpanISI = try #require(span.min())
    let invalidatingArtifactFloor = minimumSpanISI + 0.0005
    let qcInvalid = try #require(
        STPDResultPackageBuilder.candidatesWithFinalSpanCV2(
            [candidate],
            dataset: dataset,
            bandMinimumValidISISec: 0.001,
            artifactThresholdSec: invalidatingArtifactFloor
        ).first
    )
    #expect(qcInvalid.cv2 == nil)

    let invalidatingBandFloor = minimumSpanISI + 0.0005
    let bandInvalid = try #require(
        STPDResultPackageBuilder.candidatesWithFinalSpanCV2(
            [candidate],
            dataset: dataset,
            bandMinimumValidISISec: invalidatingBandFloor,
            artifactThresholdSec: 0.001
        ).first
    )
    #expect(bandInvalid.cv2 == nil)

    var existing = candidate
    existing.cv2 = 0.3141592653589793
    let preserved = try #require(
        STPDResultPackageBuilder.candidatesWithFinalSpanCV2(
            [existing],
            dataset: dataset,
            bandMinimumValidISISec: 0.001,
            artifactThresholdSec: 1.0
        ).first
    )
    #expect(preserved.cv2 == existing.cv2)

    let oneISI = cv2ReportingCandidate(
        candidate,
        startISIIndex: candidate.startISIIndex,
        endISIIndex: candidate.startISIIndex,
        nISI: 1
    )
    let oneISINormalized = try #require(
        STPDResultPackageBuilder.candidatesWithFinalSpanCV2(
            [oneISI],
            dataset: dataset,
            bandMinimumValidISISec: 0.001,
            artifactThresholdSec: 0.001
        ).first
    )
    #expect(oneISINormalized.cv2 == nil)

    let inconsistentGeometry = cv2ReportingCandidate(
        candidate,
        startISIIndex: candidate.startISIIndex,
        endISIIndex: candidate.startISIIndex,
        nISI: 2
    )
    let inconsistentNormalized = try #require(
        STPDResultPackageBuilder.candidatesWithFinalSpanCV2(
            [inconsistentGeometry],
            dataset: dataset,
            bandMinimumValidISISec: 0.001,
            artifactThresholdSec: 0.001
        ).first
    )
    #expect(inconsistentNormalized.cv2 == nil)

    let invalidSpikeGeometry = cv2ReportingCandidate(
        candidate,
        startISIIndex: candidate.startISIIndex,
        endISIIndex: candidate.endISIIndex,
        nISI: candidate.nISI,
        startSpikeIndex: candidate.startISIIndex - 1,
        endSpikeIndex: candidate.endISIIndex
    )
    let invalidSpikeGeometryNormalized = try #require(
        STPDResultPackageBuilder.candidatesWithFinalSpanCV2(
            [invalidSpikeGeometry],
            dataset: dataset,
            bandMinimumValidISISec: 0.001,
            artifactThresholdSec: 0.001
        ).first
    )
    #expect(invalidSpikeGeometryNormalized.cv2 == nil)
}
