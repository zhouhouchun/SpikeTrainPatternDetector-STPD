@testable import STPDCore
import Foundation
import Testing

private let relativeHFSCycle = [0.026, 0.028, 0.030, 0.027, 0.029, 0.028, 0.027, 0.029]
private let relativeHFSTonic = [0.100, 0.108, 0.103, 0.110, 0.102, 0.107, 0.105, 0.105]

private let relativeHFSRegimeA =
    [0.500] +
    [0.006, 0.014, 0.008, 0.016, 0.007, 0.012, 0.009, 0.011] +
    [0.500] +
    Array(repeating: relativeHFSCycle, count: 4).flatMap { $0 } +
    [0.500] + relativeHFSTonic + [0.500]

private let relativeHFSRegimeB: [Double] = {
    var values = [1.200]
    values += [0.100, 0.200, 0.100, 0.200, 0.100, 0.200, 0.140, 0.150]
    values.append(1.200)
    values += Array(
        repeating: [0.092, 0.094, 0.096, 0.093, 0.095, 0.094, 0.093, 0.095],
        count: 4
    ).flatMap { $0 }
    values.append(1.200)
    values += [0.385, 0.415, 0.398, 0.425, 0.392, 0.418, 0.405, 0.405]
    values.append(1.200)
    return values
}()

private func relativeHFSTrain(_ name: String, _ isis: [Double]) -> SpikeTrain {
    var timestamps = [0.0]
    for isi in isis { timestamps.append((timestamps.last ?? 0) + isi) }
    return SpikeTrain(name: name, timestampsSec: timestamps)
}

private func relativeHFSDataset(_ name: String, _ isis: [Double]) -> SpikeDataset {
    SpikeDataset(
        name: name,
        sourceDescription: "blind-relative-hfs-fixture",
        trains: [0.98, 1.00, 1.02].enumerated().map { offset, scale in
            relativeHFSTrain("unit_0\(offset + 1)", isis.map { $0 * scale })
        }
    )
}

private func relativeHFSSettings(
    dataset: SpikeDataset
) -> StatePatternDetectorSettings {
    var settings = StatePatternDetectorSettings(
        minValidISISec: 0.001,
        highFrequencySpikingMinSpikes: 30,
        highFrequencySpikingMinDurationSec: 0,
        highFrequencySpikingInternalPacketizationPolicy: .multiTrackEventOverlay
    )
    settings.frozenDatasetStateBandProfile = FrozenDatasetStateBandProfile.compute(
        dataset: dataset,
        minimumValidISISec: 0.001
    )
    return settings
}

private func relativeHFSCandidates(
    train: SpikeTrain,
    settings: StatePatternDetectorSettings
) -> [ClassicAnchorCandidate] {
    StatePatternDetector.detect(train: train, settings: settings).candidates.filter {
        $0.finalLabel == .highFrequencySpiking &&
            $0.hfSpikingAcceptanceRoute?.contains("frozen_dataset_relative_state_band") == true
    }
}

private func relativeHFSPause(
    train: SpikeTrain,
    index: Int,
    role: PauseBoundaryRole
) -> ClassicAnchorCandidate {
    ClassicAnchorCandidate(
        id: "\(train.id)-test-pause-\(role.rawValue)",
        trainID: train.id,
        trainName: train.name,
        candidateLayer: "test_pause_role",
        candidateClass: "test_pause_role",
        finalLabel: .pause,
        gateStatus: "pass",
        decisionPath: "pause_boundary_role=\(role.rawValue)",
        action: "accept",
        score: 1,
        priority: 1_500,
        selectedForAuto: true,
        selectionStatus: "selected_for_test",
        startISIIndex: index,
        endISIIndex: index,
        startSpikeIndex: index,
        endSpikeIndex: index + 1,
        nISI: 1,
        nValidISI: 1,
        nSpikes: 2,
        durationSec: train.isiSec[index],
        intraQ10Sec: train.isiSec[index],
        intraQ40Sec: train.isiSec[index],
        intraQ50Sec: train.isiSec[index],
        intraQ90Sec: train.isiSec[index],
        intraQ95Sec: train.isiSec[index],
        maxIntraISISec: train.isiSec[index],
        meanIntraISISec: train.isiSec[index],
        cv: nil,
        lv: nil,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: "pause",
        anchorLockLevel: .strongCandidate,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 1,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil,
        pauseBoundaryRole: role
    )
}

@Test
func frozenRelativeHFSIsIndependentOfBurstHFSAbsoluteBandOrder() throws {
    for (name, prototype) in [
        ("regime-a", relativeHFSRegimeA),
        ("regime-b", relativeHFSRegimeB),
    ] {
        let dataset = relativeHFSDataset(name, prototype)
        let settings = relativeHFSSettings(dataset: dataset)
        #expect(settings.frozenDatasetStateBandProfile?.relativeHFSAdjudicationEligibility
            == .eligibleWithDatasetScaleSeparation)

        for train in dataset.trains {
            let candidates = relativeHFSCandidates(train: train, settings: settings)
            let hfs = try #require(candidates.first {
                $0.startISIIndex == 11 && $0.endISIIndex == 42
            })
            #expect(hfs.stateDirectSupportISICount == 32)
            #expect(hfs.stateDirectSupportAdjacentPairCount == 31)
            #expect(hfs.stateInterruptionSpans.isEmpty)
            #expect(hfs.stateDirectSupportSpans.map { $0.startISIIndex } == [11])
            #expect(hfs.stateDirectSupportSpans.map { $0.endISIIndex } == [42])
        }
    }
}

@Test
func frozenRelativeHFSGeometryIsScaleEquivariantAtFixedQC() {
    var referenceGeometry: [(String, Int, Int)]?
    for scale in [1.0, 10.0, 30.0] {
        let dataset = relativeHFSDataset(
            "scale-\(scale)",
            relativeHFSRegimeA.map { $0 * scale }
        )
        let settings = relativeHFSSettings(dataset: dataset)
        let geometry = dataset.trains.flatMap { train in
            relativeHFSCandidates(train: train, settings: settings).map {
                (train.name, $0.startISIIndex, $0.endISIIndex)
            }
        }
        if let referenceGeometry {
            #expect(geometry.map { "\($0.0):\($0.1)-\($0.2)" }
                == referenceGeometry.map { "\($0.0):\($0.1)-\($0.2)" })
        } else {
            referenceGeometry = geometry
        }
    }
}

@Test
func briefPauseFormsOneEnvelopeButNeverEntersDirectMetricsOrAdjacency() throws {
    let hfs16 = Array(repeating: relativeHFSCycle, count: 2).flatMap { $0 }
    let targetISI = [0.500] + hfs16 + [0.080] + hfs16 +
        [0.500] + relativeHFSTonic + [0.500]
    let reference = relativeHFSTrain("reference", relativeHFSRegimeA)
    let target = relativeHFSTrain("target", targetISI)
    let dataset = SpikeDataset(
        name: "brief-gap",
        sourceDescription: "blind-relative-hfs-gap-fixture",
        trains: [reference, target]
    )
    let settings = relativeHFSSettings(dataset: dataset)
    let hfs = try #require(relativeHFSCandidates(train: target, settings: settings).first {
        $0.startISIIndex == 2 && $0.endISIIndex == 34
    })

    #expect(hfs.nISI == 33)
    #expect(hfs.stateDirectSupportISICount == 32)
    #expect(hfs.stateDirectSupportAdjacentPairCount == 30)
    #expect(hfs.stateDirectSupportSpans.map { $0.startISIIndex } == [2, 19])
    #expect(hfs.stateDirectSupportSpans.map { $0.endISIIndex } == [17, 34])
    #expect(hfs.stateInterruptionSpans.map { $0.startISIIndex } == [18])
    #expect(hfs.stateInterruptionSpans.map { $0.endISIIndex } == [18])
    #expect((hfs.maxIntraISISec ?? .infinity) <= 0.030 + 1e-12)
    #expect(hfs.decisionPath.contains("state_cv2_lv_do_not_cross_interruptions=true"))

    let miniPause = relativeHFSPause(
        train: target,
        index: 18,
        role: .briefStateInterruption
    )
    let resolution = StateEventCompatibilityResolver.resolveStateCandidates(
        train: target,
        candidates: [hfs, miniPause],
        selectedEvents: [],
        selectedGaps: [miniPause],
        settings: settings
    )
    #expect(resolution.fragments.isEmpty)
    #expect(resolution.consumedStateCandidateIdentities.isEmpty)
}

@Test
func miniPauseMustBeStrictlyShorterThanConfiguredCanonicalPauseFloor() {
    let hfs16 = Array(repeating: relativeHFSCycle, count: 2).flatMap { $0 }
    for (gap, expectedEnvelope) in [(0.079, true), (0.080, false)] {
        let targetISI = [0.500] + hfs16 + [gap] + hfs16 +
            [0.500] + relativeHFSTonic + [0.500]
        let reference = relativeHFSTrain("reference-\(gap)", relativeHFSRegimeA)
        let target = relativeHFSTrain("target-\(gap)", targetISI)
        let dataset = SpikeDataset(
            name: "mini-vs-canonical-\(gap)",
            sourceDescription: "blind-relative-hfs-gap-boundary-fixture",
            trains: [reference, target]
        )
        var settings = relativeHFSSettings(dataset: dataset)
        settings.highFrequencySpikingPauseBreakSec = 0.080
        let containsEnvelope = relativeHFSCandidates(train: target, settings: settings).contains {
            $0.startISIIndex == 2 && $0.endISIIndex == 34
        }
        #expect(containsEnvelope == expectedEnvelope)
    }
}

@Test
func canonicalPauseCannotPoolSubthresholdHFSSupportAcrossItsBoundary() throws {
    let hfs16 = Array(repeating: relativeHFSCycle, count: 2).flatMap { $0 }
    let targetISI = [0.500] + hfs16 + [0.080] + hfs16 +
        [0.500] + relativeHFSTonic + [0.500]
    let reference = relativeHFSTrain("reference", relativeHFSRegimeA)
    let target = relativeHFSTrain("target", targetISI)
    let dataset = SpikeDataset(
        name: "canonical-gap",
        sourceDescription: "blind-relative-hfs-gap-fixture",
        trains: [reference, target]
    )
    let settings = relativeHFSSettings(dataset: dataset)
    let parent = try #require(relativeHFSCandidates(train: target, settings: settings).first {
        $0.startISIIndex == 2 && $0.endISIIndex == 34
    })
    let canonicalPause = relativeHFSPause(
        train: target,
        index: 18,
        role: .canonicalPauseAnchor
    )
    let resolution = StateEventCompatibilityResolver.resolveStateCandidates(
        train: target,
        candidates: [parent, canonicalPause],
        selectedEvents: [],
        selectedGaps: [canonicalPause],
        settings: settings
    )

    #expect(resolution.fragments.isEmpty)
    #expect(resolution.consumedStateCandidateIdentities.contains(
        StatePatternDetector.CandidateIdentity(parent)
    ))
    #expect(resolution.boundaryCandidateIDsByConsumedStateCandidateIdentity[
        StatePatternDetector.CandidateIdentity(parent)
    ] == [canonicalPause.id])
}

@Test
func relativeHFSDiscontiguousSupportIsIdentityBearingAndPackageValidated() throws {
    let hfs16 = Array(repeating: relativeHFSCycle, count: 2).flatMap { $0 }
    let targetISI = [0.500] + hfs16 + [0.080] + hfs16 +
        [0.500] + relativeHFSTonic + [0.500]
    let dataset = SpikeDataset(
        name: "relative-hfs-package",
        sourceDescription: "blind-relative-hfs-package-fixture",
        trains: [
            relativeHFSTrain("reference", relativeHFSRegimeA),
            relativeHFSTrain("target", targetISI),
        ]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        qualitySettings: SpikeQualitySettings(
            artifactThresholdSec: 0.001,
            refractorySuspectThresholdSec: 0.001
        ),
        buildCommit: "relative-hfs-package-test"
    )
    let candidate = try #require(run.candidates.first {
        $0.trainName == "target" &&
            $0.hfSpikingAcceptanceRoute?.contains("frozen_dataset_relative_state_band") == true &&
            $0.startISIIndex == 2 && $0.endISIIndex == 34
    })
    #expect(candidate.stateDirectSupportISICount == 32)
    #expect(candidate.stateDirectSupportAdjacentPairCount == 30)

    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: dataset, run: run)
    )
    let ledger = try #require(package.table(.candidateLedgerDiagnostic))
    let sourceIDColumn = try #require(ledger.headers.firstIndex(of: "source_candidate_id"))
    let uidColumn = try #require(ledger.headers.firstIndex(of: "candidate_uid"))
    let ledgerRow = try #require(ledger.rows.first { $0[sourceIDColumn] == candidate.id })
    let candidateUID = ledgerRow[uidColumn]

    let features = try #require(package.table(.candidateFeaturesDiagnostic))
    let featuresUIDColumn = try #require(features.headers.firstIndex(of: "candidate_uid"))
    let featureRow = try #require(features.rows.first { $0[featuresUIDColumn] == candidateUID })
    func value(_ column: String) throws -> String {
        let index = try #require(features.headers.firstIndex(of: column))
        return featureRow[index]
    }
    // The HFS authority resolver now constructs these spans explicitly. HFS is not one of the
    // generic `ISIPatternFamily` hints, so `unknown` is intentionally identity-bearing rather
    // than silently collapsing back to a nil family hint during package serialization.
    #expect(try value("state_direct_support_spans") == "[\"2:17:unknown\",\"19:34:unknown\"]")
    #expect(try value("state_interruption_spans") == "[\"18:18:unknown\"]")
    #expect(try value("state_direct_support_isi_count") == "32")
    #expect(try value("state_direct_support_adjacent_pair_count") == "30")
}
