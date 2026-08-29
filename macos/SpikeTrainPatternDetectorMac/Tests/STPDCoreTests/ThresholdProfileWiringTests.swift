import Foundation
import STPDCore
import Testing

// Phase-1 manual threshold profile detector wiring. These tests exercise the per-train
// resolution of a `ManualThresholdProfile` against the adaptive bands and its effect on the
// detection pipeline, provenance (decisionPath), and CSV export. The threshold model/resolver
// themselves are covered by ManualThresholdResolverTests; here we test the wiring.

private func cumulative(_ isiSec: [Double]) -> [Double] {
    var timestamps = [0.0]
    timestamps.reserveCapacity(isiSec.count + 1)
    for isi in isiSec {
        timestamps.append((timestamps.last ?? 0) + isi)
    }
    return timestamps
}

private func regularTrain(name: String, isiSec: Double, count: Int) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: cumulative(Array(repeating: isiSec, count: count)))
}

private let burstTrain = SpikeTrain(
    name: "wiring_burst",
    timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
)
private let pauseTrain = SpikeTrain(
    name: "wiring_pause",
    timestampsSec: [0, 0.010, 0.020, 0.030, 0.230, 0.240, 0.250, 0.260]
)
private let tonicTrain = regularTrain(name: "wiring_tonic", isiSec: 0.050, count: 16)
private let hfsTrain = regularTrain(name: "wiring_hfs", isiSec: 0.008, count: 60)

private func dataset(_ trains: [SpikeTrain], name: String = "threshold wiring") -> SpikeDataset {
    SpikeDataset(name: name, sourceDescription: "unit-test", trains: trains)
}

private let bandSettings = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)

private func run(_ data: SpikeDataset, profile: ManualThresholdProfile = .automatic) -> ClassicAnchorDetectionRun {
    ClassicAnchorDetectionPipeline.run(dataset: data, bandSettings: bandSettings, manualThresholdProfile: profile)
}

private func candidateFingerprint(_ run: ClassicAnchorDetectionRun) -> [String] {
    run.results.flatMap { result in
        result.candidates.map { "\($0.trainID)|\($0.id)|\($0.finalLabel.rawValue)|\($0.selectedForAuto)|\($0.decisionPath)" }
    }
}

// MARK: - 1. All-automatic profile preserves output / counts byte-for-byte.

@Test
func allAutomaticProfilePreservesDetectorOutput() throws {
    let data = dataset([burstTrain, pauseTrain, tonicTrain, hfsTrain])
    let baseline = run(data)

    // A profile that carries values but whose modes are all automatic must still resolve to a no-op.
    let inertProfile = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .automatic, valueSec: 0.004))
    )
    #expect(inertProfile.isAllAutomatic)
    let inert = run(data, profile: inertProfile)

    #expect(candidateFingerprint(baseline) == candidateFingerprint(run(data, profile: .automatic)))
    #expect(candidateFingerprint(baseline) == candidateFingerprint(inert))
    // No manual provenance tokens anywhere under the automatic path.
    #expect(!candidateFingerprint(baseline).contains { $0.contains("resolved_threshold[") })
}

// MARK: - 2. Burst hard threshold engages the existing hard route and carries provenance.

@Test
func burstHardThresholdEngagesExistingHardRouteWithManualSource() throws {
    // A burst ISI hard gate sets burstHardThresholdEnabled + the manual source, so the existing
    // direct seed+bridge hard route (not a parallel detector) emits the burst candidate.
    let settings = ClassicAnchorSettings(
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.020,
        burstBridgeUpperSec: 0.030,
        burstHardThresholdEnabled: true,
        burstHardThresholdSource: "manual_threshold_burst_hard_gate"
    )
    let detected = ClassicAnchorDetector.detect(train: burstTrain, settings: settings)
    let hard = detected.candidates.filter { $0.hardThreshold == true }
    #expect(!hard.isEmpty)
    #expect(hard.allSatisfy { $0.hardThresholdSource == "manual_threshold_burst_hard_gate" })
    #expect(hard.allSatisfy { $0.finalLabel.isBurstEventFamily })
}

@Test
func burstHardThresholdCarriesProvenanceThroughPipeline() throws {
    let data = dataset([burstTrain])
    let profile = ManualThresholdProfile(
        burst: BurstManualThresholds(
            seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.020),
            bridgeUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.030)
        )
    )
    let result = try #require(run(data, profile: profile).result(for: "wiring_burst"))
    let burstFamily = result.candidates.filter { $0.finalLabel.isBurstEventFamily }
    #expect(!burstFamily.isEmpty)
    #expect(burstFamily.contains { $0.decisionPath.contains("resolved_threshold[burst.") })
}

// MARK: - 3. Tonic hard min/max ISI constrains tonic candidates and carries provenance.

@Test
func tonicHardISIBandConstrainsTonicCandidatesAndCarriesProvenance() throws {
    let data = dataset([tonicTrain])

    // Pipeline: a permissive tonic hard band keeps the tonic candidate and records provenance.
    let permissive = ManualThresholdProfile(
        tonic: TonicManualThresholds(
            isiLower: ManualISIThreshold(mode: .hardGate, valueSec: 0.005),
            isiUpper: ManualISIThreshold(mode: .hardGate, valueSec: 0.400)
        )
    )
    let keptTonic = (run(data, profile: permissive).result(for: "wiring_tonic")?.candidates ?? [])
        .filter { $0.finalLabel == .tonic }
    #expect(!keptTonic.isEmpty)
    #expect(keptTonic.contains { $0.decisionPath.contains("resolved_threshold[tonic.") })

    // Detector level (deterministic): a tonic ISI hard band whose upper is below the run's 50 ms
    // median removes the tonic candidate — the mechanism a resolved tonic hard gate drives. (At the
    // pipeline level a single-rate train's adaptive tonic band is a point, so any narrowing inverts
    // and the resolver safely falls back to adaptive; this exercises the gate directly.)
    let baseSettings = StatePatternDetectorSettings()
    #expect(StatePatternDetector.detect(train: tonicTrain, settings: baseSettings)
        .candidates.contains { $0.finalLabel == .tonic })
    var gated = baseSettings
    gated.manualTonicHardLowerSec = 0.001
    gated.manualTonicHardUpperSec = 0.030
    #expect(!StatePatternDetector.detect(train: tonicTrain, settings: gated)
        .candidates.contains { $0.finalLabel == .tonic })
}

// MARK: - 4. HF-tonic hard min spikes constrains HF-tonic and carries provenance (detector + pipeline).

@Test
func hfTonicHardMinSpikesConstrainsCandidatesAtDetectorLevel() throws {
    // Regular fast-tonic run above a slow burst-core floor: deterministic at the detector level.
    let train = regularTrain(name: "hf_tonic_unit", isiSec: 0.020, count: 24)
    let baseSettings = StatePatternDetectorSettings(
        highFrequencyTonicFloorSec: 0.010,
        highFrequencyTonicUpperSec: 0.030,
        highFrequencyTonicMinSpikes: 6,
        highFrequencySpikingShortUpperSec: 0.012
    )
    let base = StatePatternDetector.detect(train: train, settings: baseSettings)
    #expect(base.candidates.contains { $0.finalLabel == .highFrequencyTonic })

    // A hard min-spikes gate above the run length removes the HF-tonic candidate.
    var gated = baseSettings
    gated.highFrequencyTonicMinSpikes = 100
    let gatedResult = StatePatternDetector.detect(train: train, settings: gated)
    #expect(!gatedResult.candidates.contains { $0.finalLabel == .highFrequencyTonic })
}

// MARK: - 5. HFS hard min-spikes / min-duration affect HFS candidates and carry provenance.

@Test
func hfsHardMinSpikesConstrainsHFSCandidatesAndCarriesProvenance() throws {
    let data = dataset([hfsTrain])
    let baselineHFS = (run(data).result(for: "wiring_hfs")?.candidates ?? [])
        .filter { $0.finalLabel == .highFrequencySpiking }
    #expect(!baselineHFS.isEmpty)

    // A hard min-spikes gate larger than the run removes the HFS candidate.
    let restrictive = ManualThresholdProfile(
        hfs: HFSManualThresholds(minSpikes: ManualSpikeCountThreshold(mode: .hardGate, value: 5_000))
    )
    let restricted = (run(data, profile: restrictive).result(for: "wiring_hfs")?.candidates ?? [])
        .filter { $0.finalLabel == .highFrequencySpiking }
    #expect(restricted.isEmpty)

    // A permissive hard gate keeps the HFS candidate and records provenance.
    let permissive = ManualThresholdProfile(
        hfs: HFSManualThresholds(minSpikes: ManualSpikeCountThreshold(mode: .hardGate, value: 10))
    )
    let kept = (run(data, profile: permissive).result(for: "wiring_hfs")?.candidates ?? [])
        .filter { $0.finalLabel == .highFrequencySpiking }
    #expect(!kept.isEmpty)
    #expect(kept.contains { $0.decisionPath.contains("resolved_threshold[hfs.") })
}

@Test
func learnedHFSMinimumCannotBypassLongBurstSeparationInvariant() throws {
    let data = dataset([hfsTrain])
    let tuning = StatePatternDetectorTuning(highFrequencySpikingMinSpikes: 5)
    var learned = ManualThresholdProfile(
        hfs: HFSManualThresholds(
            minSpikes: ManualSpikeCountThreshold(mode: .hardGate, value: 5)
        )
    )
    learned.learnedProvenanceByKey = [
        "hfs.min_spikes": "learned_from_manual_annotations(test)"
    ]
    let learnedRun = ClassicAnchorDetectionPipeline.run(
        dataset: data,
        bandSettings: bandSettings,
        stateTuning: tuning,
        manualThresholdProfile: learned
    )
    let learnedEvidence = try #require(learnedRun.resolvedThresholdEvidence.first)
    let learnedMin = try #require(learnedEvidence.effectiveProfile.hfs.minSpikes)
    let longBurstMax = try #require(learnedEvidence.effectiveProfile.burst.longMaxSpikes)

    #expect(learnedMin > longBurstMax)
    #expect(learnedEvidence.learnedProvenanceByKey["hfs.min_spikes"] != nil)

    // An explicitly typed user hard gate keeps the established replacement semantics. This control
    // proves that the extra separation clamp is specific to provenance-bound learned HFS minima.
    let explicit = ManualThresholdProfile(
        hfs: HFSManualThresholds(
            minSpikes: ManualSpikeCountThreshold(mode: .hardGate, value: 5)
        )
    )
    let explicitRun = ClassicAnchorDetectionPipeline.run(
        dataset: data,
        bandSettings: bandSettings,
        stateTuning: tuning,
        manualThresholdProfile: explicit
    )
    let explicitEvidence = try #require(explicitRun.resolvedThresholdEvidence.first)
    #expect(explicitEvidence.effectiveProfile.hfs.minSpikes == 5)
}

// MARK: - 6. Pause hard min ISI affects pause candidates and carries provenance.

@Test
func pauseHardMinISIConstrainsPauseCandidatesAndCarriesProvenance() throws {
    let data = dataset([pauseTrain])
    let baselinePause = run(data).result(for: "wiring_pause")?.candidates.filter { $0.finalLabel == .pause } ?? []
    #expect(!baselinePause.isEmpty)

    // A hard pause floor above the ~0.20 s gap removes the pause.
    let restrictive = ManualThresholdProfile(
        pause: PauseManualThresholds(isiLower: ManualISIThreshold(mode: .hardGate, valueSec: 0.500))
    )
    let restricted = run(data, profile: restrictive).result(for: "wiring_pause")?.candidates.filter { $0.finalLabel == .pause } ?? []
    #expect(restricted.count < baselinePause.count)

    // A permissive hard floor keeps the pause and records provenance.
    let permissive = ManualThresholdProfile(
        pause: PauseManualThresholds(isiLower: ManualISIThreshold(mode: .hardGate, valueSec: 0.050))
    )
    let kept = run(data, profile: permissive).result(for: "wiring_pause")?.candidates.filter { $0.finalLabel == .pause } ?? []
    #expect(!kept.isEmpty)
    #expect(kept.contains { $0.decisionPath.contains("resolved_threshold[pause.") })
}

// MARK: - 7. Soft anchors expand resolved bounds but do not force a candidate.

@Test
func softAnchorDoesNotForceBurstWhenAdaptiveEvidenceAbsent() throws {
    // A clean regular tonic train has no compressed burst structure. A soft burst anchor that
    // widens the burst seed band must NOT invent a burst candidate.
    let data = dataset([regularTrain(name: "soft_no_burst", isiSec: 0.050, count: 16)])
    let profile = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.060))
    )
    let result = try #require(run(data, profile: profile).result(for: "soft_no_burst"))
    #expect(!result.candidates.contains { $0.finalLabel.isBurstEventFamily && $0.selectedForAuto })
}

// MARK: - 8. CSV export includes the threshold provenance column and never includes mm.

@Test
func csvExportIncludesResolvedThresholdsColumnAndNoMM() throws {
    let data = dataset([burstTrain])
    let profile = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.020))
    )
    let detectionRun = run(data, profile: profile)
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-20T00:00:00Z"))
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: data,
        run: detectionRun,
        annotations: detectionRun.eventAnnotations(in: data),
        reviewStatuses: [:],
        exportedAt: exportedAt
    )
    let rows = csv.split(separator: "\n").map { $0.split(separator: ",", omittingEmptySubsequences: false).map(String.init) }
    let header = try #require(rows.first)
    #expect(header.contains("resolved_thresholds"))
    #expect(!header.contains("mm"))
    #expect(!header.contains("hfs_mm"))

    // At least one data row carries a resolved-threshold provenance token in the new column.
    let thresholdIndex = try #require(header.firstIndex(of: "resolved_thresholds"))
    let anyProvenance = rows.dropFirst().contains { row in
        thresholdIndex < row.count && row[thresholdIndex].contains("resolved_threshold[burst.")
    }
    #expect(anyProvenance)
    #expect(!csv.contains("mm="))
}

// MARK: - PARAM-1: burst hard-gate enforcement on selected burst-family candidates.

// A fast 6 ms burst core with one WIDE 0.084 s internal bridge ISI (6-84-6 ms), embedded in a slow (2.0 s)
// train. structure-first emits a selected burst covering the 0.084 s internal ISI — an over-bridge burst,
// the canonical target of a Burst BRIDGE hard gate. The internal gap makes the cluster strongly irregular
// (cv >> 0.15), so the Phase 11 automatic tonic-rate-regular-burst guard leaves it alone; only the user's
// hard bridge gate demotes it. (Earlier this fixture was a uniform 0.080 s packet — the pause_response_2_s
// tonic-rate false positive — but Phase 11 now correctly demotes that in AUTO mode, so it can no longer
// serve as the auto-mode baseline for the hard-gate test.)
private let fastPacketTrain = SpikeTrain(
    name: "pause_with_fast_packet",
    timestampsSec: [0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 12.006, 12.090, 12.096,
                    14.096, 16.096, 18.096, 20.096, 22.096, 24.096, 26.096]
)

private func selectedBurstFamily(_ detectionRun: ClassicAnchorDetectionRun, train: String) -> [ClassicAnchorCandidate] {
    (detectionRun.result(for: train)?.candidates ?? []).filter { $0.finalLabel.isBurstEventFamily && $0.selectedForAuto }
}

@Test
func burstSeedMaxAutoModeValueIsInert() throws {
    // Case 2: a seedUpperISI value with mode == automatic must be a no-op vs baseline (the value is inert
    // in Auto mode — the user-confusion trap), and the violating selected burst is still present.
    let data = dataset([fastPacketTrain])
    let baseline = candidateFingerprint(run(data))
    let autoValue = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .automatic, valueSec: 0.050))
    )
    #expect(autoValue.isAllAutomatic)
    #expect(candidateFingerprint(run(data, profile: autoValue)) == baseline)
    #expect(selectedBurstFamily(run(data, profile: autoValue), train: "pause_with_fast_packet")
        .contains { ($0.maxIntraISISec ?? 0) > 0.060 })
}

@Test
func burstHardGateDemotesSelectedBurstCoveringISIAboveBridge() throws {
    let data = dataset([fastPacketTrain])

    // Baseline (no gate): a selected structure-first burst covers the 0.084 s internal bridge ISI.
    #expect(selectedBurstFamily(run(data), train: "pause_with_fast_packet")
        .contains { ($0.maxIntraISISec ?? 0) > 0.060 })

    // Hard gate seed 0.050 / bridge 0.060: the 0.084 s internal ISI exceeds the hard bridge ceiling -> demoted.
    let hard = ManualThresholdProfile(
        burst: BurstManualThresholds(
            seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.050),
            bridgeUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.060)
        )
    )
    let gatedRun = run(data, profile: hard)

    // Invariant: no selected burst-family candidate covers an ISI above the hard bridge ceiling.
    #expect(selectedBurstFamily(gatedRun, train: "pause_with_fast_packet")
        .allSatisfy { ($0.maxIntraISISec ?? 0) <= 0.060 + 1e-9 })

    // The specific violator is demoted (not deleted) with an explicit status + decisionPath note,
    // and still carries the resolved-threshold provenance (case 3 invariant).
    let violators = (gatedRun.result(for: "pause_with_fast_packet")?.candidates ?? [])
        .filter { $0.finalLabel.isBurstEventFamily && ($0.maxIntraISISec ?? 0) > 0.060 }
    #expect(!violators.isEmpty)
    #expect(violators.allSatisfy { !$0.selectedForAuto })
    #expect(violators.allSatisfy { $0.selectionStatus == "not_selected__burst_hard_gate_band_exceeded" })
    #expect(violators.allSatisfy { $0.decisionPath.contains("hard_gate_burst_band_demotion=") })
    #expect(violators.contains { $0.decisionPath.contains("resolved_threshold[burst.") })
}

@Test
func burstHardGateKeepsCompliantBurstAndCarriesProvenance() throws {
    // A dense 0.006 s burst core survives a permissive hard gate; the survivor must respect the bound
    // (maxIntra <= effective bridge upper) and carry provenance — proving the gate is not over-demoting.
    let data = dataset([burstTrain])
    let permissive = ManualThresholdProfile(
        burst: BurstManualThresholds(
            seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.020),
            bridgeUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.030)
        )
    )
    let selected = selectedBurstFamily(run(data, profile: permissive), train: "wiring_burst")
    #expect(!selected.isEmpty)
    #expect(selected.allSatisfy { ($0.maxIntraISISec ?? 0) <= 0.030 + 1e-9 })
    #expect(selected.contains { $0.decisionPath.contains("resolved_threshold[burst.") })
}

// MARK: - PARAM-2: hard Burst seed max recovers moderate-ISI true clusters (not silently narrowed).

// One train with a fast 6 ms burst (which drives a small adaptive burst upper), a legitimate moderate
// 35 ms cluster, and a sparse ~447 ms false "burst". The fast burst makes the adaptive upper far below
// 35 ms, so under the old `min(adaptive, user)` narrowing a hard Seed max = 100 ms collapsed to the tiny
// adaptive band and excluded the 35 ms cluster.
private let mixedBurstClustersTrain = SpikeTrain(
    name: "mixed_burst_clusters",
    timestampsSec: [
        0, 2, 4, 6,
        6.006, 6.012, 6.018, 6.024, 6.030,                 // fast 6 ms burst
        8.030, 10.030,
        10.065, 10.100, 10.135, 10.170, 10.205, 10.240,    // moderate 35 ms cluster (true burst)
        12.240, 14.240,
        14.687, 15.134, 15.581, 16.028,                    // sparse ~447 ms cluster (false burst)
        18.028, 20.028
    ]
)

@Test
func burstHardSeedMaxRecoversModerateClusterAndStillDemotesSparseFalseBurst() throws {
    let data = dataset([mixedBurstClustersTrain])
    let hard = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.100))
    )
    let selected = selectedBurstFamily(run(data, profile: hard), train: "mixed_burst_clusters")
    let covered = selected.compactMap(\.maxIntraISISec)

    // PARAM-2: the moderate ~35 ms cluster is RECOVERED (eligible because Seed max 100 ms is not narrowed
    // to the smaller adaptive upper).
    #expect(covered.contains { abs($0 - 0.035) < 1e-6 })
    // PARAM-1 still holds under the SAME hard setting: the ~447 ms sparse cluster is demoted, so no
    // selected burst-family candidate covers an ISI above the 100 ms hard ceiling.
    #expect(selected.allSatisfy { ($0.maxIntraISISec ?? 0) <= 0.100 + 1e-9 })
    #expect(!covered.contains { $0 > 0.100 })
}

@Test
func burstHardSeedMaxAboveAdaptiveCarriesUserHardGateProvenance() throws {
    // The widened (user-value) burst seed upper is carried as user_hard_gate provenance on surviving
    // burst-family candidates (case-3 invariant), not silently dropped.
    let data = dataset([mixedBurstClustersTrain])
    let hard = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.100))
    )
    let selected = selectedBurstFamily(run(data, profile: hard), train: "mixed_burst_clusters")
    #expect(selected.contains { $0.decisionPath.contains("resolved_threshold[burst.seed_upper_sec]=user_hard_gate") })
}

@Test
func burstHardGateRearbitratesToCompliantSubclusterAfterDemotingWideWinner() throws {
    // Regression from tonic_baseline_repeated_stimulus_burst_pause_5x5.csv / burst_response_4_s:
    // a structure-first winner included a 128.9 ms leading ISI and was correctly demoted by
    // Seed max = 100 ms, but the already-existing 27.3 ms compliant subcluster was not reselected.
    // Hard-gate enforcement must therefore make violators ineligible before a final re-arbitration.
    let train = SpikeTrain(
        name: "param2_rearbitrate_burst_response_4",
        timestampsSec: [
            0.413086, 0.886904, 1.330805, 1.775030, 2.214751, 2.701175,
            3.121998, 3.574148, 4.029013, 4.486102, 4.956260,
            5.085202, 5.097094, 5.124391, 5.136800, 5.145232, 5.158551,
            5.575505, 6.091255, 6.543579, 7.043380, 7.467439, 7.901280,
            8.098157, 8.106440
        ]
    )
    let hard = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.100))
    )

    let result = try #require(run(dataset([train]), profile: hard).result(for: train.id))
    let selected = result.candidates.filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
    let demotedWide = result.candidates.filter {
        $0.finalLabel.isBurstEventFamily &&
            $0.selectionStatus == "not_selected__burst_hard_gate_band_exceeded"
    }

    #expect(selected.contains {
        $0.startSpikeIndex == 12 &&
            $0.endSpikeIndex == 17 &&
            abs(($0.maxIntraISISec ?? .infinity) - 0.027297) < 1e-6
    })
    #expect(selected.allSatisfy { ($0.maxIntraISISec ?? 0) <= 0.100 + 1e-9 })
    #expect(demotedWide.contains {
        $0.startSpikeIndex == 11 &&
            $0.endSpikeIndex == 17 &&
            ($0.maxIntraISISec ?? 0) > 0.100
    })
}

// MARK: - PARAM-3: a moderate compact cluster must be a burst (not "others") under a hard Burst seed max.

// 5-spike cluster at 40 ms in a 120 ms tonic background (contrast 3.0x). Under hard Burst seed max = 100 ms
// the 40 ms ISIs are eligible seeds, so the cluster should be a selected burst. The bug (PARAM-3): the
// tight cluster burst is never generated — only an oversized whole-train candidate (bridging the 120 ms
// baseline) that exceeds the band and is rejected — so the cluster was labeled "others".
private func moderateClusterTrain() -> SpikeTrain {
    var ts: [Double] = [0]
    for _ in 0..<8 { ts.append(ts.last! + 0.120) }   // tonic baseline (ISI 1..8)
    for _ in 0..<4 { ts.append(ts.last! + 0.040) }   // 5-spike 40 ms cluster (ISI 9..12)
    for _ in 0..<8 { ts.append(ts.last! + 0.120) }   // tonic baseline (ISI 13..20)
    return SpikeTrain(name: "moderate_cluster", timestampsSec: ts)
}

private func twoModerateClustersInOneBridgeRunTrain() -> SpikeTrain {
    var ts: [Double] = [0]
    for _ in 0..<8 { ts.append(ts.last! + 0.120) }   // tonic baseline (ISI 1..8)
    for _ in 0..<4 { ts.append(ts.last! + 0.040) }   // first cluster (ISI 9..12)
    for _ in 0..<8 { ts.append(ts.last! + 0.120) }   // tonic baseline bridge (ISI 13..20)
    for _ in 0..<4 { ts.append(ts.last! + 0.040) }   // second cluster (ISI 21..24)
    for _ in 0..<8 { ts.append(ts.last! + 0.120) }   // tonic baseline (ISI 25..32)
    return SpikeTrain(name: "two_moderate_clusters", timestampsSec: ts)
}

@Test
func moderateCompactClusterIsBurstUnderHardSeedMaxNotOthers() throws {
    let data = dataset([moderateClusterTrain()])
    let hard = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.100))
    )
    let selected = selectedBurstFamily(run(data, profile: hard), train: "moderate_cluster")
    // The cluster (ISI indices 9...12, all 40 ms) is covered by a selected burst whose covered ISIs are
    // all within the 100 ms hard seed max (i.e. a tight cluster burst, not an oversized whole-train span).
    let coversCluster = selected.contains {
        $0.startISIIndex <= 9 && $0.endISIIndex >= 12 && ($0.maxIntraISISec ?? .infinity) <= 0.100 + 1e-9
    }
    #expect(coversCluster)
}

@Test
func hardSeedMaxSplitsMultipleModerateClustersInOneOverbridgedRun() throws {
    let data = dataset([twoModerateClustersInOneBridgeRunTrain()])
    let hard = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.100))
    )
    let selected = selectedBurstFamily(run(data, profile: hard), train: "two_moderate_clusters")
        .filter { ($0.maxIntraISISec ?? .infinity) <= 0.100 + 1e-9 }

    #expect(selected.contains { $0.startISIIndex <= 9 && $0.endISIIndex >= 12 })
    #expect(selected.contains { $0.startISIIndex <= 21 && $0.endISIIndex >= 24 })
}
