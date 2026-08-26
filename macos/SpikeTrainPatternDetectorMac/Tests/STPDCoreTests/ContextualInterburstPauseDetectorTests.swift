import Testing
@testable import STPDCore

@Suite("Contextual inter-burst Pause")
struct ContextualInterburstPauseDetectorTests {
    @Test("Bilateral frozen Burst topology produces contextual Pause below ordinary Pause threshold")
    func producesContextualPause() throws {
        let train = makeTrain(intervals: [0.010, 0.010, 0.010, 0.080, 0.010, 0.010, 0.010])
        let left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.020)
        let right = makeBurst(id: "right", train: train, start: 5, end: 7, bridgeUpper: 0.020)

        let candidates = ContextualInterburstPauseDetector.detect(
            train: train,
            frozenBurstCandidates: [right, left],
            settings: settings()
        )
        let pause = try #require(candidates.first)

        #expect(pause.startISIIndex == 4)
        #expect(pause.endISIIndex == 4)
        #expect(pause.pauseBoundaryRole == .contextualPause)
        #expect(pause.action == "accept")
        #expect(pause.gateStatus == "contextual_interburst_pause_pass")
        #expect(pause.decisionPath.contains("burst_topology_frozen=true"))
        #expect(pause.decisionPath.contains("left_burst_id=left"))
        #expect(pause.decisionPath.contains("right_burst_id=right"))
        #expect(pause.intraQ50Sec == 0.080)

        let resolved = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([left, right, pause])
        let selectedPause = try #require(resolved.first { $0.id == pause.id })
        #expect(selectedPause.selectedForAuto)
        #expect(selectedPause.selectionStatus == "selected_by_gap_track_contextual_interburst_pause")
    }

    @Test("Established canonical Pause is never duplicated or downgraded")
    func canonicalPauseWins() {
        let train = makeTrain(intervals: [0.010, 0.010, 0.010, 0.180, 0.010, 0.010, 0.010])
        let left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.020)
        let right = makeBurst(id: "right", train: train, start: 5, end: 7, bridgeUpper: 0.020)
        let canonical = makePause(
            id: "canonical",
            train: train,
            index: 4,
            role: .canonicalPauseAnchor,
            selected: true
        )

        let candidates = ContextualInterburstPauseDetector.detect(
            train: train,
            frozenBurstCandidates: [left, right],
            settings: settings(),
            existingCandidates: [canonical]
        )

        #expect(candidates.isEmpty)
    }

    @Test("Clear Burst bridge evidence does not manufacture Pause")
    func bridgeWins() {
        let train = makeTrain(intervals: [0.010, 0.010, 0.010, 0.030, 0.010, 0.010, 0.010])
        let left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.040)
        let right = makeBurst(id: "right", train: train, start: 5, end: 7, bridgeUpper: 0.040)

        let candidates = ContextualInterburstPauseDetector.detect(
            train: train,
            frozenBurstCandidates: [left, right],
            settings: settings()
        )

        #expect(candidates.isEmpty)
    }

    @Test("User-confirmed absolute Pause floor also constrains contextual evidence")
    func manualHardPauseFloorCannotBeBypassed() {
        let train = makeTrain(intervals: [0.010, 0.010, 0.010, 0.080, 0.010, 0.010, 0.010])
        let left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.020)
        let right = makeBurst(id: "right", train: train, start: 5, end: 7, bridgeUpper: 0.020)
        var constrained = settings()
        constrained.manualHardLowerSec = 0.090

        let candidates = ContextualInterburstPauseDetector.detect(
            train: train,
            frozenBurstCandidates: [left, right],
            settings: constrained
        )

        #expect(candidates.isEmpty)
    }

    @Test("Conflicting bridge and Pause evidence remains audit-only ambiguous gap")
    func conflictRemainsAmbiguous() throws {
        let train = makeTrain(intervals: [0.010, 0.010, 0.010, 0.060, 0.010, 0.010, 0.010])
        let left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.060)
        let right = makeBurst(id: "right", train: train, start: 5, end: 7, bridgeUpper: 0.060)

        let candidates = ContextualInterburstPauseDetector.detect(
            train: train,
            frozenBurstCandidates: [left, right],
            settings: settings()
        )
        let ambiguous = try #require(candidates.first)

        #expect(ambiguous.candidateClass == "ambiguous_gap")
        #expect(ambiguous.pauseBoundaryRole == nil)
        #expect(ambiguous.action == "audit_only")
        #expect(!ambiguous.isEligibleForAutoSelection)
    }

    @Test("Contextual evidence represents duplicate brief flank evidence on the gap track")
    func contextualEvidenceWinsDuplicateBriefFlank() throws {
        let train = makeTrain(intervals: [0.010, 0.010, 0.010, 0.080, 0.010, 0.010, 0.010])
        let left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.020)
        let right = makeBurst(id: "right", train: train, start: 5, end: 7, bridgeUpper: 0.020)
        let briefFlank = makeCandidate(
            id: "brief-flank",
            train: train,
            label: .pause,
            start: 4,
            end: 4,
            value: 0.080,
            lockLevel: .strongCandidate,
            selected: true,
            pauseBoundaryRole: .briefStateInterruption,
            layer: "classic_burst_flank_pause"
        )
        let contextual = try #require(
            ContextualInterburstPauseDetector.detect(
                train: train,
                frozenBurstCandidates: [left, right],
                settings: settings(),
                existingCandidates: [briefFlank]
            ).first
        )

        let resolved = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            left, right, briefFlank, contextual
        ])
        let byID = Dictionary(uniqueKeysWithValues: resolved.map { ($0.id, $0) })

        #expect(byID[contextual.id]?.selectedForAuto == true)
        #expect(byID[contextual.id]?.selectionStatus == "selected_by_gap_track_contextual_interburst_pause")
        #expect(byID[briefFlank.id]?.selectedForAuto == false)
        #expect(byID[briefFlank.id]?.selectionStatus.contains("duplicate_gap_evidence") == true)
    }

    @Test("Intervening activity wider than one ISI is not collapsed into contextual Pause")
    func widerActivityIsNotCollapsed() {
        let train = makeTrain(intervals: [0.010, 0.010, 0.010, 0.080, 0.070, 0.010, 0.010, 0.010])
        let left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.020)
        let right = makeBurst(id: "right", train: train, start: 6, end: 8, bridgeUpper: 0.020)

        let candidates = ContextualInterburstPauseDetector.detect(
            train: train,
            frozenBurstCandidates: [left, right],
            settings: settings()
        )

        #expect(candidates.isEmpty)
    }

    @Test("Contextual geometry and contrast use frozen cores rather than wider Burst envelopes")
    func frozenCoreOwnsGeometryAndReference() throws {
        let train = makeTrain(intervals: [0.040, 0.010, 0.010, 0.080, 0.010, 0.010, 0.040])
        var left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.020)
        var right = makeBurst(id: "right", train: train, start: 5, end: 7, bridgeUpper: 0.020)
        left.burstSeedRunStartISI = 2
        left.burstSeedRunEndISI = 3
        right.burstSeedRunStartISI = 5
        right.burstSeedRunEndISI = 6

        let pause = try #require(ContextualInterburstPauseDetector.detect(
            train: train,
            frozenBurstCandidates: [left, right],
            settings: settings()
        ).first)

        #expect(pause.startISIIndex == 4)
        #expect(pause.pauseBoundaryRole == .contextualPause)
        #expect(pause.decisionPath.contains("left_burst_core_span=2-3"))
        #expect(pause.decisionPath.contains("right_burst_core_span=5-6"))
        #expect(pause.decisionPath.contains("burst_reference_scope=frozen_core_q90"))
        #expect(pause.preRatioQ90.map { abs($0 - 8) < 1e-12 } == true)
        #expect(pause.postRatioQ90.map { abs($0 - 8) < 1e-12 } == true)
    }

    @Test("Phase 1B wires contextual Pause after Burst topology is frozen")
    func phase1BWiring() throws {
        let train = makeTrain(intervals: [0.010, 0.010, 0.010, 0.080, 0.010, 0.010, 0.010])
        let left = makeBurst(id: "left", train: train, start: 1, end: 3, bridgeUpper: 0.020)
        let right = makeBurst(id: "right", train: train, start: 5, end: 7, bridgeUpper: 0.020)

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [left, right],
            pauseSettings: settings(),
            stateSettings: StatePatternDetectorSettings()
        )
        let contextual = try #require(resolved.first {
            $0.pauseBoundaryRole == .contextualPause && $0.startISIIndex == 4
        })

        #expect(contextual.selectedForAuto)
        #expect(contextual.decisionPath.contains("pipeline_stage=phase1b_contextual_interburst_pause"))
    }

    private func settings() -> PauseDetectorSettings {
        PauseDetectorSettings(
            seedThresholdSec: 0.100,
            strongThresholdSec: 0.150,
            useGlobalMedianGuard: false,
            antiTonicVeto: false,
            classicBurstFlankPauseContrastMin: 5.0
        )
    }

    private func makeTrain(intervals: [Double]) -> SpikeTrain {
        var timestamps = [0.0]
        for interval in intervals {
            timestamps.append((timestamps.last ?? 0) + interval)
        }
        return SpikeTrain(name: "contextual-interburst", timestampsSec: timestamps)
    }

    private func makeBurst(
        id: String,
        train: SpikeTrain,
        start: Int,
        end: Int,
        bridgeUpper: Double
    ) -> ClassicAnchorCandidate {
        var candidate = makeCandidate(
            id: id,
            train: train,
            label: .burst,
            start: start,
            end: end,
            value: 0.010,
            lockLevel: .lockedClassic,
            selected: true,
            pauseBoundaryRole: nil
        )
        candidate.burstSeedBandUpperSec = 0.010
        candidate.burstBridgeBandUpperSec = bridgeUpper
        candidate.burstSeedRunStartISI = start
        candidate.burstSeedRunEndISI = end
        return candidate
    }

    private func makePause(
        id: String,
        train: SpikeTrain,
        index: Int,
        role: PauseBoundaryRole,
        selected: Bool
    ) -> ClassicAnchorCandidate {
        makeCandidate(
            id: id,
            train: train,
            label: .pause,
            start: index,
            end: index,
            value: train.isiSec[index] ?? 0,
            lockLevel: .lockedClassic,
            selected: selected,
            pauseBoundaryRole: role
        )
    }

    private func makeCandidate(
        id: String,
        train: SpikeTrain,
        label: ClassicAnchorLabel,
        start: Int,
        end: Int,
        value: Double,
        lockLevel: ClassicAnchorLockLevel,
        selected: Bool,
        pauseBoundaryRole: PauseBoundaryRole?,
        layer: String = "contextual_interburst_test"
    ) -> ClassicAnchorCandidate {
        ClassicAnchorCandidate(
            id: id,
            trainID: train.id,
            trainName: train.name,
            candidateLayer: layer,
            candidateClass: label.rawValue,
            finalLabel: label,
            gateStatus: "pass",
            decisionPath: "test_fixture",
            action: "accept",
            score: 1,
            priority: label == .pause ? 1_100 : 2_000,
            selectedForAuto: selected,
            selectionStatus: selected ? "selected_for_test" : "not_selected",
            startISIIndex: start,
            endISIIndex: end,
            startSpikeIndex: start,
            endSpikeIndex: end + 1,
            nISI: end - start + 1,
            nValidISI: end - start + 1,
            nSpikes: end - start + 2,
            durationSec: Double(end - start + 1) * value,
            intraQ10Sec: value,
            intraQ40Sec: value,
            intraQ50Sec: value,
            intraQ90Sec: value,
            intraQ95Sec: value,
            maxIntraISISec: value,
            meanIntraISISec: value,
            cv: nil,
            lv: nil,
            preGapSec: nil,
            postGapSec: nil,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: nil,
            edgeContrastGeomQ90: nil,
            anchorFamily: label.rawValue,
            anchorLockLevel: lockLevel,
            anchorBandLowerSec: value,
            anchorBandUpperSec: max(0.150, value),
            anchorBandSource: .structure,
            anchorContrastMinRequired: 1,
            anchorContrastGeomRequired: 1,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            pauseBoundaryRole: pauseBoundaryRole
        )
    }
}
