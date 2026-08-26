import Foundation
@testable import STPDCore
import Testing

@Suite("Pause boundary-role producers")
struct PauseBoundaryRoleProducerTests {
    @Test
    func pauseDetectorProducesCanonicalAndBriefRolesButNeverContextual() throws {
        let train = makeTrain(
            name: "pause-role-direct",
            intervals: [
                0.040, 0.040, 0.040, 0.040,
                0.120,
                0.040, 0.040, 0.040,
                0.200,
                0.040, 0.040, 0.040, 0.040,
            ]
        )
        let settings = PauseDetectorSettings(
            seedThresholdSec: 0.100,
            strongThresholdSec: 0.150,
            alpha: 2.0,
            useGlobalMedianGuard: false,
            eventCoreGapEnabled: false,
            antiTonicVeto: false
        )

        let candidates = PauseDetector.detect(train: train, settings: settings).candidates
        let brief = try #require(candidates.first { $0.startISIIndex == 5 })
        let canonical = try #require(candidates.first { $0.startISIIndex == 9 })

        #expect(brief.pauseBoundaryRole == .briefStateInterruption)
        #expect(brief.gateStatus == "pause_seed_long_isi_pass")
        #expect(brief.decisionPath.contains("pause_boundary_role=brief_state_interruption"))
        #expect(canonical.pauseBoundaryRole == .canonicalPauseAnchor)
        #expect(canonical.gateStatus == "pause_strong_long_isi_pass")
        #expect(canonical.decisionPath.contains("pause_boundary_role=canonical_pause_anchor"))
        #expect(candidates.allSatisfy { $0.pauseBoundaryRole != .contextualPause })
    }

    @Test
    func monotonicCompletionProducesCanonicalAndBriefRolesButNeverContextual() throws {
        let train = makeTrain(
            name: "pause-role-monotonic",
            intervals: [
                0.040, 0.040, 0.040, 0.040,
                0.100,
                0.040, 0.040,
                0.120,
                0.040, 0.040,
                0.200,
                0.040, 0.040, 0.040,
            ]
        )
        let settings = PauseDetectorSettings(
            seedThresholdSec: 0.100,
            strongThresholdSec: 0.150,
            eventCoreLocalFactor: 1.55,
            eventCoreGlobalFactor: 1.25,
            antiTonicVeto: false
        )
        let selectedFloor = makeCandidate(
            id: "selected-pause-floor",
            train: train,
            label: .pause,
            start: 5,
            end: 5,
            q50: 0.100,
            lockLevel: .strongCandidate,
            selected: true,
            pauseBoundaryRole: .briefStateInterruption
        )

        let completions = PauseMonotonicCompletionDetector.detect(
            train: train,
            candidates: [selectedFloor],
            settings: settings
        )
        let brief = try #require(completions.first { $0.startISIIndex == 8 })
        let canonical = try #require(completions.first { $0.startISIIndex == 11 })

        #expect(brief.pauseBoundaryRole == .briefStateInterruption)
        #expect(brief.gateStatus == "pause_floor_completion_pass")
        #expect(brief.decisionPath.contains("pause_boundary_role=brief_state_interruption"))
        #expect(canonical.pauseBoundaryRole == .canonicalPauseAnchor)
        #expect(canonical.gateStatus == "pause_floor_completion_strong_pass")
        #expect(canonical.decisionPath.contains("pause_boundary_role=canonical_pause_anchor"))
        #expect(completions.allSatisfy { $0.pauseBoundaryRole != .contextualPause })
    }

    @Test
    func classicBurstFlankDetectorProducesCanonicalAndBriefRolesButNeverContextual() throws {
        let train = makeTrain(
            name: "pause-role-burst-flanks",
            intervals: [
                0.040,
                0.200,
                0.020, 0.020, 0.020,
                0.120,
                0.040,
            ]
        )
        let selectedBurst = makeCandidate(
            id: "selected-classic-burst",
            train: train,
            label: .burst,
            start: 3,
            end: 5,
            q50: 0.020,
            lockLevel: .lockedClassic,
            selected: true
        )
        let settings = PauseDetectorSettings(
            strongThresholdSec: 0.150,
            classicBurstFlankPauseContrastMin: 5.0
        )

        let candidates = ClassicBurstFlankPauseDetector.detect(
            train: train,
            candidates: [selectedBurst],
            settings: settings
        )
        let canonical = try #require(candidates.first { $0.startISIIndex == 2 })
        let brief = try #require(candidates.first { $0.startISIIndex == 6 })

        #expect(canonical.pauseBoundaryRole == .canonicalPauseAnchor)
        #expect(canonical.gateStatus == "classic_burst_flank_strong_pause_pass")
        #expect(canonical.decisionPath.contains("pause_boundary_role=canonical_pause_anchor"))
        #expect(brief.pauseBoundaryRole == .briefStateInterruption)
        #expect(brief.gateStatus == "classic_burst_flank_structural_pause_pass")
        #expect(brief.decisionPath.contains("pause_boundary_role=brief_state_interruption"))
        #expect(candidates.allSatisfy { $0.pauseBoundaryRole != .contextualPause })
    }

    private func makeTrain(name: String, intervals: [Double]) -> SpikeTrain {
        var timestamps = [0.0]
        timestamps.reserveCapacity(intervals.count + 1)
        for interval in intervals {
            timestamps.append((timestamps.last ?? 0) + interval)
        }
        return SpikeTrain(name: name, timestampsSec: timestamps)
    }

    private func makeCandidate(
        id: String,
        train: SpikeTrain,
        label: ClassicAnchorLabel,
        start: Int,
        end: Int,
        q50: Double,
        lockLevel: ClassicAnchorLockLevel,
        selected: Bool,
        pauseBoundaryRole: PauseBoundaryRole? = nil
    ) -> ClassicAnchorCandidate {
        ClassicAnchorCandidate(
            id: id,
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "pause_boundary_role_test",
            candidateClass: label.rawValue,
            finalLabel: label,
            gateStatus: "pass",
            decisionPath: "pause_boundary_role_test",
            action: "accept",
            score: 1,
            priority: 1_000,
            selectedForAuto: selected,
            selectionStatus: selected ? "selected_for_test" : "not_selected",
            startISIIndex: start,
            endISIIndex: end,
            startSpikeIndex: start,
            endSpikeIndex: end + 1,
            nISI: end - start + 1,
            nValidISI: end - start + 1,
            nSpikes: end - start + 2,
            durationSec: Double(end - start + 1) * q50,
            intraQ10Sec: q50,
            intraQ40Sec: q50,
            intraQ50Sec: q50,
            intraQ90Sec: q50,
            intraQ95Sec: q50,
            maxIntraISISec: q50,
            meanIntraISISec: q50,
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
            anchorBandLowerSec: q50,
            anchorBandUpperSec: max(0.150, q50),
            anchorBandSource: .structure,
            anchorContrastMinRequired: 1,
            anchorContrastGeomRequired: 1,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            pauseBoundaryRole: pauseBoundaryRole
        )
    }
}
