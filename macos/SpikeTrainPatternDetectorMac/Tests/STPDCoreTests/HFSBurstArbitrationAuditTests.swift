import XCTest
@testable import STPDCore

final class HFSBurstArbitrationAuditTests: XCTestCase {
    func testHFSBurstOverlapProducesExplainableBurstWinsAuditRow() throws {
        let train = makeTrain(
            name: "train-1",
            intervals: [
                0.012, 0.009, 0.008, 0.015, 0.006,
                0.007, 0.018, 0.011, 0.12, 0.009,
                0.008, 0.013, 0.010, 0.009, 0.007,
                0.006, 0.018, 0.012, 0.011, 0.010
            ]
        )
        var hfs = makeCandidate(
            id: "hfs",
            label: .highFrequencySpiking,
            start: 1,
            end: 20,
            score: 27.5,
            priority: 1_040,
            selected: true
        )
        hfs.hfSpikingPauseBreakSec = 0.10
        hfs.hfSpikingEmbeddedBurstGroupCount = 1
        hfs.hfSpikingEmbeddedBurstCoverage = 0.20
        hfs.hfSpikingBurstPacketLike = false
        hfs.hfSpikingBurstDominated = false

        var burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 5,
            end: 8,
            score: 16.2,
            priority: 1_250,
            selected: true
        )
        burst.burstSeedBandLowerSec = 0.001
        burst.burstSeedBandUpperSec = 0.010
        burst.burstBridgeBandUpperSec = 0.020

        let protected = HFSpikingProtection.apply(
            to: [hfs, burst],
            selectedEvents: [burst],
            mode: .multiTrack
        )
        let finalCandidates = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)

        let rows = HFSBurstArbitrationAudit.build(
            train: train,
            preProtectionCandidates: [hfs, burst],
            selectedEventsUsedForPacketization: [burst],
            protectedCandidates: protected,
            finalCandidates: finalCandidates,
            pipelineStage: "test"
        )

        let row = try XCTUnwrap(rows.first)
        XCTAssertEqual(row.finalDecision, .burstSelectedHFSNotSelected)
        XCTAssertEqual(row.hfsRawScore, 27.5, accuracy: 1e-12)
        XCTAssertEqual(row.packetCount, 1)
        XCTAssertEqual(row.pauseLikeBreakCount, 1)
        XCTAssertEqual(row.seedFraction ?? -1, 0.50, accuracy: 1e-12)
        XCTAssertEqual(row.bridgeFraction ?? -1, 0.50, accuracy: 1e-12)
        XCTAssertEqual(row.suppressedBurstProposalCount, 0)
        XCTAssertEqual(row.finalSelectedEventSubtypes, ["burst_i"])
    }

    func testPacketDominatedHFSRetainsOriginalScoresInAudit() throws {
        let train = makeTrain(
            name: "train-1",
            intervals: Array(repeating: 0.008, count: 100)
        )
        var hfs = makeCandidate(
            id: "hfs",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            score: 31.0,
            priority: 1_040,
            selected: true,
            cv: 0.80
        )
        hfs.hfSpikingLargeFraction = 0.10

        let bursts = (0..<6).map { group in
            makeCandidate(
                id: "burst-\(group)",
                label: group == 0 ? .longBurst : .burst,
                start: 5 + group * 15,
                end: 9 + group * 15,
                score: group == 0 ? 24.0 : 15.0,
                priority: group == 0 ? 1_160 : 1_250,
                selected: true
            )
        }

        let protected = HFSpikingProtection.apply(
            to: [hfs] + bursts,
            selectedEvents: bursts,
            mode: .multiTrack
        )
        let final = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)
        let rows = HFSBurstArbitrationAudit.build(
            train: train,
            preProtectionCandidates: [hfs] + bursts,
            selectedEventsUsedForPacketization: bursts,
            protectedCandidates: protected,
            finalCandidates: final,
            pipelineStage: "test"
        )

        let row = try XCTUnwrap(rows.first)
        XCTAssertEqual(row.hfsRawScore, 31.0, accuracy: 1e-12)
        XCTAssertEqual(row.longBurstRawScore ?? -1, 24.0, accuracy: 1e-12)
        XCTAssertEqual(row.packetCount, 6)
        XCTAssertTrue(row.burstDominated)
        XCTAssertEqual(row.finalDecision, .burstSelectedHFSRejectedPacketDominance)
    }

    func testSupersededHFSParentIsExcludedByDefault() {
        let train = makeTrain(
            name: "train-1",
            intervals: Array(repeating: 0.008, count: 30)
        )
        var parent = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 30,
            selected: false
        )
        parent.hfSpikingBurstPacketLike = true
        var child = makeCandidate(
            id: "hfs-parent::state-split::1-15",
            label: .highFrequencySpiking,
            start: 1,
            end: 15,
            selected: true
        )
        child.hfSpikingBurstPacketLike = true
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 3,
            end: 6,
            selected: true
        )

        let rows = HFSBurstArbitrationAudit.build(
            train: train,
            preProtectionCandidates: [parent, child, burst],
            selectedEventsUsedForPacketization: [burst],
            protectedCandidates: [parent, child, burst],
            finalCandidates: [parent, child, burst],
            pipelineStage: "test"
        )

        XCTAssertEqual(rows.map(\.hfsCandidateID), ["hfs-parent::state-split::1-15"])
        XCTAssertEqual(rows.first?.hfsRootCandidateID, "hfs-parent")
    }

    func testCSVExportContainsRequestedColumns() {
        XCTAssertTrue(HFSBurstArbitrationAuditRow.csvHeader.contains("hfs_raw_score"))
        XCTAssertTrue(HFSBurstArbitrationAuditRow.csvHeader.contains("long_burst_raw_score"))
        XCTAssertTrue(HFSBurstArbitrationAuditRow.csvHeader.contains("packet_count"))
        XCTAssertTrue(HFSBurstArbitrationAuditRow.csvHeader.contains("pause_like_break_count"))
        XCTAssertTrue(HFSBurstArbitrationAuditRow.csvHeader.contains("seed_fraction"))
        XCTAssertTrue(HFSBurstArbitrationAuditRow.csvHeader.contains("bridge_fraction"))
        XCTAssertTrue(HFSBurstArbitrationAuditRow.csvHeader.contains("final_decision"))
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
        label: ClassicAnchorLabel,
        start: Int,
        end: Int,
        score: Double = 1,
        priority: Int = 100,
        selected: Bool = false,
        cv: Double = 0.10,
        q50: Double = 0.01
    ) -> ClassicAnchorCandidate {
        ClassicAnchorCandidate(
            id: id,
            trainID: "train-1",
            trainName: "train-1",
            candidateLayer: "test_candidate",
            candidateClass: label.rawValue,
            finalLabel: label,
            gateStatus: "pass",
            decisionPath: "test",
            action: "accept",
            score: score,
            priority: priority,
            selectedForAuto: selected,
            selectionStatus: selected ? "selected_for_test" : "not_selected",
            startISIIndex: start,
            endISIIndex: end,
            startSpikeIndex: start,
            endSpikeIndex: end + 1,
            nISI: max(1, end - start + 1),
            nValidISI: max(1, end - start + 1),
            nSpikes: max(2, end - start + 2),
            durationSec: Double(max(1, end - start + 1)) * q50,
            intraQ10Sec: q50,
            intraQ40Sec: q50,
            intraQ50Sec: q50,
            intraQ90Sec: q50,
            intraQ95Sec: q50,
            maxIntraISISec: q50,
            meanIntraISISec: q50,
            cv: cv,
            lv: 0.10,
            mm: 1.0,
            preGapSec: nil,
            postGapSec: nil,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: 3.0,
            edgeContrastGeomQ90: 3.0,
            anchorFamily: label.rawValue,
            anchorLockLevel: .strongCandidate,
            anchorBandLowerSec: 0.001,
            anchorBandUpperSec: 0.010,
            anchorBandSource: .structure,
            anchorContrastMinRequired: 1,
            anchorContrastGeomRequired: 1,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil
        )
    }
}
