import Foundation
@testable import STPDCore
import Testing

private func supportAuthorityTrain(_ name: String, milliseconds: [Double]) -> SpikeTrain {
    var timestamps = [0.0]
    for value in milliseconds {
        timestamps.append((timestamps.last ?? 0) + value / 1_000)
    }
    return SpikeTrain(name: name, timestampsSec: timestamps)
}

private func supportAuthorityRun(_ train: SpikeTrain) -> ClassicAnchorDetectionRun {
    ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "support-authority", sourceDescription: "unit-test", trains: [train]),
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        )
    )
}

@Test
func productionTonicCarriesEnforcedDirectSupportGeometry() throws {
    let train = supportAuthorityTrain(
        "clean_support",
        milliseconds: [100, 102, 99, 101, 98, 103, 100, 101, 99, 102]
    )
    let candidates = supportAuthorityRun(train).result(for: train.id)?.candidates ?? []
    let tonic = try #require(candidates.first {
        $0.selectedForAuto && $0.finalLabel == .tonic &&
            $0.decisionPath.contains("state_support_policy=enforced")
    })

    #expect(tonic.stateDirectSupportSpans.count == 1)
    #expect(tonic.stateDirectSupportSpans[0].startISIIndex == tonic.startISIIndex)
    #expect(tonic.stateDirectSupportSpans[0].endISIIndex == tonic.endISIIndex)
    #expect(tonic.stateInterruptionSpans.isEmpty)
    #expect(tonic.stateDirectSupportISICount == tonic.nISI)
    #expect(tonic.stateDirectSupportAdjacentPairCount == tonic.nISI - 1)
    #expect(tonic.decisionPath.contains("state_metrics_scope=direct_support"))
    #expect(tonic.decisionPath.contains("state_cv2_lv_cross_interruptions=false"))
    #expect(decisionValues(for: "state_support_policy", in: tonic.decisionPath) == ["enforced"])
    #expect(decisionValues(for: "state_n_support", in: tonic.decisionPath).count == 1)
    #expect(decisionValues(for: "state_n_core", in: tonic.decisionPath).count == 1)
    #expect(decisionValues(for: "state_support_cv", in: tonic.decisionPath).count == 1)
    #expect(decisionValues(for: "state_support_cv2", in: tonic.decisionPath).count == 1)
    #expect(decisionValues(for: "state_support_lv", in: tonic.decisionPath).count == 1)
}

@Test
func isolatedOrdinaryDeviationRemainsInAuthoritativeTonicMetrics() throws {
    let values: [Double] = [100, 105, 120, 98, 102, 100, 103, 99, 101]
    let train = supportAuthorityTrain("ordinary_deviation", milliseconds: values)
    let candidates = supportAuthorityRun(train).result(for: train.id)?.candidates ?? []
    let tonic = try #require(candidates.first {
        $0.selectedForAuto && $0.finalLabel == .tonic &&
            $0.decisionPath.contains("state_ordinary_deviations=1")
    })

    let seconds = values.map { $0 / 1_000 }
    #expect(tonic.stateDirectSupportISICount == values.count)
    #expect(abs((tonic.cv ?? -1) - (STPDStatistics.coefficientOfVariation(seconds) ?? -2)) < 1e-12)
    #expect(abs((tonic.cv2 ?? -1) - (STPDStatistics.coefficientOfVariation2(seconds) ?? -2)) < 1e-12)
    #expect(abs((tonic.lv ?? -1) - (STPDStatistics.localVariation(seconds) ?? -2)) < 1e-12)
}

@Test
func competingExcursionsAreRetainedForReviewInsteadOfForcedIntoTonic() {
    let train = supportAuthorityTrain(
        "competing_excursions",
        milliseconds: [100, 130, 98, 101, 99, 103, 100, 102, 98, 101]
    )
    let candidates = supportAuthorityRun(train).result(for: train.id)?.candidates ?? []
    let reviews = candidates.filter {
        $0.finalLabel == .reject &&
            $0.gateStatus == "state_support_review_required" &&
            $0.action == "audit_only"
    }

    #expect(!reviews.isEmpty)
    #expect(reviews.contains { $0.decisionPath.contains("state_competing_excursions=") })
    #expect(reviews.allSatisfy { !$0.stateDirectSupportSpans.isEmpty })
    #expect(reviews.allSatisfy { !$0.stateInterruptionSpans.isEmpty })
}

@Test
func disconnectedSingletonSupportExplicitlyClearsMixedEnvelopeCV2AndLV() throws {
    let values = [100.0, 102, 99, 101, 98, 103, 100, 101, 99, 102]
    let train = supportAuthorityTrain("singleton_direct_support", milliseconds: values)
    let production = supportAuthorityRun(train).result(for: train.id)?.candidates ?? []
    let selected = try #require(production.first {
        $0.selectedForAuto && $0.finalLabel == .tonic
    })

    let firstFive = values.prefix(5).map { $0 / 1_000 }
    var base = selected.withGeometry(
        idOverride: "singleton-support-base",
        startISIIndex: 1,
        endISIIndex: 5,
        startSpikeIndex: 1,
        endSpikeIndex: 6,
        nISI: 5,
        nValidISI: 5,
        nSpikes: 6,
        durationSec: firstFive.reduce(0, +),
        intraQ10Sec: nil,
        intraQ40Sec: nil,
        intraQ50Sec: nil,
        intraQ90Sec: nil,
        intraQ95Sec: nil,
        maxIntraISISec: firstFive.max(),
        meanIntraISISec: firstFive.reduce(0, +) / Double(firstFive.count),
        cv: STPDStatistics.coefficientOfVariation(firstFive),
        lv: STPDStatistics.localVariation(firstFive),
        preGapSec: nil,
        postGapSec: train.isiSec[6],
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        decisionPath: "unit_test_mixed_envelope_metrics"
    )
    base.cv2 = STPDStatistics.coefficientOfVariation2(firstFive)
    base.stateDirectSupportSpans = []
    base.stateInterruptionSpans = []
    base.stateDirectSupportISICount = nil
    base.stateDirectSupportAdjacentPairCount = nil

    func interruption(at index: Int) -> ClassicAnchorCandidate {
        var interruption = base.withGeometry(
            idOverride: "test-interruption-\(index)",
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
            decisionPath: "unit_test_selected_event"
        ).withDiagnosticOverride(
            finalLabel: .burst,
            gateStatus: "unit_test_accept",
            action: "accept",
            score: 10,
            priority: 100,
            selectedForAuto: true,
            selectionStatus: "unit_test_selected"
        )
        interruption.burstSeedRunStartISI = index
        interruption.burstSeedRunEndISI = index
        return interruption
    }

    let resolved = StateSupportAuthorityResolver.apply(
        to: [base],
        train: train,
        selectedEvents: [interruption(at: 2), interruption(at: 4)],
        selectedGaps: [],
        settings: StatePatternDetectorSettings(minValidISISec: 0.001)
    )
    let tonic = try #require(resolved.only)
    #expect(tonic.finalLabel == .tonic)
    #expect(tonic.stateDirectSupportSpans.map { $0.startISIIndex...$0.endISIIndex }
        == [1...1, 3...3, 5...5])
    #expect(tonic.stateDirectSupportAdjacentPairCount == 0)
    #expect(tonic.cv != nil)
    #expect(tonic.cv2 == nil)
    #expect(tonic.lv == nil)
}

@Test
func hfsMetricsExcludeSelectedBurstCoreAndNeverBridgeAcrossIt() throws {
    let original = supportAuthorityTrain(
        "hfs_event_exclusion",
        milliseconds: Array(repeating: 10, count: 32)
    )
    let detected = StatePatternDetector.detect(train: original).candidates
    var hfs = try #require(detected.first { $0.finalLabel == .highFrequencySpiking })
    hfs = hfs.withDiagnosticOverride(
        selectedForAuto: true,
        selectionStatus: "unit_test_selected"
    )

    var burst = hfs.withGeometry(
        idOverride: "embedded-burst-core",
        startISIIndex: 9,
        endISIIndex: 13,
        startSpikeIndex: 9,
        endSpikeIndex: 14,
        nISI: 5,
        nValidISI: 5,
        nSpikes: 6,
        durationSec: 0.050,
        intraQ10Sec: 0.010,
        intraQ40Sec: 0.010,
        intraQ50Sec: 0.010,
        intraQ90Sec: 0.010,
        intraQ95Sec: 0.010,
        maxIntraISISec: 0.010,
        meanIntraISISec: 0.010,
        cv: 0,
        lv: 0,
        preGapSec: 0.010,
        postGapSec: 0.010,
        preRatioQ90: 1,
        postRatioQ90: 1,
        edgeContrastMinQ90: 1,
        edgeContrastGeomQ90: 1,
        decisionPath: "unit_test_embedded_burst"
    ).withDiagnosticOverride(
        finalLabel: .burst,
        gateStatus: "unit_test_accept",
        action: "accept",
        score: 10,
        priority: 100,
        selectedForAuto: true,
        selectionStatus: "unit_test_selected"
    )
    burst.burstSeedRunStartISI = 10
    burst.burstSeedRunEndISI = 12

    let resolved = StateSupportAuthorityResolver.apply(
        to: [hfs],
        train: original,
        selectedEvents: [burst],
        selectedGaps: [],
        settings: StatePatternDetectorSettings(minValidISISec: 0.001)
    )
    let authoritative = try #require(resolved.only)
    #expect(authoritative.finalLabel == .highFrequencySpiking)
    #expect(authoritative.decisionPath.contains("hfs_event_core_metrics_excluded=true"))
    #expect(authoritative.stateDirectSupportSpans.map { $0.startISIIndex...$0.endISIIndex }
        == [1...9, 13...32])
    #expect(authoritative.stateInterruptionSpans.map { $0.startISIIndex...$0.endISIIndex }
        == [10...12])
    #expect(authoritative.stateDirectSupportISICount == 29)
    #expect(authoritative.stateDirectSupportAdjacentPairCount == 27)
    #expect(abs(authoritative.cv2 ?? 1) < 1e-12)
    #expect(abs(authoritative.lv ?? 1) < 1e-12)
    #expect(decisionValues(for: "hfs_direct_support_cv", in: authoritative.decisionPath).count == 1)
    #expect(decisionValues(for: "hfs_direct_support_cv2", in: authoritative.decisionPath).count == 1)
    #expect(decisionValues(for: "hfs_direct_support_lv", in: authoritative.decisionPath).count == 1)
}

@Test
func selectedBurstWithoutFrozenCoreMakesStateMetricsReviewOnly() throws {
    let train = supportAuthorityTrain(
        "missing_burst_core",
        milliseconds: Array(repeating: 10, count: 32)
    )
    var hfs = try #require(
        StatePatternDetector.detect(train: train).candidates.first {
            $0.finalLabel == .highFrequencySpiking
        }
    )
    hfs = hfs.withDiagnosticOverride(
        selectedForAuto: true,
        selectionStatus: "unit_test_selected"
    )
    var burst = hfs.withGeometry(
        idOverride: "burst-without-core",
        startISIIndex: 10,
        endISIIndex: 12,
        startSpikeIndex: 10,
        endSpikeIndex: 13,
        nISI: 3,
        nValidISI: 3,
        nSpikes: 4,
        durationSec: 0.030,
        intraQ10Sec: 0.010,
        intraQ40Sec: 0.010,
        intraQ50Sec: 0.010,
        intraQ90Sec: 0.010,
        intraQ95Sec: 0.010,
        maxIntraISISec: 0.010,
        meanIntraISISec: 0.010,
        cv: 0,
        lv: 0,
        preGapSec: 0.010,
        postGapSec: 0.010,
        preRatioQ90: 1,
        postRatioQ90: 1,
        edgeContrastMinQ90: 1,
        edgeContrastGeomQ90: 1,
        decisionPath: "unit_test_missing_burst_core"
    ).withDiagnosticOverride(
        finalLabel: .burst,
        gateStatus: "unit_test_accept",
        action: "accept",
        score: 10,
        priority: 100,
        selectedForAuto: true,
        selectionStatus: "unit_test_selected"
    )
    burst.burstSeedRunStartISI = nil
    burst.burstSeedRunEndISI = nil

    let resolved = StateSupportAuthorityResolver.apply(
        to: [hfs],
        train: train,
        selectedEvents: [burst],
        selectedGaps: [],
        settings: StatePatternDetectorSettings(minValidISISec: 0.001)
    )
    let review = try #require(resolved.only)
    #expect(review.finalLabel == .reject)
    #expect(review.gateStatus == "hfs_direct_support_review_required")
    #expect(review.decisionPath.contains("review_reason=missing_frozen_burst_core_geometry"))
    #expect(review.decisionPath.contains("hfs_missing_burst_core_ids=burst-without-core"))
}

@Test
func hfsAuthorityRevalidatesInheritedSupportAgainstItsFrozenMagnitudeUpper() throws {
    let original = supportAuthorityTrain(
        "hfs_stale_support",
        milliseconds: Array(repeating: 10, count: 32)
    )
    var hfs = try #require(
        StatePatternDetector.detect(train: original).candidates.first {
            $0.finalLabel == .highFrequencySpiking
        }
    )
    hfs = hfs.withDiagnosticOverride(
        selectedForAuto: true,
        selectionStatus: "unit_test_selected"
    )
    let lower = min(hfs.startISIIndex, hfs.endISIIndex)
    let upper = max(hfs.startISIIndex, hfs.endISIIndex)
    let outOfBandIndex = (lower + upper) / 2
    hfs.stateDirectSupportSpans = [
        ISISpan(trainID: original.id, startISIIndex: lower, endISIIndex: upper)
    ]

    var alteredMilliseconds = Array(repeating: 10.0, count: 32)
    alteredMilliseconds[outOfBandIndex - 1] = 100
    let evaluatedTrain = supportAuthorityTrain(
        "hfs_stale_support",
        milliseconds: alteredMilliseconds
    ).withID(original.id)

    let resolved = StateSupportAuthorityResolver.apply(
        to: [hfs],
        train: evaluatedTrain,
        selectedEvents: [],
        selectedGaps: [],
        settings: StatePatternDetectorSettings(minValidISISec: 0.001)
    )
    let authoritative = try #require(resolved.only)
    #expect(authoritative.finalLabel == .highFrequencySpiking)
    #expect(!authoritative.stateDirectSupportSpans.contains { span in
        span.startISIIndex <= outOfBandIndex && outOfBandIndex <= span.endISIIndex
    })
    #expect(authoritative.stateInterruptionSpans.contains { span in
        span.startISIIndex <= outOfBandIndex && outOfBandIndex <= span.endISIIndex
    })
}

private extension Array {
    var only: Element? { count == 1 ? self[0] : nil }
}

private func decisionValues(for key: String, in path: String) -> [String] {
    path.split(separator: ";").compactMap { token in
        let prefix = "\(key)="
        guard token.hasPrefix(prefix) else { return nil }
        return String(token.dropFirst(prefix.count))
    }
}
