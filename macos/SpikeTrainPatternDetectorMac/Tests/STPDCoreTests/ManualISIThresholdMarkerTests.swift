import Testing
@testable import STPDCore

@Suite("Manual ISI threshold marker")
struct ManualISIThresholdMarkerTests {
    @Test("Burst uses inclusive ISI and spike-count ranges without chopping oversized runs")
    func burstClosedRangesAndOversizedProtection() throws {
        let samples = [
            sample(1, 10), sample(2, 15),
            sample(3, 40),
            sample(4, 10), sample(5, 11), sample(6, 12), sample(7, 13), sample(8, 14),
        ]
        let rule = ManualISIThresholdRule(
            pattern: .burst,
            isiRangeSeconds: milliseconds(10...15),
            minimumSpikeCount: 3,
            maximumSpikeCount: 5
        )

        let candidates = try ManualISIThresholdMarker.propose(
            samples: samples,
            rule: rule,
            minimumValidISISeconds: 0.9 / 1_000
        )

        #expect(candidates.map(\.isiIndices) == [[1, 2]])
        #expect(candidates.first?.spikeCount == 3)
    }

    @Test("Pause includes both closed-range boundaries and excludes absolutely invalid ISI")
    func pauseClosedRangeAndQC() throws {
        let rule = ManualISIThresholdRule(
            pattern: .pause,
            isiRangeSeconds: milliseconds(100...200)
        )
        let candidates = try ManualISIThresholdMarker.propose(
            samples: [sample(1, 100), sample(2, 150), sample(3, 200), sample(4, 0)],
            rule: rule,
            minimumValidISISeconds: 0.9 / 1_000
        )

        #expect(candidates.map(\.isiIndices) == [[1], [2], [3]])
    }

    @Test("Optional Burst edge contrast uses the weaker flank over intra-run Q90")
    func burstEdgeContrastUsesWeakerQ90Ratio() throws {
        let samples = [
            sample(1, 100),
            sample(2, 10), sample(3, 20),
            sample(4, 80),
        ]
        let acceptingRule = ManualISIThresholdRule(
            pattern: .burst,
            isiRangeSeconds: milliseconds(10...20),
            minimumSpikeCount: 3,
            maximumSpikeCount: 3,
            burstMinimumEdgeContrast: 4
        )

        let accepted = try ManualISIThresholdMarker.propose(
            samples: samples,
            rule: acceptingRule,
            minimumValidISISeconds: 0.9 / 1_000
        )

        #expect(accepted.count == 1)
        #expect(abs((accepted[0].burstIntraQ90Seconds ?? 0) - 0.019) < 1e-12)
        #expect(abs((accepted[0].burstLeftEdgeContrast ?? 0) - (0.100 / 0.019)) < 1e-12)
        #expect(abs((accepted[0].burstRightEdgeContrast ?? 0) - (0.080 / 0.019)) < 1e-12)
        #expect(abs((accepted[0].burstMinimumEdgeContrast ?? 0) - (0.080 / 0.019)) < 1e-12)

        let rejectingRule = ManualISIThresholdRule(
            pattern: .burst,
            isiRangeSeconds: milliseconds(10...20),
            minimumSpikeCount: 3,
            maximumSpikeCount: 3,
            burstMinimumEdgeContrast: 4.5
        )
        let rejected = try ManualISIThresholdMarker.propose(
            samples: samples,
            rule: rejectingRule,
            minimumValidISISeconds: 0.9 / 1_000
        )
        #expect(rejected.isEmpty)
    }

    @Test("Burst contrast uses one flank at a true train edge but requires measurable evidence")
    func burstContrastHandlesTrainEdges() throws {
        let rule = ManualISIThresholdRule(
            pattern: .burst,
            isiRangeSeconds: milliseconds(10...10),
            minimumSpikeCount: 3,
            maximumSpikeCount: 3,
            burstMinimumEdgeContrast: 9
        )

        let oneSided = try ManualISIThresholdMarker.propose(
            samples: [sample(1, 10), sample(2, 10), sample(3, 100)],
            rule: rule,
            minimumValidISISeconds: 0.9 / 1_000
        )
        #expect(oneSided.count == 1)
        #expect(oneSided[0].burstLeftEdgeContrast == nil)
        #expect(oneSided[0].burstRightEdgeContrast == 10)
        #expect(oneSided[0].burstMinimumEdgeContrast == 10)

        let noFlank = try ManualISIThresholdMarker.propose(
            samples: [sample(1, 10), sample(2, 10)],
            rule: rule,
            minimumValidISISeconds: 0.9 / 1_000
        )
        #expect(noFlank.isEmpty)

        let discontinuous = try ManualISIThresholdMarker.propose(
            samples: [sample(1, 100), sample(3, 10), sample(4, 10), sample(5, 100)],
            rule: rule,
            minimumValidISISeconds: 0.9 / 1_000
        )
        #expect(discontinuous.isEmpty)
    }

    @Test("Burst contrast is optional and validates a dimensionless minimum of at least one")
    func burstContrastOptionalityAndValidation() throws {
        let unfiltered = ManualISIThresholdRule(
            pattern: .burst,
            isiRangeSeconds: milliseconds(10...10),
            minimumSpikeCount: 3,
            maximumSpikeCount: 3
        )
        let wholeTrain = try ManualISIThresholdMarker.propose(
            samples: [sample(1, 10), sample(2, 10)],
            rule: unfiltered,
            minimumValidISISeconds: 0.9 / 1_000
        )
        #expect(wholeTrain.count == 1)

        let invalid = ManualISIThresholdRule(
            pattern: .burst,
            isiRangeSeconds: milliseconds(10...10),
            minimumSpikeCount: 3,
            maximumSpikeCount: 3,
            burstMinimumEdgeContrast: 0.99
        )
        #expect(throws: ManualISIThresholdMarkerError.invalidBurstContrast) {
            try ManualISIThresholdMarker.propose(
                samples: [sample(1, 10), sample(2, 10)],
                rule: invalid,
                minimumValidISISeconds: 0.9 / 1_000
            )
        }
    }

    @Test("Tonic reuses canonical CV CV2 and LV over the entire contiguous run")
    func tonicRegularityMetrics() throws {
        let samples = [
            sample(1, 100), sample(2, 100), sample(3, 100), sample(4, 100), sample(5, 100),
        ]

        for metric in [ManualISITonicMetric.cv, .cv2, .lv] {
            let rule = ManualISIThresholdRule(
                pattern: .tonic,
                isiRangeSeconds: milliseconds(90...110),
                minimumSpikeCount: 6,
                tonicMetric: metric,
                tonicMetricRange: ManualISIThresholdClosedRange(lowerBound: 0, upperBound: 0)
            )
            let candidates = try ManualISIThresholdMarker.propose(
                samples: samples,
                rule: rule,
                minimumValidISISeconds: 0.9 / 1_000
            )
            #expect(candidates.count == 1)
            #expect(candidates[0].isiIndices == [1, 2, 3, 4, 5])
            #expect(candidates[0].regularityMetric == metric)
            #expect(candidates[0].regularityValue == 0)
        }
    }

    @Test("Short Tonic uses MM as max ISI divided by min ISI for three to five spikes")
    func shortTonicMM() throws {
        let rule = ManualISIThresholdRule(
            pattern: .tonic,
            isiRangeSeconds: milliseconds(90...120),
            minimumSpikeCount: 3,
            maximumSpikeCount: 5,
            tonicMetric: .mm,
            tonicMetricRange: ManualISIThresholdClosedRange(lowerBound: 1.2, upperBound: 1.2)
        )
        let candidates = try ManualISIThresholdMarker.propose(
            samples: [sample(1, 100), sample(2, 120), sample(3, 110), sample(4, 105)],
            rule: rule,
            minimumValidISISeconds: 0.9 / 1_000
        )

        #expect(candidates.count == 1)
        #expect(candidates[0].spikeCount == 5)
        #expect(candidates[0].regularityMetric == .mm)
        #expect(abs((candidates[0].regularityValue ?? 0) - 1.2) < 1e-12)
    }

    @Test("MM short-Tonic path cannot admit six or more spikes")
    func shortTonicMMRejectsLongConfiguration() {
        let rule = ManualISIThresholdRule(
            pattern: .tonic,
            isiRangeSeconds: milliseconds(90...120),
            minimumSpikeCount: 3,
            maximumSpikeCount: 6,
            tonicMetric: .mm,
            tonicMetricRange: ManualISIThresholdClosedRange(lowerBound: 1, upperBound: 1.2)
        )

        #expect(throws: ManualISIThresholdMarkerError.invalidSpikeCountRange) {
            try ManualISIThresholdMarker.propose(
                samples: [sample(1, 100), sample(2, 100)],
                rule: rule,
                minimumValidISISeconds: 0.9 / 1_000
            )
        }
    }

    @Test("Invalid ISI is a hard run break and is never bridged by quick labeling")
    func invalidISIBreaksRun() throws {
        let rule = ManualISIThresholdRule(
            pattern: .tonic,
            isiRangeSeconds: milliseconds(0...120),
            minimumSpikeCount: 6,
            tonicMetric: .cv2,
            tonicMetricRange: ManualISIThresholdClosedRange(lowerBound: 0, upperBound: 1)
        )
        let candidates = try ManualISIThresholdMarker.propose(
            samples: [
                sample(1, 100), sample(2, 100), sample(3, 100), sample(4, 100), sample(5, 100),
                sample(6, 0.5),
                sample(7, 100), sample(8, 100), sample(9, 100), sample(10, 100), sample(11, 100),
            ],
            rule: rule,
            minimumValidISISeconds: 0.9 / 1_000
        )

        #expect(candidates.map(\.isiIndices) == [[1, 2, 3, 4, 5], [7, 8, 9, 10, 11]])
    }

    @Test("Metric-based Tonic quick labeling requires five ISIs")
    func tonicMetricMinimumSampleSize() {
        let rule = ManualISIThresholdRule(
            pattern: .tonic,
            isiRangeSeconds: milliseconds(90...110),
            minimumSpikeCount: 5,
            tonicMetric: .cv2,
            tonicMetricRange: ManualISIThresholdClosedRange(lowerBound: 0, upperBound: 1)
        )

        #expect(throws: ManualISIThresholdMarkerError.invalidSpikeCountRange) {
            try ManualISIThresholdMarker.propose(
                samples: [sample(1, 100), sample(2, 100), sample(3, 100), sample(4, 100)],
                rule: rule,
                minimumValidISISeconds: 0.9 / 1_000
            )
        }
    }

    @Test("QC flags both zero ISI and a positive ISI below the configured floor")
    func manualQCInvalidity() {
        #expect(ManualISIQualityRule.isAbsolutelyInvalid(
            isiSeconds: 0,
            thresholdSeconds: 0.0009
        ))
        #expect(ManualISIQualityRule.isAbsolutelyInvalid(
            isiSeconds: 0.0005,
            thresholdSeconds: 0.0009
        ))
        #expect(!ManualISIQualityRule.isAbsolutelyInvalid(
            isiSeconds: 0.0009,
            thresholdSeconds: 0.0009
        ))
    }

    private func sample(_ index: Int, _ milliseconds: Double) -> ManualISIThresholdSample {
        ManualISIThresholdSample(isiIndex: index, isiSeconds: milliseconds / 1_000)
    }

    private func milliseconds(
        _ range: ClosedRange<Double>
    ) -> ManualISIThresholdClosedRange {
        ManualISIThresholdClosedRange(
            lowerBound: range.lowerBound / 1_000,
            upperBound: range.upperBound / 1_000
        )
    }
}
