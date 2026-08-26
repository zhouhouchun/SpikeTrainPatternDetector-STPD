import Foundation
@testable import STPDCore
import Testing

@Suite("State support classifier")
struct StateSupportClassifierTests {
    private let settings = StateSupportClassifierSettings(minimumValidISISec: 0.001)

    @Test("An isolated bounded tonic deviation is retained with bilateral core recovery")
    func isolatedDeviationRetainsStateSupport() {
        let analysis = classify(milliseconds: [100, 105, 120, 98, 102])

        #expect(analysis.nRaw == 5)
        #expect(analysis.nValid == 5)
        #expect(analysis.nCore == 4)
        #expect(analysis.ordinaryDeviationCount == 1)
        #expect(analysis.ordinaryDeviationSourceIndices == [3])
        #expect(analysis.competingExcursionCount == 0)
        #expect(analysis.isEligibleForAutomaticTonic(settings: settings))
        #expect(analysis.coreMedianSec == 0.101)
    }

    @Test("Adjacent deviations become competing excursions rather than hidden state support")
    func adjacentDeviationsAreNotOrdinary() {
        let analysis = classify(milliseconds: [100, 120, 122, 101, 99])

        #expect(analysis.nCore == 3)
        #expect(analysis.ordinaryDeviationCount == 0)
        #expect(analysis.competingExcursionSourceIndices == [2, 3])
        #expect(!analysis.isEligibleForAutomaticTonic(settings: settings))
    }

    @Test("An edge deviation requires trimming or review because recovery is one-sided")
    func edgeDeviationIsCompeting() {
        let analysis = classify(milliseconds: [120, 100, 102, 99, 101])

        #expect(analysis.ordinaryDeviationCount == 0)
        #expect(analysis.competingExcursionSourceIndices == [1])
        #expect(!analysis.isEligibleForAutomaticTonic(settings: settings))
    }

    @Test("Short tonic support permits no ordinary deviation")
    func shortSupportRequiresOnlyCoreISIs() {
        let clean = classify(milliseconds: [100, 102, 98])
        let deviating = classify(milliseconds: [100, 120, 102])

        #expect(clean.nSupport == 3)
        #expect(clean.ordinaryDeviationCount == 0)
        #expect(clean.isEligibleForAutomaticTonic(settings: settings))
        #expect(deviating.nSupport == 3)
        #expect(deviating.ordinaryDeviationCount == 1)
        #expect(!deviating.isEligibleForAutomaticTonic(settings: settings))
    }

    @Test("Invalid raw slots stay audit-visible and cannot establish automatic state support")
    func invalidSlotsAreNeverSilentlyBridged() {
        let observations = [
            StateSupportISIObservation(sourceIndex: 1, valueSec: 0.100),
            StateSupportISIObservation(sourceIndex: 2, valueSec: 0),
            StateSupportISIObservation(sourceIndex: 3, valueSec: 0.101)
        ]
        let analysis = StateSupportClassifier.analyze(observations, settings: settings)

        #expect(analysis.invalidCount == 1)
        #expect(analysis.observations[1].classification == .invalid)
        #expect(analysis.nCore == 2)
        #expect(!analysis.isEligibleForAutomaticTonic(settings: settings))
    }

    @Test("Core and ordinary-deviation classification is scale free")
    func classificationIsScaleFree() {
        let base = classify(milliseconds: [100, 105, 120, 98, 102])
        let scaledSettings = StateSupportClassifierSettings(minimumValidISISec: 0.010)
        let scaled = StateSupportClassifier.analyze(
            [1000, 1050, 1200, 980, 1020].enumerated().map { offset, milliseconds in
                StateSupportISIObservation(sourceIndex: offset + 1, valueSec: milliseconds / 1_000)
            },
            settings: scaledSettings
        )

        #expect(base.observations.map(\.classification) == scaled.observations.map(\.classification))
        #expect(base.nCore == scaled.nCore)
        #expect(base.ordinaryDeviationCount == scaled.ordinaryDeviationCount)
    }

    @Test("Maximum-consensus core does not require an observed value at the latent centre")
    func maximumConsensusCoreUsesCentreIntervals() {
        let analysis = classify(milliseconds: [90, 90, 110, 110])

        #expect(analysis.nCore == 4)
        #expect(analysis.ordinaryDeviationCount == 0)
        #expect(analysis.competingExcursionCount == 0)
        #expect(analysis.coreMedianSec == 0.100)
        #expect(analysis.isEligibleForAutomaticTonic(settings: settings))
    }

    @Test("Sustained tonic deviation cap is an audit warning rather than a hard veto")
    func sustainedDeviationAuditToleranceDoesNotOverrideCoreMajority() {
        let analysis = classify(milliseconds: [
            100, 120, 101, 121, 99, 119, 102, 118, 100, 101, 99, 102, 100
        ])

        #expect(analysis.ordinaryDeviationCount == 4)
        #expect(analysis.nCore > analysis.ordinaryDeviationCount)
        #expect(analysis.exceedsOrdinaryDeviationAuditTolerance(settings: settings))
        #expect(analysis.isEligibleForAutomaticTonic(settings: settings))
    }

    private func classify(milliseconds: [Double]) -> StateSupportAnalysis {
        StateSupportClassifier.analyze(
            milliseconds.enumerated().map { offset, value in
                StateSupportISIObservation(sourceIndex: offset + 1, valueSec: value / 1_000)
            },
            settings: settings
        )
    }
}
