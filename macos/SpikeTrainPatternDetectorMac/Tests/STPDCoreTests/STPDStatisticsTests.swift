@testable import STPDCore
import Testing

@Test
func coefficientOfVariationUsesSampleVariance() {
    let values = [1.0, 2.0, 3.0]

    #expect(isClose(STPDStatistics.coefficientOfVariation(values), 0.5))
}

@Test
func coefficientOfVariationMatchesRStatsSDSampleStandardDeviation() {
    // Canonical Mac standard: CV uses R stats::sd() == the sample standard
    // deviation (sum of squared deviations divided by n - 1), matching the
    // R/Shiny calc_CV implementation. This guards against any regression back to
    // a population (n) estimator.
    let values = [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0]
    let n = Double(values.count)
    let mean = 5.0
    let sumSquaredDeviations = 32.0
    let sampleCV = (sumSquaredDeviations / (n - 1)).squareRoot() / mean
    let populationCV = (sumSquaredDeviations / n).squareRoot() / mean

    #expect(isClose(STPDStatistics.coefficientOfVariation(values), sampleCV))
    #expect(abs(sampleCV - populationCV) > 1e-6)
    #expect(!isClose(STPDStatistics.coefficientOfVariation(values), populationCV, tolerance: 1e-6))
}

@Test
func adjacentStatisticsUseSharedFinitePairSemantics() {
    let values = [1.0, 3.0, 6.0]

    #expect(isClose(STPDStatistics.coefficientOfVariation2(values), 5.0 / 6.0))
    #expect(isClose(STPDStatistics.localVariation(values), 13.0 / 24.0))
}

@Test
func sharedStatisticsIgnoreNonFiniteValues() {
    let values = [1.0, .nan, 2.0, .infinity, 3.0]

    #expect(isClose(STPDStatistics.mean(values), 2.0))
    #expect(isClose(STPDStatistics.coefficientOfVariation(values), 0.5))
    #expect(isClose(STPDStatistics.coefficientOfVariation2(values), 8.0 / 15.0))
    #expect(isClose(STPDStatistics.localVariation(values), 17.0 / 75.0))
}

@Test
func sharedStatisticsReturnNilForInsufficientOrInvalidInputs() {
    #expect(STPDStatistics.mean([.nan, .infinity]) == nil)
    #expect(STPDStatistics.coefficientOfVariation([1.0]) == nil)
    #expect(STPDStatistics.coefficientOfVariation([0.0, 0.0]) == nil)
    #expect(STPDStatistics.coefficientOfVariation2([1.0]) == nil)
    #expect(STPDStatistics.localVariation([1.0]) == nil)
    #expect(STPDStatistics.localVariation([1.0, -1.0]) == nil)
}

private func isClose(_ value: Double?, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    guard let value else {
        return false
    }
    return abs(value - expected) <= tolerance
}
