import Foundation

/// Shared spike-train interval statistics used across detector modules.
///
/// CV is standardized on the sample variance estimator: the sum of squared
/// deviations divided by `n - 1`.
enum STPDStatistics {
    /// Arithmetic mean of the finite values, or `nil` when none are finite.
    static func mean(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard !finite.isEmpty else {
            return nil
        }
        return finite.reduce(0, +) / Double(finite.count)
    }

    /// Coefficient of variation using the sample variance estimator
    /// (sum of squared deviations divided by `n - 1`).
    ///
    /// Returns `nil` when fewer than two finite values are present or the mean
    /// is not strictly positive.
    static func coefficientOfVariation(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2,
              let meanValue = mean(finite),
              meanValue > 0 else {
            return nil
        }
        let variance = finite.reduce(0) { partial, value in
            let delta = value - meanValue
            return partial + delta * delta
        } / Double(finite.count - 1)
        return sqrt(variance) / meanValue
    }

    /// Adjacent-interval coefficient of variation (CV2), averaged over
    /// consecutive interval pairs.
    ///
    /// Returns `nil` when fewer than two finite values are present.
    static func coefficientOfVariation2(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2 else {
            return nil
        }
        let terms = zip(finite, finite.dropFirst()).compactMap { previous, next -> Double? in
            let denominator = previous + next
            guard denominator > 0 else {
                return nil
            }
            return 2 * abs(next - previous) / denominator
        }
        return mean(terms)
    }

    /// Local variation (LV), averaged over consecutive interval pairs.
    ///
    /// Returns `nil` when fewer than two finite values are present.
    static func localVariation(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2 else {
            return nil
        }
        let terms = zip(finite, finite.dropFirst()).compactMap { previous, next -> Double? in
            let denominator = previous + next
            guard denominator > 0 else {
                return nil
            }
            let numerator = 3 * pow(next - previous, 2)
            return numerator / pow(denominator, 2)
        }
        return mean(terms)
    }
}
