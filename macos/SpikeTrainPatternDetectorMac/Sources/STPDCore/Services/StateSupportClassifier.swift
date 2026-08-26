import Foundation

/// The role of one raw ISI slot when evaluating support for a background state.
///
/// This is deliberately distinct from a final pattern label. In particular, an
/// `.ordinaryDeviation` is a valid biological beat that remains part of the
/// standard state-support metrics; it is not an artifact and is not silently
/// removed from the train.
public enum StateSupportISIClass: String, CaseIterable, Hashable, Sendable {
    /// Non-finite, non-positive, or below the caller's QC-validity floor.
    case invalid
    /// Member of the maximal internally consistent state core.
    case core
    /// A bounded, isolated valid deviation with core recovery on both sides.
    case ordinaryDeviation = "ordinary_deviation"
    /// A valid ISI outside the support contract; it must be proposed to an
    /// event/gap/state competitor or sent to review, never hidden in a state.
    case competingExcursion = "competing_excursion"
}

/// One ISI in original train order. `sourceIndex` is preserved so callers can
/// connect the analysis to the exact raw ISI slot and audit it without
/// reconstructing or deleting spikes.
public struct StateSupportISIObservation: Hashable, Sendable {
    public let sourceIndex: Int
    public let valueSec: Double?

    public init(sourceIndex: Int, valueSec: Double?) {
        self.sourceIndex = sourceIndex
        self.valueSec = valueSec
    }
}

private struct StateSupportValidObservation: Sendable {
    let inputOffset: Int
    let sourceIndex: Int
    let valueSec: Double
}

/// Calibratable scale-free bands for a state-support analysis.
///
/// The defaults encode the approved distinction between a tight core and a
/// wider ordinary-deviation band: for a 100 ms core, 90–110 ms is core and
/// 80–125 ms is potentially an ordinary deviation. They are intentionally
/// ratios rather than absolute milliseconds, so a recording with a 10 ms or a
/// 200 ms background rhythm is treated on the same proportional scale. These
/// are a conservative starting policy and must remain explicit in any future
/// run contract/calibration profile.
public struct StateSupportClassifierSettings: Hashable, Sendable {
    public var minimumValidISISec: Double
    public var coreRatioLower: Double
    public var coreRatioUpper: Double
    public var ordinaryDeviationRatioLower: Double
    public var ordinaryDeviationRatioUpper: Double
    public var maximumAutomaticDeviationCount: Int

    public init(
        minimumValidISISec: Double,
        coreRatioLower: Double = 0.90,
        coreRatioUpper: Double = 1.10,
        ordinaryDeviationRatioLower: Double = 0.80,
        ordinaryDeviationRatioUpper: Double = 1.25,
        maximumAutomaticDeviationCount: Int = 3
    ) {
        let validFloor = minimumValidISISec.isFinite && minimumValidISISec > 0
            ? minimumValidISISec
            : 0.001
        let cleanedCoreLower = Self.positive(coreRatioLower, fallback: 0.90)
        let cleanedCoreUpper = max(cleanedCoreLower, Self.positive(coreRatioUpper, fallback: 1.10))
        let cleanedOrdinaryLower = min(
            cleanedCoreLower,
            Self.positive(ordinaryDeviationRatioLower, fallback: 0.80)
        )
        let cleanedOrdinaryUpper = max(
            cleanedCoreUpper,
            Self.positive(ordinaryDeviationRatioUpper, fallback: 1.25)
        )

        self.minimumValidISISec = validFloor
        self.coreRatioLower = cleanedCoreLower
        self.coreRatioUpper = cleanedCoreUpper
        self.ordinaryDeviationRatioLower = cleanedOrdinaryLower
        self.ordinaryDeviationRatioUpper = cleanedOrdinaryUpper
        self.maximumAutomaticDeviationCount = max(0, maximumAutomaticDeviationCount)
    }

    private static func positive(_ value: Double, fallback: Double) -> Double {
        value.isFinite && value > 0 ? value : fallback
    }
}

/// The immutable output of `StateSupportClassifier`.
///
/// `nSupport` is deliberately **not** the same as the raw count: it is the
/// number of core plus ordinary-deviation ISIs that can legitimately support a
/// state. Valid competing excursions remain visible in `nValid` and must be
/// handled by another family or review before a final state can be confirmed.
public struct StateSupportAnalysis: Hashable, Sendable {
    public struct ClassifiedObservation: Hashable, Sendable {
        public let sourceIndex: Int
        public let valueSec: Double?
        public let classification: StateSupportISIClass

        public init(sourceIndex: Int, valueSec: Double?, classification: StateSupportISIClass) {
            self.sourceIndex = sourceIndex
            self.valueSec = valueSec
            self.classification = classification
        }
    }

    public let observations: [ClassifiedObservation]
    public let coreMedianSec: Double?
    public let nRaw: Int
    public let nValid: Int
    public let nSupport: Int
    public let nCore: Int
    public let ordinaryDeviationCount: Int
    public let competingExcursionCount: Int
    public let invalidCount: Int

    public init(
        observations: [ClassifiedObservation],
        coreMedianSec: Double?,
        nRaw: Int,
        nValid: Int,
        nSupport: Int,
        nCore: Int,
        ordinaryDeviationCount: Int,
        competingExcursionCount: Int,
        invalidCount: Int
    ) {
        self.observations = observations
        self.coreMedianSec = coreMedianSec
        self.nRaw = nRaw
        self.nValid = nValid
        self.nSupport = nSupport
        self.nCore = nCore
        self.ordinaryDeviationCount = ordinaryDeviationCount
        self.competingExcursionCount = competingExcursionCount
        self.invalidCount = invalidCount
    }

    public var coreSourceIndices: [Int] {
        observations.compactMap { $0.classification == .core ? $0.sourceIndex : nil }
    }

    public var ordinaryDeviationSourceIndices: [Int] {
        observations.compactMap { $0.classification == .ordinaryDeviation ? $0.sourceIndex : nil }
    }

    public var competingExcursionSourceIndices: [Int] {
        observations.compactMap { $0.classification == .competingExcursion ? $0.sourceIndex : nil }
    }

    /// Implements the agreed automatic-tonic policy. This is intentionally a
    /// support verdict only: callers must still apply family magnitude, event,
    /// pause, and state-specific evidence before emitting a final label.
    public func isEligibleForAutomaticTonic(
        settings: StateSupportClassifierSettings
    ) -> Bool {
        guard invalidCount == 0, competingExcursionCount == 0 else { return false }
        switch nSupport {
        case ..<3:
            return false
        case 3...4:
            return ordinaryDeviationCount == 0
        case 5:
            return ordinaryDeviationCount <= 1 && nCore > ordinaryDeviationCount
        default:
            return ordinaryDeviationCount <= settings.maximumAutomaticDeviationCount
                && nCore > ordinaryDeviationCount
        }
    }
}

/// Finds a robust background-state core and separates tolerated biological
/// variation from evidence that must compete with another family.
///
/// The classifier is pure and train-order preserving. It never deletes or
/// re-times raw spikes. It is designed to become the shared support contract
/// for tonic, HF-tonic, HFS, metrics, audit, and review; this first slice makes
/// the scientific decision explicit and independently testable.
public enum StateSupportClassifier {
    public static func analyze(
        _ observations: [StateSupportISIObservation],
        settings: StateSupportClassifierSettings
    ) -> StateSupportAnalysis {
        let valid = observations.enumerated().compactMap { offset, observation -> StateSupportValidObservation? in
            guard let value = observation.valueSec,
                  value.isFinite,
                  value >= settings.minimumValidISISec else {
                return nil
            }
            return StateSupportValidObservation(
                inputOffset: offset,
                sourceIndex: observation.sourceIndex,
                valueSec: value
            )
        }
        guard !valid.isEmpty else {
            return StateSupportAnalysis(
                observations: observations.map {
                    .init(sourceIndex: $0.sourceIndex, valueSec: $0.valueSec, classification: .invalid)
                },
                coreMedianSec: nil,
                nRaw: observations.count,
                nValid: 0,
                nSupport: 0,
                nCore: 0,
                ordinaryDeviationCount: 0,
                competingExcursionCount: 0,
                invalidCount: observations.count
            )
        }

        var coreOffsets = maximumConsensusCore(valid, settings: settings)
        // Re-centre on the core-only median until membership stabilizes. This is
        // what prevents an ordinary outlier from pulling the reference centre.
        for _ in 0..<4 {
            let coreValues = valid.compactMap { coreOffsets.contains($0.inputOffset) ? $0.valueSec : nil }
            guard let centre = median(coreValues), centre > 0 else { break }
            let recentered = Set(valid.compactMap { observation -> Int? in
                isWithin(
                    observation.valueSec,
                    centre: centre,
                    lower: settings.coreRatioLower,
                    upper: settings.coreRatioUpper
                ) ? observation.inputOffset : nil
            })
            guard !recentered.isEmpty else { break }
            if recentered == coreOffsets { break }
            coreOffsets = recentered
        }

        let coreValues = valid.compactMap { coreOffsets.contains($0.inputOffset) ? $0.valueSec : nil }
        let coreMedian = median(coreValues)
        guard let coreMedian, coreMedian > 0 else {
            // Defensive fail-closed path; it should be unreachable after the
            // valid-observation guard above.
            return StateSupportAnalysis(
                observations: observations.map {
                    .init(sourceIndex: $0.sourceIndex, valueSec: $0.valueSec, classification: .invalid)
                },
                coreMedianSec: nil,
                nRaw: observations.count,
                nValid: 0,
                nSupport: 0,
                nCore: 0,
                ordinaryDeviationCount: 0,
                competingExcursionCount: 0,
                invalidCount: observations.count
            )
        }

        var classes = Array(repeating: StateSupportISIClass.invalid, count: observations.count)
        for observation in valid {
            if coreOffsets.contains(observation.inputOffset) {
                classes[observation.inputOffset] = .core
            } else if isWithin(
                observation.valueSec,
                centre: coreMedian,
                lower: settings.ordinaryDeviationRatioLower,
                upper: settings.ordinaryDeviationRatioUpper
            ) {
                classes[observation.inputOffset] = .ordinaryDeviation
            } else {
                classes[observation.inputOffset] = .competingExcursion
            }
        }

        // An ordinary deviation is tolerated only if it is isolated and both
        // immediate raw ISI neighbours are core. Invalid slots never provide a
        // fake recovery bridge, and edge deviations are trimmed/reviewed rather
        // than accepted without bilateral evidence.
        for observation in valid where classes[observation.inputOffset] == .ordinaryDeviation {
            let leftOffset = observation.inputOffset - 1
            let rightOffset = observation.inputOffset + 1
            let hasBilateralCoreRecovery =
                leftOffset >= 0 && rightOffset < observations.count &&
                classes[leftOffset] == .core && classes[rightOffset] == .core &&
                observations[leftOffset].sourceIndex + 1 == observation.sourceIndex &&
                observation.sourceIndex + 1 == observations[rightOffset].sourceIndex
            if !hasBilateralCoreRecovery {
                classes[observation.inputOffset] = .competingExcursion
            }
        }

        let classified = zip(observations, classes).map { observation, classification in
            StateSupportAnalysis.ClassifiedObservation(
                sourceIndex: observation.sourceIndex,
                valueSec: observation.valueSec,
                classification: classification
            )
        }
        let nCore = classes.filter { $0 == .core }.count
        let ordinary = classes.filter { $0 == .ordinaryDeviation }.count
        let competing = classes.filter { $0 == .competingExcursion }.count
        let invalid = classes.filter { $0 == .invalid }.count
        return StateSupportAnalysis(
            observations: classified,
            coreMedianSec: coreMedian,
            nRaw: observations.count,
            nValid: valid.count,
            nSupport: nCore + ordinary,
            nCore: nCore,
            ordinaryDeviationCount: ordinary,
            competingExcursionCount: competing,
            invalidCount: invalid
        )
    }

    private static func maximumConsensusCore(
        _ observations: [StateSupportValidObservation],
        settings: StateSupportClassifierSettings
    ) -> Set<Int> {
        var bestOffsets = Set<Int>()
        var bestResidual = Double.infinity
        var bestCentre = Double.infinity

        for centreObservation in observations {
            let centre = centreObservation.valueSec
            let members = observations.filter {
                isWithin(
                    $0.valueSec,
                    centre: centre,
                    lower: settings.coreRatioLower,
                    upper: settings.coreRatioUpper
                )
            }
            let offsets = Set(members.map(\.inputOffset))
            let residual = members.reduce(0.0) { partial, observation in
                partial + abs(log(observation.valueSec / centre))
            }
            let shouldReplace =
                offsets.count > bestOffsets.count ||
                (offsets.count == bestOffsets.count && residual < bestResidual - 1e-12) ||
                (offsets.count == bestOffsets.count && abs(residual - bestResidual) <= 1e-12 && centre < bestCentre)
            if shouldReplace {
                bestOffsets = offsets
                bestResidual = residual
                bestCentre = centre
            }
        }
        return bestOffsets
    }

    private static func isWithin(
        _ value: Double,
        centre: Double,
        lower: Double,
        upper: Double
    ) -> Bool {
        value >= centre * lower && value <= centre * upper
    }

    private static func median(_ values: [Double]) -> Double? {
        guard !values.isEmpty else { return nil }
        let sorted = values.sorted()
        let middle = sorted.count / 2
        if sorted.count.isMultiple(of: 2) {
            return (sorted[middle - 1] + sorted[middle]) / 2
        }
        return sorted[middle]
    }
}
