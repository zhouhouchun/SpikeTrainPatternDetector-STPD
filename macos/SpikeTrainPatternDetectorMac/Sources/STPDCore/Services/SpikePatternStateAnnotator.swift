import Foundation

/// A coarse display grouping of detector pattern labels for color / legend / summary overlays.
///
/// This is an INTERPRETATION / ANNOTATION layer only. It is derived from existing detector annotations
/// (auto or reviewed-final) and is never fed into detection, the `NeuralPopulationMatrix`, or PCA — PCA
/// coordinates and the population matrix are unchanged. Both the ISI State Space per-point coloring and
/// the Neural Manifold per-bin aggregation map labels through this enum.
public enum SpikePatternState: String, CaseIterable, Sendable, Hashable {
    case burst        // burst / long_burst / high_frequency_burst / possible_burst
    case pause
    case tonic
    case hfTonic      // high_frequency_tonic
    case hfs          // high_frequency_spiking
    case unlabeled

    /// Map a detector label to its display state. `reject` / `profile` collapse to `.unlabeled`.
    public init(label: ClassicAnchorLabel) {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst:
            self = .burst
        case .pause:
            self = .pause
        case .tonic:
            self = .tonic
        case .highFrequencyTonic:
            self = .hfTonic
        case .highFrequencySpiking:
            self = .hfs
        case .reject, .profile:
            self = .unlabeled
        }
    }

    /// Stable display + tie-break order (most salient first; `.unlabeled` last).
    public static let displayOrder: [SpikePatternState] = [.burst, .pause, .tonic, .hfTonic, .hfs, .unlabeled]

    public var title: String {
        switch self {
        case .burst: return "Burst"
        case .pause: return "Pause"
        case .tonic: return "Tonic"
        case .hfTonic: return "HF tonic"
        case .hfs: return "HFS"
        case .unlabeled: return "Unlabeled"
        }
    }

    /// Lower = higher priority; used to break overlap ties deterministically.
    var tieBreakRank: Int { SpikePatternState.displayOrder.firstIndex(of: self) ?? Int.max }
}

/// A minimal labeled time interval — the testable input for pattern annotation, decoupled from the full
/// `ClassicAnchorEventAnnotation`. Carries both raw and aligned bounds so callers pick the time base.
public struct SpikePatternInterval: Sendable, Hashable {
    public let trainID: String
    public let rawStartSec: Double
    public let rawEndSec: Double
    public let alignedStartSec: Double
    public let alignedEndSec: Double
    public let state: SpikePatternState

    public init(
        trainID: String,
        rawStartSec: Double,
        rawEndSec: Double,
        alignedStartSec: Double,
        alignedEndSec: Double,
        state: SpikePatternState
    ) {
        self.trainID = trainID
        self.rawStartSec = rawStartSec
        self.rawEndSec = rawEndSec
        self.alignedStartSec = alignedStartSec
        self.alignedEndSec = alignedEndSec
        self.state = state
    }

    /// From a detector annotation (maps the label to a display state).
    public init(annotation: ClassicAnchorEventAnnotation) {
        self.init(
            trainID: annotation.trainID,
            rawStartSec: annotation.rawStartSec,
            rawEndSec: annotation.rawEndSec,
            alignedStartSec: annotation.alignedStartSec,
            alignedEndSec: annotation.alignedEndSec,
            state: SpikePatternState(label: annotation.label)
        )
    }

    func bounds(useAligned: Bool) -> (start: Double, end: Double) {
        useAligned ? (alignedStartSec, alignedEndSec) : (rawStartSec, rawEndSec)
    }
}

/// Per-train labeled intervals, for POINT queries (e.g. coloring one ISI by the interval covering its
/// time). `.unlabeled` and non-finite / inverted intervals are skipped. Pure: no view / detection coupling.
public struct SpikePatternIntervals: Sendable {
    private struct Span: Sendable { let start: Double; let end: Double; let state: SpikePatternState }
    private let byTrain: [String: [Span]]

    public init(_ intervals: [SpikePatternInterval], useAligned: Bool) {
        var grouped: [String: [Span]] = [:]
        for interval in intervals where interval.state != .unlabeled {
            let (start, end) = interval.bounds(useAligned: useAligned)
            guard start.isFinite, end.isFinite, end >= start else { continue }
            grouped[interval.trainID, default: []].append(Span(start: start, end: end, state: interval.state))
        }
        for key in grouped.keys { grouped[key]?.sort { $0.start < $1.start } }
        byTrain = grouped
    }

    public init(annotations: [ClassicAnchorEventAnnotation], useAligned: Bool) {
        self.init(annotations.map(SpikePatternInterval.init(annotation:)), useAligned: useAligned)
    }

    public var isEmpty: Bool { byTrain.isEmpty }

    /// The pattern state of the interval covering `sec` on `trainID`, or `.unlabeled`. When several
    /// intervals contain the point, the highest-priority state (by `displayOrder`) wins (deterministic).
    public func state(forTrainID trainID: String, atSec sec: Double) -> SpikePatternState {
        guard let spans = byTrain[trainID] else { return .unlabeled }
        var best = SpikePatternState.unlabeled
        for span in spans where sec >= span.start && sec <= span.end {
            if span.state.tieBreakRank < best.tieBreakRank { best = span.state }
        }
        return best
    }
}

/// Per-bin pattern summary for the Neural Manifold (dominant state + covered fractions). Annotation layer
/// only — never used as a PCA input.
public struct NeuralPopulationBinPatternSummary: Sendable, Hashable {
    public let binID: Int
    /// The pattern state with the largest covered overlap in the bin (`.unlabeled` if none).
    public let dominant: SpikePatternState
    /// Covered fraction of the bin (0...1) per labeled state; absent states are 0. The remainder
    /// (`1 - labeledFraction`) is unlabeled.
    public let fractions: [SpikePatternState: Double]
    /// Total covered fraction (sum of labeled overlaps / bin width, clamped to 1).
    public let labeledFraction: Double

    public init(
        binID: Int,
        dominant: SpikePatternState,
        fractions: [SpikePatternState: Double],
        labeledFraction: Double
    ) {
        self.binID = binID
        self.dominant = dominant
        self.fractions = fractions
        self.labeledFraction = labeledFraction
    }

    public func fraction(_ state: SpikePatternState) -> Double { fractions[state] ?? 0 }
    public var burstFraction: Double { fraction(.burst) }
    public var pauseFraction: Double { fraction(.pause) }
    public var tonicFraction: Double { fraction(.tonic) }
    public var hfTonicFraction: Double { fraction(.hfTonic) }
    public var hfsFraction: Double { fraction(.hfs) }
}

/// Pure per-bin pattern aggregation for the Neural Manifold. For each bin, accumulates the time overlap of
/// every selected-train interval (raw or aligned), then reports the dominant state (largest overlap,
/// deterministic tie-break) and the covered fraction per state. PCA scores and the population matrix are
/// untouched: this only annotates the existing embedding.
public enum NeuralPopulationPatternStateAnnotator {
    public static func annotate(
        bins: [NeuralPopulationBin],
        selectedTrainIDs: Set<String>,
        useAligned: Bool,
        intervals: [SpikePatternInterval]
    ) -> [NeuralPopulationBinPatternSummary] {
        let spans: [(start: Double, end: Double, state: SpikePatternState)] = intervals.compactMap { interval in
            guard selectedTrainIDs.contains(interval.trainID), interval.state != .unlabeled else { return nil }
            let (start, end) = interval.bounds(useAligned: useAligned)
            guard start.isFinite, end.isFinite, end > start else { return nil }
            return (start, end, interval.state)
        }

        return bins.map { bin in
            let width = max(0, bin.endSec - bin.startSec)
            var overlapByState: [SpikePatternState: Double] = [:]
            for span in spans {
                let overlap = Swift.min(bin.endSec, span.end) - Swift.max(bin.startSec, span.start)
                if overlap > 0 { overlapByState[span.state, default: 0] += overlap }
            }

            var fractions: [SpikePatternState: Double] = [:]
            var labeledOverlap = 0.0
            for (state, overlap) in overlapByState {
                fractions[state] = width > 0 ? Swift.min(1.0, overlap / width) : 0
                labeledOverlap += overlap
            }
            let labeledFraction = width > 0 ? Swift.min(1.0, labeledOverlap / width) : 0

            // Dominant = largest overlap; iterating displayOrder with strict `>` makes the
            // earliest-in-priority state win ties.
            var dominant = SpikePatternState.unlabeled
            var bestOverlap = 0.0
            for state in SpikePatternState.displayOrder {
                let overlap = overlapByState[state] ?? 0
                if overlap > bestOverlap {
                    bestOverlap = overlap
                    dominant = state
                }
            }

            return NeuralPopulationBinPatternSummary(
                binID: bin.binID,
                dominant: dominant,
                fractions: fractions,
                labeledFraction: labeledFraction
            )
        }
    }

    /// Convenience for the views: aggregate directly from detector annotations + a `NeuralPopulationTimeOrigin`.
    public static func annotate(
        bins: [NeuralPopulationBin],
        selectedTrainIDs: Set<String>,
        timeOrigin: NeuralPopulationTimeOrigin,
        annotations: [ClassicAnchorEventAnnotation]
    ) -> [NeuralPopulationBinPatternSummary] {
        annotate(
            bins: bins,
            selectedTrainIDs: selectedTrainIDs,
            useAligned: timeOrigin == .aligned,
            intervals: annotations.map(SpikePatternInterval.init(annotation:))
        )
    }
}
