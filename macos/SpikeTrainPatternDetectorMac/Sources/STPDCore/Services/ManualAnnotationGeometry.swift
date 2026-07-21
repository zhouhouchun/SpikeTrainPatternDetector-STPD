import Foundation

/// The resolved spike/ISI geometry of a manual annotation against a concrete `SpikeTrain`.
///
/// ISI index convention (matches `SpikeTrain.isiSec` and `ClassicAnchorEventAnnotation`): ISI index
/// `i` (valid for `1..<timestampsSec.count`) is the interval `[timestampsSec[i-1], timestampsSec[i]]`.
/// A covered ISI range `s...e` therefore spans spike indices `(s-1)...e`.
public struct ResolvedManualAnnotationGeometry: Hashable, Sendable {
    /// False when the annotation does not belong to / does not overlap the train (flagged, never a crash).
    public let isWithinTrain: Bool
    public let startSpikeIndex: Int?
    public let endSpikeIndex: Int?
    public let startISIIndex: Int?
    public let endISIIndex: Int?

    public init(
        isWithinTrain: Bool,
        startSpikeIndex: Int?,
        endSpikeIndex: Int?,
        startISIIndex: Int?,
        endISIIndex: Int?
    ) {
        self.isWithinTrain = isWithinTrain
        self.startSpikeIndex = startSpikeIndex
        self.endSpikeIndex = endSpikeIndex
        self.startISIIndex = startISIIndex
        self.endISIIndex = endISIIndex
    }

    public var coveredISIIndices: ClosedRange<Int>? {
        guard let start = startISIIndex, let end = endISIIndex, start <= end else { return nil }
        return start...end
    }

    public var coveredSpikeIndices: ClosedRange<Int>? {
        guard let start = startSpikeIndex, let end = endSpikeIndex, start <= end else { return nil }
        return start...end
    }

    static let outside = ResolvedManualAnnotationGeometry(
        isWithinTrain: false,
        startSpikeIndex: nil,
        endSpikeIndex: nil,
        startISIIndex: nil,
        endISIIndex: nil
    )
}

/// How a manual annotation's time window is mapped to covered ISIs.
///
/// - `containedThenOverlapFallback`: prefer ISIs fully inside the window; if none, fall back to ISIs
///   the window partially overlaps (then to a spike-only span). Usable for broad/slow labels where a
///   small partial drag may reasonably indicate a wider interval (tonic, pause, other).
/// - `fullyContainedISIOnly`: only ISIs fully inside the window count; if none are fully contained the
///   annotation resolves to nothing (incompatible). Required for fast-pattern labels (burst family,
///   HF families) where exact short-ISI structure matters and partial overlap is too ambiguous.
public enum ManualAnnotationGeometryResolutionPolicy: Hashable, Sendable {
    case containedThenOverlapFallback
    case fullyContainedISIOnly
}

public extension ManualAnnotationLabel {
    /// Fast-pattern labels (burst family, HF families, and the burst-family veto) require full ISI
    /// containment; broad/slow labels keep the overlap fallback.
    var geometryResolutionPolicy: ManualAnnotationGeometryResolutionPolicy {
        switch self {
        case .burst, .longBurst, .notBurst, .highFrequencyTonic, .highFrequencySpiking:
            return .fullyContainedISIOnly
        case .tonic, .pause, .other:
            return .containedThenOverlapFallback
        }
    }
}

/// Maps a manual annotation's authoritative time range to spike/ISI indices on a single train.
/// Reversed ranges are normalized; out-of-train annotations are flagged, not fatal. Resolution is
/// label-aware: fast-pattern labels use full ISI containment only (no overlap fallback).
public enum ManualAnnotationGeometryResolver {
    private static let toleranceSec = 1e-9

    public static func resolve(annotation: ManualAnnotation, in train: SpikeTrain) -> ResolvedManualAnnotationGeometry {
        guard annotation.trainID == train.id else {
            return .outside
        }
        return resolve(
            startSec: annotation.startSec,
            endSec: annotation.endSec,
            in: train,
            policy: annotation.label.geometryResolutionPolicy
        )
    }

    /// Default (slow-label) resolution, preserved for tests and any non-label call site.
    public static func resolve(startSec: Double, endSec: Double, in train: SpikeTrain) -> ResolvedManualAnnotationGeometry {
        resolve(startSec: startSec, endSec: endSec, in: train, policy: .containedThenOverlapFallback)
    }

    public static func resolve(
        startSec: Double,
        endSec: Double,
        in train: SpikeTrain,
        policy: ManualAnnotationGeometryResolutionPolicy
    ) -> ResolvedManualAnnotationGeometry {
        // Guard the RAW bounds before min/max: `Swift.min/max(finite, .nan)` returns the finite value,
        // so checking the post-min/max result would let a non-finite bound slip through depending on
        // argument order. Guarding the inputs makes the outside-flag deterministic for any NaN/inf.
        let timestamps = train.timestampsSec
        guard startSec.isFinite, endSec.isFinite, !timestamps.isEmpty else {
            return .outside
        }
        let lower = Swift.min(startSec, endSec)
        let upper = Swift.max(startSec, endSec)

        // Primary: ISIs fully contained in [lower, upper] (both spike endpoints inside the window) —
        // this matches the auto-candidate convention (spikes inside -> the ISIs strictly between them).
        if timestamps.count >= 2 {
            var firstISI: Int?
            var lastISI: Int?
            for index in 1..<timestamps.count {
                let intervalStart = timestamps[index - 1]
                let intervalEnd = timestamps[index]
                if intervalStart >= lower - toleranceSec && intervalEnd <= upper + toleranceSec {
                    if firstISI == nil { firstISI = index }
                    lastISI = index
                }
            }
            if let start = firstISI, let end = lastISI {
                return ResolvedManualAnnotationGeometry(
                    isWithinTrain: true,
                    startSpikeIndex: start - 1,
                    endSpikeIndex: end,
                    startISIIndex: start,
                    endISIIndex: end
                )
            }
        }

        // Fast-pattern labels require full containment: no overlap / spike-only fallback. If no ISI is
        // fully contained, the mark is treated as incompatible (no covered ISI).
        if policy == .fullyContainedISIOnly {
            return .outside
        }

        // Overlap fallback (slow labels): a window sitting within / partially overlapping ISIs resolves
        // to the ISI(s) it overlaps.
        if timestamps.count >= 2 {
            var firstISI: Int?
            var lastISI: Int?
            for index in 1..<timestamps.count {
                let intervalStart = timestamps[index - 1]
                let intervalEnd = timestamps[index]
                if intervalEnd >= lower - toleranceSec && intervalStart <= upper + toleranceSec {
                    if firstISI == nil { firstISI = index }
                    lastISI = index
                }
            }
            if let start = firstISI, let end = lastISI {
                return ResolvedManualAnnotationGeometry(
                    isWithinTrain: true,
                    startSpikeIndex: start - 1,
                    endSpikeIndex: end,
                    startISIIndex: start,
                    endISIIndex: end
                )
            }
        }

        // Spike-only fallback (slow labels: e.g. a single-spike train, or a window over one spike).
        var firstSpike: Int?
        var lastSpike: Int?
        for index in timestamps.indices where timestamps[index] >= lower - toleranceSec && timestamps[index] <= upper + toleranceSec {
            if firstSpike == nil { firstSpike = index }
            lastSpike = index
        }
        if let start = firstSpike, let end = lastSpike {
            return ResolvedManualAnnotationGeometry(
                isWithinTrain: true,
                startSpikeIndex: start,
                endSpikeIndex: end,
                startISIIndex: nil,
                endISIIndex: nil
            )
        }

        return .outside
    }

    /// Returns a copy of the annotation with its cached spike/ISI indices recomputed from time
    /// against the given train. If the cached indices disagree with the current train, the
    /// recomputed values win (time is authoritative).
    public static func resolvingIndices(_ annotation: ManualAnnotation, in train: SpikeTrain) -> ManualAnnotation {
        let geometry = resolve(annotation: annotation, in: train)
        var resolved = annotation
        resolved.startSpikeIndex = geometry.startSpikeIndex
        resolved.endSpikeIndex = geometry.endSpikeIndex
        resolved.startISIIndex = geometry.startISIIndex
        resolved.endISIIndex = geometry.endISIIndex
        return resolved
    }

    /// Resolve an annotation against a set of trains, returning it with recomputed indices ONLY when
    /// it is COMPATIBLE with that dataset: its `trainID` is present AND its time range falls within
    /// that train. Returns nil for annotations that belong to a missing train or resolve outside the
    /// train — so incompatible (cross-dataset / stale) annotations are never stored, drawn, or
    /// persisted as "ghosts".
    public static func resolvingIndicesIfCompatible(_ annotation: ManualAnnotation, in trains: [SpikeTrain]) -> ManualAnnotation? {
        guard let train = trains.first(where: { $0.id == annotation.trainID }) else {
            return nil
        }
        guard resolve(annotation: annotation, in: train).isWithinTrain else {
            return nil
        }
        return resolvingIndices(annotation, in: train)
    }
}
