import Foundation

/// The per-train adaptive baseline the resolver constrains. In the pipeline this is built from the
/// train's resolved adaptive bands + detector settings (after leave-one-out band resolution). It is a
/// plain value type so the resolver is fully unit-testable without the pipeline.
public struct AdaptiveThresholdInput: Hashable, Sendable {
    public var burstSeedLowerSec: Double
    public var burstSeedUpperSec: Double
    public var burstBridgeUpperSec: Double
    public var burstMinSpikes: Int
    public var burstClassicMaxSpikes: Int
    public var burstLongMinSpikes: Int
    public var burstLongMaxSpikes: Int
    public var hfsMinSpikes: Int
    public var hfsMinDurationSec: Double
    public var hfTonicFloorSec: Double
    public var hfTonicUpperSec: Double
    public var hfTonicMinSpikes: Int
    public var tonicLowerSec: Double
    public var tonicUpperSec: Double
    public var tonicMinSpikes: Int
    public var pauseLowerSec: Double

    public init(
        burstSeedLowerSec: Double,
        burstSeedUpperSec: Double,
        burstBridgeUpperSec: Double,
        burstMinSpikes: Int,
        burstClassicMaxSpikes: Int,
        burstLongMinSpikes: Int,
        burstLongMaxSpikes: Int,
        hfsMinSpikes: Int,
        hfsMinDurationSec: Double,
        hfTonicFloorSec: Double,
        hfTonicUpperSec: Double,
        hfTonicMinSpikes: Int,
        tonicLowerSec: Double,
        tonicUpperSec: Double,
        tonicMinSpikes: Int,
        pauseLowerSec: Double
    ) {
        self.burstSeedLowerSec = burstSeedLowerSec
        self.burstSeedUpperSec = burstSeedUpperSec
        self.burstBridgeUpperSec = burstBridgeUpperSec
        self.burstMinSpikes = burstMinSpikes
        self.burstClassicMaxSpikes = burstClassicMaxSpikes
        self.burstLongMinSpikes = burstLongMinSpikes
        self.burstLongMaxSpikes = burstLongMaxSpikes
        self.hfsMinSpikes = hfsMinSpikes
        self.hfsMinDurationSec = hfsMinDurationSec
        self.hfTonicFloorSec = hfTonicFloorSec
        self.hfTonicUpperSec = hfTonicUpperSec
        self.hfTonicMinSpikes = hfTonicMinSpikes
        self.tonicLowerSec = tonicLowerSec
        self.tonicUpperSec = tonicUpperSec
        self.tonicMinSpikes = tonicMinSpikes
        self.pauseLowerSec = pauseLowerSec
    }
}

/// A single resolved threshold's audit record.
public struct ResolvedThreshold: Hashable, Sendable {
    public let key: String                 // e.g. "tonic.isi_lower_sec", "burst.min_spikes"
    public let mode: ThresholdMode
    public let source: ResolvedThresholdSource
    public let adaptiveValue: Double?
    public let userValue: Double?
    public let effectiveValue: Double?
    public let note: String?

    public init(
        key: String,
        mode: ThresholdMode,
        source: ResolvedThresholdSource,
        adaptiveValue: Double?,
        userValue: Double?,
        effectiveValue: Double?,
        note: String? = nil
    ) {
        self.key = key
        self.mode = mode
        self.source = source
        self.adaptiveValue = adaptiveValue
        self.userValue = userValue
        self.effectiveValue = effectiveValue
        self.note = note
    }

    /// Whether this resolution actually came from a user manual gate/anchor (not pure adaptive).
    public var isManual: Bool { source != .adaptive }
}

public struct ResolvedISIBand: Hashable, Sendable {
    public let lowerSec: Double
    public let upperSec: Double
    public let bridgeUpperSec: Double?

    public init(lowerSec: Double, upperSec: Double, bridgeUpperSec: Double?) {
        self.lowerSec = lowerSec
        self.upperSec = upperSec
        self.bridgeUpperSec = bridgeUpperSec
    }
}

public struct ResolvedFamilyThresholds: Hashable, Sendable {
    public let lowerSec: Double
    public let upperSec: Double
    public let bridgeUpperSec: Double?
    public let minSpikes: Int?
    public let classicMaxSpikes: Int?      // burst size labels only
    public let longMinSpikes: Int?
    public let longMaxSpikes: Int?
    public let minDurationSec: Double?      // HFS only

    public init(
        lowerSec: Double,
        upperSec: Double,
        bridgeUpperSec: Double? = nil,
        minSpikes: Int? = nil,
        classicMaxSpikes: Int? = nil,
        longMinSpikes: Int? = nil,
        longMaxSpikes: Int? = nil,
        minDurationSec: Double? = nil
    ) {
        self.lowerSec = lowerSec
        self.upperSec = upperSec
        self.bridgeUpperSec = bridgeUpperSec
        self.minSpikes = minSpikes
        self.classicMaxSpikes = classicMaxSpikes
        self.longMinSpikes = longMinSpikes
        self.longMaxSpikes = longMaxSpikes
        self.minDurationSec = minDurationSec
    }
}

/// The per-train resolved threshold profile: effective bounds per family plus full provenance.
public struct ResolvedThresholdProfile: Hashable, Sendable {
    public let burst: ResolvedFamilyThresholds
    public let hfs: ResolvedFamilyThresholds
    public let hfTonic: ResolvedFamilyThresholds
    public let tonic: ResolvedFamilyThresholds
    public let pause: ResolvedFamilyThresholds
    public let provenance: [ResolvedThreshold]
    /// Phase 1D: learned-provenance notes carried through from the `ManualThresholdProfile`, keyed by
    /// `<family>.<field>`. Additive — used only to tag candidate decisionPaths; not part of resolving.
    public var learnedProvenanceByKey: [String: String] = [:]

    public init(
        burst: ResolvedFamilyThresholds,
        hfs: ResolvedFamilyThresholds,
        hfTonic: ResolvedFamilyThresholds,
        tonic: ResolvedFamilyThresholds,
        pause: ResolvedFamilyThresholds,
        provenance: [ResolvedThreshold]
    ) {
        self.burst = burst
        self.hfs = hfs
        self.hfTonic = hfTonic
        self.tonic = tonic
        self.pause = pause
        self.provenance = provenance
    }

    /// True when at least one manual gate/anchor changed a resolved value.
    public var appliedManualThreshold: Bool { provenance.contains { $0.isManual } }

    /// Audit keys suitable for a decision path / CSV: only the manual (non-adaptive) resolutions.
    public func manualProvenanceKeys() -> [String] {
        provenance.filter(\.isManual).map { resolved in
            var key = "resolved_threshold[\(resolved.key)]=\(resolved.source.rawValue)"
            if let note = resolved.note {
                key += ":\(note)"
            }
            return key
        }
    }
}
