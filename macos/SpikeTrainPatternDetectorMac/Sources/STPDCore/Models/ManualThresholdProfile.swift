import Foundation

/// How a single user threshold participates in detection.
///
/// - `automatic`: adaptive behavior is unchanged (the threshold is off / not applied).
/// - `softAnchor`: may bias/expand (union) a resolved band, but must never force a candidate to
///   exist that adaptive logic alone would not have produced.
/// - `hardGate`: constrains the effective band/settings BEFORE candidate generation (not by silent
///   post-filtering). For ISI bounds it is narrow-only: a lower bound can only raise the floor and an
///   upper bound can only lower the ceiling.
public enum ThresholdMode: String, Codable, Hashable, Sendable, CaseIterable {
    case automatic
    case softAnchor = "soft_anchor"
    case hardGate = "hard_gate"
}

/// Provenance of a resolved threshold value, so a candidate's origin is auditable.
public enum ResolvedThresholdSource: String, Codable, Hashable, Sendable {
    case adaptive          // adaptive value used unchanged (automatic mode or inactive manual value)
    case userHardGate = "user_hard_gate"
    case userSoftAnchor = "user_soft_anchor"
}

/// An optional user ISI threshold (seconds) with its mode.
public struct ManualISIThreshold: Hashable, Sendable {
    public var mode: ThresholdMode
    public var valueSec: Double?

    public init(mode: ThresholdMode = .automatic, valueSec: Double? = nil) {
        self.mode = mode
        self.valueSec = valueSec
    }

    /// Active only when a positive finite value is supplied and the mode is not automatic.
    public var isActive: Bool {
        mode != .automatic && (valueSec.map { $0.isFinite && $0 > 0 } ?? false)
    }

    public static let automatic = ManualISIThreshold()
}

/// An optional user spike-count threshold with its mode. Spike counts support only `automatic`
/// (unchanged) and `hardGate` (replace the effective count). A `softAnchor` count would imply
/// forcing/relaxing candidate creation, which Phase 1 forbids, so it is treated as `automatic`.
public struct ManualSpikeCountThreshold: Hashable, Sendable {
    public var mode: ThresholdMode
    public var value: Int?

    public init(mode: ThresholdMode = .automatic, value: Int? = nil) {
        self.mode = mode
        self.value = value
    }

    public var isHardGate: Bool {
        mode == .hardGate && (value.map { $0 > 0 } ?? false)
    }

    public static let automatic = ManualSpikeCountThreshold()
}

// MARK: - Per-family manual thresholds (Phase 1 include-list only)

public struct BurstManualThresholds: Hashable, Sendable {
    public var seedLowerISI: ManualISIThreshold
    public var seedUpperISI: ManualISIThreshold
    public var bridgeUpperISI: ManualISIThreshold
    public var minSpikes: ManualSpikeCountThreshold
    /// Classic/long/prolonged spike-count bands are SIZE-LABEL controls only (they label burst size,
    /// they do not reject candidates), per the audit decision.
    public var classicMaxSpikes: ManualSpikeCountThreshold
    public var longMinSpikes: ManualSpikeCountThreshold
    public var longMaxSpikes: ManualSpikeCountThreshold

    public init(
        seedLowerISI: ManualISIThreshold = .automatic,
        seedUpperISI: ManualISIThreshold = .automatic,
        bridgeUpperISI: ManualISIThreshold = .automatic,
        minSpikes: ManualSpikeCountThreshold = .automatic,
        classicMaxSpikes: ManualSpikeCountThreshold = .automatic,
        longMinSpikes: ManualSpikeCountThreshold = .automatic,
        longMaxSpikes: ManualSpikeCountThreshold = .automatic
    ) {
        self.seedLowerISI = seedLowerISI
        self.seedUpperISI = seedUpperISI
        self.bridgeUpperISI = bridgeUpperISI
        self.minSpikes = minSpikes
        self.classicMaxSpikes = classicMaxSpikes
        self.longMinSpikes = longMinSpikes
        self.longMaxSpikes = longMaxSpikes
    }
}

public struct HFSManualThresholds: Hashable, Sendable {
    public var minSpikes: ManualSpikeCountThreshold
    public var minDurationSec: ManualISIThreshold

    public init(minSpikes: ManualSpikeCountThreshold = .automatic, minDurationSec: ManualISIThreshold = .automatic) {
        self.minSpikes = minSpikes
        self.minDurationSec = minDurationSec
    }
}

public struct HFTonicManualThresholds: Hashable, Sendable {
    public var minSpikes: ManualSpikeCountThreshold
    public var isiFloor: ManualISIThreshold     // lower bound
    public var isiUpper: ManualISIThreshold     // upper bound

    public init(
        minSpikes: ManualSpikeCountThreshold = .automatic,
        isiFloor: ManualISIThreshold = .automatic,
        isiUpper: ManualISIThreshold = .automatic
    ) {
        self.minSpikes = minSpikes
        self.isiFloor = isiFloor
        self.isiUpper = isiUpper
    }
}

public struct TonicManualThresholds: Hashable, Sendable {
    public var minSpikes: ManualSpikeCountThreshold
    public var isiLower: ManualISIThreshold
    public var isiUpper: ManualISIThreshold

    public init(
        minSpikes: ManualSpikeCountThreshold = .automatic,
        isiLower: ManualISIThreshold = .automatic,
        isiUpper: ManualISIThreshold = .automatic
    ) {
        self.minSpikes = minSpikes
        self.isiLower = isiLower
        self.isiUpper = isiUpper
    }
}

public struct PauseManualThresholds: Hashable, Sendable {
    public var isiLower: ManualISIThreshold

    public init(isiLower: ManualISIThreshold = .automatic) {
        self.isiLower = isiLower
    }
}

/// The unified, optional user threshold profile. All fields default to `.automatic`, so the default
/// profile is a no-op that preserves current adaptive behavior exactly.
public struct ManualThresholdProfile: Hashable, Sendable {
    public var burst: BurstManualThresholds
    public var hfs: HFSManualThresholds
    public var hfTonic: HFTonicManualThresholds
    public var tonic: TonicManualThresholds
    public var pause: PauseManualThresholds
    /// Phase 1D: optional learned-provenance notes for fields whose soft-anchor value came from manual
    /// annotations, keyed by `<family>.<field>` (e.g. `burst.seed_upper_sec`). Purely additive metadata —
    /// it carries the provenance note into the detector run so candidates can record where a learned
    /// threshold came from; it never changes any resolved value or `isAllAutomatic`.
    public var learnedProvenanceByKey: [String: String]

    public init(
        burst: BurstManualThresholds = .init(),
        hfs: HFSManualThresholds = .init(),
        hfTonic: HFTonicManualThresholds = .init(),
        tonic: TonicManualThresholds = .init(),
        pause: PauseManualThresholds = .init(),
        learnedProvenanceByKey: [String: String] = [:]
    ) {
        self.burst = burst
        self.hfs = hfs
        self.hfTonic = hfTonic
        self.tonic = tonic
        self.pause = pause
        self.learnedProvenanceByKey = learnedProvenanceByKey
    }

    /// The all-automatic profile: nothing is applied, adaptive behavior is unchanged.
    public static let automatic = ManualThresholdProfile()

    /// True when no field is active — used to short-circuit to the pure-adaptive path.
    public var isAllAutomatic: Bool {
        let isi: [ManualISIThreshold] = [
            burst.seedLowerISI, burst.seedUpperISI, burst.bridgeUpperISI,
            hfs.minDurationSec,
            hfTonic.isiFloor, hfTonic.isiUpper,
            tonic.isiLower, tonic.isiUpper,
            pause.isiLower,
        ]
        let counts: [ManualSpikeCountThreshold] = [
            burst.minSpikes, burst.classicMaxSpikes, burst.longMinSpikes, burst.longMaxSpikes,
            hfs.minSpikes, hfTonic.minSpikes, tonic.minSpikes,
        ]
        return isi.allSatisfy { !$0.isActive } && counts.allSatisfy { $0.mode == .automatic }
    }

    /// P9: the profile as seen by a train OUTSIDE the manual hard-threshold scope (`ManualThresholdScope`). Every HARD
    /// gate is demoted to `.automatic` (ignored for that train); soft anchors, automatic fields, and learned (soft)
    /// provenance are preserved — soft anchors remain global. A profile with no hard gates is returned unchanged, so
    /// scope never affects a soft-only / automatic profile.
    public func droppingHardGates() -> ManualThresholdProfile {
        ManualThresholdProfile(
            burst: BurstManualThresholds(
                seedLowerISI: burst.seedLowerISI.droppingHardGate(),
                seedUpperISI: burst.seedUpperISI.droppingHardGate(),
                bridgeUpperISI: burst.bridgeUpperISI.droppingHardGate(),
                minSpikes: burst.minSpikes.droppingHardGate(),
                classicMaxSpikes: burst.classicMaxSpikes.droppingHardGate(),
                longMinSpikes: burst.longMinSpikes.droppingHardGate(),
                longMaxSpikes: burst.longMaxSpikes.droppingHardGate()),
            hfs: HFSManualThresholds(
                minSpikes: hfs.minSpikes.droppingHardGate(),
                minDurationSec: hfs.minDurationSec.droppingHardGate()),
            hfTonic: HFTonicManualThresholds(
                minSpikes: hfTonic.minSpikes.droppingHardGate(),
                isiFloor: hfTonic.isiFloor.droppingHardGate(),
                isiUpper: hfTonic.isiUpper.droppingHardGate()),
            tonic: TonicManualThresholds(
                minSpikes: tonic.minSpikes.droppingHardGate(),
                isiLower: tonic.isiLower.droppingHardGate(),
                isiUpper: tonic.isiUpper.droppingHardGate()),
            pause: PauseManualThresholds(
                isiLower: pause.isiLower.droppingHardGate()),
            learnedProvenanceByKey: learnedProvenanceByKey)
    }
}

extension ManualISIThreshold {
    /// P9: this ISI threshold as seen outside the manual hard-threshold scope — a hard gate becomes automatic (ignored);
    /// soft anchors and automatic are unchanged.
    public func droppingHardGate() -> ManualISIThreshold {
        mode == .hardGate ? .automatic : self
    }
}

extension ManualSpikeCountThreshold {
    /// P9: this count threshold as seen outside the manual hard-threshold scope — a hard gate becomes automatic
    /// (ignored). Counts have no soft mode, so non-hard values are returned unchanged.
    public func droppingHardGate() -> ManualSpikeCountThreshold {
        mode == .hardGate ? .automatic : self
    }
}
