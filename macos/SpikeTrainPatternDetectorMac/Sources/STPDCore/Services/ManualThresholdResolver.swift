import Foundation

/// Resolves an optional user `ManualThresholdProfile` against a per-train `AdaptiveThresholdInput`
/// into a `ResolvedThresholdProfile`, applying the audit-decided semantics:
///
/// - `automatic` (or inactive): adaptive value unchanged.
/// - `hardGate` (ISI): CONSTRAIN by default — a lower bound can only rise (`max`), and most upper
///   bounds can only fall (`min`). PARAM-2 exception: burst seed/bridge uppers are explicit user
///   admission ceilings and replace the adaptive upper verbatim, so they can recover moderate-ISI
///   burst clusters while still constraining overly large ISIs when set lower. If a hard pair is
///   incompatible (lower would exceed upper) the family falls back to the adaptive band and records
///   `incompatible_user_bound` rather than silently widening biology or emitting an empty band.
/// - `softAnchor` (ISI): UNION/expand — a lower bound may fall (`min`), an upper bound may rise
///   (`max`). A soft anchor only shifts the band; it never forces a candidate to exist.
/// - Spike counts: `hardGate` replaces the effective count; `softAnchor` is treated as `automatic`
///   (a soft count would imply forcing/relaxing candidate creation, which Phase 1 forbids).
///
/// Physiological safeguard: the HF-tonic floor is not allowed to silently fall below the (effective)
/// burst seed upper. In pure-automatic the adaptive floor is preserved exactly; when a manual change
/// pushes the floor below the burst seed upper it is clamped up (with provenance), unless the user
/// hard-gated the floor (an explicit choice), which is respected and recorded.
public enum ManualThresholdResolver {
    public static func resolve(
        profile: ManualThresholdProfile,
        adaptive: AdaptiveThresholdInput
    ) -> ResolvedThresholdProfile {
        var provenance: [ResolvedThreshold] = []

        // Burst (reuses the existing burst hard-threshold route in the detector via these bounds).
        let burstLower = resolveISILower("burst.seed_lower_sec", profile.burst.seedLowerISI, adaptive.burstSeedLowerSec, &provenance)
        let burstUpper = resolveISIUpper("burst.seed_upper_sec", profile.burst.seedUpperISI, adaptive.burstSeedUpperSec, &provenance, hardGateReplaces: true)
        let (burstLowerEff, burstUpperEff) = reconcile("burst", burstLower.value, burstUpper.value, adaptive.burstSeedLowerSec, adaptive.burstSeedUpperSec, &provenance)
        let burstBridge = resolveISIUpper("burst.bridge_upper_sec", profile.burst.bridgeUpperISI, adaptive.burstBridgeUpperSec, &provenance, hardGateReplaces: true)
        let burstMinSpikes = resolveSpikeCount("burst.min_spikes", profile.burst.minSpikes, adaptive.burstMinSpikes, &provenance)
        let burstClassicMax = resolveSpikeCount("burst.classic_max_spikes", profile.burst.classicMaxSpikes, adaptive.burstClassicMaxSpikes, &provenance)
        let burstLongMin = resolveSpikeCount("burst.long_min_spikes", profile.burst.longMinSpikes, adaptive.burstLongMinSpikes, &provenance)
        let burstLongMax = resolveSpikeCount("burst.long_max_spikes", profile.burst.longMaxSpikes, adaptive.burstLongMaxSpikes, &provenance)

        // HFS (min spikes / min duration only — no ISI band gate in Phase 1).
        let hfsMinSpikes = resolveSpikeCount("hfs.min_spikes", profile.hfs.minSpikes, adaptive.hfsMinSpikes, &provenance)
        let hfsMinDuration = resolveISILower("hfs.min_duration_sec", profile.hfs.minDurationSec, adaptive.hfsMinDurationSec, &provenance)

        // HF tonic.
        let hfTonicFloorRaw = resolveISILower("hf_tonic.isi_floor_sec", profile.hfTonic.isiFloor, adaptive.hfTonicFloorSec, &provenance)
        let hfTonicUpper = resolveISIUpper("hf_tonic.isi_upper_sec", profile.hfTonic.isiUpper, adaptive.hfTonicUpperSec, &provenance)
        let hfTonicFloorEff = enforceHFTonicFloorSafeguard(
            floor: hfTonicFloorRaw,
            floorThreshold: profile.hfTonic.isiFloor,
            burstSeedUpperActive: profile.burst.seedUpperISI.isActive,
            adaptiveFloor: adaptive.hfTonicFloorSec,
            burstSeedUpperEff: burstUpperEff,
            provenance: &provenance
        )
        let (hfTonicLowerEff, hfTonicUpperEff) = reconcile("hf_tonic", hfTonicFloorEff, hfTonicUpper.value, adaptive.hfTonicFloorSec, adaptive.hfTonicUpperSec, &provenance)
        let hfTonicMinSpikes = resolveSpikeCount("hf_tonic.min_spikes", profile.hfTonic.minSpikes, adaptive.hfTonicMinSpikes, &provenance)

        // Tonic.
        let tonicLower = resolveISILower("tonic.isi_lower_sec", profile.tonic.isiLower, adaptive.tonicLowerSec, &provenance)
        let tonicUpper = resolveISIUpper("tonic.isi_upper_sec", profile.tonic.isiUpper, adaptive.tonicUpperSec, &provenance)
        let (tonicLowerEff, tonicUpperEff) = reconcile("tonic", tonicLower.value, tonicUpper.value, adaptive.tonicLowerSec, adaptive.tonicUpperSec, &provenance)
        let tonicMinSpikes = resolveSpikeCount("tonic.min_spikes", profile.tonic.minSpikes, adaptive.tonicMinSpikes, &provenance)

        // Pause (lower only in Phase 1).
        let pauseLower = resolveISILower("pause.isi_lower_sec", profile.pause.isiLower, adaptive.pauseLowerSec, &provenance)

        return ResolvedThresholdProfile(
            burst: ResolvedFamilyThresholds(
                lowerSec: burstLowerEff,
                upperSec: burstUpperEff,
                bridgeUpperSec: max(burstUpperEff, burstBridge.value),
                minSpikes: burstMinSpikes.value,
                classicMaxSpikes: burstClassicMax.value,
                longMinSpikes: burstLongMin.value,
                longMaxSpikes: burstLongMax.value
            ),
            // HFS has no ISI band gate in Phase 1: only minSpikes and minDurationSec are meaningful.
            // lowerSec/upperSec are placeholders and MUST NOT be read as an ISI band by detector wiring.
            hfs: ResolvedFamilyThresholds(
                lowerSec: adaptive.hfsMinDurationSec,
                upperSec: adaptive.hfsMinDurationSec,
                minSpikes: hfsMinSpikes.value,
                minDurationSec: hfsMinDuration.value
            ),
            hfTonic: ResolvedFamilyThresholds(
                lowerSec: hfTonicLowerEff,
                upperSec: hfTonicUpperEff,
                minSpikes: hfTonicMinSpikes.value
            ),
            tonic: ResolvedFamilyThresholds(
                lowerSec: tonicLowerEff,
                upperSec: tonicUpperEff,
                minSpikes: tonicMinSpikes.value
            ),
            pause: ResolvedFamilyThresholds(
                lowerSec: pauseLower.value,
                upperSec: pauseLower.value
            ),
            provenance: provenance
        )
    }

    // MARK: - ISI resolution

    private struct ResolvedScalar { let value: Double; let source: ResolvedThresholdSource }

    private static func resolveISILower(
        _ key: String,
        _ threshold: ManualISIThreshold,
        _ adaptive: Double,
        _ provenance: inout [ResolvedThreshold]
    ) -> ResolvedScalar {
        guard threshold.isActive, let user = threshold.valueSec else {
            provenance.append(.init(key: key, mode: .automatic, source: .adaptive, adaptiveValue: adaptive, userValue: threshold.valueSec, effectiveValue: adaptive))
            return ResolvedScalar(value: adaptive, source: .adaptive)
        }
        let effective: Double
        let source: ResolvedThresholdSource
        switch threshold.mode {
        case .hardGate:
            effective = max(adaptive, user)   // narrow-only: floor can only rise
            source = .userHardGate
        case .softAnchor:
            effective = min(adaptive, user)   // union: floor may fall (expand)
            source = .userSoftAnchor
        case .automatic:
            effective = adaptive
            source = .adaptive
        }
        provenance.append(.init(key: key, mode: threshold.mode, source: source, adaptiveValue: adaptive, userValue: user, effectiveValue: effective))
        return ResolvedScalar(value: effective, source: source)
    }

    /// `hardGateReplaces`: PARAM-2. For the burst seed/bridge upper, a hard gate is an explicit
    /// admission ceiling the user controls directly — it must use the user value verbatim (which can
    /// RAISE the ceiling above a smaller adaptive band so a legitimate moderate-ISI cluster becomes
    /// eligible, and lower it below a larger one to constrain). For every other family the hard upper
    /// stays narrow-only (`min`), so this defaults to `false` and those thresholds are unchanged.
    private static func resolveISIUpper(
        _ key: String,
        _ threshold: ManualISIThreshold,
        _ adaptive: Double,
        _ provenance: inout [ResolvedThreshold],
        hardGateReplaces: Bool = false
    ) -> ResolvedScalar {
        guard threshold.isActive, let user = threshold.valueSec else {
            provenance.append(.init(key: key, mode: .automatic, source: .adaptive, adaptiveValue: adaptive, userValue: threshold.valueSec, effectiveValue: adaptive))
            return ResolvedScalar(value: adaptive, source: .adaptive)
        }
        let effective: Double
        let source: ResolvedThresholdSource
        switch threshold.mode {
        case .hardGate:
            // narrow-only (`min`) by default; the burst seed/bridge upper REPLACES with the user value
            // so the hard seed max is never silently narrowed by a smaller adaptive burst upper.
            effective = hardGateReplaces ? user : min(adaptive, user)
            source = .userHardGate
        case .softAnchor:
            effective = max(adaptive, user)   // union: ceiling may rise (expand)
            source = .userSoftAnchor
        case .automatic:
            effective = adaptive
            source = .adaptive
        }
        provenance.append(.init(key: key, mode: threshold.mode, source: source, adaptiveValue: adaptive, userValue: user, effectiveValue: effective))
        return ResolvedScalar(value: effective, source: source)
    }

    /// If a hard constrain inverts the band (lower > upper), fail safely to the adaptive band and
    /// record provenance rather than producing an empty band or silently widening.
    private static func reconcile(
        _ family: String,
        _ lowerValue: Double,
        _ upperValue: Double,
        _ adaptiveLower: Double,
        _ adaptiveUpper: Double,
        _ provenance: inout [ResolvedThreshold]
    ) -> (Double, Double) {
        if lowerValue > upperValue {
            provenance.append(.init(
                key: "\(family).isi_band",
                mode: .hardGate,
                source: .userHardGate,
                adaptiveValue: adaptiveLower,
                userValue: lowerValue,
                effectiveValue: adaptiveLower,
                note: "incompatible_user_bound_fell_back_to_adaptive_band"
            ))
            return (adaptiveLower, max(adaptiveLower, adaptiveUpper))
        }
        return (lowerValue, max(lowerValue, upperValue))
    }

    private static func enforceHFTonicFloorSafeguard(
        floor: ResolvedScalar,
        floorThreshold: ManualISIThreshold,
        burstSeedUpperActive: Bool,
        adaptiveFloor: Double,
        burstSeedUpperEff: Double,
        provenance: inout [ResolvedThreshold]
    ) -> Double {
        guard floor.value < burstSeedUpperEff else {
            return floor.value
        }
        // The safeguard is recorded under a DISTINCT key so it never conflicts with the floor's own
        // primary resolution record, and each record keeps a consistent (mode, source) pair.
        if floorThreshold.mode == .hardGate {
            // The user explicitly hard-gated the floor below the burst seed upper: respect it.
            provenance.append(.init(
                key: "hf_tonic.isi_floor_safeguard",
                mode: .hardGate,
                source: .userHardGate,
                adaptiveValue: adaptiveFloor,
                userValue: floorThreshold.valueSec,
                effectiveValue: floor.value,
                note: "hf_tonic_floor_below_burst_seed_upper_user_explicit"
            ))
            return floor.value
        }
        if floorThreshold.isActive {
            // A SOFT floor anchor pushed the floor below the burst seed upper: clamp up.
            provenance.append(.init(
                key: "hf_tonic.isi_floor_safeguard",
                mode: .softAnchor,
                source: .userSoftAnchor,
                adaptiveValue: adaptiveFloor,
                userValue: floorThreshold.valueSec,
                effectiveValue: burstSeedUpperEff,
                note: "hf_tonic_floor_clamped_to_burst_seed_upper_via_soft_floor"
            ))
            return burstSeedUpperEff
        }
        if burstSeedUpperActive {
            // A manual burst-seed-upper change raised it above the automatic floor: clamp the floor up.
            // Attributed to the burst gate (the actual cause), not to a floor anchor.
            provenance.append(.init(
                key: "hf_tonic.isi_floor_safeguard",
                mode: .hardGate,
                source: .userHardGate,
                adaptiveValue: adaptiveFloor,
                userValue: nil,
                effectiveValue: burstSeedUpperEff,
                note: "hf_tonic_floor_clamped_to_burst_seed_upper_via_burst_seed_upper_change"
            ))
            return burstSeedUpperEff
        }
        // Pure adaptive: preserve adaptive behavior exactly (no clamp).
        return floor.value
    }

    // MARK: - Spike-count resolution

    private struct ResolvedCount { let value: Int; let source: ResolvedThresholdSource }

    private static func resolveSpikeCount(
        _ key: String,
        _ threshold: ManualSpikeCountThreshold,
        _ adaptive: Int,
        _ provenance: inout [ResolvedThreshold]
    ) -> ResolvedCount {
        guard threshold.isHardGate, let user = threshold.value else {
            provenance.append(.init(key: key, mode: .automatic, source: .adaptive, adaptiveValue: Double(adaptive), userValue: threshold.value.map(Double.init), effectiveValue: Double(adaptive)))
            return ResolvedCount(value: adaptive, source: .adaptive)
        }
        provenance.append(.init(key: key, mode: .hardGate, source: .userHardGate, adaptiveValue: Double(adaptive), userValue: Double(user), effectiveValue: Double(user)))
        return ResolvedCount(value: user, source: .userHardGate)
    }
}
