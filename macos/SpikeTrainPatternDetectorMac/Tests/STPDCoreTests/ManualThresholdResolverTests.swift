import Foundation
@testable import STPDCore
import Testing

private func baselineAdaptive(
    burstSeedUpperSec: Double = 0.010,
    hfTonicFloorSec: Double = 0.010
) -> AdaptiveThresholdInput {
    AdaptiveThresholdInput(
        burstSeedLowerSec: 0.001,
        burstSeedUpperSec: burstSeedUpperSec,
        burstBridgeUpperSec: 0.018,
        burstMinSpikes: 3,
        burstClassicMaxSpikes: 9,
        burstLongMinSpikes: 10,
        burstLongMaxSpikes: 16,
        hfsMinSpikes: 30,
        hfsMinDurationSec: 0.0,
        hfTonicFloorSec: hfTonicFloorSec,
        hfTonicUpperSec: 0.030,
        hfTonicMinSpikes: 6,
        tonicLowerSec: 0.020,
        tonicUpperSec: 0.060,
        tonicMinSpikes: 5,
        pauseLowerSec: 0.100
    )
}

@Test
func resolverAutomaticModePreservesAdaptiveValuesExactly() throws {
    let resolved = ManualThresholdResolver.resolve(profile: .automatic, adaptive: baselineAdaptive())

    #expect(!resolved.appliedManualThreshold)
    #expect(resolved.manualProvenanceKeys().isEmpty)
    #expect(resolved.tonic.lowerSec == 0.020)
    #expect(resolved.tonic.upperSec == 0.060)
    #expect(resolved.tonic.minSpikes == 5)
    #expect(resolved.burst.upperSec == 0.010)
    #expect(resolved.hfTonic.lowerSec == 0.010)
    #expect(resolved.hfTonic.upperSec == 0.030)
    #expect(resolved.pause.lowerSec == 0.100)
    #expect(ManualThresholdProfile.automatic.isAllAutomatic)
}

@Test
func resolverHardISIGateConstrainsBandNarrowOnly() throws {
    var profile = ManualThresholdProfile.automatic
    profile.tonic.isiLower = .init(mode: .hardGate, valueSec: 0.025)   // above adaptive 0.020 -> raises
    profile.tonic.isiUpper = .init(mode: .hardGate, valueSec: 0.045)   // below adaptive 0.060 -> lowers
    let resolved = ManualThresholdResolver.resolve(profile: profile, adaptive: baselineAdaptive())

    #expect(resolved.tonic.lowerSec == 0.025)   // max(adaptive 0.020, user 0.025)
    #expect(resolved.tonic.upperSec == 0.045)   // min(adaptive 0.060, user 0.045)
    #expect(resolved.appliedManualThreshold)
    #expect(resolved.manualProvenanceKeys().contains { $0.contains("tonic.isi_lower_sec") && $0.contains("user_hard_gate") })
    #expect(resolved.manualProvenanceKeys().contains { $0.contains("tonic.isi_upper_sec") && $0.contains("user_hard_gate") })
}

@Test
func resolverBurstHardUpperReplacesAndCanWidenPastAdaptive() throws {
    // PARAM-2: the burst seed/bridge upper is a user-controlled admission ceiling. A hard gate uses the
    // user value verbatim (replace), so it can RAISE the ceiling above a smaller adaptive band — the hard
    // seed max is never silently narrowed (which would exclude legitimate moderate-ISI burst clusters).
    var widening = ManualThresholdProfile.automatic
    widening.burst.seedUpperISI = .init(mode: .hardGate, valueSec: 0.020)   // above adaptive 0.010 -> widens
    let widened = ManualThresholdResolver.resolve(profile: widening, adaptive: baselineAdaptive())
    #expect(widened.burst.upperSec == 0.020)   // user value (replace), NOT min(0.010, 0.020)
    #expect(widened.manualProvenanceKeys().contains { $0.contains("burst.seed_upper_sec") && $0.contains("user_hard_gate") })

    // A hard upper BELOW the adaptive band still constrains (replace with the smaller user value).
    var lowering = ManualThresholdProfile.automatic
    lowering.burst.seedUpperISI = .init(mode: .hardGate, valueSec: 0.006)   // below adaptive 0.010 -> lowers
    #expect(ManualThresholdResolver.resolve(profile: lowering, adaptive: baselineAdaptive()).burst.upperSec == 0.006)

    // Other families are unchanged: the tonic hard upper stays narrow-only (cannot widen past adaptive).
    var tonicWide = ManualThresholdProfile.automatic
    tonicWide.tonic.isiUpper = .init(mode: .hardGate, valueSec: 0.090)   // above adaptive 0.060
    #expect(ManualThresholdResolver.resolve(profile: tonicWide, adaptive: baselineAdaptive()).tonic.upperSec == 0.060)
}

@Test
func resolverIncompatibleHardBandFallsBackToAdaptiveWithProvenance() throws {
    var profile = ManualThresholdProfile.automatic
    profile.tonic.isiLower = .init(mode: .hardGate, valueSec: 0.050)   // would raise lower above upper
    profile.tonic.isiUpper = .init(mode: .hardGate, valueSec: 0.030)   // min(0.060, 0.030) = 0.030
    let resolved = ManualThresholdResolver.resolve(profile: profile, adaptive: baselineAdaptive())
    // 0.050 > 0.030 is incompatible: fail safely to the adaptive band, do not emit an empty band.
    #expect(resolved.tonic.lowerSec == 0.020)
    #expect(resolved.tonic.upperSec == 0.060)
    #expect(resolved.manualProvenanceKeys().contains { $0.contains("incompatible_user_bound") })
}

@Test
func resolverSoftAnchorExpandsBandAsUnion() throws {
    var profile = ManualThresholdProfile.automatic
    profile.tonic.isiUpper = .init(mode: .softAnchor, valueSec: 0.080)   // above adaptive 0.060
    profile.tonic.isiLower = .init(mode: .softAnchor, valueSec: 0.012)   // below adaptive 0.020
    let resolved = ManualThresholdResolver.resolve(profile: profile, adaptive: baselineAdaptive())
    // Soft anchor unions/expands the band (it only shifts the band; the detector still decides whether
    // a candidate exists — soft never forces one).
    #expect(resolved.tonic.upperSec == 0.080)   // max(0.060, 0.080)
    #expect(resolved.tonic.lowerSec == 0.012)   // min(0.020, 0.012)
    #expect(resolved.manualProvenanceKeys().contains { $0.contains("tonic.isi_upper_sec") && $0.contains("user_soft_anchor") })
}

@Test
func resolverSoftHFTonicFloorIsClampedToBurstSeedUpper() throws {
    var profile = ManualThresholdProfile.automatic
    profile.hfTonic.isiFloor = .init(mode: .softAnchor, valueSec: 0.004)   // would fall below burst seed upper 0.010
    let resolved = ManualThresholdResolver.resolve(profile: profile, adaptive: baselineAdaptive())
    #expect(resolved.hfTonic.lowerSec == 0.010)   // clamped up to the burst seed upper
    #expect(resolved.manualProvenanceKeys().contains { $0.contains("hf_tonic_floor_clamped_to_burst_seed_upper") })
}

@Test
func resolverExplicitHardHFTonicFloorBelowBurstSeedUpperIsRespectedWithProvenance() throws {
    // Adaptive floor (0.005) already sits below the burst seed upper (0.010); a user hard floor of
    // 0.006 keeps it there explicitly (max(0.005, 0.006) = 0.006 < 0.010) and records the choice.
    var profile = ManualThresholdProfile.automatic
    profile.hfTonic.isiFloor = .init(mode: .hardGate, valueSec: 0.006)
    let adaptive = baselineAdaptive(burstSeedUpperSec: 0.010, hfTonicFloorSec: 0.005)
    let resolved = ManualThresholdResolver.resolve(profile: profile, adaptive: adaptive)
    #expect(resolved.hfTonic.lowerSec == 0.006)
    #expect(resolved.manualProvenanceKeys().contains { $0.contains("hf_tonic_floor_below_burst_seed_upper_user_explicit") })
}

@Test
func resolverAutomaticHFTonicFloorBelowBurstSeedUpperIsPreservedUnchanged() throws {
    // Pure-automatic must preserve adaptive behavior exactly, even when the adaptive floor sits below
    // the burst seed upper (no clamp, no manual provenance).
    let adaptive = baselineAdaptive(burstSeedUpperSec: 0.010, hfTonicFloorSec: 0.005)
    let resolved = ManualThresholdResolver.resolve(profile: .automatic, adaptive: adaptive)
    #expect(resolved.hfTonic.lowerSec == 0.005)
    #expect(!resolved.appliedManualThreshold)
}

@Test
func resolverHFTonicFloorClampedWhenBurstUpperGatedAboveAutomaticFloor() throws {
    // Burst seed upper is manually hard-gated and the (automatic) HF-tonic floor sits below the new
    // burst seed upper. The floor is clamped up, and the safeguard provenance is internally
    // consistent (mode/source agree) and attributed to the burst-upper change, not a floor anchor.
    var profile = ManualThresholdProfile.automatic
    profile.burst.seedUpperISI = .init(mode: .hardGate, valueSec: 0.009)   // min(0.010, 0.009) = 0.009
    let adaptive = baselineAdaptive(burstSeedUpperSec: 0.010, hfTonicFloorSec: 0.005)
    let resolved = ManualThresholdResolver.resolve(profile: profile, adaptive: adaptive)

    #expect(resolved.hfTonic.lowerSec == 0.009)   // clamped up to the effective burst seed upper
    #expect(resolved.manualProvenanceKeys().contains { $0.contains("hf_tonic_floor_clamped_to_burst_seed_upper") })
    let safeguard = try #require(resolved.provenance.first { $0.key == "hf_tonic.isi_floor_safeguard" })
    #expect(safeguard.mode == .hardGate)
    #expect(safeguard.source == .userHardGate)   // consistent pair (no automatic + soft_anchor contradiction)
    // The floor's own primary record stays automatic/adaptive and is not overwritten.
    let primary = try #require(resolved.provenance.first { $0.key == "hf_tonic.isi_floor_sec" })
    #expect(primary.mode == .automatic && primary.source == .adaptive)
}

@Test
func resolverHardSpikeCountReplacesAndSoftCountIsTreatedAsAutomatic() throws {
    var hardProfile = ManualThresholdProfile.automatic
    hardProfile.tonic.minSpikes = .init(mode: .hardGate, value: 12)
    let hardResolved = ManualThresholdResolver.resolve(profile: hardProfile, adaptive: baselineAdaptive())
    #expect(hardResolved.tonic.minSpikes == 12)   // replaces adaptive 5
    #expect(hardResolved.manualProvenanceKeys().contains { $0.contains("tonic.min_spikes") && $0.contains("user_hard_gate") })

    var softProfile = ManualThresholdProfile.automatic
    softProfile.tonic.minSpikes = .init(mode: .softAnchor, value: 12)
    let softResolved = ManualThresholdResolver.resolve(profile: softProfile, adaptive: baselineAdaptive())
    #expect(softResolved.tonic.minSpikes == 5)    // soft count -> automatic (no forcing)
    #expect(!softResolved.appliedManualThreshold)
}
