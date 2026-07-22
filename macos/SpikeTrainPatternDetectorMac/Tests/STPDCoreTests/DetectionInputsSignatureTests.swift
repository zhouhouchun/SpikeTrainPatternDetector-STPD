import Testing
@testable import STPDCore

// PARAM-5: the staleness signature must change exactly when a detection-affecting input changes, so the app can
// tell the user the displayed raster is stale after a Detector/Parameters edit (instead of silently showing
// results from the prior parameter set — the "settings appear ineffective" report).

private func signature(
    datasetID: String = "ds",
    profile: ManualThresholdProfile = .automatic,
    detectorParameters: PatternDetectionParameterSettings = .defaults,
    bandSettings: TrainAdaptiveBandSettings = TrainAdaptiveBandSettings(),
    qualitySettings: SpikeQualitySettings = SpikeQualitySettings()
) -> DetectionInputsSignature {
    DetectionInputsSignature(
        datasetID: datasetID,
        bandSettings: bandSettings,
        qualitySettings: qualitySettings,
        stateTuning: StatePatternDetectorTuning(),
        detectorParameters: detectorParameters,
        manualThresholdProfile: profile
    )
}

private func hardSeedMax(_ valueSec: Double) -> ManualThresholdProfile {
    ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: valueSec))
    )
}

@Test
func detectionSignatureIsStableForIdenticalInputs() {
    #expect(signature() == signature())
    #expect(signature(profile: hardSeedMax(0.100)) == signature(profile: hardSeedMax(0.100)))
}

@Test
func detectionSignatureChangesWhenBurstSeedMaxModeOrValueChanges() {
    // Turning a hard Burst seed max gate on, and changing its value, must each mark results stale.
    #expect(signature() != signature(profile: hardSeedMax(0.100)))
    #expect(signature(profile: hardSeedMax(0.100)) != signature(profile: hardSeedMax(0.050)))
}

@Test
func detectionSignatureChangesWhenDatasetOrDetectorParametersChange() {
    #expect(signature(datasetID: "a") != signature(datasetID: "b"))
    let tighter = PatternDetectionParameterSettings(classicBurstMaxSpikes: 4)
    #expect(signature() != signature(detectorParameters: tighter))
    let widerBand = TrainAdaptiveBandSettings(minValidISISec: 0.002, histogramBinWidthSec: 0.005)
    #expect(signature() != signature(bandSettings: widerBand))
}

@Test
func detectionSignatureIgnoresDisplayUnitButTracksDetectionRelevantQC() {
    // QC display unit (s vs ms) is UI-only and must NOT flag results stale...
    let sMode = SpikeQualitySettings(artifactThresholdSec: 0.001, refractorySuspectThresholdSec: 0.002, displayUnit: .seconds)
    let msMode = SpikeQualitySettings(artifactThresholdSec: 0.001, refractorySuspectThresholdSec: 0.002, displayUnit: .milliseconds)
    #expect(signature(qualitySettings: sMode) == signature(qualitySettings: msMode))

    // ...but a detection-relevant QC threshold change must.
    let tighterRefractory = SpikeQualitySettings(artifactThresholdSec: 0.001, refractorySuspectThresholdSec: 0.003, displayUnit: .seconds)
    #expect(signature(qualitySettings: sMode) != signature(qualitySettings: tighterRefractory))
}
