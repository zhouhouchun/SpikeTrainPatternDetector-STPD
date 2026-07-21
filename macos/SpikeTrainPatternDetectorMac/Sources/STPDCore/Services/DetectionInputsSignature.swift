import Foundation

/// A comparable snapshot of every input that determines classic-anchor detection output. The app compares the
/// signature of the CURRENT Detector/Parameters panel values against the signature the *displayed* results were
/// computed with, so it can tell the user that the on-screen detection is STALE (a re-run is needed) instead of
/// silently showing labels from a prior parameter set — the "I changed Burst seed max but nothing happened" trap.
///
/// It deliberately holds exactly the inputs passed to `HybridPatternDetectionFramework.run` (minus the fixed
/// `refractoryAction`), so any change that would alter detection — adaptive band settings, QC settings, state
/// tuning, detector parameters, or the manual threshold profile (which is built canonically by the document, so
/// an inert value left in Automatic mode does not register as a change) — flips equality. This is a pure value
/// type used only for staleness comparison; it is never a detection input itself.
public struct DetectionInputsSignature: Equatable, Sendable {
    public let datasetID: String
    public let bandSettings: TrainAdaptiveBandSettings
    public let qualitySettings: SpikeQualitySettings
    public let stateTuning: StatePatternDetectorTuning
    public let detectorParameters: PatternDetectionParameterSettings
    public let manualThresholdProfile: ManualThresholdProfile
    /// P5A: experimental Adaptive-v2 burst canonicalization toggle. It changes detection output, so it is part of the
    /// signature — flipping it marks current results stale. Default false (byte-identical to legacy behavior).
    public let useAdaptiveV2Canonicalization: Bool
    /// P8: the manual hard-threshold SCOPE intent (which trains the manual thresholds target). Default `.allTrains` =
    /// today's global behavior, so existing signatures stay byte-identical. Changing the scope kind — or the resolved
    /// target train set — flips equality and marks current results stale.
    public let manualThresholdScope: ManualThresholdScope

    public init(
        datasetID: String,
        bandSettings: TrainAdaptiveBandSettings,
        qualitySettings: SpikeQualitySettings,
        stateTuning: StatePatternDetectorTuning,
        detectorParameters: PatternDetectionParameterSettings,
        manualThresholdProfile: ManualThresholdProfile,
        useAdaptiveV2Canonicalization: Bool = false,
        manualThresholdScope: ManualThresholdScope = .allTrains
    ) {
        self.datasetID = datasetID
        self.bandSettings = bandSettings
        // `displayUnit` is a UI formatting choice (s vs ms) that does not affect detection output, so normalize
        // it out of the signature — otherwise toggling the QC display unit would over-show the "re-run" hint.
        var detectionQuality = qualitySettings
        detectionQuality.displayUnit = .seconds
        self.qualitySettings = detectionQuality
        self.stateTuning = stateTuning
        self.detectorParameters = detectorParameters
        self.manualThresholdProfile = manualThresholdProfile
        self.useAdaptiveV2Canonicalization = useAdaptiveV2Canonicalization
        self.manualThresholdScope = manualThresholdScope
    }
}
