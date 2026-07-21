import Foundation

// MARK: - R4 fix: full mirror of the document's flat manual-threshold fields (for reset / default state)
//
// `RasterDocument` holds 17 flat manual-threshold fields (per family: a `ThresholdMode` + millisecond ISI
// values and/or min-spike counts). On loading a NEW dataset these must start from Auto/defaults so that
// soft/hard thresholds typed or learned for one dataset do not silently carry into the next.
//
// This is the COMPLETE field set, distinct from `ManualThresholdFieldState` (which is only the Phase-1B
// *learnable* subset — burst seed/bridge, tonic lower/upper, hf-tonic floor/upper, pause lower). This type
// also covers the spike-count / HFS / min-duration fields so a reset is exhaustive. It is a pure value type
// so the "what does a freshly-loaded dataset start from" contract is unit-testable without the app target.
public struct ManualThresholdFields: Hashable, Sendable {
    public var burstMode: ThresholdMode
    public var burstSeedMaxISIMs: Double
    public var burstBridgeMaxISIMs: Double
    public var burstMinSpikes: Int

    public var hfsMode: ThresholdMode
    public var hfsMinSpikes: Int
    public var hfsMinDurationMs: Double

    public var hfTonicMode: ThresholdMode
    public var hfTonicMinISIMs: Double
    public var hfTonicMaxISIMs: Double
    public var hfTonicMinSpikes: Int

    public var tonicMode: ThresholdMode
    public var tonicMinISIMs: Double
    public var tonicMaxISIMs: Double
    public var tonicMinSpikes: Int

    public var pauseMode: ThresholdMode
    public var pauseMinISIMs: Double

    /// Default init == Auto/zero for every family — i.e. the state a newly-loaded dataset must start from.
    /// Values mirror the `RasterDocument` field initializers exactly.
    public init(
        burstMode: ThresholdMode = .automatic, burstSeedMaxISIMs: Double = 0, burstBridgeMaxISIMs: Double = 0, burstMinSpikes: Int = 0,
        hfsMode: ThresholdMode = .automatic, hfsMinSpikes: Int = 0, hfsMinDurationMs: Double = 0,
        hfTonicMode: ThresholdMode = .automatic, hfTonicMinISIMs: Double = 0, hfTonicMaxISIMs: Double = 0, hfTonicMinSpikes: Int = 0,
        tonicMode: ThresholdMode = .automatic, tonicMinISIMs: Double = 0, tonicMaxISIMs: Double = 0, tonicMinSpikes: Int = 0,
        pauseMode: ThresholdMode = .automatic, pauseMinISIMs: Double = 0
    ) {
        self.burstMode = burstMode
        self.burstSeedMaxISIMs = burstSeedMaxISIMs
        self.burstBridgeMaxISIMs = burstBridgeMaxISIMs
        self.burstMinSpikes = burstMinSpikes
        self.hfsMode = hfsMode
        self.hfsMinSpikes = hfsMinSpikes
        self.hfsMinDurationMs = hfsMinDurationMs
        self.hfTonicMode = hfTonicMode
        self.hfTonicMinISIMs = hfTonicMinISIMs
        self.hfTonicMaxISIMs = hfTonicMaxISIMs
        self.hfTonicMinSpikes = hfTonicMinSpikes
        self.tonicMode = tonicMode
        self.tonicMinISIMs = tonicMinISIMs
        self.tonicMaxISIMs = tonicMaxISIMs
        self.tonicMinSpikes = tonicMinSpikes
        self.pauseMode = pauseMode
        self.pauseMinISIMs = pauseMinISIMs
    }

    /// The Auto/default field state a newly-loaded dataset starts from (all families Automatic, all values 0).
    public static let automaticDefaults = ManualThresholdFields()

    /// True when every family is in `.automatic` mode and every numeric value is zero — i.e. no manual override.
    public var isAllAutomatic: Bool {
        burstMode == .automatic && hfsMode == .automatic && hfTonicMode == .automatic
            && tonicMode == .automatic && pauseMode == .automatic
            && burstSeedMaxISIMs == 0 && burstBridgeMaxISIMs == 0 && burstMinSpikes == 0
            && hfsMinSpikes == 0 && hfsMinDurationMs == 0
            && hfTonicMinISIMs == 0 && hfTonicMaxISIMs == 0 && hfTonicMinSpikes == 0
            && tonicMinISIMs == 0 && tonicMaxISIMs == 0 && tonicMinSpikes == 0
            && pauseMinISIMs == 0
    }
}
