import STPDCore
import Testing

// R4 fix: the Auto/default manual-threshold field state a newly-loaded dataset must start from. These pin the
// pure contract the document's `resetManualThresholdFields()` writes back, so a reset cannot silently drift.

@Test
func automaticDefaultsAreAllAutomaticAndZero() {
    let d = ManualThresholdFields.automaticDefaults
    #expect(d == ManualThresholdFields())          // default init == the documented defaults
    #expect(d.isAllAutomatic)

    // Every family mode is Automatic.
    #expect(d.burstMode == .automatic)
    #expect(d.hfsMode == .automatic)
    #expect(d.hfTonicMode == .automatic)
    #expect(d.tonicMode == .automatic)
    #expect(d.pauseMode == .automatic)

    // Every numeric value (ISI ms / min-spikes / min-duration) is zero.
    #expect(d.burstSeedMaxISIMs == 0 && d.burstBridgeMaxISIMs == 0 && d.burstMinSpikes == 0)
    #expect(d.hfsMinSpikes == 0 && d.hfsMinDurationMs == 0)
    #expect(d.hfTonicMinISIMs == 0 && d.hfTonicMaxISIMs == 0 && d.hfTonicMinSpikes == 0)
    #expect(d.tonicMinISIMs == 0 && d.tonicMaxISIMs == 0 && d.tonicMinSpikes == 0)
    #expect(d.pauseMinISIMs == 0)
}

@Test
func anyManualModeOverrideIsNotAllAutomatic() {
    // A mode change in any family is a manual override (the silent-carryover risk R4 fixes).
    for fields in [
        ManualThresholdFields(burstMode: .softAnchor),
        ManualThresholdFields(hfsMode: .hardGate),
        ManualThresholdFields(hfTonicMode: .softAnchor),
        ManualThresholdFields(tonicMode: .hardGate),
        ManualThresholdFields(pauseMode: .softAnchor),
    ] {
        #expect(!fields.isAllAutomatic)
    }
}

@Test
func anyNonZeroValueIsNotAllAutomatic() {
    // A nonzero numeric value (even with Automatic mode) still counts as a manual override to clear on load.
    #expect(!ManualThresholdFields(tonicMinISIMs: 30).isAllAutomatic)
    #expect(!ManualThresholdFields(burstSeedMaxISIMs: 10).isAllAutomatic)
    #expect(!ManualThresholdFields(hfsMinSpikes: 30).isAllAutomatic)
    #expect(!ManualThresholdFields(hfTonicMinSpikes: 6).isAllAutomatic)
    #expect(!ManualThresholdFields(pauseMinISIMs: 700).isAllAutomatic)
}
