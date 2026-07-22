import Foundation
import STPDCore
import Testing

// Phase 7: read-only per-ISI diagnostic — projects the train's resolved adaptive burst seed band onto a hovered
// ISI to explain why it is / is not a selected pattern. No detection logic.

private func diagnose(
    isiSec: Double,
    candidate: String? = nil,
    lower: Double? = 0.003,
    upper: Double? = 0.025,
    bridge: Double? = 0.05,
    pause: Double? = nil
) -> PerISIDiagnostic {
    PerISIDiagnosticBuilder.diagnose(
        isiSec: isiSec,
        coveringCandidateLabel: candidate,
        seedLowerSec: lower,
        seedUpperSec: upper,
        bridgeUpperSec: bridge,
        manualPauseLowerSec: pause
    )
}

@Test
func perISICandidateMembership() {
    let d = diagnose(isiSec: 0.01, candidate: "burst")
    #expect(d.belongsToCandidate)
    #expect(d.candidateLabel == "burst")
    #expect(d.bandRelationTag == "in candidate")
    #expect(d.explanation.contains("burst"))
    #expect(d.explanation.lowercased().contains("click"))
}

@Test
func perISINoBandIsExplicit() {
    let d = diagnose(isiSec: 0.01, lower: nil, upper: nil, bridge: nil)
    #expect(!d.belongsToCandidate)
    #expect(d.bandRelationTag == "no band")
    #expect(d.explanation.lowercased().contains("run the detector"))
}

@Test
func perISIBandRelations() {
    #expect(diagnose(isiSec: 0.001).bandRelationTag == "below seed lower")     // 1 ms < 3 ms
    #expect(diagnose(isiSec: 0.010).bandRelationTag == "in seed band")          // 10 ms in [3,25]
    #expect(diagnose(isiSec: 0.035).bandRelationTag == "within bridge")         // 35 ms in (25,50]
    #expect(diagnose(isiSec: 0.080).bandRelationTag == "above seed upper")      // 80 ms > 50
    // No bridge → above seed upper directly.
    #expect(diagnose(isiSec: 0.035, bridge: nil).bandRelationTag == "above seed upper")
}

@Test
func perISIInBandReportsObservationWithoutInventingCause() {
    let d = diagnose(isiSec: 0.010)
    #expect(d.explanation.lowercased().contains("not part of a selected burst"))
    #expect(d.explanation.lowercased().contains("possible explanations"))
    #expect(d.explanation.lowercased().contains("does not carry the causal provenance"))
    #expect(!d.explanation.lowercased().contains("lacks enough contiguous"))
    #expect(d.explanation.contains("3"))   // band lower 3 ms
    #expect(d.explanation.contains("25"))  // band upper 25 ms
}

@Test
func perISIManualPauseComparison() {
    let abovePause = diagnose(isiSec: 0.080, pause: 0.06)   // 80 ms ≥ 60 ms
    #expect(abovePause.explanation.contains("pause threshold"))
    #expect(abovePause.explanation.lowercased().contains("at/above"))
    #expect(abovePause.explanation.lowercased().contains("does not establish"))

    let belowPause = diagnose(isiSec: 0.020, pause: 0.06)   // 20 ms < 60 ms
    #expect(belowPause.explanation.lowercased().contains("below the configured manual pause threshold"))
    #expect(belowPause.explanation.lowercased().contains("does not establish"))
}

@Test
func perISIIsDeterministicAndCarriesExactBridgeEvidence() {
    let first = diagnose(isiSec: 0.035)
    let second = diagnose(isiSec: 0.035)
    #expect(first == second)
    #expect(first.bandRelationTag == "within bridge")
    #expect(first.seedLowerSec == 0.003)
    #expect(first.seedUpperSec == 0.025)
    #expect(first.bridgeUpperSec == 0.05)
    #expect(first.explanation.contains("35 ms"))
    #expect(first.explanation.contains("50 ms"))
}

@Test
func perISISeedBandText() {
    #expect(PerISIDiagnosticBuilder.seedBandText(lowerSec: 0.003, upperSec: 0.025) == "3–25 ms")
    #expect(PerISIDiagnosticBuilder.seedBandText(lowerSec: nil, upperSec: 0.025) == nil)
    #expect(PerISIDiagnosticBuilder.seedBandText(lowerSec: 0.03, upperSec: 0.02) == nil)
    #expect(PerISIDiagnosticBuilder.seedBandText(lowerSec: -.infinity, upperSec: 0.02) == nil)
}

@Test
func perISIRejectsInvalidISIMagnitudesBeforeCandidateOrBandClassification() {
    for invalid in [Double.nan, .infinity, -.infinity, -0.001] {
        let d = diagnose(isiSec: invalid, candidate: "burst", pause: 0.06)
        #expect(!d.belongsToCandidate)
        #expect(d.candidateLabel == nil)
        #expect(d.bandRelationTag == "invalid ISI")
        #expect(d.seedLowerSec == nil)
        #expect(d.seedUpperSec == nil)
        #expect(d.bridgeUpperSec == nil)
        #expect(d.explanation.lowercased().contains("finite"))
    }
}

@Test
func perISIRejectsMalformedBandsAndIgnoresInvalidOptionalThresholds() {
    let malformedBands = [
        diagnose(isiSec: 0.01, lower: 0.03, upper: 0.02),
        diagnose(isiSec: 0.01, lower: -0.001, upper: 0.02),
        diagnose(isiSec: 0.01, lower: 0.001, upper: .infinity),
        diagnose(isiSec: 0.01, lower: 0.0, upper: 0.0)
    ]
    #expect(malformedBands.allSatisfy { $0.bandRelationTag == "no band" })
    #expect(malformedBands.allSatisfy { $0.seedLowerSec == nil && $0.seedUpperSec == nil })

    let invalidBridge = diagnose(isiSec: 0.035, bridge: 0.02)
    #expect(invalidBridge.bandRelationTag == "above seed upper")
    #expect(invalidBridge.bridgeUpperSec == nil)

    let invalidPause = diagnose(isiSec: 0.08, pause: -.infinity)
    #expect(!invalidPause.explanation.contains("pause threshold"))
}
