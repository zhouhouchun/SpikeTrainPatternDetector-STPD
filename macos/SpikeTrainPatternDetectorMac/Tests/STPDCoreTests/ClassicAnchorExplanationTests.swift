import Foundation
import Testing
@testable import STPDCore   // ClassicAnchorCandidate's memberwise init is internal

// Phase 6: read-only "why this label" explanation builder. It only reformats the candidate's EXISTING provenance
// (label, auto-selected, adaptive band, auditReasonSummary, failureReason) — no detection logic.

private func makeCandidate(
    label: ClassicAnchorLabel,
    selectedForAuto: Bool,
    bandLowerSec: Double,
    bandUpperSec: Double,
    bandSource: ClassicAnchorBandSource,
    nSpikes: Int,
    nValidISI: Int,
    action: String = "accept",
    decisionPath: String = "test"
) -> ClassicAnchorCandidate {
    precondition(nSpikes == nValidISI + 1)
    return ClassicAnchorCandidate(
        id: "c1",
        trainID: "train-1",
        trainName: "train-1",
        candidateLayer: "test_candidate",
        candidateClass: label.rawValue,
        finalLabel: label,
        gateStatus: "pass",
        decisionPath: decisionPath,
        action: action,
        score: 1,
        priority: 100,
        selectedForAuto: selectedForAuto,
        selectionStatus: "not_selected",
        startISIIndex: nValidISI > 0 ? 1 : 0,
        endISIIndex: nValidISI,
        startSpikeIndex: 0,
        endSpikeIndex: nSpikes - 1,
        nISI: nValidISI,
        nValidISI: nValidISI,
        nSpikes: nSpikes,
        durationSec: 0.1,
        intraQ10Sec: 0.01,
        intraQ40Sec: 0.01,
        intraQ50Sec: 0.01,
        intraQ90Sec: 0.01,
        intraQ95Sec: 0.01,
        maxIntraISISec: 0.01,
        meanIntraISISec: 0.01,
        cv: 0.1,
        lv: 0.1,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: label.rawValue,
        anchorLockLevel: .lockedClassic,
        anchorBandLowerSec: bandLowerSec,
        anchorBandUpperSec: bandUpperSec,
        anchorBandSource: bandSource,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil,
        hfSpikingBurstDominated: nil
    )
}

@Test
func explanationBandTextFormatsMillisecondsAndReadableSource() {
    let candidate = makeCandidate(
        label: .burst, selectedForAuto: true,
        bandLowerSec: 0.001, bandUpperSec: 0.1, bandSource: .eventGrammarSeedBand,
        nSpikes: 5, nValidISI: 4
    )
    // 0.001 s → 1 ms, 0.1 s → 100 ms; snake_case source becomes readable.
    #expect(ClassicAnchorExplanation.bandText(candidate) == "1–100 ms · event grammar seed band")
}

@Test
func explanationHeadlineLinesReuseProvenance() {
    let candidate = makeCandidate(
        label: .possibleBurst, selectedForAuto: false,
        bandLowerSec: 0.002, bandUpperSec: 0.05, bandSource: .structure,
        nSpikes: 7, nValidISI: 6
    )
    let lines = ClassicAnchorExplanation.lines(for: candidate)

    // Fixed leading order: Label, Selected, ISI band, Spikes.
    #expect(lines.count >= 4)
    #expect(lines[0].label == "Label")
    #expect(!lines[0].value.isEmpty)
    #expect(lines[1].label == "Selected")
    #expect(lines[1].value == "Not auto-selected (review)")
    #expect(lines[2].label == "ISI band")
    #expect(lines[2].value == "2–50 ms · structure")
    #expect(lines[3].label == "Spikes")
    #expect(lines[3].value == "7 (6 valid ISI)")

    // Every rendered value is readable (no raw snake_case tokens leak through).
    #expect(lines.allSatisfy { !$0.value.contains("_") })
}

@Test
func explanationSelectedAndAutoStates() {
    let auto = makeCandidate(
        label: .burst, selectedForAuto: true,
        bandLowerSec: 0.001, bandUpperSec: 0.02, bandSource: .structure,
        nSpikes: 4, nValidISI: 3
    )
    let lines = ClassicAnchorExplanation.lines(for: auto)
    #expect(lines.first(where: { $0.label == "Selected" })?.value == "Auto-selected")
}

@Test
func explanationIsDeterministic() {
    let firstCandidate = makeCandidate(
        label: .pause, selectedForAuto: false,
        bandLowerSec: 0.05, bandUpperSec: 0.5, bandSource: .structure,
        nSpikes: 3, nValidISI: 2
    )
    let independentlyBuiltCandidate = makeCandidate(
        label: .pause, selectedForAuto: false,
        bandLowerSec: 0.05, bandUpperSec: 0.5, bandSource: .structure,
        nSpikes: 3, nValidISI: 2
    )
    let first = ClassicAnchorExplanation.lines(for: firstCandidate)
    let second = ClassicAnchorExplanation.lines(for: independentlyBuiltCandidate)
    #expect(first == second)
    #expect(first.first?.label == "Label")
    #expect(first.first?.value.lowercased() == "pause gap")
    #expect(first.first(where: { $0.label == "Spikes" })?.value == "3 (2 valid ISI)")
}
