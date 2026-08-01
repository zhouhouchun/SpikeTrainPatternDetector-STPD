import Foundation
import STPDCore
import Testing

// Pure pattern-state annotation layer: per-ISI point query (ISI State Space) + per-bin aggregation
// (Neural Manifold). Annotation/visualization only — never a PCA / population-matrix input.

private func bin(_ id: Int, _ start: Double, _ end: Double) -> NeuralPopulationBin {
    NeuralPopulationBin(binID: id, startSec: start, endSec: end, midSec: (start + end) / 2, widthSec: end - start)
}

private func interval(
    _ trainID: String, raw: ClosedRange<Double>, aligned: ClosedRange<Double>? = nil, _ state: SpikePatternState
) -> SpikePatternInterval {
    let a = aligned ?? raw
    return SpikePatternInterval(
        trainID: trainID,
        rawStartSec: raw.lowerBound, rawEndSec: raw.upperBound,
        alignedStartSec: a.lowerBound, alignedEndSec: a.upperBound,
        state: state
    )
}

private func annotate(
    _ bins: [NeuralPopulationBin], _ selected: Set<String>, useAligned: Bool, _ intervals: [SpikePatternInterval]
) -> [NeuralPopulationBinPatternSummary] {
    NeuralPopulationPatternStateAnnotator.annotate(
        bins: bins, selectedTrainIDs: selected, useAligned: useAligned, intervals: intervals
    )
}

// MARK: - 1. Label -> display state mapping.

@Test
func patternStateMapsLabelFamilies() {
    #expect(SpikePatternState(label: .burst) == .burst)
    #expect(SpikePatternState(label: .longBurst) == .burst)
    #expect(SpikePatternState(label: .highFrequencyBurst) == .burst)
    #expect(SpikePatternState(label: .possibleBurst) == .burst)
    #expect(SpikePatternState(label: .pause) == .pause)
    #expect(SpikePatternState(label: .tonic) == .tonic)
    #expect(SpikePatternState(label: .highFrequencyTonic) == .hfTonic)
    #expect(SpikePatternState(label: .highFrequencySpiking) == .hfs)
    #expect(SpikePatternState(label: .reject) == .unlabeled)
    #expect(SpikePatternState(label: .profile) == .unlabeled)
}

// MARK: - 2. Bin aggregation — no annotations -> all unlabeled.

@Test
func binsWithoutAnnotationsAreUnlabeled() {
    let result = annotate([bin(0, 0, 1), bin(1, 1, 2)], ["t"], useAligned: false, [])
    #expect(result.count == 2)
    #expect(result.allSatisfy { $0.dominant == .unlabeled && $0.labeledFraction == 0 && $0.fractions.isEmpty })
}

// MARK: - 3. A burst annotation overlapping two bins labels both burst.

@Test
func burstAnnotationOverlappingTwoBinsLabelsBoth() {
    let result = annotate([bin(0, 0, 1), bin(1, 1, 2)], ["t"], useAligned: false, [interval("t", raw: 0.5...1.5, .burst)])
    #expect(result[0].dominant == .burst)
    #expect(abs(result[0].burstFraction - 0.5) < 1e-9)   // [0.5, 1] of width 1
    #expect(result[1].dominant == .burst)
    #expect(abs(result[1].burstFraction - 0.5) < 1e-9)   // [1, 1.5] of width 1
}

// MARK: - 4. Reviewed/public source can remove a raw auto run (without touching PCA — separate input).

@Test
func reviewedSourceRemovingARunChangesAnnotationButIsJustADifferentInput() {
    let bins = [bin(0, 0, 1)]
    let auto = [interval("t", raw: 0...1, .burst)]
    let reviewedAfterVeto: [SpikePatternInterval] = []   // a not_burst veto removed the run
    #expect(annotate(bins, ["t"], useAligned: false, auto)[0].dominant == .burst)
    #expect(annotate(bins, ["t"], useAligned: false, reviewedAfterVeto)[0].dominant == .unlabeled)
}

// MARK: - 5. Raw vs aligned time-origin selects the correct interval bounds.

@Test
func timeOriginChoosesRawOrAlignedBounds() {
    let bins = [bin(0, 0, 1), bin(1, 1, 2)]
    let iv = interval("t", raw: 0.1...0.9, aligned: 1.1...1.9, .burst)   // raw in bin0, aligned in bin1
    let raw = annotate(bins, ["t"], useAligned: false, [iv])
    #expect(raw[0].dominant == .burst && raw[1].dominant == .unlabeled)
    let aligned = annotate(bins, ["t"], useAligned: true, [iv])
    #expect(aligned[0].dominant == .unlabeled && aligned[1].dominant == .burst)
}

// MARK: - 6. Dominant = largest overlap; fractions = overlap / width; ties broken by display order.

@Test
func dominantStateUsesLargestOverlapAndFractions() {
    let bins = [bin(0, 0, 1)]
    let mixed = annotate(bins, ["t"], useAligned: false, [
        interval("t", raw: 0...0.3, .burst),
        interval("t", raw: 0.3...1.0, .tonic),
    ])
    #expect(mixed[0].dominant == .tonic)                     // 0.7 > 0.3
    #expect(abs(mixed[0].burstFraction - 0.3) < 1e-9)
    #expect(abs(mixed[0].tonicFraction - 0.7) < 1e-9)
    #expect(abs(mixed[0].labeledFraction - 1.0) < 1e-9)

    // Equal overlap -> the earlier display-order state (burst before pause) wins.
    let tie = annotate(bins, ["t"], useAligned: false, [
        interval("t", raw: 0...0.5, .burst),
        interval("t", raw: 0.5...1.0, .pause),
    ])
    #expect(tie[0].dominant == .burst)
}

// MARK: - 7. Partial coverage leaves an unlabeled remainder; non-selected trains are ignored.

@Test
func partialCoverageAndSelectedTrainFilter() {
    let bins = [bin(0, 0, 1)]
    let partial = annotate(bins, ["t"], useAligned: false, [interval("t", raw: 0.25...0.75, .pause)])
    #expect(partial[0].dominant == .pause)
    #expect(abs(partial[0].pauseFraction - 0.5) < 1e-9)
    #expect(abs(partial[0].labeledFraction - 0.5) < 1e-9)   // remaining 0.5 is unlabeled

    // An interval on a non-selected train contributes nothing.
    let filtered = annotate(bins, ["t1"], useAligned: false, [interval("t2", raw: 0...1, .burst)])
    #expect(filtered[0].dominant == .unlabeled)
}

// MARK: - 8. Point query (ISI State Space): covering interval -> state, gap/other train -> unlabeled.

@Test
func pointQueryReturnsCoveringIntervalState() {
    let intervals = SpikePatternIntervals(
        [interval("t", raw: 1...2, .burst), interval("t", raw: 3...4, .pause)], useAligned: false
    )
    #expect(intervals.state(forTrainID: "t", atSec: 1.5) == .burst)
    #expect(intervals.state(forTrainID: "t", atSec: 3.5) == .pause)
    #expect(intervals.state(forTrainID: "t", atSec: 2.5) == .unlabeled)     // gap
    #expect(intervals.state(forTrainID: "other", atSec: 1.5) == .unlabeled) // other train

    // Aligned bounds are used when requested.
    let aligned = SpikePatternIntervals([interval("t", raw: 1...2, aligned: 5...6, .burst)], useAligned: true)
    #expect(aligned.state(forTrainID: "t", atSec: 5.5) == .burst)
    #expect(aligned.state(forTrainID: "t", atSec: 1.5) == .unlabeled)
}

// MARK: - 9. Reviewed/final RELABELS a run (auto burst -> reviewed pause): the bin overlay color changes.
// Mirrors the manifold's Auto (classicAnchorRawEventAnnotations) vs Final (classicAnchorPublicEventAnnotations)
// sources: the same time window resolves to a different dominant state, so reviewed overrides auto in the
// visualization data without touching PCA (the population matrix is a separate input).

@Test
func reviewedSourceRelabelingARunChangesDominantState() {
    let bins = [bin(0, 0, 1)]
    let auto = [interval("t", raw: 0...1, .burst)]
    let reviewedAfterPositiveLabel = [interval("t", raw: 0...1, .pause)]   // manual veto + positive relabel
    #expect(annotate(bins, ["t"], useAligned: false, auto)[0].dominant == .burst)
    #expect(annotate(bins, ["t"], useAligned: false, reviewedAfterPositiveLabel)[0].dominant == .pause)
}

// MARK: - 10. Point query (ISI State Space): reviewed-final overrides auto at the SAME ISI time.

@Test
func pointQueryReviewedOverridesAutoAtSameTime() {
    let auto = SpikePatternIntervals([interval("t", raw: 1...2, .burst)], useAligned: false)
    // Relabel: a positive manual label reclassifies the run -> Final colors the same ISI pause, not burst.
    let reviewedRelabel = SpikePatternIntervals([interval("t", raw: 1...2, .pause)], useAligned: false)
    #expect(auto.state(forTrainID: "t", atSec: 1.5) == .burst)
    #expect(reviewedRelabel.state(forTrainID: "t", atSec: 1.5) == .pause)

    // Veto: a not_burst veto removes the run -> Final leaves the same ISI unlabeled.
    let reviewedAfterVeto = SpikePatternIntervals([], useAligned: false)
    #expect(reviewedAfterVeto.state(forTrainID: "t", atSec: 1.5) == .unlabeled)
}
