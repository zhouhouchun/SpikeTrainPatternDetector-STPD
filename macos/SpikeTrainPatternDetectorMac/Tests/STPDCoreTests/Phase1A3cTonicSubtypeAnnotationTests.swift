import Foundation
import Testing
@testable import STPDCore

@Test
func phase1A3cTonicFamilySubtypePropagatesAndDisplays() throws {
    let train = phase1A3cTrain()

    let classic = ClassicAnchorEventAnnotation(
        candidate: phase1A3cCandidate(train: train, label: .tonic, subtype: "classic"),
        train: train
    )
    #expect(classic.stateTonicSubtype == "classic")
    #expect(classic.displaySubtypeName == "Classic tonic")

    let irregular = ClassicAnchorEventAnnotation(
        candidate: phase1A3cCandidate(train: train, label: .tonic, subtype: "irregular"),
        train: train
    )
    #expect(irregular.stateTonicSubtype == "irregular")
    #expect(irregular.displaySubtypeName == "Irregular tonic")

    let highFrequency = ClassicAnchorEventAnnotation(
        candidate: phase1A3cCandidate(
            train: train,
            label: .highFrequencyTonic,
            subtype: "high_frequency"
        ),
        train: train
    )
    #expect(highFrequency.stateTonicSubtype == "high_frequency")
    #expect(highFrequency.displaySubtypeName == "HF tonic")
}

@Test
func phase1A3cUnknownTonicSubtypeRemainsAuditableWithSafeDisplayFallback() {
    let train = phase1A3cTrain()
    let annotation = ClassicAnchorEventAnnotation(
        candidate: phase1A3cCandidate(
            train: train,
            label: .tonic,
            subtype: "future_tonic_variant"
        ),
        train: train
    )

    #expect(annotation.stateTonicSubtype == "future_tonic_variant")
    #expect(annotation.displaySubtypeName == "Tonic")
}

@Test
func phase1A3cNonTonicRelabelClearsStaleTonicSubtype() {
    let train = phase1A3cTrain()

    for label in [ClassicAnchorLabel.burst, .pause, .highFrequencySpiking] {
        let candidate = phase1A3cCandidate(
            train: train,
            label: label,
            subtype: "irregular"
        )
        let annotation = ClassicAnchorEventAnnotation(candidate: candidate, train: train)

        #expect(annotation.label == label)
        #expect(annotation.stateTonicSubtype == nil)
    }
}

@Test
func phase1A3cClippingPreservesNormalizedSubtypeAndCandidateIdentity() throws {
    let train = phase1A3cTrain()
    let candidate = phase1A3cCandidate(
        train: train,
        label: .tonic,
        subtype: "irregular",
        startISIIndex: 1,
        endISIIndex: 4
    )
    let annotation = ClassicAnchorEventAnnotation(candidate: candidate, train: train)
    let clipped = try #require(annotation.clipped(to: 2...3, in: train))

    #expect(clipped.id != annotation.id)
    #expect(clipped.candidateID == annotation.candidateID)
    #expect(clipped.label == annotation.label)
    #expect(clipped.stateTonicSubtype == "irregular")
    #expect(clipped.displaySubtypeName == "Irregular tonic")
    #expect(clipped.startISISecIndex == 2)
    #expect(clipped.endISISecIndex == 3)
    #expect(clipped.startSpikeIndex == 2)
    #expect(clipped.endSpikeIndex == 4)
    #expect(clipped.score == annotation.score)
    #expect(clipped.priority == annotation.priority)
    #expect(clipped.decisionPath == annotation.decisionPath)
}

@Test
func phase1A3cDetectionRunProjectionCarriesSubtypeWithoutChangingLabel() throws {
    let train = phase1A3cTrain()
    let candidate = phase1A3cCandidate(
        train: train,
        label: .tonic,
        subtype: "classic"
    )
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: [candidate]
            )
        ]
    )
    let dataset = SpikeDataset(
        name: "phase1a3c-dataset",
        sourceDescription: "phase1a3c-test",
        trains: [train]
    )

    let annotations = run.eventAnnotations(in: dataset, tracks: [.state])
    let annotation = try #require(annotations.first)

    #expect(annotations.count == 1)
    #expect(annotation.candidateID == candidate.id)
    #expect(annotation.label == .tonic)
    #expect(annotation.stateTonicSubtype == "classic")
    #expect(annotation.displaySubtypeName == "Classic tonic")
}

private func phase1A3cTrain() -> SpikeTrain {
    SpikeTrain(
        name: "phase1a3c-train",
        timestampsSec: [0.00, 0.04, 0.08, 0.12, 0.16]
    )
}

private func phase1A3cCandidate(
    train: SpikeTrain,
    label: ClassicAnchorLabel,
    subtype: String?,
    startISIIndex: Int = 1,
    endISIIndex: Int = 4
) -> ClassicAnchorCandidate {
    let nISI = endISIIndex - startISIIndex + 1
    var candidate = ClassicAnchorCandidate(
        id: "phase1a3c-\(label.rawValue)-\(subtype ?? "none")",
        trainID: train.id,
        trainName: train.name,
        candidateLayer: "phase1a3c_test",
        candidateClass: label.rawValue,
        finalLabel: label,
        gateStatus: "pass",
        decisionPath: "phase1a3c_annotation_contract",
        action: "accept",
        score: 0.9,
        priority: 700,
        selectedForAuto: true,
        selectionStatus: "selected_for_test",
        startISIIndex: startISIIndex,
        endISIIndex: endISIIndex,
        startSpikeIndex: startISIIndex,
        endSpikeIndex: endISIIndex + 1,
        nISI: nISI,
        nValidISI: nISI,
        nSpikes: nISI + 1,
        durationSec: Double(nISI) * 0.04,
        intraQ10Sec: 0.04,
        intraQ40Sec: 0.04,
        intraQ50Sec: 0.04,
        intraQ90Sec: 0.04,
        intraQ95Sec: 0.04,
        maxIntraISISec: 0.04,
        meanIntraISISec: 0.04,
        cv: 0,
        lv: 0,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: label.rawValue,
        anchorLockLevel: .lockedClassic,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.1,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil,
        hfSpikingBurstDominated: nil
    )
    candidate.stateTonicSubtype = subtype
    return candidate
}
