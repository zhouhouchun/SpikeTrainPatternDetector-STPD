import Foundation
import STPDCore
import Testing

private let dualTrackTrain = SpikeTrain(
    name: "unit_A",
    timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
)

private func mark(
    _ label: ManualAnnotationLabel,
    _ startISI: Int,
    _ endISI: Int,
    updatedAt: TimeInterval,
    id: UUID = UUID()
) -> ManualAnnotation {
    ManualAnnotation(
        id: id,
        trainID: dualTrackTrain.id,
        label: label,
        startSec: dualTrackTrain.timestampsSec[startISI - 1],
        endSec: dualTrackTrain.timestampsSec[endISI],
        startISIIndex: startISI,
        endISIIndex: endISI,
        startSpikeIndex: startISI - 1,
        endSpikeIndex: endISI,
        createdAt: Date(timeIntervalSince1970: updatedAt),
        updatedAt: Date(timeIntervalSince1970: updatedAt)
    )
}

@Test("Manual vocabulary preserves HFB and assigns stable state/event tracks")
func manualVocabularyHasIndependentHFBAndTracks() {
    #expect(ManualAnnotationLabel(autoLabel: .highFrequencyBurst) == .highFrequencyBurst)
    #expect(ManualAnnotationLabel.highFrequencyBurst.semanticTrack == .event)
    #expect(ManualAnnotationLabel.burst.semanticTrack == .event)
    #expect(ManualAnnotationLabel.pause.semanticTrack == .event)
    #expect(ManualAnnotationLabel.tonic.semanticTrack == .state)
    #expect(ManualAnnotationLabel.highFrequencySpiking.semanticTrack == .state)
    #expect(ManualAnnotationLabel.other.semanticTrack == .other)
    #expect(!ManualAnnotationLabel.positiveLabels(for: .event).contains(.notBurst))
}

@Test("HFS state and embedded HFB event coexist on the same ISIs")
func dualTrackProjectionPreservesEmbeddedEvent() throws {
    let annotations = [
        mark(.highFrequencySpiking, 1, 5, updatedAt: 1),
        mark(.highFrequencyBurst, 2, 4, updatedAt: 2),
    ]
    let projection = ManualISIDualTrackProjector.project(
        train: dualTrackTrain,
        annotations: annotations
    )

    #expect(projection.rows.count == 5)
    for index in 2...4 {
        let row = try #require(projection.rows.first { $0.isiIndex == index })
        #expect(row.statePattern == ManualAnnotationLabel.highFrequencySpiking.rawValue)
        #expect(row.eventPattern == ManualAnnotationLabel.highFrequencyBurst.rawValue)
        #expect(row.otherPattern.isEmpty)
    }
    #expect(projection.rows.first { $0.isiIndex == 1 }?.eventPattern == "")
}

@Test("Last edit wins only within its own semantic track")
func dualTrackLastEditWinsWithinTrackOnly() throws {
    let annotations = [
        mark(.tonic, 1, 5, updatedAt: 1),
        mark(.burst, 2, 4, updatedAt: 2),
        mark(.highFrequencySpiking, 3, 3, updatedAt: 3),
        mark(.pause, 4, 4, updatedAt: 4),
    ]
    let rows = ManualISIDualTrackProjector.project(
        train: dualTrackTrain,
        annotations: annotations
    ).rows

    let third = try #require(rows.first { $0.isiIndex == 3 })
    #expect(third.statePattern == ManualAnnotationLabel.highFrequencySpiking.rawValue)
    #expect(third.eventPattern == ManualAnnotationLabel.burst.rawValue)
    let fourth = try #require(rows.first { $0.isiIndex == 4 })
    #expect(fourth.statePattern == ManualAnnotationLabel.tonic.rawValue)
    #expect(fourth.eventPattern == ManualAnnotationLabel.pause.rawValue)
}

@Test("Bulk editor keeps state and event independent and Other explicitly replaces both")
func bulkEditorSupportsDiscontiguousAndOther() throws {
    let authoredAt = Date(timeIntervalSince1970: 10)
    var annotations = ManualISILabelDraftEditor.applying(
        label: .highFrequencySpiking,
        toISIIndices: [1, 2, 3, 5],
        train: dualTrackTrain,
        existingAnnotations: [],
        annotator: "reviewer",
        authoredAt: authoredAt
    )
    annotations = ManualISILabelDraftEditor.applying(
        label: .highFrequencyBurst,
        toISIIndices: [2, 3, 4],
        train: dualTrackTrain,
        existingAnnotations: annotations,
        annotator: "reviewer",
        authoredAt: authoredAt.addingTimeInterval(1)
    )

    var rows = ManualISIDualTrackProjector.project(
        train: dualTrackTrain,
        annotations: annotations
    ).rows
    let second = try #require(rows.first { $0.isiIndex == 2 })
    #expect(second.statePattern == ManualAnnotationLabel.highFrequencySpiking.rawValue)
    #expect(second.eventPattern == ManualAnnotationLabel.highFrequencyBurst.rawValue)
    #expect(rows.first { $0.isiIndex == 4 }?.statePattern == "")
    #expect(rows.first { $0.isiIndex == 5 }?.eventPattern == "")

    annotations = ManualISILabelDraftEditor.applying(
        label: .other,
        toISIIndices: [3],
        train: dualTrackTrain,
        existingAnnotations: annotations,
        annotator: "reviewer",
        authoredAt: authoredAt.addingTimeInterval(2)
    )
    rows = ManualISIDualTrackProjector.project(
        train: dualTrackTrain,
        annotations: annotations
    ).rows
    let third = try #require(rows.first { $0.isiIndex == 3 })
    #expect(third.statePattern.isEmpty)
    #expect(third.eventPattern.isEmpty)
    #expect(third.otherPattern == ManualAnnotationLabel.other.rawValue)

    annotations = ManualISILabelDraftEditor.clearing(
        track: .event,
        atISIIndices: [2],
        train: dualTrackTrain,
        existingAnnotations: annotations,
        annotator: "reviewer"
    )
    rows = ManualISIDualTrackProjector.project(
        train: dualTrackTrain,
        annotations: annotations
    ).rows
    let cleared = try #require(rows.first { $0.isiIndex == 2 })
    #expect(cleared.eventPattern.isEmpty)
    #expect(cleared.statePattern == ManualAnnotationLabel.highFrequencySpiking.rawValue)
}

@Test("Manual dual-track CSV contains every real ISI and explicit unsealed standing")
func manualDualTrackCSVContract() {
    let projection = ManualISIDualTrackProjector.project(
        train: dualTrackTrain,
        annotations: [
            mark(.tonic, 1, 5, updatedAt: 1),
            mark(.pause, 3, 3, updatedAt: 2),
        ]
    )
    let csv = ManualISIDualTrackCSVExporter.csv(rows: projection.rows)
    let lines = csv.split(separator: "\n", omittingEmptySubsequences: true)

    #expect(lines.count == dualTrackTrain.timestampsSec.count)
    #expect(lines[0].contains("state_pattern,event_pattern,other_pattern"))
    #expect(lines[3].contains("tonic,pause,"))
    #expect(lines.allSatisfy {
        $0 == lines[0] || $0.contains(ManualISIDualTrackProjector.localDraftAuthorityStatus)
    })
}
