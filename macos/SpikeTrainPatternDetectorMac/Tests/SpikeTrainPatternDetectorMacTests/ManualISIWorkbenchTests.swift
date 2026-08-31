import AppKit
import Foundation
@testable import STPDCore
@testable import SpikeTrainPatternDetectorMac
import Testing

@Suite("Manual ISI workbench")
@MainActor
struct ManualISIWorkbenchTests {
    @Test("Threshold assistant keeps a separate editable draft for each pattern")
    func thresholdAssistantDraftsDoNotLeakAcrossPatterns() {
        var drafts = ManualISIThresholdPatternDrafts()

        drafts[.burst].minimumISIMilliseconds = "1"
        drafts[.burst].maximumISIMilliseconds = "20"
        drafts[.burst].minimumSpikes = "4"
        drafts[.burst].maximumSpikes = "12"
        drafts[.burst].usesBurstEdgeContrast = true
        drafts[.burst].minimumBurstEdgeContrast = "4.5"

        #expect(drafts[.pause].minimumISIMilliseconds.isEmpty)
        #expect(drafts[.pause].maximumISIMilliseconds.isEmpty)
        #expect(drafts[.tonic].minimumISIMilliseconds.isEmpty)
        #expect(drafts[.tonic].maximumISIMilliseconds.isEmpty)

        drafts[.pause].minimumISIMilliseconds = "90"
        drafts[.pause].maximumISIMilliseconds = "500"
        drafts[.tonic].minimumISIMilliseconds = "25"
        drafts[.tonic].maximumISIMilliseconds = "80"

        #expect(drafts[.burst].minimumISIMilliseconds == "1")
        #expect(drafts[.burst].maximumISIMilliseconds == "20")
        #expect(drafts[.burst].minimumSpikes == "4")
        #expect(drafts[.burst].maximumSpikes == "12")
        #expect(drafts[.burst].usesBurstEdgeContrast)
        #expect(drafts[.burst].minimumBurstEdgeContrast == "4.5")
        #expect(!drafts[.pause].usesBurstEdgeContrast)
        #expect(!drafts[.tonic].usesBurstEdgeContrast)
        #expect(drafts[.pause].minimumISIMilliseconds == "90")
        #expect(drafts[.pause].maximumISIMilliseconds == "500")
        #expect(drafts[.tonic].minimumISIMilliseconds == "25")
        #expect(drafts[.tonic].maximumISIMilliseconds == "80")
    }

    @Test("Changing the Tonic metric mutates only the Tonic draft")
    func tonicMetricDefaultsStayScopedToTonicDraft() {
        var drafts = ManualISIThresholdPatternDrafts()
        drafts[.burst].minimumSpikes = "4"
        drafts[.burst].maximumSpikes = "10"
        drafts[.pause].minimumISIMilliseconds = "120"

        drafts.selectTonicMetric(.mm)

        #expect(drafts[.tonic].tonicMetric == .mm)
        #expect(drafts[.tonic].minimumSpikes == "3")
        #expect(drafts[.tonic].maximumSpikes == "5")
        #expect(drafts[.tonic].tonicMetricMinimum == "1")
        #expect(drafts[.tonic].tonicMetricMaximum.isEmpty)
        #expect(drafts[.burst].minimumSpikes == "4")
        #expect(drafts[.burst].maximumSpikes == "10")
        #expect(drafts[.pause].minimumISIMilliseconds == "120")
    }

    @Test("Manual workbench track and pattern labels follow the active UI language")
    func manualWorkbenchLabelsFollowActiveLanguage() {
        let english = STPDLocalizer(language: .en)
        let russian = STPDLocalizer(language: .ru)

        #expect(ManualAnnotationSemanticTrack.state.displayName(using: english) == "State")
        #expect(ManualAnnotationSemanticTrack.event.displayName(using: russian) == "Событие")
        #expect(ManualAnnotationSemanticTrack.other.clearTitle(using: russian) == "Очистить: Другое")
        #expect(ManualAnnotationLabel.burst.displayName(using: russian) == "пачек")
        #expect(ManualAnnotationLabel.tonic.displayName(using: russian) == "тоник")
        #expect(ManualAnnotationLabel.pause.displayName(using: russian) == "пауза")
    }

    @Test("Manual ISI table MM uses directional adjacent-ISI ratios")
    func manualISITableMMUsesOwnerDefinedDirectionalFormula() throws {
        let ratios = ManualISINeighborhoodRatios.indexedSeconds([
            (id: "isi_1", interval: 0.100),
            (id: "isi_2", interval: 0.200),
            (id: "isi_3", interval: 0.050),
        ])

        #expect(ratios["isi_1"]?.leftMM == nil)
        #expect(ratios["isi_1"]?.rightMM == 2.0)
        #expect(ratios["isi_2"]?.leftMM == 0.5)
        #expect(ratios["isi_2"]?.rightMM == 0.25)
        #expect(ratios["isi_3"]?.leftMM == 4.0)
        #expect(ratios["isi_3"]?.rightMM == nil)
    }

    @Test("Manual ISI table MM preserves exact integer-neighborhood direction and guards zero divisor")
    func manualISITableMMHandlesCanonicalTicksAndZeroCurrentISI() throws {
        let ratios = ManualISINeighborhoodRatios.indexedMicroseconds([
            (id: 1, interval: 10_000),
            (id: 2, interval: 0),
            (id: 3, interval: 20_000),
        ])

        #expect(ratios[1]?.leftMM == nil)
        #expect(ratios[1]?.rightMM == 0.0)
        #expect(ratios[2]?.leftMM == nil)
        #expect(ratios[2]?.rightMM == nil)
        #expect(ratios[3]?.leftMM == 0.0)
        #expect(ratios[3]?.rightMM == nil)
    }

    @Test("Manual QC geometry marks both endpoints of each invalid ISI")
    func manualQCGeometryMarksBothEndpointSpikes() {
        let rows = [
            ManualISIGraphicRow(
                id: "r1", isiIndex: 1, leftSeconds: 0.0, rightSeconds: 0.001,
                stateLabel: nil, eventLabel: nil, otherLabel: nil,
                isInvalidISI: true
            ),
            ManualISIGraphicRow(
                id: "r2", isiIndex: 2, leftSeconds: 0.001, rightSeconds: 0.010,
                stateLabel: nil, eventLabel: nil, otherLabel: nil,
                isInvalidISI: false
            ),
            ManualISIGraphicRow(
                id: "r3", isiIndex: 3, leftSeconds: 0.010, rightSeconds: 0.0105,
                stateLabel: nil, eventLabel: nil, otherLabel: nil,
                isInvalidISI: true
            ),
        ]

        #expect(ManualISIGraphicQCGeometry.involvedSpikeTimes(in: rows) == [
            0.0, 0.001, 0.010, 0.0105,
        ])
    }

    @Test("Threshold proposal never overwrites an existing same-track label or Other")
    func thresholdProposalProtectsExistingManualLabels() {
        let candidates = [
            ManualISIThresholdCandidate(
                pattern: .burst,
                isiIndices: [1, 2],
                spikeCount: 3
            ),
            ManualISIThresholdCandidate(
                pattern: .burst,
                isiIndices: [3, 4],
                spikeCount: 3
            ),
            ManualISIThresholdCandidate(
                pattern: .burst,
                isiIndices: [5, 6],
                spikeCount: 3
            ),
        ]
        let rows = [
            graphicRow("r1", 1, state: .tonic),
            graphicRow("r2", 2),
            graphicRow("r3", 3, event: .pause),
            graphicRow("r4", 4),
            graphicRow("r5", 5, other: .other),
            graphicRow("r6", 6),
        ]

        let result = ManualISIThresholdProposalResult(
            candidates: candidates,
            rows: rows,
            label: .burst
        )

        // A coexisting state does not block an event. Existing event and Other do.
        #expect(result.eligibleCandidates.map(\.isiIndices) == [[1, 2]])
        #expect(result.eligibleISIIndices == [1, 2])
        #expect(result.blockedCandidateCount == 2)
    }

    @Test("Changing the visible count keeps an explicitly selected manual-label train")
    func visibleCountPreservesExplicitManualTrainSelection() throws {
        let document = RasterDocument()
        let trains = (1...5).map { index in
            SpikeTrain(
                name: "unit_\(index)",
                timestampsSec: [0, 0.01, 0.02]
            )
        }
        document.dataset = SpikeDataset(name: "manual", sourceDescription: "test", trains: trains)

        let manuallyChosenTrainID = trains[3].id
        document.updateSelectedTrainIDs([manuallyChosenTrainID], scope: .raster)
        document.selectVisibleTrainCount(3, scope: .raster)

        #expect(document.selectedTrainIDs.count == 3)
        #expect(document.selectedTrainIDs.contains(manuallyChosenTrainID))

        // A viewport-only change must be selection-neutral during manual authoring.
        document.rasterVisibleWindowSeconds = 0.25
        #expect(document.selectedTrainIDs.contains(manuallyChosenTrainID))

        document.selectVisibleTrainCount(4, scope: .raster)
        #expect(document.selectedTrainIDs.count == 4)
        #expect(document.selectedTrainIDs.contains(manuallyChosenTrainID))
    }

    @Test("Document can author coexisting HFS state and HFB event without a detector run")
    func documentAuthorsDualTrackLabelsWithoutDetector() throws {
        let document = RasterDocument()
        let train = SpikeTrain(name: "unit_a", timestampsSec: [0.0, 0.01, 0.02, 0.03, 0.04])
        document.dataset = SpikeDataset(
            name: "manual",
            sourceDescription: "test",
            trains: [train]
        )

        document.applyManualISILabel(
            .highFrequencySpiking,
            trainID: train.id,
            isiIndices: [1, 2, 3, 4]
        )
        document.applyManualISILabel(
            .highFrequencyBurst,
            trainID: train.id,
            isiIndices: [2, 3]
        )

        let rows = document.manualISIRows(for: train.id)
        #expect(rows.count == 4)
        #expect(rows.allSatisfy { $0.statePattern == "high_frequency_spiking" })
        #expect(rows[0].eventPattern.isEmpty)
        #expect(rows[1].eventPattern == "high_frequency_burst")
        #expect(rows[2].eventPattern == "high_frequency_burst")
        #expect(rows[3].eventPattern.isEmpty)
        #expect(document.classicAnchorDetectionRun == nil)
    }

    @Test("Clearing one track preserves the coexisting track")
    func clearingOneTrackPreservesTheOther() throws {
        let document = makeDocument()
        let train = try #require(document.dataset?.trains.first)
        document.applyManualISILabel(
            .tonic,
            trainID: train.id,
            isiIndices: [1, 2]
        )
        document.applyManualISILabel(
            .pause,
            trainID: train.id,
            isiIndices: [2]
        )

        document.clearManualISILabel(
            track: .event,
            trainID: train.id,
            isiIndices: [2]
        )

        let row = try #require(document.manualISIRows(for: train.id).first { $0.isiIndex == 2 })
        #expect(row.statePattern == "tonic")
        #expect(row.eventPattern.isEmpty)
    }

    @Test("Undo restores the exact previous dual-track manual state")
    func undoRestoresPreviousManualState() throws {
        let document = makeDocument()
        let train = try #require(document.dataset?.trains.first)

        document.applyManualISILabel(
            .tonic,
            trainID: train.id,
            isiIndices: [1, 2]
        )
        document.applyManualISILabel(
            .pause,
            trainID: train.id,
            isiIndices: [2]
        )
        #expect(document.canUndoManualISIEdit)

        document.undoLastManualISIEdit()
        var rows = document.manualISIRows(for: train.id)
        #expect(rows.allSatisfy { $0.statePattern == "tonic" })
        #expect(rows.allSatisfy { $0.eventPattern.isEmpty })

        document.undoLastManualISIEdit()
        rows = document.manualISIRows(for: train.id)
        #expect(rows.allSatisfy { $0.statePattern.isEmpty })
        #expect(rows.allSatisfy { $0.eventPattern.isEmpty })
        #expect(!document.canUndoManualISIEdit)
    }

    @Test("Raster eraser removes only the requested track and can be undone")
    func rasterEraserPreservesCoexistingTrackAndUndo() throws {
        let document = makeDocument()
        let train = try #require(document.dataset?.trains.first)
        document.applyManualISILabel(.tonic, trainID: train.id, isiIndices: [1, 2])
        document.applyManualISILabel(.pause, trainID: train.id, isiIndices: [2])

        document.clearRasterManualISILabel(
            track: .event,
            trainID: train.id,
            isiIndices: [2]
        )
        var row = try #require(document.manualISIRows(for: train.id).first { $0.isiIndex == 2 })
        #expect(row.statePattern == "tonic")
        #expect(row.eventPattern.isEmpty)

        document.undoLastManualISIEdit()
        row = try #require(document.manualISIRows(for: train.id).first { $0.isiIndex == 2 })
        #expect(row.statePattern == "tonic")
        #expect(row.eventPattern == "pause")
    }

    @Test("Canonical identity-bound edits use the same bounded undo path")
    func canonicalManualEditsCanBeUndone() throws {
        let trainID = ScientificSpikeTrainID(try ScientificSemanticID(validating: "unit_a"))
        let groupID = ScientificEventScopeGroupID(
            try ScientificSemanticID(validating: "group_a")
        )
        let dataset = CanonicalScientificDataset(
            recordingSegment: ConfirmedRecordingSegment(
                semanticID: ScientificRecordingSegmentID(
                    try ScientificSemanticID(validating: "recording_a")
                ),
                regime: .continuousUntrialed,
                importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
                observationBounds: .unknownOrUnavailable
            ),
            activityMode: .putativeSingleUnit,
            spikeTrains: [CanonicalSpikeTrain(
                semanticID: trainID,
                rawTimestamps: [
                    MicrosecondTick(microseconds: 0),
                    MicrosecondTick(microseconds: 10_000),
                    MicrosecondTick(microseconds: 20_000),
                ]
            )],
            eventScopeGroups: [CanonicalEventScopeGroup(
                semanticID: groupID,
                timeBasis: .recordingElapsed,
                spikeTrainReferences: [trainID],
                eventDefinitions: []
            )],
            scientificAttributeDefinitions: []
        )
        let fingerprint = try CanonicalScientificDatasetFingerprinter.fingerprint(dataset)
        let document = RasterDocument()
        document.canonicalManualDataset = dataset
        document.canonicalManualISILabelDraft = CanonicalManualISILabelDraft(
            canonicalFingerprint: fingerprint
        )

        document.applyCanonicalManualISILabel(
            .tonic,
            trainID: "unit_a",
            isiIndices: [1, 2]
        )
        #expect(document.canonicalManualISILabelDraft?.decisions.count == 2)
        document.undoLastManualISIEdit()
        #expect(document.canonicalManualISILabelDraft?.decisions.isEmpty == true)
        #expect(!document.canUndoManualISIEdit)
    }

    @Test("Direct export is one row per real ISI and declares draft standing")
    func directExportIsCompleteAndUnsealed() throws {
        let document = makeDocument()
        let train = try #require(document.dataset?.trains.first)
        document.applyManualISILabel(
            .burst,
            trainID: train.id,
            isiIndices: [1, 2]
        )

        let csv = try #require(document.manualISILabelDraftCSV())
        let lines = csv.split(separator: "\n", omittingEmptySubsequences: true)
        #expect(lines.count == train.timestampsSec.count)
        #expect(lines[0].contains("state_pattern,event_pattern,other_pattern"))
        #expect(lines[0].contains("authority_status"))
        #expect(lines.dropFirst().allSatisfy {
            $0.contains(ManualISIDualTrackProjector.localDraftAuthorityStatus)
        })
        #expect(lines[1].contains("burst"))
    }

    @Test("Manual Burst and Tonic require three contiguous spikes, while HFS requires five")
    func manualPatternMinimumSpikeSupportIsEnforced() throws {
        let document = RasterDocument()
        let train = SpikeTrain(
            name: "unit_a",
            timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
        )
        document.dataset = SpikeDataset(name: "manual", sourceDescription: "test", trains: [train])

        // One ISI spans two spikes: insufficient for Burst/Tonic.
        document.applyManualISILabel(.burst, trainID: train.id, isiIndices: [1])
        document.applyManualISILabel(.tonic, trainID: train.id, isiIndices: [1])
        #expect(document.manualAnnotationsAll.isEmpty)

        // Two contiguous ISIs span three spikes and are accepted for Burst/Tonic.
        document.applyManualISILabel(.burst, trainID: train.id, isiIndices: [1, 2])
        document.applyManualISILabel(.tonic, trainID: train.id, isiIndices: [1, 2])
        let supportedRows = document.manualISIRows(for: train.id)
        #expect(supportedRows[0].eventPattern == "burst")
        #expect(supportedRows[1].statePattern == "tonic")

        // Three ISIs span four spikes: still insufficient for HFS.
        document.applyManualISILabel(.highFrequencySpiking, trainID: train.id, isiIndices: [1, 2, 3])
        #expect(supportedRows.allSatisfy { $0.statePattern != "high_frequency_spiking" })

        // Four contiguous ISIs span five spikes and are accepted for HFS.
        document.applyManualISILabel(.highFrequencySpiking, trainID: train.id, isiIndices: [1, 2, 3, 4])
        #expect(document.manualISIRows(for: train.id).prefix(4).allSatisfy {
            $0.statePattern == "high_frequency_spiking"
        })
    }

    @Test("Separated ISI fragments cannot be aggregated to satisfy a manual pattern minimum")
    func manualPatternRequiresContiguousSupport() throws {
        let document = RasterDocument()
        let train = SpikeTrain(
            name: "unit_a",
            timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
        )
        document.dataset = SpikeDataset(name: "manual", sourceDescription: "test", trains: [train])

        document.applyManualISILabel(.highFrequencySpiking, trainID: train.id, isiIndices: [1, 3, 5])

        #expect(document.manualAnnotationsAll.isEmpty)
        #expect(document.statusMessage.contains("至少需要 5 个 spike"))
    }

    @Test("Graphic drag selection resolves the same contiguous ISIs as the exact table")
    func graphicSelectionMatchesExactRows() {
        let rows = [
            ManualISIGraphicRow(
                id: "r1", isiIndex: 1, leftSeconds: 0.10, rightSeconds: 0.20,
                stateLabel: nil, eventLabel: nil, otherLabel: nil
            ),
            ManualISIGraphicRow(
                id: "r2", isiIndex: 2, leftSeconds: 0.20, rightSeconds: 0.35,
                stateLabel: nil, eventLabel: nil, otherLabel: nil
            ),
            ManualISIGraphicRow(
                id: "r3", isiIndex: 3, leftSeconds: 0.35, rightSeconds: 0.80,
                stateLabel: nil, eventLabel: nil, otherLabel: nil
            ),
        ]

        #expect(ManualISIGraphicSelection.selectedIDs(
            in: rows,
            from: 0.22,
            through: 0.70
        ) == ["r2", "r3"])
        #expect(ManualISIGraphicSelection.selectedIDs(
            in: rows,
            from: 0.70,
            through: 0.11
        ) == ["r1", "r2", "r3"])
        #expect(ManualISIGraphicSelection.selectedIDs(
            in: rows,
            from: -10,
            through: -1
        ) == ["r1"])

        #expect(ManualISIGraphicSelection.selectedIndexRanges(
            in: rows,
            selectedRowIDs: ["r1", "r2"]
        ) == [0...1])
        #expect(ManualISIGraphicSelection.selectedIndexRanges(
            in: rows,
            selectedRowIDs: ["r1", "r3"]
        ) == [0...0, 2...2])
    }

    @Test("Threshold-assisted bulk apply writes every eligible run into the timeline label source")
    func thresholdBulkApplyAppearsInManualTimelineRows() throws {
        let document = RasterDocument()
        let train = SpikeTrain(
            name: "unit_threshold",
            timestampsSec: [0, 0.010, 0.020, 0.060, 0.070, 0.080]
        )
        document.dataset = SpikeDataset(
            name: "manual",
            sourceDescription: "test",
            trains: [train]
        )
        let samples = document.manualISIRows(for: train.id).map {
            ManualISIThresholdSample(isiIndex: $0.isiIndex, isiSeconds: $0.isiSec)
        }
        let candidates = try ManualISIThresholdMarker.propose(
            samples: samples,
            rule: ManualISIThresholdRule(
                pattern: .burst,
                isiRangeSeconds: ManualISIThresholdClosedRange(
                    lowerBound: 0.009,
                    upperBound: 0.011
                ),
                minimumSpikeCount: 3,
                maximumSpikeCount: 3
            ),
            minimumValidISISeconds: 0.0009
        )
        #expect(candidates.map(\.isiIndices) == [[1, 2], [4, 5]])

        let applied = document.applyManualISILabel(
            .burst,
            trainID: train.id,
            isiIndices: Set(candidates.flatMap(\.isiIndices))
        )

        #expect(applied)
        let updatedRows = document.manualISIRows(for: train.id)
        #expect(updatedRows.map(\.eventPattern) == ["burst", "burst", "", "burst", "burst"])
        let projectedAnnotations = document.rasterManualAnnotations.filter { $0.label == .burst }
        #expect(projectedAnnotations.count == 2)
        #expect(projectedAnnotations.map { $0.startISIIndex ?? -1 } == [1, 4])
        #expect(projectedAnnotations.map { $0.endISIIndex ?? -1 } == [2, 5])
    }

    @Test("Pattern glass coalesces only adjacent equal labels on the same track")
    func patternGlassCoalescesContiguousRunsPerTrack() {
        let rows = [
            ManualISIGraphicRow(
                id: "r1", isiIndex: 1, leftSeconds: 0.10, rightSeconds: 0.20,
                stateLabel: .tonic, eventLabel: .burst, otherLabel: nil
            ),
            ManualISIGraphicRow(
                id: "r2", isiIndex: 2, leftSeconds: 0.20, rightSeconds: 0.35,
                stateLabel: .tonic, eventLabel: .burst, otherLabel: .other
            ),
            ManualISIGraphicRow(
                id: "r3", isiIndex: 3, leftSeconds: 0.35, rightSeconds: 0.50,
                stateLabel: nil, eventLabel: .pause, otherLabel: .other
            ),
            ManualISIGraphicRow(
                id: "r4", isiIndex: 4, leftSeconds: 0.50, rightSeconds: 0.80,
                stateLabel: .tonic, eventLabel: nil, otherLabel: nil
            ),
        ]

        #expect(ManualISIGraphicPatternGlass.runs(in: rows) == [
            ManualISIGraphicPatternRun(lane: .state, label: .tonic, rowRange: 0...1),
            ManualISIGraphicPatternRun(lane: .state, label: .tonic, rowRange: 3...3),
            ManualISIGraphicPatternRun(lane: .event, label: .burst, rowRange: 0...1),
            ManualISIGraphicPatternRun(lane: .event, label: .pause, rowRange: 2...2),
            ManualISIGraphicPatternRun(lane: .other, label: .other, rowRange: 1...2),
        ])
    }

    @Test("Deleting a clicked pattern run clears only that run and semantic track")
    func deletingClickedPatternRunPreservesOverlappingAndNeighboringPatterns() throws {
        let document = RasterDocument()
        let train = SpikeTrain(
            name: "unit_a",
            timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
        )
        document.dataset = SpikeDataset(name: "manual", sourceDescription: "test", trains: [train])
        document.applyManualISILabel(.tonic, trainID: train.id, isiIndices: [1, 2, 3])
        document.applyManualISILabel(.burst, trainID: train.id, isiIndices: [1, 2])
        document.applyManualISILabel(.pause, trainID: train.id, isiIndices: [4])

        let graphicRows = document.manualISIRows(for: train.id).map {
            ManualISIGraphicRow(
                id: $0.id,
                isiIndex: $0.isiIndex,
                leftSeconds: $0.leftTimestampSec,
                rightSeconds: $0.rightTimestampSec,
                stateLabel: ManualAnnotationLabel(rawValue: $0.statePattern),
                eventLabel: ManualAnnotationLabel(rawValue: $0.eventPattern),
                otherLabel: ManualAnnotationLabel(rawValue: $0.otherPattern)
            )
        }
        let burstRun = try #require(
            ManualISIGraphicPatternGlass.runs(in: graphicRows).first {
                $0.lane == .event && $0.label == .burst
            }
        )
        #expect(burstRun.rowIDs(in: graphicRows) == [graphicRows[0].id, graphicRows[1].id])
        #expect(burstRun.isiIndices(in: graphicRows) == [1, 2])

        document.clearManualISILabel(
            track: burstRun.lane.semanticTrack,
            trainID: train.id,
            isiIndices: burstRun.isiIndices(in: graphicRows)
        )

        let updated = document.manualISIRows(for: train.id)
        #expect(updated[0].eventPattern.isEmpty)
        #expect(updated[1].eventPattern.isEmpty)
        #expect(updated[0].statePattern == "tonic")
        #expect(updated[1].statePattern == "tonic")
        #expect(updated[3].eventPattern == "pause")
    }

    @Test("Delete-key capture recognizes macOS backward and forward Delete")
    func deleteKeyCaptureRecognizesBothMacOSDeleteKeys() {
        #expect(ManualISIDeleteKeyView.shouldHandleDelete(
            keyCode: 51,
            modifierFlags: []
        ))
        #expect(ManualISIDeleteKeyView.shouldHandleDelete(
            keyCode: 117,
            modifierFlags: [.function]
        ))
        #expect(!ManualISIDeleteKeyView.shouldHandleDelete(
            keyCode: 51,
            modifierFlags: [.command]
        ))
        #expect(!ManualISIDeleteKeyView.shouldHandleDelete(
            keyCode: 36,
            modifierFlags: []
        ))
    }

    private func makeDocument() -> RasterDocument {
        let document = RasterDocument()
        document.dataset = SpikeDataset(
            name: "manual",
            sourceDescription: "test",
            trains: [SpikeTrain(name: "unit_a", timestampsSec: [0.1, 0.2, 0.3])]
        )
        return document
    }

    private func graphicRow(
        _ id: String,
        _ index: Int,
        state: ManualAnnotationLabel? = nil,
        event: ManualAnnotationLabel? = nil,
        other: ManualAnnotationLabel? = nil
    ) -> ManualISIGraphicRow {
        ManualISIGraphicRow(
            id: id,
            isiIndex: index,
            leftSeconds: Double(index - 1) * 0.01,
            rightSeconds: Double(index) * 0.01,
            stateLabel: state,
            eventLabel: event,
            otherLabel: other
        )
    }
}
