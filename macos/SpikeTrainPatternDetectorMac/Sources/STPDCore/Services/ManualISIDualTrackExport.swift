import Foundation

/// One real ISI in the manual-label workbench/export. State and event are separate columns so an
/// embedded event never destroys its surrounding state label. This table is intentionally independent
/// of detector authority and carries an explicit unsealed status until canonical activation is built.
public struct ManualISIDualTrackExportRow: Identifiable, Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let isiIndex: Int
    public let leftSpikeArrayIndex: Int
    public let rightSpikeArrayIndex: Int
    public let leftTimestampSec: Double
    public let rightTimestampSec: Double
    public let isiSec: Double
    public let statePattern: String
    public let eventPattern: String
    public let otherPattern: String
    public let stateAnnotationID: String
    public let eventAnnotationID: String
    public let otherAnnotationID: String
    public let burstVeto: Bool
    public let reviewNote: String
    public let authorityStatus: String

    public var id: String { "\(trainID)\u{1}\(isiIndex)" }

    public init(
        trainID: String,
        trainName: String,
        isiIndex: Int,
        leftSpikeArrayIndex: Int,
        rightSpikeArrayIndex: Int,
        leftTimestampSec: Double,
        rightTimestampSec: Double,
        isiSec: Double,
        statePattern: String,
        eventPattern: String,
        otherPattern: String,
        stateAnnotationID: String,
        eventAnnotationID: String,
        otherAnnotationID: String,
        burstVeto: Bool,
        reviewNote: String,
        authorityStatus: String
    ) {
        self.trainID = trainID
        self.trainName = trainName
        self.isiIndex = isiIndex
        self.leftSpikeArrayIndex = leftSpikeArrayIndex
        self.rightSpikeArrayIndex = rightSpikeArrayIndex
        self.leftTimestampSec = leftTimestampSec
        self.rightTimestampSec = rightTimestampSec
        self.isiSec = isiSec
        self.statePattern = statePattern
        self.eventPattern = eventPattern
        self.otherPattern = otherPattern
        self.stateAnnotationID = stateAnnotationID
        self.eventAnnotationID = eventAnnotationID
        self.otherAnnotationID = otherAnnotationID
        self.burstVeto = burstVeto
        self.reviewNote = reviewNote
        self.authorityStatus = authorityStatus
    }
}

public struct ManualISIDualTrackProjection: Hashable, Sendable {
    public let rows: [ManualISIDualTrackExportRow]
    public let skippedAnnotationCount: Int

    public init(rows: [ManualISIDualTrackExportRow], skippedAnnotationCount: Int) {
        self.rows = rows
        self.skippedAnnotationCount = skippedAnnotationCount
    }
}

/// Pure projection from range annotations to a dual-track per-ISI table. Overlap resolution is
/// deterministic last-edit-wins WITHIN a track; edits on another track never erase it.
public enum ManualISIDualTrackProjector {
    public static let localDraftAuthorityStatus = "local_manual_draft_unsealed"

    private struct Assignment {
        let label: ManualAnnotationLabel
        let annotationID: UUID
    }

    public static func project(
        dataset: SpikeDataset,
        annotationsByTrain: [String: [ManualAnnotation]]
    ) -> ManualISIDualTrackProjection {
        var rows: [ManualISIDualTrackExportRow] = []
        var skipped = 0
        for train in dataset.trains {
            let projection = project(
                train: train,
                annotations: annotationsByTrain[train.id] ?? []
            )
            rows.append(contentsOf: projection.rows)
            skipped += projection.skippedAnnotationCount
        }
        return ManualISIDualTrackProjection(
            rows: rows,
            skippedAnnotationCount: skipped
        )
    }

    public static func project(
        train: SpikeTrain,
        annotations: [ManualAnnotation]
    ) -> ManualISIDualTrackProjection {
        let canonical = ManualAnnotationProjector.canonicalizedAnnotations(annotations)
        var skipped = annotations.count - canonical.count
        var stateByISI: [Int: Assignment] = [:]
        var eventByISI: [Int: Assignment] = [:]
        var otherByISI: [Int: Assignment] = [:]
        var burstVetoByISI: [Int: UUID] = [:]

        for annotation in canonical {
            guard annotation.trainID == train.id else {
                skipped += 1
                continue
            }
            let geometry = ManualAnnotationGeometryResolver.resolve(
                annotation: annotation,
                in: train
            )
            guard geometry.isWithinTrain,
                  let covered = geometry.coveredISIIndices else {
                skipped += 1
                continue
            }
            let validIndices = covered.filter {
                $0 > 0 && $0 < train.timestampsSec.count
            }
            guard !validIndices.isEmpty else {
                skipped += 1
                continue
            }

            if annotation.label == .notBurst {
                for index in validIndices {
                    burstVetoByISI[index] = annotation.id
                }
                continue
            }
            guard annotation.label.polarity == .positive else {
                continue
            }
            let assignment = Assignment(
                label: annotation.label,
                annotationID: annotation.id
            )
            for index in validIndices {
                switch annotation.label.semanticTrack {
                case .state: stateByISI[index] = assignment
                case .event: eventByISI[index] = assignment
                case .other: otherByISI[index] = assignment
                }
            }
        }

        var rows: [ManualISIDualTrackExportRow] = []
        rows.reserveCapacity(max(0, train.timestampsSec.count - 1))
        if train.timestampsSec.count >= 2 {
            for index in 1..<train.timestampsSec.count {
                let state = stateByISI[index]
                let event = eventByISI[index]
                let other = otherByISI[index]
                let hasVeto = burstVetoByISI[index] != nil
                let burstConflict = hasVeto && event.map {
                    ManualAnnotationProjector.burstFamilyLabels.contains(
                        $0.label.rawValue
                    )
                } == true
                rows.append(ManualISIDualTrackExportRow(
                    trainID: train.id,
                    trainName: train.name,
                    isiIndex: index,
                    leftSpikeArrayIndex: index - 1,
                    rightSpikeArrayIndex: index,
                    leftTimestampSec: train.timestampsSec[index - 1],
                    rightTimestampSec: train.timestampsSec[index],
                    isiSec: train.timestampsSec[index] - train.timestampsSec[index - 1],
                    statePattern: state?.label.rawValue ?? "",
                    eventPattern: event?.label.rawValue ?? "",
                    otherPattern: other?.label.rawValue ?? "",
                    stateAnnotationID: state?.annotationID.uuidString ?? "",
                    eventAnnotationID: event?.annotationID.uuidString ?? "",
                    otherAnnotationID: other?.annotationID.uuidString ?? "",
                    burstVeto: hasVeto,
                    reviewNote: burstConflict
                        ? "manual_burst_and_not_burst_overlap"
                        : "",
                    authorityStatus: localDraftAuthorityStatus
                ))
            }
        }
        return ManualISIDualTrackProjection(
            rows: rows,
            skippedAnnotationCount: skipped
        )
    }
}

/// Deterministic editor used by the macOS workbench. It materializes the current dual-track result,
/// applies one bulk edit, then rebuilds minimal contiguous range annotations. Existing negative veto
/// evidence is preserved verbatim.
public enum ManualISILabelDraftEditor {
    public static func applying(
        label: ManualAnnotationLabel,
        toISIIndices selected: Set<Int>,
        train: SpikeTrain,
        existingAnnotations: [ManualAnnotation],
        annotator: String?,
        authoredAt: Date = Date()
    ) -> [ManualAnnotation] {
        guard label.polarity == .positive else {
            return existingAnnotations
        }
        var maps = positiveMaps(train: train, annotations: existingAnnotations)
        let valid = selected.filter { $0 > 0 && $0 < train.timestampsSec.count }
        for index in valid {
            if label.semanticTrack == .other {
                maps[.state]?.removeValue(forKey: index)
                maps[.event]?.removeValue(forKey: index)
            } else {
                maps[.other]?.removeValue(forKey: index)
            }
            maps[label.semanticTrack, default: [:]][index] = label
        }
        return rebuild(
            maps: maps,
            train: train,
            negativeAnnotations: existingAnnotations.filter {
                $0.label.polarity == .negative
            },
            annotator: annotator,
            authoredAt: authoredAt
        )
    }

    public static func clearing(
        track: ManualAnnotationSemanticTrack,
        atISIIndices selected: Set<Int>,
        train: SpikeTrain,
        existingAnnotations: [ManualAnnotation],
        annotator: String?,
        authoredAt: Date = Date()
    ) -> [ManualAnnotation] {
        var maps = positiveMaps(train: train, annotations: existingAnnotations)
        for index in selected {
            maps[track]?.removeValue(forKey: index)
        }
        return rebuild(
            maps: maps,
            train: train,
            negativeAnnotations: existingAnnotations.filter {
                $0.label.polarity == .negative
            },
            annotator: annotator,
            authoredAt: authoredAt
        )
    }

    private static func positiveMaps(
        train: SpikeTrain,
        annotations: [ManualAnnotation]
    ) -> [ManualAnnotationSemanticTrack: [Int: ManualAnnotationLabel]] {
        let rows = ManualISIDualTrackProjector.project(
            train: train,
            annotations: annotations
        ).rows
        var maps: [ManualAnnotationSemanticTrack: [Int: ManualAnnotationLabel]] = [:]
        for row in rows {
            if let label = ManualAnnotationLabel(rawValue: row.statePattern) {
                maps[.state, default: [:]][row.isiIndex] = label
            }
            if let label = ManualAnnotationLabel(rawValue: row.eventPattern) {
                maps[.event, default: [:]][row.isiIndex] = label
            }
            if let label = ManualAnnotationLabel(rawValue: row.otherPattern) {
                maps[.other, default: [:]][row.isiIndex] = label
            }
        }
        return maps
    }

    private static func rebuild(
        maps: [ManualAnnotationSemanticTrack: [Int: ManualAnnotationLabel]],
        train: SpikeTrain,
        negativeAnnotations: [ManualAnnotation],
        annotator: String?,
        authoredAt: Date
    ) -> [ManualAnnotation] {
        var rebuilt = ManualAnnotationProjector.canonicalizedAnnotations(
            negativeAnnotations
        )
        for track in ManualAnnotationSemanticTrack.allCases {
            let assignments = maps[track] ?? [:]
            let ordered = assignments.keys.sorted()
            var cursor = 0
            while cursor < ordered.count {
                let start = ordered[cursor]
                guard let label = assignments[start] else {
                    cursor += 1
                    continue
                }
                var end = start
                cursor += 1
                while cursor < ordered.count,
                      ordered[cursor] == end + 1,
                      assignments[ordered[cursor]] == label {
                    end = ordered[cursor]
                    cursor += 1
                }
                guard start > 0, end < train.timestampsSec.count else {
                    continue
                }
                rebuilt.append(ManualAnnotation(
                    trainID: train.id,
                    label: label,
                    startSec: train.timestampsSec[start - 1],
                    endSec: train.timestampsSec[end],
                    startISIIndex: start,
                    endISIIndex: end,
                    startSpikeIndex: start - 1,
                    endSpikeIndex: end,
                    note: "authored_in_manual_isi_workbench",
                    annotator: annotator,
                    annotatorIdentitySource: annotator.map { _ in .localAccount },
                    createdAt: authoredAt,
                    updatedAt: authoredAt
                ))
            }
        }
        return rebuilt
    }
}

public enum ManualISIDualTrackCSVExporter {
    public static let headers = [
        "train_id", "train_name", "isi_index",
        "left_spike_array_index", "right_spike_array_index",
        "left_timestamp_sec", "right_timestamp_sec", "isi_sec",
        "state_pattern", "event_pattern", "other_pattern",
        "state_annotation_id", "event_annotation_id", "other_annotation_id",
        "burst_veto", "review_note", "authority_status",
    ]

    public static func csv(rows: [ManualISIDualTrackExportRow]) -> String {
        table(rows: rows).csv()
    }

    public static func table(rows: [ManualISIDualTrackExportRow]) -> ManualISIExportTable {
        ManualISIExportTable(headers: headers, rows: rows.map { row in
            [
                row.trainID, row.trainName, String(row.isiIndex),
                String(row.leftSpikeArrayIndex), String(row.rightSpikeArrayIndex),
                number(row.leftTimestampSec), number(row.rightTimestampSec),
                number(row.isiSec), row.statePattern, row.eventPattern,
                row.otherPattern, row.stateAnnotationID, row.eventAnnotationID,
                row.otherAnnotationID, row.burstVeto ? "true" : "false",
                row.reviewNote, row.authorityStatus,
            ]
        })
    }

    private static func number(_ value: Double) -> String {
        value.isFinite ? String(format: "%.12g", value) : ""
    }

}
