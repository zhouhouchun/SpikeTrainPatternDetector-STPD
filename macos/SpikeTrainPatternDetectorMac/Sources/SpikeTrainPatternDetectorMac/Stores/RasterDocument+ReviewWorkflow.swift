import AppKit
import Foundation
import STPDCore
import UniformTypeIdentifiers

extension RasterDocument {
    var reviewableCandidates: [ClassicAnchorCandidate] {
        guard let run = classicAnchorDetectionRun else { return [] }
        let annotationIDs = Set(classicAnchorCandidateAuditAnnotations.map(\.candidateID))
        return run.candidates.filter { candidate in
            candidate.selectedForAuto
                && annotationIDs.contains(candidate.id)
                && [.event, .gap, .state, .review].contains(candidate.auditRecommendedTrack)
        }
    }

    private func reviewQueueItems() -> [ClassicAnchorReviewQueueItem] {
        guard let dataset else { return [] }
        let trains = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        let minimumValid = max(qualitySettings.artifactThresholdSec, 1e-9)
        return reviewableCandidates.map { candidate in
            let burst = ClassicAnchorReviewQueue.burstFamilyLabels.contains(candidate.finalLabel)
            let pause = candidate.finalLabel == .pause
            let representative: Double? = {
                guard (burst || pause), let train = trains[candidate.trainID] else { return nil }
                return ClassicAnchorReviewQueue.representativeISISec(
                    train: train,
                    startISIIndex: candidate.startISIIndex,
                    endISIIndex: candidate.endISIIndex,
                    minValidISISec: minimumValid,
                    preferMaximum: burst
                )
            }()
            return ClassicAnchorReviewQueueItem(
                id: candidate.id,
                trainID: candidate.trainID,
                trainName: candidate.trainName,
                label: candidate.finalLabel,
                isStrongCandidate: candidate.anchorLockLevel == .strongCandidate,
                priority: candidate.priority,
                startISIIndex: candidate.startISIIndex,
                rank: reviewQueueRank(candidate),
                representativeISISec: representative
            )
        }
    }

    private func reviewQueueRank(_ candidate: ClassicAnchorCandidate) -> Int {
        switch reviewStatus(for: candidate.id) {
        case .needsReview: return 0
        case .unreviewed where candidate.auditReviewRequired: return 1
        case .unreviewed where candidate.auditRecommendedTrack == .review: return 2
        case .unreviewed where candidate.anchorLockLevel == .strongCandidate: return 3
        case .unreviewed: return 4
        case .accepted, .rejected: return 5
        }
    }

    func reviewQueue(channel: ClassicAnchorReviewChannel? = nil) -> [ClassicAnchorReviewQueueItem] {
        ClassicAnchorReviewQueue.orderedItems(
            reviewQueueItems(),
            channel: channel ?? activeReviewChannel
        )
    }

    func reviewQueueSummary(channel: ClassicAnchorReviewChannel? = nil) -> ClassicAnchorReviewQueueSummary {
        ClassicAnchorReviewQueue.summary(
            reviewQueueItems(),
            channel: channel ?? activeReviewChannel,
            isOpen: { item in
                let status = reviewStatus(for: item.id)
                return status == .unreviewed || status == .needsReview
            }
        )
    }

    func activeReviewChannelPosition() -> (current: Int, total: Int) {
        let queue = reviewQueue()
        guard let id = focusedClassicAnchorCandidateID,
              let index = queue.firstIndex(where: { $0.id == id }) else {
            return (0, queue.count)
        }
        return (index + 1, queue.count)
    }

    func focusFirstOrNextInActiveReviewChannel() {
        let queue = reviewQueue()
        guard !queue.isEmpty else {
            statusMessage = "当前审核类别中没有结构候选。"
            return
        }
        guard let focused = focusedClassicAnchorCandidateID,
              let index = queue.firstIndex(where: { $0.id == focused }) else {
            let firstOpen = queue.first {
                let status = reviewStatus(for: $0.id)
                return status == .unreviewed || status == .needsReview
            }
            focusClassicAnchorCandidate((firstOpen ?? queue[0]).id)
            return
        }
        focusClassicAnchorCandidate(queue[(index + 1) % queue.count].id)
    }

    func focusAdjacentReviewCandidate(forward: Bool) {
        let queue = reviewQueue()
        guard !queue.isEmpty else { return }
        guard let focused = focusedClassicAnchorCandidateID,
              let index = queue.firstIndex(where: { $0.id == focused }) else {
            focusClassicAnchorCandidate(queue[0].id)
            return
        }
        let next = forward ? (index + 1) % queue.count : (index - 1 + queue.count) % queue.count
        focusClassicAnchorCandidate(queue[next].id)
    }

    func batchAcceptTargetCount() -> Int {
        guard let focused = focusedClassicAnchorCandidate else { return 0 }
        return ClassicAnchorReviewQueue.batchAcceptTargetIDs(
            reviewQueueItems(),
            trainID: focused.trainID,
            channel: activeReviewChannel,
            isAcceptable: {
                let status = reviewStatus(for: $0.id)
                return status == .unreviewed || status == .needsReview
            }
        ).count
    }

    func batchAcceptCurrentTrainAndChannel() {
        guard let focused = focusedClassicAnchorCandidate else { return }
        let ids = ClassicAnchorReviewQueue.batchAcceptTargetIDs(
            reviewQueueItems(),
            trainID: focused.trainID,
            channel: activeReviewChannel,
            isAcceptable: {
                let status = reviewStatus(for: $0.id)
                return status == .unreviewed || status == .needsReview
            }
        )
        guard !ids.isEmpty else {
            statusMessage = "当前 train 与审核类别中没有待接受候选。"
            return
        }
        for id in ids { setReviewStatus(.accepted, for: id) }
        statusMessage = "已接受 (ids.count) 个候选；未改变拒绝项、手工标记或检测结果。"
    }

    func autoLabelsByISI(forTrainID trainID: String) -> [Int: String] {
        var labels: [Int: String] = [:]
        for annotation in classicAnchorRawEventAnnotations where annotation.trainID == trainID {
            guard annotation.startISISecIndex <= annotation.endISISecIndex else { continue }
            for isi in annotation.startISISecIndex...annotation.endISISecIndex {
                labels[isi] = annotation.label.rawValue
            }
        }
        return labels
    }

    func manualAnnotationProjection(forTrainID trainID: String) -> ManualAnnotationProjection? {
        guard let train = dataset?.trains.first(where: { $0.id == trainID }) else { return nil }
        return ManualAnnotationProjector.project(
            train: train,
            autoLabelsByISI: autoLabelsByISI(forTrainID: trainID),
            annotations: manualAnnotationsByTrain[trainID] ?? [],
            honorManualLock: true,
            manualNegativeLabelsEnabled: true,
            minValidISISeconds: qualitySettings.artifactThresholdSec
        )
    }

    func reviewedFinalISIExportRows() -> [ReviewedISIExportRow] {
        guard let dataset else { return [] }
        var projections: [String: ManualAnnotationProjection] = [:]
        for train in dataset.trains {
            if let projection = manualAnnotationProjection(forTrainID: train.id), projection.hasManualEffect {
                projections[train.id] = projection
            }
        }
        let rejected = Set(classicAnchorReviewStatuses.compactMap {
            $0.value == .rejected ? $0.key : nil
        })
        return ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: classicAnchorRawEventAnnotations,
            projectionsByTrain: projections,
            reviewRejectedCandidateIDs: rejected
        )
    }

    var canExportReviewedISIDraft: Bool {
        guard let dataset else { return false }
        return classicAnchorDetectionRun != nil && dataset.trains.contains {
            $0.timestampsSec.count >= 2
        }
    }

    func exportReviewedISIDraftCSVWithPanel() {
        guard let dataset, canExportReviewedISIDraft else {
            statusMessage = "请先运行检测，或改用手工 ISI 草稿导出。"
            return
        }
        let panel = NSSavePanel()
        panel.allowedContentTypes = [.commaSeparatedText]
        panel.canCreateDirectories = true
        panel.isExtensionHidden = false
        let stem = dataset.name.replacingOccurrences(
            of: "[^A-Za-z0-9._-]+", with: "_", options: .regularExpression
        )
        panel.nameFieldStringValue = "\(stem.isEmpty ? "dataset" : stem)_reviewed_isi_DRAFT.csv"
        panel.message = "导出逐 ISI 的自动标签和人工审核后标签。该文件是未封存的探索性草稿，不代表权威科学结果。"
        guard panel.runModal() == .OK, let url = panel.url else { return }
        do {
            let csv = ReviewedISIExportCSVExporter.csv(rows: reviewedFinalISIExportRows())
            try csv.write(to: url, atomically: true, encoding: .utf8)
            statusMessage = "已导出逐 ISI 审核草稿：\(url.lastPathComponent)。"
            lastErrorMessage = nil
        } catch {
            statusMessage = "逐 ISI 审核草稿导出失败。"
            lastErrorMessage = error.localizedDescription
        }
    }
}
