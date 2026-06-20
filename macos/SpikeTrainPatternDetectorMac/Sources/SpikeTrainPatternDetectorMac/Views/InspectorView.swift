import STPDCore
import SwiftUI

struct InspectorView: View {
    @Bindable var document: RasterDocument

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                if let candidate = document.focusedClassicAnchorCandidate,
                   let annotation = document.focusedClassicAnchorEventAnnotation {
                    focusedEventInspector(candidate: candidate, annotation: annotation)
                } else {
                    emptyEventInspector
                }

                Divider()

                datasetInspector
            }
            .padding(16)
            .frame(maxWidth: .infinity, alignment: .topLeading)
        }
        .background(Color(nsColor: .textBackgroundColor))
        .overlay(alignment: .leading) {
            Rectangle()
                .fill(Color(nsColor: .separatorColor).opacity(0.42))
                .frame(width: 1)
        }
    }

    private func focusedEventInspector(
        candidate: ClassicAnchorCandidate,
        annotation: ClassicAnchorEventAnnotation
    ) -> some View {
        VStack(alignment: .leading, spacing: 14) {
            header(candidate: candidate)
            reviewControls(candidate: candidate)
            eventSummary(candidate: candidate, annotation: annotation)
            candidateAudit(candidate: candidate)
            hfsBurstArbitrationAudit(candidate: candidate)
            decisionPath(candidate.decisionPath)
        }
    }

    private func header(candidate: ClassicAnchorCandidate) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 8) {
                Text("Event Inspector")
                    .font(.headline)
                Spacer()
                statusBadge(document.reviewStatus(for: candidate.id))
            }

            Text(candidate.trainName)
                .font(.subheadline.weight(.semibold))
                .lineLimit(2)
                .truncationMode(.middle)

            HStack(spacing: 7) {
                capsule(candidate.finalLabel.rawValue, tint: labelColor(candidate.finalLabel))
                capsule(candidate.anchorLockLevel == .lockedClassic ? "locked classic" : "strong candidate", tint: lockColor(candidate.anchorLockLevel))
            }
        }
    }

    private func reviewControls(candidate: ClassicAnchorCandidate) -> some View {
        let candidateID = candidate.id
        let status = document.reviewStatus(for: candidateID)
        return VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 8) {
                Text("Manual review")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Spacer()
                statusBadge(status)
            }

            HStack(spacing: 8) {
                reviewButton(.accepted, candidateID: candidateID)
                reviewButton(.needsReview, candidateID: candidateID)
                reviewButton(.rejected, candidateID: candidateID)
            }

            Text(status.inspectorDescription)
                .font(.caption2)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)

            HStack(spacing: 8) {
                Button {
                    document.setReviewStatus(.unreviewed, for: candidateID)
                } label: {
                    Label("Clear review", systemImage: "circle")
                }
                .liquidGlassButtonStyle()
                .disabled(document.reviewStatus(for: candidateID) == .unreviewed)

                Button {
                    document.focusClassicAnchorCandidate(candidateID)
                } label: {
                    Label("Center", systemImage: "scope")
                }
                .liquidGlassButtonStyle()

                Button {
                    document.clearClassicAnchorFocus()
                } label: {
                    Label("Exit review", systemImage: "xmark.circle")
                }
                .liquidGlassButtonStyle()
            }
        }
        .padding(12)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private func reviewButton(_ status: ClassicAnchorReviewStatus, candidateID: String) -> some View {
        let isSelected = document.reviewStatus(for: candidateID) == status
        return Button {
            document.setReviewStatusAndAdvance(status, for: candidateID)
        } label: {
            Label(status.shortTitle, systemImage: status.systemImage)
                .lineLimit(1)
        }
        .liquidGlassButtonStyle(prominent: isSelected)
        .foregroundStyle(isSelected ? Color.white : status.tint)
    }

    private func eventSummary(
        candidate: ClassicAnchorCandidate,
        annotation: ClassicAnchorEventAnnotation
    ) -> some View {
        VStack(alignment: .leading, spacing: 10) {
            sectionTitle("Event metrics")

            Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 7) {
                metricRow("Raw start", TimeFormatting.seconds(annotation.rawStartSec))
                metricRow("Raw end", TimeFormatting.seconds(annotation.rawEndSec))
                metricRow("Aligned start", TimeFormatting.seconds(annotation.alignedStartSec))
                metricRow("Aligned end", TimeFormatting.seconds(annotation.alignedEndSec))
                metricRow("Duration", time(candidate.durationSec ?? annotation.durationSec))
                metricRow("Spike span", "\(candidate.startSpikeIndex)-\(candidate.endSpikeIndex)")
                metricRow("ISI span", "\(candidate.startISIIndex)-\(candidate.endISIIndex)")
                metricRow("Spikes", "\(candidate.nSpikes)")
                metricRow("Band", "\(time(candidate.anchorBandLowerSec)) - \(time(candidate.anchorBandUpperSec))")
                metricRow("Pre gap", time(candidate.preGapSec))
                metricRow("Post gap", time(candidate.postGapSec))
                metricRow("Edge contrast", number(candidate.edgeContrastMinQ90))
                metricRow("Score", number(candidate.score))
                metricRow("Refractory", "\(candidate.refractorySuspectCount)")
            }
        }
        .padding(12)
        .background(Color(nsColor: .controlBackgroundColor).opacity(0.72), in: RoundedRectangle(cornerRadius: 8))
    }

    private func candidateAudit(candidate: ClassicAnchorCandidate) -> some View {
        let manualStatus = document.reviewStatus(for: candidate.id)
        return VStack(alignment: .leading, spacing: 10) {
            sectionTitle("Candidate audit")

            Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 7) {
                metricRow("Manual review", manualStatus.title)
                metricRow("Auto selected", candidate.selectedForAuto ? "Yes" : "No")
                metricRow("Selection", candidate.selectionStatus)
                metricRow("Gate", candidate.gateStatus)
                metricRow("Action", candidate.action)
                metricRow("Why", candidate.auditReasonSummary)
                metricRow("Failure", candidate.failureReason.isEmpty ? "None" : candidate.failureReason)
                metricRow("Track", candidate.auditRecommendedTrackRawValue)
                metricRow("Family", candidate.auditRecommendedFamily)
                metricRow("Subtype", candidate.auditRecommendedSubtype)
                metricRow("Final class", candidate.auditRecommendedFinalClass)
                metricRow("Event track", candidate.auditRecommendedEventTrackClass)
                metricRow("Algorithm audit", candidate.auditReviewStatus)
                metricRow("Algorithm review", candidate.auditReviewRequired ? "Required" : "No")
                metricRow("Confidence", candidate.auditConfidenceTier)
                if !candidate.auditUncertaintyReason.isEmpty {
                    metricRow("Uncertainty", candidate.auditUncertaintyReason)
                }
                if !candidate.reviewEvidenceClass.isEmpty {
                    metricRow("Review evidence", candidate.reviewEvidenceClass)
                }
                if !candidate.reviewEvidenceStrength.isEmpty {
                    metricRow("Review strength", candidate.reviewEvidenceStrength)
                }
                if !candidate.reviewEvidenceSummary.isEmpty {
                    metricRow("Review summary", candidate.reviewEvidenceSummary)
                }
                if !candidate.possibleBurstStructureSummary.isEmpty {
                    metricRow("Possible burst structure", candidate.possibleBurstStructureSummary)
                }
                if !candidate.auditLongBurstDefinitionStatus.isEmpty {
                    metricRow("Long burst status", candidate.auditLongBurstDefinitionStatus)
                }
                metricRow("Layer", candidate.candidateLayer)
                metricRow("Class", candidate.candidateClass)
                metricRow("Diagnostic", candidate.candidateDiagnosticClass)
                if candidate.suppressedByHFSpikingState == true {
                    metricRow("Suppressed original", candidate.suppressedOriginalLabel ?? "NA")
                    metricRow("Suppressor", candidate.hfSpikingSuppressorID ?? "NA")
                }
            }
        }
        .padding(12)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    @ViewBuilder
    private func hfsBurstArbitrationAudit(candidate: ClassicAnchorCandidate) -> some View {
        let rows = hfsBurstAuditRows(for: candidate)
        if !rows.isEmpty {
            VStack(alignment: .leading, spacing: 10) {
                sectionTitle("HFS-burst arbitration")

                ForEach(rows.prefix(3)) { row in
                    VStack(alignment: .leading, spacing: 8) {
                        Text(row.finalDecision.rawValue)
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(row.requiresReview ? .orange : .secondary)
                            .textSelection(.enabled)

                        Text(row.compactSummary)
                            .font(.caption)
                            .foregroundStyle(.secondary)
                            .textSelection(.enabled)
                            .fixedSize(horizontal: false, vertical: true)

                        Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 6) {
                            metricRow("HFS selected", row.hfsSelectedForAuto ? "Yes" : "No")
                            metricRow("HFS status", row.hfsSelectionStatus)
                            metricRow("Packets", "\(row.packetCount)")
                            metricRow("Packet coverage", percent(row.packetCoverage))
                            metricRow("Pause-like breaks", "\(row.pauseLikeBreakCount)")
                            metricRow("Seed fraction", percent(row.seedFraction))
                            metricRow("Bridge fraction", percent(row.bridgeFraction))
                            metricRow("Review", row.requiresReview ? "Recommended" : "No")
                        }
                    }
                    .padding(10)
                    .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
                }

                if rows.count > 3 {
                    Text("\(rows.count - 3) additional audit row(s) overlap this candidate. Use the HFS-burst audit CSV export for the full table.")
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
            }
            .padding(12)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
        }
    }

    private func decisionPath(_ path: String) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            sectionTitle("Decision path")
            Text(path)
                .font(.caption)
                .foregroundStyle(.secondary)
                .textSelection(.enabled)
                .fixedSize(horizontal: false, vertical: true)
        }
        .padding(12)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private var emptyEventInspector: some View {
        VStack(alignment: .leading, spacing: 10) {
            Text("Event Inspector")
                .font(.headline)
            Text("Select a structural candidate to inspect timestamps, detection metrics, and manual review state.")
                .font(.callout)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)
        }
        .padding(12)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private var datasetInspector: some View {
        VStack(alignment: .leading, spacing: 12) {
            Text("Dataset")
                .font(.headline)

            if let dataset = document.dataset {
                Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 7) {
                    metricRow("Trains", "\(dataset.trains.count)")
                    metricRow("Spikes", dataset.totalSpikeCount.formatted())
                    metricRow("Aligned", TimeFormatting.seconds(dataset.maxAlignedDurationSec))
                    metricRow("Raw span", TimeFormatting.seconds(dataset.rawDurationSec))
                    metricRow("Raster visible", "\(visibleTrains(in: dataset).count)")
                    metricRow("ISI visible", "\(isiVisibleTrains(in: dataset).count)")
                }

                Divider()

                Text("Source")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Text(dataset.sourceDescription)
                    .font(.caption)
                    .textSelection(.enabled)
                    .lineLimit(8)
            } else {
                Text(document.lastErrorMessage ?? "No dataset loaded.")
                    .foregroundStyle(.secondary)
            }
        }
    }

    private func metricRow(_ title: String, _ value: String) -> some View {
        GridRow {
            Text(title)
                .foregroundStyle(.secondary)
            Text(value)
                .monospacedDigit()
                .textSelection(.enabled)
        }
    }

    private func statusBadge(_ status: ClassicAnchorReviewStatus) -> some View {
        Label(status.title, systemImage: status.systemImage)
            .font(.caption.weight(.semibold))
            .foregroundStyle(status.tint)
            .padding(.horizontal, 8)
            .padding(.vertical, 3)
            .background(status.tint.opacity(0.12), in: Capsule())
    }

    private func capsule(_ text: String, tint: Color) -> some View {
        Text(text)
            .font(.caption.weight(.semibold))
            .foregroundStyle(tint)
            .padding(.horizontal, 8)
            .padding(.vertical, 3)
            .background(tint.opacity(0.12), in: Capsule())
    }

    private func sectionTitle(_ title: String) -> some View {
        Text(title)
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
    }

    private func visibleTrains(in dataset: SpikeDataset) -> [SpikeTrain] {
        let selected = document.selectedTrainIDsIncludingFocusedCandidate(for: .raster)
        guard !selected.isEmpty else {
            return []
        }
        return dataset.trains.filter { selected.contains($0.id) }
    }

    private func isiVisibleTrains(in dataset: SpikeDataset) -> [SpikeTrain] {
        let selected = document.selectedTrainIDsIncludingFocusedCandidate(for: .isiTimeline)
        guard !selected.isEmpty else {
            return []
        }
        return dataset.trains.filter { selected.contains($0.id) }
    }

    private func time(_ value: Double?) -> String {
        guard let value else {
            return "NA"
        }
        return TimeFormatting.seconds(value)
    }

    private func number(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.3f", value)
    }

    private func percent(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.1f%%", value * 100)
    }

    private func hfsBurstAuditRows(for candidate: ClassicAnchorCandidate) -> [HFSBurstArbitrationAuditRow] {
        guard let run = document.classicAnchorDetectionRun else {
            return []
        }

        let candidateStart = min(candidate.startISIIndex, candidate.endISIIndex)
        let candidateEnd = max(candidate.startISIIndex, candidate.endISIIndex)
        return run.hfsBurstArbitrationAuditRows.filter { row in
            guard row.trainID == candidate.trainID else {
                return false
            }
            if row.hfsCandidateID == candidate.id || row.hfsRootCandidateID == candidate.id {
                return true
            }
            let conflictStart = min(row.conflictStartISIIndex, row.hfsStartISIIndex)
            let conflictEnd = max(row.conflictEndISIIndex, row.hfsEndISIIndex)
            return candidateStart <= conflictEnd && candidateEnd >= conflictStart
        }
        .sorted { lhs, rhs in
            if lhs.requiresReview != rhs.requiresReview {
                return lhs.requiresReview && !rhs.requiresReview
            }
            if lhs.conflictStartISIIndex != rhs.conflictStartISIIndex {
                return lhs.conflictStartISIIndex < rhs.conflictStartISIIndex
            }
            return lhs.id < rhs.id
        }
    }

    private func labelColor(_ label: ClassicAnchorLabel) -> Color {
        switch label {
        case .burst:
            return .blue
        case .highFrequencyBurst:
            return .pink
        case .longBurst:
            return .purple
        case .possibleBurst:
            return .orange
        case .tonic:
            return .green
        case .highFrequencyTonic:
            return .teal
        case .highFrequencySpiking:
            return .red
        case .pause:
            return .indigo
        case .reject, .profile:
            return .secondary
        }
    }

    private func lockColor(_ lock: ClassicAnchorLockLevel) -> Color {
        switch lock {
        case .lockedClassic:
            return .green
        case .strongCandidate:
            return .orange
        case .auditOnly:
            return .secondary
        }
    }
}
