import STPDCore
import SwiftUI

struct InspectorView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                if let candidate = document.focusedClassicAnchorCandidate,
                   let annotation = document.focusedClassicAnchorEventAnnotation {
                    // Under review: the full event inspector takes priority. A still-pinned ISI is retained as a
                    // compact chip (not lost) so it returns when the candidate is cleared.
                    focusedEventInspector(candidate: candidate, annotation: annotation)
                    if let pinned = document.pinnedISIDiagnostic {
                        pinnedISIRetainedChip(pinned)
                    }
                    Divider()
                    manualAnnotationsInspector
                } else if let pinned = document.pinnedISIDiagnostic {
                    // No candidate focused: show the pinned ISI diagnostic as the primary content.
                    pinnedISIInspector(pinned)
                    Divider()
                    manualAnnotationsInspector
                } else {
                    // Nothing focused or pinned: a compact hint plus the (compact-when-empty) manual-annotation
                    // section, with no divider, so an idle inspector reads as one calm, low-footprint state.
                    emptyEventInspector
                    manualAnnotationsInspector
                }
            }
            .padding(16)
            .frame(maxWidth: .infinity, alignment: .topLeading)
        }
        .background(.clear)
    }

    private func focusedEventInspector(
        candidate: ClassicAnchorCandidate,
        annotation: ClassicAnchorEventAnnotation
    ) -> some View {
        VStack(alignment: .leading, spacing: 14) {
            header(candidate: candidate)
            labelExplanation(candidate: candidate)
            reviewControls(candidate: candidate)
            eventSummary(candidate: candidate, annotation: annotation)
            isiBoundaryEvidence(candidate: candidate)
            eventnessAudit(candidate: candidate)
            boundarySensitivityPreview()
            nearMissReview(candidate: candidate)
            candidateAudit(candidate: candidate)
            hfsBurstArbitrationAudit(candidate: candidate)
            AdaptiveV2ExplanationView(decisionPath: candidate.decisionPath)
            manualThresholdScopeProvenanceLine(candidate: candidate)
            decisionPath(candidate.decisionPath)
        }
    }

    // P10: a small, read-only line surfacing the manual hard-threshold scope that let a hard gate through for this
    // candidate's train. Renders nothing when the candidate carries no `manual_threshold_scope=...` note (out-of-scope /
    // soft-only / automatic). The raw decision-path token stays visible below for power users.
    @ViewBuilder
    private func manualThresholdScopeProvenanceLine(candidate: ClassicAnchorCandidate) -> some View {
        if let label = ManualThresholdScopeProvenance.parse(decisionPath: candidate.decisionPath).label(language: l10n.language) {
            HStack(alignment: .firstTextBaseline, spacing: 6) {
                Image(systemName: "scope")
                    .font(.caption2)
                    .foregroundStyle(STPDAppTheme.accent)
                Text(label)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
                    .textSelection(.enabled)
            }
        }
    }

    private func header(candidate: ClassicAnchorCandidate) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 8) {
                Text(l10n.language == .zh ? "事件检查器" : "Event Inspector")
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

    /// A concise "why this label" headline, synthesized from the candidate's existing detector provenance via the
    /// read-only `ClassicAnchorExplanation` builder, plus the document-level review/override and the active
    /// manual-threshold mode for this candidate's family. The detailed audit grids stay below for power users.
    private func labelExplanation(candidate: ClassicAnchorCandidate) -> some View {
        let lines = ClassicAnchorExplanation.lines(for: candidate)
        let manual = activeManualMode(for: candidate)
        let review = document.reviewStatus(for: candidate.id)
        return VStack(alignment: .leading, spacing: 8) {
            sectionTitle("Why this label")

            Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 6) {
                ForEach(lines.indices, id: \.self) { index in
                    let line = lines[index]
                    GridRow {
                        Text(line.label)
                            .foregroundStyle(.secondary)
                        Text(line.value)
                            .foregroundStyle(line.isFailure ? Color.orange : Color.primary)
                            .fixedSize(horizontal: false, vertical: true)
                            .textSelection(.enabled)
                    }
                    .font(.caption)
                }
                if review != .unreviewed {
                    GridRow {
                        Text(l10n.language == .zh ? "审核" : "Review")
                            .foregroundStyle(.secondary)
                        Text("\(review.title) (manual override)")
                            .foregroundStyle(.primary)
                    }
                    .font(.caption)
                }
            }

            if let manual {
                Text("Manual \(manual.familyName) gate: \(manual.modeTitle) — \(manual.effect)")
                    .font(.caption2)
                    .foregroundStyle(manual.mode == .hardGate ? Color.orange : STPDAppTheme.accent)
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
        .padding(12)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    /// The active (non-automatic) manual-threshold mode that applies to this candidate's family, if any — read
    /// from the document's per-family manual mode. Provenance/display only; it never changes detection here.
    private func activeManualMode(
        for candidate: ClassicAnchorCandidate
    ) -> (mode: ThresholdMode, familyName: String, modeTitle: String, effect: String)? {
        let family = (candidate.auditRecommendedFamily + " " + candidate.finalLabel.rawValue
            + " " + candidate.anchorFamily).lowercased()
        let mode: ThresholdMode
        let name: String
        if family.contains("pause") {
            mode = document.manualPauseMode; name = "Pause"
        } else if family.contains("hf") && family.contains("tonic") {
            mode = document.manualHFTonicMode; name = "HF tonic"
        } else if family.contains("spiking") || family.contains("hfs") {
            mode = document.manualHFSMode; name = "HFS"
        } else if family.contains("tonic") {
            mode = document.manualTonicMode; name = "Tonic"
        } else if family.contains("burst") {
            mode = document.manualBurstMode; name = "Burst"
        } else {
            return nil
        }
        guard mode != .automatic else { return nil }
        let modeTitle: String
        let effect: String
        switch mode {
        case .automatic:
            return nil
        case .softAnchor:
            modeTitle = "Soft"; effect = "widens / anchors the band (never narrows)"
        case .hardGate:
            modeTitle = "Hard"
            effect = name == "Burst"
                ? "seed / bridge admission ceiling (above adaptive recovers moderate ISI; below constrains)"
                : "constrains the band (narrow-only)"
        }
        return (mode, name, modeTitle, effect)
    }

    private func reviewControls(candidate: ClassicAnchorCandidate) -> some View {
        let candidateID = candidate.id
        let status = document.reviewStatus(for: candidateID)
        return VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 8) {
                Text(l10n.language == .zh ? "人工审核" : "Manual review")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Spacer()
                statusBadge(status)
            }

            // Primary: the three review decisions, full-width, so they read as the inspector's main action.
            HStack(spacing: 8) {
                reviewButton(.accepted, candidateID: candidateID).frame(maxWidth: .infinity)
                reviewButton(.needsReview, candidateID: candidateID).frame(maxWidth: .infinity)
                reviewButton(.rejected, candidateID: candidateID).frame(maxWidth: .infinity)
            }

            Text(status.inspectorDescription)
                .font(.caption2)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)

            // Secondary: compact, chromeless utility/navigation icons — clearly subordinate to the review actions.
            HStack(spacing: 4) {
                secondaryReviewButton("scope", help: "Center the raster on this structure") {
                    document.focusClassicAnchorCandidate(candidateID)
                }
                secondaryReviewButton("xmark.circle", help: "Exit review") {
                    document.clearClassicAnchorFocus()
                }
                secondaryReviewButton("circle.slash", help: "Clear review status (set unreviewed)") {
                    document.setReviewStatus(.unreviewed, for: candidateID)
                }
                .disabled(status == .unreviewed)

                Spacer(minLength: 0)

                secondaryReviewButton("chevron.left", help: "Previous structure in the review queue (does not change review status).") {
                    document.focusAdjacentReviewCandidate(forward: false)
                }
                secondaryReviewButton("chevron.right", help: "Next structure in the review queue (does not change review status).") {
                    document.focusAdjacentReviewCandidate(forward: true)
                }
            }

            reviewNavigationControls(candidate: candidate)

            // Manual annotation is a separate concept from review status; this only authors an
            // annotation over the candidate's interval and never changes the review state.
            Button {
                document.createManualAnnotationFromFocusedCandidate()
            } label: {
                Label(l10n.language == .zh ? "按候选范围创建人工标记" : "Manual label from candidate", systemImage: "hand.draw")
            }
            .liquidGlassButtonStyle()
            .disabled(!document.canCreateManualAnnotationFromFocusedCandidate)
            .help("Author a positive manual annotation over this candidate's interval (does not change review status).")
        }
        .padding(12)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    @ViewBuilder
    private var manualAnnotationsInspector: some View {
        let annotations = document.manualAnnotationsAll
        if annotations.isEmpty {
            // Compact empty state: a single header line + hint, with no card, so an inspector with nothing under
            // review does not show a large empty gray panel.
            VStack(alignment: .leading, spacing: 4) {
                HStack(alignment: .firstTextBaseline) {
                    Text(l10n.language == .zh ? "人工标记" : "Manual annotations")
                        .font(.subheadline.weight(.semibold))
                    Spacer()
                    Text("0")
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                }
                Text(l10n.language == .zh
                    ? "尚无标记。请在时间戳图顶部开启手工标记并在同一行拖动，或按当前候选范围创建。"
                    : "None yet. Enable Annotate mode in the raster header and drag on a train, or create one from a focused candidate.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
        } else {
            manualAnnotationsCard(annotations)
        }
    }

    private func manualAnnotationsCard(_ annotations: [ManualAnnotation]) -> some View {
        // Keep the inspector compact: show the most recent annotations only.
        let recent = Array(annotations.suffix(12).reversed())
        return VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .firstTextBaseline) {
                Text(l10n.language == .zh ? "人工标记" : "Manual annotations")
                    .font(.headline)
                Spacer()
                Text("\(annotations.count)")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
            }

            VStack(alignment: .leading, spacing: 6) {
                ForEach(recent) { annotation in
                    manualAnnotationRow(annotation)
                }
            }
            if annotations.count > recent.count {
                Text("Showing \(recent.count) of \(annotations.count). Export CSV for the full list.")
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            }

            if let summary = document.manualCalibrationSummary, !summary.isEmpty {
                manualCalibrationPreview(summary)
            }
        }
        .padding(12)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    @ViewBuilder
    private func manualCalibrationPreview(_ summary: ManualAnnotationCalibrationSummary) -> some View {
        Divider()
        VStack(alignment: .leading, spacing: 5) {
            HStack(alignment: .firstTextBaseline) {
                Text(l10n.language == .zh ? "人工标记校准预览" : "Manual calibration preview")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Spacer()
                Text(l10n.language == .zh ? "未应用" : "not applied")
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            }
            // q10 / median / q90 in ms per positive label/family; usable rows are emphasized.
            ForEach(summary.rows.filter(\.isPositive), id: \.label) { row in
                calibrationRow(row)
            }
            Text("Preview/audit only — these manual-derived values are not applied to the detector (source: manual_annotations).")
                .font(.caption2)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)
        }
    }

    private func calibrationRow(_ row: ManualAnnotationCalibrationLabelSummary) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 6) {
            Text(row.displayName)
                .font(.caption2.weight(row.isFamily ? .semibold : .regular))
                .foregroundStyle(row.isUsableForCalibration ? Color.primary : Color.secondary)
            Spacer(minLength: 4)
            Text("n=\(row.coveredISICount)")
                .font(.caption2)
                .foregroundStyle(.secondary)
                .monospacedDigit()
            Text("\(milliseconds(row.q10ISISeconds))/\(milliseconds(row.medianISISeconds))/\(milliseconds(row.q90ISISeconds)) ms")
                .font(.caption2)
                .foregroundStyle(.secondary)
                .monospacedDigit()
                .lineLimit(1)
        }
    }

    private func milliseconds(_ seconds: Double?) -> String {
        guard let seconds, seconds.isFinite else { return "—" }
        return String(format: "%.1f", seconds * 1000)
    }

    private func manualAnnotationRow(_ annotation: ManualAnnotation) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 8) {
            VStack(alignment: .leading, spacing: 1) {
                Text(annotation.label.displayName)
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(annotation.polarity == .negative ? Color.secondary : Color.primary)
                Text("\(annotation.trainID) · \(time(annotation.normalizedStartSec))–\(time(annotation.normalizedEndSec))")
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .truncationMode(.middle)
            }
            Spacer(minLength: 4)
            Button(role: .destructive) {
                document.removeManualAnnotation(id: annotation.id, trainID: annotation.trainID)
            } label: {
                Image(systemName: "trash")
            }
            .buttonStyle(.borderless)
            .help("Delete this manual annotation")
        }
    }

    /// Channel selector + safe batch-accept for the focused candidate's train. Navigation/batch only —
    /// never changes detection, candidates, or manual annotations.
    /// Read-only queue indicator for the active channel, e.g. `Pause · #2/10 · 7 open`.
    private var reviewQueuePositionText: String {
        let summary = document.reviewQueueSummary()
        let position = document.activeReviewChannelPosition()
        let channel = document.activeReviewChannel.title
        if position.current > 0 {
            return "\(channel) · #\(position.current)/\(position.total) · \(summary.open) open"
        }
        return "\(channel) · \(summary.open) open / \(summary.total)"
    }

    private func reviewNavigationControls(candidate: ClassicAnchorCandidate) -> some View {
        let batchCount = document.batchAcceptTargetCount()
        return VStack(alignment: .leading, spacing: 8) {
            HStack(spacing: 8) {
                Text("Channel")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Picker("", selection: $document.activeReviewChannel) {
                    ForEach(ClassicAnchorReviewChannel.allCases) { channel in
                        Text(channel.title).tag(channel)
                    }
                }
                .labelsHidden()
                .frame(width: 150)
                .help("Limit previous/next navigation and batch accept to one detector pattern family.")
            }

            Text(reviewQueuePositionText)
                .font(.caption2)
                .foregroundStyle(.secondary)
                .monospacedDigit()

            Button {
                document.batchAcceptCurrentTrainAndChannel()
            } label: {
                Label(
                    "Accept remaining \(batchCount) \(document.activeReviewChannel.title) in \(candidate.trainName)",
                    systemImage: "checkmark.circle.fill"
                )
                .lineLimit(1)
                .truncationMode(.middle)
            }
            .liquidGlassButtonStyle()
            .disabled(batchCount == 0)
            .help("Accept all unreviewed / needs-review structures in this train for the active channel. Never changes rejected or already-accepted reviews, and never touches manual annotations or detector output.")
        }
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

    /// Compact, chromeless icon button for secondary review utilities (center / exit / clear / prev / next),
    /// quieter than the prominent review-decision buttons so the primary actions stay dominant.
    private func secondaryReviewButton(
        _ systemImage: String,
        help: String,
        action: @escaping () -> Void
    ) -> some View {
        Button(action: action) {
            Image(systemName: systemImage)
        }
        .liquidGlassToolbarButtonStyle()
        .help(help)
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
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    /// Phase 2B: per-candidate ISI temporal-profile evidence — diagnostic only, never affects
    /// detection. Computed fresh from the candidate's train + ISI span via the pure evidence service.
    @ViewBuilder
    private func isiBoundaryEvidence(candidate: ClassicAnchorCandidate) -> some View {
        if let train = document.dataset?.trains.first(where: { $0.id == candidate.trainID }) {
            let minValid = document.classicAnchorDetectionRun?.bandSettings.minValidISISec
                ?? document.adaptiveDetectorBandSettings.minValidISISec
            let evidence = ISITemporalProfileEvidenceBuilder.makeEvidence(
                for: candidate,
                train: train,
                minValidISISec: minValid
            )
            VStack(alignment: .leading, spacing: 10) {
                sectionTitle("ISI boundary evidence")

                Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 7) {
                    metricRow("Edge contrast min", number(evidence.edgeContrastMin))
                    metricRow("Edge contrast geom", number(evidence.edgeContrastGeom))
                    metricRow("Pre edge ratio", number(evidence.preEdgeRatio))
                    metricRow("Post edge ratio", number(evidence.postEdgeRatio))
                    metricRow("Flank count", "\(evidence.flankCount)")
                    metricRow("Core-q percentile", number(evidence.coreQPct))
                    metricRow("Percentile reliable", evidence.percentileReliable ? "Yes" : "No")
                    metricRow("Local median ISI", time(evidence.localMedianISISec))
                    metricRow("Local compression", number(evidence.localCompressionRatio))
                }

                Text("Diagnostic only — these ISI boundary metrics do not affect detection.")
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            }
            .padding(12)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
        }
    }

    /// Phase 2C: candidate eventness audit — diagnostic only, never affects detection. Computed fresh
    /// from the candidate's train + ISI span (reuses Phase 2B evidence for the edge metric).
    @ViewBuilder
    private func eventnessAudit(candidate: ClassicAnchorCandidate) -> some View {
        if let train = document.dataset?.trains.first(where: { $0.id == candidate.trainID }) {
            let minValid = document.classicAnchorDetectionRun?.bandSettings.minValidISISec
                ?? document.adaptiveDetectorBandSettings.minValidISISec
            let audit = ISICandidateEventnessAuditor.makeAudit(
                for: candidate,
                train: train,
                minValidISISec: minValid
            )
            VStack(alignment: .leading, spacing: 10) {
                sectionTitle("Eventness audit")

                Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 7) {
                    metricRow("Eventness score", number(audit.eventnessScore))
                    metricRow("Eventness zone", audit.eventnessZone)
                    metricRow("Regularity score", number(audit.regularityScore))
                    metricRow("Edge component", number(audit.eventnessEdgeComponent))
                    metricRow("Context component", number(audit.eventnessContextComponent))
                    metricRow("Context contrast", number(audit.contextContrast))
                    metricRow("Return to baseline", number(audit.returnToBaselineScore))
                    metricRow("Distant context median", time(audit.distantContextMedianSec))
                    metricRow("q10 / q50 / q90", "\(time(audit.q10ISISec)) / \(time(audit.q50ISISec)) / \(time(audit.q90ISISec))")
                    metricRow("q90/q10 ratio", number(audit.q90Q10Ratio))
                    metricRow("Medium review", audit.mediumEventnessReview ? "Yes" : "No")
                    metricRow("Recommendation", audit.auditRecommendation)
                }

                Text("Diagnostic only — eventness does not affect detection, labels, or boundaries.")
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            }
            .padding(12)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
        }
    }

    /// SEG-PREVIEW-1: boundary sensitivity preview — diagnostic only, never affects detection. Recomputes
    /// CV/CV2/LV over expanded/contracted copies of the focused candidate's ISI span and checks the tonic
    /// regularity gates. Read-only: no threshold/label/setting mutation, no rerun, no staleness.
    /// SEG-PREVIEW-1 entry: card for the focused candidate's span (the candidate is already the inspector header,
    /// so no extra span caption / focus affordance is needed).
    @ViewBuilder
    private func boundarySensitivityPreview() -> some View {
        if let preview = document.focusedSegmentSensitivityPreview {
            boundarySensitivityCard(preview: preview, spanCaption: nil, focusCandidateID: nil)
        }
    }

    /// Shared renderer (SEG-PREVIEW-1 + SEG-PREVIEW-2). `spanCaption` states which candidate/span is evaluated;
    /// `focusCandidateID` adds a "Focus candidate" link (used from the pinned-ISI inspector).
    @ViewBuilder
    private func boundarySensitivityCard(
        preview: SegmentSensitivityPreview, spanCaption: String?, focusCandidateID: String?
    ) -> some View {
        VStack(alignment: .leading, spacing: 9) {
            HStack(alignment: .firstTextBaseline) {
                sectionTitle(l10n.t("边界敏感性预览"))
                if let focusCandidateID {
                    Spacer()
                    Button(l10n.t("聚焦候选")) { document.focusClassicAnchorCandidate(focusCandidateID) }
                        .buttonStyle(.plain)
                        .font(.caption2.weight(.medium))
                        .foregroundStyle(STPDAppTheme.accent)
                }
            }

            // SEG-PREVIEW-4: one-line summary verdict at the top of the card.
            let summary = preview.summaryCategory
            HStack(alignment: .top, spacing: 6) {
                capsule(summaryTitle(summary), tint: summaryTint(summary))
                Text(summarySentence(summary))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
            }

            if let spanCaption {
                Text(spanCaption)
                    .font(.caption2.weight(.medium))
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
            }
            Text(l10n.t("仅为诊断预览：不修改阈值、检测结果或标签。展开 = 纳入相邻 ISI 后是否仍通过强直门限；收缩 = 去掉边缘 ISI 后核心是否仍为强直。"))
                .font(.caption2)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)

            Text("\(l10n.t("强直门限"))  " + String(
                format: "CV≤%.2f · CV2≤%.2f · LV≤%.2f · spikes≥%d",
                preview.gates.cvMax, preview.gates.cv2Max, preview.gates.lvMax, preview.gates.minSpikes
            ))
            .font(.system(.caption2, design: .monospaced))
            .foregroundStyle(.secondary)

            sensitivityGroup(l10n.t("展开"), rows: preview.displayedExpansionRows)
            sensitivityGroup(l10n.t("核心稳定性"), rows: preview.displayedContractionRows)

            Text(l10n.t("仅预览，未应用。"))
                .font(.caption2.weight(.medium))
                .foregroundStyle(.secondary)
        }
        .padding(12)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private func summaryTitle(_ category: SegmentSensitivitySummaryCategory) -> String {
        switch category {
        case .stableTonicCore: return l10n.t("稳定强直核心")
        case .boundarySensitive: return l10n.t("边界敏感")
        case .edgeDependent: return l10n.t("依赖边缘 ISI")
        case .currentFailsButCorePasses: return l10n.t("核心可通过")
        case .insufficientCore: return l10n.t("核心不足")
        case .notTonicCompatible: return l10n.t("不符合强直")
        }
    }

    private func summarySentence(_ category: SegmentSensitivitySummaryCategory) -> String {
        switch category {
        case .stableTonicCore: return l10n.t("当前通过；扩展仍通过，收缩通过或过短——核心稳健。")
        case .boundarySensitive: return l10n.t("当前通过，但纳入相邻 ISI 会改变判定——边界处敏感。")
        case .edgeDependent: return l10n.t("当前通过，但去掉边缘 ISI 会改变判定——判定依赖边缘 ISI。")
        case .currentFailsButCorePasses: return l10n.t("当前不通过，但收缩到核心可满足强直门限。")
        case .insufficientCore: return l10n.t("收缩多为过短，无法评估核心稳定性。")
        case .notTonicCompatible: return l10n.t("当前不通过，且无收缩能满足门限。")
        }
    }

    private func summaryTint(_ category: SegmentSensitivitySummaryCategory) -> Color {
        switch category {
        case .stableTonicCore: return .green
        case .boundarySensitive, .edgeDependent: return .orange
        case .currentFailsButCorePasses: return .teal
        case .insufficientCore: return Color.secondary
        case .notTonicCompatible: return .red
        }
    }

    @ViewBuilder
    private func sensitivityGroup(_ title: String, rows: [SegmentSensitivityRow]) -> some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(title)
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
            ForEach(rows, id: \.variant) { row in
                sensitivityRow(row)
            }
        }
    }

    private func sensitivityRow(_ row: SegmentSensitivityRow) -> some View {
        VStack(alignment: .leading, spacing: 1) {
            HStack(spacing: 6) {
                Text(variantLabel(row.variant))
                    .font(.caption2.weight(.medium))
                    .frame(width: 72, alignment: .leading)
                verdictBadge(row.verdict)
                if row.clippedAtBoundary {
                    Text(l10n.t("边界")).font(.caption2).foregroundStyle(.secondary)
                }
                Spacer(minLength: 0)
                stabilityText(row.stability)
            }
            Text(sensitivityMetricsLine(row))
                .font(.system(.caption2, design: .monospaced))
                .foregroundStyle(.secondary)
            Text(sensitivityReasonLine(row))
                .font(.caption2)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)
        }
    }

    private func sensitivityMetricsLine(_ row: SegmentSensitivityRow) -> String {
        let span = "[\(row.startISIIndex)–\(row.endISIIndex)]"
        if row.verdict.isTooShort && row.nISI < 2 {
            return "\(span) " + l10n.t("过短")
        }
        return "\(span) n=\(row.nISI) · \(time(row.meanISISec)) · CV \(number(row.cv)) · CV2 \(number(row.cv2)) · LV \(number(row.lv))"
    }

    private func sensitivityReasonLine(_ row: SegmentSensitivityRow) -> String {
        if row.variant == .current {
            return l10n.t("当前检测到的候选边界。")
        }
        if row.verdict.isTooShort {
            return l10n.t("核心过短，无法单独评估强直规则性。")
        }
        if let reason = row.failureReason {
            return String(format: l10n.t("失败原因：%@。"), reason)
        }
        switch row.stability {
        case .stable:
            return l10n.t("纳入或移除该边缘后仍通过强直门限。")
        case .changed:
            return l10n.t("该边界变化会改变强直兼容性。")
        case .reference, .notComparable:
            return l10n.t("仅用于解释边界敏感性。")
        }
    }

    private func variantLabel(_ variant: SegmentSensitivityVariant) -> String {
        switch variant {
        case .current: return l10n.t("当前")
        case .expandLeft: return l10n.t("左 +1")
        case .expandRight: return l10n.t("右 +1")
        case .expandBoth: return l10n.t("两侧 +1")
        case .contractLeft: return l10n.t("左 −1")
        case .contractRight: return l10n.t("右 −1")
        case .contractBoth: return l10n.t("两侧 −1")
        }
    }

    @ViewBuilder
    private func verdictBadge(_ verdict: SegmentTonicVerdict) -> some View {
        switch verdict {
        case .pass:
            capsule(l10n.t("强直"), tint: .green)
        case let .fail(metric, _, _):
            capsule(metric + " ✗", tint: .orange)
        case .tooShort:
            capsule(l10n.t("过短"), tint: Color.secondary)
        }
    }

    @ViewBuilder
    private func stabilityText(_ stability: SegmentStability) -> some View {
        switch stability {
        case .reference, .notComparable:
            EmptyView()
        case .stable:
            Text(l10n.t("稳定")).font(.caption2).foregroundStyle(.secondary)
        case .changed:
            Text(l10n.t("改变")).font(.caption2.weight(.semibold)).foregroundStyle(.orange)
        }
    }

    /// Phase 2D: near-miss review — diagnostic only, never affects detection. Shown for eligible
    /// (rejected / unselected / evidence-only) candidates; explains how close they were to a gate.
    @ViewBuilder
    private func nearMissReview(candidate: ClassicAnchorCandidate) -> some View {
        if let train = document.dataset?.trains.first(where: { $0.id == candidate.trainID }),
           ISINearMissAuditor.isEligible(candidate) {
            let minValid = document.classicAnchorDetectionRun?.bandSettings.minValidISISec
                ?? document.adaptiveDetectorBandSettings.minValidISISec
            let audit = ISINearMissAuditor.makeAudit(
                for: candidate,
                train: train,
                minValidISISec: minValid
            )
            VStack(alignment: .leading, spacing: 10) {
                sectionTitle("Near-miss review")

                Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 7) {
                    metricRow("Near miss", audit.isNearMiss ? "Yes" : "No")
                    metricRow("Best category", audit.bestCategory)
                    metricRow("Parameter", audit.bestParameter ?? "—")
                    metricRow("Direction", audit.bestDirection)
                    metricRow("Threshold", number(audit.bestCurrentValue))
                    metricRow("Candidate value", number(audit.bestRequiredValue))
                    metricRow("Relative change", number(audit.bestRelativeChange))
                    metricRow("Failed gates", "\(audit.failureCount)")
                    metricRow("Near-miss score", number(audit.nearMissScore))
                    metricRow("Reason", audit.reason)
                }

                Text("Diagnostic only — near-miss review never creates candidates or changes detection.")
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            }
            .padding(12)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
        }
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
        // No focused candidate: show a single compact hint instead of a large empty gray panel. The manual
        // annotations section below remains the inspector's primary content when nothing is under review.
        Text(l10n.language == .zh
            ? "请选择一个结构候选以检查时间戳、检测指标和人工审核状态；也可在时间戳图上单击一个 ISI 固定其诊断信息。"
            : "Select a structural candidate to inspect timestamps, detection metrics, and manual review state. Click an ISI on the raster to pin its diagnostic.")
            .font(.callout)
            .foregroundStyle(.secondary)
            .fixedSize(horizontal: false, vertical: true)
            .frame(maxWidth: .infinity, alignment: .leading)
    }

    /// Phase 8: the pinned per-ISI diagnostic (clicked on the raster). Reuses the Phase 7 `PerISIDiagnostic`.
    private func pinnedISIInspector(_ pinned: PinnedISIDiagnostic) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline) {
                sectionTitle("Pinned ISI")
                Spacer()
                Button(l10n.language == .zh ? "清除" : "Clear") { document.clearPinnedISIDiagnostic() }
                    .buttonStyle(.plain)
                    .font(.caption.weight(.medium))
                    .foregroundStyle(STPDAppTheme.accent)
            }

            Text(pinned.trainName)
                .font(.subheadline.weight(.semibold))
                .lineLimit(2)
                .truncationMode(.middle)

            Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 12, verticalSpacing: 6) {
                metricRow("Train", pinned.trainID)
                metricRow("ISI index", "\(pinned.isiIndex)")
                metricRow("Left", time(pinned.leftTimestampSec))
                metricRow("Right", time(pinned.rightTimestampSec))
                metricRow("ISI", time(pinned.isiSec))
                metricRow("Auto label", pinned.autoLabel ?? "Other")
                if let review = pinned.reviewStatus {
                    metricRow("Reviewed", review)
                }
                metricRow("In candidate", pinned.belongsToCandidate ? "Yes" : "No")
                if let band = PerISIDiagnosticBuilder.seedBandText(
                    lowerSec: pinned.diagnostic.seedLowerSec,
                    upperSec: pinned.diagnostic.seedUpperSec
                ) {
                    metricRow("Seed band", band)
                }
                metricRow("Vs band", pinned.diagnostic.bandRelationTag)
            }

            AdaptiveV2ExplanationView(
                explanation: document.adaptiveV2Explanation(trainID: pinned.trainID, isiIndex: pinned.isiIndex))

            Text(pinned.diagnostic.explanation)
                .font(.caption)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)

            if let comparison = document.pinnedISIComparison, comparison.after == pinned {
                pinnedComparisonSection(comparison)
            }

            // SEG-PREVIEW-2: when the pinned ISI is inside a tonic candidate, evaluate that candidate's FULL span.
            if let sensitivity = document.pinnedTonicSegmentSensitivity {
                boundarySensitivityCard(
                    preview: sensitivity.preview,
                    spanCaption: "\(l10n.t("评估覆盖该 ISI 的强直候选"))  [\(sensitivity.startISIIndex)–\(sensitivity.endISIIndex)]",
                    focusCandidateID: sensitivity.candidateID
                )
            } else if pinned.belongsToCandidate {
                Text(l10n.t("敏感性预览仅适用于强直候选。"))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            }
        }
        .padding(12)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    /// Phase 9: compact Before → After of the pinned ISI across the last detector rerun.
    private func pinnedComparisonSection(_ comparison: PinnedISIComparison) -> some View {
        VStack(alignment: .leading, spacing: 6) {
            Divider()
            HStack(alignment: .firstTextBaseline) {
                Text(l10n.language == .zh ? "自上次重新检测后" : "Since last rerun")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Spacer()
                Text(comparison.anyChange ? "Changed" : "No change")
                    .font(.caption2.weight(.semibold))
                    .foregroundStyle(comparison.anyChange ? Color.orange : Color.secondary)
            }
            Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 10, verticalSpacing: 5) {
                comparisonRow("Auto label",
                              comparison.before.autoLabel ?? "Other",
                              comparison.after.autoLabel ?? "Other",
                              comparison.autoLabelChanged)
                comparisonRow("Final/reviewed",
                              comparison.before.reviewStatus ?? "—",
                              comparison.after.reviewStatus ?? "—",
                              comparison.reviewChanged)
                comparisonRow("Candidate",
                              comparison.before.belongsToCandidate ? "Yes" : "No",
                              comparison.after.belongsToCandidate ? "Yes" : "No",
                              comparison.candidateCoverageChanged)
                comparisonRow("Band",
                              comparison.before.diagnostic.bandRelationTag,
                              comparison.after.diagnostic.bandRelationTag,
                              comparison.bandRelationChanged)
            }
        }
    }

    private func comparisonRow(_ label: String, _ before: String, _ after: String, _ changed: Bool) -> some View {
        GridRow {
            Text(label)
                .foregroundStyle(.secondary)
            HStack(spacing: 4) {
                Text(before)
                    .foregroundStyle(.secondary)
                Image(systemName: "arrow.right")
                    .font(.system(size: 8))
                    .foregroundStyle(.tertiary)
                Text(after)
                    .foregroundStyle(changed ? Color.orange : Color.primary)
            }
            .lineLimit(1)
            .truncationMode(.middle)
        }
        .font(.caption2)
    }

    /// Compact chip shown under a focused candidate to indicate a pinned ISI is retained (candidate has priority).
    private func pinnedISIRetainedChip(_ pinned: PinnedISIDiagnostic) -> some View {
        HStack(spacing: 8) {
            Image(systemName: "pin.fill")
                .font(.caption2)
                .foregroundStyle(.secondary)
            Text(l10n.language == .zh
                ? "已保留固定的 ISI #\(pinned.isiIndex) · \(pinned.trainName)"
                : "Pinned ISI #\(pinned.isiIndex) · \(pinned.trainName) retained")
                .font(.caption2)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .truncationMode(.middle)
            Spacer(minLength: 4)
            Button(l10n.language == .zh ? "清除" : "Clear") { document.clearPinnedISIDiagnostic() }
                .buttonStyle(.plain)
                .font(.caption2.weight(.medium))
                .foregroundStyle(STPDAppTheme.accent)
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 6)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 7))
    }

    private func metricRow(_ title: String, _ value: String) -> some View {
        GridRow {
            Text(inspectorText(title))
                .foregroundStyle(.secondary)
            Text(inspectorText(value))
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
        Text(inspectorText(title))
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
    }

    /// Display-only localization. Detector tokens, IDs, decisions, and exported values remain unchanged.
    private func inspectorText(_ text: String) -> String {
        guard l10n.language == .zh else { return text }
        return Self.inspectorChinese[text] ?? text
    }

    private static let inspectorChinese: [String: String] = [
        "Why this label": "为何得到此标签",
        "Event metrics": "事件指标",
        "ISI boundary evidence": "ISI 边界证据",
        "Eventness audit": "事件性审计",
        "Near-miss review": "近阈值复核",
        "Candidate audit": "候选审计",
        "HFS-burst arbitration": "HFS 与 Burst 仲裁",
        "Decision path": "决策路径",
        "Pinned ISI": "固定的 ISI",
        "Raw start": "原始起点", "Raw end": "原始终点",
        "Aligned start": "对齐起点", "Aligned end": "对齐终点",
        "Duration": "时长", "Spike span": "Spike 范围", "ISI span": "ISI 范围",
        "Spikes": "Spike 数", "Band": "区间", "Pre gap": "前间隔", "Post gap": "后间隔",
        "Edge contrast": "边缘对比度", "Score": "得分", "Refractory": "疑似不应期",
        "Manual review": "人工审核", "Auto selected": "自动入选", "Selection": "选择状态",
        "Gate": "门控", "Action": "处理动作", "Why": "原因", "Failure": "失败原因",
        "Track": "轨道", "Family": "模式家族", "Subtype": "亚型", "Final class": "最终类别",
        "Event track": "事件轨道", "Algorithm audit": "算法审计", "Algorithm review": "算法复核",
        "Confidence": "置信度", "Uncertainty": "不确定性", "Review evidence": "复核证据",
        "Review strength": "复核证据强度", "Review summary": "复核摘要",
        "Layer": "层级", "Class": "类别", "Diagnostic": "诊断",
        "Train": "Spike train", "ISI index": "ISI 序号", "Left": "左侧时间戳", "Right": "右侧时间戳",
        "Auto label": "自动标签", "Reviewed": "审核状态", "In candidate": "位于候选内",
        "Seed band": "种子区间", "Vs band": "相对区间",
        "Yes": "是", "No": "否", "None": "无", "Required": "需要",
        "Recommended": "建议复核", "Other": "其他",
    ]

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
