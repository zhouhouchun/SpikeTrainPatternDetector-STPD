import STPDCore
import SwiftUI

struct StructuralCandidatesView: View {
    @Bindable var document: RasterDocument
    @Binding var selectedSection: WorkbenchSection?
    @State private var labelFilter: CandidateLabelFilter = .all
    @State private var showsAutoSelectedOnly = false
    @State private var enablesTimeWindowAudit = false
    @State private var timeWindowMode: RasterTimeMode = .aligned
    @State private var timeWindowTrainQuery = ""
    @State private var timeWindowStartSec = 0.0
    @State private var timeWindowEndSec = 0.0
    @State private var candidateDisplayLimit = 400

    private var candidates: [ClassicAnchorCandidate] {
        guard let run = document.classicAnchorDetectionRun else {
            return []
        }
        let annotationLookup = hasValidTimeWindowAuditRange ? annotationsByCandidateID : [:]
        return run.candidates.filter { candidate in
            candidatePassesFilters(candidate, annotationsByCandidateID: annotationLookup)
        }
        .sorted { lhs, rhs in
            let lhsReviewRank = reviewQueueRank(lhs)
            let rhsReviewRank = reviewQueueRank(rhs)
            if lhsReviewRank != rhsReviewRank {
                return lhsReviewRank < rhsReviewRank
            }
            if lhs.anchorLockLevel != rhs.anchorLockLevel {
                return lhs.anchorLockLevel == .lockedClassic
            }
            if lhs.priority != rhs.priority {
                return lhs.priority > rhs.priority
            }
            if lhs.trainName != rhs.trainName {
                return lhs.trainName < rhs.trainName
            }
            return lhs.startISIIndex < rhs.startISIIndex
        }
    }

    private var totalCandidateCount: Int {
        document.classicAnchorDetectionRun?.candidateCount ?? 0
    }

    private var annotationsByCandidateID: [String: ClassicAnchorEventAnnotation] {
        var annotations: [String: ClassicAnchorEventAnnotation] = [:]
        for annotation in document.classicAnchorCandidateAuditAnnotations {
            annotations[annotation.candidateID] = annotation
        }
        return annotations
    }

    private var hasValidTimeWindowAuditRange: Bool {
        enablesTimeWindowAudit && timeWindowEndSec != timeWindowStartSec
    }

    private var fragmentReviewSnapshots: [FragmentTrainReviewSnapshot] {
        guard enablesTimeWindowAudit,
              let dataset = document.dataset,
              let run = document.classicAnchorDetectionRun else {
            return []
        }

        let trainQuery = timeWindowTrainQuery.trimmingCharacters(in: .whitespacesAndNewlines)
        let queryStart = min(timeWindowStartSec, timeWindowEndSec)
        let queryEnd = max(timeWindowStartSec, timeWindowEndSec)
        guard queryEnd > queryStart else {
            return []
        }

        let allCandidates = run.candidates
        let annotationLookup = annotationsByCandidateID
        let matchingTrains = dataset.trains.filter { train in
            trainQuery.isEmpty ||
                train.name.range(of: trainQuery, options: [.caseInsensitive, .diacriticInsensitive]) != nil
        }

        return matchingTrains.compactMap { train in
            let rows = fragmentISIRows(
                train: train,
                queryStart: queryStart,
                queryEnd: queryEnd,
                candidates: allCandidates
            )
            let localCandidates = allCandidates
                .filter {
                    $0.trainID == train.id &&
                        candidatePassesTimeWindowAudit($0, annotationsByCandidateID: annotationLookup)
                }
                .sorted(by: fragmentCandidateSort)
            guard !rows.isEmpty || !localCandidates.isEmpty else {
                return nil
            }
            return FragmentTrainReviewSnapshot(
                train: train,
                isiRows: rows,
                candidates: localCandidates
            )
        }
        .prefix(4)
        .map { $0 }
    }

    private func reviewQueueRank(_ candidate: ClassicAnchorCandidate) -> Int {
        let manualStatus = document.reviewStatus(for: candidate.id)
        let isReviewableTrack = candidate.auditRecommendedTrack == .event ||
            candidate.auditRecommendedTrack == .gap ||
            candidate.auditRecommendedTrack == .state ||
            candidate.auditRecommendedTrack == .review
        if manualStatus == .needsReview, candidate.selectedForAuto, isReviewableTrack {
            return 0
        }
        if manualStatus == .unreviewed, candidate.selectedForAuto, candidate.auditReviewRequired {
            return 1
        }
        if manualStatus == .unreviewed, candidate.selectedForAuto, candidate.auditRecommendedTrack == .review {
            return 2
        }
        if manualStatus == .unreviewed,
           candidate.selectedForAuto,
           candidate.anchorLockLevel == .strongCandidate,
           isReviewableTrack {
            return 3
        }
        if manualStatus == .unreviewed, candidate.selectedForAuto, isReviewableTrack {
            return 4
        }
        if candidate.selectedForAuto, isReviewableTrack {
            return 5
        }
        if manualStatus == .unreviewed, candidate.auditReviewRequired {
            return 6
        }
        if candidate.auditRecommendedTrack == .diagnostic || candidate.action == "audit_only" {
            return 8
        }
        return 7
    }

    var body: some View {
        ScrollViewReader { proxy in
            ScrollView([.vertical, .horizontal]) {
                VStack(alignment: .leading, spacing: 16) {
                    header
                        .id(StructuralCandidatesScrollTarget.top)

                    if document.dataset == nil {
                        ContentUnavailableView(
                            "No Dataset",
                            systemImage: "rectangle.connected.to.line.below",
                            description: Text(document.lastErrorMessage ?? "Load a spike train dataset first.")
                        )
                        .frame(maxWidth: .infinity, minHeight: 360)
                    } else if let run = document.classicAnchorDetectionRun {
                        summary(run: run)
                        if let report = run.performanceReport {
                            performancePanel(report)
                        }
                        timeWindowAuditControls
                        candidatesTable(candidates)
                    } else {
                        ContentUnavailableView(
                            "No Candidate Audit",
                            systemImage: "rectangle.connected.to.line.below",
                            description: Text("Run adaptive classic-anchor detection to generate structural candidates.")
                        )
                        .frame(maxWidth: .infinity, minHeight: 360)
                    }
                }
                .padding(18)
                .frame(minWidth: 1040, maxWidth: .infinity, alignment: .topLeading)
            }
            .onChange(of: document.detectorLastRunDate) { _, newDate in
                guard newDate != nil else {
                    return
                }
                candidateDisplayLimit = 400
                withAnimation(.snappy(duration: 0.18)) {
                    proxy.scrollTo(StructuralCandidatesScrollTarget.top, anchor: .topLeading)
                }
            }
            .onChange(of: labelFilter) { _, _ in
                candidateDisplayLimit = 400
            }
            .onChange(of: showsAutoSelectedOnly) { _, _ in
                candidateDisplayLimit = 400
            }
        }
        .frame(minWidth: 720)
    }

    private var header: some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Image(systemName: "rectangle.connected.to.line.below")
                .foregroundStyle(.secondary)
            Text("结构候选")
                .font(.title3.weight(.semibold))
            Text("Live")
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(.quaternary, in: Capsule())
            Spacer()
            if document.isDetectorRunning {
                ProgressView()
                    .controlSize(.small)
                Text(document.detectorStatusMessage)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .truncationMode(.tail)
                    .frame(maxWidth: 260, alignment: .trailing)
            }
            Button {
                document.importClassicAnchorReviewStatusesWithPanel()
            } label: {
                Label("导入审核", systemImage: "tray.and.arrow.down")
            }
            .liquidGlassButtonStyle()
            .disabled(!document.hasDetectorResults)

            Button {
                document.exportClassicAnchorEventsCSVWithPanel()
            } label: {
                Label("导出 CSV", systemImage: "square.and.arrow.down")
            }
            .liquidGlassButtonStyle()
            .disabled(!document.hasDetectorResults)

            Button {
                document.runAdaptiveClassicAnchorDetection()
            } label: {
                Label(
                    document.isDetectorRunning ? "检测中" : (document.hasDetectorResults ? "重新检测" : "运行检测"),
                    systemImage: document.isDetectorRunning ? "hourglass" : "play.fill"
                )
            }
            .liquidGlassButtonStyle(prominent: true)
            .disabled(document.dataset == nil || document.isDetectorRunning)
        }
    }

    private func summary(run: ClassicAnchorDetectionRun) -> some View {
        HStack(spacing: 10) {
            if let report = run.performanceReport {
                tile("Runtime", formatRuntime(report.totalWallTimeMs), tint: .blue)
                if let slowest = report.slowestTrain {
                    tile("Slowest train", formatRuntime(slowest.totalMeasuredTimeMs), tint: .orange)
                }
            }
            tile("Candidates", "\(run.candidateCount)")
            tile("Locked classic", "\(run.lockedClassicCount)", tint: .green)
            tile("Strong", "\(run.strongCandidateCount)", tint: .orange)
            tile("Burst", "\(run.burstCount)")
            tile("HF burst", "\(run.highFrequencyBurstCount)", tint: .pink)
            tile("Long burst", "\(run.longBurstCount)")
            tile("Review", "\(run.selectedReviewCount)", tint: .orange)
            tile("HFS", "\(run.highFrequencySpikingCount)", tint: .red)
            tile("HF tonic", "\(run.highFrequencyTonicCount)", tint: .teal)
            tile("Tonic", "\(run.tonicCount)", tint: .green)
            tile("Pause", "\(run.pauseCount)", tint: .indigo)
            tile("Auto", "\(run.selectedAutoCount)", tint: .blue)
            tile("Event track", "\(run.selectedEventCount)", tint: .blue)
            tile("Gap track", "\(run.selectedGapCount)", tint: .indigo)
            tile("State track", "\(run.selectedStateCount)", tint: .teal)
        }
    }

    private func tile(_ title: String, _ value: String, tint: Color? = nil) -> some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(value)
                .font(.title3.weight(.semibold))
                .monospacedDigit()
                .lineLimit(1)
                .minimumScaleFactor(0.82)
        }
        .padding(14)
        .frame(minWidth: 140, maxWidth: .infinity, alignment: .leading)
        .background((tint ?? Color(nsColor: .textBackgroundColor)).opacity(tint == nil ? 0.62 : 0.11), in: RoundedRectangle(cornerRadius: 8))
    }

    private func performancePanel(_ report: ClassicAnchorDetectionPerformanceReport) -> some View {
        DisclosureGroup {
            VStack(alignment: .leading, spacing: 12) {
                HStack(alignment: .top, spacing: 18) {
                    performanceColumn(
                        title: "Pipeline stages",
                        rows: report.stageTimings.map { timing in
                            (performanceStageTitle(timing.name), formatRuntime(timing.wallTimeMs))
                        }
                    )

                    performanceColumn(
                        title: "Slowest trains",
                        rows: report.slowestTrains(limit: 5).map { train in
                            (
                                train.trainName,
                                "\(formatRuntime(train.totalMeasuredTimeMs)) · \(train.spikeCount) spikes · \(train.candidateCount) candidates"
                            )
                        }
                    )
                }
            }
            .padding(.top, 8)
        } label: {
            HStack(spacing: 8) {
                Image(systemName: "speedometer")
                    .foregroundStyle(.secondary)
                Text("Performance")
                    .font(.headline)
                Text("\(report.trainCount) trains · \(report.spikeCount) spikes · \(formatRuntime(report.totalWallTimeMs))")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
        .padding(12)
        .background(Color(nsColor: .textBackgroundColor).opacity(0.62), in: RoundedRectangle(cornerRadius: 8))
    }

    private func performanceColumn(title: String, rows: [(String, String)]) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            ForEach(Array(rows.enumerated()), id: \.offset) { _, row in
                HStack(spacing: 10) {
                    Text(row.0)
                        .lineLimit(1)
                        .truncationMode(.middle)
                        .frame(width: 260, alignment: .leading)
                    Text(row.1)
                        .monospacedDigit()
                        .lineLimit(1)
                        .frame(width: 220, alignment: .leading)
                        .foregroundStyle(.secondary)
                }
                .font(.caption)
            }
            if rows.isEmpty {
                Text("No timing rows recorded.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
        .frame(maxWidth: .infinity, alignment: .topLeading)
    }

    private func performanceStageTitle(_ name: String) -> String {
        name
            .replacingOccurrences(of: "_", with: " ")
            .capitalized
    }

    private func formatRuntime(_ milliseconds: Double) -> String {
        guard milliseconds.isFinite, milliseconds >= 0 else {
            return "NA"
        }
        if milliseconds < 1_000 {
            return String(format: "%.0f ms", milliseconds)
        }
        return String(format: "%.2f s", milliseconds / 1_000)
    }

    private func candidatesTable(_ candidates: [ClassicAnchorCandidate]) -> some View {
        let displayLimit = max(1, candidateDisplayLimit)
        let displayedCandidates = Array(candidates.prefix(displayLimit))
        let hiddenCount = max(0, candidates.count - displayedCandidates.count)

        return VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .center, spacing: 12) {
                Text("Candidate audit")
                    .font(.headline)
                Text("\(displayedCandidates.count) shown · \(candidates.count) filtered / \(totalCandidateCount) total")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer(minLength: 18)
                Picker("Pattern", selection: $labelFilter) {
                    ForEach(CandidateLabelFilter.allCases) { filter in
                        Text(filter.title).tag(filter)
                    }
                }
                .pickerStyle(.menu)
                .frame(width: 188)

                Toggle("Auto only", isOn: $showsAutoSelectedOnly)
                    .toggleStyle(.checkbox)
            }

            LazyVStack(alignment: .leading, spacing: 0) {
                headerRow
                Divider()
                ForEach(Array(displayedCandidates.enumerated()), id: \.offset) { _, candidate in
                    candidateRow(candidate)
                }
                if hiddenCount > 0 {
                    Button {
                        candidateDisplayLimit += 400
                    } label: {
                        Label("Show 400 more (\(hiddenCount) hidden)", systemImage: "chevron.down")
                            .frame(maxWidth: .infinity, alignment: .center)
                            .padding(.vertical, 10)
                    }
                    .buttonStyle(.plain)
                    .foregroundStyle(.secondary)
                    .background(Color.secondary.opacity(0.045), in: RoundedRectangle(cornerRadius: 7))
                    .padding(.top, 6)
                }
                if candidates.isEmpty {
                    Text("No candidates match the current filters.")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                        .padding(.horizontal, 8)
                        .padding(.vertical, 16)
                }
            }
        }
    }

    private var timeWindowAuditControls: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .firstTextBaseline, spacing: 10) {
                Toggle("片段核对", isOn: Binding(
                    get: { enablesTimeWindowAudit },
                    set: { newValue in
                        enablesTimeWindowAudit = newValue
                        if !newValue {
                            document.clearClassicAnchorFocus()
                        }
                    }
                ))
                    .toggleStyle(.checkbox)
                Text("显示与指定时间窗重叠的全部候选，包括未选中、拒绝和被压制候选。")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
                Button("Clear") {
                    enablesTimeWindowAudit = false
                    timeWindowTrainQuery = ""
                    timeWindowStartSec = 0
                    timeWindowEndSec = 0
                    document.clearClassicAnchorFocus()
                }
                .liquidGlassButtonStyle()
            }

            HStack(spacing: 12) {
                Picker("Time", selection: $timeWindowMode) {
                    ForEach(RasterTimeMode.allCases) { mode in
                        Text(mode.title).tag(mode)
                    }
                }
                .pickerStyle(.segmented)
                .frame(width: 188)

                TextField("Train name contains", text: $timeWindowTrainQuery)
                    .textFieldStyle(.roundedBorder)
                    .frame(width: 260)

                Text("Start")
                    .foregroundStyle(.secondary)
                DebouncedDoubleField("s", value: $timeWindowStartSec, width: 92, maxFractionDigits: 6)
                Text("s")
                    .foregroundStyle(.secondary)

                Text("End")
                    .foregroundStyle(.secondary)
                DebouncedDoubleField("s", value: $timeWindowEndSec, width: 92, maxFractionDigits: 6)
                Text("s")
                    .foregroundStyle(.secondary)

                Button("带入当前候选") {
                    loadFocusedCandidateWindow()
                }
                .liquidGlassButtonStyle()
                .disabled(document.focusedClassicAnchorEventAnnotation == nil)

                Spacer(minLength: 12)
            }
            .disabled(!enablesTimeWindowAudit)

            if enablesTimeWindowAudit {
                fragmentReviewPanel
            }
        }
        .padding(12)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private var fragmentReviewPanel: some View {
        let snapshots = fragmentReviewSnapshots

        return VStack(alignment: .leading, spacing: 12) {
            HStack(alignment: .center, spacing: 10) {
                Text("片段审查器")
                    .font(.headline)
                Text(fragmentWindowSummary)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
                Text("\(snapshots.count) train(s)")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
            }

            if timeWindowEndSec == timeWindowStartSec {
                Text("输入有效的 Start / End，或先点击候选表中的一行再点“带入当前候选”。")
                    .font(.callout)
                    .foregroundStyle(.secondary)
                    .padding(.vertical, 6)
            } else if snapshots.isEmpty {
                Text("当前片段内没有找到可核对的 ISI 或候选。可以扩大时间窗，或输入更精确的 train 名称。")
                    .font(.callout)
                    .foregroundStyle(.secondary)
                    .padding(.vertical, 6)
            } else {
                ForEach(snapshots) { snapshot in
                    fragmentTrainSection(snapshot)
                }
            }
        }
        .padding(12)
        .background(Color(nsColor: .textBackgroundColor).opacity(0.54), in: RoundedRectangle(cornerRadius: 8))
    }

    private var fragmentWindowSummary: String {
        let lower = min(timeWindowStartSec, timeWindowEndSec)
        let upper = max(timeWindowStartSec, timeWindowEndSec)
        guard upper > lower else {
            return "no active window"
        }
        return "\(timeWindowMode.title) \(TimeFormatting.seconds(lower)) - \(TimeFormatting.seconds(upper))"
    }

    private func fragmentTrainSection(_ snapshot: FragmentTrainReviewSnapshot) -> some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .firstTextBaseline, spacing: 10) {
                Text(snapshot.train.name)
                    .font(.callout.weight(.semibold))
                    .lineLimit(1)
                    .truncationMode(.middle)
                Text("\(snapshot.isiRows.count) local ISI · \(snapshot.candidates.count) candidate(s)")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
            }

            HStack(alignment: .top, spacing: 14) {
                fragmentISITable(snapshot)
                    .frame(width: 650, alignment: .topLeading)
                fragmentCandidateEvidenceList(snapshot)
                    .frame(minWidth: 420, maxWidth: .infinity, alignment: .topLeading)
            }
        }
        .padding(.vertical, 10)
        .overlay(alignment: .bottom) {
            Divider()
        }
    }

    private func fragmentISITable(_ snapshot: FragmentTrainReviewSnapshot) -> some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack(spacing: 8) {
                fragmentHeader("ISI#", 54)
                fragmentHeader("Left", 100)
                fragmentHeader("Right", 100)
                fragmentHeader("ISI", 86)
                fragmentHeader("Selected", 128)
                fragmentHeader("Audit status", 250)
            }
            .padding(.bottom, 5)

            ForEach(snapshot.isiRows.prefix(18)) { row in
                HStack(spacing: 8) {
                    Text("\(row.index)")
                        .monospacedDigit()
                        .frame(width: 54, alignment: .leading)
                    Text(TimeFormatting.seconds(row.leftTimeSec))
                        .monospacedDigit()
                        .frame(width: 100, alignment: .leading)
                    Text(TimeFormatting.seconds(row.rightTimeSec))
                        .monospacedDigit()
                        .frame(width: 100, alignment: .leading)
                    Text(TimeFormatting.seconds(row.isiSec))
                        .monospacedDigit()
                        .frame(width: 86, alignment: .leading)
                    Text(selectedSummary(row))
                        .lineLimit(1)
                        .truncationMode(.middle)
                        .frame(width: 128, alignment: .leading)
                    Text(auditSummary(row))
                        .lineLimit(1)
                        .truncationMode(.middle)
                        .frame(width: 250, alignment: .leading)
                        .help(auditHelp(row))
                }
                .font(.caption)
                .foregroundStyle(rowForeground(row))
                .padding(.vertical, 4)
                .padding(.horizontal, 6)
                .background(rowBackground(row), in: RoundedRectangle(cornerRadius: 5))
            }

            if snapshot.isiRows.count > 18 {
                Text("Showing first 18 local ISIs; narrow the window for more detail.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .padding(.top, 6)
            }
        }
    }

    private func fragmentCandidateEvidenceList(_ snapshot: FragmentTrainReviewSnapshot) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack {
                Text("候选与冲突")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Spacer()
                Text("\(snapshot.selectedCandidateCount) selected")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
            }

            ForEach(snapshot.candidates.prefix(12)) { candidate in
                Button {
                    focus(candidate, section: .alignedRaster)
                } label: {
                    VStack(alignment: .leading, spacing: 4) {
                        HStack(spacing: 8) {
                            Text(candidate.finalLabel.rawValue)
                                .font(.caption.weight(.semibold))
                                .foregroundStyle(labelTint(candidate.finalLabel))
                            Text(candidate.selectedForAuto ? "selected" : candidate.selectionStatus)
                                .font(.caption)
                                .foregroundStyle(candidate.selectedForAuto ? .blue : .secondary)
                            Spacer()
                            Text("ISI \(candidate.startISIIndex)-\(candidate.endISIIndex)")
                                .font(.caption)
                                .foregroundStyle(.secondary)
                        }
                        Text(semanticReason(candidate))
                            .font(.caption)
                            .foregroundStyle(.secondary)
                            .lineLimit(2)
                        Text(candidate.pipelineStageSummary)
                            .font(.caption2)
                            .foregroundStyle(.tertiary)
                            .lineLimit(1)
                            .truncationMode(.middle)
                    }
                    .padding(8)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .background(fragmentCandidateBackground(candidate), in: RoundedRectangle(cornerRadius: 7))
                }
                .buttonStyle(.plain)
            }

            if snapshot.candidates.count > 12 {
                Text("\(snapshot.candidates.count - 12) more candidate(s) hidden in this fragment.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
    }

    private var headerRow: some View {
        HStack(spacing: 12) {
            columnHeader("Review", 116)
            columnHeader("Status", 96)
            columnHeader("Auto", 72)
            columnHeader("Label", 112)
            columnHeader("Train", 280)
            columnHeader("ISI span", 90)
            columnHeader("Spike span", 98)
            columnHeader("Duration", 90)
            columnHeader("Band", 140)
            columnHeader("Contrast", 84)
            columnHeader("Audit", 180)
            columnHeader("Why", 220)
            columnHeader("Stage", 220)
            columnHeader("Decision", 300)
        }
        .padding(.horizontal, 8)
        .padding(.vertical, 7)
    }

    private func candidateRow(_ candidate: ClassicAnchorCandidate) -> some View {
        let isFocused = document.focusedClassicAnchorCandidateID == candidate.id

        return Button {
            focus(candidate, section: .alignedRaster)
        } label: {
            HStack(spacing: 12) {
                reviewLabel(document.reviewStatus(for: candidate.id))
                    .frame(width: 116, alignment: .leading)
                statusLabel(candidate)
                    .frame(width: 96, alignment: .leading)
                autoLabel(candidate)
                    .frame(width: 72, alignment: .leading)
                tableCell(candidate.finalLabel.rawValue, 112)
                Text(candidate.trainName)
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .frame(width: 280, alignment: .leading)
                tableCell("\(candidate.startISIIndex)-\(candidate.endISIIndex)", 90)
                tableCell("\(candidate.startSpikeIndex)-\(candidate.endSpikeIndex)", 98)
                tableCell(time(candidate.durationSec), 90)
                tableCell("\(time(candidate.anchorBandLowerSec)) - \(time(candidate.anchorBandUpperSec))", 140)
                tableCell(number(candidate.edgeContrastMinQ90), 84)
                Text(auditSummary(candidate))
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .frame(width: 180, alignment: .leading)
                    .help(auditHelp(candidate))
                Text(semanticReason(candidate))
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .frame(width: 220, alignment: .leading)
                    .help(semanticReason(candidate))
                Text(candidate.pipelineStageSummary)
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .frame(width: 220, alignment: .leading)
                    .help(candidate.pipelineStageSummary)
                Text(candidate.decisionPath)
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .frame(width: 300, alignment: .leading)
                    .help(candidate.decisionPath)
            }
            .contentShape(RoundedRectangle(cornerRadius: 7))
        }
        .font(.callout)
        .padding(.horizontal, 8)
        .padding(.vertical, 6)
        .background(rowColor(candidate, isFocused: isFocused), in: RoundedRectangle(cornerRadius: 7))
        .overlay {
            if isFocused {
                RoundedRectangle(cornerRadius: 7)
                    .stroke(Color.accentColor.opacity(0.78), lineWidth: 1.4)
            }
        }
        .buttonStyle(.plain)
        .contextMenu {
            Button("在 raster 查看") {
                focus(candidate, section: .alignedRaster)
            }
            Button("在 ISI 时间剖面查看") {
                focus(candidate, section: .isiProfile)
            }
            Divider()
            Button("Accept") {
                document.setReviewStatus(.accepted, for: candidate.id)
            }
            Button("Needs review") {
                document.setReviewStatus(.needsReview, for: candidate.id)
            }
            Button("Reject") {
                document.setReviewStatus(.rejected, for: candidate.id)
            }
            Button("Clear review") {
                document.setReviewStatus(.unreviewed, for: candidate.id)
            }
        }
    }

    private func loadFocusedCandidateWindow() {
        guard let annotation = document.focusedClassicAnchorEventAnnotation else {
            return
        }
        timeWindowTrainQuery = annotation.trainName
        switch timeWindowMode {
        case .aligned:
            timeWindowStartSec = annotation.alignedStartSec
            timeWindowEndSec = annotation.alignedEndSec
        case .raw:
            timeWindowStartSec = annotation.rawStartSec
            timeWindowEndSec = annotation.rawEndSec
        }
    }

    private func fragmentISIRows(
        train: SpikeTrain,
        queryStart: Double,
        queryEnd: Double,
        candidates: [ClassicAnchorCandidate]
    ) -> [FragmentISIRow] {
        let overlappingIndices = train.isiSec.indices.dropFirst().filter { index in
            guard train.timestampsSec.indices.contains(index - 1),
                  train.timestampsSec.indices.contains(index),
                  train.isiSec[index] != nil else {
                return false
            }
            let left = train.rasterTimestamp(at: index - 1, mode: timeWindowMode)
            let right = train.rasterTimestamp(at: index, mode: timeWindowMode)
            return right >= queryStart && left <= queryEnd
        }
        guard let first = overlappingIndices.first,
              let last = overlappingIndices.last else {
            return []
        }

        let lower = max(1, first - 2)
        let upper = min(train.isiSec.count - 1, last + 2)
        guard lower <= upper else {
            return []
        }

        let trainCandidates = candidates
            .filter { $0.trainID == train.id }
            .sorted(by: fragmentCandidateSort)

        return (lower...upper).compactMap { index in
            guard train.timestampsSec.indices.contains(index - 1),
                  train.timestampsSec.indices.contains(index),
                  let isi = train.isiSec[index],
                  isi.isFinite else {
                return nil
            }
            let coveringCandidates = trainCandidates.filter { candidate in
                let candidateStart = min(candidate.startISIIndex, candidate.endISIIndex)
                let candidateEnd = max(candidate.startISIIndex, candidate.endISIIndex)
                return candidateStart <= index && index <= candidateEnd
            }
            let inRequestedWindow = overlappingIndices.contains(index)
            return FragmentISIRow(
                index: index,
                leftTimeSec: train.rasterTimestamp(at: index - 1, mode: timeWindowMode),
                rightTimeSec: train.rasterTimestamp(at: index, mode: timeWindowMode),
                isiSec: isi,
                inRequestedWindow: inRequestedWindow,
                candidates: coveringCandidates
            )
        }
    }

    private func fragmentCandidateSort(_ lhs: ClassicAnchorCandidate, _ rhs: ClassicAnchorCandidate) -> Bool {
        if lhs.selectedForAuto != rhs.selectedForAuto {
            return lhs.selectedForAuto
        }
        if lhs.auditRecommendedTrack != rhs.auditRecommendedTrack {
            return lhs.auditRecommendedTrack.rawValue < rhs.auditRecommendedTrack.rawValue
        }
        if lhs.priority != rhs.priority {
            return lhs.priority > rhs.priority
        }
        if lhs.startISIIndex != rhs.startISIIndex {
            return lhs.startISIIndex < rhs.startISIIndex
        }
        return lhs.id < rhs.id
    }

    private func fragmentHeader(_ title: String, _ width: CGFloat) -> some View {
        Text(title)
            .font(.caption2.weight(.semibold))
            .foregroundStyle(.secondary)
            .frame(width: width, alignment: .leading)
    }

    private func selectedSummary(_ row: FragmentISIRow) -> String {
        let selected = row.candidates.filter(\.selectedForAuto)
        guard !selected.isEmpty else {
            return "none"
        }
        return selected
            .map { $0.finalLabel.rawValue }
            .uniquedPreservingOrder()
            .joined(separator: ", ")
    }

    private func auditSummary(_ row: FragmentISIRow) -> String {
        if row.candidates.isEmpty {
            return row.inRequestedWindow ? "no candidate covers this ISI" : "context only"
        }
        let selected = row.candidates.filter(\.selectedForAuto)
        if !selected.isEmpty {
            return selected
                .map { "\($0.finalLabel.rawValue): \($0.auditReviewStatus)" }
                .uniquedPreservingOrder()
                .joined(separator: " · ")
        }
        return row.candidates
            .prefix(3)
            .map { "\($0.finalLabel.rawValue): \($0.selectionStatus)" }
            .joined(separator: " · ")
    }

    private func auditHelp(_ row: FragmentISIRow) -> String {
        guard !row.candidates.isEmpty else {
            return "No candidate covers ISI \(row.index)."
        }
        return row.candidates.map { candidate in
            [
                "\(candidate.finalLabel.rawValue) · \(candidate.selectedForAuto ? "selected" : "not selected")",
                "track=\(candidate.auditRecommendedTrackRawValue)",
                "isi_span=\(candidate.startISIIndex)-\(candidate.endISIIndex)",
                "status=\(candidate.selectionStatus)",
                "stage=\(candidate.pipelineStageSummary)",
                "why=\(semanticReason(candidate))",
                "decision=\(candidate.decisionPath)"
            ].joined(separator: "\n")
        }
        .joined(separator: "\n\n")
    }

    private func rowForeground(_ row: FragmentISIRow) -> Color {
        if row.candidates.contains(where: { $0.selectedForAuto }) {
            return .primary
        }
        if row.candidates.contains(where: { $0.auditRecommendedTrack == .diagnostic }) {
            return .red
        }
        if row.candidates.isEmpty {
            return row.inRequestedWindow ? .secondary : Color.secondary.opacity(0.72)
        }
        return .secondary
    }

    private func rowBackground(_ row: FragmentISIRow) -> Color {
        if row.candidates.contains(where: { $0.selectedForAuto }) {
            return Color.blue.opacity(0.075)
        }
        if row.candidates.contains(where: { $0.auditRecommendedTrack == .diagnostic }) {
            return Color.red.opacity(0.055)
        }
        if !row.inRequestedWindow {
            return Color.secondary.opacity(0.030)
        }
        if row.candidates.isEmpty {
            return Color.yellow.opacity(0.045)
        }
        return Color.secondary.opacity(0.045)
    }

    private func labelTint(_ label: ClassicAnchorLabel) -> Color {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst:
            return .orange
        case .possibleBurst:
            return .yellow
        case .pause:
            return .blue
        case .tonic, .highFrequencyTonic:
            return .green
        case .highFrequencySpiking:
            return .red
        case .reject:
            return .red
        case .profile:
            return .secondary
        }
    }

    private func fragmentCandidateBackground(_ candidate: ClassicAnchorCandidate) -> Color {
        if candidate.selectedForAuto {
            return labelTint(candidate.finalLabel).opacity(0.10)
        }
        if candidate.auditRecommendedTrack == .diagnostic {
            return Color.red.opacity(0.055)
        }
        if candidate.auditRecommendedTrack == .review {
            return Color.orange.opacity(0.055)
        }
        return Color.secondary.opacity(0.040)
    }

    private func candidatePassesFilters(
        _ candidate: ClassicAnchorCandidate,
        annotationsByCandidateID: [String: ClassicAnchorEventAnnotation]
    ) -> Bool {
        if showsAutoSelectedOnly, !candidate.selectedForAuto {
            return false
        }
        guard let label = labelFilter.label else {
            return candidatePassesTimeWindowAudit(candidate, annotationsByCandidateID: annotationsByCandidateID)
        }
        return candidate.finalLabel == label &&
            candidatePassesTimeWindowAudit(candidate, annotationsByCandidateID: annotationsByCandidateID)
    }

    private func candidatePassesTimeWindowAudit(
        _ candidate: ClassicAnchorCandidate,
        annotationsByCandidateID: [String: ClassicAnchorEventAnnotation]
    ) -> Bool {
        guard hasValidTimeWindowAuditRange else {
            return true
        }
        let trainQuery = timeWindowTrainQuery.trimmingCharacters(in: .whitespacesAndNewlines)
        if !trainQuery.isEmpty,
           candidate.trainName.range(of: trainQuery, options: [.caseInsensitive, .diacriticInsensitive]) == nil {
            return false
        }
        guard let annotation = annotationsByCandidateID[candidate.id] else {
            return false
        }
        let queryStart = min(timeWindowStartSec, timeWindowEndSec)
        let queryEnd = max(timeWindowStartSec, timeWindowEndSec)
        let candidateStart: Double
        let candidateEnd: Double
        switch timeWindowMode {
        case .aligned:
            candidateStart = annotation.alignedStartSec
            candidateEnd = annotation.alignedEndSec
        case .raw:
            candidateStart = annotation.rawStartSec
            candidateEnd = annotation.rawEndSec
        }
        return candidateEnd >= queryStart && candidateStart <= queryEnd
    }

    private func statusLabel(_ candidate: ClassicAnchorCandidate) -> some View {
        let locked = candidate.anchorLockLevel == .lockedClassic
        let audit = candidate.anchorLockLevel == .auditOnly
        let tint: Color = locked ? .green : (audit ? .secondary : .orange)
        return Text(locked ? "Locked" : (audit ? "Audit" : "Strong"))
            .font(.caption.weight(.semibold))
            .foregroundStyle(tint)
            .padding(.horizontal, 8)
            .padding(.vertical, 3)
            .background(tint.opacity(audit ? 0.08 : 0.12), in: Capsule())
    }

    private func reviewLabel(_ status: ClassicAnchorReviewStatus) -> some View {
        Label(status.title, systemImage: status.systemImage)
            .font(.caption.weight(.semibold))
            .foregroundStyle(status.tint)
            .padding(.horizontal, 8)
            .padding(.vertical, 3)
            .background(status.tint.opacity(0.11), in: Capsule())
    }

    private func autoLabel(_ candidate: ClassicAnchorCandidate) -> some View {
        Text(candidate.selectedForAuto ? "Auto" : "No")
            .font(.caption.weight(.semibold))
            .foregroundStyle(candidate.selectedForAuto ? Color.blue : Color.secondary)
            .padding(.horizontal, 8)
            .padding(.vertical, 3)
            .background(
                (candidate.selectedForAuto ? Color.blue.opacity(0.11) : Color.secondary.opacity(0.08)),
                in: Capsule()
            )
            .help(candidate.selectionStatus)
    }

    private func rowColor(_ candidate: ClassicAnchorCandidate, isFocused: Bool) -> Color {
        if isFocused {
            return Color.accentColor.opacity(0.105)
        }
        switch candidate.anchorLockLevel {
        case .lockedClassic:
            return Color.green.opacity(0.055)
        case .strongCandidate:
            return Color.orange.opacity(0.070)
        case .auditOnly:
            return Color.secondary.opacity(0.035)
        }
    }

    private func focus(_ candidate: ClassicAnchorCandidate, section: WorkbenchSection) {
        document.focusClassicAnchorCandidate(candidate.id, adjustRasterReviewWindow: section == .alignedRaster || section == .rawRaster)
        selectedSection = section
    }

    private func semanticReason(_ candidate: ClassicAnchorCandidate) -> String {
        if !candidate.reviewEvidenceSummary.isEmpty {
            return candidate.reviewEvidenceSummary
        }
        return candidate.auditReasonSummary
    }

    private func auditSummary(_ candidate: ClassicAnchorCandidate) -> String {
        let strength = candidate.reviewEvidenceStrength.isEmpty ? "" : " · \(candidate.reviewEvidenceStrength)"
        return "\(candidate.auditRecommendedTrackRawValue) · \(candidate.auditRecommendedEventTrackClass) · \(candidate.auditRecommendedSubtype)\(strength) · \(candidate.auditReviewStatus)"
    }

    private func auditHelp(_ candidate: ClassicAnchorCandidate) -> String {
        [
            "track=\(candidate.auditRecommendedTrackRawValue)",
            "family=\(candidate.auditRecommendedFamily)",
            "subtype=\(candidate.auditRecommendedSubtype)",
            "final=\(candidate.auditRecommendedFinalClass)",
            "event_track=\(candidate.auditRecommendedEventTrackClass)",
            "audit_status=\(candidate.auditReviewStatus)",
            "confidence=\(candidate.auditConfidenceTier)",
            candidate.reviewEvidenceClass.isEmpty ? nil : "review_evidence_class=\(candidate.reviewEvidenceClass)",
            candidate.reviewEvidenceStrength.isEmpty ? nil : "review_evidence_strength=\(candidate.reviewEvidenceStrength)",
            candidate.reviewEvidenceSummary.isEmpty ? nil : "review_evidence_summary=\(candidate.reviewEvidenceSummary)",
            candidate.auditUncertaintyReason.isEmpty ? nil : "uncertainty=\(candidate.auditUncertaintyReason)",
            candidate.auditLongBurstDefinitionStatus.isEmpty ? nil : "long_burst_status=\(candidate.auditLongBurstDefinitionStatus)"
        ]
        .compactMap { $0 }
        .joined(separator: "\n")
    }

    private func columnHeader(_ title: String, _ width: CGFloat) -> some View {
        Text(title)
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
            .frame(width: width, alignment: .leading)
    }

    private func tableCell(_ value: String, _ width: CGFloat) -> some View {
        Text(value)
            .lineLimit(1)
            .truncationMode(.middle)
            .frame(width: width, alignment: .leading)
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
}

private enum StructuralCandidatesScrollTarget: Hashable {
    case top
}

private struct FragmentTrainReviewSnapshot: Identifiable {
    let train: SpikeTrain
    let isiRows: [FragmentISIRow]
    let candidates: [ClassicAnchorCandidate]

    var id: String {
        train.id
    }

    var selectedCandidateCount: Int {
        candidates.filter(\.selectedForAuto).count
    }
}

private struct FragmentISIRow: Identifiable {
    let index: Int
    let leftTimeSec: Double
    let rightTimeSec: Double
    let isiSec: Double
    let inRequestedWindow: Bool
    let candidates: [ClassicAnchorCandidate]

    var id: Int {
        index
    }
}

private extension Array where Element: Hashable {
    func uniquedPreservingOrder() -> [Element] {
        var seen = Set<Element>()
        var values: [Element] = []
        values.reserveCapacity(count)
        for value in self where seen.insert(value).inserted {
            values.append(value)
        }
        return values
    }
}

private enum CandidateLabelFilter: String, CaseIterable, Identifiable {
    case all
    case burst
    case highFrequencyBurst
    case longBurst
    case possibleBurst
    case highFrequencySpiking
    case highFrequencyTonic
    case tonic
    case pause
    case reject

    var id: String {
        rawValue
    }

    var title: String {
        switch self {
        case .all:
            return "All patterns"
        case .burst:
            return "Burst"
        case .highFrequencyBurst:
            return "HF burst"
        case .longBurst:
            return "Long burst"
        case .possibleBurst:
            return "Burst II"
        case .highFrequencySpiking:
            return "HFS"
        case .highFrequencyTonic:
            return "HF tonic"
        case .tonic:
            return "Tonic"
        case .pause:
            return "Pause"
        case .reject:
            return "Reject"
        }
    }

    var label: ClassicAnchorLabel? {
        switch self {
        case .all:
            return nil
        case .burst:
            return .burst
        case .highFrequencyBurst:
            return .highFrequencyBurst
        case .longBurst:
            return .longBurst
        case .possibleBurst:
            return .possibleBurst
        case .highFrequencySpiking:
            return .highFrequencySpiking
        case .highFrequencyTonic:
            return .highFrequencyTonic
        case .tonic:
            return .tonic
        case .pause:
            return .pause
        case .reject:
            return .reject
        }
    }
}
