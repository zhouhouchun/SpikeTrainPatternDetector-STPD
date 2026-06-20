import STPDCore
import SwiftUI

struct DetectorParametersView: View {
    @Bindable var document: RasterDocument

    var body: some View {
        ScrollView([.vertical, .horizontal]) {
            VStack(alignment: .leading, spacing: 16) {
                header

                if let dataset = document.dataset {
                    detectorControls(dataset: dataset)
                    eventPatternControls
                    statePatternControls

                    if let run = document.classicAnchorDetectionRun {
                        summaryStrip(run: run)
                        adaptiveBandTable(run: run)
                        candidateAuditTable(run: run)
                    } else {
                        ContentUnavailableView(
                            "No Detection Run",
                            systemImage: "switch.2",
                            description: Text(document.detectorStatusMessage)
                        )
                        .frame(maxWidth: .infinity, minHeight: 320)
                    }
                } else {
                    ContentUnavailableView(
                        "No Dataset",
                        systemImage: "switch.2",
                        description: Text(document.lastErrorMessage ?? "Open a raw spike timestamp CSV or load the bundled sample.")
                    )
                    .frame(maxWidth: .infinity, minHeight: 360)
                }
            }
            .padding(18)
            .frame(minWidth: 980, maxWidth: .infinity, alignment: .topLeading)
        }
        .frame(minWidth: 720)
    }

    private var header: some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Image(systemName: "switch.2")
                .foregroundStyle(.secondary)
            Text("检测器 / 参数")
                .font(.title3.weight(.semibold))
            Text("Live")
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(.quaternary, in: Capsule())
            Spacer(minLength: 28)
            Button {
                document.runAdaptiveClassicAnchorDetection()
            } label: {
                Label(document.hasDetectorResults ? "重新检测" : "运行检测", systemImage: "play.fill")
            }
            .liquidGlassButtonStyle(prominent: true)
            .disabled(document.isDetectorRunning || document.dataset == nil)
        }
    }

    private func detectorControls(dataset: SpikeDataset) -> some View {
        HStack(alignment: .center, spacing: 18) {
            detectorMetric("Trains", "\(dataset.trains.count)")
            detectorMetric("Spikes", "\(dataset.totalSpikeCount)")
            detectorMetric("Min valid ISI", TimeFormatting.seconds(document.adaptiveDetectorBandSettings.minValidISISec))

            HStack(spacing: 8) {
                Text("Histogram bin")
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .fixedSize(horizontal: true, vertical: false)
                    .help("Only controls ISI distribution summaries and adaptive diagnostics. Burst seed bands are derived from structural Burst I anchors, not from a default histogram interval.")
                DebouncedDoubleField(
                    "5",
                    value: histogramBinBinding,
                    width: 70
                )
                .help("Diagnostic histogram bin width in milliseconds.")
                Text("ms")
                    .foregroundStyle(.secondary)
            }

            detectorMetric("Refractory", TimeFormatting.seconds(document.qualitySettings.refractorySuspectThresholdSec))

            Spacer(minLength: 18)

            if document.isDetectorRunning {
                ProgressView()
                    .controlSize(.small)
            }

            Text(document.detectorStatusMessage)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .truncationMode(.tail)
        }
        .padding(14)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private var histogramBinBinding: Binding<Double> {
        Binding(
            get: { document.detectorHistogramBinWidthMs },
            set: { value in
                document.detectorHistogramBinWidthMs = max(0.1, value)
            }
        )
    }

    private var eventPatternControls: some View {
        VStack(alignment: .leading, spacing: 10) {
            sectionTitle("Event pattern parameters")

            HStack(alignment: .top, spacing: 12) {
                parameterCard(title: "Classic burst") {
                    doubleParameter("Contrast I", keyPath: \.detectorClassicBurstContrastMin, lower: 1, upper: 30)
                    doubleParameter("Pause flank", keyPath: \.detectorClassicBurstFlankPauseContrastMin, lower: document.detectorClassicBurstContrastMin, upper: 50)
                    intParameter("Min spikes", keyPath: \.detectorClassicBurstMinSpikes, range: 3...100)
                    intParameter("Max spikes", keyPath: \.detectorClassicBurstMaxSpikes, range: 3...100)
                    Text("Contrast I accepts Burst I. Pause flank only seeds pause when the boundary ISI is this many times intra-burst q90.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }

                parameterCard(title: "Pause") {
                    doubleParameter("Min ISI ms", keyPath: \.detectorPauseMinISIMs, lower: 0, upper: nil)
                    Text("0 = auto. Manual values override the train-adaptive pause lower bound after structural burst flanks and tonic guards are computed.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }
            }
        }
    }

    private var statePatternControls: some View {
        VStack(alignment: .leading, spacing: 10) {
            sectionTitle("State pattern parameters")

            HStack(alignment: .top, spacing: 12) {
                parameterCard(title: "Tonic") {
                    intParameter("Min spikes", keyPath: \.detectorTonicMinSpikes, range: 3...500)
                    doubleParameter("CV max", keyPath: \.detectorTonicCVMax, lower: 0.01, upper: 5)
                    doubleParameter("CV2 max", keyPath: \.detectorTonicCV2Max, lower: 0.01, upper: 5)
                    doubleParameter("LV max", keyPath: \.detectorTonicLVMax, lower: 0.01, upper: 5)
                    doubleParameter("MM min", keyPath: \.detectorTonicMMMin, lower: 0.01, upper: 10)
                    doubleParameter("MM max", keyPath: \.detectorTonicMMMax, lower: document.detectorTonicMMMin, upper: 10)
                    doubleParameter("Burst frac", keyPath: \.detectorTonicBurstSeedFractionMax, lower: 0, upper: 1)
                }

                parameterCard(title: "HF tonic") {
                    intParameter("Min spikes", keyPath: \.detectorHighFrequencyTonicMinSpikes, range: 3...500)
                    doubleParameter("Low tail", keyPath: \.detectorHighFrequencyTonicLowTailFractionMax, lower: 0, upper: 1)
                    doubleParameter("CV max", keyPath: \.detectorHighFrequencyTonicCVMax, lower: 0.01, upper: 5)
                    doubleParameter("CV2 max", keyPath: \.detectorHighFrequencyTonicCV2Max, lower: 0.01, upper: 5)
                    doubleParameter("LV max", keyPath: \.detectorHighFrequencyTonicLVMax, lower: 0.01, upper: 5)
                    doubleParameter("MM max", keyPath: \.detectorHighFrequencyTonicMMMax, lower: 0.01, upper: 10)
                }

                parameterCard(title: "HFS") {
                    intParameter("Min spikes", keyPath: \.detectorHighFrequencySpikingMinSpikes, range: 3...2000)
                    doubleParameter("Short frac", keyPath: \.detectorHighFrequencySpikingShortFractionMin, lower: 0, upper: 1)
                    doubleParameter("Large frac", keyPath: \.detectorHighFrequencySpikingAllowedLargeFraction, lower: 0, upper: 1)
                    intParameter("Max large", keyPath: \.detectorHighFrequencySpikingMaxConsecutiveLargeISI, range: 0...50)
                }
            }
        }
    }

    private func parameterCard<Content: View>(
        title: String,
        @ViewBuilder content: () -> Content
    ) -> some View {
        VStack(alignment: .leading, spacing: 10) {
            Text(title)
                .font(.headline)
            VStack(alignment: .leading, spacing: 8) {
                content()
            }
        }
        .padding(14)
        .frame(width: 302, alignment: .topLeading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private func doubleParameter(
        _ label: String,
        keyPath: ReferenceWritableKeyPath<RasterDocument, Double>,
        lower: Double,
        upper: Double? = nil
    ) -> some View {
        HStack(spacing: 8) {
            Text(label)
                .foregroundStyle(.secondary)
                .frame(width: 92, alignment: .leading)
            DebouncedDoubleField(
                "0",
                value: boundedDoubleBinding(keyPath, lower: lower, upper: upper),
                width: 76,
                maxFractionDigits: 3
            )
        }
    }

    private func intParameter(
        _ label: String,
        keyPath: ReferenceWritableKeyPath<RasterDocument, Int>,
        range: ClosedRange<Int>
    ) -> some View {
        HStack(spacing: 8) {
            Text(label)
                .foregroundStyle(.secondary)
                .frame(width: 92, alignment: .leading)
            DebouncedIntField(
                "0",
                value: boundedIntBinding(keyPath, range: range),
                range: range,
                width: 76
            )
        }
    }

    private func boundedDoubleBinding(
        _ keyPath: ReferenceWritableKeyPath<RasterDocument, Double>,
        lower: Double,
        upper: Double?
    ) -> Binding<Double> {
        Binding(
            get: { document[keyPath: keyPath] },
            set: { value in
                let finite = value.isFinite ? value : lower
                let lowered = max(finite, lower)
                document[keyPath: keyPath] = upper.map { min(lowered, $0) } ?? lowered
            }
        )
    }

    private func boundedIntBinding(
        _ keyPath: ReferenceWritableKeyPath<RasterDocument, Int>,
        range: ClosedRange<Int>
    ) -> Binding<Int> {
        Binding(
            get: { document[keyPath: keyPath] },
            set: { value in
                document[keyPath: keyPath] = min(max(value, range.lowerBound), range.upperBound)
            }
        )
    }

    private func detectorMetric(_ title: String, _ value: String) -> some View {
        VStack(alignment: .leading, spacing: 3) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(value)
                .font(.callout.weight(.semibold))
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
        }
    }

    private func summaryStrip(run: ClassicAnchorDetectionRun) -> some View {
        HStack(spacing: 10) {
            DetectorSummaryTile(title: "Candidates", value: "\(run.candidateCount)")
            DetectorSummaryTile(title: "Locked", value: "\(run.lockedClassicCount)", level: .good)
            DetectorSummaryTile(title: "Strong", value: "\(run.strongCandidateCount)", level: run.strongCandidateCount > 0 ? .warning : .plain)
            DetectorSummaryTile(title: "Burst", value: "\(run.burstCount)")
            DetectorSummaryTile(title: "HF burst", value: "\(run.highFrequencyBurstCount)")
            DetectorSummaryTile(title: "Long burst", value: "\(run.longBurstCount)")
            DetectorSummaryTile(title: "Review", value: "\(run.selectedReviewCount)", level: run.selectedReviewCount > 0 ? .warning : .plain)
            DetectorSummaryTile(title: "HFS", value: "\(run.highFrequencySpikingCount)", level: run.highFrequencySpikingCount > 0 ? .warning : .plain)
            DetectorSummaryTile(title: "HF tonic", value: "\(run.highFrequencyTonicCount)")
            DetectorSummaryTile(title: "Tonic", value: "\(run.tonicCount)")
            DetectorSummaryTile(title: "Pause", value: "\(run.pauseCount)")
            DetectorSummaryTile(title: "Auto", value: "\(run.selectedAutoCount)", level: .good)
            DetectorSummaryTile(title: "Event track", value: "\(run.selectedEventCount)", level: .good)
            DetectorSummaryTile(title: "Gap track", value: "\(run.selectedGapCount)", level: run.selectedGapCount > 0 ? .warning : .plain)
            DetectorSummaryTile(title: "State track", value: "\(run.selectedStateCount)", level: run.selectedStateCount > 0 ? .warning : .plain)
        }
    }

    private func adaptiveBandTable(run: ClassicAnchorDetectionRun) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            tableTitle("Adaptive burst bands")

            VStack(alignment: .leading, spacing: 0) {
                HStack(spacing: 12) {
                    tableHeader("Train", width: 280)
                    tableHeader("Valid ISI", width: 72)
                    tableHeader("Seed lower", width: 108)
                    tableHeader("Seed upper", width: 108)
                    tableHeader("Bridge", width: 108)
                    tableHeader("S", width: 50)
                    tableHeader("Source", width: 86)
                    tableHeader("Candidates", width: 82)
                    tableHeader("Locked", width: 70)
                }
                .padding(.horizontal, 8)
                .padding(.vertical, 7)

                Divider()

                ForEach(run.resolutions, id: \.trainID) { resolution in
                    let result = run.result(for: resolution.trainID)
                    adaptiveBandRow(resolution: resolution, result: result)
                }
            }
        }
    }

    private func adaptiveBandRow(
        resolution: TrainAdaptiveBandResolution,
        result: ClassicAnchorDetectionResult?
    ) -> some View {
        let burst = resolution.burstBand
        let candidates = result?.candidates ?? []
        let locked = candidates.filter { $0.anchorLockLevel == .lockedClassic }

        return HStack(spacing: 12) {
            Text(resolution.trainName)
                .lineLimit(1)
                .truncationMode(.middle)
                .frame(width: 280, alignment: .leading)
            tableCell("\(resolution.validISICount)", width: 72)
            tableCell(time(burst?.seedLowerSec), width: 108)
            tableCell(time(burst?.seedUpperSec), width: 108)
            tableCell(time(burst?.bridgeUpperSec), width: 108)
            tableCell(number(burst?.contrastS), width: 50)
            tableCell(burst?.primarySource.rawValue ?? "none", width: 86)
            tableCell("\(candidates.count)", width: 82)
            tableCell("\(locked.count)", width: 70)
        }
        .font(.callout)
        .padding(.horizontal, 8)
        .padding(.vertical, 6)
        .background(rowFill(level: locked.isEmpty ? .plain : .good), in: RoundedRectangle(cornerRadius: 7))
    }

    private func candidateAuditTable(run: ClassicAnchorDetectionRun) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            tableTitle("Candidate audit")

            VStack(alignment: .leading, spacing: 0) {
                HStack(spacing: 12) {
                    tableHeader("Status", width: 96)
                    tableHeader("Label", width: 104)
                    tableHeader("Train", width: 250)
                    tableHeader("Spikes", width: 60)
                    tableHeader("Start ISI", width: 72)
                    tableHeader("End ISI", width: 72)
                    tableHeader("Duration", width: 92)
                    tableHeader("Band", width: 138)
                    tableHeader("Edge contrast", width: 104)
                    tableHeader("Score", width: 66)
                    tableHeader("Reason", width: 280)
                }
                .padding(.horizontal, 8)
                .padding(.vertical, 7)

                Divider()

                ForEach(candidateRows(run: run)) { candidate in
                    candidateRow(candidate)
                }
            }
        }
    }

    private func candidateRows(run: ClassicAnchorDetectionRun) -> [ClassicAnchorCandidate] {
        run.candidates.sorted { lhs, rhs in
            if lhs.anchorLockLevel != rhs.anchorLockLevel {
                return lockSortRank(lhs.anchorLockLevel) < lockSortRank(rhs.anchorLockLevel)
            }
            if lhs.priority != rhs.priority {
                return lhs.priority > rhs.priority
            }
            return lhs.score > rhs.score
        }
    }

    private func candidateRow(_ candidate: ClassicAnchorCandidate) -> some View {
        let lockTint = lockColor(candidate.anchorLockLevel)
        return HStack(spacing: 12) {
            Text(lockLabel(candidate.anchorLockLevel))
                .font(.caption.weight(.semibold))
                .foregroundStyle(lockTint)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(
                    lockTint.opacity(candidate.anchorLockLevel == .auditOnly ? 0.08 : 0.12),
                    in: Capsule()
                )
                .frame(width: 96, alignment: .leading)
            tableCell(candidate.finalLabel.rawValue, width: 104)
            Text(candidate.trainName)
                .lineLimit(1)
                .truncationMode(.middle)
                .frame(width: 250, alignment: .leading)
            tableCell("\(candidate.nSpikes)", width: 60)
            tableCell("\(candidate.startISIIndex)", width: 72)
            tableCell("\(candidate.endISIIndex)", width: 72)
            tableCell(time(candidate.durationSec), width: 92)
            tableCell("\(time(candidate.anchorBandLowerSec)) - \(time(candidate.anchorBandUpperSec))", width: 138)
            tableCell(number(candidate.edgeContrastMinQ90), width: 104)
            tableCell(number(candidate.score), width: 66)
            tableCell(candidateShortReason(candidate), width: 280)
        }
        .font(.callout)
        .padding(.horizontal, 8)
        .padding(.vertical, 6)
        .background(rowFill(level: rowLevel(candidate.anchorLockLevel)), in: RoundedRectangle(cornerRadius: 7))
    }

    private func lockSortRank(_ lock: ClassicAnchorLockLevel) -> Int {
        switch lock {
        case .lockedClassic:
            return 0
        case .strongCandidate:
            return 1
        case .auditOnly:
            return 2
        }
    }

    private func lockLabel(_ lock: ClassicAnchorLockLevel) -> String {
        switch lock {
        case .lockedClassic:
            return "Locked"
        case .strongCandidate:
            return "Strong"
        case .auditOnly:
            return "Audit"
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

    private func candidateShortReason(_ candidate: ClassicAnchorCandidate) -> String {
        if !candidate.reviewEvidenceSummary.isEmpty {
            return candidate.reviewEvidenceSummary
        }
        if !candidate.possibleBurstStructureSummary.isEmpty {
            return candidate.possibleBurstStructureSummary
        }
        return candidate.auditReasonSummary
    }

    private func rowLevel(_ lock: ClassicAnchorLockLevel) -> RowLevel {
        switch lock {
        case .lockedClassic:
            return .good
        case .strongCandidate:
            return .warning
        case .auditOnly:
            return .plain
        }
    }

    private func tableTitle(_ title: String) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Text(title)
                .font(.headline)
            if let date = document.detectorLastRunDate {
                Text(date.formatted(date: .omitted, time: .standard))
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
    }

    private func sectionTitle(_ title: String) -> some View {
        Text(title)
            .font(.headline)
    }

    private func tableHeader(_ title: String, width: CGFloat) -> some View {
        Text(title)
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
            .frame(width: width, alignment: .leading)
    }

    private func tableCell(_ value: String, width: CGFloat) -> some View {
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
        if abs(value) >= 100 {
            return String(format: "%.1f", value)
        }
        if abs(value) >= 10 {
            return String(format: "%.2f", value)
        }
        return String(format: "%.3f", value)
    }

    private enum RowLevel {
        case plain
        case good
        case warning
    }

    private func rowFill(level: RowLevel) -> Color {
        switch level {
        case .plain:
            return Color.clear
        case .good:
            return Color.green.opacity(0.055)
        case .warning:
            return Color.orange.opacity(0.070)
        }
    }
}

private struct DetectorSummaryTile: View {
    let title: String
    let value: String
    var level: Level = .plain

    enum Level {
        case plain
        case good
        case warning
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(value)
                .font(.title3.weight(.semibold))
        }
        .padding(14)
        .frame(minWidth: 132, maxWidth: .infinity, alignment: .leading)
        .background(fill, in: RoundedRectangle(cornerRadius: 8))
    }

    private var fill: Color {
        switch level {
        case .plain:
            return Color(nsColor: .textBackgroundColor).opacity(0.62)
        case .good:
            return Color.green.opacity(0.10)
        case .warning:
            return Color.orange.opacity(0.12)
        }
    }
}
