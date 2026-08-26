import STPDCore
import AppKit
import SwiftUI

private struct ResultTableDisplayPackage: Sendable, Hashable {
    enum Origin: Sendable, Hashable {
        case currentRun
        case verifiedReadback
    }

    struct Table: Sendable, Hashable {
        let fileName: String
        let rowCount: Int
        let columns: [STPDResultColumnDefinition]
        let rows: [[String]]
    }

    struct ConsistencyCheck: Sendable, Hashable {
        let id: String
        let status: String
        let severity: String
        let details: String
    }

    let origin: Origin
    let schemaVersion: String
    let detectorVersion: String
    let runID: String
    let datasetDigest: String
    let settingsDigest: String
    let sourceMode: String
    let ownerName: String
    let ownerEmail: String
    let tables: [Table]
    let consistencyChecks: [ConsistencyCheck]

    init(readback: STPDResultPackageReadResult) {
        origin = .verifiedReadback
        schemaVersion = readback.schemaVersion
        detectorVersion = readback.detectorVersion
        runID = readback.runID
        datasetDigest = readback.datasetDigest
        settingsDigest = readback.settingsDigest
        sourceMode = readback.sourceMode
        ownerName = readback.ownerName
        ownerEmail = readback.ownerEmail
        tables = readback.tables.map {
            Table(
                fileName: $0.fileName,
                rowCount: $0.rowCount,
                columns: $0.columns,
                rows: $0.rows
            )
        }
        consistencyChecks = readback.consistencyChecks.map {
            ConsistencyCheck(
                id: $0.id,
                status: $0.status,
                severity: $0.severity,
                details: $0.details
            )
        }
    }

    init(currentRun package: STPDResultPackage) throws {
        origin = .currentRun
        schemaVersion = package.manifest.schemaVersion
        detectorVersion = package.manifest.detectorVersion
        runID = package.manifest.runID
        datasetDigest = package.manifest.datasetDigest
        settingsDigest = package.manifest.settingsDigest
        sourceMode = package.manifest.sourceMode
        ownerName = package.manifest.ownerName
        ownerEmail = package.manifest.ownerEmail

        tables = try package.manifest.tables.map { manifestTable in
            guard let tableID = STPDResultTable(rawValue: manifestTable.fileName),
                  let table = package.table(tableID) else {
                throw STPDResultPackageError.missingTable(manifestTable.fileName)
            }
            guard table.rows.count == manifestTable.rowCount else {
                throw STPDResultPackageError.invalidTable(
                    table: manifestTable.fileName,
                    reason: "live table row count does not match its validated manifest"
                )
            }
            return Table(
                fileName: manifestTable.fileName,
                rowCount: table.rows.count,
                columns: table.columnDefinitions,
                rows: table.rows
            )
        }

        guard let checkTable = package.table(.resultConsistencyCheck) else {
            throw STPDResultPackageError.missingTable(
                STPDResultTable.resultConsistencyCheck.rawValue
            )
        }
        let columnIndex = Dictionary(
            uniqueKeysWithValues: checkTable.headers.enumerated().map {
                ($0.element, $0.offset)
            }
        )
        guard let idIndex = columnIndex["check_id"],
              let statusIndex = columnIndex["status"],
              let severityIndex = columnIndex["severity"],
              let detailsIndex = columnIndex["details"] else {
            throw STPDResultPackageError.invalidTable(
                table: STPDResultTable.resultConsistencyCheck.rawValue,
                reason: "live consistency table is missing required display columns"
            )
        }
        consistencyChecks = try checkTable.rows.map { row in
            guard row.indices.contains(idIndex),
                  row.indices.contains(statusIndex),
                  row.indices.contains(severityIndex),
                  row.indices.contains(detailsIndex) else {
                throw STPDResultPackageError.invalidTable(
                    table: STPDResultTable.resultConsistencyCheck.rawValue,
                    reason: "live consistency row width is invalid"
                )
            }
            return ConsistencyCheck(
                id: row[idIndex],
                status: row[statusIndex],
                severity: row[severityIndex],
                details: row[detailsIndex]
            )
        }
    }
}

private enum CurrentRunTableBuildOutcome: Sendable {
    case success(ResultTableDisplayPackage)
    case failure(String)
}

private enum ResultTableVisibleSource: String, CaseIterable, Hashable {
    case currentRun
    case openedPackage
}

private enum ResultSemanticBuildOutcome: Sendable {
    case success(STPDResultSemanticPresentation)
    case failure(String)
}

private struct ResultSemanticPresentationKey: Hashable, Sendable {
    let origin: ResultTableDisplayPackage.Origin
    let runID: String
    let datasetDigest: String
    let settingsDigest: String
    let sourceMode: String
    let tableShapes: [String]
    let consistencyShapes: [String]
    let revision: Int

    init(package: ResultTableDisplayPackage, revision: Int) {
        origin = package.origin
        runID = package.runID
        datasetDigest = package.datasetDigest
        settingsDigest = package.settingsDigest
        sourceMode = package.sourceMode
        tableShapes = package.tables.map {
            "\($0.fileName)|\($0.rowCount)|\($0.columns.count)"
        }
        consistencyShapes = package.consistencyChecks.map {
            "\($0.id)|\($0.status)|\($0.severity)"
        }
        self.revision = revision
    }
}

private struct ResultSemanticInspectorGroup: Identifiable {
    let id: String
    let title: String
    let systemImage: String
    let fields: [(column: STPDResultSemanticColumn, value: String)]
}

private struct ScientificNoteLabelStyle: LabelStyle {
    func makeBody(configuration: Configuration) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 8) {
            configuration.icon
                .font(.system(size: 5))
                .foregroundStyle(.tertiary)
            configuration.title
        }
    }
}

private struct CurrentRunTableAuthorityKey: Hashable {
    let runID: String
    let approvedManualAnnotationImports:
        [ManualAnnotationCSVApprovedBatch]
    let candidateReviews: [STPDCandidateReviewInput]
    let currentSettingsSnapshot: DetectionRunSettingsSnapshot

    init(seed: ResultPackageExportSeed) {
        runID = seed.run.runIdentity.runID
        approvedManualAnnotationImports =
            seed.approvedManualAnnotationImports
        candidateReviews = seed.candidateReviews
        currentSettingsSnapshot = seed.currentSettingsSnapshot
    }
}

/// Read-only scientific review for both the active detector run and a verified `.stpdresult` package.
///
/// The current-run path uses the same Core package builder and authoritative table contracts as export.
/// The readback path starts from exactly what `STPDResultPackageReader` returned. R-inspired semantic
/// tables are deterministic presentation projections; the raw authoritative tables remain available
/// under Advanced and are never modified.
struct DetectorResultReviewView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n

    @State private var selectedReviewItemID = "overview"
    @State private var semanticSearchText = ""
    @State private var semanticSortColumnID: String?
    @State private var semanticSortAscending = true
    @State private var showAdvancedTables = false
    @State private var visibleSource: ResultTableVisibleSource = .currentRun
    @State private var currentRunPackage: ResultTableDisplayPackage?
    @State private var currentRunPackageAuthorityKey:
        CurrentRunTableAuthorityKey?
    @State private var isPreparingCurrentRunPackage = false
    @State private var currentRunPackageErrorMessage: String?
    @State private var currentRunBuildRequestID = 0
    @State private var currentRunRefreshID = 0
    @State private var selectedSemanticRowID: String?
    @State private var semanticPresentationRevision = 0
    @State private var semanticBuildRequestID = 0
    @State private var cachedSemanticPresentationKey:
        ResultSemanticPresentationKey?
    @State private var cachedSemanticPresentationOutcome:
        ResultSemanticBuildOutcome?
    @State private var isPreparingSemanticPresentation = false

    private var delayedPreparationMessage: String? {
        if isPreparingCurrentRunPackage {
            return l10n.t("正在生成当前检测的权威结果表…")
        }
        if isPreparingSemanticPresentation {
            return l10n.t("正在构建结果表的语义化审阅视图…")
        }
        return nil
    }

    private typealias ReadResult = ResultTableDisplayPackage
    private typealias VerifiedTable = ResultTableDisplayPackage.Table

    private static let rowNumberWidth: CGFloat = 60

    var body: some View {
        VStack(spacing: 0) {
            headerBar
            Divider()
            content
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .delayedBackgroundProgress(delayedPreparationMessage)
        .task(id: currentRunAuthorityKey) {
            await prepareCurrentRunPackage()
        }
        .task(id: currentRunRefreshID) {
            guard currentRunRefreshID > 0 else {
                return
            }
            await prepareCurrentRunPackage(force: true)
        }
        .onAppear {
            normalizeVisibleSource(
                preferOpenedPackage: document.loadedResultPackage != nil
            )
        }
        .onChange(of: document.resultPackageLoadCompletionID) { _, _ in
            resetSemanticReviewState(invalidatePresentation: true)
            visibleSource = .openedPackage
        }
        .onChange(of: visibleSource) { _, _ in
            resetSemanticReviewState(invalidatePresentation: true)
        }
        .onChange(
            of: document.classicAnchorDetectionRun?.runIdentity.runID
        ) { _, _ in
            resetSemanticReviewState(invalidatePresentation: true)
            normalizeVisibleSource()
        }
        .onChange(of: document.loadedResultPackageURL?.path) { _, newPath in
            resetSemanticReviewState(invalidatePresentation: true)
            normalizeVisibleSource(
                preferOpenedPackage:
                    newPath != nil && document.loadedResultPackage != nil
            )
        }
    }

    private var currentRunAuthorityKey: CurrentRunTableAuthorityKey? {
        guard let seed = try? document.currentResultPackageExportSeed() else {
            return nil
        }
        return CurrentRunTableAuthorityKey(seed: seed)
    }

    // MARK: - Header bar

    private var headerBar: some View {
        HStack(spacing: 12) {
            Text("结果表")
                .font(.title3.weight(.semibold))
            if document.isResultPackageReading || isPreparingCurrentRunPackage {
                ProgressView().controlSize(.small)
            }
            if isPreparingSemanticPresentation {
                ProgressView().controlSize(.small)
            }
            Spacer()
            if document.classicAnchorDetectionRun != nil,
               document.loadedResultPackage != nil {
                Picker("Source", selection: $visibleSource) {
                    Text("当前检测")
                        .tag(ResultTableVisibleSource.currentRun)
                    Text("已打开的结果包")
                        .tag(ResultTableVisibleSource.openedPackage)
                }
                .labelsHidden()
                .pickerStyle(.segmented)
                .frame(width: 230)
                .help("Choose between live detector tables and the opened result package")
            }

            if document.classicAnchorDetectionRun != nil,
               visibleSource == .currentRun {
                Button {
                    currentRunRefreshID &+= 1
                } label: {
                    Label("刷新当前检测", systemImage: "arrow.clockwise")
                }
                .disabled(
                    document.isResultPackageReading
                        || isPreparingCurrentRunPackage
                        || document.isDetectorRunning
                )
                .help("Rebuild the tables from the active detector run and current reviews")
            }

            Button {
                document.openResultPackageWithPanel()
            } label: {
                Label("打开结果包…", systemImage: "shippingbox")
            }
            .disabled(document.isResultPackageReading || isPreparingCurrentRunPackage)
            .help("Open a verified .stpdresult package (read-only)")

            if document.loadedResultPackage != nil {
                Button {
                    document.clearLoadedResultPackage()
                    visibleSource = .currentRun
                    resetSemanticReviewState(invalidatePresentation: true)
                } label: {
                    Label("清除", systemImage: "xmark.circle")
                }
                .help("Remove the loaded package from the viewer. Does not affect the active dataset or detection.")
            }
        }
        .padding(.horizontal, 20)
        .padding(.vertical, 12)
    }

    // MARK: - Top-level state routing (loading / loaded / failure / empty)

    @ViewBuilder
    private var content: some View {
        if document.isResultPackageReading {
            centeredMessage(
                systemImage: "hourglass",
                title: "Reading package…",
                subtitle: "Verifying the .stpdresult package. This can take a moment for larger packages."
            )
        } else {
            VStack(spacing: 0) {
                if let message =
                    document.resultPackageReadbackErrorMessage {
                    readErrorBanner(message)
                    Divider()
                }
                if visibleSource == .openedPackage {
                    openedPackageContent
                } else {
                    currentRunContent
                }
            }
        }
    }

    @ViewBuilder
    private var currentRunContent: some View {
        let liveAuthorityKey = currentRunAuthorityKey
        let cachedPackageIsCurrent =
            currentRunPackageAuthorityKey != nil
                && currentRunPackageAuthorityKey == liveAuthorityKey

        if isPreparingCurrentRunPackage {
            centeredMessage(
                systemImage: "tablecells",
                title: "Preparing current run tables…",
                subtitle: "Building the authoritative R-style tables from the active detector run."
            )
        } else if cachedPackageIsCurrent,
                  let package = currentRunPackage {
            loadedState(package)
        } else if currentRunPackage != nil {
            centeredMessage(
                systemImage: "arrow.triangle.2.circlepath",
                title: "Updating current run tables…",
                subtitle: "The run authority changed. The previous tables are hidden until the current dataset, settings, reviews, and approved imports are rebuilt."
            )
        } else if let message = currentRunPackageErrorMessage {
            centeredMessage(
                systemImage: "exclamationmark.triangle",
                title: "Could not prepare current run tables",
                subtitle: message,
                tint: .orange,
                actionTitle: "Try Again",
                action: {
                    currentRunRefreshID &+= 1
                }
            )
        } else {
            centeredMessage(
                systemImage: "tablecells",
                title: "No result tables yet",
                subtitle: "Run detection to view the current R-style output tables here, or open a verified .stpdresult package.",
                actionTitle: "Open Result Package…",
                action: { document.openResultPackageWithPanel() }
            )
        }
    }

    @ViewBuilder
    private var openedPackageContent: some View {
        if let package = document.loadedResultPackage {
            loadedState(ReadResult(readback: package))
        } else {
            centeredMessage(
                systemImage: "shippingbox",
                title: "No result package opened",
                subtitle: "Open a verified .stpdresult package to inspect its authoritative tables.",
                actionTitle: "Open Result Package…",
                action: { document.openResultPackageWithPanel() }
            )
        }
    }

    // MARK: - Loaded state

    @ViewBuilder
    private func loadedState(_ package: ReadResult) -> some View {
        let key = ResultSemanticPresentationKey(
            package: package,
            revision: semanticPresentationRevision
        )
        Group {
            if cachedSemanticPresentationKey == key,
               let outcome = cachedSemanticPresentationOutcome {
                switch outcome {
                case .success(let presentation):
                    VStack(spacing: 0) {
                        summarySection(package, overview: presentation.overview)
                        Divider()
                        HSplitView {
                            scientificSidebar(package, presentation: presentation)
                                .frame(minWidth: 245, idealWidth: 285, maxWidth: 340)
                            reviewContent(package, presentation: presentation)
                                .frame(minWidth: 520, maxWidth: .infinity)
                        }
                    }
                case .failure(let message):
                    semanticFailureState(package, message: message)
                }
            } else {
                centeredMessage(
                    systemImage: "tablecells",
                    title: "Preparing scientific tables…",
                    subtitle: "Joining authoritative event, candidate, and per-ISI records without blocking the interface."
                )
            }
        }
        .task(id: key) {
            await prepareSemanticPresentation(for: package, key: key)
        }
    }

    // MARK: A. Scientific package summary

    private func summarySection(
        _ package: ReadResult,
        overview: STPDResultSemanticOverview
    ) -> some View {
        HStack(spacing: 12) {
            packageOriginLabel(package.origin)
                .font(.callout.weight(.semibold))

            if package.origin == .verifiedReadback,
               let url = document.loadedResultPackageURL {
                Text(url.lastPathComponent)
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .help(url.path)
            } else {
                Text("当前检测运行")
            }

            Divider().frame(height: 16)
            Label(
                overview.datasetName.isEmpty ? "Unnamed dataset" : overview.datasetName,
                systemImage: "cylinder"
            )
            Divider().frame(height: 16)
            Text("\(overview.allEventCount) events")
            Text("·")
                .foregroundStyle(.tertiary)
            Text("\(overview.reviewCandidateCount) review")
                .foregroundStyle(
                    overview.reviewCandidateCount == 0
                        ? Color.secondary : Color.orange
                )
            Spacer(minLength: 12)
            Text("schema \(package.schemaVersion)")
                .foregroundStyle(.secondary)
            Text("run \(shortIdentifier(package.runID))")
                .foregroundStyle(.secondary)
                .help(package.runID)
        }
        .font(.caption)
        .padding(.horizontal, 20)
        .padding(.vertical, 9)
    }

    // MARK: B. Scientific navigation

    private func scientificSidebar(
        _ package: ReadResult,
        presentation: STPDResultSemanticPresentation
    ) -> some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 14) {
                navigationButton(
                    id: "overview",
                    title: "Overview",
                    systemImage: "chart.bar.xaxis",
                    count: presentation.overview.allEventCount,
                    color: .accentColor
                )

                navigationSection(
                    title: "Review",
                    systemImage: "checklist",
                    tables: presentation.reviewTables,
                    color: .orange
                )
                navigationSection(
                    title: "Scientific analysis",
                    systemImage: "waveform.path.ecg",
                    tables: presentation.scientificTables,
                    color: .green
                )
                navigationSection(
                    title: "Per-ISI",
                    systemImage: "timeline.selection",
                    tables: presentation.isiTables,
                    color: .blue
                )
                navigationSection(
                    title: "Diagnostics",
                    systemImage: "stethoscope",
                    tables: presentation.diagnosticTables,
                    color: .purple
                )

                Divider()

                DisclosureGroup(isExpanded: $showAdvancedTables) {
                    VStack(alignment: .leading, spacing: 3) {
                        navigationButton(
                            id: "advanced.consistency",
                            title: "Consistency checks",
                            systemImage: "checkmark.shield",
                            count: package.consistencyChecks.count,
                            color: .secondary
                        )
                        ForEach(package.tables, id: \.fileName) { table in
                            navigationButton(
                                id: rawTableID(table.fileName),
                                title: table.fileName,
                                systemImage: "tablecells",
                                count: table.rowCount,
                                color: .secondary,
                                compact: true
                            )
                        }
                    }
                    .padding(.top, 6)
                } label: {
                    Label("高级原始表", systemImage: "wrench.and.screwdriver")
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                }
            }
            .padding(10)
        }
    }

    private func navigationSection(
        title: String,
        systemImage: String,
        tables: [STPDResultSemanticTable],
        color: Color
    ) -> some View {
        VStack(alignment: .leading, spacing: 3) {
            Label(title, systemImage: systemImage)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.bottom, 2)
            ForEach(tables) { table in
                navigationButton(
                    id: table.id,
                    title: table.title,
                    systemImage: semanticTableIcon(table),
                    count: table.rows.count,
                    color: color
                )
            }
        }
    }

    private func navigationButton(
        id: String,
        title: String,
        systemImage: String,
        count: Int,
        color: Color,
        compact: Bool = false
    ) -> some View {
        let selected = selectedReviewItemID == id
        return Button {
            selectedReviewItemID = id
            semanticSearchText = ""
            semanticSortColumnID = nil
            semanticSortAscending = true
            selectedSemanticRowID = nil
        } label: {
            HStack(spacing: 8) {
                Rectangle()
                    .fill(selected ? color : Color.clear)
                    .frame(width: 3, height: compact ? 18 : 25)
                Image(systemName: systemImage)
                    .frame(width: 16)
                    .foregroundStyle(selected ? color : .secondary)
                Text(title)
                    .font(compact ? .caption2 : .callout)
                    .lineLimit(1)
                    .truncationMode(.middle)
                Spacer(minLength: 6)
                Text("\(count)")
                    .font(.caption2.monospacedDigit())
                    .foregroundStyle(.secondary)
            }
            .padding(.horizontal, 6)
            .padding(.vertical, compact ? 3 : 5)
            .contentShape(Rectangle())
            .background(
                selected ? color.opacity(0.10) : Color.clear,
                in: RoundedRectangle(cornerRadius: 4)
            )
        }
        .buttonStyle(.plain)
        .help(title)
    }

    // MARK: C. Scientific content

    @ViewBuilder
    private func reviewContent(
        _ package: ReadResult,
        presentation: STPDResultSemanticPresentation
    ) -> some View {
        let selection = effectiveSelection(
            package: package,
            presentation: presentation
        )
        if selection == "overview" {
            scientificOverview(package, presentation: presentation)
        } else if let table = presentation.table(id: selection) {
            semanticTableViewer(table)
        } else if selection == "advanced.consistency" {
            consistencyViewer(package)
        } else if let table = rawTable(for: selection, package: package) {
            rawTableViewer(table)
        } else {
            scientificOverview(package, presentation: presentation)
        }
    }

    private func effectiveSelection(
        package: ReadResult,
        presentation: STPDResultSemanticPresentation
    ) -> String {
        if selectedReviewItemID == "overview"
            || selectedReviewItemID == "advanced.consistency"
            || presentation.table(id: selectedReviewItemID) != nil
            || rawTable(for: selectedReviewItemID, package: package) != nil {
            return selectedReviewItemID
        }
        return "overview"
    }

    private func scientificOverview(
        _ package: ReadResult,
        presentation: STPDResultSemanticPresentation
    ) -> some View {
        let overview = presentation.overview
        let metrics: [(String, Int, Color)] = [
            ("All events", overview.allEventCount, .primary),
            ("Accepted / authoritative", overview.authoritativeEventCount, .green),
            ("High confidence", overview.highConfidenceEventCount, .green),
            ("Review candidates", overview.reviewCandidateCount, .orange),
            ("Burst family", overview.burstFamilyEventCount, .orange),
            ("ISI labels", overview.isiLabelCount, .blue),
            ("Structure candidates", overview.structureCandidateCount, .purple),
            ("Diagnostic candidates", overview.diagnosticCandidateCount, .purple),
            ("Consistency attention", overview.consistencyAttentionCount, .orange),
        ]

        return ScrollView {
            VStack(alignment: .leading, spacing: 24) {
                VStack(alignment: .leading, spacing: 5) {
                    Text("科学结果审核")
                        .font(.title2.weight(.semibold))
                    Text(
                        overview.datasetName.isEmpty
                            ? "Authoritative detector output"
                            : overview.datasetName
                    )
                    .font(.callout)
                    .foregroundStyle(.secondary)
                }

                LazyVGrid(
                    columns: [
                        GridItem(.adaptive(minimum: 150, maximum: 220), spacing: 12)
                    ],
                    alignment: .leading,
                    spacing: 12
                ) {
                    ForEach(Array(metrics.enumerated()), id: \.offset) { _, metric in
                        overviewMetric(metric.0, value: metric.1, color: metric.2)
                    }
                }

                overviewSectionTitle(
                    "Pattern distribution",
                    systemImage: "chart.bar"
                )
                Grid(alignment: .leading, horizontalSpacing: 28, verticalSpacing: 7) {
                    GridRow {
                        Text("模式")
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                        Text("事件数")
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                    }
                    Divider().gridCellColumns(2)
                    ForEach(overview.patternCounts) { item in
                        GridRow {
                            Label {
                                Text(displayPattern(item.pattern))
                            } icon: {
                                Circle()
                                    .fill(statusColor(item.pattern))
                                    .frame(width: 8, height: 8)
                            }
                            Text("\(item.count)")
                                .monospacedDigit()
                        }
                    }
                }

                overviewSectionTitle(
                    "Run integrity",
                    systemImage: "checkmark.shield"
                )
                HStack(spacing: 20) {
                    Label(
                        "\(overview.consistencyPassCount) passed",
                        systemImage: "checkmark.circle.fill"
                    )
                    .foregroundStyle(.green)
                    Label(
                        "\(overview.consistencyAttentionCount) need attention",
                        systemImage: "exclamationmark.triangle.fill"
                    )
                    .foregroundStyle(
                        overview.consistencyAttentionCount == 0
                            ? Color.secondary : Color.orange
                    )
                    Text("\(package.tables.count) authoritative tables")
                        .foregroundStyle(.secondary)
                }
                .font(.callout)

                overviewSectionTitle(
                    "Interpretation notes",
                    systemImage: "info.circle"
                )
                VStack(alignment: .leading, spacing: 8) {
                    ForEach(presentation.scientificNotes, id: \.self) { note in
                        Label(note, systemImage: "circle.fill")
                            .labelStyle(ScientificNoteLabelStyle())
                    }
                }
                .font(.callout)
                .foregroundStyle(.secondary)

                Divider()
                HStack(spacing: 18) {
                    summaryFact("Detector", package.detectorVersion)
                    summaryFact("Source mode", package.sourceMode)
                    summaryFact("Owner", package.ownerName)
                }
            }
            .padding(24)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
    }

    private func overviewMetric(
        _ title: String,
        value: Int,
        color: Color
    ) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            Text("\(value)")
                .font(.title2.monospacedDigit().weight(.semibold))
                .foregroundStyle(color)
            Text(title)
                .font(.caption)
                .foregroundStyle(.secondary)
        }
        .frame(maxWidth: .infinity, minHeight: 58, alignment: .leading)
        .padding(10)
        .background(.quaternary.opacity(0.45), in: RoundedRectangle(cornerRadius: 6))
    }

    private func overviewSectionTitle(
        _ title: String,
        systemImage: String
    ) -> some View {
        Label(title, systemImage: systemImage)
            .font(.headline)
    }

    private func summaryFact(_ label: String, _ value: String) -> some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(label)
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(value.isEmpty ? "—" : value)
                .font(.caption)
                .textSelection(.enabled)
        }
    }

    private func semanticTableViewer(
        _ table: STPDResultSemanticTable
    ) -> some View {
        let rows = visibleRows(for: table)
        return VStack(alignment: .leading, spacing: 0) {
            VStack(alignment: .leading, spacing: 6) {
                HStack(alignment: .firstTextBaseline, spacing: 10) {
                    Text(table.title)
                        .font(.headline)
                    Text("\(rows.count) of \(table.rows.count) rows")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                    Spacer()
                    TextField("Filter rows", text: $semanticSearchText)
                        .textFieldStyle(.roundedBorder)
                        .frame(width: 230)
                }
                Text(table.subtitle)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Text("Source: \(table.sourceFileNames.joined(separator: ", "))")
                    .font(.caption2)
                    .foregroundStyle(.tertiary)
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .help(table.sourceFileNames.joined(separator: "\n"))
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 10)

            Divider()

            if table.rows.isEmpty {
                centeredMessage(
                    systemImage: "tray",
                    title: "No records",
                    subtitle: "This scientific table is valid and contains zero rows."
                )
            } else if rows.isEmpty {
                centeredMessage(
                    systemImage: "magnifyingglass",
                    title: "No matching rows",
                    subtitle: "Clear or change the row filter."
                )
            } else {
                semanticTableGrid(table, rows: rows)
            }
        }
        .onChange(of: semanticSearchText) { _, _ in
            normalizeSemanticRowSelection(rows: visibleRows(for: table))
        }
        .onChange(of: semanticSortColumnID) { _, _ in
            normalizeSemanticRowSelection(rows: visibleRows(for: table))
        }
        .onChange(of: semanticSortAscending) { _, _ in
            normalizeSemanticRowSelection(rows: visibleRows(for: table))
        }
    }

    private func semanticTableGrid(
        _ table: STPDResultSemanticTable,
        rows: [STPDResultSemanticRow]
    ) -> some View {
        let columnWidths = semanticColumnWidths(
            columns: table.columns,
            rows: table.rows
        )
        let totalWidth =
            Self.rowNumberWidth
            + columnWidths.reduce(CGFloat.zero, +)
        let selectedRow =
            rows.first { $0.id == selectedSemanticRowID } ?? rows.first
        let effectiveSelectedRowID = selectedRow?.id

        return HSplitView {
            ScrollView([.horizontal, .vertical]) {
                LazyVStack(
                    alignment: .leading,
                    spacing: 0,
                    pinnedViews: [.sectionHeaders]
                ) {
                    Section {
                        ForEach(
                            Array(rows.enumerated()),
                            id: \.element.id
                        ) { index, row in
                            semanticBodyRow(
                                index: index,
                                row: row,
                                columns: table.columns,
                                columnWidths: columnWidths,
                                isSelected: row.id == effectiveSelectedRowID
                            )
                            .contentShape(Rectangle())
                            .onTapGesture {
                                selectedSemanticRowID = row.id
                            }
                            .accessibilityElement(children: .combine)
                            .accessibilityLabel(
                                semanticRowAccessibilityLabel(
                                    row,
                                    columns: table.columns,
                                    rowNumber: index + 1
                                )
                            )
                            .accessibilityValue(
                                row.id == effectiveSelectedRowID
                                    ? "Selected" : "Not selected"
                            )
                        }
                    } header: {
                        semanticHeaderRow(
                            table.columns,
                            columnWidths: columnWidths
                        )
                    }
                }
                .frame(width: totalWidth, alignment: .leading)
            }
            .id(table.id)
            .frame(minWidth: 500, maxWidth: .infinity)

            semanticRowInspector(table: table, row: selectedRow)
                .frame(minWidth: 270, idealWidth: 330, maxWidth: 430)
        }
        .task(id: rows.map(\.id)) {
            normalizeSemanticRowSelection(rows: rows)
        }
    }

    private func semanticHeaderRow(
        _ columns: [STPDResultSemanticColumn],
        columnWidths: [CGFloat]
    ) -> some View {
        HStack(spacing: 0) {
            headerCell("#", width: Self.rowNumberWidth, alignment: .trailing)
            ForEach(Array(columns.enumerated()), id: \.element.id) {
                index, column in
                let width = columnWidths.indices.contains(index)
                    ? columnWidths[index]
                    : semanticMinimumColumnWidth(column)
                Button {
                    if semanticSortColumnID == column.id {
                        semanticSortAscending.toggle()
                    } else {
                        semanticSortColumnID = column.id
                        semanticSortAscending = true
                    }
                } label: {
                    HStack(spacing: 4) {
                        Text(column.displayTitle)
                            .lineLimit(1)
                        if semanticSortColumnID == column.id {
                            Image(
                                systemName:
                                    semanticSortAscending
                                    ? "chevron.up" : "chevron.down"
                            )
                            .font(.caption2)
                        }
                    }
                    .font(.caption.weight(.semibold))
                    .fixedSize(horizontal: true, vertical: false)
                    .padding(.horizontal, 8)
                    .padding(.vertical, 6)
                    .frame(
                        width: width,
                        alignment: semanticColumnAlignment(column)
                    )
                    .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .help(
                    column.help.isEmpty
                        ? "Sort by \(column.displayTitle)"
                        : column.help
                )
                .accessibilityLabel("Sort by \(column.displayTitle)")
                .accessibilityValue(
                    semanticSortColumnID == column.id
                        ? (semanticSortAscending ? "Ascending" : "Descending")
                        : "Not sorted"
                )
            }
        }
        .background(.regularMaterial)
        .overlay(alignment: .bottom) { Divider() }
    }

    private func semanticBodyRow(
        index: Int,
        row: STPDResultSemanticRow,
        columns: [STPDResultSemanticColumn],
        columnWidths: [CGFloat],
        isSelected: Bool
    ) -> some View {
        HStack(spacing: 0) {
            Text("\(index + 1)")
                .font(.caption2.monospacedDigit())
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.vertical, 5)
                .frame(width: Self.rowNumberWidth, alignment: .trailing)
            ForEach(Array(columns.enumerated()), id: \.element.id) { offset, column in
                let value = row.values.indices.contains(offset)
                    ? row.values[offset] : ""
                semanticBodyCell(
                    value,
                    column: column,
                    width: columnWidths.indices.contains(offset)
                        ? columnWidths[offset]
                        : semanticMinimumColumnWidth(column)
                )
            }
        }
        .background(
            isSelected
                ? Color.accentColor.opacity(0.14)
                : (
                    index.isMultiple(of: 2)
                        ? Color.clear : Color.primary.opacity(0.035)
                )
        )
    }

    private func semanticBodyCell(
        _ value: String,
        column: STPDResultSemanticColumn,
        width: CGFloat
    ) -> some View {
        let displayValue = semanticDisplayValue(value, column: column)
        return Text(displayValue)
            .font(semanticColumnFont(column))
            .foregroundStyle(
                column.kind == .status
                    ? statusColor(value) : Color.primary
            )
            .lineLimit(1)
            .fixedSize(horizontal: true, vertical: false)
            .help(value.isEmpty ? "Unavailable" : value)
            .textSelection(.enabled)
            .padding(.horizontal, 8)
            .padding(.vertical, 5)
            .frame(width: width, alignment: semanticColumnAlignment(column))
    }

    private func semanticRowInspector(
        table: STPDResultSemanticTable,
        row: STPDResultSemanticRow?
    ) -> some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack(spacing: 8) {
                Image(systemName: "sidebar.right")
                    .foregroundStyle(.secondary)
                Text("行明细")
                    .font(.headline)
                Spacer()
                if let row {
                    Text(shortIdentifier(row.id))
                        .font(.caption.monospaced())
                        .foregroundStyle(.secondary)
                        .help(row.id)
                }
            }
            .padding(.horizontal, 14)
            .padding(.vertical, 10)
            Divider()

            if let row {
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 18) {
                        ForEach(
                            semanticInspectorGroups(table: table, row: row)
                        ) { group in
                            VStack(alignment: .leading, spacing: 7) {
                                Label(group.title, systemImage: group.systemImage)
                                    .font(.caption.weight(.semibold))
                                    .foregroundStyle(.secondary)

                                ForEach(
                                    Array(group.fields.enumerated()),
                                    id: \.offset
                                ) { _, field in
                                    semanticInspectorField(
                                        column: field.column,
                                        value: field.value
                                    )
                                }
                            }
                        }
                    }
                    .padding(14)
                    .frame(maxWidth: .infinity, alignment: .leading)
                }
            } else {
                centeredMessage(
                    systemImage: "cursorarrow.click",
                    title: "No row selected",
                    subtitle: "Select a table row to inspect its scientific fields."
                )
            }
        }
        .background(Color.primary.opacity(0.018))
    }

    private func semanticInspectorField(
        column: STPDResultSemanticColumn,
        value: String
    ) -> some View {
        let displayValue = semanticDisplayValue(value, column: column)
        return VStack(alignment: .leading, spacing: 2) {
            Text(column.displayTitle)
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(displayValue)
                .font(semanticColumnFont(column))
                .foregroundStyle(
                    column.kind == .status
                        ? statusColor(value) : Color.primary
                )
                .fixedSize(horizontal: false, vertical: true)
                .textSelection(.enabled)
                .help(
                    column.help.isEmpty
                        ? (value.isEmpty ? "Unavailable" : value)
                        : column.help
                )
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .accessibilityElement(children: .combine)
        .accessibilityLabel(column.displayTitle)
        .accessibilityValue(value.isEmpty ? "Unavailable" : value)
    }

    private func semanticInspectorGroups(
        table: STPDResultSemanticTable,
        row: STPDResultSemanticRow
    ) -> [ResultSemanticInspectorGroup] {
        var grouped: [String: [
            (column: STPDResultSemanticColumn, value: String)
        ]] = [:]
        for (index, column) in table.columns.enumerated() {
            let value = row.values.indices.contains(index)
                ? row.values[index] : ""
            grouped[semanticInspectorGroupID(for: column.id), default: []]
                .append((column, value))
        }

        let definitions: [(String, String, String)] = [
            ("identity", "Identity and authority", "checkmark.seal"),
            ("geometry", "Geometry", "ruler"),
            ("metrics", "Structure and regularity", "function"),
            ("boundary", "Boundary evidence", "arrow.left.and.right"),
            ("decision", "Decision and audit", "doc.text.magnifyingglass"),
            ("additional", "Additional fields", "ellipsis.circle"),
        ]
        return definitions.compactMap { id, title, systemImage in
            guard let fields = grouped[id], !fields.isEmpty else {
                return nil
            }
            return ResultSemanticInspectorGroup(
                id: id,
                title: title,
                systemImage: systemImage,
                fields: fields
            )
        }
    }

    private func semanticInspectorGroupID(for columnID: String) -> String {
        let id = columnID.lowercased()
        let identityIDs: Set<String> = [
            "event_id", "candidate_id", "candidate_uid", "isi_id", "train",
            "pattern", "subtype", "class", "layer", "final_label",
            "final_pattern", "final_subtype", "final_source", "review",
            "authority", "authority_origin", "confidence", "selected",
            "selection", "auto_pattern", "auto_subtype",
        ]
        let geometryIDs: Set<String> = [
            "aligned_start", "aligned_end", "raw_start", "raw_end",
            "duration", "n_spikes", "n_isi", "isi_index", "aligned_time",
            "raw_time", "isi", "isi_range", "start_isi_index",
            "end_isi_index",
        ]
        let metricIDs: Set<String> = [
            "mean_isi", "median_isi", "min_isi", "max_isi", "q10_isi",
            "q25_isi", "q50_isi", "q75_isi", "q90_isi", "core_q90_isi",
            "cv", "cv2", "lv", "score", "structure_score",
            "metric_availability",
        ]
        let boundaryIDs: Set<String> = [
            "pre_gap", "post_gap", "pre_contrast", "post_contrast",
            "edge_contrast", "geom_contrast", "required_contrast",
            "boundary_source", "pre_isi", "post_isi", "context_pre_isi",
            "context_post_isi",
        ]

        if identityIDs.contains(id)
            || id.hasSuffix("_id")
            || id.contains("authority") {
            return "identity"
        }
        if geometryIDs.contains(id)
            || id.contains("start")
            || id.contains("end")
            || id.contains("duration")
            || id.contains("time") {
            return "geometry"
        }
        if boundaryIDs.contains(id)
            || id.contains("contrast")
            || id.contains("boundary")
            || id.hasPrefix("pre_")
            || id.hasPrefix("post_") {
            return "boundary"
        }
        if metricIDs.contains(id)
            || id.contains("median")
            || id.contains("mean")
            || id.contains("quantile")
            || id.hasPrefix("q") {
            return "metrics"
        }
        if id.contains("reason")
            || id.contains("decision")
            || id.contains("action")
            || id.contains("gate")
            || id.contains("source")
            || id.contains("uncertainty")
            || id.contains("failure")
            || id.contains("audit")
            || id.contains("note")
            || id.contains("changed")
            || id.contains("artifact")
            || id.contains("refractory") {
            return "decision"
        }
        return "additional"
    }

    private func semanticRowAccessibilityLabel(
        _ row: STPDResultSemanticRow,
        columns: [STPDResultSemanticColumn],
        rowNumber: Int
    ) -> String {
        let summary = columns.enumerated().compactMap { index, column -> String? in
            guard row.values.indices.contains(index),
                  !row.values[index].isEmpty else {
                return nil
            }
            return "\(column.title): \(row.values[index])"
        }
        .prefix(5)
        .joined(separator: ", ")
        return summary.isEmpty
            ? "Row \(rowNumber)"
            : "Row \(rowNumber), \(summary)"
    }

    private func normalizeSemanticRowSelection(
        rows: [STPDResultSemanticRow]
    ) {
        guard let selectedSemanticRowID,
              rows.contains(where: { $0.id == selectedSemanticRowID }) else {
            self.selectedSemanticRowID = rows.first?.id
            return
        }
    }

    private func visibleRows(
        for table: STPDResultSemanticTable
    ) -> [STPDResultSemanticRow] {
        let query = semanticSearchText
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .lowercased()
        var rows = query.isEmpty
            ? table.rows
            : table.rows.filter { row in
                row.values.contains { $0.lowercased().contains(query) }
            }

        guard let sortID = semanticSortColumnID,
              let index = table.columns.firstIndex(where: { $0.id == sortID }) else {
            return rows
        }
        let kind = table.columns[index].kind
        rows.sort { lhs, rhs in
            let left = lhs.values.indices.contains(index) ? lhs.values[index] : ""
            let right = rhs.values.indices.contains(index) ? rhs.values[index] : ""
            if left.isEmpty != right.isEmpty {
                return !left.isEmpty
            }
            if left == right {
                return lhs.id < rhs.id
            }
            if kind == .real || kind == .integer,
               let leftNumber = Double(left),
               let rightNumber = Double(right),
               leftNumber != rightNumber {
                return semanticSortAscending
                    ? leftNumber < rightNumber
                    : leftNumber > rightNumber
            }
            let comparison = left.localizedStandardCompare(right)
            if comparison == .orderedSame {
                return lhs.id < rhs.id
            }
            return semanticSortAscending
                ? comparison == .orderedAscending
                : comparison == .orderedDescending
        }
        return rows
    }

    private func semanticColumnWidths(
        columns: [STPDResultSemanticColumn],
        rows: [STPDResultSemanticRow]
    ) -> [CGFloat] {
        columns.enumerated().map { index, column in
            let headerWidth = semanticMeasuredWidth(
                column.displayTitle,
                font: .systemFont(
                    ofSize: NSFont.smallSystemFontSize,
                    weight: .semibold
                )
            ) + 38
            let valueFont: NSFont
            switch column.kind {
            case .identifier:
                valueFont = NSFont.monospacedSystemFont(
                    ofSize: NSFont.smallSystemFontSize,
                    weight: .regular
                )
            case .integer, .real:
                valueFont = NSFont.monospacedDigitSystemFont(
                    ofSize: NSFont.smallSystemFontSize,
                    weight: .regular
                )
            case .text, .status, .longText:
                valueFont = NSFont.systemFont(
                    ofSize: NSFont.smallSystemFontSize
                )
            }
            let widestValue = rows.reduce(CGFloat.zero) { widest, row in
                let rawValue = row.values.indices.contains(index)
                    ? row.values[index] : ""
                let displayValue = semanticDisplayValue(
                    rawValue,
                    column: column
                )
                return max(
                    widest,
                    semanticMeasuredWidth(displayValue, font: valueFont) + 20
                )
            }
            return max(
                semanticMinimumColumnWidth(column),
                ceil(max(headerWidth, widestValue))
            )
        }
    }

    private func semanticMeasuredWidth(
        _ value: String,
        font: NSFont
    ) -> CGFloat {
        (value as NSString).size(withAttributes: [.font: font]).width
    }

    private func semanticMinimumColumnWidth(
        _ column: STPDResultSemanticColumn
    ) -> CGFloat {
        switch column.kind {
        case .identifier:
            return 110
        case .longText:
            return 170
        case .integer:
            return 64
        case .real:
            return 88
        case .status:
            return 92
        case .text:
            return 105
        }
    }

    private func semanticColumnAlignment(
        _ column: STPDResultSemanticColumn
    ) -> Alignment {
        switch column.kind {
        case .integer, .real:
            return .trailing
        case .identifier, .text, .status, .longText:
            return .leading
        }
    }

    private func semanticColumnFont(
        _ column: STPDResultSemanticColumn
    ) -> Font {
        switch column.kind {
        case .identifier:
            return .caption.monospaced()
        case .integer, .real:
            return .caption.monospacedDigit()
        case .text, .status, .longText:
            return .caption
        }
    }

    private func semanticDisplayValue(
        _ rawValue: String,
        column: STPDResultSemanticColumn
    ) -> String {
        guard !rawValue.isEmpty else {
            return "—"
        }
        guard column.kind == .real,
              let number = Double(rawValue),
              number.isFinite else {
            return rawValue
        }
        guard number != 0 else {
            return "0"
        }
        let magnitude = abs(number)
        if magnitude < 0.000001 || magnitude >= 1_000_000 {
            return String(format: "%.4e", number)
        }
        var fixed = String(format: "%.6f", number)
        while fixed.last == "0" {
            fixed.removeLast()
        }
        if fixed.last == "." {
            fixed.removeLast()
        }
        return fixed
    }

    // MARK: D. Advanced authoritative tables

    private func rawTableViewer(_ table: VerifiedTable) -> some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack(spacing: 10) {
                Text(table.fileName)
                    .font(.headline)
                    .textSelection(.enabled)
                badge("raw authoritative", color: .secondary)
                Text("\(table.columns.count) columns · \(table.rowCount) rows")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 8)
            Divider()
            if table.rowCount == 0 {
                emptyTableState(table)
            } else {
                tableGrid(table)
            }
        }
    }

    private func consistencyViewer(_ package: ReadResult) -> some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack {
                Text("一致性检查")
                    .font(.headline)
                Text("\(package.consistencyChecks.count)")
                    .font(.caption.monospacedDigit())
                    .foregroundStyle(.secondary)
                Spacer()
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 10)
            Divider()

            if package.consistencyChecks.isEmpty {
                centeredMessage(
                    systemImage: "checkmark.shield",
                    title: "No consistency checks recorded",
                    subtitle: "The package contains no consistency-check rows."
                )
            } else {
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 0) {
                        ForEach(
                            Array(package.consistencyChecks.enumerated()),
                            id: \.offset
                        ) { index, check in
                            VStack(alignment: .leading, spacing: 5) {
                                HStack(spacing: 8) {
                                    Text(check.id)
                                        .font(.callout.weight(.semibold))
                                        .textSelection(.enabled)
                                    Spacer()
                                    badge(
                                        check.status,
                                        color: check.status == "pass" ? .green : .orange
                                    )
                                    badge(
                                        check.severity,
                                        color: check.severity == "info" ? .blue : .orange
                                    )
                                }
                                Text(check.details)
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                                    .textSelection(.enabled)
                            }
                            .padding(12)
                            .background(
                                index.isMultiple(of: 2)
                                    ? Color.clear : Color.primary.opacity(0.035)
                            )
                            Divider()
                        }
                    }
                }
            }
        }
    }

    private func semanticFailureState(
        _ package: ReadResult,
        message: String
    ) -> some View {
        let table = rawTable(for: selectedReviewItemID, package: package)
            ?? package.tables.first
        return VStack(spacing: 0) {
            HStack(alignment: .firstTextBaseline, spacing: 8) {
                Image(systemName: "exclamationmark.triangle.fill")
                    .foregroundStyle(.orange)
                Text("科学投影不可用")
                    .font(.caption.weight(.semibold))
                Text(message)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .textSelection(.enabled)
                Spacer()
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 9)
            .background(Color.orange.opacity(0.12))
            HSplitView {
                rawOnlySidebar(package)
                    .frame(minWidth: 240, idealWidth: 285, maxWidth: 340)
                if let table {
                    rawTableViewer(table)
                } else {
                    centeredMessage(
                        systemImage: "tablecells",
                        title: "No raw table available",
                        subtitle: message,
                        tint: .orange
                    )
                }
            }
        }
    }

    private func rawOnlySidebar(_ package: ReadResult) -> some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 3) {
                Text("权威原始表")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                    .padding(.horizontal, 8)
                    .padding(.bottom, 4)
                ForEach(package.tables, id: \.fileName) { table in
                    navigationButton(
                        id: rawTableID(table.fileName),
                        title: table.fileName,
                        systemImage: "tablecells",
                        count: table.rowCount,
                        color: .secondary,
                        compact: true
                    )
                }
            }
            .padding(10)
        }
    }

    private func rawTableID(_ fileName: String) -> String {
        "raw:\(fileName)"
    }

    private func rawTable(
        for selection: String,
        package: ReadResult
    ) -> VerifiedTable? {
        guard selection.hasPrefix("raw:") else {
            return nil
        }
        let fileName = String(selection.dropFirst(4))
        return package.tables.first { $0.fileName == fileName }
    }

    private func semanticTableIcon(
        _ table: STPDResultSemanticTable
    ) -> String {
        switch table.id {
        case STPDResultSemanticPresentation.highConfidenceTableID:
            return "checkmark.seal"
        case STPDResultSemanticPresentation.reviewCandidatesTableID:
            return "exclamationmark.bubble"
        case STPDResultSemanticPresentation.allEventsTableID:
            return "list.bullet.rectangle"
        case STPDResultSemanticPresentation.burstFamilyTableID:
            return "bolt.horizontal"
        case STPDResultSemanticPresentation.isiLabelsTableID:
            return "timeline.selection"
        case STPDResultSemanticPresentation.structureCandidatesTableID:
            return "point.3.connected.trianglepath.dotted"
        case STPDResultSemanticPresentation.diagnosticCandidatesTableID:
            return "scope"
        default:
            return "tablecells"
        }
    }

    private func statusColor(_ value: String) -> Color {
        let normalized = value
            .lowercased()
            .replacingOccurrences(of: "_", with: " ")
        if normalized.contains("possible") || normalized.contains("review") {
            return .orange
        }
        if normalized.contains("burst") {
            return .orange
        }
        if normalized.contains("tonic") {
            return .green
        }
        if normalized.contains("pause") {
            return .blue
        }
        if normalized == "false" || normalized == "no"
            || normalized.contains("not selected")
            || normalized.contains("unselected")
            || normalized.contains("not accepted") {
            return .secondary
        }
        if normalized.contains("reject") || normalized == "fail"
            || normalized.contains("invalid") {
            return .red
        }
        if normalized.contains("accept") || normalized == "pass"
            || normalized == "true" || normalized.contains("selected") {
            return .green
        }
        if normalized.contains("manual") {
            return .purple
        }
        return .primary
    }

    private func displayPattern(_ value: String) -> String {
        value
            .replacingOccurrences(of: "_", with: " ")
            .split(separator: " ")
            .map { $0.capitalized }
            .joined(separator: " ")
    }

    private func shortIdentifier(_ value: String) -> String {
        guard value.count > 12 else {
            return value
        }
        return "\(value.prefix(6))…\(value.suffix(4))"
    }

    private func emptyTableState(_ table: VerifiedTable) -> some View {
        let columnWidths = rawColumnWidths(table)
        return VStack(spacing: 0) {
            // Keep the column header visible so the table's shape is clear even with no rows.
            ScrollView(.horizontal, showsIndicators: true) {
                tableHeaderRow(
                    columns: table.columns,
                    columnWidths: columnWidths
                )
            }
            Spacer()
            Label("此表没有数据行（行数为 0）。", systemImage: "tray")
                .font(.callout)
                .foregroundStyle(.secondary)
            Spacer()
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func tableGrid(_ table: VerifiedTable) -> some View {
        let columnWidths = rawColumnWidths(table)
        let totalWidth =
            Self.rowNumberWidth
            + columnWidths.reduce(CGFloat.zero, +)
        return ScrollView([.horizontal, .vertical]) {
            // Lazy rows keep ordinary larger tables usable; the pinned section header keeps the column
            // names visible during vertical scroll and scrolls with the body horizontally.
            LazyVStack(alignment: .leading, spacing: 0, pinnedViews: [.sectionHeaders]) {
                Section {
                    ForEach(Array(table.rows.enumerated()), id: \.offset) { index, row in
                        tableBodyRow(
                            index: index,
                            row: row,
                            columns: table.columns,
                            columnWidths: columnWidths
                        )
                    }
                } header: {
                    tableHeaderRow(
                        columns: table.columns,
                        columnWidths: columnWidths
                    )
                }
            }
            .frame(width: totalWidth, alignment: .leading)
        }
        .id(table.fileName)
    }

    private func tableHeaderRow(
        columns: [STPDResultColumnDefinition],
        columnWidths: [CGFloat]
    ) -> some View {
        HStack(spacing: 0) {
            headerCell("#", width: Self.rowNumberWidth, alignment: .trailing)
            ForEach(Array(columns.enumerated()), id: \.offset) { index, column in
                headerCell(
                    column.name,
                    width: columnWidths.indices.contains(index)
                        ? columnWidths[index] : rawMinimumColumnWidth(column),
                    alignment: rawColumnAlignment(column)
                )
            }
        }
        .background(.regularMaterial)
        .overlay(alignment: .bottom) { Divider() }
    }

    private func headerCell(_ text: String, width: CGFloat, alignment: Alignment) -> some View {
        Text(text)
            .font(.caption.weight(.semibold))
            .lineLimit(1)
            .fixedSize(horizontal: true, vertical: false)
            .help(text)
            .padding(.horizontal, 8)
            .padding(.vertical, 6)
            .frame(width: width, alignment: alignment)
    }

    private func tableBodyRow(
        index: Int,
        row: [String],
        columns: [STPDResultColumnDefinition],
        columnWidths: [CGFloat]
    ) -> some View {
        HStack(spacing: 0) {
            Text("\(index + 1)")
                .font(.caption2.monospacedDigit())
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.vertical, 5)
                .frame(width: Self.rowNumberWidth, alignment: .trailing)
            ForEach(Array(columns.enumerated()), id: \.offset) { index, column in
                let value = row.indices.contains(index) ? row[index] : ""
                bodyCell(
                    value,
                    column: column,
                    width: columnWidths.indices.contains(index)
                        ? columnWidths[index] : rawMinimumColumnWidth(column)
                )
            }
        }
        .background(index.isMultiple(of: 2) ? Color.clear : Color.primary.opacity(0.035))
    }

    private func bodyCell(
        _ value: String,
        column: STPDResultColumnDefinition,
        width: CGFloat
    ) -> some View {
        Text(value)
            .font(rawColumnFont(column))
            .lineLimit(1)
            .fixedSize(horizontal: true, vertical: false)
            .help(value)
            .textSelection(.enabled)
            .padding(.horizontal, 8)
            .padding(.vertical, 5)
            .frame(width: width, alignment: rawColumnAlignment(column))
    }

    private func rawColumnWidths(_ table: VerifiedTable) -> [CGFloat] {
        table.columns.enumerated().map { index, column in
            let headerWidth = semanticMeasuredWidth(
                column.name,
                font: .systemFont(
                    ofSize: NSFont.smallSystemFontSize,
                    weight: .semibold
                )
            ) + 20
            let valueFont = rawNSFont(column)
            let widestValue = table.rows.reduce(CGFloat.zero) { widest, row in
                let value = row.indices.contains(index) ? row[index] : ""
                return max(
                    widest,
                    semanticMeasuredWidth(value, font: valueFont) + 20
                )
            }
            return max(
                rawMinimumColumnWidth(column),
                ceil(max(headerWidth, widestValue))
            )
        }
    }

    private func rawMinimumColumnWidth(
        _ column: STPDResultColumnDefinition
    ) -> CGFloat {
        switch column.type {
        case .integer, .real:
            return 72
        case .boolean:
            return 78
        case .timestamp:
            return 150
        case .string, .stringList:
            return 105
        }
    }

    private func rawColumnAlignment(
        _ column: STPDResultColumnDefinition
    ) -> Alignment {
        switch column.type {
        case .integer, .real:
            return .trailing
        case .string, .boolean, .timestamp, .stringList:
            return .leading
        }
    }

    private func rawColumnFont(
        _ column: STPDResultColumnDefinition
    ) -> Font {
        switch column.type {
        case .integer, .real:
            return .caption.monospacedDigit()
        case .string, .boolean, .timestamp, .stringList:
            return .caption.monospaced()
        }
    }

    private func rawNSFont(
        _ column: STPDResultColumnDefinition
    ) -> NSFont {
        switch column.type {
        case .integer, .real:
            return .monospacedDigitSystemFont(
                ofSize: NSFont.smallSystemFontSize,
                weight: .regular
            )
        case .string, .boolean, .timestamp, .stringList:
            return .monospacedSystemFont(
                ofSize: NSFont.smallSystemFontSize,
                weight: .regular
            )
        }
    }

    // MARK: - Semantic table preparation

    @MainActor
    private func prepareSemanticPresentation(
        for package: ReadResult,
        key: ResultSemanticPresentationKey
    ) async {
        if cachedSemanticPresentationKey == key,
           cachedSemanticPresentationOutcome != nil {
            return
        }

        semanticBuildRequestID &+= 1
        let requestID = semanticBuildRequestID
        isPreparingSemanticPresentation = true

        let buildTask = Task.detached(priority: .userInitiated) {
            do {
                try Task.checkCancellation()
                let sourceTables = package.tables.map {
                    STPDResultSemanticSourceTable(
                        fileName: $0.fileName,
                        columns: $0.columns,
                        rows: $0.rows
                    )
                }
                let consistencyChecks = package.consistencyChecks.map {
                    STPDResultSemanticConsistencyCheck(
                        id: $0.id,
                        status: $0.status,
                        severity: $0.severity,
                        details: $0.details
                    )
                }
                try Task.checkCancellation()
                return ResultSemanticBuildOutcome.success(
                    try STPDResultSemanticPresentation.make(
                        sourceTables: sourceTables,
                        consistencyChecks: consistencyChecks
                    )
                )
            } catch {
                return ResultSemanticBuildOutcome.failure(
                    error.localizedDescription
                )
            }
        }
        let outcome = await withTaskCancellationHandler {
            await buildTask.value
        } onCancel: {
            buildTask.cancel()
        }

        guard !Task.isCancelled,
              requestID == semanticBuildRequestID,
              key.revision == semanticPresentationRevision else {
            return
        }
        cachedSemanticPresentationKey = key
        cachedSemanticPresentationOutcome = outcome
        isPreparingSemanticPresentation = false
    }

    // MARK: - Current run table preparation

    @MainActor
    private func prepareCurrentRunPackage(force: Bool = false) async {
        guard let runID = document.classicAnchorDetectionRun?.runIdentity.runID else {
            currentRunBuildRequestID += 1
            currentRunPackage = nil
            currentRunPackageAuthorityKey = nil
            currentRunPackageErrorMessage = nil
            isPreparingCurrentRunPackage = false
            return
        }
        let requestedAuthorityKey = currentRunAuthorityKey
        if !force,
           currentRunPackageAuthorityKey == requestedAuthorityKey,
           currentRunPackage != nil {
            return
        }

        currentRunBuildRequestID += 1
        let requestID = currentRunBuildRequestID
        currentRunPackage = nil
        currentRunPackageAuthorityKey = nil
        currentRunPackageErrorMessage = nil
        isPreparingCurrentRunPackage = true

        let seed: ResultPackageExportSeed
        do {
            seed = try document.currentResultPackageExportSeed()
        } catch {
            guard requestID == currentRunBuildRequestID else {
                return
            }
            currentRunPackageErrorMessage = error.localizedDescription
            isPreparingCurrentRunPackage = false
            return
        }

        let buildTask = Task.detached(priority: .userInitiated) {
            do {
                try Task.checkCancellation()
                let input = try seed.snapshot()
                try Task.checkCancellation()
                let package = try STPDResultPackageBuilder.build(input)
                try Task.checkCancellation()
                return CurrentRunTableBuildOutcome.success(
                    try ResultTableDisplayPackage(currentRun: package)
                )
            } catch {
                return CurrentRunTableBuildOutcome.failure(
                    error.localizedDescription
                )
            }
        }
        let outcome = await withTaskCancellationHandler {
            await buildTask.value
        } onCancel: {
            buildTask.cancel()
        }

        guard !Task.isCancelled,
              requestID == currentRunBuildRequestID,
              document.classicAnchorDetectionRun?.runIdentity.runID == runID else {
            return
        }

        let liveSeed: ResultPackageExportSeed
        do {
            liveSeed = try document.currentResultPackageExportSeed()
        } catch {
            currentRunPackageErrorMessage = error.localizedDescription
            isPreparingCurrentRunPackage = false
            return
        }
        guard seed.hasSameAuthorityContext(as: liveSeed) else {
            currentRunPackageErrorMessage =
                "The active dataset, settings, reviews, or approved manual imports changed while the tables were being built. Refresh to use the current state."
            isPreparingCurrentRunPackage = false
            return
        }
        let completedAuthorityKey =
            CurrentRunTableAuthorityKey(seed: liveSeed)

        switch outcome {
        case let .success(package):
            resetSemanticReviewState(invalidatePresentation: true)
            currentRunPackage = package
            currentRunPackageAuthorityKey = completedAuthorityKey
            currentRunPackageErrorMessage = nil
        case let .failure(message):
            currentRunPackage = nil
            currentRunPackageAuthorityKey = nil
            currentRunPackageErrorMessage = message
        }
        isPreparingCurrentRunPackage = false
    }

    // MARK: - Shared helpers

    private func resetSemanticReviewState(
        invalidatePresentation: Bool
    ) {
        selectedReviewItemID = "overview"
        selectedSemanticRowID = nil
        semanticSearchText = ""
        semanticSortColumnID = nil
        semanticSortAscending = true

        guard invalidatePresentation else {
            return
        }
        semanticPresentationRevision &+= 1
        semanticBuildRequestID &+= 1
        cachedSemanticPresentationKey = nil
        cachedSemanticPresentationOutcome = nil
        isPreparingSemanticPresentation = false
    }

    private func normalizeVisibleSource(
        preferOpenedPackage: Bool = false
    ) {
        let hasCurrentRun = document.classicAnchorDetectionRun != nil
        let hasOpenedPackage = document.loadedResultPackage != nil

        if preferOpenedPackage, hasOpenedPackage {
            visibleSource = .openedPackage
            return
        }
        if visibleSource == .currentRun,
           !hasCurrentRun,
           hasOpenedPackage {
            visibleSource = .openedPackage
        } else if visibleSource == .openedPackage,
                  !hasOpenedPackage,
                  hasCurrentRun {
            visibleSource = .currentRun
        }
    }

    @ViewBuilder
    private func packageOriginLabel(
        _ origin: ResultTableDisplayPackage.Origin
    ) -> some View {
        switch origin {
        case .currentRun:
            Label("当前检测", systemImage: "play.circle.fill")
                .foregroundStyle(.blue)
        case .verifiedReadback:
            Label("已验证结果包", systemImage: "checkmark.seal.fill")
                .foregroundStyle(.green)
        }
    }

    private func sectionLabel(_ text: String) -> some View {
        Text(text)
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.horizontal, 12)
            .padding(.vertical, 6)
            .background(.quaternary.opacity(0.5))
    }

    private func readErrorBanner(_ message: String) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 8) {
            Image(systemName: "exclamationmark.triangle.fill")
                .foregroundStyle(.orange)
            VStack(alignment: .leading, spacing: 2) {
                Text("无法读取结果包")
                    .font(.caption.weight(.semibold))
                Text(message)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .textSelection(.enabled)
                    .help(message)
            }
            Spacer(minLength: 8)
            Button {
                document.dismissResultPackageReadbackError()
            } label: {
                Image(systemName: "xmark")
            }
            .buttonStyle(.borderless)
            .help("Dismiss this message. The loaded package is unchanged.")
        }
        .padding(.horizontal, 20)
        .padding(.vertical, 10)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(Color.orange.opacity(0.12))
    }

    private func badge(_ text: String, color: Color) -> some View {
        Text(text)
            .font(.caption2.weight(.semibold))
            .padding(.horizontal, 6)
            .padding(.vertical, 2)
            .background(color.opacity(0.18), in: Capsule())
            .foregroundStyle(color)
    }

    private func centeredMessage(
        systemImage: String,
        title: String,
        subtitle: String,
        tint: Color = .secondary,
        actionTitle: String? = nil,
        action: (() -> Void)? = nil
    ) -> some View {
        VStack(spacing: 12) {
            Image(systemName: systemImage)
                .font(.system(size: 40))
                .foregroundStyle(tint)
            Text(title)
                .font(.headline)
            Text(subtitle)
                .font(.callout)
                .foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
                .textSelection(.enabled)
                .frame(maxWidth: 480)
            if let actionTitle, let action {
                Button(actionTitle, action: action)
                    .disabled(
                        document.isResultPackageReading
                            || isPreparingCurrentRunPackage
                    )
                    .padding(.top, 4)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .padding(40)
    }
}
