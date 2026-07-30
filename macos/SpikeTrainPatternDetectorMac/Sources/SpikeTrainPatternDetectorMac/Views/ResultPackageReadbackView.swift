import STPDCore
import SwiftUI

/// Phase 2.2C-B4: a production, read-only viewer for a verified `.stpdresult` package. It displays exactly
/// what `STPDResultPackageReader` returned — every `VerifiedTable.columns` name and `VerifiedTable.rows`
/// cell string verbatim — and never reparses CSV, reformats, sorts, merges, translates, or reinterprets
/// any value. It reads from the document's isolated readback state only; it never touches the active
/// detector document.
struct ResultPackageReadbackView: View {
    @Bindable var document: RasterDocument

    /// The user's current table pick by file name. `nil` (or a stale name after a reload) resolves to the
    /// first table, so the default is the first table and prior selections stay stable when still present.
    @State private var selectedTableFileName: String?

    private typealias ReadResult = STPDResultPackageReadResult
    private typealias VerifiedTable = STPDResultPackageReadResult.VerifiedTable

    private static let cellWidth: CGFloat = 168
    private static let rowNumberWidth: CGFloat = 60

    var body: some View {
        VStack(spacing: 0) {
            headerBar
            Divider()
            content
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }

    // MARK: - Header bar

    private var headerBar: some View {
        HStack(spacing: 12) {
            Image(systemName: "tablecells")
                .foregroundStyle(.secondary)
            Text("Result package readback")
                .font(.title3.weight(.semibold))
            if document.isResultPackageReading {
                ProgressView().controlSize(.small)
            }
            Spacer()
            Button {
                document.openResultPackageWithPanel()
            } label: {
                Label("Open Result Package…", systemImage: "shippingbox")
            }
            .disabled(document.isResultPackageReading)
            .help("Open a verified .stpdresult package (read-only)")

            if document.loadedResultPackage != nil {
                Button {
                    document.clearLoadedResultPackage()
                } label: {
                    Label("Clear", systemImage: "xmark.circle")
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
        } else if let package = document.loadedResultPackage {
            loadedState(package)
        } else if let message = document.resultPackageReadbackErrorMessage {
            centeredMessage(
                systemImage: "exclamationmark.triangle",
                title: "Could not read the package",
                subtitle: message,
                tint: .orange,
                actionTitle: "Try Another Package…",
                action: { document.openResultPackageWithPanel() }
            )
        } else {
            centeredMessage(
                systemImage: "shippingbox",
                title: "No result package loaded",
                subtitle: "Open a verified .stpdresult package to view its R-style output tables, read-only.",
                actionTitle: "Open Result Package…",
                action: { document.openResultPackageWithPanel() }
            )
        }
    }

    // MARK: - Loaded state

    private func loadedState(_ package: ReadResult) -> some View {
        VStack(spacing: 0) {
            // A read that fails while a package is already loaded keeps the good package on screen, but the
            // failure must still be presented (App workflow requirement 7) rather than silently swallowed.
            if let message = document.resultPackageReadbackErrorMessage {
                readErrorBanner(message)
                Divider()
            }
            summarySection(package)
            Divider()
            HSplitView {
                sidebar(package)
                    .frame(minWidth: 240, idealWidth: 300, maxWidth: 380)
                tableViewer(package)
                    .frame(minWidth: 380, maxWidth: .infinity)
            }
        }
    }

    // MARK: A. Package summary

    private func summarySection(_ package: ReadResult) -> some View {
        // The verified state and package filename are shown on the badge line; the grid carries the rest of
        // the required summary fields.
        let fields: [(String, String)] = [
            ("Schema version", package.schemaVersion),
            ("Detector version", package.detectorVersion),
            ("Run ID", package.runID),
            ("Dataset digest", package.datasetDigest),
            ("Settings digest", package.settingsDigest),
            ("Source mode", package.sourceMode),
            ("Owner name", package.ownerName),
            ("Owner email", package.ownerEmail),
            ("Tables", "\(package.tables.count)"),
            ("Consistency checks", "\(package.consistencyChecks.count)"),
        ]
        return VStack(alignment: .leading, spacing: 10) {
            HStack(spacing: 10) {
                Label(
                    package.verified ? "Verified" : "Not verified",
                    systemImage: package.verified ? "checkmark.seal.fill" : "xmark.seal.fill"
                )
                .font(.callout.weight(.semibold))
                .foregroundStyle(package.verified ? Color.green : Color.red)

                if let url = document.loadedResultPackageURL {
                    Text(url.lastPathComponent)
                        .font(.callout)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                        .truncationMode(.middle)
                        .textSelection(.enabled)
                        .help(url.path)
                }
                Spacer()
            }

            LazyVGrid(
                columns: [GridItem(.adaptive(minimum: 260, maximum: 460), alignment: .leading)],
                alignment: .leading,
                spacing: 6
            ) {
                ForEach(fields, id: \.0) { field in
                    summaryField(label: field.0, value: field.1)
                }
            }
        }
        .padding(.horizontal, 20)
        .padding(.vertical, 12)
    }

    private func summaryField(label: String, value: String) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 8) {
            Text(label)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
                .frame(width: 130, alignment: .leading)
            Text(value.isEmpty ? "—" : value)
                .font(.caption.monospaced())
                .lineLimit(1)
                .truncationMode(.middle)
                .textSelection(.enabled)
                .help(value)
            Spacer(minLength: 0)
        }
    }

    // MARK: - Sidebar (table selector + consistency checks)

    private func sidebar(_ package: ReadResult) -> some View {
        VSplitView {
            tableSelector(package)
                .frame(minHeight: 160)
            consistencyChecks(package)
                .frame(minHeight: 120)
        }
    }

    // MARK: B. Table selector

    private func tableSelector(_ package: ReadResult) -> some View {
        VStack(alignment: .leading, spacing: 0) {
            sectionLabel("Tables (\(package.tables.count))")
            List(selection: tableSelectionBinding(package)) {
                // `package.tables` is already in deterministic file-name order (the reader sorts it); it is
                // displayed as-is without any re-sorting.
                ForEach(package.tables, id: \.fileName) { table in
                    VStack(alignment: .leading, spacing: 2) {
                        Text(table.fileName)
                            .font(.callout)
                            .lineLimit(1)
                            .truncationMode(.middle)
                            .help(table.fileName)
                        Text("\(table.rowCount) row\(table.rowCount == 1 ? "" : "s")")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                    .tag(table.fileName)
                }
            }
            .listStyle(.sidebar)
        }
    }

    /// Selection binding whose getter resolves to the effective selected table (first table by default, or
    /// a still-present prior pick), keeping selection stable across reloads without extra state syncing.
    private func tableSelectionBinding(_ package: ReadResult) -> Binding<String?> {
        Binding(
            get: { selectedTable(in: package)?.fileName },
            set: { selectedTableFileName = $0 }
        )
    }

    private func selectedTable(in package: ReadResult) -> VerifiedTable? {
        if let name = selectedTableFileName,
           let match = package.tables.first(where: { $0.fileName == name }) {
            return match
        }
        return package.tables.first
    }

    // MARK: D. Consistency checks

    private func consistencyChecks(_ package: ReadResult) -> some View {
        VStack(alignment: .leading, spacing: 0) {
            sectionLabel("Consistency checks (\(package.consistencyChecks.count))")
            if package.consistencyChecks.isEmpty {
                Text("No consistency checks recorded.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .padding(.horizontal, 12)
                    .padding(.vertical, 8)
                    .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
            } else {
                List {
                    ForEach(Array(package.consistencyChecks.enumerated()), id: \.offset) { _, check in
                        VStack(alignment: .leading, spacing: 3) {
                            HStack(spacing: 6) {
                                Text(check.id)
                                    .font(.caption.weight(.semibold))
                                    .textSelection(.enabled)
                                Spacer(minLength: 4)
                                badge(check.status, color: check.status == "pass" ? .green : .orange)
                                badge(check.severity, color: check.severity == "info" ? .blue : .orange)
                            }
                            Text(check.details)
                                .font(.caption2)
                                .foregroundStyle(.secondary)
                                .textSelection(.enabled)
                                .help(check.details)
                        }
                        .padding(.vertical, 2)
                    }
                }
            }
        }
    }

    // MARK: C. Table viewer

    private func tableViewer(_ package: ReadResult) -> some View {
        VStack(alignment: .leading, spacing: 0) {
            if let table = selectedTable(in: package) {
                tableHeaderBar(table)
                Divider()
                if table.rowCount == 0 {
                    emptyTableState(table)
                } else {
                    tableGrid(table)
                }
            } else {
                centeredMessage(
                    systemImage: "tablecells",
                    title: "No table selected",
                    subtitle: "Select a table from the list to view its rows."
                )
            }
        }
    }

    private func tableHeaderBar(_ table: VerifiedTable) -> some View {
        HStack(spacing: 10) {
            Text(table.fileName)
                .font(.headline)
                .textSelection(.enabled)
            Text("\(table.columns.count) columns · \(table.rowCount) rows")
                .font(.caption)
                .foregroundStyle(.secondary)
            Spacer()
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 8)
    }

    private func emptyTableState(_ table: VerifiedTable) -> some View {
        VStack(spacing: 0) {
            // Keep the column header visible so the table's shape is clear even with no rows.
            ScrollView(.horizontal, showsIndicators: true) {
                tableHeaderRow(columnNames: table.columns.map(\.name))
            }
            Spacer()
            Label("This table has no rows (row count 0).", systemImage: "tray")
                .font(.callout)
                .foregroundStyle(.secondary)
            Spacer()
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func tableGrid(_ table: VerifiedTable) -> some View {
        let columnNames = table.columns.map(\.name)
        let totalWidth = Self.rowNumberWidth + CGFloat(columnNames.count) * Self.cellWidth
        return ScrollView([.horizontal, .vertical]) {
            // Lazy rows keep ordinary larger tables usable; the pinned section header keeps the column
            // names visible during vertical scroll and scrolls with the body horizontally.
            LazyVStack(alignment: .leading, spacing: 0, pinnedViews: [.sectionHeaders]) {
                Section {
                    ForEach(Array(table.rows.enumerated()), id: \.offset) { index, row in
                        tableBodyRow(index: index, row: row)
                    }
                } header: {
                    tableHeaderRow(columnNames: columnNames)
                }
            }
            .frame(width: totalWidth, alignment: .leading)
        }
    }

    private func tableHeaderRow(columnNames: [String]) -> some View {
        HStack(spacing: 0) {
            headerCell("#", width: Self.rowNumberWidth, alignment: .trailing)
            ForEach(Array(columnNames.enumerated()), id: \.offset) { _, name in
                headerCell(name, width: Self.cellWidth, alignment: .leading)
            }
        }
        .background(.regularMaterial)
        .overlay(alignment: .bottom) { Divider() }
    }

    private func headerCell(_ text: String, width: CGFloat, alignment: Alignment) -> some View {
        Text(text)
            .font(.caption.weight(.semibold))
            .lineLimit(1)
            .truncationMode(.tail)
            .help(text)
            .frame(width: width, alignment: alignment)
            .padding(.horizontal, 8)
            .padding(.vertical, 6)
    }

    private func tableBodyRow(index: Int, row: [String]) -> some View {
        HStack(spacing: 0) {
            Text("\(index + 1)")
                .font(.caption2.monospacedDigit())
                .foregroundStyle(.secondary)
                .frame(width: Self.rowNumberWidth, alignment: .trailing)
                .padding(.horizontal, 8)
                .padding(.vertical, 5)
            // Each cell is displayed exactly as returned; only the on-screen line is truncated (the full
            // value is exposed via tooltip and text selection), never the underlying string.
            ForEach(Array(row.enumerated()), id: \.offset) { _, value in
                bodyCell(value)
            }
        }
        .background(index.isMultiple(of: 2) ? Color.clear : Color.primary.opacity(0.035))
    }

    private func bodyCell(_ value: String) -> some View {
        Text(value)
            .font(.caption.monospaced())
            .lineLimit(1)
            .truncationMode(.tail)
            .help(value)
            .textSelection(.enabled)
            .frame(width: Self.cellWidth, alignment: .leading)
            .padding(.horizontal, 8)
            .padding(.vertical, 5)
    }

    // MARK: - Shared helpers

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
                Text("Could not read the package")
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
                    .disabled(document.isResultPackageReading)
                    .padding(.top, 4)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .padding(40)
    }
}
