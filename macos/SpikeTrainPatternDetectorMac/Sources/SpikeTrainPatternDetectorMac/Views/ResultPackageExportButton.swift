import SwiftUI

struct ResultPackageExportButton: View {
    @Bindable var document: RasterDocument

    var body: some View {
        Button {
            document.exportResultPackageWithPanel()
        } label: {
            if document.isResultPackageExporting {
                HStack(spacing: 6) {
                    ProgressView()
                        .controlSize(.small)
                    Text("正在导出结果包…")
                }
            } else {
                Label("导出结果包…", systemImage: "archivebox")
            }
        }
        .disabled(
            document.isResultPackageExporting
                || !document.canExportCurrentResultPackage
        )
        .help(
            document.activeDatasetScientificStanding.permitsSealedResultExport
                ? "Export the normalized provenance-bound result tables for the current detector and review snapshot"
                : "Locked: demo and legacy imports are exploratory until canonical scientific confirmation is implemented and completed"
        )
    }
}
