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
                    Text("Exporting Result Package…")
                }
            } else {
                Label("Export Result Package…", systemImage: "archivebox")
            }
        }
        .disabled(
            document.isResultPackageExporting
                || !document.canExportCurrentResultPackage
        )
        .help("Export the normalized provenance-bound result tables for the current detector and review snapshot")
    }
}
