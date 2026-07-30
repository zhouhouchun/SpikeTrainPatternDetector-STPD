import SwiftUI

struct WorkbenchDetailView: View {
    @Bindable var document: RasterDocument
    let section: WorkbenchSection
    @Binding var selectedSection: WorkbenchSection?

    var body: some View {
        switch section {
        case .alignedRaster:
            RasterWorkspaceView(document: document, initialTimeMode: .aligned)
                .id(section.id)
        case .rawRaster:
            RasterWorkspaceView(document: document, initialTimeMode: .raw)
                .id(section.id)
        case .dataQC:
            DataQCView(document: document)
                .id(section.id)
        case .isiProfile:
            ISITimelineView(document: document)
                .id(section.id)
        case .isiStateSpace:
            ISIStateSpaceView(document: document)
                .id(section.id)
        case .structuralCandidates:
            StructuralCandidatesView(document: document, selectedSection: $selectedSection)
                .id(section.id)
        case .datasetISIHistogram:
            DatasetISIHistogramView(document: document)
                .id(section.id)
        case .detectorParameters:
            DetectorParametersView(document: document)
                .id(section.id)
        case .eventsOutput:
            ResultPackageReadbackView(document: document)
                .id(section.id)
        default:
            ModulePlaceholderView(document: document, section: section)
        }
    }
}

private struct ModulePlaceholderView: View {
    @Bindable var document: RasterDocument
    let section: WorkbenchSection

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 18) {
                header

                Divider()

                HStack(alignment: .top, spacing: 16) {
                    VStack(alignment: .leading, spacing: 12) {
                        PlaceholderPanel(
                            title: "Shiny source",
                            rows: [
                                "R/18_ui.R:\(section.sourceLine)",
                                section.group.title,
                                section.migrationStatus
                            ]
                        )

                        PlaceholderPanel(
                            title: "Next migration slot",
                            rows: [
                                "Controls",
                                "Result tables",
                                "Plots"
                            ]
                        )
                    }
                    .frame(minWidth: 240, idealWidth: 280, maxWidth: 340)

                    PlaceholderPanel(
                        title: "Module surface",
                        rows: placeholderRows
                    )
                    .frame(maxWidth: .infinity, alignment: .topLeading)
                }
            }
            .padding(20)
            .frame(maxWidth: .infinity, alignment: .topLeading)
        }
    }

    private var header: some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Image(systemName: section.systemImage)
                .foregroundStyle(.secondary)
            Text(section.title)
                .font(.title3.weight(.semibold))
            Text("UI shell")
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(.quaternary, in: Capsule())
            Spacer()
        }
    }

    private var placeholderRows: [String] {
        [
            "Native macOS page is reserved",
            "Algorithm module is not migrated yet",
            document.dataset == nil ? "No dataset loaded" : "Dataset loaded"
        ]
    }
}

private struct PlaceholderPanel: View {
    let title: String
    let rows: [String]

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            Text(title)
                .font(.headline)

            ForEach(rows, id: \.self) { row in
                HStack(spacing: 8) {
                    Circle()
                        .fill(Color.secondary.opacity(0.45))
                        .frame(width: 5, height: 5)
                    Text(row)
                        .foregroundStyle(.secondary)
                        .lineLimit(2)
                }
            }
        }
        .padding(14)
        .frame(maxWidth: .infinity, alignment: .topLeading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }
}
