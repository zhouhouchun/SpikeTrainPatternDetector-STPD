import STPDCore
import SwiftUI

struct ContentView: View {
    @Bindable var document: RasterDocument
    @State private var selectedSection: WorkbenchSection? = .alignedRaster
    @State private var isSidebarVisible = true
    @State private var isInspectorVisible = true

    var body: some View {
        HSplitView {
            if isSidebarVisible {
                SidebarView(document: document, selectedSection: $selectedSection)
                    .frame(minWidth: 190, idealWidth: 230, maxWidth: 300)
            }

            WorkbenchDetailView(
                document: document,
                section: selectedSection ?? .alignedRaster,
                selectedSection: $selectedSection
            )
            .frame(minWidth: 720, maxWidth: .infinity, maxHeight: .infinity)

            if isInspectorVisible {
                InspectorView(document: document)
                    .frame(minWidth: 280, idealWidth: 320, maxWidth: 390)
            }
        }
        .frame(minWidth: 980, minHeight: 640)
        .onChange(of: document.classicAnchorFocusRequestID) { _, _ in
            if document.focusedClassicAnchorCandidateID != nil {
                isInspectorVisible = true
            } else {
                isInspectorVisible = false
            }
        }
        .onChange(of: document.resultPackageLoadCompletionID) { _, _ in
            // A verified result package finished loading — surface it on the Events / Output page.
            selectedSection = .eventsOutput
        }
        .toolbar {
            ToolbarItem {
                Button {
                    isSidebarVisible.toggle()
                } label: {
                    Label(isSidebarVisible ? "Hide Sidebar" : "Show Sidebar", systemImage: "sidebar.left")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()
                .help(isSidebarVisible ? "Hide sidebar" : "Show sidebar")
            }

            ToolbarItem {
                Button {
                    isInspectorVisible.toggle()
                } label: {
                    Label(isInspectorVisible ? "Hide Inspector" : "Show Inspector", systemImage: "sidebar.right")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()
                .help(isInspectorVisible ? "Hide inspector" : "Show inspector")
            }

            ToolbarItemGroup {
                Button {
                    document.openCSVWithPanel()
                } label: {
                    Label("Open CSV", systemImage: "folder")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()

                Button {
                    document.importManualAnnotationsWithPanel()
                } label: {
                    Label("Import Manual Annotations", systemImage: "person.text.rectangle")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()
                .disabled(
                    !document.canImportAuthoritativeManualAnnotations
                )
                .help("Import identity-bound manual annotations with explicit approval")

                Button {
                    document.openResultPackageWithPanel()
                } label: {
                    Label("Open Result Package", systemImage: "shippingbox")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()
                .disabled(document.isResultPackageReading)
                .help("Open a verified .stpdresult package (read-only)")

                ResultPackageExportButton(document: document)
                    .labelStyle(.iconOnly)
                    .liquidGlassToolbarButtonStyle()

                Button {
                    document.exportClassicAnchorEventsCSVWithPanel()
                } label: {
                    Label("Export Events CSV", systemImage: "square.and.arrow.down")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()
                .disabled(!document.hasDetectorResults)
                .help("Export structural candidate events as CSV")

                Button {
                    document.exportHFSBurstArbitrationAuditCSVWithPanel()
                } label: {
                    Label("Export HFS-Burst Audit CSV", systemImage: "doc.text.magnifyingglass")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()
                .disabled(!document.hasHFSBurstArbitrationAuditRows)
                .help("Export diagnostic HFS versus burst arbitration evidence")

                Button {
                    document.importClassicAnchorReviewStatusesWithPanel()
                } label: {
                    Label("Import Reviews", systemImage: "tray.and.arrow.down")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()
                .disabled(!document.hasDetectorResults)
                .help("Import manual review states from an exported event CSV")

                Button {
                    document.loadBundledSample()
                } label: {
                    Label("Load Sample", systemImage: "arrow.clockwise")
                }
                .labelStyle(.iconOnly)
                .liquidGlassToolbarButtonStyle()
            }
        }
    }
}
