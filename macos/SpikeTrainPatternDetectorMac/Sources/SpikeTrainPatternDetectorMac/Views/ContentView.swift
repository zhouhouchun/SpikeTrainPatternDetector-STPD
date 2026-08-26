import STPDCore
import SwiftUI

struct ContentView: View {
    @Bindable var document: RasterDocument
    @State private var selectedSection: WorkbenchSection? = .alignedRaster
    @State private var isSidebarVisible = true
    // The detail inspector is useful on demand, but it should not consume
    // workspace width until the user opens the right sidebar explicitly.
    @State private var isInspectorVisible = false
    @AppStorage(AppLanguageStorage.key) private var languageRaw = STPDLanguage.zh.rawValue

    private var language: STPDLanguage { STPDLanguage(rawValue: languageRaw) ?? .zh }

    private var sidebarMinWidth: CGFloat { language == .ru ? 170 : 145 }
    private var sidebarIdealWidth: CGFloat { language == .ru ? 205 : 175 }
    private var sidebarMaxWidth: CGFloat { language == .ru ? 260 : 225 }

    private var languageBinding: Binding<STPDLanguage> {
        Binding(
            get: { STPDLanguage(rawValue: languageRaw) ?? .zh },
            set: { languageRaw = $0.rawValue }
        )
    }

    private var longRunningOperationMessage: String? {
        let source: String?
        if document.isDetectorRunning {
            source = "正在运行模式检测并构建审计结果…"
        } else if document.isManualAnnotationImporting {
            source = "正在导入、校验并绑定手工标记…"
        } else if document.isCanonicalManualISIDraftImporting {
            source = "正在读取并验证规范手工 ISI 草稿…"
        } else if document.isResultPackageExporting {
            source = "正在构建、校验并导出结果包…"
        } else if document.isResultPackageReading {
            source = "正在读取并完整校验结果包…"
        } else if document.scientificImportCoordinator.isPersisting {
            source = "正在保存或恢复并校验科学导入合同…"
        } else if case .computing = document.neuralManifoldIsomapState {
            source = "正在计算 Isomap 神经流形…"
        } else if case .computing = document.neuralManifoldPhateState {
            source = "正在计算 PHATE 神经流形…"
        } else {
            source = nil
        }
        return source.map { STPDLocalization.text($0, language: language) }
    }

    var body: some View {
        let localizer = STPDLocalizer(language: language)
        return VStack(spacing: 0) {
            MainContentHeader(
                document: document,
                language: languageBinding,
                isSidebarVisible: $isSidebarVisible,
                isInspectorVisible: $isInspectorVisible
            )
            .workbenchGlassPane(cornerRadius: 14)
            .padding(.horizontal, 8)
            .padding(.top, 6)
            .padding(.bottom, 4)

            HSplitView {
                if isSidebarVisible {
                    SidebarView(document: document, selectedSection: $selectedSection)
                        .frame(
                            minWidth: sidebarMinWidth,
                            idealWidth: sidebarIdealWidth,
                            maxWidth: sidebarMaxWidth
                        )
                        .workbenchGlassPane(cornerRadius: 16)
                        .padding(.leading, 8)
                        .padding(.vertical, 4)
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
                        .workbenchGlassPane(cornerRadius: 16)
                        .padding(.trailing, 8)
                        .padding(.vertical, 4)
                }
            }
        }
        .frame(minWidth: 980, minHeight: 640)
        .background(Color(nsColor: .windowBackgroundColor))
        .environment(\.l10n, localizer)
        .delayedBackgroundProgress(longRunningOperationMessage)
        .tint(STPDAppTheme.accent)
        .accentColor(STPDAppTheme.accent)
        .onChange(of: document.classicAnchorFocusRequestID) { _, _ in
            if document.focusedClassicAnchorCandidateID != nil {
                isInspectorVisible = true
            } else if document.pinnedISIDiagnostic == nil {
                isInspectorVisible = false
            }
        }
        .onChange(of: document.pinnedISIRequestID) { _, _ in
            if document.pinnedISIDiagnostic != nil { isInspectorVisible = true }
        }
        .onChange(of: document.resultPackageLoadCompletionID) { _, _ in
            // A verified result package finished loading — surface it on the Events / Output page.
            selectedSection = .eventsOutput
        }
        .sheet(
            isPresented: $document.isScientificImportSheetPresented,
            onDismiss: {
                // Preserve a verified persisted confirmation when the user finishes the wizard and
                // proceeds to manual analysis. An unfinished/cancelled import still retires all
                // source-bound transient state.
                if document.scientificImportCoordinator.persistedConfirmation == nil {
                    document.scientificImportCoordinator.cancel()
                }
            }
        ) {
            ScientificImportSheet(
                coordinator: document.scientificImportCoordinator,
                onClose: { document.dismissScientificImportReview() },
                onProceedToManualAnalysis: {
                    _ = document.prepareCanonicalManualWorkbench()
                    selectedSection = .manualDetectorReport
                    document.dismissScientificImportReview()
                }
            )
        }
    }
}

/// In-content application header.  This is deliberately not an NSTitlebar
/// accessory: window titlebars can collapse or disappear in full-screen, while
/// the workbench header must remain available for navigation and export.
struct MainContentHeader: View {
    @Bindable var document: RasterDocument
    @Binding var language: STPDLanguage
    @Binding var isSidebarVisible: Bool
    @Binding var isInspectorVisible: Bool

    var body: some View {
        HStack(alignment: .center, spacing: 14) {
            HStack(spacing: 10) {
                MainTitlebarLogo()
                    // The mark is intentionally the visual anchor of the
                    // header, independent of the adjacent title size.
                    .frame(width: 45, height: 45)

                Text("Spike Train Pattern Detector")
                    .font(.system(size: 20, weight: .semibold))
                    .foregroundStyle(.primary)
                    .lineLimit(1)
                    .minimumScaleFactor(0.8)
            }
            .layoutPriority(1)

            Spacer(minLength: 12)

            MainTitlebarActions(
                document: document,
                language: $language,
                isLeftSidebarVisible: $isSidebarVisible,
                isRightSidebarVisible: $isInspectorVisible
            )
                .environment(\.controlSize, .regular)
        }
        .padding(.horizontal, 18)
        .padding(.top, 5)
        .padding(.bottom, 5)
        .frame(maxWidth: .infinity, minHeight: 45, alignment: .center)
        .accessibilityElement(children: .contain)
        .accessibilityLabel("Spike Train Pattern Detector controls")
    }
}

/// A restrained, native-looking glass surface used only for the three
/// persistent workbench regions.  It replaces high-contrast separator lines
/// with depth, while keeping the central scientific workspace unobstructed.
private struct WorkbenchGlassPane: ViewModifier {
    let cornerRadius: CGFloat

    func body(content: Content) -> some View {
        let shape = RoundedRectangle(cornerRadius: cornerRadius, style: .continuous)

        content
            .background {
                if #available(macOS 26.0, *) {
                    shape
                        .fill(.clear)
                        .glassEffect(.regular, in: shape)
                } else {
                    shape.fill(.regularMaterial)
                }
            }
            .clipShape(shape)
            .shadow(color: Color.black.opacity(0.075), radius: 9, x: 0, y: 3)
            .shadow(color: Color.white.opacity(0.30), radius: 1, x: 0, y: -0.5)
    }
}

private extension View {
    func workbenchGlassPane(cornerRadius: CGFloat) -> some View {
        modifier(WorkbenchGlassPane(cornerRadius: cornerRadius))
    }
}
