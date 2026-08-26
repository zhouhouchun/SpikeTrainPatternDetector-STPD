import AppKit
import STPDCore
import SwiftUI

@MainActor
@main
final class AppDelegate: NSObject, NSApplicationDelegate, NSMenuItemValidation {
    private static var retainedDelegate: AppDelegate?

    private let document = RasterDocument()
    private var mainWindow: NSWindow?
    private var distributionFoundationWindow: NSWindow?

    static func main() {
        let app = NSApplication.shared
        let delegate = AppDelegate()
        retainedDelegate = delegate
        app.delegate = delegate
        app.setActivationPolicy(.regular)
        app.finishLaunching()
        app.run()
    }

    func applicationDidFinishLaunching(_ notification: Notification) {
        installMenus()
        document.loadBundledSampleIfNeeded()
        DispatchQueue.main.async {
            self.showMainWindow()
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.75) {
            self.showMainWindow()
        }
    }

    func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool {
        showMainWindow()
        return true
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        false
    }

    @objc private func openScientificData() {
        showMainWindow()
        document.openScientificImportWithPanel()
    }

    @objc private func loadSample() {
        document.loadBundledSample()
        showMainWindow()
    }

    @objc private func importManualAnnotations() {
        showMainWindow()
        document.importManualAnnotationsWithPanel()
    }

    @objc private func openResultPackage() {
        showMainWindow()
        document.openResultPackageWithPanel()
    }

    @objc private func openCanonicalManualResultPackage() {
        showMainWindow()
        document.openCanonicalManualResultPackageWithPanel()
    }

    func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
        if menuItem.action == #selector(importManualAnnotations) {
            return document.canImportAuthoritativeManualAnnotations
        }
        if menuItem.action == #selector(openResultPackage)
            || menuItem.action == #selector(openCanonicalManualResultPackage) {
            return !document.isResultPackageReading
        }
        return true
    }

    // Temporary DEBUG path: open a window inspecting the distribution-first foundation (D1-D3) for the
    // live document. Not part of the production Workbench navigation.
    @objc private func openDistributionFoundation() {
        let window = distributionFoundationWindow ?? makeDistributionFoundationWindow()
        distributionFoundationWindow = window
        placeOnVisibleScreenIfNeeded(window)
        window.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
    }

    private func showMainWindow() {
        let window = mainWindow ?? makeMainWindow()
        mainWindow = window
        placeOnVisibleScreenIfNeeded(window)
        window.makeKeyAndOrderFront(nil)
        window.orderFrontRegardless()
        NSApp.activate(ignoringOtherApps: true)
    }

    private func makeMainWindow() -> NSWindow {
        let rootView = ContentView(document: document)
            .frame(minWidth: 1240, minHeight: 700)

        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1420, height: 820),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        window.title = "Spike Train Pattern Detector"
        window.titleVisibility = .hidden
        window.titlebarAppearsTransparent = true
        // Keep the titlebar free of app controls.  The persistent title/actions
        // row lives inside ContentView so it remains visible in full-screen and
        // follows the document content rather than the window chrome.
        window.contentViewController = NSHostingController(rootView: rootView)
        window.isReleasedWhenClosed = false
        window.minSize = NSSize(width: 1240, height: 700)
        return window
    }

    private func makeDistributionFoundationWindow() -> NSWindow {
        let rootView = ISIDistributionFoundationDebugHost(document: document)
            .frame(minWidth: 680, minHeight: 640)

        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 760, height: 820),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        window.title = "Distribution-first foundation (D1-D3) — Debug"
        window.contentViewController = NSHostingController(rootView: rootView)
        window.isReleasedWhenClosed = false
        return window
    }

    private func placeOnVisibleScreenIfNeeded(_ window: NSWindow) {
        guard let screen = preferredVisibleScreen() else {
            return
        }

        let visibleFrame = screen.visibleFrame
        let currentFrame = window.frame
        let targetContainsWindow = visibleFrame.intersects(currentFrame) &&
            currentFrame.minX >= visibleFrame.minX &&
            currentFrame.maxX <= visibleFrame.maxX &&
            currentFrame.minY >= visibleFrame.minY &&
            currentFrame.maxY <= visibleFrame.maxY

        guard !targetContainsWindow || currentFrame.width < 100 || currentFrame.height < 100 else {
            return
        }

        let width = min(max(currentFrame.width, 1360), visibleFrame.width * 0.92)
        let height = min(max(currentFrame.height, 760), visibleFrame.height * 0.88)
        let x = visibleFrame.minX + max(40, (visibleFrame.width - width) / 2)
        let y = visibleFrame.minY + max(40, (visibleFrame.height - height) / 2)
        window.setFrame(NSRect(x: x, y: y, width: width, height: height), display: true)
    }

    private func preferredVisibleScreen() -> NSScreen? {
        NSScreen.screens.first { screen in
            screen.visibleFrame.minX >= 0 && screen.visibleFrame.minY >= 0
        } ?? NSScreen.main ?? NSScreen.screens.first
    }

    private func installMenus() {
        let mainMenu = NSMenu()

        let appMenuItem = NSMenuItem()
        mainMenu.addItem(appMenuItem)
        let appMenu = NSMenu()
        appMenuItem.submenu = appMenu
        appMenu.addItem(
            withTitle: "Quit Spike Train Pattern Detector",
            action: #selector(NSApplication.terminate(_:)),
            keyEquivalent: "q"
        )

        let fileMenuItem = NSMenuItem()
        mainMenu.addItem(fileMenuItem)
        let fileMenu = NSMenu(title: "File")
        fileMenuItem.submenu = fileMenu
        fileMenu.addItem(
            withTitle: "Import CSV or XLSX...",
            action: #selector(openScientificData),
            keyEquivalent: "o"
        ).target = self
        fileMenu.addItem(
            withTitle: "Import Manual Annotations...",
            action: #selector(importManualAnnotations),
            keyEquivalent: ""
        ).target = self
        fileMenu.addItem(
            withTitle: "Open Result Package...",
            action: #selector(openResultPackage),
            keyEquivalent: ""
        ).target = self
        fileMenu.addItem(
            withTitle: "Open Manual Result Package...",
            action: #selector(openCanonicalManualResultPackage),
            keyEquivalent: ""
        ).target = self
        fileMenu.addItem(.separator())
        fileMenu.addItem(
            withTitle: "Load Demo Sample",
            action: #selector(loadSample),
            keyEquivalent: "r"
        ).target = self

        // Temporary Debug menu — runtime inspection of the distribution-first foundation (D1-D3).
        let debugMenuItem = NSMenuItem()
        mainMenu.addItem(debugMenuItem)
        let debugMenu = NSMenu(title: "Debug")
        debugMenuItem.submenu = debugMenu
        debugMenu.addItem(
            withTitle: "Distribution-first foundation (D1-D3)",
            action: #selector(openDistributionFoundation),
            keyEquivalent: ""
        ).target = self

        NSApp.mainMenu = mainMenu
    }
}

struct MainTitlebarLogo: View {
    private let green = Color(red: 0.149, green: 0.569, blue: 0.455)

    private var logoImage: Image {
        if let url = Bundle.main.url(forResource: "LogoMark", withExtension: "png"),
           let image = NSImage(contentsOf: url) {
            return Image(nsImage: image)
        }
        return Image(systemName: "waveform.path.ecg")
    }

    var body: some View {
        let mark = logoImage
            .resizable()
            .renderingMode(.template)
            .scaledToFit()

        ZStack {
            if #available(macOS 26.0, *) {
                // The native Liquid Glass surface is clipped back to the mark's
                // alpha, so the glass belongs to the green line rather than to a
                // rectangular icon backdrop.
                mark
                    .foregroundStyle(green)
                    .glassEffect(.regular.tint(green.opacity(0.86)), in: Rectangle())
                    .mask(mark)
            } else {
                // macOS 14 fallback: retain the native adaptive material and a
                // restrained highlight for systems without glassEffect.
                mark.foregroundStyle(green.opacity(0.82))
                mark.foregroundStyle(.regularMaterial).opacity(0.48)
                mark.foregroundStyle(
                    LinearGradient(
                        colors: [
                            Color.white.opacity(0.72),
                            green.opacity(0.90),
                            Color.black.opacity(0.18),
                        ],
                        startPoint: .topLeading,
                        endPoint: .bottomTrailing
                    )
                )
            }

            mark
                .foregroundStyle(Color.white.opacity(0.42))
                .blur(radius: 0.30)
                .offset(y: -0.45)
                .mask(
                    LinearGradient(
                        colors: [.white, .white.opacity(0.08), .clear],
                        startPoint: .top,
                        endPoint: .bottom
                    )
                )
        }
        .padding(1)
        .accessibilityLabel("Spike Train Pattern Detector logo")
    }
}

struct MainTitlebarActions: View {
    @Bindable var document: RasterDocument
    @Binding var language: STPDLanguage
    @Binding var isLeftSidebarVisible: Bool
    @Binding var isRightSidebarVisible: Bool
    @Environment(\.l10n) private var l10n

    var body: some View {
        ViewThatFits(in: .horizontal) {
            HStack(spacing: 12) {
                TitlebarLanguageControl(selection: $language)
                Button { isLeftSidebarVisible.toggle() } label: { actionLabel("左侧栏") }
                Button { isRightSidebarVisible.toggle() } label: { actionLabel("右侧栏") }
                Button { document.openScientificImportWithPanel() } label: { actionLabel("导入数据") }
                Menu { outputMenu } label: { actionLabel("输出") }
                Menu { moreMenu } label: { actionLabel("更多") }
            }

            HStack(spacing: 12) {
                TitlebarLanguageControl(selection: $language)
                Menu {
                    Button(l10n.t("左侧栏")) { isLeftSidebarVisible.toggle() }
                    Button(l10n.t("右侧栏")) { isRightSidebarVisible.toggle() }
                    Button(l10n.t("导入数据")) { document.openScientificImportWithPanel() }
                    Menu(l10n.t("输出")) { outputMenu }
                    Menu(l10n.t("更多")) { moreMenu }
                } label: {
                    actionLabel("功能")
                }
            }
        }
        .font(.system(size: 15, weight: .medium))
        .buttonStyle(.borderless)
        .padding(.horizontal, 8)
        .padding(.vertical, 3)
        .background(.thinMaterial, in: Capsule())
        .fixedSize(horizontal: false, vertical: true)
    }

    private func actionLabel(_ title: String) -> some View {
        Text(l10n.t(title))
            .font(.system(size: 15, weight: .medium))
            .foregroundStyle(.primary)
    }

    @ViewBuilder
    private var outputMenu: some View {
        Button(l10n.t("打开结果包")) { document.openResultPackageWithPanel() }
            .disabled(document.isResultPackageReading)
        Button(l10n.t("打开人工审核结果包")) { document.openCanonicalManualResultPackageWithPanel() }
            .disabled(document.isResultPackageReading)
        Button(l10n.t("导出事件 CSV")) { document.exportClassicAnchorEventsCSVWithPanel() }
            .disabled(!document.canExportClassicAnchorEventsCSV)
        Button(l10n.t("导出逐 ISI 审核草稿")) { document.exportReviewedISIDraftCSVWithPanel() }
            .disabled(!document.canExportReviewedISIDraft)
        Button(l10n.t("导出 HFS-Burst 审计 CSV")) { document.exportHFSBurstArbitrationAuditCSVWithPanel() }
            .disabled(!document.canExportHFSBurstArbitrationAuditCSV)
        ResultPackageExportButton(document: document)
    }

    @ViewBuilder
    private var moreMenu: some View {
        Button(l10n.t("导入人工标注")) { document.importManualAnnotationsWithPanel() }
            .disabled(!document.canImportAuthoritativeManualAnnotations)
        Button(l10n.t("导入审核结果")) { document.importClassicAnchorReviewStatusesWithPanel() }
            .disabled(!document.hasDetectorResults)
        Button(l10n.t("加载示例")) { document.loadBundledSample() }
    }
}

struct TitlebarLanguageControl: View {
    @Binding var selection: STPDLanguage

    var body: some View {
        HStack(spacing: 2) {
            ForEach(STPDLanguage.allCases) { language in
                Button {
                    selection = language
                } label: {
                    Text(language.nativeLabel)
                        .font(.system(size: 15, weight: .semibold))
                        .foregroundStyle(selection == language ? Color.white : Color.primary)
                        .padding(.horizontal, 8)
                        .padding(.vertical, 4)
                        .background {
                            if selection == language {
                                Capsule().fill(STPDAppTheme.accent)
                            }
                        }
                }
                .buttonStyle(.plain)
                .accessibilityLabel(language.nativeLabel)
            }
        }
    }
}
