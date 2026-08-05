import AppKit
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

    func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
        if menuItem.action == #selector(importManualAnnotations) {
            return document.canImportAuthoritativeManualAnnotations
        }
        if menuItem.action == #selector(openResultPackage) {
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
