import AppKit
import Foundation
import STPDCore
import UniformTypeIdentifiers

// Phase 2.2C-B4: read-only `.stpdresult` loading. `STPDResultPackageReader` is the sole readback
// authority; this file only drives an open panel, runs the read off the MainActor, and marshals a small
// Sendable outcome back. It never mutates the active detector document (dataset / run / settings /
// reviews / manual annotations), and it never reparses CSV or reinterprets any table value.
extension RasterDocument {
    /// The `.stpdresult` package content type, resolved from its filename extension. Used only to hint the
    /// open panel; the reader — not the panel — is the authority on what constitutes a valid package.
    private static var stpdResultPackageContentType: UTType? {
        UTType(filenameExtension: "stpdresult")
    }

    /// Presents an `NSOpenPanel` for a single `.stpdresult` package and begins a read. Shared verbatim by
    /// the File menu, the toolbar, and the Events/Output page button, so all three paths behave identically.
    func openResultPackageWithPanel() {
        // Prevent duplicate read requests: ignore a new open while a read is already in flight.
        guard !isResultPackageReading else {
            return
        }

        let panel = NSOpenPanel()
        panel.allowsMultipleSelection = false
        panel.canChooseDirectories = true
        panel.canChooseFiles = true
        panel.message = "Choose a verified .stpdresult package to view. It is opened strictly read-only and validated on open."
        panel.prompt = "Open"
        if let contentType = Self.stpdResultPackageContentType {
            panel.allowedContentTypes = [contentType]
        }

        guard panel.runModal() == .OK, let url = panel.url else {
            return
        }

        beginResultPackageRead(at: url)
    }

    /// Starts an isolated, detached read. A monotonically increasing request token guards against a stale
    /// completion overwriting a newer request (or a completion arriving after an explicit clear).
    private func beginResultPackageRead(at url: URL) {
        let token = resultPackageReadRequestToken &+ 1
        resultPackageReadRequestToken = token
        isResultPackageReading = true
        resultPackageReadbackErrorMessage = nil

        // Only Sendable values (`url`, `token`) cross into the detached task; no mutable document state is
        // captured. The reader is a nonisolated pure function, so it runs off the MainActor.
        Task.detached(priority: .userInitiated) {
            let outcome: ResultPackageReadOutcome
            do {
                let result = try STPDResultPackageReader.read(packageAt: url)
                outcome = .success(result)
            } catch {
                outcome = .failure(error.localizedDescription)
            }
            await MainActor.run {
                self.finishResultPackageRead(outcome, url: url, token: token)
            }
        }
    }

    /// Applies a completed read on the MainActor, ignoring it if a newer request has since started.
    private func finishResultPackageRead(_ outcome: ResultPackageReadOutcome, url: URL, token: Int) {
        guard token == resultPackageReadRequestToken else {
            return // superseded by a newer read request or an explicit clear
        }

        isResultPackageReading = false

        switch outcome {
        case .success(let result):
            loadedResultPackage = result
            loadedResultPackageURL = url
            resultPackageReadbackErrorMessage = nil
            resultPackageLoadCompletionID &+= 1 // signal the UI to navigate to `.eventsOutput`
        case .failure(let message):
            // Retain no partial result; the active detector document is left untouched.
            resultPackageReadbackErrorMessage = message
        }
    }

    /// Dismisses only the last read-failure message (e.g. after a failed open while a package is already
    /// loaded). Leaves the loaded package and the active detector document untouched.
    func dismissResultPackageReadbackError() {
        resultPackageReadbackErrorMessage = nil
    }

    /// Clears only the in-memory readback result. The active detector document is untouched. Bumping the
    /// request token ensures any in-flight read cannot repopulate the viewer after an explicit clear.
    func clearLoadedResultPackage() {
        resultPackageReadRequestToken &+= 1
        isResultPackageReading = false
        loadedResultPackage = nil
        loadedResultPackageURL = nil
        resultPackageReadbackErrorMessage = nil
    }
}

/// A small Sendable success/failure value marshaled from the detached read back to the MainActor.
private enum ResultPackageReadOutcome: Sendable {
    case success(STPDResultPackageReadResult)
    case failure(String)
}
