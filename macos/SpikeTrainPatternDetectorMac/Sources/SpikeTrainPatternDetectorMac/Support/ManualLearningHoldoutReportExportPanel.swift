import AppKit
import Foundation
import STPDCore
import STPDTabularIO
import UniformTypeIdentifiers

enum ManualLearningReportFormat: Int, CaseIterable {
    case csv
    case xlsx

    var fileExtension: String {
        switch self {
        case .csv: "csv"
        case .xlsx: "xlsx"
        }
    }

    var contentType: UTType {
        switch self {
        case .csv: .commaSeparatedText
        case .xlsx: UTType(filenameExtension: "xlsx") ?? .data
        }
    }
}

struct ManualLearningHoldoutReportExportSelection {
    let destination: URL
    let format: ManualLearningReportFormat
}

@MainActor
enum ManualLearningHoldoutReportExportPanel {
    static func chooseDestination(
        suggestedStem: String,
        message: String,
        formatLabel: String,
        csvTitle: String,
        xlsxTitle: String
    ) -> ManualLearningHoldoutReportExportSelection? {
        let panel = NSSavePanel()
        panel.canCreateDirectories = true
        panel.isExtensionHidden = false
        panel.message = message

        let label = NSTextField(labelWithString: formatLabel)
        label.font = .systemFont(ofSize: NSFont.systemFontSize, weight: .medium)
        let popup = NSPopUpButton(frame: .zero, pullsDown: false)
        popup.addItems(withTitles: [csvTitle, xlsxTitle])
        let accessory = NSStackView(views: [label, popup])
        accessory.orientation = .horizontal
        accessory.alignment = .centerY
        accessory.spacing = 10
        accessory.edgeInsets = NSEdgeInsets(top: 8, left: 0, bottom: 8, right: 0)
        panel.accessoryView = accessory

        func selectedFormat() -> ManualLearningReportFormat {
            ManualLearningReportFormat(rawValue: max(0, popup.indexOfSelectedItem)) ?? .csv
        }
        func applySelection() {
            let format = selectedFormat()
            panel.allowedContentTypes = [format.contentType]
            panel.nameFieldStringValue = "\(suggestedStem).\(format.fileExtension)"
        }
        let relay = PopupSelectionRelay(action: applySelection)
        popup.target = relay
        popup.action = #selector(PopupSelectionRelay.changed)
        applySelection()

        let response = withExtendedLifetime(relay) { panel.runModal() }
        guard response == .OK, let selectedURL = panel.url else { return nil }
        let format = selectedFormat()
        guard selectedURL.pathExtension.caseInsensitiveCompare(format.fileExtension)
                == .orderedSame else { return nil }
        return ManualLearningHoldoutReportExportSelection(
            destination: selectedURL,
            format: format
        )
    }

    static func write(
        report: ManualLearningHoldoutValidationReport,
        selection: ManualLearningHoldoutReportExportSelection
    ) throws {
        let table = ManualLearningHoldoutReportExporter.table(report: report)
        let data: Data
        switch selection.format {
        case .csv:
            data = Data(ManualLearningHoldoutReportExporter.csv(report: report).utf8)
        case .xlsx:
            data = try RectangularTableXLSXExporter.data(
                table: table,
                worksheetName: "Holdout validation"
            )
        }
        try data.write(to: selection.destination, options: .atomic)
    }
}

@MainActor
private final class PopupSelectionRelay: NSObject {
    private let action: () -> Void

    init(action: @escaping () -> Void) {
        self.action = action
    }

    @objc func changed() {
        action()
    }
}
