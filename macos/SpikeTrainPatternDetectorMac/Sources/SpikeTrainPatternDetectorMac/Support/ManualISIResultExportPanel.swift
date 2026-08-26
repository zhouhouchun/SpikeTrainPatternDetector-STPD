import AppKit
import Foundation
import STPDCore
import STPDTabularIO
import UniformTypeIdentifiers

enum ManualISIResultExportFormat: String, CaseIterable {
    case csv
    case xlsx
    case nex

    var fileExtension: String {
        switch self {
        case .csv: "zip"
        case .xlsx: "xlsx"
        case .nex: "nex"
        }
    }

    func suggestedFileName(stem: String) -> String {
        switch self {
        case .csv: "\(stem)_csv.zip"
        case .xlsx: "\(stem).xlsx"
        case .nex: "\(stem).nex"
        }
    }

    var menuTitle: String {
        switch self {
        case .csv: "CSV — 每条 spike train 一个文件（ZIP）"
        case .xlsx: "XLSX — 每条 spike train 一个工作表"
        case .nex: "NEX — NeuroExplorer spike 与标记"
        }
    }

    var contentType: UTType {
        switch self {
        case .csv:
            UTType(filenameExtension: "zip", conformingTo: .archive) ?? .data
        case .xlsx:
            UTType(filenameExtension: "xlsx") ?? .data
        case .nex:
            UTType(filenameExtension: "nex", conformingTo: .data) ?? .data
        }
    }
}

struct ManualISIResultExportContent {
    let table: ManualISIExportTable
    let nexTrains: [ManualISINEXTrain]
}

@MainActor
enum ManualISIResultExportPanel {
    static func save(
        content: ManualISIResultExportContent,
        suggestedStem: String,
        message: String
    ) throws -> URL? {
        let panel = NSSavePanel()
        panel.canCreateDirectories = true
        panel.isExtensionHidden = false
        panel.message = message
        let accessory = FormatAccessory(panel: panel, suggestedStem: suggestedStem)
        panel.accessoryView = accessory.view
        accessory.applySelection()

        guard panel.runModal() == .OK, let url = panel.url else { return nil }
        let format = accessory.selectedFormat
        let destination = url.deletingPathExtension()
            .appendingPathExtension(format.fileExtension)
        let data: Data
        switch format {
        case .csv:
            data = try ManualISIResultCSVBundleExporter.data(table: content.table)
        case .xlsx:
            data = try ManualISIResultXLSXExporter.data(table: content.table)
        case .nex:
            data = try NeuroExplorerNEXCodec.writeManualISIResult(
                trains: content.nexTrains
            )
        }
        try data.write(to: destination, options: .atomic)
        return destination
    }
}

@MainActor
private final class FormatAccessory: NSObject {
    let view: NSView
    private let popUp = NSPopUpButton(frame: .zero, pullsDown: false)
    private weak var panel: NSSavePanel?
    private let suggestedStem: String

    var selectedFormat: ManualISIResultExportFormat {
        let index = Swift.max(0, popUp.indexOfSelectedItem)
        return ManualISIResultExportFormat.allCases[
            Swift.min(index, ManualISIResultExportFormat.allCases.count - 1)
        ]
    }

    init(panel: NSSavePanel, suggestedStem: String) {
        self.panel = panel
        self.suggestedStem = suggestedStem
        let label = NSTextField(labelWithString: "导出格式")
        label.font = .systemFont(ofSize: NSFont.systemFontSize, weight: .medium)
        popUp.addItems(withTitles: ManualISIResultExportFormat.allCases.map(\.menuTitle))
        let stack = NSStackView(views: [label, popUp])
        stack.orientation = .horizontal
        stack.alignment = .centerY
        stack.spacing = 10
        stack.edgeInsets = NSEdgeInsets(top: 8, left: 0, bottom: 8, right: 0)
        view = stack
        super.init()
        popUp.target = self
        popUp.action = #selector(selectionChanged)
    }

    @objc private func selectionChanged() { applySelection() }

    func applySelection() {
        guard let panel else { return }
        let format = selectedFormat
        panel.allowedContentTypes = [format.contentType]
        panel.nameFieldStringValue = format.suggestedFileName(stem: suggestedStem)
    }
}
