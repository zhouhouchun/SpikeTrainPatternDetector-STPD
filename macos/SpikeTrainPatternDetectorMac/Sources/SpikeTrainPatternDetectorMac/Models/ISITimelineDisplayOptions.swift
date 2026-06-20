import Foundation

enum ISITimelineLayoutMode: String, CaseIterable, Identifiable {
    case separateAxes
    case overlay

    var id: String { rawValue }

    var title: String {
        switch self {
        case .separateAxes:
            return "Separate axes"
        case .overlay:
            return "Overlay"
        }
    }
}

enum ISIYAxisScale: String, CaseIterable, Identifiable {
    case linear
    case log

    var id: String { rawValue }

    var title: String {
        switch self {
        case .linear:
            return "Linear"
        case .log:
            return "Log"
        }
    }
}
