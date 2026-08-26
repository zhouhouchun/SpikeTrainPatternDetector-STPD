import STPDCore
import SwiftUI

/// Shared pattern-state color palette for the visualization/annotation overlays (ISI State Space + Neural
/// Manifold). Nature-ish hues; `.unlabeled` is a neutral gray. Display only — never a PCA / detection input.
enum SpikePatternColor {
    static func color(_ state: SpikePatternState) -> Color {
        switch state {
        case .burst: return Color(red: 0xC6 / 255, green: 0x5A / 255, blue: 0x9B / 255)
        case .pause: return Color(red: 0x46 / 255, green: 0x8F / 255, blue: 0xD6 / 255)
        case .tonic: return Color(red: 0x3A / 255, green: 0x9E / 255, blue: 0x65 / 255)
        case .hfTonic: return Color(red: 0xE0 / 255, green: 0x9B / 255, blue: 0x3C / 255)
        case .hfs: return Color(red: 0xEC / 255, green: 0x69 / 255, blue: 0x65 / 255)
        case .unlabeled: return Color(red: 0.60, green: 0.63, blue: 0.67)
        }
    }

    /// States shown in a legend, in display order.
    static let legendStates: [SpikePatternState] = [.burst, .pause, .tonic, .hfTonic, .hfs, .unlabeled]
}
