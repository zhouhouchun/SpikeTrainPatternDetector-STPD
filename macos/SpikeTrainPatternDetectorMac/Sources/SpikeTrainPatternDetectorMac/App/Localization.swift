import STPDCore
import SwiftUI

/// A lightweight localizer injected through the SwiftUI environment. Views call `l10n.t("中文")` (or
/// `l10n("中文")`). Switching `language` re-renders every view that reads `\.l10n` without touching the
/// document, detection results, manual annotations, review state, neural-manifold state, or raster state —
/// those live in separate observable stores and are never recreated by a language change.
struct STPDLocalizer: Equatable {
    var language: STPDLanguage = .zh

    func t(_ source: String) -> String { STPDLocalization.text(source, language: language) }
    func callAsFunction(_ source: String) -> String { t(source) }
}
private struct STPDLocalizerKey: EnvironmentKey {
    static let defaultValue = STPDLocalizer(language: .zh)
}

extension EnvironmentValues {
    /// The current localizer. Inject once at the SwiftUI root with `.environment(\.l10n, STPDLocalizer(...))`.
    var l10n: STPDLocalizer {
        get { self[STPDLocalizerKey.self] }
        set { self[STPDLocalizerKey.self] = newValue }
    }
}

/// `@AppStorage` key for the persisted UI language. Persisted via `UserDefaults`, so the choice survives an
/// app relaunch. Mirrors the R/Shiny `localStorage.getItem('stpd_ui_language')` key name.
enum AppLanguageStorage {
    static let key = "stpd_ui_language"

    static func current() -> STPDLanguage {
        STPDLanguage(rawValue: UserDefaults.standard.string(forKey: key) ?? "") ?? .zh
    }
}

extension SpikePatternState {
    /// Chinese source string for the pattern legend / display, routed through `STPDLocalization` so the
    /// English form comes from the shared dictionary (never a separate hard-coded English label).
    var localizationSourceZH: String {
        switch self {
        case .burst: return "爆发"
        case .pause: return "暂停"
        case .tonic: return "强直发放"
        case .hfTonic: return "高频强直发放"
        case .hfs: return "高频连续发放"
        case .unlabeled: return "未标注"
        }
    }
}
