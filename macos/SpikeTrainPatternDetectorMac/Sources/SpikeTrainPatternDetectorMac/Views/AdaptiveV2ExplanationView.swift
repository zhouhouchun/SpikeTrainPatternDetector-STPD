import SwiftUI
import STPDCore

/// P7A — display-only SwiftUI block that explains an Adaptive-v2 burst-canonicalization decision in plain language.
/// It is a pure function of a candidate's `decisionPath` (parsed by the STPDCore `AdaptiveV2CanonicalizationExplanation`
/// helper) and renders NOTHING when there is no Adaptive-v2 marker, so callers can drop it in unconditionally. The same
/// view is reused by the focused Inspector, the pinned-ISI inspector, and the raster hover card so the wording and tone
/// stay consistent everywhere. Changes nothing about detection — it only surfaces markers the detector already wrote.
struct AdaptiveV2ExplanationView: View {
    @Environment(\.l10n) private var l10n
    let explanation: AdaptiveV2CanonicalizationExplanation
    /// Compact = a single-line footer (hover card / inline). Non-compact = a titled card with its own material background.
    var compact: Bool = false

    init(decisionPath: String, compact: Bool = false) {
        self.explanation = AdaptiveV2CanonicalizationExplanation.parse(decisionPath: decisionPath)
        self.compact = compact
    }

    init(explanation: AdaptiveV2CanonicalizationExplanation, compact: Bool = false) {
        self.explanation = explanation
        self.compact = compact
    }

    private var tint: Color {
        switch explanation.tone {
        case .info: return STPDAppTheme.accent
        case .review: return .orange
        case .warning: return .orange
        }
    }

    private var icon: String {
        switch explanation.kind {
        case .boundaryTrim: return "scissors"
        case .strongCoreRescue: return "checkmark.seal"   // P11A: kept canonical on strong-core evidence
        case .demotedToPossible: return explanation.tone == .warning ? "questionmark.circle" : "arrow.down.circle"
        case .none: return "info.circle"
        }
    }

    var body: some View {
        if explanation.isPresent {
            if compact {
                HStack(alignment: .firstTextBaseline, spacing: 6) {
                    Image(systemName: icon)
                        .font(.caption2)
                        .foregroundStyle(tint)
                    Text(explanation.shortTitle(language: l10n.language))
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }
            } else {
                VStack(alignment: .leading, spacing: 6) {
                    Text("Adaptive v2")
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                    HStack(alignment: .firstTextBaseline, spacing: 6) {
                        Image(systemName: icon)
                            .font(.caption)
                            .foregroundStyle(tint)
                        Text(explanation.shortTitle(language: l10n.language))
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(tint)
                    }
                    Text(explanation.plainLanguageSummary(language: l10n.language))
                        .font(.caption)
                        .foregroundStyle(.primary)
                        .fixedSize(horizontal: false, vertical: true)
                        .textSelection(.enabled)
                    if let ceilingLabel = explanation.ceilingLabel(language: l10n.language) {
                        Text(ceilingLabel)
                            .font(.caption2)
                            .foregroundStyle(.secondary)
                    }
                }
                .padding(12)
                .frame(maxWidth: .infinity, alignment: .leading)
                .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
            }
        }
    }
}
