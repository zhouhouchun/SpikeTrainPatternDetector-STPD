import SwiftUI

extension View {
    @ViewBuilder
    func liquidGlassButtonStyle(prominent: Bool = false) -> some View {
        if #available(macOS 26.0, *) {
            if prominent {
                self.buttonStyle(.glassProminent)
            } else {
                self.buttonStyle(.glass)
            }
        } else {
            self.buttonStyle(LegacyLiquidGlassButtonStyle(prominent: prominent))
        }
    }

    func liquidGlassToolbarButtonStyle() -> some View {
        buttonStyle(PressOnlyToolbarGlassButtonStyle())
    }
}

struct LegacyLiquidGlassButtonStyle: ButtonStyle {
    let prominent: Bool
    @Environment(\.isEnabled) private var isEnabled

    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .padding(.horizontal, 10)
            .padding(.vertical, 6)
            .foregroundStyle(foregroundStyle)
            .background {
                RoundedRectangle(cornerRadius: 9, style: .continuous)
                    .fill(backgroundFill(isPressed: configuration.isPressed))
                    .overlay {
                        RoundedRectangle(cornerRadius: 9, style: .continuous)
                            .stroke(borderColor, lineWidth: 1)
                    }
                    .shadow(color: .black.opacity(prominent ? 0.10 : 0.07), radius: 10, y: 4)
            }
            .scaleEffect(configuration.isPressed ? 0.985 : 1)
            .opacity(isEnabled ? 1 : 0.45)
            .animation(.snappy(duration: 0.16), value: configuration.isPressed)
    }

    private var foregroundStyle: Color {
        prominent ? .white : .primary
    }

    private var borderColor: Color {
        prominent ? Color.accentColor.opacity(0.28) : Color(nsColor: .separatorColor).opacity(0.35)
    }

    private func backgroundFill(isPressed: Bool) -> Color {
        if prominent {
            return Color.accentColor.opacity(isPressed ? 0.82 : 0.94)
        }
        return Color(nsColor: .textBackgroundColor).opacity(isPressed ? 0.78 : 0.92)
    }
}

struct PressOnlyToolbarGlassButtonStyle: ButtonStyle {
    @Environment(\.isEnabled) private var isEnabled

    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(.system(size: 15, weight: .medium))
            .foregroundStyle(.primary)
            .frame(width: 34, height: 30)
            .background {
                RoundedRectangle(cornerRadius: 11, style: .continuous)
                    .fill(Color(nsColor: .controlBackgroundColor).opacity(configuration.isPressed ? 0.82 : 0))
                    .overlay {
                        RoundedRectangle(cornerRadius: 11, style: .continuous)
                            .stroke(Color(nsColor: .separatorColor).opacity(configuration.isPressed ? 0.34 : 0), lineWidth: 1)
                    }
            }
            .scaleEffect(configuration.isPressed ? 0.94 : 1)
            .opacity(isEnabled ? 1 : 0.38)
            .animation(.snappy(duration: 0.14), value: configuration.isPressed)
    }
}

struct GlassSegmentedControl<Value: Hashable>: View {
    let options: [(value: Value, title: String)]
    @Binding var selection: Value
    var minSegmentWidth: CGFloat = 48
    @Namespace private var selectionNamespace

    var body: some View {
        HStack(spacing: 3) {
            ForEach(Array(options.enumerated()), id: \.offset) { _, option in
                segment(option)
            }
        }
        .padding(3)
        .background {
            RoundedRectangle(cornerRadius: 11, style: .continuous)
                .fill(Color(nsColor: .textBackgroundColor).opacity(0.88))
                .overlay {
                    RoundedRectangle(cornerRadius: 11, style: .continuous)
                        .stroke(Color(nsColor: .separatorColor).opacity(0.34), lineWidth: 1)
                }
                .shadow(color: .black.opacity(0.06), radius: 11, y: 4)
        }
        .animation(.snappy(duration: 0.22), value: selection)
    }

    private func segment(_ option: (value: Value, title: String)) -> some View {
        let isSelected = selection == option.value

        return Button {
            withAnimation(.snappy(duration: 0.22)) {
                selection = option.value
            }
        } label: {
            Text(option.title)
                .font(.callout.weight(isSelected ? .semibold : .medium))
                .foregroundStyle(isSelected ? Color.white : Color.primary)
                .lineLimit(1)
                .frame(minWidth: minSegmentWidth)
                .padding(.horizontal, 9)
                .padding(.vertical, 5)
                .contentShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
                .background {
                    if isSelected {
                        RoundedRectangle(cornerRadius: 8, style: .continuous)
                            .fill(Color.accentColor)
                            .matchedGeometryEffect(id: "selected-segment", in: selectionNamespace)
                            .overlay(alignment: .top) {
                                RoundedRectangle(cornerRadius: 8, style: .continuous)
                                    .stroke(Color.white.opacity(0.28), lineWidth: 1)
                            }
                            .shadow(color: Color.accentColor.opacity(0.25), radius: 8, y: 2)
                    }
                }
        }
        .buttonStyle(.plain)
        .accessibilityLabel(option.title)
    }
}
