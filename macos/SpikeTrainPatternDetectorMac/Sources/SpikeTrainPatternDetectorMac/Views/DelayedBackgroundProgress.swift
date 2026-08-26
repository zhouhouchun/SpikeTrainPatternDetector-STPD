import SwiftUI

/// Displays an indeterminate linear progress bar only when one operation remains active for at
/// least five seconds. Short operations therefore do not flash transient chrome, while genuinely
/// long background work always explains what the app is doing.
struct DelayedBackgroundProgress: View {
    let operationMessage: String?
    var delayNanoseconds: UInt64 = 5_000_000_000

    @State private var visibleMessage: String?

    var body: some View {
        Group {
            if let visibleMessage {
                VStack(alignment: .leading, spacing: 7) {
                    Text(visibleMessage)
                        .font(.callout.weight(.semibold))
                        .lineLimit(2)
                    ProgressView()
                        .progressViewStyle(.linear)
                        .frame(minWidth: 260, idealWidth: 340, maxWidth: 420)
                }
                .padding(.horizontal, 14)
                .padding(.vertical, 11)
                .background(.regularMaterial, in: RoundedRectangle(cornerRadius: 12))
                .overlay {
                    RoundedRectangle(cornerRadius: 12)
                        .stroke(Color.primary.opacity(0.10), lineWidth: 0.8)
                }
                .shadow(color: .black.opacity(0.10), radius: 12, y: 4)
                .accessibilityElement(children: .combine)
                .accessibilityAddTraits(.updatesFrequently)
            }
        }
        .task(id: operationMessage) {
            visibleMessage = nil
            guard let operationMessage else { return }
            do {
                try await Task.sleep(nanoseconds: delayNanoseconds)
            } catch {
                return
            }
            guard !Task.isCancelled else { return }
            visibleMessage = operationMessage
        }
        .onChange(of: operationMessage) { _, message in
            if message == nil { visibleMessage = nil }
        }
    }
}

extension View {
    func delayedBackgroundProgress(
        _ operationMessage: String?,
        alignment: Alignment = .top
    ) -> some View {
        overlay(alignment: alignment) {
            DelayedBackgroundProgress(operationMessage: operationMessage)
                .padding(14)
                .allowsHitTesting(false)
                .zIndex(1_000)
        }
    }
}
