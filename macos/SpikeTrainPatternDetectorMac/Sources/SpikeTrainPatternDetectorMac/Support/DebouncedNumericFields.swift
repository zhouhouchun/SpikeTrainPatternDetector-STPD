import SwiftUI

struct DebouncedDoubleField: View {
    let placeholder: String
    @Binding var value: Double
    var width: CGFloat
    var maxFractionDigits = 6

    @FocusState private var isFocused: Bool
    @State private var text: String
    @State private var pendingCommit: Task<Void, Never>?

    init(_ placeholder: String, value: Binding<Double>, width: CGFloat, maxFractionDigits: Int = 6) {
        self.placeholder = placeholder
        _value = value
        self.width = width
        self.maxFractionDigits = maxFractionDigits
        _text = State(initialValue: Self.format(value.wrappedValue, maxFractionDigits: maxFractionDigits))
    }

    var body: some View {
        TextField(placeholder, text: $text)
            .textFieldStyle(.roundedBorder)
            .monospacedDigit()
            .frame(width: width)
            .focused($isFocused)
            .onSubmit {
                commit(normalizeText: true)
            }
            .onChange(of: isFocused) { _, focused in
                if focused {
                    return
                }
                commit(normalizeText: true)
            }
            .onChange(of: text) { _, newText in
                guard newText != Self.format(value, maxFractionDigits: maxFractionDigits) else {
                    return
                }
                scheduleCommit()
            }
            .onChange(of: value) { _, newValue in
                guard shouldSyncExternalValue(newValue) else {
                    return
                }
                pendingCommit?.cancel()
                text = Self.format(newValue, maxFractionDigits: maxFractionDigits)
            }
            .onDisappear {
                pendingCommit?.cancel()
            }
    }

    private func scheduleCommit() {
        pendingCommit?.cancel()
        pendingCommit = Task {
            try? await Task.sleep(nanoseconds: 1_000_000_000)
            guard !Task.isCancelled else {
                return
            }
            await MainActor.run {
                commit(normalizeText: false)
            }
        }
    }

    private func commit(normalizeText: Bool) {
        pendingCommit?.cancel()
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard let parsed = Double(trimmed.replacingOccurrences(of: ",", with: ".")) else {
            if normalizeText {
                text = Self.format(value, maxFractionDigits: maxFractionDigits)
            }
            return
        }

        value = parsed
        if normalizeText {
            text = Self.format(value, maxFractionDigits: maxFractionDigits)
        }
    }

    private static func format(_ value: Double, maxFractionDigits: Int) -> String {
        guard value.isFinite else {
            return ""
        }

        let format = "%.\(maxFractionDigits)f"
        var text = String(format: format, value)
        while text.contains(".") && text.last == "0" {
            text.removeLast()
        }
        if text.last == "." {
            text.removeLast()
        }
        return text
    }

    private func shouldSyncExternalValue(_ newValue: Double) -> Bool {
        if !isFocused {
            return true
        }

        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard let parsed = Double(trimmed.replacingOccurrences(of: ",", with: ".")), parsed.isFinite else {
            return true
        }

        return abs(parsed - newValue) > max(1e-12, abs(newValue) * 1e-9)
    }
}

struct DebouncedIntField: View {
    let placeholder: String
    @Binding var value: Int
    var range: ClosedRange<Int>
    var width: CGFloat

    @FocusState private var isFocused: Bool
    @State private var text: String
    @State private var pendingCommit: Task<Void, Never>?

    init(_ placeholder: String, value: Binding<Int>, range: ClosedRange<Int>, width: CGFloat) {
        self.placeholder = placeholder
        _value = value
        self.range = range
        self.width = width
        _text = State(initialValue: "\(value.wrappedValue)")
    }

    var body: some View {
        TextField(placeholder, text: $text)
            .textFieldStyle(.roundedBorder)
            .monospacedDigit()
            .frame(width: width)
            .focused($isFocused)
            .onSubmit {
                commit(normalizeText: true)
            }
            .onChange(of: isFocused) { _, focused in
                if focused {
                    return
                }
                commit(normalizeText: true)
            }
            .onChange(of: text) { _, newText in
                guard newText != "\(value)" else {
                    return
                }
                scheduleCommit()
            }
            .onChange(of: value) { _, newValue in
                guard shouldSyncExternalValue(newValue) else {
                    return
                }
                pendingCommit?.cancel()
                text = "\(newValue)"
            }
            .onDisappear {
                pendingCommit?.cancel()
            }
    }

    private func scheduleCommit() {
        pendingCommit?.cancel()
        pendingCommit = Task {
            try? await Task.sleep(nanoseconds: 1_000_000_000)
            guard !Task.isCancelled else {
                return
            }
            await MainActor.run {
                commit(normalizeText: false)
            }
        }
    }

    private func commit(normalizeText: Bool) {
        pendingCommit?.cancel()
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard let parsed = Int(trimmed) else {
            if normalizeText {
                text = "\(value)"
            }
            return
        }

        value = min(max(parsed, range.lowerBound), range.upperBound)
        if normalizeText {
            text = "\(value)"
        }
    }

    private func shouldSyncExternalValue(_ newValue: Int) -> Bool {
        if !isFocused {
            return true
        }

        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        return Int(trimmed) != newValue
    }
}
