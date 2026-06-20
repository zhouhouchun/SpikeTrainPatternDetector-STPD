import STPDCore
import SwiftUI

struct PatternLegendEntry: Hashable {
    let label: ClassicAnchorLabel
    let name: String
    let color: Color

    static func visibleEntries(
        annotations: [ClassicAnchorEventAnnotation],
        trainIDs: Set<String>,
        timeRange: RasterTimeRange,
        timeMode: RasterTimeMode
    ) -> [PatternLegendEntry] {
        guard !annotations.isEmpty, !trainIDs.isEmpty else {
            return []
        }

        var visibleLabels = Set<ClassicAnchorLabel>()
        for annotation in annotations where trainIDs.contains(annotation.trainID) {
            let start = min(timeStart(annotation, mode: timeMode), timeEnd(annotation, mode: timeMode))
            let end = max(timeStart(annotation, mode: timeMode), timeEnd(annotation, mode: timeMode))
            guard end >= timeRange.lowerBound,
                  start <= timeRange.upperBound else {
                continue
            }
            visibleLabels.insert(annotation.visualLabel)
        }

        return ClassicAnchorLabel.allCases
            .filter { label in
                label != .reject &&
                    label != .profile &&
                    visibleLabels.contains(label)
            }
            .map { label in
                PatternLegendEntry(
                    label: label,
                    name: displayName(for: label),
                    color: color(for: label)
                )
            }
    }

    static func displayName(for label: ClassicAnchorLabel) -> String {
        switch label {
        case .burst:
            return "Burst"
        case .highFrequencyBurst:
            return "Burst"
        case .longBurst:
            return "Burst"
        case .possibleBurst:
            return "Burst"
        case .tonic:
            return "Tonic"
        case .highFrequencyTonic:
            return "HF tonic"
        case .highFrequencySpiking:
            return "HFS"
        case .pause:
            return "Pause"
        case .reject:
            return "Reject"
        case .profile:
            return "Profile"
        }
    }

    static func color(for label: ClassicAnchorLabel) -> Color {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst:
            return color(hex: 0xD55E00)
        case .tonic:
            return color(hex: 0x009E73)
        case .highFrequencyTonic:
            return color(hex: 0x35B779)
        case .highFrequencySpiking:
            return color(hex: 0xCC79A7)
        case .pause:
            return color(hex: 0x0072B2)
        case .reject, .profile:
            return .secondary
        }
    }

    private static func timeStart(_ annotation: ClassicAnchorEventAnnotation, mode: RasterTimeMode) -> Double {
        switch mode {
        case .aligned:
            return annotation.alignedStartSec
        case .raw:
            return annotation.rawStartSec
        }
    }

    private static func timeEnd(_ annotation: ClassicAnchorEventAnnotation, mode: RasterTimeMode) -> Double {
        switch mode {
        case .aligned:
            return annotation.alignedEndSec
        case .raw:
            return annotation.rawEndSec
        }
    }

    private static func color(hex: UInt32) -> Color {
        Color(
            red: Double((hex >> 16) & 0xFF) / 255.0,
            green: Double((hex >> 8) & 0xFF) / 255.0,
            blue: Double(hex & 0xFF) / 255.0
        )
    }
}

struct PatternLegendStrip: View {
    let entries: [PatternLegendEntry]

    var body: some View {
        if !entries.isEmpty {
            ViewThatFits(in: .horizontal) {
                HStack(spacing: 8) {
                    ForEach(entries, id: \.label) { entry in
                        chip(entry)
                    }
                }

                LazyVGrid(
                    columns: [
                        GridItem(.flexible(minimum: 68), spacing: 6, alignment: .trailing),
                        GridItem(.flexible(minimum: 68), spacing: 6, alignment: .trailing)
                    ],
                    alignment: .trailing,
                    spacing: 4
                ) {
                    ForEach(entries, id: \.label) { entry in
                        chip(entry)
                    }
                }
            }
            .font(.caption2.weight(.semibold))
            .padding(.horizontal, 7)
            .padding(.vertical, 5)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8, style: .continuous))
            .overlay {
                RoundedRectangle(cornerRadius: 8, style: .continuous)
                    .stroke(Color(nsColor: .separatorColor).opacity(0.32), lineWidth: 1)
            }
            .frame(maxWidth: .infinity, alignment: .trailing)
        }
    }

    private func chip(_ entry: PatternLegendEntry) -> some View {
        HStack(spacing: 5) {
            Capsule()
                .fill(entry.color.opacity(0.86))
                .frame(width: 18, height: 4)
            Text(entry.name)
                .foregroundStyle(.secondary)
                .lineLimit(1)
        }
        .fixedSize(horizontal: true, vertical: false)
    }
}
