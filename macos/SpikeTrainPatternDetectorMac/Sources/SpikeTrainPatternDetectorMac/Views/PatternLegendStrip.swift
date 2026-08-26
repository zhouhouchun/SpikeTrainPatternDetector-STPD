import STPDCore
import SwiftUI

struct PatternLegendEntry: Hashable, Identifiable {
    let id: String
    /// A stable semantic key used to avoid duplicate chips when automatic and manual evidence
    /// describe the same displayed pattern.
    let key: String
    let name: String
    let color: Color

    static func visibleEntries(
        annotations: [ClassicAnchorEventAnnotation],
        manualAnnotations: [ManualAnnotation],
        trains: [SpikeTrain],
        trainIDs: Set<String>,
        timeRange: RasterTimeRange,
        timeMode: RasterTimeMode
    ) -> [PatternLegendEntry] {
        guard !trainIDs.isEmpty else {
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

        var entries = ClassicAnchorLabel.allCases
            .filter { label in
                label != .reject &&
                    label != .profile &&
                    visibleLabels.contains(label)
            }
            .map { label in
                PatternLegendEntry(
                    id: "automatic-\(label.rawValue)",
                    key: automaticLegendKey(for: label),
                    name: displayName(for: label),
                    color: color(for: label)
                )
            }

        let trainByID = Dictionary(uniqueKeysWithValues: trains.map { ($0.id, $0) })
        var manualLabels = Set<ManualAnnotationLabel>()
        for annotation in manualAnnotations where trainIDs.contains(annotation.trainID) {
            guard annotation.label.polarity == .positive,
                  let train = trainByID[annotation.trainID] else { continue }
            let range: ClosedRange<Double>
            switch timeMode {
            case .raw:
                range = annotation.normalizedStartSec...annotation.normalizedEndSec
            case .aligned:
                guard let first = train.firstTimestampSec else { continue }
                range = (annotation.normalizedStartSec - first)...(annotation.normalizedEndSec - first)
            }
            guard range.upperBound >= timeRange.lowerBound,
                  range.lowerBound <= timeRange.upperBound else { continue }
            manualLabels.insert(annotation.label)
        }

        for label in ManualAnnotationLabel.allCases where manualLabels.contains(label) {
            let key = manualLegendKey(for: label)
            guard !entries.contains(where: { $0.key == key }) else { continue }
            entries.append(
                PatternLegendEntry(
                    id: "manual-\(label.rawValue)",
                    key: key,
                    name: label.displayName,
                    color: manualColor(for: label)
                )
            )
        }
        return entries
    }

    /// Timeline consumers do not own manual annotation geometry. Retain their automatic-only
    /// projection while the timestamp raster supplies the richer manual-aware overload above.
    static func visibleEntries(
        annotations: [ClassicAnchorEventAnnotation],
        trainIDs: Set<String>,
        timeRange: RasterTimeRange,
        timeMode: RasterTimeMode
    ) -> [PatternLegendEntry] {
        visibleEntries(
            annotations: annotations,
            manualAnnotations: [],
            trains: [],
            trainIDs: trainIDs,
            timeRange: timeRange,
            timeMode: timeMode
        )
    }

    private static func automaticLegendKey(for label: ClassicAnchorLabel) -> String {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst: "burst"
        case .tonic: "tonic"
        case .highFrequencyTonic: "high_frequency_tonic"
        case .highFrequencySpiking: "high_frequency_spiking"
        case .pause: "pause"
        case .reject: "reject"
        case .profile: "profile"
        }
    }

    private static func manualLegendKey(for label: ManualAnnotationLabel) -> String {
        switch label {
        case .burst: "burst"
        default: label.rawValue
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

    private static func manualColor(for label: ManualAnnotationLabel) -> Color {
        switch label {
        case .burst: .orange
        case .highFrequencyBurst: .red
        case .longBurst: .pink
        case .tonic: .green
        case .highFrequencyTonic: .teal
        case .highFrequencySpiking: .purple
        case .pause: .blue
        case .other: .gray
        case .notBurst: .secondary
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
                    ForEach(entries) { entry in
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
                    ForEach(entries) { entry in
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
