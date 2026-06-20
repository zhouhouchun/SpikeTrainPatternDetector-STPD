import STPDCore
import SwiftUI

struct SpikeTrainSelectorPopover: View {
    @Bindable var document: RasterDocument
    let dataset: SpikeDataset
    let scope: SpikeTrainSelectionScope

    @State private var searchText = ""

    private var filteredTrains: [SpikeTrain] {
        let query = searchText.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !query.isEmpty else {
            return dataset.trains
        }
        return dataset.trains.filter { $0.name.localizedCaseInsensitiveContains(query) }
    }

    private var selection: Set<String> {
        document.selectedTrainIDs(for: scope)
    }

    var body: some View {
        VStack(spacing: 0) {
            header
                .padding(14)

            Divider()

            searchBar
                .padding(.horizontal, 14)
                .padding(.vertical, 10)

            Divider()

            ScrollView {
                if filteredTrains.isEmpty {
                    ContentUnavailableView(
                        "没有匹配的 spike train",
                        systemImage: "magnifyingglass",
                        description: Text("请换一个关键词，或清空搜索内容。")
                    )
                    .padding(.top, 42)
                } else {
                    LazyVStack(spacing: 0) {
                        ForEach(filteredTrains) { train in
                            trainRow(train)
                            Divider()
                        }
                    }
                }
            }
        }
        .frame(minWidth: 640, idealWidth: 700, maxWidth: 760, minHeight: 360, idealHeight: 430, maxHeight: 540)
    }

    private var header: some View {
        HStack(alignment: .center, spacing: 10) {
            VStack(alignment: .leading, spacing: 2) {
                Text(scope.selectorTitle)
                    .font(.headline)
                Text("已选择 \(selection.count) / \(dataset.trains.count)")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .monospacedDigit()
            }

            Spacer()

            Button {
                document.selectVisibleTrainCount(dataset.trains.count, scope: scope)
            } label: {
                Label("全选", systemImage: "checkmark.circle")
            }
            .liquidGlassButtonStyle()

            Button {
                document.updateSelectedTrainIDs([], scope: scope)
            } label: {
                Label("清空", systemImage: "xmark.circle")
            }
            .liquidGlassButtonStyle()
        }
    }

    private var searchBar: some View {
        HStack(spacing: 8) {
            Image(systemName: "magnifyingglass")
                .foregroundStyle(.secondary)
            TextField("搜索 spike train", text: $searchText)
                .textFieldStyle(.roundedBorder)
        }
    }

    private func trainRow(_ train: SpikeTrain) -> some View {
        Button {
            toggleSelection(for: train)
        } label: {
            HStack(alignment: .top, spacing: 10) {
                Image(systemName: selection.contains(train.id) ? "checkmark.square.fill" : "square")
                    .font(.title3)
                    .foregroundStyle(selection.contains(train.id) ? Color.accentColor : Color.secondary.opacity(0.5))
                    .frame(width: 22)
                    .padding(.top, 1)

                VStack(alignment: .leading, spacing: 5) {
                    Text(train.name)
                        .font(.subheadline.weight(.semibold))
                        .lineLimit(1)
                        .truncationMode(.middle)

                    HStack(spacing: 12) {
                        trainMetric("Spike 数", "\(train.spikeCount)")
                        trainMetric("时长", TimeFormatting.seconds(train.rawDurationSec))
                        trainMetric("起点", timeOrNA(train.firstTimestampSec))
                        trainMetric("终点", timeOrNA(train.lastTimestampSec))
                        trainMetric("发放率", firingRate(train))
                    }
                }

                Spacer(minLength: 0)
            }
            .contentShape(Rectangle())
            .padding(.horizontal, 14)
            .padding(.vertical, 8)
        }
        .buttonStyle(.plain)
        .background(selection.contains(train.id) ? Color.accentColor.opacity(0.08) : Color.clear)
    }

    private func trainMetric(_ title: String, _ value: String) -> some View {
        VStack(alignment: .leading, spacing: 1) {
            Text(title)
                .font(.caption2)
                .foregroundStyle(.secondary)
            Text(value)
                .font(.caption)
                .monospacedDigit()
        }
        .frame(minWidth: 72, alignment: .leading)
    }

    private func toggleSelection(for train: SpikeTrain) {
        var nextSelection = selection
        if nextSelection.contains(train.id) {
            nextSelection.remove(train.id)
        } else {
            nextSelection.insert(train.id)
        }
        document.updateSelectedTrainIDs(nextSelection, scope: scope)
    }

    private func timeOrNA(_ value: Double?) -> String {
        guard let value else {
            return "NA"
        }
        return TimeFormatting.seconds(value)
    }

    private func firingRate(_ train: SpikeTrain) -> String {
        let duration = train.rawDurationSec
        guard duration > 0 else {
            return "NA"
        }
        return String(format: "%.2f Hz", Double(train.spikeCount) / duration)
    }
}
