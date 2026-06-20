import STPDCore
import SwiftUI

struct SpikeTrainCountControls: View {
    @Bindable var document: RasterDocument
    let dataset: SpikeDataset
    let scope: SpikeTrainSelectionScope
    var showsTitle = false
    var titleWidth: CGFloat?

    @State private var isTrainSelectorPresented = false

    var body: some View {
        HStack(spacing: 8) {
            if showsTitle {
                Text(scope.title)
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .fixedSize(horizontal: true, vertical: false)
                    .frame(width: titleWidth, alignment: .leading)
            }

            Text("显示")
                .font(.caption)
                .foregroundStyle(.secondary)

            DebouncedIntField(
                "N",
                value: visibleTrainCountBinding,
                range: 1...max(dataset.trains.count, 1),
                width: 52
            )

            Text("条")
                .font(.caption)
                .foregroundStyle(.secondary)

            Button("全部") {
                document.selectVisibleTrainCount(dataset.trains.count, scope: scope)
            }
            .font(.caption)
            .liquidGlassButtonStyle()

            Button {
                isTrainSelectorPresented = true
            } label: {
                ViewThatFits(in: .horizontal) {
                    Label("选择", systemImage: "checklist")
                    Image(systemName: "checklist")
                }
            }
            .font(.caption)
            .liquidGlassButtonStyle()
            .help("打开详细 spike train 选择器。")
            .popover(isPresented: $isTrainSelectorPresented, arrowEdge: .trailing) {
                SpikeTrainSelectorPopover(document: document, dataset: dataset, scope: scope)
            }
        }
        .disabled(dataset.trains.isEmpty)
        .help(scope.help)
    }

    private var visibleTrainCountBinding: Binding<Int> {
        Binding(
            get: {
                min(max(document.requestedVisibleTrainCount(for: scope), 1), max(dataset.trains.count, 1))
            },
            set: { count in
                document.selectVisibleTrainCount(count, scope: scope)
            }
        )
    }
}
