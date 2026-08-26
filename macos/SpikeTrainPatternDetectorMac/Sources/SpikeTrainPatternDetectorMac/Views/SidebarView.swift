import STPDCore
import SwiftUI

struct SidebarView: View {
    @Bindable var document: RasterDocument
    @Binding var selectedSection: WorkbenchSection?
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @Environment(\.l10n) private var l10n
    @State private var hoveredSection: WorkbenchSection?
    @State private var dragOriginSection: WorkbenchSection?
    @State private var dragPreviewSection: WorkbenchSection?
    @State private var pendingHoverTarget: WorkbenchSection?
    @State private var pendingHoverCommitTask: Task<Void, Never>?
    @State private var pendingDragTarget: WorkbenchSection?
    @State private var pendingDragCommitTask: Task<Void, Never>?
    @State private var rowFrames: [WorkbenchSection: CGRect] = [:]
    private var rowHeight: CGFloat { l10n.language == .ru ? 44 : 34 }

    private let sidebarCoordinateSpace = "stpd.sidebar.modules"

    var body: some View {
        VStack(spacing: 0) {
            moduleList
                .frame(minHeight: 340, maxHeight: .infinity)
        }
        .background(.clear)
    }

    private var moduleList: some View {
        ScrollView {
            LazyVStack(alignment: .leading, spacing: 14) {
                ForEach(visibleGroups) { entry in
                    sidebarGroup(entry)
                }
            }
            .padding(.horizontal, 8)
            .padding(.vertical, 12)
        }
        .background(.clear)
        .coordinateSpace(name: sidebarCoordinateSpace)
        .onPreferenceChange(SidebarRowFramesPreferenceKey.self) { rowFrames = $0 }
        .onHover { isInside in
            if !isInside, dragOriginSection == nil {
                cancelPendingHoverCommit()
            }
        }
    }

    private var visibleGroups: [VisibleSidebarGroup] {
        WorkbenchGroup.allCases.compactMap { group in
            let sections = WorkbenchSection.sections(in: group).filter(\.isLive)
            return sections.isEmpty ? nil : VisibleSidebarGroup(group: group, sections: sections)
        }
    }

    private func sidebarGroup(_ entry: VisibleSidebarGroup) -> some View {
        VStack(alignment: .trailing, spacing: 6) {
            Text(l10n.t(entry.group.title))
                .font(.callout.weight(.bold))
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .frame(maxWidth: .infinity, alignment: .trailing)
                .padding(.horizontal, 6)

            VStack(spacing: 2) {
                ForEach(entry.sections) { section in
                    sidebarRow(section)
                }
            }
            .padding(5)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background {
                RoundedRectangle(cornerRadius: 10, style: .continuous)
                    .fill(Color(nsColor: .controlBackgroundColor).opacity(0.62))
                    .overlay {
                        RoundedRectangle(cornerRadius: 10, style: .continuous)
                            .stroke(Color(nsColor: .separatorColor).opacity(0.34), lineWidth: 1)
                    }
            }
        }
    }

    @ViewBuilder
    private func sidebarRow(_ section: WorkbenchSection) -> some View {
        let isSelected = selectedSection == section
        let isHighlighted = highlightedSection == section

        let row = Button {
            select(section)
        } label: {
            Text(l10n.t(section.title))
                .fontWeight(isHighlighted ? .semibold : .regular)
                .foregroundStyle(Color.primary)
                .lineLimit(l10n.language == .ru ? 2 : 1)
                .minimumScaleFactor(l10n.language == .ru ? 0.88 : 0.9)
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(.horizontal, 9)
                .padding(.vertical, 6)
                .contentShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
                .background {
                    if isHighlighted {
                        SidebarLiquidGlassSelection(isBeingDragged: dragOriginSection != nil)
                    }
                }
                .frame(height: rowHeight)
        }
        .buttonStyle(.plain)
        .help(l10n.t("悬停约2秒或单击切换；也可按住当前玻璃块拖到目标功能后松开。"))
        .accessibilityAddTraits(isSelected ? .isSelected : [])
        .onHover { isInside in
            guard dragOriginSection == nil else { return }
            if isInside {
                scheduleHoverCommit(for: section)
            } else if pendingHoverTarget == section {
                cancelPendingHoverCommit()
            }
        }
        .background {
            GeometryReader { proxy in
                Color.clear.preference(
                    key: SidebarRowFramesPreferenceKey.self,
                    value: [section: proxy.frame(in: .named(sidebarCoordinateSpace))]
                )
            }
        }

        if isHighlighted || dragOriginSection == section {
            row.highPriorityGesture(selectionDragGesture(startingAt: section))
        } else {
            row
        }
    }

    private var highlightedSection: WorkbenchSection? {
        dragPreviewSection ?? hoveredSection ?? selectedSection
    }

    private func select(_ section: WorkbenchSection) {
        cancelPendingHoverCommit()
        let applySelection = {
            hoveredSection = section
            selectedSection = section
        }
        if reduceMotion {
            applySelection()
        } else {
            withAnimation(.easeInOut(duration: 1.0)) {
                applySelection()
            }
        }
    }

    private func scheduleHoverCommit(for section: WorkbenchSection) {
        guard section != hoveredSection else {
            cancelPendingHoverCommit()
            return
        }
        guard pendingHoverTarget != section else { return }

        cancelPendingHoverCommit()
        pendingHoverTarget = section
        pendingHoverCommitTask = Task { [section] in
            try? await Task.sleep(nanoseconds: 2_000_000_000)
            guard !Task.isCancelled else { return }
            await MainActor.run {
                guard
                    dragOriginSection == nil,
                    pendingHoverTarget == section
                else { return }

                pendingHoverTarget = nil
                if reduceMotion {
                    hoveredSection = section
                } else {
                    withAnimation(.easeInOut(duration: 1.0)) {
                        hoveredSection = section
                    }
                }
            }
        }
    }

    private func selectionDragGesture(startingAt section: WorkbenchSection) -> some Gesture {
        DragGesture(minimumDistance: 6, coordinateSpace: .named(sidebarCoordinateSpace))
            .onChanged { value in
                if dragOriginSection == nil {
                    dragOriginSection = section
                    cancelPendingHoverCommit()
                    hoveredSection = nil
                    dragPreviewSection = section
                    pendingDragTarget = nil
                    cancelPendingDragCommit()
                }
                guard dragOriginSection == section else { return }
                let target = SidebarDragTargetResolver.target(
                    at: value.location,
                    rowFrames: rowFrames,
                    current: dragPreviewSection
                )
                guard target != dragPreviewSection else {
                    if pendingDragTarget == target || pendingDragTarget == nil {
                        return
                    }
                    cancelPendingDragCommit()
                    pendingDragTarget = nil
                    return
                }

                if target == nil {
                    cancelPendingDragCommit()
                    pendingDragTarget = nil
                    return
                }

                if pendingDragTarget == target {
                    return
                }

                pendingDragTarget = target
                cancelPendingDragCommit()
                pendingDragCommitTask = Task { [target] in
                    try? await Task.sleep(nanoseconds: 2_000_000_000)
                    await MainActor.run {
                        guard
                            dragOriginSection == section,
                            pendingDragTarget == target,
                            let settledTarget = target
                        else { return }

                        if reduceMotion {
                            dragPreviewSection = settledTarget
                        } else {
                            withAnimation(.easeInOut(duration: 1.0)) {
                                dragPreviewSection = settledTarget
                            }
                        }
                        pendingDragTarget = nil
                    }
                }
            }
            .onEnded { _ in
                guard dragOriginSection == section else { return }
                cancelPendingDragCommit()
                let target = dragPreviewSection ?? section
                dragOriginSection = nil
                dragPreviewSection = nil
                pendingDragTarget = nil
                select(target)
            }
    }

    private func cancelPendingDragCommit() {
        pendingDragCommitTask?.cancel()
        pendingDragCommitTask = nil
    }

    private func cancelPendingHoverCommit() {
        pendingHoverCommitTask?.cancel()
        pendingHoverCommitTask = nil
        pendingHoverTarget = nil
    }
}

private struct VisibleSidebarGroup: Identifiable {
    let group: WorkbenchGroup
    let sections: [WorkbenchSection]
    var id: String { group.id }
}

struct SidebarDragTargetResolver {
    static func target(
        at location: CGPoint,
        rowFrames: [WorkbenchSection: CGRect],
        current: WorkbenchSection?
    ) -> WorkbenchSection? {
        let ordered = rowFrames.sorted {
            if $0.value.midY == $1.value.midY { return $0.key.rawValue < $1.key.rawValue }
            return $0.value.midY < $1.value.midY
        }
        if let containing = ordered.first(where: { $0.value.contains(location) }) {
            return containing.key
        }

        guard let current else {
            return ordered.min {
                abs($0.value.midY - location.y) < abs($1.value.midY - location.y)
            }?.key
        }

        if let currentFrame = rowFrames[current] {
            let distanceToCurrentCenter = abs(currentFrame.midY - location.y)
            if distanceToCurrentCenter <= max(currentFrame.height, 1) * 1.3 {
                return current
            }
        }

        return current
    }
}

private struct SidebarRowFramesPreferenceKey: PreferenceKey {
    static let defaultValue: [WorkbenchSection: CGRect] = [:]

    static func reduce(
        value: inout [WorkbenchSection: CGRect],
        nextValue: () -> [WorkbenchSection: CGRect]
    ) {
        value.merge(nextValue(), uniquingKeysWith: { _, latest in latest })
    }
}

private struct SidebarLiquidGlassSelection: View {
    let isBeingDragged: Bool

    private let cornerRadius: CGFloat = 9
    private var shape: RoundedRectangle {
        RoundedRectangle(cornerRadius: cornerRadius, style: .continuous)
    }

    var body: some View {
        shape
            .fill(.regularMaterial)
            .overlay { bodyTint }
            .overlay { refractiveEdgeLobes }
            .overlay(alignment: .top) { topSpecularBand }
            .overlay(alignment: .topLeading) { movingCaustic }
            .overlay { innerRim }
            .overlay { outerRim }
            .clipShape(shape)
            .compositingGroup()
            .shadow(color: Color.black.opacity(0.08), radius: 8, x: 0, y: 2)
            .shadow(color: Color.white.opacity(0.24), radius: 1, x: 0, y: -1)
            .scaleEffect(isBeingDragged ? 1.012 : 1)
            .animation(.snappy(duration: 0.18), value: isBeingDragged)
    }

    private var bodyTint: some View {
        shape.fill(
            LinearGradient(
                colors: [
                    Color.white.opacity(0.62),
                    Color(nsColor: .textBackgroundColor).opacity(0.30),
                    Color(nsColor: .controlBackgroundColor).opacity(0.34),
                    Color.white.opacity(0.24),
                ],
                startPoint: .topLeading,
                endPoint: .bottomTrailing
            )
        )
    }

    private var refractiveEdgeLobes: some View {
        GeometryReader { geometry in
            let height = max(geometry.size.height, 1)
            let lobeWidth = max(34, height * 1.22)

            HStack(spacing: 0) {
                edgeLobe(flipped: false)
                    .frame(width: lobeWidth, height: height * 1.65)
                    .offset(x: -lobeWidth * 0.44)
                Spacer(minLength: 0)
                edgeLobe(flipped: true)
                    .frame(width: lobeWidth, height: height * 1.65)
                    .offset(x: lobeWidth * 0.44)
            }
            .frame(width: geometry.size.width, height: geometry.size.height)
            .allowsHitTesting(false)
        }
        .blendMode(.screen)
        .opacity(0.92)
    }

    private func edgeLobe(flipped: Bool) -> some View {
        Ellipse()
            .fill(
                RadialGradient(
                    colors: [
                        Color.white.opacity(0.72),
                        Color.white.opacity(0.30),
                        Color(nsColor: .separatorColor).opacity(0.12),
                        Color.clear,
                    ],
                    center: flipped ? .trailing : .leading,
                    startRadius: 1,
                    endRadius: 34
                )
            )
            .overlay {
                Ellipse()
                    .stroke(
                        LinearGradient(
                            colors: [
                                Color.white.opacity(0.82),
                                Color(nsColor: .separatorColor).opacity(0.10),
                                Color.clear,
                            ],
                            startPoint: flipped ? .trailing : .leading,
                            endPoint: flipped ? .leading : .trailing
                        ),
                        lineWidth: 1
                    )
                    .blur(radius: 0.15)
            }
    }

    private var topSpecularBand: some View {
        Capsule(style: .continuous)
            .fill(
                LinearGradient(
                    colors: [
                        Color.white.opacity(0.70),
                        Color.white.opacity(0.24),
                        Color.clear,
                    ],
                    startPoint: .top,
                    endPoint: .bottom
                )
            )
            .frame(height: 9)
            .padding(.horizontal, 13)
            .padding(.top, 2)
            .blendMode(.screen)
    }

    private var movingCaustic: some View {
        RoundedRectangle(cornerRadius: 3, style: .continuous)
            .fill(Color.white.opacity(0.42))
            .frame(width: 44, height: 3)
            .rotationEffect(.degrees(-10))
            .offset(x: 18, y: 4)
            .blur(radius: 0.5)
            .blendMode(.screen)
    }

    private var innerRim: some View {
        shape.strokeBorder(
            LinearGradient(
                colors: [
                    Color.white.opacity(0.82),
                    Color.white.opacity(0.22),
                    Color(nsColor: .separatorColor).opacity(0.16),
                    Color.white.opacity(0.34),
                ],
                startPoint: .topLeading,
                endPoint: .bottomTrailing
            ),
            lineWidth: 1.1
        )
    }

    private var outerRim: some View {
        shape
            .strokeBorder(
                LinearGradient(
                    colors: [
                        Color.white.opacity(0.56),
                        Color(nsColor: .separatorColor).opacity(0.28),
                        Color.black.opacity(0.08),
                    ],
                    startPoint: .top,
                    endPoint: .bottom
                ),
                lineWidth: 0.65
            )
            .blur(radius: 0.12)
    }
}
