import STPDCore
import SwiftUI

struct SidebarView: View {
    @Bindable var document: RasterDocument
    @Binding var selectedSection: WorkbenchSection?

    var body: some View {
        VStack(spacing: 0) {
            moduleList
                .frame(minHeight: 340, maxHeight: .infinity)
        }
        .background(Color(nsColor: .textBackgroundColor))
        .overlay(alignment: .trailing) {
            Rectangle()
                .fill(Color(nsColor: .separatorColor).opacity(0.42))
                .frame(width: 1)
        }
    }

    private var moduleList: some View {
        ScrollView {
            LazyVStack(alignment: .leading, spacing: 16) {
                ForEach(WorkbenchGroup.allCases) { group in
                    VStack(alignment: .leading, spacing: 3) {
                        Text(group.title)
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                            .padding(.horizontal, 10)

                        ForEach(WorkbenchSection.sections(in: group)) { section in
                            sidebarRow(section)
                        }
                    }
                }
            }
            .padding(.horizontal, 6)
            .padding(.vertical, 10)
        }
        .background(.clear)
    }

    private func sidebarRow(_ section: WorkbenchSection) -> some View {
        let isSelected = selectedSection == section

        return Button {
            selectedSection = section
        } label: {
            HStack(spacing: 8) {
                Image(systemName: section.systemImage)
                    .foregroundStyle(isSelected ? Color.white.opacity(0.95) : (section.isLive ? Color.accentColor : Color.secondary))
                    .frame(width: 16)

                VStack(alignment: .leading, spacing: 1) {
                    Text(section.title)
                        .foregroundStyle(isSelected ? Color.white : Color.primary)
                        .lineLimit(1)
                    Text(section.migrationStatus)
                        .font(.caption)
                        .foregroundStyle(isSelected ? Color.white.opacity(0.78) : Color.secondary)
                        .lineLimit(1)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.horizontal, 7)
            .padding(.vertical, 5)
            .contentShape(RoundedRectangle(cornerRadius: 7))
            .background {
                if isSelected {
                    RoundedRectangle(cornerRadius: 7)
                        .fill(Color.accentColor)
                } else {
                    RoundedRectangle(cornerRadius: 7)
                        .fill(Color.clear)
                }
            }
        }
        .buttonStyle(.plain)
    }

}
