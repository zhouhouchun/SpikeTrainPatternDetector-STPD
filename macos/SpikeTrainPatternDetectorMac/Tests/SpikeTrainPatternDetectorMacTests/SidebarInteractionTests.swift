import CoreGraphics
import Testing
@testable import SpikeTrainPatternDetectorMac

@Suite("Sidebar selection interaction")
struct SidebarInteractionTests {
    @Test("Drag selection resolves rows and the nearest inter-group target deterministically")
    func dragTargetResolution() {
        let frames: [WorkbenchSection: CGRect] = [
            .alignedRaster: CGRect(x: 0, y: 10, width: 180, height: 30),
            .rawRaster: CGRect(x: 0, y: 42, width: 180, height: 30),
            .dataQC: CGRect(x: 0, y: 92, width: 180, height: 30),
        ]

        #expect(SidebarDragTargetResolver.target(
            at: CGPoint(x: 40, y: 50),
            rowFrames: frames,
            current: nil
        ) == .rawRaster)
        #expect(SidebarDragTargetResolver.target(
            at: CGPoint(x: 40, y: 84),
            rowFrames: frames,
            current: nil
        ) == .dataQC)
        #expect(SidebarDragTargetResolver.target(
            at: CGPoint(x: 600, y: -40),
            rowFrames: frames,
            current: nil
        ) == .alignedRaster)
    }

    @Test("An empty or single-row geometry remains safe")
    func sparseGeometry() {
        #expect(SidebarDragTargetResolver.target(
            at: .zero,
            rowFrames: [:],
            current: nil
        ) == nil)
        #expect(SidebarDragTargetResolver.target(
            at: CGPoint(x: -500, y: 9_000),
            rowFrames: [.manualDetectorReport: CGRect(x: 0, y: 0, width: 100, height: 20)],
            current: nil
        ) == .manualDetectorReport)
    }

    @Test("Active row keeps sticky intent when pointer is ambiguous")
    func dragTargetStickyCurrentRow() {
        let frames: [WorkbenchSection: CGRect] = [
            .rawRaster: CGRect(x: 0, y: 42, width: 180, height: 30),
            .dataQC: CGRect(x: 0, y: 92, width: 180, height: 30),
        ]

        #expect(
            SidebarDragTargetResolver.target(
                at: CGPoint(x: 40, y: 65),
                rowFrames: frames,
                current: .rawRaster
            ) == .rawRaster
        )

        #expect(
            SidebarDragTargetResolver.target(
                at: CGPoint(x: 40, y: 88),
                rowFrames: frames,
                current: .rawRaster
            ) == .rawRaster
        )

        #expect(
            SidebarDragTargetResolver.target(
                at: CGPoint(x: 40, y: 120),
                rowFrames: frames,
                current: .rawRaster
            ) == .dataQC
        )
    }
}
