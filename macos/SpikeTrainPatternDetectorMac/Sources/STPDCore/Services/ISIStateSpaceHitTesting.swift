import Foundation

/// A plotted point's position in the embedding scatter's on-screen (canvas) coordinate space. Used purely to
/// resolve which point the cursor is nearest for the hover inspector — never an embedding input.
public struct ISIStateSpaceScreenPoint: Hashable, Sendable {
    public let x: Double
    public let y: Double

    public init(x: Double, y: Double) {
        self.x = x
        self.y = y
    }
}

/// Pure nearest-point selection for the ISI State Space embedding scatters (PCA / Isomap / logISI phase
/// portrait). Given the on-screen positions of the plotted points and the cursor location in the same
/// coordinate space, it returns the index of the nearest point within `maxDistance`, or `nil` when none is
/// close enough (so the hover card hides). This is the testable seam behind the otherwise view-only hover:
/// projection (data → screen) stays in the SwiftUI canvas, but the selection math lives here.
public enum ISIStateSpaceHitTesting {
    /// The index of the position nearest `target` whose Euclidean distance is `<= maxDistance`, or `nil` if
    /// there is none (empty input, everything beyond the radius, or a negative radius). Distances are compared
    /// squared (no `sqrt`). Ties resolve to the lower index, so selection is stable in dense plots; non-finite
    /// coordinates never win (their squared distance is NaN, which fails every comparison).
    public static func nearestIndex(
        positions: [ISIStateSpaceScreenPoint],
        to target: ISIStateSpaceScreenPoint,
        maxDistance: Double
    ) -> Int? {
        guard maxDistance >= 0 else { return nil }
        let maxSquared = maxDistance * maxDistance
        var bestIndex: Int?
        var bestSquared = Double.greatestFiniteMagnitude
        for (index, position) in positions.enumerated() {
            let dx = position.x - target.x
            let dy = position.y - target.y
            let squared = dx * dx + dy * dy
            if squared <= maxSquared, squared < bestSquared {
                bestSquared = squared
                bestIndex = index
            }
        }
        return bestIndex
    }
}
