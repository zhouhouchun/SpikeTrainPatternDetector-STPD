import Foundation

/// Stable encoding of a population bin's identity into the 3-D scene's point-node name, and back. The scene
/// builder writes `nodeName(forBinID:)` onto each point sphere; the click hit-test reads `binID(fromNodeName:)`
/// to recover which bin was picked. Keeping the round-trip in one pure, tested place prevents the writer and
/// reader from drifting (a prefix mismatch would silently break 3-D selection). Display/interaction only —
/// never a PCA / matrix input.
public enum NeuralManifoldScene3DNodeNaming {
    /// Prefix that marks a node as a selectable population-bin point (distinguishing it from axes, labels, the
    /// trajectory line, and the selection marker, which the hit-test must ignore).
    public static let pointPrefix = "stpd.nm.point:"

    public static func nodeName(forBinID binID: Int) -> String {
        pointPrefix + String(binID)
    }

    /// The bin ID encoded in `name`, or `nil` when `name` is absent or is not a point node (so non-point hits
    /// and empty-space clicks resolve to "no selection").
    public static func binID(fromNodeName name: String?) -> Int? {
        guard let name, name.hasPrefix(pointPrefix) else { return nil }
        return Int(name.dropFirst(pointPrefix.count))
    }
}
