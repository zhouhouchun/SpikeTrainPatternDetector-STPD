import Foundation

public enum PinnedISIComparisonStatus: String, Hashable, Sendable {
    case comparable
    case identityMismatch = "identity_mismatch"
    case geometryMismatch = "geometry_mismatch"
}

/// A before/after comparison of a single pinned ISI across a detector rerun. Both sides are read-only
/// `PinnedISIDiagnostic` snapshots (the Phase 8 model wrapping the Phase 7 `PerISIDiagnostic`); this type only
/// derives "changed vs unchanged" flags for the inspector. It contains no detection logic.
public struct PinnedISIComparison: Hashable, Sendable {
    /// The pinned ISI state captured immediately before the rerun.
    public let before: PinnedISIDiagnostic
    /// The same train + ISI index recomputed from the new detector run.
    public let after: PinnedISIDiagnostic
    /// Whether both snapshots describe the same immutable biological interval.
    public let status: PinnedISIComparisonStatus

    public init(before: PinnedISIDiagnostic, after: PinnedISIDiagnostic) {
        self.before = before
        self.after = after
        if before.trainID != after.trainID || before.isiIndex != after.isiIndex {
            self.status = .identityMismatch
        } else if !Self.sameGeometry(before, after) {
            self.status = .geometryMismatch
        } else {
            self.status = .comparable
        }
    }

    public var isComparable: Bool { status == .comparable }

    /// The covering candidate's detector label changed (including gaining/losing a label).
    public var autoLabelChanged: Bool { isComparable && before.autoLabel != after.autoLabel }

    /// The manual review/final state changed (e.g. a new candidate resets the review).
    public var reviewChanged: Bool { isComparable && before.reviewStatus != after.reviewStatus }

    /// Whether the ISI is covered by a public candidate changed.
    public var candidateCoverageChanged: Bool {
        isComparable && before.belongsToCandidate != after.belongsToCandidate
    }

    /// The ISI's relationship to the adaptive seed band changed.
    public var bandRelationChanged: Bool {
        isComparable && Self.bandLocation(before) != Self.bandLocation(after)
    }

    /// True when any tracked dimension changed across the rerun.
    public var anyChange: Bool {
        autoLabelChanged || reviewChanged || candidateCoverageChanged || bandRelationChanged
    }

    private static func sameGeometry(_ lhs: PinnedISIDiagnostic, _ rhs: PinnedISIDiagnostic) -> Bool {
        guard lhs.leftTimestampSec.isFinite,
              lhs.rightTimestampSec.isFinite,
              rhs.leftTimestampSec.isFinite,
              rhs.rightTimestampSec.isFinite,
              lhs.isiSec.isFinite,
              rhs.isiSec.isFinite else {
            return false
        }

        // Endpoint tolerance follows the interval's duration, not the absolute recording epoch. A purely
        // relative timestamp comparison would tolerate second-scale movement at Unix-epoch magnitudes.
        // Retain a small ULP allowance so recomputing the same large timestamp remains stable.
        let intervalScale = max(1e-12, abs(lhs.isiSec), abs(rhs.isiSec))
        let endpointMagnitude = max(
            abs(lhs.leftTimestampSec), abs(lhs.rightTimestampSec),
            abs(rhs.leftTimestampSec), abs(rhs.rightTimestampSec)
        )
        let endpointTolerance = max(
            1e-12,
            intervalScale * 1e-9,
            endpointMagnitude * Double.ulpOfOne * 8
        )

        return abs(lhs.leftTimestampSec - rhs.leftTimestampSec) <= endpointTolerance
            && abs(lhs.rightTimestampSec - rhs.rightTimestampSec) <= endpointTolerance
            && approximatelyEqualInterval(lhs.isiSec, rhs.isiSec)
    }

    private static func approximatelyEqualInterval(_ lhs: Double, _ rhs: Double) -> Bool {
        guard lhs.isFinite, rhs.isFinite else { return false }
        let scale = max(1e-12, abs(lhs), abs(rhs))
        return abs(lhs - rhs) <= max(1e-12, scale * 1e-9)
    }

    private enum BandLocation: Hashable {
        case noBand
        case belowSeed
        case inSeed
        case withinBridge
        case aboveSeed
    }

    /// Candidate coverage has its own comparison flag and must not contaminate the numerical band relation.
    /// `PerISIDiagnostic.bandRelationTag` intentionally says "in candidate" for covered ISIs, so comparing that
    /// presentation tag directly would report a false band change whenever coverage alone changed.
    private static func bandLocation(_ snapshot: PinnedISIDiagnostic) -> BandLocation {
        let diagnostic = snapshot.diagnostic
        guard snapshot.isiSec.isFinite,
              let lower = diagnostic.seedLowerSec,
              let upper = diagnostic.seedUpperSec,
              lower.isFinite, upper.isFinite,
              lower >= 0, upper > 0, lower <= upper else {
            return .noBand
        }
        if snapshot.isiSec < lower { return .belowSeed }
        if snapshot.isiSec <= upper { return .inSeed }
        if let bridge = diagnostic.bridgeUpperSec,
           bridge.isFinite, bridge >= upper,
           snapshot.isiSec <= bridge {
            return .withinBridge
        }
        return .aboveSeed
    }
}
