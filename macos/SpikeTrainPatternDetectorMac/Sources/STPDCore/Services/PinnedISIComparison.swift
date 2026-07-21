import Foundation

/// A before/after comparison of a single pinned ISI across a detector rerun. Both sides are read-only
/// `PinnedISIDiagnostic` snapshots (the Phase 8 model wrapping the Phase 7 `PerISIDiagnostic`); this type only
/// derives "changed vs unchanged" flags for the inspector. It contains no detection logic.
public struct PinnedISIComparison: Hashable, Sendable {
    /// The pinned ISI state captured immediately before the rerun.
    public let before: PinnedISIDiagnostic
    /// The same train + ISI index recomputed from the new detector run.
    public let after: PinnedISIDiagnostic

    public init(before: PinnedISIDiagnostic, after: PinnedISIDiagnostic) {
        self.before = before
        self.after = after
    }

    /// The covering candidate's detector label changed (including gaining/losing a label).
    public var autoLabelChanged: Bool { before.autoLabel != after.autoLabel }

    /// The manual review/final state changed (e.g. a new candidate resets the review).
    public var reviewChanged: Bool { before.reviewStatus != after.reviewStatus }

    /// Whether the ISI is covered by a public candidate changed.
    public var candidateCoverageChanged: Bool { before.belongsToCandidate != after.belongsToCandidate }

    /// The ISI's relationship to the adaptive seed band changed.
    public var bandRelationChanged: Bool {
        before.diagnostic.bandRelationTag != after.diagnostic.bandRelationTag
    }

    /// True when any tracked dimension changed across the rerun.
    public var anyChange: Bool {
        autoLabelChanged || reviewChanged || candidateCoverageChanged || bandRelationChanged
    }
}
