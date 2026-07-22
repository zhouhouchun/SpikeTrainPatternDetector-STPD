import Foundation

/// A manual-review navigation / batch channel that groups detector pattern families. Drives the review
/// queue ordering, previous/next navigation, and the batch-accept target set.
public enum ClassicAnchorReviewChannel: String, CaseIterable, Hashable, Sendable, Identifiable {
    case all
    case burst
    case pause
    case tonic
    case highFrequencySpiking
    case reviewOther

    public var id: String { rawValue }

    public static let burstFamilyLabels: Set<ClassicAnchorLabel> = [
        .burst, .highFrequencyBurst, .longBurst, .possibleBurst
    ]

    /// Whether a final label belongs to this channel.
    public func contains(_ label: ClassicAnchorLabel) -> Bool {
        switch self {
        case .all:
            return true
        case .burst:
            return Self.burstFamilyLabels.contains(label)
        case .pause:
            return label == .pause
        case .tonic:
            return label == .tonic || label == .highFrequencyTonic
        case .highFrequencySpiking:
            return label == .highFrequencySpiking
        case .reviewOther:
            return !Self.burstFamilyLabels.contains(label)
                && label != .pause && label != .tonic && label != .highFrequencyTonic
                && label != .highFrequencySpiking
        }
    }

    public var title: String {
        switch self {
        case .all: return "All"
        case .burst: return "Burst family"
        case .pause: return "Pause"
        case .tonic: return "Tonic family"
        case .highFrequencySpiking: return "HFS"
        case .reviewOther: return "Review / other"
        }
    }
}

/// One orderable unit in the manual review queue. A pure value type so the ordering / batch logic is
/// unit-testable without a full (internally-initialized) `ClassicAnchorCandidate`.
public struct ClassicAnchorReviewQueueItem: Hashable, Sendable {
    public let id: String
    public let trainID: String
    public let trainName: String
    public let label: ClassicAnchorLabel
    public let isStrongCandidate: Bool
    public let priority: Int
    public let startISIIndex: Int
    /// Priority tier supplied by the caller (lower = reviewed sooner); preserves the app's existing
    /// review-priority tiers (needsReview / auditReviewRequired / review-track / strong / ...).
    public let rank: Int
    /// Representative candidate ISI: for pause the minimum valid in-span ISI, for burst-family the
    /// maximum valid in-span ISI; `nil` for other labels or when it cannot be resolved.
    public let representativeISISec: Double?

    public init(
        id: String, trainID: String, trainName: String, label: ClassicAnchorLabel,
        isStrongCandidate: Bool, priority: Int, startISIIndex: Int, rank: Int,
        representativeISISec: Double?
    ) {
        self.id = id; self.trainID = trainID; self.trainName = trainName; self.label = label
        self.isStrongCandidate = isStrongCandidate; self.priority = priority
        self.startISIIndex = startISIIndex; self.rank = rank
        self.representativeISISec = representativeISISec.flatMap { $0.isFinite ? $0 : nil }
    }
}

/// Compact counts for a review channel: total reviewable structures and how many are still open
/// (unreviewed / needs-review).
public struct ClassicAnchorReviewQueueSummary: Hashable, Sendable {
    public let total: Int
    public let open: Int

    public init(total: Int, open: Int) {
        self.total = total
        self.open = open
    }
}

/// Pure review-queue ordering + batch-accept target helpers.
public enum ClassicAnchorReviewQueue {
    public static let burstFamilyLabels = ClassicAnchorReviewChannel.burstFamilyLabels

    /// Total and open (unreviewed / needs-review) counts for the items in a channel. `isOpen` is the
    /// caller's predicate (true only for unreviewed / needs-review).
    public static func summary(
        _ items: [ClassicAnchorReviewQueueItem],
        channel: ClassicAnchorReviewChannel,
        isOpen: (ClassicAnchorReviewQueueItem) -> Bool
    ) -> ClassicAnchorReviewQueueSummary {
        let inChannel = items.filter { channel == .all || channel.contains($0.label) }
        return ClassicAnchorReviewQueueSummary(total: inChannel.count, open: inChannel.filter(isOpen).count)
    }

    /// Representative candidate ISI for a span: the maximum (`preferMaximum`) or minimum finite valid
    /// in-span ISI. Operates on `train.isiSec` (0-based, `isiSec[i]` valid for `1..<count`); `nil` when
    /// the span has no valid ISI.
    public static func representativeISISec(
        train: SpikeTrain,
        startISIIndex: Int,
        endISIIndex: Int,
        minValidISISec: Double,
        preferMaximum: Bool
    ) -> Double? {
        let isi = train.isiSec
        let n = isi.count
        let low = max(1, min(startISIIndex, endISIIndex))
        let high = min(n - 1, max(startISIIndex, endISIIndex))
        guard low <= high else { return nil }
        var values: [Double] = []
        for i in low...high {
            if let value = isi[i], value.isFinite, value >= minValidISISec { values.append(value) }
        }
        guard !values.isEmpty else { return nil }
        return preferMaximum ? values.max() : values.min()
    }

    /// The ordered review queue, optionally filtered to one channel and/or one train. Within each
    /// priority tier (`rank`) candidates are grouped by train then by pattern family; within a
    /// (train, family) group pause candidates are ordered by representative ISI ascending and
    /// burst-family by representative ISI descending, then deterministic fallbacks
    /// (strong → priority → start ISI → id).
    public static func orderedItems(
        _ items: [ClassicAnchorReviewQueueItem],
        channel: ClassicAnchorReviewChannel = .all,
        trainID: String? = nil
    ) -> [ClassicAnchorReviewQueueItem] {
        items
            .filter { (channel == .all || channel.contains($0.label)) && (trainID == nil || $0.trainID == trainID) }
            .sorted(by: precedes)
    }

    /// The candidate ids a batch accept should target: items in `trainID` and `channel` for which
    /// `isAcceptable` is true (the caller passes a predicate that is true only for unreviewed /
    /// needs-review items, so rejected / accepted are never touched).
    public static func batchAcceptTargetIDs(
        _ items: [ClassicAnchorReviewQueueItem],
        trainID: String,
        channel: ClassicAnchorReviewChannel,
        isAcceptable: (ClassicAnchorReviewQueueItem) -> Bool
    ) -> [String] {
        items
            .filter { $0.trainID == trainID && (channel == .all || channel.contains($0.label)) && isAcceptable($0) }
            .map(\.id)
    }

    // MARK: - Ordering

    private static func familyRank(_ label: ClassicAnchorLabel) -> Int {
        if burstFamilyLabels.contains(label) { return 0 }
        if label == .pause { return 1 }
        if label == .tonic || label == .highFrequencyTonic { return 2 }
        if label == .highFrequencySpiking { return 3 }
        return 4
    }

    private static func precedes(_ lhs: ClassicAnchorReviewQueueItem, _ rhs: ClassicAnchorReviewQueueItem) -> Bool {
        if lhs.rank != rhs.rank { return lhs.rank < rhs.rank }
        if lhs.trainName != rhs.trainName { return lhs.trainName < rhs.trainName }
        let lhsFamily = familyRank(lhs.label)
        let rhsFamily = familyRank(rhs.label)
        if lhsFamily != rhsFamily { return lhsFamily < rhsFamily }
        // Same tier, same train, same family: pattern-specific representative-ISI ordering. The nil
        // (unresolved) representative ISI is mapped to a sentinel so this stays a TOTAL comparison — a
        // valid strict-weak ordering (Array.sorted requires it). Unresolved-ISI items sort LAST within
        // the family; equal (mapped) keys fall through to the deterministic fallbacks below.
        if lhs.label == .pause || burstFamilyLabels.contains(lhs.label) {
            // familyRank already guarantees lhs and rhs share this family.
            let ascending = lhs.label == .pause            // pause: smallest first; burst: largest first
            let sentinel = ascending ? Double.infinity : -Double.infinity
            let lhsISI = lhs.representativeISISec ?? sentinel
            let rhsISI = rhs.representativeISISec ?? sentinel
            if lhsISI != rhsISI {
                return ascending ? (lhsISI < rhsISI) : (lhsISI > rhsISI)
            }
        }
        // Deterministic fallbacks (preserve existing behavior).
        if lhs.isStrongCandidate != rhs.isStrongCandidate { return lhs.isStrongCandidate }
        if lhs.priority != rhs.priority { return lhs.priority > rhs.priority }
        if lhs.startISIIndex != rhs.startISIIndex { return lhs.startISIIndex < rhs.startISIIndex }
        return lhs.id < rhs.id
    }
}
