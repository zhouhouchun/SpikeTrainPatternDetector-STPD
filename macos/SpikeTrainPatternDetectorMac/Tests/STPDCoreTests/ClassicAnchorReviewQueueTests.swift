import Foundation
import STPDCore
import Testing

// Manual review queue ordering + batch-accept target selection.

private func item(
    _ id: String, train: String = "T", name: String = "T", label: ClassicAnchorLabel,
    rank: Int = 4, strong: Bool = false, priority: Int = 0, start: Int = 1, repISI: Double? = nil
) -> ClassicAnchorReviewQueueItem {
    ClassicAnchorReviewQueueItem(
        id: id, trainID: train, trainName: name, label: label, isStrongCandidate: strong,
        priority: priority, startISIIndex: start, rank: rank, representativeISISec: repISI
    )
}

// MARK: - 1. Pause: smallest representative ISI first (ascending).

@Test
func pauseCandidatesOrderBySmallestISIFirst() {
    let ordered = ClassicAnchorReviewQueue.orderedItems([
        item("slow", label: .pause, repISI: 0.120),   // 120 ms
        item("fast", label: .pause, repISI: 0.080)    // 80 ms
    ])
    #expect(ordered.map(\.id) == ["fast", "slow"])
}

// MARK: - 2. Burst-family: largest representative ISI first (descending).

@Test
func burstCandidatesOrderByLargestISIFirst() {
    let ordered = ClassicAnchorReviewQueue.orderedItems([
        item("small", label: .burst, repISI: 0.006),   // max intra-burst 6 ms
        item("large", label: .burst, repISI: 0.012)    // max intra-burst 12 ms
    ])
    #expect(ordered.map(\.id) == ["large", "small"])
    // Mixed burst-family labels still order by representative ISI within the family.
    let mixed = ClassicAnchorReviewQueue.orderedItems([
        item("lb", label: .longBurst, repISI: 0.007),
        item("hf", label: .highFrequencyBurst, repISI: 0.011)
    ])
    #expect(mixed.map(\.id) == ["hf", "lb"])
}

// MARK: - 3. Deterministic fallback when ISI cannot resolve.

@Test
func fallbackOrderingIsDeterministicWhenISIUnresolved() {
    // No representative ISI -> fall back to strong, priority, start ISI, id.
    let byPriority = ClassicAnchorReviewQueue.orderedItems([
        item("low", label: .burst, priority: 1, start: 5, repISI: nil),
        item("high", label: .burst, priority: 2, start: 3, repISI: nil)
    ])
    #expect(byPriority.map(\.id) == ["high", "low"])   // higher priority first

    let byID = ClassicAnchorReviewQueue.orderedItems([
        item("z", label: .tonic, repISI: nil),
        item("a", label: .tonic, repISI: nil)
    ])
    #expect(byID.map(\.id) == ["a", "z"])              // stable id tiebreak
}

// MARK: - 3b. Unresolved (nil) ISI items mix safely with resolved ones (strict-weak ordering).

@Test
func nilISIItemsSortLastWithinFamilyAndStayConsistent() {
    // Mixing a nil-ISI item with two differently-valued resolved items must not break the sort
    // contract; the nil item sorts LAST within the family, resolved items keep their direction.
    let pause = ClassicAnchorReviewQueue.orderedItems([
        item("nilP", label: .pause, repISI: nil),
        item("slow", label: .pause, repISI: 0.30),
        item("fast", label: .pause, repISI: 0.10)
    ])
    #expect(pause.map(\.id) == ["fast", "slow", "nilP"])

    let burst = ClassicAnchorReviewQueue.orderedItems([
        item("nilB", label: .burst, repISI: nil),
        item("small", label: .burst, repISI: 0.006),
        item("large", label: .burst, repISI: 0.012)
    ])
    #expect(burst.map(\.id) == ["large", "small", "nilB"])
}

@Test
func nonFiniteRepresentativeISIsNormalizeToUnresolvedAndSortDeterministically() {
    let inputs = [
        item("nan", label: .pause, repISI: .nan),
        item("posInf", label: .pause, repISI: .infinity),
        item("negInf", label: .pause, repISI: -.infinity),
        item("finite", label: .pause, repISI: 0.2)
    ]

    #expect(inputs[0].representativeISISec == nil)
    #expect(inputs[1].representativeISISec == nil)
    #expect(inputs[2].representativeISISec == nil)

    let expected = ClassicAnchorReviewQueue.orderedItems(inputs).map(\.id)
    #expect(expected.first == "finite")
    #expect(ClassicAnchorReviewQueue.orderedItems(Array(inputs.reversed())).map(\.id) == expected)
    #expect(ClassicAnchorReviewQueue.orderedItems([inputs[2], inputs[0], inputs[3], inputs[1]]).map(\.id) == expected)
}

// MARK: - 4. Priority tiers (rank) dominate the representative-ISI ordering.

@Test
func rankTierDominatesISIOrdering() {
    let ordered = ClassicAnchorReviewQueue.orderedItems([
        item("burstTier4", label: .burst, rank: 4, repISI: 0.012),
        item("pauseTier0", label: .pause, rank: 0, repISI: 0.500)
    ])
    #expect(ordered.map(\.id) == ["pauseTier0", "burstTier4"])   // rank 0 before rank 4
}

// MARK: - 5. Representative ISI from a train (min for pause, max for burst).

@Test
func representativeISIReadsMinOrMaxInSpan() {
    // isiSec[1]=0.005, isiSec[2]=0.012, isiSec[3]=0.008
    let train = SpikeTrain(name: "t", timestampsSec: [0, 0.005, 0.017, 0.025])
    let maxISI = ClassicAnchorReviewQueue.representativeISISec(
        train: train, startISIIndex: 1, endISIIndex: 3, minValidISISec: 0.001, preferMaximum: true
    )
    let minISI = ClassicAnchorReviewQueue.representativeISISec(
        train: train, startISIIndex: 1, endISIIndex: 3, minValidISISec: 0.001, preferMaximum: false
    )
    #expect(abs((maxISI ?? 0) - 0.012) < 1e-9)
    #expect(abs((minISI ?? 0) - 0.005) < 1e-9)
    // Out-of-range span -> nil (fall back to existing ordering).
    #expect(ClassicAnchorReviewQueue.representativeISISec(
        train: train, startISIIndex: 0, endISIIndex: 0, minValidISISec: 0.001, preferMaximum: true
    ) == nil)
}

// MARK: - 6. Channel filtering.

@Test
func channelFilteringSelectsTheRightFamily() {
    let items = [
        item("p", label: .pause, repISI: 0.1),
        item("b", label: .burst, repISI: 0.01),
        item("t", label: .tonic),
        item("h", label: .highFrequencySpiking)
    ]
    #expect(ClassicAnchorReviewQueue.orderedItems(items, channel: .pause).map(\.id) == ["p"])
    #expect(ClassicAnchorReviewQueue.orderedItems(items, channel: .burst).map(\.id) == ["b"])
    #expect(ClassicAnchorReviewQueue.orderedItems(items, channel: .tonic).map(\.id) == ["t"])
    #expect(ClassicAnchorReviewQueue.orderedItems(items, channel: .highFrequencySpiking).map(\.id) == ["h"])
    #expect(Set(ClassicAnchorReviewQueue.orderedItems(items, channel: .all).map(\.id)) == ["p", "b", "t", "h"])
}

// MARK: - 7. Batch-accept target set respects train + channel + status.

@Test
func batchAcceptTargetsOnlyUnreviewedOrNeedsReviewSameTrainChannel() {
    let items = [
        item("pA", train: "T", label: .pause),   // unreviewed
        item("pB", train: "T", label: .pause),   // needsReview
        item("pC", train: "T", label: .pause),   // rejected -> never touched
        item("pD", train: "T", label: .pause),   // accepted -> not churned
        item("bE", train: "T", label: .burst),   // wrong channel
        item("pF", train: "U", label: .pause)    // other train
    ]
    let status: [String: String] = [
        "pA": "unreviewed", "pB": "needsReview", "pC": "rejected", "pD": "accepted",
        "bE": "unreviewed", "pF": "unreviewed"
    ]
    let acceptable: (ClassicAnchorReviewQueueItem) -> Bool = {
        status[$0.id] == "unreviewed" || status[$0.id] == "needsReview"
    }
    let targets = ClassicAnchorReviewQueue.batchAcceptTargetIDs(
        items, trainID: "T", channel: .pause, isAcceptable: acceptable
    )
    #expect(Set(targets) == ["pA", "pB"])   // rejected, accepted, wrong-channel, other-train all excluded
}

// MARK: - 8. Channel summary: total + open counts respect the channel filter.

@Test
func summaryCountsTotalAndOpenPerChannel() {
    let items = [
        item("pA", label: .pause),     // open
        item("pB", label: .pause),     // open
        item("pC", label: .pause),     // closed (reviewed)
        item("bA", label: .burst),     // open
        item("bB", label: .burst),     // closed
        item("t", label: .tonic),      // open
        item("h", label: .highFrequencySpiking)   // closed
    ]
    let openIDs: Set<String> = ["pA", "pB", "bA", "t"]
    let isOpen: (ClassicAnchorReviewQueueItem) -> Bool = { openIDs.contains($0.id) }

    let pause = ClassicAnchorReviewQueue.summary(items, channel: .pause, isOpen: isOpen)
    #expect(pause.total == 3)
    #expect(pause.open == 2)

    let burst = ClassicAnchorReviewQueue.summary(items, channel: .burst, isOpen: isOpen)
    #expect(burst.total == 2)
    #expect(burst.open == 1)

    let all = ClassicAnchorReviewQueue.summary(items, channel: .all, isOpen: isOpen)
    #expect(all.total == 7)
    #expect(all.open == 4)

    // A channel with no open items reports open == 0 but a non-zero total.
    let hfs = ClassicAnchorReviewQueue.summary(items, channel: .highFrequencySpiking, isOpen: isOpen)
    #expect(hfs.total == 1)
    #expect(hfs.open == 0)
}
