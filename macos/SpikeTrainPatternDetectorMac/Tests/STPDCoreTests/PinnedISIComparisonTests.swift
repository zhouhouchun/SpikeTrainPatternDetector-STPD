import Foundation
import STPDCore
import Testing

// Phase 9: before/after comparison of a pinned ISI across a detector rerun. Pure change-flag derivation over two
// reused Phase 8 `PinnedISIDiagnostic` snapshots; no detection logic.

private func diagnostic(
    candidate: String?,
    isiSec: Double,
    lowerSec: Double = 0.003,
    upperSec: Double = 0.025
) -> PerISIDiagnostic {
    PerISIDiagnosticBuilder.diagnose(
        isiSec: isiSec,
        coveringCandidateLabel: candidate,
        seedLowerSec: lowerSec,
        seedUpperSec: upperSec,
        bridgeUpperSec: 0.05,
        manualPauseLowerSec: nil
    )
}

private func pin(
    trainID: String = "t1",
    isiIndex: Int = 12,
    leftTimestampSec: Double = 1.0,
    rightTimestampSec: Double = 1.01,
    isiSec: Double = 0.01,
    candidate: String?,
    reviewStatus: String?,
    lowerSec: Double = 0.003,
    upperSec: Double = 0.025
) -> PinnedISIDiagnostic {
    PinnedISIDiagnostic(
        trainID: trainID, trainName: "Train 1", isiIndex: isiIndex,
        leftTimestampSec: leftTimestampSec, rightTimestampSec: rightTimestampSec, isiSec: isiSec,
        reviewStatus: reviewStatus,
        diagnostic: diagnostic(candidate: candidate, isiSec: isiSec, lowerSec: lowerSec, upperSec: upperSec)
    )
}

@Test
func comparisonReportsNoChangeForIdenticalSnapshots() {
    let before = pin(candidate: "burst", reviewStatus: "Accepted")
    let after = pin(candidate: "burst", reviewStatus: "Accepted")
    let comparison = PinnedISIComparison(before: before, after: after)
    #expect(comparison.status == .comparable)
    #expect(comparison.isComparable)
    #expect(!comparison.autoLabelChanged)
    #expect(!comparison.reviewChanged)
    #expect(!comparison.candidateCoverageChanged)
    #expect(!comparison.bandRelationChanged)
    #expect(!comparison.anyChange)
}

@Test
func comparisonDetectsAutoLabelAndCoverageChange() {
    // The physical ISI is identical; only the detector/review result changes.
    let before = pin(candidate: "burst", reviewStatus: "Accepted")
    let after = pin(candidate: nil, reviewStatus: nil)
    let comparison = PinnedISIComparison(before: before, after: after)
    #expect(comparison.status == .comparable)
    #expect(comparison.autoLabelChanged)        // "burst" -> nil
    #expect(comparison.candidateCoverageChanged) // covered -> uncovered
    #expect(comparison.reviewChanged)            // "Accepted" -> nil
    #expect(!comparison.bandRelationChanged)     // same ISI and same band
    #expect(comparison.anyChange)
}

@Test
func comparisonDetectsReviewChangeAlone() {
    // Same covering label and band relation; only the review status was reset by the rerun.
    let before = pin(candidate: "burst", reviewStatus: "Accepted")
    let after = pin(candidate: "burst", reviewStatus: "Unreviewed")
    let comparison = PinnedISIComparison(before: before, after: after)
    #expect(!comparison.autoLabelChanged)
    #expect(!comparison.candidateCoverageChanged)
    #expect(!comparison.bandRelationChanged)
    #expect(comparison.reviewChanged)
    #expect(comparison.anyChange)
}

@Test
func comparisonDetectsBandRelationChangeWithoutCoverageChange() {
    // The same physical ISI is compared against a changed resolved seed band.
    let before = pin(candidate: nil, reviewStatus: nil, lowerSec: 0.003, upperSec: 0.025)
    let after = pin(candidate: nil, reviewStatus: nil, lowerSec: 0.02, upperSec: 0.03)
    let comparison = PinnedISIComparison(before: before, after: after)
    #expect(comparison.status == .comparable)
    #expect(!comparison.autoLabelChanged)
    #expect(!comparison.candidateCoverageChanged)
    #expect(!comparison.reviewChanged)
    #expect(comparison.bandRelationChanged)
    #expect(comparison.anyChange)
}

@Test
func comparisonRejectsDifferentISIIdentityWithoutReportingSemanticChanges() {
    let before = pin(trainID: "t1", isiIndex: 12, candidate: "burst", reviewStatus: "Accepted")
    let after = pin(trainID: "t2", isiIndex: 12, candidate: nil, reviewStatus: nil)
    let comparison = PinnedISIComparison(before: before, after: after)

    #expect(comparison.status == .identityMismatch)
    #expect(!comparison.isComparable)
    #expect(!comparison.autoLabelChanged)
    #expect(!comparison.reviewChanged)
    #expect(!comparison.candidateCoverageChanged)
    #expect(!comparison.bandRelationChanged)
    #expect(!comparison.anyChange)
}

@Test
func comparisonRejectsChangedGeometryForTheSameIndexedISI() {
    let before = pin(candidate: "burst", reviewStatus: "Accepted")
    let after = pin(
        leftTimestampSec: 1.0,
        rightTimestampSec: 1.02,
        isiSec: 0.02,
        candidate: nil,
        reviewStatus: nil
    )
    let comparison = PinnedISIComparison(before: before, after: after)

    #expect(comparison.status == .geometryMismatch)
    #expect(!comparison.isComparable)
    #expect(!comparison.autoLabelChanged)
    #expect(!comparison.reviewChanged)
    #expect(!comparison.candidateCoverageChanged)
    #expect(!comparison.bandRelationChanged)
    #expect(!comparison.anyChange)
}

@Test
func comparisonRejectsMacroscopicShiftAtEpochScale() {
    let epoch = 1_700_000_000.0
    let before = pin(
        leftTimestampSec: epoch,
        rightTimestampSec: epoch + 0.01,
        isiSec: 0.01,
        candidate: "burst",
        reviewStatus: "Accepted"
    )
    let after = pin(
        leftTimestampSec: epoch + 0.5,
        rightTimestampSec: epoch + 0.51,
        isiSec: 0.01,
        candidate: "burst",
        reviewStatus: "Accepted"
    )

    #expect(PinnedISIComparison(before: before, after: after).status == .geometryMismatch)
}

@Test
func comparisonAllowsFloatingPointULPJitterAtEpochScale() {
    let epoch = 1_700_000_000.0
    let right = epoch + 0.01
    let before = pin(
        leftTimestampSec: epoch,
        rightTimestampSec: right,
        isiSec: 0.01,
        candidate: "burst",
        reviewStatus: "Accepted"
    )
    let after = pin(
        leftTimestampSec: epoch.nextUp,
        rightTimestampSec: right.nextUp,
        isiSec: 0.01,
        candidate: "burst",
        reviewStatus: "Accepted"
    )

    #expect(PinnedISIComparison(before: before, after: after).status == .comparable)
}
