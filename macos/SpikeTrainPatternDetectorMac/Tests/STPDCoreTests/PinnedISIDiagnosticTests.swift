import Foundation
import STPDCore
import Testing

// Phase 8: pinned per-ISI diagnostic snapshot. It wraps the reused Phase 7 `PerISIDiagnostic`; no detection logic.

private func diagnostic(candidate: String?, isiSec: Double = 0.08) -> PerISIDiagnostic {
    PerISIDiagnosticBuilder.diagnose(
        isiSec: isiSec,
        coveringCandidateLabel: candidate,
        seedLowerSec: 0.003,
        seedUpperSec: 0.025,
        bridgeUpperSec: 0.05,
        manualPauseLowerSec: nil
    )
}

@Test
func pinnedISIWrapsCandidateDiagnostic() {
    let diag = diagnostic(candidate: "burst")
    let pin = PinnedISIDiagnostic(
        trainID: "t1", trainName: "Train 1", isiIndex: 42,
        leftTimestampSec: 1.0, rightTimestampSec: 1.08, isiSec: 0.08,
        reviewStatus: "Accepted", diagnostic: diag
    )
    #expect(pin.id == "t1#42")
    #expect(pin.belongsToCandidate)
    #expect(pin.autoLabel == "burst")
    #expect(pin.reviewStatus == "Accepted")
    #expect(pin.isiSec == 0.08)
    #expect(pin.diagnostic == diag)   // reuses Phase 7, not a copy
}

@Test
func pinnedISIOtherHasNoLabelOrReview() {
    let diag = diagnostic(candidate: nil)   // 80 ms, above the 25 ms seed upper / 50 ms bridge
    let pin = PinnedISIDiagnostic(
        trainID: "t1", trainName: "Train 1", isiIndex: 7,
        leftTimestampSec: 0, rightTimestampSec: 0.08, isiSec: 0.08,
        reviewStatus: nil, diagnostic: diag
    )
    #expect(!pin.belongsToCandidate)
    #expect(pin.autoLabel == nil)
    #expect(pin.reviewStatus == nil)
    #expect(pin.diagnostic.bandRelationTag == "above seed upper")
}

@Test
func pinnedISIIsEquatable() {
    let diag = diagnostic(candidate: nil)
    let a = PinnedISIDiagnostic(trainID: "t1", trainName: "Train 1", isiIndex: 7,
                                leftTimestampSec: 0, rightTimestampSec: 0.08, isiSec: 0.08,
                                reviewStatus: nil, diagnostic: diag)
    let b = PinnedISIDiagnostic(trainID: "t1", trainName: "Train 1", isiIndex: 7,
                                leftTimestampSec: 0, rightTimestampSec: 0.08, isiSec: 0.08,
                                reviewStatus: nil, diagnostic: diag)
    #expect(a == b)
}
