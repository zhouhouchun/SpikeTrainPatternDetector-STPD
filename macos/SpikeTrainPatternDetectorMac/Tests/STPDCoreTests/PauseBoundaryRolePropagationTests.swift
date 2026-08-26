import Foundation
import Testing
@testable import STPDCore

@Suite("Pause boundary role propagation")
struct PauseBoundaryRolePropagationTests {
    @Test("candidate copy helpers preserve every pause boundary role")
    func candidateCopyHelpersPreservePauseBoundaryRole() {
        let roles: [PauseBoundaryRole?] = [
            nil,
            .canonicalPauseAnchor,
            .contextualPause,
            .briefStateInterruption
        ]

        for role in roles {
            let candidate = makePauseCandidate(role: role)

            let autoSelected = candidate.withAutoSelection(
                selectedForAuto: false,
                selectionStatus: "copy_helper_test"
            )
            #expect(autoSelected.pauseBoundaryRole == role)
            #expect(autoSelected.selectedForAuto == false)
            #expect(autoSelected.selectionStatus == "copy_helper_test")

            let diagnostic = candidate.withDiagnosticOverride(
                gateStatus: "review",
                decisionPath: "copy_helper_diagnostic",
                action: "review",
                selectedForAuto: false,
                selectionStatus: "diagnostic_copy_test"
            )
            #expect(diagnostic.pauseBoundaryRole == role)
            #expect(diagnostic.gateStatus == "review")
            #expect(diagnostic.decisionPath == "copy_helper_diagnostic")

            let geometry = candidate.withGeometry(
                startISIIndex: 2,
                endISIIndex: 3,
                startSpikeIndex: 2,
                endSpikeIndex: 4,
                nISI: 2,
                nValidISI: 2,
                nSpikes: 3,
                durationSec: 0.2,
                intraQ10Sec: 0.1,
                intraQ40Sec: 0.1,
                intraQ50Sec: 0.1,
                intraQ90Sec: 0.1,
                intraQ95Sec: 0.1,
                maxIntraISISec: 0.1,
                meanIntraISISec: 0.1,
                cv: 0,
                lv: 0,
                preGapSec: 0.1,
                postGapSec: 0.1,
                preRatioQ90: 1,
                postRatioQ90: 1,
                edgeContrastMinQ90: 1,
                edgeContrastGeomQ90: 1,
                decisionPath: "copy_helper_geometry"
            )
            #expect(geometry.pauseBoundaryRole == role)
            #expect(geometry.startISIIndex == 2)
            #expect(geometry.endISIIndex == 3)
            #expect(geometry.decisionPath == "copy_helper_geometry")
        }
    }

    @Test("event-source construction, clipping, and materialization preserve role")
    func eventSourceProjectionPreservesPauseBoundaryRole() throws {
        let train = makePauseTrain()
        let candidate = makePauseCandidate(
            train: train,
            role: .briefStateInterruption,
            startISIIndex: 1,
            endISIIndex: 5
        )
        let annotation = ClassicAnchorEventAnnotation(candidate: candidate, train: train)
        let source = try #require(annotation.automaticEventSources.first)

        #expect(source.pauseBoundaryRole == .briefStateInterruption)
        #expect(source.supportISIIndices == [1, 2, 3, 4, 5])

        let retained = try #require(source.retainingSupport([2, 3, 4]))
        #expect(retained.pauseBoundaryRole == .briefStateInterruption)
        #expect(retained.supportISIIndices == [2, 3, 4])

        let clipped = try #require(annotation.clipped(to: 2...4, in: train))
        let clippedSource = try #require(clipped.automaticEventSources.first)
        #expect(clippedSource.pauseBoundaryRole == .briefStateInterruption)
        #expect(clippedSource.supportISIIndices == [2, 3, 4])
        #expect(clipped.automaticSupportISIIndices == [2, 3, 4])

        let materialized = try #require(
            ClassicAnchorEventAnnotation.materializedAutomaticComponent(
                source: source,
                to: 3...4,
                in: train,
                manualLockApplied: true
            )
        )
        let materializedSource = try #require(materialized.automaticEventSources.first)
        #expect(materializedSource.pauseBoundaryRole == .briefStateInterruption)
        #expect(materializedSource.supportISIIndices == [1, 2, 3, 4, 5])
        #expect(materialized.automaticSupportISIIndices == [3, 4])
    }

    @Test("normalization unions only sources with the same pause boundary role")
    func normalizationTreatsPauseBoundaryRoleAsSourceIdentity() {
        let canonicalA = makeAutomaticSource(
            role: .canonicalPauseAnchor,
            supportISIIndices: [3, 1]
        )
        let canonicalB = makeAutomaticSource(
            role: .canonicalPauseAnchor,
            supportISIIndices: [2, 3]
        )
        let brief = makeAutomaticSource(
            role: .briefStateInterruption,
            supportISIIndices: [4]
        )

        #expect(Set([canonicalA, brief]).count == 2)

        let normalized = ClassicAnchorEventAnnotation.normalizedAutomaticSources([
            canonicalA,
            brief,
            canonicalB
        ])

        #expect(normalized.count == 2)
        #expect(normalized.map(\.pauseBoundaryRole) == [
            .briefStateInterruption,
            .canonicalPauseAnchor
        ])
        #expect(normalized[0].supportISIIndices == [4])
        #expect(normalized[1].supportISIIndices == [1, 2, 3])
    }

    @Test("source ordering by pause boundary role is deterministic")
    func normalizedSourceOrderingIsInputInvariant() {
        let sources = [
            makeAutomaticSource(role: .contextualPause, supportISIIndices: [1]),
            makeAutomaticSource(role: nil, supportISIIndices: [1]),
            makeAutomaticSource(role: .canonicalPauseAnchor, supportISIIndices: [1]),
            makeAutomaticSource(role: .briefStateInterruption, supportISIIndices: [1])
        ]

        let forward = ClassicAnchorEventAnnotation.normalizedAutomaticSources(sources)
        let reverse = ClassicAnchorEventAnnotation.normalizedAutomaticSources(sources.reversed())

        #expect(forward == reverse)
        #expect(forward.map(\.pauseBoundaryRole) == [
            nil,
            .briefStateInterruption,
            .canonicalPauseAnchor,
            .contextualPause
        ])
    }
}

private func makePauseTrain() -> SpikeTrain {
    SpikeTrain(
        name: "pause-role-propagation-train",
        timestampsSec: [0, 0.1, 0.2, 0.3, 0.4, 0.5]
    )
}

private func makePauseCandidate(
    train: SpikeTrain = makePauseTrain(),
    role: PauseBoundaryRole?,
    startISIIndex: Int = 1,
    endISIIndex: Int = 4
) -> ClassicAnchorCandidate {
    let nISI = endISIIndex - startISIIndex + 1
    var candidate = ClassicAnchorCandidate(
        id: "pause-role-\(role?.rawValue ?? "nil")",
        trainID: train.id,
        trainName: train.name,
        candidateLayer: "pause_role_propagation_test",
        candidateClass: ClassicAnchorLabel.pause.rawValue,
        finalLabel: .pause,
        gateStatus: "pass",
        decisionPath: "pause_role_propagation_test",
        action: "accept",
        score: 0.8,
        priority: 500,
        selectedForAuto: true,
        selectionStatus: "selected_for_test",
        startISIIndex: startISIIndex,
        endISIIndex: endISIIndex,
        startSpikeIndex: startISIIndex,
        endSpikeIndex: endISIIndex + 1,
        nISI: nISI,
        nValidISI: nISI,
        nSpikes: nISI + 1,
        durationSec: Double(nISI) * 0.1,
        intraQ10Sec: 0.1,
        intraQ40Sec: 0.1,
        intraQ50Sec: 0.1,
        intraQ90Sec: 0.1,
        intraQ95Sec: 0.1,
        maxIntraISISec: 0.1,
        meanIntraISISec: 0.1,
        cv: 0,
        lv: 0,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: "pause",
        anchorLockLevel: .strongCandidate,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.2,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil
    )
    candidate.pauseBoundaryRole = role
    return candidate
}

private func makeAutomaticSource(
    role: PauseBoundaryRole?,
    supportISIIndices: [Int]
) -> ClassicAnchorAutomaticEventSource {
    ClassicAnchorAutomaticEventSource(
        annotationID: "shared-annotation",
        candidateID: "shared-candidate",
        trainID: "shared-train",
        supportISIIndices: supportISIIndices,
        semanticTrack: .gap,
        eventTrackClass: ClassicAnchorLabel.pause.rawValue,
        auditRecommendedSubtype: "pause",
        auditReviewStatus: "accepted",
        label: .pause,
        lockLevel: .strongCandidate,
        pauseBoundaryRole: role,
        stateTonicSubtype: nil,
        score: 0.8,
        priority: 500,
        decisionPath: "pause_role_source_test"
    )
}
