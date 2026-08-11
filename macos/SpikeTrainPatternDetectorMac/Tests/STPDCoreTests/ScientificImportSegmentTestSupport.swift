@testable import STPDCore

/// Shared fixtures for the single dataset-global RecordingSegment. Tests must provide explicit
/// segment decisions; these helpers make the standard confirmed segment explicit and reusable.

func testSegmentID(_ id: String = "segment_a") -> ScientificRecordingSegmentID {
    ScientificRecordingSegmentID(try! ScientificSemanticID(validating: id))
}

func standardConfirmedRecordingSegment(
    id: String = "segment_a",
    regime: ScientificRecordingRegime = .continuousUntrialed,
    coverage: ImportedExcerptCoverage = .allSpikeTrainsFullImportedExcerpt,
    bounds: ObservationBoundsAvailability = .unknownOrUnavailable
) -> ConfirmedRecordingSegment {
    ConfirmedRecordingSegment(
        semanticID: testSegmentID(id),
        regime: regime,
        importedExcerptCoverage: coverage,
        observationBounds: bounds
    )
}

extension ScientificImportManifestDraft {
    /// Returns a copy with the four RecordingSegment decisions explicitly confirmed. Used by test
    /// helpers so drafts resolve; individual tests may still set fields directly to exercise
    /// unresolved / tampered cases.
    func withStandardRecordingSegment(
        id: String = "segment_a",
        regime: ScientificRecordingRegime = .continuousUntrialed,
        coverage: ImportedExcerptCoverage = .allSpikeTrainsFullImportedExcerpt,
        bounds: ObservationBoundsAvailability = .unknownOrUnavailable
    ) -> ScientificImportManifestDraft {
        var copy = self
        copy.recordingSegmentID = testSegmentID(id)
        copy.recordingRegime = regime
        copy.importedExcerptCoverage = coverage
        copy.observationBoundsAvailability = bounds
        return copy
    }
}
