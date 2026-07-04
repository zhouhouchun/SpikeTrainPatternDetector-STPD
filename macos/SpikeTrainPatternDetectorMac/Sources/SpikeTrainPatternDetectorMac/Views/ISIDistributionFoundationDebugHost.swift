import STPDCore
import SwiftUI

/// TEMPORARY debug host that makes the standalone `ISIDistributionFoundationView` reactive to the live
/// `RasterDocument` (dataset + detection run). It is opened from the Debug menu, NOT part of the
/// production Workbench navigation — that clean wiring is deferred until the foreign nav refactor is
/// committed. Uses real runtime data only; when no dataset is loaded it shows the empty/no-data state.
struct ISIDistributionFoundationDebugHost: View {
    @Bindable var document: RasterDocument

    var body: some View {
        // Reading these @Observable properties in `body` makes the view recompute when a dataset loads
        // or a detection run completes (no snapshot-at-open staleness).
        ISIDistributionFoundationView(
            dataset: document.dataset ?? Self.emptyDataset,
            runDistribution: document.classicAnchorDetectionRun?.datasetISIDistribution,
            minimumValidISISec: document.adaptiveDetectorBandSettings.minValidISISec
        )
    }

    /// A clearly-empty dataset used only when nothing is loaded (drives the view's no-data state).
    private static let emptyDataset = SpikeDataset(name: "(no dataset)", sourceDescription: "debug", trains: [])
}
