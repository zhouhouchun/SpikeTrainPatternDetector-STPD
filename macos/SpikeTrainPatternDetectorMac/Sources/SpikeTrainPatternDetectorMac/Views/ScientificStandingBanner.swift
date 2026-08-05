import SwiftUI

/// A persistent warning on every primary detector-output surface while the active data came from
/// an exploratory path. This is a product-safety label, not a substitute for the lower export gate.
struct ScientificStandingBanner: View {
    @Bindable var document: RasterDocument

    var body: some View {
        if document.dataset != nil,
           !document.activeDatasetScientificStanding.permitsAuthoritativeDetectorArtifactExport {
            Label(
                "Exploratory data only — this demo or legacy import has not passed canonical scientific confirmation. Detector output is non-authoritative, and scientific artifact export is locked.",
                systemImage: "lock.shield"
            )
            .font(.callout.weight(.semibold))
            .foregroundStyle(.orange)
            .padding(12)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(.orange.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
        }
    }
}
