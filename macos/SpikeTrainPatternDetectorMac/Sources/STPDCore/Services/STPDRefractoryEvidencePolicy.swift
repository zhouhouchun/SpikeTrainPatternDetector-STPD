enum STPDRefractoryEvidencePolicy {
    static let minimalBurstISIThreshold = 2

    static func shouldApplyAction(refCount: Int, nISI: Int) -> Bool {
        refCount > 0 && nISI == minimalBurstISIThreshold
    }

    static func effectiveAction(
        refCount: Int,
        nISI: Int,
        requestedAction: ClassicAnchorRefractoryAction
    ) -> ClassicAnchorRefractoryAction? {
        guard refCount > 0 else {
            return nil
        }
        return shouldApplyAction(refCount: refCount, nISI: nISI)
            ? requestedAction
            : .warnOnly
    }
}
