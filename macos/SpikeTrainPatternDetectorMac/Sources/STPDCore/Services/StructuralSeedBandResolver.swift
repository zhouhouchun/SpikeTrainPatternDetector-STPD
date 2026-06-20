import Foundation

public enum StructuralSeedBandResolver {
    public static func summarize(
        train: SpikeTrain,
        resolution: TrainAdaptiveBandResolution,
        candidates: [ClassicAnchorCandidate],
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings()
    ) -> StructuralSeedBandSummary {
        let minValidISISec = max(resolution.minValidISISec, qualitySettings.artifactThresholdSec)
        let structuralCandidates = candidates.filter { candidate in
            candidate.selectedForAuto ||
                candidate.isEligibleForAutoSelection ||
                candidate.isStructuralPausePriorEvidence
        }

        let strongBurstAnchors = structuralCandidates.filter { candidate in
            candidate.finalLabel.isCanonicalBurstFamily &&
                candidate.selectedForAuto
        }
        let weakPossibleBurstAnchors = structuralCandidates.filter(\.isWeakBurstStructuralSeedEvidence)
        let usingWeakPossibleBurstSeeds = strongBurstAnchors.isEmpty && !weakPossibleBurstAnchors.isEmpty
        let burstAnchors = usingWeakPossibleBurstSeeds ? weakPossibleBurstAnchors : strongBurstAnchors
        let burstQ90Values = burstAnchors.compactMap {
            finite($0.intraQ90Sec, minValidISISec: minValidISISec)
        }
        let burstBridgeValues = burstAnchors.compactMap { candidate in
            eventBalancedBridgeReference(
                candidate,
                minValidISISec: minValidISISec
            )
        }
        let burstSeedUpper = burstAnchors.isEmpty
            ? nil
            : robustCentralUpper(burstQ90Values)
        let burstBridgeUpper = burstAnchors.isEmpty
            ? nil
            : maxFinite(
                burstSeedUpper,
                robustBridgeUpper(burstBridgeValues)
            )
        let burstCoreExclusionUpper = burstSeedUpper

        let tonicAnchors = structuralCandidates.filter { candidate in
            candidate.isClassicTonicStructuralSeedEvidence
        }
        let tonicLowerValues = tonicAnchors.compactMap { candidate in
            tonicLowerAnchorValue(
                candidate,
                minValidISISec: minValidISISec,
                burstCoreExclusionUpper: burstCoreExclusionUpper
            )
        }
        let tonicUpperValues = tonicAnchors.compactMap { candidate in
            tonicUpperAnchorValue(
                candidate,
                minValidISISec: minValidISISec
            )
        }
        let tonicBurstCoreExclusionApplied = tonicAnchors.contains {
            tonicLowerWouldUseBurstCoreExclusion(
                $0,
                minValidISISec: minValidISISec,
                burstCoreExclusionUpper: burstCoreExclusionUpper
            )
        }

        let pauseAnchors = structuralCandidates.filter { candidate in
            candidate.finalLabel == .pause &&
                (
                    candidate.selectedForAuto ||
                        candidate.candidateLayer == "classic_burst_flank_pause" ||
                        candidate.candidateLayer == "possible_burst_flank_pause_prior" ||
                        candidate.isStructuralPausePriorEvidence
                )
        }
        let pausePoolAnchors = pauseAnchors.filter { candidate in
            candidate.candidateLayer == "classic_burst_flank_pause" ||
                candidate.candidateLayer == "possible_burst_flank_pause_prior" ||
                candidate.decisionPath.lowercased().contains("pause_pool_anchor")
        }
        let pausePoolSource = structuralPausePoolSource(pausePoolAnchors)
        let pauseValues = pauseAnchors.compactMap { candidate in
            pauseAnchorValue(
                candidate,
                train: train,
                minValidISISec: minValidISISec
            )
        }

        let summary = StructuralSeedBandSummary(
            burstAnchorCount: burstAnchors.count,
            burstSupportWeight: usingWeakPossibleBurstSeeds
                ? weakStructuralSupportWeight(anchorCount: burstAnchors.count)
                : nil,
            burstSeedUpperSec: burstSeedUpper,
            burstBridgeUpperSec: burstBridgeUpper,
            tonicAnchorCount: tonicAnchors.count,
            tonicSeedLowerSec: quantile(tonicLowerValues, probability: 0.20),
            tonicSeedUpperSec: quantile(tonicUpperValues, probability: 0.80),
            pauseAnchorCount: pauseAnchors.count,
            pausePoolAnchorCount: pausePoolAnchors.count,
            pauseSeedLowerSec: structuralLowerAnchor(pauseValues),
            pauseSeedUpperSec: quantile(pauseValues, probability: 0.80),
            pausePoolSource: pausePoolSource,
            source: structuralSeedSource(
                hasStructuralCandidates: !structuralCandidates.isEmpty,
                hasPausePoolAnchors: !pausePoolAnchors.isEmpty,
                usingWeakPossibleBurstSeeds: usingWeakPossibleBurstSeeds,
                hasClassicTonicAnchors: !tonicAnchors.isEmpty,
                tonicBurstCoreExclusionApplied: tonicBurstCoreExclusionApplied,
                hasTonicPauseRelationship: !tonicAnchors.isEmpty && !pauseAnchors.isEmpty
            )
        )
        return summary.hasAnyAnchor ? summary : .empty
    }

    public static func attachingSummary(
        to resolution: TrainAdaptiveBandResolution,
        summary: StructuralSeedBandSummary
    ) -> TrainAdaptiveBandResolution {
        let refined = refineBands(
            in: resolution,
            with: summary
        )
        return TrainAdaptiveBandResolution(
            trainID: resolution.trainID,
            trainName: resolution.trainName,
            minValidISISec: resolution.minValidISISec,
            histogramBinWidthSec: resolution.histogramBinWidthSec,
            validISICount: resolution.validISICount,
            bands: refined.bands,
            thresholdRows: refined.rows,
            seedBandProfile: refined.profile,
            structuralSeedSummary: summary
        )
    }

    public static func applyingDatasetSummary(
        to resolution: TrainAdaptiveBandResolution,
        datasetSummary: StructuralDatasetSeedSummary
    ) -> TrainAdaptiveBandResolution {
        guard datasetSummary.hasAnyAnchor else {
            return resolution
        }
        let local = resolution.structuralSeedSummary
        let datasetCoverage = datasetSummary.trainCount > 0
            ? Double(datasetSummary.seededTrainCount) / Double(datasetSummary.trainCount)
            : 0
        let burstWeight = shrinkageWeight(
            localValue: local.burstSeedUpperSec,
            localWeight: local.burstSupportWeight
        )
        let tonicWeight = shrinkageWeight(
            localValue: local.tonicSeedLowerSec ?? local.tonicSeedUpperSec,
            localWeight: local.tonicSupportWeight
        )
        let pauseWeight = shrinkageWeight(
            localValue: local.pauseSeedLowerSec ?? local.pauseSeedUpperSec,
            localWeight: local.pauseSupportWeight
        )

        let merged = StructuralSeedBandSummary(
            burstAnchorCount: local.burstAnchorCount,
            burstSupportWeight: effectiveSupportWeight(
                localValue: local.burstSeedUpperSec,
                localWeight: local.burstSupportWeight,
                datasetValue: datasetSummary.burstSeedUpperSec,
                datasetWeight: datasetSummary.burstSupportWeight,
                datasetCoverage: datasetCoverage
            ),
            burstSeedUpperSec: structuralShrinkLog(
                local.burstSeedUpperSec,
                toward: datasetSummary.burstSeedUpperSec,
                weight: burstWeight
            ),
            burstBridgeUpperSec: structuralShrinkLog(
                local.burstBridgeUpperSec,
                toward: datasetSummary.burstBridgeUpperSec,
                weight: burstWeight
            ),
            tonicAnchorCount: local.tonicAnchorCount,
            tonicSupportWeight: effectiveSupportWeight(
                localValue: local.tonicSeedLowerSec ?? local.tonicSeedUpperSec,
                localWeight: local.tonicSupportWeight,
                datasetValue: datasetSummary.tonicSeedLowerSec ?? datasetSummary.tonicSeedUpperSec,
                datasetWeight: datasetSummary.tonicSupportWeight,
                datasetCoverage: datasetCoverage
            ),
            tonicSeedLowerSec: shrinkLog(
                local.tonicSeedLowerSec,
                toward: datasetSummary.tonicSeedLowerSec,
                weight: tonicWeight
            ),
            tonicSeedUpperSec: shrinkLog(
                local.tonicSeedUpperSec,
                toward: datasetSummary.tonicSeedUpperSec,
                weight: tonicWeight
            ),
            pauseAnchorCount: local.pauseAnchorCount,
            pausePoolAnchorCount: local.pausePoolAnchorCount,
            pauseSupportWeight: effectiveSupportWeight(
                localValue: local.pauseSeedLowerSec ?? local.pauseSeedUpperSec,
                localWeight: local.pauseSupportWeight,
                datasetValue: datasetSummary.pauseSeedLowerSec ?? datasetSummary.pauseSeedUpperSec,
                datasetWeight: datasetSummary.pauseSupportWeight,
                datasetCoverage: datasetCoverage
            ),
            pausePoolSupportWeight: effectiveSupportWeight(
                localValue: local.pauseSeedLowerSec,
                localWeight: local.pausePoolSupportWeight,
                datasetValue: datasetSummary.pauseSeedLowerSec,
                datasetWeight: datasetSummary.pausePoolSupportWeight,
                datasetCoverage: datasetCoverage
            ),
            pauseSeedLowerSec: shrinkLog(
                local.pauseSeedLowerSec,
                toward: datasetSummary.pauseSeedLowerSec,
                weight: pauseWeight
            ),
            pauseSeedUpperSec: shrinkLog(
                local.pauseSeedUpperSec,
                toward: datasetSummary.pauseSeedUpperSec,
                weight: pauseWeight
            ),
            pausePoolSource: local.pausePoolSource != "none" ? local.pausePoolSource : datasetSummary.pausePoolSource,
            source: datasetShrinkageSource(local: local, datasetSummary: datasetSummary)
        )
        return attachingSummary(to: resolution, summary: merged)
    }

    private static func refineBands(
        in resolution: TrainAdaptiveBandResolution,
        with summary: StructuralSeedBandSummary
    ) -> (
        bands: [AdaptiveBandPattern: AdaptiveBand],
        rows: [AdaptiveBandThresholdRow],
        profile: TrainSeedBandProfile
    ) {
        var bands = resolution.bands
        var rows = resolution.thresholdRows
        let burstSeedUpper = finitePositive(summary.burstSeedUpperSec)
        let burstBridgeUpper = finitePositive(summary.burstBridgeUpperSec)
        let tonicSeedLower = finitePositive(summary.tonicSeedLowerSec)
        let tonicSeedUpper = finitePositive(summary.tonicSeedUpperSec)
        let pauseSeedLower = finitePositive(summary.pauseSeedLowerSec)
        let pauseSeedUpper = finitePositive(summary.pauseSeedUpperSec)
        let orderingAdjusted = structuralNonoverlapWouldAdjust(
            burstSeedUpper: burstSeedUpper,
            burstBridgeUpper: burstBridgeUpper,
            tonicSeedLower: tonicSeedLower,
            tonicSeedUpper: tonicSeedUpper,
            pauseSeedLower: pauseSeedLower
        )

        if let burstSeedUpper {
            let existing = resolution.band(for: .burst) ?? AdaptiveBand(
                pattern: .burst,
                seedLowerSec: resolution.minValidISISec,
                seedUpperSec: burstSeedUpper,
                bridgeUpperSec: burstBridgeUpper ?? burstSeedUpper,
                contrastS: nil,
                primarySource: .structure
            )
            let seedLower = existing.seedLowerSec
            let seedUpper = max(seedLower, burstSeedUpper)
            let rawBridgeUpper = max(
                seedUpper,
                burstBridgeUpper ?? seedUpper
            )
            let refinedBurst = AdaptiveBand(
                pattern: .burst,
                seedLowerSec: seedLower,
                seedUpperSec: seedUpper,
                bridgeUpperSec: rawBridgeUpper,
                contrastS: existing.contrastS,
                primarySource: .structure
            )
            bands[.burst] = refinedBurst
            rows = refinedRows(oldRows: rows, pattern: .burst, refinedBand: refinedBurst)
        }

        if let tonicLower = tonicSeedLower {
            let existing = resolution.band(for: .tonic) ?? AdaptiveBand(
                pattern: .tonic,
                seedLowerSec: tonicLower,
                seedUpperSec: tonicSeedUpper ?? tonicLower,
                bridgeUpperSec: tonicSeedUpper ?? tonicLower,
                contrastS: nil,
                primarySource: .structure
            )
            let tonicUpper = max(
                tonicLower,
                tonicSeedUpper ?? existing.seedUpperSec
            )
            let refinedTonic = AdaptiveBand(
                pattern: .tonic,
                seedLowerSec: tonicLower,
                seedUpperSec: tonicUpper,
                bridgeUpperSec: max(existing.bridgeUpperSec, tonicUpper),
                contrastS: existing.contrastS,
                primarySource: .structure
            )
            bands[.tonic] = refinedTonic
            rows = refinedRows(oldRows: rows, pattern: .tonic, refinedBand: refinedTonic)
        }

        guard let pauseLower = pauseSeedLower else {
            return (
                bands,
                rows,
                refinedProfile(
                    from: resolution.seedBandProfile,
                    summary: summary,
                    orderingAdjusted: orderingAdjusted
                )
            )
        }

        let existing = resolution.band(for: .pause) ?? AdaptiveBand(
            pattern: .pause,
            seedLowerSec: pauseLower,
            seedUpperSec: pauseLower,
            bridgeUpperSec: pauseLower,
            contrastS: nil,
            primarySource: .structure
        )
        let pauseUpper = max(
            existing.seedUpperSec,
            pauseLower,
            pauseSeedUpper ?? pauseLower
        )
        let refinedPause = AdaptiveBand(
            pattern: .pause,
            seedLowerSec: pauseLower,
            seedUpperSec: pauseUpper,
            bridgeUpperSec: max(existing.bridgeUpperSec, pauseUpper),
            contrastS: existing.contrastS,
            primarySource: .structure
        )
        bands[.pause] = refinedPause

        rows = refinedRows(
            oldRows: rows,
            pattern: .pause,
            refinedBand: refinedPause
        )
        return (
            bands,
            rows,
            refinedProfile(
                from: resolution.seedBandProfile,
                summary: summary,
                orderingAdjusted: orderingAdjusted
            )
        )
    }

    private static func refinedProfile(
        from profile: TrainSeedBandProfile,
        summary: StructuralSeedBandSummary,
        orderingAdjusted: Bool
    ) -> TrainSeedBandProfile {
        return TrainSeedBandProfile(
            nValidISI: profile.nValidISI,
            datasetISISeedLowSec: profile.datasetISISeedLowSec,
            datasetISISeedHighSec: profile.datasetISISeedHighSec,
            datasetISIBridgeHighSec: profile.datasetISIBridgeHighSec,
            datasetISIBoundaryFloorSec: profile.datasetISIBoundaryFloorSec,
            seedLowPercentileInTrain: profile.seedLowPercentileInTrain,
            seedHighPercentileInTrain: profile.seedHighPercentileInTrain,
            seedBandFraction: profile.seedBandFraction,
            seedRunCount: profile.seedRunCount,
            maxSeedRunLength: profile.maxSeedRunLength,
            medianISISec: profile.medianISISec,
            pauseFraction: profile.pauseFraction,
            phenotypePrior: refinedPhenotypePrior(
                existing: profile.phenotypePrior,
                summary: summary,
                orderingAdjusted: orderingAdjusted
            )
        )
    }

    private static func refinedRows(
        oldRows: [AdaptiveBandThresholdRow],
        pattern: AdaptiveBandPattern,
        refinedBand: AdaptiveBand
    ) -> [AdaptiveBandThresholdRow] {
        let refinedValues: [AdaptiveBandField: Double] = [
            .seedLowerSec: refinedBand.seedLowerSec,
            .seedUpperSec: refinedBand.seedUpperSec,
            .bridgeUpperSec: refinedBand.bridgeUpperSec
        ]
        return oldRows.map { row in
            guard row.pattern == pattern,
                  let value = refinedValues[row.field] else {
                return row
            }
            return AdaptiveBandThresholdRow(
                pattern: row.pattern,
                field: row.field,
                histogramSec: row.histogramSec,
                defaultSec: row.defaultSec,
                effectiveSec: value,
                source: .structure
            )
        }
    }

    private static func refinedPhenotypePrior(
        existing: String,
        summary: StructuralSeedBandSummary,
        orderingAdjusted: Bool
    ) -> String {
        var markers: [String] = []
        if orderingAdjusted {
            markers.append("structural_nonoverlap_ordering_audited_not_applied_to_final_bands")
        }
        if summary.burstAnchorCount > 0 || summary.tonicAnchorCount > 0 || summary.pauseAnchorCount > 0 {
            markers.append("structural_seed_bands_attached")
        }
        if summary.pausePoolAnchorCount > 0 {
            markers.append("burst_flank_pause_pool_attached")
            if summary.pausePoolSource.contains("possible_burst") {
                markers.append("possible_burst_flank_pause_prior_pool_attached")
            }
        }
        if summary.source.contains("possible_burst_seed") {
            markers.append("possible_burst_seed_attached")
        }
        if summary.source.contains("classic_tonic_state_seed") {
            markers.append("classic_tonic_state_seed_attached")
        }
        if summary.source.contains("tonic_burst_core_exclusion") {
            markers.append("tonic_burst_core_exclusion_applied")
        }
        if summary.source.contains("tonic_pause_relationship") {
            markers.append("tonic_pause_boundary_relationship_attached")
        }
        guard !markers.isEmpty else {
            return existing
        }
        var parts = existing == "none" ? [] : existing.components(separatedBy: "__")
        for marker in markers where !parts.contains(marker) {
            parts.append(marker)
        }
        return parts.isEmpty ? "none" : parts.joined(separator: "__")
    }

    private static func structuralNonoverlapWouldAdjust(
        burstSeedUpper: Double?,
        burstBridgeUpper: Double?,
        tonicSeedLower: Double?,
        tonicSeedUpper: Double?,
        pauseSeedLower: Double?
    ) -> Bool {
        if let burstUpper = burstSeedUpper, let tonicLower = tonicSeedLower, burstUpper > tonicLower {
            return true
        }
        if let tonicUpper = tonicSeedUpper, let pauseLower = pauseSeedLower, tonicUpper > pauseLower {
            return true
        }
        if let burstUpper = burstSeedUpper, let pauseLower = pauseSeedLower, burstUpper > pauseLower {
            return true
        }
        if let bridgeUpper = burstBridgeUpper, let pauseLower = pauseSeedLower, bridgeUpper >= pauseLower {
            return true
        }
        return false
    }

    private static func shrinkageWeight(localValue: Double?, localWeight: Double) -> Double {
        guard finitePositive(localValue) != nil else {
            return 0
        }
        guard localWeight.isFinite else {
            return 0
        }
        return min(1, max(0, localWeight))
    }

    private static func effectiveSupportWeight(
        localValue: Double?,
        localWeight: Double,
        datasetValue: Double?,
        datasetWeight: Double,
        datasetCoverage: Double
    ) -> Double {
        let cleanLocal = min(1, max(0, localWeight.isFinite ? localWeight : 0))
        let cleanDataset = min(1, max(0, datasetWeight.isFinite ? datasetWeight : 0))
        let cleanCoverage = min(1, max(0, datasetCoverage.isFinite ? datasetCoverage : 0))
        guard finitePositive(datasetValue) != nil else {
            return finitePositive(localValue) == nil ? 0 : cleanLocal
        }
        guard finitePositive(localValue) != nil else {
            return cleanDataset * cleanCoverage
        }
        return min(1, cleanLocal + (1 - cleanLocal) * cleanDataset * cleanCoverage * 0.25)
    }

    private static func weakStructuralSupportWeight(anchorCount: Int) -> Double {
        let count = Double(max(0, anchorCount))
        guard count > 0 else {
            return 0
        }
        return min(0.35, count / (count + 8))
    }

    private static func structuralSeedSource(
        hasStructuralCandidates: Bool,
        hasPausePoolAnchors: Bool,
        usingWeakPossibleBurstSeeds: Bool,
        hasClassicTonicAnchors: Bool,
        tonicBurstCoreExclusionApplied: Bool,
        hasTonicPauseRelationship: Bool
    ) -> String {
        guard hasStructuralCandidates else {
            return "none"
        }
        var parts = ["structural_candidate_audit"]
        if usingWeakPossibleBurstSeeds {
            parts.append("possible_burst_seed_fallback")
        }
        if hasPausePoolAnchors {
            parts.append("pause_pool")
        }
        if hasClassicTonicAnchors {
            parts.append("classic_tonic_state_seed")
        }
        if tonicBurstCoreExclusionApplied {
            parts.append("tonic_burst_core_exclusion")
        }
        if hasTonicPauseRelationship {
            parts.append("tonic_pause_relationship")
        }
        return parts.joined(separator: "_with_")
    }

    private static func structuralPausePoolSource(_ pausePoolAnchors: [ClassicAnchorCandidate]) -> String {
        guard !pausePoolAnchors.isEmpty else {
            return "none"
        }
        let hasClassic = pausePoolAnchors.contains { $0.candidateLayer == "classic_burst_flank_pause" }
        let hasPossible = pausePoolAnchors.contains { $0.candidateLayer == "possible_burst_flank_pause_prior" }
        switch (hasClassic, hasPossible) {
        case (true, true):
            return "classic_and_possible_burst_flank_pause_pool"
        case (true, false):
            return "classic_burst_flank_pause_pool"
        case (false, true):
            return "possible_burst_flank_pause_prior_pool"
        case (false, false):
            return "structural_pause_prior_pool"
        }
    }

    private static func shrinkLog(_ localValue: Double?, toward datasetValue: Double?, weight: Double) -> Double? {
        let local = finitePositive(localValue)
        let dataset = finitePositive(datasetValue)
        switch (local, dataset) {
        case let (.some(local), .some(dataset)):
            let cleanWeight = min(1, max(0, weight.isFinite ? weight : 0))
            return exp(cleanWeight * log(local) + (1 - cleanWeight) * log(dataset))
        case let (.some(local), .none):
            return local
        case let (.none, .some(dataset)):
            return dataset
        case (.none, .none):
            return nil
        }
    }

    private static func structuralShrinkLog(_ localValue: Double?, toward datasetValue: Double?, weight: Double) -> Double? {
        let local = finitePositive(localValue)
        let dataset = finitePositive(datasetValue)
        switch (local, dataset) {
        case let (.some(local), .some(dataset)):
            let cleanWeight = min(1, max(0, weight.isFinite ? weight : 0))
            let blended = exp(cleanWeight * log(local) + (1 - cleanWeight) * log(dataset))
            return min(local, blended)
        case let (.some(local), .none):
            return local
        case (.none, .some):
            return nil
        case (.none, .none):
            return nil
        }
    }

    private static func datasetShrinkageSource(
        local: StructuralSeedBandSummary,
        datasetSummary: StructuralDatasetSeedSummary
    ) -> String {
        var parts = local.source == "none" ? [] : local.source.components(separatedBy: "__")
        if !local.hasAnyAnchor {
            parts.append("no_train_structural_seed")
        }
        if datasetSummary.hasAnyAnchor {
            parts.append("dataset_structural_seed_shrinkage")
        }
        if datasetSummary.pausePoolAnchorCount > 0 {
            parts.append("dataset_burst_flank_pause_pool_available")
            if datasetSummary.pausePoolSource.contains("possible_burst") {
                parts.append("dataset_possible_burst_flank_pause_prior_pool_available")
            }
        }
        var seen = Set<String>()
        let unique = parts.filter { seen.insert($0).inserted }
        return unique.isEmpty ? "none" : unique.joined(separator: "__")
    }

    private static func pauseAnchorValue(
        _ candidate: ClassicAnchorCandidate,
        train: SpikeTrain,
        minValidISISec: Double
    ) -> Double? {
        if candidate.nISI == 1,
           let value = validISI(train: train, index: candidate.startISIIndex, minValidISISec: minValidISISec) {
            return value
        }
        return finite(
            candidate.maxIntraISISec ??
                candidate.meanIntraISISec ??
                candidate.intraQ50Sec ??
                candidate.intraQ90Sec ??
                candidate.durationSec,
            minValidISISec: minValidISISec
        )
    }

    private static func eventBalancedBridgeReference(
        _ candidate: ClassicAnchorCandidate,
        minValidISISec: Double
    ) -> Double? {
        let values = [
            finite(candidate.intraQ90Sec, minValidISISec: minValidISISec),
            finite(candidate.intraQ95Sec, minValidISISec: minValidISISec),
            finite(candidate.meanIntraISISec, minValidISISec: minValidISISec)
        ].compactMap { $0 }
        guard !values.isEmpty else {
            return nil
        }
        return quantile(values, probability: 0.75)
    }

    private static func tonicLowerAnchorValue(
        _ candidate: ClassicAnchorCandidate,
        minValidISISec: Double,
        burstCoreExclusionUpper: Double?
    ) -> Double? {
        guard let base = tonicRawLowerAnchorValue(
            candidate,
            minValidISISec: minValidISISec
        ) else {
            return nil
        }
        guard let burstUpper = finitePositive(burstCoreExclusionUpper),
              base <= burstUpper + tolerance(for: burstUpper) else {
            return base
        }
        return burstUpper
    }

    private static func tonicLowerWouldUseBurstCoreExclusion(
        _ candidate: ClassicAnchorCandidate,
        minValidISISec: Double,
        burstCoreExclusionUpper: Double?
    ) -> Bool {
        guard let base = tonicRawLowerAnchorValue(
            candidate,
            minValidISISec: minValidISISec
        ),
              let burstUpper = finitePositive(burstCoreExclusionUpper) else {
            return false
        }
        return base <= burstUpper + tolerance(for: burstUpper)
    }

    private static func tonicRawLowerAnchorValue(
        _ candidate: ClassicAnchorCandidate,
        minValidISISec: Double
    ) -> Double? {
        finite(
            candidate.intraQ10Sec ??
                candidate.intraQ40Sec ??
                candidate.intraQ50Sec ??
                candidate.meanIntraISISec,
            minValidISISec: minValidISISec
        )
    }

    private static func tonicUpperAnchorValue(
        _ candidate: ClassicAnchorCandidate,
        minValidISISec: Double
    ) -> Double? {
        finite(
            candidate.intraQ90Sec ??
                candidate.intraQ95Sec ??
                candidate.maxIntraISISec ??
                candidate.meanIntraISISec,
            minValidISISec: minValidISISec
        )
    }

    private static func validISI(
        train: SpikeTrain,
        index: Int,
        minValidISISec: Double
    ) -> Double? {
        guard index > 0,
              train.isiSec.indices.contains(index),
              let value = train.isiSec[index],
              value.isFinite,
              value >= minValidISISec - tolerance(for: minValidISISec) else {
            return nil
        }
        return value
    }

    private static func structuralLowerAnchor(_ values: [Double]) -> Double? {
        let sample = SortedFiniteSample(values, positiveOnly: true)
        guard !sample.isEmpty else {
            return nil
        }
        if sample.count < 4 {
            return sample.values.first
        }
        return sample.quantile(0.20)
    }

    private static func robustCentralUpper(_ values: [Double]) -> Double? {
        let sample = SortedFiniteSample(values, positiveOnly: true)
        guard !sample.isEmpty else {
            return nil
        }
        if sample.count < 4 {
            return sample.quantile(0.50)
        }
        return sample.quantile(0.50)
    }

    private static func robustBridgeUpper(_ values: [Double]) -> Double? {
        let sample = SortedFiniteSample(values, positiveOnly: true)
        guard !sample.isEmpty else {
            return nil
        }
        if sample.count < 4 {
            return sample.quantile(0.50)
        }
        return sample.quantile(0.75)
    }

    private static func finite(_ value: Double?, minValidISISec: Double) -> Double? {
        guard let value,
              value.isFinite,
              value >= minValidISISec - tolerance(for: minValidISISec) else {
            return nil
        }
        return value
    }

    private static func finitePositive(_ value: Double?) -> Double? {
        guard let value,
              value.isFinite,
              value > 0 else {
            return nil
        }
        return value
    }

    private static func maxFinite(_ values: Double?...) -> Double? {
        values.compactMap { value -> Double? in
            guard let value, value.isFinite, value > 0 else {
                return nil
            }
            return value
        }
        .max()
    }

    private static func quantile(_ values: [Double], probability: Double) -> Double? {
        let sorted = values.filter(\.isFinite).filter { $0 > 0 }.sorted()
        guard !sorted.isEmpty else {
            return nil
        }
        guard sorted.count > 1 else {
            return sorted[0]
        }
        let p = min(max(probability, 0), 1)
        let h = 1 + (Double(sorted.count) - 1) * p
        let lowerIndex = max(0, Int(floor(h)) - 1)
        let upperIndex = max(0, Int(ceil(h)) - 1)
        let lowerValue = sorted[min(lowerIndex, sorted.count - 1)]
        let upperValue = sorted[min(upperIndex, sorted.count - 1)]
        return lowerValue + (h - floor(h)) * (upperValue - lowerValue)
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }
}
