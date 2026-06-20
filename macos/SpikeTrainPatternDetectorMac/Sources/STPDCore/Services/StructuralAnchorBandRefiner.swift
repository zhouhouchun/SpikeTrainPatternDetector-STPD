import Foundation

public enum StructuralAnchorBandRefiner {
    public static func refine(
        train: SpikeTrain,
        resolution: TrainAdaptiveBandResolution,
        burstCandidates: [ClassicAnchorCandidate],
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        classicBurstFlankPauseContrastMin: Double = 5.0
    ) -> TrainAdaptiveBandResolution {
        let minValidISISec = max(resolution.minValidISISec, qualitySettings.artifactThresholdSec)
        let structuralBursts = burstCandidates.filter { candidate in
            isBurstAllowedToSeedPauseBand(candidate)
        }
        guard !structuralBursts.isEmpty else {
            return resolution
        }

        let flankValues = burstFlankPauseAnchors(
            train: train,
            bursts: structuralBursts,
            minValidISISec: minValidISISec,
            contrastRequired: classicBurstFlankPauseContrastMin
        )
        guard !flankValues.isEmpty,
              let structuralPauseLower = structuralLowerAnchor(flankValues) else {
            return resolution
        }

        let existingPause = resolution.band(for: .pause) ?? AdaptiveBand(
            pattern: .pause,
            seedLowerSec: 0.100,
            seedUpperSec: 0.150,
            bridgeUpperSec: 0.150,
            contrastS: nil,
            primarySource: .default
        )
        let pauseUpper = max(
            existingPause.seedUpperSec,
            structuralPauseLower,
            quantile(flankValues, probability: 0.80) ?? structuralPauseLower
        )
        let refinedPause = AdaptiveBand(
            pattern: .pause,
            seedLowerSec: structuralPauseLower,
            seedUpperSec: pauseUpper,
            bridgeUpperSec: max(existingPause.bridgeUpperSec, pauseUpper),
            contrastS: existingPause.contrastS,
            primarySource: .structure
        )

        var bands = resolution.bands
        bands[.pause] = refinedPause
        let rows = refinedRows(
            oldRows: resolution.thresholdRows,
            pattern: .pause,
            refinedBand: refinedPause
        )
        let profile = resolution.seedBandProfile
        let phenotypePrior: String
        if !flankValues.isEmpty || resolution.structuralSeedSummary.pausePoolAnchorCount > 0 {
            phenotypePrior = profile.phenotypePrior == "none"
                ? "classic_burst_flank_pause_pool_refined"
                : "\(profile.phenotypePrior)__classic_burst_flank_pause_pool_refined"
        } else {
            phenotypePrior = profile.phenotypePrior
        }
        let refinedProfile = TrainSeedBandProfile(
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
            pauseFraction: pauseFraction(train: train, threshold: structuralPauseLower, minValidISISec: minValidISISec),
            phenotypePrior: phenotypePrior
        )
        let refinedSummary = summaryWithFlankPausePool(
            existing: resolution.structuralSeedSummary,
            structuralBursts: structuralBursts,
            flankValues: flankValues,
            pauseLower: structuralPauseLower,
            pauseUpper: pauseUpper
        )

        return TrainAdaptiveBandResolution(
            trainID: resolution.trainID,
            trainName: resolution.trainName,
            minValidISISec: resolution.minValidISISec,
            histogramBinWidthSec: resolution.histogramBinWidthSec,
            validISICount: resolution.validISICount,
            bands: bands,
            thresholdRows: rows,
            seedBandProfile: refinedProfile,
            structuralSeedSummary: refinedSummary
        )
    }

    private static func burstFlankPauseAnchors(
        train: SpikeTrain,
        bursts: [ClassicAnchorCandidate],
        minValidISISec: Double,
        contrastRequired: Double
    ) -> [Double] {
        var anchors: [Double] = []
        var seen = Set<Int>()
        for burst in bursts {
            let reference = [
                burst.intraQ90Sec,
                burst.intraQ95Sec,
                burst.maxIntraISISec,
                burst.meanIntraISISec
            ]
                .compactMap { $0 }
                .filter { $0.isFinite && $0 > 0 }
                .first
            guard let reference else {
                continue
            }
            let required = max(1, contrastRequired)
            for index in [burst.startISIIndex - 1, burst.endISIIndex + 1] {
                guard !seen.contains(index),
                      let value = validISI(train: train, index: index, minValidISISec: minValidISISec),
                      value / reference >= required - tolerance(for: required) else {
                    continue
                }
                seen.insert(index)
                anchors.append(value)
            }
        }
        return anchors.sorted()
    }

    private static func isBurstAllowedToSeedPauseBand(_ candidate: ClassicAnchorCandidate) -> Bool {
        guard candidate.selectedForAuto,
              candidate.isEligibleForAutoSelection,
              candidate.finalLabel.isCanonicalBurstFamily,
              candidate.anchorLockLevel != .auditOnly else {
            return false
        }
        if candidate.anchorLockLevel == .lockedClassic {
            return true
        }
        if candidate.candidateLayer == "structural_seed_bridge_expansion_burst" {
            return true
        }
        let normalizedGate = candidate.gateStatus.lowercased()
        let normalizedPath = candidate.decisionPath.lowercased()
        return normalizedGate.contains("pass") ||
            normalizedPath.contains("strict_pass") ||
            normalizedPath.contains("post_merge_revalidation")
    }

    private static func structuralLowerAnchor(_ values: [Double]) -> Double? {
        guard !values.isEmpty else {
            return nil
        }
        if values.count < 4 {
            return values.min()
        }
        return quantile(values, probability: 0.20) ?? values.min()
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

    private static func pauseFraction(
        train: SpikeTrain,
        threshold: Double,
        minValidISISec: Double
    ) -> Double? {
        let values = train.isiSec.indices.compactMap { index -> Double? in
            validISI(train: train, index: index, minValidISISec: minValidISISec)
        }
        guard !values.isEmpty else {
            return nil
        }
        let count = values.filter { $0 >= threshold - tolerance(for: threshold) }.count
        return Double(count) / Double(values.count)
    }

    private static func summaryWithFlankPausePool(
        existing: StructuralSeedBandSummary,
        structuralBursts: [ClassicAnchorCandidate],
        flankValues: [Double],
        pauseLower: Double,
        pauseUpper: Double
    ) -> StructuralSeedBandSummary {
        guard !flankValues.isEmpty else {
            return existing
        }

        let burstQ90Values = structuralBursts
            .compactMap(\.intraQ90Sec)
            .filter { $0.isFinite && $0 > 0 }
        let burstBridgeValues = structuralBursts
            .flatMap { [$0.intraQ90Sec, $0.intraQ95Sec, $0.meanIntraISISec] }
            .compactMap { $0 }
            .filter { $0.isFinite && $0 > 0 }

        let pausePoolSource = mergedPausePoolSource(
            existing.pausePoolSource,
            adding: "classic_burst_flank_pause_pool"
        )
        let source = mergedSource(
            existing.source,
            adding: "initial_structural_burst_flank_pause_pool"
        )

        return StructuralSeedBandSummary(
            burstAnchorCount: max(existing.burstAnchorCount, structuralBursts.count),
            burstSupportWeight: existing.burstSupportWeight,
            burstSeedUpperSec: firstPositive(
                existing.burstSeedUpperSec,
                quantile(burstQ90Values, probability: 0.50)
            ),
            burstBridgeUpperSec: firstPositive(
                existing.burstBridgeUpperSec,
                quantile(burstBridgeValues, probability: 0.75)
            ),
            tonicAnchorCount: existing.tonicAnchorCount,
            tonicSupportWeight: existing.tonicSupportWeight,
            tonicSeedLowerSec: existing.tonicSeedLowerSec,
            tonicSeedUpperSec: existing.tonicSeedUpperSec,
            pauseAnchorCount: max(existing.pauseAnchorCount, flankValues.count),
            pausePoolAnchorCount: existing.pausePoolAnchorCount + flankValues.count,
            pauseSupportWeight: nil,
            pausePoolSupportWeight: nil,
            pauseSeedLowerSec: minPositive(existing.pauseSeedLowerSec, pauseLower),
            pauseSeedUpperSec: maxPositive(existing.pauseSeedUpperSec, pauseUpper),
            pausePoolSource: pausePoolSource,
            source: source
        )
    }

    private static func mergedPausePoolSource(_ existing: String, adding source: String) -> String {
        guard existing != "none", !existing.isEmpty else {
            return source
        }
        guard !existing.contains(source) else {
            return existing
        }
        return "\(existing)__\(source)"
    }

    private static func mergedSource(_ existing: String, adding source: String) -> String {
        guard existing != "none", !existing.isEmpty else {
            return source
        }
        guard !existing.contains(source) else {
            return existing
        }
        return "\(existing)__\(source)"
    }

    private static func firstPositive(_ lhs: Double?, _ rhs: Double?) -> Double? {
        finitePositive(lhs) ?? finitePositive(rhs)
    }

    private static func minPositive(_ lhs: Double?, _ rhs: Double?) -> Double? {
        let values = [lhs, rhs].compactMap(finitePositive)
        return values.min()
    }

    private static func maxPositive(_ lhs: Double?, _ rhs: Double?) -> Double? {
        let values = [lhs, rhs].compactMap(finitePositive)
        return values.max()
    }

    private static func finitePositive(_ value: Double?) -> Double? {
        guard let value, value.isFinite, value > 0 else {
            return nil
        }
        return value
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

    private static func quantile(_ values: [Double], probability: Double) -> Double? {
        let sorted = values.filter(\.isFinite).sorted()
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
