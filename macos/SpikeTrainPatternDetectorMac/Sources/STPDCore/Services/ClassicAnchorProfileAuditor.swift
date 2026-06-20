import Foundation

public enum ClassicAnchorProfileAuditor {
    public static func seedBandProfileCandidate(
        train: SpikeTrain,
        resolution: TrainAdaptiveBandResolution
    ) -> ClassicAnchorCandidate {
        let profile = resolution.seedBandProfile
        return ClassicAnchorCandidate(
            id: "\(train.id)-dataset-isi-train-seed-band-profile",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "dataset_isi_train_seed_band_profile",
            candidateClass: "train_profile",
            finalLabel: .profile,
            gateStatus: "profile",
            decisionPath: "dataset_seed_band_percentiles_are_outputs_not_seed_gates",
            action: "audit_only",
            score: 0,
            priority: 0,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: 0,
            endISIIndex: 0,
            startSpikeIndex: 0,
            endSpikeIndex: 0,
            nISI: 0,
            nValidISI: profile.nValidISI,
            nSpikes: train.spikeCount,
            durationSec: nil,
            intraQ10Sec: nil,
            intraQ40Sec: nil,
            intraQ50Sec: profile.medianISISec,
            intraQ90Sec: nil,
            intraQ95Sec: nil,
            maxIntraISISec: nil,
            meanIntraISISec: nil,
            cv: nil,
            lv: nil,
            mm: nil,
            preGapSec: nil,
            postGapSec: nil,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: nil,
            edgeContrastGeomQ90: nil,
            anchorFamily: "dataset_isi_seed_band_profile",
            anchorLockLevel: .auditOnly,
            anchorBandLowerSec: profile.datasetISISeedLowSec,
            anchorBandUpperSec: profile.datasetISISeedHighSec,
            anchorBandSource: .none,
            anchorContrastMinRequired: resolution.burstBand?.contrastS ?? 3.0,
            anchorContrastGeomRequired: resolution.burstBand?.contrastS ?? 3.0,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            profileSeedLowPercentileInTrain: profile.seedLowPercentileInTrain,
            profileSeedHighPercentileInTrain: profile.seedHighPercentileInTrain,
            profileSeedBandFraction: profile.seedBandFraction,
            profileSeedRunCount: profile.seedRunCount,
            profileMaxSeedRunLength: profile.maxSeedRunLength,
            profileMedianISISec: profile.medianISISec,
            profilePauseFraction: profile.pauseFraction,
            profilePhenotypePrior: profile.phenotypePrior,
            profileBridgeUpperSec: profile.datasetISIBridgeHighSec,
            profileBoundaryFloorSec: profile.datasetISIBoundaryFloorSec
        )
    }

    public static func eventCoreProfileCandidate(
        train: SpikeTrain,
        resolution: TrainAdaptiveBandResolution,
        detectorSettings: ClassicAnchorSettings,
        pauseSettings: PauseDetectorSettings
    ) -> ClassicAnchorCandidate {
        let burst = resolution.burstBand
        let validValues = validISISec(train: train, minValidISISec: detectorSettings.minValidISISec)
        let validSample = SortedFiniteSample(validValues)
        let seedFlags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let isi = train.isiSec[index],
                  isi.isFinite,
                  isi >= detectorSettings.minValidISISec else {
                return false
            }
            guard let burst else {
                return false
            }
            return isi >= burst.seedLowerSec && isi <= burst.seedUpperSec
        }
        let seedRuns = boolRuns(seedFlags)
        let maxSeedRun = seedRuns.map { $0.end - $0.start + 1 }.max() ?? 0
        let seedBandFraction = validValues.isEmpty
            ? nil
            : burst.map { band in
                fraction(validValues) { $0 >= band.seedLowerSec && $0 <= band.seedUpperSec }
            }
        let pauseFraction = validValues.isEmpty
            ? nil
            : fraction(validValues) { $0 >= pauseSettings.seedThresholdSec }
        let phenotype = eventCorePhenotype(
            nValidISI: validValues.count,
            seedBandFraction: seedBandFraction,
            seedRunCount: seedRuns.count,
            maxSeedRunLength: maxSeedRun,
            pauseFraction: pauseFraction
        )

        return ClassicAnchorCandidate(
            id: "\(train.id)-event-core-train-isi-band-profile",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "event_core_train_isi_band_profile",
            candidateClass: "train_profile",
            finalLabel: .profile,
            gateStatus: "profile",
            decisionPath: "dataset_manual_isi_band_profile_percentiles_are_outputs",
            action: "audit_only",
            score: 0,
            priority: 0,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: 0,
            endISIIndex: 0,
            startSpikeIndex: 0,
            endSpikeIndex: 0,
            nISI: 0,
            nValidISI: validValues.count,
            nSpikes: train.spikeCount,
            durationSec: nil,
            intraQ10Sec: validSample.quantile(0.10),
            intraQ40Sec: nil,
            intraQ50Sec: validSample.quantile(0.50),
            intraQ90Sec: validSample.quantile(0.90),
            intraQ95Sec: nil,
            maxIntraISISec: nil,
            meanIntraISISec: nil,
            cv: nil,
            lv: nil,
            mm: nil,
            preGapSec: nil,
            postGapSec: nil,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: nil,
            edgeContrastGeomQ90: nil,
            anchorFamily: "event_core_train_isi_band_profile",
            anchorLockLevel: .auditOnly,
            anchorBandLowerSec: burst?.seedLowerSec ?? 0,
            anchorBandUpperSec: burst?.seedUpperSec ?? 0,
            anchorBandSource: burst?.primarySource == .structure ? .structure : .none,
            anchorContrastMinRequired: detectorSettings.burstContrastMin,
            anchorContrastGeomRequired: detectorSettings.burstContrastGeomMin,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            profileSeedLowPercentileInTrain: validValues.isEmpty ? nil : burst.map { band in
                fraction(validValues) { $0 <= band.seedLowerSec } * 100
            },
            profileSeedHighPercentileInTrain: validValues.isEmpty ? nil : burst.map { band in
                fraction(validValues) { $0 <= band.seedUpperSec } * 100
            },
            profileSeedBandFraction: seedBandFraction,
            profileSeedRunCount: seedRuns.count,
            profileMaxSeedRunLength: maxSeedRun,
            profileMedianISISec: validSample.quantile(0.50),
            profileQ10ISISec: validSample.quantile(0.10),
            profileQ25ISISec: validSample.quantile(0.25),
            profileQ90ISISec: validSample.quantile(0.90),
            profilePauseFraction: pauseFraction,
            profilePhenotypePrior: phenotype,
            profileBridgeUpperSec: burst?.bridgeUpperSec,
            profileBoundaryFloorSec: 0,
            profileBoundaryFloorHard: false,
            profileBurstContrastS: detectorSettings.burstContrastMin,
            profilePossibleContrastS: detectorSettings.possibleBurstContrastMin
        )
    }

    public static func structuralSeedProfileCandidate(
        train: SpikeTrain,
        resolution: TrainAdaptiveBandResolution
    ) -> ClassicAnchorCandidate {
        let summary = resolution.structuralSeedSummary
        let anchorUpper = summary.pauseSeedLowerSec ??
            summary.tonicSeedUpperSec ??
            summary.burstBridgeUpperSec ??
            summary.burstSeedUpperSec ??
            resolution.burstBand?.bridgeUpperSec ??
            resolution.burstBand?.seedUpperSec ??
            0
        let anchorLower = summary.tonicSeedLowerSec ??
            summary.burstSeedUpperSec ??
            resolution.burstBand?.seedLowerSec ??
            0
        let pauseAnchorFraction = resolution.validISICount > 0
            ? Double(summary.pauseAnchorCount) / Double(resolution.validISICount)
            : nil
        var decisionPath = [
            "structural_seed_summary",
            "source=\(summary.source)",
            "anchor_weighting=selected_event_balanced",
            "burst_anchors=\(summary.burstAnchorCount)",
            "burst_support_weight=\(weightText(summary.burstSupportWeight))",
            "tonic_anchors=\(summary.tonicAnchorCount)",
            "tonic_support_weight=\(weightText(summary.tonicSupportWeight))",
            "pause_anchors=\(summary.pauseAnchorCount)",
            "pause_pool_anchors=\(summary.pausePoolAnchorCount)",
            "pause_support_weight=\(weightText(summary.pauseSupportWeight))",
            "pause_pool_support_weight=\(weightText(summary.pausePoolSupportWeight))",
            "pause_pool_source=\(summary.pausePoolSource)"
        ]
        if resolution.seedBandProfile.phenotypePrior
            .contains("structural_nonoverlap_ordering_audited_not_applied_to_final_bands") {
            decisionPath.append("structural_nonoverlap_ordering_audited_not_applied_to_final_bands=true")
        }

        return ClassicAnchorCandidate(
            id: "\(train.id)-structural-seed-train-band-profile",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "structural_seed_train_band_profile",
            candidateClass: "train_profile",
            finalLabel: .profile,
            gateStatus: "profile",
            decisionPath: decisionPath.joined(separator: ";"),
            action: "audit_only",
            score: 0,
            priority: 0,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: 0,
            endISIIndex: 0,
            startSpikeIndex: 0,
            endSpikeIndex: 0,
            nISI: 0,
            nValidISI: resolution.validISICount,
            nSpikes: train.spikeCount,
            durationSec: nil,
            intraQ10Sec: summary.tonicSeedLowerSec,
            intraQ40Sec: nil,
            intraQ50Sec: resolution.seedBandProfile.medianISISec,
            intraQ90Sec: summary.tonicSeedUpperSec,
            intraQ95Sec: nil,
            maxIntraISISec: summary.pauseSeedLowerSec,
            meanIntraISISec: nil,
            cv: nil,
            lv: nil,
            mm: nil,
            preGapSec: nil,
            postGapSec: nil,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: nil,
            edgeContrastGeomQ90: nil,
            anchorFamily: "structural_seed_band_profile",
            anchorLockLevel: .auditOnly,
            anchorBandLowerSec: anchorLower,
            anchorBandUpperSec: anchorUpper,
            anchorBandSource: .structure,
            anchorContrastMinRequired: resolution.burstBand?.contrastS ?? 3.0,
            anchorContrastGeomRequired: resolution.burstBand?.contrastS ?? 3.0,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            profileSeedLowPercentileInTrain: nil,
            profileSeedHighPercentileInTrain: nil,
            profileSeedBandFraction: nil,
            profileSeedRunCount: summary.burstAnchorCount,
            profileMaxSeedRunLength: nil,
            profileMedianISISec: resolution.seedBandProfile.medianISISec,
            profilePauseFraction: pauseAnchorFraction,
            profilePhenotypePrior: summary.source,
            profileBridgeUpperSec: summary.burstBridgeUpperSec,
            profileBoundaryFloorSec: summary.pauseSeedLowerSec
        )
    }

    public static func datasetStructuralSeedProfileCandidate(
        dataset: SpikeDataset,
        summary: StructuralDatasetSeedSummary
    ) -> ClassicAnchorCandidate? {
        guard summary.trainCount > 0 else {
            return nil
        }
        let anchorUpper = summary.pauseSeedLowerSec ??
            summary.tonicSeedUpperSec ??
            summary.burstBridgeUpperSec ??
            summary.burstSeedUpperSec ??
            0
        let anchorLower = summary.tonicSeedLowerSec ??
            summary.burstSeedUpperSec ??
            0
        let seededTrainFraction = summary.trainCount > 0
            ? Double(summary.seededTrainCount) / Double(summary.trainCount)
            : nil
        let totalValidISI = dataset.trains.reduce(0) { total, train in
            total + train.isiSec.indices.filter { index in
                index > 0 &&
                    train.isiSec[index].map { $0.isFinite && $0 > 0 } == true
            }.count
        }
        var decisionPath = [
            "dataset_structural_seed_aggregate",
            "source=\(summary.source)",
            "aggregation=train_balanced",
            "trains=\(summary.trainCount)",
            "seeded_trains=\(summary.seededTrainCount)",
            "burst_anchors=\(summary.burstAnchorCount)",
            "burst_support_weight=\(weightText(summary.burstSupportWeight))",
            "tonic_anchors=\(summary.tonicAnchorCount)",
            "tonic_support_weight=\(weightText(summary.tonicSupportWeight))",
            "pause_anchors=\(summary.pauseAnchorCount)",
            "pause_pool_anchors=\(summary.pausePoolAnchorCount)",
            "pause_support_weight=\(weightText(summary.pauseSupportWeight))",
            "pause_pool_support_weight=\(weightText(summary.pausePoolSupportWeight))",
            "pause_pool_source=\(summary.pausePoolSource)"
        ]
        if summary.source.contains("nonoverlap_ordering_audit") {
            decisionPath.append("structural_nonoverlap_ordering_audited_not_applied_to_final_bands=true")
        }

        return ClassicAnchorCandidate(
            id: "\(dataset.id.uuidString)-structural-dataset-seed-profile",
            trainID: "__dataset__",
            trainName: "Dataset structural seed profile",
            candidateLayer: "structural_dataset_seed_profile",
            candidateClass: "dataset_profile",
            finalLabel: .profile,
            gateStatus: "profile",
            decisionPath: decisionPath.joined(separator: ";"),
            action: "audit_only",
            score: 0,
            priority: 0,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: 0,
            endISIIndex: 0,
            startSpikeIndex: 0,
            endSpikeIndex: 0,
            nISI: 0,
            nValidISI: totalValidISI,
            nSpikes: dataset.totalSpikeCount,
            durationSec: nil,
            intraQ10Sec: summary.tonicSeedLowerSec,
            intraQ40Sec: nil,
            intraQ50Sec: nil,
            intraQ90Sec: summary.tonicSeedUpperSec,
            intraQ95Sec: nil,
            maxIntraISISec: summary.pauseSeedLowerSec,
            meanIntraISISec: nil,
            cv: nil,
            lv: nil,
            mm: nil,
            preGapSec: nil,
            postGapSec: nil,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: nil,
            edgeContrastGeomQ90: nil,
            anchorFamily: "structural_dataset_seed_profile",
            anchorLockLevel: .auditOnly,
            anchorBandLowerSec: anchorLower,
            anchorBandUpperSec: anchorUpper,
            anchorBandSource: .structure,
            anchorContrastMinRequired: 0,
            anchorContrastGeomRequired: 0,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            profileSeedLowPercentileInTrain: nil,
            profileSeedHighPercentileInTrain: nil,
            profileSeedBandFraction: seededTrainFraction,
            profileSeedRunCount: summary.seededTrainCount,
            profileMaxSeedRunLength: nil,
            profileMedianISISec: nil,
            profilePauseFraction: nil,
            profilePhenotypePrior: summary.source,
            profileBridgeUpperSec: summary.burstBridgeUpperSec,
            profileBoundaryFloorSec: summary.pauseSeedLowerSec
        )
    }

    private static func validISISec(train: SpikeTrain, minValidISISec: Double) -> [Double] {
        train.isiSec.indices.compactMap { index -> Double? in
            guard index > 0,
                  let isi = train.isiSec[index],
                  isi.isFinite,
                  isi >= minValidISISec else {
                return nil
            }
            return isi
        }
    }

    private static func boolRuns(_ flags: [Bool]) -> [(start: Int, end: Int)] {
        var runs: [(start: Int, end: Int)] = []
        var start: Int?
        for index in flags.indices {
            if flags[index] {
                if start == nil {
                    start = index
                }
            } else if let s = start {
                runs.append((s, index - 1))
                start = nil
            }
        }
        if let s = start {
            runs.append((s, max(s, flags.count - 1)))
        }
        return runs
    }

    private static func fraction(_ values: [Double], where predicate: (Double) -> Bool) -> Double {
        guard !values.isEmpty else {
            return .nan
        }
        let count = values.reduce(0) { partial, value in
            partial + (predicate(value) ? 1 : 0)
        }
        return Double(count) / Double(values.count)
    }

    private static func eventCorePhenotype(
        nValidISI: Int,
        seedBandFraction: Double?,
        seedRunCount: Int,
        maxSeedRunLength: Int,
        pauseFraction: Double?
    ) -> String {
        if nValidISI < 10 {
            return "low_spike_count_unreliable"
        }
        if let seedBandFraction, seedBandFraction >= 0.40, maxSeedRunLength >= 10 {
            return "hf_spiking_like_seed_dominant"
        }
        if let seedBandFraction, seedBandFraction <= 0.02 {
            return "seed_sparse_tonic_or_slow"
        }
        if let pauseFraction, pauseFraction >= 0.25 {
            return "pause_dominant"
        }
        if seedRunCount > 0, let seedBandFraction, seedBandFraction > 0.02 {
            return "burst_capable"
        }
        return "mixed"
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
        let h = (Double(sorted.count) - 1) * p + 1
        let lowerIndex = max(0, Int(floor(h)) - 1)
        let upperIndex = max(0, Int(ceil(h)) - 1)
        let lowerValue = sorted[min(lowerIndex, sorted.count - 1)]
        let upperValue = sorted[min(upperIndex, sorted.count - 1)]
        return lowerValue + (h - floor(h)) * (upperValue - lowerValue)
    }

    private static func weightText(_ value: Double) -> String {
        guard value.isFinite else {
            return "0.000"
        }
        return String(format: "%.3f", min(1, max(0, value)))
    }
}
