import Foundation

public enum BurstSeedRunAssembler {
    public static func detect(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        resolution: TrainAdaptiveBandResolution,
        settings: ClassicAnchorSettings
    ) -> [ClassicAnchorCandidate] {
        guard settings.isEnabled,
              settings.isBurstEnabled,
              train.spikeCount > 3,
              settings.enabledLabels.contains(.burst) else {
            return []
        }

        let anchors = selectedBurstAnchors(candidates)
        guard let band = seedBand(
                train: train,
                anchors: anchors,
                resolution: resolution,
                settings: settings
              ) else {
            return []
        }

        let seedFlag = seedFlags(
            train: train,
            band: band,
            settings: settings
        )
        let bridgeFlag = bridgeFlags(
            train: train,
            band: band,
            settings: settings
        )
        let runs = assembleRuns(
            seedFlag: seedFlag,
            bridgeFlag: bridgeFlag,
            maxConsecutiveBridgeISI: 1,
            maxTotalBridgeISI: settings.burstBridgeMaxCount
        )
        guard !runs.isEmpty else {
            return []
        }

        let exactSelectedBurstSpans = Set(
            anchors.map { "\($0.startISIIndex)-\($0.endISIIndex)" }
        )
        var output: [ClassicAnchorCandidate] = []
        output.reserveCapacity(runs.count)
        var seen = Set<String>()

        for run in runs {
            let exactKey = "\(run.start)-\(run.end)"
            guard seen.insert(exactKey).inserted,
                  !exactSelectedBurstSpans.contains(exactKey),
                  let candidate = candidate(
                    train: train,
                    run: run,
                    band: band,
                    settings: settings,
                    anchorCount: anchors.count,
                    index: output.count + 1
                  ) else {
                continue
            }
            output.append(candidate)
        }

        return output
    }

    private struct SeedBand {
        let lower: Double
        let seedUpper: Double
        let bridgeUpper: Double
        let source: String
        let anchorQ80: Double?
        let anchorQ90: Double?
        let anchorQ95: Double?
    }

    private struct SeedRun {
        let start: Int
        let end: Int
        let seedStart: Int
        let seedEnd: Int
        let seedCount: Int
        let bridgeCount: Int
    }

    private struct Metrics {
        let values: [Double]
        let nISI: Int
        let nValidISI: Int
        let nSpikes: Int
        let durationSec: Double?
        let q10: Double?
        let q40: Double?
        let q50: Double?
        let q80: Double?
        let q90: Double?
        let q95: Double?
        let max: Double?
        let mean: Double?
        let cv: Double?
        let lv: Double?
        let preGap: Double?
        let postGap: Double?
        let preRatioQ90: Double?
        let postRatioQ90: Double?
        let edgeContrastMinQ90: Double?
        let edgeContrastGeomQ90: Double?
        let refractorySuspectCount: Int
    }

    private static func selectedBurstAnchors(
        _ candidates: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        candidates.filter { candidate in
            candidate.selectedForAuto &&
                candidate.isEligibleForAutoSelection &&
                candidate.finalLabel == .burst &&
                candidate.anchorLockLevel == .lockedClassic &&
                candidate.candidateLayer != "train_burst_seed_run_assembly"
        }
        .sorted { lhs, rhs in
            if lhs.startISIIndex != rhs.startISIIndex {
                return lhs.startISIIndex < rhs.startISIIndex
            }
            return lhs.endISIIndex < rhs.endISIIndex
        }
    }

    private static func seedBand(
        train: SpikeTrain,
        anchors: [ClassicAnchorCandidate],
        resolution: TrainAdaptiveBandResolution,
        settings: ClassicAnchorSettings
    ) -> SeedBand? {
        let anchorValues = anchors.flatMap { anchor -> [Double] in
            let lower = max(1, min(anchor.startISIIndex, anchor.endISIIndex))
            let upper = min(train.isiSec.count - 1, max(anchor.startISIIndex, anchor.endISIIndex))
            guard lower <= upper else {
                return []
            }
            return (lower...upper).compactMap { index in
                finiteValidISI(train.isiSec[index], minValidISISec: settings.minValidISISec)
            }
        }
        let anchorQ80 = anchorValues.isEmpty ? nil : quantile(anchorValues, probability: 0.80)
        let anchorQ90 = anchorValues.isEmpty ? nil : quantile(anchorValues, probability: 0.90)
        let anchorQ95 = anchorValues.isEmpty ? nil : quantile(anchorValues, probability: 0.95)
        let structuralBurstBand = resolution.burstBand.flatMap { band -> AdaptiveBand? in
            band.primarySource == .structure ? band : nil
        }
        let resolutionSeedUpper = resolution.structuralSeedSummary.burstSeedUpperSec ??
            structuralBurstBand?.seedUpperSec
        let empiricalSeedUpper = anchorQ80 ?? anchorQ90
        let rawSeedUpper = resolutionSeedUpper ?? empiricalSeedUpper
        guard let rawSeedUpper,
              rawSeedUpper.isFinite,
              rawSeedUpper >= settings.minValidISISec else {
            return nil
        }
        guard !anchorValues.isEmpty || resolutionSeedUpper != nil else {
            return nil
        }

        let resolutionBridgeUpper = resolution.structuralSeedSummary.burstBridgeUpperSec ??
            structuralBurstBand?.bridgeUpperSec
        let exploratoryBridgeCap = rawSeedUpper * 2.0
        let compactBridgeCap = burstCompactnessUpper(train: train, settings: settings).map {
            max(rawSeedUpper, min(exploratoryBridgeCap, $0))
        }
        let bridgeCap = compactBridgeCap ?? rawSeedUpper * 1.35
        let cappedResolutionBridgeUpper = resolutionBridgeUpper.map {
            min(max(rawSeedUpper, $0), bridgeCap)
        }
        let empiricalBridgeUpper = finiteMax(anchorQ90, anchorQ95).map {
            min(max(rawSeedUpper, $0), bridgeCap)
        }
        let bridgeUpper = finiteMax(
            cappedResolutionBridgeUpper,
            empiricalBridgeUpper,
            bridgeCap,
            rawSeedUpper
        ) ?? rawSeedUpper
        let source: String
        if !anchorValues.isEmpty {
            source = resolution.structuralSeedSummary.source.isEmpty
                ? "selected_classic_burst_anchor_values"
                : "\(resolution.structuralSeedSummary.source)+selected_classic_burst_anchor_values"
        } else if resolutionSeedUpper != nil {
            source = resolution.structuralSeedSummary.source.isEmpty
                ? "train_adaptive_structural_seed_band"
                : resolution.structuralSeedSummary.source
        } else {
            return nil
        }

        let lower = settings.minValidISISec
        return SeedBand(
            lower: lower,
            seedUpper: max(lower, rawSeedUpper),
            bridgeUpper: max(max(lower, rawSeedUpper), bridgeUpper),
            source: source,
            anchorQ80: anchorQ80,
            anchorQ90: anchorQ90,
            anchorQ95: anchorQ95
        )
    }

    private static func seedFlags(
        train: SpikeTrain,
        band: SeedBand,
        settings: ClassicAnchorSettings
    ) -> [Bool] {
        var seed = Array(repeating: false, count: train.isiSec.count)
        for index in train.isiSec.indices where index > 0 {
            guard let isi = finiteValidISI(train.isiSec[index], minValidISISec: settings.minValidISISec) else {
                continue
            }
            seed[index] = isi >= band.lower - tolerance(for: band.lower) &&
                isi <= band.seedUpper + tolerance(for: band.seedUpper)
        }
        return seed
    }

    private static func bridgeFlags(
        train: SpikeTrain,
        band: SeedBand,
        settings: ClassicAnchorSettings
    ) -> [Bool] {
        guard band.bridgeUpper > band.seedUpper + tolerance(for: band.seedUpper) else {
            return Array(repeating: false, count: train.isiSec.count)
        }

        var bridge = Array(repeating: false, count: train.isiSec.count)
        for index in train.isiSec.indices where index > 0 {
            guard let isi = finiteValidISI(train.isiSec[index], minValidISISec: settings.minValidISISec) else {
                continue
            }
            bridge[index] = isi > band.seedUpper + tolerance(for: band.seedUpper) &&
                isi <= band.bridgeUpper + tolerance(for: band.bridgeUpper)
        }
        return bridge
    }

    private static func assembleRuns(
        seedFlag: [Bool],
        bridgeFlag: [Bool],
        maxConsecutiveBridgeISI: Int,
        maxTotalBridgeISI: Int
    ) -> [SeedRun] {
        guard seedFlag.count > 1 else {
            return []
        }

        let minContiguousISI = 2
        var runs: [SeedRun] = []
        var start: Int?
        var seedStart: Int?
        var lastSeed: Int?
        var seedCount = 0
        var bridgeCount = 0
        var pendingBridgeCount = 0

        func finish() {
            guard let start,
                  let seedStart,
                  let end = lastSeed,
                  seedCount >= minContiguousISI else {
                return
            }
            runs.append(
                SeedRun(
                    start: start,
                    end: end,
                    seedStart: seedStart,
                    seedEnd: end,
                    seedCount: seedCount,
                    bridgeCount: bridgeCount
                )
            )
        }

        func reset() {
            start = nil
            seedStart = nil
            lastSeed = nil
            seedCount = 0
            bridgeCount = 0
            pendingBridgeCount = 0
        }

        for index in seedFlag.indices where index > 0 {
            if seedFlag[index] {
                if start == nil {
                    start = index
                    seedStart = index
                } else {
                    bridgeCount += pendingBridgeCount
                }
                lastSeed = index
                seedCount += 1
                pendingBridgeCount = 0
            } else if bridgeFlag.indices.contains(index),
                      bridgeFlag[index],
                      start != nil,
                      pendingBridgeCount < max(0, maxConsecutiveBridgeISI),
                      bridgeCount + pendingBridgeCount + 1 <= max(0, maxTotalBridgeISI) {
                pendingBridgeCount += 1
            } else {
                finish()
                reset()
            }
        }
        finish()
        return runs
    }

    private static func candidate(
        train: SpikeTrain,
        run: SeedRun,
        band: SeedBand,
        settings: ClassicAnchorSettings,
        anchorCount: Int,
        index: Int
    ) -> ClassicAnchorCandidate? {
        guard let metrics = metrics(
            train: train,
            start: run.start,
            end: run.end,
            settings: settings
        ) else {
            return nil
        }
        guard metrics.nSpikes >= settings.minSpikes else {
            return nil
        }
        guard burstCompactnessPass(
            metrics: metrics,
            train: train,
            settings: settings
        ) else {
            return nil
        }
        if isLikelyHFSpikingRun(metrics: metrics, settings: settings) {
            return nil
        }

        let seedPurity = Double(run.seedCount) / Double(max(1, metrics.nValidISI))
        let bridgeFraction = Double(run.bridgeCount) / Double(max(1, metrics.nISI))
        let label: ClassicAnchorLabel = .possibleBurst
        guard settings.enabledLabels.contains(.possibleBurst) else {
            return nil
        }
        let refCount = refractorySuspectCount(
            train: train,
            start: run.start,
            end: run.end,
            settings: settings
        )
        if refCount > 0 {
            switch settings.refractoryAction {
            case .excludeCandidate, .reject:
                return nil
            case .warnOnly, .demoteToPossible, .review, .markMultiunitContamination:
                break
            }
        }

        let edge = metrics.edgeContrastGeomQ90 ?? metrics.edgeContrastMinQ90 ?? 1
        let score = log1p(Double(metrics.nISI)) +
            0.40 * log(max(edge, 1)) +
            0.35 * seedPurity
        let priority = 1_245
        let decisionPath = [
            "train_burst_seed_run_assembly",
            "source=classic_burst_anchor_intra_band",
            "seed_band_source=\(band.source)",
            "seed_lower_sec=\(format(band.lower))",
            "seed_upper_sec=\(format(band.seedUpper))",
            "anchor_count=\(anchorCount)",
            "anchor_q80_sec=\(format(band.anchorQ80))",
            "anchor_q90_sec=\(format(band.anchorQ90))",
            "anchor_q95_sec=\(format(band.anchorQ95))",
            "seed_count=\(run.seedCount)",
            "bridge_count=\(run.bridgeCount)",
            "bridge_fraction=\(format(bridgeFraction))",
            "seed_purity=\(format(seedPurity))",
            "min_contiguous_isi_required=2",
            "max_consecutive_bridge_isi=1",
            "max_total_bridge_isi=\(settings.burstBridgeMaxCount)",
            "strict_flank_contrast_required=false",
            "bridge_required=false",
            "auto_selected=true",
            "possible_burst_structural_definition=seed_run_burst_ii",
            "burst_subtype=burst_ii",
            "rule=classic_burst_seed_band_run_min2_with_internal_bridge_guard"
        ].joined(separator: ";")

        var candidate = ClassicAnchorCandidate(
            id: "\(train.id)-burst-seed-run-assembly-\(run.start)-\(run.end)-\(index)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "train_burst_seed_run_assembly",
            candidateClass: run.bridgeCount > 0
                ? "classic_anchor_intra_band_bridge_burst_ii"
                : "classic_anchor_intra_band_two_isi_burst_ii",
            finalLabel: label,
            gateStatus: run.bridgeCount > 0
                ? "burst_seed_run_bridge_burst_ii_pass"
                : "burst_seed_run_burst_ii_pass",
            decisionPath: decisionPath,
            action: "accept_burst_ii",
            score: score,
            priority: priority,
            selectedForAuto: true,
            selectionStatus: "selected_by_burst_seed_run_assembly",
            startISIIndex: run.start,
            endISIIndex: run.end,
            startSpikeIndex: run.start,
            endSpikeIndex: run.end + 1,
            nISI: metrics.nISI,
            nValidISI: metrics.nValidISI,
            nSpikes: metrics.nSpikes,
            durationSec: metrics.durationSec,
            intraQ10Sec: metrics.q10,
            intraQ40Sec: metrics.q40,
            intraQ50Sec: metrics.q50,
            intraQ90Sec: metrics.q90,
            intraQ95Sec: metrics.q95,
            maxIntraISISec: metrics.max,
            meanIntraISISec: metrics.mean,
            cv: metrics.cv,
            lv: metrics.lv,
            mm: nil,
            preGapSec: metrics.preGap,
            postGapSec: metrics.postGap,
            preRatioQ90: metrics.preRatioQ90,
            postRatioQ90: metrics.postRatioQ90,
            edgeContrastMinQ90: metrics.edgeContrastMinQ90,
            edgeContrastGeomQ90: metrics.edgeContrastGeomQ90,
            anchorFamily: "classic_burst_seed_run",
            anchorLockLevel: .strongCandidate,
            anchorBandLowerSec: band.lower,
            anchorBandUpperSec: band.seedUpper,
            anchorBandSource: .structure,
            anchorContrastMinRequired: 1,
            anchorContrastGeomRequired: 1,
            refractorySuspectCount: refCount,
            refractorySuspectAction: refCount > 0 ? settings.refractoryAction : nil
        )
        candidate.burstSeedRunStartISI = run.seedStart
        candidate.burstSeedRunEndISI = run.seedEnd
        candidate.burstSeedBandLowerSec = band.lower
        candidate.burstSeedBandUpperSec = band.seedUpper
        candidate.burstBridgeBandUpperSec = band.bridgeUpper
        candidate.burstBridgeCountPass = true
        candidate.burstBridgeFractionPass = true
        candidate.burstQ90BridgePass = true
        candidate.burstSizeLabelBeforeReview = label.rawValue
        return candidate
    }

    private static func isLikelyHFSpikingRun(
        metrics: Metrics,
        settings: ClassicAnchorSettings
    ) -> Bool {
        let duration = metrics.durationSec ?? 0
        let hfsSpikeFloor = max(settings.longMaxSpikes + 1, 30)
        if metrics.nSpikes >= hfsSpikeFloor {
            return true
        }

        let sustainedSpikeFloor = max(settings.longMaxSpikes + 1, 17)
        if metrics.nSpikes >= sustainedSpikeFloor,
           duration >= 0.20 {
            return true
        }

        if metrics.nISI >= 18,
           let q80 = metrics.q80,
           let q90 = metrics.q90,
           q90 <= max(q80 * 1.5, settings.effectiveBurstBandUpperSec * 1.35) {
            return true
        }

        return false
    }

    private static func burstCompactnessPass(
        metrics: Metrics,
        train: SpikeTrain,
        settings: ClassicAnchorSettings
    ) -> Bool {
        guard let q90 = metrics.q90,
              q90.isFinite,
              let compactUpper = burstCompactnessUpper(train: train, settings: settings),
              compactUpper.isFinite,
              compactUpper > 0 else {
            return true
        }
        return q90 <= compactUpper + tolerance(for: compactUpper)
    }

    private static func burstCompactnessUpper(
        train: SpikeTrain,
        settings: ClassicAnchorSettings
    ) -> Double? {
        let values = train.isiSec.indices.compactMap { index -> Double? in
            guard index > 0,
                  let isi = finiteValidISI(
                    train.isiSec[index],
                    minValidISISec: settings.minValidISISec
                  ) else {
                return nil
            }
            return isi
        }
        guard values.count >= 8,
              let q80 = quantile(values, probability: 0.80),
              q80.isFinite,
              q80 > settings.minValidISISec else {
            return nil
        }
        return max(
            settings.minValidISISec,
            q80 * settings.burstEpisodeBackgroundFraction
        )
    }

    private static func metrics(
        train: SpikeTrain,
        start: Int,
        end: Int,
        settings: ClassicAnchorSettings
    ) -> Metrics? {
        guard start >= 1,
              end >= start,
              end < train.isiSec.count,
              train.timestampsSec.indices.contains(start - 1),
              train.timestampsSec.indices.contains(end) else {
            return nil
        }
        let values = (start...end).compactMap { index in
            finiteValidISI(train.isiSec[index], minValidISISec: settings.minValidISISec)
        }
        guard !values.isEmpty else {
            return nil
        }
        let nISI = end - start + 1
        let duration = train.timestampsSec[end] - train.timestampsSec[start - 1]
        let mean = values.reduce(0, +) / Double(values.count)
        let sd: Double? = values.count > 1
            ? sqrt(values.reduce(0) { $0 + pow($1 - mean, 2) } / Double(values.count - 1))
            : nil
        let cv = sd.flatMap { mean > 0 ? $0 / mean : nil }
        let lv = localVariation(values)
        let sample = SortedFiniteSample(values)
        let q90 = sample.quantile(0.90)
        let pre = start > 1 ? finiteValidISI(train.isiSec[start - 1], minValidISISec: settings.minValidISISec) : nil
        let post = end < train.isiSec.count - 1
            ? finiteValidISI(train.isiSec[end + 1], minValidISISec: settings.minValidISISec)
            : nil
        let preRatio = ratio(pre, over: q90)
        let postRatio = ratio(post, over: q90)
        let edgeMin = finiteMin(preRatio, postRatio)
        let edgeGeom: Double?
        if let preRatio,
           let postRatio,
           preRatio.isFinite,
           postRatio.isFinite,
           preRatio > 0,
           postRatio > 0 {
            edgeGeom = sqrt(preRatio * postRatio)
        } else {
            edgeGeom = finiteMax(preRatio, postRatio)
        }
        return Metrics(
            values: values,
            nISI: nISI,
            nValidISI: values.count,
            nSpikes: end - start + 2,
            durationSec: duration.isFinite ? duration : nil,
            q10: sample.quantile(0.10),
            q40: sample.quantile(0.40),
            q50: sample.quantile(0.50),
            q80: sample.quantile(0.80),
            q90: q90,
            q95: sample.quantile(0.95),
            max: values.max(),
            mean: mean.isFinite ? mean : nil,
            cv: cv,
            lv: lv,
            preGap: pre,
            postGap: post,
            preRatioQ90: preRatio,
            postRatioQ90: postRatio,
            edgeContrastMinQ90: edgeMin,
            edgeContrastGeomQ90: edgeGeom,
            refractorySuspectCount: refractorySuspectCount(
                train: train,
                start: start,
                end: end,
                settings: settings
            )
        )
    }

    private static func refractorySuspectCount(
        train: SpikeTrain,
        start: Int,
        end: Int,
        settings: ClassicAnchorSettings
    ) -> Int {
        guard start <= end else {
            return 0
        }
        return (start...end).reduce(0) { total, index in
            guard let value = train.isiSec[index],
                  value.isFinite,
                  value < settings.refractorySuspectSec else {
                return total
            }
            return total + 1
        }
    }

    private static func finiteValidISI(_ value: Double?, minValidISISec: Double) -> Double? {
        guard let value,
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
        if sorted.count == 1 {
            return sorted[0]
        }
        let clamped = min(max(probability, 0), 1)
        let position = clamped * Double(sorted.count - 1)
        let lower = Int(floor(position))
        let upper = Int(ceil(position))
        if lower == upper {
            return sorted[lower]
        }
        let fraction = position - Double(lower)
        return sorted[lower] * (1 - fraction) + sorted[upper] * fraction
    }

    private static func localVariation(_ values: [Double]) -> Double? {
        guard values.count > 1 else {
            return nil
        }
        var sum = 0.0
        var count = 0
        for pair in zip(values, values.dropFirst()) {
            let denominator = pair.0 + pair.1
            guard denominator > 0 else {
                continue
            }
            sum += 3 * pow(pair.0 - pair.1, 2) / pow(denominator, 2)
            count += 1
        }
        guard count > 0 else {
            return nil
        }
        return sum / Double(count)
    }

    private static func ratio(_ numerator: Double?, over denominator: Double?) -> Double? {
        guard let numerator,
              let denominator,
              numerator.isFinite,
              denominator.isFinite,
              denominator > 0 else {
            return nil
        }
        return numerator / denominator
    }

    private static func finiteMax(_ values: Double?...) -> Double? {
        values.compactMap { value -> Double? in
            guard let value, value.isFinite else {
                return nil
            }
            return value
        }
        .max()
    }

    private static func finiteMin(_ values: Double?...) -> Double? {
        values.compactMap { value -> Double? in
            guard let value, value.isFinite else {
                return nil
            }
            return value
        }
        .min()
    }

    private static func format(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), value)
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }
}
