import Foundation

public enum BurstLocalCompletionDetector {
    public static func detect(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        settings: StatePatternDetectorSettings
    ) -> [ClassicAnchorCandidate] {
        guard train.spikeCount >= 3 else {
            return []
        }

        let selectedEvents = candidates.filter {
            $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .event &&
                $0.finalLabel.isBurstEventFamily
        }
        guard !selectedEvents.isEmpty else {
            return []
        }

        let selectedGaps = candidates.filter {
            $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .gap &&
                $0.finalLabel == .pause
        }
        let selectedStates = candidates.filter {
            $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .state &&
                ($0.finalLabel == .tonic || $0.finalLabel == .highFrequencyTonic)
        }

        let eventIndices = coveredIndices(selectedEvents)
        let gapIndices = coveredIndices(selectedGaps)
        let stateIndices = coveredIndices(selectedStates)
        let seedUpper = max(settings.minValidISISec, settings.burstSeedUpperSec)
        let relaxedBridgeUpper = max(seedUpper, seedUpper * 1.25)
        let standaloneRelaxedUpper = max(relaxedBridgeUpper, seedUpper * 1.60)

        var output: [ClassicAnchorCandidate] = []
        var index = 1
        while index < train.isiSec.count {
            if eventIndices.contains(index) || gapIndices.contains(index) || stateIndices.contains(index) {
                index += 1
                continue
            }
            guard let isi = finiteValidISI(train.isiSec[index], minValidISISec: settings.minValidISISec),
                  isi <= standaloneRelaxedUpper + tolerance(for: standaloneRelaxedUpper) else {
                index += 1
                continue
            }

            let start = index
            var end = index
            while end + 1 < train.isiSec.count,
                  !eventIndices.contains(end + 1),
                  !gapIndices.contains(end + 1),
                  !stateIndices.contains(end + 1),
                  let next = finiteValidISI(train.isiSec[end + 1], minValidISISec: settings.minValidISISec),
                  next <= standaloneRelaxedUpper + tolerance(for: standaloneRelaxedUpper) {
                end += 1
            }

            if let candidate = localCompletionCandidate(
                train: train,
                start: start,
                end: end,
                eventIndices: eventIndices,
                stateIndices: stateIndices,
                seedUpper: seedUpper,
                relaxedBridgeUpper: relaxedBridgeUpper,
                standaloneRelaxedUpper: standaloneRelaxedUpper,
                settings: settings,
                index: output.count + 1
            ) {
                output.append(candidate)
            }
            index = end + 1
        }

        return output
    }

    private enum CompletionRule: String {
        case bridgeBetweenBurstCores = "bridge_between_selected_burst_cores"
        case prefixBeforeBurstCore = "prefix_before_selected_burst_core"
        case suffixAfterBurstCore = "suffix_after_selected_burst_core"
        case flankedRelaxedTwoISIBurst = "flanked_relaxed_two_isi_burst"
    }

    private static func localCompletionCandidate(
        train: SpikeTrain,
        start: Int,
        end: Int,
        eventIndices: Set<Int>,
        stateIndices: Set<Int>,
        seedUpper: Double,
        relaxedBridgeUpper: Double,
        standaloneRelaxedUpper: Double,
        settings: StatePatternDetectorSettings,
        index: Int
    ) -> ClassicAnchorCandidate? {
        let length = end - start + 1
        guard length > 0, length <= 3 else {
            return nil
        }
        let values = (start...end).compactMap {
            finiteValidISI(train.isiSec[$0], minValidISISec: settings.minValidISISec)
        }
        guard values.count == length,
              let q90 = quantile(values, probability: 0.90),
              q90.isFinite,
              q90 > 0 else {
            return nil
        }

        let seedCount = values.filter {
            $0 <= seedUpper + tolerance(for: seedUpper)
        }.count
        let allWithinBridge = values.allSatisfy {
            $0 <= relaxedBridgeUpper + tolerance(for: relaxedBridgeUpper)
        }
        let allWithinStandalone = values.allSatisfy {
            $0 <= standaloneRelaxedUpper + tolerance(for: standaloneRelaxedUpper)
        }

        let preIsBurst = start > 1 && eventIndices.contains(start - 1)
        let postIsBurst = end + 1 < train.isiSec.count && eventIndices.contains(end + 1)
        let preIsStableState = start > 1 && stateIndices.contains(start - 1)
        let postIsStableState = end + 1 < train.isiSec.count && stateIndices.contains(end + 1)
        let preGap = finiteValidISI(
            start > 1 ? train.isiSec[start - 1] : nil,
            minValidISISec: settings.minValidISISec
        )
        let postGap = finiteValidISI(
            end + 1 < train.isiSec.count ? train.isiSec[end + 1] : nil,
            minValidISISec: settings.minValidISISec
        )

        let rule: CompletionRule
        if preIsBurst && postIsBurst && length <= 2 && allWithinBridge {
            rule = .bridgeBetweenBurstCores
        } else if !preIsBurst && postIsBurst && length == 3 && seedCount >= 1 && allWithinBridge &&
                    gap(preGap, exceeds: q90, multiplier: 2.0) {
            rule = .prefixBeforeBurstCore
        } else if preIsBurst && !postIsBurst && length == 1 && allWithinBridge &&
                    gap(postGap, exceeds: q90, multiplier: 2.0) {
            rule = .suffixAfterBurstCore
        } else if !preIsBurst && !postIsBurst && length == 2 && seedCount >= 1 &&
                    allWithinStandalone &&
                    preIsStableState &&
                    gap(preGap, exceeds: q90, multiplier: 2.0) &&
                    gap(postGap, exceeds: q90, multiplier: 1.75) {
            rule = .flankedRelaxedTwoISIBurst
        } else {
            return nil
        }

        guard train.timestampsSec.indices.contains(start - 1),
              train.timestampsSec.indices.contains(end) else {
            return nil
        }

        let duration = train.timestampsSec[end] - train.timestampsSec[start - 1]
        let q10 = quantile(values, probability: 0.10)
        let q40 = quantile(values, probability: 0.40)
        let q50 = quantile(values, probability: 0.50)
        let q95 = quantile(values, probability: 0.95)
        let mean = values.reduce(0, +) / Double(values.count)
        let maxISI = values.max()
        let score = log1p(Double(length)) +
            0.40 * log(max((preGap ?? q90) / q90, 1)) +
            0.40 * log(max((postGap ?? q90) / q90, 1)) +
            0.30 * (Double(seedCount) / Double(max(1, length)))

        let decisionPath = [
            "burst_local_completion",
            "possible_burst_structural_definition=local_completion_burst_ii",
            "burst_subtype=burst_ii",
            "rule=\(rule.rawValue)",
            "source=selected_burst_context_plus_structure_derived_seed_band",
            "seed_upper_sec=\(format(seedUpper))",
            "relaxed_bridge_upper_sec=\(format(relaxedBridgeUpper))",
            "standalone_relaxed_upper_sec=\(format(standaloneRelaxedUpper))",
            "seed_count=\(seedCount)",
            "n_isi=\(length)",
            "pre_is_selected_burst=\(preIsBurst)",
            "post_is_selected_burst=\(postIsBurst)",
            "pre_is_selected_stable_state=\(preIsStableState)",
            "post_is_selected_stable_state=\(postIsStableState)",
            "pre_gap_sec=\(format(preGap))",
            "post_gap_sec=\(format(postGap))",
            "intra_q90_sec=\(format(q90))",
            "global_seed_run_bridge_relaxation=false",
            "auto_selected=true"
        ].joined(separator: ";")

        var candidate = ClassicAnchorCandidate(
            id: "\(train.id)-burst-local-completion-\(start)-\(end)-\(index)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "burst_local_completion",
            candidateClass: rule.rawValue,
            finalLabel: .possibleBurst,
            gateStatus: "burst_local_completion_pass",
            decisionPath: decisionPath,
            action: "accept_burst_ii_local_completion",
            score: score,
            priority: 2_665,
            selectedForAuto: true,
            selectionStatus: "selected_by_burst_local_completion",
            startISIIndex: start,
            endISIIndex: end,
            startSpikeIndex: start,
            endSpikeIndex: end + 1,
            nISI: length,
            nValidISI: length,
            nSpikes: length + 1,
            durationSec: duration.isFinite ? duration : nil,
            intraQ10Sec: q10,
            intraQ40Sec: q40,
            intraQ50Sec: q50,
            intraQ90Sec: q90,
            intraQ95Sec: q95,
            maxIntraISISec: maxISI,
            meanIntraISISec: mean,
            cv: STPDStatistics.coefficientOfVariation(values),
            lv: STPDStatistics.localVariation(values),
            preGapSec: preGap,
            postGapSec: postGap,
            preRatioQ90: ratio(preGap, over: q90),
            postRatioQ90: ratio(postGap, over: q90),
            edgeContrastMinQ90: finiteMin(ratio(preGap, over: q90), ratio(postGap, over: q90)),
            edgeContrastGeomQ90: geometricMean(ratio(preGap, over: q90), ratio(postGap, over: q90)),
            anchorFamily: "classic_burst_seed_context",
            anchorLockLevel: .strongCandidate,
            anchorBandLowerSec: settings.minValidISISec,
            anchorBandUpperSec: seedUpper,
            anchorBandSource: .structure,
            anchorContrastMinRequired: 1,
            anchorContrastGeomRequired: 1,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil
        )
        candidate.burstSeedRunStartISI = start
        candidate.burstSeedRunEndISI = end
        candidate.burstSeedBandLowerSec = settings.minValidISISec
        candidate.burstSeedBandUpperSec = seedUpper
        candidate.burstBridgeBandUpperSec = relaxedBridgeUpper
        candidate.burstBridgeCountPass = true
        candidate.burstBridgeFractionPass = true
        candidate.burstQ90BridgePass = true
        candidate.burstSizeLabelBeforeReview = ClassicAnchorLabel.possibleBurst.rawValue
        return candidate
    }

    private static func coveredIndices(_ candidates: [ClassicAnchorCandidate]) -> Set<Int> {
        var indices = Set<Int>()
        for candidate in candidates {
            let lower = min(candidate.startISIIndex, candidate.endISIIndex)
            let upper = max(candidate.startISIIndex, candidate.endISIIndex)
            guard lower <= upper else {
                continue
            }
            for index in lower...upper {
                indices.insert(index)
            }
        }
        return indices
    }

    private static func gap(_ value: Double?, exceeds reference: Double, multiplier: Double) -> Bool {
        guard let value, value.isFinite, reference.isFinite, reference > 0 else {
            return false
        }
        return value >= reference * multiplier - tolerance(for: reference * multiplier)
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
        let sorted = values.filter { $0.isFinite }.sorted()
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

    private static func ratio(_ value: Double?, over denominator: Double?) -> Double? {
        guard let value,
              let denominator,
              value.isFinite,
              denominator.isFinite,
              denominator > 0 else {
            return nil
        }
        return value / denominator
    }

    private static func finiteMin(_ lhs: Double?, _ rhs: Double?) -> Double? {
        switch (lhs, rhs) {
        case let (lhs?, rhs?) where lhs.isFinite && rhs.isFinite:
            return min(lhs, rhs)
        case let (lhs?, nil) where lhs.isFinite:
            return lhs
        case let (nil, rhs?) where rhs.isFinite:
            return rhs
        default:
            return nil
        }
    }

    private static func geometricMean(_ lhs: Double?, _ rhs: Double?) -> Double? {
        switch (lhs, rhs) {
        case let (lhs?, rhs?) where lhs.isFinite && rhs.isFinite && lhs > 0 && rhs > 0:
            return sqrt(lhs * rhs)
        case let (lhs?, nil) where lhs.isFinite:
            return lhs
        case let (nil, rhs?) where rhs.isFinite:
            return rhs
        default:
            return nil
        }
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
