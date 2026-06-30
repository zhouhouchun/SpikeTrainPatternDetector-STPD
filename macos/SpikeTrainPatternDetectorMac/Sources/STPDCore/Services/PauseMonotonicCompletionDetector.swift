import Foundation

public enum PauseMonotonicCompletionDetector {
    public static func detect(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        settings: PauseDetectorSettings,
        additionalBlockedISIIndices: Set<Int> = []
    ) -> [ClassicAnchorCandidate] {
        guard settings.isEnabled, train.spikeCount >= 2 else {
            return []
        }

        let selectedPauses = candidates.filter { candidate in
            candidate.finalLabel == .pause &&
                candidate.selectedForAuto &&
                candidate.isEligibleForAutoSelection &&
                !candidate.isStructuralPausePriorEvidence
        }
        guard let pauseFloor = inferredPauseFloor(from: selectedPauses, settings: settings) else {
            return []
        }

        // Do not emit duplicate completion candidates where another formal
        // pause detector has already supplied eligible evidence. The selected
        // pause subset still determines the calibrated monotonic floor.
        let existingFormalPauseIndices = coveredIndices(
            candidates: candidates.filter { candidate in
                candidate.finalLabel == .pause &&
                    candidate.isEligibleForAutoSelection &&
                    !candidate.isStructuralPausePriorEvidence
            }
        )
        // State/review tracks do not block pause evidence generation. Final
        // arbitration still gives state labels priority over overlapping pause
        // completions. Selected burst-family event cores are hard conflicts for
        // monotonic pause completion, including Burst II, HF burst, and long
        // burst once they are on the event track.
        let occupiedHardEventIndices = coveredIndices(
            candidates: candidates.filter { candidate in
                candidate.selectedForAuto &&
                    candidate.isEligibleForAutoSelection &&
                    candidate.arbitrationTrack == .event &&
                    candidate.finalLabel.isBurstEventFamily
            }
        )

        // R parity: a completing ISI must be a pause RELATIVE to the local and global background, not
        // merely above the calibrated floor. Mirrors the formal PauseDetector / R `event_core_pause_gap`
        // (R/38_event_grammar_core.R:835-846): flag an ISI only when it also exceeds the local median by
        // `eventCoreLocalFactor` (R `pause_relative_local_factor`, 1.55) and the global median by
        // `eventCoreGlobalFactor` (R `pause_relative_global_factor`, 1.25). Without this guard a uniform
        // tonic baseline that merely sits above a low pause floor — seeded by short pre-burst transition
        // ISIs whose gap is long versus the burst but shorter than the baseline — is wrongly completed as
        // pause (PAUSE-1: the leading baseline ISIs of burst_response_2_s were labeled pause).
        let globalMedian = median(validTrainISIs(train, settings: settings))
        let globalThreshold = globalMedian.map { $0 * settings.eventCoreGlobalFactor }
        let baseLongIndices = Set(train.isiSec.indices.filter { index in
            index > 0 && (finiteValidISI(train.isiSec[index], settings: settings)
                .map { $0 >= pauseFloor - tolerance(for: pauseFloor) } ?? false)
        })

        var completions: [ClassicAnchorCandidate] = []
        for index in train.isiSec.indices where index > 0 {
            guard !existingFormalPauseIndices.contains(index),
                  !occupiedHardEventIndices.contains(index),
                  !additionalBlockedISIIndices.contains(index),
                  let isi = finiteValidISI(train.isiSec[index], settings: settings),
                  isi >= pauseFloor - tolerance(for: pauseFloor),
                  exceedsLocalAndGlobalBackground(
                    isi: isi,
                    centerIndex: index,
                    train: train,
                    baseLongIndices: baseLongIndices,
                    globalThreshold: globalThreshold,
                    settings: settings
                  ),
                  let candidate = completionCandidate(
                    train: train,
                    index: index,
                    isi: isi,
                    pauseFloor: pauseFloor,
                    settings: settings,
                  ) else {
                continue
            }
            completions.append(candidate)
        }
        return completions
    }

    private static func inferredPauseFloor(
        from pauses: [ClassicAnchorCandidate],
        settings: PauseDetectorSettings
    ) -> Double? {
        let values = pauses.compactMap { candidate in
            robustPauseFloorEvidence(candidate, settings: settings)
        }
        guard let floor = quantile(values, probability: 0.25),
              floor.isFinite,
              floor > 0 else {
            return nil
        }
        return max(settings.minValidISISec, floor)
    }

    private static func robustPauseFloorEvidence(
        _ candidate: ClassicAnchorCandidate,
        settings: PauseDetectorSettings
    ) -> Double? {
        let centralValues = [
            candidate.intraQ40Sec,
            candidate.intraQ50Sec,
            candidate.meanIntraISISec,
            candidate.maxIntraISISec
        ]
        .compactMap { validPauseEvidence($0, settings: settings) }
        guard let central = quantile(centralValues, probability: 0.50) else {
            return validPauseEvidence(candidate.anchorBandLowerSec, settings: settings)
        }
        if let lower = validPauseEvidence(candidate.anchorBandLowerSec, settings: settings) {
            return max(central, lower)
        }
        return central
    }

    private static func validPauseEvidence(_ value: Double?, settings: PauseDetectorSettings) -> Double? {
        guard let value,
              value.isFinite,
              value >= settings.minValidISISec - tolerance(for: settings.minValidISISec) else {
            return nil
        }
        return value
    }

    private static func coveredIndices(candidates: [ClassicAnchorCandidate]) -> Set<Int> {
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

    private static func completionCandidate(
        train: SpikeTrain,
        index: Int,
        isi: Double,
        pauseFloor: Double,
        settings: PauseDetectorSettings
    ) -> ClassicAnchorCandidate? {
        guard train.timestampsSec.indices.contains(index - 1),
              train.timestampsSec.indices.contains(index) else {
            return nil
        }

        let duration = train.timestampsSec[index] - train.timestampsSec[index - 1]
        let score = isi / max(pauseFloor, settings.minValidISISec)
        let isStrong = isi >= max(settings.strongThresholdSec, pauseFloor) -
            tolerance(for: max(settings.strongThresholdSec, pauseFloor))
        let decisionPath = [
            "train_pause_floor_monotonic_completion",
            "source=selected_pause_floor",
            "pause_floor_sec=\(format(pauseFloor))",
            "isi_sec=\(format(isi))",
            "rule=if_same_train_pause_min_established_then_larger_unoccupied_isi_is_pause",
            "occupied_by_selected_hard_event=false",
            "blocked_by_external_qc_or_manual_constraint=false",
            "state_and_review_tracks_do_not_block_gap_evidence_generation=true",
            "state_track_may_override_gap_in_final_arbitration=true"
        ].joined(separator: ";")

        return ClassicAnchorCandidate(
            id: "\(train.id)-pause-monotonic-floor-\(index)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "train_pause_floor_completion",
            candidateClass: "single_isi_pause_floor_completion",
            finalLabel: .pause,
            gateStatus: isStrong ? "pause_floor_completion_strong_pass" : "pause_floor_completion_pass",
            decisionPath: decisionPath,
            action: "accept",
            score: score,
            priority: isStrong ? 940 : 910,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: index,
            endISIIndex: index,
            startSpikeIndex: index,
            endSpikeIndex: index + 1,
            nISI: 1,
            nValidISI: 1,
            nSpikes: 2,
            durationSec: duration.isFinite ? duration : isi,
            intraQ10Sec: isi,
            intraQ40Sec: isi,
            intraQ50Sec: isi,
            intraQ90Sec: isi,
            intraQ95Sec: isi,
            maxIntraISISec: isi,
            meanIntraISISec: isi,
            cv: nil,
            lv: nil,
            preGapSec: finiteValidISI(index > 1 ? train.isiSec[index - 1] : nil, settings: settings),
            postGapSec: finiteValidISI(index < train.isiSec.count - 1 ? train.isiSec[index + 1] : nil, settings: settings),
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: score,
            edgeContrastGeomQ90: score,
            anchorFamily: "pause",
            anchorLockLevel: isStrong ? .lockedClassic : .strongCandidate,
            anchorBandLowerSec: pauseFloor,
            anchorBandUpperSec: max(settings.displayUpperSec, pauseFloor, isi),
            anchorBandSource: .structure,
            anchorContrastMinRequired: 1,
            anchorContrastGeomRequired: 1,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil
        )
    }

    private static func finiteValidISI(_ value: Double?, settings: PauseDetectorSettings) -> Double? {
        guard let value,
              value.isFinite,
              value >= settings.minValidISISec - tolerance(for: settings.minValidISISec) else {
            return nil
        }
        return value
    }

    /// R `event_core_pause_gap` relative criteria (R/38_event_grammar_core.R:843-845): an ISI is a pause
    /// only when it exceeds the local median by `eventCoreLocalFactor` and the global median by
    /// `eventCoreGlobalFactor`. The local median excludes the floor-exceeding ISIs (R `exclude_idx`) so it
    /// reflects the surrounding baseline, with a fallback excluding only the center index.
    private static func exceedsLocalAndGlobalBackground(
        isi: Double,
        centerIndex: Int,
        train: SpikeTrain,
        baseLongIndices: Set<Int>,
        globalThreshold: Double?,
        settings: PauseDetectorSettings
    ) -> Bool {
        let localMedian = localMedianISI(
            train.isiSec,
            centerIndex: centerIndex,
            window: settings.localWindow,
            excluding: baseLongIndices,
            settings: settings
        ) ?? localMedianISI(
            train.isiSec,
            centerIndex: centerIndex,
            window: settings.localWindow,
            excluding: [centerIndex],
            settings: settings
        )
        let localOK = localMedian.map {
            isi >= $0 * settings.eventCoreLocalFactor - tolerance(for: $0 * settings.eventCoreLocalFactor)
        } ?? true
        let globalOK = globalThreshold.map { isi >= $0 - tolerance(for: $0) } ?? true
        return localOK && globalOK
    }

    private static func validTrainISIs(_ train: SpikeTrain, settings: PauseDetectorSettings) -> [Double] {
        train.isiSec.indices.compactMap { index in
            index > 0 ? finiteValidISI(train.isiSec[index], settings: settings) : nil
        }
    }

    private static func localMedianISI(
        _ values: [Double?],
        centerIndex: Int,
        window: Int,
        excluding excludedIndices: Set<Int>,
        settings: PauseDetectorSettings
    ) -> Double? {
        guard !values.isEmpty else {
            return nil
        }
        let lower = max(1, centerIndex - window)
        let upper = min(values.count - 1, centerIndex + window)
        guard lower <= upper else {
            return nil
        }
        let localValues = (lower...upper).compactMap { index -> Double? in
            guard !excludedIndices.contains(index) else {
                return nil
            }
            return finiteValidISI(values[index], settings: settings)
        }
        return median(localValues)
    }

    private static func median(_ values: [Double]) -> Double? {
        quantile(values, probability: 0.5)
    }

    private static func format(_ value: Double) -> String {
        guard value.isFinite else {
            return "NA"
        }
        return String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), value)
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

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }
}
