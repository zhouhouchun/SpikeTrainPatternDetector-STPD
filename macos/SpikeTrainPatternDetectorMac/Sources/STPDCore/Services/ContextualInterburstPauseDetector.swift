import Foundation

/// Produces contextual/inter-burst Pause evidence only after Burst event topology
/// has been selected and frozen for the current resolution pass.
///
/// This detector never changes Burst candidates or their thresholds. It examines
/// only the single unoccupied ISI between two independent selected Burst cores.
/// A gap is contextual Pause evidence when it is sufficiently larger than both
/// flanking Burst cores and is not still explained by either core's bridge band.
/// Conflicting evidence is retained as an audit-only `ambiguous_gap` candidate.
public enum ContextualInterburstPauseDetector {
    public static func detect(
        train: SpikeTrain,
        frozenBurstCandidates: [ClassicAnchorCandidate],
        settings: PauseDetectorSettings,
        existingCandidates: [ClassicAnchorCandidate] = []
    ) -> [ClassicAnchorCandidate] {
        guard settings.isEnabled, train.spikeCount >= 2 else { return [] }

        let bursts = frozenBurstCandidates
            .filter {
                $0.trainID == train.id &&
                    $0.selectedForAuto &&
                    $0.isEligibleForAutoSelection &&
                    $0.arbitrationTrack == .event &&
                    $0.finalLabel.isCanonicalBurstFamily
            }
            .compactMap(frozenBurstCore)
            .sorted(by: burstCoreOrder)
        guard bursts.count >= 2 else { return [] }

        var candidates: [ClassicAnchorCandidate] = []
        for pairIndex in 0..<(bursts.count - 1) {
            let leftCore = bursts[pairIndex]
            let rightCore = bursts[pairIndex + 1]
            let left = leftCore.candidate
            let right = rightCore.candidate
            let leftEnd = leftCore.range.upperBound
            let rightStart = rightCore.range.lowerBound

            // One physical ISI must be the complete gap between the two cores.
            // Wider intervening activity is not silently collapsed into one Pause.
            guard rightStart - leftEnd == 2 else { continue }
            let gapIndex = leftEnd + 1
            guard let gap = validISI(train: train, at: gapIndex, floor: settings.minValidISISec),
                  let leftReference = burstReferenceUpper(
                    train: train, core: leftCore.range, floor: settings.minValidISISec
                  ),
                  let rightReference = burstReferenceUpper(
                    train: train, core: rightCore.range, floor: settings.minValidISISec
                  ) else {
                continue
            }
            // Contextual Pause may legitimately be shorter than the ordinary adaptive Pause floor,
            // but it must never bypass a user-confirmed absolute hard gate.
            if let hardFloor = settings.manualHardLowerSec,
               gap < hardFloor - tolerance(for: hardFloor) {
                continue
            }

            // A previously established canonical Pause remains the authoritative
            // hard boundary. Contextual evidence must not duplicate or downgrade it.
            if existingCandidates.contains(where: {
                $0.trainID == train.id &&
                    $0.finalLabel == .pause &&
                    $0.pauseBoundaryRole == .canonicalPauseAnchor &&
                    $0.selectedForAuto &&
                    $0.isEligibleForAutoSelection &&
                    min($0.startISIIndex, $0.endISIIndex) == gapIndex &&
                    max($0.startISIIndex, $0.endISIIndex) == gapIndex
            }) {
                continue
            }

            let leftContrast = gap / leftReference
            let rightContrast = gap / rightReference
            guard leftContrast.isFinite, rightContrast.isFinite else { continue }
            let bilateralContrast = min(leftContrast, rightContrast)
            let requiredContrast = settings.classicBurstFlankPauseContrastMin
            let pauseContrastPass = bilateralContrast >= requiredContrast - tolerance(for: requiredContrast)

            let leftBridgeUpper = burstBridgeUpper(left)
            let rightBridgeUpper = burstBridgeUpper(right)
            let bridgeUpper = max(leftBridgeUpper, rightBridgeUpper)
            let bridgeExplainsGap = gap <= bridgeUpper + tolerance(for: bridgeUpper)

            // When bridge support is positive and Pause contrast is negative, the
            // frozen Burst topology explains the gap; do not manufacture Pause.
            guard pauseContrastPass || !bridgeExplainsGap else { continue }

            // Both-positive and both-negative results are genuinely conflicting or
            // under-separated evidence. Preserve them for review without assigning
            // a biological Pause role or allowing automatic selection.
            let ambiguous = pauseContrastPass == bridgeExplainsGap
            candidates.append(
                makeCandidate(
                    train: train,
                    gapIndex: gapIndex,
                    gap: gap,
                    left: left,
                    right: right,
                    leftCore: leftCore.range,
                    rightCore: rightCore.range,
                    leftReference: leftReference,
                    rightReference: rightReference,
                    leftContrast: leftContrast,
                    rightContrast: rightContrast,
                    requiredContrast: requiredContrast,
                    bridgeUpper: bridgeUpper,
                    bridgeExplainsGap: bridgeExplainsGap,
                    ambiguous: ambiguous
                )
            )
        }
        return candidates
    }

    private static func makeCandidate(
        train: SpikeTrain,
        gapIndex: Int,
        gap: Double,
        left: ClassicAnchorCandidate,
        right: ClassicAnchorCandidate,
        leftCore: ClosedRange<Int>,
        rightCore: ClosedRange<Int>,
        leftReference: Double,
        rightReference: Double,
        leftContrast: Double,
        rightContrast: Double,
        requiredContrast: Double,
        bridgeUpper: Double,
        bridgeExplainsGap: Bool,
        ambiguous: Bool
    ) -> ClassicAnchorCandidate {
        let bilateralContrast = min(leftContrast, rightContrast)
        let geometricContrast = sqrt(max(0, leftContrast * rightContrast))
        let layer = ambiguous ? "contextual_interburst_gap_review" : "contextual_interburst_pause"
        let candidateClass = ambiguous ? "ambiguous_gap" : "contextual_interburst_pause"
        let decisionPath = [
            ambiguous ? "ambiguous_gap" : "contextual_interburst_pause",
            "pause_boundary_role=\(ambiguous ? "unassigned" : PauseBoundaryRole.contextualPause.rawValue)",
            "burst_topology_frozen=true",
            "burst_support_reestimated=false",
            "left_burst_id=\(left.id)",
            "right_burst_id=\(right.id)",
            "left_burst_core_span=\(leftCore.lowerBound)-\(leftCore.upperBound)",
            "right_burst_core_span=\(rightCore.lowerBound)-\(rightCore.upperBound)",
            "burst_reference_scope=frozen_core_q90",
            "gap_isi_index=\(gapIndex)",
            "gap_sec=\(format(gap))",
            "left_burst_reference_sec=\(format(leftReference))",
            "right_burst_reference_sec=\(format(rightReference))",
            "left_contrast=\(format(leftContrast))",
            "right_contrast=\(format(rightContrast))",
            "bilateral_contrast=\(format(bilateralContrast))",
            "required_bilateral_contrast=\(format(requiredContrast))",
            "burst_bridge_upper_sec=\(format(bridgeUpper))",
            "bridge_explains_gap=\(bridgeExplainsGap)",
            "single_unoccupied_interburst_isi=true"
        ].joined(separator: ";")

        return ClassicAnchorCandidate(
            id: "\(train.id)-\(layer)-\(gapIndex)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: layer,
            candidateClass: candidateClass,
            finalLabel: .pause,
            gateStatus: ambiguous ? "contextual_interburst_gap_ambiguous" : "contextual_interburst_pause_pass",
            decisionPath: decisionPath,
            action: ambiguous ? "audit_only" : "accept",
            score: log1p(gap) + log(max(1, geometricContrast)),
            priority: ambiguous ? 0 : 1_060,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: gapIndex,
            endISIIndex: gapIndex,
            startSpikeIndex: gapIndex,
            endSpikeIndex: gapIndex + 1,
            nISI: 1,
            nValidISI: 1,
            nSpikes: 2,
            durationSec: gap,
            intraQ10Sec: gap,
            intraQ40Sec: gap,
            intraQ50Sec: gap,
            intraQ90Sec: gap,
            intraQ95Sec: gap,
            maxIntraISISec: gap,
            meanIntraISISec: gap,
            cv: nil,
            lv: nil,
            preGapSec: validISI(train: train, at: gapIndex - 1, floor: 0),
            postGapSec: validISI(train: train, at: gapIndex + 1, floor: 0),
            preRatioQ90: leftContrast,
            postRatioQ90: rightContrast,
            edgeContrastMinQ90: bilateralContrast,
            edgeContrastGeomQ90: geometricContrast,
            anchorFamily: "pause",
            anchorLockLevel: ambiguous ? .auditOnly : .strongCandidate,
            anchorBandLowerSec: min(leftReference, rightReference),
            anchorBandUpperSec: max(gap, bridgeUpper),
            anchorBandSource: .structure,
            anchorContrastMinRequired: requiredContrast,
            anchorContrastGeomRequired: requiredContrast,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            pauseBoundaryRole: ambiguous ? nil : .contextualPause
        )
    }

    private struct FrozenBurstCore {
        let candidate: ClassicAnchorCandidate
        let range: ClosedRange<Int>
    }

    private static func frozenBurstCore(_ candidate: ClassicAnchorCandidate) -> FrozenBurstCore? {
        guard let rawStart = candidate.burstSeedRunStartISI,
              let rawEnd = candidate.burstSeedRunEndISI else { return nil }
        let lower = min(rawStart, rawEnd)
        let upper = max(rawStart, rawEnd)
        let envelopeLower = min(candidate.startISIIndex, candidate.endISIIndex)
        let envelopeUpper = max(candidate.startISIIndex, candidate.endISIIndex)
        guard lower >= envelopeLower, upper <= envelopeUpper else { return nil }
        return FrozenBurstCore(candidate: candidate, range: lower...upper)
    }

    private static func burstReferenceUpper(
        train: SpikeTrain,
        core: ClosedRange<Int>,
        floor: Double
    ) -> Double? {
        let values = core.compactMap { validISI(train: train, at: $0, floor: floor) }
        guard values.count == core.count else { return nil }
        return SortedFiniteSample(values, positiveOnly: true).quantile(0.90)
    }

    private static func burstBridgeUpper(_ candidate: ClassicAnchorCandidate) -> Double {
        let values = [
            candidate.burstBridgeBandUpperSec,
            candidate.burstSeedBandUpperSec,
            candidate.intraQ95Sec,
            candidate.intraQ90Sec
        ]
        .compactMap { $0 }
        .filter { $0.isFinite && $0 > 0 }
        if let upper = values.max() { return upper }
        return candidate.anchorBandUpperSec.isFinite && candidate.anchorBandUpperSec > 0
            ? candidate.anchorBandUpperSec
            : 0
    }

    private static func validISI(train: SpikeTrain, at index: Int, floor: Double) -> Double? {
        guard index > 0,
              train.isiSec.indices.contains(index),
              let value = train.isiSec[index],
              value.isFinite,
              value >= floor - tolerance(for: floor) else {
            return nil
        }
        return value
    }

    private static func burstCoreOrder(_ lhs: FrozenBurstCore, _ rhs: FrozenBurstCore) -> Bool {
        if lhs.range.lowerBound != rhs.range.lowerBound {
            return lhs.range.lowerBound < rhs.range.lowerBound
        }
        if lhs.range.upperBound != rhs.range.upperBound {
            return lhs.range.upperBound < rhs.range.upperBound
        }
        return lhs.candidate.id < rhs.candidate.id
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }

    private static func format(_ value: Double) -> String {
        String(format: "%.6g", value)
    }

}
