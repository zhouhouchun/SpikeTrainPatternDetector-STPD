import Foundation

/// Applies the approved Tonic-family and HFS support contracts after event/gap topology is frozen.
///
/// Candidate generation remains parallel and evidence-preserving. This resolver is intentionally later:
/// selected Burst/Pause interruptions must be removed before `n_support`, `n_core`, CV, CV2, and LV become
/// authoritative. It never crosses an interruption when constructing CV2/LV adjacency.
public enum StateSupportAuthorityResolver {
    public static func apply(
        to candidates: [ClassicAnchorCandidate],
        train: SpikeTrain,
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate],
        settings: StatePatternDetectorSettings
    ) -> [ClassicAnchorCandidate] {
        candidates.flatMap { candidate -> [ClassicAnchorCandidate] in
            guard candidate.trainID == train.id,
                  candidate.isEligibleForAutoSelection else {
                return [candidate]
            }
            switch candidate.finalLabel {
            case .tonic, .highFrequencyTonic:
                guard candidate.stateDirectSupportSpans.isEmpty ||
                        !candidate.decisionPath.contains("state_support_policy=enforced") else {
                    return [candidate]
                }
                return resolveTonicFamily(
                    candidate,
                    train: train,
                    selectedEvents: selectedEvents,
                    selectedGaps: selectedGaps,
                    settings: settings
                )
            case .highFrequencySpiking:
                guard !candidate.decisionPath.contains("hfs_direct_support_policy=enforced") else {
                    return [candidate]
                }
                return [resolveHighFrequencySpiking(
                    candidate,
                    train: train,
                    selectedEvents: selectedEvents,
                    selectedGaps: selectedGaps,
                    settings: settings
                )]
            default:
                return [candidate]
            }
        }
    }

    private static func resolveTonicFamily(
        _ candidate: ClassicAnchorCandidate,
        train: SpikeTrain,
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate],
        settings: StatePatternDetectorSettings
    ) -> [ClassicAnchorCandidate] {
        let lower = min(candidate.startISIIndex, candidate.endISIIndex)
        let upper = max(candidate.startISIIndex, candidate.endISIIndex)
        guard lower > 0, lower <= upper, upper < train.isiSec.count else {
            return [demote(candidate, reason: "invalid_state_support_geometry", tokens: [])]
        }

        let interruptionGeometry = frozenInterruptionGeometry(
            lower: lower,
            upper: upper,
            trainID: candidate.trainID,
            selectedEvents: selectedEvents,
            selectedGaps: selectedGaps
        )
        guard interruptionGeometry.missingBurstCoreIDs.isEmpty else {
            return [demote(
                candidate,
                reason: "missing_frozen_burst_core_geometry",
                tokens: [
                    "state_missing_burst_core_ids=\(interruptionGeometry.missingBurstCoreIDs.joined(separator: ","))",
                    "state_event_core_geometry=burst_seed_run",
                    "state_pause_geometry=full_gap"
                ]
            )]
        }
        var recognizedInterruptionIndices = Set<Int>()
        for interruption in interruptionGeometry.interruptions {
            recognizedInterruptionIndices.formUnion(interruption.range)
        }

        let supportSettings = StateSupportClassifierSettings(
            minimumValidISISec: settings.minValidISISec
        )
        let observations = (lower...upper).map { index in
            StateSupportISIObservation(
                sourceIndex: index,
                valueSec: recognizedInterruptionIndices.contains(index) ? nil : train.isiSec[index]
            )
        }
        let support = StateSupportClassifier.analyze(observations, settings: supportSettings)
        let eligible = support.isEligibleForAutomaticTonic(
            settings: supportSettings,
            recognizedInterruptionCount: recognizedInterruptionIndices.count
        )

        let directIndices = support.observations.compactMap { observation -> Int? in
            switch observation.classification {
            case .core, .ordinaryDeviation: return observation.sourceIndex
            case .invalid, .competingExcursion: return nil
            }
        }
        let directSpans = contiguousSpans(directIndices, trainID: train.id, family: .tonic)
        let directSet = Set(directIndices)
        let interruptionIndices = (lower...upper).filter { !directSet.contains($0) }
        let interruptionSpans = contiguousSpans(interruptionIndices, trainID: train.id, family: .tonic)
        let adjacentPairCount = directSpans.reduce(0) { $0 + max(0, $1.rawISICount - 1) }
        let metrics = directMetrics(
            train: train,
            directSpans: directSpans,
            minimumValidISISec: settings.minValidISISec
        )
        let interruptionIDs = interruptionGeometry.interruptions.map(\.id).sorted()
        let tokens = [
            "state_support_policy=enforced",
            "state_support_scope=direct_support_after_frozen_event_gap_topology",
            "state_n_raw=\(support.nRaw)",
            "state_n_valid=\(support.nValid)",
            "state_n_support=\(support.nSupport)",
            "state_n_core=\(support.nCore)",
            "state_ordinary_deviations=\(support.ordinaryDeviationCount)",
            "state_ordinary_deviation_audit_warning=\(support.exceedsOrdinaryDeviationAuditTolerance(settings: supportSettings))",
            "state_competing_excursions=\(support.competingExcursionCount)",
            "state_invalid_slots=\(support.invalidCount)",
            "state_recognized_interruption_isi_count=\(recognizedInterruptionIndices.count)",
            "state_recognized_interruption_ids=\(interruptionIDs.isEmpty ? "none" : interruptionIDs.joined(separator: ","))",
            "state_event_core_geometry=burst_seed_run",
            "state_pause_geometry=full_gap",
            "state_support_auto_eligible=\(eligible)",
            "state_metrics_scope=direct_support",
            "state_cv2_lv_cross_interruptions=false",
            "state_support_cv=\(formatted(metrics?.cv))",
            "state_support_cv2=\(formatted(metrics?.cv2))",
            "state_support_lv=\(formatted(metrics?.lv))",
            "mixed_envelope_cv=\(formatted(candidate.cv))",
            "mixed_envelope_cv2=\(formatted(candidate.cv2))",
            "mixed_envelope_lv=\(formatted(candidate.lv))"
        ]
        let decisionPath = appending(tokens: tokens, to: candidate.decisionPath)
        var projected: ClassicAnchorCandidate
        if eligible, let metrics {
            projected = candidate.withDiagnosticOverride(
                decisionPath: decisionPath,
                stateSupportCV: metrics.cv,
                stateSupportCV2: metrics.cv2,
                stateSupportLV: metrics.lv,
                replaceStateSupportMetrics: true,
                selectedForAuto: candidate.selectedForAuto,
                selectionStatus: candidate.selectionStatus
            )
        } else if let trimRange = edgeTrimRange(
            support: support,
            lower: lower,
            upper: upper,
            settings: supportSettings,
            alreadyTrimmed: candidate.decisionPath.contains("state_support_edge_trim=true")
        ), let rebuilt = StatePatternDetector.rebuildSplitCandidate(
            train: train,
            parent: candidate,
            range: trimRange,
            settings: settings,
            splitByCandidateIDs: ["state_support_edge_trim"]
        ) {
            let trimTokens = tokens + [
                "state_support_edge_trim=true",
                "state_support_original_span=\(lower)-\(upper)",
                "state_support_trimmed_span=\(trimRange.lowerBound)-\(trimRange.upperBound)"
            ]
            var parentAudit = demote(
                candidate,
                reason: "state_support_edge_deviation_trimmed",
                tokens: trimTokens,
                metrics: metrics
            )
            parentAudit.stateDirectSupportSpans = directSpans
            parentAudit.stateInterruptionSpans = interruptionSpans
            parentAudit.stateDirectSupportISICount = directIndices.count
            parentAudit.stateDirectSupportAdjacentPairCount = adjacentPairCount

            let rebuiltPath = appending(
                tokens: [
                    "state_support_edge_trim=true",
                    "state_support_original_span=\(lower)-\(upper)",
                    "state_support_trimmed_span=\(trimRange.lowerBound)-\(trimRange.upperBound)"
                ],
                to: rebuilt.decisionPath
            )
            let trimmed = rebuilt.withDiagnosticOverride(
                decisionPath: rebuiltPath,
                selectedForAuto: rebuilt.selectedForAuto,
                selectionStatus: rebuilt.selectionStatus
            )
            return [parentAudit] + resolveTonicFamily(
                trimmed,
                train: train,
                selectedEvents: selectedEvents,
                selectedGaps: selectedGaps,
                settings: settings
            )
        } else {
            projected = demote(
                candidate,
                reason: "state_support_contract_not_satisfied",
                tokens: tokens,
                metrics: metrics
            )
        }
        projected.stateDirectSupportSpans = directSpans
        projected.stateInterruptionSpans = interruptionSpans
        projected.stateDirectSupportISICount = directIndices.count
        projected.stateDirectSupportAdjacentPairCount = adjacentPairCount
        return [projected]
    }

    /// At most one observation at each edge may be trimmed when it lies inside the ordinary-deviation
    /// band around the frozen core median. A far competing excursion, an interior excursion, or a later
    /// trim request remains review-only; this prevents iterative trimming from hiding a competing run.
    private static func edgeTrimRange(
        support: StateSupportAnalysis,
        lower: Int,
        upper: Int,
        settings: StateSupportClassifierSettings,
        alreadyTrimmed: Bool
    ) -> ClosedRange<Int>? {
        guard !alreadyTrimmed,
              let centre = support.coreMedianSec,
              centre.isFinite,
              centre > 0 else { return nil }

        func isTrimmable(_ observation: StateSupportAnalysis.ClassifiedObservation) -> Bool {
            guard observation.classification == .competingExcursion,
                  let value = observation.valueSec,
                  value.isFinite else { return false }
            return value >= centre * settings.ordinaryDeviationRatioLower - 1e-12 &&
                value <= centre * settings.ordinaryDeviationRatioUpper + 1e-12
        }

        var trimmedLower = lower
        var trimmedUpper = upper
        if let first = support.observations.first,
           first.sourceIndex == lower,
           isTrimmable(first) {
            trimmedLower += 1
        }
        if let last = support.observations.last,
           last.sourceIndex == upper,
           isTrimmable(last) {
            trimmedUpper -= 1
        }
        guard trimmedLower <= trimmedUpper,
              trimmedLower != lower || trimmedUpper != upper else {
            return nil
        }
        return trimmedLower...trimmedUpper
    }

    /// HFS has its own magnitude, duration, and packetization contract, so it must not be passed
    /// through the Tonic core/deviation classifier. This projection preserves the detector's HFS
    /// direct-support definition, removes every frozen event/gap core from it, and then recomputes
    /// the default state metrics without manufacturing adjacency across those interruptions.
    private static func resolveHighFrequencySpiking(
        _ candidate: ClassicAnchorCandidate,
        train: SpikeTrain,
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate],
        settings: StatePatternDetectorSettings
    ) -> ClassicAnchorCandidate {
        let lower = min(candidate.startISIIndex, candidate.endISIIndex)
        let upper = max(candidate.startISIIndex, candidate.endISIIndex)
        guard lower > 0, lower <= upper, upper < train.isiSec.count else {
            return demoteHFS(candidate, reason: "invalid_hfs_direct_support_geometry", tokens: [])
        }

        let interruptionGeometry = frozenInterruptionGeometry(
            lower: lower,
            upper: upper,
            trainID: candidate.trainID,
            selectedEvents: selectedEvents,
            selectedGaps: selectedGaps
        )
        guard interruptionGeometry.missingBurstCoreIDs.isEmpty else {
            return demoteHFS(
                candidate,
                reason: "missing_frozen_burst_core_geometry",
                tokens: [
                    "hfs_missing_burst_core_ids=\(interruptionGeometry.missingBurstCoreIDs.joined(separator: ","))",
                    "hfs_event_core_geometry=burst_seed_run",
                    "hfs_pause_geometry=full_gap"
                ]
            )
        }
        var recognizedInterruptionIndices = Set<Int>()
        for interruption in interruptionGeometry.interruptions {
            recognizedInterruptionIndices.formUnion(interruption.range)
        }

        let inheritedSpans = candidate.stateDirectSupportSpans.filter { span in
            span.trainID == candidate.trainID &&
                max(lower, span.startISIIndex) <= min(upper, span.endISIIndex)
        }
        guard let directUpper = candidate.hfSpikingShortUpperSec.flatMap({
            $0.isFinite && $0 > 0 ? $0 : nil
        }) ?? (candidate.anchorBandUpperSec.isFinite && candidate.anchorBandUpperSec > 0
            ? candidate.anchorBandUpperSec
            : nil) else {
            return demoteHFS(candidate, reason: "invalid_hfs_direct_support_upper", tokens: [])
        }
        let inheritedIndices: Set<Int>
        if inheritedSpans.isEmpty {
            inheritedIndices = Set((lower...upper).filter { index in
                guard let value = train.isiSec[index], value.isFinite else { return false }
                return value >= settings.minValidISISec &&
                    value <= directUpper + max(1e-12, abs(directUpper) * 1e-6)
            })
        } else {
            inheritedIndices = Set(inheritedSpans.flatMap { span in
                Array(max(lower, span.startISIIndex)...min(upper, span.endISIIndex))
            })
        }
        let directIndices = inheritedIndices
            .subtracting(recognizedInterruptionIndices)
            .filter { index in
                guard let value = train.isiSec[index] else { return false }
                return value.isFinite &&
                    value >= settings.minValidISISec &&
                    value <= directUpper + max(1e-12, abs(directUpper) * 1e-6)
            }
            .sorted()
        let directSet = Set(directIndices)
        let directSpans = contiguousSpans(directIndices, trainID: train.id, family: .unknown)
        let interruptionSpans = contiguousSpans(
            (lower...upper).filter { !directSet.contains($0) },
            trainID: train.id,
            family: .unknown
        )
        let adjacentPairCount = directSpans.reduce(0) { $0 + max(0, $1.rawISICount - 1) }
        let metrics = directMetrics(
            train: train,
            directSpans: directSpans,
            minimumValidISISec: settings.minValidISISec
        )

        let requiredSpikes = max(
            3,
            candidate.hfSpikingMinSpikesRequired ?? settings.highFrequencySpikingMinSpikes
        )
        let rawRequiredIndependentSupport = ceil(
            Double(requiredSpikes - 1) * settings.highFrequencySpikingShortFractionMin
        )
        let requiredIndependentSupport = !rawRequiredIndependentSupport.isFinite ||
            rawRequiredIndependentSupport >= Double(Int.max)
            ? Int.max
            : max(1, Int(rawRequiredIndependentSupport))
        let durationPass = candidate.durationSec.map {
            $0 >= settings.highFrequencySpikingMinDurationSec -
                max(1e-12, abs(settings.highFrequencySpikingMinDurationSec) * 1e-6)
        } ?? false
        let directSupportPass = directIndices.count >= requiredIndependentSupport
        let eligible = metrics != nil && durationPass && directSupportPass
        let interruptionIDs = interruptionGeometry.interruptions.map(\.id).sorted()
        let tokens = [
            "hfs_direct_support_policy=enforced",
            "hfs_direct_support_scope=detector_support_minus_frozen_event_gap_cores",
            "hfs_event_core_metrics_excluded=true",
            "hfs_cv2_lv_cross_interruptions=false",
            "hfs_direct_support_isi_count=\(directIndices.count)",
            "hfs_direct_support_adjacent_pair_count=\(adjacentPairCount)",
            "hfs_independent_support_required=\(requiredIndependentSupport)",
            "hfs_independent_support_pass=\(directSupportPass)",
            "hfs_duration_pass=\(durationPass)",
            "hfs_recognized_interruption_isi_count=\(recognizedInterruptionIndices.count)",
            "hfs_recognized_interruption_ids=\(interruptionIDs.isEmpty ? "none" : interruptionIDs.joined(separator: ","))",
            "hfs_event_core_geometry=burst_seed_run",
            "hfs_pause_geometry=full_gap",
            "hfs_direct_support_cv=\(formatted(metrics?.cv))",
            "hfs_direct_support_cv2=\(formatted(metrics?.cv2))",
            "hfs_direct_support_lv=\(formatted(metrics?.lv))",
            "mixed_envelope_cv=\(formatted(candidate.cv))",
            "mixed_envelope_cv2=\(formatted(candidate.cv2))",
            "mixed_envelope_lv=\(formatted(candidate.lv))"
        ]
        let decisionPath = appending(tokens: tokens, to: candidate.decisionPath)
        var projected: ClassicAnchorCandidate
        if eligible, let metrics {
            projected = candidate.withDiagnosticOverride(
                decisionPath: decisionPath,
                stateSupportCV: metrics.cv,
                stateSupportCV2: metrics.cv2,
                stateSupportLV: metrics.lv,
                replaceStateSupportMetrics: true,
                selectedForAuto: candidate.selectedForAuto,
                selectionStatus: candidate.selectionStatus
            )
        } else {
            projected = demoteHFS(
                candidate,
                reason: "hfs_independent_direct_support_not_satisfied",
                tokens: tokens,
                metrics: metrics
            )
        }
        projected.stateDirectSupportSpans = directSpans
        projected.stateInterruptionSpans = interruptionSpans
        projected.stateDirectSupportISICount = directIndices.count
        projected.stateDirectSupportAdjacentPairCount = adjacentPairCount
        return projected
    }

    private struct FrozenInterruption {
        let id: String
        let range: ClosedRange<Int>
    }

    private struct FrozenInterruptionGeometry {
        let interruptions: [FrozenInterruption]
        let missingBurstCoreIDs: [String]
    }

    /// State support excludes the frozen core of Burst-family events, not their wider boundary/bridge
    /// envelope. Pause candidates represent gap evidence directly, so their full selected span is excluded.
    /// A selected Burst without valid core geometry makes direct-support metrics underdetermined and therefore
    /// fails closed to review instead of silently treating the whole event envelope as its core.
    private static func frozenInterruptionGeometry(
        lower: Int,
        upper: Int,
        trainID: String,
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate]
    ) -> FrozenInterruptionGeometry {
        let selected = (selectedEvents + selectedGaps).filter {
            $0.trainID == trainID && $0.selectedForAuto && $0.isEligibleForAutoSelection
        }
        var interruptions: [FrozenInterruption] = []
        var missingBurstCoreIDs: [String] = []

        for candidate in selected {
            let envelopeLower = min(candidate.startISIIndex, candidate.endISIIndex)
            let envelopeUpper = max(candidate.startISIIndex, candidate.endISIIndex)
            guard max(lower, envelopeLower) <= min(upper, envelopeUpper) else { continue }

            let sourceRange: ClosedRange<Int>
            if candidate.finalLabel.isBurstEventFamily {
                guard let rawCoreStart = candidate.burstSeedRunStartISI,
                      let rawCoreEnd = candidate.burstSeedRunEndISI else {
                    missingBurstCoreIDs.append(candidate.id)
                    continue
                }
                let coreLower = min(rawCoreStart, rawCoreEnd)
                let coreUpper = max(rawCoreStart, rawCoreEnd)
                guard coreLower >= envelopeLower, coreUpper <= envelopeUpper else {
                    missingBurstCoreIDs.append(candidate.id)
                    continue
                }
                sourceRange = coreLower...coreUpper
            } else if candidate.finalLabel == .pause {
                sourceRange = envelopeLower...envelopeUpper
            } else {
                continue
            }

            let clippedLower = max(lower, sourceRange.lowerBound)
            let clippedUpper = min(upper, sourceRange.upperBound)
            if clippedLower <= clippedUpper {
                interruptions.append(FrozenInterruption(
                    id: candidate.id,
                    range: clippedLower...clippedUpper
                ))
            }
        }

        return FrozenInterruptionGeometry(
            interruptions: interruptions.sorted {
                ($0.range.lowerBound, $0.range.upperBound, $0.id) <
                    ($1.range.lowerBound, $1.range.upperBound, $1.id)
            },
            missingBurstCoreIDs: Array(Set(missingBurstCoreIDs)).sorted()
        )
    }

    private struct DirectMetrics {
        let cv: Double?
        let cv2: Double?
        let lv: Double?
    }

    private static func directMetrics(
        train: SpikeTrain,
        directSpans: [ISISpan],
        minimumValidISISec: Double
    ) -> DirectMetrics? {
        let segments = directSpans.map { span in
            (span.startISIIndex...span.endISIIndex).compactMap { index -> Double? in
                guard let value = train.isiSec[index] else { return nil }
                guard value.isFinite, value >= minimumValidISISec else { return nil }
                return value
            }
        }
        let values = segments.flatMap { $0 }
        guard !values.isEmpty else { return nil }
        var cv2Terms: [Double] = []
        var lvTerms: [Double] = []
        for segment in segments {
            for (left, right) in zip(segment, segment.dropFirst()) {
                let denominator = left + right
                guard denominator > 0 else { continue }
                cv2Terms.append(2 * abs(right - left) / denominator)
                lvTerms.append(3 * pow(right - left, 2) / pow(denominator, 2))
            }
        }
        return DirectMetrics(
            cv: STPDStatistics.coefficientOfVariation(values),
            cv2: cv2Terms.isEmpty ? nil : cv2Terms.reduce(0, +) / Double(cv2Terms.count),
            lv: lvTerms.isEmpty ? nil : lvTerms.reduce(0, +) / Double(lvTerms.count)
        )
    }

    private static func contiguousSpans(
        _ indices: [Int],
        trainID: String,
        family: ISIPatternFamily
    ) -> [ISISpan] {
        let sorted = Array(Set(indices)).sorted()
        guard var start = sorted.first else { return [] }
        var previous = start
        var spans: [ISISpan] = []
        for index in sorted.dropFirst() {
            if index == previous + 1 {
                previous = index
                continue
            }
            spans.append(ISISpan(
                trainID: trainID, startISIIndex: start, endISIIndex: previous, familyHint: family
            ))
            start = index
            previous = index
        }
        spans.append(ISISpan(
            trainID: trainID, startISIIndex: start, endISIIndex: previous, familyHint: family
        ))
        return spans
    }

    private static func demote(
        _ candidate: ClassicAnchorCandidate,
        reason: String,
        tokens: [String],
        metrics: DirectMetrics? = nil
    ) -> ClassicAnchorCandidate {
        let decisionPath = appending(
            tokens: ["proposed_state_label=\(candidate.finalLabel.rawValue)", "review_reason=\(reason)"] + tokens,
            to: candidate.decisionPath
        )
        return candidate.withDiagnosticOverride(
            finalLabel: .reject,
            gateStatus: "state_support_review_required",
            decisionPath: decisionPath,
            action: "audit_only",
            score: 0,
            priority: 0,
            stateSupportCV: metrics?.cv,
            stateSupportCV2: metrics?.cv2,
            stateSupportLV: metrics?.lv,
            replaceStateSupportMetrics: true,
            selectedForAuto: false,
            selectionStatus: "not_selected__state_support_review_required"
        )
    }

    private static func demoteHFS(
        _ candidate: ClassicAnchorCandidate,
        reason: String,
        tokens: [String],
        metrics: DirectMetrics? = nil
    ) -> ClassicAnchorCandidate {
        let decisionPath = appending(
            tokens: ["proposed_state_label=\(candidate.finalLabel.rawValue)", "review_reason=\(reason)"] + tokens,
            to: candidate.decisionPath
        )
        return candidate.withDiagnosticOverride(
            finalLabel: .reject,
            gateStatus: "hfs_direct_support_review_required",
            decisionPath: decisionPath,
            action: "audit_only",
            score: 0,
            priority: 0,
            stateSupportCV: metrics?.cv,
            stateSupportCV2: metrics?.cv2,
            stateSupportLV: metrics?.lv,
            replaceStateSupportMetrics: true,
            selectedForAuto: false,
            selectionStatus: "not_selected__hfs_direct_support_review_required"
        )
    }

    private static func appending(tokens: [String], to path: String) -> String {
        // This resolver is the final owner of direct-support authority. Earlier candidate
        // generation may have emitted provisional `state_*` evidence with the same key, so
        // exact-string de-duplication is insufficient: it can leave both `audited` and
        // `enforced` values in one decision path. Replace by key here to keep one canonical
        // value while preserving unrelated upstream provenance in its original order.
        var parts = path.split(separator: ";").map(String.init)
        for token in tokens where !token.isEmpty {
            if let key = tokenKey(token) {
                parts.removeAll { tokenKey($0) == key }
                parts.append(token)
            } else if !parts.contains(token) {
                parts.append(token)
            }
        }
        return parts.joined(separator: ";")
    }

    private static func tokenKey(_ token: String) -> Substring? {
        guard let separator = token.firstIndex(of: "=") else { return nil }
        return token[..<separator]
    }

    private static func formatted(_ value: Double?) -> String {
        guard let value, value.isFinite else { return "NA" }
        return String(format: "%.12g", value)
    }
}
