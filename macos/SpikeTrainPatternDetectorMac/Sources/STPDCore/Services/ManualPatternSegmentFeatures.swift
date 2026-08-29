import Foundation

public struct ManualPatternOverlayCount: Hashable, Sendable {
    public let label: ManualAnnotationLabel
    public let isiCount: Int

    public init(label: ManualAnnotationLabel, isiCount: Int) {
        self.label = label
        self.isiCount = isiCount
    }
}

public struct ManualPatternISIDistribution: Hashable, Sendable {
    public let sampleCount: Int
    public let tailQuantileStanding: ManualPatternTailQuantileStanding
    public let minimumMicroseconds: Int64?
    public let q10Microseconds: Double?
    public let medianMicroseconds: Double?
    public let q80Microseconds: Double?
    public let q90Microseconds: Double?
    public let q95Microseconds: Double?
    public let maximumMicroseconds: Int64?
    public let meanMicroseconds: Double?
}

public enum ManualPatternTailQuantileStanding: String, CaseIterable, Hashable, Sendable {
    /// Fewer than ten ISIs: q80/q90/q95 are shown only to audit the observed segment.
    case smallSampleDescriptive = "small_sample_descriptive"
    /// Ten or more ISIs: still descriptive; no single-segment tail quantile has threshold authority.
    case descriptiveOnly = "descriptive_only"
}

public enum ManualPatternMetricStanding: String, CaseIterable, Hashable, Sendable {
    case insufficient
    case computable
}

public struct ManualPatternRegularityFeatures: Hashable, Sendable {
    /// MM is computable only for one contiguous span of 2–4 ISIs (3–5 spikes).
    public let mmStanding: ManualPatternMetricStanding
    /// CV is computable from at least five direct-support ISIs, even across support spans.
    public let cvStanding: ManualPatternMetricStanding
    /// CV2/LV are computable only from at least four real source-adjacent pairs.
    public let cv2LVStanding: ManualPatternMetricStanding
    public let mm: Double?
    public let cv: Double?
    public let cv2: Double?
    public let lv: Double?
    public let cvISICount: Int
    /// Count of real source-adjacent direct-support pairs used by CV2/LV.
    public let adjacentPairCount: Int
}

public enum ManualPatternTrainReferenceStanding: String, CaseIterable, Hashable, Sendable {
    case insufficientReference = "insufficient_reference"
    case computable
    case degenerateScale = "degenerate_scale"
}

public struct ManualPatternTrainRelativeFeatures: Hashable, Sendable {
    /// This is a leave-current-segment-out whole-train reference, not biological background and not
    /// negative evidence. Unknown and other labeled ISIs remain part of the reference distribution.
    public let trainReferenceISICount: Int
    public let percentileStanding: ManualPatternTrainReferenceStanding
    public let robustLogZStanding: ManualPatternTrainReferenceStanding
    /// Median empirical percentile on the unit interval [0, 1].
    public let medianPercentile: Double?
    /// Median robust z score in log(ISI), using train-reference median and scaled MAD.
    public let medianRobustLogZ: Double?
}

public enum ManualPatternDurationStanding: String, CaseIterable, Hashable, Sendable {
    /// There is no direct-support interval whose duration could be measured.
    case insufficientSupport = "insufficient_support"
    case available
    case arithmeticOverflow = "arithmetic_overflow"
}

/// One stable record per contiguous manual State or Event segment. Raw geometry, direct numerical
/// support, excluded evidence, and cross-track overlays are deliberately separate.
public struct ManualPatternFeatureRecord: Identifiable, Hashable, Sendable {
    public let id: String
    /// Cross-platform digest over exact snapshot/segment identity only.
    public let sourceIdentityDigest: String
    /// Runtime-local reproducibility diagnostic over derived floating-point feature values. It is
    /// not a canonical cross-platform scientific identity.
    public let featureComputationDigest: String
    public let trainID: ScientificSpikeTrainID
    public let track: ManualAnnotationSemanticTrack
    public let label: ManualAnnotationLabel
    public let startISIIndex: Int
    public let endISIIndex: Int
    public let leftTick: MicrosecondTick
    public let rightTick: MicrosecondTick
    public let envelopeDurationMicroseconds: Int64?
    public let envelopeDurationStanding: ManualPatternDurationStanding
    public let directSupportDurationMicroseconds: Int64?
    public let directSupportDurationStanding: ManualPatternDurationStanding
    public let rawISICount: Int
    public let rawEnvelopeSpikeCount: Int
    public let validISICount: Int
    public let directSupportISICount: Int
    /// Sum of `(span ISI count + 1)` across independent direct-support spans.
    public let directSupportSpikeCount: Int
    public let directSupportSpanCount: Int
    public let excludedQCISICount: Int
    public let excludedEmbeddedEventISICount: Int
    public let excludedConflictISICount: Int
    /// Mutually exclusive union of all exclusions for this record.
    public let excludedAnyISICount: Int
    public let nonPositiveISICount: Int
    public let belowMinimumISICount: Int
    public let directSupportISI: ManualPatternISIDistribution
    public let regularity: ManualPatternRegularityFeatures
    public let trainRelative: ManualPatternTrainRelativeFeatures
    public let overlayStateCounts: [ManualPatternOverlayCount]
    public let overlayEventCounts: [ManualPatternOverlayCount]

    /// Whether the record has any conflict-free numerical support. This is not a sufficiency or
    /// authority decision for later parameter learning.
    public var hasUsableDirectSupport: Bool {
        directSupportISICount > 0
    }
}

/// Deterministic report-only table. It is not a learned threshold set and cannot alter a detector.
public struct ManualPatternFeatureTable: Hashable, Sendable {
    public let schemaContractID: String
    public let schemaContractDigest: String
    public let evidenceSnapshotDigest: String
    public let records: [ManualPatternFeatureRecord]
    /// Stable digest over exact snapshot and record source identities.
    public let sourceIdentityDigest: String
    /// Runtime-local diagnostic over derived floating-point outputs.
    public let featureComputationDigest: String
}

public enum ManualPatternSegmentFeatureExtractor {
    public static let schemaContractID =
        "manual_pattern_segment_features_dual_track_direct_support"
    public static let minimumTrainReferenceISICount = 30

    public static func extract(
        from snapshot: ManualLearningEvidenceSnapshot
    ) -> ManualPatternFeatureTable {
        var schemaTokens = [
            schemaContractID,
            "sha256_length_prefixed_utf8_tokens",
            "one_record_per_contiguous_positive_state_or_event_segment",
            "raw_geometry_retained",
            "qc_conflict_and_state_embedded_event_excluded_from_direct_support",
            "target_specific_conflicts_preserve_event_state_orthogonality",
            "mm_for_one_contiguous_span_of_two_through_four_direct_isis",
            "cv_for_at_least_five_direct_isis",
            "cv2_lv_for_at_least_four_real_direct_support_adjacent_pairs",
            "cv_sample_variance_welford",
            "cv2_standard_adjacent_pair_formula",
            "lv_standard_adjacent_pair_formula",
            "segment_tail_quantiles_are_descriptive_only",
            "tail_quantile_small_sample_boundary_ten_isis",
            "leave_segment_out_train_reference_is_not_biological_background",
            "relative_reference_requires_thirty_valid_reference_isis",
            "empirical_percentile_midrank_unit_interval",
            "robust_log_z_median_scaled_mad_1.4826",
            "zero_mad_has_degenerate_scale_standing",
            "envelope_and_direct_support_spike_count_duration_are_distinct",
            "missing_direct_support_duration_is_not_zero",
            "duration_arithmetic_overflow_is_explicit",
            "source_identity_digest_exact_feature_computation_digest_runtime_local",
            "report_only_no_detector_authority",
        ]
        ManualPatternTailQuantileStanding.allCases.forEach {
            schemaTokens.append("tail_quantile_standing:\($0.rawValue)")
        }
        ManualPatternMetricStanding.allCases.forEach {
            schemaTokens.append("metric_standing:\($0.rawValue)")
        }
        ManualPatternTrainReferenceStanding.allCases.forEach {
            schemaTokens.append("train_reference_standing:\($0.rawValue)")
        }
        ManualPatternDurationStanding.allCases.forEach {
            schemaTokens.append("duration_standing:\($0.rawValue)")
        }
        schemaTokens.append(contentsOf: ManualLearningEvidenceSchema.semanticRuleTokens)
        let schemaDigest = ManualLearningDigest.hex(tokens: schemaTokens)
        let groupedByTrain = Dictionary(grouping: snapshot.rows, by: \.trainID)
        var records: [ManualPatternFeatureRecord] = []
        for trainID in groupedByTrain.keys.sorted(by: trainOrdered) {
            let trainRows = (groupedByTrain[trainID] ?? []).sorted { $0.isiIndex < $1.isiIndex }
            let trainReference = TrainRelativeReference(rows: trainRows)
            records.append(contentsOf: extract(
                track: .state,
                trainRows: trainRows,
                trainReference: trainReference,
                snapshot: snapshot,
                schemaDigest: schemaDigest
            ))
            records.append(contentsOf: extract(
                track: .event,
                trainRows: trainRows,
                trainReference: trainReference,
                snapshot: snapshot,
                schemaDigest: schemaDigest
            ))
        }
        records.sort(by: recordOrdered)
        let sourceIdentityDigest = ManualLearningDigest.hex(tokens: [
            schemaContractID,
            schemaDigest,
            snapshot.digest,
            String(records.count),
        ] + records.map(\.sourceIdentityDigest))
        let featureComputationDigest = ManualLearningDigest.hex(tokens: [
            "runtime_local_manual_pattern_feature_computation",
            schemaDigest,
            sourceIdentityDigest,
        ] + records.map(\.featureComputationDigest))
        return ManualPatternFeatureTable(
            schemaContractID: schemaContractID,
            schemaContractDigest: schemaDigest,
            evidenceSnapshotDigest: snapshot.digest,
            records: records,
            sourceIdentityDigest: sourceIdentityDigest,
            featureComputationDigest: featureComputationDigest
        )
    }

    private static func extract(
        track: ManualAnnotationSemanticTrack,
        trainRows: [ManualLearningEvidenceRow],
        trainReference: TrainRelativeReference,
        snapshot: ManualLearningEvidenceSnapshot,
        schemaDigest: String
    ) -> [ManualPatternFeatureRecord] {
        var segments: [[ManualLearningEvidenceRow]] = []
        var current: [ManualLearningEvidenceRow] = []
        var currentLabel: ManualAnnotationLabel?
        for row in trainRows {
            let label = label(row, on: track)
            let continues = label != nil
                && label == currentLabel
                && current.last.map { $0.isiIndex + 1 == row.isiIndex } == true
            if continues {
                current.append(row)
            } else {
                if !current.isEmpty { segments.append(current) }
                if let label, label.polarity == .positive {
                    current = [row]
                    currentLabel = label
                } else {
                    current = []
                    currentLabel = nil
                }
            }
        }
        if !current.isEmpty { segments.append(current) }
        return segments.compactMap {
            makeRecord(
                rows: $0,
                track: track,
                trainReference: trainReference,
                snapshot: snapshot,
                schemaDigest: schemaDigest
            )
        }
    }

    private static func makeRecord(
        rows: [ManualLearningEvidenceRow],
        track: ManualAnnotationSemanticTrack,
        trainReference: TrainRelativeReference,
        snapshot: ManualLearningEvidenceSnapshot,
        schemaDigest: String
    ) -> ManualPatternFeatureRecord? {
        guard let first = rows.first,
              let last = rows.last,
              let patternLabel = label(first, on: track) else { return nil }
        let validCount = rows.count { $0.quality == .valid }
        let excludedQC = rows.count - validCount
        let nonPositiveCount = rows.count { $0.quality == .nonPositive }
        let belowMinimumCount = rows.count { $0.quality == .belowMinimum }
        let excludedEmbedded = track == .state
            ? rows.count { $0.quality == .valid && $0.eventLabel != nil }
            : 0
        let excludedConflict = rows.count {
            $0.quality == .valid
                && !(track == .state && $0.eventLabel != nil)
                && hasRelevantConflict($0, track: track, label: patternLabel)
        }
        let directRows = rows.filter { row in
            guard row.quality == .valid else { return false }
            if track == .state, row.eventLabel != nil { return false }
            return !hasRelevantConflict(row, track: track, label: patternLabel)
        }
        let directSpans = contiguousSpans(directRows)
        let values = directRows.map(\.intervalMicroseconds)
        let distribution = distribution(values)
        let regularity = regularity(values: values, spans: directSpans)
        let excludedReferenceValues = rows.filter { $0.quality == .valid }
            .map(\.intervalMicroseconds)
            .sorted()
        let relative = trainRelative(
            values: values,
            reference: trainReference,
            excludedReferenceValues: excludedReferenceValues
        )
        let envelopeDuration = duration(from: first.leftTick, to: last.rightTick)
        let directDuration = exactSum(values)
        let excludedAny = excludedQC + excludedEmbedded + excludedConflict
        let overlayStates = track == .event ? overlayCounts(rows.compactMap(\.stateLabel)) : []
        let overlayEvents = track == .state ? overlayCounts(rows.compactMap(\.eventLabel)) : []
        let identityTokens = [
            snapshot.digest,
            schemaDigest,
            first.trainID.semanticID.canonicalText,
            track.rawValue,
            patternLabel.rawValue,
            String(first.isiIndex),
            String(last.isiIndex),
        ]
        let sourceIdentityDigest = ManualLearningDigest.hex(tokens: [
            "manual_pattern_segment_identity",
        ] + identityTokens)
        var featureTokens: [String] = [
            "runtime_local_manual_pattern_segment_features",
            sourceIdentityDigest,
            String(first.leftTick.microseconds),
            String(last.rightTick.microseconds),
            optionalInt64(envelopeDuration.value),
            envelopeDuration.standing.rawValue,
            optionalInt64(directDuration.value),
            directDuration.standing.rawValue,
            String(rows.count),
            String(validCount),
            String(directRows.count),
            String(directRows.count + directSpans.count),
            String(directSpans.count),
            String(excludedQC),
            String(excludedEmbedded),
            String(excludedConflict),
            String(excludedAny),
            String(nonPositiveCount),
            String(belowMinimumCount),
        ]
        featureTokens.append(contentsOf: [
            String(distribution.sampleCount),
            distribution.tailQuantileStanding.rawValue,
            optionalInt64(distribution.minimumMicroseconds),
            optionalDouble(distribution.q10Microseconds),
            optionalDouble(distribution.medianMicroseconds),
            optionalDouble(distribution.q80Microseconds),
            optionalDouble(distribution.q90Microseconds),
            optionalDouble(distribution.q95Microseconds),
            optionalInt64(distribution.maximumMicroseconds),
            optionalDouble(distribution.meanMicroseconds),
            regularity.mmStanding.rawValue,
            regularity.cvStanding.rawValue,
            regularity.cv2LVStanding.rawValue,
            optionalDouble(regularity.mm),
            optionalDouble(regularity.cv),
            optionalDouble(regularity.cv2),
            optionalDouble(regularity.lv),
            String(regularity.cvISICount),
            String(regularity.adjacentPairCount),
            relative.percentileStanding.rawValue,
            relative.robustLogZStanding.rawValue,
            String(relative.trainReferenceISICount),
            optionalDouble(relative.medianPercentile),
            optionalDouble(relative.medianRobustLogZ),
        ])
        featureTokens.append(contentsOf: overlayStates.flatMap {
            [$0.label.rawValue, String($0.isiCount)]
        })
        featureTokens.append(contentsOf: overlayEvents.flatMap {
            [$0.label.rawValue, String($0.isiCount)]
        })
        return ManualPatternFeatureRecord(
            id: sourceIdentityDigest,
            sourceIdentityDigest: sourceIdentityDigest,
            featureComputationDigest: ManualLearningDigest.hex(tokens: featureTokens),
            trainID: first.trainID,
            track: track,
            label: patternLabel,
            startISIIndex: first.isiIndex,
            endISIIndex: last.isiIndex,
            leftTick: first.leftTick,
            rightTick: last.rightTick,
            envelopeDurationMicroseconds: envelopeDuration.value,
            envelopeDurationStanding: envelopeDuration.standing,
            directSupportDurationMicroseconds: directDuration.value,
            directSupportDurationStanding: directDuration.standing,
            rawISICount: rows.count,
            rawEnvelopeSpikeCount: rows.count + 1,
            validISICount: validCount,
            directSupportISICount: directRows.count,
            directSupportSpikeCount: directRows.count + directSpans.count,
            directSupportSpanCount: directSpans.count,
            excludedQCISICount: excludedQC,
            excludedEmbeddedEventISICount: excludedEmbedded,
            excludedConflictISICount: excludedConflict,
            excludedAnyISICount: excludedAny,
            nonPositiveISICount: nonPositiveCount,
            belowMinimumISICount: belowMinimumCount,
            directSupportISI: distribution,
            regularity: regularity,
            trainRelative: relative,
            overlayStateCounts: overlayStates,
            overlayEventCounts: overlayEvents
        )
    }

    private static func hasRelevantConflict(
        _ row: ManualLearningEvidenceRow,
        track: ManualAnnotationSemanticTrack,
        label: ManualAnnotationLabel
    ) -> Bool {
        row.conflicts.contains { $0.isRelevant(to: track, label: label) }
    }

    private struct DurationResult {
        let value: Int64?
        let standing: ManualPatternDurationStanding
    }

    private static func duration(
        from left: MicrosecondTick,
        to right: MicrosecondTick
    ) -> DurationResult {
        do {
            return DurationResult(
                value: try right.interval(since: left),
                standing: .available
            )
        } catch {
            return DurationResult(value: nil, standing: .arithmeticOverflow)
        }
    }

    private static func exactSum(_ values: [Int64]) -> DurationResult {
        guard !values.isEmpty else {
            return DurationResult(value: nil, standing: .insufficientSupport)
        }
        var total: Int64 = 0
        for value in values {
            let result = total.addingReportingOverflow(value)
            guard !result.overflow else {
                return DurationResult(value: nil, standing: .arithmeticOverflow)
            }
            total = result.partialValue
        }
        return DurationResult(value: total, standing: .available)
    }

    private static func label(
        _ row: ManualLearningEvidenceRow,
        on track: ManualAnnotationSemanticTrack
    ) -> ManualAnnotationLabel? {
        switch track {
        case .state: return row.stateLabel
        case .event: return row.eventLabel
        case .other: return row.otherLabel
        }
    }

    private static func contiguousSpans(
        _ rows: [ManualLearningEvidenceRow]
    ) -> [[ManualLearningEvidenceRow]] {
        var result: [[ManualLearningEvidenceRow]] = []
        for row in rows {
            if result.last?.last.map({ $0.isiIndex + 1 == row.isiIndex }) == true {
                result[result.count - 1].append(row)
            } else {
                result.append([row])
            }
        }
        return result
    }

    private static func distribution(_ values: [Int64]) -> ManualPatternISIDistribution {
        let sorted = values.sorted()
        return ManualPatternISIDistribution(
            sampleCount: values.count,
            tailQuantileStanding: values.count < 10
                ? .smallSampleDescriptive
                : .descriptiveOnly,
            minimumMicroseconds: sorted.first,
            q10Microseconds: quantile(sorted, probability: 0.10),
            medianMicroseconds: quantile(sorted, probability: 0.50),
            q80Microseconds: quantile(sorted, probability: 0.80),
            q90Microseconds: quantile(sorted, probability: 0.90),
            q95Microseconds: quantile(sorted, probability: 0.95),
            maximumMicroseconds: sorted.last,
            meanMicroseconds: values.isEmpty
                ? nil
                : values.reduce(0.0) { $0 + Double($1) } / Double(values.count)
        )
    }

    private static func regularity(
        values: [Int64],
        spans: [[ManualLearningEvidenceRow]]
    ) -> ManualPatternRegularityFeatures {
        let mmIsComputable = spans.count == 1
            && spans[0].count == values.count
            && (2...4).contains(values.count)
        let mm: Double?
        if mmIsComputable,
           let minimum = values.min(), minimum > 0,
           let maximum = values.max() {
            mm = Double(maximum) / Double(minimum)
        } else {
            mm = nil
        }
        let cvIsComputable = values.count >= 5
        var mean = 0.0
        var sumSquaredDeviation = 0.0
        for (offset, value) in values.enumerated() {
            let sample = Double(value)
            let count = Double(offset + 1)
            let delta = sample - mean
            mean += delta / count
            sumSquaredDeviation += delta * (sample - mean)
        }
        let cv = cvIsComputable && mean > 0
            ? sqrt(sumSquaredDeviation / Double(values.count - 1)) / mean
            : nil
        var cv2Terms: [Double] = []
        var lvTerms: [Double] = []
        for span in spans where span.count >= 2 {
            for pairIndex in 1..<span.count {
                let left = Double(span[pairIndex - 1].intervalMicroseconds)
                let right = Double(span[pairIndex].intervalMicroseconds)
                let sum = left + right
                guard sum > 0 else { continue }
                let normalized = (right - left) / sum
                cv2Terms.append(2 * abs(normalized))
                lvTerms.append(3 * normalized * normalized)
            }
        }
        let adjacentMetricsAreComputable = cv2Terms.count >= 4
        return ManualPatternRegularityFeatures(
            mmStanding: mm == nil ? .insufficient : .computable,
            cvStanding: cv == nil ? .insufficient : .computable,
            cv2LVStanding: adjacentMetricsAreComputable ? .computable : .insufficient,
            mm: mm,
            cv: cv,
            cv2: adjacentMetricsAreComputable ? average(cv2Terms) : nil,
            lv: adjacentMetricsAreComputable ? average(lvTerms) : nil,
            cvISICount: values.count,
            adjacentPairCount: cv2Terms.count
        )
    }

    private static func trainRelative(
        values: [Int64],
        reference: TrainRelativeReference,
        excludedReferenceValues: [Int64]
    ) -> ManualPatternTrainRelativeFeatures {
        let referenceCount = reference.values.count - excludedReferenceValues.count
        guard values.count > 0, referenceCount >= minimumTrainReferenceISICount else {
            return ManualPatternTrainRelativeFeatures(
                trainReferenceISICount: referenceCount,
                percentileStanding: .insufficientReference,
                robustLogZStanding: .insufficientReference,
                medianPercentile: nil,
                medianRobustLogZ: nil
            )
        }
        let excludedLogs = excludedReferenceValues.map { log(Double($0)) }
        let percentiles = values.map {
            reference.empiricalPercentile(
                $0,
                excluding: excludedReferenceValues,
                remainingCount: referenceCount
            )
        }.sorted()
        let center = reference.medianLog(
            excluding: excludedLogs,
            remainingCount: referenceCount
        )
        let scaledMAD = center.flatMap {
            reference.medianAbsoluteLogDeviation(
                center: $0,
                excluding: excludedLogs,
                remainingCount: referenceCount
            )
        }.map { $0 * 1.4826 }
        let robustScores: [Double]
        if let center, let scaledMAD, scaledMAD > 0 {
            robustScores = values.map { (log(Double($0)) - center) / scaledMAD }.sorted()
        } else {
            robustScores = []
        }
        return ManualPatternTrainRelativeFeatures(
            trainReferenceISICount: referenceCount,
            percentileStanding: .computable,
            robustLogZStanding: robustScores.isEmpty ? .degenerateScale : .computable,
            medianPercentile: quantile(percentiles, probability: 0.50),
            medianRobustLogZ: quantile(robustScores, probability: 0.50)
        )
    }

    /// One O(N log N) index per train. Each segment then obtains an exact leave-segment-out
    /// percentile/median and a bounded-log-query MAD without rescanning the whole train.
    private struct TrainRelativeReference {
        let values: [Int64]
        let logs: [Double]

        init(rows: [ManualLearningEvidenceRow]) {
            values = rows.filter { $0.quality == .valid }
                .map(\.intervalMicroseconds)
                .sorted()
            logs = values.map { log(Double($0)) }
        }

        func empiricalPercentile(
            _ value: Int64,
            excluding excluded: [Int64],
            remainingCount: Int
        ) -> Double {
            let fullLower = values.partitioningIndex { $0 >= value }
            let excludedLower = excluded.partitioningIndex { $0 >= value }
            let below = fullLower - excludedLower
            let fullUpper = values.partitioningIndex { $0 > value }
            let excludedUpper = excluded.partitioningIndex { $0 > value }
            let equal = (fullUpper - fullLower) - (excludedUpper - excludedLower)
            return (Double(below) + 0.5 * Double(equal)) / Double(remainingCount)
        }

        func medianLog(excluding excluded: [Double], remainingCount: Int) -> Double? {
            quantileExcluding(
                sorted: logs,
                excluded: excluded,
                remainingCount: remainingCount,
                probability: 0.50
            )
        }

        func medianAbsoluteLogDeviation(
            center: Double,
            excluding excluded: [Double],
            remainingCount: Int
        ) -> Double? {
            guard remainingCount > 0,
                  let first = valueAtRank(
                    0, sorted: logs, excluded: excluded, remainingCount: remainingCount
                  ),
                  let last = valueAtRank(
                    remainingCount - 1,
                    sorted: logs,
                    excluded: excluded,
                    remainingCount: remainingCount
                  ) else { return nil }
            let upperRadius = max(abs(first - center), abs(last - center))
            func deviationAtRank(_ rank: Int) -> Double {
                var lower = 0.0
                var upper = upperRadius
                for _ in 0..<64 {
                    let middle = lower + (upper - lower) / 2
                    let lowValue = center - middle
                    let highValue = center + middle
                    let fullCount = logs.partitioningIndex { $0 > highValue }
                        - logs.partitioningIndex { $0 >= lowValue }
                    let excludedCount = excluded.partitioningIndex { $0 > highValue }
                        - excluded.partitioningIndex { $0 >= lowValue }
                    if fullCount - excludedCount > rank {
                        upper = middle
                    } else {
                        lower = middle
                    }
                }
                return upper
            }
            let position = 0.5 * Double(remainingCount - 1)
            let lowerRank = Int(floor(position))
            let upperRank = Int(ceil(position))
            if lowerRank == upperRank { return deviationAtRank(lowerRank) }
            return (deviationAtRank(lowerRank) + deviationAtRank(upperRank)) / 2
        }

        private func quantileExcluding(
            sorted: [Double],
            excluded: [Double],
            remainingCount: Int,
            probability: Double
        ) -> Double? {
            guard remainingCount > 0 else { return nil }
            let position = probability * Double(remainingCount - 1)
            let lowerRank = Int(floor(position))
            let upperRank = Int(ceil(position))
            guard let lower = valueAtRank(
                lowerRank, sorted: sorted, excluded: excluded, remainingCount: remainingCount
            ), let upper = valueAtRank(
                upperRank, sorted: sorted, excluded: excluded, remainingCount: remainingCount
            ) else { return nil }
            return lower + (position - Double(lowerRank)) * (upper - lower)
        }

        private func valueAtRank(
            _ rank: Int,
            sorted: [Double],
            excluded: [Double],
            remainingCount: Int
        ) -> Double? {
            guard rank >= 0, rank < remainingCount, !sorted.isEmpty else { return nil }
            var lower = 0
            var upper = sorted.count
            while lower < upper {
                let middle = lower + (upper - lower) / 2
                let candidate = sorted[middle]
                let retainedAtOrBelow = sorted.partitioningIndex { $0 > candidate }
                    - excluded.partitioningIndex { $0 > candidate }
                if retainedAtOrBelow > rank {
                    upper = middle
                } else {
                    lower = middle + 1
                }
            }
            return lower < sorted.count ? sorted[lower] : nil
        }
    }

    private static func quantile(_ sorted: [Int64], probability: Double) -> Double? {
        quantile(sorted.map(Double.init), probability: probability)
    }

    private static func quantile(_ sorted: [Double], probability: Double) -> Double? {
        guard !sorted.isEmpty else { return nil }
        if sorted.count == 1 { return sorted[0] }
        let position = max(0, min(1, probability)) * Double(sorted.count - 1)
        let lower = Int(floor(position))
        let upper = Int(ceil(position))
        if lower == upper { return sorted[lower] }
        let fraction = position - Double(lower)
        return sorted[lower] + fraction * (sorted[upper] - sorted[lower])
    }

    private static func average(_ values: [Double]) -> Double? {
        values.isEmpty ? nil : values.reduce(0, +) / Double(values.count)
    }

    private static func overlayCounts(
        _ labels: [ManualAnnotationLabel]
    ) -> [ManualPatternOverlayCount] {
        Dictionary(grouping: labels, by: { $0 }).map {
            ManualPatternOverlayCount(label: $0.key, isiCount: $0.value.count)
        }.sorted { $0.label.rawValue < $1.label.rawValue }
    }

    private static func trainOrdered(
        _ lhs: ScientificSpikeTrainID,
        _ rhs: ScientificSpikeTrainID
    ) -> Bool {
        lhs.semanticID.canonicalText.utf8.lexicographicallyPrecedes(
            rhs.semanticID.canonicalText.utf8
        )
    }

    private static func recordOrdered(
        _ lhs: ManualPatternFeatureRecord,
        _ rhs: ManualPatternFeatureRecord
    ) -> Bool {
        let left = lhs.trainID.semanticID.canonicalText
        let right = rhs.trainID.semanticID.canonicalText
        if left != right { return left.utf8.lexicographicallyPrecedes(right.utf8) }
        if lhs.track != rhs.track { return lhs.track.rawValue < rhs.track.rawValue }
        if lhs.startISIIndex != rhs.startISIIndex { return lhs.startISIIndex < rhs.startISIIndex }
        if lhs.endISIIndex != rhs.endISIIndex { return lhs.endISIIndex < rhs.endISIIndex }
        return lhs.label.rawValue < rhs.label.rawValue
    }

    private static func optionalDouble(_ value: Double?) -> String {
        guard let value else { return "nil" }
        if value == 0 { return "zero" }
        if value.isNaN { return "nan" }
        if value == .infinity { return "positive_infinity" }
        if value == -.infinity { return "negative_infinity" }
        return String(value.bitPattern, radix: 16)
    }

    private static func optionalInt64(_ value: Int64?) -> String {
        value.map(String.init) ?? "nil"
    }
}

private extension Array {
    /// First index whose element satisfies a monotone predicate.
    func partitioningIndex(where belongsInSecondPartition: (Element) -> Bool) -> Int {
        var lower = 0
        var upper = count
        while lower < upper {
            let middle = lower + (upper - lower) / 2
            if belongsInSecondPartition(self[middle]) {
                upper = middle
            } else {
                lower = middle + 1
            }
        }
        return lower
    }
}
