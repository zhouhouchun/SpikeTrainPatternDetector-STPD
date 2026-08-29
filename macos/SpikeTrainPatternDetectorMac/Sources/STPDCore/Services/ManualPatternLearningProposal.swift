import Foundation

/// Pattern families exposed by the first manual-learning workflow. Event and State semantics remain
/// orthogonal in the evidence table; this enum only groups records that estimate the same parameter
/// family.
public enum ManualPatternLearningFamily: String, CaseIterable, Hashable, Sendable {
    case burstFamily = "burst_family"
    case tonic
    case highFrequencyTonic = "high_frequency_tonic"
    case highFrequencySpiking = "high_frequency_spiking"
    case pause
}

/// Evidence authority for a family proposal. These names describe observational coverage, not
/// biological replication: the canonical import contract currently identifies trains but not
/// independent neurons, animals, or recording sessions.
public enum ManualPatternLearningStanding: String, CaseIterable, Hashable, Sendable {
    case insufficient
    case exploratorySingleTrain = "exploratory_single_train"
    case provisionalTwoTrains = "provisional_two_trains"
    case supportedMultiTrain = "supported_multi_train"
}

public enum ManualPatternValidationStanding: String, CaseIterable, Hashable, Sendable {
    case unavailable
    case passed
    case mixed
    case failed
}

public enum ManualPatternLearningDiagnosticCode: String, CaseIterable, Hashable, Sendable {
    case insufficientFamilyEvidence = "insufficient_family_evidence"
    case singleTrainOnly = "single_train_only"
    case crossTrainValidationMixed = "cross_train_validation_mixed"
    case crossTrainValidationFailed = "cross_train_validation_failed"
    case expectedBurstTonicOrderNotObserved = "expected_burst_tonic_order_not_observed"
    case expectedTonicPauseOrderNotObserved = "expected_tonic_pause_order_not_observed"
    case expectedHFSBelowTonicNotObserved = "expected_hfs_below_tonic_not_observed"
    case expectedHFTonicAboveBurstNotObserved = "expected_hf_tonic_above_burst_not_observed"
    case hfsHasNoCompatibleThresholdField = "hfs_has_no_compatible_threshold_field"
}

public enum ManualPatternLearningDiagnosticSeverity: String, CaseIterable, Hashable, Sendable {
    case information
    case warning
}

public struct ManualPatternLearningDiagnostic: Hashable, Sendable {
    public let code: ManualPatternLearningDiagnosticCode
    public let severity: ManualPatternLearningDiagnosticSeverity
    public let family: ManualPatternLearningFamily?

    public init(
        code: ManualPatternLearningDiagnosticCode,
        severity: ManualPatternLearningDiagnosticSeverity,
        family: ManualPatternLearningFamily? = nil
    ) {
        self.code = code
        self.severity = severity
        self.family = family
    }
}

/// A scalar summarized without pooling raw ISIs across trains. Each segment contributes one value;
/// segment values are reduced to one median per train; train medians then receive equal weight.
public struct ManualPatternTrainBalancedScalar: Hashable, Sendable {
    public let contributingSegmentCount: Int
    public let contributingTrainCount: Int
    public let trainBalancedMedian: Double?
    public let minimumTrainMedian: Double?
    public let maximumTrainMedian: Double?

    public init(
        contributingSegmentCount: Int,
        contributingTrainCount: Int,
        trainBalancedMedian: Double?,
        minimumTrainMedian: Double?,
        maximumTrainMedian: Double?
    ) {
        self.contributingSegmentCount = contributingSegmentCount
        self.contributingTrainCount = contributingTrainCount
        self.trainBalancedMedian = trainBalancedMedian
        self.minimumTrainMedian = minimumTrainMedian
        self.maximumTrainMedian = maximumTrainMedian
    }
}

public struct ManualPatternCrossTrainValidation: Hashable, Sendable {
    public let standing: ManualPatternValidationStanding
    public let heldOutTrainCount: Int
    public let insideExpectedBandCount: Int
    public let coverageFraction: Double?

    public init(
        standing: ManualPatternValidationStanding,
        heldOutTrainCount: Int,
        insideExpectedBandCount: Int,
        coverageFraction: Double?
    ) {
        self.standing = standing
        self.heldOutTrainCount = heldOutTrainCount
        self.insideExpectedBandCount = insideExpectedBandCount
        self.coverageFraction = coverageFraction
    }
}

/// Report for one pattern family. Absolute values are retained in exact source microsecond units;
/// relative position and regularity are separate evidence rather than replacements for geometry.
public struct ManualPatternFamilyLearningSummary: Hashable, Sendable {
    public let family: ManualPatternLearningFamily
    public let standing: ManualPatternLearningStanding
    /// All manually annotated family segments, including segments with no usable direct support.
    public let segmentCount: Int
    public let usableSegmentCount: Int
    /// All trains containing an annotated family segment.
    public let trainCount: Int
    public let usableTrainCount: Int
    public let directSupportISICount: Int
    public let directSupportSpikeCount: Int
    public let excludedISICount: Int
    public let isiQ10Microseconds: ManualPatternTrainBalancedScalar
    public let isiMedianMicroseconds: ManualPatternTrainBalancedScalar
    public let isiQ90Microseconds: ManualPatternTrainBalancedScalar
    public let isiQ95Microseconds: ManualPatternTrainBalancedScalar
    public let directDurationMicroseconds: ManualPatternTrainBalancedScalar
    public let directSpikeCount: ManualPatternTrainBalancedScalar
    public let mm: ManualPatternTrainBalancedScalar
    public let cv: ManualPatternTrainBalancedScalar
    public let cv2: ManualPatternTrainBalancedScalar
    public let lv: ManualPatternTrainBalancedScalar
    public let trainRelativeMedianPercentile: ManualPatternTrainBalancedScalar
    public let trainRelativeMedianRobustLogZ: ManualPatternTrainBalancedScalar
    public let validation: ManualPatternCrossTrainValidation
}

/// Identity-bound preview. `compatibleThresholdProposal` contains only fields already supported by
/// the detector's safe soft-anchor contract. Report-only evidence (notably HFS size/duration/ISI)
/// remains visible but cannot silently acquire detector authority.
public struct ManualPatternLearningProposal: Hashable, Sendable {
    public let schemaContractID: String
    public let schemaContractDigest: String
    public let featureSourceIdentityDigest: String
    public let featureComputationDigest: String
    public let summaries: [ManualPatternFamilyLearningSummary]
    public let diagnostics: [ManualPatternLearningDiagnostic]
    public let compatibleThresholdProposal: LearnedThresholdProposal
    /// Stable source identity. It excludes derived floating-point values.
    public let sourceIdentityDigest: String
    /// Runtime-local deterministic diagnostic over all derived proposal values.
    public let proposalComputationDigest: String

    public var hasApplicableThresholds: Bool {
        !compatibleThresholdProposal.isAllAutomatic
    }
}

public enum ManualPatternLearningProposalBuilder {
    public static let schemaContractID =
        "manual_pattern_train_balanced_learning_proposal"

    /// Build a report and a compatible soft-anchor proposal. The feature table is already bound to
    /// its evidence snapshot, so this stage never re-reads mutable annotations or detector output.
    public static func build(
        from table: ManualPatternFeatureTable
    ) -> ManualPatternLearningProposal {
        let schemaDigest = ManualLearningDigest.hex(tokens: schemaTokens)
        let summaries = ManualPatternLearningFamily.allCases.map {
            summarize(family: $0, records: records(for: $0, in: table.records))
        }
        var diagnostics = summaries.flatMap(familyDiagnostics)
        diagnostics.append(contentsOf: orderDiagnostics(summaries))
        diagnostics.sort(by: diagnosticOrdered)
        let thresholdProposal = compatibleProposal(from: summaries)
        let sourceIdentityDigest = ManualLearningDigest.hex(tokens: [
            schemaContractID,
            schemaDigest,
            table.schemaContractID,
            table.schemaContractDigest,
            table.evidenceSnapshotDigest,
            table.sourceIdentityDigest,
        ])
        let computationTokens = [
            "runtime_local_manual_pattern_learning_proposal",
            sourceIdentityDigest,
            table.featureComputationDigest,
        ] + summaries.flatMap(summaryTokens)
            + diagnostics.flatMap {
                [$0.code.rawValue, $0.severity.rawValue, $0.family?.rawValue ?? ""]
            }
            + thresholdProposalTokens(thresholdProposal)
        return ManualPatternLearningProposal(
            schemaContractID: schemaContractID,
            schemaContractDigest: schemaDigest,
            featureSourceIdentityDigest: table.sourceIdentityDigest,
            featureComputationDigest: table.featureComputationDigest,
            summaries: summaries,
            diagnostics: diagnostics,
            compatibleThresholdProposal: thresholdProposal,
            sourceIdentityDigest: sourceIdentityDigest,
            proposalComputationDigest: ManualLearningDigest.hex(tokens: computationTokens)
        )
    }

    private static var schemaTokens: [String] {
        [
            schemaContractID,
            "sha256_length_prefixed_utf8_tokens",
            "one_segment_one_vote_then_within_train_median_then_equal_train_median",
            "raw_isi_samples_are_never_pooled_across_trains",
            "absolute_microsecond_relative_position_and_regularity_evidence_remain_separate",
            "burst_family_is_burst_high_frequency_burst_and_long_burst_events",
            "state_direct_support_excludes_embedded_event_support_upstream",
            "tail_quantiles_require_multiple_segments_before_threshold_mapping",
            "minimum_evidence:burst_family:usable_segments>=2:direct_isis>=4",
            "minimum_evidence:tonic:usable_segments>=2:direct_isis>=4",
            "minimum_evidence:high_frequency_tonic:usable_segments>=2:direct_isis>=4",
            "minimum_evidence:high_frequency_spiking:usable_segments>=2:direct_isis>=8",
            "minimum_evidence:pause:usable_segments>=3:direct_isis>=3",
            "burst_q90_to_seed_upper_q95_to_bridge_upper_soft_anchor",
            "tonic_q10_q90_to_lower_upper_soft_anchor",
            "high_frequency_tonic_q10_q90_to_floor_upper_soft_anchor",
            "pause_q10_to_lower_soft_anchor",
            "hfs_isi_duration_and_spike_count_are_report_only_until_a_compatible_field_exists",
            "counts_and_regularity_are_report_only_not_hard_gates",
            "leave_one_train_out_validation_is_diagnostic_not_authority",
            "validation:non_pause:heldout_segment_median_within_other_train_balanced_q10_q90",
            "validation:pause:heldout_segment_median_at_or_above_other_train_balanced_q10",
            "validation:passed_fraction>=0.75:mixed_fraction>=0.5:failed_fraction<0.5",
            "support_score:min_usable_segments_usable_trains_over_n_plus_6",
            "soft_order_checks_do_not_impose_nonoverlapping_distributions",
            "diagnostic:insufficient_family_evidence:standing==insufficient:information",
            "diagnostic:single_train_only:standing==exploratory_single_train:information",
            "diagnostic:cross_train_validation_mixed:validation==mixed:warning",
            "diagnostic:cross_train_validation_failed:validation==failed:warning",
            "diagnostic:hfs_no_compatible_field:family==high_frequency_spiking_and_standing!=insufficient:information",
            "diagnostic:burst_tonic_order:burst_center>=tonic_center:warning",
            "diagnostic:tonic_pause_order:tonic_center>=pause_center:warning",
            "diagnostic:hfs_tonic_order:hfs_center>=tonic_center:warning",
            "diagnostic:hf_tonic_burst_order:hf_tonic_center<=burst_center:warning",
            "proposal_never_runs_or_mutates_a_detector",
        ]
        + ManualPatternLearningFamily.allCases.map { "family:\($0.rawValue)" }
        + ManualPatternLearningStanding.allCases.map { "standing:\($0.rawValue)" }
        + ManualPatternValidationStanding.allCases.map { "validation:\($0.rawValue)" }
        + ManualPatternLearningDiagnosticCode.allCases.map { "diagnostic:\($0.rawValue)" }
        + ManualPatternLearningDiagnosticSeverity.allCases.map { "severity:\($0.rawValue)" }
        + [
            "mapping:burst:seed_upper_sec:train_balanced_median_segment_q90",
            "mapping:burst:bridge_upper_sec:train_balanced_median_segment_q95",
            "mapping:tonic:isi_lower_sec:train_balanced_median_segment_q10",
            "mapping:tonic:isi_upper_sec:train_balanced_median_segment_q90",
            "mapping:hf_tonic:isi_floor_sec:train_balanced_median_segment_q10",
            "mapping:hf_tonic:isi_upper_sec:train_balanced_median_segment_q90",
            "mapping:pause:isi_lower_sec:train_balanced_median_segment_q10",
        ]
    }

    private static func records(
        for family: ManualPatternLearningFamily,
        in records: [ManualPatternFeatureRecord]
    ) -> [ManualPatternFeatureRecord] {
        records.filter { record in
            switch family {
            case .burstFamily:
                return record.track == .event
                    && [.burst, .highFrequencyBurst, .longBurst].contains(record.label)
            case .tonic:
                return record.track == .state && record.label == .tonic
            case .highFrequencyTonic:
                return record.track == .state && record.label == .highFrequencyTonic
            case .highFrequencySpiking:
                return record.track == .state && record.label == .highFrequencySpiking
            case .pause:
                return record.track == .event && record.label == .pause
            }
        }
    }

    private static func summarize(
        family: ManualPatternLearningFamily,
        records: [ManualPatternFeatureRecord]
    ) -> ManualPatternFamilyLearningSummary {
        let usable = records.filter(\.hasUsableDirectSupport)
        let annotatedTrainCount = Set(records.map(\.trainID)).count
        let usableTrainCount = Set(usable.map(\.trainID)).count
        let directISIs = usable.reduce(0) { $0 + $1.directSupportISICount }
        let standing: ManualPatternLearningStanding
        if !hasMinimumEvidence(family: family, records: usable, directISIs: directISIs) {
            standing = .insufficient
        } else if usableTrainCount == 1 {
            standing = .exploratorySingleTrain
        } else if usableTrainCount == 2 {
            standing = .provisionalTwoTrains
        } else {
            standing = .supportedMultiTrain
        }
        return ManualPatternFamilyLearningSummary(
            family: family,
            standing: standing,
            segmentCount: records.count,
            usableSegmentCount: usable.count,
            trainCount: annotatedTrainCount,
            usableTrainCount: usableTrainCount,
            directSupportISICount: directISIs,
            directSupportSpikeCount: usable.reduce(0) { $0 + $1.directSupportSpikeCount },
            excludedISICount: records.reduce(0) { $0 + $1.excludedAnyISICount },
            isiQ10Microseconds: balanced(usable) { $0.directSupportISI.q10Microseconds },
            isiMedianMicroseconds: balanced(usable) { $0.directSupportISI.medianMicroseconds },
            isiQ90Microseconds: balanced(usable) { $0.directSupportISI.q90Microseconds },
            isiQ95Microseconds: balanced(usable) { $0.directSupportISI.q95Microseconds },
            directDurationMicroseconds: balanced(usable) {
                $0.directSupportDurationMicroseconds.map(Double.init)
            },
            directSpikeCount: balanced(usable) { Double($0.directSupportSpikeCount) },
            mm: balanced(usable) { $0.regularity.mm },
            cv: balanced(usable) { $0.regularity.cv },
            cv2: balanced(usable) { $0.regularity.cv2 },
            lv: balanced(usable) { $0.regularity.lv },
            trainRelativeMedianPercentile: balanced(usable) {
                $0.trainRelative.medianPercentile
            },
            trainRelativeMedianRobustLogZ: balanced(usable) {
                $0.trainRelative.medianRobustLogZ
            },
            validation: validate(family: family, records: usable)
        )
    }

    private static func hasMinimumEvidence(
        family: ManualPatternLearningFamily,
        records: [ManualPatternFeatureRecord],
        directISIs: Int
    ) -> Bool {
        switch family {
        case .pause:
            return records.count >= 3 && directISIs >= 3
        case .highFrequencySpiking:
            return records.count >= 2 && directISIs >= 8
        case .burstFamily, .tonic, .highFrequencyTonic:
            return records.count >= 2 && directISIs >= 4
        }
    }

    private static func balanced(
        _ records: [ManualPatternFeatureRecord],
        value: (ManualPatternFeatureRecord) -> Double?
    ) -> ManualPatternTrainBalancedScalar {
        let usable = records.compactMap { record -> (ScientificSpikeTrainID, Double)? in
            guard let scalar = value(record), scalar.isFinite else { return nil }
            return (record.trainID, scalar)
        }
        let grouped = Dictionary(grouping: usable, by: \.0)
        let trainMedians = grouped.values.compactMap { group in
            median(group.map(\.1))
        }.sorted()
        return ManualPatternTrainBalancedScalar(
            contributingSegmentCount: usable.count,
            contributingTrainCount: trainMedians.count,
            trainBalancedMedian: median(trainMedians),
            minimumTrainMedian: trainMedians.first,
            maximumTrainMedian: trainMedians.last
        )
    }

    private static func validate(
        family: ManualPatternLearningFamily,
        records: [ManualPatternFeatureRecord]
    ) -> ManualPatternCrossTrainValidation {
        let grouped = Dictionary(grouping: records, by: \.trainID)
        guard grouped.count >= 2 else {
            return ManualPatternCrossTrainValidation(
                standing: .unavailable,
                heldOutTrainCount: 0,
                insideExpectedBandCount: 0,
                coverageFraction: nil
            )
        }
        var evaluated = 0
        var inside = 0
        for trainID in grouped.keys {
            guard let heldOutCenter = median(
                (grouped[trainID] ?? []).compactMap { $0.directSupportISI.medianMicroseconds }
            ) else { continue }
            let training = records.filter { $0.trainID != trainID }
            let lower = balanced(training) { $0.directSupportISI.q10Microseconds }
                .trainBalancedMedian
            let upper = balanced(training) { $0.directSupportISI.q90Microseconds }
                .trainBalancedMedian
            guard let lower, let upper else { continue }
            evaluated += 1
            if family == .pause {
                if heldOutCenter >= lower { inside += 1 }
            } else if heldOutCenter >= lower && heldOutCenter <= upper {
                inside += 1
            }
        }
        guard evaluated > 0 else {
            return ManualPatternCrossTrainValidation(
                standing: .unavailable,
                heldOutTrainCount: 0,
                insideExpectedBandCount: 0,
                coverageFraction: nil
            )
        }
        let fraction = Double(inside) / Double(evaluated)
        let standing: ManualPatternValidationStanding
        if fraction >= 0.75 {
            standing = .passed
        } else if fraction >= 0.5 {
            standing = .mixed
        } else {
            standing = .failed
        }
        return ManualPatternCrossTrainValidation(
            standing: standing,
            heldOutTrainCount: evaluated,
            insideExpectedBandCount: inside,
            coverageFraction: fraction
        )
    }

    private static func familyDiagnostics(
        _ summary: ManualPatternFamilyLearningSummary
    ) -> [ManualPatternLearningDiagnostic] {
        var result: [ManualPatternLearningDiagnostic] = []
        switch summary.standing {
        case .insufficient:
            result.append(.init(
                code: .insufficientFamilyEvidence,
                severity: .information,
                family: summary.family
            ))
        case .exploratorySingleTrain:
            result.append(.init(
                code: .singleTrainOnly,
                severity: .information,
                family: summary.family
            ))
        case .provisionalTwoTrains, .supportedMultiTrain:
            break
        }
        switch summary.validation.standing {
        case .mixed:
            result.append(.init(
                code: .crossTrainValidationMixed,
                severity: .warning,
                family: summary.family
            ))
        case .failed:
            result.append(.init(
                code: .crossTrainValidationFailed,
                severity: .warning,
                family: summary.family
            ))
        case .unavailable, .passed:
            break
        }
        if summary.family == .highFrequencySpiking,
           summary.standing != .insufficient {
            result.append(.init(
                code: .hfsHasNoCompatibleThresholdField,
                severity: .information,
                family: summary.family
            ))
        }
        return result
    }

    private static func orderDiagnostics(
        _ summaries: [ManualPatternFamilyLearningSummary]
    ) -> [ManualPatternLearningDiagnostic] {
        let byFamily = Dictionary(uniqueKeysWithValues: summaries.map { ($0.family, $0) })
        func center(_ family: ManualPatternLearningFamily) -> Double? {
            byFamily[family]?.isiMedianMicroseconds.trainBalancedMedian
        }
        var result: [ManualPatternLearningDiagnostic] = []
        if let burst = center(.burstFamily), let tonic = center(.tonic), burst >= tonic {
            result.append(.init(
                code: .expectedBurstTonicOrderNotObserved,
                severity: .warning
            ))
        }
        if let tonic = center(.tonic), let pause = center(.pause), tonic >= pause {
            result.append(.init(
                code: .expectedTonicPauseOrderNotObserved,
                severity: .warning
            ))
        }
        if let hfs = center(.highFrequencySpiking), let tonic = center(.tonic), hfs >= tonic {
            result.append(.init(
                code: .expectedHFSBelowTonicNotObserved,
                severity: .warning
            ))
        }
        if let hfTonic = center(.highFrequencyTonic), let burst = center(.burstFamily),
           hfTonic <= burst {
            result.append(.init(
                code: .expectedHFTonicAboveBurstNotObserved,
                severity: .warning
            ))
        }
        return result
    }

    private static func compatibleProposal(
        from summaries: [ManualPatternFamilyLearningSummary]
    ) -> LearnedThresholdProposal {
        var burst = BurstManualThresholds()
        var tonic = TonicManualThresholds()
        var hfTonic = HFTonicManualThresholds()
        var pause = PauseManualThresholds()
        var contributions: [LearnedThresholdContribution] = []
        var skipped: [LearnedThresholdSkip] = []
        let byFamily = Dictionary(uniqueKeysWithValues: summaries.map { ($0.family, $0) })

        func threshold(_ microseconds: Double?) -> ManualISIThreshold {
            guard let microseconds, microseconds.isFinite, microseconds > 0 else {
                return .automatic
            }
            return ManualISIThreshold(mode: .softAnchor, valueSec: microseconds / 1_000_000)
        }
        func add(
            family: ManualPatternLearningFamily,
            field: String,
            statistic: String,
            scalar: ManualPatternTrainBalancedScalar,
            summary: ManualPatternFamilyLearningSummary
        ) {
            guard let microseconds = scalar.trainBalancedMedian,
                  microseconds.isFinite, microseconds > 0 else { return }
            let n = min(summary.usableSegmentCount, summary.usableTrainCount)
            let support = n == 0 ? 0 : Double(n) / Double(n + 6)
            contributions.append(LearnedThresholdContribution(
                family: contributionFamily(family),
                field: field,
                sourceLabel: family.rawValue,
                statistic: "train_balanced_median_segment_\(statistic)",
                valueSec: microseconds / 1_000_000,
                mode: .softAnchor,
                annotationCount: summary.usableSegmentCount,
                trainCount: summary.usableTrainCount,
                coveredISICount: summary.directSupportISICount,
                confidence: support
            ))
        }

        if let summary = byFamily[.burstFamily], summary.standing != .insufficient {
            burst.seedUpperISI = threshold(summary.isiQ90Microseconds.trainBalancedMedian)
            burst.bridgeUpperISI = threshold(summary.isiQ95Microseconds.trainBalancedMedian)
            add(family: .burstFamily, field: "seed_upper_sec", statistic: "q90",
                scalar: summary.isiQ90Microseconds, summary: summary)
            add(family: .burstFamily, field: "bridge_upper_sec", statistic: "q95",
                scalar: summary.isiQ95Microseconds, summary: summary)
        } else {
            skipped.append(.init(sourceLabel: ManualPatternLearningFamily.burstFamily.rawValue,
                                 reason: .notUsableMinCount))
        }
        if let summary = byFamily[.tonic], summary.standing != .insufficient {
            tonic.isiLower = threshold(summary.isiQ10Microseconds.trainBalancedMedian)
            tonic.isiUpper = threshold(summary.isiQ90Microseconds.trainBalancedMedian)
            add(family: .tonic, field: "isi_lower_sec", statistic: "q10",
                scalar: summary.isiQ10Microseconds, summary: summary)
            add(family: .tonic, field: "isi_upper_sec", statistic: "q90",
                scalar: summary.isiQ90Microseconds, summary: summary)
        } else {
            skipped.append(.init(sourceLabel: ManualPatternLearningFamily.tonic.rawValue,
                                 reason: .notUsableMinCount))
        }
        if let summary = byFamily[.highFrequencyTonic], summary.standing != .insufficient {
            hfTonic.isiFloor = threshold(summary.isiQ10Microseconds.trainBalancedMedian)
            hfTonic.isiUpper = threshold(summary.isiQ90Microseconds.trainBalancedMedian)
            add(family: .highFrequencyTonic, field: "isi_floor_sec", statistic: "q10",
                scalar: summary.isiQ10Microseconds, summary: summary)
            add(family: .highFrequencyTonic, field: "isi_upper_sec", statistic: "q90",
                scalar: summary.isiQ90Microseconds, summary: summary)
        } else {
            skipped.append(.init(
                sourceLabel: ManualPatternLearningFamily.highFrequencyTonic.rawValue,
                reason: .notUsableMinCount
            ))
        }
        if let summary = byFamily[.pause], summary.standing != .insufficient {
            pause.isiLower = threshold(summary.isiQ10Microseconds.trainBalancedMedian)
            add(family: .pause, field: "isi_lower_sec", statistic: "q10",
                scalar: summary.isiQ10Microseconds, summary: summary)
        } else {
            skipped.append(.init(sourceLabel: ManualPatternLearningFamily.pause.rawValue,
                                 reason: .notUsableMinCount))
        }
        skipped.append(.init(
            sourceLabel: ManualPatternLearningFamily.highFrequencySpiking.rawValue,
            reason: .noMappableField
        ))
        return LearnedThresholdProposal(
            profile: ManualThresholdProfile(
                burst: burst,
                hfs: HFSManualThresholds(),
                hfTonic: hfTonic,
                tonic: tonic,
                pause: pause
            ),
            contributions: contributions,
            skipped: skipped,
            mode: .softAnchor
        )
    }

    private static func contributionFamily(_ family: ManualPatternLearningFamily) -> String {
        switch family {
        case .burstFamily: return "burst"
        case .highFrequencyTonic: return "hf_tonic"
        case .highFrequencySpiking: return "hfs"
        case .tonic: return "tonic"
        case .pause: return "pause"
        }
    }

    private static func median(_ values: [Double]) -> Double? {
        let sorted = values.filter(\.isFinite).sorted()
        guard !sorted.isEmpty else { return nil }
        let midpoint = sorted.count / 2
        if sorted.count.isMultiple(of: 2) {
            return (sorted[midpoint - 1] + sorted[midpoint]) / 2
        }
        return sorted[midpoint]
    }

    private static func summaryTokens(
        _ value: ManualPatternFamilyLearningSummary
    ) -> [String] {
        var tokens = [
            value.family.rawValue,
            value.standing.rawValue,
            String(value.segmentCount),
            String(value.usableSegmentCount),
            String(value.trainCount),
            String(value.usableTrainCount),
            String(value.directSupportISICount),
            String(value.directSupportSpikeCount),
            String(value.excludedISICount),
        ]
        let scalars: [ManualPatternTrainBalancedScalar] = [
            value.isiQ10Microseconds,
            value.isiMedianMicroseconds,
            value.isiQ90Microseconds,
            value.isiQ95Microseconds,
            value.directDurationMicroseconds,
            value.directSpikeCount,
            value.mm,
            value.cv,
            value.cv2,
            value.lv,
            value.trainRelativeMedianPercentile,
            value.trainRelativeMedianRobustLogZ,
        ]
        for scalar in scalars {
            tokens.append(contentsOf: [
                String(scalar.contributingSegmentCount),
                String(scalar.contributingTrainCount),
                scalar.trainBalancedMedian.map { String($0) } ?? "",
                scalar.minimumTrainMedian.map { String($0) } ?? "",
                scalar.maximumTrainMedian.map { String($0) } ?? "",
            ])
        }
        tokens.append(contentsOf: [
            value.validation.standing.rawValue,
            String(value.validation.heldOutTrainCount),
            String(value.validation.insideExpectedBandCount),
            value.validation.coverageFraction.map { String($0) } ?? "",
        ])
        return tokens
    }

    private static func thresholdProposalTokens(
        _ value: LearnedThresholdProposal
    ) -> [String] {
        func isi(_ field: ManualISIThreshold) -> [String] {
            [field.mode.rawValue, field.valueSec.map { String($0) } ?? ""]
        }
        func count(_ field: ManualSpikeCountThreshold) -> [String] {
            [field.mode.rawValue, field.value.map { String($0) } ?? ""]
        }
        let profile = value.profile
        var tokens = ["proposal_mode", value.mode.rawValue]
        tokens += ["burst.seed_lower"] + isi(profile.burst.seedLowerISI)
        tokens += ["burst.seed_upper"] + isi(profile.burst.seedUpperISI)
        tokens += ["burst.bridge_upper"] + isi(profile.burst.bridgeUpperISI)
        tokens += ["burst.min_spikes"] + count(profile.burst.minSpikes)
        tokens += ["burst.classic_max"] + count(profile.burst.classicMaxSpikes)
        tokens += ["burst.long_min"] + count(profile.burst.longMinSpikes)
        tokens += ["burst.long_max"] + count(profile.burst.longMaxSpikes)
        tokens += ["hfs.min_spikes"] + count(profile.hfs.minSpikes)
        tokens += ["hfs.min_duration"] + isi(profile.hfs.minDurationSec)
        tokens += ["hf_tonic.min_spikes"] + count(profile.hfTonic.minSpikes)
        tokens += ["hf_tonic.floor"] + isi(profile.hfTonic.isiFloor)
        tokens += ["hf_tonic.upper"] + isi(profile.hfTonic.isiUpper)
        tokens += ["tonic.min_spikes"] + count(profile.tonic.minSpikes)
        tokens += ["tonic.lower"] + isi(profile.tonic.isiLower)
        tokens += ["tonic.upper"] + isi(profile.tonic.isiUpper)
        tokens += ["pause.lower"] + isi(profile.pause.isiLower)
        for item in value.contributions {
            tokens += [
                "contribution", item.family, item.field, item.sourceLabel,
                item.statistic, String(item.valueSec), item.mode.rawValue,
                String(item.evidenceRunCount), String(item.trainCount),
                String(item.coveredISICount), String(item.supportScore),
            ]
        }
        for item in value.skipped {
            tokens += ["skip", item.sourceLabel, item.reason.rawValue]
        }
        for key in profile.learnedProvenanceByKey.keys.sorted() {
            tokens += ["profile_provenance", key, profile.learnedProvenanceByKey[key] ?? ""]
        }
        return tokens
    }

    private static func diagnosticOrdered(
        _ lhs: ManualPatternLearningDiagnostic,
        _ rhs: ManualPatternLearningDiagnostic
    ) -> Bool {
        let leftFamily = lhs.family?.rawValue ?? ""
        let rightFamily = rhs.family?.rawValue ?? ""
        if leftFamily != rightFamily { return leftFamily < rightFamily }
        return lhs.code.rawValue < rhs.code.rawValue
    }
}

public extension ManualPatternLearningProposal {
    /// Add the exact proposal identity to an otherwise compatibility-oriented learned-threshold
    /// note. The note remains a single deterministic token for run identity and result-package
    /// persistence; the source digest binds the canonical dataset, labels, QC boundary, and feature
    /// contract, while the computation digest binds the complete derived proposal.
    func identityBoundProvenanceNote(_ note: String) -> String {
        let suffix = "schema_digest=\(schemaContractDigest),source_identity=\(sourceIdentityDigest),proposal_digest=\(proposalComputationDigest)"
        guard note.last == ")" else { return note + ";" + suffix }
        return String(note.dropLast()) + "," + suffix + ")"
    }
}
