import Foundation

/// Rectangular, identity-bearing export of one holdout run. It contains one family summary row plus
/// one row per assessed held-out train. CSV and XLSX consume the same table. JSON-valued cells are
/// used for variable-length identities/settings/contributions so valid Unicode or separator-bearing
/// semantic IDs remain lossless.
public enum ManualLearningHoldoutReportExporter {
    public static let headers = [
        "schema_contract_id", "schema_contract_digest", "admission_contract_digest",
        "report_digest", "source_dataset_digest", "held_out_evidence_digest",
        "held_out_evaluation_digest", "calibration_proposal_source_identity_digest",
        "calibration_proposal_computation_digest", "row_scope", "held_out_train_id_json",
        "assessment_status", "geometry_review_complete", "baseline_settings_digest",
        "learned_settings_digest",
        "baseline_settings_entries_json", "learned_settings_entries_json",
        "calibration_train_ids_json", "held_out_train_ids_json", "excluded_train_ids_json",
        "family", "family_evaluation_digest", "admission_disposition",
        "admission_reasons_json", "compatible_threshold_available",
        "learned_threshold_contributions_json", "calibration_standing",
        "calibration_validation_standing", "calibration_usable_segment_count",
        "calibration_usable_train_count", "calibration_direct_support_isi_count",
        "assessed_train_count", "truth_positive_train_count", "explicit_negative_train_count",
        "geometry_complete_train_count", "assessed_isi_count", "truth_positive_isi_count",
        "baseline_predicted_positive_isi_count", "baseline_true_positive_isi_count",
        "baseline_false_positive_isi_count", "baseline_false_negative_isi_count",
        "baseline_true_negative_isi_count", "baseline_precision", "baseline_recall",
        "baseline_f1", "baseline_truth_segment_count", "baseline_predicted_segment_count",
        "baseline_mean_best_segment_iou", "baseline_mean_matched_boundary_error_isi",
        "baseline_false_split_count", "baseline_false_merge_count",
        "learned_predicted_positive_isi_count", "learned_true_positive_isi_count",
        "learned_false_positive_isi_count", "learned_false_negative_isi_count",
        "learned_true_negative_isi_count", "learned_precision", "learned_recall", "learned_f1",
        "learned_truth_segment_count", "learned_predicted_segment_count",
        "learned_mean_best_segment_iou", "learned_mean_matched_boundary_error_isi",
        "learned_false_split_count", "learned_false_merge_count", "f1_delta",
        "mean_best_segment_iou_delta", "mean_matched_boundary_error_isi_delta",
        "false_positive_delta", "false_negative_delta", "false_split_delta", "false_merge_delta",
    ]

    public static func table(
        report: ManualLearningHoldoutValidationReport
    ) -> ManualISIExportTable {
        let comparisons = Dictionary(uniqueKeysWithValues: report.comparisons.map {
            ($0.family, $0)
        })
        let admissions = Dictionary(uniqueKeysWithValues: report.admissions.map {
            ($0.family, $0)
        })
        let summaries = Dictionary(uniqueKeysWithValues: report.calibrationProposal.summaries.map {
            ($0.family, $0)
        })
        var rows: [[String]] = []
        for family in ManualPatternLearningFamily.allCases {
            guard let comparison = comparisons[family],
                  let admission = admissions[family],
                  let summary = summaries[family] else { continue }
            let common = CommonRow(
                report: report,
                comparison: comparison,
                admission: admission,
                summary: summary,
                compatibleThresholdAvailable: comparison.hasCompatibleIntervention
            )
            rows.append(row(
                common: common,
                scope: "family_summary",
                trainID: "",
                isAssessed: comparison.baseline.assessedISICount > 0,
                geometryComplete: comparison.assessedTrainCount > 0
                    && comparison.geometryCompleteTrainCount == comparison.assessedTrainCount,
                assessedTrainCount: comparison.assessedTrainCount,
                truthPositiveTrainCount: comparison.truthPositiveTrainCount,
                explicitNegativeTrainCount: comparison.explicitNegativeTrainCount,
                geometryCompleteTrainCount: comparison.geometryCompleteTrainCount,
                baseline: comparison.baseline,
                learned: comparison.learned,
                deltas: deltas(comparison)
            ))
            for train in comparison.trainComparisons {
                rows.append(row(
                    common: common,
                    scope: "held_out_train",
                    trainID: train.trainID,
                    isAssessed: train.baseline.assessedISICount > 0,
                    geometryComplete: train.geometryIsComplete,
                    assessedTrainCount: train.baseline.assessedISICount > 0 ? 1 : 0,
                    truthPositiveTrainCount: train.baseline.truthPositiveISICount > 0 ? 1 : 0,
                    explicitNegativeTrainCount:
                        train.baseline.assessedISICount > train.baseline.truthPositiveISICount ? 1 : 0,
                    geometryCompleteTrainCount: train.geometryIsComplete ? 1 : 0,
                    baseline: train.baseline,
                    learned: train.learned,
                    deltas: deltas(train)
                ))
            }
        }
        return ManualISIExportTable(headers: headers, rows: rows)
    }

    public static func csv(
        report: ManualLearningHoldoutValidationReport,
        lineEnding: String = "\r\n"
    ) -> String {
        table(report: report).csv(lineEnding: lineEnding)
    }

    private struct CommonRow {
        let report: ManualLearningHoldoutValidationReport
        let comparison: ManualLearningHoldoutFamilyComparison
        let admission: ManualLearningFamilyAdmission
        let summary: ManualPatternFamilyLearningSummary
        let compatibleThresholdAvailable: Bool
    }

    private struct Deltas {
        let f1: Double?
        let iou: Double?
        let boundary: Double?
        let falsePositive: Int
        let falseNegative: Int
        let falseSplit: Int
        let falseMerge: Int
    }

    private static func row(
        common: CommonRow,
        scope: String,
        trainID: String,
        isAssessed: Bool,
        geometryComplete: Bool,
        assessedTrainCount: Int,
        truthPositiveTrainCount: Int,
        explicitNegativeTrainCount: Int,
        geometryCompleteTrainCount: Int,
        baseline: ManualLearningDetectorMetrics,
        learned: ManualLearningDetectorMetrics,
        deltas: Deltas
    ) -> [String] {
        let report = common.report
        let comparison = common.comparison
        let summary = common.summary
        return [
            report.schemaContractID, report.schemaContractDigest,
            report.admissionContractDigest, report.reportDigest, report.sourceDatasetDigest,
            report.heldOutEvidenceDigest, report.heldOutEvaluationDigest,
            report.calibrationProposal.sourceIdentityDigest,
            report.calibrationProposal.proposalComputationDigest,
            scope, jsonString(trainID), isAssessed ? "assessed" : "unassessed",
            geometryComplete ? "true" : "false",
            report.baselineSettingsDigest, comparison.learnedSettingsDigest,
            settingsJSON(report.baselineSettingsEntries),
            settingsJSON(comparison.learnedSettingsEntries),
            jsonArray(report.calibrationTrainIDs), jsonArray(report.heldOutTrainIDs),
            jsonArray(report.excludedTrainIDs), comparison.family.rawValue,
            comparison.heldOutEvaluationDigest, common.admission.disposition.rawValue,
            jsonArray(common.admission.reasons.map(\.rawValue)),
            common.compatibleThresholdAvailable ? "true" : "false",
            contributionsJSON(
                report.calibrationProposal.compatibleThresholdProposal.contributions,
                family: comparison.family.compatibleThresholdFamilyKey
            ),
            summary.standing.rawValue, summary.validation.standing.rawValue,
            String(summary.usableSegmentCount), String(summary.usableTrainCount),
            String(summary.directSupportISICount), String(assessedTrainCount),
            String(truthPositiveTrainCount), String(explicitNegativeTrainCount),
            String(geometryCompleteTrainCount), String(baseline.assessedISICount),
            String(baseline.truthPositiveISICount),
        ] + metricFields(baseline) + metricFields(learned) + [
            number(deltas.f1), number(deltas.iou), number(deltas.boundary),
            String(deltas.falsePositive), String(deltas.falseNegative),
            String(deltas.falseSplit), String(deltas.falseMerge),
        ]
    }

    private static func deltas(_ value: ManualLearningHoldoutFamilyComparison) -> Deltas {
        Deltas(
            f1: value.f1Delta, iou: value.meanBestSegmentIoUDelta,
            boundary: value.meanMatchedBoundaryErrorDelta,
            falsePositive: value.falsePositiveDelta, falseNegative: value.falseNegativeDelta,
            falseSplit: value.falseSplitDelta, falseMerge: value.falseMergeDelta
        )
    }

    private static func deltas(_ value: ManualLearningHoldoutTrainComparison) -> Deltas {
        Deltas(
            f1: value.f1Delta, iou: value.meanBestSegmentIoUDelta,
            boundary: value.meanMatchedBoundaryErrorDelta,
            falsePositive: value.falsePositiveDelta, falseNegative: value.falseNegativeDelta,
            falseSplit: value.falseSplitDelta, falseMerge: value.falseMergeDelta
        )
    }

    private static func metricFields(_ value: ManualLearningDetectorMetrics) -> [String] {
        [
            String(value.predictedPositiveISICount), String(value.truePositiveISICount),
            String(value.falsePositiveISICount), String(value.falseNegativeISICount),
            String(value.trueNegativeISICount), number(value.precision), number(value.recall),
            number(value.f1), String(value.truthSegmentCount),
            String(value.predictedSegmentCount), number(value.meanBestSegmentIoU),
            number(value.meanMatchedBoundaryErrorISI), String(value.falseSplitCount),
            String(value.falseMergeCount),
        ]
    }

    private static func jsonArray(_ values: [String]) -> String { json(values) }

    private static func jsonString(_ value: String) -> String {
        guard let data = try? JSONSerialization.data(
            withJSONObject: value,
            options: [.fragmentsAllowed]
        ), let result = String(data: data, encoding: .utf8) else { return "\"\"" }
        return result
    }

    private static func settingsJSON(_ entries: [DetectionSettingEntry]) -> String {
        json(entries.map { ["key": $0.key, "value": $0.value] })
    }

    private static func contributionsJSON(
        _ values: [LearnedThresholdContribution],
        family: String?
    ) -> String {
        let selected = values.filter { $0.family == family }.sorted {
            if $0.family != $1.family { return $0.family < $1.family }
            return $0.field < $1.field
        }
        return json(selected.map { value in
            [
                "family": value.family, "field": value.field,
                "source_label": value.sourceLabel, "statistic": value.statistic,
                "value_kind": value.value.seconds == nil ? "spike_count" : "seconds",
                "value": value.value.seconds.map(number)
                    ?? value.value.spikeCount.map(String.init) ?? "",
                "mode": value.mode.rawValue,
                "evidence_run_count": String(value.evidenceRunCount),
                "train_count": String(value.trainCount),
                "covered_isi_count": String(value.coveredISICount),
                "support_score": number(value.supportScore),
            ]
        })
    }

    private static func json(_ object: Any) -> String {
        guard JSONSerialization.isValidJSONObject(object),
              let data = try? JSONSerialization.data(withJSONObject: object, options: [.sortedKeys]),
              let value = String(data: data, encoding: .utf8) else { return "[]" }
        return value
    }

    private static func number(_ value: Double?) -> String {
        guard let value, value.isFinite else { return "" }
        return String(format: "%.17g", locale: Locale(identifier: "en_US_POSIX"), value)
    }
}
