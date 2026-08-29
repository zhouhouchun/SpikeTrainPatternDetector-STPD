import Foundation
import Testing
@testable import STPDCore

@Suite("Manual-learning train holdout validation")
struct ManualLearningHoldoutValidationTests {
    @Test("Effective minimum-valid-ISI conversion is exact and never silently rounded")
    func effectiveMinimumValidISIIsExact() throws {
        let exact = ManualLearningHoldoutValidationConfiguration(
            bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001234),
            qualitySettings: SpikeQualitySettings(artifactThresholdSec: 0.0009)
        )
        #expect(try exact.exactMinimumValidISIMicroseconds() == 1_234)

        let fractional = ManualLearningHoldoutValidationConfiguration(
            bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.0012345),
            qualitySettings: SpikeQualitySettings(artifactThresholdSec: 0.0009)
        )
        #expect(throws: ManualLearningHoldoutValidationError
            .minimumValidISINotExactlyRepresentable) {
            try fractional.exactMinimumValidISIMicroseconds()
        }
    }

    @Test("Support and segment metrics retain precision, geometry, and fragmentation separately")
    func metricCalculatorReportsExactSupportAndGeometry() throws {
        let truth = Set([1, 2, 3, 6, 7])
        let baseline = Set([1, 2, 6, 7, 8])
        let learned = truth
        let points = (1...8).map {
            ManualLearningHoldoutEvaluationPoint(
                trainID: "held_out",
                isiIndex: $0,
                truthIsPositive: truth.contains($0),
                baselinePredictedPositive: baseline.contains($0),
                learnedPredictedPositive: learned.contains($0)
            )
        }

        let result = ManualLearningHoldoutMetricCalculator.compare(
            family: .highFrequencySpiking,
            points: points
        )

        #expect(result.baseline.assessedISICount == 8)
        #expect(result.baseline.truePositiveISICount == 4)
        #expect(result.baseline.falsePositiveISICount == 1)
        #expect(result.baseline.falseNegativeISICount == 1)
        #expect(result.baseline.trueNegativeISICount == 2)
        #expect(abs(try #require(result.baseline.f1) - 0.8) < 1e-12)
        #expect(result.baseline.truthSegmentCount == 2)
        #expect(result.baseline.predictedSegmentCount == 2)
        #expect(abs(try #require(result.baseline.meanBestSegmentIoU) - 2.0 / 3.0) < 1e-12)
        #expect(try #require(result.baseline.meanMatchedBoundaryErrorISI) == 0.5)
        #expect(result.baseline.falseSplitCount == 0)
        #expect(result.baseline.falseMergeCount == 0)
        #expect(result.assessedTrainCount == 1)
        #expect(result.truthPositiveTrainCount == 1)
        #expect(result.explicitNegativeTrainCount == 1)

        #expect(result.learned.truePositiveISICount == 5)
        #expect(result.learned.falsePositiveISICount == 0)
        #expect(result.learned.falseNegativeISICount == 0)
        #expect(result.learned.f1 == 1)
        #expect(result.learned.meanBestSegmentIoU == 1)
        #expect(result.learned.meanMatchedBoundaryErrorISI == 0)
        #expect(abs(try #require(result.f1Delta) - 0.2) < 1e-12)
    }

    @Test("Unreviewed ISIs are absent rather than silently treated as negatives")
    func unreviewedRowsCannotBecomeFalsePositives() {
        let points = [
            ManualLearningHoldoutEvaluationPoint(
                trainID: "held_out",
                isiIndex: 4,
                truthIsPositive: true,
                baselinePredictedPositive: false,
                learnedPredictedPositive: true
            ),
        ]

        let result = ManualLearningHoldoutMetricCalculator.compare(
            family: .tonic,
            points: points
        )

        #expect(result.baseline.assessedISICount == 1)
        #expect(result.baseline.falsePositiveISICount == 0)
        #expect(result.learned.assessedISICount == 1)
        #expect(result.learned.falsePositiveISICount == 0)
    }

    @Test("Sparse review keeps support metrics but makes segment geometry unavailable")
    func sparseReviewCannotAuthorizeGeometry() {
        let points = [1, 3].map {
            ManualLearningHoldoutEvaluationPoint(
                trainID: "held_out",
                isiIndex: $0,
                truthIsPositive: true,
                baselinePredictedPositive: false,
                learnedPredictedPositive: true
            )
        }
        let result = ManualLearningHoldoutMetricCalculator.compare(
            family: .tonic,
            points: points,
            geometryCompleteTrainIDs: []
        )
        #expect(result.baseline.assessedISICount == 2)
        #expect(result.geometryCompleteTrainCount == 0)
        #expect(result.baseline.meanBestSegmentIoU == nil)
        #expect(result.learned.meanMatchedBoundaryErrorISI == nil)
        #expect(result.baseline.truthSegmentCount == 0)
        #expect(result.trainComparisons.first?.geometryIsComplete == false)
    }

    @Test("Evaluation identity changes when point placement changes despite identical metrics")
    func evaluationDigestBindsPointPlacement() {
        func comparison(indices: [Int]) -> ManualLearningHoldoutFamilyComparison {
            ManualLearningHoldoutMetricCalculator.compare(
                family: .pause,
                points: indices.map {
                    ManualLearningHoldoutEvaluationPoint(
                        trainID: "held_out",
                        isiIndex: $0,
                        truthIsPositive: true,
                        baselinePredictedPositive: false,
                        learnedPredictedPositive: true
                    )
                }
            )
        }
        let left = comparison(indices: [1, 2])
        let right = comparison(indices: [7, 8])
        #expect(left.baseline == right.baseline)
        #expect(left.learned == right.learned)
        #expect(left.heldOutEvaluationDigest != right.heldOutEvaluationDigest)
    }

    @Test("Calibration labels learn once and held-out labels only evaluate detector output")
    func endToEndValidationHasNoHeldOutLabelLeakage() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [
                8_000, 8_000, 8_000, 8_000, 8_000, 8_000,
                100_000,
                9_000, 9_000, 9_000, 9_000, 9_000, 9_000,
            ],
            "unit_b": [
                10_000, 10_000, 10_000, 10_000,
                10_000, 10_000, 10_000, 10_000,
            ],
            "unit_c": [
                11_000, 11_000, 11_000, 11_000,
                11_000, 11_000, 11_000, 11_000,
            ],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...6, .state, .highFrequencySpiking),
                ("unit_a", 8...13, .state, .highFrequencySpiking),
                ("unit_b", 1...8, .state, .highFrequencySpiking),
                ("unit_c", 1...8, .state, .highFrequencySpiking),
            ]
        )
        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: context.fingerprint,
            decisions: decisions
        )
        let split = ManualLearningHoldoutSplit(
            calibrationTrainIDs: [
                try #require(context.trainIDs["unit_a"]),
                try #require(context.trainIDs["unit_b"]),
            ],
            heldOutTrainIDs: [try #require(context.trainIDs["unit_c"])]
        )

        let first = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: draft,
            split: split
        )
        let second = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: draft,
            split: split
        )
        let hfsSummary = try #require(first.calibrationProposal.summaries.first {
            $0.family == .highFrequencySpiking
        })
        let hfsComparison = try #require(first.comparisons.first {
            $0.family == .highFrequencySpiking
        })

        // The held-out fourth HFS segment never enters the proposal.
        #expect(hfsSummary.segmentCount == 3)
        #expect(hfsSummary.usableTrainCount == 2)
        #expect(hfsSummary.directSupportISICount == 20)
        #expect(first.calibrationProposal.compatibleThresholdProposal.profile.hfs.minSpikes.mode == .hardGate)
        #expect(first.calibrationProposal.compatibleThresholdProposal.profile.hfs.minDurationSec.mode == .hardGate)
        #expect(first.calibrationTrainIDs == ["unit_a", "unit_b"])
        #expect(first.heldOutTrainIDs == ["unit_c"])
        #expect(first.excludedTrainIDs.isEmpty)
        #expect(hfsComparison.baseline.assessedISICount == 8)
        #expect(hfsComparison.baseline.truthPositiveISICount == 8)
        #expect(hfsComparison.learned.assessedISICount == 8)
        #expect(first.baselineSettingsDigest != hfsComparison.learnedSettingsDigest)
        #expect(first.reportDigest == second.reportDigest)
        #expect(first.comparisons == second.comparisons)
        #expect(first.admissions == second.admissions)

        let hfsAdmission = try #require(first.admissions.first {
            $0.family == .highFrequencySpiking
        })
        #expect(hfsAdmission.disposition == .insufficientEvidence)
        #expect(hfsAdmission.reasons.contains(.noHeldOutExplicitNegativeSupport))

        let table = ManualLearningHoldoutReportExporter.table(report: first)
        #expect(table.headers == ManualLearningHoldoutReportExporter.headers)
        #expect(table.rows.count >= ManualPatternLearningFamily.allCases.count)
        #expect(table.rows.allSatisfy { $0.count == table.headers.count })
        let reportDigestColumn = try #require(table.headers.firstIndex(of: "report_digest"))
        #expect(table.rows.allSatisfy { $0[reportDigestColumn] == first.reportDigest })
        #expect(ManualLearningHoldoutReportExporter.csv(report: first)
            .contains(first.sourceDatasetDigest))
    }

    @Test("Each learned arm changes one family over the exact current profile and scope")
    func learnedArmsAreCausallyIsolatedAndRetainBaselineInputs() throws {
        let intervals: [Int64] = [
            8_000, 8_000, 8_000, 8_000, 8_000, 8_000,
            120_000,
            50_000, 50_000, 50_000, 50_000, 50_000, 50_000,
            120_000,
            8_000, 8_000, 8_000, 8_000, 8_000, 8_000,
        ]
        let context = try makeContext(intervalsByTrain: [
            "unit_a": intervals,
            "unit_b": intervals,
            "unit_c": intervals,
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...6, .state, .highFrequencySpiking),
                ("unit_a", 8...13, .state, .tonic),
                ("unit_a", 15...20, .state, .highFrequencySpiking),
                ("unit_b", 1...6, .state, .highFrequencySpiking),
                ("unit_b", 8...13, .state, .tonic),
                ("unit_c", 1...6, .state, .highFrequencySpiking),
                ("unit_c", 8...13, .state, .tonic),
                ("unit_c", 7...7, .other, .other),
            ]
        )
        let split = ManualLearningHoldoutSplit(
            calibrationTrainIDs: [
                try #require(context.trainIDs["unit_a"]),
                try #require(context.trainIDs["unit_b"]),
            ],
            heldOutTrainIDs: [try #require(context.trainIDs["unit_c"])]
        )
        let baselineProfile = ManualThresholdProfile(
            pause: PauseManualThresholds(
                isiLower: ManualISIThreshold(mode: .softAnchor, valueSec: 0.09)
            )
        )
        let report = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            split: split,
            configuration: ManualLearningHoldoutValidationConfiguration(
                baselineManualThresholdProfile: baselineProfile,
                manualThresholdScope: ManualThresholdScope(
                    kind: .selectedTrains,
                    trainIDs: ["unit_c"]
                )
            )
        )
        let hfs = try #require(report.comparisons.first {
            $0.family == .highFrequencySpiking
        })
        let tonic = try #require(report.comparisons.first { $0.family == .tonic })
        let hfsEntries = Dictionary(uniqueKeysWithValues: hfs.learnedSettingsEntries.map {
            ($0.key, $0.value)
        })
        let tonicEntries = Dictionary(uniqueKeysWithValues: tonic.learnedSettingsEntries.map {
            ($0.key, $0.value)
        })
        #expect(hfsEntries["manual.hfs.min_spikes.mode"] == "hard_gate")
        #expect(hfsEntries["manual.tonic.isi_lower.mode"] == "automatic")
        #expect(tonicEntries["manual.tonic.isi_lower.mode"] == "soft_anchor")
        #expect(tonicEntries["manual.hfs.min_spikes.mode"] == "automatic")
        #expect(hfsEntries["manual.pause.isi_lower.value_sec"] == "0.09")
        #expect(tonicEntries["manual.pause.isi_lower.value_sec"] == "0.09")
        #expect(hfsEntries["manual.scope.kind"] == "selectedTrains")
        #expect(hfsEntries["manual.scope.train_id.000000"] == "unit_c")
        #expect(hfs.learnedSettingsDigest != tonic.learnedSettingsDigest)
    }

    @Test("Report export preserves separator-bearing Unicode IDs and reproducible settings")
    func reportExportUsesLosslessJSONCells() throws {
        let context = try makeContext(intervalsByTrain: [
            "cal|a": Array(repeating: 8_000, count: 8),
            "校准|β": Array(repeating: 9_000, count: 8),
            "=held|γ": Array(repeating: 10_000, count: 8),
            "unreviewed_δ": Array(repeating: 12_000, count: 8),
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("cal|a", 1...8, .state, .highFrequencySpiking),
                ("校准|β", 1...8, .state, .highFrequencySpiking),
                ("=held|γ", 1...4, .state, .highFrequencySpiking),
                ("=held|γ", 5...8, .other, .other),
            ]
        )
        let report = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            split: ManualLearningHoldoutSplit(
                calibrationTrainIDs: [
                    try #require(context.trainIDs["cal|a"]),
                    try #require(context.trainIDs["校准|β"]),
                ],
                heldOutTrainIDs: [
                    try #require(context.trainIDs["=held|γ"]),
                    try #require(context.trainIDs["unreviewed_δ"]),
                ]
            )
        )
        let table = ManualLearningHoldoutReportExporter.table(report: report)
        let calibrationIndex = try #require(table.headers.firstIndex(
            of: "calibration_train_ids_json"
        ))
        let settingsIndex = try #require(table.headers.firstIndex(
            of: "learned_settings_entries_json"
        ))
        let contributionIndex = try #require(table.headers.firstIndex(
            of: "learned_threshold_contributions_json"
        ))
        let scopeIndex = try #require(table.headers.firstIndex(of: "row_scope"))
        let trainIndex = try #require(table.headers.firstIndex(
            of: "held_out_train_id_json"
        ))
        let assessmentIndex = try #require(table.headers.firstIndex(
            of: "assessment_status"
        ))
        let geometryIndex = try #require(table.headers.firstIndex(
            of: "geometry_review_complete"
        ))
        let assessedTrainCountIndex = try #require(table.headers.firstIndex(
            of: "assessed_train_count"
        ))
        let familyIndex = try #require(table.headers.firstIndex(of: "family"))
        let compatibleIndex = try #require(table.headers.firstIndex(
            of: "compatible_threshold_available"
        ))
        let first = try #require(table.rows.first)
        let decodedIDs = try #require(
            try JSONSerialization.jsonObject(with: Data(first[calibrationIndex].utf8))
                as? [String]
        )
        #expect(Set(decodedIDs) == Set(["cal|a", "校准|β"]))
        let decodedSettings = try #require(
            try JSONSerialization.jsonObject(with: Data(first[settingsIndex].utf8))
                as? [[String: String]]
        )
        #expect(decodedSettings.contains { $0["key"] == "manual.scope.kind" })
        _ = try #require(
            try JSONSerialization.jsonObject(with: Data(first[contributionIndex].utf8))
                as? [[String: String]]
        )
        let trainRows = table.rows.filter { $0[scopeIndex] == "held_out_train" }
        #expect(trainRows.count
            == ManualPatternLearningFamily.allCases.count * report.heldOutTrainIDs.count)
        #expect(trainRows.allSatisfy { row in
            (try? JSONDecoder().decode(String.self, from: Data(row[trainIndex].utf8))) != nil
        })
        let unassessedRows = trainRows.filter { row in
            (try? JSONDecoder().decode(String.self, from: Data(row[trainIndex].utf8)))
                == "unreviewed_δ"
        }
        #expect(unassessedRows.allSatisfy {
            $0[assessmentIndex] == "unassessed"
                && $0[assessedTrainCountIndex] == "0"
                && $0[geometryIndex] == "false"
        })
        #expect(table.rows.filter {
            $0[scopeIndex] == "family_summary" && $0[assessmentIndex] == "unassessed"
        }.allSatisfy { $0[geometryIndex] == "false" })
        #expect(trainRows.contains { row in
            (try? JSONDecoder().decode(String.self, from: Data(row[trainIndex].utf8)))
                == "=held|γ"
        })
        #expect(!ManualLearningHoldoutReportExporter.csv(report: report)
            .contains(",=held|γ,"))
        for comparison in report.comparisons {
            let summaryRow = try #require(table.rows.first {
                $0[scopeIndex] == "family_summary"
                    && $0[familyIndex] == comparison.family.rawValue
            })
            #expect(summaryRow[compatibleIndex]
                == (comparison.hasCompatibleIntervention ? "true" : "false"))
        }
    }

    @Test("Admission is conservative, family-specific, and never hides metric trade-offs")
    func admissionPolicyRequiresCompleteEvidenceAndNoMeasuredRegression() {
        let summary = admissionSummary()
        let eligible = ManualLearningAdmissionEvaluator.assess(
            comparison: admissionComparison(
                baseline: admissionMetrics(tp: 4, fp: 2, fn: 2, tn: 4, iou: 0.60,
                                           boundary: 1.0, splits: 1, merges: 1),
                learned: admissionMetrics(tp: 5, fp: 1, fn: 1, tn: 5, iou: 0.80,
                                          boundary: 0.5, splits: 0, merges: 0)
            ),
            calibrationSummary: summary
        )
        #expect(eligible.disposition == .eligibleForExplicitApplication)
        #expect(eligible.reasons == [.observedImprovementWithoutMeasuredRegression])

        let tradeOff = ManualLearningAdmissionEvaluator.assess(
            comparison: admissionComparison(
                baseline: admissionMetrics(tp: 4, fp: 1, fn: 2, tn: 5, iou: 0.60,
                                           boundary: 1.0, splits: 0, merges: 0),
                learned: admissionMetrics(tp: 5, fp: 2, fn: 1, tn: 4, iou: 0.75,
                                          boundary: 0.8, splits: 0, merges: 0)
            ),
            calibrationSummary: summary
        )
        #expect(tradeOff.disposition == .reportOnly)
        #expect(tradeOff.reasons.contains(.falsePositivesIncreased))

        let unsupported = ManualLearningAdmissionEvaluator.assess(
            comparison: admissionComparison(
                baseline: admissionMetrics(tp: 4, fp: 2, fn: 2, tn: 4, iou: 0.60,
                                           boundary: 1.0, splits: 1, merges: 1),
                learned: admissionMetrics(tp: 5, fp: 1, fn: 1, tn: 5, iou: 0.80,
                                          boundary: 0.5, splits: 0, merges: 0),
                hasCompatibleIntervention: false
            ),
            calibrationSummary: summary
        )
        #expect(unsupported.disposition == .insufficientEvidence)
        #expect(unsupported.reasons.contains(.noCompatibleThreshold))
    }

    @Test("One regressing held-out train blocks a pooled improvement")
    func perTrainRegressionCannotBeMaskedByPooling() {
        let summary = admissionSummary()
        let improvingBaseline = admissionMetrics(
            tp: 4, fp: 3, fn: 2, tn: 3, iou: 0.50, boundary: 1.0, splits: 1, merges: 1
        )
        let improvingLearned = admissionMetrics(
            tp: 6, fp: 0, fn: 0, tn: 6, iou: 1.0, boundary: 0, splits: 0, merges: 0
        )
        let regressingBaseline = admissionMetrics(
            tp: 2, fp: 0, fn: 0, tn: 4, iou: 1.0, boundary: 0, splits: 0, merges: 0
        )
        let regressingLearned = admissionMetrics(
            tp: 2, fp: 1, fn: 0, tn: 3, iou: 1.0, boundary: 0, splits: 0, merges: 0
        )
        let pooledBaseline = admissionMetrics(
            tp: 6, fp: 3, fn: 2, tn: 7, iou: 0.75, boundary: 0.5, splits: 1, merges: 1
        )
        let pooledLearned = admissionMetrics(
            tp: 8, fp: 1, fn: 0, tn: 9, iou: 1.0, boundary: 0, splits: 0, merges: 0
        )
        let comparison = ManualLearningHoldoutFamilyComparison(
            family: .tonic,
            hasCompatibleIntervention: true,
            assessedTrainCount: 2,
            truthPositiveTrainCount: 2,
            explicitNegativeTrainCount: 2,
            geometryCompleteTrainCount: 2,
            heldOutEvaluationDigest: "evaluation",
            trainComparisons: [
                .init(
                    trainID: "improving",
                    geometryIsComplete: true,
                    baseline: improvingBaseline,
                    learned: improvingLearned
                ),
                .init(
                    trainID: "regressing",
                    geometryIsComplete: true,
                    baseline: regressingBaseline,
                    learned: regressingLearned
                ),
            ],
            baseline: pooledBaseline,
            learned: pooledLearned
        )
        #expect((comparison.f1Delta ?? 0) > 0)
        let admission = ManualLearningAdmissionEvaluator.assess(
            comparison: comparison,
            calibrationSummary: summary
        )
        #expect(admission.disposition == .reportOnly)
        #expect(admission.reasons.contains(.oneOrMoreHeldOutTrainsRegressed))
    }

    @Test("Incomplete held-out geometry is report-only even when support improves")
    func incompleteGeometryBlocksAdmission() {
        let baseline = admissionMetrics(
            tp: 4, fp: 2, fn: 2, tn: 4, iou: 0.60, boundary: 1, splits: 1, merges: 1
        )
        let learned = admissionMetrics(
            tp: 5, fp: 1, fn: 1, tn: 5, iou: 0.80, boundary: 0.5, splits: 0, merges: 0
        )
        let comparison = ManualLearningHoldoutFamilyComparison(
            family: .tonic,
            hasCompatibleIntervention: true,
            assessedTrainCount: 2,
            truthPositiveTrainCount: 2,
            explicitNegativeTrainCount: 2,
            geometryCompleteTrainCount: 1,
            heldOutEvaluationDigest: "evaluation",
            trainComparisons: [
                .init(trainID: "complete", geometryIsComplete: true,
                      baseline: baseline, learned: learned),
                .init(trainID: "partial", geometryIsComplete: false,
                      baseline: baseline, learned: learned),
            ],
            baseline: baseline,
            learned: learned
        )
        let admission = ManualLearningAdmissionEvaluator.assess(
            comparison: comparison,
            calibrationSummary: admissionSummary()
        )
        #expect(admission.disposition == .reportOnly)
        #expect(admission.reasons.contains(.heldOutGeometryReviewIncomplete))
    }

    @Test("Changing held-out labels changes evaluation only, never learning or detector settings")
    func heldOutLabelsCannotChangeLearnedInputs() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": Array(repeating: 8_000, count: 8),
            "unit_b": Array(repeating: 10_000, count: 8),
            "unit_c": Array(repeating: 11_000, count: 8),
        ])
        let calibration = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...8, .state, .highFrequencySpiking),
                ("unit_b", 1...8, .state, .highFrequencySpiking),
            ]
        )
        let heldOutHFS = try labels(
            context: context,
            specifications: [("unit_c", 1...8, .state, .highFrequencySpiking)]
        )
        let heldOutTonic = try labels(
            context: context,
            specifications: [("unit_c", 1...8, .state, .tonic)]
        )
        let split = ManualLearningHoldoutSplit(
            calibrationTrainIDs: [
                try #require(context.trainIDs["unit_a"]),
                try #require(context.trainIDs["unit_b"]),
            ],
            heldOutTrainIDs: [try #require(context.trainIDs["unit_c"])]
        )

        let hfsTruth = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: calibration + heldOutHFS
            ),
            split: split
        )
        let tonicTruth = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: calibration + heldOutTonic
            ),
            split: split
        )

        #expect(hfsTruth.calibrationProposal == tonicTruth.calibrationProposal)
        #expect(hfsTruth.baselineSettingsDigest == tonicTruth.baselineSettingsDigest)
        #expect(hfsTruth.comparisons.map(\.learnedSettingsDigest)
            == tonicTruth.comparisons.map(\.learnedSettingsDigest))
        #expect(hfsTruth.comparisons != tonicTruth.comparisons)
        #expect(hfsTruth.heldOutEvidenceDigest != tonicTruth.heldOutEvidenceDigest)
        #expect(hfsTruth.heldOutEvaluationDigest != tonicTruth.heldOutEvaluationDigest)
        #expect(hfsTruth.schemaContractDigest == ManualLearningHoldoutValidator.schemaContractDigest)
        #expect(hfsTruth.admissionContractDigest
            == ManualLearningHoldoutValidator.admissionContractDigest)
        #expect(hfsTruth.reportDigest != tonicTruth.reportDigest)
    }

    @Test("One train cannot be both calibration and held out")
    func overlappingSplitFailsClosed() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 10_000, 10_000],
            "unit_b": [20_000, 20_000, 20_000],
        ])
        let unitA = try #require(context.trainIDs["unit_a"])
        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: context.fingerprint,
            decisions: [
                CanonicalManualISILabelDecision(
                    trainID: unitA,
                    isiIndex: 1,
                    track: .event,
                    label: .pause
                ),
            ]
        )

        #expect(throws: ManualLearningHoldoutValidationError.overlappingTrainRoles([unitA])) {
            try ManualLearningHoldoutValidator.validate(
                dataset: context.dataset,
                fingerprint: context.fingerprint,
                draft: draft,
                split: ManualLearningHoldoutSplit(
                    calibrationTrainIDs: [unitA],
                    heldOutTrainIDs: [unitA]
                )
            )
        }
    }

    private typealias Context = (
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        trainIDs: [String: ScientificSpikeTrainID]
    )

    private func admissionComparison(
        baseline: ManualLearningDetectorMetrics,
        learned: ManualLearningDetectorMetrics,
        hasCompatibleIntervention: Bool = true
    ) -> ManualLearningHoldoutFamilyComparison {
        ManualLearningHoldoutFamilyComparison(
            family: .tonic,
            hasCompatibleIntervention: hasCompatibleIntervention,
            assessedTrainCount: 2,
            truthPositiveTrainCount: 2,
            explicitNegativeTrainCount: 2,
            geometryCompleteTrainCount: 2,
            heldOutEvaluationDigest: "evaluation",
            trainComparisons: [
                ManualLearningHoldoutTrainComparison(
                    trainID: "held_out_a",
                    geometryIsComplete: true,
                    baseline: baseline,
                    learned: learned
                ),
                ManualLearningHoldoutTrainComparison(
                    trainID: "held_out_b",
                    geometryIsComplete: true,
                    baseline: baseline,
                    learned: learned
                ),
            ],
            baseline: baseline,
            learned: learned
        )
    }

    private func admissionMetrics(
        tp: Int,
        fp: Int,
        fn: Int,
        tn: Int,
        iou: Double,
        boundary: Double,
        splits: Int,
        merges: Int
    ) -> ManualLearningDetectorMetrics {
        let precision = Double(tp) / Double(tp + fp)
        let recall = Double(tp) / Double(tp + fn)
        let f1 = Double(2 * tp) / Double(2 * tp + fp + fn)
        return ManualLearningDetectorMetrics(
            assessedISICount: tp + fp + fn + tn,
            truthPositiveISICount: tp + fn,
            predictedPositiveISICount: tp + fp,
            truePositiveISICount: tp,
            falsePositiveISICount: fp,
            falseNegativeISICount: fn,
            trueNegativeISICount: tn,
            precision: precision,
            recall: recall,
            f1: f1,
            truthSegmentCount: 2,
            predictedSegmentCount: 2,
            meanBestSegmentIoU: iou,
            meanMatchedBoundaryErrorISI: boundary,
            falseSplitCount: splits,
            falseMergeCount: merges
        )
    }

    private func admissionSummary() -> ManualPatternFamilyLearningSummary {
        let scalar = ManualPatternTrainBalancedScalar(
            contributingSegmentCount: 4,
            contributingTrainCount: 3,
            trainBalancedMedian: 20_000,
            minimumTrainMedian: 18_000,
            maximumTrainMedian: 22_000
        )
        return ManualPatternFamilyLearningSummary(
            family: .tonic,
            standing: .supportedMultiTrain,
            segmentCount: 4,
            usableSegmentCount: 4,
            trainCount: 3,
            usableTrainCount: 3,
            directSupportISICount: 40,
            directSupportSpikeCount: 44,
            excludedISICount: 0,
            isiQ10Microseconds: scalar,
            isiMedianMicroseconds: scalar,
            isiQ90Microseconds: scalar,
            isiQ95Microseconds: scalar,
            directDurationMicroseconds: scalar,
            directSpikeCount: scalar,
            mm: scalar,
            cv: scalar,
            cv2: scalar,
            lv: scalar,
            trainRelativeMedianPercentile: scalar,
            trainRelativeMedianRobustLogZ: scalar,
            validation: ManualPatternCrossTrainValidation(
                standing: .passed,
                heldOutTrainCount: 3,
                insideExpectedBandCount: 3,
                coverageFraction: 1
            )
        )
    }

    private func labels(
        context: Context,
        specifications: [(String, ClosedRange<Int>, ManualAnnotationSemanticTrack,
                          ManualAnnotationLabel)]
    ) throws -> [CanonicalManualISILabelDecision] {
        try specifications.flatMap { name, range, track, label in
            let trainID = try #require(context.trainIDs[name])
            return range.map {
                CanonicalManualISILabelDecision(
                    trainID: trainID,
                    isiIndex: $0,
                    track: track,
                    label: label
                )
            }
        }
    }

    private func makeContext(intervalsByTrain: [String: [Int64]]) throws -> Context {
        var trainIDs: [String: ScientificSpikeTrainID] = [:]
        let orderedNames = intervalsByTrain.keys.sorted()
        let trains = try orderedNames.map { name -> CanonicalSpikeTrain in
            let trainID = ScientificSpikeTrainID(try ScientificSemanticID(validating: name))
            trainIDs[name] = trainID
            var tick: Int64 = 0
            var ticks = [MicrosecondTick(microseconds: tick)]
            for interval in intervalsByTrain[name] ?? [] {
                tick += interval
                ticks.append(MicrosecondTick(microseconds: tick))
            }
            return CanonicalSpikeTrain(semanticID: trainID, rawTimestamps: ticks)
        }
        let groups = try orderedNames.map { name -> CanonicalEventScopeGroup in
            let trainID = try #require(trainIDs[name])
            return CanonicalEventScopeGroup(
                semanticID: ScientificEventScopeGroupID(
                    try ScientificSemanticID(validating: "group_\(name)")
                ),
                timeBasis: .recordingElapsed,
                spikeTrainReferences: [trainID],
                eventDefinitions: []
            )
        }
        let dataset = CanonicalScientificDataset(
            recordingSegment: ConfirmedRecordingSegment(
                semanticID: ScientificRecordingSegmentID(
                    try ScientificSemanticID(validating: "recording")
                ),
                regime: .continuousUntrialed,
                importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
                observationBounds: .unknownOrUnavailable
            ),
            activityMode: .putativeSingleUnit,
            spikeTrains: trains,
            eventScopeGroups: groups,
            scientificAttributeDefinitions: []
        )
        return (
            dataset,
            try CanonicalScientificDatasetFingerprinter.fingerprint(dataset),
            trainIDs
        )
    }
}
