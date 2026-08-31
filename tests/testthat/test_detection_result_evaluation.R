test_that("standalone evaluation separates event discovery from boundary accuracy", {
  # A five-spike reference Burst contains four canonical ISIs. Predicting the
  # first four spikes covers three of those four ISIs, hence IoU = 0.75.
  reference <- data.frame(
    train = "train_1", pattern = "burst", start_isi = 10L, end_isi = 13L,
    stringsAsFactors = FALSE
  )
  predicted <- data.frame(
    train = "train_1", pattern = "burst", start_isi = 10L, end_isi = 12L,
    stringsAsFactors = FALSE
  )
  predicted_before <- serialize(predicted, NULL, version = 3)
  reference_before <- serialize(reference, NULL, version = 3)

  result <- stpd_evaluate_detection_results(predicted, reference)

  expect_s3_class(result, "stpd_detection_evaluation")
  expect_equal(result$protocol$iou_threshold, c(0.25, 0.50, 0.75))
  expect_equal(result$event_metrics$true_positive_n, c(1L, 1L, 1L))
  expect_equal(result$event_metrics$recall, c(1, 1, 1))
  expect_equal(result$matches$iou, c(0.75, 0.75, 0.75))
  expect_equal(result$matches$truth_isi_recovery, c(0.75, 0.75, 0.75))
  expect_false(any(result$matches$exact_boundary_match))
  expect_equal(result$isi_metrics$truth_isi_recall, 0.75)
  expect_equal(result$isi_metrics$predicted_isi_precision, 1)
  expect_false(result$fragmentation_by_reference$fragmented)
  expect_equal(result$fragmentation_metrics$false_split_rate_all_truth, 0)
  expect_identical(serialize(predicted, NULL, version = 3), predicted_before)
  expect_identical(serialize(reference, NULL, version = 3), reference_before)
})

test_that("strict thresholds and exact boundaries are reported independently", {
  reference <- data.frame(
    train = c("train_1", "train_1"), pattern = c("burst", "burst"),
    start_isi = c(1L, 20L), end_isi = c(4L, 23L), stringsAsFactors = FALSE
  )
  predicted <- data.frame(
    train = c("train_1", "train_1"), pattern = c("burst", "burst"),
    start_isi = c(1L, 20L), end_isi = c(4L, 21L), stringsAsFactors = FALSE
  )

  result <- stpd_evaluate_detection_results(predicted, reference)
  metric <- result$event_metrics
  expect_equal(metric$true_positive_n, c(2L, 2L, 1L))
  expect_equal(metric$exact_boundary_match_n, c(1L, 1L, 1L))
  expect_equal(metric$recall, c(1, 1, 0.5))
  expect_equal(result$isi_metrics$truth_isi_recall, 0.75)
  expect_equal(result$isi_metrics$predicted_isi_precision, 1)
})

test_that("result evaluation is label- and detector-isolated", {
  expect_false(any(c("ds", "params", "detector") %in% names(formals(stpd_evaluate_detection_results))))
  empty <- data.frame(train = character(), pattern = character(),
                      start_isi = integer(), end_isi = integer())
  result <- stpd_evaluate_detection_results(empty, empty)
  expect_equal(nrow(result$event_metrics), 0L)
  expect_equal(nrow(result$matches), 0L)
  expect_equal(nrow(result$fragmentation_by_reference), 0L)
  expect_equal(nrow(result$fragmentation_metrics), 0L)
  expect_error(
    stpd_evaluate_detection_results(
      data.frame(train = "t", pattern = "burst", start_isi = 0, end_isi = 1), empty
    ),
    "invalid train/class values or non-integer ISI bounds",
    fixed = TRUE
  )
  expect_error(stpd_evaluate_detection_results(empty, empty, iou_thresholds = c(0, 0.5)),
               "iou_thresholds must contain finite, unique values in (0, 1].", fixed = TRUE)
  expect_error(stpd_evaluate_detection_results(empty, empty, iou_thresholds = c(0.5, 0.5)),
               "iou_thresholds must contain finite, unique values in (0, 1].", fixed = TRUE)
})

test_that("zero-match event F1 is zero rather than missing", {
  reference <- data.frame(
    train = "train_1", pattern = "burst", start_isi = 1L, end_isi = 4L,
    stringsAsFactors = FALSE
  )
  predicted <- data.frame(
    train = "train_1", pattern = "burst", start_isi = 20L, end_isi = 23L,
    stringsAsFactors = FALSE
  )

  result <- stpd_evaluate_detection_results(predicted, reference)

  expect_equal(result$protocol$iou_threshold, c(0.25, 0.50, 0.75))
  expect_equal(result$event_metrics$true_positive_n, c(0L, 0L, 0L))
  expect_equal(result$event_metrics$precision, c(0, 0, 0))
  expect_equal(result$event_metrics$recall, c(0, 0, 0))
  expect_equal(result$event_metrics$F1, c(0, 0, 0))

  empty <- predicted[0, , drop = FALSE]
  expect_equal(
    stpd_evaluate_detection_results(empty, reference)$event_metrics$F1,
    c(0, 0, 0)
  )
  expect_equal(
    stpd_evaluate_detection_results(predicted, empty)$event_metrics$F1,
    c(0, 0, 0)
  )
})

test_that("reference fragmentation is independent of one-to-one IoU matching", {
  reference <- data.frame(
    train = c("train_1", "train_1"), pattern = c("burst", "burst"),
    start_isi = c(10L, 40L), end_isi = c(20L, 50L),
    stringsAsFactors = FALSE
  )
  predicted <- data.frame(
    train = rep("train_1", 3L), pattern = rep("burst", 3L),
    start_isi = c(10L, 16L, 70L), end_isi = c(14L, 20L, 80L),
    stringsAsFactors = FALSE
  )

  result <- stpd_evaluate_detection_results(predicted, reference)
  detail <- result$fragmentation_by_reference
  metric <- result$fragmentation_metrics

  expect_equal(result$protocol$iou_threshold, c(0.25, 0.50, 0.75))
  expect_equal(result$event_metrics$true_positive_n, c(1L, 0L, 0L))
  expect_equal(result$event_metrics$F1, c(0.4, 0, 0))
  expect_equal(detail$overlapping_predicted_n, c(2L, 0L))
  expect_identical(detail$overlapping_predicted_indices, c("1;2", ""))
  expect_equal(detail$recovered_truth_isi_n, c(10L, 0L))
  expect_equal(detail$truth_isi_recovery, c(10 / 11, 0))
  expect_identical(detail$detected_any_overlap, c(TRUE, FALSE))
  expect_identical(detail$fragmented, c(TRUE, FALSE))
  expect_equal(detail$excess_fragment_n, c(1L, 0L))
  expect_equal(metric$truth_n, 2L)
  expect_equal(metric$detected_truth_n, 1L)
  expect_equal(metric$fragmented_truth_n, 1L)
  expect_equal(metric$total_excess_fragment_n, 1L)
  expect_equal(metric$false_split_rate_all_truth, 0.5)
  expect_equal(metric$false_split_rate_detected_truth, 1)
  expect_equal(metric$excess_fragments_per_truth, 0.5)
  expect_equal(metric$mean_fragments_per_detected_truth, 2)
})

test_that("one prediction spanning two references is not a false split", {
  reference <- data.frame(
    train = c("train_1", "train_1"), pattern = c("burst", "burst"),
    start_isi = c(10L, 20L), end_isi = c(14L, 24L),
    stringsAsFactors = FALSE
  )
  predicted <- data.frame(
    train = "train_1", pattern = "burst", start_isi = 9L, end_isi = 25L,
    stringsAsFactors = FALSE
  )

  result <- stpd_evaluate_detection_results(predicted, reference)

  expect_equal(result$fragmentation_by_reference$overlapping_predicted_n,
               c(1L, 1L))
  expect_false(any(result$fragmentation_by_reference$fragmented))
  expect_equal(result$fragmentation_metrics$false_split_rate_all_truth, 0)
  expect_equal(result$fragmentation_metrics$mean_fragments_per_detected_truth, 1)
})
