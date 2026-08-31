reviewer_validation_event_table <- function(start_isi, end_isi,
                                            train = "train_1",
                                            pattern = "burst") {
  data.frame(
    train = rep(train, length(start_isi)),
    pattern = rep(pattern, length(start_isi)),
    start_isi = as.integer(start_isi),
    end_isi = as.integer(end_isi),
    stringsAsFactors = FALSE
  )
}

test_that("event matching is one-to-one when predictions compete for one truth", {
  pred <- reviewer_validation_event_table(c(2L, 3L), c(5L, 5L))
  truth <- reviewer_validation_event_table(2L, 5L)

  matches <- stpd_match_events(pred, truth, iou_min = 0.25)
  metrics <- stpd_event_level_metrics(pred, truth, iou_min = 0.25)

  expect_named(matches, c("pred_index", "truth_index", "iou", "pattern"))
  expect_equal(nrow(matches), 1L)
  expect_equal(length(unique(matches$pred_index)), nrow(matches))
  expect_equal(length(unique(matches$truth_index)), nrow(matches))
  expect_identical(metrics$true_positive_n[metrics$pattern == "burst"], 1L)
  expect_identical(metrics$false_positive_n[metrics$pattern == "burst"], 1L)
  expect_identical(metrics$false_negative_n[metrics$pattern == "burst"], 0L)
})

test_that("shared matcher maximizes cardinality before IoU", {
  pred <- reviewer_validation_event_table(c(2L, 11L), c(10L, 19L))
  truth <- reviewer_validation_event_table(c(2L, 4L), c(3L, 12L))

  public <- stpd_match_events(pred, truth, iou_min = 0.1)
  compatibility <- SpikeTrainPatternDetector:::stpd_match_events_greedy(
    pred, truth, iou_min = 0.1
  )
  any_label <- SpikeTrainPatternDetector:::stpd_match_events_any_label_greedy(
    pred, truth, iou_min = 0.1
  )

  # Highest-IoU-first would select prediction 1 -> truth 2 and stop at one.
  # The optimal assignment selects both eligible non-conflicting pairs.
  expect_identical(public$pred_index, c(1L, 2L))
  expect_identical(public$truth_index, c(1L, 2L))
  expect_equal(public$iou, c(2 / 9, 2 / 16))
  expect_identical(compatibility$pred_index, public$pred_index)
  expect_identical(compatibility$truth_index, public$truth_index)
  expect_identical(any_label$a_index, public$pred_index)
  expect_identical(any_label$b_index, public$truth_index)
})

test_that("shared matcher maximizes total IoU and resolves exact ties deterministically", {
  pred <- reviewer_validation_event_table(c(2L, 4L), c(10L, 12L))
  truth <- reviewer_validation_event_table(c(2L, 4L), c(10L, 12L))
  optimal <- SpikeTrainPatternDetector:::stpd_match_events_optimal(
    pred, truth, iou_min = 0.25
  )

  expect_identical(optimal$pred_index, c(1L, 2L))
  expect_identical(optimal$truth_index, c(1L, 2L))
  expect_equal(sum(optimal$iou), 2)

  tied_pred <- reviewer_validation_event_table(c(2L, 2L), c(5L, 5L))
  tied_truth <- reviewer_validation_event_table(c(2L, 2L), c(5L, 5L))
  first <- SpikeTrainPatternDetector:::stpd_match_events_optimal(
    tied_pred, tied_truth, iou_min = 0.25
  )
  second <- SpikeTrainPatternDetector:::stpd_match_events_optimal(
    tied_pred, tied_truth, iou_min = 0.25
  )

  expect_identical(first, second)
  expect_identical(first$pred_index, c(1L, 2L))
  expect_identical(first$truth_index, c(1L, 2L))
})

test_that("event matcher rejects invalid IoU thresholds", {
  pred <- reviewer_validation_event_table(2L, 5L)
  truth <- reviewer_validation_event_table(2L, 5L)
  message <- "iou_min must be one finite number in (0, 1]."

  expect_error(stpd_match_events(pred, truth, iou_min = 0), message, fixed = TRUE)
  expect_error(stpd_match_events(pred, truth, iou_min = -0.1), message, fixed = TRUE)
  expect_error(stpd_match_events(pred, truth, iou_min = 1.1), message, fixed = TRUE)
  expect_error(stpd_match_events(pred, truth, iou_min = c(0.25, 0.5)), message, fixed = TRUE)
})

test_that("scientific validation is not estimable without an independent calibration train", {
  ds <- list(
    meta = list(display_name = "hand_table", unit_in = "s"),
    trains = list(only_train = data.frame()),
    train_settings = list()
  )
  testthat::local_mocked_bindings(
    stpd_split_trains_by_manual_events = function(...) {
      data.frame(
        train = "only_train", manual_event_n = 1L,
        split = "validation", stringsAsFactors = FALSE
      )
    },
    stpd_freeze_thresholds_for_trains = function(...) {
      stop("threshold freeze must not run without an independent calibration split")
    },
    stpd_detect = function(...) {
      stop("detector must not run when held-out performance is not estimable")
    },
    .package = "SpikeTrainPatternDetector"
  )

  report <- stpd_scientific_validation_report(
    ds, default_params(), threshold_freeze = "calibration",
    bootstrap_ci = FALSE, score_calibrator = "none"
  )

  expect_identical(report$meta$validation_status, "not_estimable")
  expect_identical(report$meta$matching_rule, stpd_event_matching_rule())
  expect_identical(report$meta$validation_reason, "no_independent_calibration_split")
  expect_identical(report$meta$threshold_freeze_status, "skipped_not_estimable")
  expect_identical(report$meta$threshold_training_scope, "not_applicable_not_estimable")
  expect_identical(report$meta$threshold_training_train_n, 0L)
  expect_false(any(grepl("fallback", unlist(report$meta), fixed = TRUE)))
  expect_equal(nrow(report$calibration_metrics), 0L)
  expect_equal(nrow(report$validation_metrics), 0L)
  expect_equal(nrow(report$truth_events), 0L)
  expect_equal(nrow(report$predicted_events), 0L)
})

test_that("scientific validation freezes thresholds on calibration trains only", {
  ds <- list(
    meta = list(display_name = "hand_table", unit_in = "s"),
    trains = list(calibration = data.frame(), validation = data.frame()),
    train_settings = list()
  )
  observed <- new.env(parent = emptyenv())
  observed$freeze_trains <- character()
  observed$detect_n <- 0L
  truth <- dplyr::bind_rows(
    reviewer_validation_event_table(2L, 5L, train = "calibration"),
    reviewer_validation_event_table(2L, 5L, train = "validation")
  )
  predicted <- truth
  predicted$auto_score <- c(0.8, 0.9)

  testthat::local_mocked_bindings(
    stpd_split_trains_by_manual_events = function(...) {
      data.frame(
        train = c("calibration", "validation"),
        manual_event_n = c(1L, 1L),
        split = c("calibration", "validation"),
        stringsAsFactors = FALSE
      )
    },
    stpd_freeze_thresholds_for_trains = function(ds, params, calibration_trains, ...) {
      observed$freeze_trains <- calibration_trains
      params$detector$freeze_dataset_thresholds <- FALSE
      params
    },
    stpd_detect = function(ds, ...) {
      observed$detect_n <- observed$detect_n + 1L
      ds
    },
    stpd_extract_events_by_source = function(ds, params, source, ...) {
      if (identical(source, "manual")) truth else predicted
    },
    .package = "SpikeTrainPatternDetector"
  )

  report <- stpd_scientific_validation_report(
    ds, default_params(), threshold_freeze = "calibration",
    bootstrap_ci = FALSE, score_calibrator = "none"
  )

  expect_identical(report$meta$validation_status, "estimable")
  expect_identical(report$meta$matching_rule, stpd_event_matching_rule())
  expect_identical(
    report$meta$validation_reason,
    "independent_calibration_validation_split_available"
  )
  expect_identical(report$meta$threshold_freeze_status, "frozen")
  expect_identical(report$meta$threshold_training_scope, "calibration_only")
  expect_identical(observed$freeze_trains, "calibration")
  expect_identical(observed$detect_n, 1L)
})
