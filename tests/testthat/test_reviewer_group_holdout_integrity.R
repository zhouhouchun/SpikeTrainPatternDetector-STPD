reviewer_group_holdout_dataset <- function(train_names, train_metadata = NULL,
                                           train_columns = NULL, dataset_meta = list()) {
  trains <- setNames(lapply(seq_along(train_names), function(i) {
    out <- data.frame(timestamp_sec = c(0, 0.1), stringsAsFactors = FALSE)
    if (!is.null(train_columns)) {
      for (column in names(train_columns)) out[[column]] <- train_columns[[column]][i]
    }
    out
  }), train_names)
  dataset_meta$train_metadata <- train_metadata
  list(meta = dataset_meta, trains = trains, train_settings = list())
}

reviewer_group_holdout_manual_events <- function(train_names) {
  data.frame(
    train = train_names,
    pattern = rep("burst", length(train_names)),
    start_isi = rep(1L, length(train_names)),
    end_isi = rep(2L, length(train_names)),
    stringsAsFactors = FALSE
  )
}

test_that("stratified holdout keeps multi-stratum groups intact and deterministic", {
  strata <- data.frame(
    train = c("a_1", "a_2", "b_1", "b_2"),
    manual_event_n = 1L,
    stratum = c("alpha", "beta", "alpha", "alpha"),
    subject = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
  )

  first <- stpd_stratified_train_splits(
    strata, validation_fraction = 0.5, n_repeats = 4L,
    seed = 17L, group_col = "subject"
  )
  second <- stpd_stratified_train_splits(
    strata, validation_fraction = 0.5, n_repeats = 4L,
    seed = 17L, group_col = "subject"
  )

  expect_identical(first, second)
  expect_true(all(first$split_status == "estimable"))
  expect_identical(
    unique(first$group_stratum[first$validation_group == "subject::A"]),
    "alpha||beta"
  )
  for (repeat_id in unique(first$repeat_id)) {
    one <- first[first$repeat_id == repeat_id, , drop = FALSE]
    calibration <- unique(one$validation_group[one$split == "calibration"])
    validation <- unique(one$validation_group[one$split == "validation"])
    expect_length(intersect(calibration, validation), 0L)
    for (group in unique(one$validation_group)) {
      assigned <- unique(one$split[one$validation_group == group & one$split != "unlabeled"])
      expect_length(assigned, 1L)
    }
  }
})

test_that("group resolution honors explicit metadata then subject priority", {
  train_names <- paste0("train_", 1:4)
  dataset_metadata <- data.frame(
    train = train_names,
    subject = c("S1", "S1", "S2", "S2"),
    animal = c("A1", "A2", "A3", "A4"),
    stringsAsFactors = FALSE
  )
  explicit_metadata <- data.frame(
    train = train_names,
    cohort = c("C1", "C1", "C2", "C2"),
    stringsAsFactors = FALSE
  )
  ds <- reviewer_group_holdout_dataset(train_names, train_metadata = dataset_metadata)
  testthat::local_mocked_bindings(
    stpd_extract_events_by_source = function(ds, params, source, selected_trains, ...) {
      reviewer_group_holdout_manual_events(selected_trains)
    },
    .package = "SpikeTrainPatternDetector"
  )

  automatic <- stpd_train_validation_strata(ds, default_params_sec())
  explicit <- stpd_train_validation_strata(
    ds, default_params_sec(), metadata = explicit_metadata, group_col = "cohort"
  )

  expect_identical(automatic$validation_group, c("subject::S1", "subject::S1", "subject::S2", "subject::S2"))
  expect_true(all(automatic$validation_group_col == "subject"))
  expect_true(all(automatic$validation_group_source == "dataset_train_metadata"))
  expect_identical(explicit$validation_group, c("cohort::C1", "cohort::C1", "cohort::C2", "cohort::C2"))
  expect_true(all(explicit$validation_group_source == "explicit_metadata"))
})

test_that("group resolution uses train metadata and falls back to train identity", {
  train_names <- paste0("train_", 1:3)
  ds_animal <- reviewer_group_holdout_dataset(
    train_names,
    train_columns = list(animal = c("A", "A", "B"))
  )
  ds_train <- reviewer_group_holdout_dataset(train_names)
  testthat::local_mocked_bindings(
    stpd_extract_events_by_source = function(ds, params, source, selected_trains, ...) {
      reviewer_group_holdout_manual_events(selected_trains)
    },
    .package = "SpikeTrainPatternDetector"
  )

  animal <- stpd_train_validation_strata(ds_animal, default_params_sec())
  fallback <- stpd_train_validation_strata(ds_train, default_params_sec())

  expect_identical(animal$validation_group, c("animal::A", "animal::A", "animal::B"))
  expect_true(all(animal$validation_group_source == "train_column"))
  expect_identical(fallback$validation_group, paste0("train::", train_names))
  expect_true(all(fallback$validation_group_source == "fallback_train"))

  split <- stpd_stratified_train_splits(fallback, n_repeats = 2L, seed = 3L)
  expect_true(all(split$split_status == "estimable"))
  expect_true(all(split$split_note == "stratified_train_holdout"))
})

test_that("one independent group yields a fixed not-estimable split schema", {
  strata <- data.frame(
    train = c("train_1", "train_2"),
    manual_event_n = c(1L, 1L),
    stratum = c("x", "y"),
    subject = c("same", "same"),
    stringsAsFactors = FALSE
  )
  split <- stpd_stratified_train_splits(
    strata, n_repeats = 2L, seed = 1L, group_col = "subject"
  )

  expect_true(all(split$split == "not_estimable"))
  expect_true(all(split$split_status == "not_estimable"))
  expect_true(all(split$split_reason == "insufficient_independent_groups"))
  expect_true(all(split$eligible_group_n == 1L))
  expect_true(all(split$calibration_group_n == 0L))
  expect_true(all(split$validation_group_n == 0L))
  expect_true(all(c(
    "validation_group", "group_stratum", "split_status", "split_reason",
    "eligible_group_n", "calibration_group_n", "validation_group_n"
  ) %in% names(split)))
})

test_that("calibration freeze refuses missing sides and group overlap before detection", {
  ds <- reviewer_group_holdout_dataset(c("train_1", "train_2"))
  testthat::local_mocked_bindings(
    stpd_extract_events_by_source = function(ds, params, source, selected_trains, ...) {
      reviewer_group_holdout_manual_events(selected_trains)
    },
    stpd_freeze_thresholds_for_trains = function(...) {
      stop("threshold freezing must not run for a non-estimable split")
    },
    stpd_detect = function(...) {
      stop("detection must not run for a non-estimable split")
    },
    .package = "SpikeTrainPatternDetector"
  )

  cases <- list(
    missing_calibration = data.frame(train = "train_2", split = "validation"),
    missing_validation = data.frame(train = "train_1", split = "calibration"),
    group_overlap = data.frame(
      train = c("train_1", "train_2"),
      split = c("calibration", "validation"),
      validation_group = c("subject::same", "subject::same")
    )
  )
  expected_reasons <- c(
    missing_calibration = "calibration_split_missing",
    missing_validation = "validation_split_missing",
    group_overlap = "calibration_validation_group_overlap"
  )

  for (case in names(cases)) {
    report <- stpd_event_level_validation_report(
      ds, default_params_sec(), selected_trains = names(ds$trains),
      split_table = cases[[case]], threshold_freeze = "calibration"
    )
    expect_identical(report$meta$validation_status, "not_estimable", info = case)
    expect_identical(report$meta$validation_reason, expected_reasons[[case]], info = case)
    expect_identical(report$meta$detection_executed, FALSE, info = case)
    expect_identical(report$meta$matching_rule, stpd_event_matching_rule(), info = case)
    expect_equal(nrow(report$metrics), 0L, info = case)
    expect_equal(nrow(report$predicted_events), 0L, info = case)
    expect_true(all(c(
      "frozen_score_calibrated_predictions", "manual_uncertainty_meta"
    ) %in% names(report)), info = case)
  }
})

test_that("repeated holdout does not manufacture metrics when groups are insufficient", {
  train_names <- c("train_1", "train_2")
  ds <- reviewer_group_holdout_dataset(
    train_names,
    train_metadata = data.frame(train = train_names, subject = "same")
  )
  testthat::local_mocked_bindings(
    stpd_extract_events_by_source = function(ds, params, source, selected_trains, ...) {
      reviewer_group_holdout_manual_events(selected_trains)
    },
    stpd_event_level_validation_report = function(...) {
      stop("validation report must not run without two independent groups")
    },
    stpd_detect = function(...) {
      stop("detector must not run without two independent groups")
    },
    .package = "SpikeTrainPatternDetector"
  )

  report <- stpd_repeated_train_holdout_validation(
    ds, default_params_sec(), n_repeats = 3L, group_col = "subject"
  )

  expect_identical(report$meta$validation_status, "not_estimable")
  expect_identical(report$meta$validation_reason, "insufficient_independent_groups")
  expect_identical(report$meta$estimable_repeat_n, 0L)
  expect_identical(report$meta$matching_rule, stpd_event_matching_rule())
  expect_equal(nrow(report$repeat_summary), 3L)
  expect_true(all(report$repeat_summary$validation_status == "not_estimable"))
  expect_true(all(is.na(report$repeat_summary$macro_F1)))
  expect_equal(nrow(report$repeat_metrics), 0L)
  expect_equal(nrow(report$summary), 0L)
  expect_true(all(c("pattern", "F1", "repeat_id") %in% names(report$repeat_metrics)))
  expect_true(all(c("pattern", "mean_F1", "repeat_n") %in% names(report$summary)))
})

test_that("repeated holdout evaluates only disjoint groups on the estimable path", {
  train_names <- paste0("train_", 1:4)
  ds <- reviewer_group_holdout_dataset(
    train_names,
    train_metadata = data.frame(
      train = train_names,
      subject = c("S1", "S1", "S2", "S2"),
      stringsAsFactors = FALSE
    )
  )
  evaluated <- new.env(parent = emptyenv())
  evaluated$n <- 0L
  testthat::local_mocked_bindings(
    stpd_extract_events_by_source = function(ds, params, source, selected_trains, ...) {
      reviewer_group_holdout_manual_events(selected_trains)
    },
    stpd_event_level_validation_report = function(ds, params, selected_trains, split_table, ...) {
      calibration <- unique(split_table$validation_group[split_table$split == "calibration"])
      validation <- unique(split_table$validation_group[split_table$split == "validation"])
      if (length(intersect(calibration, validation)) > 0L) stop("group leakage reached evaluation")
      evaluated$n <- evaluated$n + 1L
      list(
        meta = data.frame(
          validation_status = "estimable",
          validation_reason = "independent_calibration_validation_split_available"
        ),
        metrics = data.frame(
          split = "validation", metric_mode = "strict_high_confidence",
          pattern = "burst", truth_n = 1L, predicted_n = 1L,
          true_positive_n = 1L, false_positive_n = 0L, false_negative_n = 0L,
          precision = 1, recall = 1, F1 = 1, stringsAsFactors = FALSE
        )
      )
    },
    .package = "SpikeTrainPatternDetector"
  )

  report <- stpd_repeated_train_holdout_validation(
    ds, default_params_sec(), n_repeats = 2L,
    validation_fraction = 0.5, group_col = "subject"
  )

  expect_identical(evaluated$n, 2L)
  expect_identical(report$meta$validation_status, "estimable")
  expect_identical(report$meta$estimable_repeat_n, 2L)
  expect_true(all(report$repeat_summary$calibration_validation_group_overlap_n == 0L))
  expect_true(all(report$repeat_summary$validation_status == "estimable"))
  expect_equal(report$repeat_summary$macro_F1, c(1, 1))
  expect_equal(nrow(report$repeat_metrics), 2L)
  expect_equal(report$summary$mean_F1, 1)
})

test_that("nested tuning retains baseline when inner groups are insufficient", {
  train_names <- c("train_1", "train_2")
  ds <- reviewer_group_holdout_dataset(
    train_names,
    train_metadata = data.frame(train = train_names, subject = "same")
  )
  testthat::local_mocked_bindings(
    stpd_extract_events_by_source = function(ds, params, source, selected_trains, ...) {
      reviewer_group_holdout_manual_events(selected_trains)
    },
    stpd_event_level_validation_report = function(...) {
      stop("inner validation must not run without two independent groups")
    },
    .package = "SpikeTrainPatternDetector"
  )

  tuned <- stpd_nested_tune_params(
    ds, default_params_sec(), calibration_trains = train_names,
    group_col = "subject"
  )

  expect_identical(tuned$selected_variant_id, "baseline_current")
  expect_identical(tuned$validation_status, "not_estimable")
  expect_identical(tuned$validation_reason, "insufficient_independent_groups")
  expect_true(tuned$tuning_table$selected)
  expect_true(all(c(
    "macro_F1", "selection_metric", "validation_status", "validation_reason", "note"
  ) %in% names(tuned$tuning_table)))
})
