ui_state_internal <- function(name) {
  getFromNamespace(name, "SpikeTrainPatternDetector")
}

ui_state_make_dataset <- function(two_trains = TRUE) {
  make_train <- function(offset = 0) {
    timestamp <- offset + c(0, 0.01, 0.03, 0.07, 0.12)
    data.frame(
      idx = seq_along(timestamp),
      timestamp_sec = timestamp,
      ISI_sec = c(NA_real_, diff(timestamp)),
      pattern_manual = rep("", length(timestamp)),
      pattern_manual_negative = rep("", length(timestamp)),
      pattern_auto = rep("", length(timestamp)),
      stringsAsFactors = FALSE
    )
  }
  trains <- list(train_1 = make_train())
  if (isTRUE(two_trains)) trains$train_2 <- make_train(0.002)
  list(
    trains = trains,
    task_events = data.frame(),
    meta = list(
      display_name = "ui_state_fixture",
      input_sha256 = paste(rep("a", 64L), collapse = "")
    ),
    results = list()
  )
}

ui_state_attach_run <- function(ds, params, selected = names(ds$trains),
                                run_id = "ui_state_run_1") {
  ds$params_effective <- params
  result_row <- data.frame(
    train = selected[[1L]], start_isi = 2L, end_isi = 3L,
    run_id = run_id, params_hash = stpd_params_hash(params),
    stringsAsFactors = FALSE
  )
  ds$results$events <- result_row
  ds$results$candidate_ledger <- result_row
  ds$results$event_ledger <- result_row
  ds$results$candidate_features <- result_row
  ds$results$final_classification_audit <- result_row
  ds$results$run_metadata_public <- data.frame(
    run_id = run_id,
    params_hash = stpd_params_hash(params),
    dataset_name = ds$meta$display_name,
    input_sha256 = ds$meta$input_sha256,
    total_train_n = as.integer(length(ds$trains)),
    selected_train_n = as.integer(length(selected)),
    selected_trains = paste(selected, collapse = ";"),
    stringsAsFactors = FALSE
  )
  ds
}

ui_state_snapshot <- function(ds, params, dataset_id = "dataset_a") {
  ui_state_internal("stpd_ui_run_identity")(
    ds, params = params, dataset_id = dataset_id
  )
}

test_that("UI run state returns a stable status contract", {
  state_fun <- ui_state_internal("stpd_ui_run_state")
  ds <- ui_state_make_dataset()
  state <- state_fun(ds, params = default_params(), dataset_id = "dataset_a")

  expect_s3_class(state, "stpd_ui_status")
  expect_identical(
    names(state),
    c(
      "code", "level", "title", "detail", "scope_n", "scope_total",
      "scope_label", "selected_trains", "reason_codes", "verifiable"
    )
  )
  expect_identical(state$code, "run_not_started")
  expect_identical(state$scope_label, "0/2")
  expect_false(state$verifiable)
})

test_that("UI run state distinguishes current whole and partial scopes", {
  state_fun <- ui_state_internal("stpd_ui_run_state")
  params <- default_params()
  whole <- ui_state_attach_run(ui_state_make_dataset(), params)
  whole_snapshot <- ui_state_snapshot(whole, params)
  whole_state <- state_fun(
    whole, params = params, dataset_id = "dataset_a",
    run_identity = whole_snapshot
  )
  expect_identical(whole_state$code, "run_current")
  expect_identical(whole_state$level, "success")
  expect_identical(whole_state$scope_label, "2/2")
  expect_true(whole_state$verifiable)

  partial <- ui_state_attach_run(
    ui_state_make_dataset(), params, selected = "train_1"
  )
  partial_snapshot <- ui_state_snapshot(partial, params)
  partial_state <- state_fun(
    partial, params = params, dataset_id = "dataset_a",
    run_identity = partial_snapshot
  )
  expect_identical(partial_state$code, "run_partial_current")
  expect_identical(partial_state$level, "info")
  expect_identical(partial_state$scope_label, "1/2")
  expect_identical(partial_state$selected_trains, "train_1")

  unselected_changed <- partial
  unselected_changed$trains$train_2$timestamp_sec[3] <- 0.033
  unselected_changed$trains$train_2$ISI_sec <- c(
    NA_real_, diff(unselected_changed$trains$train_2$timestamp_sec)
  )
  unselected_changed$trains$train_2$pattern_manual[3] <- "burst"
  unselected_changed$trains$train_2$pattern_auto[4] <- "pause"
  unselected_changed$train_settings$isi_thresholds$train_2 <- list(
    burst_max_sec = 0.03
  )
  unselected_state <- state_fun(
    unselected_changed, params = params, dataset_id = "dataset_a",
    run_identity = partial_snapshot
  )
  expect_identical(unselected_state$code, "run_partial_current")

  selected_changed <- partial
  selected_changed$trains$train_1$timestamp_sec[3] <- 0.033
  selected_state <- state_fun(
    selected_changed, params = params, dataset_id = "dataset_a",
    run_identity = partial_snapshot
  )
  expect_identical(selected_state$code, "run_data_changed")
})

test_that("formal exports require a current full run with matching provenance", {
  gate_fun <- ui_state_internal("stpd_ui_formal_export_state")
  params <- default_params()
  ds <- ui_state_attach_run(ui_state_make_dataset(), params)
  snapshot <- ui_state_snapshot(ds, params)

  ready <- gate_fun(
    ds, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_true(ready$eligible)
  expect_identical(ready$code, "formal_export_ready")
  expect_identical(ready$selected_train_n, 2L)

  partial <- ui_state_attach_run(
    ui_state_make_dataset(), params, selected = "train_1"
  )
  partial_snapshot <- ui_state_snapshot(partial, params)
  partial_gate <- gate_fun(
    partial, params = params, dataset_id = "dataset_a",
    run_identity = partial_snapshot
  )
  expect_false(partial_gate$eligible)
  expect_identical(
    partial_gate$code, "formal_export_run_partial_current"
  )

  changed_params <- params
  changed_params$spiketrainpattern$qc$artifact_min_valid_isi_sec <-
    changed_params$spiketrainpattern$qc$artifact_min_valid_isi_sec + 0.0001
  stale_gate <- gate_fun(
    ds, params = changed_params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_false(stale_gate$eligible)
  expect_identical(stale_gate$code, "formal_export_run_params_changed")

  tampered_metadata <- ds
  tampered_metadata$results$run_metadata_public$input_sha256 <-
    paste(rep("b", 64L), collapse = "")
  tampered_metadata$results$run_metadata_public$dataset_name <- "FOREIGN_NAME"
  metadata_gate <- gate_fun(
    tampered_metadata, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_false(metadata_gate$eligible)
  expect_identical(
    metadata_gate$code, "formal_export_run_metadata_mismatch"
  )

  tampered_effective <- ds
  tampered_effective$params_effective <- changed_params
  effective_gate <- gate_fun(
    tampered_effective, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_false(effective_gate$eligible)
  expect_identical(
    effective_gate$code, "formal_export_run_metadata_mismatch"
  )
})

test_that("UI run state distinguishes parameter, data, and dataset changes", {
  state_fun <- ui_state_internal("stpd_ui_run_state")
  params <- default_params()
  ds <- ui_state_attach_run(ui_state_make_dataset(), params)
  snapshot <- ui_state_snapshot(ds, params)

  changed_params <- params
  changed_params$spiketrainpattern$qc$artifact_min_valid_isi_sec <-
    changed_params$spiketrainpattern$qc$artifact_min_valid_isi_sec + 0.0001
  param_state <- state_fun(
    ds, params = changed_params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_identical(param_state$code, "run_params_changed")
  expect_true("params_changed" %in% param_state$reason_codes)

  changed_data <- ds
  changed_data$trains$train_1$timestamp_sec[3] <- 0.031
  data_state <- state_fun(
    changed_data, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_identical(data_state$code, "run_data_changed")
  expect_true("spike_data_changed" %in% data_state$reason_codes)

  changed_auto <- ds
  changed_auto$trains$train_1$pattern_auto[3] <- "burst"
  changed_auto$trains$train_1$auto_score <- rep(NA_real_, 5L)
  changed_auto$trains$train_1$auto_score[3] <- 0.9
  auto_state <- state_fun(
    changed_auto, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_identical(auto_state$code, "run_output_modified")
  expect_true("auto_output_changed" %in% auto_state$reason_codes)

  changed_manual <- ds
  changed_manual$trains$train_1$pattern_manual[3] <- "burst"
  manual_state <- state_fun(
    changed_manual, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_identical(manual_state$code, "run_manual_context_changed")

  changed_context <- ds
  changed_context$train_settings <- list(
    isi_thresholds = list(train_1 = list(burst_max_sec = 0.02))
  )
  context_state <- state_fun(
    changed_context, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_identical(context_state$code, "run_detector_context_changed")

  foreign_state <- state_fun(
    ds, params = params, dataset_id = "dataset_b",
    run_identity = snapshot
  )
  expect_identical(foreign_state$code, "run_foreign_dataset")
  expect_identical(foreign_state$level, "danger")
})

test_that("UI run state fails soft when provenance cannot be verified", {
  state_fun <- ui_state_internal("stpd_ui_run_state")
  params <- default_params()
  ds <- ui_state_attach_run(ui_state_make_dataset(), params)
  snapshot <- ui_state_snapshot(ds, params)
  snapshot$run_id <- ""
  snapshot$data_sha256 <- ""
  snapshot$ui_params_sha256 <- ""
  snapshot$verifiable <- FALSE

  state <- state_fun(
    ds, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_identical(state$code, "run_identity_unverifiable")
  expect_identical(state$level, "warning")
  expect_false(state$verifiable)
})

test_that("a detector result with zero selected trains is unverifiable", {
  state_fun <- ui_state_internal("stpd_ui_run_state")
  params <- default_params()
  ds <- ui_state_attach_run(ui_state_make_dataset(), params)
  snapshot <- ui_state_snapshot(ds, params)
  snapshot$selected_train_n <- 0L
  snapshot$selected_trains <- character()
  snapshot$verifiable <- TRUE

  state <- state_fun(
    ds, params = params, dataset_id = "dataset_a",
    run_identity = snapshot
  )
  expect_identical(state$code, "run_identity_unverifiable")
  expect_false(state$verifiable)

  rebuilt <- stpd_ui_run_identity(
    ds, params = params, dataset_id = "dataset_a",
    selected_trains = character()
  )
  # Metadata restores the valid stored scope; a forged zero-scope snapshot is
  # never treated as current by the state evaluator above.
  expect_true(rebuilt$verifiable)
  expect_identical(rebuilt$selected_train_n, 2L)
})

test_that("dataset identity separates spike data from MANUAL and Review truth", {
  identity_fun <- ui_state_internal("stpd_ui_dataset_identity")
  ds <- ui_state_make_dataset()
  base <- identity_fun(ds, "dataset_a")

  manual <- ds
  manual$trains$train_1$pattern_manual[3] <- "burst"
  manual_identity <- identity_fun(manual, "dataset_a")
  expect_identical(manual_identity$data_sha256, base$data_sha256)
  expect_false(identical(
    manual_identity$manual_truth_sha256, base$manual_truth_sha256
  ))
  expect_identical(
    manual_identity$review_truth_sha256, base$review_truth_sha256
  )

  review <- ds
  review$results$multitrack_review <- list(
    transition_history = data.frame(
      transition_id = "transition_1", transition_action = "confirm",
      stringsAsFactors = FALSE
    )
  )
  review_identity <- identity_fun(review, "dataset_a")
  expect_identical(review_identity$data_sha256, base$data_sha256)
  expect_identical(
    review_identity$manual_truth_sha256, base$manual_truth_sha256
  )
  expect_false(identical(
    review_identity$review_truth_sha256, base$review_truth_sha256
  ))
})

test_that("validation state detects MANUAL and Review truth changes", {
  dataset_identity_fun <- ui_state_internal("stpd_ui_dataset_identity")
  validation_identity_fun <- ui_state_internal("stpd_ui_validation_identity")
  validation_state_fun <- ui_state_internal("stpd_ui_validation_state")
  params <- default_params()

  manual_ds <- ui_state_attach_run(ui_state_make_dataset(), params)
  manual_run <- ui_state_snapshot(manual_ds, params)
  manual_dataset_identity <- dataset_identity_fun(manual_ds, "dataset_a")
  report <- list(meta = data.frame(
    validation_run_id = "validation_1", stringsAsFactors = FALSE
  ))
  manual_validation <- validation_identity_fun(
    report,
    dataset_identity = manual_dataset_identity,
    run_identity = manual_run,
    selected_trains = names(manual_ds$trains),
    truth_dependencies = "manual"
  )
  current <- validation_state_fun(
    manual_ds, params = params, dataset_id = "dataset_a",
    run_identity = manual_run, validation_identity = manual_validation
  )
  expect_identical(current$code, "validation_current")

  manual_changed <- manual_ds
  manual_changed$trains$train_1$pattern_manual[3] <- "burst"
  manual_state <- validation_state_fun(
    manual_changed, params = params, dataset_id = "dataset_a",
    run_identity = manual_run, validation_identity = manual_validation
  )
  expect_identical(manual_state$code, "validation_manual_truth_changed")

  review_ds <- manual_ds
  review_ds$results$multitrack_review <- list(
    transition_history = data.frame(
      transition_id = "transition_1", transition_action = "confirm",
      stringsAsFactors = FALSE
    )
  )
  review_run <- ui_state_snapshot(review_ds, params)
  review_validation <- validation_identity_fun(
    report,
    dataset_identity = dataset_identity_fun(review_ds, "dataset_a"),
    run_identity = review_run,
    selected_trains = names(review_ds$trains),
    truth_dependencies = "review"
  )
  review_changed <- review_ds
  review_changed$results$multitrack_review$transition_history <- rbind(
    review_changed$results$multitrack_review$transition_history,
    data.frame(
      transition_id = "transition_2", transition_action = "revoke",
      stringsAsFactors = FALSE
    )
  )
  review_state <- validation_state_fun(
    review_changed, params = params, dataset_id = "dataset_a",
    run_identity = review_run, validation_identity = review_validation
  )
  expect_identical(review_state$code, "validation_review_truth_changed")
})

test_that("validation state tracks external truth, source run, and scope", {
  dataset_identity_fun <- ui_state_internal("stpd_ui_dataset_identity")
  validation_identity_fun <- ui_state_internal("stpd_ui_validation_identity")
  validation_state_fun <- ui_state_internal("stpd_ui_validation_state")
  hash_fun <- ui_state_internal("stpd_ui_state_sha256")
  params <- default_params()
  ds <- ui_state_attach_run(ui_state_make_dataset(), params)
  run <- ui_state_snapshot(ds, params)
  dataset_identity <- dataset_identity_fun(ds, "dataset_a")
  truth_a <- hash_fun(data.frame(id = "truth_a", stringsAsFactors = FALSE))
  truth_b <- hash_fun(data.frame(id = "truth_b", stringsAsFactors = FALSE))
  report <- list(metadata = data.frame(
    schema_version = "validation_fixture_v1",
    run_id = run$run_id,
    params_hash = run$effective_params_sha256,
    truth_sha256 = truth_a,
    stringsAsFactors = FALSE
  ))
  validation <- validation_identity_fun(
    report, dataset_identity = dataset_identity, run_identity = run,
    truth_sha256 = truth_a, selected_trains = "train_1",
    truth_dependencies = "external"
  )
  partial <- validation_state_fun(
    ds, params = params, dataset_id = "dataset_a", run_identity = run,
    validation_identity = validation, current_truth_sha256 = truth_a
  )
  expect_identical(partial$code, "validation_partial_current")
  expect_identical(partial$scope_label, "1/2")

  changed_truth <- validation_state_fun(
    ds, params = params, dataset_id = "dataset_a", run_identity = run,
    validation_identity = validation, current_truth_sha256 = truth_b
  )
  expect_identical(changed_truth$code, "validation_truth_changed")

  new_run <- run
  new_run$run_id <- "ui_state_run_2"
  changed_run <- validation_state_fun(
    ds, params = params, dataset_id = "dataset_a", run_identity = new_run,
    validation_identity = validation, current_truth_sha256 = truth_a
  )
  expect_identical(changed_run$code, "validation_run_changed")

  foreign <- validation_state_fun(
    ds, params = params, dataset_id = "dataset_b", run_identity = run,
    validation_identity = validation, current_truth_sha256 = truth_a
  )
  expect_identical(foreign$code, "validation_foreign_dataset")

  unverifiable <- validation_state_fun(
    ds, params = params, dataset_id = "dataset_a", run_identity = run,
    validation_identity = validation, current_truth_sha256 = NULL
  )
  expect_identical(unverifiable$code, "validation_identity_unverifiable")
  expect_false(unverifiable$verifiable)
})

test_that("shadow-independent validation is current without a main detector run", {
  dataset_identity_fun <- ui_state_internal("stpd_ui_dataset_identity")
  validation_identity_fun <- ui_state_internal("stpd_ui_validation_identity")
  validation_state_fun <- ui_state_internal("stpd_ui_validation_state")
  params_hash_fun <- ui_state_internal("stpd_ui_state_params_sha256")
  params <- default_params()
  ds <- ui_state_make_dataset()
  dataset_identity <- dataset_identity_fun(ds, "dataset_a")
  no_run <- ui_state_snapshot(ds, params)
  expect_false(no_run$has_run)

  report <- list(meta = data.frame(
    validation_run_id = "shadow_validation_1",
    stringsAsFactors = FALSE
  ))
  validation <- validation_identity_fun(
    report,
    dataset_identity = dataset_identity,
    run_identity = no_run,
    selected_trains = names(ds$trains),
    truth_dependencies = "manual",
    source_kind = "shadow_independent",
    source_params_sha256 = params_hash_fun(params)
  )
  expect_identical(validation$source_kind, "shadow_independent")
  expect_false(validation$requires_active_run)
  expect_identical(validation$source_run_id, "")

  current <- validation_state_fun(
    ds, params = params, dataset_id = "dataset_a",
    run_identity = no_run, validation_identity = validation,
    dataset_identity = dataset_identity
  )
  expect_identical(current$code, "validation_current")

  changed_params <- params
  changed_params$spiketrainpattern$qc$artifact_min_valid_isi_sec <-
    changed_params$spiketrainpattern$qc$artifact_min_valid_isi_sec + 0.0001
  stale <- validation_state_fun(
    ds, params = changed_params, dataset_id = "dataset_a",
    run_identity = no_run, validation_identity = validation
  )
  expect_identical(stale$code, "validation_run_changed")

  changed_context <- ds
  changed_context$train_settings <- list(
    isi_thresholds = list(train_1 = list(burst_max_sec = 0.02))
  )
  stale_context <- validation_state_fun(
    changed_context, params = params, dataset_id = "dataset_a",
    run_identity = no_run, validation_identity = validation
  )
  expect_identical(stale_context$code, "validation_run_changed")
})

test_that("partial validation identity is scoped to actually scored trains", {
  dataset_identity_fun <- ui_state_internal("stpd_ui_dataset_identity")
  validation_identity_fun <- ui_state_internal("stpd_ui_validation_identity")
  validation_state_fun <- ui_state_internal("stpd_ui_validation_state")
  params_hash_fun <- ui_state_internal("stpd_ui_state_params_sha256")
  params <- default_params()
  ds <- ui_state_make_dataset()
  identity <- validation_identity_fun(
    list(meta = data.frame(
      validation_run_id = "scoped_validation_1", stringsAsFactors = FALSE
    )),
    dataset_identity = dataset_identity_fun(ds, "dataset_a"),
    run_identity = ui_state_snapshot(ds, params),
    selected_trains = "train_1",
    truth_dependencies = "manual",
    source_kind = "shadow_independent",
    source_params_sha256 = params_hash_fun(params),
    dataset = ds
  )

  unscored_changed <- ds
  unscored_changed$trains$train_2$timestamp_sec[3] <- 0.033
  unscored_changed$trains$train_2$ISI_sec <- c(
    NA_real_, diff(unscored_changed$trains$train_2$timestamp_sec)
  )
  unscored_changed$trains$train_2$pattern_manual[3] <- "burst"
  unscored_changed$train_settings$isi_thresholds$train_2 <- list(
    burst_max_sec = 0.03
  )
  unscored_state <- validation_state_fun(
    unscored_changed, params = params, dataset_id = "dataset_a",
    run_identity = ui_state_snapshot(ds, params),
    validation_identity = identity
  )
  expect_identical(unscored_state$code, "validation_partial_current")

  scored_changed <- ds
  scored_changed$trains$train_1$pattern_manual[3] <- "burst"
  scored_state <- validation_state_fun(
    scored_changed, params = params, dataset_id = "dataset_a",
    run_identity = ui_state_snapshot(ds, params),
    validation_identity = identity
  )
  expect_identical(scored_state$code, "validation_manual_truth_changed")

  zero_scope <- validation_identity_fun(
    list(meta = data.frame(
      validation_run_id = "zero_scope_validation", stringsAsFactors = FALSE
    )),
    dataset_identity = dataset_identity_fun(ds, "dataset_a"),
    run_identity = ui_state_snapshot(ds, params),
    selected_trains = character(),
    truth_dependencies = "manual",
    source_kind = "shadow_independent",
    source_params_sha256 = params_hash_fun(params),
    dataset = ds
  )
  expect_identical(zero_scope$selected_train_n, 0L)
  expect_identical(zero_scope$selected_trains, character())
})

test_that("combined UI state model is side-effect free", {
  model_fun <- ui_state_internal("stpd_ui_state_model")
  params <- default_params()
  ds <- ui_state_attach_run(ui_state_make_dataset(), params)
  before <- serialize(ds, NULL, version = 3)
  run <- ui_state_snapshot(ds, params)

  model <- model_fun(
    ds, params = params, dataset_id = "dataset_a", run_identity = run
  )
  expect_identical(model$run$code, "run_current")
  expect_identical(model$validation$code, "validation_not_started")
  expect_identical(serialize(ds, NULL, version = 3), before)
})
