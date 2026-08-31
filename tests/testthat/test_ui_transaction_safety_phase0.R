ui_transaction_fixture <- function(use_reactive_values = FALSE) {
  state <- list(
    datasets = list(
      dataset_a = list(
        trains = list(
          train_1 = data.frame(
            idx = 1:3,
            pattern_manual = c("", "burst", ""),
            pattern_manual_negative = c("", "", "not_burst"),
            pattern_auto = c("tonic", "burst", "pause"),
            stringsAsFactors = FALSE
          )
        ),
        results = list(
          final_audit = data.frame(event_id = "audit_1", stringsAsFactors = FALSE),
          multitrack_review_history = data.frame(
            transition_id = "transition_1",
            stringsAsFactors = FALSE
          )
        ),
        meta = list(display_name = "Dataset A")
      ),
      dataset_b = list(
        trains = list(
          train_2 = data.frame(
            idx = 1:2,
            pattern_manual = c("tonic", ""),
            pattern_auto = c("tonic", "tonic"),
            stringsAsFactors = FALSE
          )
        ),
        results = list(run_id = "run_b"),
        meta = list(display_name = "Dataset B")
      )
    ),
    current_id = "dataset_a",
    manual_detector_eval = list(
      dataset_id = "dataset_a",
      metrics = data.frame(precision = 0.5)
    ),
    scientific_validation = list(
      dataset_id = "dataset_a",
      metrics = data.frame(f1 = 0.4),
      estimand = "label_blind_detector_performance"
    ),
    parameter_sensitivity = list(hash = "sensitivity_before"),
    parameter_sensitivity_status = "complete",
    possible_burst_promotion_preview = list(total_eligible_events = 1L),
    possible_burst_promotion_status = "previewed",
    final_audit_last_summary = data.frame(n_promoted = 1L),
    final_audit_last_events = data.frame(event_id = "audit_1"),
    final_audit_status = "current",
    last_detector_summary = "run before",
    near_miss_rerun_summary = "near miss before",
    batch_status = "batch before",
    preview_candidate = list(candidate_id = "candidate_1"),
    cluster_a = list(train = "train_1", idx = 2L),
    cluster_b = NULL,
    last_plotly_selection = data.frame(x = 1, y = 1),
    view_align_x = c(0, 1),
    raw_view_x = c(0, 1)
  )

  mutable_cache <- new.env(parent = emptyenv())
  mutable_cache$value <- "before"
  state$mutable_cache <- mutable_cache

  if (isTRUE(use_reactive_values)) return(do.call(shiny::reactiveValues, state))
  rv <- new.env(parent = emptyenv())
  list2env(state, envir = rv)
  rv
}

ui_transaction_state_copy <- function(rv) {
  # Serialized comparison is reference-independent: two separately cloned
  # environments with the same contents are the same transaction state even
  # though R correctly gives them different object identities.
  snapshot <- SpikeTrainPatternDetector:::stpd_ui_transaction_capture(rv)
  state <- SpikeTrainPatternDetector:::stpd_ui_transaction_snapshot_state(snapshot)
  serialize(state, NULL, version = 3L)
}

test_that("transaction action metadata uses a stable schema and supported IDs", {
  registry <- SpikeTrainPatternDetector:::stpd_ui_transaction_action_registry()
  expect_identical(
    registry$action_id[1:7],
    c(
      "clear_all_datasets", "replace_workspace", "remove_dataset",
      "clear_manual_labels", "clear_auto_labels", "clear_final_audit",
      "legacy_overwrite_manual"
    )
  )
  expect_false(anyDuplicated(registry$action_id) > 0L)

  rv <- ui_transaction_fixture()
  record <- SpikeTrainPatternDetector:::stpd_ui_transaction_record(
    rv,
    action_id = "clear_manual_labels",
    dataset_ids = c("dataset_b", "dataset_a", "dataset_b"),
    train_ids = c("train_2", "train_1"),
    target_tracks = c("MANUAL", "Event", "MANUAL"),
    target_count = 3L,
    details = list(patterns = c("burst", "not_burst")),
    now = as.POSIXct("2026-08-16 12:34:56", tz = "UTC")
  )

  expect_identical(record$metadata$schema_version, "stpd.ui.transaction/3")
  expect_identical(record$metadata$transaction_id, "ui-tx-00000001")
  expect_identical(record$metadata$action_id, "clear_manual_labels")
  expect_identical(record$metadata$scope, "selection")
  expect_identical(record$metadata$dataset_ids, c("dataset_a", "dataset_b"))
  expect_identical(record$metadata$train_ids, c("train_1", "train_2"))
  expect_identical(record$metadata$target_tracks, c("Event", "MANUAL"))
  expect_identical(record$metadata$target_count, 3L)
  expect_identical(record$metadata$created_at_utc, "2026-08-16T12:34:56.000Z")
  expect_match(record$metadata$pre_state_sha256, "^[0-9a-f]{64}$")
  expect_identical(record$metadata$pre_state_sha256, record$snapshot$state_sha256)
  expect_identical(record$metadata$post_state_sha256, "")
  expect_identical(record$metadata$history_max_depth, 10L)
  expect_equal(record$metadata$history_max_bytes, 64 * 1024^2)
  expect_false("state" %in% names(record$snapshot))
  expect_true(is.raw(record$snapshot$state_payload))
  expect_gt(record$snapshot$uncompressed_bytes, 0)
  expect_gt(record$snapshot$compressed_bytes, 0)
  expect_error(
    SpikeTrainPatternDetector:::stpd_ui_transaction_record(rv, "unknown_action"),
    "Unsupported destructive UI action_id"
  )
})

test_that("cancelled destructive actions are exact no-ops", {
  rv <- ui_transaction_fixture()
  before <- ui_transaction_state_copy(rv)
  sequence_before <- SpikeTrainPatternDetector:::stpd_ui_transaction_store_get(
    rv, ".stpd_ui_transaction_sequence", 0L
  )

  out <- SpikeTrainPatternDetector:::stpd_ui_transaction_run(
    rv,
    action_id = "clear_all_datasets",
    confirmed = FALSE,
    mutate = function() stop("cancelled mutation must never run")
  )

  expect_false(out$applied)
  expect_true(out$cancelled)
  expect_identical(ui_transaction_state_copy(rv), before)
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 0L)
  expect_identical(
    SpikeTrainPatternDetector:::stpd_ui_transaction_store_get(
      rv, ".stpd_ui_transaction_sequence", 0L
    ),
    sequence_before
  )
})

test_that("failed destructive actions roll back atomically and do not enter undo history", {
  rv <- ui_transaction_fixture()
  before <- ui_transaction_state_copy(rv)
  sequence_before <- SpikeTrainPatternDetector:::stpd_ui_transaction_store_get(
    rv, ".stpd_ui_transaction_sequence", 0L
  )

  expect_error(
    SpikeTrainPatternDetector:::stpd_ui_transaction_run(
      rv,
      action_id = "legacy_overwrite_manual",
      dataset_ids = "dataset_a",
      train_ids = "train_1",
      target_tracks = "MANUAL",
      mutate = function() {
        rv$datasets$dataset_a$trains$train_1$pattern_manual[] <- "burst"
        rv$datasets$dataset_a$results <- list(overwritten = TRUE)
        rv$manual_detector_eval <- NULL
        rv$scientific_validation <- list(wrong = TRUE)
        rv$created_during_failed_action <- "must disappear"
        stop("simulated write failure")
      }
    ),
    "simulated write failure"
  )

  expect_identical(ui_transaction_state_copy(rv), before)
  expect_false(exists("created_during_failed_action", envir = rv, inherits = FALSE))
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 0L)
  expect_identical(
    SpikeTrainPatternDetector:::stpd_ui_transaction_store_get(
      rv, ".stpd_ui_transaction_sequence", 0L
    ),
    sequence_before
  )
})

test_that("an unfinalized production-style push can be aborted atomically", {
  rv <- ui_transaction_fixture()
  before <- ui_transaction_state_copy(rv)
  prior_ids <- SpikeTrainPatternDetector:::stpd_ui_transaction_ids(rv)
  record <- SpikeTrainPatternDetector:::stpd_ui_transaction_push(
    rv,
    action_id = "manual_label_edit",
    dataset_ids = "dataset_a",
    train_ids = "train_1"
  )
  rv$datasets$dataset_a$trains$train_1$pattern_manual[1] <- "burst"
  rv$scientific_validation <- list(partial_write = TRUE)

  aborted <- SpikeTrainPatternDetector:::stpd_ui_transaction_abort_new(
    rv, prior_ids
  )
  expect_length(aborted, 1L)
  expect_identical(aborted[[1]]$transaction_id,
                   record$metadata$transaction_id)
  expect_identical(ui_transaction_state_copy(rv), before)
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 0L)
})

test_that("failed pushes preserve history evicted only after successful finalize", {
  rv <- ui_transaction_fixture()
  for (i in 1:5) {
    SpikeTrainPatternDetector:::stpd_ui_transaction_run(
      rv,
      action_id = "manual_label_edit",
      dataset_ids = "dataset_a",
      target_count = i,
      max_depth = 5L,
      mutate = local({
        step <- i
        function() rv$last_detector_summary <- paste0("stable_", step)
      })
    )
  }
  stable_ids <- SpikeTrainPatternDetector:::stpd_ui_transaction_ids(rv)
  expect_length(stable_ids, 5L)

  prior_ids <- stable_ids
  SpikeTrainPatternDetector:::stpd_ui_transaction_push(
    rv,
    action_id = "manual_label_edit",
    dataset_ids = "dataset_a",
    max_depth = 5L,
    max_bytes = 1
  )
  # Neither the depth limit nor a deliberately tiny byte budget may consume
  # earlier recovery points before the new mutation succeeds.
  expect_length(SpikeTrainPatternDetector:::stpd_ui_transaction_ids(rv), 6L)
  SpikeTrainPatternDetector:::stpd_ui_transaction_abort_new(rv, prior_ids)
  expect_identical(
    SpikeTrainPatternDetector:::stpd_ui_transaction_ids(rv), stable_ids
  )

  # On success, finalization applies both limits and keeps the newest action.
  pushed <- SpikeTrainPatternDetector:::stpd_ui_transaction_push(
    rv,
    action_id = "manual_label_edit",
    dataset_ids = "dataset_a",
    max_depth = 5L,
    max_bytes = 1
  )
  rv$last_detector_summary <- "success_after_budget"
  SpikeTrainPatternDetector:::stpd_ui_transaction_finalize(
    rv, pushed$metadata$transaction_id
  )
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 1L)
  expect_identical(
    SpikeTrainPatternDetector:::stpd_ui_transaction_ids(rv),
    pushed$metadata$transaction_id
  )
})

test_that("undo is LIFO and restores complete datasets plus derived session results", {
  rv <- ui_transaction_fixture()
  initial <- ui_transaction_state_copy(rv)

  first <- SpikeTrainPatternDetector:::stpd_ui_transaction_run(
    rv,
    action_id = "remove_dataset",
    dataset_ids = "dataset_a",
    mutate = function() {
      rv$datasets$dataset_a <- NULL
      rv$current_id <- "dataset_b"
      rv$manual_detector_eval <- NULL
      rv$scientific_validation <- NULL
      rv$final_audit_status <- "dataset removed"
      "removed_a"
    }
  )
  after_first <- ui_transaction_state_copy(rv)

  second <- SpikeTrainPatternDetector:::stpd_ui_transaction_run(
    rv,
    action_id = "clear_all_datasets",
    mutate = function() {
      rv$datasets <- list()
      rv$current_id <- NULL
      rv$parameter_sensitivity <- NULL
      rv$possible_burst_promotion_preview <- NULL
      rv$last_detector_summary <- "cleared"
      "cleared_all"
    }
  )

  expect_true(first$applied)
  expect_true(second$applied)
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 2L)

  undo_second <- SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv)
  expect_identical(undo_second$metadata$action_id, "clear_all_datasets")
  expect_identical(ui_transaction_state_copy(rv), after_first)

  undo_first <- SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv)
  expect_identical(undo_first$metadata$action_id, "remove_dataset")
  expect_identical(ui_transaction_state_copy(rv), initial)
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 0L)
  expect_null(SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv))
})

test_that("bounded undo history retains only the newest complete transactions", {
  rv <- ui_transaction_fixture()

  for (i in 1:3) {
    SpikeTrainPatternDetector:::stpd_ui_transaction_run(
      rv,
      action_id = "clear_auto_labels",
      dataset_ids = "dataset_a",
      target_count = i,
      max_depth = 2L,
      details = list(step = i),
      mutate = local({
        step <- i
        function() rv$last_detector_summary <- paste0("step_", step)
      })
    )
  }

  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 2L)
  history <- SpikeTrainPatternDetector:::stpd_ui_transaction_history(rv)
  expect_identical(vapply(history, `[[`, integer(1), "target_count"), 2:3)

  SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv)
  expect_identical(rv$last_detector_summary, "step_2")
  SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv)
  expect_identical(rv$last_detector_summary, "step_1")
  expect_null(SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv))
})

test_that("compressed undo history also obeys its cumulative byte budget", {
  rv <- ui_transaction_fixture()

  for (i in 1:3) {
    SpikeTrainPatternDetector:::stpd_ui_transaction_run(
      rv,
      action_id = "clear_auto_labels",
      dataset_ids = "dataset_a",
      target_count = i,
      max_depth = 10L,
      max_bytes = 1,
      details = list(step = i),
      mutate = local({
        step <- i
        function() rv$last_detector_summary <- paste0("budget_step_", step)
      })
    )
  }

  # The newest action remains undoable even when that one record alone is
  # larger than the configured budget; older records are evicted first.
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 1L)
  expect_identical(
    SpikeTrainPatternDetector:::stpd_ui_transaction_history(rv)[[1]]$target_count,
    3L
  )
  SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv)
  expect_identical(rv$last_detector_summary, "budget_step_2")
})

test_that("push peek pop and restore never share mutable references", {
  rv <- ui_transaction_fixture()
  SpikeTrainPatternDetector:::stpd_ui_transaction_push(
    rv,
    action_id = "clear_final_audit",
    dataset_ids = "dataset_a",
    train_ids = "train_1",
    target_tracks = "audit_final"
  )

  # A mutable environment changed after push cannot alter the saved snapshot.
  rv$mutable_cache$value <- "live changed"
  first_peek <- SpikeTrainPatternDetector:::stpd_ui_transaction_peek(rv)
  first_state <- SpikeTrainPatternDetector:::stpd_ui_transaction_snapshot_state(
    first_peek$snapshot
  )
  expect_identical(first_state$mutable_cache$value, "before")

  # A caller changing a decoded record cannot alter the compressed stack.
  first_state$mutable_cache$value <- "tampered caller copy"
  second_peek <- SpikeTrainPatternDetector:::stpd_ui_transaction_peek(rv)
  second_state <- SpikeTrainPatternDetector:::stpd_ui_transaction_snapshot_state(
    second_peek$snapshot
  )
  expect_identical(second_state$mutable_cache$value, "before")

  popped <- SpikeTrainPatternDetector:::stpd_ui_transaction_pop(rv)
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 0L)
  SpikeTrainPatternDetector:::stpd_ui_transaction_restore(rv, popped)
  expect_identical(rv$mutable_cache$value, "before")

  # Restoration decodes a fresh object: later changes to another decoded copy
  # of the popped payload are inert.
  popped_state <- SpikeTrainPatternDetector:::stpd_ui_transaction_snapshot_state(
    popped$snapshot
  )
  popped_state$mutable_cache$value <- "changed after restore"
  popped_state$datasets$dataset_a$trains$train_1$pattern_manual[] <- "other"
  expect_identical(rv$mutable_cache$value, "before")
  expect_identical(
    rv$datasets$dataset_a$trains$train_1$pattern_manual,
    c("", "burst", "")
  )
})

test_that("all required destructive action families restore complete derived state", {
  actions <- c(
    "clear_all_datasets", "remove_dataset", "clear_manual_labels",
    "clear_auto_labels", "clear_final_audit", "legacy_overwrite_manual"
  )

  for (action_id in actions) {
    rv <- ui_transaction_fixture()
    before <- ui_transaction_state_copy(rv)
    SpikeTrainPatternDetector:::stpd_ui_transaction_run(
      rv,
      action_id = action_id,
      dataset_ids = "dataset_a",
      train_ids = "train_1",
      mutate = function() {
        rv$datasets$dataset_a$trains$train_1$pattern_manual[] <- "changed"
        rv$datasets$dataset_a$trains$train_1$pattern_auto[] <- ""
        rv$datasets$dataset_a$results <- list()
        rv$manual_detector_eval <- list(stale = TRUE)
        rv$scientific_validation <- list(stale = TRUE)
        rv$parameter_sensitivity <- NULL
        rv$final_audit_last_summary <- NULL
        rv$possible_burst_promotion_preview <- NULL
      }
    )
    SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv)
    expect_identical(ui_transaction_state_copy(rv), before, info = action_id)
  }
})

test_that("transaction snapshots work with Shiny reactiveValues", {
  rv <- ui_transaction_fixture(use_reactive_values = TRUE)
  before <- ui_transaction_state_copy(rv)

  SpikeTrainPatternDetector:::stpd_ui_transaction_run(
    rv,
    action_id = "clear_all_datasets",
    mutate = function() {
      rv$datasets <- list()
      rv$current_id <- NULL
      rv$scientific_validation <- NULL
    }
  )
  expect_length(shiny::isolate(rv$datasets), 0L)
  SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv)
  expect_identical(ui_transaction_state_copy(rv), before)
})

test_that("reactiveValues LIFO undo treats absent and NULL optional state equally", {
  rv <- ui_transaction_fixture(use_reactive_values = TRUE)
  initial_datasets <- serialize(shiny::isolate(rv$datasets), NULL, version = 3L)

  SpikeTrainPatternDetector:::stpd_ui_transaction_run(
    rv,
    action_id = "manual_label_edit",
    dataset_ids = "dataset_a",
    mutate = function() {
      datasets <- shiny::isolate(rv$datasets)
      datasets$dataset_a$trains$train_1$pattern_manual[1] <- "burst"
      rv$datasets <- datasets
    }
  )
  after_first_datasets <- serialize(
    shiny::isolate(rv$datasets), NULL, version = 3L
  )
  after_first_guard <- SpikeTrainPatternDetector:::stpd_ui_transaction_guard_sha256(rv)

  # parameter_delta_preview was absent in the original reactiveValues.  Shiny
  # can only restore it as a NULL binding, which is scientifically equivalent.
  SpikeTrainPatternDetector:::stpd_ui_transaction_run(
    rv,
    action_id = "clear_auto_labels",
    dataset_ids = "dataset_a",
    mutate = function() rv$parameter_delta_preview <- list(delta = 1)
  )
  SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv)
  expect_identical(
    serialize(shiny::isolate(rv$datasets), NULL, version = 3L),
    after_first_datasets
  )
  expect_null(shiny::isolate(rv$parameter_delta_preview))
  expect_identical(
    SpikeTrainPatternDetector:::stpd_ui_transaction_guard_sha256(rv),
    after_first_guard
  )

  expect_no_error(SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv))
  expect_identical(
    serialize(shiny::isolate(rv$datasets), NULL, version = 3L),
    initial_datasets
  )
})

test_that("undo refuses to erase later untracked scientific results", {
  rv <- ui_transaction_fixture()
  SpikeTrainPatternDetector:::stpd_ui_transaction_push(
    rv,
    action_id = "manual_label_edit",
    dataset_ids = "dataset_a",
    train_ids = "train_1",
    target_tracks = "MANUAL"
  )
  rv$datasets$dataset_a$trains$train_1$pattern_manual[1] <- "burst"
  SpikeTrainPatternDetector:::stpd_ui_transaction_finalize(rv)

  # Simulate a subsequent detector run that is not part of this older manual
  # transaction.  Restoring the full pre-action snapshot would erase it.
  rv$datasets$dataset_a$trains$train_1$pattern_auto[1] <- "tonic"
  rv$datasets$dataset_a$results$run_metadata <- data.frame(
    run_id = "later_run", stringsAsFactors = FALSE
  )
  before <- serialize(rv$datasets, NULL, version = 3L)

  expect_error(
    SpikeTrainPatternDetector:::stpd_ui_transaction_undo(rv),
    "Undo is no longer safe"
  )
  expect_identical(serialize(rv$datasets, NULL, version = 3L), before)
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 0L)
})

test_that("tampered snapshots fail closed without consuming history", {
  rv <- ui_transaction_fixture()
  SpikeTrainPatternDetector:::stpd_ui_transaction_push(
    rv,
    action_id = "remove_dataset",
    dataset_ids = "dataset_a"
  )
  record <- SpikeTrainPatternDetector:::stpd_ui_transaction_peek(rv)
  record$snapshot$state_payload[1] <- as.raw(
    bitwXor(as.integer(record$snapshot$state_payload[1]), 1L)
  )

  expect_error(
    SpikeTrainPatternDetector:::stpd_ui_transaction_restore(rv, record),
    "integrity check failed"
  )
  expect_identical(rv$current_id, "dataset_a")
  expect_identical(SpikeTrainPatternDetector:::stpd_ui_transaction_depth(rv), 1L)
})
