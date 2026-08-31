test_that("orientation navigation assigns every agreed tab exactly once", {
  groups <- stpd_ui_orientation_groups()
  tabs <- unname(unlist(groups, use.names = FALSE))

  expect_identical(
    names(groups),
    c(
      "\u6570\u636E", "\u68C0\u6D4B", "\u6D4F\u89C8", "\u590D\u6838",
      "\u9A8C\u8BC1", "\u5BFC\u51FA", "\u5206\u6790", "\u4E13\u5BB6"
    )
  )
  expect_length(tabs, 21L)
  expect_identical(anyDuplicated(tabs), 0L)
  expect_identical(
    stpd_ui_orientation_tab_group(c(
      "\u6570\u636E QC", "\u539F\u59CB\u65F6\u95F4\u6233\u56FE",
      "\u4E8B\u4EF6 / \u8F93\u51FA", "not-a-tab"
    )),
    c("\u6570\u636E", "\u6D4F\u89C8", "\u5BFC\u51FA", NA_character_)
  )
})

test_that("orientation workflow keeps stable targets and control anchors", {
  workflow <- stpd_ui_orientation_workflow()
  expect_identical(
    workflow$step_id,
    c(
      "orientation_step_data", "orientation_step_params",
      "orientation_step_run", "orientation_step_validate",
      "orientation_step_export"
    )
  )
  expect_identical(
    workflow$target_tab,
    c(
      "\u6570\u636E QC", "\u68C0\u6D4B\u5668 / \u53C2\u6570",
      "\u68C0\u6D4B\u5668 / \u53C2\u6570",
      "\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A",
      "\u4E8B\u4EF6 / \u8F93\u51FA"
    )
  )
  expect_identical(
    workflow$control_anchor,
    c(
      "stpd_data_controls", "stpd_detect_controls", "stpd_run_controls",
      NA_character_, "stpd_export_controls"
    )
  )
  expect_identical(
    stpd_ui_orientation_tab_step(c(
      "\u6570\u636E QC", "\u79CD\u5B50 / \u6865\u63A5\u8BCA\u65AD",
      "\u72B6\u6001\u8F68\u8FF9", "\u79D1\u5B66\u9A8C\u8BC1", "\u6279\u5904\u7406 / API"
    )),
    workflow$step_id
  )
})

test_that("legacy labels map to non-mutually-exclusive semantic tracks", {
  labels <- c(
    "possible_burst", "burst", "long_burst", "pause", "tonic", "HFT",
    "HFS", "high-frequency-tonic", "high frequency spiking", "others"
  )
  expect_identical(
    stpd_orientation_semantic_track(labels),
    c(
      "review", "event", "event", "gap", "state", "state", "state",
      "state", "state", "diagnostic"
    )
  )
})

orientation_event_fixture <- function() {
  data.frame(
    event_id = c(1L, 1L),
    dataset = c("dataset-A", "dataset-A"),
    train = c("train-1", "train-2"),
    pattern = c("possible_burst", "burst"),
    start_time_sec = c(12, 12),
    end_time_sec = c(14, 14),
    duration_sec = c(2, 2),
    n_spikes = c(4L, 5L),
    n_isi = c(3L, 4L),
    label_source = c("auto", "auto"),
    auto_score = c(0.4, 0.9),
    stringsAsFactors = FALSE
  )
}

test_that("event row identity is train-qualified, source-qualified, and unit-stable", {
  events <- orientation_event_fixture()
  keys_a <- stpd_orientation_event_row_key(events, source = "audit_final")
  keys_b <- stpd_orientation_event_row_key(events, source = "manual")

  expect_false(anyNA(keys_a))
  expect_identical(anyDuplicated(keys_a), 0L)
  expect_false(any(keys_a == keys_b))

  compact_ms <- stpd_orientation_event_display_model(
    events, source = "audit_final", time_unit = "ms", mode = "compact"
  )
  compact_s <- stpd_orientation_event_display_model(
    events, source = "audit_final", time_unit = "s", mode = "compact"
  )
  expect_identical(compact_ms$event_row_key, compact_s$event_row_key)
  expect_equal(compact_ms$start_time, compact_s$start_time * 1000)
  expect_identical(compact_ms$semantic_track, c("review", "event"))
  expect_identical(events$start_time_sec, c(12, 12))
})

test_that("every canonical identity field contributes to the event row key", {
  base <- orientation_event_fixture()[1, , drop = FALSE]
  base_key <- stpd_orientation_event_row_key(base, source = "audit_final")
  variants <- list(
    transform(base, dataset = "dataset-B"),
    transform(base, train = "train-other"),
    transform(base, pattern = "burst"),
    transform(base, start_time_sec = 12.1),
    transform(base, end_time_sec = 14.1),
    transform(base, event_id = 2L)
  )
  variant_keys <- vapply(
    variants,
    function(row) stpd_orientation_event_row_key(row, source = "audit_final"),
    character(1)
  )
  source_key <- stpd_orientation_event_row_key(base, source = "manual")

  expect_false(any(c(variant_keys, source_key) == base_key))
  expect_identical(anyDuplicated(c(base_key, variant_keys, source_key)), 0L)
})

test_that("full event display keeps audit fields while compact mode stays focused", {
  events <- orientation_event_fixture()
  events$MM <- c(1.2, 1.4)
  events$user_override_reason <- c("", "confirmed")
  events$pre_ISI_sec <- c(0.2, 0.3)

  compact <- stpd_orientation_event_display_model(
    events, source = "audit_final", mode = "compact"
  )
  full <- stpd_orientation_event_display_model(
    events, source = "audit_final", mode = "full"
  )
  expect_false("MM" %in% names(compact))
  expect_true(all(c("MM", "user_override_reason", "pre_ISI") %in% names(full)))
  expect_equal(full$pre_ISI, c(200, 300))
  expect_identical(attr(full, "time_unit"), "ms")
})

test_that("event jump plan returns clamped raw and aligned windows", {
  event <- orientation_event_fixture()[1, , drop = FALSE]
  event$train <- "train-1"
  ds <- list(
    trains = list(train.1 = data.frame(timestamp_sec = seq(10, 20, by = 1))),
    task_events = data.frame()
  )
  names(ds$trains) <- "train-1"

  plan <- stpd_orientation_event_jump_plan(
    event, ds, source = "audit_final", display_unit = "s", padding_sec = 1
  )
  expect_true(plan$valid)
  expect_identical(plan$reason, "ready")
  expect_equal(plan$raw_window_sec, c(11, 15))
  expect_equal(plan$aligned_window_sec, c(1, 5))
  expect_equal(plan$target_window, c(1, 5))
  expect_identical(plan$target_tab, "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")

  edge <- event
  edge$start_time_sec <- 10.1
  edge$end_time_sec <- 19.9
  clamped <- stpd_orientation_event_jump_plan(
    edge, ds, source = "audit_final", display_unit = "ms", padding_sec = 1
  )
  expect_true(clamped$valid)
  expect_equal(clamped$raw_window_sec, c(10, 20))
  expect_equal(clamped$aligned_window_sec, c(0, 10))
  expect_equal(clamped$target_window, c(0, 10000))
})

test_that("task-event datasets choose raw windows without changing event geometry", {
  event <- orientation_event_fixture()[1, , drop = FALSE]
  event$train <- "train-1"
  ds <- list(
    trains = list(`train-1` = data.frame(timestamp_sec = seq(10, 20, by = 1))),
    task_events = data.frame(
      event_name = "task", event_time_sec = 12.5, stringsAsFactors = FALSE
    )
  )
  plan <- stpd_orientation_event_jump_plan(
    event, ds, source = "audit_final", display_unit = "s", padding_sec = 1
  )
  expect_true(plan$valid)
  expect_identical(plan$target_tab, "\u539F\u59CB\u65F6\u95F4\u6233\u56FE")
  expect_equal(plan$target_window, c(11, 15))
  expect_equal(plan$aligned_window_sec, c(1, 5))
})

test_that("invalid event rows and train geometry fail closed", {
  event <- orientation_event_fixture()[1, , drop = FALSE]
  ds <- list(
    trains = list(`train-1` = data.frame(timestamp_sec = seq(10, 20, by = 1))),
    task_events = data.frame()
  )

  missing_train <- event
  missing_train$train <- "unknown"
  expect_identical(
    stpd_orientation_event_jump_plan(missing_train, ds, source = "audit_final")$reason,
    "train_not_found"
  )

  backwards <- event
  backwards$train <- "train-1"
  backwards$end_time_sec <- 11
  expect_identical(
    stpd_orientation_event_jump_plan(backwards, ds, source = "audit_final")$reason,
    "event_geometry_invalid"
  )

  outside <- event
  outside$train <- "train-1"
  outside$start_time_sec <- 9
  expect_identical(
    stpd_orientation_event_jump_plan(outside, ds, source = "audit_final")$reason,
    "event_outside_train"
  )

  missing_identity <- event
  missing_identity$train <- "train-1"
  missing_identity$event_id <- ""
  invalid_identity <- stpd_orientation_event_jump_plan(
    missing_identity, ds, source = "audit_final", padding_sec = 1
  )
  expect_false(invalid_identity$valid)
  expect_identical(invalid_identity$reason, "event_identity_invalid")
  expect_true(all(is.na(invalid_identity$target_window)))
})

test_that("event display model derives missing durations row by row", {
  events <- data.frame(
    dataset = c("d", "d"),
    train = c("a", "a"),
    pattern = c("burst", "pause"),
    event_id = c(1L, 2L),
    start_time_sec = c(1, 5),
    end_time_sec = c(2.5, 8),
    stringsAsFactors = FALSE
  )
  model <- stpd_orientation_event_display_model(
    events, source = "auto", time_unit = "s"
  )
  expect_equal(model$duration, c(1.5, 3))
})

test_that("AUTO orientation rows respect the latest partial-run scope", {
  events <- data.frame(
    train = c("train_current", "train_old"),
    pattern = c("burst", "burst"),
    stringsAsFactors = FALSE
  )
  partial <- stpd_ui_status_record(
    "run_partial_current", "info", "partial", "partial",
    1L, 2L, selected_trains = "train_current"
  )
  scoped <- stpd_orientation_event_scope(
    events, "auto", partial,
    list(
      has_run = TRUE, selected_trains = "train_current",
      selected_train_n = 1L, total_train_n = 2L
    )
  )
  expect_identical(scoped$status, "partial_current")
  expect_identical(scoped$events$train, "train_current")
  expect_true(scoped$current)

  stale <- partial
  stale$code <- "run_params_changed"
  stale_scoped <- stpd_orientation_event_scope(
    events, "auto", stale,
    list(
      has_run = TRUE, selected_trains = "train_current",
      selected_train_n = 1L, total_train_n = 2L
    )
  )
  expect_identical(stale_scoped$status, "historical_scoped")
  expect_identical(stale_scoped$events$train, "train_current")
  expect_false(stale_scoped$current)

  legacy <- stpd_orientation_event_scope(
    events, "manual", partial,
    list(
      has_run = TRUE, selected_trains = "train_current",
      selected_train_n = 1L, total_train_n = 2L
    )
  )
  expect_identical(legacy$events$train, events$train)
})

test_that("AUTO orientation fails closed for an empty or unverifiable run scope", {
  events <- data.frame(
    train = c("train_a", "train_b"), pattern = c("burst", "pause"),
    stringsAsFactors = FALSE
  )
  misleading_state <- stpd_ui_status_record(
    "run_partial_current", "info", "partial", "partial",
    0L, 2L, selected_trains = character()
  )
  scoped <- stpd_orientation_event_scope(
    events, "auto", misleading_state,
    list(
      has_run = TRUE, selected_trains = character(),
      selected_train_n = 0L, total_train_n = 2L
    )
  )
  expect_identical(scoped$status, "historical_unverified")
  expect_false(scoped$current)
  expect_identical(nrow(scoped$events), 0L)
})
