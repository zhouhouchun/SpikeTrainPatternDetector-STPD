stpd_phase0_installed_server_source <- function() {
  namespace <- asNamespace("SpikeTrainPatternDetector")
  function_names <- c(
    "server",
    grep(
      "^stpd_server_install_",
      ls(namespace, all.names = TRUE),
      value = TRUE
    )
  )
  function_names <- unique(function_names[
    vapply(function_names, exists, logical(1), envir = namespace, inherits = FALSE)
  ])
  paste(
    unlist(lapply(function_names, function(name) {
      deparse(get(name, envir = namespace, inherits = FALSE), width.cutoff = 500L)
    })),
    collapse = "\n"
  )
}

test_that("Phase-0 UI exposes persistent run and validation status surfaces", {
  ui_html <- paste(as.character(SpikeTrainPatternDetector:::ui), collapse = "\n")

  expect_match(ui_html, "ui_run_state_status", fixed = TRUE)
  expect_match(ui_html, "manual_validation_state_status", fixed = TRUE)
  expect_match(ui_html, "scientific_validation_state_status", fixed = TRUE)
  expect_match(ui_html, "撤销上一次变更", fixed = TRUE)
  expect_match(ui_html, "快速预览：仅检测当前可见 trains", fixed = TRUE)
  expect_false(grepl("possible_burst_promote_overwrite", ui_html, fixed = TRUE))
})

test_that("destructive server actions are confirmation-gated and undoable", {
  server_source <- stpd_phase0_installed_server_source()

  for (id in c(
    "confirm_clear_all_datasets", "confirm_remove_dataset",
    "confirm_collapse_duplicate_spikes_current",
    "confirm_collapse_duplicate_spikes_all",
    "confirm_clear_selected_manual", "confirm_clear_selected_auto",
    "confirm_clear_all_manual", "confirm_clear_all_auto",
    "confirm_rebuild_final_audit", "confirm_clear_final_audit",
    "confirm_promote_possible_to_final_audit",
    "confirm_apply_possible_burst_promotion",
    "confirm_revert_possible_burst_promotion",
    "confirm_replace_workspace"
  )) {
    expect_match(server_source, id, fixed = TRUE)
  }
  expect_match(server_source, "stpd_ui_transaction_undo(rv)", fixed = TRUE)
  expect_match(server_source, "stpd_ui_transaction_push(", fixed = TRUE)
  expect_match(server_source, "confirm_clear_all_datasets_token", fixed = TRUE)
  expect_match(server_source, "consume_ui_confirmation(", fixed = TRUE)
  expect_match(server_source, "stpd_ui_transaction_finalize(rv", fixed = TRUE)
  expect_match(server_source, "stpd_ui_transaction_abort_new(rv", fixed = TRUE)
  expect_match(server_source, "max_depth = 5L", fixed = TRUE)
})

test_that("formal exports are result-state gated and ignore display-only others", {
  source <- stpd_phase0_installed_server_source()
  ui_source <- paste(as.character(SpikeTrainPatternDetector:::ui), collapse = "\n")

  expect_match(source, "formal_results_export_context", fixed = TRUE)
  expect_match(source, "stpd_ui_formal_export_state(", fixed = TRUE)
  expect_match(source, "isTRUE(state$eligible)", fixed = TRUE)
  expect_gte(length(regmatches(
    source, gregexpr("auto_others = FALSE", source, fixed = TRUE)
  )[[1]]), 3L)
  expect_match(ui_source, "formal_results_export_control", fixed = TRUE)
  expect_false(grepl('id="download_results_zip"', ui_source, fixed = TRUE))
  expect_false(grepl('id="download_labeled_csv"', ui_source, fixed = TRUE))
})

test_that("workspace import is prepared locally before an atomic confirmed swap", {
  source <- stpd_phase0_installed_server_source()
  prepare_pos <- regexpr("candidate_datasets <- obj$datasets", source, fixed = TRUE)
  commit_pos <- regexpr("rv$datasets <- candidate$datasets", source, fixed = TRUE)
  expect_gt(prepare_pos, 0L)
  expect_gt(commit_pos, 0L)
  expect_false(grepl("rv$datasets <- obj$datasets", source, fixed = TRUE))
  expect_match(source, "stpd_ui_transaction_run(", fixed = TRUE)
  expect_match(source, 'action_id = "replace_workspace"', fixed = TRUE)
  expect_match(source, "confirm_replace_workspace", fixed = TRUE)

  bad_train <- data.frame(
    idx = 1:2, timestamp_sec = c(0, 0.01),
    ISI_sec = I(list(NA_real_, 0.01)),
    stringsAsFactors = FALSE
  )
  expect_false(stpd_workspace_rds_is_valid(list(
    datasets = list(bad = list(trains = list(train_1 = bad_train)))
  )))
})

test_that("detector scope and stale rows are explicit in the Raster contract", {
  source <- stpd_phase0_installed_server_source()

  expect_match(source, "ui_run_identity_by_dataset", fixed = TRUE)
  expect_match(source, "stpd_ui_run_identity(", fixed = TRUE)
  expect_match(source, "run_partial_current", fixed = TRUE)
  expect_match(source, "run_identity_unverifiable", fixed = TRUE)
  expect_match(source, "train_status", fixed = TRUE)
  expect_match(source, "analysis_status", fixed = TRUE)

  # Detection must no longer delete duplicate spikes as an implicit side
  # effect.  The run is blocked and points to the separately confirmed QC
  # action instead.
  expect_match(source, "stpd_qc_errors_are_exact_duplicate_only", fixed = TRUE)
  expect_false(grepl("auto_collapsed_duplicates", source, fixed = TRUE))
})

test_that("Phase 2B disables legacy promotion in the visible workflow", {
  source <- stpd_phase0_installed_server_source()
  expect_match(source, "phase2b_state_present", fixed = TRUE)
  expect_match(source, "legacy_promotion_controls", fixed = TRUE)
  expect_match(source, "stpd-state-error", fixed = TRUE)
  expect_match(source, "overwrite_manual = FALSE", fixed = TRUE)
  expect_false(grepl("input$possible_burst_promote_overwrite", source, fixed = TRUE))
})

test_that("customdata box selection writes MANUAL label and creates one undo step", {
  timestamp <- c(0, 0.01, 0.02, 0.04)
  dat <- data.frame(
    idx = 1:4, timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, diff(timestamp)),
    ISI_pct = c(NA_real_, 33, 66, 100),
    pattern_manual = rep("", 4),
    pattern_manual_negative = rep("", 4),
    pattern_auto = rep("", 4),
    auto_score = rep(NA_real_, 4),
    stringsAsFactors = FALSE
  )
  ds <- list(
    trains = list(train_1 = dat), task_events = data.frame(),
    meta = list(display_name = "manual_selection_fixture", unit_in = "s"),
    results = list(), train_settings = list(),
    params_last = default_params(), params_est = default_params()
  )

  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      rv$datasets <- list(dataset_1 = ds)
      rv$current_id <- "dataset_1"
      session$setInputs(
        main_tabs = "对齐时间戳图", pattern = "burst",
        time_unit = "s", qc_isi_unit = "ms",
        artifact_isi_ms = 0.9, auto_label_selection = FALSE
      )
      plot_input <- list()
      plot_input[["plotly_selected-raster"]] <- paste0(
        '[{"x":0.01,"y":1,"customdata":"train_1__2"},',
        '{"x":0.02,"y":1,"customdata":"train_1__3"}]'
      )
      do.call(session$setInputs, plot_input)
      session$flushReact()
      session$setInputs(add_annot = 1)
      session$flushReact()

      expect_identical(
        rv$datasets$dataset_1$trains$train_1$pattern_manual,
        c("", "", "burst", "")
      )
      expect_identical(stpd_ui_transaction_depth(rv), 1L)
      session$setInputs(undo_last_manual_action = 1)
      session$flushReact()
      expect_identical(
        rv$datasets$dataset_1$trains$train_1$pattern_manual,
        rep("", 4)
      )

      dataset_2 <- ds
      dataset_2$meta$display_name <- "second_dataset"
      rv$datasets <- list(dataset_1 = ds, dataset_2 = dataset_2)
      rv$current_id <- "dataset_1"
      session$setInputs(remove_dataset = 1)
      session$flushReact()
      expect_identical(
        rv$ui_pending_confirmation$context$dataset_id, "dataset_1"
      )

      # Changing the current selector after opening the modal must not retarget
      # the confirmed action.  Replaying the same confirmation must be inert.
      rv$current_id <- "dataset_2"
      session$setInputs(confirm_remove_dataset = 1)
      session$flushReact()
      expect_identical(names(rv$datasets), "dataset_2")
      expect_identical(stpd_ui_transaction_depth(rv), 1L)
      session$setInputs(confirm_remove_dataset = 2)
      session$flushReact()
      expect_identical(names(rv$datasets), "dataset_2")
      expect_identical(stpd_ui_transaction_depth(rv), 1L)

      # A production-style push followed by a later write failure must restore
      # all partial mutations and leave the earlier finalized undo intact.
      before_failed_action <- serialize(rv$datasets, NULL, version = 3L)
      depth_before_failed_action <- stpd_ui_transaction_depth(rv)
      expect_false(run_manual_ui_action({
        push_ui_undo(
          "manual_label_edit", "injected failing manual action",
          dataset_ids = rv$current_id
        )
        rv$datasets <- list()
        rv$scientific_validation <- list(partial = TRUE)
        stop("injected failure after push")
      }, prefix = "injected failure"))
      session$flushReact()
      expect_identical(
        serialize(rv$datasets, NULL, version = 3L),
        before_failed_action
      )
      expect_identical(
        stpd_ui_transaction_depth(rv), depth_before_failed_action
      )
    })
  ))
})
