orientation_integration_train <- function(auto, manual) {
  timestamp <- c(1, 1.1, 1.2, 1.4, 1.8, 2.2)
  data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, diff(timestamp)),
    ISI_pct = c(NA_real_, 20, 40, 60, 80, 100),
    pattern_manual = manual,
    pattern_manual_negative = rep("", length(timestamp)),
    pattern_auto = auto,
    auto_score = seq(0.1, 0.6, by = 0.1),
    stringsAsFactors = FALSE
  )
}

orientation_integration_dataset <- function(task_events = TRUE) {
  list(
    trains = list(
      train_a = orientation_integration_train(
        auto = c("", "possible_burst", "possible_burst", "", "", ""),
        manual = c("", "", "", "pause", "pause", "")
      ),
      # This train intentionally has MANUAL burst labels and no AUTO labels.
      # It makes an accidental MANUAL-first fallback observable.
      train_b = orientation_integration_train(
        auto = rep("", 6L),
        manual = rep("burst", 6L)
      )
    ),
    task_events = if (isTRUE(task_events)) {
      data.frame(
        event_name = "stimulus",
        event_time_sec = 1.5,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame()
    },
    meta = list(display_name = "Orientation integration fixture", unit_in = "s"),
    results = list(),
    train_settings = list(),
    params_last = default_params(),
    params_est = default_params()
  )
}

orientation_integration_html <- function(x) {
  paste(as.character(x), collapse = "")
}

test_that("Orientation state fixtures distinguish no-run, full, partial, and stale", {
  ds <- orientation_integration_dataset(task_events = FALSE)
  params <- default_params()
  dataset_identity <- stpd_ui_dataset_identity(ds, "orientation_fixture")

  no_run <- stpd_ui_run_identity(
    ds, params = params, dataset_id = "orientation_fixture",
    selected_trains = names(ds$trains), dataset_identity = dataset_identity
  )
  expect_identical(
    stpd_ui_run_state(
      ds, params, "orientation_fixture", no_run, dataset_identity
    )$code,
    "run_not_started"
  )

  full <- no_run
  full$has_run <- TRUE
  full$run_id <- "run_full_fixture"
  full$verifiable <- TRUE
  full_state <- stpd_ui_run_state(
    ds, params, "orientation_fixture", full, dataset_identity
  )
  expect_identical(full_state$code, "run_current")
  expect_identical(full_state$scope_label, "2/2")

  partial <- stpd_ui_run_identity(
    ds, params = params, dataset_id = "orientation_fixture",
    selected_trains = "train_a", dataset_identity = dataset_identity
  )
  partial$has_run <- TRUE
  partial$run_id <- "run_partial_fixture"
  partial$verifiable <- TRUE
  partial_state <- stpd_ui_run_state(
    ds, params, "orientation_fixture", partial, dataset_identity
  )
  expect_identical(partial_state$code, "run_partial_current")
  expect_identical(partial_state$scope_label, "1/2")

  stale <- full
  stale$data_sha256 <- paste(rep("0", 64L), collapse = "")
  stale_state <- stpd_ui_run_state(
    ds, params, "orientation_fixture", stale, dataset_identity
  )
  expect_identical(stale_state$code, "run_data_changed")
  expect_identical(stale_state$scope_label, "2/2")
  expect_true("data_changed" %in% stale_state$reason_codes)
})

test_that("Orientation live session links navigation, scope, events, and export status", {
  ds <- orientation_integration_dataset(task_events = TRUE)
  ui_html <- orientation_integration_html(ui)
  expect_match(
    ui_html,
    'name="events_view" value="auto" checked="checked"',
    fixed = TRUE
  )
  expect_false(grepl(
    'name="events_view" value="manual" checked="checked"',
    ui_html,
    fixed = TRUE
  ))

  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      rv$datasets <- list(orientation_fixture = ds)
      rv$current_id <- "orientation_fixture"
      session$setInputs(
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        train_display_mode = "selected_only",
        trains = "train_a",
        visible_trains_per_page = 10L,
        train_page = 1L,
        use_train_metadata_filter = FALSE,
        events_detail_mode = "compact",
        events_jump_target = "automatic"
      )
      session$flushReact()

      # Viewing scope and detector scope are separate, persistent concepts.
      context_html <- orientation_integration_html(
        output$orientation_context_bar
      )
      expect_match(context_html, 'role="status"', fixed = TRUE)
      expect_match(context_html, 'aria-live="polite"', fixed = TRUE)
      expect_match(
        context_html, 'data-run-code="run_not_started"', fixed = TRUE
      )
      expect_match(
        context_html, 'data-viewing-scope="1/2"', fixed = TRUE
      )
      expect_match(
        context_html, 'data-detected-scope="0/2"', fixed = TRUE
      )

      sent_inputs <- list()
      sent_custom <- list()
      session$sendInputMessage <- function(inputId, message) {
        sent_inputs[[length(sent_inputs) + 1L]] <<- list(
          inputId = inputId, message = message
        )
      }
      session$sendCustomMessage <- function(type, message) {
        sent_custom[[length(sent_custom) + 1L]] <<- list(
          type = type, message = message
        )
      }

      before_data <- serialize(rv$datasets, NULL, version = 3L)
      before_run_identity <- serialize(
        rv$ui_run_identity_by_dataset, NULL, version = 3L
      )
      before_summary <- rv$last_detector_summary

      # Fire all five navigation-only workflow actions in registration order.
      session$setInputs(
        orientation_step_data = 1L,
        orientation_step_params = 1L,
        orientation_step_run = 1L,
        orientation_step_validate = 1L,
        orientation_step_export = 1L
      )
      session$flushReact()

      tab_messages <- Filter(
        function(x) identical(x$inputId, "main_tabs"), sent_inputs
      )
      expect_identical(
        vapply(tab_messages, function(x) x$message$value, character(1)),
        c(
          "\u6570\u636E QC", "\u68C0\u6D4B\u5668 / \u53C2\u6570",
          "\u68C0\u6D4B\u5668 / \u53C2\u6570",
          "\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A",
          "\u4E8B\u4EF6 / \u8F93\u51FA"
        )
      )
      focus_messages <- Filter(
        function(x) identical(x$type, "stpd-orientation-focus"),
        sent_custom
      )
      expect_identical(
        vapply(focus_messages, function(x) x$message$anchor, character(1)),
        c(
          "stpd_data_controls", "stpd_detect_controls",
          "stpd_run_controls", "stpd_export_controls"
        )
      )
      expect_true(all(vapply(
        focus_messages,
        function(x) isTRUE(x$message$expandSidebar),
        logical(1)
      )))

      # In particular, step 3 only focuses the detector controls. It cannot
      # create detector output, a run identity, or any scientific mutation.
      expect_identical(
        serialize(rv$datasets, NULL, version = 3L), before_data
      )
      expect_identical(
        serialize(rv$ui_run_identity_by_dataset, NULL, version = 3L),
        before_run_identity
      )
      expect_identical(rv$last_detector_summary, before_summary)

      # The server default is AUTO, but without a trustworthy detector run it
      # must not expose imported or stale AUTO labels as current intervals.
      expect_null(input$events_view)
      no_run_current <- orientation_events_current()
      expect_identical(no_run_current$source, "auto")
      expect_identical(no_run_current$scope_status, "historical_unverified")
      expect_identical(nrow(no_run_current$events), 0L)

      # Add a session-only, trustworthy one-train run snapshot. This preserves
      # the scientific fixture while exercising current AUTO row navigation.
      dataset_identity <- stpd_ui_dataset_identity(
        current_dataset(), rv$current_id
      )
      partial_identity <- stpd_ui_run_identity(
        current_dataset(), params = current_ui_params(),
        dataset_id = rv$current_id, selected_trains = "train_a",
        dataset_identity = dataset_identity
      )
      partial_identity$has_run <- TRUE
      partial_identity$run_id <- "orientation_partial_fixture"
      partial_identity$verifiable <- TRUE
      rv$ui_run_identity_by_dataset[[rv$current_id]] <- partial_identity
      session$flushReact()

      current <- orientation_events_current()
      expect_identical(current$scope_status, "partial_current")
      expect_identical(current$source, "auto")
      expect_identical(as.character(current$events$pattern), "possible_burst")
      expect_identical(current$display$semantic_track, "review")
      expect_false(any(current$events$train == "train_b"))
      expect_false(any(current$events$pattern == "burst"))

      key <- current$display$event_row_key[[1L]]
      expect_true(nzchar(key))
      row <- current$events[1L, , drop = FALSE]
      plan <- stpd_orientation_event_jump_plan(
        row, current_dataset(), source = current$source,
        dataset_id = rv$current_id, display_unit = "s", padding_sec = 0.05
      )
      expect_true(plan$valid)
      expect_identical(plan$train, "train_a")
      expect_identical(plan$target_tab, "\u539F\u59CB\u65F6\u95F4\u6233\u56FE")
      expect_equal(plan$target_window, c(1, 1.25))

      sent_inputs <- list()
      sent_custom <- list()
      session$setInputs(events_table_row_key = list(key = key, nonce = 1))
      session$flushReact()
      session$flushReact()

      expect_identical(rv$event_table_focus$key, key)
      expect_identical(rv$event_table_focus$source, "auto")
      expect_identical(rv$event_table_focus$train, "train_a")
      expect_identical(
        rv$event_table_focus$target_tab, "\u539F\u59CB\u65F6\u95F4\u6233\u56FE"
      )
      jump_tabs <- Filter(
        function(x) identical(x$inputId, "main_tabs"), sent_inputs
      )
      expect_identical(
        tail(vapply(jump_tabs, function(x) x$message$value, character(1)), 1L),
        "\u539F\u59CB\u65F6\u95F4\u6233\u56FE"
      )
      raw_windows <- Filter(
        function(x) identical(x$inputId, "raw_xrange"), sent_inputs
      )
      expect_length(raw_windows, 1L)
      rendered_window <- as.numeric(raw_windows[[1L]]$message$value)
      expect_lte(rendered_window[[1L]], row$start_time_sec[[1L]])
      expect_gte(rendered_window[[2L]], row$end_time_sec[[1L]])

      # The export presenter reports server generation progress and scope; it
      # does not claim that the browser has already saved the file.
      rv$formal_export_progress <- list(
        dataset_id = "orientation_fixture", state = "preparing",
        progress = 0.65, detail = "Writing validation tables.", scope = "1/2"
      )
      session$flushReact()
      progress_html <- orientation_integration_html(
        output$formal_export_progress_status
      )
      expect_match(progress_html, "is-active", fixed = TRUE)
      expect_match(progress_html, "65%", fixed = TRUE)
      expect_match(progress_html, "\u5BFC\u51FA\u9636\u6BB5\uFF1Apreparing", fixed = TRUE)
      expect_match(progress_html, "\u8303\u56F4\uFF1A1/2", fixed = TRUE)
      expect_false(grepl("Writing validation tables.", progress_html, fixed = TRUE))

      # The same session-only progress record is rendered in English after a
      # language switch; no export is rerun and no scientific state changes.
      session$setInputs(ui_language = "en")
      session$flushReact()
      progress_html <- orientation_integration_html(
        output$formal_export_progress_status
      )
      expect_match(progress_html, "Export phase: preparing", fixed = TRUE)
      expect_false(grepl("Writing validation tables.", progress_html, fixed = TRUE))
      expect_match(progress_html, "Scope: 1/2", fixed = TRUE)
      expect_match(progress_html, 'role="status"', fixed = TRUE)
      expect_match(progress_html, 'aria-live="polite"', fixed = TRUE)

      rv$formal_export_progress <- list(
        dataset_id = "orientation_fixture", state = "generated",
        progress = 1, detail = "Generated on the server.", scope = "2/2"
      )
      session$flushReact()
      generated_html <- orientation_integration_html(
        output$formal_export_progress_status
      )
      expect_match(generated_html, "is-success", fixed = TRUE)
      expect_match(generated_html, "100%", fixed = TRUE)
      expect_match(generated_html, "The server-side file was generated.", fixed = TRUE)
      expect_false(grepl("Generated on the server.", generated_html, fixed = TRUE))
      expect_false(grepl("saved", generated_html, ignore.case = TRUE))
    })
  ))
})
