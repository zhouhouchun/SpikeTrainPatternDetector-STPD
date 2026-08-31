selected_scope_fixture <- function(n_trains = 12L) {
  timestamps <- c(0, 0.01, 0.025, 0.05, 0.09)
  one_train <- data.frame(
    idx = seq_along(timestamps),
    timestamp_sec = timestamps,
    ISI_sec = c(NA_real_, diff(timestamps)),
    pattern_manual = rep("", length(timestamps)),
    pattern_manual_negative = rep("", length(timestamps)),
    pattern_auto = rep("", length(timestamps)),
    stringsAsFactors = FALSE
  )
  trains <- stats::setNames(
    replicate(n_trains, one_train, simplify = FALSE),
    paste0("train_", seq_len(n_trains))
  )
  SpikeTrainPatternDetector:::make_dataset(
    "selected-scope-fixture", "synthetic", trains, unit_in = "s"
  )
}

test_that("selected-only treats NULL and explicit empty selection as no scope", {
  ds <- selected_scope_fixture()
  train_names <- names(ds$trains)

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- list(selected_scope_fixture = ds)
      rv$current_id <- "selected_scope_fixture"
      session$setInputs(
        train_display_mode = "selected_only",
        use_train_metadata_filter = FALSE,
        detector_selected_only = TRUE,
        visible_trains_per_page = 4L,
        train_page = 2L,
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9
      )
      session$flushReact()

      # Shiny reports an empty multiple select as NULL. The first-ten default
      # is supplied by selectize after initialization; before that, and after
      # a user clears it, the server must fail closed.
      expect_null(input$trains)
      expect_identical(displayed_train_names(), character(0))

      # The browser's cleared-multiple-select shape is NULL. The selected-only
      # detector must stop at its empty-target guard in that exact state.
      assign(
        "stpd_product_pre_detection_qc",
        function(...) stop("detector pipeline reached with an empty selection"),
        envir = environment(run_detector_from_ui)
      )
      before <- serialize(rv$datasets, NULL, version = 3L)
      expect_no_error(result <- run_detector_from_ui(
        params_override = default_params(),
        switch_to_plot = FALSE,
        notify = FALSE
      ))
      expect_null(result)
      expect_s3_class(rv$last_detector_summary, "stpd_ui_bilingual")
      expect_identical(
        detector_summary_current(rv$last_detector_summary, lang = "zh"),
        stpd_ui_copy("no_selected_trains", lang = "zh")
      )
      expect_identical(
        detector_summary_current(rv$last_detector_summary, lang = "en"),
        stpd_ui_copy("no_selected_trains", lang = "en")
      )
      expect_identical(serialize(rv$datasets, NULL, version = 3L), before)

      # TestServer can also represent an explicit zero-length vector; it has
      # the same no-scope semantics and never becomes the first train.
      session$setInputs(trains = character(0))
      session$flushReact()
      expect_identical(input$trains, character(0))
      expect_identical(displayed_train_names(), character(0))

      # Page mode continues to select exactly the visible page and does not
      # depend on the explicitly empty selectize input.
      session$setInputs(train_display_mode = "paged_all")
      session$flushReact()
      expect_identical(displayed_train_names(), train_names[5:8])
    })
  ))
})

test_that("empty legacy mutation selectors never inherit the Raster scope", {
  ds <- selected_scope_fixture(3L)

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- list(selected_scope_fixture = ds)
      rv$current_id <- "selected_scope_fixture"
      session$setInputs(
        train_display_mode = "selected_only",
        trains = "train_1",
        use_train_metadata_filter = FALSE,
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9
      )
      session$flushReact()

      expect_identical(displayed_train_names(), "train_1")
      expect_null(input$possible_burst_promote_trains)
      expect_null(input$final_audit_trains)
      expect_identical(possible_burst_promote_selected_trains(), character())
      expect_identical(final_audit_selected_trains(), character())

      session$setInputs(
        possible_burst_promote_trains = "train_2",
        final_audit_trains = "train_3"
      )
      session$flushReact()
      expect_identical(possible_burst_promote_selected_trains(), "train_2")
      expect_identical(final_audit_selected_trains(), "train_3")
    })
  ))
})
