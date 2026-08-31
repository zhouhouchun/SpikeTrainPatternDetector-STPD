ui_dynamic_status_fixture <- function() {
  timestamps <- c(0, 0.01, 0.025, 0.05)
  train <- data.frame(
    idx = seq_along(timestamps),
    timestamp_sec = timestamps,
    ISI_sec = c(NA_real_, diff(timestamps)),
    pattern_manual = rep("", length(timestamps)),
    pattern_manual_negative = rep("", length(timestamps)),
    pattern_auto = rep("", length(timestamps)),
    stringsAsFactors = FALSE
  )
  make_dataset(
    name = "ui-dynamic-status-fixture",
    source = "synthetic",
    trains = list(train_1 = train),
    unit_in = "s"
  )
}

test_that("dynamic status outputs select only the current language", {
  skip_if_not_installed("shiny")

  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      session$setInputs(ui_language = "zh")
      detector_notify_error(
        simpleError("SYNTHETIC_PARAMETER_DETAIL"),
        prefix = "\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u5931\u8D25",
        status = "parameter_sensitivity"
      )
      session$flushReact()

      zh_parameter <- output$parameter_sensitivity_status
      expect_match(zh_parameter, "\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u5931\u8D25", fixed = TRUE)
      expect_match(zh_parameter, "\u6280\u672F\u8BE6\u60C5\uFF1ASYNTHETIC_PARAMETER_DETAIL", fixed = TRUE)
      expect_false(grepl("Parameter-sensitivity scan failed", zh_parameter, fixed = TRUE))
      status_bytes <- serialize(rv$parameter_sensitivity_status, NULL, version = 3L)

      session$setInputs(ui_language = "en")
      session$flushReact()
      en_parameter <- output$parameter_sensitivity_status
      expect_match(en_parameter, "Parameter-sensitivity scan failed", fixed = TRUE)
      expect_match(en_parameter, "Technical details: SYNTHETIC_PARAMETER_DETAIL", fixed = TRUE)
      expect_false(grepl("[\u3400-\u9FFF]", en_parameter, perl = TRUE))
      expect_identical(
        serialize(rv$parameter_sensitivity_status, NULL, version = 3L),
        status_bytes
      )

      rv$final_audit_status <- ui_bilingual_value(
        "\u6700\u7EC8\u5BA1\u8BA1\u72B6\u6001 3",
        "Final-audit status 3"
      )
      rv$parameter_delta_preview_status <- ui_bilingual_value(
        "\u5DEE\u5F02\u9884\u89C8\u72B6\u6001 4",
        "Difference-preview status 4"
      )
      rv$possible_burst_promotion_status <- ui_bilingual_value(
        "possible_burst \u6279\u91CF\u5347\u7EA7\u72B6\u6001 5",
        "possible_burst bulk-promotion status 5"
      )
      session$flushReact()
      expect_identical(output$final_audit_status, "Final-audit status 3")
      expect_identical(output$parameter_delta_preview_status, "Difference-preview status 4")
      expect_identical(output$possible_burst_promotion_status, "possible_burst bulk-promotion status 5")

      bilingual_bytes <- serialize(
        list(
          rv$final_audit_status,
          rv$parameter_delta_preview_status,
          rv$possible_burst_promotion_status
        ),
        NULL,
        version = 3L
      )
      session$setInputs(ui_language = "zh")
      session$flushReact()
      expect_identical(output$final_audit_status, "\u6700\u7EC8\u5BA1\u8BA1\u72B6\u6001 3")
      expect_identical(output$parameter_delta_preview_status, "\u5DEE\u5F02\u9884\u89C8\u72B6\u6001 4")
      expect_identical(output$possible_burst_promotion_status, "possible_burst \u6279\u91CF\u5347\u7EA7\u72B6\u6001 5")
      expect_identical(
        serialize(
          list(
            rv$final_audit_status,
            rv$parameter_delta_preview_status,
            rv$possible_burst_promotion_status
          ),
          NULL,
          version = 3L
        ),
        bilingual_bytes
      )
    })
  ))
})

test_that("unknown manual and rollback failures label raw condition details", {
  skip_if_not_installed("shiny")

  captured <- new.env(parent = emptyenv())
  captured$messages <- character()
  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      action_env <- environment(run_manual_ui_action)
      assign(
        "showNotification",
        function(ui, ...) {
          captured$messages <- c(
            captured$messages,
            paste(as.character(ui), collapse = "")
          )
          invisible(NULL)
        },
        envir = action_env
      )

      session$setInputs(ui_language = "zh")
      expect_false(run_manual_ui_action(
        stop("SYNTHETIC_PRIMARY_DETAIL"),
        prefix = "\u624B\u52A8\u64CD\u4F5C\u672A\u5B8C\u6210"
      ))
      expect_match(tail(captured$messages, 1L), "\u6280\u672F\u8BE6\u60C5\uFF1ASYNTHETIC_PRIMARY_DETAIL", fixed = TRUE)

      session$setInputs(ui_language = "en")
      expect_false(run_manual_ui_action(
        stop("SYNTHETIC_PRIMARY_DETAIL"),
        prefix = "\u624B\u52A8\u64CD\u4F5C\u672A\u5B8C\u6210"
      ))
      en_primary <- tail(captured$messages, 1L)
      expect_match(en_primary, "Manual operation did not complete", fixed = TRUE)
      expect_match(en_primary, "Technical details: SYNTHETIC_PRIMARY_DETAIL", fixed = TRUE)
      expect_false(grepl("[\u3400-\u9FFF]", en_primary, perl = TRUE))

      assign(
        "stpd_ui_transaction_abort_new",
        function(...) stop("SYNTHETIC_ROLLBACK_DETAIL"),
        envir = action_env
      )
      expect_false(run_manual_ui_action(
        stop("SYNTHETIC_PRIMARY_DETAIL"),
        prefix = "\u624B\u52A8\u64CD\u4F5C\u672A\u5B8C\u6210"
      ))
      en_rollback <- tail(captured$messages, 1L)
      expect_match(en_rollback, "Technical details: SYNTHETIC_PRIMARY_DETAIL", fixed = TRUE)
      expect_match(en_rollback, "Automatic rollback failed", fixed = TRUE)
      expect_match(en_rollback, "Technical details: SYNTHETIC_ROLLBACK_DETAIL", fixed = TRUE)
      expect_false(grepl("[\u3400-\u9FFF]", en_rollback, perl = TRUE))
    })
  ))
})

test_that("Legacy rebuild and revert confirmations are fully localized", {
  skip_if_not_installed("shiny")

  ds <- ui_dynamic_status_fixture()
  captured <- new.env(parent = emptyenv())
  captured$modals <- character()
  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      modal_env <- environment(show_ui_confirmation)
      assign(
        "showModal",
        function(ui, ...) {
          captured$modals <- c(
            captured$modals,
            paste(as.character(ui), collapse = "")
          )
          invisible(NULL)
        },
        envir = modal_env
      )

      rv$datasets <- list(ui_dynamic_status_fixture = ds)
      rv$current_id <- "ui_dynamic_status_fixture"
      session$setInputs(
        ui_language = "en",
        final_audit_trains = "train_1",
        possible_burst_promote_trains = "train_1"
      )
      session$flushReact()

      session$setInputs(rebuild_final_audit = 1L)
      session$flushReact()
      session$setInputs(revert_possible_burst_promotion = 1L)
      session$flushReact()

      expect_length(captured$modals, 2L)
      expect_match(
        captured$modals[[1L]],
        "Confirm rebuilding the Legacy audit layer",
        fixed = TRUE
      )
      expect_match(
        captured$modals[[2L]],
        "Confirm reverting the Legacy possible_burst promotion",
        fixed = TRUE
      )
      expect_true(all(grepl("Confirm action", captured$modals, fixed = TRUE)))
      expect_true(all(grepl("Cancel", captured$modals, fixed = TRUE)))
      expect_false(any(grepl("[\u3400-\u9FFF]", captured$modals, perl = TRUE)))

      science_before <- serialize(rv$datasets, NULL, version = 3L)
      expect_true(is.list(rv$ui_pending_confirmation))
      session$setInputs(ui_language = "zh")
      session$flushReact()
      expect_null(rv$ui_pending_confirmation)
      expect_identical(serialize(rv$datasets, NULL, version = 3L), science_before)
    })
  ))
})

test_that("Legacy controls and selectors follow same-session language changes", {
  skip_if_not_installed("shiny")

  ds <- ui_dynamic_status_fixture()
  ds$trains$train_2 <- ds$trains$train_1
  ds$trains$train_3 <- ds$trains$train_1
  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      rv$datasets <- list(ui_dynamic_status_fixture = ds)
      rv$current_id <- "ui_dynamic_status_fixture"
      dataset_bytes <- serialize(rv$datasets, NULL, version = 3L)

      collect_copy <- function() {
        paste(
          as.character(output$final_audit_promote_control),
          as.character(output$legacy_promotion_controls),
          as.character(output$possible_burst_promote_train_selector),
          as.character(output$final_audit_train_selector),
          collapse = "\n"
        )
      }

      session$setInputs(
        ui_language = "en",
        final_audit_trains = "train_3",
        possible_burst_promote_trains = "train_2"
      )
      session$flushReact()
      en <- collect_copy()
      expect_match(en, "Legacy: promote possible to real", fixed = TRUE)
      expect_match(en, "Preview Legacy promotion", fixed = TRUE)
      expect_match(en, "Apply Legacy promotion", fixed = TRUE)
      expect_match(en, "Revert Legacy promotion", fixed = TRUE)
      expect_match(
        en,
        "Select trains whose possible_burst labels will be bulk-promoted",
        fixed = TRUE
      )
      expect_match(en, "Select trains for the final-audit result", fixed = TRUE)
      expect_match(en, "Select one or more spike trains", fixed = TRUE)
      expect_match(en, '<option value="train_2" selected>', fixed = TRUE)
      expect_match(en, '<option value="train_3" selected>', fixed = TRUE)
      expect_false(grepl("[\u3400-\u9FFF]", en, perl = TRUE))

      session$setInputs(ui_language = "zh")
      session$flushReact()
      zh <- collect_copy()
      expect_match(zh, "Legacy\uFF1A\u5C06 possible \u5347\u7EA7\u4E3A real", fixed = TRUE)
      expect_match(zh, "\u9884\u89C8 Legacy \u5347\u7EA7", fixed = TRUE)
      expect_match(
        zh,
        "\u9009\u62E9\u8981\u6279\u91CF\u5347\u7EA7 possible_burst \u7684 trains",
        fixed = TRUE
      )
      expect_match(zh, "\u53EF\u9009\u591A\u6761 spike train", fixed = TRUE)
      expect_match(zh, '<option value="train_2" selected>', fixed = TRUE)
      expect_match(zh, '<option value="train_3" selected>', fixed = TRUE)
      expect_identical(serialize(rv$datasets, NULL, version = 3L), dataset_bytes)

      empty_ds <- ds
      empty_ds$trains <- list()
      empty_ds$quality <- data.frame()
      rv$datasets <- list(empty = empty_ds)
      rv$current_id <- "empty"
      session$setInputs(ui_language = "en")
      session$flushReact()
      for (id in c(
        "possible_burst_promote_train_selector",
        "final_audit_train_selector"
      )) {
        condition <- tryCatch(output[[id]], error = identity)
        expect_s3_class(condition, "validation")
        expect_identical(conditionMessage(condition), "No trains are available.")
      }

      phase2b_ds <- ds
      phase2b_ds$results <- list(multitrack_review = list())
      rv$datasets <- list(phase2b = phase2b_ds)
      rv$current_id <- "phase2b"
      session$flushReact()
      phase2b_copy <- paste(
        as.character(output$final_audit_promote_control),
        as.character(output$legacy_promotion_controls),
        collapse = "\n"
      )
      expect_match(phase2b_copy, "Phase 2B active", fixed = TRUE)
      expect_match(phase2b_copy, "Legacy bulk promotion disabled", fixed = TRUE)
      expect_false(grepl("[\u3400-\u9FFF]", phase2b_copy, perl = TRUE))
    })
  ))
})

test_that("parameter-sensitivity Plotly copy follows language without mutating results", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  ds <- ui_dynamic_status_fixture()
  sensitivity_summary <- data.frame(
    parameter_path = rep("event_core.seed_band_upper_sec", 2L),
    parameter_label = rep("Burst seed ISI 上限", 2L),
    variant_value = c("0.01", "0.02"),
    macro_F1 = c(0.4, 0.6),
    macro_precision = c(0.5, 0.7),
    macro_recall = c(0.35, 0.55),
    changed_event_n = c(1L, 2L),
    stringsAsFactors = FALSE
  )

  suppressMessages(suppressWarnings(
    shiny::testServer(server, {
      rv$datasets <- list(ui_dynamic_status_fixture = ds)
      rv$current_id <- "ui_dynamic_status_fixture"
      rv$parameter_sensitivity <- list(summary = sensitivity_summary)

      session$setInputs(
        ui_language = "zh",
        parameter_sensitivity_plot_metric = "macro_F1"
      )
      session$flushReact()
      zh <- paste(
        as.character(output$parameter_sensitivity_metric_plot),
        collapse = ""
      )
      science_bytes <- serialize(
        rv$parameter_sensitivity,
        NULL,
        version = 3L
      )

      expect_true(stpd_i18n_contains_cjk(zh))
      expect_match(zh, "参数：", fixed = TRUE)
      expect_match(zh, "参数变体值", fixed = TRUE)

      session$setInputs(ui_language = "en")
      session$flushReact()
      en <- paste(
        as.character(output$parameter_sensitivity_metric_plot),
        collapse = ""
      )

      expect_false(stpd_i18n_contains_cjk(en))
      for (copy in c(
        "Parameter: ",
        "Path: ",
        "Variant value: ",
        "Event differences: ",
        "Parameter variant value",
        "Burst seed ISI upper bound"
      )) {
        expect_match(en, copy, fixed = TRUE)
      }
      expect_identical(
        serialize(rv$parameter_sensitivity, NULL, version = 3L),
        science_bytes
      )

      session$setInputs(ui_language = "zh")
      session$flushReact()
      zh_again <- paste(
        as.character(output$parameter_sensitivity_metric_plot),
        collapse = ""
      )

      expect_true(stpd_i18n_contains_cjk(zh_again))
      expect_match(zh_again, "参数：", fixed = TRUE)
      expect_match(zh_again, "参数变体值", fixed = TRUE)
      expect_identical(
        serialize(rv$parameter_sensitivity, NULL, version = 3L),
        science_bytes
      )
    })
  ))
})
