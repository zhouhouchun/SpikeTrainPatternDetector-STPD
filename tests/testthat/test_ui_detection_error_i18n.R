detection_error_i18n_env <- function(lang = "zh") {
  env <- new.env(parent = environment(stpd_server_install_detection_module))
  env$output <- new.env(parent = emptyenv())
  env$input <- shiny::reactiveValues(run_detector = 0L)
  env$rv <- shiny::reactiveValues(last_detector_summary = NULL)
  env$session <- shiny::MockShinySession$new()
  env$ui_language <- function() lang
  env$ui_text <- function(key, ...) stpd_ui_copy(key, lang = lang, ...)
  env$notifications <- character()
  env$showNotification <- function(ui, ...) {
    env$notifications <- c(env$notifications, as.character(ui))
    invisible(NULL)
  }
  env$updateTabsetPanel <- function(...) invisible(NULL)
  shiny::withReactiveDomain(
    env$session,
    stpd_server_install_detection_module(env)
  )
  env
}

test_that("non-duplicate pre-detection QC localizes prose and quarantines raw details", {
  env <- detection_error_i18n_env("zh")
  on.exit(env$session$close(), add = TRUE)
  qc <- data.frame(
    warning_level = "error",
    train = "train_1",
    warning_message = "invalid_duration=TRUE; opaque_engine_failure=ABC",
    n_spikes = 12L,
    duration_sec = 0,
    n_duplicate_timestamps = 0L,
    n_zero_or_negative_ISI = 0L,
    n_zero_or_negative_timestamp_steps = 0L,
    timestamp_ISI_mismatch = FALSE,
    stringsAsFactors = FALSE
  )
  raw <- paste0(
    "Pre-detection QC found 1 train(s) with data-integrity errors. ",
    "train_1: invalid_duration=TRUE; opaque_engine_failure=ABC"
  )
  summary <- env$detector_pre_qc_summary(qc, raw)
  marker <- stpd_ui_copy("technical_detail", lang = "zh", detail = "")
  zh_prose <- strsplit(unname(summary[["zh"]]), marker, fixed = TRUE)[[1]][1]

  expect_s3_class(summary, "stpd_ui_bilingual")
  expect_setequal(names(summary), c("zh", "en"))
  expect_match(summary[["zh"]], "\u8BB0\u5F55\u65F6\u957F\u65E0\u6548", fixed = TRUE)
  expect_false(grepl("Pre-detection QC found", zh_prose, fixed = TRUE))
  expect_false(grepl("opaque_engine_failure", zh_prose, fixed = TRUE))
  expect_match(summary[["zh"]], paste0(marker, "Pre-detection QC found"), fixed = TRUE)
  expect_match(summary[["en"]], "Technical details: Pre-detection QC found", fixed = TRUE)
})

test_that("parameter and detector errors remain bilingual with unknown prose in technical details", {
  env <- detection_error_i18n_env("zh")
  on.exit(env$session$close(), add = TRUE)
  env$current_dataset <- function() list(trains = list(), meta = list())
  env$stpd_validate_params <- function(params) data.frame(
    path = "detector.min_valid_isi_sec",
    issue = "numeric parameter has missing or invalid value",
    severity = "error",
    stringsAsFactors = FALSE
  )

  result <- env$run_detector_from_ui(
    params_override = list(), switch_to_plot = FALSE, notify = FALSE
  )
  summary <- shiny::isolate(env$rv$last_detector_summary)
  expect_null(result)
  expect_s3_class(summary, "stpd_ui_bilingual")
  expect_match(summary[["zh"]], "numeric \u53C2\u6570\u7F3A\u5931\u6216\u53D6\u503C\u65E0\u6548", fixed = TRUE)
  expect_false(grepl(
    "numeric parameter has missing or invalid value",
    summary[["zh"]], fixed = TRUE
  ))
  expect_match(
    summary[["en"]],
    "numeric parameter has missing or invalid value",
    fixed = TRUE
  )
  expect_identical(tail(env$notifications, 1L), unname(summary[["zh"]]))

  unknown <- env$detector_parameter_issue_summary(data.frame(
    path = "detector.future",
    issue = "opaque future validator failure",
    severity = "error",
    stringsAsFactors = FALSE
  ))
  marker <- stpd_ui_copy("technical_detail", lang = "zh", detail = "")
  zh_prose <- strsplit(unname(unknown[["zh"]]), marker, fixed = TRUE)[[1]][1]
  expect_false(grepl("opaque future validator failure", zh_prose, fixed = TRUE))
  expect_match(
    unknown[["zh"]],
    paste0(marker, "detector.future - opaque future validator failure"),
    fixed = TRUE
  )

  env$detector_notify_error(
    simpleError("opaque detector engine exception"),
    prefix = "\u6279\u5904\u7406\u672A\u5B8C\u6210",
    status = "batch"
  )
  batch <- shiny::isolate(env$rv$batch_status)
  batch_prose <- strsplit(unname(batch[["zh"]]), marker, fixed = TRUE)[[1]][1]
  expect_s3_class(batch, "stpd_ui_bilingual")
  expect_false(grepl("opaque detector engine exception", batch_prose, fixed = TRUE))
  expect_match(batch[["zh"]], paste0(marker, "opaque detector engine exception"), fixed = TRUE)
  expect_match(batch[["en"]], "Technical details: opaque detector engine exception", fixed = TRUE)
})
