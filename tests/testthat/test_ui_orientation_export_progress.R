orientation_export_module_env <- function(mock_session = FALSE) {
  env <- new.env(parent = environment(stpd_server_install_export_module))
  env$output <- new.env(parent = emptyenv())
  env$rv <- shiny::reactiveValues(
    current_id = "dataset_fixture",
    formal_export_sequence = 0L,
    formal_export_progress = NULL
  )
  env$session <- if (isTRUE(mock_session)) {
    shiny::MockShinySession$new()
  } else {
    list(token = "orientation-test")
  }
  stpd_server_install_export_module(env)
  env
}

orientation_export_fixture_dataset <- function() {
  list(meta = list(display_name = "Orientation fixture"))
}

test_that("formal export progress records an immutable scoped identity", {
  env <- orientation_export_module_env()
  ds <- orientation_export_fixture_dataset()

  export_id <- env$formal_export_progress_begin("results_zip", ds)
  initial <- shiny::isolate(env$rv$formal_export_progress)
  expect_setequal(names(initial), c(
    "export_id", "kind", "state", "dataset_id", "dataset_name",
    "run_id", "params_sha256", "scope", "selected_train_n",
    "total_train_n", "selected_trains", "phase", "progress",
    "started_at", "updated_at", "completed_at", "detail", "detail_i18n",
    "file_size_bytes", "error_message"
  ))
  expect_identical(initial$export_id, export_id)
  expect_identical(initial$kind, "results_zip")
  expect_identical(initial$state, "preparing")
  expect_identical(initial$dataset_id, "dataset_fixture")
  expect_identical(initial$dataset_name, "Orientation fixture")
  expect_identical(initial$phase, "validate")
  expect_identical(initial$progress, 0)
  expect_identical(initial$detail_i18n, c(zh = "", en = ""))
  expect_match(initial$started_at, "Z$")
  expect_identical(initial$completed_at, "")

  context <- list(state = list(
    run_id = "run_fixture_1",
    params_sha256 = paste(rep("a", 64L), collapse = ""),
    selected_train_n = 2L,
    total_train_n = 2L,
    selected_trains = c("train_1", "train_2")
  ))
  expect_true(env$formal_export_progress_bind_context(export_id, context))
  expect_true(env$formal_export_progress_update(
    export_id, phase = "write_event_tables", progress = 0.65,
    detail = "Event tables written."
  ))
  # Progress is monotone even if a late phase reports a smaller value.
  expect_true(env$formal_export_progress_update(
    export_id, phase = "write_event_tables", progress = 0.40,
    detail = "Late update."
  ))
  current <- shiny::isolate(env$rv$formal_export_progress)
  expect_identical(current$run_id, "run_fixture_1")
  expect_identical(current$params_sha256, context$state$params_sha256)
  expect_identical(current$scope, "2/2")
  expect_identical(current$selected_train_n, 2L)
  expect_identical(current$total_train_n, 2L)
  expect_identical(current$selected_trains, c("train_1", "train_2"))
  expect_identical(current$progress, 0.65)
})

test_that("superseded and terminal formal exports cannot overwrite current state", {
  env <- orientation_export_module_env()
  ds <- orientation_export_fixture_dataset()

  old_id <- env$formal_export_progress_begin("labeled_csv", ds)
  current_id <- env$formal_export_progress_begin("results_zip", ds)
  expect_false(identical(old_id, current_id))
  expect_false(env$formal_export_progress_update(
    old_id, phase = "generated", progress = 1, state = "generated"
  ))
  expect_identical(
    shiny::isolate(env$rv$formal_export_progress$export_id),
    current_id
  )
  expect_identical(
    shiny::isolate(env$rv$formal_export_progress$state),
    "preparing"
  )

  expect_true(env$formal_export_progress_update(
    current_id, phase = "generated", progress = 1,
    detail = "Generated.", state = "generated", file_size_bytes = 12
  ))
  expect_false(env$formal_export_progress_update(
    current_id, phase = "archive", progress = 0.9,
    detail = "Stale preparing update.", state = "preparing"
  ))
  expect_false(env$formal_export_progress_bind_context(
    current_id,
    list(state = list(run_id = "stale_run"))
  ))
  terminal <- shiny::isolate(env$rv$formal_export_progress)
  expect_identical(terminal$state, "generated")
  expect_identical(terminal$phase, "generated")
  expect_identical(terminal$progress, 1)
  expect_true(nzchar(terminal$completed_at))
})

test_that("formal export completion verifies bytes and avoids a browser-save claim", {
  env <- orientation_export_module_env()
  ds <- orientation_export_fixture_dataset()
  notifications <- list()
  env$showNotification <- function(ui, type = NULL, duration = NULL, ...) {
    notifications[[length(notifications) + 1L]] <<- list(
      text = as.character(ui), type = type, duration = duration
    )
    invisible(NULL)
  }

  export_id <- env$formal_export_progress_begin("labeled_csv", ds)
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  writeLines(c("a,b", "1,2"), file, useBytes = TRUE)
  expect_true(env$formal_export_notify_generated(
    export_id, "Formal labeled CSV", file
  ))
  generated <- shiny::isolate(env$rv$formal_export_progress)
  expect_identical(generated$state, "generated")
  expect_identical(generated$phase, "generated")
  expect_true(is.finite(generated$file_size_bytes))
  expect_gt(generated$file_size_bytes, 0)
  expect_match(notifications[[1L]]$text, "generated on the server", fixed = TRUE)
  expect_match(notifications[[1L]]$text, "browser will continue", fixed = TRUE)
  expect_false(grepl("saved", notifications[[1L]]$text, ignore.case = TRUE))

  empty <- tempfile(fileext = ".zip")
  on.exit(unlink(empty), add = TRUE)
  file.create(empty)
  expect_error(
    env$formal_export_generated_file_size(empty),
    "did not generate a non-empty file",
    fixed = TRUE
  )

  error_id <- env$formal_export_progress_begin("results_zip", ds)
  expect_true(env$formal_export_notify_error(
    error_id, "Formal results ZIP", simpleError("fixture failure")
  ))
  failed <- shiny::isolate(env$rv$formal_export_progress)
  expect_identical(failed$state, "error")
  expect_identical(failed$phase, "error")
  expect_identical(failed$error_message, "fixture failure")
  expect_true(nzchar(failed$completed_at))
  expect_identical(notifications[[2L]]$type, "error")
})

test_that("labeled CSV handler reaches generated and error terminal states", {
  handler_content <- function(env) {
    handler <- env$output$download_labeled_csv
    render_func <- get("renderFunc", envir = environment(handler))
    get("content", envir = environment(render_func))
  }
  prepare_env <- function() {
    env <- orientation_export_module_env(mock_session = TRUE)
    ds <- stpd_golden_test_dataset("middle_burst")
    ds$meta$display_name <- "Export fixture"
    env$input <- list(time_unit = "ms")
    env$current_dataset <- function() ds
    env$formal_results_export_context <- function(ds) list(
      params = default_params(),
      state = list(
        eligible = TRUE,
        run_id = "run_fixture",
        params_sha256 = paste(rep("b", 64L), collapse = ""),
        selected_train_n = length(ds$trains),
        total_train_n = length(ds$trains),
        selected_trains = names(ds$trains)
      )
    )
    env$min_valid_isi_sec <- function() default_params()$detector$min_valid_isi_sec
    env$showNotification <- function(...) invisible(NULL)
    env
  }

  success_env <- prepare_env()
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file), add = TRUE)
  shiny::withReactiveDomain(
    success_env$session,
    handler_content(success_env)(csv_file)
  )
  expect_true(file.exists(csv_file))
  expect_gt(file.info(csv_file)$size, 0)
  success <- shiny::isolate(success_env$rv$formal_export_progress)
  expect_identical(success$kind, "labeled_csv")
  expect_identical(success$state, "generated")
  expect_identical(success$phase, "generated")
  expect_identical(success$scope, "1/1")

  error_env <- prepare_env()
  error_env$write_csv_safe <- function(...) {
    stop("fixture writer failed", call. = FALSE)
  }
  failed_file <- tempfile(fileext = ".csv")
  on.exit(unlink(failed_file), add = TRUE)
  expect_error(
    shiny::withReactiveDomain(
      error_env$session,
      handler_content(error_env)(failed_file)
    ),
    "fixture writer failed",
    fixed = TRUE
  )
  failed <- shiny::isolate(error_env$rv$formal_export_progress)
  expect_identical(failed$state, "error")
  expect_identical(failed$phase, "error")
  expect_identical(failed$error_message, "fixture writer failed")
  expect_true(nzchar(failed$completed_at))
})

test_that("both formal handlers expose real phased progress without changing contracts", {
  source <- paste(
    deparse(body(stpd_server_install_export_module), width.cutoff = 500L),
    collapse = "\n"
  )

  expect_gte(length(gregexpr("withProgress\\(", source)[[1L]]), 2L)
  expect_match(source, 'formal_export_progress_begin("labeled_csv", ds)', fixed = TRUE)
  expect_match(source, 'formal_export_progress_begin("results_zip", ds)', fixed = TRUE)
  expect_match(source, 'export_id, "write_event_tables"', fixed = TRUE)
  expect_match(source, 'export_id, "write_validation"', fixed = TRUE)
  expect_match(source, 'export_id, "archive"', fixed = TRUE)
  expect_match(source, "formal_export_generated_file_size(file)", fixed = TRUE)
  expect_gte(length(gregexpr("stop\\(e\\)", source)[[1L]]), 2L)

  # The formal gate and established scientific/export contracts remain explicit.
  expect_match(source, "stpd_ui_formal_export_state(", fixed = TRUE)
  expect_match(source, "auto_others = FALSE", fixed = TRUE)
  expect_match(source, "write_tiered_result_exports(ds_norm, p, out_dir)", fixed = TRUE)
  expect_match(source, "stpd_write_multitrack_preview(ds_norm, out_dir)", fixed = TRUE)
  expect_match(source, "stpd_write_multitrack_review(ds_norm, out_dir)", fixed = TRUE)
})
