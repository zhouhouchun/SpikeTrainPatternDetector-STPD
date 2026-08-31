export_scope_adv_train <- function(offset = 0, auto_label = "") {
  increments <- rep(c(0.020, 0.018, 0.025, 0.120, 0.030), 8L)
  timestamp <- offset + cumsum(c(0, increments))
  n <- length(timestamp)
  labels <- rep("", n)
  if (nzchar(auto_label)) labels[2:4] <- auto_label
  data.frame(
    idx = seq_len(n),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, diff(timestamp)),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = labels,
    stringsAsFactors = FALSE
  )
}

export_scope_adv_raw_dataset <- function(name = "export_scope_adv") {
  SpikeTrainPatternDetector:::make_dataset(
    name,
    "synthetic",
    list(
      train_a = export_scope_adv_train(),
      train_b = export_scope_adv_train(0.001)
    ),
    unit_in = "s"
  )
}

export_scope_adv_detected_dataset <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      params <- default_params()
      cached <<- stpd_detect(
        export_scope_adv_raw_dataset(),
        params,
        selected_trains = c("train_a", "train_b"),
        collect_diagnostics = TRUE
      )
    }
    unserialize(serialize(cached, NULL, version = 3L))
  }
})

export_scope_adv_run_identity <- function(ds, dataset_id = "export_scope_adv") {
  stpd_ui_run_identity(
    ds,
    params = ds$params_effective,
    dataset_id = dataset_id,
    selected_trains = names(ds$trains)
  )
}

export_scope_adv_mutate_result_table <- function(ds, table_name) {
  table <- ds$results[[table_name]]
  stopifnot(is.data.frame(table), nrow(table) > 0L, ncol(table) > 0L)
  numeric_columns <- names(table)[vapply(table, is.numeric, logical(1))]
  if (length(numeric_columns) > 0L) {
    column <- numeric_columns[[1L]]
    row <- which(is.finite(table[[column]]))[1L]
    if (is.na(row)) row <- 1L
    old <- suppressWarnings(as.numeric(table[[column]][[row]]))
    if (!is.finite(old)) old <- 0
    table[[column]][[row]] <- old + 0.125
  } else {
    column <- names(table)[[1L]]
    table[[column]] <- as.character(table[[column]])
    table[[column]][[1L]] <- paste0(table[[column]][[1L]], "_tampered")
  }
  ds$results[[table_name]] <- table
  ds
}

export_scope_adv_partial_dataset <- function() {
  ds <- SpikeTrainPatternDetector:::make_dataset(
    "export_scope_partial",
    "synthetic",
    list(
      train_a = export_scope_adv_train(auto_label = "possible_burst"),
      train_b = export_scope_adv_train(0.001, auto_label = "pause")
    ),
    unit_in = "s"
  )
  params <- default_params()
  params_hash <- stpd_params_hash(params)
  ds$params_effective <- params
  ds$params_last <- params
  ds$params_est <- params
  ds$results$run_metadata_public <- data.frame(
    run_id = "export_scope_partial_run",
    params_hash = params_hash,
    dataset_name = "export_scope_partial",
    input_sha256 = "",
    total_train_n = 2L,
    selected_train_n = 1L,
    selected_trains = "train_a",
    stringsAsFactors = FALSE
  )
  ds
}

export_scope_adv_html <- function(x) {
  paste(as.character(x), collapse = "")
}

export_scope_adv_failure_ids <- function(plan) {
  failures <- plan$failures %||% list()
  ids <- character()
  if (is.data.frame(failures)) {
    id_column <- intersect(c("dataset_id", "id"), names(failures))[1L]
    if (!is.na(id_column)) ids <- as.character(failures[[id_column]])
  } else if (is.list(failures) && length(failures) > 0L) {
    ids <- names(failures) %||% character()
  }
  entries <- plan$entries %||% data.frame()
  if (is.data.frame(entries) && nrow(entries) > 0L &&
      "eligible" %in% names(entries)) {
    id_column <- intersect(c("dataset_id", "id"), names(entries))[1L]
    if (!is.na(id_column)) {
      ids <- c(ids, as.character(entries[[id_column]][!entries$eligible]))
    }
  }
  sort(unique(ids[!is.na(ids) & nzchar(ids)]), method = "radix")
}

export_scope_adv_attach_fixed_export_artifacts <- function(ds) {
  artifact_names <- c(
    "final_audit_summary", "final_audit_events", "final_audit_history",
    "final_audit_event_history", "possible_burst_promotion_audit",
    "possible_burst_promotion_summary"
  )
  for (name in artifact_names) {
    ds$results[[name]] <- data.frame(
      artifact = name, value = 1, stringsAsFactors = FALSE
    )
  }
  ds$quality <- data.frame(
    train = names(ds$trains),
    warning_level = rep("ok", length(ds$trains)),
    value = seq_along(ds$trains),
    stringsAsFactors = FALSE
  )
  ds
}

export_scope_adv_mutate_fixed_artifact <- function(ds, artifact) {
  if (identical(artifact, "data_quality")) {
    stopifnot(is.data.frame(ds$quality), nrow(ds$quality) > 0L)
    ds$quality$value[[1L]] <- ds$quality$value[[1L]] + 1
    return(ds)
  }
  export_scope_adv_mutate_result_table(ds, artifact)
}

export_scope_adv_attach_stale_validation <- function(
    ds, dataset_id = "export_scope_adv") {
  dataset_identity <- stpd_ui_dataset_identity(ds, dataset_id)
  run_identity <- export_scope_adv_run_identity(ds, dataset_id)
  report <- list(
    meta = data.frame(
      validation_run_id = "export_scope_stale_validation",
      stringsAsFactors = FALSE
    ),
    split = data.frame(
      train = names(ds$trains), split = "validation",
      stringsAsFactors = FALSE
    ),
    calibration_metrics = data.frame(metric = "fixture", value = 1),
    validation_metrics = data.frame(metric = "fixture", value = 1),
    overfit_report = data.frame(item = "fixture", value = "ok")
  )
  identity <- stpd_ui_validation_identity(
    report,
    dataset_identity = dataset_identity,
    run_identity = run_identity,
    selected_trains = names(ds$trains),
    truth_dependencies = character(),
    source_kind = "shadow_independent",
    source_params_sha256 = run_identity$effective_params_sha256,
    dataset = ds
  )
  identity$source_params_sha256 <- paste(rep("e", 64L), collapse = "")
  report$ui_identity <- identity
  ds$results$scientific_validation <- report
  list(
    dataset = ds,
    report = report,
    validation_identity = identity,
    dataset_identity = stpd_ui_dataset_identity(ds, dataset_id),
    run_identity = export_scope_adv_run_identity(ds, dataset_id)
  )
}

export_scope_adv_download_content <- function(handler) {
  render_func <- get("renderFunc", envir = environment(handler))
  get("content", envir = environment(render_func))
}

export_scope_adv_zip_members <- function(file) {
  as.character(utils::unzip(file, list = TRUE)$Name)
}

export_scope_adv_extract_zip <- function(file) {
  out_dir <- tempfile("export_scope_adv_unzip_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(file, exdir = out_dir)
  out_dir
}

export_scope_adv_detailed_validation_members <- function(members) {
  names_only <- basename(as.character(members))
  setdiff(
    names_only[grepl("^Scientific_validation_", names_only)],
    "Scientific_validation_summary.csv"
  )
}

test_that("formal ZIP gate binds candidate and event ledgers byte-for-byte", {
  ds <- export_scope_adv_detected_dataset()
  run_identity <- export_scope_adv_run_identity(ds)
  output_identity <- stpd_ui_detector_output_identity(
    ds, selected_trains = run_identity$selected_trains
  )

  expect_true(output_identity$verifiable)
  expect_true(nzchar(output_identity$combined_sha256))
  expect_true(all(c("candidate_ledger", "event_ledger") %in%
    output_identity$artifact_names))

  baseline <- stpd_ui_formal_export_state(
    ds,
    params = ds$params_effective,
    dataset_id = "export_scope_adv",
    run_identity = run_identity,
    expected_detector_output_identity = output_identity
  )
  expect_true(baseline$eligible)
  expect_identical(baseline$code, "formal_export_ready")

  for (table_name in c("candidate_ledger", "event_ledger")) {
    tampered <- export_scope_adv_mutate_result_table(ds, table_name)
    # The per-ISI AUTO labels are unchanged.  This proves that the dedicated
    # detector-artifact identity, rather than the older run-state hash, closes
    # the formal-export path.
    expect_identical(
      stpd_ui_run_state(
        tampered,
        params = ds$params_effective,
        dataset_id = "export_scope_adv",
        run_identity = run_identity
      )$code,
      "run_current"
    )

    artifact_state <- stpd_ui_detector_output_state(
      tampered,
      expected_identity = output_identity,
      selected_trains = run_identity$selected_trains
    )
    expect_false(artifact_state$current)
    expect_true(length(artifact_state$reason_codes) > 0L)

    gate <- stpd_ui_formal_export_state(
      tampered,
      params = ds$params_effective,
      dataset_id = "export_scope_adv",
      run_identity = run_identity,
      expected_detector_output_identity = output_identity
    )
    expect_false(gate$eligible)
    expect_match(gate$code, "^formal_export_detector_output_")
  }
})

test_that("formal ZIP gate binds every exported audit and quality artifact", {
  ds <- export_scope_adv_attach_fixed_export_artifacts(
    export_scope_adv_detected_dataset()
  )
  run_identity <- export_scope_adv_run_identity(ds)
  output_identity <- run_identity$detector_output_identity
  protected <- c(
    "final_audit_summary", "final_audit_events", "final_audit_history",
    "final_audit_event_history", "possible_burst_promotion_audit",
    "possible_burst_promotion_summary", "data_quality"
  )

  expect_true(output_identity$verifiable)
  expect_true(all(protected %in% output_identity$artifact_names))
  baseline <- stpd_ui_formal_export_state(
    ds,
    params = ds$params_effective,
    dataset_id = "export_scope_adv",
    run_identity = run_identity,
    expected_detector_output_identity = output_identity
  )
  expect_true(baseline$eligible)

  for (artifact in protected) {
    tampered <- export_scope_adv_mutate_fixed_artifact(ds, artifact)
    artifact_state <- stpd_ui_detector_output_state(
      tampered,
      expected_identity = output_identity,
      selected_trains = run_identity$selected_trains
    )
    expect_false(artifact_state$current)
    expect_true(artifact %in% artifact_state$changed_artifacts)

    gate <- stpd_ui_formal_export_state(
      tampered,
      params = ds$params_effective,
      dataset_id = "export_scope_adv",
      run_identity = run_identity,
      expected_detector_output_identity = output_identity
    )
    expect_false(gate$eligible)
    expect_identical(
      gate$code, "formal_export_detector_output_changed"
    )
  }
})

test_that("legal final-audit rebuild refreshes identity before export", {
  dataset_id <- "export_scope_legal_audit"
  ds <- export_scope_adv_detected_dataset()
  run_identity <- export_scope_adv_run_identity(ds, dataset_id)
  before_sha <- run_identity$detector_output_identity$combined_sha256

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- stats::setNames(list(ds), dataset_id)
      rv$current_id <- dataset_id
      rv$ui_run_identity_by_dataset <- stats::setNames(
        list(run_identity), dataset_id
      )
      session$setInputs(
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        final_audit_trains = names(ds$trains)
      )
      session$flushReact()

      # Exercise both user actions: request the confirmation and then accept
      # the still-current train/dataset scope.
      session$setInputs(rebuild_final_audit = 1L)
      session$flushReact()
      pending <- shiny::isolate(rv$ui_pending_confirmation)
      expect_identical(pending$confirm_id, "confirm_rebuild_final_audit")
      expect_setequal(pending$context$train_ids, names(ds$trains))

      session$setInputs(confirm_rebuild_final_audit = 1L)
      session$flushReact()

      rebuilt <- shiny::isolate(rv$datasets[[dataset_id]])
      refreshed <- shiny::isolate(
        rv$ui_run_identity_by_dataset[[dataset_id]]
      )
      expect_true(refreshed$detector_output_identity$verifiable)
      expect_false(identical(
        refreshed$detector_output_identity$combined_sha256, before_sha
      ))
      expect_identical(
        refreshed$detector_output_identity$combined_sha256,
        stpd_ui_detector_output_identity(
          rebuilt, selected_trains = refreshed$selected_trains
        )$combined_sha256
      )
      allowed_review_changes <- c(
        "legacy_final_audit_train_output",
        "final_audit_summary", "final_audit_events",
        "final_audit_history", "final_audit_event_history"
      )
      legal_delta <- stpd_ui_detector_output_state(
        rebuilt,
        expected_identity = run_identity$detector_output_identity,
        selected_trains = run_identity$selected_trains
      )
      expect_false(legal_delta$current)
      expect_setequal(
        legal_delta$changed_artifacts, allowed_review_changes
      )

      ready <- stpd_ui_formal_export_state(
        rebuilt,
        params = rebuilt$params_effective,
        dataset_id = dataset_id,
        run_identity = refreshed,
        expected_detector_output_identity =
          refreshed$detector_output_identity
      )
      expect_true(ready$eligible)
      expect_identical(ready$code, "formal_export_ready")

      # Inject a non-allowlisted legacy-override mutation into an otherwise
      # legitimate rebuild result.  The commit must reject the whole candidate
      # dataset, leaving the live dataset and trusted identity untouched.
      server_env <- environment(apply_final_audit_from_ui)
      original_apply <- get(
        "stpd_apply_final_audit", envir = server_env, inherits = TRUE
      )
      assign(
        "stpd_apply_final_audit",
        function(...) {
          result <- original_apply(...)
          train <- names(result$dataset$trains)[[1L]]
          dat <- result$dataset$trains[[train]]
          if (!("pattern_user_override_reason" %in% names(dat))) {
            dat$pattern_user_override_reason <- rep("", nrow(dat))
          }
          dat$pattern_user_override_reason[[1L]] <-
            "non_allowlisted_commit_injection"
          result$dataset$trains[[train]] <- dat
          result
        },
        envir = server_env
      )
      session$setInputs(rebuild_final_audit = 2L)
      session$flushReact()
      expect_identical(
        shiny::isolate(rv$ui_pending_confirmation$confirm_id),
        "confirm_rebuild_final_audit"
      )
      session$setInputs(confirm_rebuild_final_audit = 2L)
      session$flushReact()
      expect_identical(
        shiny::isolate(rv$datasets[[dataset_id]]), rebuilt
      )
      expect_identical(
        shiny::isolate(rv$ui_run_identity_by_dataset[[dataset_id]]),
        refreshed
      )
      ready_after_rejected_commit <- stpd_ui_formal_export_state(
        shiny::isolate(rv$datasets[[dataset_id]]),
        params = rebuilt$params_effective,
        dataset_id = dataset_id,
        run_identity = refreshed,
        expected_detector_output_identity =
          refreshed$detector_output_identity
      )
      expect_true(ready_after_rejected_commit$eligible)
      expect_identical(
        ready_after_rejected_commit$code, "formal_export_ready"
      )

      tampered <- export_scope_adv_mutate_result_table(
        rebuilt, "final_audit_summary"
      )
      blocked <- stpd_ui_formal_export_state(
        tampered,
        params = rebuilt$params_effective,
        dataset_id = dataset_id,
        run_identity = refreshed,
        expected_detector_output_identity =
          refreshed$detector_output_identity
      )
      expect_false(blocked$eligible)
      expect_identical(
        blocked$code, "formal_export_detector_output_changed"
      )

      audit_column_tampered <- rebuilt
      train <- names(audit_column_tampered$trains)[[1L]]
      expect_true(
        "pattern_audit_final" %in%
          names(audit_column_tampered$trains[[train]])
      )
      audit_column_tampered$trains[[train]]$
        pattern_audit_final[[1L]] <- "tampered_audit_final"
      audit_column_state <- stpd_ui_detector_output_state(
        audit_column_tampered,
        expected_identity = refreshed$detector_output_identity,
        selected_trains = refreshed$selected_trains
      )
      expect_false(audit_column_state$current)
      expect_true(
        "legacy_final_audit_train_output" %in%
          audit_column_state$changed_artifacts
      )
      audit_column_gate <- stpd_ui_formal_export_state(
        audit_column_tampered,
        params = rebuilt$params_effective,
        dataset_id = dataset_id,
        run_identity = refreshed,
        expected_detector_output_identity =
          refreshed$detector_output_identity
      )
      expect_false(audit_column_gate$eligible)
      expect_identical(
        audit_column_gate$code,
        "formal_export_detector_output_changed"
      )

      override_tampered <- rebuilt
      override_train <- names(override_tampered$trains)[[1L]]
      override_dat <- override_tampered$trains[[override_train]]
      if (!("pattern_user_override_reason" %in% names(override_dat))) {
        override_dat$pattern_user_override_reason <- rep(
          "", nrow(override_dat)
        )
      }
      override_dat$pattern_user_override_reason[[1L]] <-
        "tampered_legacy_override"
      override_tampered$trains[[override_train]] <- override_dat
      override_state <- stpd_ui_detector_output_state(
        override_tampered,
        expected_identity = refreshed$detector_output_identity,
        selected_trains = refreshed$selected_trains
      )
      expect_false(override_state$current)
      expect_true(
        "legacy_override_train_output" %in%
          override_state$changed_artifacts
      )
      override_gate <- stpd_ui_formal_export_state(
        override_tampered,
        params = rebuilt$params_effective,
        dataset_id = dataset_id,
        run_identity = refreshed,
        expected_detector_output_identity =
          refreshed$detector_output_identity
      )
      expect_false(override_gate$eligible)
      expect_identical(
        override_gate$code,
        "formal_export_detector_output_changed"
      )
    })
  ))
})

test_that("final-audit actions cannot rebase pre-existing detector tamper", {
  dataset_id <- "export_scope_tamper_before_audit"
  ds <- export_scope_adv_detected_dataset()
  run_identity <- export_scope_adv_run_identity(ds, dataset_id)
  baseline <- stpd_ui_formal_export_state(
    ds,
    params = ds$params_effective,
    dataset_id = dataset_id,
    run_identity = run_identity,
    expected_detector_output_identity =
      run_identity$detector_output_identity
  )
  expect_true(baseline$eligible)
  expect_identical(baseline$code, "formal_export_ready")

  cases <- list(
    list(
      artifact = "candidate_ledger",
      request = "rebuild_final_audit",
      confirm = "confirm_rebuild_final_audit"
    ),
    list(
      artifact = "event_ledger",
      request = "clear_final_audit",
      confirm = "confirm_clear_final_audit"
    ),
    list(
      artifact = "data_quality",
      request = "rebuild_final_audit",
      confirm = "confirm_rebuild_final_audit"
    )
  )

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      for (i in seq_along(cases)) {
        case <- cases[[i]]
        tampered <- export_scope_adv_mutate_fixed_artifact(
          ds, case$artifact
        )
        tampered_output <- stpd_ui_detector_output_identity(
          tampered, selected_trains = run_identity$selected_trains
        )
        before_state <- stpd_ui_detector_output_state(
          tampered,
          expected_identity = run_identity$detector_output_identity,
          selected_trains = run_identity$selected_trains
        )
        expect_false(before_state$current)
        expect_true(case$artifact %in% before_state$changed_artifacts)
        before_gate <- stpd_ui_formal_export_state(
          tampered,
          params = tampered$params_effective,
          dataset_id = dataset_id,
          run_identity = run_identity,
          expected_detector_output_identity =
            run_identity$detector_output_identity
        )
        expect_false(before_gate$eligible)
        expect_identical(
          before_gate$code, "formal_export_detector_output_changed"
        )

        rv$datasets <- stats::setNames(list(tampered), dataset_id)
        rv$current_id <- dataset_id
        rv$ui_run_identity_by_dataset <- stats::setNames(
          list(run_identity), dataset_id
        )
        rv$ui_pending_confirmation <- NULL
        session$setInputs(
          time_unit = "s",
          qc_isi_unit = "ms",
          artifact_isi_ms = 0.9,
          final_audit_trains = names(ds$trains)
        )
        session$flushReact()

        do.call(
          session$setInputs,
          stats::setNames(list(as.integer(i)), case$request)
        )
        session$flushReact()
        pending <- shiny::isolate(rv$ui_pending_confirmation)
        expect_identical(pending$confirm_id, case$confirm)
        do.call(
          session$setInputs,
          stats::setNames(list(as.integer(i)), case$confirm)
        )
        session$flushReact()

        after <- shiny::isolate(rv$datasets[[dataset_id]])
        stored_after <- shiny::isolate(
          rv$ui_run_identity_by_dataset[[dataset_id]]
        )
        expect_null(shiny::isolate(rv$ui_pending_confirmation))
        expect_identical(after, tampered)
        expect_identical(stored_after, run_identity)
        expect_identical(
          stpd_ui_detector_output_identity(
            after, selected_trains = run_identity$selected_trains
          )$combined_sha256,
          tampered_output$combined_sha256
        )
        after_state <- stpd_ui_detector_output_state(
          after,
          expected_identity = stored_after$detector_output_identity,
          selected_trains = stored_after$selected_trains
        )
        expect_false(after_state$current)
        expect_true(case$artifact %in% after_state$changed_artifacts)
        after_gate <- stpd_ui_formal_export_state(
          after,
          params = after$params_effective,
          dataset_id = dataset_id,
          run_identity = stored_after,
          expected_detector_output_identity =
            stored_after$detector_output_identity
        )
        expect_false(after_gate$eligible)
        expect_identical(
          after_gate$code, "formal_export_detector_output_changed"
        )
      }
    })
  ))
})

test_that("legacy promotion and revert never make a stale run export-ready", {
  dataset_id <- "export_scope_legacy_stays_stale"
  ds <- export_scope_adv_detected_dataset()
  run_identity <- export_scope_adv_run_identity(ds, dataset_id)

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- stats::setNames(list(ds), dataset_id)
      rv$current_id <- dataset_id
      rv$ui_run_identity_by_dataset <- stats::setNames(
        list(run_identity), dataset_id
      )
      session$setInputs(
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        possible_burst_promote_trains = names(ds$trains)
      )
      session$flushReact()

      session$setInputs(preview_possible_burst_promotion = 1L)
      session$flushReact()
      expect_gt(
        shiny::isolate(
          rv$possible_burst_promotion_preview$total_eligible_isi
        ),
        0L
      )
      session$setInputs(apply_possible_burst_promotion = 1L)
      session$flushReact()
      expect_identical(
        shiny::isolate(rv$ui_pending_confirmation$confirm_id),
        "confirm_apply_possible_burst_promotion"
      )
      session$setInputs(confirm_apply_possible_burst_promotion = 1L)
      session$flushReact()

      promoted <- shiny::isolate(rv$datasets[[dataset_id]])
      stored_promoted <- shiny::isolate(
        rv$ui_run_identity_by_dataset[[dataset_id]]
      )
      expect_identical(stored_promoted, run_identity)
      promoted_run_state <- stpd_ui_run_state(
        promoted,
        params = promoted$params_effective,
        dataset_id = dataset_id,
        run_identity = stored_promoted
      )
      expect_identical(
        promoted_run_state$code, "run_manual_context_changed"
      )
      promoted_gate <- stpd_ui_formal_export_state(
        promoted,
        params = promoted$params_effective,
        dataset_id = dataset_id,
        run_identity = stored_promoted,
        expected_detector_output_identity =
          stored_promoted$detector_output_identity
      )
      expect_false(promoted_gate$eligible)
      expect_identical(
        promoted_gate$code,
        "formal_export_run_manual_context_changed"
      )

      session$setInputs(revert_possible_burst_promotion = 1L)
      session$flushReact()
      expect_identical(
        shiny::isolate(rv$ui_pending_confirmation$confirm_id),
        "confirm_revert_possible_burst_promotion"
      )
      session$setInputs(confirm_revert_possible_burst_promotion = 1L)
      session$flushReact()

      after_revert_attempt <- shiny::isolate(rv$datasets[[dataset_id]])
      stored_after_revert <- shiny::isolate(
        rv$ui_run_identity_by_dataset[[dataset_id]]
      )
      expect_identical(stored_after_revert, run_identity)
      output_after_revert <- stpd_ui_detector_output_state(
        after_revert_attempt,
        expected_identity = run_identity$detector_output_identity,
        selected_trains = run_identity$selected_trains
      )
      expect_false(output_after_revert$current)
      expect_true(any(c(
        "possible_burst_promotion_audit",
        "possible_burst_promotion_summary"
      ) %in% output_after_revert$changed_artifacts))
      run_after_revert <- stpd_ui_run_state(
        after_revert_attempt,
        params = after_revert_attempt$params_effective,
        dataset_id = dataset_id,
        run_identity = stored_after_revert
      )
      expect_true(run_after_revert$code %in% c(
        "run_current", "run_manual_context_changed"
      ))
      gate_after_revert <- stpd_ui_formal_export_state(
        after_revert_attempt,
        params = after_revert_attempt$params_effective,
        dataset_id = dataset_id,
        run_identity = stored_after_revert,
        expected_detector_output_identity =
          stored_after_revert$detector_output_identity
      )
      expect_false(gate_after_revert$eligible)
      expect_true(gate_after_revert$code %in% c(
        "formal_export_run_manual_context_changed",
        "formal_export_detector_output_changed"
      ))
    })
  ))
})

test_that("legacy promotion and revert never erase pre-existing output tamper", {
  dataset_id <- "export_scope_tamper_before_legacy"
  base <- export_scope_adv_detected_dataset()
  base_identity <- export_scope_adv_run_identity(base, dataset_id)
  candidate_tampered <- export_scope_adv_mutate_result_table(
    base, "candidate_ledger"
  )

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- stats::setNames(list(candidate_tampered), dataset_id)
      rv$current_id <- dataset_id
      rv$ui_run_identity_by_dataset <- stats::setNames(
        list(base_identity), dataset_id
      )
      session$setInputs(
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        possible_burst_promote_trains = names(base$trains)
      )
      session$flushReact()

      session$setInputs(preview_possible_burst_promotion = 1L)
      session$flushReact()
      expect_gt(
        shiny::isolate(
          rv$possible_burst_promotion_preview$total_eligible_isi
        ),
        0L
      )
      session$setInputs(apply_possible_burst_promotion = 1L)
      session$flushReact()
      expect_identical(
        shiny::isolate(rv$ui_pending_confirmation$confirm_id),
        "confirm_apply_possible_burst_promotion"
      )
      session$setInputs(confirm_apply_possible_burst_promotion = 1L)
      session$flushReact()

      after <- shiny::isolate(rv$datasets[[dataset_id]])
      stored_after <- shiny::isolate(
        rv$ui_run_identity_by_dataset[[dataset_id]]
      )
      expect_identical(stored_after, base_identity)
      state <- stpd_ui_detector_output_state(
        after,
        expected_identity = stored_after$detector_output_identity,
        selected_trains = stored_after$selected_trains
      )
      expect_false(state$current)
      expect_true("candidate_ledger" %in% state$changed_artifacts)
      gate <- stpd_ui_formal_export_state(
        after,
        params = after$params_effective,
        dataset_id = dataset_id,
        run_identity = stored_after,
        expected_detector_output_identity =
          stored_after$detector_output_identity
      )
      expect_false(gate$eligible)
    })
  ))

  promoted <- stpd_promote_possible_burst(
    base,
    selected_trains = names(base$trains),
    overwrite_manual = FALSE,
    reason = "export_scope_revert_fixture"
  )$dataset
  rerun <- stpd_detect(
    promoted,
    params = promoted$params_effective,
    selected_trains = names(promoted$trains),
    lock_manual = TRUE,
    collect_diagnostics = TRUE
  )
  rerun_identity <- export_scope_adv_run_identity(rerun, dataset_id)
  expect_identical(
    stpd_ui_formal_export_state(
      rerun,
      params = rerun$params_effective,
      dataset_id = dataset_id,
      run_identity = rerun_identity,
      expected_detector_output_identity =
        rerun_identity$detector_output_identity
    )$code,
    "formal_export_ready"
  )
  event_tampered <- export_scope_adv_mutate_result_table(
    rerun, "event_ledger"
  )

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- stats::setNames(list(event_tampered), dataset_id)
      rv$current_id <- dataset_id
      rv$ui_run_identity_by_dataset <- stats::setNames(
        list(rerun_identity), dataset_id
      )
      session$setInputs(
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        possible_burst_promote_trains = names(rerun$trains)
      )
      session$flushReact()

      session$setInputs(revert_possible_burst_promotion = 1L)
      session$flushReact()
      expect_identical(
        shiny::isolate(rv$ui_pending_confirmation$confirm_id),
        "confirm_revert_possible_burst_promotion"
      )
      session$setInputs(confirm_revert_possible_burst_promotion = 1L)
      session$flushReact()

      after <- shiny::isolate(rv$datasets[[dataset_id]])
      stored_after <- shiny::isolate(
        rv$ui_run_identity_by_dataset[[dataset_id]]
      )
      expect_identical(stored_after, rerun_identity)
      state <- stpd_ui_detector_output_state(
        after,
        expected_identity = stored_after$detector_output_identity,
        selected_trains = stored_after$selected_trains
      )
      expect_false(state$current)
      expect_true("event_ledger" %in% state$changed_artifacts)
      gate <- stpd_ui_formal_export_state(
        after,
        params = after$params_effective,
        dataset_id = dataset_id,
        run_identity = stored_after,
        expected_detector_output_identity =
          stored_after$detector_output_identity
      )
      expect_false(gate$eligible)
    })
  ))
})

test_that("partial-run AUTO table and jump cannot reach out-of-scope history", {
  ds <- export_scope_adv_partial_dataset()
  dataset_id <- "export_scope_partial"
  dataset_identity <- stpd_ui_dataset_identity(ds, dataset_id)
  run_identity <- stpd_ui_run_identity(
    ds,
    params = ds$params_effective,
    dataset_id = dataset_id,
    selected_trains = "train_a",
    dataset_identity = dataset_identity
  )
  expect_identical(
    stpd_ui_run_state(
      ds, ds$params_effective, dataset_id, run_identity, dataset_identity
    )$code,
    "run_partial_current"
  )

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- stats::setNames(list(ds), dataset_id)
      rv$current_id <- dataset_id
      rv$ui_run_identity_by_dataset <- stats::setNames(
        list(run_identity), dataset_id
      )
      session$setInputs(
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        train_display_mode = "paged_all",
        visible_trains_per_page = 10L,
        train_page = 1L,
        use_train_metadata_filter = FALSE,
        events_detail_mode = "compact",
        events_jump_target = "automatic"
      )
      session$flushReact()

      current <- orientation_events_current()
      expect_identical(current$source, "auto")
      expect_gt(nrow(current$events), 0L)
      expect_setequal(as.character(current$events$train), "train_a")
      expect_false(any(as.character(current$events$train) == "train_b"))

      all_events <- derive_interval_tables(
        ds$trains,
        source = "auto",
        auto_others = FALSE,
        dataset_map = c(train_a = dataset_id, train_b = dataset_id),
        min_isi_sec = ds$params_effective$detector$min_valid_isi_sec,
        contrast_q = ds$params_effective$burst$contrast_q %||% 0.90,
        context_k = ds$params_effective$burst$context_k %||% 5L
      )$events
      all_display <- stpd_orientation_event_display_model(
        all_events,
        source = "auto",
        dataset_id = dataset_id,
        time_unit = "s",
        mode = "compact"
      )
      excluded_key <- all_display$event_row_key[
        match("train_b", as.character(all_display$train))
      ]
      expect_true(length(excluded_key) == 1L && nzchar(excluded_key))
      expect_false(excluded_key %in% current$display$event_row_key)

      before_focus <- shiny::isolate(rv$event_table_focus)
      session$setInputs(
        events_table_row_key = list(key = excluded_key, nonce = 991L)
      )
      session$flushReact()
      expect_identical(shiny::isolate(rv$event_table_focus), before_focus)
    })
  ))
})

test_that("generated export progress is superseded by run, params, or scope changes", {
  ds <- export_scope_adv_detected_dataset()
  dataset_id <- "export_scope_adv"
  run_identity <- export_scope_adv_run_identity(ds, dataset_id)

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- stats::setNames(list(ds), dataset_id)
      rv$current_id <- dataset_id
      rv$ui_run_identity_by_dataset <- stats::setNames(
        list(run_identity), dataset_id
      )
      session$setInputs(
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9
      )
      session$flushReact()

      base_progress <- list(
        export_id = "export_scope_progress",
        kind = "results_zip",
        dataset_id = dataset_id,
        dataset_name = dataset_id,
        run_id = run_identity$run_id,
        params_sha256 = run_identity$effective_params_sha256,
        selected_train_n = run_identity$selected_train_n,
        total_train_n = run_identity$total_train_n,
        selected_trains = run_identity$selected_trains,
        scope = "2/2",
        state = "generated",
        phase = "verify",
        progress = 1,
        detail = "Generated on the server."
      )

      mismatches <- list(
        run = within(base_progress, run_id <- "superseded_run"),
        params = within(
          base_progress,
          params_sha256 <- paste(rep("f", 64L), collapse = "")
        ),
        scope = within(base_progress, {
          selected_train_n <- 1L
          selected_trains <- "train_a"
          scope <- "1/2"
        })
      )
      for (progress in mismatches) {
        rv$formal_export_progress <- progress
        session$flushReact()
        html <- export_scope_adv_html(output$formal_export_progress_status)
        expect_match(html, "is-superseded", fixed = TRUE)
        expect_false(grepl("is-success", html, fixed = TRUE))
      }
    })
  ))
})

test_that("stale scientific validation is explicitly omitted from formal export", {
  ds <- export_scope_adv_detected_dataset()
  dataset_id <- "export_scope_adv"
  dataset_identity <- stpd_ui_dataset_identity(ds, dataset_id)
  run_identity <- export_scope_adv_run_identity(ds, dataset_id)
  validation <- list(
    metadata = data.frame(
      validation_run_id = "export_scope_validation",
      stringsAsFactors = FALSE
    ),
    metrics = data.frame(metric = "fixture", value = 1)
  )
  validation_identity <- stpd_ui_validation_identity(
    validation,
    dataset_identity = dataset_identity,
    run_identity = run_identity,
    selected_trains = names(ds$trains),
    truth_dependencies = character(),
    source_kind = "shadow_independent",
    source_params_sha256 = run_identity$effective_params_sha256,
    dataset = ds
  )
  expect_identical(
    stpd_ui_validation_state(
      ds,
      params = ds$params_effective,
      dataset_id = dataset_id,
      run_identity = run_identity,
      validation_identity = validation_identity,
      dataset_identity = dataset_identity
    )$code,
    "validation_current"
  )

  stale_identity <- validation_identity
  stale_identity$source_params_sha256 <- paste(rep("e", 64L), collapse = "")
  expect_identical(
    stpd_ui_validation_state(
      ds,
      params = ds$params_effective,
      dataset_id = dataset_id,
      run_identity = run_identity,
      validation_identity = stale_identity,
      dataset_identity = dataset_identity
    )$code,
    "validation_run_changed"
  )

  decision <- stpd_ui_validation_export_decision(
    ds,
    params = ds$params_effective,
    dataset_id = dataset_id,
    run_identity = run_identity,
    validation = validation,
    validation_identity = stale_identity,
    dataset_identity = dataset_identity,
    stale_policy = "omit"
  )
  expect_identical(decision$action, "omit")
  expect_false(decision$include)
  expect_match(decision$code, "validation.*(stale|changed|omit)")
  expect_true(length(decision$reason) > 0L || length(decision$state) > 0L)
})

test_that("real formal ZIP omits stale or changed scientific validation", {
  attached <- export_scope_adv_attach_stale_validation(
    export_scope_adv_detected_dataset(), "export_scope_adv"
  )
  ds <- attached$dataset
  run_identity <- attached$run_identity
  dataset_identity <- attached$dataset_identity

  env <- new.env(parent = environment(stpd_server_install_export_module))
  env$output <- new.env(parent = emptyenv())
  env$rv <- shiny::reactiveValues(
    current_id = "export_scope_adv",
    formal_export_sequence = 0L,
    formal_export_progress = NULL
  )
  env$session <- shiny::MockShinySession$new()
  env$input <- list(time_unit = "s")
  env$current_dataset <- function() ds
  env$formal_results_export_context <- function(dataset) list(
    params = dataset$params_effective,
    state = list(
      eligible = TRUE,
      run_id = run_identity$run_id,
      params_sha256 = run_identity$effective_params_sha256,
      selected_train_n = run_identity$selected_train_n,
      total_train_n = run_identity$total_train_n,
      selected_trains = run_identity$selected_trains
    ),
    run_identity = run_identity,
    dataset_identity = dataset_identity
  )
  env$normalize_dataset <- function(dataset) dataset
  env$min_valid_isi_sec <- function() {
    ds$params_effective$detector$min_valid_isi_sec
  }
  env$qc_isi_unit <- function() "ms"
  env$refractory_suspect_sec <- function() 0.001
  env$showNotification <- function(...) invisible(NULL)
  stpd_server_install_export_module(env)
  # The installer owns this symbol and therefore replaces any fixture supplied
  # before installation.  Override it afterwards so this test exercises the
  # real ZIP writer while keeping its formal-gate context deterministic.
  env$formal_results_export_context <- function(dataset) list(
    params = dataset$params_effective,
    state = list(
      eligible = TRUE,
      run_id = run_identity$run_id,
      params_sha256 = run_identity$effective_params_sha256,
      selected_train_n = run_identity$selected_train_n,
      total_train_n = run_identity$total_train_n,
      selected_trains = run_identity$selected_trains
    ),
    run_identity = run_identity,
    dataset_identity = dataset_identity
  )

  zip_file <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_file), add = TRUE)
  shiny::withReactiveDomain(
    env$session,
    export_scope_adv_download_content(
      env$output$download_results_zip
    )(zip_file)
  )
  expect_true(file.exists(zip_file))
  expect_gt(file.info(zip_file)$size, 0)

  members <- export_scope_adv_zip_members(zip_file)
  expect_length(export_scope_adv_detailed_validation_members(members), 0L)
  expect_true("Validation_export_status.csv" %in% members)
  required <- c(
    "Detector_run_metadata.csv", "Export_run_metadata.csv",
    "ISI_labels.csv", "Validation_export_status.csv"
  )
  checked <- stpd_ui_validate_zip_archive(
    zip_file, required_files = required, sentinel_files = required
  )
  expect_true(checked$valid)
  expect_true(checked$sentinel_verified)

  extracted <- export_scope_adv_extract_zip(zip_file)
  on.exit(unlink(extracted, recursive = TRUE, force = TRUE), add = TRUE)
  status <- utils::read.csv(
    file.path(extracted, "Validation_export_status.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_identical(as.character(status$action), "omit")
  expect_match(as.character(status$code), "validation_export_omitted_")

  # A report whose attached identity is otherwise current must also fail
  # closed if one metric cell changes after that identity was captured.
  ds <- export_scope_adv_detected_dataset()
  run_identity <- export_scope_adv_run_identity(ds, "export_scope_adv")
  dataset_identity <- stpd_ui_dataset_identity(ds, "export_scope_adv")
  report <- list(
    meta = data.frame(
      validation_run_id = "export_scope_report_tamper",
      stringsAsFactors = FALSE
    ),
    split = data.frame(
      train = names(ds$trains), split = "validation",
      stringsAsFactors = FALSE
    ),
    calibration_metrics = data.frame(metric = "fixture", value = 1),
    validation_metrics = data.frame(metric = "fixture", value = 1),
    overfit_report = data.frame(item = "fixture", value = "ok")
  )
  report_identity <- stpd_ui_validation_identity(
    report,
    dataset_identity = dataset_identity,
    run_identity = run_identity,
    selected_trains = names(ds$trains),
    truth_dependencies = character(),
    source_kind = "shadow_independent",
    source_params_sha256 = run_identity$effective_params_sha256,
    dataset = ds
  )
  report[["ui_identity"]] <- report_identity
  report$validation_metrics$value[[1L]] <- 2
  ds$results$scientific_validation <- report
  run_identity <- export_scope_adv_run_identity(ds, "export_scope_adv")
  dataset_identity <- stpd_ui_dataset_identity(ds, "export_scope_adv")
  report_decision <- stpd_ui_validation_export_decision(
    ds = ds,
    validation = report,
    params = ds$params_effective,
    dataset_id = "export_scope_adv",
    run_identity = run_identity,
    validation_identity = report[["ui_identity"]],
    dataset_identity = dataset_identity,
    stale_policy = "omit"
  )
  expect_identical(report_decision$action, "omit")
  expect_identical(
    report_decision$code, "validation_export_report_changed"
  )

  report_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(report_zip), add = TRUE)
  shiny::withReactiveDomain(
    env$session,
    export_scope_adv_download_content(
      env$output$download_results_zip
    )(report_zip)
  )
  expect_true(file.exists(report_zip))
  report_members <- export_scope_adv_zip_members(report_zip)
  expect_length(
    export_scope_adv_detailed_validation_members(report_members), 0L
  )
  report_extracted <- export_scope_adv_extract_zip(report_zip)
  on.exit(
    unlink(report_extracted, recursive = TRUE, force = TRUE), add = TRUE
  )
  report_status <- utils::read.csv(
    file.path(report_extracted, "Validation_export_status.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_identical(as.character(report_status$action), "omit")
  expect_identical(
    as.character(report_status$code),
    "validation_export_report_changed"
  )
})

test_that("batch export plan blocks unrun, parameter-mismatched, and mixed sets", {
  good <- export_scope_adv_detected_dataset()
  good$params_last <- good$params_effective
  unrun <- export_scope_adv_raw_dataset("export_scope_unrun")
  unrun$params_last <- default_params()
  unrun$params_est <- default_params()

  good_run <- export_scope_adv_run_identity(good, "good")
  unrun_run <- stpd_ui_run_identity(
    unrun,
    params = unrun$params_last,
    dataset_id = "unrun",
    selected_trains = names(unrun$trains)
  )
  good_output <- stpd_ui_detector_output_identity(
    good, selected_trains = good_run$selected_trains
  )
  unrun_output <- stpd_ui_detector_output_identity(
    unrun, selected_trains = unrun_run$selected_trains
  )
  unrun_plan <- stpd_ui_batch_export_plan(
    datasets = list(unrun = unrun),
    params_by_dataset = list(unrun = unrun$params_last),
    run_identities = list(unrun = unrun_run),
    expected_detector_outputs = list(unrun = unrun_output),
    dataset_ids = "unrun"
  )
  expect_false(unrun_plan$eligible)
  expect_true("unrun" %in% export_scope_adv_failure_ids(unrun_plan))

  changed_params <- good$params_effective
  changed_params$spiketrainpattern$qc$artifact_min_valid_isi_sec <-
    changed_params$spiketrainpattern$qc$artifact_min_valid_isi_sec + 0.0001
  mismatch_plan <- stpd_ui_batch_export_plan(
    datasets = list(good = good),
    params_by_dataset = list(good = changed_params),
    run_identities = list(good = good_run),
    expected_detector_outputs = list(good = good_output),
    dataset_ids = "good"
  )
  expect_false(mismatch_plan$eligible)
  expect_true("good" %in% export_scope_adv_failure_ids(mismatch_plan))
  expect_identical(
    mismatch_plan$failures$code[mismatch_plan$failures$dataset_id == "good"],
    "batch_export_params_effective_mismatch"
  )

  mixed_plan <- stpd_ui_batch_export_plan(
    datasets = list(good = good, unrun = unrun),
    params_by_dataset = list(
      good = good$params_effective,
      unrun = unrun$params_last
    ),
    run_identities = list(good = good_run, unrun = unrun_run),
    expected_detector_outputs = list(
      good = good_output,
      unrun = unrun_output
    ),
    dataset_ids = c("good", "unrun")
  )
  expect_false(mixed_plan$eligible)
  expect_true("unrun" %in% export_scope_adv_failure_ids(mixed_plan))
  expect_true("good" %in% names(mixed_plan$eligible_entries))

  entries <- mixed_plan$entries %||% data.frame()
  if (is.data.frame(entries) && nrow(entries) > 0L &&
      all(c("dataset_id", "eligible") %in% names(entries))) {
    expect_false(any(entries$dataset_id == "unrun" & entries$eligible))
  }
})

test_that("batch download handler cannot bypass an ineligible plan", {
  unrun <- export_scope_adv_raw_dataset("export_scope_unrun_handler")
  unrun$params_last <- default_params()
  unrun$params_est <- default_params()
  unrun_run <- stpd_ui_run_identity(
    unrun,
    params = unrun$params_last,
    dataset_id = "unrun",
    selected_trains = names(unrun$trains)
  )

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- list(unrun = unrun)
      rv$current_id <- "unrun"
      rv$ui_run_identity_by_dataset <- list(unrun = unrun_run)
      session$setInputs(
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9
      )
      session$flushReact()

      expect_error(
        output$download_batch_results_zip,
        regexp = "(?i)batch[_ ]export.*(gate|blocked|failed)"
      )
    })
  ))
})

test_that("run-all stores private effective params and real batch ZIP succeeds", {
  dataset_id <- "run_all_private_params"
  ds <- export_scope_adv_raw_dataset(dataset_id)
  ds$params_last <- default_params()
  ds$params_est <- default_params()
  ui_params <- default_params()
  ui_params$spiketrainpattern$qc$artifact_min_valid_isi_sec <-
    ui_params$spiketrainpattern$qc$artifact_min_valid_isi_sec + 0.0001
  expected_effective <- effective_params_for_detector(ui_params)

  suppressMessages(suppressWarnings(
    shiny::testServer(SpikeTrainPatternDetector:::server, {
      rv$datasets <- stats::setNames(list(ds), dataset_id)
      rv$current_id <- dataset_id
      assign(
        "read_params_from_ui", function() ui_params,
        envir = environment(run_detector_from_ui)
      )
      session$setInputs(
        ui_language = "en",
        time_unit = "s",
        qc_isi_unit = "ms",
        artifact_isi_ms = 0.9,
        run_all_datasets = 1L
      )
      session$flushReact()

      detected <- rv$datasets[[dataset_id]]
      stored_run <- rv$ui_run_identity_by_dataset[[dataset_id]]
      expect_true(is.list(stored_run) && isTRUE(stored_run$has_run))
      expect_true(stored_run$detector_output_identity$verifiable)
      expect_identical(
        detected$params_effective$spiketrainpattern$qc$
          artifact_min_valid_isi_sec,
        expected_effective$spiketrainpattern$qc$
          artifact_min_valid_isi_sec
      )
      expect_identical(
        detected$params_effective$detector$min_valid_isi_sec,
        expected_effective$detector$min_valid_isi_sec
      )
      expect_identical(
        stored_run$effective_params_sha256,
        stpd_ui_state_params_sha256(detected$params_effective)
      )
      expect_match(rv$batch_status, "Last batch run:", fixed = TRUE)

      attached <- export_scope_adv_attach_stale_validation(
        detected, dataset_id
      )
      detected$results$scientific_validation <- attached$report
      rv$datasets[[dataset_id]] <- detected
      session$flushReact()

      zip_file <- output$download_batch_results_zip
      on.exit(unlink(zip_file), add = TRUE)
      expect_true(file.exists(zip_file))
      expect_gt(file.info(zip_file)$size, 0)
      members <- export_scope_adv_zip_members(zip_file)
      expect_length(
        export_scope_adv_detailed_validation_members(members), 0L
      )
      expect_true("Batch_export_manifest.csv" %in% members)

      extracted <- export_scope_adv_extract_zip(zip_file)
      on.exit(unlink(extracted, recursive = TRUE, force = TRUE), add = TRUE)
      manifest <- utils::read.csv(
        file.path(extracted, "Batch_export_manifest.csv"),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      expect_identical(as.character(manifest$dataset_id), dataset_id)
      expect_identical(as.character(manifest$validation_action), "omit")
      expect_identical(
        as.character(manifest$params_sha256),
        stored_run$effective_params_sha256
      )
      directory_name <- as.character(manifest$directory_name[[1L]])
      status_path <- file.path(
        extracted, directory_name, "Validation_export_status.csv"
      )
      status <- utils::read.csv(
        status_path, stringsAsFactors = FALSE, check.names = FALSE
      )
      expect_identical(as.character(status$action), "omit")
      expect_match(as.character(status$code), "validation_export_omitted_")

      required <- c(
        "Batch_export_manifest.csv",
        file.path(directory_name, "Detector_run_metadata.csv"),
        file.path(directory_name, "Export_run_metadata.csv"),
        file.path(directory_name, "ISI_labels_final.csv"),
        file.path(directory_name, "Validation_export_status.csv")
      )
      checked <- stpd_ui_validate_zip_archive(
        zip_file, required_files = required, sentinel_files = required
      )
      expect_true(checked$valid)
      expect_true(checked$sentinel_verified)
    })
  ))
})

test_that("plain text with a zip suffix is rejected as an archive", {
  fake_zip <- tempfile(fileext = ".zip")
  writeLines("this is not a ZIP archive", fake_zip, useBytes = TRUE)

  checked <- stpd_ui_validate_zip_archive(
    fake_zip,
    required_files = "Detector_run_metadata.csv",
    sentinel_files = "Detector_run_metadata.csv"
  )
  expect_false(checked$valid)
  expect_true(checked$code %in% c("zip_invalid_archive", "zip_unreadable"))
  expect_true(nzchar(checked$reason))
})

test_that("ZIP verifier rejects missing and empty strengthened sentinels", {
  source_dir <- tempfile("export_scope_adv_zip_source_")
  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(source_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("run_id,params_hash", "fixture,hash"),
    file.path(source_dir, "Detector_run_metadata.csv"),
    useBytes = TRUE
  )
  file.create(file.path(source_dir, "Validation_export_status.csv"))

  archive <- tempfile(fileext = ".zip")
  on.exit(unlink(archive), add = TRUE)
  old <- setwd(source_dir)
  on.exit(setwd(old), add = TRUE)
  utils::zip(
    zipfile = archive,
    files = c(
      "Detector_run_metadata.csv", "Validation_export_status.csv"
    )
  )
  setwd(old)

  missing <- stpd_ui_validate_zip_archive(
    archive,
    required_files = c(
      "Detector_run_metadata.csv", "Export_run_metadata.csv"
    ),
    sentinel_files = c(
      "Detector_run_metadata.csv", "Export_run_metadata.csv"
    )
  )
  expect_false(missing$valid)
  expect_identical(missing$code, "zip_required_file_missing")
  expect_identical(missing$missing_files, "Export_run_metadata.csv")

  empty <- stpd_ui_validate_zip_archive(
    archive,
    required_files = c(
      "Detector_run_metadata.csv", "Validation_export_status.csv"
    ),
    sentinel_files = "Validation_export_status.csv"
  )
  expect_false(empty$valid)
  expect_identical(empty$code, "zip_sentinel_empty")
  expect_identical(
    empty$empty_sentinels, "Validation_export_status.csv"
  )
  expect_false(empty$sentinel_verified)
})
