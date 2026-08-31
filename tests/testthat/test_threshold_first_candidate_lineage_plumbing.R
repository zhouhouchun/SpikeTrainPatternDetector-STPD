plumbing_scientific_projection <- function(result) {
  out <- stpd_candidate_lineage_strip(result)
  if (is.data.frame(out$results$run_metadata) &&
      "run_time" %in% names(out$results$run_metadata)) {
    out$results$run_metadata$run_time[] <- "<runtime>"
  }
  if (is.data.frame(out$results$run_metadata_public) &&
      "run_started_at_utc" %in% names(out$results$run_metadata_public)) {
    out$results$run_metadata_public$run_started_at_utc[] <- "<runtime>"
  }
  out
}

plumbing_core_detection_projection <- function(result) {
  strip_runtime <- function(x) {
    if (!is.data.frame(x)) return(x)
    x[intersect(c("run_id", "audit_time"), names(x))] <- NULL
    rownames(x) <- NULL
    x
  }
  trains <- lapply(result$trains, function(train) {
    keep <- intersect(
      c("idx", "timestamp_sec", "ISI_sec", "pattern_auto", "auto_score"),
      names(train)
    )
    out <- train[, keep, drop = FALSE]
    rownames(out) <- NULL
    out
  })
  result_names <- c(
    "threshold_table", "events", "candidate_ledger", "candidate_features",
    "final_decisions"
  )
  results <- lapply(result_names, function(name) {
    strip_runtime(result$results[[name]])
  })
  names(results) <- result_names
  list(trains = trains, results = results)
}

plumbing_live_runs <- local({
  cache <- new.env(parent = emptyenv())
  dataset <- stpd_golden_test_dataset("middle_burst")

  function(audit_level, collect_diagnostics) {
    key <- paste(audit_level, collect_diagnostics, sep = "__")
    if (!exists(key, envir = cache, inherits = FALSE)) {
      set.seed(73109)
      seed_before <- .Random.seed
      kind_before <- RNGkind()
      options_before <- options()
      warnings <- character()
      result <- testthat::with_mocked_bindings(
        withCallingHandlers(
          stpd_detect(
            dataset,
            default_params(),
            selected_trains = "train_1",
            collect_diagnostics = collect_diagnostics,
            label_blind = TRUE,
            audit_level = audit_level
          ),
          warning = function(warning) {
            warnings <<- c(warnings, conditionMessage(warning))
            invokeRestart("muffleWarning")
          }
        ),
        stpd_new_run_id = function(...) "stpd_run_lineage_equivalence",
        .package = "SpikeTrainPatternDetector"
      )
      value <- list(
        result = result,
        seed_unchanged = identical(.Random.seed, seed_before),
        kind_unchanged = identical(RNGkind(), kind_before),
        options_unchanged = identical(options(), options_before),
        seed_after = .Random.seed,
        kind_after = RNGkind(),
        warnings = warnings
      )
      assign(key, value, envir = cache)
    }
    unserialize(serialize(get(key, envir = cache, inherits = FALSE), NULL))
  }
})

test_that("live audit levels are additive and orthogonal to legacy diagnostics", {
  grid <- expand.grid(
    audit_level = c("off", "summary", "full"),
    collect_diagnostics = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  runs <- lapply(seq_len(nrow(grid)), function(i) {
    plumbing_live_runs(grid$audit_level[[i]], grid$collect_diagnostics[[i]])
  })

  projections <- lapply(runs, function(run) {
    plumbing_scientific_projection(run$result)
  })
  for (diagnostics in c(FALSE, TRUE)) {
    indices <- which(grid$collect_diagnostics == diagnostics)
    reference <- projections[[indices[[1L]]]]
    expect_true(all(vapply(
      projections[indices[-1L]], identical, logical(1), reference
    )))
    reference_bytes <- serialize(reference, NULL, version = 3L)
    expect_true(all(vapply(
      projections[indices[-1L]],
      function(value) identical(
        serialize(value, NULL, version = 3L), reference_bytes
      ),
      logical(1)
    )))
    reference_seed <- runs[[indices[[1L]]]]$seed_after
    reference_kind <- runs[[indices[[1L]]]]$kind_after
    expect_true(all(vapply(
      runs[indices[-1L]],
      function(run) identical(run$seed_after, reference_seed), logical(1)
    )))
    expect_true(all(vapply(
      runs[indices[-1L]],
      function(run) identical(run$kind_after, reference_kind), logical(1)
    )))
  }
  expect_true(all(vapply(runs, `[[`, logical(1), "seed_unchanged")))
  expect_true(all(vapply(runs, `[[`, logical(1), "kind_unchanged")))
  expect_true(all(vapply(runs, `[[`, logical(1), "options_unchanged")))
  expect_true(all(vapply(runs, function(run) !length(run$warnings), logical(1))))

  params_hashes <- vapply(runs, function(run) {
    as.character(run$result$results$run_metadata_public$params_hash[[1L]])
  }, character(1))
  expect_length(unique(params_hashes), 1L)
  expect_true(all(vapply(runs, function(run) {
    identical(
      stpd_params_hash(run$result$params_effective),
      as.character(run$result$results$run_metadata_public$params_hash[[1L]])
    )
  }, logical(1))))
  expect_identical(
    vapply(runs, function(run) {
      isTRUE(run$result$results$run_metadata_public$diagnostics_collected[[1L]])
    }, logical(1)),
    grid$collect_diagnostics
  )

  for (i in seq_len(nrow(grid))) {
    result <- runs[[i]]$result
    if (identical(grid$audit_level[[i]], "off")) {
      expect_false("candidate_lineage_audit" %in% names(result))
      next
    }
    expect_true("candidate_lineage_audit" %in% names(result))
    expect_false("candidate_lineage_audit" %in% names(result$results))
    expect_false(any(vapply(result$trains, function(train) {
      !is.null(attr(train, "candidate_lineage_audit", exact = TRUE))
    }, logical(1))))
    envelope <- result$candidate_lineage_audit
    expect_silent(stpd_candidate_lineage_validate_envelope(envelope))
    expect_identical(envelope$metadata$audit_level, "off")
    expect_identical(
      envelope$metadata$evidence_authority, "diagnostic_unavailable"
    )
    expect_identical(
      envelope$metadata$unavailable_reason, "adapter_mapping_unavailable"
    )
    expect_false(envelope$metadata$universe_complete)
    expect_true(all(vapply(envelope$bundle, nrow, integer(1)) == 0L))
    receipt <- attr(
      envelope, "candidate_lineage_request_receipt", exact = TRUE
    )
    expect_identical(
      receipt$requested_audit_level, grid$audit_level[[i]]
    )
    expect_identical(receipt$collector_state, "sealed")
    expect_identical(
      receipt$instrumentation_status, "direct_complete"
    )
    expect_identical(
      receipt$params_hash,
      as.character(result$results$run_metadata_public$params_hash[[1L]])
    )
    expect_identical(
      receipt$run_id,
      as.character(result$results$run_metadata_public$run_id[[1L]])
    )
    expect_true(all(receipt$train_receipts$train_state == "completed"))
    expect_true(all(
      receipt$train_receipts$instrumentation_status == "direct_complete"
    ))
    expect_true(stpd_candidate_lineage_live_validate_result(
      result, receipt$run_id, receipt$params_hash, receipt$dataset_id,
      receipt$target_trains
    ))
    scientific <- stpd_candidate_lineage_scientific_projection(result)
    expect_false("candidate_lineage_audit" %in% names(scientific))
    expect_identical(scientific, stpd_candidate_lineage_strip(result))
    expect_identical(
      stpd_candidate_lineage_scientific_projection_hash(result),
      stpd_candidate_lineage_scientific_projection_hash(scientific)
    )
  }
})

test_that("major public wrappers expose and forward audit_level", {
  public_wrappers <- c(
    "run_detector", "run_detector_dataset", "stpd_detect_dataset_core",
    "stpd_run_detector"
  )
  expect_true(all(vapply(public_wrappers, function(name) {
    "audit_level" %in% names(formals(get(name, mode = "function")))
  }, logical(1))))

  calls <- list()
  testthat::local_mocked_bindings(
    stpd_detect = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      structure(list(ok = TRUE), class = "plumbing_forward_probe")
    },
    .package = "SpikeTrainPatternDetector"
  )
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  expect_s3_class(
    run_detector(ds, params, audit_level = "summary"),
    "plumbing_forward_probe"
  )
  expect_s3_class(
    run_detector_dataset(ds, params, audit_level = "full"),
    "plumbing_forward_probe"
  )
  expect_s3_class(
    stpd_detect_dataset_core(ds, params, audit_level = "summary"),
    "plumbing_forward_probe"
  )
  expect_s3_class(
    stpd_run_detector(ds, params, audit_level = "full"),
    "plumbing_forward_probe"
  )
  expect_identical(
    vapply(calls, function(call) call$audit_level, character(1)),
    c("summary", "full", "summary", "full")
  )

  calls <- list()
  expect_s3_class(run_detector(ds, params), "plumbing_forward_probe")
  expect_false("audit_level" %in% names(calls[[1L]]))
})

test_that("ledger-only candidate APIs do not accept a lossy audit request", {
  ledger_only <- c(
    "generate_candidates", "stpd_generate_candidates",
    "stpd_generate_candidate_events"
  )
  expect_true(all(vapply(ledger_only, function(name) {
    !("audit_level" %in% names(formals(get(name, mode = "function"))))
  }, logical(1))))
})

test_that("single-train APIs reject dormant threshold-first run config", {
  train <- stpd_golden_test_dataset("middle_burst")$trains$train_1
  configured <- default_params()
  configured$threshold_first <- stpd_threshold_first_run_config(
    "legacy", "ordered_fallback", "lock", "summary"
  )

  for (entry in c(
    "run_detector_train", "stpd_detect_train_core",
    "stpd_detect_train_dispatch", "stpd_detect_train_event_grammar"
  )) {
    set.seed(9970)
    seed_before <- .Random.seed
    error <- tryCatch(
      get(entry, mode = "function")(train, configured, train = "train_1"),
      error = identity
    )
    expect_s3_class(
      error, "stpd_candidate_lineage_run_config_not_activated"
    )
    expect_identical(.Random.seed, seed_before)
  }
})

test_that("a bare train list may contain a train named candidate_lineage_audit", {
  original <- stpd_golden_test_dataset("middle_burst")$trains$train_1
  bare <- list(candidate_lineage_audit = original)
  result <- run_detector_dataset_internal(
    bare, default_params(), selected_trains = "candidate_lineage_audit",
    collect_diagnostics = FALSE, audit_level = "off"
  )
  expect_true("candidate_lineage_audit" %in% names(result$trains))
  expect_identical(
    nrow(result$trains$candidate_lineage_audit), nrow(original)
  )
})

test_that("detector reruns strip stale top-level lineage audit", {
  stale <- plumbing_live_runs("summary", FALSE)$result
  expect_true("candidate_lineage_audit" %in% names(stale))

  rerun <- stpd_detect(
    stale,
    default_params(),
    selected_trains = "train_1",
    collect_diagnostics = FALSE,
    label_blind = TRUE,
    audit_level = "off"
  )
  expect_false("candidate_lineage_audit" %in% names(rerun))
  expect_identical(
    plumbing_core_detection_projection(rerun),
    plumbing_core_detection_projection(stale)
  )
})

test_that("invalid requests and dormant run config fail closed before RNG use", {
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  invalid <- list("FULL", NA_character_, c("off", "full"), TRUE)

  for (value in invalid) {
    set.seed(9971)
    seed_before <- .Random.seed
    error <- tryCatch(
      stpd_detect(ds, params, audit_level = value),
      error = identity
    )
    expect_s3_class(error, "stpd_threshold_first_error")
    expect_identical(.Random.seed, seed_before)
  }

  configured <- params
  configured$threshold_first <- stpd_threshold_first_run_config(
    "legacy", "ordered_fallback", "lock", "summary"
  )
  set.seed(9972)
  seed_before <- .Random.seed
  dormant_explicit <- tryCatch(
    stpd_detect(ds, configured, audit_level = "full"),
    error = identity
  )
  expect_s3_class(
    dormant_explicit, "stpd_candidate_lineage_run_config_not_activated"
  )
  expect_identical(.Random.seed, seed_before)

  set.seed(9973)
  seed_before <- .Random.seed
  dormant_implicit <- tryCatch(
    stpd_detect(ds, configured), error = identity
  )
  expect_s3_class(
    dormant_implicit, "stpd_candidate_lineage_run_config_not_activated"
  )
  expect_identical(.Random.seed, seed_before)
  expect_false("candidate_lineage_audit" %in% names(ds))
})
