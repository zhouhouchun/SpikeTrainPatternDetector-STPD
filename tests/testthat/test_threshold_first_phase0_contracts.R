threshold_first_error <- function(expr) {
  tryCatch(force(expr), error = function(error) error)
}

threshold_first_fixture <- function(unit = "s") {
  scale <- if (identical(unit, "ms")) 1000 else 1
  data.frame(
    idx = 101:105,
    timestamp_sec = c(0.02, 0, 0.01, 0.03, 0.05) * scale,
    ISI_sec = c(0.01, NA, 0.01, 0.01, 0.02) * scale,
    pattern_manual = c("burst", "", "pause", "", "tonic"),
    pattern_manual_negative = c("", "pause", "", "", ""),
    pattern_auto = c("burst", "", "", "pause", ""),
    auto_score = seq(0.1, 0.5, by = 0.1),
    stringsAsFactors = FALSE
  )
}

threshold_first_label_blind_provenance <- function() {
  stpd_threshold_first_input_provenance("label_blind")
}

threshold_first_normalize <- function(...) {
  stpd_threshold_first_normalize_train_view(
    ..., input_provenance = threshold_first_label_blind_provenance()
  )
}

threshold_first_params <- function(x = list(alpha = 1L)) {
  stpd_threshold_first_scientific_params(x)
}

test_that("orthogonal run config is explicit and fails closed", {
  expected <- list(
    schema_version = "stpd_threshold_first_run_config_v1",
    engine_algorithm = "legacy",
    threshold_source = "ordered_fallback",
    manual_policy = "lock",
    audit_level = "off"
  )
  expect_identical(stpd_threshold_first_run_config(), expected)

  hybrid <- stpd_threshold_first_run_config(
    "threshold_first_experimental", "train_adaptive", "ignore", "full"
  )
  expect_identical(hybrid$threshold_source, "train_adaptive")
  expect_identical(
    stpd_threshold_first_run_config(
      "threshold_first_experimental", "train_adaptive", "lock", "summary"
    )$manual_policy,
    "lock"
  )

  invalid_enum <- threshold_first_error(stpd_threshold_first_run_config(
    "future_engine", "ordered_fallback", "lock", "off"
  ))
  expect_s3_class(invalid_enum, "stpd_threshold_first_enum_invalid")

  invalid_combo <- threshold_first_error(stpd_threshold_first_run_config(
    "threshold_first_shadow", "ordered_fallback", "ignore", "summary"
  ))
  expect_s3_class(
    invalid_combo, "stpd_threshold_first_mode_combination_invalid"
  )

  unknown <- expected
  unknown$unexpected <- TRUE
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_run_config(unknown)),
    "stpd_threshold_first_unknown_field"
  )
  duplicated <- expected
  names(duplicated)[[2L]] <- "schema_version"
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_run_config(duplicated)),
    "stpd_threshold_first_run_config_invalid"
  )
})

test_that("legacy migration is explicit, idempotent, and rejects unknown modes", {
  old <- list(
    event_grammar = list(threshold_source_mode = "auto_priority")
  )
  migrated <- stpd_threshold_first_migrate_legacy_config(
    old, label_blind = TRUE, lock_manual = TRUE, collect_diagnostics = TRUE
  )
  expect_identical(migrated$config$engine_algorithm, "legacy")
  expect_identical(migrated$config$threshold_source, "ordered_fallback")
  expect_identical(migrated$config$manual_policy, "ignore")
  expect_identical(migrated$config$audit_level, "full")
  expect_identical(
    migrated$migration$legacy_threshold_source_mode, "auto_priority"
  )
  expect_true(migrated$migration$changed)

  current <- stpd_threshold_first_migrate_legacy_config(
    list(threshold_first = migrated$config)
  )
  expect_identical(current$config, migrated$config)
  expect_false(current$migration$changed)
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_migrate_legacy_config(
      list(threshold_first = stpd_threshold_first_run_config(
        "threshold_first_shadow", "user", "lock", "off"
      )),
      label_blind = TRUE
    )),
    "stpd_threshold_first_label_blind_conflict"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_migrate_legacy_config(
      list(), label_blind = 1L
    )),
    "stpd_threshold_first_field_invalid"
  )

  partial <- list(engine_algorithm = "legacy")
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_migrate_legacy_config(partial)),
    "stpd_threshold_first_partial_new_config"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_migrate_legacy_config(
      list(event_grammar = list(threshold_source_mode = "guess"))
    )),
    "stpd_threshold_first_legacy_source_unknown"
  )
})

test_that("legacy migration rejects duplicate and partial-match keys", {
  duplicated_top <- list(
    list(threshold_source_mode = "auto"),
    list(threshold_source_mode = "manual")
  )
  names(duplicated_top) <- c("event_grammar", "event_grammar")
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_migrate_legacy_config(
      duplicated_top
    )),
    "stpd_threshold_first_legacy_config_invalid"
  )

  duplicated_nested <- list("auto", "manual")
  names(duplicated_nested) <- c(
    "threshold_source_mode", "threshold_source_mode"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_migrate_legacy_config(
      list(event_grammar = duplicated_nested)
    )),
    "stpd_threshold_first_legacy_config_invalid"
  )

  partial_cases <- list(
    list(event_grammar_extra = list(threshold_source_mode = "manual")),
    list(event_gram = list(threshold_source_mode = "manual")),
    list(event_grammar = list(threshold_source_mode_extra = "manual")),
    list(event_grammar = list(threshold_source_mod = "manual")),
    list(spiketrainpattern_extra = list(
      engine = list(threshold_source_mode = "manual")
    )),
    list(spiketrainpat = list(
      engine = list(threshold_source_mode = "manual")
    )),
    list(spiketrainpattern = list(
      engine_extra = list(threshold_source_mode = "manual")
    )),
    list(spiketrainpattern = list(
      eng = list(threshold_source_mode = "manual")
    ))
  )
  for (candidate in partial_cases) {
    expect_s3_class(
      threshold_first_error(stpd_threshold_first_migrate_legacy_config(
        candidate
      )),
      "stpd_threshold_first_legacy_config_unknown_field"
    )
  }
})

test_that("all public enumerations reject abbreviations", {
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_input_provenance("label")),
    "stpd_threshold_first_enum_invalid"
  )
  fixture <- data.frame(timestamp_sec = c(0, 0.01, 0.02))
  expect_s3_class(
    threshold_first_error(threshold_first_normalize(
      fixture, "abbrev_unit", time_unit = "m"
    )),
    "stpd_threshold_first_enum_invalid"
  )
  expect_s3_class(
    threshold_first_error(threshold_first_normalize(
      fixture, "abbrev_duplicate", duplicate_policy = "collapse"
    )),
    "stpd_threshold_first_enum_invalid"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_empirical_tail_p(
      1, 1:4, "l"
    )),
    "stpd_threshold_first_enum_invalid"
  )

  view <- threshold_first_normalize(fixture, "provider_abbrev")
  sha <- function(letter) paste(rep(letter, 64L), collapse = "")
  provider_args <- list(
    provider_id = "provider", provider_version = "1",
    provider_state_sha256 = sha("a"), fold_manifest_sha256 = sha("b"),
    train_view_sha256 = stpd_threshold_first_train_view_hash(view),
    information_access = "manual_aware", performance_use = "not_eligible",
    group_separation_status = "not_required",
    calibration_group_ids = character(), evaluation_group_ids = "evaluation"
  )
  for (replacement in list(
      list(information_access = "label"),
      list(performance_use = "not"),
      list(group_separation_status = "not"))) {
    args <- utils::modifyList(provider_args, replacement)
    expect_s3_class(
      threshold_first_error(do.call(
        stpd_threshold_first_provider_identity, args
      )),
      "stpd_threshold_first_enum_invalid"
    )
  }

  mapping_args <- list(
    run_status = "complete", provider_decision = "positive",
    coverage_status = "complete", model_adequacy_status = "adequate",
    normalization_status = "normalized"
  )
  for (replacement in list(
      list(run_status = "comp"), list(provider_decision = "pos"),
      list(coverage_status = "part"),
      list(model_adequacy_status = "adeq"),
      list(normalization_status = "norm"))) {
    args <- utils::modifyList(mapping_args, replacement)
    expect_s3_class(
      threshold_first_error(do.call(
        stpd_threshold_first_map_provider_status, args
      )),
      "stpd_threshold_first_enum_invalid"
    )
  }
})

test_that("normalized view is rebuilt from a strict label-free allowlist", {
  dat <- threshold_first_fixture()
  view <- threshold_first_normalize(dat, "train_A")
  expect_true(stpd_threshold_first_validate_train_view(view))
  expect_equal(view$spikes$timestamp_sec, c(0, 0.01, 0.02, 0.03, 0.05))
  expect_equal(view$spikes$representative_raw_row_index, c(2L, 3L, 1L, 4L, 5L))
  expect_equal(view$isis$ISI_sec, c(NA, 0.01, 0.01, 0.01, 0.02))

  contaminated <- dat
  contaminated$pattern_manual <- rev(contaminated$pattern_manual)
  contaminated$pattern_auto <- "injected_prediction"
  contaminated$truth_label <- paste0("truth_", seq_len(nrow(contaminated)))
  contaminated$review_cache <- I(lapply(seq_len(nrow(contaminated)), list))
  contaminated <- contaminated[rev(names(contaminated))]
  contaminated_view <- threshold_first_normalize(
    contaminated, "train_A"
  )
  expect_identical(
    stpd_threshold_first_label_free_view(contaminated_view),
    stpd_threshold_first_label_free_view(view)
  )
  expect_identical(
    stpd_threshold_first_train_view_hash(contaminated_view),
    stpd_threshold_first_train_view_hash(view)
  )
  all_names <- unlist(lapply(
    stpd_threshold_first_label_free_view(view)[c("spikes", "isis", "index_map")],
    names
  ))
  expect_false(any(grepl(
    "manual|truth|auto|review|cache|prediction", all_names, ignore.case = TRUE
  )))
})

test_that("duplicates and nonfinite rows remain auditable", {
  dat <- data.frame(
    idx = 1:6,
    timestamp_sec = c(0.2, 0, 0.1, 0.1, NA_real_, Inf),
    stringsAsFactors = FALSE
  )
  expect_s3_class(
    threshold_first_error(threshold_first_normalize(
      dat, "train_dup", duplicate_policy = "error"
    )),
    "stpd_threshold_first_duplicate_timestamp"
  )
  view <- threshold_first_normalize(
    dat, "train_dup", duplicate_policy = "collapse_exact"
  )
  expect_equal(view$spikes$timestamp_sec, c(0, 0.1, 0.2))
  expect_identical(view$metadata$collapsed_duplicate_row_count, 1L)
  expect_identical(view$metadata$rejected_raw_row_count, 2L)
  expect_false(view$metadata$fit_eligible)
  expect_equal(sum(view$index_map$mapping_status == "retained"), 3L)
  expect_equal(sum(view$index_map$mapping_status == "collapsed_duplicate"), 1L)
  expect_equal(
    sum(view$index_map$mapping_status == "dropped_nonfinite_timestamp"), 2L
  )
  expect_equal(view$index_map$normalized_spike_index[3:4], c(2L, 2L))
  expect_match(stpd_threshold_first_train_view_audit_hash(view), "^[0-9a-f]{64}$")
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_train_view_hash(view)),
    "stpd_threshold_first_train_view_fit_ineligible"
  )

  impossible_error_policy <- view
  impossible_error_policy$metadata$duplicate_policy <- "error"
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(
      impossible_error_policy
    )),
    "stpd_threshold_first_train_view_schema_invalid"
  )
})

test_that("time input and materialized table columns are plain real vectors", {
  complex_input <- data.frame(
    timestamp_sec = c(0 + 9i, 0.01 + 8i, 0.02 + 7i)
  )
  expect_s3_class(
    threshold_first_error(threshold_first_normalize(
      complex_input, "complex_time"
    )),
    "stpd_threshold_first_field_invalid"
  )

  view <- threshold_first_normalize(
    data.frame(timestamp_sec = c(0, 0.01, 0.02)), "matrix_tamper"
  )
  matrix_column <- view
  matrix_column$spikes$timestamp_sec <- I(cbind(
    view$spikes$timestamp_sec, view$spikes$timestamp_sec + 1
  ))
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(
      matrix_column
    )),
    "stpd_threshold_first_train_view_schema_invalid"
  )
  attributed_floor <- view
  attributed_floor$metadata$min_valid_isi_sec <- structure(
    attributed_floor$metadata$min_valid_isi_sec, units = "sec"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(
      attributed_floor
    )),
    "stpd_threshold_first_train_view_schema_invalid"
  )
})

test_that("raw hard boundaries project only to the sorted acquisition-block start", {
  view <- threshold_first_normalize(
    data.frame(timestamp_sec = c(0, 0.5, 0.1)), "sorted_boundary",
    hard_boundary_before = c(FALSE, TRUE, FALSE)
  )
  expect_equal(view$spikes$timestamp_sec, c(0, 0.1, 0.5))
  expect_identical(
    view$spikes$acquisition_block_id,
    c("acquisition_000001", "acquisition_000002", "acquisition_000002")
  )
  expect_identical(view$spikes$hard_boundary_before, c(TRUE, TRUE, FALSE))
  expect_true(stpd_threshold_first_validate_train_view(view))
})

test_that("nonfinite timestamps hard-cut both sides and segment re-entry fails closed", {
  missing_middle <- threshold_first_normalize(
    data.frame(idx = 1:3, timestamp_sec = c(0, NA_real_, 0.2)),
    "missing_middle"
  )
  expect_false(missing_middle$metadata$fit_eligible)
  expect_true(all(missing_middle$isis$hard_boundary_before))
  expect_true(all(is.na(missing_middle$isis$ISI_sec)))
  expect_false(any(missing_middle$isis$valid_isi))

  reentry <- data.frame(
    idx = 1:5, timestamp_sec = c(0, 0.01, 0, 0.01, 0.02),
    segment_id = c("a", "a", "b", "b", "a")
  )
  expect_s3_class(
    threshold_first_error(threshold_first_normalize(reentry, "reentry")),
    "stpd_threshold_first_segment_reentry"
  )
})

test_that("raw acquisition causes exactly determine normalized hard boundaries", {
  gap <- threshold_first_normalize(
    data.frame(timestamp_sec = c(0, NA_real_, 1)), "raw_gap"
  )
  removed <- gap
  removed$spikes$acquisition_block_id[[2L]] <- "acquisition_000001"
  removed$spikes$analysis_block_id[[2L]] <- "analysis_000001"
  removed$spikes$normalized_analysis_block_spike_index[[2L]] <- 2L
  removed$spikes$hard_boundary_before[[2L]] <- FALSE
  removed$isis$acquisition_block_id[[2L]] <- "acquisition_000001"
  removed$isis$analysis_block_id[[2L]] <- "analysis_000001"
  removed$isis$left_spike_index[[2L]] <- 1L
  removed$isis$ISI_sec[[2L]] <- 1
  removed$isis$valid_isi[[2L]] <- TRUE
  removed$isis$invalid_reason[[2L]] <- ""
  removed$isis$hard_boundary_before[[2L]] <- FALSE
  removed$index_map$acquisition_block_id[[3L]] <- "acquisition_000001"
  removed$index_map$analysis_block_id[[3L]] <- "analysis_000001"
  removed$metadata$normalized_analysis_block_count <- 1L
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(removed)),
    "stpd_threshold_first_boundary_invalid"
  )

  continuous <- threshold_first_normalize(
    data.frame(timestamp_sec = c(0, 1, 2)), "raw_continuous"
  )
  injected <- continuous
  injected$spikes$acquisition_block_id[2:3] <- "acquisition_000002"
  injected$spikes$analysis_block_id[2:3] <- "analysis_000002"
  injected$spikes$normalized_analysis_block_spike_index[2:3] <- 1:2
  injected$spikes$hard_boundary_before[[2L]] <- TRUE
  injected$isis$acquisition_block_id[2:3] <- "acquisition_000002"
  injected$isis$analysis_block_id[2:3] <- "analysis_000002"
  injected$isis$left_spike_index[[2L]] <- NA_integer_
  injected$isis$ISI_sec[[2L]] <- NA_real_
  injected$isis$valid_isi[[2L]] <- FALSE
  injected$isis$invalid_reason[[2L]] <- "hard_boundary"
  injected$isis$hard_boundary_before[[2L]] <- TRUE
  injected$index_map$acquisition_block_id[2:3] <- "acquisition_000002"
  injected$index_map$analysis_block_id[2:3] <- "analysis_000002"
  injected$metadata$normalized_analysis_block_count <- 2L
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(injected)),
    "stpd_threshold_first_boundary_invalid"
  )
})

test_that("raw-row permutation changes audit mapping but not scientific input", {
  dat <- threshold_first_fixture()
  permuted <- dat[c(3L, 5L, 1L, 4L, 2L), , drop = FALSE]
  first <- threshold_first_normalize(dat, "permutation")
  second <- threshold_first_normalize(permuted, "permutation")

  expect_identical(
    stpd_threshold_first_scientific_train_projection(first),
    stpd_threshold_first_scientific_train_projection(second)
  )
  expect_identical(
    stpd_threshold_first_train_view_hash(first),
    stpd_threshold_first_train_view_hash(second)
  )
  expect_false(identical(
    stpd_threshold_first_train_view_audit_hash(first),
    stpd_threshold_first_train_view_audit_hash(second)
  ))
})

test_that("train-view validation closes metadata, mappings, coordinates, and masks", {
  view <- threshold_first_normalize(
    data.frame(
      idx = 1:5, timestamp_sec = c(0, 0.01, 0.01, 0.03, 0.05),
      hard_boundary_before = c(FALSE, FALSE, FALSE, TRUE, FALSE),
      valid_isi_mask = c(TRUE, TRUE, FALSE, TRUE, TRUE)
    ),
    "closure", duplicate_policy = "collapse_exact"
  )
  expect_true(stpd_threshold_first_validate_train_view(view))

  tampered <- list(
    metadata_count = function(x) {
      x$metadata$raw_row_count <- 999L; x
    },
    mapping_status = function(x) {
      x$index_map$mapping_status[[1L]] <- "invented"; x
    },
    representative = function(x) {
      x$spikes$representative_raw_row_index[[1L]] <- 2L; x
    },
    isi_segment = function(x) {
      x$isis$segment_id[[2L]] <- "invented"; x
    },
    isi_boundary = function(x) {
      x$isis$hard_boundary_before[[2L]] <- TRUE; x
    },
    validity = function(x) {
      x$isis$valid_isi[[2L]] <- !x$isis$valid_isi[[2L]]; x
    },
    reason = function(x) {
      x$isis$invalid_reason[[2L]] <- "invented"; x
    }
  )
  for (mutate in tampered) {
    expect_s3_class(
      threshold_first_error(stpd_threshold_first_validate_train_view(
        mutate(view)
      )),
      "stpd_threshold_first_error"
    )
  }
})

test_that("label-free projection requires frozen acquisition/QC provenance", {
  unknown <- stpd_threshold_first_normalize_train_view(
    threshold_first_fixture(), "unknown_provenance"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_label_free_view(unknown)),
    "stpd_threshold_first_label_blind_provenance_required"
  )
  bad <- threshold_first_label_blind_provenance()
  bad$qc_mask_policy_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_input_provenance(bad)),
    "stpd_threshold_first_label_blind_provenance_invalid"
  )
})

test_that("segments, hard boundaries, and QC masks cannot become Pause support", {
  segmented <- data.frame(
    idx = 1:4,
    timestamp_sec = c(0, 0.01, 0, 0.02),
    segment_id = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )
  segmented_view <- threshold_first_normalize(
    segmented, "segmented"
  )
  expect_identical(
    which(segmented_view$isis$hard_boundary_before), c(1L, 3L)
  )
  expect_true(all(is.na(segmented_view$isis$ISI_sec[c(1L, 3L)])))
  expect_false(any(segmented_view$isis$valid_isi[c(1L, 3L)]))

  bounded <- data.frame(
    idx = 1:5,
    timestamp_sec = c(0, 0.01, 0.02, 0.50, 0.51),
    hard_boundary_before = c(TRUE, FALSE, FALSE, TRUE, FALSE),
    valid_isi_mask = c(FALSE, TRUE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  bounded_view <- threshold_first_normalize(
    bounded, "bounded"
  )
  expect_true(is.na(bounded_view$isis$ISI_sec[[4L]]))
  expect_identical(bounded_view$isis$invalid_reason[[4L]], "hard_boundary")
  expect_identical(bounded_view$isis$invalid_reason[[3L]], "caller_masked")
  expect_false(bounded_view$isis$valid_isi[[3L]])
  expect_true(bounded_view$isis$valid_isi[[5L]])
})

test_that("the QC floor is one explicit unclassed double", {
  fixture <- data.frame(timestamp_sec = c(0, 0.01, 0.02))
  for (bad in list(c(0.001, 0.002), 1L, TRUE, "0.001",
                   structure(0.001, units = "sec"))) {
    expect_s3_class(
      threshold_first_error(threshold_first_normalize(
        fixture, "bad_floor", min_valid_isi_sec = bad
      )),
      "stpd_threshold_first_field_invalid"
    )
  }
  expect_true(stpd_threshold_first_validate_train_view(
    threshold_first_normalize(
      fixture, "valid_floor", min_valid_isi_sec = 0.001
    )
  ))
})

test_that("seconds and milliseconds have the same scientific view and hash", {
  seconds <- threshold_first_normalize(
    threshold_first_fixture("s"), "unit_train", time_unit = "s"
  )
  milliseconds <- threshold_first_normalize(
    threshold_first_fixture("ms"), "unit_train", time_unit = "ms"
  )
  expect_false(identical(
    stpd_threshold_first_label_free_view(seconds),
    stpd_threshold_first_label_free_view(milliseconds)
  ))
  expect_identical(
    stpd_threshold_first_scientific_train_projection(seconds),
    stpd_threshold_first_scientific_train_projection(milliseconds)
  )
  expect_identical(
    stpd_threshold_first_train_view_hash(seconds),
    stpd_threshold_first_train_view_hash(milliseconds)
  )
})

test_that("scientific hash includes science and excludes audit materialization", {
  view <- threshold_first_normalize(
    threshold_first_fixture(), "hash_train"
  )
  off <- stpd_threshold_first_run_config(
    "threshold_first_shadow", "user", "ignore", "off"
  )
  full <- stpd_threshold_first_run_config(
    "threshold_first_shadow", "user", "ignore", "full"
  )
  params_a <- threshold_first_params(
    list(z = 2L, a = list(beta = 0.2, alpha = 0.1))
  )
  params_reordered <- threshold_first_params(
    list(a = list(alpha = 0.1, beta = 0.2), z = 2L)
  )
  hash_a <- stpd_threshold_first_scientific_hash(view, off, params_a)
  expect_match(hash_a, "^[0-9a-f]{64}$")
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_scientific_hash(
      view, off, params_a, rng_kind = "Mersenne-Twister",
      rng_seed = 1 + 99i
    )),
    "stpd_threshold_first_numeric_input_invalid"
  )
  expect_identical(
    hash_a,
    stpd_threshold_first_scientific_hash(view, full, params_reordered)
  )
  params_changed <- threshold_first_params(
    list(z = 2L, a = list(beta = 0.2, alpha = 0.11))
  )
  expect_false(identical(
    hash_a,
    stpd_threshold_first_scientific_hash(view, off, params_changed)
  ))

  provider <- stpd_threshold_first_run_config(
    "threshold_first_shadow", "provider", "ignore", "summary"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_scientific_hash(
      view, provider, params_a
    )),
    "stpd_threshold_first_provider_state_required"
  )
  provider_hash <- paste(rep("a", 64L), collapse = "")
  fold_hash <- paste(rep("b", 64L), collapse = "")
  provider_identity <- stpd_threshold_first_provider_identity(
    provider_id = "mean_isi", provider_version = "1.0",
    provider_state_sha256 = provider_hash,
    fold_manifest_sha256 = fold_hash,
    train_view_sha256 = stpd_threshold_first_train_view_hash(view),
    information_access = "label_blind",
    performance_use = "label_blind_detector_performance",
    group_separation_status = "not_required",
    labels_used = FALSE, reference_annotations_used = FALSE,
    evaluation_group_ids = "evaluation_group_1"
  )
  expect_match(
    stpd_threshold_first_scientific_hash(
      view, provider, params_a, provider_identity = provider_identity
    ),
    "^[0-9a-f]{64}$"
  )
  locked <- stpd_threshold_first_run_config(
    "threshold_first_shadow", "user", "lock", "off"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_scientific_hash(
      view, locked, params_a
    )),
    "stpd_threshold_first_label_blind_manual_policy_conflict"
  )
})

test_that("canonical train-view and scientific hashes match the Phase 0 golden fixture", {
  dat <- data.frame(
    idx = 101:105,
    timestamp_sec = c(0.02, 0, 0.01, 0.03, 0.05),
    ISI_sec = c(0.01, NA, 0.01, 0.01, 0.02),
    pattern_manual = c("burst", "", "pause", "", "tonic"),
    stringsAsFactors = FALSE
  )
  view <- threshold_first_normalize(dat, "golden_train")
  expect_identical(
    stpd_threshold_first_train_view_hash(view),
    "62d14090ffebf7663152122436496c4670cbf9271937b962f076cbe7116c4021"
  )
  config <- stpd_threshold_first_run_config(
    "threshold_first_shadow", "user", "ignore", "off"
  )
  expect_identical(
    stpd_threshold_first_scientific_hash(
      view, config, threshold_first_params(list(a = 1L, b = 0.25))
    ),
    "8863cc1675888e1bacc389c422f1867981ad02bc88ee8f6b0b2822f739383763"
  )
})

test_that("numeric primitives have frozen inclusive-tail semantics", {
  contract <- stpd_threshold_first_numeric_contract()
  expect_identical(contract$definition_status, "definition_frozen")
  expect_identical(
    contract$engineering_defaults_status,
    "shadow_development_preregistered"
  )
  expect_identical(contract$contrast_seed_min_spikes, 4L)
  expect_identical(
    contract$contrast_seed_scope,
    "initial_contrast_proposer_only_not_final_burst_gate"
  )
  expect_equal(contract$strict_single_bridge_ratio_max, 3.5)
  expect_equal(
    stpd_threshold_first_empirical_tail_p(c(1, 4), 1:4, "lower"),
    c(0.4, 1.0)
  )
  expect_equal(
    stpd_threshold_first_empirical_tail_p(c(1, 4), 1:4, "upper"),
    c(1.0, 0.4)
  )
  p <- c(0.01, 0.04, 0.03)
  expect_identical(stpd_threshold_first_bh_adjust(p), stats::p.adjust(p, "BH"))
  expect_equal(stpd_threshold_first_run_surprise(c(0.1, 0.01)), 3)
  expect_equal(
    stpd_threshold_first_log_residual(c(0.01, 0.02), 0.01),
    c(0, log(2))
  )
  expect_identical(
    stpd_threshold_first_robust_log_reference(rep(0.01, 30))$status,
    "unresolved_zero_mad"
  )
  expect_identical(
    stpd_threshold_first_robust_log_reference(c(0.01, 0.02), 30)$status,
    "unresolved_insufficient_reference"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_robust_log_reference(
      seq(0.01, 0.04, length.out = 30), 2.5
    )),
    "stpd_threshold_first_numeric_input_invalid"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_robust_log_reference(
      c(seq(0.01, 0.04, length.out = 30), NA_real_), 30
    )),
    "stpd_threshold_first_numeric_input_invalid"
  )
  expect_identical(
    stpd_threshold_first_robust_log_reference(
      c(rep(0.01, 29), 0.01 + 1e-14), 30
    )$status,
    "unresolved_zero_mad"
  )
  expect_identical(
    stpd_threshold_first_q_zone(c(0.05, 0.10, 0.10001)),
    c("stable_support", "gray_zone", "outside")
  )
  expect_identical(
    stpd_threshold_first_qc_floor_status(c(0.0008, 0.0009), 0.0009),
    c("below_qc_floor", "eligible")
  )
  accepted_bridge <- stpd_threshold_first_bridge_ratio(
    c(0.01, 0.01, 0.035, 0.01, 0.01), 3L,
    c(TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  rejected_bridge <- stpd_threshold_first_bridge_ratio(
    c(0.01, 0.01, 0.035001, 0.01, 0.01), 3L,
    c(TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  expect_equal(accepted_bridge$bridge_ratio, 3.5)
  expect_identical(accepted_bridge$status, "bridge_supported")
  expect_identical(rejected_bridge$status, "bridge_rejected")
  asymmetric <- stpd_threshold_first_bridge_ratio(
    c(1, 1, 0.04, 0.01, 0.01), 3L,
    c(TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  expect_identical(asymmetric$status, "bridge_rejected")
  insufficient <- stpd_threshold_first_bridge_ratio(
    c(0.01, 0.035, 0.01), 2L, c(TRUE, FALSE, TRUE)
  )
  expect_identical(
    insufficient$status, "bridge_rejected_insufficient_direct_flank"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_bridge_ratio(
      c(0.01, 0.035, 0.01), 2L, c(TRUE, FALSE, TRUE),
      minimum_flank_isi = 1L
    )),
    "stpd_threshold_first_numeric_input_invalid"
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_bridge_ratio(
      c(0.01, 0.01, 0.04, 0.01, 0.01), 3L,
      c(TRUE, TRUE, FALSE, TRUE, TRUE), ratio_max = 4
    )),
    "stpd_threshold_first_numeric_input_invalid"
  )
  expect_identical(
    stpd_threshold_first_resolution_action("unresolved_zero_mad"),
    "abstain_no_borrowing"
  )
  for (bad in list("0.01", TRUE, 0.01 + 1i, matrix(0.01, 1, 1))) {
    expect_s3_class(
      threshold_first_error(stpd_threshold_first_empirical_tail_p(
        bad, c(0.01, 0.02), "lower"
      )),
      "stpd_threshold_first_numeric_input_invalid"
    )
  }
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_log_residual(
      c(0.01, 0.02), c(0.01, 0.02)
    )),
    "stpd_threshold_first_numeric_input_invalid"
  )
})

test_that("block identifiers are mechanically derived and cannot be renamed or merged", {
  dat <- data.frame(timestamp_sec = c(0, 0.01, 0.03, 0.04))
  view <- threshold_first_normalize(
    dat, "block_contract", hard_boundary_before = c(FALSE, FALSE, TRUE, FALSE)
  )

  renamed <- view
  renamed$spikes$acquisition_block_id <- sub(
    "acquisition_", "arbitrary_", renamed$spikes$acquisition_block_id
  )
  renamed$isis$acquisition_block_id <- renamed$spikes$acquisition_block_id
  renamed$index_map$acquisition_block_id <- sub(
    "acquisition_", "arbitrary_", renamed$index_map$acquisition_block_id
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(renamed)),
    "stpd_threshold_first_boundary_invalid"
  )

  merged <- view
  merged$spikes$acquisition_block_id[] <- "acquisition_000001"
  merged$isis$acquisition_block_id[] <- "acquisition_000001"
  merged$index_map$acquisition_block_id[] <- "acquisition_000001"
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(merged)),
    "stpd_threshold_first_boundary_invalid"
  )

  renamed_analysis <- view
  renamed_analysis$spikes$analysis_block_id <- sub(
    "analysis_", "arbitrary_", renamed_analysis$spikes$analysis_block_id
  )
  renamed_analysis$isis$analysis_block_id <- renamed_analysis$spikes$analysis_block_id
  renamed_analysis$index_map$analysis_block_id <- sub(
    "analysis_", "arbitrary_", renamed_analysis$index_map$analysis_block_id
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(renamed_analysis)),
    "stpd_threshold_first_boundary_invalid"
  )
})

test_that("ISI closure uses one explicit absolute tolerance at large scales", {
  view <- threshold_first_normalize(
    data.frame(timestamp_sec = c(0, 1e8)), "large_scale"
  )
  tampered <- view
  tampered$isis$ISI_sec[[2L]] <- 1e9
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_train_view(tampered)),
    "stpd_threshold_first_timestamp_non_monotonic"
  )
})

test_that("local references and BH families never cross analysis blocks", {
  valid <- rep(TRUE, 80L)
  blocks <- c(rep("a", 40L), rep("b", 40L))
  selected <- stpd_threshold_first_local_reference_indices(
    valid, blocks, query_index = 20L, each_side = 5L, guard = 2L,
    minimum_n = 10L
  )
  expect_identical(selected$status, "resolved")
  expect_identical(selected$left_indices, 13:17)
  expect_identical(selected$right_indices, 23:27)
  expect_true(all(blocks[selected$indices] == "a"))
  invalid_query <- stpd_threshold_first_local_reference_indices(
    replace(valid, 20L, FALSE), blocks, query_index = 20L,
    each_side = 5L, guard = 2L, minimum_n = 10L
  )
  expect_identical(invalid_query$status, "unresolved_query_ineligible")
  expect_identical(invalid_query$indices, integer())
  expect_identical(
    stpd_threshold_first_resolution_action(invalid_query$status),
    "abstain_no_borrowing"
  )

  raw_p <- c(0.01, 0.04, 0.04)
  adjusted <- stpd_threshold_first_adjust_tail_by_family(
    raw_p, c("train_a_block_1_burst", "train_a_block_1_burst",
             "train_a_block_2_burst")
  )
  expect_equal(adjusted, c(0.02, 0.04, 0.04))
  expect_equal(stpd_threshold_first_local_ratio(0.02, log(0.01)), 2)
})

test_that("scientific parameter hashing canonicalizes the caller payload", {
  first <- threshold_first_params(list(
    band = c(upper = 0.02, lower = 0.001), seed_spikes = 4L
  ))
  reordered <- threshold_first_params(list(
    seed_spikes = 4L, band = c(lower = 0.001, upper = 0.02)
  ))
  expect_identical(
    first$effective_params_sha256, reordered$effective_params_sha256
  )
  expect_identical(first$payload_role, "caller_supplied_contract_payload")
  expect_identical(
    first$completeness_status,
    "unverified_until_engine_parameter_resolver_wiring"
  )

  duplicated <- c(0.001, 0.02)
  names(duplicated) <- c("lower", "lower")
  expect_s3_class(
    threshold_first_error(threshold_first_params(list(band = duplicated))),
    "stpd_threshold_first_hash_names_invalid"
  )

  expect_s3_class(
    threshold_first_error(threshold_first_params(list(band = matrix(1:4, 2)))),
    "stpd_threshold_first_scientific_params_invalid"
  )
  expect_s3_class(
    threshold_first_error(threshold_first_params(list(
      duration = structure(1, units = "sec")
    ))),
    "stpd_threshold_first_scientific_params_invalid"
  )
})

test_that("provider absence is not silently reinterpreted as negative evidence", {
  expect_identical(
    stpd_threshold_first_validate_provider_status("resolved", "no_support"),
    list(status = "resolved", outcome = "no_support")
  )
  expect_identical(
    stpd_threshold_first_validate_provider_status("unsupported", "not_applicable"),
    list(status = "unsupported", outcome = "not_applicable")
  )
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_provider_status(
      "unresolved", "no_support"
    )),
    "stpd_threshold_first_provider_status_invalid"
  )
  expect_identical(
    stpd_threshold_first_map_provider_status(
      "complete", "positive", coverage_status = "partial",
      model_adequacy_status = "adequate"
    ),
    list(status = "resolved", outcome = "support")
  )
  expect_identical(
    stpd_threshold_first_map_provider_status(
      "complete", "positive", coverage_status = "none",
      model_adequacy_status = "adequate"
    ),
    list(status = "error", outcome = "not_applicable")
  )
  expect_identical(
    stpd_threshold_first_map_provider_status(
      "complete", "negative", coverage_status = "complete",
      model_adequacy_status = "adequate"
    ),
    list(status = "resolved", outcome = "no_support")
  )
  expect_identical(
    stpd_threshold_first_map_provider_status(
      "complete", "negative", coverage_status = "partial",
      model_adequacy_status = "adequate"
    ),
    list(status = "unresolved", outcome = "not_applicable")
  )
  expect_identical(
    stpd_threshold_first_map_provider_status(
      "rejected", "indeterminate", capability_supported = FALSE
    ),
    list(status = "unsupported", outcome = "not_applicable")
  )
  expect_identical(
    stpd_threshold_first_map_provider_status(
      "complete", "positive", normalization_status = "rejected"
    ),
    list(status = "error", outcome = "not_applicable")
  )
})

test_that("provider identity proves information access and group separation", {
  view <- threshold_first_normalize(threshold_first_fixture(), "provider_view")
  sha <- function(letter) paste(rep(letter, 64L), collapse = "")
  identity <- stpd_threshold_first_provider_identity(
    provider_id = "manual_examples", provider_version = "1.0",
    provider_state_sha256 = sha("a"), fold_manifest_sha256 = sha("b"),
    train_view_sha256 = stpd_threshold_first_train_view_hash(view),
    information_access = "manual_aware",
    performance_use = "heldout_detector_performance_only",
    group_separation_status = "verified_disjoint",
    labels_used = TRUE, reference_annotations_used = TRUE,
    calibration_group_ids = c("left_1", "right_1"),
    evaluation_group_ids = "left_2"
  )
  expect_true(stpd_threshold_first_validate_provider_identity(identity))
  expect_identical(identity$group_overlap_count, 0L)

  expect_s3_class(
    threshold_first_error(stpd_threshold_first_provider_identity(
      provider_id = "contradictory", provider_version = "1.0",
      provider_state_sha256 = sha("a"), fold_manifest_sha256 = sha("b"),
      train_view_sha256 = stpd_threshold_first_train_view_hash(view),
      information_access = "label_blind",
      performance_use = "heldout_detector_performance_only",
      group_separation_status = "verified_disjoint",
      labels_used = TRUE, reference_annotations_used = FALSE,
      calibration_group_ids = "left_1", evaluation_group_ids = "left_2"
    )),
    "stpd_threshold_first_provider_identity_invalid"
  )

  expect_s3_class(
    threshold_first_error(stpd_threshold_first_provider_identity(
      provider_id = "manual_examples", provider_version = "1.0",
      provider_state_sha256 = sha("a"), fold_manifest_sha256 = sha("b"),
      train_view_sha256 = stpd_threshold_first_train_view_hash(view),
      information_access = "manual_aware",
      performance_use = "label_blind_detector_performance",
      group_separation_status = "verified_disjoint",
      labels_used = TRUE, reference_annotations_used = TRUE,
      calibration_group_ids = "same_group",
      evaluation_group_ids = "same_group"
    )),
    "stpd_threshold_first_provider_identity_invalid"
  )

  tampered <- identity
  tampered$evaluation_groups_sha256 <- sha("c")
  expect_s3_class(
    threshold_first_error(stpd_threshold_first_validate_provider_identity(
      tampered
    )),
    "stpd_threshold_first_provider_identity_invalid"
  )
})

test_that("Phase 0 contracts remain dormant and preserve frozen legacy identities", {
  manifest <- stpd_threshold_first_phase0_manifest()
  values <- setNames(manifest$value, manifest$field)
  expect_identical(values[["detector_wiring_status"]], "dormant_not_connected")
  expect_identical(
    values[["frozen_legacy_scientific_payload_sha256"]],
    "9af37b989edc848c9d7a5c9eedc160ae080eed131b53c9846010c402fd23a545"
  )
  expect_identical(
    values[["frozen_provider_contract_sha256"]],
    "29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b"
  )
  file_manifest <- utils::read.csv(
    file.path(
      testthat::test_path("..", ".."), "validation",
      "threshold_first_phase0", "phase0_manifest.csv"
    ),
    stringsAsFactors = FALSE, colClasses = c("character", "character")
  )
  file_values <- setNames(file_manifest$value, file_manifest$field)
  manifest_mapping <- c(
    contract_version = "contract_version",
    run_config_schema = "run_config_schema",
    train_view_schema = "normalized_train_view_schema",
    label_free_schema = "label_free_view_schema",
    input_provenance_schema = "input_provenance_schema",
    numeric_schema = "numeric_contract_schema",
    scientific_hash_schema = "scientific_hash_schema",
    scientific_params_schema = "scientific_params_schema",
    provider_interface_schema = "provider_interface_schema",
    provider_identity_schema = "provider_identity_schema",
    provider_mapping_schema = "provider_mapping_schema",
    legacy_default_engine = "legacy_default_engine",
    legacy_default_threshold_source = "legacy_default_threshold_source",
    detector_wiring_status = "new_detector_wiring",
    frozen_legacy_scientific_payload_sha256 =
      "frozen_legacy_scientific_payload_sha256",
    frozen_provider_contract_sha256 = "frozen_provider_contract_sha256",
    phase0_golden_scientific_train_input_sha256 =
      "phase0_golden_scientific_train_input_sha256",
    phase0_golden_scientific_run_sha256 =
      "phase0_golden_scientific_run_sha256"
  )
  for (package_field in names(manifest_mapping)) {
    expect_identical(
      values[[package_field]], file_values[[manifest_mapping[[package_field]]]]
    )
  }
  expect_identical(
    stpd_provider_contract_hash(),
    "29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b"
  )
  expect_identical(
    stpd_params_hash(default_params()),
    "2dc2d2fd197f86ae9bdd126f2b0591efbf5c9343e06f17789ea8553685c2e380"
  )

  description <- read.dcf(file.path(testthat::test_path("..", ".."), "DESCRIPTION"))
  collate <- strsplit(description[1L, "Collate"], "[[:space:]]+")[[1L]]
  collate <- gsub("^'|'$", "", collate)
  expect_lt(
    match("89_threshold_first_contracts.R", collate),
    match("01_default_params.R", collate)
  )
})
