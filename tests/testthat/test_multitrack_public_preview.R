preview_enabled_params <- function(enabled = TRUE) {
  params <- default_params()
  params$spiketrainpattern <- params$spiketrainpattern %||% list()
  params$spiketrainpattern$multitrack_preview <- list(
    enabled = enabled,
    schema_version = "stpd_multitrack_public_preview_v1"
  )
  params
}

preview_candidate_pool <- function() {
  rows <- list(
    list("hfs", "high_frequency_spiking", 10L, 45L, 20, 900),
    list("burst_in_hfs", "burst", 20L, 23L, 30, 1200),
    list("review_a", "possible_burst", 30L, 35L, 12, 300),
    list("review_b", "possible_burst", 32L, 36L, 11, 300),
    list("tonic", "tonic", 50L, 100L, 20, 700),
    list("burst_left", "burst", 60L, 63L, 25, 1200),
    list("burst_right", "long_burst", 80L, 83L, 24, 1150)
  )
  dplyr::bind_rows(lapply(rows, function(row) {
    data.frame(
      candidate_id = row[[1]],
      candidate_layer = "phase2_preview_fixture",
      candidate_source = "synthetic_unit_fixture",
      final_label = row[[2]],
      start_isi = row[[3]],
      end_isi = row[[4]],
      n_isi = as.integer(row[[4]] - row[[3]] + 1L),
      score = as.numeric(row[[5]]),
      priority = as.numeric(row[[6]]),
      stringsAsFactors = FALSE
    )
  }))
}

preview_train <- function(pool = preview_candidate_pool(), params = NULL) {
  params <- params %||% preview_enabled_params()
  n <- 110L
  isi <- c(NA_real_, rep(0.04, n - 1L))
  dat <- data.frame(
    idx = seq_len(n),
    timestamp_sec = c(0, cumsum(isi[-1L])),
    ISI_sec = isi,
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("legacy_unchanged", n),
    stringsAsFactors = FALSE
  )
  phase1a <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
    pool,
    patterns = c(
      "burst", "long_burst", "possible_burst",
      "high_frequency_spiking", "tonic", "pause"
    ),
    params = params
  )
  phase1b <- SpikeTrainPatternDetector:::stpd_multitrack_compatibility_shadow(
    phase1a,
    dat = dat,
    params = params,
    variable_params = NULL,
    min_isi_sec = 0.001
  )
  attr(dat, "multitrack_shadow") <- phase1a
  attr(dat, "multitrack_compatibility_shadow") <- phase1b
  dat
}

preview_dataset <- function(
    trains, run_id = "preview_test_run",
    params_hash = stpd_params_hash(preview_enabled_params(TRUE))) {
  list(
    trains = trains,
    results = list(run_metadata = data.frame(
      run_id = run_id,
      params_hash = params_hash,
      stringsAsFactors = FALSE
    )),
    meta = list(display_name = "phase2_preview_fixture", unit_in = "s")
  )
}

expect_preview_failed_closed <- function(result, code) {
  expect_identical(result$metadata$materialization_status, "failed_closed")
  expect_identical(result$metadata$failure_code, code)
  expect_equal(nrow(result$intervals), 0L)
  expect_equal(nrow(result$relationships), 0L)
  expect_equal(nrow(result$per_isi), 0L)
  expect_equal(nrow(result$hfs_dominance_evidence), 0L)
  expect_gt(nrow(result$invariants), 0L)
  expect_true(all(result$invariants$status == "failed_closed"))
  invisible(result)
}

test_that("preview policy is exact, hashed, OFF by default, and strips stale output", {
  policy <- SpikeTrainPatternDetector:::stpd_multitrack_preview_policy(
    preview_enabled_params(FALSE)
  )
  expect_false(policy$enabled)
  expect_false(policy$authoritative)
  expect_identical(
    policy$schema_version,
    "stpd_multitrack_public_preview_v1"
  )
  expect_match(policy$policy_hash, "^[0-9a-f]{64}$")
  expect_false(identical(
    SpikeTrainPatternDetector:::stpd_multitrack_preview_policy_hash(
      preview_enabled_params(FALSE)
    ),
    SpikeTrainPatternDetector:::stpd_multitrack_preview_policy_hash(
      preview_enabled_params(TRUE)
    )
  ))

  params <- preview_enabled_params(FALSE)
  dat <- preview_train(params = preview_enabled_params(TRUE))
  dat$pattern_auto_event_preview <- "stale"
  dat$pattern_auto_event_preview_interval_id <- "stale_id"
  ds <- preview_dataset(list(train_1 = dat))
  ds$results$multitrack_preview <- list(stale = TRUE)
  legacy <- ds$trains$train_1$pattern_auto

  expect_null(SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    ds, params
  ))
  out <- SpikeTrainPatternDetector:::stpd_multitrack_preview_attach(ds, params)
  expect_false("multitrack_preview" %in% names(out$results))
  expect_false(any(grepl(
    "^pattern_auto_.*_preview($|_)", names(out$trains$train_1), perl = TRUE
  )))
  expect_identical(out$trains$train_1$pattern_auto, legacy)

  bad <- preview_enabled_params(TRUE)
  bad$spiketrainpattern$multitrack_preview$schema_version <- "future_v2"
  expect_error(
    SpikeTrainPatternDetector:::stpd_multitrack_preview_policy(bad),
    "Unsupported"
  )
  failed <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    ds, bad
  )
  expect_identical(failed$metadata$materialization_status, "failed_closed")
  expect_identical(failed$metadata$failure_code, "invalid_preview_policy")
  expect_equal(nrow(failed$intervals), 0L)

  bad_off <- preview_enabled_params(FALSE)
  bad_off$spiketrainpattern$multitrack_preview$schema_version <- "future_v2"
  expect_null(SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    ds, bad_off
  ))
})

test_that("HFS and Burst coexist while every Review candidate remains D-012 isolated", {
  params <- preview_enabled_params(TRUE)
  ds <- preview_dataset(list(train_1 = preview_train(params = params)))
  preview <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    ds, params
  )

  expect_s3_class(preview, "stpd_multitrack_public_preview")
  expect_identical(preview$metadata$materialization_status, "materialized")
  expect_false(preview$metadata$authoritative)
  expect_setequal(
    names(preview),
    c(
      "metadata", "intervals", "relationships", "per_isi",
      "hfs_dominance_evidence", "invariants", "table_manifest"
    )
  )

  intervals <- preview$intervals
  hfs <- intervals[
    intervals$candidate_id == "hfs" & intervals$interval_kind == "candidate",
    , drop = FALSE
  ]
  burst <- intervals[intervals$candidate_id == "burst_in_hfs", , drop = FALSE]
  expect_true(hfs$active_in_preview)
  expect_true(burst$active_in_preview)

  row20 <- preview$per_isi[preview$per_isi$isi_index == 20L, , drop = FALSE]
  expect_identical(row20$pattern_auto_state_preview, "high_frequency_spiking")
  expect_identical(row20$pattern_auto_event_preview, "burst")
  expect_true(nzchar(row20$pattern_auto_state_preview_interval_id))
  expect_true(nzchar(row20$pattern_auto_event_preview_interval_id))

  reviews <- intervals[intervals$semantic_track == "review", , drop = FALSE]
  expect_setequal(reviews$candidate_id, c("review_a", "review_b"))
  expect_equal(sum(reviews$active_in_preview), 1L)
  expect_true(all(reviews$review_target_track == "event"))
  expect_true(all(reviews$review_target_label == "burst"))
  expect_true(all(reviews$review_promotion_required))
  expect_false(any(reviews$eligible_for_primary_event_metrics))
  expect_false(any(reviews$eligible_for_state_split))
  expect_false(any(reviews$eligible_for_hfs_dominance))
  expect_equal(
    preview$per_isi$review_candidate_count[
      preview$per_isi$isi_index %in% 32:35
    ],
    rep(2L, 4L)
  )
  projected_review_ids <- unique(
    preview$per_isi$pattern_auto_review_preview_interval_id[
      nzchar(preview$per_isi$pattern_auto_review_preview_interval_id)
    ]
  )
  expect_identical(length(projected_review_ids), 1L)
  expect_identical(
    projected_review_ids,
    reviews$interval_id[reviews$active_in_preview]
  )

  expect_gt(nrow(preview$hfs_dominance_evidence), 0L)
  expect_false(any(grepl(
    "review_", preview$hfs_dominance_evidence$contributor_candidate_ids
  )))
  expect_true(all(nzchar(
    preview$hfs_dominance_evidence$hfs_interval_id
  )))
  expect_setequal(
    preview$invariants$check_name,
    c(
      "interval_id_unique",
      "relationship_foreign_keys",
      "active_within_track_non_overlap",
      "per_isi_first_row_empty",
      "d012_review_isolation",
      "dominance_evidence_non_destructive"
    )
  )
  expect_true(all(preview$invariants$status == "pass"))
  expect_gt(nrow(preview$intervals), 0L)
  expect_gt(nrow(preview$relationships), 0L)
  expect_gt(nrow(preview$per_isi), 0L)
})

test_that("split State parent is inactive and only evaluated passing fragments project", {
  params <- preview_enabled_params(TRUE)
  pool <- dplyr::bind_rows(
    preview_candidate_pool(),
    data.frame(
      candidate_id = c("pause_a", "pause_b"),
      candidate_layer = "phase2_preview_fixture",
      candidate_source = "synthetic_unit_fixture",
      final_label = "pause",
      start_isi = c(64L, 84L),
      end_isi = c(64L, 84L),
      n_isi = 1L,
      score = 20,
      priority = 300,
      stringsAsFactors = FALSE
    )
  )
  dat <- preview_train(pool = pool, params = params)
  compatibility <- attr(dat, "multitrack_compatibility_shadow")
  fragments <- compatibility$state_fragments
  tonic <- which(
    fragments$root_candidate_id == "tonic" & fragments$split_kind != "none"
  )
  expect_length(tonic, 3L)
  fragments$gate_evaluated[tonic] <- c(TRUE, TRUE, FALSE)
  fragments$provisional_gate_pass[tonic] <- c(TRUE, FALSE, NA)
  fragments$provisional_gate_status[tonic] <- c(
    "fixture_pass", "fixture_fail", "fixture_unresolved"
  )
  compatibility$state_fragments <- fragments
  attr(dat, "multitrack_compatibility_shadow") <- compatibility

  preview <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = dat)), params
  )
  state <- preview$intervals[
    preview$intervals$semantic_track == "state" &
      preview$intervals$root_candidate_id == "tonic",
    , drop = FALSE
  ]
  parent <- state[state$interval_kind == "candidate", , drop = FALSE]
  children <- state[state$interval_kind == "state_fragment", , drop = FALSE]
  expect_false(parent$active_in_preview)
  expect_equal(sum(children$active_in_preview), 1L)
  expect_setequal(
    children$activity_status,
    c(
      "active_fragment_regate_passed",
      "inactive_fragment_regate_failed",
      "inactive_fragment_regate_unresolved"
    )
  )
  expect_true(all(children$parent_interval_id == parent$interval_id))
  expect_true(all(
    preview$per_isi$pattern_auto_state_preview[50:63] == "tonic"
  ))
  expect_true(all(
    preview$per_isi$pattern_auto_state_preview[65:83] == ""
  ))
  expect_true(all(
    preview$per_isi$pattern_auto_state_preview[85:100] == ""
  ))
  expect_true(all(
    preview$per_isi$pattern_auto_state_preview[c(64L, 84L)] == ""
  ))
})

test_that("interval IDs are train-qualified and every relationship FK and geometry is valid", {
  params <- preview_enabled_params(TRUE)
  train <- preview_train(params = params)
  preview <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_a = train, train_b = train)), params
  )
  expect_identical(preview$metadata$materialization_status, "materialized")
  intervals <- preview$intervals
  same_candidate <- intervals[
    intervals$candidate_id == "burst_in_hfs" &
      intervals$interval_kind == "candidate",
    , drop = FALSE
  ]
  expect_equal(nrow(same_candidate), 2L)
  expect_equal(length(unique(same_candidate$interval_id)), 2L)
  expect_identical(anyDuplicated(intervals$interval_id), 0L)

  relationships <- preview$relationships
  expect_true(all(relationships$source_interval_id %in% intervals$interval_id))
  expect_true(all(relationships$target_interval_id %in% intervals$interval_id))
  expect_true(all(relationships$overlap_isi_n > 0L))
  expect_true(all(relationships$source_overlap_fraction > 0 &
    relationships$source_overlap_fraction <= 1))
  expect_true(all(relationships$target_overlap_fraction > 0 &
    relationships$target_overlap_fraction <= 1))
  expect_true(all(relationships$intersection_over_union > 0 &
    relationships$intersection_over_union <= 1))
  expect_true(all(relationships$policy_hash == preview$metadata$policy_hash))
})

test_that("public identities are invariant to candidate permutation and semantic duplicates fail closed", {
  params <- preview_enabled_params(TRUE)
  pool <- preview_candidate_pool()
  reference <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = preview_train(pool, params))), params
  )
  permuted <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(
      train_1 = preview_train(pool[rev(seq_len(nrow(pool))), , drop = FALSE], params)
    )),
    params
  )
  expect_identical(reference$metadata$materialization_status, "materialized")
  expect_identical(permuted$metadata$materialization_status, "materialized")

  stable_intervals <- function(x) {
    out <- x$intervals
    out <- out[order(out$interval_id, method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
    out
  }
  expect_false("source_candidate_index" %in% names(reference$intervals))
  expect_identical(stable_intervals(reference), stable_intervals(permuted))
  expect_identical(reference$relationships, permuted$relationships)
  expect_identical(reference$per_isi, permuted$per_isi)
  expect_identical(
    reference$hfs_dominance_evidence,
    permuted$hfs_dominance_evidence
  )

  duplicate_pool <- dplyr::bind_rows(pool, pool[1L, , drop = FALSE])
  duplicate <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = preview_train(duplicate_pool, params))),
    params
  )
  expect_identical(duplicate$metadata$materialization_status, "failed_closed")
  expect_identical(duplicate$metadata$failure_code, "duplicate_interval_id")
})

test_that("stale or tampered Phase 1 provenance fails closed", {
  params <- preview_enabled_params(TRUE)
  dat <- preview_train(params = params)

  tampered_policy <- dat
  shadow <- attr(tampered_policy, "multitrack_shadow")
  shadow$multitrack_policy_hash <- paste(rep("0", 64L), collapse = "")
  attr(tampered_policy, "multitrack_shadow") <- shadow
  policy_result <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = tampered_policy)), params
  )
  expect_identical(
    policy_result$metadata$materialization_status, "failed_closed"
  )
  expect_identical(
    policy_result$metadata$failure_code, "shadow_policy_hash_mismatch"
  )
  expect_equal(nrow(policy_result$intervals), 0L)

  tampered_source <- dat
  compatibility <- attr(
    tampered_source, "multitrack_compatibility_shadow"
  )
  compatibility$source_params_hash <- paste(rep("f", 64L), collapse = "")
  attr(tampered_source, "multitrack_compatibility_shadow") <- compatibility
  source_result <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = tampered_source)), params
  )
  expect_identical(
    source_result$metadata$materialization_status, "failed_closed"
  )
  expect_identical(
    source_result$metadata$failure_code,
    "shadow_source_params_hash_mismatch"
  )
  expect_equal(nrow(source_result$per_isi), 0L)

  wrong_public_hash <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(
      list(train_1 = dat),
      params_hash = paste(rep("a", 64L), collapse = "")
    ),
    params
  )
  expect_identical(
    wrong_public_hash$metadata$materialization_status,
    "failed_closed"
  )
  expect_identical(
    wrong_public_hash$metadata$failure_code,
    "public_params_hash_mismatch"
  )
  expect_equal(nrow(wrong_public_hash$intervals), 0L)
})

test_that("high-risk Preview abort codes remain reachable and fail closed", {
  params <- preview_enabled_params(TRUE)
  base_dat <- preview_train(params = params)
  materialize_one <- function(dat, selected_trains = NULL) {
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      preview_dataset(list(train_1 = dat)), params,
      selected_trains = selected_trains
    )
  }

  missing_train <- materialize_one(base_dat, selected_trains = "ghost_train")
  expect_preview_failed_closed(missing_train, "selected_train_missing")

  missing_shadow <- base_dat
  attr(missing_shadow, "multitrack_shadow") <- NULL
  missing_shadow_result <- materialize_one(missing_shadow)
  expect_preview_failed_closed(missing_shadow_result, "shadow_product_missing")

  missing_candidate_field <- base_dat
  shadow <- attr(missing_candidate_field, "multitrack_shadow")
  shadow$candidates$final_label <- NULL
  attr(missing_candidate_field, "multitrack_shadow") <- shadow
  missing_candidate_result <- materialize_one(missing_candidate_field)
  expect_preview_failed_closed(
    missing_candidate_result, "candidate_schema_missing"
  )

  invalid_geometry <- base_dat
  shadow <- attr(invalid_geometry, "multitrack_shadow")
  shadow$candidates$start_isi[1] <- 1L
  attr(invalid_geometry, "multitrack_shadow") <- shadow
  invalid_geometry_result <- materialize_one(invalid_geometry)
  expect_preview_failed_closed(
    invalid_geometry_result, "invalid_interval_geometry"
  )

  invariant_violation <- base_dat
  compatibility <- attr(
    invariant_violation, "multitrack_compatibility_shadow"
  )
  compatibility$invariants <- dplyr::bind_rows(
    compatibility$invariants,
    data.frame(
      invariant = "forced_test_violation",
      severity = "violation",
      state_candidate_id = "",
      other_candidate_id = "",
      message = "forced Phase 1B violation",
      stringsAsFactors = FALSE
    )
  )
  attr(invariant_violation, "multitrack_compatibility_shadow") <-
    compatibility
  invariant_result <- materialize_one(invariant_violation)
  expect_preview_failed_closed(
    invariant_result, "phase1b_invariant_violation"
  )

  d012_violation <- base_dat
  shadow <- attr(d012_violation, "multitrack_shadow")
  review_row <- which(shadow$candidates$semantic_track == "review")[1]
  shadow$candidates$review_target_track[review_row] <- "state"
  attr(d012_violation, "multitrack_shadow") <- shadow
  d012_result <- materialize_one(d012_violation)
  expect_preview_failed_closed(d012_result, "d012_review_isolation_failed")

  duplicate_relationship <- base_dat
  compatibility <- attr(
    duplicate_relationship, "multitrack_compatibility_shadow"
  )
  expect_gt(nrow(compatibility$relationships), 0L)
  compatibility$relationships <- dplyr::bind_rows(
    compatibility$relationships,
    compatibility$relationships[1, , drop = FALSE]
  )
  attr(duplicate_relationship, "multitrack_compatibility_shadow") <-
    compatibility
  duplicate_relationship_result <- materialize_one(duplicate_relationship)
  expect_preview_failed_closed(
    duplicate_relationship_result, "duplicate_relationship_id"
  )

  ineligible_contributor <- base_dat
  compatibility <- attr(
    ineligible_contributor, "multitrack_compatibility_shadow"
  )
  expect_gt(nrow(compatibility$hfs_dominance), 0L)
  contributor_id <- strsplit(
    compatibility$hfs_dominance$contributor_candidate_ids[1],
    ";", fixed = TRUE
  )[[1]][1]
  shadow <- attr(ineligible_contributor, "multitrack_shadow")
  contributor_row <- which(shadow$candidates$candidate_id == contributor_id)[1]
  expect_true(is.finite(contributor_row))
  shadow$candidates$selected_within_track[contributor_row] <- FALSE
  attr(ineligible_contributor, "multitrack_shadow") <- shadow
  ineligible_result <- materialize_one(ineligible_contributor)
  expect_preview_failed_closed(
    ineligible_result, "hfs_dominance_contributor_ineligible"
  )
})

test_that("relationship, dominance, and State-lineage tampering fails closed", {
  params <- preview_enabled_params(TRUE)
  dat <- preview_train(params = params)

  bad_relationship <- dat
  compatibility <- attr(bad_relationship, "multitrack_compatibility_shadow")
  expect_gt(nrow(compatibility$relationships), 0L)
  compatibility$relationships$source_candidate_id[1] <- "bogus_wrong_id"
  attr(bad_relationship, "multitrack_compatibility_shadow") <- compatibility
  relationship_result <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = bad_relationship)), params
  )
  expect_identical(
    relationship_result$metadata$failure_code,
    "relationship_foreign_key_failed"
  )

  bad_contributor <- dat
  compatibility <- attr(bad_contributor, "multitrack_compatibility_shadow")
  expect_gt(nrow(compatibility$hfs_dominance), 0L)
  compatibility$hfs_dominance$contributor_candidate_ids[1] <-
    "bogus_wrong_id"
  attr(bad_contributor, "multitrack_compatibility_shadow") <- compatibility
  contributor_result <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = bad_contributor)), params
  )
  expect_identical(
    contributor_result$metadata$failure_code,
    "hfs_dominance_contributor_identity_mismatch"
  )

  destructive_relationship <- dat
  compatibility <- attr(
    destructive_relationship, "multitrack_compatibility_shadow"
  )
  compatibility$relationships$non_destructive[1] <- FALSE
  attr(destructive_relationship, "multitrack_compatibility_shadow") <-
    compatibility
  destructive_relationship_result <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      preview_dataset(list(train_1 = destructive_relationship)), params
    )
  expect_identical(
    destructive_relationship_result$metadata$failure_code,
    "destructive_relationship_not_allowed"
  )

  destructive_dominance <- dat
  compatibility <- attr(
    destructive_dominance, "multitrack_compatibility_shadow"
  )
  compatibility$hfs_dominance$destructive_action_applied[1] <- TRUE
  attr(destructive_dominance, "multitrack_compatibility_shadow") <-
    compatibility
  destructive_dominance_result <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      preview_dataset(list(train_1 = destructive_dominance)), params
    )
  expect_identical(
    destructive_dominance_result$metadata$failure_code,
    "destructive_hfs_dominance_not_allowed"
  )

  lineage_pool <- dplyr::bind_rows(
    preview_candidate_pool(),
    data.frame(
      candidate_id = "pause_for_lineage",
      candidate_layer = "phase2_preview_fixture",
      candidate_source = "synthetic_unit_fixture",
      final_label = "pause",
      start_isi = 70L,
      end_isi = 70L,
      n_isi = 1L,
      score = 20,
      priority = 300,
      stringsAsFactors = FALSE
    )
  )
  bad_lineage <- preview_train(pool = lineage_pool, params = params)
  compatibility <- attr(bad_lineage, "multitrack_compatibility_shadow")
  split_parent <- which(compatibility$state_parents$split_kind != "none")[1]
  expect_true(is.finite(split_parent))
  compatibility$state_parents$selected_within_track_preserved[split_parent] <-
    FALSE
  attr(bad_lineage, "multitrack_compatibility_shadow") <- compatibility
  lineage_result <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = bad_lineage)), params
  )
  expect_identical(
    lineage_result$metadata$failure_code,
    "state_parent_lineage_invalid"
  )
})

test_that("same-track active overlap fails closed instead of using first-writer wins", {
  params <- preview_enabled_params(TRUE)
  dat <- preview_train(params = params)
  shadow <- attr(dat, "multitrack_shadow")
  review <- which(shadow$candidates$semantic_track == "review")
  expect_length(review, 2L)
  shadow$candidates$selected_within_track[review] <- TRUE
  attr(dat, "multitrack_shadow") <- shadow

  legacy <- dat$pattern_auto
  preview <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = dat)), params
  )
  expect_identical(preview$metadata$materialization_status, "failed_closed")
  expect_identical(preview$metadata$failure_code, "active_same_track_overlap")
  expect_equal(nrow(preview$intervals), 0L)
  expect_equal(nrow(preview$relationships), 0L)
  expect_equal(nrow(preview$per_isi), 0L)
  expect_identical(dat$pattern_auto, legacy)
  expect_true(all(preview$invariants$status == "failed_closed"))
})

test_that("empty selected trains retain typed public tables", {
  params <- preview_enabled_params(TRUE)
  empty <- data.frame(
    idx = integer(),
    timestamp_sec = double(),
    ISI_sec = double(),
    pattern_auto = character(),
    stringsAsFactors = FALSE
  )
  preview <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(empty_train = empty)), params
  )
  expect_identical(preview$metadata$materialization_status, "materialized")
  expect_equal(nrow(preview$intervals), 0L)
  expect_equal(nrow(preview$relationships), 0L)
  expect_equal(nrow(preview$per_isi), 0L)
  expect_type(preview$intervals$interval_id, "character")
  expect_type(preview$per_isi$review_candidate_count, "integer")
  empty_tables <- SpikeTrainPatternDetector:::stpd_multitrack_preview_empty_tables()
  expect_true(all(vapply(
    empty_tables,
    function(table) identical(anyDuplicated(names(table)), 0L),
    logical(1)
  )))
  expect_equal(nrow(preview$table_manifest), 6L)
  expect_true(all(nzchar(preview$table_manifest$column_types)))

  none <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_1 = preview_train(params = params))),
    params,
    selected_trains = character()
  )
  expect_equal(none$metadata$selected_train_n, 0L)
  expect_equal(nrow(none$intervals), 0L)
})

test_that("selected_trains strictly scopes every public table and metadata", {
  params <- preview_enabled_params(TRUE)
  train_a <- preview_train(params = params)
  train_b <- preview_train(params = params)
  preview <- SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
    preview_dataset(list(train_a = train_a, train_b = train_b)),
    params,
    selected_trains = "train_b"
  )
  expect_identical(preview$metadata$materialization_status, "materialized")
  expect_identical(preview$metadata$selected_train_n, 1L)
  expect_identical(preview$metadata$selected_trains, "train_b")
  for (name in c(
    "intervals", "relationships", "per_isi", "hfs_dominance_evidence",
    "invariants"
  )) {
    table <- preview[[name]]
    scoped <- unique(as.character(table$train))
    scoped <- scoped[!is.na(scoped) & nzchar(scoped)]
    expect_true(length(scoped) == 0L || setequal(scoped, "train_b"))
  }
})

test_that("preview export writes six data CSVs, manifest, and an identical RDS", {
  params <- preview_enabled_params(TRUE)
  ds <- preview_dataset(list(train_1 = preview_train(params = params)))
  ds <- SpikeTrainPatternDetector:::stpd_multitrack_preview_attach(
    ds, params
  )
  out_dir <- tempfile("multitrack_preview_export_")
  paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
    ds, out_dir
  )
  expect_length(paths, 8L)
  expect_true(all(file.exists(paths)))
  expect_equal(sum(grepl("\\.csv$", paths, ignore.case = TRUE)), 7L)
  expect_identical(readRDS(paths[["rds"]]), ds$results$multitrack_preview)
  expect_identical(
    attr(paths, "multitrack_preview_export_status"), "written"
  )

  manifest <- utils::read.csv(
    paths[["manifest"]], stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_equal(nrow(manifest), 6L)
  expect_true(all(grepl(":", manifest$column_types, fixed = TRUE)))
  for (i in seq_len(nrow(manifest))) {
    table_path <- file.path(out_dir, manifest$file_name[i])
    roundtrip <- utils::read.csv(
      table_path, stringsAsFactors = FALSE, check.names = FALSE
    )
    expect_identical(nrow(roundtrip), as.integer(manifest$row_count[i]))
    expect_identical(ncol(roundtrip), as.integer(manifest$column_count[i]))
  }

  # Reusing the same directory for an OFF result must remove only the stale
  # Preview artifacts rather than leaking them into a later export bundle.
  off_ds <- SpikeTrainPatternDetector:::stpd_multitrack_preview_strip(ds)
  empty_paths_same_dir <-
    SpikeTrainPatternDetector:::stpd_write_multitrack_preview(off_ds, out_dir)
  expect_length(empty_paths_same_dir, 0L)
  expect_identical(
    attr(empty_paths_same_dir, "multitrack_preview_export_status"),
    "not_present"
  )
  expect_false(any(file.exists(file.path(
    out_dir,
    c(
      "Multitrack_preview_metadata.csv",
      "Multitrack_preview_intervals.csv",
      "Multitrack_preview_relationships.csv",
      "Multitrack_preview_per_isi.csv",
      "Multitrack_preview_hfs_dominance_evidence.csv",
      "Multitrack_preview_invariants.csv",
      "Multitrack_preview_manifest.csv",
      "Multitrack_preview.rds"
    )
  ))))

  no_preview_dir <- tempfile("multitrack_preview_off_")
  empty_paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
    preview_dataset(list()), no_preview_dir
  )
  expect_length(empty_paths, 0L)
  expect_false(dir.exists(no_preview_dir))
})

test_that("persisted Preview corruption fails soft and cannot control write paths", {
  params <- preview_enabled_params(TRUE)
  ds <- preview_dataset(list(train_1 = preview_train(params = params)))
  ds <- SpikeTrainPatternDetector:::stpd_multitrack_preview_attach(ds, params)
  fixed_files <- c(
    "Multitrack_preview_metadata.csv",
    "Multitrack_preview_intervals.csv",
    "Multitrack_preview_relationships.csv",
    "Multitrack_preview_per_isi.csv",
    "Multitrack_preview_hfs_dominance_evidence.csv",
    "Multitrack_preview_invariants.csv",
    "Multitrack_preview_manifest.csv",
    "Multitrack_preview.rds"
  )

  out_dir <- tempfile("multitrack_preview_corrupt_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  unrelated <- file.path(out_dir, "legacy_sentinel.txt")
  writeLines("legacy intact", unrelated)

  # Establish a real stale artifact set. Every corruption case below must
  # remove this set before returning rather than merely avoiding new writes.
  initial_paths <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
    ds, out_dir
  )
  expect_identical(
    attr(initial_paths, "multitrack_preview_export_status"), "written"
  )
  expect_true(all(file.exists(file.path(out_dir, fixed_files))))

  missing_component <- ds
  missing_component$results$multitrack_preview$per_isi <- NULL
  expect_warning(
    skipped <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
      missing_component, out_dir
    ),
    "multitrack_preview_export_skipped"
  )
  expect_length(skipped, 0L)
  expect_identical(
    attr(skipped, "multitrack_preview_export_status"),
    "skipped_invalid_persisted_preview"
  )
  expect_identical(readLines(unrelated), "legacy intact")
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))

  path_tamper <- ds
  escape_target <- file.path(dirname(out_dir), "preview_escape_sentinel.csv")
  on.exit(unlink(escape_target, force = TRUE), add = TRUE)
  writeLines("outside intact", escape_target)
  path_tamper$results$multitrack_preview$table_manifest$file_name[1] <-
    paste0("../", basename(escape_target))
  expect_warning(
    path_skipped <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
      path_tamper, out_dir
    ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(path_skipped, "multitrack_preview_export_code"),
    "preview_manifest_filename_invalid"
  )
  expect_identical(readLines(escape_target), "outside intact")
  expect_identical(readLines(unrelated), "legacy intact")
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))

  stale_payload <- ds
  stale_payload$results$multitrack_preview$intervals$pattern[1] <-
    "tampered_after_materialization"
  expect_warning(
    hash_skipped <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
      stale_payload, out_dir
    ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(hash_skipped, "multitrack_preview_export_code"),
    "preview_manifest_payload_mismatch"
  )
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))

  refresh_manifest_row <- function(preview, table_name) {
    row <- match(table_name, preview$table_manifest$table_name)
    table <- preview[[table_name]]
    preview$table_manifest$row_count[row] <- as.integer(nrow(table))
    preview$table_manifest$column_count[row] <- as.integer(ncol(table))
    preview$table_manifest$column_types[row] <-
      SpikeTrainPatternDetector:::stpd_multitrack_preview_column_types(table)
    preview$table_manifest$table_sha256[row] <-
      SpikeTrainPatternDetector:::stpd_multitrack_preview_table_hash(table)
    preview
  }

  metadata_tamper <- ds
  metadata_tamper$results$multitrack_preview$metadata$
    legacy_projection_changed <- TRUE
  metadata_tamper$results$multitrack_preview <- refresh_manifest_row(
    metadata_tamper$results$multitrack_preview, "metadata"
  )
  expect_warning(
    metadata_skipped <-
      SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
        metadata_tamper, out_dir
      ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(metadata_skipped, "multitrack_preview_export_code"),
    "preview_metadata_semantics_invalid"
  )
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))

  per_isi_tamper <- ds
  id_columns <- grep(
    "_preview_interval_id$",
    names(per_isi_tamper$results$multitrack_preview$per_isi),
    value = TRUE
  )
  id_matrix <- per_isi_tamper$results$multitrack_preview$per_isi[id_columns]
  nonempty <- which(
    Reduce(`|`, lapply(id_matrix, function(x) nzchar(as.character(x))))
  )[1]
  expect_true(is.finite(nonempty))
  target_column <- id_columns[which(vapply(id_matrix, function(x) {
    nzchar(as.character(x[nonempty]))
  }, logical(1)))[1]]
  per_isi_tamper$results$multitrack_preview$per_isi[
    nonempty, target_column
  ] <- "ghost_interval_id"
  per_isi_tamper$results$multitrack_preview <- refresh_manifest_row(
    per_isi_tamper$results$multitrack_preview, "per_isi"
  )
  expect_warning(
    per_isi_skipped <-
      SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
        per_isi_tamper, out_dir
      ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(per_isi_skipped, "multitrack_preview_export_code"),
    "preview_per_isi_projection_invalid"
  )
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))

  cross_run <- ds
  cross_run$results$run_metadata$run_id <- "different_parent_run"
  expect_warning(
    cross_run_skipped <-
      SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
        cross_run, out_dir
      ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(cross_run_skipped, "multitrack_preview_export_code"),
    "preview_parent_identity_mismatch"
  )
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))

  extra_component <- ds
  extra_component$results$multitrack_preview$unexpected_component <- list(
    should_not_enter_canonical_rds = TRUE
  )
  expect_warning(
    extra_skipped <-
      SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
        extra_component, out_dir
      ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(extra_skipped, "multitrack_preview_export_code"),
    "preview_component_schema_invalid"
  )
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))
  semantic_tamper <- ds
  review_row <- which(
    semantic_tamper$results$multitrack_preview$intervals$semantic_track ==
      "review"
  )[1]
  expect_true(is.finite(review_row))
  semantic_tamper$results$multitrack_preview$intervals$
    review_target_track[review_row] <- "state"
  semantic_tamper$results$multitrack_preview <- refresh_manifest_row(
    semantic_tamper$results$multitrack_preview, "intervals"
  )
  expect_warning(
    semantic_skipped <-
      SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
        semantic_tamper, out_dir
      ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(semantic_skipped, "multitrack_preview_export_code"),
    "preview_semantic_invariant_failed"
  )
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))

  destructive_tamper <- ds
  expect_gt(
    nrow(destructive_tamper$results$multitrack_preview$relationships), 0L
  )
  destructive_tamper$results$multitrack_preview$relationships$
    non_destructive[1] <- FALSE
  destructive_tamper$results$multitrack_preview <- refresh_manifest_row(
    destructive_tamper$results$multitrack_preview, "relationships"
  )
  expect_warning(
    destructive_skipped <-
      SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
        destructive_tamper, out_dir
      ),
    "multitrack_preview_export_skipped"
  )
  expect_identical(
    attr(destructive_skipped, "multitrack_preview_export_code"),
    "preview_relationship_invariant_failed"
  )
  expect_false(any(file.exists(file.path(out_dir, fixed_files))))

  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn), add = TRUE)
  options(warn = 2)
  warn_as_error_safe <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
    missing_component, out_dir
  )
  expect_identical(
    attr(warn_as_error_safe, "multitrack_preview_export_status"),
    "skipped_invalid_persisted_preview"
  )

  warning_path <- file.path(out_dir, "Methodological_warnings.txt")
  unlink(warning_path, recursive = TRUE, force = TRUE)
  dir.create(warning_path)
  log_failure_safe <- SpikeTrainPatternDetector:::stpd_write_multitrack_preview(
    missing_component, out_dir
  )
  expect_identical(
    attr(log_failure_safe, "multitrack_preview_export_status"),
    "skipped_invalid_persisted_preview"
  )
})
