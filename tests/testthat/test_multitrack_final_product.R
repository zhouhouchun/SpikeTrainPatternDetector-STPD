gate_b_final_candidate <- function(id, label, start, end, priority = 800) {
  data.frame(
    candidate_id = id, candidate_layer = "gate_b_final_fixture",
    candidate_source = "unit_fixture", final_label = label,
    start_isi = as.integer(start), end_isi = as.integer(end),
    n_isi = as.integer(end - start + 1L), score = 20,
    priority = as.numeric(priority), stringsAsFactors = FALSE
  )
}

gate_b_final_fixture <- function(exact_event = FALSE, hfs = TRUE) {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  pool <- gate_b_final_candidate("review", "possible_burst", 20, 23, 300)
  if (exact_event) pool <- dplyr::bind_rows(
    pool, gate_b_final_candidate("burst", "burst", 20, 23, 1200)
  )
  if (hfs) pool <- dplyr::bind_rows(
    pool,
    gate_b_final_candidate("hfs", "high_frequency_spiking", 10, 45, 1000)
  )
  n <- 60L
  dat <- data.frame(
    idx = seq_len(n), timestamp_sec = c(0, cumsum(rep(0.04, n - 1L))),
    ISI_sec = c(NA_real_, rep(0.04, n - 1L)), pattern_manual = "",
    pattern_manual_negative = "", pattern_auto = "",
    stringsAsFactors = FALSE
  )
  phase1a <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
    pool, patterns = unique(pool$final_label), params = params
  )
  phase1b <- SpikeTrainPatternDetector:::stpd_multitrack_compatibility_shadow(
    phase1a, dat, params, min_isi_sec = 0.001
  )
  attr(dat, "multitrack_shadow") <- phase1a
  attr(dat, "multitrack_compatibility_shadow") <- phase1b
  params_hash <- stpd_params_hash(params)
  ds <- list(
    trains = list(train_1 = dat), params_effective = params,
    results = list(run_metadata = data.frame(
      run_id = "gate_b_final_run", params_hash = params_hash,
      stringsAsFactors = FALSE
    ))
  )
  ds$results$multitrack_preview <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      ds, params, run_id = "gate_b_final_run", params_hash = params_hash
    )
  ds <- SpikeTrainPatternDetector:::stpd_multitrack_auto_attach(
    ds, params, "train_1", "gate_b_final_run", params_hash
  )
  ds <- SpikeTrainPatternDetector:::stpd_multitrack_final_attach(ds)
  review <- ds$results$multitrack_preview$intervals
  review <- review[review$semantic_track == "review" &
                     review$active_in_preview, , drop = FALSE]
  list(
    ds = ds,
    request = data.frame(
      train = review$train,
      source_review_interval_id = review$interval_id,
      stringsAsFactors = FALSE
    )
  )
}

gate_b_final_confirm <- function(fixture) {
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    reviewer = "blind-reviewer", reason = "predeclared adjudication"
  )
}

gate_b_final_reseal <- function(product) {
  payload <- setdiff(names(product), c("metadata", "manifest"))
  product$metadata$product_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_auto_hash(product[payload])
  tables <- product[setdiff(names(product), "manifest")]
  product$manifest <-
    SpikeTrainPatternDetector:::stpd_multitrack_final_manifest(
      tables, product$metadata$run_id, product$metadata$params_hash
    )
  product
}

test_that("Gate B FINAL is exact-parent-bound and pending before promotion", {
  fixture <- gate_b_final_fixture()
  final <- stpd_multitrack_final(fixture$ds)
  auto <- stpd_multitrack_auto(fixture$ds)

  expect_identical(final$metadata$schema_version, "stpd_multitrack_final_v2")
  expect_identical(final$metadata$materialization_status, "materialized")
  expect_identical(final$metadata$migration_status, "not_required")
  expect_false(final$metadata$authoritative)
  expect_identical(final$metadata$authority_scope, "none_preview")
  expect_identical(final$metadata$promotion_status, "pending")
  expect_false(final$metadata$biological_ground_truth)
  expect_identical(
    final$metadata$parent_auto_product_sha256, auto$metadata$product_sha256
  )
  expect_identical(final$events, auto$events)
  expect_identical(final$states, auto$states)
  expect_identical(final$gaps, auto$gaps)
  expect_identical(final$per_isi, auto$per_isi)
})

test_that("exact Review migration adds one Burst inside HFS non-destructively", {
  applied <- gate_b_final_confirm(gate_b_final_fixture())
  final <- stpd_multitrack_final(applied$dataset)

  expect_identical(final$metadata$migration_status, "migrated_exact")
  expect_equal(nrow(final$migration_history), 1L)
  expect_identical(
    final$migration_history$legacy_transition_sha256,
    applied$transitions$transition_sha256
  )
  expect_equal(nrow(final$events), 1L)
  expect_equal(nrow(final$states), 1L)
  expect_identical(final$states$state_class, "high_frequency_spiking")
  expect_equal(nrow(final$review_event_links), 1L)
  expect_true(final$review_event_links$creates_new_event)
  expect_true(final$review_event_links$non_destructive)
  expect_equal(nrow(final$state_event_relationships), 1L)
  expect_identical(
    final$state_event_relationships$relationship_type,
    "event_contained_in_state"
  )
  inside <- final$per_isi$isi_index %in% 20:23
  expect_true(all(nzchar(final$per_isi$event_id[inside])))
  expect_true(all(nzchar(final$per_isi$state_id[inside])))
})

test_that("exact automatic Burst is linked and never double counted", {
  applied <- gate_b_final_confirm(gate_b_final_fixture(exact_event = TRUE))
  final <- stpd_multitrack_final(applied$dataset)

  expect_equal(nrow(final$events), 1L)
  expect_equal(nrow(final$review_event_links), 1L)
  expect_false(final$review_event_links$creates_new_event)
  expect_identical(final$review_event_links$event_id, final$events$event_id)
})

test_that("revocation is append-only and removes the reviewed Event projection", {
  applied <- gate_b_final_confirm(gate_b_final_fixture())
  preflight <- stpd_multitrack_review_preflight(
    applied$dataset,
    data.frame(
      train = applied$transitions$train,
      source_review_interval_id = applied$transitions$source_review_interval_id,
      stringsAsFactors = FALSE
    ), "revoke"
  )
  revoked <- stpd_multitrack_review_revoke(
    applied$dataset,
    data.frame(
      train = applied$transitions$train,
      source_review_interval_id = applied$transitions$source_review_interval_id,
      stringsAsFactors = FALSE
    ), preflight$precondition_sha256, "blind-reviewer", "withdraw decision"
  )
  final <- stpd_multitrack_final(revoked$dataset)

  expect_equal(nrow(final$migration_history), 2L)
  expect_identical(final$current_decisions$current_status, "revoked")
  expect_equal(nrow(final$events), 0L)
  expect_equal(nrow(final$review_event_links), 0L)
  expect_true(all(grepl("^[0-9a-f]{64}$",
                        final$migration_history$legacy_transition_sha256)))
})

test_that("stale legacy history becomes typed readjudication, never replay", {
  applied <- gate_b_final_confirm(gate_b_final_fixture())
  stale <- applied$dataset
  stale$results$multitrack_review <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_stale_product(
      stale$results$multitrack_review, "new detector run"
    )
  stale <- SpikeTrainPatternDetector:::stpd_multitrack_final_attach(stale)
  final <- stpd_multitrack_final(stale)

  expect_identical(final$metadata$materialization_status, "failed_closed")
  expect_identical(final$metadata$migration_status, "requires_readjudication")
  expect_identical(final$metadata$failure_code,
                   "legacy_review_parent_is_not_current")
  expect_equal(nrow(final$events), 0L)
  expect_equal(nrow(final$states), 0L)
  expect_equal(nrow(final$per_isi), 0L)
})

test_that("resealed coordinated relationship and per-ISI tampering is rejected", {
  applied <- gate_b_final_confirm(gate_b_final_fixture())
  product <- stpd_multitrack_final(applied$dataset)

  missing_relation <- product
  missing_relation$state_event_relationships <-
    missing_relation$state_event_relationships[0, , drop = FALSE]
  missing_relation$metadata$state_event_relationships_n <- 0L
  missing_relation <- gate_b_final_reseal(missing_relation)
  error <- tryCatch(stpd_multitrack_final(missing_relation), error = function(e) e)
  expect_s3_class(error, "stpd_multitrack_final_error")
  expect_identical(error$code, "final_relationship_closure_invalid")

  altered_per_isi <- product
  altered_per_isi$per_isi$event_id[20] <- ""
  altered_per_isi$per_isi$event_family[20] <- ""
  altered_per_isi$per_isi$event_extent_class[20] <- ""
  altered_per_isi$per_isi$event_frequency_class[20] <- ""
  altered_per_isi <- gate_b_final_reseal(altered_per_isi)
  error <- tryCatch(stpd_multitrack_final(altered_per_isi), error = function(e) e)
  expect_s3_class(error, "stpd_multitrack_final_error")
  expect_identical(error$code, "final_per_isi_closure_invalid")
})
