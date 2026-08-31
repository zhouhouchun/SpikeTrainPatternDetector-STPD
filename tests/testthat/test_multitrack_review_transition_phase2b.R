phase2b_review_params <- function() {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  params
}

phase2b_review_candidate <- function(id, label, start, end, score = 20, priority = 800) {
  data.frame(
    candidate_id = id,
    candidate_layer = "phase2b_transition_fixture",
    candidate_source = "synthetic_unit_fixture",
    final_label = label,
    start_isi = as.integer(start),
    end_isi = as.integer(end),
    n_isi = as.integer(end - start + 1L),
    score = as.numeric(score),
    priority = as.numeric(priority),
    stringsAsFactors = FALSE
  )
}

phase2b_review_fixture <- function(
    event = c("none", "exact_burst", "partial_burst", "exact_long"),
    state = c("none", "hfs", "tonic", "hft"), gap = FALSE,
    second_review = FALSE) {
  event <- match.arg(event)
  state <- match.arg(state)
  params <- phase2b_review_params()
  pool <- phase2b_review_candidate(
    "review_source", "possible_burst", 20L, 23L, score = 12, priority = 300
  )
  if (isTRUE(second_review)) {
    pool <- dplyr::bind_rows(pool, phase2b_review_candidate(
      "review_source_2", "possible_burst", 35L, 38L,
      score = 11, priority = 300
    ))
  }
  if (event == "exact_burst") {
    pool <- dplyr::bind_rows(pool, phase2b_review_candidate(
      "automatic_burst", "burst", 20L, 23L, score = 30, priority = 1200
    ))
  } else if (event == "partial_burst") {
    pool <- dplyr::bind_rows(pool, phase2b_review_candidate(
      "automatic_burst", "burst", 18L, 21L, score = 30, priority = 1200
    ))
  } else if (event == "exact_long") {
    pool <- dplyr::bind_rows(pool, phase2b_review_candidate(
      "automatic_long", "long_burst", 20L, 23L, score = 30, priority = 1200
    ))
  }
  if (state == "hfs") {
    pool <- dplyr::bind_rows(pool, phase2b_review_candidate(
      "state", "high_frequency_spiking", 10L, 45L, score = 20, priority = 900
    ))
  } else if (state == "tonic") {
    pool <- dplyr::bind_rows(pool, phase2b_review_candidate(
      "state", "tonic", 10L, 45L, score = 20, priority = 900
    ))
  } else if (state == "hft") {
    pool <- dplyr::bind_rows(pool, phase2b_review_candidate(
      "state", "high_frequency_tonic", 10L, 45L, score = 20, priority = 900
    ))
  }
  if (isTRUE(gap)) {
    pool <- dplyr::bind_rows(pool, phase2b_review_candidate(
      "pause", "pause", 22L, 22L, score = 20, priority = 1000
    ))
  }
  n <- 60L
  dat <- data.frame(
    idx = seq_len(n),
    timestamp_sec = c(0, cumsum(rep(0.04, n - 1L))),
    ISI_sec = c(NA_real_, rep(0.04, n - 1L)),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("", n),
    stringsAsFactors = FALSE
  )
  phase1a <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
    pool,
    patterns = c(
      "burst", "long_burst", "possible_burst", "high_frequency_spiking",
      "high_frequency_tonic", "tonic", "pause"
    ),
    params = params
  )
  phase1b <- SpikeTrainPatternDetector:::stpd_multitrack_compatibility_shadow(
    phase1a, dat = dat, params = params, variable_params = NULL,
    min_isi_sec = 0.001
  )
  attr(dat, "multitrack_shadow") <- phase1a
  attr(dat, "multitrack_compatibility_shadow") <- phase1b
  params_hash <- stpd_params_hash(params)
  ds <- list(
    trains = list(train_1 = dat),
    results = list(run_metadata = data.frame(
      run_id = "phase2b_transition_test_run",
      params_hash = params_hash,
      stringsAsFactors = FALSE
    )),
    meta = list(display_name = "phase2b_transition_fixture", unit_in = "s"),
    params_effective = params
  )
  ds$results$multitrack_preview <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      ds, params, run_id = "phase2b_transition_test_run",
      params_hash = params_hash
    )
  review <- ds$results$multitrack_preview$intervals
  review <- review[
    review$semantic_track == "review" & review$label == "possible_burst" &
      review$active_in_preview,
    , drop = FALSE
  ]
  stopifnot(nrow(review) == if (isTRUE(second_review)) 2L else 1L)
  list(
    ds = ds,
    params = params,
    request = data.frame(
      train = review$train,
      source_review_interval_id = review$interval_id,
      stringsAsFactors = FALSE
    )
  )
}

phase2b_expect_code <- function(expression, code) {
  error <- tryCatch(expression, error = function(e) e)
  expect_s3_class(error, "stpd_multitrack_review_error")
  expect_identical(error$code, code)
  invisible(error)
}

test_that("Phase 2B request identity is train-qualified and candidate IDs are rejected", {
  fixture <- phase2b_review_fixture()
  phase2b_expect_code(
    stpd_multitrack_review_preflight(
      fixture$ds,
      data.frame(candidate_id = "review_source", stringsAsFactors = FALSE),
      "confirm"
    ),
    "request_schema_invalid"
  )
  phase2b_expect_code(
    stpd_multitrack_review_preflight(
      fixture$ds,
      data.frame(
        source_review_interval_id = fixture$request$source_review_interval_id,
        train = "train_1",
        stringsAsFactors = FALSE
      ),
      "confirm"
    ),
    "request_schema_invalid"
  )
})

test_that("Phase 2B direct apply guards reject malformed audit inputs exactly", {
  fixture <- phase2b_review_fixture()
  before <- serialize(fixture$ds, NULL)
  missing_identity <- fixture$request
  missing_identity$train <- ""
  phase2b_expect_code(
    stpd_multitrack_review_preflight(
      fixture$ds, missing_identity, "confirm"
    ),
    "request_identity_missing"
  )

  duplicate <- dplyr::bind_rows(fixture$request, fixture$request)
  phase2b_expect_code(
    stpd_multitrack_review_preflight(fixture$ds, duplicate, "confirm"),
    "duplicate_request_identity"
  )

  phase2b_expect_code(
    stpd_multitrack_review_apply(
      fixture$ds, fixture$request, "confirm",
      expected_precondition_sha256 = "not-a-sha256",
      reviewer = "reviewer", reason = "guard fixture",
      operation_id = "phase2c_invalid_hash"
    ),
    "precondition_hash_invalid"
  )

  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  phase2b_expect_code(
    stpd_multitrack_review_apply(
      fixture$ds, fixture$request, "confirm",
      expected_precondition_sha256 = preflight$precondition_sha256,
      reviewer = "", reason = "guard fixture",
      operation_id = "phase2c_missing_reviewer"
    ),
    "reviewer_or_reason_missing"
  )
  expect_identical(serialize(fixture$ds, NULL), before)
  expect_equal(nrow(stpd_multitrack_review_history(fixture$ds)), 0L)
})

test_that("Phase 2B preflight reports lifecycle failures exactly", {
  fixture <- phase2b_review_fixture(event = "exact_burst")

  missing <- fixture$request
  missing$source_review_interval_id <- "mtr_missing_review_interval"
  missing_preflight <- stpd_multitrack_review_preflight(
    fixture$ds, missing, "confirm"
  )
  expect_false(missing_preflight$eligible)
  expect_identical(
    missing_preflight$decisions$failure_code,
    "source_review_interval_missing"
  )

  event_row <- fixture$ds$results$multitrack_preview$intervals
  event_row <- event_row[
    event_row$semantic_track == "event" & event_row$active_in_preview,
    , drop = FALSE
  ]
  expect_equal(nrow(event_row), 1L)
  ineligible <- data.frame(
    train = event_row$train,
    source_review_interval_id = event_row$interval_id,
    stringsAsFactors = FALSE
  )
  event_preflight <- stpd_multitrack_review_preflight(
    fixture$ds, ineligible, "confirm"
  )
  expect_false(event_preflight$eligible)
  expect_identical(
    event_preflight$decisions$failure_code,
    "source_review_interval_ineligible"
  )

  revoke_preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "revoke"
  )
  expect_false(revoke_preflight$eligible)
  expect_identical(
    revoke_preflight$decisions$failure_code,
    "source_review_interval_not_confirmed"
  )

  confirm_preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, confirm_preflight$precondition_sha256,
    reviewer = "phase2c_lifecycle", reason = "lifecycle fixture",
    operation_id = "phase2c_lifecycle_confirm"
  )
  repeat_preflight <- stpd_multitrack_review_preflight(
    confirmed$dataset, fixture$request, "confirm"
  )
  expect_false(repeat_preflight$eligible)
  expect_identical(
    repeat_preflight$decisions$failure_code,
    "source_review_interval_already_confirmed"
  )
})

test_that("two valid Review confirmations commit as one idempotent operation", {
  fixture <- phase2b_review_fixture(state = "hfs", second_review = TRUE)
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  expect_true(preflight$eligible)
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    reviewer = "phase2c_batch", reason = "two valid Review intervals",
    operation_id = "phase2c_atomic_success"
  )
  history <- confirmed$product$transition_history
  expect_identical(history$transition_sequence, 1:2)
  expect_identical(
    history$operation_id,
    rep("phase2c_atomic_success", 2L)
  )
  expect_identical(
    history$precondition_sha256,
    rep(preflight$precondition_sha256, 2L)
  )
  expect_identical(history$previous_transition_id[2], history$transition_id[1])
  expect_identical(
    history$previous_transition_sha256[2], history$transition_sha256[1]
  )
  expect_equal(sum(
    confirmed$product$final_intervals$semantic_track == "event" &
      confirmed$product$final_intervals$label == "burst"
  ), 2L)
  expect_true(any(
    confirmed$product$final_intervals$semantic_track == "state" &
      confirmed$product$final_intervals$label == "high_frequency_spiking"
  ))

  replay <- stpd_multitrack_review_confirm(
    confirmed$dataset, fixture$request, preflight$precondition_sha256,
    reviewer = "phase2c_batch", reason = "two valid Review intervals",
    operation_id = "phase2c_atomic_success"
  )
  expect_true(replay$idempotent_replay)
  expect_equal(nrow(replay$product$transition_history), 2L)
  expect_identical(serialize(replay$dataset, NULL), serialize(confirmed$dataset, NULL))
})

test_that("confirmation creates an Event overlay while preserving HFS and legacy outputs", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preview_before <- serialize(fixture$ds$results$multitrack_preview, NULL)
  trains_before <- serialize(fixture$ds$trains, NULL)
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  expect_true(preflight$eligible)
  expect_identical(preflight$decisions$proposed_effect, "create_event")
  result <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    reviewer = "reviewer_a", reason = "confirmed morphology",
    operation_id = "phase2b_confirm_hfs"
  )
  product <- result$product
  expect_identical(serialize(result$dataset$results$multitrack_preview, NULL), preview_before)
  expect_identical(serialize(result$dataset$trains, NULL), trains_before)
  expect_equal(sum(product$final_intervals$semantic_track == "event"), 1L)
  expect_true(any(
    product$final_intervals$semantic_track == "state" &
      product$final_intervals$label == "high_frequency_spiking"
  ))
  expect_false(any(
    product$final_intervals$semantic_track == "review" &
      product$final_intervals$final_interval_id == fixture$request$source_review_interval_id
  ))
  event_rows <- product$final_per_isi$isi_index %in% 20:23
  expect_true(all(product$final_per_isi$pattern_final_event[event_rows] == "burst"))
  expect_true(all(
    product$final_per_isi$pattern_final_state[event_rows] ==
      "high_frequency_spiking"
  ))
})

test_that("real detector candidates without candidate_source can confirm and revoke", {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  detected <- stpd_detect(
    stpd_golden_test_dataset("boundary_start"), params,
    selected_trains = "train_1", lock_manual = FALSE,
    collect_diagnostics = TRUE, label_blind = TRUE
  )
  shadow <- attr(detected$trains$train_1, "multitrack_shadow")
  expect_gt(nrow(shadow$candidates), 0L)
  expect_false("candidate_source" %in% names(shadow$candidates))
  review <- detected$results$multitrack_preview$intervals
  review <- review[
    review$semantic_track == "review" & review$label == "possible_burst" &
      review$active_in_preview,
    , drop = FALSE
  ]
  expect_equal(nrow(review), 1L)
  expect_identical(review$candidate_source, "")
  request <- data.frame(
    train = review$train,
    source_review_interval_id = review$interval_id,
    stringsAsFactors = FALSE
  )
  preflight <- stpd_multitrack_review_preflight(detected, request, "confirm")
  expect_true(preflight$eligible)
  confirmed <- stpd_multitrack_review_confirm(
    detected, request, preflight$precondition_sha256,
    reviewer = "production_shape_reviewer",
    reason = "real detector candidate provenance regression",
    operation_id = "phase2b_production_shape_confirm"
  )
  expect_equal(nrow(confirmed$product$transition_history), 1L)
  expect_identical(
    confirmed$product$transition_history$source_candidate_source, ""
  )
  expect_equal(sum(
    confirmed$product$final_intervals$semantic_track == "event" &
      confirmed$product$final_intervals$label == "burst"
  ), 1L)

  revoke_preflight <- stpd_multitrack_review_preflight(
    confirmed$dataset, request, "revoke"
  )
  expect_true(revoke_preflight$eligible)
  revoked <- stpd_multitrack_review_revoke(
    confirmed$dataset, request, revoke_preflight$precondition_sha256,
    reviewer = "production_shape_reviewer",
    reason = "real detector candidate revoke regression",
    operation_id = "phase2b_production_shape_revoke"
  )
  expect_identical(
    revoked$product$transition_history$transition_action,
    c("confirm", "revoke")
  )
  expect_identical(
    revoked$product$transition_history$source_candidate_source,
    c("", "")
  )
})

test_that("public Phase 2B accessors never return an invalid authoritative product", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    reviewer = "reviewer_accessor", reason = "accessor validation",
    operation_id = "phase2b_accessor_confirm"
  )$dataset
  expect_identical(
    stpd_multitrack_review_product(confirmed),
    confirmed$results$multitrack_review
  )
  expect_identical(
    stpd_multitrack_review_history(confirmed),
    confirmed$results$multitrack_review$transition_history
  )

  vetoed <- confirmed
  vetoed$trains$train_1$pattern_manual_negative[21L] <- "not_burst"
  phase2b_expect_code(
    stpd_multitrack_review_product(vetoed),
    "confirmed_source_now_vetoed"
  )
  phase2b_expect_code(
    stpd_multitrack_review_history(vetoed),
    "confirmed_source_now_vetoed"
  )

  stale <- SpikeTrainPatternDetector:::stpd_multitrack_review_stale_product(
    confirmed$results$multitrack_review, "accessor_rerun_test"
  )
  stale_ds <- confirmed
  stale_ds$results$multitrack_review <- stale
  expect_false(stpd_multitrack_review_product(stale_ds)$metadata$authoritative)
  expect_identical(
    stpd_multitrack_review_history(stale_ds), stale$transition_history
  )

  altered_class <- confirmed
  class(altered_class$results$multitrack_review$transition_history) <-
    c("foreign_history", "data.frame")
  phase2b_expect_code(
    stpd_multitrack_review_product(altered_class),
    "review_product_table_schema_invalid"
  )
  phase2b_expect_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_validate_history(
      altered_class$results$multitrack_review$transition_history
    ),
    "transition_history_schema_invalid"
  )
})

test_that("history validation enforces one immutable payload per atomic operation", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    reviewer = "reviewer_operation", reason = "operation invariant fixture",
    operation_id = "phase2b_operation_invariant"
  )
  first <- confirmed$transitions
  second <- first
  second$transition_sequence <- 2L
  second$source_review_interval_id <- paste0(
    second$source_review_interval_id, "_nonoverlap"
  )
  second$source_candidate_id <- "review_source_nonoverlap"
  second$source_start_isi <- 30L
  second$source_end_isi <- 33L
  second$source_row_sha256 <- paste(rep("a", 64L), collapse = "")
  second$precondition_sha256 <- paste(rep("b", 64L), collapse = "")
  second$target_event_interval_id <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_effect_interval_id(
      second$parent_preview_run_id,
      second$train,
      second$source_review_interval_id
    )
  second$previous_transition_id <- first$transition_id
  second$previous_transition_sha256 <- first$transition_sha256
  second$transition_id <- paste0(
    "mtr_transition_",
    substr(SpikeTrainPatternDetector:::stpd_multitrack_review_sha256(
      paste(
        second$operation_id, second$train,
        second$source_review_interval_id,
        second$transition_action, sep = "|"
      ), serialize = FALSE
    ), 1L, 24L)
  )
  second$transition_sha256 <- ""
  second$transition_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_transition_hash(second)
  forged <- dplyr::bind_rows(first, second)
  phase2b_expect_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_validate_history(forged),
    "transition_history_operation_payload_invalid"
  )

  zero_start <- first
  zero_start$source_start_isi <- 0L
  zero_start$transition_sha256 <- ""
  zero_start$transition_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_transition_hash(
      zero_start
    )
  phase2b_expect_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_validate_history(
      zero_start
    ),
    "transition_history_domain_invalid"
  )
})

test_that("persisted transition source and effect semantics are replayed", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    reviewer = "phase2c_semantics", reason = "semantic replay fixture",
    operation_id = "phase2c_semantic_replay"
  )
  preview <- fixture$ds$results$multitrack_preview

  wrong_effect <- confirmed$transitions
  wrong_effect$proposed_effect <- "link_existing_event"
  wrong_effect$transition_sha256 <- ""
  wrong_effect$transition_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_transition_hash(
      wrong_effect
    )
  phase2b_expect_code(
    SpikeTrainPatternDetector:::
      stpd_multitrack_review_validate_history_against_preview(
        fixture$ds, preview, wrong_effect
      ),
    "transition_effect_semantics_mismatch"
  )

  wrong_source <- confirmed$transitions
  wrong_source$source_start_isi <- wrong_source$source_start_isi + 1L
  wrong_source$transition_sha256 <- ""
  wrong_source$transition_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_transition_hash(
      wrong_source
    )
  phase2b_expect_code(
    SpikeTrainPatternDetector:::
      stpd_multitrack_review_validate_history_against_preview(
        fixture$ds, preview, wrong_source
      ),
    "transition_source_semantics_mismatch"
  )
})

test_that("full parent Preview identity scopes current decisions and idempotency", {
  first <- phase2b_review_fixture(state = "hfs")
  first_preflight <- stpd_multitrack_review_preflight(
    first$ds, first$request, "confirm"
  )
  first_confirmed <- stpd_multitrack_review_confirm(
    first$ds, first$request, first_preflight$precondition_sha256,
    reviewer = "reviewer_parent", reason = "first parent",
    operation_id = "phase2b_parent_identity_operation"
  )

  second <- phase2b_review_fixture(state = "none")
  expect_identical(
    first$ds$results$multitrack_preview$metadata$run_id,
    second$ds$results$multitrack_preview$metadata$run_id
  )
  expect_identical(
    first$ds$results$multitrack_preview$metadata$params_hash,
    second$ds$results$multitrack_preview$metadata$params_hash
  )
  expect_false(identical(
    SpikeTrainPatternDetector:::stpd_multitrack_review_preview_manifest_hash(
      first$ds$results$multitrack_preview
    ),
    SpikeTrainPatternDetector:::stpd_multitrack_review_preview_manifest_hash(
      second$ds$results$multitrack_preview
    )
  ))
  expect_identical(
    first$request$source_review_interval_id,
    second$request$source_review_interval_id
  )
  phase2b_expect_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_build_product(
      second$ds,
      second$ds$results$multitrack_preview,
      first_confirmed$product$transition_history
    ),
    "active_review_parent_history_missing"
  )

  second$ds$results$multitrack_review <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_stale_product(
      first_confirmed$product, "changed_parent_manifest"
    )
  second_preflight <- stpd_multitrack_review_preflight(
    second$ds, second$request, "confirm"
  )
  expect_true(second_preflight$eligible)
  phase2b_expect_code(
    stpd_multitrack_review_confirm(
      second$ds, second$request, first_preflight$precondition_sha256,
      reviewer = "reviewer_parent", reason = "first parent",
      operation_id = "phase2b_parent_identity_operation"
    ),
    "operation_id_parent_conflict"
  )
  second_confirmed <- stpd_multitrack_review_confirm(
    second$ds, second$request, second_preflight$precondition_sha256,
    reviewer = "reviewer_parent", reason = "second parent",
    operation_id = "phase2b_parent_identity_operation_2"
  )
  expect_equal(nrow(second_confirmed$product$transition_history), 2L)
  expect_true(second_confirmed$product$metadata$authoritative)
  reordered <- second_confirmed$dataset
  reordered$results$multitrack_review$transition_history <-
    reordered$results$multitrack_review$transition_history[2:1, , drop = FALSE]
  phase2b_expect_code(
    stpd_multitrack_review_product(reordered),
    "review_product_history_order_invalid"
  )
  phase2b_expect_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_build_product(
      first$ds,
      first$ds$results$multitrack_preview,
      second_confirmed$product$transition_history
    ),
    "active_review_parent_history_missing"
  )

  rolled_metadata <- second_confirmed$product
  first_preview <- first$ds$results$multitrack_preview
  rolled_metadata$metadata$run_id <- first_preview$metadata$run_id
  rolled_metadata$metadata$params_hash <- first_preview$metadata$params_hash
  rolled_metadata$metadata$parent_preview_schema_version <-
    first_preview$metadata$schema_version
  rolled_metadata$metadata$parent_preview_policy_hash <-
    first_preview$metadata$policy_hash
  rolled_metadata$metadata$parent_preview_manifest_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_preview_manifest_hash(
      first_preview
    )
  rolled_stale <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_stale_product(
      rolled_metadata, "forged_parent_rollback"
    )
  rolled_ds <- second_confirmed$dataset
  rolled_ds$results$multitrack_review <- rolled_stale
  phase2b_expect_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_validate_product(
      rolled_ds
    ),
    "stale_review_parent_identity_mismatch"
  )
})

test_that("an exact same-span automatic Burst is linked and counted once", {
  fixture <- phase2b_review_fixture(event = "exact_burst", state = "hfs")
  before_event_n <- sum(
    fixture$ds$results$multitrack_preview$intervals$semantic_track == "event" &
      fixture$ds$results$multitrack_preview$intervals$active_in_preview
  )
  preflight <- stpd_multitrack_review_preflight(fixture$ds, fixture$request, "confirm")
  expect_true(preflight$eligible)
  expect_identical(preflight$decisions$proposed_effect, "link_existing_event")
  result <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    reviewer = "reviewer_a", reason = "same span endorsement",
    operation_id = "phase2b_link_exact"
  )
  expect_equal(sum(result$product$final_intervals$semantic_track == "event"), before_event_n)
  expect_false(result$product$manual_intervals$creates_new_event)
  expect_identical(
    result$product$manual_intervals$linked_event_interval_id,
    preflight$decisions$target_event_interval_id
  )
  expect_equal(nrow(result$product$final_relationships), 1L)
})

test_that("partial or different-label Events fail closed atomically", {
  for (kind in c("partial_burst", "exact_long")) {
    fixture <- phase2b_review_fixture(event = kind)
    preflight <- stpd_multitrack_review_preflight(fixture$ds, fixture$request, "confirm")
    expect_false(preflight$eligible)
    expect_identical(preflight$decisions$failure_code, "event_track_conflict")
    original <- serialize(fixture$ds, NULL)
    phase2b_expect_code(
      stpd_multitrack_review_confirm(
        fixture$ds, fixture$request, preflight$precondition_sha256,
        reviewer = "reviewer_a", reason = "must fail",
        operation_id = paste0("phase2b_conflict_", kind)
      ),
      "event_track_conflict"
    )
    expect_identical(serialize(fixture$ds, NULL), original)
  }
})

test_that("Pause/not_burst veto confirmation while Burst coexists with accepted States", {
  gap <- phase2b_review_fixture(gap = TRUE)
  gap_pre <- stpd_multitrack_review_preflight(gap$ds, gap$request, "confirm")
  expect_false(gap_pre$eligible)
  expect_identical(gap_pre$decisions$failure_code, "gap_track_conflict")

  for (state in c("hfs", "tonic")) {
    fixture <- phase2b_review_fixture(state = state)
    preflight <- stpd_multitrack_review_preflight(fixture$ds, fixture$request, "confirm")
    expect_true(preflight$eligible)
    expect_identical(preflight$decisions$failure_code, "")
    expect_identical(preflight$decisions$proposed_effect, "create_event")
  }

  # HFT is subtype evidence on an accepted Broad-HFS parent and cannot create
  # an independent State.  The parent/subtype product tests cover the subtype;
  # this legacy fixture therefore remains non-blocking rather than pretending
  # that a standalone HFT candidate is a materialized State.
  hft <- phase2b_review_fixture(state = "hft")
  hft_pre <- stpd_multitrack_review_preflight(hft$ds, hft$request, "confirm")
  expect_true(hft_pre$eligible)
  expect_identical(hft_pre$decisions$failure_code, "")

  veto <- phase2b_review_fixture(state = "hfs")
  veto$ds$trains$train_1$pattern_manual_negative[21:22] <- "not_burst"
  veto_pre <- stpd_multitrack_review_preflight(veto$ds, veto$request, "confirm")
  expect_false(veto_pre$eligible)
  expect_identical(veto_pre$decisions$failure_code, "manual_not_burst_veto")
})

test_that("legacy not_burst escape hatch is vetoed and bound to the precondition", {
  fixture <- phase2b_review_fixture(state = "hfs")
  baseline <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  legacy_veto <- fixture$ds
  legacy_veto$trains$train_1$pattern_manual[21L] <- "not_burst"
  vetoed <- stpd_multitrack_review_preflight(
    legacy_veto, fixture$request, "confirm"
  )
  expect_false(vetoed$eligible)
  expect_identical(vetoed$decisions$failure_code, "manual_not_burst_veto")
  expect_false(identical(
    vetoed$decisions$manual_negative_sha256,
    baseline$decisions$manual_negative_sha256
  ))
  expect_false(identical(
    vetoed$precondition_sha256, baseline$precondition_sha256
  ))

  positive_manual <- fixture$ds
  positive_manual$trains$train_1$pattern_manual[21L] <- "burst"
  positive_pre <- stpd_multitrack_review_preflight(
    positive_manual, fixture$request, "confirm"
  )
  expect_true(positive_pre$eligible)
  expect_identical(
    positive_pre$decisions$manual_negative_sha256,
    baseline$decisions$manual_negative_sha256
  )
  expect_identical(positive_pre$precondition_sha256, baseline$precondition_sha256)

  canonical_veto <- fixture$ds
  canonical_veto$trains$train_1$pattern_manual_negative[22L] <- "hard_negative"
  canonical_pre <- stpd_multitrack_review_preflight(
    canonical_veto, fixture$request, "confirm"
  )
  expect_false(canonical_pre$eligible)
  expect_identical(
    canonical_pre$decisions$failure_code, "manual_not_burst_veto"
  )
  expect_false(identical(
    canonical_pre$decisions$manual_negative_sha256,
    vetoed$decisions$manual_negative_sha256
  ))

  outside <- fixture$ds
  outside$trains$train_1$pattern_manual[5L] <- "not_burst"
  outside_pre <- stpd_multitrack_review_preflight(
    outside, fixture$request, "confirm"
  )
  expect_true(outside_pre$eligible)
})

test_that("a new legacy-column veto invalidates the product but permits revoke", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    "legacy_veto_reviewer", "confirmation before legacy veto",
    "phase2b_legacy_veto_confirm"
  )
  challenged <- confirmed$dataset
  challenged$trains$train_1$pattern_manual[21L] <- "not-burst"
  phase2b_expect_code(
    stpd_multitrack_review_product(challenged),
    "confirmed_source_now_vetoed"
  )
  revoke_preflight <- stpd_multitrack_review_preflight(
    challenged, fixture$request, "revoke"
  )
  expect_true(revoke_preflight$eligible)
  revoked <- stpd_multitrack_review_revoke(
    challenged, fixture$request, revoke_preflight$precondition_sha256,
    "legacy_veto_reviewer", "explicit revoke after legacy veto",
    "phase2b_legacy_veto_revoke"
  )
  expect_identical(
    tail(revoked$product$transition_history$transition_action, 1L),
    "revoke"
  )
})

test_that("preconditions are stale-safe and operations are idempotent", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(fixture$ds, fixture$request, "confirm")
  changed <- fixture$ds
  changed$trains$train_1$pattern_manual_negative[20] <- "not_burst"
  phase2b_expect_code(
    stpd_multitrack_review_confirm(
      changed, fixture$request, preflight$precondition_sha256,
      reviewer = "reviewer_a", reason = "stale test",
      operation_id = "phase2b_stale"
    ),
    "stale_precondition"
  )

  first <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    reviewer = "reviewer_a", reason = "idempotency test",
    operation_id = "phase2b_idempotent"
  )
  replay <- stpd_multitrack_review_confirm(
    first$dataset, fixture$request, preflight$precondition_sha256,
    reviewer = "reviewer_a", reason = "idempotency test",
    operation_id = "phase2b_idempotent"
  )
  expect_true(replay$idempotent_replay)
  expect_identical(serialize(replay$dataset, NULL), serialize(first$dataset, NULL))
  expect_equal(nrow(stpd_multitrack_review_history(replay$dataset)), 1L)
  phase2b_expect_code(
    stpd_multitrack_review_confirm(
      first$dataset, fixture$request, preflight$precondition_sha256,
      reviewer = "reviewer_b", reason = "different payload",
      operation_id = "phase2b_idempotent"
    ),
    "operation_id_payload_conflict"
  )
})

test_that("a mixed-validity multi-request operation appends no partial history", {
  fixture <- phase2b_review_fixture(state = "hfs", second_review = TRUE)
  expect_equal(nrow(fixture$request), 2L)
  second <- fixture$ds$results$multitrack_preview$intervals[
    fixture$ds$results$multitrack_preview$intervals$interval_id ==
      fixture$request$source_review_interval_id[2],
    , drop = FALSE
  ]
  fixture$ds$trains$train_1$pattern_manual_negative[
    seq.int(as.integer(second$start_isi[1]), as.integer(second$end_isi[1]))
  ] <- "not_burst"
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  expect_false(preflight$eligible)
  expect_equal(sum(preflight$decisions$eligible), 1L)
  expect_identical(
    sort(preflight$decisions$failure_code, method = "radix"),
    c("", "manual_not_burst_veto")
  )
  before <- serialize(fixture$ds, NULL)
  phase2b_expect_code(
    stpd_multitrack_review_confirm(
      fixture$ds, fixture$request, preflight$precondition_sha256,
      reviewer = "atomic_batch_reviewer",
      reason = "mixed-validity batch must fail atomically",
      operation_id = "phase2b_atomic_mixed_batch"
    ),
    "manual_not_burst_veto"
  )
  expect_identical(serialize(fixture$ds, NULL), before)
  expect_equal(nrow(stpd_multitrack_review_history(fixture$ds)), 0L)
})

test_that("a later not_burst veto blocks use but still permits explicit revoke", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(fixture$ds, fixture$request, "confirm")
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    "reviewer_a", "initial confirmation", "phase2b_veto_then_revoke"
  )
  challenged <- confirmed$dataset
  challenged$trains$train_1$pattern_manual_negative[20:23] <- "not_burst"
  expect_error(
    stpd_multitrack_review_preflight(challenged, fixture$request, "confirm"),
    class = "stpd_multitrack_review_error"
  )
  revoke_preflight <- stpd_multitrack_review_preflight(
    challenged, fixture$request, "revoke"
  )
  expect_true(revoke_preflight$eligible)
  revoked <- stpd_multitrack_review_revoke(
    challenged, fixture$request, revoke_preflight$precondition_sha256,
    "reviewer_a", "resolved by explicit revoke", "phase2b_veto_revoke"
  )
  expect_equal(nrow(revoked$product$manual_intervals), 0L)
  expect_identical(
    tail(stpd_multitrack_review_history(revoked$dataset)$transition_action, 1L),
    "revoke"
  )
})

test_that("confirm revoke confirm is append-only and rerun/label-blind stripping is safe", {
  fixture <- phase2b_review_fixture(state = "hfs")
  initial_review_hash <-
    SpikeTrainPatternDetector:::stpd_review_state_hash(fixture$ds)
  pre1 <- stpd_multitrack_review_preflight(fixture$ds, fixture$request, "confirm")
  first <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, pre1$precondition_sha256,
    "reviewer_a", "first confirmation", "phase2b_cycle_1"
  )
  confirmed_review_hash <-
    SpikeTrainPatternDetector:::stpd_review_state_hash(first$dataset)
  expect_false(identical(confirmed_review_hash, initial_review_hash))
  pre2 <- stpd_multitrack_review_preflight(first$dataset, fixture$request, "revoke")
  second <- stpd_multitrack_review_revoke(
    first$dataset, fixture$request, pre2$precondition_sha256,
    "reviewer_a", "revoked after review", "phase2b_cycle_2"
  )
  revoked_review_hash <-
    SpikeTrainPatternDetector:::stpd_review_state_hash(second$dataset)
  expect_false(identical(revoked_review_hash, confirmed_review_hash))
  expect_false(identical(revoked_review_hash, initial_review_hash))
  expect_equal(nrow(second$product$manual_intervals), 0L)
  expect_true(any(second$product$final_intervals$semantic_track == "review"))
  pre3 <- stpd_multitrack_review_preflight(second$dataset, fixture$request, "confirm")
  third <- stpd_multitrack_review_confirm(
    second$dataset, fixture$request, pre3$precondition_sha256,
    "reviewer_b", "second confirmation", "phase2b_cycle_3"
  )
  history <- stpd_multitrack_review_history(third$dataset)
  expect_identical(history$transition_action, c("confirm", "revoke", "confirm"))
  expect_identical(history$transition_sequence, 1:3)
  expect_equal(length(unique(history$transition_sha256)), 3L)

  stale <- SpikeTrainPatternDetector:::stpd_multitrack_review_strip_for_rerun(third$dataset)
  expect_true(SpikeTrainPatternDetector:::stpd_phase2b_has_state(stale))
  expect_identical(
    stale$results$multitrack_review$metadata$lifecycle_status,
    "stale_after_detector_rerun"
  )
  expect_equal(nrow(stale$results$multitrack_review$transition_history), 3L)
  expect_equal(nrow(stale$results$multitrack_review$final_intervals), 0L)
  expect_identical(
    serialize(
      SpikeTrainPatternDetector:::stpd_multitrack_review_strip_for_rerun(stale),
      NULL
    ),
    serialize(stale, NULL)
  )

  blind <- SpikeTrainPatternDetector:::stpd_multitrack_review_strip_label_blind(third$dataset)
  expect_false(SpikeTrainPatternDetector:::stpd_phase2b_has_state(blind))
  expect_identical(
    serialize(blind$results$multitrack_preview, NULL),
    serialize(third$dataset$results$multitrack_preview, NULL)
  )
})

test_that("rerun detachment removes the whole product and restores history only", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    "reviewer_a", "archive isolation fixture", "phase2b_archive_isolation"
  )$dataset

  detached <- SpikeTrainPatternDetector:::stpd_multitrack_review_detach_for_rerun(
    confirmed
  )
  expect_false(SpikeTrainPatternDetector:::stpd_phase2b_has_state(detached$dataset))
  expect_false(is.null(detached$archive))
  expect_identical(
    detached$archive$metadata$lifecycle_status,
    "stale_after_detector_rerun"
  )
  expect_identical(detached$archive$metadata$authoritative, FALSE)
  expect_true(all(!detached$archive$table_manifest$authoritative))
  expect_equal(nrow(detached$archive$transition_history), 1L)
  expect_true(all(vapply(
    c(
      "current_decisions", "manual_intervals", "final_intervals",
      "final_relationships", "final_per_isi", "invariants"
    ),
    function(name) nrow(detached$archive[[name]]) == 0L,
    logical(1)
  )))

  restored <- SpikeTrainPatternDetector:::stpd_multitrack_review_restore_archive(
    detached$dataset, detached$archive
  )
  expect_true(SpikeTrainPatternDetector:::stpd_phase2b_has_state(restored))
  expect_identical(
    serialize(restored$results$multitrack_review, NULL),
    serialize(detached$archive, NULL)
  )

  no_state_before <- serialize(fixture$ds, NULL)
  no_state <- SpikeTrainPatternDetector:::stpd_multitrack_review_detach_for_rerun(
    fixture$ds
  )
  expect_null(no_state$archive)
  expect_identical(serialize(no_state$dataset, NULL), no_state_before)
  expect_identical(
    serialize(
      SpikeTrainPatternDetector:::stpd_multitrack_review_restore_archive(
        no_state$dataset, no_state$archive
      ),
      NULL
    ),
    no_state_before
  )

  tampered <- confirmed
  tampered$results$multitrack_review$transition_history$transition_sha256[1] <-
    paste(rep("0", 64L), collapse = "")
  phase2b_expect_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_detach_for_rerun(
      tampered
    ),
    "transition_history_hash_chain_invalid"
  )
})

test_that("product wrapper keeps Phase 2B absent from QC thresholds and detector", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    "reviewer_a", "wrapper isolation fixture", "phase2b_wrapper_isolation"
  )$dataset
  observed <- new.env(parent = emptyenv())
  observed$qc <- FALSE
  observed$thresholds <- FALSE
  observed$detector <- FALSE

  testthat::local_mocked_bindings(
    stpd_product_pre_detection_qc = function(ds, params, target_trains) {
      observed$qc <- !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      data.frame()
    },
    stpd_product_attach_dataset_thresholds = function(params, ds, target_trains) {
      observed$thresholds <-
        !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      params
    },
    run_detector_dataset_internal_base = function(
        ds, params, selected_trains = NULL, lock_manual = TRUE,
        collect_diagnostics = TRUE, progress_callback = NULL) {
      observed$detector <-
        !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      ds
    },
    .package = "SpikeTrainPatternDetector"
  )
  params <- fixture$params
  params$detector$freeze_dataset_thresholds <- TRUE
  out <- run_detector_dataset_internal(
    confirmed, params, selected_trains = "train_1",
    collect_diagnostics = FALSE
  )

  expect_true(observed$qc)
  expect_true(observed$thresholds)
  expect_true(observed$detector)
  expect_identical(
    out$results$multitrack_review$metadata$lifecycle_status,
    "stale_after_detector_rerun"
  )
  expect_identical(out$results$multitrack_review$metadata$authoritative, FALSE)
  expect_equal(nrow(out$results$multitrack_review$transition_history), 1L)
  expect_equal(nrow(out$results$multitrack_review$final_intervals), 0L)
})

test_that("direct detector boundary detaches Phase 2B before Preview stripping", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    "reviewer_a", "direct isolation fixture", "phase2b_direct_isolation"
  )$dataset
  observed_absent <- FALSE
  original_preview_strip <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_strip
  testthat::local_mocked_bindings(
    stpd_multitrack_preview_strip = function(ds) {
      observed_absent <<-
        !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      original_preview_strip(ds)
    },
    .package = "SpikeTrainPatternDetector"
  )
  out <- SpikeTrainPatternDetector:::run_detector_dataset_internal_base(
    confirmed, fixture$params, selected_trains = "train_1",
    collect_diagnostics = FALSE
  )
  expect_true(observed_absent)
  expect_identical(
    out$results$multitrack_review$metadata$lifecycle_status,
    "stale_after_detector_rerun"
  )
  expect_identical(out$results$multitrack_review$metadata$authoritative, FALSE)
  expect_equal(nrow(out$results$multitrack_review$transition_history), 1L)
  expect_equal(nrow(out$results$multitrack_review$manual_intervals), 0L)
  expect_equal(nrow(out$results$multitrack_review$final_per_isi), 0L)
})

test_that("public detector keeps review history outside every automatic report", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, fixture$request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    "reviewer_a", "outer boundary fixture", "phase2b_outer_isolation"
  )$dataset
  observed <- new.env(parent = emptyenv())
  observed$internal <- FALSE
  observed$distribution <- FALSE
  observed$reports <- logical()

  testthat::local_mocked_bindings(
    run_detector_dataset_internal = function(
        ds, params, selected_trains = NULL, lock_manual = TRUE,
        collect_diagnostics = TRUE, progress_callback = NULL) {
      observed$internal <- !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      ds$results$run_metadata <- data.frame(
        run_id = params$meta$run_id,
        params_hash = params$meta$params_hash,
        stringsAsFactors = FALSE
      )
      ds$results$candidate_ledger <- data.frame()
      ds$results$event_audit <- data.frame()
      ds$results$candidate_features <- data.frame()
      ds$results$final_decisions <- data.frame()
      ds$results$events <- data.frame()
      ds <- SpikeTrainPatternDetector:::stpd_multitrack_auto_attach(
        ds,
        params = params,
        selected_trains = selected_trains,
        run_id = params$meta$run_id,
        params_hash = params$meta$params_hash
      )
      ds
    },
    stpd_add_distributional_results = function(ds, ...) {
      observed$distribution <-
        !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      ds
    },
    stpd_result_consistency_check = function(ds, ...) {
      observed$reports <- c(
        observed$reports,
        !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      )
      data.frame()
    },
    stpd_scientific_validation_summary = function(ds, ...) {
      observed$reports <- c(
        observed$reports,
        !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      )
      data.frame()
    },
    stpd_event_level_validation = function(ds, ...) {
      observed$reports <- c(
        observed$reports,
        !SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds)
      )
      data.frame()
    },
    .package = "SpikeTrainPatternDetector"
  )
  out <- SpikeTrainPatternDetector:::stpd_detect_productized(
    confirmed, fixture$params, selected_trains = "train_1",
    collect_diagnostics = FALSE
  )
  expect_true(observed$internal)
  expect_true(observed$distribution)
  expect_length(observed$reports, 4L)
  expect_true(all(observed$reports))
  expect_identical(
    out$results$multitrack_review$metadata$lifecycle_status,
    "stale_after_detector_rerun"
  )
  expect_identical(out$results$multitrack_review$metadata$authoritative, FALSE)
  expect_equal(nrow(out$results$multitrack_review$transition_history), 1L)
  expect_equal(nrow(out$results$multitrack_review$final_intervals), 0L)
})

test_that("idempotent replay validates the history chain and fixed schema first", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(fixture$ds, fixture$request, "confirm")
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    "reviewer_a", "tamper fixture", "phase2b_tamper_replay"
  )

  bad_hash <- confirmed$dataset
  bad_hash$results$multitrack_review$transition_history$transition_sha256[1] <-
    paste(rep("0", 64L), collapse = "")
  phase2b_expect_code(
    stpd_multitrack_review_confirm(
      bad_hash, fixture$request, preflight$precondition_sha256,
      "reviewer_a", "tamper fixture", "phase2b_tamper_replay"
    ),
    "transition_history_hash_chain_invalid"
  )

  missing_column <- confirmed$dataset
  missing_column$results$multitrack_review$transition_history$source_candidate_layer <- NULL
  phase2b_expect_code(
    stpd_multitrack_review_preflight(missing_column, fixture$request, "revoke"),
    "review_product_table_schema_invalid"
  )
})

test_that("persisted product and parent Preview are deterministically revalidated", {
  fixture <- phase2b_review_fixture(state = "hfs")
  preflight <- stpd_multitrack_review_preflight(fixture$ds, fixture$request, "confirm")
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, fixture$request, preflight$precondition_sha256,
    "reviewer_a", "product tamper fixture", "phase2b_product_tamper"
  )

  tampered_product <- confirmed$dataset
  history <- tampered_product$results$multitrack_review$transition_history
  history$reviewer[1] <- "attacker"
  history$transition_sha256[1] <- ""
  history$transition_sha256[1] <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_transition_hash(history)
  tampered_product$results$multitrack_review$transition_history <- history
  # Even if the attacker refreshes the table hash in the manifest, metadata and
  # deterministic reconstruction still bind the product to its original chain.
  manifest <- tampered_product$results$multitrack_review$table_manifest
  row <- match("transition_history", manifest$table_name)
  manifest$table_sha256[row] <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_table_hash(history)
  tampered_product$results$multitrack_review$table_manifest <- manifest
  expect_error(
    stpd_multitrack_review_preflight(tampered_product, fixture$request, "revoke"),
    class = "stpd_multitrack_review_error"
  )

  tampered_preview <- fixture$ds
  index <- match(
    fixture$request$source_review_interval_id,
    tampered_preview$results$multitrack_preview$intervals$interval_id
  )
  tampered_preview$results$multitrack_preview$intervals$candidate_source[index] <-
    "tampered_source"
  preview_manifest <- tampered_preview$results$multitrack_preview$table_manifest
  interval_manifest_row <- match("intervals", preview_manifest$table_name)
  preview_manifest$table_sha256[interval_manifest_row] <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_table_hash(
      tampered_preview$results$multitrack_preview$intervals
    )
  tampered_preview$results$multitrack_preview$table_manifest <- preview_manifest
  expect_error(
    stpd_multitrack_review_preflight(tampered_preview, fixture$request, "confirm"),
    class = "stpd_multitrack_review_error"
  )
})
