phase2c_fk_candidate <- function(id, label, start, end, score, priority) {
  data.frame(
    candidate_id = id,
    candidate_layer = "phase2c_export_fk_fixture",
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

phase2c_export_fk_fixture <- function() {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  pool <- dplyr::bind_rows(
    phase2c_fk_candidate(
      "review_source", "possible_burst", 20L, 23L, 12, 300
    ),
    phase2c_fk_candidate(
      "hfs_state", "high_frequency_spiking", 10L, 45L, 20, 900
    ),
    phase2c_fk_candidate(
      "existing_burst", "burst", 20L, 23L, 30, 1200
    )
  )
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
    patterns = c("burst", "possible_burst", "high_frequency_spiking"),
    params = params
  )
  phase1b <- SpikeTrainPatternDetector:::stpd_multitrack_compatibility_shadow(
    phase1a, dat = dat, params = params, variable_params = NULL,
    min_isi_sec = 0.001
  )
  attr(dat, "multitrack_shadow") <- phase1a
  attr(dat, "multitrack_compatibility_shadow") <- phase1b
  params_hash <- stpd_params_hash(params)
  run_id <- "phase2c_export_fk_test_run"
  ds <- list(
    trains = list(train_A = dat, train_B = dat),
    params_effective = params,
    results = list(run_metadata = data.frame(
      run_id = run_id,
      params_hash = params_hash,
      stringsAsFactors = FALSE
    )),
    meta = list(display_name = "phase2c_export_fk_fixture", unit_in = "s")
  )
  ds$results$multitrack_preview <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      ds, params, run_id = run_id, params_hash = params_hash
    )
  review <- ds$results$multitrack_preview$intervals
  review <- review[
    review$semantic_track == "review" & review$label == "possible_burst" &
      review$active_in_preview,
    , drop = FALSE
  ]
  stopifnot(nrow(review) == 2L)
  request <- data.frame(
    train = review$train,
    source_review_interval_id = review$interval_id,
    stringsAsFactors = FALSE
  )
  preflight <- stpd_multitrack_review_preflight(ds, request, "confirm")
  confirmed <- stpd_multitrack_review_confirm(
    ds, request, preflight$precondition_sha256,
    reviewer = "phase2c_fk_reviewer",
    reason = "validate train-qualified exact-span export relationships",
    operation_id = "phase2c_export_fk_confirm"
  )
  specs <- SpikeTrainPatternDetector:::stpd_multitrack_review_export_table_specs()
  tables <- lapply(names(specs), function(name) confirmed$product[[name]])
  names(tables) <- names(specs)
  list(
    ds = confirmed$dataset,
    preview = confirmed$dataset$results$multitrack_preview,
    product = confirmed$product,
    tables = tables
  )
}

phase2c_expect_export_fk_code <- function(call, expected_code) {
  caught <- tryCatch(
    {
      force(call)
      NULL
    },
    error = function(e) e
  )
  expect_s3_class(
    caught, "stpd_multitrack_review_export_validation_error"
  )
  expect_identical(caught$code, expected_code)
}

test_that("Phase 2C export relationships are train-qualified exact-span Event links", {
  fixture <- phase2c_export_fk_fixture()
  expect_equal(nrow(fixture$tables$final_relationships), 2L)
  expect_silent(
    SpikeTrainPatternDetector:::stpd_multitrack_review_export_validate_foreign_keys(
        fixture$tables, fixture$preview, fixture$ds
      )
  )

  state_target <- fixture$product$final_intervals[
    fixture$product$final_intervals$train == "train_A" &
      fixture$product$final_intervals$semantic_track == "state" &
      fixture$product$final_intervals$label == "high_frequency_spiking",
    , drop = FALSE
  ]
  expect_equal(nrow(state_target), 1L)
  wrong_track <- fixture$tables
  link_a <- which(wrong_track$final_relationships$train == "train_A")
  wrong_track$final_relationships$target_final_interval_id[link_a] <-
    state_target$final_interval_id
  wrong_track$final_relationships$relationship_id[link_a] <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_relationship_id(
      wrong_track$final_relationships$run_id[link_a],
      wrong_track$final_relationships$train[link_a],
      wrong_track$final_relationships$source_review_interval_id[link_a],
      wrong_track$final_relationships$target_final_interval_id[link_a]
    )
  phase2c_expect_export_fk_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_export_validate_foreign_keys(
        wrong_track, fixture$preview, fixture$ds
      ),
    "review_relationship_foreign_key_invalid"
  )

  wrong_train <- fixture$tables
  event_b <- fixture$product$final_intervals[
    fixture$product$final_intervals$train == "train_B" &
      fixture$product$final_intervals$semantic_track == "event" &
      fixture$product$final_intervals$label == "burst",
    , drop = FALSE
  ]
  expect_equal(nrow(event_b), 1L)
  wrong_train$final_relationships$target_final_interval_id[link_a] <-
    event_b$final_interval_id
  wrong_train$final_relationships$relationship_id[link_a] <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_relationship_id(
      wrong_train$final_relationships$run_id[link_a],
      wrong_train$final_relationships$train[link_a],
      wrong_train$final_relationships$source_review_interval_id[link_a],
      wrong_train$final_relationships$target_final_interval_id[link_a]
    )
  phase2c_expect_export_fk_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_export_validate_foreign_keys(
        wrong_train, fixture$preview, fixture$ds
      ),
    "review_relationship_foreign_key_invalid"
  )
})

test_that("Phase 2C manual targets must resolve to the same final Event Burst", {
  fixture <- phase2c_export_fk_fixture()
  state_target <- fixture$product$final_intervals[
    fixture$product$final_intervals$train == "train_A" &
      fixture$product$final_intervals$semantic_track == "state",
    , drop = FALSE
  ]
  expect_equal(nrow(state_target), 1L)
  wrong_manual <- fixture$tables
  manual_a <- which(wrong_manual$manual_intervals$train == "train_A")
  wrong_manual$manual_intervals$linked_event_interval_id[manual_a] <-
    state_target$final_interval_id
  phase2c_expect_export_fk_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_export_validate_foreign_keys(
        wrong_manual, fixture$preview, fixture$ds
      ),
    "review_manual_foreign_key_invalid"
  )
})

test_that("Phase 2C relationship identity, effect, and exact-span checks fail closed", {
  fixture <- phase2c_export_fk_fixture()
  link_a <- which(fixture$tables$final_relationships$train == "train_A")
  manual_a <- which(fixture$tables$manual_intervals$train == "train_A")
  event_a <- which(
    fixture$tables$final_intervals$train == "train_A" &
      fixture$tables$final_intervals$semantic_track == "event" &
      fixture$tables$final_intervals$label == "burst"
  )
  expect_length(link_a, 1L)
  expect_length(manual_a, 1L)
  expect_length(event_a, 1L)

  wrong_id <- fixture$tables
  wrong_id$final_relationships$relationship_id[link_a] <-
    "relationship_identity_was_not_recomputed"
  phase2c_expect_export_fk_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_export_validate_foreign_keys(
      wrong_id, fixture$preview, fixture$ds
    ),
    "review_relationship_foreign_key_invalid"
  )

  wrong_effect <- fixture$tables
  wrong_effect$final_relationships$action_effect[link_a] <- "create_event"
  phase2c_expect_export_fk_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_export_validate_foreign_keys(
      wrong_effect, fixture$preview, fixture$ds
    ),
    "review_relationship_foreign_key_invalid"
  )

  shifted_relationship <- fixture$tables
  shifted_relationship$final_intervals$start_isi[event_a] <- 21L
  shifted_relationship$final_intervals$n_isi[event_a] <- 3L
  phase2c_expect_export_fk_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_export_validate_foreign_keys(
      shifted_relationship, fixture$preview, fixture$ds
    ),
    "review_manual_foreign_key_invalid"
  )

  wrong_manual_effect <- fixture$tables
  wrong_manual_effect$manual_intervals$action_effect[manual_a] <- "create_event"
  phase2c_expect_export_fk_code(
    SpikeTrainPatternDetector:::stpd_multitrack_review_export_validate_foreign_keys(
      wrong_manual_effect, fixture$preview, fixture$ds
    ),
    "review_manual_foreign_key_invalid"
  )
})
