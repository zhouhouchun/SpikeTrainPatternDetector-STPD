auto_candidate_test_dat <- function(n = 100L) {
  isi <- c(NA_real_, rep(0.005, n - 1L))
  pattern_auto <- rep("", n)
  pattern_auto[3:4] <- "legacy_sentinel"
  data.frame(
    idx = seq_len(n),
    timestamp_sec = c(0, cumsum(isi[-1L])),
    ISI_sec = isi,
    pattern_auto = pattern_auto,
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    stringsAsFactors = FALSE
  )
}

auto_candidate_pool <- function(event_pause_conflict = FALSE) {
  out <- data.frame(
    candidate_id = c("hfs", "burst", "long", "hf_burst", "pause", "review"),
    candidate_layer = "auto_candidate_fixture",
    candidate_source = "pre_hf_spiking_protection",
    final_label = c(
      "high_frequency_spiking", "burst", "long_burst",
      "high_frequency_burst", "pause", "possible_burst"
    ),
    start_isi = c(10L, 20L, 35L, 50L, 80L, 65L),
    end_isi = c(70L, 24L, 39L, 54L, 80L, 68L),
    n_isi = c(61L, 5L, 5L, 5L, 1L, 4L),
    score = c(50, 10, 10, 10, 10, 5),
    priority = c(1000, 1200, 1200, 1200, 300, 500),
    stringsAsFactors = FALSE
  )
  if (isTRUE(event_pause_conflict)) {
    out$start_isi[out$candidate_id == "pause"] <- 22L
    out$end_isi[out$candidate_id == "pause"] <- 22L
  }
  out
}

auto_candidate_fixture <- function(pool = auto_candidate_pool(),
                                   run_id = "auto_candidate_fixture") {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- FALSE
  dat <- auto_candidate_test_dat()
  select_shadow <- getFromNamespace(
    "stpd_multitrack_shadow_select", "SpikeTrainPatternDetector"
  )
  resolve_shadow <- getFromNamespace(
    "stpd_multitrack_compatibility_shadow", "SpikeTrainPatternDetector"
  )
  phase1a <- select_shadow(
    pool, patterns = unique(as.character(pool$final_label)), params = params
  )
  phase1b <- resolve_shadow(
    phase1a, dat = dat, params = params, min_isi_sec = 0.001
  )
  attr(dat, "multitrack_shadow") <- phase1a
  attr(dat, "multitrack_compatibility_shadow") <- phase1b
  params_hash <- stpd_params_hash(params)
  ds <- list(
    trains = list(train_1 = dat),
    results = list(
      run_metadata = data.frame(
        run_id = run_id, params_hash = params_hash,
        stringsAsFactors = FALSE
      ),
      events = data.frame(
        train = "train_1", class = "legacy_sentinel",
        stringsAsFactors = FALSE
      )
    ),
    params_effective = params
  )
  list(ds = ds, params = params, run_id = run_id, params_hash = params_hash)
}

auto_candidate_attach <- function(fixture) {
  attach <- getFromNamespace(
    "stpd_multitrack_auto_attach", "SpikeTrainPatternDetector"
  )
  attach(
    fixture$ds, fixture$params, selected_trains = "train_1",
    run_id = fixture$run_id, params_hash = fixture$params_hash
  )
}

auto_candidate_reseal <- function(product) {
  hash <- getFromNamespace(
    "stpd_multitrack_auto_hash", "SpikeTrainPatternDetector"
  )
  manifest <- getFromNamespace(
    "stpd_multitrack_auto_manifest", "SpikeTrainPatternDetector"
  )
  payload_names <- c(
    "events", "states", "gaps", "review_candidates",
    "state_event_relationships", "per_isi", "hfs_context", "invariants"
  )
  product$metadata$product_sha256 <- hash(product[payload_names])
  product$manifest <- manifest(
    product[c("metadata", payload_names)],
    product$metadata$policy_hash,
    product$metadata$run_id,
    product$metadata$params_hash
  )
  product
}

test_that("default detection materializes a pending v2 AUTO candidate", {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"), params,
    selected_trains = "train_1", collect_diagnostics = FALSE,
    label_blind = TRUE
  )
  product <- stpd_multitrack_auto(out)
  metadata <- product$metadata

  expect_identical(metadata$schema_version, "stpd_multitrack_auto_v3")
  expect_false(metadata$authoritative)
  expect_identical(metadata$authority_scope, "none_preview")
  expect_identical(metadata$intended_target_scope, "automatic_prediction_record")
  expect_identical(metadata$promotion_gate, "gate_b")
  expect_identical(metadata$promotion_status, "pending")
  expect_true(metadata$canonical_candidate_schema)
  expect_false(metadata$biological_ground_truth)
  expect_identical(metadata$materialization_status, "materialized")
  expect_true(metadata$label_blind_execution)
  expect_false(metadata$detector_performance_eligible)
  expect_identical(
    metadata$detector_performance_block_reason, "gate_b_pending"
  )
  expect_identical(
    metadata$per_isi_materialization, "full_selected_train_scope"
  )
  expect_equal(nrow(product$per_isi), nrow(out$trains$train_1))
  expect_identical(product$per_isi$isi_index, seq_len(nrow(out$trains$train_1)))
  expect_identical(
    product$per_isi$timestamp_sec,
    as.numeric(out$trains$train_1$timestamp_sec)
  )
  expect_identical(
    product$per_isi$ISI_sec,
    as.numeric(out$trains$train_1$ISI_sec)
  )
  expect_true(attr(product, "parent_binding_verified"))
  expect_match(metadata$input_sha256, "^[0-9a-f]{64}$")
  expect_match(metadata$threshold_table_sha256, "^[0-9a-f]{64}$")

  # The optional public Preview retains its historical full per-ISI behavior.
  expect_s3_class(out$results$multitrack_preview, "stpd_multitrack_public_preview")
  expect_equal(
    nrow(out$results$multitrack_preview$per_isi),
    nrow(out$trains$train_1)
  )
  expect_identical(
    out$results$run_metadata_public$multitrack_auto_promotion_status,
    "pending"
  )
})

test_that("HFS and canonical Burst Events coexist without subtype double-counting", {
  fixture <- auto_candidate_fixture()
  legacy_auto <- fixture$ds$trains$train_1$pattern_auto
  legacy_events <- fixture$ds$results$events
  legacy_hash_before <- digest::digest(
    list(pattern_auto = legacy_auto, events = legacy_events),
    algo = "sha256", serialize = TRUE
  )
  out <- auto_candidate_attach(fixture)
  product <- stpd_multitrack_auto(out)

  expect_identical(out$trains$train_1$pattern_auto, legacy_auto)
  expect_identical(out$results$events, legacy_events)
  expect_identical(
    digest::digest(
      list(
        pattern_auto = out$trains$train_1$pattern_auto,
        events = out$results$events
      ),
      algo = "sha256", serialize = TRUE
    ),
    legacy_hash_before
  )
  expect_identical(product$events$event_family, rep("burst", 3L))
  expect_identical(
    product$events$extent_class,
    c("classic", "long", "unresolved")
  )
  expect_identical(
    product$events$frequency_class,
    c("unresolved", "unresolved", "high_frequency")
  )
  expect_equal(length(unique(product$events$event_id)), 3L)
  expect_equal(nrow(product$states), 1L)
  expect_identical(product$states$state_class, "high_frequency_spiking")
  expect_equal(nrow(product$state_event_relationships), 3L)
  expect_true(all(product$state_event_relationships$non_destructive))
  expect_true(all(
    product$state_event_relationships$compatibility_rule ==
      "hfs_burst_non_destructive_coexistence"
  ))
  expect_true(product$hfs_context$state_preserved)
  expect_false(product$hfs_context$destructive_action_applied)

  policy <- getFromNamespace(
    "stpd_multitrack_preview_policy", "SpikeTrainPatternDetector"
  )({
    value <- fixture$params
    value$spiketrainpattern$multitrack_preview$enabled <- TRUE
    value
  })
  build_preview <- getFromNamespace(
    "stpd_multitrack_preview_build", "SpikeTrainPatternDetector"
  )
  default_preview <- build_preview(
    fixture$ds, fixture$params, "train_1", fixture$run_id,
    fixture$params_hash, policy
  )
  explicit_preview <- build_preview(
    fixture$ds, fixture$params, "train_1", fixture$run_id,
    fixture$params_hash, policy, materialize_per_isi = TRUE
  )
  expect_identical(default_preview, explicit_preview)
})

test_that("same-geometry subtype evidence becomes one orthogonal Event", {
  pool <- auto_candidate_pool()
  pool$start_isi[pool$candidate_id == "hf_burst"] <- 20L
  pool$end_isi[pool$candidate_id == "hf_burst"] <- 24L
  pool$n_isi[pool$candidate_id == "hf_burst"] <- 5L
  out <- auto_candidate_attach(
    auto_candidate_fixture(pool, "same_geometry_burst_hfb")
  )
  product <- stpd_multitrack_auto(out)
  merged <- product$events[
    product$events$start_isi == 20L & product$events$end_isi == 24L,
    , drop = FALSE
  ]

  expect_equal(nrow(merged), 1L)
  expect_identical(merged$event_family, "burst")
  expect_identical(merged$extent_class, "classic")
  expect_identical(merged$frequency_class, "high_frequency")
  expect_setequal(
    strsplit(merged$provenance_source_labels, ";", fixed = TRUE)[[1]],
    c("burst", "high_frequency_burst")
  )
  expect_false(merged$extent_conflict)
  expect_identical(merged$modifier_diagnostic, "")
  expect_equal(nrow(product$events), 2L)
  expect_identical(product$hfs_context$selected_burst_event_n, 2L)

  conflict_pool <- auto_candidate_pool()
  conflict_pool$start_isi[conflict_pool$candidate_id == "long"] <- 20L
  conflict_pool$end_isi[conflict_pool$candidate_id == "long"] <- 24L
  conflict_pool$n_isi[conflict_pool$candidate_id == "long"] <- 5L
  conflict <- stpd_multitrack_auto(auto_candidate_attach(
    auto_candidate_fixture(conflict_pool, "same_geometry_extent_conflict")
  ))
  merged_conflict <- conflict$events[
    conflict$events$start_isi == 20L & conflict$events$end_isi == 24L,
    , drop = FALSE
  ]
  expect_equal(nrow(merged_conflict), 1L)
  expect_identical(merged_conflict$extent_class, "unresolved")
  expect_true(merged_conflict$extent_conflict)
  expect_match(
    merged_conflict$modifier_diagnostic, "conflicting_extent_evidence"
  )
})

test_that("downstream materialization fails closed instead of re-detecting an Event", {
  fixture <- auto_candidate_fixture(auto_candidate_pool(TRUE), "pause_conflict")
  legacy_auto <- fixture$ds$trains$train_1$pattern_auto
  legacy_events <- fixture$ds$results$events

  expect_no_error(out <- auto_candidate_attach(fixture))
  product <- stpd_multitrack_auto(out)
  expect_identical(out$trains$train_1$pattern_auto, legacy_auto)
  expect_identical(out$results$events, legacy_events)
  expect_identical(product$metadata$materialization_status, "failed_closed")
  expect_identical(
    product$metadata$failure_code,
    "event_crosses_upstream_hard_boundary"
  )
  expect_identical(
    product$metadata$event_pause_conflict_policy,
    paste0(
      "canonical_pause_excluded_from_direct_support__",
      "state_episode_parent_acceptance_inherited__",
      "downstream_detector_reentry_forbidden"
    )
  )
  expect_equal(nrow(product$per_isi), 0L)
  expect_equal(nrow(product$events), 0L)
  expect_match(
    product$metadata$failure_message,
    "downstream redetection is forbidden"
  )
})

test_that("canonical Pauses exclude direct HFS support without deleting the accepted episode", {
  pool <- data.frame(
    candidate_id = c("hfs_parent", "pause_a", "pause_b", "burst_overlay"),
    candidate_layer = "auto_candidate_fixture",
    candidate_source = "pre_hf_spiking_protection",
    final_label = c("high_frequency_spiking", "pause", "pause", "burst"),
    start_isi = c(10L, 27L, 35L, 12L),
    end_isi = c(44L, 27L, 35L, 15L),
    n_isi = c(35L, 1L, 1L, 4L),
    score = c(50, 10, 10, 10),
    priority = c(1000, 300, 300, 1200),
    stringsAsFactors = FALSE
  )
  product <- stpd_multitrack_auto(auto_candidate_attach(
    auto_candidate_fixture(pool, "pause_interrupted_hfs_episode")
  ))

  hfs <- product$states[
    product$states$state_class == "high_frequency_spiking",
    , drop = FALSE
  ]
  expect_identical(hfs$start_isi, c(10L, 28L, 36L))
  expect_identical(hfs$end_isi, c(26L, 34L, 44L))
  expect_identical(hfs$n_isi, c(17L, 7L, 9L))
  expect_equal(length(unique(hfs$state_episode_id)), 1L)
  expect_true(all(hfs$episode_start_isi == 10L))
  expect_true(all(hfs$episode_end_isi == 44L))
  expect_true(all(hfs$episode_n_isi == 35L))
  expect_true(all(hfs$episode_direct_support_isi_n == 33L))
  expect_true(all(hfs$episode_pause_isi_n == 2L))
  expect_identical(hfs$support_fragment_index, 1:3)
  expect_true(all(hfs$support_fragment_n == 3L))
  expect_true(all(
    hfs$support_acceptance_basis ==
      "inherited_from_accepted_parent_episode"
  ))

  episode_id <- unique(hfs$state_episode_id)
  episode_rows <- product$per_isi$isi_index >= 10L &
    product$per_isi$isi_index <= 44L
  pause_rows <- product$per_isi$isi_index %in% c(27L, 35L)
  direct_rows <- episode_rows & !pause_rows
  expect_true(all(product$per_isi$state_episode_id[episode_rows] == episode_id))
  expect_true(all(product$per_isi$state_support_role[pause_rows] ==
    "canonical_pause_gap"))
  expect_true(all(product$per_isi$state_id[pause_rows] == ""))
  expect_true(all(nzchar(product$per_isi$gap_id[pause_rows])))
  expect_true(all(product$per_isi$state_support_role[direct_rows] ==
    "direct_support"))
  expect_true(all(nzchar(product$per_isi$state_id[direct_rows])))

  # Burst remains an Event overlay and never changes the State episode or its
  # direct-support fragments.
  expect_true(any(
    product$state_event_relationships$compatibility_rule ==
      "hfs_burst_non_destructive_coexistence"
  ))
})

test_that("invalid ISI support is a hard boundary rather than inherited State", {
  fixture <- auto_candidate_fixture(run_id = "invalid_support")
  fixture$ds$trains$train_1$ISI_sec[30L] <- NA_real_
  legacy_auto <- fixture$ds$trains$train_1$pattern_auto

  expect_no_error(out <- auto_candidate_attach(fixture))
  product <- stpd_multitrack_auto(out)
  expect_identical(out$trains$train_1$pattern_auto, legacy_auto)
  invalid_row <- product$per_isi$isi_index == 30L
  expect_true(any(invalid_row))
  expect_true(is.na(product$per_isi$ISI_sec[invalid_row]))
  expect_identical(product$per_isi$event_id[invalid_row], "")
  expect_identical(product$per_isi$state_id[invalid_row], "")
  expect_identical(product$per_isi$gap_id[invalid_row], "")
  expect_false(any(
    product$states$start_isi <= 30L & product$states$end_isi >= 30L
  ))
  expect_false(any(
    product$events$start_isi <= 30L & product$events$end_isi >= 30L
  ))
})

test_that("per-ISI interval reconstruction rejects coordinated resealing", {
  product <- stpd_multitrack_auto(auto_candidate_attach(auto_candidate_fixture()))
  expect_gt(nrow(product$events), 0L)
  event <- product$events[1L, , drop = FALSE]
  tampered <- product
  hit <- which(
    tampered$per_isi$train == event$train &
      tampered$per_isi$isi_index == event$start_isi
  )
  expect_length(hit, 1L)
  tampered$per_isi$event_id[hit] <- ""
  tampered$per_isi$event_family[hit] <- ""
  tampered$per_isi$event_extent_class[hit] <- ""
  tampered$per_isi$event_frequency_class[hit] <- ""
  tampered <- auto_candidate_reseal(tampered)
  error <- tryCatch(
    getFromNamespace(
      "stpd_multitrack_auto_validate", "SpikeTrainPatternDetector"
    )(tampered, parent = NULL),
    error = function(e) e
  )
  expect_s3_class(error, "stpd_multitrack_auto_error")
  expect_identical(error$code, "per_isi_interval_closure_invalid")
})

test_that("all registered legacy pipelines complete with an honest candidate status", {
  registry <- getFromNamespace(
    "stpd_train_pipeline_registry", "SpikeTrainPatternDetector"
  )()
  for (pipeline in names(registry)) {
    params <- default_params()
    params$detector$train_pipeline <- pipeline
    expect_no_error(out <- stpd_detect(
      stpd_golden_test_dataset("middle_burst"), params,
      selected_trains = "train_1", collect_diagnostics = FALSE
    ))
    expect_true("pattern_auto" %in% names(out$trains$train_1))
    metadata <- out$results$multitrack_auto$metadata
    expected_status <- if (identical(pipeline, "hf_protected")) {
      "materialized"
    } else {
      "not_available"
    }
    expect_identical(metadata$materialization_status, expected_status)
    if (!identical(pipeline, "hf_protected")) {
      expect_identical(
        metadata$failure_code, "source_multitrack_evidence_unavailable"
      )
      expect_false(metadata$source_multitrack_evidence_available)
    }
  }
})

test_that("parent rematerialization rejects fully resealed table tampering", {
  out <- auto_candidate_attach(auto_candidate_fixture())
  mutations <- list(
    events = function(product) {
      product$events$candidate_source[1] <- "tampered"
      product
    },
    states = function(product) {
      product$states$candidate_source[1] <- "tampered"
      product
    },
    state_event_relationships = function(product) {
      product$state_event_relationships$compatibility_rule[1] <- "tampered"
      product
    },
    per_isi = function(product) {
      product$per_isi$tampered <- rep("", nrow(product$per_isi))
      product
    }
  )

  expected_codes <- c(
    events = "event_family_invalid",
    states = "parent_rematerialization_mismatch",
    state_event_relationships =
      "state_event_relationship_closure_invalid",
    per_isi = "fixed_table_schema_invalid"
  )
  for (table in names(mutations)) {
    changed <- out
    changed$results$multitrack_auto <- auto_candidate_reseal(
      mutations[[table]](changed$results$multitrack_auto)
    )
    error <- tryCatch(stpd_multitrack_auto(changed), error = function(e) e)
    expect_s3_class(error, "stpd_multitrack_auto_error")
    expect_identical(error$code, unname(expected_codes[[table]]))
  }
})

test_that("detached resealed products fail closed on semantic corruption", {
  product <- stpd_multitrack_auto(
    auto_candidate_attach(auto_candidate_fixture())
  )
  mutations <- list(
    fixed_schema = function(value) {
      value$events$start_isi <- as.numeric(value$events$start_isi)
      value
    },
    metadata_counts = function(value) {
      value$metadata$events_n <- value$metadata$events_n + 1L
      value
    },
    event_geometry = function(value) {
      value$events$n_spikes[1] <- value$events$n_spikes[1] + 1L
      value
    },
    event_modifier = function(value) {
      value$events$frequency_class[1] <- "high_frequency"
      value
    },
    relationship_geometry = function(value) {
      value$state_event_relationships$event_overlap_fraction[1] <- 0.5
      value
    },
    hfs_context_closure = function(value) {
      value$hfs_context$selected_burst_event_n[1] <-
        value$hfs_context$selected_burst_event_n[1] + 1L
      value
    },
    hfs_context_missing = function(value) {
      value$hfs_context <- value$hfs_context[0, , drop = FALSE]
      value$metadata$hfs_context_n <- 0L
      value
    }
  )
  expected_codes <- c(
    fixed_schema = "fixed_table_schema_invalid",
    metadata_counts = "metadata_counts_invalid",
    event_geometry = "interval_geometry_invalid",
    event_modifier = "event_family_invalid",
    relationship_geometry = "state_event_relationship_closure_invalid",
    hfs_context_closure = "hfs_context_closure_invalid",
    hfs_context_missing = "hfs_context_closure_invalid"
  )

  for (name in names(mutations)) {
    changed <- auto_candidate_reseal(mutations[[name]](product))
    error <- tryCatch(stpd_multitrack_auto(changed), error = function(e) e)
    expect_s3_class(error, "stpd_multitrack_auto_error")
    expect_identical(error$code, unname(expected_codes[[name]]), info = name)
  }
})

test_that("detached resealing cannot hide a missing true Event-HFS relation", {
  product <- stpd_multitrack_auto(
    auto_candidate_attach(auto_candidate_fixture())
  )
  removed_event_id <- product$state_event_relationships$event_id[1]
  product$state_event_relationships <-
    product$state_event_relationships[-1, , drop = FALSE]
  context <- product$hfs_context[1, , drop = FALSE]
  contributor_ids <- strsplit(
    context$contributor_event_ids, ";", fixed = TRUE
  )[[1]]
  contributor_ids <- contributor_ids[contributor_ids != removed_event_id]
  contributor_ids <- sort(unique(contributor_ids), method = "radix")
  product$hfs_context$contributor_event_ids[1] <- paste(
    contributor_ids, collapse = ";"
  )
  contributor_events <- product$events[
    match(contributor_ids, product$events$event_id), , drop = FALSE
  ]
  state <- product$states[
    product$states$state_id == context$state_id, , drop = FALSE
  ]
  union_stats <- getFromNamespace(
    "stpd_multitrack_auto_hfs_union_stats", "SpikeTrainPatternDetector"
  )(contributor_events, state)
  product$hfs_context$selected_burst_event_n[1] <- union_stats$event_n
  product$hfs_context$selected_burst_group_n[1] <- union_stats$group_n
  product$hfs_context$burst_covered_isi_n[1] <- union_stats$covered_n
  product$hfs_context$burst_isi_coverage[1] <- union_stats$coverage
  product$metadata$state_event_relationships_n <- as.integer(
    nrow(product$state_event_relationships)
  )
  product <- auto_candidate_reseal(product)

  error <- tryCatch(stpd_multitrack_auto(product), error = function(e) e)
  expect_s3_class(error, "stpd_multitrack_auto_error")
  expect_identical(error$code, "state_event_relationship_closure_invalid")
})

test_that("new manual-aware and label-blind runs strip stale auto products", {
  input <- stpd_golden_test_dataset("middle_burst")
  input$results <- list(multitrack_auto = list(stale = TRUE))
  for (label_blind in c(FALSE, TRUE)) {
    expect_no_error(out <- stpd_detect(
      input, default_params(), selected_trains = "train_1",
      collect_diagnostics = FALSE, label_blind = label_blind
    ))
    expect_s3_class(
      out$results$multitrack_auto, "stpd_multitrack_auto_product"
    )
    expect_false(identical(out$results$multitrack_auto, input$results$multitrack_auto))
  }
})

test_that("legacy export omits a corrupt candidate and continues", {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"), params,
    selected_trains = "train_1", collect_diagnostics = FALSE
  )
  out$results$multitrack_auto$events$candidate_source[1] <- "tampered"
  out$results$multitrack_auto <- auto_candidate_reseal(
    out$results$multitrack_auto
  )
  directory <- tempfile("multitrack_auto_fail_soft_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  expect_message(
    expect_no_error(stpd_export_results(out, params, directory)),
    "candidate export failed"
  )
  expect_true(file.exists(file.path(directory, "Events_final.csv")))
  expect_true(file.exists(file.path(directory, "Multitrack_preview.rds")))
  expect_false(file.exists(file.path(
    directory, "Multitrack_auto_candidate.rds"
  )))
  status_path <- file.path(
    directory, "Multitrack_auto_candidate_export_status.csv"
  )
  expect_true(file.exists(status_path))
  status <- utils::read.csv(status_path, stringsAsFactors = FALSE)
  expect_identical(status$export_status, "omit")
  expect_identical(status$status_code, "candidate_export_omitted")
  expect_true(status$warning)
  expect_true(status$legacy_preview_review_export_continues)
  expect_match(status$technical_detail, "provenance|rematerialization")
})

test_that("reused output directory removes stale candidate when product is absent", {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"), params,
    selected_trains = "train_1", collect_diagnostics = FALSE
  )
  directory <- tempfile("multitrack_auto_reused_output_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  expect_no_error(stpd_export_results(out, params, directory))
  expect_true(file.exists(file.path(
    directory, "Multitrack_auto_candidate.rds"
  )))
  without_candidate <- out
  without_candidate$results$multitrack_auto <- NULL
  expect_no_error(stpd_export_results(without_candidate, params, directory))

  candidate_files <- getFromNamespace(
    "stpd_multitrack_auto_export_filenames", "SpikeTrainPatternDetector"
  )(include_status = FALSE)
  expect_false(any(file.exists(file.path(directory, candidate_files))))
  status <- utils::read.csv(
    file.path(directory, "Multitrack_auto_candidate_export_status.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(status$export_status, "not_present")
  expect_identical(status$status_code, "candidate_product_not_present")
  expect_false(status$candidate_artifacts_written)
  expect_false(status$warning)

  dangling_path <- file.path(directory, "Multitrack_auto_candidate.rds")
  expect_true(file.symlink("missing_candidate_target", dangling_path))
  expect_false(file.exists(dangling_path))
  expect_true(nzchar(Sys.readlink(dangling_path)))
  expect_no_error(stpd_export_results(without_candidate, params, directory))
  path_exists <- getFromNamespace(
    "stpd_multitrack_auto_path_exists", "SpikeTrainPatternDetector"
  )
  expect_false(path_exists(dangling_path))
  dangling_status <- utils::read.csv(
    file.path(directory, "Multitrack_auto_candidate_export_status.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(dangling_status$export_status, "not_present")
})

test_that("cleanup postcondition cannot falsely report stale files omitted", {
  directory <- tempfile("multitrack_auto_cleanup_failure_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  stale_path <- file.path(directory, "Multitrack_auto_candidate.rds")
  saveRDS(list(stale = TRUE), stale_path)
  testthat::local_mocked_bindings(
    stpd_multitrack_auto_cleanup_exports = function(out_dir) {
      invisible(character())
    },
    .package = "SpikeTrainPatternDetector"
  )

  fixture <- auto_candidate_fixture(run_id = "cleanup_failure_main_export")
  error <- tryCatch(
    stpd_export_results(fixture$ds, fixture$params, directory),
    error = function(e) e
  )
  expect_s3_class(error, "stpd_multitrack_auto_error")
  expect_identical(error$code, "candidate_export_cleanup_failed")
  expect_true(file.exists(stale_path))
  expect_false(file.exists(file.path(
    directory, "Multitrack_auto_candidate_export_status.csv"
  )))
})

test_that("candidate writer uses unambiguous pending filenames", {
  out <- auto_candidate_attach(auto_candidate_fixture())
  directory <- tempfile("multitrack_auto_candidate_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  paths <- stpd_write_multitrack_auto(out, directory)
  expected <- c(
    "Multitrack_auto_candidate_metadata.csv",
    "Multitrack_auto_candidate_events.csv",
    "Multitrack_auto_candidate_states.csv",
    "Multitrack_auto_candidate_gaps.csv",
    "Multitrack_auto_candidate_review_candidates.csv",
    "Multitrack_auto_candidate_state_event_relationships.csv",
    "Multitrack_auto_candidate_per_isi.csv",
    "Multitrack_auto_candidate_hfs_context.csv",
    "Multitrack_auto_candidate_invariants.csv",
    "Multitrack_auto_candidate_manifest.csv",
    "Multitrack_auto_candidate.rds"
  )
  expect_setequal(basename(paths), expected)
  expect_false(file.exists(file.path(directory, "Multitrack_auto.rds")))
  saved <- readRDS(file.path(directory, "Multitrack_auto_candidate.rds"))
  expect_identical(saved$metadata$promotion_status, "pending")
  expect_identical(
    saved$metadata$per_isi_materialization, "full_selected_train_scope"
  )
  expect_equal(nrow(saved$per_isi), nrow(out$trains$train_1))
})
