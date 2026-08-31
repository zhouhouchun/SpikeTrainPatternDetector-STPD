active_entry_error <- function(expr) tryCatch(expr, error = identity)

active_entry_collector <- function(
    train = "train_1", level = "full", run_id = "run_active_entry") {
  collector <- stpd_candidate_lineage_collector_new(level)
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("b", 64L), collapse = ""),
    "golden_middle_burst", train
  )
  collector
}

test_that("direct observation buffers are immutable, typed, and run-local", {
  collector <- active_entry_collector()
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  payload <- data.frame(
    train = "train_1", n_interval_rows = 12L, min_isi_sec = 0.001,
    stringsAsFactors = FALSE
  )
  first_id <- stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1", payload
  )
  second_id <- stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    unserialize(serialize(payload, NULL, version = 3L))
  )
  expect_identical(first_id, second_id)
  payload$n_interval_rows <- 999L
  snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_identical(snapshot$instrumentation_status, "direct_partial")
  expect_identical(nrow(snapshot$observation_index), 1L)
  expect_identical(snapshot$observation_index$record_n, 1L)
  expect_identical(
    snapshot$observations$hf_protected_impl_entry_v1$n_interval_rows,
    12L
  )
  snapshot$observations$hf_protected_impl_entry_v1$n_interval_rows <- 0L
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(shard)$
      observations$hf_protected_impl_entry_v1$n_interval_rows,
    12L
  )

  conflicting <- data.frame(
    train = "train_1", n_interval_rows = 13L, min_isi_sec = 0.001,
    stringsAsFactors = FALSE
  )
  before_conflict <- stpd_candidate_lineage_collector_observation_snapshot(
    shard
  )
  expect_s3_class(
    active_entry_error(stpd_candidate_lineage_collector_capture(
      shard, "hf_protected_impl_entry_v1", conflicting
    )),
    "stpd_candidate_lineage_collector_hook_conflict"
  )
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(shard),
    before_conflict
  )
  expect_s3_class(
    active_entry_error(stpd_candidate_lineage_collector_capture(
      shard, "nested_payload_v1",
      data.frame(x = I(list(list(value = 1L))))
    )),
    "stpd_candidate_lineage_collector_payload_invalid"
  )
  stpd_candidate_lineage_collector_end_train(collector, shard)
  expect_identical(collector$instrumentation_status, "direct_partial")

  result <- stpd_candidate_lineage_collector_finalize_unavailable(
    list(science = TRUE), collector
  )
  receipt <- attr(
    result$candidate_lineage_audit,
    "candidate_lineage_request_receipt", exact = TRUE
  )
  expect_identical(
    result$candidate_lineage_audit$metadata$unavailable_reason,
    "candidate_universe_unavailable"
  )
  expect_identical(receipt$instrumentation_status, "direct_partial")
  expect_true(stpd_candidate_lineage_live_validate_result(
    result, receipt$run_id, receipt$params_hash, receipt$dataset_id,
    receipt$target_trains
  ))
})

test_that("entry hook rejects live attributes and cross-pipeline semantics", {
  collector <- active_entry_collector(run_id = "run_hook_binding")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  live_payload <- data.frame(
    train = "train_1", n_interval_rows = 2L, min_isi_sec = 0.001,
    stringsAsFactors = FALSE
  )
  attr(live_payload, "live") <- new.env(parent = emptyenv())
  expect_s3_class(
    active_entry_error(stpd_candidate_lineage_collector_capture(
      shard, "hf_protected_impl_entry_v1", live_payload
    )),
    "stpd_candidate_lineage_collector_payload_invalid"
  )
  nested_live_payload <- data.frame(
    train = "train_1", n_interval_rows = 2L, min_isi_sec = 0.001,
    stringsAsFactors = FALSE
  )
  nested_rows <- attr(nested_live_payload, "row.names", exact = TRUE)
  attr(nested_rows, "live") <- new.env(parent = emptyenv())
  attr(nested_live_payload, "row.names") <- nested_rows
  expect_s3_class(
    active_entry_error(stpd_candidate_lineage_collector_capture(
      shard, "hf_protected_impl_entry_v1", nested_live_payload
    )),
    "stpd_candidate_lineage_collector_payload_invalid"
  )
  expect_s3_class(
    active_entry_error(stpd_candidate_lineage_collector_capture(
      shard, "hf_protected_impl_entry_v1",
      data.frame(train = "other_train", n_interval_rows = 2L,
                 min_isi_sec = 0.001, stringsAsFactors = FALSE)
    )),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )

  alternate_collector <- active_entry_collector(
    run_id = "run_wrong_pipeline"
  )
  alternate <- stpd_candidate_lineage_collector_begin_train(
    alternate_collector, "train_1"
  )
  stpd_candidate_lineage_collector_note_pipeline(
    alternate, "event_grammar_core", train = "train_1"
  )
  ds <- stpd_golden_test_dataset("middle_burst")
  expect_s3_class(
    active_entry_error(stpd_detect_train_hf_protected_impl(
      ds$trains$train_1, default_params(), train = "train_1",
      lock_manual = FALSE, candidate_lineage_collector = alternate
    )),
    "stpd_candidate_lineage_collector_hook_pipeline_mismatch"
  )
})

test_that("direct observation storage is hash-replayed and tamper-evident", {
  collector <- active_entry_collector(run_id = "run_observation_tamper")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    data.frame(train = "train_1", n_interval_rows = 2L,
               min_isi_sec = 0.001, stringsAsFactors = FALSE)
  )
  shard$observations$hf_protected_impl_entry_v1$n_interval_rows <- 200L
  expect_s3_class(
    active_entry_error(
      stpd_candidate_lineage_collector_observation_snapshot(shard)
    ),
    "stpd_candidate_lineage_collector_observation_hash_mismatch"
  )
})

test_that("partial observation buffers cannot self-assert direct complete", {
  collector <- active_entry_collector(run_id = "run_false_complete")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    data.frame(train = "train_1", n_interval_rows = 2L,
               min_isi_sec = 0.001, stringsAsFactors = FALSE)
  )
  shard$instrumentation_status <- "direct_complete"
  expect_s3_class(
    active_entry_error(
      stpd_candidate_lineage_collector_observation_snapshot(shard)
    ),
    "stpd_candidate_lineage_collector_observation_status_invalid"
  )
})

test_that("mixed train instrumentation coverage fails closed", {
  collector <- active_entry_collector(
    train = c("train_1", "train_2"), run_id = "run_mixed_coverage"
  )
  direct <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    direct, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_collector_capture(
    direct, "hf_protected_impl_entry_v1",
    data.frame(train = "train_1", n_interval_rows = 2L,
               min_isi_sec = 0.001, stringsAsFactors = FALSE)
  )
  stpd_candidate_lineage_collector_end_train(collector, direct)

  unobserved <- stpd_candidate_lineage_collector_begin_train(
    collector, "train_2"
  )
  stpd_candidate_lineage_collector_note_pipeline(
    unobserved, "event_grammar_core", train = "train_2"
  )
  stpd_candidate_lineage_collector_end_train(collector, unobserved)
  expect_s3_class(
    active_entry_error(
      stpd_candidate_lineage_collector_finalize_unavailable(
        list(science = TRUE), collector
      )
    ),
    "stpd_candidate_lineage_collector_instrumentation_mixed"
  )
  expect_identical(collector$state, "open")
})

test_that("hf-protected entry capture is byte-neutral and directly observed", {
  ds <- stpd_golden_test_dataset("middle_burst")
  dat <- ds$trains$train_1
  params <- default_params()
  baseline <- run_detector_one_train(
    dat, params, min_isi_sec = 0.001, train = "train_1",
    lock_manual = FALSE
  )

  collector <- active_entry_collector()
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  old_options <- options(
    stpd.test.candidate_universe_release_fail_after_hook = 2L
  )
  on.exit(options(old_options), add = TRUE)
  expect_s3_class(
    active_entry_error(run_detector_one_train(
      dat, params, min_isi_sec = 0.001, train = "train_1",
      lock_manual = FALSE, candidate_lineage_collector = shard
    )),
    "stpd_candidate_lineage_collector_candidate_universe_release_fault_injected"
  )
  failed_snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_identical(nrow(failed_snapshot$observation_index), 77L)
  expect_identical(failed_snapshot$instrumentation_status, "direct_partial")
  expect_false(exists(
    "complete_universe_release_replay_cache", shard, inherits = FALSE
  ))
  release_context <- get("complete_universe_release_context", shard)
  expect_identical(
    failed_snapshot$observation_index, release_context$prior_index
  )
  for (fault_position in c(1L, 3L)) {
    expect_s3_class(
      active_entry_error(
        stpd_candidate_lineage_capture_complete_universe_release(
          shard, "train_1", .test_fail_after_hook = fault_position
        )
      ),
      "stpd_candidate_lineage_collector_candidate_universe_release_fault_injected"
    )
    expect_identical(
      stpd_candidate_lineage_collector_observation_snapshot(shard),
      failed_snapshot
    )
    expect_false(exists(
      "complete_universe_release_replay_cache", shard, inherits = FALSE
    ))
  }
  options(stpd.test.candidate_universe_release_fail_after_hook = NULL)
  stpd_candidate_lineage_capture_complete_universe_release(shard, "train_1")
  observed <- get("complete_universe_release_context", shard)$final_train_product
  expect_identical(observed, baseline)
  snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_identical(
    snapshot$observation_index$hook_id,
    c(
      "hf_protected_impl_entry_v1",
      "pause_raw_entry_v1",
      "pause_raw_support_v1",
      "pause_raw_runs_v1",
      "pause_raw_output_v1",
      "pause_boundary_projection_sources_v1",
      "pause_boundary_projection_output_v1",
      "pause_raw_receipt_v1",
      "burst_dispatch_final_route_v1",
      "burst_structure_first_pre_cap_v1",
      "burst_structure_first_post_cap_v1",
      "burst_structure_first_scan_receipt_v1",
      "burst_detector_final_entry_v1",
      "burst_raw_threshold_support_v1",
      "burst_seed_runs_v1",
      "burst_threshold_stage4_entry_v1",
      "burst_threshold_stage4_episode_roots_v1",
      "burst_threshold_stage4_attempts_v1",
      "burst_threshold_stage4_candidates_v1",
      "burst_threshold_stage4_receipt_v1",
      "burst_union_pre_cap_v1",
      "burst_union_post_cap_v1",
      "burst_union_rank_receipt_v1",
      "burst_refractory_entry_v1",
      "burst_refractory_attempts_v1",
      "burst_refractory_output_v1",
      "burst_refractory_receipt_v1",
      "burst_pause_ownership_entry_v1",
      "burst_pause_ownership_eligibility_v1",
      "burst_pause_ownership_selection_v1",
      "burst_pause_ownership_attempts_v1",
      "burst_pause_ownership_output_v1",
      "burst_pause_ownership_receipt_v1",
      "contextual_pause_entry_v1",
      "contextual_pause_generic_input_v1",
      "contextual_pause_ownership_support_v1",
      "contextual_pause_generic_downgrade_attempts_v1",
      "contextual_pause_generic_post_downgrade_v1",
      "contextual_pause_interburst_pair_attempts_v1",
      "contextual_pause_interburst_candidates_v1",
      "contextual_pause_combined_output_v1",
      "contextual_pause_receipt_v1",
      "burst_pause_boundary_entry_v1",
      "burst_pause_boundary_sources_v1",
      "burst_pause_boundary_attempts_v1",
      "burst_pause_boundary_output_v1",
      "burst_pause_boundary_receipt_v1",
      "burst_hard_threshold_entry_v1",
      "burst_hard_threshold_support_v1",
      "burst_hard_threshold_runs_v1",
      "burst_hard_threshold_pre_refractory_candidates_v1",
      "burst_hard_threshold_refractory_attempts_v1",
      "burst_hard_threshold_pre_boundary_output_v1",
      "burst_hard_threshold_boundary_sources_v1",
      "burst_hard_threshold_boundary_attempts_v1",
      "burst_hard_threshold_post_boundary_output_v1",
      "burst_hard_threshold_receipt_v1",
      "gap_final_entry_v1",
      "gap_final_candidate_input_v1",
      "gap_final_arbitration_v1",
      "gap_final_materialized_output_v1",
      "gap_final_receipt_v1",
      "hfs_state_entry_v1",
      "hfs_state_candidates_v1",
      "hfs_state_support_roles_v1",
      "hfs_state_parent_selection_v1",
      "hfs_state_receipt_v1",
      "tonic_state_entry_v1",
      "tonic_state_candidates_v1",
      "tonic_state_frequency_regularity_v1",
      "tonic_state_fragment_redetection_v1",
      "tonic_state_receipt_v1",
      "nested_hfs_review_entry_v1",
      "nested_hfs_review_candidates_v1",
      "nested_hfs_review_local_contrast_v1",
      "nested_hfs_review_parent_invariance_v1",
      "nested_hfs_review_receipt_v1",
      "candidate_universe_release_manifest_v1",
      "candidate_universe_release_products_v1",
      "candidate_universe_release_receipt_v1"
    )
  )
  expect_identical(snapshot$instrumentation_status, "direct_complete")
  manifest <- snapshot$observations$candidate_universe_release_manifest_v1
  products <- snapshot$observations$candidate_universe_release_products_v1
  release <- snapshot$observations$candidate_universe_release_receipt_v1
  expect_identical(manifest$expected_prior_hook_n, 77L)
  expect_identical(manifest$observed_prior_hook_n, 77L)
  expect_identical(manifest$release_hook_n, 3L)
  expect_identical(manifest$max_observation_hook_n, 80L)
  expect_true(manifest$all_family_roots_closed)
  expect_identical(manifest$root_receipt_n, 13L)
  expect_identical(manifest$root_closure_status,
                   "all_roots_closed_with_explicit_cap_accounting")
  expect_identical(products$binding_status,
                   "exact_returned_train_product_bound")
  expect_true(all(grepl("^[0-9a-f]{64}$", unlist(products[grep(
    "_sha256$", names(products)
  )], use.names = FALSE))))
  expect_identical(release$observation_universe_status, "direct_complete")
  expect_identical(
    release$canonical_adapter_status,
    "not_materialized_adapter_mapping_unavailable"
  )
  expect_identical(release$release_detector_invocation_n, 0L)
  expect_identical(release$release_scientific_candidate_n, 0L)
  expect_identical(release$resource_scope_n, 5L)
  expect_false(release$publication_authority)
  expect_true(all(grepl("^[0-9a-f]{64}$", unlist(products[grep(
    "_sha256$", names(products)
  )], use.names = FALSE))))
  expect_true(grepl("^[0-9a-f]{64}$", release$resource_audit_sha256))
  shard$observations$candidate_universe_release_products_v1$
    final_train_product_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_s3_class(
    active_entry_error(
      stpd_candidate_lineage_collector_observation_snapshot(shard)
    ),
    "stpd_candidate_lineage_collector_observation_hash_mismatch"
  )
  shard$observations$candidate_universe_release_products_v1 <- products
  shard$observations$candidate_universe_release_receipt_v1$
    resource_audit_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_s3_class(
    active_entry_error(
      stpd_candidate_lineage_collector_observation_snapshot(shard)
    ),
    "stpd_candidate_lineage_collector_observation_hash_mismatch"
  )
  shard$observations$candidate_universe_release_receipt_v1 <- release
  shard$observations$candidate_universe_release_receipt_v1$
    publication_authority <- TRUE
  expect_s3_class(
    active_entry_error(
      stpd_candidate_lineage_collector_observation_snapshot(shard)
    ),
    "stpd_candidate_lineage_collector_observation_hash_mismatch"
  )
  shard$observations$candidate_universe_release_receipt_v1$
    publication_authority <- FALSE
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(shard)$
      instrumentation_status,
    "direct_complete"
  )
  entry <- snapshot$observations$hf_protected_impl_entry_v1
  expect_identical(entry$train, "train_1")
  expect_identical(entry$n_interval_rows, as.integer(nrow(dat)))
  expect_identical(entry$min_isi_sec, 0.001)
  stpd_candidate_lineage_collector_end_train(collector, shard)
  result <- stpd_candidate_lineage_collector_finalize_unavailable(
    list(science = TRUE), collector
  )
  receipt <- attr(
    result$candidate_lineage_audit,
    "candidate_lineage_request_receipt", exact = TRUE
  )
  expect_identical(
    result$candidate_lineage_audit$metadata$unavailable_reason,
    "adapter_mapping_unavailable"
  )
  expect_identical(receipt$instrumentation_status, "direct_complete")
  expect_true(stpd_candidate_lineage_live_validate_result(
    result, receipt$run_id, receipt$params_hash, receipt$dataset_id,
    receipt$target_trains
  ))
})

test_that("complete-universe release fails before mutation when roots are missing", {
  collector <- active_entry_collector(run_id = "run_incomplete_release")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    data.frame(train = "train_1", n_interval_rows = 2L,
               min_isi_sec = 0.001, stringsAsFactors = FALSE)
  )
  before <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_s3_class(
    active_entry_error(stpd_candidate_lineage_begin_complete_universe_release(
      shard, "train_1", data.frame(), character(), list(), data.frame(),
      data.frame(), "", data.frame(), default_params(),
      stpd_event_core_params(default_params())
    )),
    "stpd_candidate_lineage_collector_candidate_universe_prior_incomplete"
  )
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(shard), before
  )
})

test_that("uninstrumented alternate pipelines remain honestly unavailable", {
  ds <- stpd_golden_test_dataset("middle_burst")
  baseline <- stpd_detect_train_dispatch(
    ds$trains$train_1, default_params(), train = "train_1",
    lock_manual = FALSE, pipeline = "event_grammar_core"
  )
  collector <- active_entry_collector(run_id = "run_alternate_pipeline")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  observed <- stpd_detect_train_dispatch(
    ds$trains$train_1, default_params(), train = "train_1",
    lock_manual = FALSE, pipeline = "event_grammar_core",
    candidate_lineage_collector = shard
  )
  expect_identical(observed, baseline)
  snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_identical(
    snapshot$instrumentation_status, "pipeline_not_instrumented"
  )
  expect_identical(nrow(snapshot$observation_index), 0L)
  stpd_candidate_lineage_collector_end_train(collector, shard)
  result <- stpd_candidate_lineage_collector_finalize_unavailable(
    list(science = TRUE), collector
  )
  expect_identical(
    result$candidate_lineage_audit$metadata$unavailable_reason,
    "pipeline_not_instrumented"
  )
})

test_that("entry observation capture preserves RNG and options", {
  collector <- active_entry_collector(run_id = "run_active_rng")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  set.seed(9127)
  before_seed <- .Random.seed
  before_kind <- RNGkind()
  before_options <- options()
  stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    data.frame(train = "train_1", n_interval_rows = 2L,
               min_isi_sec = 0.001, stringsAsFactors = FALSE)
  )
  expect_identical(.Random.seed, before_seed)
  expect_identical(RNGkind(), before_kind)
  expect_identical(options(), before_options)
})
