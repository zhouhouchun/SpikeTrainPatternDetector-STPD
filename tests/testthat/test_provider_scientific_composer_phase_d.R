d1_dataset <- function() {
  SpikeTrainPatternDetector:::make_dataset(
    "phase-d", "synthetic",
    list(t1 = data.frame(timestamp_sec = seq(0, 2, by = 0.01)))
  )
}

d1_record <- function(key, start, end, track, label) {
  list(
    source_record_key = key, train_key = "t1",
    source_start = as.integer(start), source_end = as.integer(end),
    semantic_track = track, proposed_label = label,
    provider_decision = "positive", score_name = NULL, score_value = NULL,
    score_direction = "not_applicable", uncertainty_kind = "none",
    uncertainty_lower = NULL, uncertainty_upper = NULL,
    evidence_manifest_sha256 = NULL
  )
}

d1_records <- function() {
  list(
    d1_record("broad-1", 10, 20, "state", "broad_hfs"),
    d1_record("hft-1", 10, 20, "state", "hft"),
    d1_record("burst-1", 12, 14, "event", "burst"),
    d1_record("pause-conflict", 15, 15, "gap", "pause"),
    d1_record("pause-link", 22, 23, "gap", "pause"),
    d1_record("broad-2", 25, 35, "state", "broad_hfs"),
    d1_record("burst-2", 30, 32, "event", "burst"),
    d1_record("tonic", 38, 45, "state", "tonic"),
    d1_record("burst-3", 40, 42, "event", "burst"),
    d1_record("pause-1", 50, 50, "gap", "pause"),
    d1_record("pause-2", 55, 55, "gap", "pause"),
    d1_record("pause-3", 60, 60, "gap", "pause"),
    d1_record("pause-4", 65, 65, "gap", "pause"),
    d1_record("pause-5", 70, 70, "gap", "pause")
  )
}

d1_bundle <- function(role = "automatic_prediction") {
  artifact <- list(
    schema_version = "stpd_external_interval_artifact_v1",
    provider_version = "phase-d-fixture-1.0.0",
    provider_code_sha256 = paste(rep("d", 64L), collapse = ""),
    information_access = "unknown",
    coordinate_profile_id = "train_row_isi_one_closed_v1",
    records = d1_records()
  )
  raw <- charToRaw(enc2utf8(jsonlite::toJSON(
    artifact, auto_unbox = TRUE, null = "null", na = "null", digits = 17
  )))
  request <- list(
    schema_version = "stpd_provider_import_request_v1",
    adapter_key = "external_interval_v1", adapter_version = "1.0.0",
    output_role = role, generation_mode = "external_only",
    selected_train_keys = "t1", parameters = list()
  )
  stpd_import_provider_batch(d1_dataset(), request, raw)
}

d1_decision <- function(bundle, mode = "auto", adjudication = NULL) {
  stpd_provider_composer_decision(
    bundle, bundle$provider_runs$provider_run_id[[1L]], mode, adjudication,
    scientific_owner = "scientific-owner-pseudonym",
    rationale = "Approved provider-independent descriptive composition.",
    decided_utc = "2026-08-27T14:00:00Z"
  )
}

d1_error <- function(expr, code) {
  error <- tryCatch(force(expr), error = function(e) e)
  expect_s3_class(error, "stpd_provider_composer_error")
  expect_identical(error$code, code)
  expect_true(inherits(error, code))
  invisible(error)
}

test_that("Phase D composes orthogonal relationships without changing children", {
  bundle <- d1_bundle()
  before <- serialize(bundle, NULL, version = 3L)
  decision <- d1_decision(bundle)
  product <- stpd_compose_provider_science(bundle, decision)

  expect_silent(stpd_validate_provider_composition(bundle, product))
  expect_identical(serialize(bundle, NULL, version = 3L), before)
  expect_equal(nrow(product$children), nrow(bundle$candidate_intervals))
  expect_setequal(
    product$children$source_candidate_id,
    bundle$candidate_intervals$candidate_id
  )
  child_geometry <- product$children[
    order(product$children$source_candidate_id),
    c("source_candidate_id", "child_domain", "child_class",
      "canonical_start_isi", "canonical_end_isi",
      "canonical_start_spike", "canonical_end_spike",
      "canonical_start_time_sec", "canonical_end_time_sec"),
    drop = FALSE
  ]
  source_geometry <- bundle$candidate_intervals[
    order(bundle$candidate_intervals$candidate_id),
    c("candidate_id", "semantic_track", "proposed_label",
      "canonical_start_isi", "canonical_end_isi",
      "canonical_start_spike", "canonical_end_spike",
      "canonical_start_time_sec", "canonical_end_time_sec"),
    drop = FALSE
  ]
  names(source_geometry)[1:3] <- names(child_geometry)[1:3]
  rownames(child_geometry) <- NULL
  rownames(source_geometry) <- NULL
  expect_identical(child_geometry, source_geometry)
  expect_true(all(product$children$non_destructive))
  expect_false(product$metadata$detection_recomposition_performed)
  expect_false(product$metadata$automatic_provider_fusion)
  expect_false(product$metadata$authoritative)

  expect_true("hfs_subtype_of_broad_hfs" %in%
                product$relationships$relationship_type)
  expect_true("burst_embedded_in_carrier_state" %in%
                product$relationships$relationship_type)
  expect_true("pause_interrupted_broad_hfs" %in%
                product$relationships$relationship_type)
  expect_true("canonical_pause_overlaps_broad_hfs_direct_support" %in%
                product$conflicts$conflict_type)
  expect_identical(
    product$metadata$materialization_status,
    "materialized_with_scientific_conflicts"
  )
})

test_that("Phase D shares only the frozen legacy cadence budget", {
  policy <- SpikeTrainPatternDetector:::stpd_provider_composer_policy()
  expect_identical(
    policy$window_policy,
    SpikeTrainPatternDetector:::stpd_event_regime_window_policy()
  )
  expect_false(policy$detection_recomposition_performed)
  expect_false(policy$automatic_provider_fusion)
  expect_identical(
    policy$expansion,
    "left_to_right_one_pass_fixed_anchor_no_recursive_parent_input"
  )
})

test_that("Phase D recurrence is post-selection, bounded, and nonrecursive", {
  bundle <- d1_bundle()
  product <- stpd_compose_provider_science(bundle, d1_decision(bundle))
  expect_setequal(
    product$regimes$regime_class,
    c("recurrent_bursting", "recurrent_pausing")
  )
  burst <- product$regimes[
    product$regimes$regime_class == "recurrent_bursting", , drop = FALSE
  ]
  pause <- product$regimes[
    product$regimes$regime_class == "recurrent_pausing", , drop = FALSE
  ]
  expect_gte(burst$child_n, 3L)
  expect_identical(burst$trigger_child_n, 3L)
  expect_gte(pause$child_n, 5L)
  expect_identical(pause$trigger_child_n, 5L)
  expect_true(all(product$memberships$child_domain %in% c("event", "gap")))
  expect_false(any(product$memberships$child_domain == "regime"))
  expect_true(all(product$regimes$one_pass_nonrecursive))
  expect_true(all(product$regimes$temporal_budget_pass))
})

test_that("candidate-support cannot enter AUTO composition", {
  bundle <- d1_bundle("candidate_support")
  d1_error(d1_decision(bundle), "composer_authority_invalid")

  adjudication <- stpd_new_provider_adjudication(bundle)
  candidate <- bundle$candidate_intervals[
    bundle$candidate_intervals$source_record_key == "burst-1", , drop = FALSE
  ]
  request <- stpd_provider_review_request(
    adjudication, "accept_as_is", candidate$provider_run_id[[1L]],
    candidate$candidate_id[[1L]], "support-accept-1", "reviewer-pseudonym",
    "Explicitly accept one support interval.", "2026-08-27T14:01:00Z"
  )
  reviewed <- stpd_apply_provider_adjudication(
    bundle, adjudication, request
  )
  decision <- d1_decision(bundle, "adjudicated", reviewed)
  product <- stpd_compose_provider_science(bundle, decision, reviewed)
  expect_equal(nrow(product$children), 1L)
  expect_identical(product$children$source_candidate_id,
                   candidate$candidate_id)
  expect_identical(product$metadata$information_access, "manual_aware")
  expect_identical(product$metadata$performance_use,
                   "adjudicated_agreement_only")
})

test_that("adjudication overlays AUTO without pooling provider runs", {
  bundle <- d1_bundle()
  adjudication <- stpd_new_provider_adjudication(bundle)
  candidate <- bundle$candidate_intervals[
    bundle$candidate_intervals$source_record_key == "burst-2", , drop = FALSE
  ]
  request <- stpd_provider_review_request(
    adjudication, "reject", candidate$provider_run_id[[1L]],
    candidate$candidate_id[[1L]], "reject-burst-2", "reviewer-pseudonym",
    "Reject one provider Burst.", "2026-08-27T14:02:00Z"
  )
  reviewed <- stpd_apply_provider_adjudication(
    bundle, adjudication, request
  )
  product <- stpd_compose_provider_science(
    bundle, d1_decision(bundle, "adjudicated", reviewed), reviewed
  )
  expect_false(candidate$candidate_id %in%
                 product$children$source_candidate_id)
  expect_equal(nrow(product$children), nrow(bundle$candidate_intervals) - 1L)
  expect_true(all(product$children$provider_run_id ==
                    bundle$provider_runs$provider_run_id[[1L]]))
})

test_that("rejecting a Broad-HFS parent preserves subtype and raises conflict", {
  bundle <- d1_bundle()
  adjudication <- stpd_new_provider_adjudication(bundle)
  broad <- bundle$candidate_intervals[
    bundle$candidate_intervals$source_record_key == "broad-1", , drop = FALSE
  ]
  request <- stpd_provider_review_request(
    adjudication, "reject", broad$provider_run_id[[1L]],
    broad$candidate_id[[1L]], "reject-broad-parent", "reviewer-pseudonym",
    "Reject parent but preserve provider subtype for review.",
    "2026-08-27T14:03:00Z"
  )
  reviewed <- stpd_apply_provider_adjudication(
    bundle, adjudication, request
  )
  product <- stpd_compose_provider_science(
    bundle, d1_decision(bundle, "adjudicated", reviewed), reviewed
  )
  expect_false(broad$candidate_id %in% product$children$source_candidate_id)
  expect_true(any(product$children$child_class == "hft"))
  expect_true("hfs_subtype_parent_missing" %in%
                product$conflicts$conflict_type)
})

test_that("an explicit decision never pools a second provider run", {
  first <- d1_bundle()
  # Use the package's validated combine path with a genuinely distinct run.
  dataset <- d1_dataset()
  artifact <- list(
    schema_version = "stpd_external_interval_artifact_v1",
    provider_version = "second-provider-1.0.0",
    provider_code_sha256 = paste(rep("e", 64L), collapse = ""),
    information_access = "unknown",
    coordinate_profile_id = "train_row_isi_one_closed_v1",
    records = list(d1_record("second-burst", 80, 82, "event", "burst"))
  )
  raw <- charToRaw(enc2utf8(jsonlite::toJSON(
    artifact, auto_unbox = TRUE, null = "null", na = "null", digits = 17
  )))
  request <- list(
    schema_version = "stpd_provider_import_request_v1",
    adapter_key = "external_interval_v1", adapter_version = "1.0.0",
    output_role = "automatic_prediction", generation_mode = "external_only",
    selected_train_keys = "t1", parameters = list()
  )
  second <- stpd_import_provider_batch(dataset, request, raw)
  combined <- SpikeTrainPatternDetector:::stpd_provider_combine_bundles_v1(
    list(first, second)
  )
  decision <- stpd_provider_composer_decision(
    combined, first$provider_runs$provider_run_id[[1L]], "auto", NULL,
    "scientific-owner-pseudonym", "Select first provider only.",
    "2026-08-27T14:04:00Z"
  )
  product <- stpd_compose_provider_science(combined, decision)
  expect_true(all(product$children$provider_run_id ==
                    first$provider_runs$provider_run_id[[1L]]))
  expect_false(second$candidate_intervals$candidate_id %in%
                 product$children$source_candidate_id)
})

test_that("Phase D decision, product, and manifest are tamper evident", {
  bundle <- d1_bundle()
  decision <- d1_decision(bundle)
  changed_decision <- decision
  changed_decision$rationale <- "changed"
  d1_error(
    stpd_compose_provider_science(bundle, changed_decision),
    "composer_decision_invalid"
  )
  product <- stpd_compose_provider_science(bundle, decision)
  tampered <- product
  tampered$children$canonical_end_isi[[1L]] <-
    tampered$children$canonical_end_isi[[1L]] + 1L
  d1_error(
    stpd_validate_provider_composition(bundle, tampered),
    "composer_product_invalid"
  )
  tampered <- product
  tampered$manifest$table_sha256[[1L]] <- paste(rep("0", 64L), collapse = "")
  d1_error(
    stpd_validate_provider_composition(bundle, tampered),
    "composer_manifest_invalid"
  )
  tampered <- product
  tampered$conflicts$primary_child_id[[1L]] <- paste(rep("0", 64L), collapse = "")
  d1_error(
    stpd_validate_provider_composition(bundle, tampered),
    "composer_conflict_invalid"
  )
  tampered <- product
  tampered$children$provider_run_id[[1L]] <- paste(rep("f", 64L), collapse = "")
  d1_error(
    stpd_validate_provider_composition(
      bundle, tampered, rematerialize = FALSE
    ),
    "composer_children_invalid"
  )
})
