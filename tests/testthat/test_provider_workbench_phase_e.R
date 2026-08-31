e1_dataset <- function() {
  SpikeTrainPatternDetector:::make_dataset(
    "phase-e", "synthetic",
    list(t1 = data.frame(timestamp_sec = seq(0, 2, by = 0.01)))
  )
}

e1_record <- function(key, start, end, track = "event", label = "burst") {
  list(
    source_record_key = key, train_key = "t1",
    source_start = as.integer(start), source_end = as.integer(end),
    semantic_track = track, proposed_label = label,
    provider_decision = "positive", score_name = "provider_score",
    score_value = 0.8, score_direction = "higher_is_stronger",
    uncertainty_kind = "none", uncertainty_lower = NULL,
    uncertainty_upper = NULL, evidence_manifest_sha256 = NULL
  )
}

e1_bundle <- function(code = "a", version = "provider-a-1.0.0",
                      role = "automatic_prediction") {
  artifact <- list(
    schema_version = "stpd_external_interval_artifact_v1",
    provider_version = version,
    provider_code_sha256 = paste(rep(code, 64L), collapse = ""),
    information_access = "unknown",
    coordinate_profile_id = "train_row_isi_one_closed_v1",
    records = list(
      e1_record("burst-1", 10, 13),
      e1_record("burst-2", 20, 23),
      e1_record("burst-3", 30, 33),
      e1_record("broad", 8, 35, "state", "broad_hfs"),
      e1_record("pause", 40, 40, "gap", "pause")
    )
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
  stpd_import_provider_batch(e1_dataset(), request, raw)
}

e1_compose <- function(bundle, run_id = bundle$provider_runs$provider_run_id[[1L]],
                       mode = "auto", adjudication = NULL) {
  decision <- stpd_provider_composer_decision(
    bundle, run_id, mode, adjudication,
    scientific_owner = "phase-e-owner",
    rationale = "Phase E explicit provider workbench selection.",
    decided_utc = "2026-08-27T18:00:00Z"
  )
  stpd_compose_provider_science(bundle, decision, adjudication)
}

e1_error <- function(expr, code) {
  error <- tryCatch(force(expr), error = function(e) e)
  expect_s3_class(error, "stpd_provider_workbench_error")
  expect_identical(error$code, code)
  invisible(error)
}

test_that("Phase E view retains every run but selects exactly one", {
  first <- e1_bundle("a", "provider-a-1.0.0")
  second <- e1_bundle("b", "provider-b-1.0.0")
  combined <- SpikeTrainPatternDetector:::stpd_provider_combine_bundles_v1(
    list(first, second)
  )
  run_id <- first$provider_runs$provider_run_id[[1L]]
  composition <- e1_compose(combined, run_id)
  before_bundle <- serialize(combined, NULL, version = 3L)
  before_composition <- serialize(composition, NULL, version = 3L)
  view <- stpd_provider_review_view(combined, composition)

  expect_silent(stpd_validate_provider_review_view(
    combined, composition, view
  ))
  expect_equal(nrow(view$provider_catalog), 2L)
  expect_equal(sum(view$provider_catalog$selected), 1L)
  expect_identical(
    view$provider_catalog$provider_run_id[view$provider_catalog$selected],
    run_id
  )
  expect_true(all(view$provider_records$provider_run_id == run_id))
  expect_true(all(view$selected_intervals$provider_run_id == run_id))
  expect_false(any(second$candidate_intervals$candidate_id %in%
                     view$provider_records$source_record_id))
  expect_identical(view$metadata$source_mode, "auto")
  expect_identical(
    view$metadata$estimand_notice,
    "adjudicated_agreement_only_not_detector_performance"
  )
  expect_identical(serialize(combined, NULL, version = 3L), before_bundle)
  expect_identical(serialize(composition, NULL, version = 3L),
                   before_composition)
})

test_that("Phase E displays AUTO versus adjudicated reject and adjustment", {
  bundle <- e1_bundle()
  adjudication <- stpd_new_provider_adjudication(bundle)
  burst1 <- bundle$candidate_intervals[
    bundle$candidate_intervals$source_record_key == "burst-1", , drop = FALSE
  ]
  reject <- stpd_provider_review_request(
    adjudication, "reject", burst1$provider_run_id[[1L]],
    burst1$candidate_id[[1L]], "phase-e-reject", "reviewer-pseudonym",
    "Reject one provider interval.", "2026-08-27T18:01:00Z"
  )
  adjudication <- stpd_apply_provider_adjudication(
    bundle, adjudication, reject
  )
  burst2 <- bundle$candidate_intervals[
    bundle$candidate_intervals$source_record_key == "burst-2", , drop = FALSE
  ]
  adjust <- stpd_provider_review_request(
    adjudication, "adjust_bounds", burst2$provider_run_id[[1L]],
    burst2$candidate_id[[1L]], "phase-e-adjust", "reviewer-pseudonym",
    "Adjust one provider interval.", "2026-08-27T18:02:00Z",
    adjusted_start_isi = 19L, adjusted_end_isi = 24L
  )
  adjudication <- stpd_apply_provider_adjudication(
    bundle, adjudication, adjust, dataset = e1_dataset()
  )
  composition <- e1_compose(bundle, mode = "adjudicated",
                            adjudication = adjudication)
  view <- stpd_provider_review_view(bundle, composition, adjudication)
  statuses <- setNames(
    view$auto_adjudicated_delta$delta_status,
    view$auto_adjudicated_delta$source_record_id
  )
  expect_identical(statuses[[burst1$candidate_id]],
                   "rejected_by_adjudication")
  expect_identical(statuses[[burst2$candidate_id]],
                   "bounds_adjusted_by_adjudication")
  changed <- view$auto_adjudicated_delta[
    view$auto_adjudicated_delta$source_record_id == burst2$candidate_id,
    , drop = FALSE
  ]
  expect_identical(changed$start_shift_isi, -1L)
  expect_identical(changed$end_shift_isi, 1L)
  expect_identical(view$metadata$performance_use,
                   "adjudicated_agreement_only")
  expect_identical(
    view$metadata$estimand_notice,
    "adjudicated_agreement_only_not_detector_performance"
  )
  expect_true(nrow(view$relationships) > 0L)
  expect_equal(nrow(view$regimes), 0L)
})

test_that("candidate support appears only after explicit acceptance", {
  bundle <- e1_bundle(role = "candidate_support")
  adjudication <- stpd_new_provider_adjudication(bundle)
  candidate <- bundle$candidate_intervals[1L, , drop = FALSE]
  request <- stpd_provider_review_request(
    adjudication, "accept_as_is", candidate$provider_run_id[[1L]],
    candidate$candidate_id[[1L]], "phase-e-accept", "reviewer-pseudonym",
    "Accept one support interval.", "2026-08-27T18:03:00Z"
  )
  adjudication <- stpd_apply_provider_adjudication(
    bundle, adjudication, request
  )
  composition <- e1_compose(bundle, mode = "adjudicated",
                            adjudication = adjudication)
  view <- stpd_provider_review_view(bundle, composition, adjudication)
  expect_equal(nrow(view$provider_records), 5L)
  expect_equal(nrow(view$selected_intervals), 1L)
  expect_identical(
    view$auto_adjudicated_delta$delta_status[
      view$auto_adjudicated_delta$source_record_id == candidate$candidate_id
    ],
    "accepted_as_is_by_adjudication"
  )
  expect_true(all(
    view$auto_adjudicated_delta$delta_status[
      view$auto_adjudicated_delta$source_record_id != candidate$candidate_id
    ] == "not_selected_without_acceptance"
  ))
})

test_that("Phase E view is fail-closed and tamper evident", {
  bundle <- e1_bundle()
  composition <- e1_compose(bundle)
  view <- stpd_provider_review_view(bundle, composition)
  tampered <- view
  tampered$provider_catalog$selected[] <- FALSE
  e1_error(
    stpd_validate_provider_review_view(
      bundle, composition, tampered, rematerialize = FALSE
    ),
    "provider_workbench_run_pooling_forbidden"
  )
  tampered <- view
  tampered$selected_intervals$canonical_end_isi[[1L]] <-
    tampered$selected_intervals$canonical_end_isi[[1L]] + 1L
  e1_error(
    stpd_validate_provider_review_view(
      bundle, composition, tampered, rematerialize = FALSE
    ),
    "provider_workbench_manifest_invalid"
  )
})

e1_reference_intervals <- function(view) {
  x <- view$selected_intervals
  data.frame(
    reference_interval_id = paste0("ref-", seq_len(nrow(x))),
    train_key = x$train_key, semantic_track = x$semantic_track,
    label = x$label, canonical_start_isi = x$canonical_start_isi,
    canonical_end_isi = x$canonical_end_isi, stringsAsFactors = FALSE
  )
}

e1_reference <- function(bundle, view,
                         authority = "adjudicated_reference_record",
                         access = "not_blinded_to_predictions",
                         blinded = FALSE) {
  stpd_provider_reference_bundle(
    e1_reference_intervals(view), "phase-e-reference",
    bundle$provider_runs$dataset_snapshot_sha256[[1L]], authority, access,
    blinded, "Synthetic Phase E contract-test reference.",
    "2026-08-27T18:10:00Z"
  )
}

test_that("normalized scoring keeps provider, label, and estimand explicit", {
  bundle <- e1_bundle()
  adjudication <- stpd_new_provider_adjudication(bundle)
  composition <- e1_compose(bundle, mode = "adjudicated",
                            adjudication = adjudication)
  view <- stpd_provider_review_view(bundle, composition, adjudication)
  reference <- e1_reference(bundle, view)
  score <- stpd_score_provider_view(
    bundle, composition, view, reference, "adjudicated_agreement", 0.5,
    adjudication
  )
  expect_silent(stpd_validate_provider_score(
    bundle, composition, view, reference, score, adjudication
  ))
  expect_identical(score$metadata$provider_run_id,
                   bundle$provider_runs$provider_run_id)
  expect_identical(score$metadata$estimand, "adjudicated_agreement")
  expect_false(score$metadata$provider_pooling)
  expect_false(score$metadata$label_pooling)
  expect_true(all(score$metrics_by_label$precision == 1))
  expect_true(all(score$metrics_by_label$recall == 1))
  expect_true(all(score$metrics_by_label$f1 == 1))
  expect_false(any(score$fragmentation$false_split))
})

test_that("detector-performance authority cannot be self-certified", {
  bundle <- e1_bundle()
  composition <- e1_compose(bundle)
  view <- stpd_provider_review_view(bundle, composition)
  reference <- e1_reference(
    bundle, view, "independent_reference_standard",
    "blinded_to_predictions", TRUE
  )
  e1_error(
    stpd_score_provider_view(
      bundle, composition, view, reference, "detector_performance"
    ),
    "provider_score_estimand_authority_invalid"
  )
  internally_built <- SpikeTrainPatternDetector:::stpd_provider_score_build(
    view, reference, "adjudicated_agreement", 0.5
  )
  e1_error(
    stpd_validate_provider_score(
      bundle, composition, view, reference, internally_built
    ),
    "provider_score_estimand_authority_invalid"
  )
})

test_that("reference and score products are tamper evident", {
  bundle <- e1_bundle()
  adjudication <- stpd_new_provider_adjudication(bundle)
  composition <- e1_compose(bundle, mode = "adjudicated",
                            adjudication = adjudication)
  view <- stpd_provider_review_view(bundle, composition, adjudication)
  reference <- e1_reference(bundle, view)
  tampered_reference <- reference
  tampered_reference$intervals$canonical_end_isi[[1L]] <-
    tampered_reference$intervals$canonical_end_isi[[1L]] + 1L
  e1_error(
    SpikeTrainPatternDetector:::stpd_validate_provider_reference_bundle(
      tampered_reference
    ),
    "provider_reference_manifest_invalid"
  )
  score <- stpd_score_provider_view(
    bundle, composition, view, reference, "adjudicated_agreement",
    adjudication = adjudication
  )
  tampered_score <- score
  tampered_score$metadata$label_pooling <- TRUE
  e1_error(
    stpd_validate_provider_score(
      bundle, composition, view, reference, tampered_score, adjudication
    ),
    "provider_score_identity_invalid"
  )
})

test_that("transactional export writes completion manifest last", {
  bundle <- e1_bundle()
  adjudication <- stpd_new_provider_adjudication(bundle)
  composition <- e1_compose(bundle, mode = "adjudicated",
                            adjudication = adjudication)
  view <- stpd_provider_review_view(bundle, composition, adjudication)
  reference <- e1_reference(bundle, view)
  score <- stpd_score_provider_view(
    bundle, composition, view, reference, "adjudicated_agreement",
    adjudication = adjudication
  )
  out <- tempfile("phase-e-provider-export-")
  on.exit(if (dir.exists(out)) unlink(out, recursive = TRUE), add = TRUE)
  expect_identical(
    stpd_write_provider_workbench(
      bundle, composition, view, out, adjudication, score, reference
    ),
    normalizePath(out, mustWork = TRUE)
  )
  expect_true(file.exists(file.path(out, "completion_manifest.json")))
  expect_true(all(file.exists(file.path(out, c(
    "provider_bundle.rds", "provider_adjudication.rds",
    "provider_composition.rds", "provider_workbench_view.rds",
    "provider_reference.rds", "provider_score.rds"
  )))))
  manifest <- jsonlite::read_json(file.path(out, "completion_manifest.json"))
  expect_identical(
    manifest$completion_status,
    "complete_after_all_payloads_verified"
  )
  expect_identical(manifest$view_product_sha256,
                   view$metadata$product_sha256[[1L]])

  invalid <- score
  invalid$metadata$provider_pooling <- TRUE
  blocked <- tempfile("phase-e-provider-export-blocked-")
  e1_error(
    stpd_write_provider_workbench(
      bundle, composition, view, blocked, adjudication, invalid, reference
    ),
    "provider_score_identity_invalid"
  )
  expect_false(dir.exists(blocked))
})

test_that("Shiny workbench imports a validated bundle and builds one-run view", {
  bundle <- e1_bundle()
  bundle_file <- tempfile(fileext = ".rds")
  saveRDS(bundle, bundle_file, version = 3L)
  on.exit(unlink(bundle_file), add = TRUE)

  mini_server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      provider_bundle = NULL, provider_adjudication = NULL,
      provider_composition = NULL, provider_review_view = NULL,
      provider_reference = NULL, provider_score = NULL,
      provider_workbench_message = NULL
    )
    ui_language <- function() "en"
    ui_current_copy <- function(zh, en, lang = ui_language()) en
    ui_condition_detail <- function(e) conditionMessage(e)
    stpd_server_install_provider_workbench_module(environment())
  }

  shiny::testServer(mini_server, {
    session$setInputs(provider_bundle_in = list(
      name = "provider_bundle.rds", size = file.info(bundle_file)$size,
      type = "application/octet-stream", datapath = bundle_file
    ))
    session$flushReact()
    expect_silent(stpd_validate_provider_bundle(rv$provider_bundle))
    expect_null(rv$provider_review_view)

    run_id <- bundle$provider_runs$provider_run_id[[1L]]
    session$setInputs(
      provider_selected_run = run_id, provider_source_mode = "auto",
      provider_scientific_owner = "phase-e-ui-owner",
      provider_composition_rationale = "Explicit UI contract test.",
      provider_build_view = 1L
    )
    session$flushReact()
    expect_silent(stpd_validate_provider_review_view(
      rv$provider_bundle, rv$provider_composition, rv$provider_review_view
    ))
    expect_identical(rv$provider_review_view$metadata$provider_run_id, run_id)
    expect_equal(sum(rv$provider_review_view$provider_catalog$selected), 1L)

    record_id <- bundle$candidate_intervals$candidate_id[[1L]]
    session$setInputs(
      provider_selected_record = record_id,
      provider_review_action = "accept_as_is",
      provider_reviewer_id = "phase-e-reviewer",
      provider_review_reason = "Explicit UI acceptance contract test.",
      provider_apply_review = 1L
    )
    session$flushReact()
    expect_silent(stpd_validate_provider_adjudication(
      rv$provider_bundle, rv$provider_adjudication
    ))
    expect_null(rv$provider_review_view)
    expect_identical(
      rv$provider_adjudication$current_decisions$source_record_id, record_id
    )

    session$setInputs(provider_source_mode = "adjudicated",
                      provider_build_view = 2L)
    session$flushReact()
    expect_identical(rv$provider_review_view$metadata$source_mode,
                     "adjudicated")
    expect_true(record_id %in%
                  rv$provider_review_view$selected_intervals$source_record_id)
    expect_identical(
      rv$provider_review_view$auto_adjudicated_delta$delta_status[
        rv$provider_review_view$auto_adjudicated_delta$source_record_id ==
          record_id
      ],
      "accepted_as_is_by_adjudication"
    )
  })
})

test_that("Phase E UI exposes explicit provider and estimand controls", {
  ui_source <- paste(deparse(SpikeTrainPatternDetector:::ui,
                             width.cutoff = 500L), collapse = "\n")
  server_source <- paste(deparse(SpikeTrainPatternDetector:::server,
                                 width.cutoff = 500L), collapse = "\n")
  expect_match(ui_source, "provider_bundle_in", fixed = TRUE)
  expect_match(ui_source, "provider_source_mode", fixed = TRUE)
  expect_match(ui_source, "provider_score_estimand", fixed = TRUE)
  expect_match(ui_source, "provider_workbench_out", fixed = TRUE)
  expect_match(server_source, "stpd_server_install_provider_workbench_module",
               fixed = TRUE)
})
