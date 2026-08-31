b2_internal <- function(name) {
  getFromNamespace(name, "SpikeTrainPatternDetector")
}

b2_sha <- function(letter = "a") paste(rep(letter, 64L), collapse = "")

b2_source_leaf_sha256 <- function(path) {
  size <- file.info(path)$size
  bytes <- readBin(path, what = "raw", n = size)
  normalized <- gsub("\r\n", "\n", rawToChar(bytes), fixed = TRUE)
  digest::digest(charToRaw(normalized), algo = "sha256", serialize = FALSE)
}

b2_dataset <- function(trains, id = "phase-b2") {
  trains <- lapply(trains, function(x) {
    if (is.data.frame(x)) x else data.frame(timestamp_sec = as.double(x))
  })
  SpikeTrainPatternDetector:::make_dataset(id, "synthetic", trains)
}

b2_request <- function(adapter_key, output_role = "automatic_prediction",
                       generation_mode = "external_only", trains = "t1",
                       parameters = list()) {
  list(
    schema_version = "stpd_provider_import_request_v1",
    adapter_key = adapter_key,
    adapter_version = "1.0.0",
    output_role = output_role,
    generation_mode = generation_mode,
    selected_train_keys = as.character(trains),
    parameters = parameters
  )
}

b2_expect_error <- function(expr, code) {
  error <- tryCatch(force(expr), error = function(e) e)
  expect_s3_class(error, "stpd_provider_contract_error")
  expect_true(inherits(error, code))
  expect_identical(error$code, code)
  invisible(error)
}

b2_json_raw <- function(x) {
  text <- jsonlite::toJSON(
    x, auto_unbox = TRUE, null = "null", na = "null", digits = 17,
    pretty = FALSE
  )
  out <- charToRaw(enc2utf8(text))
  attributes(out) <- NULL
  out
}

b2_interval_record <- function(
    key = "r1", train = "t1", start = 2, end = 3,
    track = "event", label = "burst", decision = "positive") {
  list(
    source_record_key = key,
    train_key = train,
    source_start = start,
    source_end = end,
    semantic_track = track,
    proposed_label = label,
    provider_decision = decision,
    score_name = NULL,
    score_value = NULL,
    score_direction = "not_applicable",
    uncertainty_kind = "none",
    uncertainty_lower = NULL,
    uncertainty_upper = NULL,
    evidence_manifest_sha256 = NULL
  )
}

b2_interval_artifact <- function(records, profile = "train_row_isi_one_closed_v1",
                                 information = "unknown",
                                 schema = "stpd_external_interval_artifact_v1",
                                 provider_version = "external-1.0.0",
                                 provider_code = b2_sha("a")) {
  b2_json_raw(list(
    schema_version = schema,
    provider_version = provider_version,
    provider_code_sha256 = provider_code,
    information_access = information,
    coordinate_profile_id = profile,
    records = records
  ))
}

b2_per_isi_record <- function(key, train, diff_index, decision) {
  list(
    source_record_key = key,
    train_key = train,
    diff_index = as.integer(diff_index),
    provider_decision = decision,
    score_name = NULL,
    score_value = NULL,
    score_direction = "not_applicable",
    uncertainty_kind = "none",
    uncertainty_lower = NULL,
    uncertainty_upper = NULL,
    evidence_manifest_sha256 = NULL
  )
}

b2_per_isi_artifact <- function(records, track = "event", label = "burst",
                                profile = "per_isi_one_closed_v1",
                                information = "unknown") {
  b2_json_raw(list(
    schema_version = "stpd_external_per_isi_artifact_v1",
    provider_version = "external-1.0.0",
    provider_code_sha256 = b2_sha("b"),
    information_access = information,
    coordinate_profile_id = profile,
    semantic_track = track,
    target_label = label,
    records = records
  ))
}

b2_import_interval <- function(dataset, artifact,
                               role = "automatic_prediction") {
  stpd_import_provider_batch(
    dataset,
    b2_request("external_interval_v1", output_role = role),
    artifact
  )
}

b2_mean_boundary_dataset <- function() {
  b2_dataset(list(
    first = cumsum(c(0, 0.004, 0.005, 0.100, 0.200)),
    last = cumsum(c(0, 0.200, 0.100, 0.004, 0.005))
  ), "mean-boundaries")
}

b2_logisi_dataset <- function() {
  set.seed(3)
  short_a <- 10^stats::rnorm(200, log10(0.005), 0.04)
  long <- 10^stats::rnorm(200, log10(0.050), 0.05)
  short_b <- 10^stats::rnorm(200, log10(0.005), 0.04)
  b2_dataset(list(t1 = cumsum(c(0, short_a, long, short_b))),
             "logisi-boundaries")
}

b2_hft_materialization_fixture <- function(n) {
  index <- seq_len(n)
  sha <- b2_sha("a")
  geometry <- data.frame(
    source_record_key = sprintf("hft-%06d", index),
    source_record_sha256 = rep(sha, n),
    normalization_audit_id = sprintf("na-%06d", index),
    train_key = rep("t1", n),
    train_timestamp_sha256 = rep(sha, n),
    canonical_start_isi = as.integer(index + 1L),
    canonical_end_isi = as.integer(index + 1L),
    canonical_start_spike = as.integer(index),
    canonical_end_spike = as.integer(index + 1L),
    canonical_start_time_sec = as.double(index - 1L) / 1000,
    canonical_end_time_sec = as.double(index) / 1000,
    stringsAsFactors = FALSE
  )
  semantics <- lapply(index, function(i) {
    list(
      source_record_key = sprintf("hft-%06d", i),
      provider_decision = "positive", semantic_track = "state",
      proposed_label = "hft", score_name = NA_character_,
      score_value = NA_real_, score_direction = "not_applicable",
      uncertainty_kind = "none", uncertainty_lower = NA_real_,
      uncertainty_upper = NA_real_, evidence_manifest_sha256 = NA_character_
    )
  })
  list(
    batch = list(
      atomic_accept = TRUE,
      normalization_audit = data.frame(row = index),
      normalized_geometry = geometry
    ),
    semantics = semantics
  )
}

test_that("Phase B2 leaves the frozen Phase-A and Phase-B1 identities unchanged", {
  expect_identical(
    stpd_provider_contract_hash(),
    "29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b"
  )
  expect_identical(
    b2_internal("stpd_provider_adapter_protocol_hash_v1")(),
    "5b0cc6ca2fe6bdf2bb7bd2bddb066632ea3b0646b1096be87c77bec292a26877"
  )
  spine <- b2_internal("stpd_provider_build_spine_v1")(
    b2_dataset(list(t1 = c(0, 0.1, 0.2, 0.3)), "b1-golden")
  )
  expect_identical(
    spine$timestamp_spine_sha256,
    "34f09f001cb4848ad14623f73ba01dd403dee599f2d9b4cf0c3ddeaba63f44e5"
  )
  expect_identical(
    spine$dataset_snapshot_sha256,
    "dceced8315cdaaf5b9bed7450c49ab5ada83d0d737de2331e4c8767ea7c3f994"
  )
})

test_that("public contract objects cannot poison internal provenance caches", {
  skip_if_not_installed("data.table")
  frozen <- "29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b"

  registry <- stpd_provider_contract_registry()
  data.table::set(
    registry$provider_allowlist, 1L, "adapter_key", "tampered-by-reference"
  )
  expect_identical(stpd_provider_contract_hash(), frozen)

  prototypes <- stpd_provider_bundle_prototypes()
  data.table::setattr(prototypes$normalization_audit, "tampered", TRUE)
  fresh_prototypes <- stpd_provider_bundle_prototypes()
  expect_null(attr(fresh_prototypes$normalization_audit, "tampered", exact = TRUE))

  audit_row <- b2_internal("stpd_provider_normalization_audit_prototype")()
  data.table::setattr(audit_row, "tampered", TRUE)
  fresh_audit_row <- b2_internal("stpd_provider_normalization_audit_prototype")()
  expect_null(attr(fresh_audit_row, "tampered", exact = TRUE))
})

test_that("registry exposes exactly five adapters and twelve frozen routes", {
  registry <- b2_internal("stpd_provider_adapter_registry_v1")()
  expect_identical(
    registry$adapters$adapter_key,
    c("native_stpd_postcomposer_v1", "mean_isi_v1", "logisi_newbd_v1",
      "external_interval_v1", "external_per_isi_v1")
  )
  expect_identical(nrow(registry$adapters), 5L)
  expect_identical(nrow(registry$routes), 12L)
  expect_identical(
    paste(registry$routes$adapter_key, registry$routes$output_role,
          registry$routes$generation_mode, sep = "|"),
    c(
      "native_stpd_postcomposer_v1|automatic_prediction|native_only",
      "native_stpd_postcomposer_v1|automatic_prediction|support_to_native",
      "mean_isi_v1|automatic_prediction|external_only",
      "mean_isi_v1|candidate_support|external_only",
      "mean_isi_v1|calibration_output|support_to_native",
      "logisi_newbd_v1|automatic_prediction|external_only",
      "logisi_newbd_v1|candidate_support|external_only",
      "logisi_newbd_v1|calibration_output|support_to_native",
      "external_interval_v1|automatic_prediction|external_only",
      "external_interval_v1|candidate_support|external_only",
      "external_per_isi_v1|automatic_prediction|external_only",
      "external_per_isi_v1|candidate_support|external_only"
    )
  )
  expect_identical(sum(registry$routes$enabled), 8L)
  expect_identical(
    registry$routes$gate_code[!registry$routes$enabled],
    c("native_gate_b_pending", "native_gate_b_pending",
      "calibration_disabled_phase_b2", "calibration_disabled_phase_b2")
  )

  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.03)))
  b2_expect_error(
    stpd_import_provider_batch(
      ds,
      b2_request("native_stpd_postcomposer_v1",
                 generation_mode = "native_only")
    ),
    "native_gate_b_pending"
  )
  for (adapter in c("mean_isi_v1", "logisi_newbd_v1")) {
    b2_expect_error(
      stpd_import_provider_batch(
        ds,
        b2_request(adapter, output_role = "calibration_output",
                   generation_mode = "support_to_native")
      ),
      "provider_mode_role_invalid"
    )
  }
})

test_that("public entry points have a narrow exact surface with no bypass", {
  expect_identical(names(formals(stpd_import_provider_batch)),
                   c("dataset", "request", "artifact"))
  expect_identical(names(formals(
    b2_internal("stpd_provider_combine_bundles_v1")
  )), "bundles")
  expect_length(formals(b2_internal("stpd_provider_adapter_registry_v1")), 0L)

  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.03)))
  request <- b2_request("external_interval_v1")
  request$coordinate_specs <- list(prevalidated = TRUE)
  b2_expect_error(
    stpd_import_provider_batch(
      ds, request, b2_interval_artifact(list(b2_interval_record()))
    ),
    "unknown_column"
  )
})

test_that("Mean-ISI reads only the strict timestamp spine and preserves diff edges", {
  ds <- b2_mean_boundary_dataset()
  before <- unserialize(serialize(ds, NULL))
  request <- b2_request("mean_isi_v1", trains = c("first", "last"))
  baseline <- stpd_import_provider_batch(ds, request)
  expect_identical(ds, before)
  expect_identical(baseline$provider_runs$run_status, "complete")
  expect_true(all(baseline$candidate_intervals$record_kind ==
                    "automatic_assertion"))
  expect_identical(baseline$provider_runs$authority_scope,
                   "automatic_prediction_record")
  first <- subset(baseline$candidate_intervals, train_key == "first")
  last <- subset(baseline$candidate_intervals, train_key == "last")
  expect_equal(min(first$canonical_start_isi), 2L)
  expect_equal(max(last$canonical_end_isi), 5L)

  changed <- unserialize(serialize(ds, NULL))
  changed$results <- list(secret = "reference-dependent-result")
  changed$manual <- list(labels = rep("burst", 100))
  changed$reference <- data.frame(label = "truth")
  changed$trains$first$manual_label <- "secret"
  changed_before <- unserialize(serialize(changed, NULL))
  perturbed <- stpd_import_provider_batch(changed, request)
  expect_identical(changed, changed_before)
  expect_identical(perturbed$provider_runs$provider_run_id,
                   baseline$provider_runs$provider_run_id)
  expect_identical(perturbed$provider_runs$raw_output_sha256,
                   baseline$provider_runs$raw_output_sha256)
  expect_identical(perturbed$candidate_intervals$candidate_id,
                   baseline$candidate_intervals$candidate_id)

  support <- stpd_import_provider_batch(
    ds,
    b2_request("mean_isi_v1", output_role = "candidate_support",
               trains = c("first", "last"))
  )
  expect_identical(support$provider_runs$authority_scope,
                   "support_evidence_only")
  expect_true(all(support$candidate_intervals$record_kind == "support"))
  expect_true(all(support$thresholds$threshold_role == "effective_parameter"))
})

test_that("LogISI reads only the strict spine and preserves first and last diff support", {
  ds <- b2_logisi_dataset()
  before <- unserialize(serialize(ds, NULL))
  request <- b2_request("logisi_newbd_v1")
  baseline <- stpd_import_provider_batch(ds, request)
  expect_identical(ds, before)
  expect_identical(baseline$provider_runs$run_status, "complete")
  expect_equal(min(baseline$candidate_intervals$canonical_start_isi), 2L)
  expect_equal(max(baseline$candidate_intervals$canonical_end_isi),
               nrow(ds$trains$t1))
  expect_identical(baseline$provider_runs$information_access, "label_blind")

  changed <- unserialize(serialize(ds, NULL))
  changed$results <- list(reference = "must-not-be-read")
  changed$manual <- list(secret = TRUE)
  changed$reference <- list(labels = "all")
  perturbed <- stpd_import_provider_batch(changed, request)
  expect_identical(perturbed$provider_runs$provider_run_id,
                   baseline$provider_runs$provider_run_id)
  expect_identical(perturbed$provider_runs$raw_output_sha256,
                   baseline$provider_runs$raw_output_sha256)

  support <- stpd_import_provider_batch(
    ds,
    b2_request("logisi_newbd_v1", output_role = "candidate_support")
  )
  expect_identical(support$provider_runs$authority_scope,
                   "support_evidence_only")
  expect_true(all(support$candidate_intervals$record_kind == "support"))
})

test_that("unresolved and truncated internal providers fail closed", {
  tiny <- b2_dataset(list(t1 = c(0, 0.1)))
  for (adapter in c("mean_isi_v1", "logisi_newbd_v1")) {
    out <- stpd_import_provider_batch(tiny, b2_request(adapter))
    diagnostics <- attr(out, "stpd_provider_import_diagnostics")
    expect_identical(out$provider_runs$run_status, "rejected")
    expect_identical(out$provider_runs$authority_scope, "none_candidate")
    expect_identical(diagnostics$rejection_code,
                     "provider_threshold_unresolved")
    expect_equal(nrow(out$candidate_intervals), 0L)
    expect_equal(nrow(out$thresholds), 0L)
  }

  isis <- rep(c(0.004, 0.005, 0.080), 40)
  ds <- b2_dataset(list(t1 = cumsum(c(0, isis))), "mean-truncated")
  out <- stpd_import_provider_batch(
    ds,
    b2_request("mean_isi_v1", parameters = list(max_windows = 1L))
  )
  expect_identical(out$provider_runs$run_status, "rejected")
  expect_match(attr(out, "stpd_provider_import_diagnostics")$rejection_detail,
               "search_truncated")
  expect_equal(nrow(out$candidate_intervals), 0L)
  expect_equal(nrow(out$thresholds), 0L)
})

test_that("threshold scopes and external effective parameters have golden IDs", {
  ds <- b2_dataset(list(
    alpha = c(0, 0.01, 0.02),
    beta = c(0, 0.01, 0.02)
  ), "scope-golden")
  spine <- b2_internal("stpd_provider_build_spine_v1")(
    ds, selected_trains = c("alpha", "beta")
  )
  scope_hash <- b2_internal("stpd_provider_b2_train_scope_sha256")
  expect_identical(
    scope_hash(spine, "alpha"),
    "eaa45cdd663a7a78715e6122d603b8d69ad3761e550cb6b7ce80bbe9af2ddd5f"
  )
  expect_identical(
    scope_hash(spine, "beta"),
    "f72ebe55f3826b002542fbdd8a18a0b061f8e531cc71bf7d3d8493fd83c35006"
  )
  expect_false(identical(
    scope_hash(spine, "alpha"), scope_hash(spine, "beta")
  ))

  hash_domain <- b2_internal("stpd_provider_hash_domain")
  interval_parameters <- list(
    artifact_schema_version = "stpd_external_interval_artifact_v1",
    coordinate_profile_id = "train_row_isi_one_closed_v1",
    semantic_track = NULL, target_label = NULL
  )
  per_isi_parameters <- list(
    artifact_schema_version = "stpd_external_per_isi_artifact_v1",
    coordinate_profile_id = "per_isi_one_closed_v1",
    semantic_track = "event", target_label = "burst"
  )
  expect_identical(
    hash_domain("stpd-provider-parameters-v1", interval_parameters),
    "070391deec194ad68ebaadc5fe807563076ebcbee5563846edebfc6cd1c1bb70"
  )
  expect_identical(
    hash_domain("stpd-provider-parameters-v1", per_isi_parameters),
    "18c673b0faa91c4f98890acef3bb9ada6e461fad5815fe985789861eeef47271"
  )
})

test_that("external adapters accept raw exact unknown-info schemas only", {
  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.03)))
  good <- list(b2_interval_record())
  out <- b2_import_interval(ds, b2_interval_artifact(good))
  expect_identical(out$provider_runs$run_status, "complete")
  expect_identical(out$provider_runs$information_access, "unknown")
  expect_equal(nrow(out$thresholds), 0L)

  b2_expect_error(
    b2_import_interval(ds, rawToChar(b2_interval_artifact(good))),
    "executable_import_forbidden"
  )
  b2_expect_error(
    b2_import_interval(ds, b2_interval_artifact(good,
                                                 information = "label_blind")),
    "label_blind_leakage"
  )
  b2_expect_error(
    b2_import_interval(ds, b2_interval_artifact(
      good, schema = "stpd_external_interval_artifact_v2"
    )),
    "schema_version_unsupported"
  )

  bad_label <- b2_interval_record(label = "not_a_registered_label")
  b2_expect_error(
    b2_import_interval(ds, b2_interval_artifact(list(bad_label))),
    "enum_invalid"
  )
  extra <- b2_interval_record()
  extra$prevalidated <- TRUE
  b2_expect_error(
    b2_import_interval(ds, b2_interval_artifact(list(extra))),
    "unknown_column"
  )
  b2_expect_error(
    stpd_import_provider_batch(
      ds, b2_request("mean_isi_v1"), b2_interval_artifact(good)
    ),
    "executable_import_forbidden"
  )
})

test_that("an invalid interval rejects the whole batch without partial claims", {
  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.03, 0.04)))
  records <- list(
    b2_interval_record("good", start = 2, end = 3),
    b2_interval_record("bad", start = 999, end = 1000)
  )
  out <- b2_import_interval(ds, b2_interval_artifact(records))
  diagnostics <- attr(out, "stpd_provider_import_diagnostics")
  expect_identical(out$provider_runs$run_status, "rejected")
  expect_identical(diagnostics$rejection_code, "normalization_batch_rejected")
  expect_equal(nrow(out$normalization_audit), 2L)
  expect_setequal(out$normalization_audit$normalization_status,
                  c("normalized", "rejected"))
  expect_equal(nrow(out$candidate_intervals), 0L)
  expect_equal(nrow(out$thresholds), 0L)
})

test_that("per-ISI import requires n-1 coverage and preserves binary decisions", {
  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.04, 0.07, 0.11)))
  records <- lapply(1:5, function(i) {
    b2_per_isi_record(paste0("d", i), "t1", i,
                      if (i %% 2L) "positive" else "negative")
  })
  out <- stpd_import_provider_batch(
    ds, b2_request("external_per_isi_v1"), b2_per_isi_artifact(records)
  )
  expect_identical(out$provider_runs$run_status, "complete")
  expect_equal(nrow(out$normalization_audit), 5L)
  expect_equal(nrow(out$candidate_intervals), 5L)
  expect_setequal(out$candidate_intervals$provider_decision,
                  c("positive", "negative"))
  expect_identical(sort(out$candidate_intervals$canonical_start_isi), 2:6)
  batch_ids <- b2_internal("stpd_provider_candidate_ids_v1")(
    out$candidate_intervals
  )
  scalar_ids <- vapply(seq_len(nrow(out$candidate_intervals)), function(i) {
    b2_internal("stpd_provider_candidate_id_v1")(
      out$candidate_intervals[i, , drop = FALSE]
    )
  }, character(1))
  expect_identical(batch_ids, scalar_ids)

  b2_expect_error(
    stpd_import_provider_batch(
      ds, b2_request("external_per_isi_v1"),
      b2_per_isi_artifact(records[-5L])
    ),
    "coordinate_roundtrip_mismatch"
  )

  nonbinary <- records
  nonbinary[[1L]]$provider_decision <- "indeterminate"
  b2_expect_error(
    stpd_import_provider_batch(
      ds, b2_request("external_per_isi_v1"),
      b2_per_isi_artifact(nonbinary)
    ),
    "provider_capability_mismatch"
  )
})

test_that("per-ISI batches without HFS do not invent empty-key conflicts", {
  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.04, 0.07, 0.11)))
  all_negative <- lapply(1:5, function(i) {
    b2_per_isi_record(paste0("negative-", i), "t1", i, "negative")
  })
  burst_only <- all_negative
  burst_only[[2L]]$provider_decision <- "positive"

  for (records in list(all_negative, burst_only)) {
    out <- stpd_import_provider_batch(
      ds, b2_request("external_per_isi_v1"),
      b2_per_isi_artifact(records, track = "event", label = "burst")
    )
    diagnostics <- attr(out, "stpd_provider_import_diagnostics")
    expect_identical(out$provider_runs$run_status, "complete")
    expect_identical(diagnostics$rejection_code, NA_character_)
    expect_equal(nrow(out$normalization_audit), 5L)
    expect_equal(nrow(out$candidate_intervals), 5L)
  }
})

test_that("HFS subtype imports enforce one Broad-HFS parent on frozen geometry", {
  ds <- b2_dataset(list(t1 = seq(0, 0.09, by = 0.01)))
  subtype <- b2_interval_record(
    "hft", start = 2, end = 8, track = "state", label = "hft"
  )
  auto_parent <- b2_import_interval(ds, b2_interval_artifact(list(subtype)))
  expect_identical(auto_parent$provider_runs$run_status, "complete")
  expect_setequal(auto_parent$candidate_intervals$proposed_label,
                  c("hft", "broad_hfs"))
  geometry <- auto_parent$candidate_intervals[, c(
    "canonical_start_isi", "canonical_end_isi", "canonical_start_spike",
    "canonical_end_spike", "canonical_start_time_sec",
    "canonical_end_time_sec"
  )]
  expect_identical(unname(unlist(geometry[1L, ], use.names = FALSE)),
                   unname(unlist(geometry[2L, ], use.names = FALSE)))

  explicit <- b2_interval_record(
    "parent", start = 2, end = 8, track = "state", label = "broad_hfs"
  )
  reused <- b2_import_interval(
    ds, b2_interval_artifact(list(subtype, explicit))
  )
  expect_identical(reused$provider_runs$run_status, "complete")
  expect_equal(nrow(reused$candidate_intervals), 2L)
  expect_equal(sum(reused$candidate_intervals$proposed_label == "broad_hfs"),
               1L)

  hfi <- b2_interval_record(
    "hfi", start = 2, end = 8, track = "state", label = "hf_irregular"
  )
  conflict <- b2_import_interval(
    ds, b2_interval_artifact(list(subtype, hfi))
  )
  expect_identical(conflict$provider_runs$run_status, "rejected")
  expect_identical(
    attr(conflict, "stpd_provider_import_diagnostics")$rejection_code,
    "provider_semantic_rejected"
  )
  expect_equal(nrow(conflict$candidate_intervals), 0L)
})

test_that("providers coexist on identical geometry without identity collision", {
  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.03, 0.04)))
  artifact <- b2_interval_artifact(list(b2_interval_record()))
  automatic <- b2_import_interval(ds, artifact, "automatic_prediction")
  support <- b2_import_interval(ds, artifact, "candidate_support")
  expect_false(identical(automatic$provider_runs$provider_run_id,
                         support$provider_runs$provider_run_id))
  expect_false(identical(automatic$candidate_intervals$candidate_id,
                         support$candidate_intervals$candidate_id))
  expect_identical(
    automatic$candidate_intervals[, c("canonical_start_isi",
                                      "canonical_end_isi")],
    support$candidate_intervals[, c("canonical_start_isi",
                                    "canonical_end_isi")]
  )
  combined <- b2_internal("stpd_provider_combine_bundles_v1")(
    list(automatic, support)
  )
  expect_equal(nrow(combined$provider_runs), 2L)
  expect_equal(nrow(combined$candidate_intervals), 2L)
  expect_setequal(combined$provider_runs$authority_scope,
                  c("automatic_prediction_record", "support_evidence_only"))
})

test_that("manifest source leaves and adapter/provider hashes are recomputable", {
  manifest_path <- b2_internal("stpd_provider_b2_manifest_path")()
  source_root <- dirname(dirname(manifest_path))
  manifest <- jsonlite::fromJSON(
    paste(readLines(manifest_path, warn = FALSE, encoding = "UTF-8"),
          collapse = "\n"),
    simplifyVector = FALSE
  )
  expect_identical(manifest$provider_contract_sha256,
                   stpd_provider_contract_hash())
  expect_identical(
    manifest$provider_adapter_protocol_sha256,
    b2_internal("stpd_provider_adapter_protocol_hash_v1")()
  )
  hash_domain <- b2_internal("stpd_provider_hash_domain")
  provider_specs <- list(
    native_stpd_postcomposer_v1 = list(
      provider_key = "native_stpd", provider_version = "1.0.0",
      status = "native_gate_b_pending", paths = character()
    ),
    mean_isi_v1 = list(
      provider_key = "mean_isi", provider_version = "1.0.0",
      status = NULL, paths = "R/33_support_misi.R"
    ),
    logisi_newbd_v1 = list(
      provider_key = "logisi_newbd", provider_version = "1.0.0",
      status = NULL, paths = "R/34_support_logisi.R"
    ),
    external_interval_v1 = list(
      provider_key = "external_data_import",
      provider_version = "unverified_external_declaration_v1",
      status = "not_runtime_authority", paths = character()
    ),
    external_per_isi_v1 = list(
      provider_key = "external_data_import",
      provider_version = "unverified_external_declaration_v1",
      status = "not_runtime_authority", paths = character()
    )
  )
  for (entry in manifest$adapters) {
    deps <- entry$dependency_files
    deps <- deps[order(vapply(deps, `[[`, character(1), "path"),
                       method = "radix")]
    for (dependency in deps) {
      expect_identical(
        b2_source_leaf_sha256(file.path(source_root, dependency$path)),
        dependency$sha256
      )
    }
    expect_identical(
      hash_domain("stpd-provider-adapter-code-v1", list(
        provider_contract_sha256 = manifest$provider_contract_sha256,
        provider_adapter_protocol_sha256 =
          manifest$provider_adapter_protocol_sha256,
        adapter_key = entry$adapter_key,
        adapter_version = entry$adapter_version,
        dependency_files = deps
      )),
      entry$adapter_code_sha256
    )
    provider_spec <- provider_specs[[entry$adapter_key]]
    dependency_paths <- vapply(deps, `[[`, character(1), "path")
    provider_dependencies <- if (length(provider_spec$paths)) {
      deps[match(provider_spec$paths, dependency_paths)]
    } else {
      list()
    }
    provider_payload <- list(
      provider_key = provider_spec$provider_key,
      provider_version = provider_spec$provider_version
    )
    if (!is.null(provider_spec$status)) {
      provider_payload$status <- provider_spec$status
    }
    provider_payload$dependency_files <- provider_dependencies
    expect_identical(
      hash_domain("stpd-provider-code-v1", provider_payload),
      entry$provider_code_sha256
    )
  }

  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.03)))
  raw <- b2_interval_artifact(list(b2_interval_record()),
                              provider_version = "declared-v7",
                              provider_code = b2_sha("c"))
  out <- b2_import_interval(ds, raw)
  expect_identical(
    out$provider_runs$raw_output_sha256,
    digest::digest(raw, algo = "sha256", serialize = FALSE)
  )
  expect_identical(
    out$provider_runs$provider_code_sha256,
    hash_domain("stpd-external-unverified-provider-code-declaration-v1", list(
      provider_version = "declared-v7",
      declared_provider_code_sha256 = b2_sha("c")
    ))
  )
})

test_that("external imports are deterministic and never mutate the dataset", {
  ds <- b2_dataset(list(t1 = c(0, 0.01, 0.02, 0.03, 0.04)))
  before <- unserialize(serialize(ds, NULL))
  artifact <- b2_interval_artifact(list(b2_interval_record()))
  a <- b2_import_interval(ds, artifact)
  b <- b2_import_interval(ds, artifact)
  expect_identical(ds, before)
  expect_identical(a$provider_runs$provider_run_id,
                   b$provider_runs$provider_run_id)
  expect_identical(a$provider_runs$input_artifact_sha256,
                   b$provider_runs$input_artifact_sha256)
  expect_identical(a$provider_runs$raw_output_sha256,
                   b$provider_runs$raw_output_sha256)
  expect_identical(a$candidate_intervals$candidate_id,
                   b$candidate_intervals$candidate_id)
  expect_identical(a$normalization_audit$source_record_sha256,
                   b$normalization_audit$source_record_sha256)
})

test_that("public 10k per-ISI import remains complete and bounded", {
  n <- 10000L
  train <- "scale-train"
  timestamps <- cumsum(c(0, rep(c(0.004, 0.010), length.out = n)))
  ds <- b2_dataset(setNames(list(timestamps), train), "public-10k-per-isi")
  before <- unserialize(serialize(ds, NULL, version = 3L))
  records <- lapply(seq_len(n), function(i) {
    b2_per_isi_record(
      sprintf("scale-%05d", i), train, i,
      if (i %% 1000L == 0L) "positive" else "negative"
    )
  })
  request <- b2_request("external_per_isi_v1", trains = train)
  artifact <- b2_per_isi_artifact(records)
  elapsed <- system.time(
    out <- stpd_import_provider_batch(ds, request, artifact)
  )[["elapsed"]]
  expect_identical(ds, before)
  expect_identical(out$provider_runs$run_status, "complete")
  expect_equal(nrow(out$normalization_audit), n)
  expect_equal(nrow(out$candidate_intervals), n)
  expect_true(all(out$normalization_audit$normalization_status == "normalized"))
  expect_true(nzchar(attr(out, "stpd_provider_import_diagnostics")$coverage_sha256))
  expect_silent(stpd_validate_provider_bundle(out))
  expect_lt(elapsed, 180)
})

test_that("HFS parent planning is bounded and 50k materialization is scalable", {
  check_rows <- b2_internal("stpd_provider_b2_check_materialized_rows")
  expect_identical(check_rows(249999L, 1L), 250000)
  b2_expect_error(check_rows(250000L, 1L), "resource_limit_exceeded")

  fixture <- b2_hft_materialization_fixture(50000L)
  elapsed <- system.time(
    out <- b2_internal("stpd_provider_b2_materialize_candidates")(
      fixture$batch, fixture$semantics,
      paste0("pr_", b2_sha("b")), "automatic_prediction"
    )
  )[["elapsed"]]
  expect_equal(nrow(out), 100000L)
  expect_equal(sum(out$proposed_label == "hft"), 50000L)
  expect_equal(sum(out$proposed_label == "broad_hfs"), 50000L)
  expect_identical(anyDuplicated(out$candidate_id), 0L)
  expect_lt(elapsed, 180)
})
