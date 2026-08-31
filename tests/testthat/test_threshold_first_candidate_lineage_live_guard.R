live_guard_error <- function(expr) tryCatch(expr, error = identity)

live_guard_receipt <- function(level = "full", run_id = "run_live") {
  collector <- stpd_candidate_lineage_collector_new(level)
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste0("params_", run_id), "dataset_live",
    c("train_1", "train_2")
  )
  if (!identical(level, "off")) {
    for (train in collector$target_trains) {
      shard <- stpd_candidate_lineage_collector_begin_train(collector, train)
      stpd_candidate_lineage_collector_note_pipeline(
        shard, "hf_protected", train = train
      )
      stpd_candidate_lineage_collector_end_train(collector, shard)
    }
  }
  collector$state <- "sealed"
  stpd_candidate_lineage_collector_snapshot(collector)
}

live_guard_cap <- function(receipt) {
  values <- list(
    run_id = receipt$run_id, params_hash = receipt$params_hash,
    dataset_id = receipt$dataset_id, train_id = receipt$target_trains[[1L]],
    group_id = "group_live", fold_id = "fold_live",
    analysis_block_id = "block_live", family_id = "burst_event",
    semantic_track = "Event", stage = "within_track_selected",
    stage_order = 7L, candidate_cap = 10L, policy_id = "top_k_v1",
    policy_status = "applied_complete"
  )
  values$stage_scope_id <- stpd_candidate_lineage_cap_scope_id(
    values$run_id, values$params_hash, values$dataset_id, values$train_id,
    values$group_id, values$fold_id, values$analysis_block_id,
    values$family_id, values$semantic_track, values$stage, values$policy_id
  )
  schema <- stpd_candidate_lineage_cap_policy_schema()
  values <- values[names(schema)]
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

live_guard_empty_envelope <- function(receipt, level = "off") {
  attached <- stpd_candidate_lineage_attach(
    list(science = 1L), stpd_candidate_lineage_empty_bundle(),
    stpd_candidate_lineage_empty_authoritative_products(),
    stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = "diagnostic_unavailable",
    unavailable_reason = "pipeline_not_instrumented"
  )
  envelope <- attached$candidate_lineage_audit
  if (!identical(level, "off")) {
    envelope$metadata$audit_level <- level
    envelope$metadata$evidence_status_sha256 <-
      stpd_threshold_first_hash_domain(
        "stpd-candidate-lineage-evidence-status-v1",
        list(
          schema_version = envelope$metadata$schema_version,
          audit_level = level,
          evidence_authority = envelope$metadata$evidence_authority,
          universe_complete = envelope$metadata$universe_complete,
          unavailable_reason = envelope$metadata$unavailable_reason
        )
      )
  }
  attr(envelope, "candidate_lineage_request_receipt") <- receipt
  attr(envelope, "candidate_lineage_request_receipt_sha256") <-
    stpd_candidate_lineage_live_receipt_sha256(receipt)
  envelope
}

test_that("live receipts have an exact schema and reject cross-binding", {
  receipt <- live_guard_receipt("full")
  envelope <- live_guard_empty_envelope(receipt)
  expect_identical(names(receipt), c(
    "receipt_version", "collector_version", "requested_audit_level",
    "collector_state", "bound", "run_id", "params_hash", "dataset_id",
    "target_trains", "instrumentation_status", "train_receipts"
  ))
  expect_identical(names(receipt$train_receipts), c(
    "collector_version", "requested_audit_level", "collector_state",
    "run_id", "params_hash", "dataset_id", "train", "train_state",
    "pipeline_id", "pipeline_entered", "instrumentation_status"
  ))
  expect_true(stpd_candidate_lineage_live_validate_receipt(
    receipt, envelope, receipt$run_id, receipt$params_hash,
    receipt$dataset_id, receipt$target_trains
  ))
  expect_true(stpd_candidate_lineage_live_validate_envelope(
    envelope, receipt, receipt$run_id, receipt$params_hash,
    receipt$dataset_id, receipt$target_trains
  ))

  bindings <- list(
    list(expected_run_id = "other_run"),
    list(expected_params_hash = "other_params"),
    list(expected_dataset_id = "other_dataset"),
    list(expected_target_trains = rev(receipt$target_trains))
  )
  for (binding in bindings) {
    error <- live_guard_error(do.call(
      stpd_candidate_lineage_live_validate_receipt,
      c(list(receipt = receipt, envelope = envelope), binding)
    ))
    expect_s3_class(error, "stpd_candidate_lineage_error")
  }

  tampered <- list(
    within(receipt, receipt_version <- "tampered"),
    within(receipt, collector_state <- "open"),
    within(receipt, requested_audit_level <- "summary"),
    within(receipt, target_trains <- rev(target_trains))
  )
  tampered[[5L]] <- receipt
  tampered[[5L]]$train_receipts$params_hash[[1L]] <- "tampered"
  for (value in tampered) {
    expect_s3_class(
      live_guard_error(stpd_candidate_lineage_live_validate_receipt(
        value, envelope
      )),
      "stpd_candidate_lineage_error"
    )
  }
})

test_that("live attachment forbids non-publication products and off-summary caps", {
  receipt <- live_guard_receipt("full")
  product_schema <- stpd_candidate_lineage_authoritative_product_schema()
  products <- as.data.frame(lapply(product_schema, function(type) switch(
    type, character = "x", integer = 1L, double = 1, logical = TRUE
  )), stringsAsFactors = FALSE, check.names = FALSE)

  expect_s3_class(live_guard_error(stpd_candidate_lineage_live_attach(
    list(science = 1L), stpd_candidate_lineage_empty_bundle(), products,
    stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = "diagnostic_unavailable",
    unavailable_reason = "pipeline_not_instrumented", receipt = receipt,
    expected_run_id = receipt$run_id,
    expected_params_hash = receipt$params_hash,
    expected_dataset_id = receipt$dataset_id,
    expected_target_trains = receipt$target_trains
  )), "stpd_candidate_lineage_live_diagnostic_products_forbidden")

  for (level in c("off", "summary")) {
    level_receipt <- live_guard_receipt(level)
    envelope <- live_guard_empty_envelope(level_receipt, level)
    envelope$expected_cap_scopes <- live_guard_cap(level_receipt)
    # Recompute the frozen manifest so this exercises the live symmetry guard,
    # rather than merely failing R90's generic hash-integrity check.
    envelope$metadata$cap_manifest_sha256 <- stpd_threshold_first_hash_domain(
      "stpd-candidate-lineage-cap-manifest-v1",
      stpd_candidate_lineage_canonical_cap_manifest(
        envelope$expected_cap_scopes
      )
    )
    expect_true(stpd_candidate_lineage_validate_envelope(envelope), info = level)
    expect_s3_class(live_guard_error(
      stpd_candidate_lineage_live_validate_envelope(
        envelope, level_receipt, level_receipt$run_id,
        level_receipt$params_hash, level_receipt$dataset_id,
        level_receipt$target_trains
      )
    ), "stpd_candidate_lineage_live_reduced_audit_caps_forbidden")
  }
})

test_that("live guard closes the frozen diagnostic-product asymmetry", {
  receipt <- live_guard_receipt("full", "run_product_exploit")
  envelope <- live_guard_empty_envelope(receipt)
  schema <- stpd_candidate_lineage_authoritative_product_schema()
  products <- as.data.frame(lapply(schema, function(type) switch(
    type, character = "x", integer = 1L, double = 1, logical = TRUE
  )), stringsAsFactors = FALSE, check.names = FALSE)
  envelope$authoritative_products <- products
  envelope$metadata$product_manifest_sha256 <-
    stpd_threshold_first_hash_domain(
      "stpd-candidate-lineage-product-manifest-v1",
      stpd_candidate_lineage_canonical_product_manifest(products)
    )
  # R90 accepts this legacy diagnostic shape after schema and manifest hashing.
  # The live layer must reject it even though every frozen hash is consistent.
  expect_true(stpd_candidate_lineage_validate_envelope(envelope))
  expect_s3_class(
    live_guard_error(stpd_candidate_lineage_live_validate_envelope(
      envelope, receipt, receipt$run_id, receipt$params_hash,
      receipt$dataset_id, receipt$target_trains
    )),
    "stpd_candidate_lineage_live_diagnostic_products_forbidden"
  )
})

test_that("off live evidence cannot smuggle an external source registry", {
  receipt <- live_guard_receipt("full", "run_adapter_exploit")
  envelope <- live_guard_empty_envelope(receipt)
  adapter <- data.frame(
    registry_version = "stpd_source_registry_v1",
    registry_source = "external", provider_id = "external_detector",
    raw_candidate_layer = "candidate",
    raw_candidate_class = "spike_cluster",
    raw_final_label = "candidate_burst",
    raw_candidate_source = "external_v1",
    source_candidate_class = "burst", source_final_label = "burst",
    candidate_class = "burst", semantic_track = "event",
    family_id = "burst", stringsAsFactors = FALSE, check.names = FALSE
  )
  envelope$source_adapter_registry <- adapter
  envelope$metadata$source_registry_sha256 <-
    stpd_threshold_first_hash_domain(
      "stpd-candidate-lineage-source-registry-v1",
      stpd_candidate_lineage_source_registry(adapter)
    )
  expect_true(stpd_candidate_lineage_validate_envelope(envelope))
  expect_s3_class(
    live_guard_error(stpd_candidate_lineage_live_validate_envelope(
      envelope, receipt, receipt$run_id, receipt$params_hash,
      receipt$dataset_id, receipt$target_trains
    )),
    "stpd_candidate_lineage_live_off_source_adapters_forbidden"
  )
})

test_that("live publication authority is disabled until direct coverage closes", {
  receipt <- live_guard_receipt("full", "run_publication_kill")
  error <- live_guard_error(stpd_candidate_lineage_live_attach(
    list(science = 1L), stpd_candidate_lineage_empty_bundle(),
    stpd_candidate_lineage_empty_authoritative_products(),
    stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = "publication_authoritative", receipt = receipt,
    expected_run_id = receipt$run_id,
    expected_params_hash = receipt$params_hash,
    expected_dataset_id = receipt$dataset_id,
    expected_target_trains = receipt$target_trains
  ))
  expect_s3_class(
    error, "stpd_candidate_lineage_live_publication_not_enabled"
  )
})

test_that("live attachment is anchored to canonical scientific metadata", {
  receipt <- live_guard_receipt("full", "run_scientific_binding")
  scientific <- list(results = list(run_metadata_public = data.frame(
    run_id = receipt$run_id,
    params_hash = receipt$params_hash,
    dataset_name = receipt$dataset_id,
    selected_trains = paste(receipt$target_trains, collapse = ";"),
    stringsAsFactors = FALSE
  )))
  args <- list(
    scientific_result = scientific,
    audit_bundle = stpd_candidate_lineage_empty_bundle(),
    authoritative_products =
      stpd_candidate_lineage_empty_authoritative_products(),
    expected_cap_scopes = stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = "diagnostic_unavailable",
    unavailable_reason = "pipeline_not_instrumented", receipt = receipt,
    expected_run_id = receipt$run_id,
    expected_params_hash = receipt$params_hash,
    expected_dataset_id = receipt$dataset_id,
    expected_target_trains = receipt$target_trains
  )
  attached <- do.call(stpd_candidate_lineage_live_attach, args)
  expect_true(stpd_candidate_lineage_live_validate_result(
    attached, receipt$run_id, receipt$params_hash, receipt$dataset_id,
    receipt$target_trains
  ))

  replacements <- list(
    run_id = "other_run", params_hash = "other_hash",
    dataset_name = "other_dataset", selected_trains = "train_2;train_1"
  )
  for (field in names(replacements)) {
    bad <- args
    bad$scientific_result$results$run_metadata_public[[field]][1L] <-
      replacements[[field]]
    expect_s3_class(
      live_guard_error(do.call(stpd_candidate_lineage_live_attach, bad)),
      if (identical(field, "selected_trains")) {
        "stpd_candidate_lineage_live_scientific_scope_mismatch"
      } else "stpd_candidate_lineage_live_binding_mismatch"
    )
  }

  conflict <- args
  conflict$scientific_result$results$run_metadata <- data.frame(
    run_id = "conflicting_internal_run",
    params_hash = receipt$params_hash,
    selected_trains = paste(receipt$target_trains, collapse = ";"),
    stringsAsFactors = FALSE
  )
  expect_s3_class(
    live_guard_error(do.call(stpd_candidate_lineage_live_attach, conflict)),
    "stpd_candidate_lineage_live_scientific_metadata_conflict"
  )
})

test_that("production R files cannot bypass the live attach boundary", {
  root <- testthat::test_path("..", "..", "R")
  files <- list.files(root, pattern = "[.]R$", full.names = TRUE)
  allowed <- c(
    "90_candidate_lineage_funnel.R", "92_candidate_lineage_live_guard.R"
  )
  bypass <- vapply(files, function(path) {
    if (basename(path) %in% allowed) return(FALSE)
    any(grepl(
      "stpd_candidate_lineage_attach\\(",
      readLines(path, warn = FALSE), perl = TRUE
    ))
  }, logical(1))
  expect_identical(basename(files[bypass]), character())
})

test_that("production unavailable finalization passes the live guard", {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, "run_finalize", "params_run_finalize", "dataset_live",
    c("train_1", "train_2")
  )
  for (train in collector$target_trains) {
    shard <- stpd_candidate_lineage_collector_begin_train(collector, train)
    stpd_candidate_lineage_collector_note_pipeline(
      shard, "hf_protected", train = train
    )
    stpd_candidate_lineage_collector_end_train(collector, shard)
  }
  result <- stpd_candidate_lineage_collector_finalize_unavailable(
    list(science = list(value = 1L)), collector
  )
  envelope <- result$candidate_lineage_audit
  receipt <- attr(
    envelope, "candidate_lineage_request_receipt", exact = TRUE
  )
  expect_identical(result$science, list(value = 1L))
  expect_true(stpd_candidate_lineage_live_validate_envelope(
    envelope, receipt, "run_finalize", "params_run_finalize",
    "dataset_live", c("train_1", "train_2")
  ))
})

test_that("live envelope and receipt survive an RDS round trip", {
  receipt <- live_guard_receipt("full", "run_rds")
  attached <- stpd_candidate_lineage_live_attach(
    list(science = 7L), stpd_candidate_lineage_empty_bundle(),
    stpd_candidate_lineage_empty_authoritative_products(),
    stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = "diagnostic_unavailable",
    unavailable_reason = "pipeline_not_instrumented", receipt = receipt,
    expected_run_id = receipt$run_id,
    expected_params_hash = receipt$params_hash,
    expected_dataset_id = receipt$dataset_id,
    expected_target_trains = receipt$target_trains
  )
  restored <- unserialize(serialize(attached, NULL, version = 3L))
  envelope <- restored$candidate_lineage_audit
  restored_receipt <- attr(
    envelope, "candidate_lineage_request_receipt", exact = TRUE
  )
  expect_identical(restored, attached)
  expect_true(stpd_candidate_lineage_live_validate_envelope(
    envelope, restored_receipt, "run_rds", "params_run_rds",
    "dataset_live", c("train_1", "train_2")
  ))

  tampered <- envelope
  tampered_receipt <- restored_receipt
  tampered_receipt$train_receipts$pipeline_id[1L] <- "other_pipeline"
  attr(tampered, "candidate_lineage_request_receipt") <- tampered_receipt
  expect_s3_class(
    live_guard_error(stpd_candidate_lineage_live_validate_envelope(
      tampered, tampered_receipt, "run_rds", "params_run_rds",
      "dataset_live", c("train_1", "train_2")
    )),
    "stpd_candidate_lineage_live_receipt_hash_mismatch"
  )
})
