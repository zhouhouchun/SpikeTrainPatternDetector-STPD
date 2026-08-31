gb3_fix_hash64 <- function(letter = "a") strrep(letter, 64L)

gb3_fix_train <- function(letter = "a") paste0("tr_", gb3_fix_hash64(letter))

gb3_fix_bundle <- function() {
  stpd_gate_b_v3_phase1_bundle(verify_frozen = FALSE)
}

test_that("canonical scalars reject negative zero and validate real UTC bounds", {
  bundle <- gb3_fix_bundle()
  testthat::local_mocked_bindings(
    stpd_gate_b_v3_phase1_bundle = function(...) bundle,
    .package = "SpikeTrainPatternDetector"
  )
  canonical <- getFromNamespace(
    "stpd_gate_b_v3_canonical_json", "SpikeTrainPatternDetector"
  )
  expect_identical(canonical(0), "0")
  expect_error(canonical(-0), "Negative zero")
  expect_error(canonical(list(value = -0)), "Negative zero")
  expect_error(canonical(c(1, -0)), "Negative zero")

  minimal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_minimal_product", "SpikeTrainPatternDetector"
  )
  product <- minimal(with_overlap = TRUE)
  negative_threshold <- product$threshold_instances[1L, , drop = FALSE]
  negative_threshold$value_num <- -0
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    negative_threshold, "threshold_instances", bundle, tables = product
  ), "forbid negative zero")

  signature <- data.frame(
    signature_schema = "stpd_gate_b_v3_signature_1",
    signature_domain = "gate_b_approval",
    payload_sha256 = gb3_fix_hash64("1"), algorithm = "Ed25519",
    key_id = "fixture-key",
    public_key_fingerprint_sha256 = gb3_fix_hash64("2"),
    signature_base64 = "AA==", release_sequence = "1",
    not_before_utc = "2024-02-29T00:00:00.000000Z",
    not_after_utc = "2024-03-01T00:00:00.000000Z",
    stringsAsFactors = FALSE
  )
  expect_true(stpd_gate_b_v3_validate_table_prototype(
    signature, "signature_envelope", bundle
  ))
  invalid_values <- c(
    "0000-01-01T00:00:00.000000Z",
    "2023-02-29T00:00:00.000000Z",
    "2024-13-01T00:00:00.000000Z",
    "2024-01-32T00:00:00.000000Z",
    "2024-01-01T24:00:00.000000Z",
    "2024-01-01T00:60:00.000000Z",
    "2024-01-01T00:00:60.000000Z"
  )
  for (bad in invalid_values) {
    attacked <- signature
    attacked$not_before_utc <- bad
    expect_error(stpd_gate_b_v3_validate_table_prototype(
      attacked, "signature_envelope", bundle
    ), "exact RFC3339")
  }
  equal_bounds <- signature
  equal_bounds$not_before_utc <- equal_bounds$not_after_utc
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    equal_bounds, "signature_envelope", bundle
  ), "not_before_utc < not_after_utc")
  reversed <- signature
  reversed$not_before_utc <- "2024-03-02T00:00:00.000000Z"
  expect_error(stpd_gate_b_v3_validate_table_prototype(
    reversed, "signature_envelope", bundle
  ), "not_before_utc < not_after_utc")
})

test_that("AUTO and FINAL parent-history matrices fail closed after resealing", {
  bundle <- gb3_fix_bundle()
  testthat::local_mocked_bindings(
    stpd_gate_b_v3_phase1_bundle = function(...) bundle,
    .package = "SpikeTrainPatternDetector"
  )
  minimal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_minimal_product", "SpikeTrainPatternDetector"
  )
  reseal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_reseal_product", "SpikeTrainPatternDetector"
  )
  structural <- getFromNamespace(
    ".stpd_gate_b_v3_validate_product_prototype_structural",
    "SpikeTrainPatternDetector"
  )
  history_check <- getFromNamespace(
    "stpd_gate_b_v3_validate_product_history_matrix",
    "SpikeTrainPatternDetector"
  )
  history_head <- getFromNamespace(
    "stpd_gate_b_v3_history_head_hash", "SpikeTrainPatternDetector"
  )
  product <- minimal(with_overlap = TRUE)
  expect_true(history_check(product))

  bad_auto <- product
  bad_auto$materialized_product_identity$parent_auto_product_hash <-
    gb3_fix_hash64("b")
  bad_auto$materialized_product_identity$history_head_hash <-
    gb3_fix_hash64("c")
  bad_auto <- reseal(bad_auto, bundle)
  expect_error(structural(bad_auto, bundle), "AUTO identity")

  parent_hash <- product$materialized_product_identity$product_hash[[1L]]
  valid_final <- product
  valid_final$materialized_product_identity$product_kind <- "final"
  valid_final$materialized_product_identity$parent_auto_product_hash <-
    parent_hash
  valid_final$materialized_product_identity$history_head_hash <- history_head(
    parent_hash, valid_final$adjudication_transitions
  )
  expect_true(history_check(valid_final))

  missing_parent <- valid_final
  missing_parent$materialized_product_identity$parent_auto_product_hash <-
    NA_character_
  missing_parent <- reseal(missing_parent, bundle)
  expect_error(structural(missing_parent, bundle), "FINAL identity")

  wrong_history <- valid_final
  wrong_history$materialized_product_identity$history_head_hash <-
    gb3_fix_hash64("d")
  wrong_history <- reseal(wrong_history, bundle)
  expect_error(structural(wrong_history, bundle), "history-head hash")
})

test_that("label-blind reconstruction never dispatches caller S3 accessors", {
  bundle <- gb3_fix_bundle()
  testthat::local_mocked_bindings(
    stpd_gate_b_v3_phase1_bundle = function(...) bundle,
    .package = "SpikeTrainPatternDetector"
  )
  dispatch <- new.env(parent = emptyenv())
  dispatch$dollar <- FALSE
  dispatch$index <- FALSE
  dispatch$context <- FALSE
  assign("$.gb3_fix_evil_df", function(x, name) {
    dispatch$dollar <- TRUE
    attr(x, "hidden_timestamp", exact = TRUE)
  }, envir = .GlobalEnv)
  assign("[[.gb3_fix_evil_df", function(x, ...) {
    dispatch$index <- TRUE
    attr(x, "hidden_timestamp", exact = TRUE)
  }, envir = .GlobalEnv)
  assign("[[.gb3_fix_evil_list", function(x, ...) {
    dispatch$index <- TRUE
    NextMethod()
  }, envir = .GlobalEnv)
  assign("[.gb3_fix_evil_context", function(x, ...) {
    dispatch$context <- TRUE
    NextMethod()
  }, envir = .GlobalEnv)
  on.exit(rm(list = c("$.gb3_fix_evil_df", "[[.gb3_fix_evil_df",
                      "[[.gb3_fix_evil_list", "[.gb3_fix_evil_context"),
             envir = .GlobalEnv), add = TRUE)

  train <- gb3_fix_train("a")
  clean_row <- data.frame(timestamp = c(0, 0.1, 0.2))
  clean <- list(trains = setNames(list(clean_row), train))
  context <- setNames(list(list(timestamp_unit = "seconds")), train)

  evil_row <- clean_row
  class(evil_row) <- c("gb3_fix_evil_df", "data.frame")
  attr(evil_row, "hidden_timestamp") <- c(0, 10, 20)
  evil_trains <- setNames(list(evil_row), train)
  class(evil_trains) <- c("gb3_fix_evil_list", "list")
  evil_ds <- list(trains = evil_trains)
  class(evil_ds) <- c("gb3_fix_evil_list", "list")
  evil_context <- context
  class(evil_context) <- c("gb3_fix_evil_context", "list")

  expected <- stpd_gate_b_v3_label_blind_input(clean, context)
  observed <- stpd_gate_b_v3_label_blind_input(evil_ds, evil_context)
  expect_identical(observed, expected)
  expect_false(dispatch$dollar)
  expect_false(dispatch$index)
  expect_false(dispatch$context)

  active_called <- FALSE
  active <- new.env(parent = emptyenv())
  makeActiveBinding("trains", function(value) {
    active_called <<- TRUE
    stop("ACTIVE_BINDING_REACHED")
  }, active)
  expect_error(stpd_gate_b_v3_label_blind_input(active), "must be a list")
  expect_false(active_called)
})

test_that("nonempty current-product provenance recomputes source rows and indices", {
  bundle <- gb3_fix_bundle()
  testthat::local_mocked_bindings(
    stpd_gate_b_v3_phase1_bundle = function(...) bundle,
    .package = "SpikeTrainPatternDetector"
  )
  minimal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_minimal_product", "SpikeTrainPatternDetector"
  )
  reseal <- getFromNamespace(
    "stpd_gate_b_v3_phase1_reseal_product", "SpikeTrainPatternDetector"
  )
  structural <- getFromNamespace(
    ".stpd_gate_b_v3_validate_product_prototype_structural",
    "SpikeTrainPatternDetector"
  )
  canonical <- getFromNamespace(
    "stpd_gate_b_v3_canonical_json", "SpikeTrainPatternDetector"
  )
  source_hash <- getFromNamespace(
    "stpd_gate_b_v3_source_row_hash", "SpikeTrainPatternDetector"
  )
  product <- minimal(with_overlap = TRUE)
  domain <- function(name) product$entity_domain_registry[
    product$entity_domain_registry$entity_domain == name &
      product$entity_domain_registry$active, , drop = FALSE
  ]
  source_key <- list(
    event_candidate_id = product$event_candidates$event_candidate_id[[1L]]
  )
  edge <- data.frame(
    schema_version = "stpd_multitrack_v3_1",
    detection_root_id =
      product$materialized_product_identity$detection_root_id[[1L]],
    entity_domain_id = domain("event")$entity_domain_id[[1L]],
    entity_id = product$events$event_id[[1L]],
    entity_product_hash = NA_character_, edge_index = 1L,
    evidence_role_registry_id = product$contract_registries$registry_entry_id[
      product$contract_registries$registry_domain == "evidence_role" &
        product$contract_registries$active
    ][[1L]],
    source_domain_id = domain("event_candidate")$entity_domain_id[[1L]],
    source_row_key_json = canonical(source_key),
    source_product_hash = NA_character_,
    source_row_hash = source_hash(
      domain("event_candidate"), "event_candidates",
      product$event_candidates[1L, , drop = FALSE], source_key,
      NA_character_, bundle
    ),
    evidence_id = product$evidence_records$evidence_id[[1L]],
    stringsAsFactors = FALSE
  )
  valid <- product
  valid$entity_evidence_edges <- edge
  valid <- reseal(valid, bundle)
  expect_true(structural(valid, bundle))

  history_head <- getFromNamespace(
    "stpd_gate_b_v3_history_head_hash", "SpikeTrainPatternDetector"
  )
  valid_final <- valid
  parent_hash <- valid$materialized_product_identity$product_hash[[1L]]
  valid_final$materialized_product_identity$product_kind <- "final"
  valid_final$materialized_product_identity$parent_auto_product_hash <-
    parent_hash
  valid_final$materialized_product_identity$history_head_hash <- history_head(
    parent_hash, valid_final$adjudication_transitions
  )
  if (nrow(valid_final$product_status_envelope) > 0L) {
    valid_final$product_status_envelope$product_kind <- "final"
  }
  valid_final <- reseal(valid_final, bundle)
  expect_true(structural(valid_final, bundle))

  wrong_hash <- valid
  wrong_hash$entity_evidence_edges$source_row_hash <- gb3_fix_hash64("e")
  wrong_hash <- reseal(wrong_hash, bundle)
  expect_error(structural(wrong_hash, bundle), "source-row hash is invalid")

  wrong_index <- valid
  wrong_index$entity_evidence_edges$edge_index <- 2L
  wrong_index <- reseal(wrong_index, bundle)
  expect_error(structural(wrong_index, bundle), "edge-index expected set")

  changed_source <- valid
  old_evidence_id <-
    changed_source$event_candidates$base_event_evidence_id[[1L]]
  replacement_ids <- setdiff(
    changed_source$evidence_records$evidence_id, old_evidence_id
  )
  expect_gt(length(replacement_ids), 0L)
  changed_source$event_candidates$base_event_evidence_id <- replacement_ids[[1L]]
  expect_false(identical(
    old_evidence_id,
    changed_source$event_candidates$base_event_evidence_id[[1L]]
  ))
  changed_source <- reseal(changed_source, bundle)
  expect_error(structural(changed_source, bundle), "source-row hash is invalid")
})

test_that("partition identities bind normalized rows and remain session held-out", {
  input_check <- getFromNamespace(
    "stpd_gate_b_v3_validate_partition_input_binding",
    "SpikeTrainPatternDetector"
  )
  holdout_check <- getFromNamespace(
    "stpd_gate_b_v3_validate_partition_holdout",
    "SpikeTrainPatternDetector"
  )
  normalized <- data.frame(
    normalized_timestamp_bytes_sha256 =
      c(gb3_fix_hash64("1"), gb3_fix_hash64("2")),
    patient_group_id_hash = c(NA_character_, gb3_fix_hash64("3")),
    session_group_id_hash = c(gb3_fix_hash64("4"), gb3_fix_hash64("5")),
    stringsAsFactors = FALSE
  )
  memberships <- data.frame(
    partition_id = c("partition-a", "partition-a"),
    group_role = c("development", "validation"),
    patient_group_id_hash = normalized$patient_group_id_hash,
    session_group_id_hash = normalized$session_group_id_hash,
    source_input_hash = normalized$normalized_timestamp_bytes_sha256,
    stringsAsFactors = FALSE
  )
  expect_true(input_check(memberships, normalized))
  expect_true(holdout_check(memberships))

  wrong_input <- memberships
  wrong_input$source_input_hash[[1L]] <- gb3_fix_hash64("9")
  expect_error(input_check(wrong_input, normalized), "does not uniquely bind")
  wrong_patient <- memberships
  wrong_patient$patient_group_id_hash[[1L]] <- gb3_fix_hash64("8")
  expect_error(input_check(wrong_patient, normalized), "does not uniquely bind")
  wrong_session <- memberships
  wrong_session$session_group_id_hash[[1L]] <- gb3_fix_hash64("8")
  expect_error(input_check(wrong_session, normalized), "does not uniquely bind")

  missing_patient_leak <- memberships
  missing_patient_leak$session_group_id_hash[[2L]] <-
    missing_patient_leak$session_group_id_hash[[1L]]
  expect_error(holdout_check(missing_patient_leak), "sessions must be held out")

  patient_leak <- memberships
  patient_leak$patient_group_id_hash <- rep(gb3_fix_hash64("7"), 2L)
  expect_error(holdout_check(patient_leak), "patients must be held out")
})

test_that("resource budgets reject rows and JSON before serializers or staging", {
  bundle <- gb3_fix_bundle()
  contract <- stpd_gate_b_v3_complexity_contract()
  contract$limit[contract$budget_metric == "json_bytes_max"] <- 32
  contract$limit[contract$budget_metric == "artifact_bytes_total"] <- 32
  contract$limit[contract$budget_metric == "table_rows_max"] <- 1
  json_called <- FALSE
  table_called <- FALSE
  testthat::local_mocked_bindings(
    stpd_gate_b_v3_phase1_bundle = function(...) bundle,
    stpd_gate_b_v3_complexity_contract = function() contract,
    stpd_gate_b_v3_peak_rss_bytes = function() 1,
    stpd_gate_b_v3_resource_serialize_json = function(value) {
      json_called <<- TRUE
      stop("SERIALIZER_REACHED")
    },
    stpd_gate_b_v3_resource_serialize_table = function(...) {
      table_called <<- TRUE
      stop("SERIALIZER_REACHED")
    },
    .package = "SpikeTrainPatternDetector"
  )
  root <- tempfile("gb3-preflight-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  json_result <- stpd_gate_b_v3_resource_staging_probe(root, list(list(
    op = "write_json", relative_path = "large.json",
    value = list(payload = strrep("x", 64L))
  )))
  expect_identical(json_result$failure_code, "resource_budget_exceeded")
  expect_identical(json_result$measurement_phase, "preflight_upper_bound")
  expect_identical(json_result$staging_cleanup, "not_created")
  expect_false(json_called)
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)

  compact_rows <- structure(
    list(), class = "data.frame", row.names = c(NA_integer_, -2L)
  )
  table_result <- stpd_gate_b_v3_resource_staging_probe(root, list(list(
    op = "write_canonical_table", relative_path = "events.json",
    table_name = "events", value = compact_rows
  )))
  expect_identical(table_result$failure_code, "resource_budget_exceeded")
  expect_identical(table_result$measurement_phase, "preflight_upper_bound")
  expect_false(table_called)
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("phase-1 DAG summary derives its count from the exact node set", {
  expected_component <- getFromNamespace(
    "stpd_gate_b_v3_phase1_expected_contract_component",
    "SpikeTrainPatternDetector"
  )
  required_nodes <- getFromNamespace(
    "stpd_gate_b_v3_required_hash_nodes", "SpikeTrainPatternDetector"
  )
  expect_identical(
    expected_component("jcs_hash_dag")$observation,
    as.integer(length(required_nodes()))
  )
})
