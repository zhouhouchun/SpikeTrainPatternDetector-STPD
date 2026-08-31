# Threshold-first Gate 1B live evidence guard -----------------------------
#
# R90 is the frozen structural contract. This stricter production boundary
# closes two deliberately deferred asymmetries: diagnostic evidence cannot
# carry authoritative products, and reduced (off/summary) evidence cannot
# carry cap manifests. Publication authority stays disabled until every live
# candidate stage and final product is instrumented directly.

STPD_CANDIDATE_LINEAGE_LIVE_GUARD_VERSION <-
  "stpd_candidate_lineage_live_guard_v1"
STPD_CANDIDATE_LINEAGE_LIVE_PUBLICATION_ENABLED <- FALSE
STPD_CANDIDATE_LINEAGE_INSTRUMENTATION_STATUSES <- c(
  "pipeline_not_instrumented", "direct_partial", "direct_complete"
)

stpd_candidate_lineage_live_abort <- function(code, message, field = NULL) {
  stpd_candidate_lineage_abort(
    paste0("live_", code), message, field = field
  )
}

stpd_candidate_lineage_live_receipt_names <- function() {
  c(
    "receipt_version", "collector_version", "requested_audit_level",
    "collector_state", "bound", "run_id", "params_hash", "dataset_id",
    "target_trains", "instrumentation_status", "train_receipts"
  )
}

stpd_candidate_lineage_live_train_receipt_names <- function() {
  c(
    "collector_version", "requested_audit_level", "collector_state",
    "run_id", "params_hash", "dataset_id", "train", "train_state",
    "pipeline_id", "pipeline_entered", "instrumentation_status"
  )
}

stpd_candidate_lineage_live_scalar_text <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stpd_candidate_lineage_live_abort(
      "receipt_invalid",
      paste0(field, " must be one non-empty character value."),
      field = field
    )
  }
  x
}

stpd_candidate_lineage_live_expected_text <- function(actual, expected, field) {
  if (is.null(expected)) return(invisible(TRUE))
  expected <- stpd_candidate_lineage_live_scalar_text(expected, field)
  if (!identical(actual, expected)) {
    stpd_candidate_lineage_live_abort(
      "binding_mismatch",
      paste0("Receipt ", field, " does not match the active run."),
      field = field
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_live_require_active_binding <- function(
    expected_run_id, expected_params_hash, expected_dataset_id,
    expected_target_trains) {
  stpd_candidate_lineage_live_scalar_text(expected_run_id, "run_id")
  stpd_candidate_lineage_live_scalar_text(expected_params_hash, "params_hash")
  stpd_candidate_lineage_live_scalar_text(expected_dataset_id, "dataset_id")
  if (!is.character(expected_target_trains) ||
      !length(expected_target_trains) || anyNA(expected_target_trains) ||
      any(!nzchar(expected_target_trains)) ||
      anyDuplicated(expected_target_trains)) {
    stpd_candidate_lineage_live_abort(
      "active_binding_required",
      "Live evidence acceptance requires an ordered active target-train scope.",
      field = "target_trains"
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_live_receipt_sha256 <- function(receipt) {
  payload <- receipt
  attributes(payload) <- attributes(payload)[setdiff(
    names(attributes(payload)), "candidate_lineage_request_receipt_sha256"
  )]
  stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-request-receipt-v1", payload
  )
}

stpd_candidate_lineage_live_validate_receipt_shape <- function(receipt) {
  if (!is.list(receipt) || !identical(
    names(receipt), stpd_candidate_lineage_live_receipt_names()
  )) {
    stpd_candidate_lineage_live_abort(
      "receipt_invalid", "Candidate-lineage request receipt has an invalid shape."
    )
  }
  if (!identical(
    receipt$receipt_version,
    STPD_CANDIDATE_LINEAGE_REQUEST_RECEIPT_VERSION
  ) || !identical(
    receipt$collector_version,
    STPD_CANDIDATE_LINEAGE_COLLECTOR_VERSION
  )) {
    stpd_candidate_lineage_live_abort(
      "receipt_version_invalid", "Candidate-lineage receipt version is unsupported."
    )
  }
  requested <- receipt$requested_audit_level
  if (!is.character(requested) || length(requested) != 1L || is.na(requested) ||
      !(requested %in% STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS)) {
    stpd_candidate_lineage_live_abort(
      "receipt_invalid", "Receipt requested_audit_level is invalid.",
      field = "requested_audit_level"
    )
  }
  if (!identical(receipt$collector_state, "sealed") ||
      !is.logical(receipt$bound) || length(receipt$bound) != 1L ||
      !isTRUE(receipt$bound)) {
    stpd_candidate_lineage_live_abort(
      "receipt_unsealed",
      "A live evidence receipt must describe a sealed, bound collector."
    )
  }
  run_id <- stpd_candidate_lineage_live_scalar_text(receipt$run_id, "run_id")
  params_hash <- stpd_candidate_lineage_live_scalar_text(
    receipt$params_hash, "params_hash"
  )
  dataset_id <- stpd_candidate_lineage_live_scalar_text(
    receipt$dataset_id, "dataset_id"
  )
  target_trains <- receipt$target_trains
  if (!is.character(target_trains) || !length(target_trains) ||
      anyNA(target_trains) || any(!nzchar(target_trains)) ||
      anyDuplicated(target_trains)) {
    stpd_candidate_lineage_live_abort(
      "receipt_scope_invalid",
      "Receipt target_trains must be a non-empty ordered vector of unique train IDs.",
      field = "target_trains"
    )
  }
  instrumentation <- stpd_candidate_lineage_live_scalar_text(
    receipt$instrumentation_status, "instrumentation_status"
  )
  if (!(instrumentation %in%
        STPD_CANDIDATE_LINEAGE_INSTRUMENTATION_STATUSES)) {
    stpd_candidate_lineage_live_abort(
      "receipt_invalid", "Receipt instrumentation_status is unsupported.",
      field = "instrumentation_status"
    )
  }

  trains <- receipt$train_receipts
  if (!is.data.frame(trains) || !identical(
    names(trains), stpd_candidate_lineage_live_train_receipt_names()
  ) || nrow(trains) != length(target_trains) ||
      !identical(as.character(trains$train), target_trains)) {
    stpd_candidate_lineage_live_abort(
      "receipt_scope_invalid",
      "Train receipts must contain exactly one ordered row for every target train."
    )
  }
  character_fields <- setdiff(
    stpd_candidate_lineage_live_train_receipt_names(), "pipeline_entered"
  )
  if (any(!vapply(trains[character_fields], is.character, logical(1))) ||
      !is.logical(trains$pipeline_entered) || anyNA(trains$pipeline_entered) ||
      !all(trains$pipeline_entered) || anyNA(trains[character_fields]) ||
      any(!nzchar(as.matrix(trains[character_fields])))) {
    stpd_candidate_lineage_live_abort(
      "receipt_invalid", "Train receipt columns or pipeline entries are invalid."
    )
  }
  expected_columns <- list(
    collector_version = receipt$collector_version,
    requested_audit_level = requested,
    collector_state = receipt$collector_state,
    run_id = run_id,
    params_hash = params_hash,
    dataset_id = dataset_id,
    train_state = "completed",
    instrumentation_status = instrumentation
  )
  for (field in names(expected_columns)) {
    if (!all(trains[[field]] == expected_columns[[field]])) {
      stpd_candidate_lineage_live_abort(
        "receipt_cross_binding_invalid",
        paste0("Train receipt field ", field,
               " is not bound to its run receipt."),
        field = field
      )
    }
  }
  if (anyDuplicated(trains$train)) {
    stpd_candidate_lineage_live_abort(
      "receipt_scope_invalid", "Train receipts contain duplicate train IDs."
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_live_validate_scope_table <- function(
    table, table_name, receipt) {
  if (!is.data.frame(table) || !nrow(table)) return(invisible(TRUE))
  bindings <- c(
    run_id = receipt$run_id, params_hash = receipt$params_hash,
    dataset_id = receipt$dataset_id
  )
  for (field in names(bindings)) {
    if (!(field %in% names(table)) || anyNA(table[[field]]) ||
        any(as.character(table[[field]]) != bindings[[field]])) {
      stpd_candidate_lineage_live_abort(
        "scope_binding_invalid",
        paste0(table_name, " is not wholly bound to the receipt ", field, "."),
        field = field
      )
    }
  }
  if (!("train_id" %in% names(table)) || anyNA(table$train_id) ||
      any(!(as.character(table$train_id) %in% receipt$target_trains))) {
    stpd_candidate_lineage_live_abort(
      "scope_binding_invalid",
      paste0(table_name, " contains a train outside the receipt scope."),
      field = "train_id"
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_live_validate_receipt <- function(
    receipt, envelope,
    expected_run_id = NULL, expected_params_hash = NULL,
    expected_dataset_id = NULL, expected_target_trains = NULL) {
  stpd_candidate_lineage_live_validate_receipt_shape(receipt)
  stpd_candidate_lineage_live_expected_text(
    receipt$run_id, expected_run_id, "run_id"
  )
  stpd_candidate_lineage_live_expected_text(
    receipt$params_hash, expected_params_hash, "params_hash"
  )
  stpd_candidate_lineage_live_expected_text(
    receipt$dataset_id, expected_dataset_id, "dataset_id"
  )
  if (!is.null(expected_target_trains)) {
    if (!is.character(expected_target_trains) || anyNA(expected_target_trains) ||
        any(!nzchar(expected_target_trains)) ||
        !identical(receipt$target_trains, expected_target_trains)) {
      stpd_candidate_lineage_live_abort(
        "binding_mismatch",
        "Receipt target_trains do not match the ordered active run scope.",
        field = "target_trains"
      )
    }
  }
  if (!is.list(envelope) || !is.list(envelope$metadata)) {
    stpd_candidate_lineage_live_abort(
      "envelope_invalid",
      "Receipt validation requires a candidate-lineage envelope."
    )
  }
  level <- envelope$metadata$audit_level
  authority <- envelope$metadata$evidence_authority
  requested_rank <- match(
    receipt$requested_audit_level, STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS
  )
  level_rank <- match(level, STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS)
  if (is.na(level_rank) || level_rank > requested_rank) {
    stpd_candidate_lineage_live_abort(
      "audit_level_escalation",
      "Materialized audit level cannot exceed the requested audit level."
    )
  }
  if (!identical(authority, "diagnostic_unavailable")) {
    if (!isTRUE(STPD_CANDIDATE_LINEAGE_LIVE_PUBLICATION_ENABLED)) {
      stpd_candidate_lineage_live_abort(
        "publication_not_enabled",
        "Live publication authority is disabled until direct coverage closes."
      )
    }
    if (!identical(receipt$requested_audit_level, "full") ||
        !identical(level, "full") ||
        !identical(receipt$instrumentation_status, "direct_complete")) {
      stpd_candidate_lineage_live_abort(
        "publication_receipt_invalid",
        "Publication authority requires a full request and direct-complete receipt."
      )
    }
  }
  if (identical(receipt$instrumentation_status, "pipeline_not_instrumented")) {
    if (!identical(level, "off") ||
        !identical(authority, "diagnostic_unavailable") ||
        !identical(envelope$metadata$unavailable_reason,
                   "pipeline_not_instrumented")) {
      stpd_candidate_lineage_live_abort(
        "instrumentation_claim_invalid",
        "An uninstrumented pipeline must materialize an off unavailable envelope."
      )
    }
  }
  tables <- c(
    envelope$bundle,
    list(
      authoritative_products = envelope$authoritative_products,
      expected_cap_scopes = envelope$expected_cap_scopes
    )
  )
  for (table_name in names(tables)) {
    stpd_candidate_lineage_live_validate_scope_table(
      tables[[table_name]], table_name, receipt
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_live_validate_scientific_binding <- function(
    scientific_result, receipt) {
  if (!is.list(scientific_result) || is.data.frame(scientific_result)) {
    stpd_candidate_lineage_live_abort(
      "scientific_result_invalid",
      "Live attachment requires a named scientific result list."
    )
  }
  results <- scientific_result$results
  if (!is.list(results)) return(invisible(TRUE))
  public <- results$run_metadata_public
  internal <- results$run_metadata
  first_text <- function(table, field) {
    if (!is.data.frame(table) || !nrow(table) || !(field %in% names(table))) {
      return(NULL)
    }
    if (nrow(table) != 1L) {
      stpd_candidate_lineage_live_abort(
        "scientific_metadata_invalid",
        "Candidate-lineage scientific run metadata must contain exactly one row."
      )
    }
    value <- as.character(table[[field]][1L])
    if (length(value) != 1L || is.na(value) || !nzchar(value)) NULL else value
  }
  public_run_id <- first_text(public, "run_id")
  internal_run_id <- first_text(internal, "run_id")
  public_params_hash <- first_text(public, "params_hash")
  internal_params_hash <- first_text(internal, "params_hash")
  public_selected <- first_text(public, "selected_trains")
  internal_selected <- first_text(internal, "selected_trains")
  pairs <- list(
    run_id = c(public_run_id, internal_run_id),
    params_hash = c(public_params_hash, internal_params_hash),
    selected_trains = c(public_selected, internal_selected)
  )
  for (field in names(pairs)) {
    values <- pairs[[field]]
    if (length(values) == 2L && !identical(values[[1L]], values[[2L]])) {
      stpd_candidate_lineage_live_abort(
        "scientific_metadata_conflict",
        paste0("Public and internal run metadata disagree on ", field, "."),
        field = field
      )
    }
  }
  run_id <- public_run_id %||% internal_run_id
  params_hash <- public_params_hash %||% internal_params_hash
  dataset_id <- first_text(public, "dataset_name")
  selected <- public_selected %||% internal_selected
  if (!is.null(run_id)) {
    stpd_candidate_lineage_live_expected_text(
      receipt$run_id, run_id, "run_id"
    )
  }
  if (!is.null(params_hash)) {
    stpd_candidate_lineage_live_expected_text(
      receipt$params_hash, params_hash, "params_hash"
    )
  }
  if (!is.null(dataset_id)) {
    stpd_candidate_lineage_live_expected_text(
      receipt$dataset_id, dataset_id, "dataset_id"
    )
  }
  if (!is.null(selected)) {
    selected_trains <- strsplit(selected, ";", fixed = TRUE)[[1L]]
    if (!identical(receipt$target_trains, selected_trains)) {
      stpd_candidate_lineage_live_abort(
        "scientific_scope_mismatch",
        "Receipt target_trains do not match scientific run metadata.",
        field = "target_trains"
      )
    }
  }
  invisible(TRUE)
}

stpd_candidate_lineage_live_validate_envelope <- function(
    envelope, receipt = NULL,
    expected_run_id = NULL, expected_params_hash = NULL,
    expected_dataset_id = NULL, expected_target_trains = NULL) {
  stpd_candidate_lineage_live_require_active_binding(
    expected_run_id, expected_params_hash, expected_dataset_id,
    expected_target_trains
  )
  stpd_candidate_lineage_validate_envelope(envelope)
  level <- envelope$metadata$audit_level
  authority <- envelope$metadata$evidence_authority
  products <- envelope$authoritative_products
  caps <- envelope$expected_cap_scopes
  if (!identical(authority, "publication_authoritative") && nrow(products)) {
    stpd_candidate_lineage_live_abort(
      "diagnostic_products_forbidden",
      paste(
        "Diagnostic-unavailable evidence must carry a typed-empty",
        "authoritative-product manifest."
      )
    )
  }
  if (level %in% c("off", "summary") && nrow(caps)) {
    stpd_candidate_lineage_live_abort(
      "reduced_audit_caps_forbidden",
      "Off and summary evidence must carry a typed-empty cap manifest."
    )
  }
  if (identical(level, "off") && nrow(envelope$source_adapter_registry)) {
    stpd_candidate_lineage_live_abort(
      "off_source_adapters_forbidden",
      "Off evidence must carry a typed-empty source-adapter registry."
    )
  }
  embedded_receipt <- attr(
    envelope, "candidate_lineage_request_receipt", exact = TRUE
  )
  if (is.null(receipt)) receipt <- embedded_receipt
  if (is.null(receipt)) {
    stpd_candidate_lineage_live_abort(
      "receipt_missing",
      "A live candidate-lineage envelope requires a request receipt."
    )
  }
  if (is.null(embedded_receipt) || !identical(embedded_receipt, receipt)) {
    stpd_candidate_lineage_live_abort(
      "receipt_binding_invalid",
      "The embedded request receipt does not match the validated receipt."
    )
  }
  receipt_hash <- attr(
    envelope, "candidate_lineage_request_receipt_sha256", exact = TRUE
  )
  expected_receipt_hash <- stpd_candidate_lineage_live_receipt_sha256(receipt)
  if (!is.character(receipt_hash) || length(receipt_hash) != 1L ||
      is.na(receipt_hash) || !identical(receipt_hash, expected_receipt_hash)) {
    stpd_candidate_lineage_live_abort(
      "receipt_hash_mismatch",
      "The embedded request receipt hash does not match its payload."
    )
  }
  stpd_candidate_lineage_live_validate_receipt(
    receipt, envelope, expected_run_id, expected_params_hash,
    expected_dataset_id, expected_target_trains
  )
  invisible(TRUE)
}

stpd_candidate_lineage_live_attach <- function(
    scientific_result, audit_bundle,
    authoritative_products = stpd_candidate_lineage_empty_authoritative_products(),
    expected_cap_scopes = stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = c("publication_authoritative", "diagnostic_unavailable"),
    source_adapters = stpd_candidate_lineage_empty_source_adapters(),
    unavailable_reason = NULL, receipt,
    expected_run_id = NULL, expected_params_hash = NULL,
    expected_dataset_id = NULL, expected_target_trains = NULL) {
  validation_mode <- match.arg(validation_mode)
  stpd_candidate_lineage_live_require_active_binding(
    expected_run_id, expected_params_hash, expected_dataset_id,
    expected_target_trains
  )
  stpd_candidate_lineage_live_validate_receipt_shape(receipt)
  stpd_candidate_lineage_live_expected_text(
    receipt$run_id, expected_run_id, "run_id"
  )
  stpd_candidate_lineage_live_expected_text(
    receipt$params_hash, expected_params_hash, "params_hash"
  )
  stpd_candidate_lineage_live_expected_text(
    receipt$dataset_id, expected_dataset_id, "dataset_id"
  )
  if (!is.null(expected_target_trains) &&
      !identical(receipt$target_trains, expected_target_trains)) {
    stpd_candidate_lineage_live_abort(
      "binding_mismatch",
      "Receipt target_trains do not match the ordered active run scope.",
      field = "target_trains"
    )
  }
  stpd_candidate_lineage_live_validate_scientific_binding(
    scientific_result, receipt
  )
  stpd_candidate_lineage_assert_external_schema(
    authoritative_products,
    stpd_candidate_lineage_authoritative_product_schema(),
    "authoritative_product"
  )
  stpd_candidate_lineage_validate_cap_policies(expected_cap_scopes)
  if (!identical(validation_mode, "publication_authoritative") &&
      nrow(authoritative_products)) {
    stpd_candidate_lineage_live_abort(
      "diagnostic_products_forbidden",
      "Diagnostic-unavailable evidence cannot attach authoritative products."
    )
  }
  materialized_level <- if (
    all(vapply(audit_bundle, nrow, integer(1)) == 0L)
  ) "off" else if (!nrow(audit_bundle$candidate_lineage_edges) &&
                    !nrow(audit_bundle$candidate_stage_events)) {
    "summary"
  } else "full"
  if (materialized_level %in% c("off", "summary") &&
      nrow(expected_cap_scopes)) {
    stpd_candidate_lineage_live_abort(
      "reduced_audit_caps_forbidden",
      "Off and summary materializations cannot attach cap policies."
    )
  }
  if (identical(validation_mode, "publication_authoritative") &&
      !isTRUE(STPD_CANDIDATE_LINEAGE_LIVE_PUBLICATION_ENABLED)) {
    stpd_candidate_lineage_live_abort(
      "publication_not_enabled",
      "Live publication authority is disabled until direct coverage closes."
    )
  }
  out <- stpd_candidate_lineage_attach(
    scientific_result, audit_bundle,
    authoritative_products = authoritative_products,
    expected_cap_scopes = expected_cap_scopes,
    validation_mode = validation_mode,
    source_adapters = source_adapters,
    unavailable_reason = unavailable_reason
  )
  envelope <- out$candidate_lineage_audit
  attr(envelope, "candidate_lineage_request_receipt") <- receipt
  attr(envelope, "candidate_lineage_request_receipt_sha256") <-
    stpd_candidate_lineage_live_receipt_sha256(receipt)
  stpd_candidate_lineage_live_validate_envelope(
    envelope, receipt, expected_run_id, expected_params_hash,
    expected_dataset_id, expected_target_trains
  )
  out$candidate_lineage_audit <- envelope
  out
}

stpd_candidate_lineage_live_validate_result <- function(
    scientific_result, expected_run_id, expected_params_hash,
    expected_dataset_id, expected_target_trains) {
  stpd_candidate_lineage_live_require_active_binding(
    expected_run_id, expected_params_hash, expected_dataset_id,
    expected_target_trains
  )
  if (!is.list(scientific_result) ||
      !("candidate_lineage_audit" %in% names(scientific_result))) {
    stpd_candidate_lineage_live_abort(
      "envelope_missing",
      "The scientific result does not contain live candidate-lineage evidence."
    )
  }
  envelope <- scientific_result$candidate_lineage_audit
  receipt <- attr(
    envelope, "candidate_lineage_request_receipt", exact = TRUE
  )
  stpd_candidate_lineage_live_validate_scientific_binding(
    scientific_result, receipt
  )
  stpd_candidate_lineage_live_validate_envelope(
    envelope, receipt, expected_run_id, expected_params_hash,
    expected_dataset_id, expected_target_trains
  )
  invisible(TRUE)
}
