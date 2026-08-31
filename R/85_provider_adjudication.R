# Provider-agnostic adjudication v2 ------------------------------------------

STPD_PROVIDER_ADJUDICATION_VERSION <- "stpd_provider_adjudication_v2"
STPD_PROVIDER_ADJUDICATION_REQUEST_VERSION <-
  "stpd_provider_adjudication_request_v2"

stpd_provider_adjudication_actions <- function() {
  c("accept_as_is", "reject", "adjust_bounds", "void_prior_decision")
}

stpd_provider_adjudication_abort <- function(code, message,
                                              provider_run_id = NA_character_,
                                              source_record_id = NA_character_,
                                              value = NULL) {
  stpd_provider_contract_abort(
    code, message, table = "provider_adjudication_v2",
    provider_run_id = provider_run_id,
    source_record_key = source_record_id,
    offending_value = value
  )
}

stpd_provider_adjudication_empty <- function() {
  list(
    history = data.frame(
      schema_version = character(), sequence = integer(),
      decision_id = character(), operation_id = character(),
      request_sha256 = character(), action = character(),
      provider_run_id = character(), source_record_id = character(),
      source_record_key = character(), source_candidate_sha256 = character(),
      expected_parent_product_sha256 = character(),
      target_decision_id = character(), prior_decision_id = character(),
      adjusted_start_isi = integer(), adjusted_end_isi = integer(),
      adjusted_start_spike = integer(), adjusted_end_spike = integer(),
      adjusted_start_time_sec = double(), adjusted_end_time_sec = double(),
      reviewer_id = character(), reason = character(), decided_utc = character(),
      transition_sha256 = character(), stringsAsFactors = FALSE
    ),
    current_decisions = data.frame(
      provider_run_id = character(), source_record_id = character(),
      current_decision_id = character(), action = character(),
      effect = character(), adjudicated_interval_id = character(),
      stringsAsFactors = FALSE
    ),
    adjudicated_intervals = data.frame(
      adjudicated_interval_id = character(), provider_run_id = character(),
      source_record_id = character(), source_record_key = character(),
      train_key = character(), dataset_snapshot_sha256 = character(),
      semantic_track = character(), proposed_label = character(),
      canonical_start_isi = integer(), canonical_end_isi = integer(),
      canonical_start_spike = integer(), canonical_end_spike = integer(),
      canonical_start_time_sec = double(), canonical_end_time_sec = double(),
      canonical_interval_closure = character(), geometry_origin = character(),
      decision_id = character(), stringsAsFactors = FALSE
    ),
    lineage = data.frame(
      adjudicated_interval_id = character(), source_candidate_id = character(),
      source_geometry_sha256 = character(), decision_id = character(),
      relation = character(), stringsAsFactors = FALSE
    )
  )
}

stpd_provider_adjudication_source_sha256 <- function(provider_bundle) {
  stpd_provider_hash_domain(
    "stpd-provider-adjudication-source-v2", provider_bundle
  )
}

stpd_provider_adjudication_payload <- function(x) {
  x[c("schema_version", "information_access", "authority_scope",
      "performance_use", "legacy_compatibility", "source_bundle_sha256", "history",
      "current_decisions", "adjudicated_intervals", "lineage")]
}

stpd_provider_adjudication_product_sha256 <- function(x) {
  stpd_provider_hash_domain(
    "stpd-provider-adjudication-product-v2",
    stpd_provider_adjudication_payload(x)
  )
}

stpd_provider_adjudication_candidate <- function(provider_bundle,
                                                  provider_run_id,
                                                  source_record_id) {
  rows <- provider_bundle$candidate_intervals
  hit <- which(rows$provider_run_id == provider_run_id &
                 rows$candidate_id == source_record_id)
  if (length(hit) != 1L) {
    stpd_provider_adjudication_abort(
      "review_parent_not_found",
      "The provider run and source record do not identify one candidate.",
      provider_run_id, source_record_id
    )
  }
  rows[hit, , drop = FALSE]
}

#' Create an empty provider adjudication v2 descendant
#'
#' @param provider_bundle A validated immutable provider bundle v1.
#' @return A separately hashed, empty adjudication v2 product.
#' @export
stpd_new_provider_adjudication <- function(provider_bundle) {
  before <- serialize(provider_bundle, NULL, version = 3L)
  stpd_validate_provider_bundle(provider_bundle)
  empty <- stpd_provider_adjudication_empty()
  out <- c(list(
    schema_version = STPD_PROVIDER_ADJUDICATION_VERSION,
    information_access = "manual_aware",
    authority_scope = "adjudicated_prediction_record",
    performance_use = "adjudicated_agreement_only",
    legacy_compatibility = "phase2b_v1_read_only",
    source_bundle_sha256 =
      stpd_provider_adjudication_source_sha256(provider_bundle)
  ), empty)
  out$product_sha256 <- stpd_provider_adjudication_product_sha256(out)
  class(out) <- c("stpd_provider_adjudication_v2", "list")
  if (!identical(before, serialize(provider_bundle, NULL, version = 3L))) {
    stpd_provider_adjudication_abort(
      "review_source_stale", "Creating adjudication mutated provider AUTO."
    )
  }
  out
}

stpd_provider_adjudication_scalar <- function(x, name, allow_na = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.object(x) ||
      (!allow_na && (is.na(x) || !nzchar(x))) ||
      (allow_na && !is.na(x) && !nzchar(x))) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch",
      paste0(name, " must be one plain non-empty character scalar."), value = x
    )
  }
  enc2utf8(x)
}

stpd_provider_adjudication_integer <- function(x, name) {
  if (length(x) != 1L || is.object(x) ||
      !typeof(x) %in% c("integer", "double") || !is.finite(x) ||
      x != floor(x) || x < 1 || x > .Machine$integer.max) {
    stpd_provider_adjudication_abort(
      "review_bounds_invalid", paste0(name, " must be one positive integer."),
      value = x
    )
  }
  as.integer(x)
}

stpd_provider_adjudication_request <- function(request) {
  required <- c(
    "schema_version", "operation_id", "action", "provider_run_id",
    "source_record_id", "expected_product_sha256", "reviewer_id", "reason",
    "decided_utc", "adjusted_start_isi", "adjusted_end_isi",
    "target_decision_id"
  )
  if (!is.list(request) || is.object(request) ||
      is.null(names(request)) || !identical(names(request), required)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch",
      "The adjudication request must use the exact ordered v2 fields."
    )
  }
  out <- request
  for (name in required[1:9]) {
    out[[name]] <- stpd_provider_adjudication_scalar(out[[name]], name)
  }
  byte_limits <- c(
    schema_version = 128L, operation_id = 256L, action = 64L,
    provider_run_id = 64L, source_record_id = 64L,
    expected_product_sha256 = 64L, reviewer_id = 256L, reason = 4096L,
    decided_utc = 64L
  )
  too_long <- names(byte_limits)[vapply(names(byte_limits), function(name) {
    nchar(out[[name]], type = "bytes") > byte_limits[[name]]
  }, logical(1))]
  if (length(too_long)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch",
      paste0(too_long[[1L]], " exceeds the v2 byte limit.")
    )
  }
  if (!identical(out$schema_version,
                 STPD_PROVIDER_ADJUDICATION_REQUEST_VERSION)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch", "Unsupported adjudication request version."
    )
  }
  if (!(out$action %in% stpd_provider_adjudication_actions())) {
    stpd_provider_adjudication_abort(
      "review_action_unsupported", "The requested review action is unsupported.",
      out$provider_run_id, out$source_record_id, out$action
    )
  }
  if (!grepl("^[0-9a-f]{64}$", out$expected_product_sha256)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch", "expected_product_sha256 is invalid."
    )
  }
  if (!grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$",
    out$decided_utc
  )) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch", "decided_utc must be RFC3339 UTC."
    )
  }
  parsed_time <- suppressWarnings(as.POSIXct(
    sub("Z$", "", out$decided_utc), format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  ))
  if (is.na(parsed_time)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch", "decided_utc is not a valid UTC date."
    )
  }
  out$adjusted_start_isi <- if (is.null(out$adjusted_start_isi) ||
                                (length(out$adjusted_start_isi) == 1L &&
                                 is.na(out$adjusted_start_isi))) NA_integer_ else
    stpd_provider_adjudication_integer(out$adjusted_start_isi,
                                       "adjusted_start_isi")
  out$adjusted_end_isi <- if (is.null(out$adjusted_end_isi) ||
                              (length(out$adjusted_end_isi) == 1L &&
                               is.na(out$adjusted_end_isi))) NA_integer_ else
    stpd_provider_adjudication_integer(out$adjusted_end_isi,
                                       "adjusted_end_isi")
  out$target_decision_id <- if (is.null(out$target_decision_id) ||
                                (length(out$target_decision_id) == 1L &&
                                 is.na(out$target_decision_id))) NA_character_ else
    stpd_provider_adjudication_scalar(out$target_decision_id,
                                      "target_decision_id")
  bounds_action <- identical(out$action, "adjust_bounds")
  void_action <- identical(out$action, "void_prior_decision")
  has_both_bounds <- !is.na(out$adjusted_start_isi) &&
    !is.na(out$adjusted_end_isi)
  has_any_bound <- !is.na(out$adjusted_start_isi) ||
    !is.na(out$adjusted_end_isi)
  if ((bounds_action && !has_both_bounds) ||
      (!bounds_action && has_any_bound)) {
    stpd_provider_adjudication_abort(
      "review_bounds_invalid",
      "Only adjust_bounds supplies both adjusted ISI boundaries."
    )
  }
  if (void_action != !is.na(out$target_decision_id)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch",
      "Only void_prior_decision supplies target_decision_id."
    )
  }
  out
}

#' Build an exact provider adjudication v2 request
#'
#' @param adjudication The current provider adjudication v2 parent.
#' @param action One supported v2 action.
#' @param provider_run_id,source_record_id The immutable provider target.
#' @param operation_id A caller-generated idempotency key.
#' @param reviewer_id A reviewer pseudonym, not direct identity.
#' @param reason A non-empty audit reason.
#' @param decided_utc An explicit RFC3339 UTC audit time.
#' @param adjusted_start_isi,adjusted_end_isi Bounds for `adjust_bounds` only.
#' @param target_decision_id Current decision for `void_prior_decision` only.
#' @return An exact ordered request accepted by
#'   [stpd_apply_provider_adjudication()].
#' @export
stpd_provider_review_request <- function(
    adjudication, action, provider_run_id, source_record_id, operation_id,
    reviewer_id, reason, decided_utc, adjusted_start_isi = NULL,
    adjusted_end_isi = NULL, target_decision_id = NULL) {
  if (!is.list(adjudication) || is.null(adjudication$product_sha256)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch",
      "A current adjudication v2 parent is required."
    )
  }
  request <- list(
    schema_version = STPD_PROVIDER_ADJUDICATION_REQUEST_VERSION,
    operation_id = operation_id, action = action,
    provider_run_id = provider_run_id, source_record_id = source_record_id,
    expected_product_sha256 = adjudication$product_sha256,
    reviewer_id = reviewer_id, reason = reason, decided_utc = decided_utc,
    adjusted_start_isi = adjusted_start_isi,
    adjusted_end_isi = adjusted_end_isi,
    target_decision_id = target_decision_id
  )
  stpd_provider_adjudication_request(request)
}

stpd_provider_adjudication_request_sha256 <- function(request) {
  stpd_provider_hash_domain("stpd-provider-adjudication-request-v2", request)
}

stpd_provider_adjudication_request_from_history <- function(row) {
  stpd_provider_adjudication_request(list(
    schema_version = STPD_PROVIDER_ADJUDICATION_REQUEST_VERSION,
    operation_id = row$operation_id[[1L]], action = row$action[[1L]],
    provider_run_id = row$provider_run_id[[1L]],
    source_record_id = row$source_record_id[[1L]],
    expected_product_sha256 = row$expected_parent_product_sha256[[1L]],
    reviewer_id = row$reviewer_id[[1L]], reason = row$reason[[1L]],
    decided_utc = row$decided_utc[[1L]],
    adjusted_start_isi = if (is.na(row$adjusted_start_isi[[1L]])) NULL else
      row$adjusted_start_isi[[1L]],
    adjusted_end_isi = if (is.na(row$adjusted_end_isi[[1L]])) NULL else
      row$adjusted_end_isi[[1L]],
    target_decision_id = if (is.na(row$target_decision_id[[1L]])) NULL else
      row$target_decision_id[[1L]]
  ))
}

stpd_provider_adjudication_source_geometry_sha256 <- function(candidate) {
  fields <- c(
    "candidate_id", "provider_run_id", "train_key", "train_timestamp_sha256",
    "semantic_track", "proposed_label", "canonical_start_isi",
    "canonical_end_isi", "canonical_start_spike", "canonical_end_spike",
    "canonical_start_time_sec", "canonical_end_time_sec",
    "canonical_interval_closure"
  )
  stpd_provider_hash_domain(
    "stpd-provider-adjudication-source-geometry-v2",
    as.list(candidate[1L, fields, drop = FALSE])
  )
}

stpd_provider_adjudication_dataset_geometry <- function(dataset, candidate,
                                                         start_isi, end_isi) {
  if (is.null(dataset)) {
    stpd_provider_adjudication_abort(
      "review_hard_boundary_crossed",
      "adjust_bounds requires the immutable source dataset for boundary proof.",
      candidate$provider_run_id, candidate$candidate_id
    )
  }
  dataset_before <- serialize(dataset, NULL, version = 3L)
  spine <- stpd_provider_build_spine_v1(
    dataset, selected_trains = candidate$train_key
  )
  manifest <- spine$train_manifest[1L, , drop = FALSE]
  if (!identical(manifest$train_timestamp_sha256,
                 candidate$train_timestamp_sha256)) {
    stpd_provider_adjudication_abort(
      "review_source_stale", "The adjustment dataset no longer matches its train.",
      candidate$provider_run_id, candidate$candidate_id
    )
  }
  n <- manifest$n_spikes[[1L]]
  if (start_isi < 2L || end_isi > n || start_isi > end_isi) {
    stpd_provider_adjudication_abort(
      "review_hard_boundary_crossed",
      "Adjusted bounds cross the canonical train acquisition boundary.",
      candidate$provider_run_id, candidate$candidate_id,
      c(start_isi, end_isi)
    )
  }
  source_start <- candidate$canonical_start_isi[[1L]]
  source_end <- candidate$canonical_end_isi[[1L]]
  if (end_isi < source_start || start_isi > source_end) {
    stpd_provider_adjudication_abort(
      "review_bounds_invalid",
      "Adjusted geometry must overlap its immutable source interval.",
      candidate$provider_run_id, candidate$candidate_id
    )
  }
  if (!identical(dataset_before, serialize(dataset, NULL, version = 3L))) {
    stpd_provider_adjudication_abort(
      "review_source_stale", "Boundary validation mutated the source dataset."
    )
  }
  timestamps <- spine$timestamps[[candidate$train_key[[1L]]]]
  c(start_isi = as.integer(start_isi), end_isi = as.integer(end_isi),
    start_spike = as.integer(start_isi - 1L), end_spike = as.integer(end_isi),
    start_time_sec = timestamps[[start_isi - 1L]],
    end_time_sec = timestamps[[end_isi]])
}

stpd_provider_adjudication_interval_rows <- function(candidate, history_row,
                                                      provider_bundle) {
  action <- history_row$action[[1L]]
  positive <- identical(candidate$provider_decision[[1L]], "positive")
  if (!positive || !(action %in% c("accept_as_is", "adjust_bounds"))) {
    return(list(interval = stpd_provider_adjudication_empty()$adjudicated_intervals,
                lineage = stpd_provider_adjudication_empty()$lineage,
                interval_id = NA_character_))
  }
  adjusted <- identical(action, "adjust_bounds")
  geometry <- if (adjusted) {
    c(
      canonical_start_isi = history_row$adjusted_start_isi[[1L]],
      canonical_end_isi = history_row$adjusted_end_isi[[1L]],
      canonical_start_spike = history_row$adjusted_start_spike[[1L]],
      canonical_end_spike = history_row$adjusted_end_spike[[1L]],
      canonical_start_time_sec = history_row$adjusted_start_time_sec[[1L]],
      canonical_end_time_sec = history_row$adjusted_end_time_sec[[1L]]
    )
  } else {
    unlist(candidate[1L, c("canonical_start_isi", "canonical_end_isi",
                           "canonical_start_spike", "canonical_end_spike",
                           "canonical_start_time_sec",
                           "canonical_end_time_sec")],
           use.names = TRUE)
  }
  interval_id <- stpd_provider_hash_domain(
    "stpd-provider-adjudicated-interval-v2",
    list(source_candidate_id = candidate$candidate_id[[1L]],
         relation = if (adjusted) "adjusted_bounds" else "accepted_as_is",
         geometry = geometry)
  )
  run <- history_row$provider_run_id[[1L]]
  run_row <- provider_bundle$provider_runs[
    provider_bundle$provider_runs$provider_run_id == run, , drop = FALSE
  ]
  interval <- data.frame(
    adjudicated_interval_id = interval_id, provider_run_id = run,
    source_record_id = candidate$candidate_id[[1L]],
    source_record_key = candidate$source_record_key[[1L]],
    train_key = candidate$train_key[[1L]],
    dataset_snapshot_sha256 = run_row$dataset_snapshot_sha256[[1L]],
    semantic_track = candidate$semantic_track[[1L]],
    proposed_label = candidate$proposed_label[[1L]],
    canonical_start_isi = as.integer(geometry[["canonical_start_isi"]]),
    canonical_end_isi = as.integer(geometry[["canonical_end_isi"]]),
    canonical_start_spike = as.integer(geometry[["canonical_start_spike"]]),
    canonical_end_spike = as.integer(geometry[["canonical_end_spike"]]),
    canonical_start_time_sec =
      as.double(geometry[["canonical_start_time_sec"]]),
    canonical_end_time_sec =
      as.double(geometry[["canonical_end_time_sec"]]),
    canonical_interval_closure = candidate$canonical_interval_closure[[1L]],
    geometry_origin = if (adjusted) "adjusted" else "source",
    decision_id = history_row$decision_id[[1L]], stringsAsFactors = FALSE
  )
  lineage <- data.frame(
    adjudicated_interval_id = interval_id,
    source_candidate_id = candidate$candidate_id[[1L]],
    source_geometry_sha256 =
      stpd_provider_adjudication_source_geometry_sha256(candidate),
    decision_id = history_row$decision_id[[1L]],
    relation = if (adjusted) "adjusted_bounds" else "accepted_as_is",
    stringsAsFactors = FALSE
  )
  list(interval = interval, lineage = lineage, interval_id = interval_id)
}

stpd_provider_adjudication_sort <- function(product) {
  if (nrow(product$current_decisions)) {
    product$current_decisions <- product$current_decisions[order(
      product$current_decisions$provider_run_id,
      product$current_decisions$source_record_id, method = "radix"
    ), , drop = FALSE]
    rownames(product$current_decisions) <- NULL
  }
  if (nrow(product$adjudicated_intervals)) {
    product$adjudicated_intervals <- product$adjudicated_intervals[order(
      product$adjudicated_intervals$adjudicated_interval_id,
      method = "radix"
    ), , drop = FALSE]
    rownames(product$adjudicated_intervals) <- NULL
  }
  if (nrow(product$lineage)) {
    product$lineage <- product$lineage[order(
      product$lineage$adjudicated_interval_id, method = "radix"
    ), , drop = FALSE]
    rownames(product$lineage) <- NULL
  }
  product
}

stpd_provider_adjudication_apply_resolved <- function(product, provider_bundle,
                                                       row) {
  candidate <- stpd_provider_adjudication_candidate(
    provider_bundle, row$provider_run_id[[1L]], row$source_record_id[[1L]]
  )
  key_hit <- product$current_decisions$provider_run_id == row$provider_run_id &
    product$current_decisions$source_record_id == row$source_record_id
  current <- which(key_hit)
  if (identical(row$action[[1L]], "void_prior_decision")) {
    if (length(current) != 1L ||
        !identical(product$current_decisions$current_decision_id[[current]],
                   row$target_decision_id[[1L]])) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch",
        "Compensation must target the current effective decision.",
        row$provider_run_id[[1L]], row$source_record_id[[1L]]
      )
    }
    interval_id <- product$current_decisions$adjudicated_interval_id[[current]]
    product$current_decisions <- product$current_decisions[-current, , drop = FALSE]
    if (!is.na(interval_id)) {
      product$adjudicated_intervals <- product$adjudicated_intervals[
        product$adjudicated_intervals$adjudicated_interval_id != interval_id,
        , drop = FALSE
      ]
      product$lineage <- product$lineage[
        product$lineage$adjudicated_interval_id != interval_id, , drop = FALSE
      ]
    }
  } else {
    if (length(current)) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch",
        "Void the current decision before applying a different decision.",
        row$provider_run_id[[1L]], row$source_record_id[[1L]]
      )
    }
    materialized <- stpd_provider_adjudication_interval_rows(
      candidate, row, provider_bundle
    )
    effect <- switch(row$action[[1L]], accept_as_is = "accepted",
                     reject = "rejected", adjust_bounds = "adjusted")
    product$current_decisions <- rbind(
      product$current_decisions,
      data.frame(
        provider_run_id = row$provider_run_id[[1L]],
        source_record_id = row$source_record_id[[1L]],
        current_decision_id = row$decision_id[[1L]], action = row$action[[1L]],
        effect = effect, adjudicated_interval_id = materialized$interval_id,
        stringsAsFactors = FALSE
      )
    )
    product$adjudicated_intervals <- rbind(
      product$adjudicated_intervals, materialized$interval
    )
    product$lineage <- rbind(product$lineage, materialized$lineage)
  }
  product$history <- rbind(product$history, row)
  product <- stpd_provider_adjudication_sort(product)
  product$product_sha256 <- stpd_provider_adjudication_product_sha256(product)
  class(product) <- c("stpd_provider_adjudication_v2", "list")
  product
}

#' Apply one append-only provider adjudication decision
#'
#' @param provider_bundle The immutable provider bundle v1.
#' @param adjudication A provider adjudication v2 product created from the bundle.
#' @param request An exact ordered `stpd_provider_adjudication_request_v2` list.
#' @param dataset The source dataset, required only for `adjust_bounds`.
#' @return A new adjudication descendant. Provider AUTO is not modified.
#' @export
stpd_apply_provider_adjudication <- function(provider_bundle, adjudication,
                                              request, dataset = NULL) {
  bundle_before <- serialize(provider_bundle, NULL, version = 3L)
  adjudication_before <- serialize(adjudication, NULL, version = 3L)
  stpd_validate_provider_bundle(provider_bundle)
  stpd_validate_provider_adjudication(provider_bundle, adjudication)
  request <- stpd_provider_adjudication_request(request)
  request_sha <- stpd_provider_adjudication_request_sha256(request)
  duplicate <- which(adjudication$history$operation_id == request$operation_id)
  if (length(duplicate)) {
    if (length(duplicate) == 1L &&
        identical(adjudication$history$request_sha256[[duplicate]], request_sha)) {
      return(adjudication)
    }
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch",
      "operation_id was already used with a different request."
    )
  }
  if (!identical(request$expected_product_sha256,
                 adjudication$product_sha256)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch", "The adjudication parent hash is stale."
    )
  }
  candidate <- stpd_provider_adjudication_candidate(
    provider_bundle, request$provider_run_id, request$source_record_id
  )
  current <- adjudication$current_decisions[
    adjudication$current_decisions$provider_run_id == request$provider_run_id &
      adjudication$current_decisions$source_record_id == request$source_record_id,
    , drop = FALSE
  ]
  prior <- if (nrow(current)) current$current_decision_id[[1L]] else NA_character_
  if (request$action == "void_prior_decision") {
    if (!nrow(current) ||
        !identical(request$target_decision_id, prior)) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch",
        "void_prior_decision must target the current effective decision.",
        request$provider_run_id, request$source_record_id
      )
    }
  } else if (nrow(current)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch",
      "Void the current decision before applying a different decision.",
      request$provider_run_id, request$source_record_id
    )
  }
  geometry <- c(start_isi = NA_integer_, end_isi = NA_integer_,
                start_spike = NA_integer_, end_spike = NA_integer_,
                start_time_sec = NA_real_, end_time_sec = NA_real_)
  if (request$action == "adjust_bounds") {
    if (!identical(candidate$provider_decision[[1L]], "positive")) {
      stpd_provider_adjudication_abort(
        "review_bounds_invalid", "Only a positive interval can adjust bounds.",
        request$provider_run_id, request$source_record_id
      )
    }
    geometry <- stpd_provider_adjudication_dataset_geometry(
      dataset, candidate, request$adjusted_start_isi, request$adjusted_end_isi
    )
  }
  sequence <- nrow(adjudication$history) + 1L
  decision_id <- stpd_provider_hash_domain(
    "stpd-provider-adjudication-decision-v2",
    list(sequence = sequence, request_sha256 = request_sha,
         source_candidate_sha256 = candidate$candidate_id[[1L]])
  )
  row <- data.frame(
    schema_version = STPD_PROVIDER_ADJUDICATION_VERSION,
    sequence = as.integer(sequence), decision_id = decision_id,
    operation_id = request$operation_id, request_sha256 = request_sha,
    action = request$action, provider_run_id = request$provider_run_id,
    source_record_id = request$source_record_id,
    source_record_key = candidate$source_record_key[[1L]],
    source_candidate_sha256 = candidate$candidate_id[[1L]],
    expected_parent_product_sha256 = request$expected_product_sha256,
    target_decision_id = request$target_decision_id,
    prior_decision_id = prior,
    adjusted_start_isi = as.integer(geometry[["start_isi"]]),
    adjusted_end_isi = as.integer(geometry[["end_isi"]]),
    adjusted_start_spike = as.integer(geometry[["start_spike"]]),
    adjusted_end_spike = as.integer(geometry[["end_spike"]]),
    adjusted_start_time_sec = as.double(geometry[["start_time_sec"]]),
    adjusted_end_time_sec = as.double(geometry[["end_time_sec"]]),
    reviewer_id = request$reviewer_id, reason = request$reason,
    decided_utc = request$decided_utc, transition_sha256 = NA_character_,
    stringsAsFactors = FALSE
  )
  row$transition_sha256 <- stpd_provider_hash_domain(
    "stpd-provider-adjudication-transition-v2",
    as.list(row[1L, setdiff(names(row), "transition_sha256"), drop = FALSE])
  )
  out <- stpd_provider_adjudication_apply_resolved(
    adjudication, provider_bundle, row
  )
  if (!identical(bundle_before, serialize(provider_bundle, NULL, version = 3L)) ||
      !identical(adjudication_before,
                 serialize(adjudication, NULL, version = 3L))) {
    stpd_provider_adjudication_abort(
      "review_source_stale", "Adjudication mutated an immutable parent."
    )
  }
  stpd_validate_provider_adjudication(provider_bundle, out)
  out
}

stpd_provider_adjudication_check_structure <- function(adjudication) {
  expected <- c(
    "schema_version", "information_access", "authority_scope",
    "performance_use", "legacy_compatibility", "source_bundle_sha256",
    "history", "current_decisions", "adjudicated_intervals", "lineage",
    "product_sha256"
  )
  if (!is.list(adjudication) ||
      !identical(names(adjudication), expected) ||
      !identical(adjudication$schema_version,
                 STPD_PROVIDER_ADJUDICATION_VERSION)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch", "Invalid adjudication v2 structure."
    )
  }
  authority <- c(
    information_access = "manual_aware",
    authority_scope = "adjudicated_prediction_record",
    performance_use = "adjudicated_agreement_only",
    legacy_compatibility = "phase2b_v1_read_only"
  )
  for (name in names(authority)) {
    if (!identical(adjudication[[name]], authority[[name]])) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch",
        paste0("Invalid adjudication authority field: ", name, ".")
      )
    }
  }
  prototypes <- stpd_provider_adjudication_empty()
  for (name in names(prototypes)) {
    value <- adjudication[[name]]
    prototype <- prototypes[[name]]
    if (!is.data.frame(value) || !identical(names(value), names(prototype))) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch",
        paste0("Invalid adjudication table: ", name, ".")
      )
    }
    for (column in names(prototype)) {
      if (!identical(typeof(value[[column]]), typeof(prototype[[column]]))) {
        stpd_provider_adjudication_abort(
          "review_precondition_mismatch",
          paste0("Invalid adjudication column type: ", name, "$", column, ".")
        )
      }
    }
  }
  invisible(TRUE)
}

#' Replay append-only provider adjudication history
#'
#' @param provider_bundle The exact immutable provider bundle v1.
#' @param history A v2 history table previously emitted by this package.
#' @return The deterministically reconstructed adjudication product.
#' @export
stpd_replay_provider_adjudication <- function(provider_bundle, history) {
  stpd_validate_provider_bundle(provider_bundle)
  prototype <- stpd_provider_adjudication_empty()$history
  if (!is.data.frame(history) || !identical(names(history), names(prototype)) ||
      any(vapply(names(prototype), function(name) {
        !identical(typeof(history[[name]]), typeof(prototype[[name]]))
      }, logical(1)))) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch", "Invalid adjudication history schema."
    )
  }
  product <- stpd_new_provider_adjudication(provider_bundle)
  if (!nrow(history)) return(product)
  if (!identical(history$sequence, seq_len(nrow(history))) ||
      anyDuplicated(history$decision_id) || anyDuplicated(history$operation_id)) {
    stpd_provider_adjudication_abort(
      "review_precondition_mismatch", "History sequence or identities are invalid."
    )
  }
  for (i in seq_len(nrow(history))) {
    row <- history[i, , drop = FALSE]
    if (!identical(row$schema_version[[1L]],
                   STPD_PROVIDER_ADJUDICATION_VERSION) ||
        !(row$action[[1L]] %in% stpd_provider_adjudication_actions())) {
      stpd_provider_adjudication_abort(
        "review_action_unsupported", "History contains an unsupported action."
      )
    }
    candidate <- stpd_provider_adjudication_candidate(
      provider_bundle, row$provider_run_id[[1L]], row$source_record_id[[1L]]
    )
    normalized_request <-
      stpd_provider_adjudication_request_from_history(row)
    if (!identical(row$request_sha256[[1L]],
                   stpd_provider_adjudication_request_sha256(
                     normalized_request
                   ))) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch", "History request hash is invalid."
      )
    }
    if (!identical(row$source_candidate_sha256[[1L]],
                   candidate$candidate_id[[1L]]) ||
        !identical(row$source_record_key[[1L]],
                   candidate$source_record_key[[1L]])) {
      stpd_provider_adjudication_abort(
        "review_source_stale", "History source identity is stale."
      )
    }
    if (!identical(row$expected_parent_product_sha256[[1L]],
                   product$product_sha256)) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch", "History parent hash chain is broken."
      )
    }
    current <- product$current_decisions[
      product$current_decisions$provider_run_id == row$provider_run_id[[1L]] &
        product$current_decisions$source_record_id == row$source_record_id[[1L]],
      , drop = FALSE
    ]
    expected_prior <- if (nrow(current)) {
      current$current_decision_id[[1L]]
    } else NA_character_
    if (!identical(row$prior_decision_id[[1L]], expected_prior)) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch", "History prior-decision chain is invalid."
      )
    }
    if (identical(row$action[[1L]], "adjust_bounds")) {
      bounds <- c(row$adjusted_start_isi[[1L]], row$adjusted_end_isi[[1L]])
      spikes <- c(row$adjusted_start_spike[[1L]],
                  row$adjusted_end_spike[[1L]])
      times <- c(row$adjusted_start_time_sec[[1L]],
                 row$adjusted_end_time_sec[[1L]])
      source <- c(candidate$canonical_start_isi[[1L]],
                  candidate$canonical_end_isi[[1L]])
      if (!identical(candidate$provider_decision[[1L]], "positive") ||
          anyNA(c(bounds, spikes, times)) || any(!is.finite(times)) ||
          times[[1L]] >= times[[2L]] || bounds[[1L]] < 2L ||
          bounds[[1L]] > bounds[[2L]] || spikes[[1L]] != bounds[[1L]] - 1L ||
          spikes[[2L]] != bounds[[2L]] || bounds[[2L]] < source[[1L]] ||
          bounds[[1L]] > source[[2L]]) {
        stpd_provider_adjudication_abort(
          "review_bounds_invalid", "History contains invalid adjusted geometry."
        )
      }
    } else if (any(!is.na(c(
      row$adjusted_start_isi[[1L]], row$adjusted_end_isi[[1L]],
      row$adjusted_start_spike[[1L]], row$adjusted_end_spike[[1L]],
      row$adjusted_start_time_sec[[1L]], row$adjusted_end_time_sec[[1L]]
    )))) {
      stpd_provider_adjudication_abort(
        "review_bounds_invalid", "A non-adjust action contains adjusted geometry."
      )
    }
    expected_decision <- stpd_provider_hash_domain(
      "stpd-provider-adjudication-decision-v2",
      list(sequence = i, request_sha256 = row$request_sha256[[1L]],
           source_candidate_sha256 = candidate$candidate_id[[1L]])
    )
    expected_transition <- stpd_provider_hash_domain(
      "stpd-provider-adjudication-transition-v2",
      as.list(row[1L, setdiff(names(row), "transition_sha256"), drop = FALSE])
    )
    if (!identical(row$decision_id[[1L]], expected_decision) ||
        !identical(row$transition_sha256[[1L]], expected_transition)) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch", "History identity hash is invalid."
      )
    }
    product <- stpd_provider_adjudication_apply_resolved(
      product, provider_bundle, row
    )
  }
  product
}

#' Validate a provider adjudication v2 descendant
#'
#' @param provider_bundle The exact immutable provider bundle v1.
#' @param adjudication The descendant product to validate.
#' @return Invisibly `TRUE`; otherwise a typed fail-closed error.
#' @export
stpd_validate_provider_adjudication <- function(provider_bundle, adjudication) {
  bundle_before <- serialize(provider_bundle, NULL, version = 3L)
  product_before <- serialize(adjudication, NULL, version = 3L)
  stpd_validate_provider_bundle(provider_bundle)
  stpd_provider_adjudication_check_structure(adjudication)
  source_sha <- stpd_provider_adjudication_source_sha256(provider_bundle)
  if (!identical(adjudication$source_bundle_sha256, source_sha)) {
    stpd_provider_adjudication_abort(
      "review_source_stale", "Adjudication belongs to a different provider bundle."
    )
  }
  replayed <- stpd_replay_provider_adjudication(provider_bundle,
                                                adjudication$history)
  for (name in c("information_access", "authority_scope", "performance_use",
                 "legacy_compatibility", "source_bundle_sha256", "history", "current_decisions",
                 "adjudicated_intervals", "lineage", "product_sha256")) {
    if (!identical(adjudication[[name]], replayed[[name]])) {
      stpd_provider_adjudication_abort(
        "review_precondition_mismatch",
        paste0("Adjudication replay mismatch in ", name, ".")
      )
    }
  }
  if (!identical(bundle_before, serialize(provider_bundle, NULL, version = 3L)) ||
      !identical(product_before, serialize(adjudication, NULL, version = 3L))) {
    stpd_provider_adjudication_abort(
      "review_source_stale", "Validation mutated an immutable product."
    )
  }
  invisible(TRUE)
}

#' Export a validated provider adjudication v2 product
#'
#' @param provider_bundle The exact immutable provider bundle v1.
#' @param adjudication A valid adjudication v2 descendant.
#' @param out_dir A new output directory. Existing paths fail closed.
#' @return Invisibly, the normalized output directory.
#' @export
stpd_write_provider_adjudication <- function(provider_bundle, adjudication,
                                              out_dir) {
  stpd_validate_provider_adjudication(provider_bundle, adjudication)
  if (!is.character(out_dir) || length(out_dir) != 1L || is.na(out_dir) ||
      !nzchar(out_dir)) stop("out_dir must be one path.", call. = FALSE)
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  if (file.exists(out_dir) || dir.exists(out_dir)) {
    stop("out_dir already exists.", call. = FALSE)
  }
  parent <- dirname(out_dir)
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE)
  stage <- tempfile("stpd-adjudication-v2-", tmpdir = parent)
  if (!dir.create(stage)) stop("Could not create export staging directory.",
                               call. = FALSE)
  completed <- FALSE
  on.exit(if (!completed && dir.exists(stage)) unlink(stage, recursive = TRUE),
          add = TRUE)
  saveRDS(adjudication, file.path(stage, "adjudication_v2.rds"), version = 3L)
  tables <- c("history", "current_decisions", "adjudicated_intervals", "lineage")
  for (name in tables) {
    utils::write.csv(adjudication[[name]], file.path(stage, paste0(name, ".csv")),
                     row.names = FALSE, na = "")
  }
  files <- sort(list.files(stage), method = "radix")
  manifest <- list(
    schema_version = STPD_PROVIDER_ADJUDICATION_VERSION,
    information_access = adjudication$information_access,
    authority_scope = adjudication$authority_scope,
    performance_use = adjudication$performance_use,
    source_bundle_sha256 = adjudication$source_bundle_sha256,
    product_sha256 = adjudication$product_sha256,
    files = setNames(lapply(files, function(name) {
      digest::digest(file.path(stage, name), algo = "sha256", file = TRUE,
                     serialize = FALSE)
    }), files)
  )
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
             file.path(stage, "manifest.json"), useBytes = TRUE)
  if (!file.rename(stage, out_dir)) {
    stop("Could not atomically finalize adjudication export.", call. = FALSE)
  }
  completed <- TRUE
  invisible(out_dir)
}
