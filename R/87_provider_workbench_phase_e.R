# Phase E: provider workbench view model -------------------------------------

STPD_PROVIDER_WORKBENCH_VIEW_VERSION <- "stpd_provider_workbench_view_v1"

stpd_provider_workbench_abort <- function(code, message) {
  condition <- structure(
    list(message = as.character(message)[1L], call = NULL,
         code = as.character(code)[1L]),
    class = c(as.character(code)[1L], "stpd_provider_workbench_error",
              "error", "condition")
  )
  stop(condition)
}

stpd_provider_workbench_empty <- function() {
  list(
    metadata = data.frame(
      schema_version = character(), provider_run_id = character(),
      source_mode = character(), source_bundle_sha256 = character(),
      source_product_sha256 = character(), composition_sha256 = character(),
      information_access = character(), authority_scope = character(),
      performance_use = character(), estimand_notice = character(),
      provider_runs_n = integer(), provider_records_n = integer(),
      selected_intervals_n = integer(), changed_records_n = integer(),
      relationships_n = integer(), regimes_n = integer(), conflicts_n = integer(),
      product_sha256 = character(), stringsAsFactors = FALSE
    ),
    provider_catalog = data.frame(
      provider_run_id = character(), provider_key = character(),
      provider_kind = character(), provider_version = character(),
      adapter_key = character(), adapter_version = character(),
      output_role = character(), authority_scope = character(),
      generation_mode = character(), information_access = character(),
      capability_profile = character(), run_status = character(),
      dataset_snapshot_sha256 = character(), normalized_output_sha256 = character(),
      selected = logical(), auto_composition_eligible = logical(),
      adjudication_attached = logical(), stringsAsFactors = FALSE
    ),
    provider_records = data.frame(
      provider_run_id = character(), source_record_id = character(),
      source_record_key = character(), record_kind = character(),
      provider_decision = character(), train_key = character(),
      semantic_track = character(), proposed_label = character(),
      canonical_start_isi = integer(), canonical_end_isi = integer(),
      canonical_start_spike = integer(), canonical_end_spike = integer(),
      canonical_start_time_sec = double(), canonical_end_time_sec = double(),
      score_name = character(), score_value = double(),
      uncertainty_kind = character(), uncertainty_lower = double(),
      uncertainty_upper = double(), stringsAsFactors = FALSE
    ),
    selected_intervals = data.frame(
      provider_run_id = character(), selected_interval_id = character(),
      source_record_id = character(), review_decision_id = character(),
      train_key = character(), semantic_track = character(),
      label = character(), canonical_start_isi = integer(),
      canonical_end_isi = integer(), canonical_start_spike = integer(),
      canonical_end_spike = integer(), canonical_start_time_sec = double(),
      canonical_end_time_sec = double(), geometry_origin = character(),
      selection_status = character(), stringsAsFactors = FALSE
    ),
    auto_adjudicated_delta = data.frame(
      provider_run_id = character(), source_record_id = character(),
      selected_interval_id = character(), train_key = character(),
      semantic_track = character(), label = character(), delta_status = character(),
      provider_start_isi = integer(), provider_end_isi = integer(),
      selected_start_isi = integer(), selected_end_isi = integer(),
      start_shift_isi = integer(), end_shift_isi = integer(),
      review_decision_id = character(), stringsAsFactors = FALSE
    ),
    relationships = stpd_provider_composer_empty()$relationships,
    regimes = stpd_provider_composer_empty()$regimes,
    memberships = stpd_provider_composer_empty()$memberships,
    conflicts = stpd_provider_composer_empty()$conflicts,
    manifest = data.frame(
      schema_version = character(), table_name = character(),
      row_count = integer(), column_count = integer(),
      column_types = character(), table_sha256 = character(),
      stringsAsFactors = FALSE
    )
  )
}

stpd_provider_workbench_manifest <- function(tables) {
  data.frame(
    schema_version = STPD_PROVIDER_WORKBENCH_VIEW_VERSION,
    table_name = names(tables),
    row_count = vapply(tables, nrow, integer(1)),
    column_count = vapply(tables, ncol, integer(1)),
    column_types = vapply(tables, function(x) paste(
      paste(names(x), vapply(x, typeof, character(1)), sep = ":"),
      collapse = ";"
    ), character(1)),
    table_sha256 = vapply(tables, function(x) stpd_provider_hash_domain(
      "stpd-provider-workbench-view-table-v1", x
    ), character(1)), stringsAsFactors = FALSE
  )
}

stpd_provider_workbench_catalog <- function(provider_bundle, selected_run,
                                             adjudication) {
  fields <- c(
    "provider_run_id", "provider_key", "provider_kind", "provider_version",
    "adapter_key", "adapter_version", "output_role", "authority_scope",
    "generation_mode", "information_access", "capability_profile",
    "run_status", "dataset_snapshot_sha256", "normalized_output_sha256"
  )
  out <- provider_bundle$provider_runs[, fields, drop = FALSE]
  out$selected <- out$provider_run_id == selected_run
  out$auto_composition_eligible <-
    out$output_role == "automatic_prediction" &
    out$authority_scope == "automatic_prediction_record" &
    out$run_status == "complete"
  out$adjudication_attached <- FALSE
  if (!is.null(adjudication)) {
    reviewed_runs <- unique(as.character(adjudication$history$provider_run_id))
    out$adjudication_attached <- out$provider_run_id %in% reviewed_runs
    out$adjudication_attached[out$selected] <- TRUE
  }
  out <- out[order(!out$selected, out$provider_key, out$provider_run_id,
                   method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_provider_workbench_records <- function(provider_bundle, selected_run) {
  x <- provider_bundle$candidate_intervals
  x <- x[x$provider_run_id == selected_run, , drop = FALSE]
  if (!nrow(x)) return(stpd_provider_workbench_empty()$provider_records)
  out <- data.frame(
    provider_run_id = x$provider_run_id, source_record_id = x$candidate_id,
    source_record_key = x$source_record_key, record_kind = x$record_kind,
    provider_decision = x$provider_decision, train_key = x$train_key,
    semantic_track = x$semantic_track, proposed_label = x$proposed_label,
    canonical_start_isi = x$canonical_start_isi,
    canonical_end_isi = x$canonical_end_isi,
    canonical_start_spike = x$canonical_start_spike,
    canonical_end_spike = x$canonical_end_spike,
    canonical_start_time_sec = x$canonical_start_time_sec,
    canonical_end_time_sec = x$canonical_end_time_sec,
    score_name = x$score_name, score_value = x$score_value,
    uncertainty_kind = x$uncertainty_kind,
    uncertainty_lower = x$uncertainty_lower,
    uncertainty_upper = x$uncertainty_upper, stringsAsFactors = FALSE
  )
  out <- out[order(
    out$train_key, out$semantic_track, out$canonical_start_isi,
    out$canonical_end_isi, out$source_record_id, method = "radix"
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_provider_workbench_selected <- function(composition) {
  x <- composition$children
  if (!nrow(x)) return(stpd_provider_workbench_empty()$selected_intervals)
  out <- data.frame(
    provider_run_id = x$provider_run_id, selected_interval_id = x$child_id,
    source_record_id = x$source_candidate_id,
    review_decision_id = x$review_decision_id, train_key = x$train_key,
    semantic_track = x$child_domain, label = x$child_class,
    canonical_start_isi = x$canonical_start_isi,
    canonical_end_isi = x$canonical_end_isi,
    canonical_start_spike = x$canonical_start_spike,
    canonical_end_spike = x$canonical_end_spike,
    canonical_start_time_sec = x$canonical_start_time_sec,
    canonical_end_time_sec = x$canonical_end_time_sec,
    geometry_origin = x$geometry_origin,
    selection_status = x$selection_status, stringsAsFactors = FALSE
  )
  out <- out[order(
    out$train_key, out$semantic_track, out$canonical_start_isi,
    out$canonical_end_isi, out$selected_interval_id, method = "radix"
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_provider_workbench_delta <- function(records, selected, source_mode,
                                          adjudication) {
  records <- records[records$provider_decision == "positive", , drop = FALSE]
  if (!nrow(records)) return(stpd_provider_workbench_empty()$auto_adjudicated_delta)
  current <- if (is.null(adjudication)) {
    stpd_provider_adjudication_empty()$current_decisions
  } else adjudication$current_decisions
  rows <- vector("list", nrow(records))
  for (i in seq_len(nrow(records))) {
    source <- records[i, , drop = FALSE]
    hit <- which(selected$source_record_id == source$source_record_id[[1L]])
    review_hit <- which(current$source_record_id == source$source_record_id[[1L]] &
                          current$provider_run_id == source$provider_run_id[[1L]])
    effect <- if (length(review_hit) == 1L) current$effect[[review_hit]] else ""
    target <- if (length(hit) == 1L) selected[hit, , drop = FALSE] else NULL
    status <- if (source_mode == "auto") {
      "provider_auto_selected"
    } else if (identical(effect, "rejected")) {
      "rejected_by_adjudication"
    } else if (identical(effect, "adjusted")) {
      "bounds_adjusted_by_adjudication"
    } else if (identical(effect, "accepted")) {
      "accepted_as_is_by_adjudication"
    } else if (is.null(target)) {
      "not_selected_without_acceptance"
    } else {
      "retained_unreviewed_provider_auto"
    }
    selected_id <- if (is.null(target)) NA_character_ else
      target$selected_interval_id[[1L]]
    selected_start <- if (is.null(target)) NA_integer_ else
      target$canonical_start_isi[[1L]]
    selected_end <- if (is.null(target)) NA_integer_ else
      target$canonical_end_isi[[1L]]
    review_id <- if (length(review_hit) == 1L) {
      current$current_decision_id[[review_hit]]
    } else NA_character_
    rows[[i]] <- data.frame(
      provider_run_id = source$provider_run_id[[1L]],
      source_record_id = source$source_record_id[[1L]],
      selected_interval_id = selected_id, train_key = source$train_key[[1L]],
      semantic_track = source$semantic_track[[1L]],
      label = source$proposed_label[[1L]], delta_status = status,
      provider_start_isi = source$canonical_start_isi[[1L]],
      provider_end_isi = source$canonical_end_isi[[1L]],
      selected_start_isi = selected_start, selected_end_isi = selected_end,
      start_shift_isi = if (is.na(selected_start)) NA_integer_ else
        selected_start - source$canonical_start_isi[[1L]],
      end_shift_isi = if (is.na(selected_end)) NA_integer_ else
        selected_end - source$canonical_end_isi[[1L]],
      review_decision_id = review_id, stringsAsFactors = FALSE
    )
  }
  out <- dplyr::bind_rows(
    stpd_provider_workbench_empty()$auto_adjudicated_delta, rows
  )
  out <- out[order(out$train_key, out$semantic_track, out$provider_start_isi,
                   out$source_record_id, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_provider_workbench_build <- function(provider_bundle, composition,
                                          adjudication = NULL) {
  run_id <- composition$metadata$provider_run_id[[1L]]
  source_mode <- composition$metadata$source_mode[[1L]]
  catalog <- stpd_provider_workbench_catalog(
    provider_bundle, run_id, adjudication
  )
  records <- stpd_provider_workbench_records(provider_bundle, run_id)
  selected <- stpd_provider_workbench_selected(composition)
  delta <- stpd_provider_workbench_delta(
    records, selected, source_mode, adjudication
  )
  changed_n <- sum(!delta$delta_status %in% c(
    "provider_auto_selected", "retained_unreviewed_provider_auto",
    "accepted_as_is_by_adjudication"
  ))
  estimand_notice <- if (source_mode == "auto" &&
                           composition$metadata$information_access[[1L]] ==
                           "label_blind") {
    "detector_performance_requires_independent_blinded_reference"
  } else {
    "adjudicated_agreement_only_not_detector_performance"
  }
  metadata <- data.frame(
    schema_version = STPD_PROVIDER_WORKBENCH_VIEW_VERSION,
    provider_run_id = run_id, source_mode = source_mode,
    source_bundle_sha256 = composition$metadata$source_bundle_sha256[[1L]],
    source_product_sha256 = composition$metadata$source_product_sha256[[1L]],
    composition_sha256 = composition$metadata$product_sha256[[1L]],
    information_access = composition$metadata$information_access[[1L]],
    authority_scope = composition$metadata$authority_scope[[1L]],
    performance_use = composition$metadata$performance_use[[1L]],
    estimand_notice = estimand_notice,
    provider_runs_n = as.integer(nrow(catalog)),
    provider_records_n = as.integer(nrow(records)),
    selected_intervals_n = as.integer(nrow(selected)),
    changed_records_n = as.integer(changed_n),
    relationships_n = as.integer(nrow(composition$relationships)),
    regimes_n = as.integer(nrow(composition$regimes)),
    conflicts_n = as.integer(nrow(composition$conflicts)),
    product_sha256 = NA_character_, stringsAsFactors = FALSE
  )
  payload <- list(
    provider_catalog = catalog, provider_records = records,
    selected_intervals = selected, auto_adjudicated_delta = delta,
    relationships = composition$relationships,
    regimes = composition$regimes, memberships = composition$memberships,
    conflicts = composition$conflicts
  )
  metadata$product_sha256 <- stpd_provider_hash_domain(
    "stpd-provider-workbench-view-product-v1",
    list(metadata = metadata[setdiff(names(metadata), "product_sha256")],
         payload = payload)
  )
  tables <- c(list(metadata = metadata), payload)
  structure(
    c(tables, list(manifest = stpd_provider_workbench_manifest(tables))),
    class = c("stpd_provider_workbench_view_v1", "list")
  )
}

#' Build a provider-review workbench view
#'
#' @param provider_bundle A validated provider bundle v1 containing every run.
#' @param composition One Phase D composition selecting exactly one run/source.
#' @param adjudication The exact Phase C parent for an adjudicated composition.
#' @return A deterministic, non-authoritative workbench view model.
#' @export
stpd_provider_review_view <- function(provider_bundle, composition,
                                      adjudication = NULL) {
  bundle_before <- serialize(provider_bundle, NULL, version = 3L)
  composition_before <- serialize(composition, NULL, version = 3L)
  adjudication_before <- if (is.null(adjudication)) NULL else
    serialize(adjudication, NULL, version = 3L)
  stpd_validate_provider_bundle(provider_bundle)
  stpd_validate_provider_composition(
    provider_bundle, composition, adjudication
  )
  out <- stpd_provider_workbench_build(
    provider_bundle, composition, adjudication
  )
  stpd_validate_provider_review_view(
    provider_bundle, composition, out, adjudication, rematerialize = FALSE
  )
  unchanged <- identical(bundle_before,
                         serialize(provider_bundle, NULL, version = 3L)) &&
    identical(composition_before, serialize(composition, NULL, version = 3L)) &&
    (is.null(adjudication) || identical(
      adjudication_before, serialize(adjudication, NULL, version = 3L)
    ))
  if (!unchanged) stpd_provider_workbench_abort(
    "provider_workbench_parent_mutated",
    "Building the workbench view mutated an immutable parent."
  )
  out
}

#' Validate a provider-review workbench view
#'
#' @param provider_bundle The exact provider bundle parent.
#' @param composition The exact Phase D composition parent.
#' @param view A workbench view v1 product.
#' @param adjudication The exact Phase C parent when applicable.
#' @param rematerialize Rebuild and compare every table when `TRUE`.
#' @return Invisibly `TRUE`; otherwise a typed fail-closed error.
#' @export
stpd_validate_provider_review_view <- function(
    provider_bundle, composition, view, adjudication = NULL,
    rematerialize = TRUE) {
  stpd_validate_provider_bundle(provider_bundle)
  stpd_validate_provider_composition(
    provider_bundle, composition, adjudication
  )
  prototype <- stpd_provider_workbench_empty()
  if (!inherits(view, "stpd_provider_workbench_view_v1") ||
      !identical(names(view), names(prototype))) {
    stpd_provider_workbench_abort(
      "provider_workbench_view_invalid", "Workbench tables are incomplete."
    )
  }
  for (name in names(prototype)) {
    x <- view[[name]]
    p <- prototype[[name]]
    if (!is.data.frame(x) || !identical(names(x), names(p)) ||
        any(vapply(names(p), function(column) {
          !identical(typeof(x[[column]]), typeof(p[[column]]))
        }, logical(1)))) {
      stpd_provider_workbench_abort(
        "provider_workbench_view_invalid",
        paste0("Invalid workbench table: ", name, ".")
      )
    }
  }
  metadata <- view$metadata
  if (nrow(metadata) != 1L ||
      !identical(metadata$schema_version[[1L]],
                 STPD_PROVIDER_WORKBENCH_VIEW_VERSION) ||
      !identical(metadata$provider_run_id[[1L]],
                 composition$metadata$provider_run_id[[1L]]) ||
      !identical(metadata$source_mode[[1L]],
                 composition$metadata$source_mode[[1L]]) ||
      !identical(metadata$composition_sha256[[1L]],
                 composition$metadata$product_sha256[[1L]])) {
    stpd_provider_workbench_abort(
      "provider_workbench_identity_invalid", "Workbench identity is stale."
    )
  }
  if (sum(view$provider_catalog$selected) != 1L ||
      !identical(view$provider_catalog$provider_run_id[
        view$provider_catalog$selected
      ], metadata$provider_run_id[[1L]]) ||
      any(view$provider_records$provider_run_id !=
            metadata$provider_run_id[[1L]]) ||
      any(view$selected_intervals$provider_run_id !=
            metadata$provider_run_id[[1L]]) ||
      any(view$auto_adjudicated_delta$provider_run_id !=
            metadata$provider_run_id[[1L]])) {
    stpd_provider_workbench_abort(
      "provider_workbench_run_pooling_forbidden",
      "The selected view pooled or lost its explicit provider run."
    )
  }
  expected_counts <- c(
    provider_runs_n = nrow(view$provider_catalog),
    provider_records_n = nrow(view$provider_records),
    selected_intervals_n = nrow(view$selected_intervals),
    relationships_n = nrow(view$relationships),
    regimes_n = nrow(view$regimes), conflicts_n = nrow(view$conflicts)
  )
  if (!identical(
    as.integer(unlist(metadata[names(expected_counts)], use.names = FALSE)),
    as.integer(expected_counts)
  )) {
    stpd_provider_workbench_abort(
      "provider_workbench_view_invalid", "Workbench counts are stale."
    )
  }
  tables <- view[setdiff(names(view), "manifest")]
  payload <- tables[setdiff(names(tables), "metadata")]
  expected_hash <- stpd_provider_hash_domain(
    "stpd-provider-workbench-view-product-v1",
    list(metadata = metadata[setdiff(names(metadata), "product_sha256")],
         payload = payload)
  )
  if (!identical(metadata$product_sha256[[1L]], expected_hash) ||
      !identical(view$manifest, stpd_provider_workbench_manifest(tables))) {
    stpd_provider_workbench_abort(
      "provider_workbench_manifest_invalid",
      "Workbench product or manifest hash is stale."
    )
  }
  if (isTRUE(rematerialize)) {
    expected <- stpd_provider_workbench_build(
      provider_bundle, composition, adjudication
    )
    if (!identical(view, expected)) stpd_provider_workbench_abort(
      "provider_workbench_view_invalid",
      "Deterministic workbench rematerialization failed."
    )
  }
  invisible(TRUE)
}
