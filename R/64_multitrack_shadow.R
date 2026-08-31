# Observation-only semantic-track selection for the Phase 1A migration.
#
# This module deliberately does not project labels back to per-ISI AUTO fields.
# It takes the candidate pool before the legacy HFS protection pass, assigns each
# candidate to one semantic track, and reuses the existing weighted interval
# selector independently within each selectable track. Cross-track compatibility,
# state splitting, and HFS dominance belong to later phases.

stpd_multitrack_policy_parameter_path <- function() {
  "spiketrainpattern.multitrack_shadow"
}

stpd_multitrack_policy_block <- function(params = NULL) {
  defaults <- (stpd_product_schema_defaults() %||% list())$multitrack_shadow
  if (is.null(defaults) || !is.list(defaults) || length(defaults) == 0L) {
    stop("The public multi-track shadow policy defaults are unavailable.", call. = FALSE)
  }
  if (is.null(params)) return(defaults)
  configured <- stpd_path_get(
    params, stpd_multitrack_policy_parameter_path(), default = list()
  )
  stpd_fill_defaults(configured, defaults)
}

stpd_multitrack_policy_hash <- function(params = NULL) {
  payload <- stpd_multitrack_policy_block(params)
  normalized <- if (exists("stpd_params_hash_normalize", mode = "function")) {
    stpd_params_hash_normalize(payload)
  } else {
    payload
  }
  digest::digest(normalized, algo = "sha256", serialize = TRUE)
}

stpd_multitrack_source_params_hash <- function(params = NULL) {
  source <- params %||% default_params_sec()
  tryCatch(stpd_params_hash(source), error = function(e) "")
}

stpd_multitrack_shadow_track_ontology <- function() {
  list(
    event = c("burst", "long_burst", "high_frequency_burst", "hf_burst"),
    state = c(
      "high_frequency_spiking", "high_frequency_tonic",
      "high_frequency_irregular_state", "tonic"
    ),
    gap = "pause",
    review = "possible_burst",
    profile = "profile",
    default = "diagnostic"
  )
}

stpd_multitrack_shadow_track_ontology_text <- function() {
  paste0(
    "event=burst,long_burst,high_frequency_burst,hf_burst|",
    paste0(
      "state=high_frequency_spiking,high_frequency_tonic,",
      "high_frequency_irregular_state,tonic|"
    ),
    "gap=pause|review=possible_burst|profile=profile|default=diagnostic"
  )
}

stpd_multitrack_fixed_policy_value <- function(block, field, expected) {
  actual <- as.character(block[[field]] %||% "")[1]
  if (!identical(actual, expected)) {
    stop(
      paste0(
        "Unsupported fixed multi-track policy value for ", field, ": '",
        actual, "'. Expected '", expected,
        "'. Update the implementation and policy version together."
      ),
      call. = FALSE
    )
  }
  expected
}

stpd_multitrack_fixed_policy_flag <- function(block, field, expected) {
  actual <- block[[field]]
  if (length(actual) != 1L || is.na(actual) || !is.logical(actual) ||
      !identical(actual, expected)) {
    stop(
      paste0(
        "Unsupported fixed multi-track policy value for ", field, ": '",
        paste(as.character(actual), collapse = ","), "'. Expected '",
        as.character(expected),
        "'. Update the implementation and policy version together."
      ),
      call. = FALSE
    )
  }
  expected
}

stpd_multitrack_shadow_policy <- function(params = NULL) {
  block <- stpd_multitrack_policy_block(params)
  expected <- stpd_multitrack_shadow_track_ontology_text()
  policy_version <- stpd_multitrack_fixed_policy_value(
    block, "phase1a_policy_version", "phase1a_observation_only_v3"
  )
  ontology_version <- stpd_multitrack_fixed_policy_value(
    block, "phase1a_track_ontology_version", "stpd_multitrack_semantic_tracks_v2"
  )
  ontology <- stpd_multitrack_fixed_policy_value(
    block, "phase1a_track_ontology", expected
  )
  review_target_track <- stpd_multitrack_fixed_policy_value(
    block, "possible_burst_review_target_track", "event"
  )
  review_target_label <- stpd_multitrack_fixed_policy_value(
    block, "possible_burst_review_target_label", "burst"
  )
  review_promotion_required <- stpd_multitrack_fixed_policy_flag(
    block, "possible_burst_review_promotion_required", TRUE
  )
  review_policy_status <- stpd_multitrack_fixed_policy_value(
    block,
    "possible_burst_review_policy_status",
    paste0(
      "review_only__not_accepted_event__manual_confirmation_required__",
      "primary_event_metrics_ineligible__event_state_coexistence_non_destructive__",
      "hfs_dominance_ineligible"
    )
  )
  list(
    policy_version = policy_version,
    track_ontology_version = ontology_version,
    track_ontology = ontology,
    track_ontology_sha256 = digest::digest(expected, algo = "sha256", serialize = FALSE),
    review_target_track = review_target_track,
    review_target_label = review_target_label,
    review_promotion_required = review_promotion_required,
    review_policy_status = review_policy_status
  )
}

stpd_multitrack_shadow_select_state_pool <- function(
    selection_pool, patterns = NULL) {
  out <- selection_pool
  out$selected_for_auto <- FALSE
  out$selection_status <- "not_selected"
  if (nrow(out) == 0L) return(out)

  labels <- stpd_multitrack_shadow_normalize_label(out$final_label)
  broad_parent <- labels == "high_frequency_spiking"
  subtype_evidence <- labels %in% c(
    "high_frequency_tonic", "high_frequency_irregular_state"
  )
  tonic <- labels == "tonic"

  # Broad HFS is the only high-frequency State support generator. HFT and
  # HF-irregular are classifications on the already accepted support and may
  # never add, expand, or outscore their parent geometry.
  parent_requested <- is.null(patterns) || any(c(
    "high_frequency_spiking", "high_frequency_tonic",
    "high_frequency_irregular_state"
  ) %in% stpd_multitrack_shadow_normalize_label(patterns))
  if (parent_requested && any(broad_parent)) {
    selected_parent <- stpd_event_core_weighted_select(
      out[broad_parent, , drop = FALSE], locked = NULL,
      patterns = "high_frequency_spiking"
    )
    out$selected_for_auto[broad_parent] <- selected_parent$selected_for_auto
    out$selection_status[broad_parent] <- selected_parent$selection_status
  }

  max_end <- suppressWarnings(max(as.integer(out$end_isi), na.rm = TRUE))
  if (!is.finite(max_end) || max_end < 1L) max_end <- 0L
  locked_by_parent <- rep(FALSE, as.integer(max_end))
  selected_parent_rows <- which(broad_parent & out$selected_for_auto)
  for (i in selected_parent_rows) {
    start <- suppressWarnings(as.integer(out$start_isi[i]))
    end <- suppressWarnings(as.integer(out$end_isi[i]))
    if (is.finite(start) && is.finite(end) && start >= 1L && end >= start) {
      locked_by_parent[start:end] <- TRUE
    }
  }

  if (any(subtype_evidence)) {
    overlaps_parent <- vapply(which(subtype_evidence), function(i) {
      start <- suppressWarnings(as.integer(out$start_isi[i]))
      end <- suppressWarnings(as.integer(out$end_isi[i]))
      is.finite(start) && is.finite(end) && start >= 1L && end >= start &&
        length(locked_by_parent) >= end && any(locked_by_parent[start:end])
    }, logical(1))
    out$selection_status[subtype_evidence] <- ifelse(
      overlaps_parent,
      "subtype_evidence_for_selected_broad_hf_parent",
      "subtype_evidence_without_selected_broad_hf_parent"
    )
  }

  if (any(tonic)) {
    selected_tonic <- stpd_event_core_weighted_select(
      out[tonic, , drop = FALSE],
      locked = if (length(locked_by_parent) > 0L) locked_by_parent else NULL,
      patterns = "tonic"
    )
    status <- as.character(selected_tonic$selection_status)
    status[status == "blocked_by_manual_label"] <-
      "blocked_by_selected_broad_hf_parent"
    out$selected_for_auto[tonic] <- selected_tonic$selected_for_auto
    out$selection_status[tonic] <- status
  }

  other <- !(broad_parent | subtype_evidence | tonic)
  if (any(other)) {
    selected_other <- stpd_event_core_weighted_select(
      out[other, , drop = FALSE], locked = NULL, patterns = patterns
    )
    out$selected_for_auto[other] <- selected_other$selected_for_auto
    out$selection_status[other] <- selected_other$selection_status
  }
  out
}

stpd_multitrack_shadow_normalize_label <- function(label) {
  lab <- tolower(trimws(as.character(label)))
  lab[is.na(lab)] <- ""
  gsub("[ -]+", "_", lab)
}

stpd_multitrack_shadow_semantic_track <- function(label, policy = NULL) {
  policy <- policy %||% stpd_multitrack_shadow_policy()
  ontology <- stpd_multitrack_shadow_track_ontology()
  lab <- stpd_multitrack_shadow_normalize_label(label)

  track <- rep(ontology$default, length(lab))
  track[lab %in% ontology$event] <- "event"
  track[lab %in% ontology$state] <- "state"
  track[lab %in% ontology$gap] <- "gap"
  track[lab %in% ontology$review] <- "review"
  track[lab %in% ontology$profile] <- "profile"
  unname(track)
}

stpd_multitrack_shadow_column <- function(x, name, default) {
  if (name %in% names(x)) x[[name]] else rep(default, nrow(x))
}

stpd_multitrack_shadow_canonical_order <- function(candidates, include_track = FALSE) {
  if (is.null(candidates) || nrow(candidates) == 0L) return(integer())

  starts <- suppressWarnings(as.integer(stpd_multitrack_shadow_column(candidates, "start_isi", NA_integer_)))
  ends <- suppressWarnings(as.integer(stpd_multitrack_shadow_column(candidates, "end_isi", NA_integer_)))
  labels <- as.character(stpd_multitrack_shadow_column(candidates, "final_label", ""))
  ids <- as.character(stpd_multitrack_shadow_column(candidates, "candidate_id", ""))
  layers <- as.character(stpd_multitrack_shadow_column(candidates, "candidate_layer", ""))
  scores <- suppressWarnings(as.numeric(stpd_multitrack_shadow_column(candidates, "score", NA_real_)))
  priorities <- suppressWarnings(as.numeric(stpd_multitrack_shadow_column(candidates, "priority", NA_real_)))
  n_isi <- suppressWarnings(as.integer(stpd_multitrack_shadow_column(candidates, "n_isi", NA_integer_)))

  if (isTRUE(include_track)) {
    tracks <- as.character(stpd_multitrack_shadow_column(candidates, "semantic_track", "diagnostic"))
    track_rank <- match(tracks, c("event", "state", "gap", "review", "diagnostic", "profile"))
    track_rank[is.na(track_rank)] <- 99L
  } else {
    track_rank <- rep(1L, nrow(candidates))
  }

  # candidate_id is the final deterministic tie-break for candidates with the
  # same interval and value. Detector-generated candidate IDs are stable within
  # a train, so input row permutation cannot change the chosen winner.
  order(
    track_rank, ends, starts, labels, ids, layers, scores, priorities, n_isi,
    na.last = TRUE, method = "radix"
  )
}

stpd_multitrack_shadow_select <- function(candidate_pool, patterns = NULL, params = NULL) {
  policy <- stpd_multitrack_shadow_policy(params)
  if (is.null(candidate_pool)) candidate_pool <- data.frame()
  candidates <- as.data.frame(candidate_pool, stringsAsFactors = FALSE)
  # A zero-candidate train still exposes a stable typed contract.  Normal
  # detector pools already contain these fields, so the additions are confined
  # to missing/empty schemas and do not alter real candidate values.
  required_candidate_columns <- list(
    candidate_id = character(),
    candidate_layer = character(),
    candidate_source = character(),
    final_label = character(),
    start_isi = integer(),
    end_isi = integer(),
    n_isi = integer(),
    score = double(),
    priority = double()
  )
  if (nrow(candidates) == 0L) {
    for (column in names(required_candidate_columns)) {
      if (!(column %in% names(candidates))) {
        prototype <- required_candidate_columns[[column]]
        candidates[[column]] <- rep(prototype, length.out = 0L)
      }
    }
  }
  candidates$source_candidate_index <- seq_len(nrow(candidates))

  labels <- as.character(stpd_multitrack_shadow_column(candidates, "final_label", ""))
  candidates$semantic_track <- stpd_multitrack_shadow_semantic_track(labels, policy = policy)
  possible_burst <- stpd_multitrack_shadow_normalize_label(labels) == "possible_burst"
  candidates$review_target_track <- rep("", nrow(candidates))
  candidates$review_target_label <- rep("", nrow(candidates))
  candidates$review_promotion_required <- rep(FALSE, nrow(candidates))
  candidates$review_policy_status <- rep("", nrow(candidates))
  if (any(possible_burst)) {
    candidates$review_target_track[possible_burst] <- policy$review_target_track
    candidates$review_target_label[possible_burst] <- policy$review_target_label
    candidates$review_promotion_required[possible_burst] <-
      policy$review_promotion_required
    candidates$review_policy_status[possible_burst] <- policy$review_policy_status
  }
  candidates$selected_within_track <- rep(FALSE, nrow(candidates))
  candidates$track_selection_status <- as.character(ifelse(
    candidates$semantic_track == "profile",
    "profile_not_selectable",
    ifelse(
      candidates$semantic_track == "diagnostic",
      "diagnostic_not_selectable",
      "not_selected_within_track"
    )
  ))

  selectable_tracks <- c("event", "state", "gap", "review")
  for (track in selectable_tracks) {
    track_rows <- which(candidates$semantic_track == track)
    if (length(track_rows) == 0L) next

    pool <- candidates[track_rows, , drop = FALSE]
    pool_order <- stpd_multitrack_shadow_canonical_order(pool)
    pool <- pool[pool_order, , drop = FALSE]
    source_rows <- track_rows[pool_order]

    # Keep the original label in the audit ledger, but use its canonical form
    # for selection and pattern filtering. This makes track assignment and
    # within-track arbitration agree for equivalent space/hyphen spellings.
    selection_pool <- pool
    selection_pool$final_label <- stpd_multitrack_shadow_normalize_label(
      selection_pool$final_label
    )
    selected_pool <- if (identical(track, "state")) {
      stpd_multitrack_shadow_select_state_pool(
        selection_pool, patterns = patterns
      )
    } else {
      stpd_event_core_weighted_select(
        selection_pool,
        locked = NULL,
        patterns = patterns
      )
    }
    selected <- as.logical(selected_pool$selected_for_auto)
    selected[is.na(selected)] <- FALSE
    status <- as.character(selected_pool$selection_status)
    status[is.na(status) | !nzchar(status)] <- "not_selected"

    candidates$selected_within_track[source_rows] <- selected
    candidates$track_selection_status[source_rows] <- ifelse(
      selected,
      paste0("selected_within_", track, "_weighted_interval_grammar"),
      paste0(status, "__within_", track, "_track")
    )
  }

  canonical_order <- stpd_multitrack_shadow_canonical_order(candidates, include_track = TRUE)
  if (length(canonical_order) > 0L) {
    candidates <- candidates[canonical_order, , drop = FALSE]
    rownames(candidates) <- NULL
  }
  # Keep the full pre-protection evidence exactly once in `candidates`.  A full
  # selected-row copy is expensive because diagnostic candidate tables are wide;
  # the persisted convenience view is intentionally narrow and can always be
  # joined back through `source_candidate_index` or `candidate_id`.
  selected_view_columns <- intersect(
    c(
      "source_candidate_index", "candidate_id", "candidate_layer",
      "candidate_source", "final_label", "start_isi", "end_isi", "n_isi",
      "score", "priority", "semantic_track", "selected_within_track",
      "track_selection_status", "review_target_track", "review_target_label",
      "review_promotion_required", "review_policy_status"
    ),
    names(candidates)
  )
  selected_candidates <- candidates[
    candidates$selected_within_track,
    selected_view_columns,
    drop = FALSE
  ]
  rownames(selected_candidates) <- NULL

  track_levels <- c("event", "state", "gap", "review", "diagnostic", "profile")
  track_summary <- data.frame(
    semantic_track = track_levels,
    n_candidates = vapply(track_levels, function(track) {
      sum(candidates$semantic_track == track, na.rm = TRUE)
    }, integer(1)),
    n_selected_within_track = vapply(track_levels, function(track) {
      sum(candidates$semantic_track == track & candidates$selected_within_track, na.rm = TRUE)
    }, integer(1)),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      policy_version = policy$policy_version,
      track_ontology_version = policy$track_ontology_version,
      track_ontology = policy$track_ontology,
      track_ontology_sha256 = policy$track_ontology_sha256,
      multitrack_policy_parameter_path = stpd_multitrack_policy_parameter_path(),
      multitrack_policy_hash = stpd_multitrack_policy_hash(params),
      multitrack_policy_hash_algorithm = "SHA-256",
      source_params_hash = stpd_multitrack_source_params_hash(params),
      authoritative = FALSE,
      candidate_source = "pre_hf_spiking_protection",
      cross_track_resolution = "none",
      legacy_projection_changed = FALSE,
      candidates = candidates,
      selected_candidates = selected_candidates,
      track_summary = track_summary
    ),
    class = c("stpd_multitrack_shadow", "list")
  )
}
