# Threshold-first Gate 1B: internal Burst Stage-4 observations ------------
#
# These helpers observe the existing ordered threshold-centred search. They do
# not alter its loops, shared generation budget, geometry claims or candidates.
# Only actually visited loop members are attempts; cap-blocked future members
# remain absent and are represented by an incomplete-early-stop receipt.

stpd_candidate_lineage_stage4_support_hash <- function(
    root_type, root_ordinal, start_row, end_row, isi, valid) {
  rows <- if (is.finite(start_row) && is.finite(end_row) &&
      start_row >= 2L && end_row >= start_row && end_row <= length(isi)) {
    seq.int(as.integer(start_row), as.integer(end_row))
  } else {
    integer()
  }
  support <- data.frame(
    detector_row_index = as.integer(rows),
    isi_sec = as.numeric(isi[rows]), valid_isi = as.logical(valid[rows]),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  stpd_threshold_first_hash_domain(
    "stpd-burst-stage4-source-root-v1",
    list(
      root_type = as.character(root_type)[1L],
      root_ordinal = as.integer(root_ordinal)[1L], support = support
    )
  )
}

stpd_candidate_lineage_stage4_short_receipt <- function(
    train_collector, train, n_interval_rows, vp, params) {
  if (is.null(train_collector)) return(invisible(NULL))
  empty_roots <- stpd_candidate_lineage_empty_hook_payload(
    "burst_threshold_stage4_episode_roots_v1"
  )
  empty_entry <- stpd_candidate_lineage_empty_hook_payload(
    "burst_threshold_stage4_entry_v1"
  )
  empty_attempts <- stpd_candidate_lineage_empty_hook_payload(
    "burst_threshold_stage4_attempts_v1"
  )
  empty_candidates <- stpd_candidate_lineage_empty_hook_payload(
    "burst_threshold_stage4_candidates_v1"
  )
  max_candidates <- max(
    1L, stpd_event_core_int((vp %||% list())$max_candidates %||% 3000L, 3000L)
  )
  max_expand <- max(
    0L, stpd_event_core_int((vp %||% list())$max_expand %||% 0L, 0L)
  )
  native_bridge <- stpd_event_grammar_num(
    (vp %||% list())$bridge_high %||% 0, 0
  )
  proposed_bridge <- stpd_event_grammar_num(
    (vp %||% list())$cross_train_borrowing_bridge_candidate_sec %||%
      native_bridge,
    native_bridge
  )
  candidate_bridge <- max(native_bridge, proposed_bridge)
  borrowed_run_max <- max(1L, stpd_event_grammar_int(
    ((params$event_grammar %||% list())$
      burst_extension_max_consecutive_borrowed_isi) %||% 2L,
    2L
  ))
  receipt <- data.frame(
    train = as.character(train)[1L], burst_pipeline_id = "final",
    applicability_status = "not_applicable_short_train",
    n_interval_rows = as.integer(n_interval_rows)[1L],
    max_candidates = as.integer(max_candidates), max_expand = as.integer(max_expand),
    candidate_bridge_high_sec = as.numeric(candidate_bridge),
    episode_upper_sec = as.numeric(candidate_bridge),
    borrowed_run_max = as.integer(borrowed_run_max),
    single_connector_enabled = isTRUE(
      (params$event_grammar %||% list())$single_expanded_bridge_enabled %||%
        FALSE
    ),
    seed_root_n = 0L, seed_eligible_root_n = 0L, episode_root_n = 0L,
    observed_attempt_n = 0L, emitted_candidate_n = 0L,
    duplicate_n = 0L, filter_rejected_n = 0L,
    seed_attempt_n = 0L, dense_attempt_n = 0L, connector_attempt_n = 0L,
    seed_candidate_n = 0L, dense_candidate_n = 0L,
    connector_candidate_n = 0L,
    seed_status = "not_applicable_short_train",
    dense_status = "not_applicable_short_train",
    connector_status = "not_applicable_short_train",
    cap_check_triggered = FALSE, stop_subroute = "",
    stop_source_root_ordinal = NA_integer_, stop_rows_n = 0L,
    stage4_search_exhausted = FALSE,
    coverage_status = "stage4_not_applicable_short_train",
    proposal_intrusion_mask_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-proposal-intrusion-mask-v1", logical()
    ),
    episode_support_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-episode-support-v1", data.frame()
    ),
    attempt_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-attempts-v1", empty_attempts
    ),
    candidate_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-candidates-v1", empty_candidates
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_threshold_stage4_entry_v1", empty_entry
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_threshold_stage4_episode_roots_v1", empty_roots
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_threshold_stage4_attempts_v1", empty_attempts
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_threshold_stage4_candidates_v1", empty_candidates
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_threshold_stage4_receipt_v1", receipt
  )
  invisible(receipt)
}

stpd_candidate_lineage_stage4_entry_from_context <- function(context) {
  vp <- context$vp
  params <- context$params
  eg <- params$event_grammar %||% list()
  native_bridge <- stpd_event_grammar_num(vp$bridge_high, 0)
  proposed_bridge <- stpd_event_grammar_num(
    vp$cross_train_borrowing_bridge_candidate_sec %||% native_bridge,
    native_bridge
  )
  candidate_bridge <- max(native_bridge, proposed_bridge)
  borrowed_run_max <- max(1L, stpd_event_grammar_int(
    eg$burst_extension_max_consecutive_borrowed_isi %||% 2L, 2L
  ))
  episode_upper <- max(
    candidate_bridge,
    native_bridge * stpd_event_grammar_num(
      eg$structural_burst_episode_bridge_factor %||% 1.75, 1.75
    ),
    na.rm = TRUE
  )
  if (is.finite(vp$pause_thr) && vp$pause_thr > 0) {
    episode_upper <- min(episode_upper, vp$pause_thr * (1 - 1e-6))
  }
  if (!is.finite(episode_upper) || episode_upper <= native_bridge) {
    episode_upper <- native_bridge
  }
  data.frame(
    train = as.character(context$train)[1L],
    burst_pipeline_id = "final", applicability_status = "applied",
    n_interval_rows = as.integer(nrow(context$dat)),
    min_isi_sec = as.numeric(context$min_isi_sec)[1L],
    max_candidates = as.integer(max(
      1L, stpd_event_core_int(vp$max_candidates %||% 3000L, 3000L)
    )),
    max_expand = as.integer(max(
      0L, stpd_event_core_int(vp$max_expand %||% 0L, 0L)
    )),
    min_spikes = as.integer(vp$min_spikes),
    min_seed_isi_count = as.integer(vp$min_seed_isi_n),
    native_bridge_high_sec = as.numeric(native_bridge),
    candidate_bridge_high_sec = as.numeric(candidate_bridge),
    episode_upper_sec = as.numeric(episode_upper),
    borrowed_run_max = as.integer(borrowed_run_max),
    episode_min_isi = as.integer(max(3L, stpd_event_grammar_int(
      eg$structural_burst_episode_min_isi %||% 3L, 3L
    ))),
    single_connector_enabled = isTRUE(
      eg$single_expanded_bridge_enabled %||% FALSE
    ),
    connector_side_seed_min = as.integer(max(
      1L, stpd_event_grammar_int(vp$min_seed_isi_n, 2L)
    )),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_stage4_observer_new <- function(
    train_collector, train, dat, vp, params, min_isi_sec, seed_runs,
    isi, valid, proposal_intrusion_mask, candidate_bridge_high,
    episode_upper, borrowed_run_max) {
  if (is.null(train_collector)) return(NULL)
  observer <- new.env(parent = emptyenv())
  class(observer) <- "stpd_candidate_lineage_stage4_observer"
  observer$collector <- train_collector
  observer$train <- as.character(train)[1L]
  observer$n_interval_rows <- as.integer(nrow(dat))
  observer$max_candidates <- as.integer(max(
    1L, stpd_event_core_int(vp$max_candidates %||% 3000L, 3000L)
  ))
  observer$max_expand <- as.integer(max(
    0L, stpd_event_core_int(vp$max_expand %||% 0L, 0L)
  ))
  observer$candidate_bridge_high <- as.numeric(candidate_bridge_high)[1L]
  observer$episode_upper <- as.numeric(episode_upper)[1L]
  observer$borrowed_run_max <- as.integer(borrowed_run_max)[1L]
  observer$single_connector_enabled <- isTRUE(
    (params$event_grammar %||% list())$single_expanded_bridge_enabled %||%
      FALSE
  )
  observer$episode_min_isi <- max(3L, stpd_event_grammar_int(
    (params$event_grammar %||% list())$structural_burst_episode_min_isi %||%
      3L, 3L
  ))
  observer$connector_side_seed_min <- max(1L, stpd_event_grammar_int(
    vp$min_seed_isi_n, 2L
  ))
  observer$isi <- as.numeric(isi)
  observer$valid <- as.logical(valid)
  observer$seed_runs <- seed_runs
  observer$seed_root_n <- as.integer(nrow(seed_runs))
  observer$seed_eligible_root_n <- as.integer(if (nrow(seed_runs)) {
    sum(seed_runs$min_seed_pass)
  } else 0L)
  observer$episode_roots <- stpd_candidate_lineage_empty_hook_payload(
    "burst_threshold_stage4_episode_roots_v1"
  )
  observer$episode_support_sha256 <- stpd_threshold_first_hash_domain(
    "stpd-burst-stage4-episode-support-v1", data.frame()
  )
  observer$proposal_intrusion_mask_sha256 <-
    stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-proposal-intrusion-mask-v1",
      as.logical(proposal_intrusion_mask)
    )
  observer$attempts <- list()
  observer$candidate_rows <- list()
  observer$route_attempt_n <- c(
    seed_expansion = 0L, dense_episode = 0L,
    single_expanded_bridge = 0L
  )
  observer$shared_claims <- new.env(hash = TRUE, parent = emptyenv())
  observer$bridge_claims <- new.env(hash = TRUE, parent = emptyenv())
  observer$cap_check_triggered <- FALSE
  observer$stop_subroute <- ""
  observer$stop_source_root_ordinal <- NA_integer_
  observer$stop_rows_n <- 0L
  observer$sealed <- FALSE
  train_collector$stage4_replay_context <- list(
    dat = dat, params = params, vp = vp,
    min_isi_sec = as.numeric(min_isi_sec)[1L], train = observer$train
  )
  entry <- stpd_candidate_lineage_stage4_entry_from_context(
    train_collector$stage4_replay_context
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_threshold_stage4_entry_v1", entry
  )
  observer
}

stpd_candidate_lineage_stage4_set_episode_roots <- function(
    observer, episode_runs, episode_intrusion_mask, episode_flag) {
  if (is.null(observer)) return(invisible(NULL))
  roots <- stpd_candidate_lineage_empty_hook_payload(
    "burst_threshold_stage4_episode_roots_v1"
  )
  if (nrow(episode_runs)) {
    roots <- data.frame(
      train = rep(observer$train, nrow(episode_runs)),
      root_ordinal = as.integer(seq_len(nrow(episode_runs))),
      detector_start_row = as.integer(episode_runs$start_isi),
      detector_end_row = as.integer(episode_runs$end_isi),
      start_isi = as.integer(episode_runs$start_isi - 1L),
      end_isi = as.integer(episode_runs$end_isi - 1L),
      support_sha256 = vapply(seq_len(nrow(episode_runs)), function(i) {
        stpd_candidate_lineage_stage4_support_hash(
          "episode_root", i, episode_runs$start_isi[[i]],
          episode_runs$end_isi[[i]], observer$isi, observer$valid
        )
      }, character(1)),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  support_rows <- if (length(observer$isi) >= 2L) {
    seq.int(2L, length(observer$isi))
  } else integer()
  episode_support <- data.frame(
    detector_row_index = as.integer(support_rows),
    intrusion_mask = as.logical(episode_intrusion_mask[support_rows]),
    episode_member = as.logical(episode_flag[support_rows]),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  observer$episode_roots <- roots
  observer$episode_support_sha256 <- stpd_threshold_first_hash_domain(
    "stpd-burst-stage4-episode-support-v1", episode_support
  )
  invisible(roots)
}

stpd_candidate_lineage_stage4_begin_attempt <- function(
    observer, subroute, source_root_ordinal, source_root_start_row,
    source_root_end_row, proposed_start_row = NA_integer_,
    proposed_end_row = NA_integer_, rows_before_attempt) {
  if (is.null(observer)) return(NA_integer_)
  subroute <- as.character(subroute)[1L]
  observer$route_attempt_n[[subroute]] <-
    observer$route_attempt_n[[subroute]] + 1L
  ordinal <- as.integer(length(observer$attempts) + 1L)
  root_type <- switch(
    subroute, seed_expansion = "seed_run",
    dense_episode = "dense_episode_run",
    single_expanded_bridge = "connector_episode_run"
  )
  root_hash <- if (identical(subroute, "seed_expansion")) {
    stpd_candidate_lineage_stage4_support_hash(
      root_type, source_root_ordinal, source_root_start_row,
      source_root_end_row, observer$isi, observer$valid
    )
  } else if (source_root_ordinal >= 1L &&
      source_root_ordinal <= nrow(observer$episode_roots)) {
    observer$episode_roots$support_sha256[[source_root_ordinal]]
  } else {
    stpd_candidate_lineage_stage4_support_hash(
      root_type, source_root_ordinal, source_root_start_row,
      source_root_end_row, observer$isi, observer$valid
    )
  }
  geometry_available <- is.finite(proposed_start_row) &&
    is.finite(proposed_end_row) && proposed_start_row >= 2L &&
    proposed_end_row >= proposed_start_row
  observer$attempts[[ordinal]] <- list(
    train = observer$train, attempt_ordinal = ordinal,
    subroute = subroute,
    route_attempt_ordinal = as.integer(observer$route_attempt_n[[subroute]]),
    source_root_type = root_type,
    source_root_ordinal = as.integer(source_root_ordinal),
    source_root_start_row = as.integer(source_root_start_row),
    source_root_end_row = as.integer(source_root_end_row),
    source_root_start_isi = as.integer(source_root_start_row - 1L),
    source_root_end_isi = as.integer(source_root_end_row - 1L),
    source_root_sha256 = root_hash,
    proposed_start_row = as.integer(proposed_start_row),
    proposed_end_row = as.integer(proposed_end_row),
    proposed_start_isi = if (geometry_available) {
      as.integer(proposed_start_row - 1L)
    } else NA_integer_,
    proposed_end_isi = if (geometry_available) {
      as.integer(proposed_end_row - 1L)
    } else NA_integer_,
    geometry_available = geometry_available,
    geometry_key = if (geometry_available) {
      paste0(as.integer(proposed_start_row), "_", as.integer(proposed_end_row))
    } else "",
    duplicate_domain = "", geometry_claimed = FALSE,
    duplicate_of_attempt_ordinal = NA_integer_,
    rows_before_attempt = as.integer(rows_before_attempt),
    max_candidates = observer$max_candidates,
    terminal_status = "", terminal_reason = "",
    candidate_append_ordinal = NA_integer_, candidate_id = "",
    candidate_source_sha256 = ""
  )
  ordinal
}

stpd_candidate_lineage_stage4_set_geometry <- function(
    observer, attempt_ordinal, start_row, end_row) {
  if (is.null(observer)) return(invisible(NULL))
  row <- observer$attempts[[attempt_ordinal]]
  available <- is.finite(start_row) && is.finite(end_row) &&
    start_row >= 2L && end_row >= start_row
  row$proposed_start_row <- as.integer(start_row)
  row$proposed_end_row <- as.integer(end_row)
  row$proposed_start_isi <- if (available) as.integer(start_row - 1L) else
    NA_integer_
  row$proposed_end_isi <- if (available) as.integer(end_row - 1L) else
    NA_integer_
  row$geometry_available <- available
  row$geometry_key <- if (available) {
    paste0(as.integer(start_row), "_", as.integer(end_row))
  } else ""
  observer$attempts[[attempt_ordinal]] <- row
  invisible(row)
}

stpd_candidate_lineage_stage4_claim_geometry <- function(
    observer, attempt_ordinal, duplicate_domain) {
  if (is.null(observer)) return(invisible(NULL))
  row <- observer$attempts[[attempt_ordinal]]
  if (!isTRUE(row$geometry_available)) {
    stpd_candidate_lineage_abort(
      "collector_stage4_geometry_unavailable",
      "A Stage-4 geometry cannot be claimed before it is observed."
    )
  }
  domain <- as.character(duplicate_domain)[1L]
  claims <- if (identical(domain, "seed_dense_shared_seen")) {
    observer$shared_claims
  } else if (identical(domain, "single_connector_bridge_seen")) {
    observer$bridge_claims
  } else {
    stpd_candidate_lineage_abort(
      "collector_stage4_duplicate_domain_invalid",
      "Stage-4 duplicate claims require a frozen duplicate domain."
    )
  }
  row$duplicate_domain <- domain
  row$geometry_claimed <- TRUE
  duplicate_of <- if (exists(row$geometry_key, envir = claims, inherits = FALSE)) {
    get(row$geometry_key, envir = claims, inherits = FALSE)
  } else {
    assign(row$geometry_key, attempt_ordinal, envir = claims)
    NA_integer_
  }
  row$duplicate_of_attempt_ordinal <- as.integer(duplicate_of)
  observer$attempts[[attempt_ordinal]] <- row
  invisible(list(
    duplicate = !is.na(duplicate_of),
    duplicate_of_attempt_ordinal = as.integer(duplicate_of)
  ))
}

stpd_candidate_lineage_stage4_finish_filter <- function(
    observer, attempt_ordinal, reason) {
  if (is.null(observer)) return(invisible(NULL))
  row <- observer$attempts[[attempt_ordinal]]
  row$terminal_status <- "filter_rejected"
  row$terminal_reason <- as.character(reason)[1L]
  observer$attempts[[attempt_ordinal]] <- row
  invisible(row)
}

stpd_candidate_lineage_stage4_finish_duplicate <- function(
    observer, attempt_ordinal) {
  if (is.null(observer)) return(invisible(NULL))
  row <- observer$attempts[[attempt_ordinal]]
  row$terminal_status <- "duplicate_skipped"
  row$terminal_reason <- "geometry_already_claimed_in_duplicate_domain"
  observer$attempts[[attempt_ordinal]] <- row
  invisible(row)
}

stpd_candidate_lineage_stage4_finish_candidate <- function(
    observer, attempt_ordinal, candidate_row) {
  if (is.null(observer)) return(invisible(NULL))
  candidate_row <- as.data.frame(
    candidate_row, stringsAsFactors = FALSE, check.names = FALSE
  )
  append_ordinal <- as.integer(length(observer$candidate_rows) + 1L)
  source_hash <- stpd_candidate_lineage_candidate_source_hash(
    candidate_row, "threshold_centred"
  )
  candidate_id <- stpd_candidate_lineage_candidate_column(
    candidate_row, "candidate_id", "character"
  )[[1L]]
  row <- observer$attempts[[attempt_ordinal]]
  row$terminal_status <- "candidate_emitted"
  row$terminal_reason <- "candidate_appended_to_stage4_rows"
  row$candidate_append_ordinal <- append_ordinal
  row$candidate_id <- candidate_id
  row$candidate_source_sha256 <- source_hash
  observer$attempts[[attempt_ordinal]] <- row
  observer$candidate_rows[[append_ordinal]] <- list(
    attempt_ordinal = as.integer(attempt_ordinal), subroute = row$subroute,
    candidate = candidate_row, source_hash = source_hash
  )
  invisible(row)
}

stpd_candidate_lineage_stage4_note_cap <- function(
    observer, subroute, source_root_ordinal, rows_n) {
  if (is.null(observer)) return(invisible(NULL))
  if (!isTRUE(observer$cap_check_triggered)) {
    observer$cap_check_triggered <- TRUE
    observer$stop_subroute <- as.character(subroute)[1L]
    observer$stop_source_root_ordinal <- as.integer(source_root_ordinal)[1L]
    observer$stop_rows_n <- as.integer(rows_n)[1L]
  }
  invisible(NULL)
}

stpd_candidate_lineage_stage4_attempt_payload <- function(observer) {
  if (!length(observer$attempts)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_threshold_stage4_attempts_v1"
    ))
  }
  rows <- lapply(observer$attempts, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  })
  out <- dplyr::bind_rows(rows)
  schema <- stpd_candidate_lineage_observation_hook_schema(
    "burst_threshold_stage4_attempts_v1"
  )
  out[, names(schema), drop = FALSE]
}

stpd_candidate_lineage_stage4_candidate_payload <- function(observer) {
  if (!length(observer$candidate_rows)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_threshold_stage4_candidates_v1"
    ))
  }
  rows <- lapply(seq_along(observer$candidate_rows), function(i) {
    item <- observer$candidate_rows[[i]]
    candidate <- item$candidate
    start_row <- stpd_candidate_lineage_candidate_column(
      candidate, "start_isi", "integer"
    )[[1L]]
    end_row <- stpd_candidate_lineage_candidate_column(
      candidate, "end_isi", "integer"
    )[[1L]]
    observed <- data.frame(
      train = observer$train, candidate_append_ordinal = as.integer(i),
      source_attempt_ordinal = item$attempt_ordinal,
      subroute = item$subroute,
      candidate_id = stpd_candidate_lineage_candidate_column(
        candidate, "candidate_id", "character"
      )[[1L]],
      source_payload_sha256 = item$source_hash,
      detector_start_row = start_row, detector_end_row = end_row,
      start_isi = as.integer(start_row - 1L),
      end_isi = as.integer(end_row - 1L),
      candidate_layer = stpd_candidate_lineage_candidate_column(
        candidate, "candidate_layer", "character"
      )[[1L]],
      candidate_class = stpd_candidate_lineage_candidate_column(
        candidate, "candidate_class", "character"
      )[[1L]],
      final_label = stpd_candidate_lineage_candidate_column(
        candidate, "final_label", "character"
      )[[1L]],
      gate_status = stpd_candidate_lineage_candidate_column(
        candidate, "gate_status", "character"
      )[[1L]],
      action = stpd_candidate_lineage_candidate_column(
        candidate, "action", "character"
      )[[1L]],
      priority = stpd_candidate_lineage_candidate_column(
        candidate, "priority", "double"
      )[[1L]],
      score = stpd_candidate_lineage_candidate_column(
        candidate, "score", "double"
      )[[1L]],
      stringsAsFactors = FALSE, check.names = FALSE
    )
    observed$candidate_observation_sha256 <-
      stpd_threshold_first_hash_domain(
        "stpd-burst-stage4-candidate-observation-v1", observed
      )
    schema <- stpd_candidate_lineage_observation_hook_schema(
      "burst_threshold_stage4_candidates_v1"
    )
    observed[, names(schema), drop = FALSE]
  })
  dplyr::bind_rows(rows)
}

stpd_candidate_lineage_stage4_scientific_subroute <- function(candidates) {
  classes <- stpd_candidate_lineage_candidate_column(
    candidates, "candidate_class", "character"
  )
  unname(c(
    event_grammar_seed_centered_burst = "seed_expansion",
    event_grammar_dense_short_isi_episode = "dense_episode",
    event_grammar_single_expanded_bridge_burst =
      "single_expanded_bridge"
  )[classes])
}

stpd_candidate_lineage_stage4_candidate_payload_from_scientific <- function(
    scientific_out, train, attempts) {
  if (is.null(scientific_out) || !nrow(scientific_out)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_threshold_stage4_candidates_v1"
    ))
  }
  emitted <- which(attempts$terminal_status == "candidate_emitted")
  if (length(emitted) != nrow(scientific_out)) {
    stpd_candidate_lineage_abort(
      "collector_stage4_scientific_candidate_count_mismatch",
      "Stage-4 emitted attempts do not close to the scientific output."
    )
  }
  routes <- stpd_candidate_lineage_stage4_scientific_subroute(scientific_out)
  if (anyNA(routes) || !identical(routes, attempts$subroute[emitted])) {
    stpd_candidate_lineage_abort(
      "collector_stage4_scientific_candidate_route_mismatch",
      "Stage-4 emitted attempt routes do not close to scientific candidates."
    )
  }
  rows <- lapply(seq_len(nrow(scientific_out)), function(i) {
    candidate <- scientific_out[i, , drop = FALSE]
    start_row <- stpd_candidate_lineage_candidate_column(
      candidate, "start_isi", "integer"
    )[[1L]]
    end_row <- stpd_candidate_lineage_candidate_column(
      candidate, "end_isi", "integer"
    )[[1L]]
    observed <- data.frame(
      train = as.character(train)[1L],
      candidate_append_ordinal = as.integer(i),
      source_attempt_ordinal = attempts$attempt_ordinal[emitted[[i]]],
      subroute = routes[[i]],
      candidate_id = stpd_candidate_lineage_candidate_column(
        candidate, "candidate_id", "character"
      )[[1L]],
      source_payload_sha256 =
        stpd_candidate_lineage_candidate_source_hash(
          candidate, "threshold_centred"
        ),
      detector_start_row = start_row, detector_end_row = end_row,
      start_isi = as.integer(start_row - 1L),
      end_isi = as.integer(end_row - 1L),
      candidate_layer = stpd_candidate_lineage_candidate_column(
        candidate, "candidate_layer", "character"
      )[[1L]],
      candidate_class = stpd_candidate_lineage_candidate_column(
        candidate, "candidate_class", "character"
      )[[1L]],
      final_label = stpd_candidate_lineage_candidate_column(
        candidate, "final_label", "character"
      )[[1L]],
      gate_status = stpd_candidate_lineage_candidate_column(
        candidate, "gate_status", "character"
      )[[1L]],
      action = stpd_candidate_lineage_candidate_column(
        candidate, "action", "character"
      )[[1L]],
      priority = stpd_candidate_lineage_candidate_column(
        candidate, "priority", "double"
      )[[1L]],
      score = stpd_candidate_lineage_candidate_column(
        candidate, "score", "double"
      )[[1L]],
      stringsAsFactors = FALSE, check.names = FALSE
    )
    observed$candidate_observation_sha256 <-
      stpd_threshold_first_hash_domain(
        "stpd-burst-stage4-candidate-observation-v1", observed
      )
    schema <- stpd_candidate_lineage_observation_hook_schema(
      "burst_threshold_stage4_candidates_v1"
    )
    observed[, names(schema), drop = FALSE]
  })
  dplyr::bind_rows(rows)
}

stpd_candidate_lineage_stage4_route_statuses <- function(observer) {
  episode_n <- nrow(observer$episode_roots)
  stop_route <- observer$stop_subroute
  capped <- isTRUE(observer$cap_check_triggered)
  route_order <- c(
    seed_expansion = 1L, dense_episode = 2L,
    single_expanded_bridge = 3L
  )
  status_for <- function(route, has_source, enabled = TRUE) {
    if (!enabled) return("not_applied_disabled")
    if (!has_source) return("not_applicable_no_source_roots")
    if (!capped) return("search_exhausted")
    if (route_order[[route]] < route_order[[stop_route]]) {
      return("search_exhausted_before_shared_cap")
    }
    if (identical(route, stop_route)) return("truncated_by_shared_cap")
    "not_entered_shared_cap"
  }
  c(
    seed_status = status_for(
      "seed_expansion", observer$seed_eligible_root_n > 0L
    ),
    dense_status = status_for("dense_episode", episode_n > 0L),
    connector_status = status_for(
      "single_expanded_bridge", episode_n > 0L,
      observer$single_connector_enabled
    )
  )
}

stpd_candidate_lineage_stage4_finalize <- function(observer, scientific_out) {
  if (is.null(observer)) return(invisible(NULL))
  if (isTRUE(observer$sealed)) {
    stpd_candidate_lineage_abort(
      "collector_stage4_observer_sealed",
      "A Stage-4 observer can be finalized only once."
    )
  }
  observer$collector$stage4_scientific_out <- as.data.frame(
    scientific_out, stringsAsFactors = FALSE, check.names = FALSE
  )
  attempts <- stpd_candidate_lineage_stage4_attempt_payload(observer)
  observed_candidates <-
    stpd_candidate_lineage_stage4_candidate_payload(observer)
  candidates <-
    stpd_candidate_lineage_stage4_candidate_payload_from_scientific(
      observer$collector$stage4_scientific_out, observer$train, attempts
    )
  if (!identical(observed_candidates, candidates)) {
    stpd_candidate_lineage_abort(
      "collector_stage4_observer_scientific_mismatch",
      paste(
        "Stage-4 observer candidates must exactly match the returned",
        "threshold-centred scientific rows."
      )
    )
  }
  statuses <- stpd_candidate_lineage_stage4_route_statuses(observer)
  subroute_count <- function(table, subroute) {
    as.integer(sum(table$subroute == subroute))
  }
  candidate_n <- nrow(candidates)
  receipt <- data.frame(
    train = observer$train, burst_pipeline_id = "final",
    applicability_status = "applied",
    n_interval_rows = observer$n_interval_rows,
    max_candidates = observer$max_candidates,
    max_expand = observer$max_expand,
    candidate_bridge_high_sec = observer$candidate_bridge_high,
    episode_upper_sec = observer$episode_upper,
    borrowed_run_max = observer$borrowed_run_max,
    single_connector_enabled = observer$single_connector_enabled,
    seed_root_n = observer$seed_root_n,
    seed_eligible_root_n = observer$seed_eligible_root_n,
    episode_root_n = as.integer(nrow(observer$episode_roots)),
    observed_attempt_n = as.integer(nrow(attempts)),
    emitted_candidate_n = as.integer(candidate_n),
    duplicate_n = as.integer(sum(
      attempts$terminal_status == "duplicate_skipped"
    )),
    filter_rejected_n = as.integer(sum(
      attempts$terminal_status == "filter_rejected"
    )),
    seed_attempt_n = subroute_count(attempts, "seed_expansion"),
    dense_attempt_n = subroute_count(attempts, "dense_episode"),
    connector_attempt_n = subroute_count(
      attempts, "single_expanded_bridge"
    ),
    seed_candidate_n = subroute_count(candidates, "seed_expansion"),
    dense_candidate_n = subroute_count(candidates, "dense_episode"),
    connector_candidate_n = subroute_count(
      candidates, "single_expanded_bridge"
    ),
    seed_status = unname(statuses[["seed_status"]]),
    dense_status = unname(statuses[["dense_status"]]),
    connector_status = unname(statuses[["connector_status"]]),
    cap_check_triggered = observer$cap_check_triggered,
    stop_subroute = observer$stop_subroute,
    stop_source_root_ordinal = observer$stop_source_root_ordinal,
    stop_rows_n = observer$stop_rows_n,
    stage4_search_exhausted = !observer$cap_check_triggered,
    coverage_status = if (observer$cap_check_triggered) {
      "stage4_incomplete_early_stop"
    } else {
      "stage4_search_complete"
    },
    proposal_intrusion_mask_sha256 =
      observer$proposal_intrusion_mask_sha256,
    episode_support_sha256 = observer$episode_support_sha256,
    attempt_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-attempts-v1", attempts
    ),
    candidate_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-candidates-v1", candidates
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    observer$collector, "burst_threshold_stage4_episode_roots_v1",
    observer$episode_roots
  )
  stpd_candidate_lineage_collector_capture(
    observer$collector, "burst_threshold_stage4_attempts_v1", attempts
  )
  stpd_candidate_lineage_collector_capture(
    observer$collector, "burst_threshold_stage4_candidates_v1", candidates
  )
  stpd_candidate_lineage_collector_capture(
    observer$collector, "burst_threshold_stage4_receipt_v1", receipt
  )
  observer$sealed <- TRUE
  invisible(list(
    episode_roots = observer$episode_roots,
    attempts = attempts, candidates = candidates, receipt = receipt
  ))
}

# Semantic replay helpers -------------------------------------------------

stpd_candidate_lineage_stage4_raw_vectors <- function(raw, n_interval_rows) {
  isi <- rep(NA_real_, n_interval_rows)
  valid <- rep(FALSE, n_interval_rows)
  if (!is.null(raw) && nrow(raw)) {
    isi[raw$detector_row_index] <- raw$isi_sec
    valid[raw$detector_row_index] <- raw$valid_isi
  }
  list(isi = isi, valid = valid)
}

stpd_candidate_lineage_stage4_episode_roots_expected <- function(
    raw, n_interval_rows, episode_upper, borrowed_run_max) {
  expected <- stpd_candidate_lineage_empty_hook_payload(
    "burst_threshold_stage4_episode_roots_v1"
  )
  if (is.null(raw) || !nrow(raw) || n_interval_rows <= 2L) return(expected)
  vectors <- stpd_candidate_lineage_stage4_raw_vectors(raw, n_interval_rows)
  native_bridge <- raw$native_bridge_high_sec[[1L]]
  intrusion <- stpd_event_grammar_burst_intrusion_mask(
    vectors$isi, vectors$valid, native_bridge, episode_upper,
    borrowed_run_max
  )
  flag <- vectors$valid & vectors$isi <= episode_upper & !intrusion
  runs <- stpd_event_grammar_bool_runs(flag)
  if (!nrow(runs)) return(expected)
  data.frame(
    train = rep(raw$train[[1L]], nrow(runs)),
    root_ordinal = as.integer(seq_len(nrow(runs))),
    detector_start_row = as.integer(runs$start_isi),
    detector_end_row = as.integer(runs$end_isi),
    start_isi = as.integer(runs$start_isi - 1L),
    end_isi = as.integer(runs$end_isi - 1L),
    support_sha256 = vapply(seq_len(nrow(runs)), function(i) {
      stpd_candidate_lineage_stage4_support_hash(
        "episode_root", i, runs$start_isi[[i]], runs$end_isi[[i]],
        vectors$isi, vectors$valid
      )
    }, character(1)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_stage4_episode_roots_are_valid <- function(
    payload, train, raw) {
  if (!nrow(payload)) return(TRUE)
  if (is.null(raw) || !nrow(raw)) return(FALSE)
  n <- raw$detector_row_index[[nrow(raw)]]
  vectors <- stpd_candidate_lineage_stage4_raw_vectors(raw, n)
  hashes <- vapply(seq_len(nrow(payload)), function(i) {
    stpd_candidate_lineage_stage4_support_hash(
      "episode_root", payload$root_ordinal[[i]],
      payload$detector_start_row[[i]], payload$detector_end_row[[i]],
      vectors$isi, vectors$valid
    )
  }, character(1))
  !anyNA(payload) && all(payload$train == train) &&
    identical(payload$root_ordinal, as.integer(seq_len(nrow(payload)))) &&
    all(payload$detector_start_row >= 2L) &&
    all(payload$detector_end_row >= payload$detector_start_row) &&
    all(payload$detector_end_row <= n) &&
    identical(payload$detector_start_row, payload$start_isi + 1L) &&
    identical(payload$detector_end_row, payload$end_isi + 1L) &&
    identical(payload$support_sha256, hashes)
}

stpd_candidate_lineage_stage4_replay_observer <- function(
    context, entry, seed_runs, episode_roots, raw) {
  vectors <- stpd_candidate_lineage_stage4_raw_vectors(
    raw, entry$n_interval_rows[[1L]]
  )
  observer <- new.env(parent = emptyenv())
  observer$train <- context$train
  observer$isi <- vectors$isi
  observer$valid <- vectors$valid
  observer$seed_runs <- seed_runs
  observer$episode_roots <- episode_roots
  observer$max_candidates <- entry$max_candidates[[1L]]
  observer$attempts <- list()
  observer$candidate_rows <- list()
  observer$route_attempt_n <- c(
    seed_expansion = 0L, dense_episode = 0L,
    single_expanded_bridge = 0L
  )
  observer$shared_claims <- new.env(hash = TRUE, parent = emptyenv())
  observer$bridge_claims <- new.env(hash = TRUE, parent = emptyenv())
  observer$cap_check_triggered <- FALSE
  observer$stop_subroute <- ""
  observer$stop_source_root_ordinal <- NA_integer_
  observer$stop_rows_n <- 0L
  observer
}

stpd_candidate_lineage_stage4_replay_emit <- function(
    observer, attempt_ordinal, scientific_out, route, start_row, end_row) {
  ordinal <- length(observer$candidate_rows) + 1L
  if (ordinal > nrow(scientific_out)) {
    stop("Stage-4 replay emitted beyond the scientific output.", call. = FALSE)
  }
  candidate <- scientific_out[ordinal, , drop = FALSE]
  observed_route <- stpd_candidate_lineage_stage4_scientific_subroute(
    candidate
  )
  observed_start <- stpd_candidate_lineage_candidate_column(
    candidate, "start_isi", "integer"
  )[[1L]]
  observed_end <- stpd_candidate_lineage_candidate_column(
    candidate, "end_isi", "integer"
  )[[1L]]
  if (!identical(observed_route[[1L]], route) ||
      !identical(observed_start, as.integer(start_row)) ||
      !identical(observed_end, as.integer(end_row))) {
    stop(
      "Stage-4 replay geometry/route diverged from scientific output.",
      call. = FALSE
    )
  }
  stpd_candidate_lineage_stage4_finish_candidate(
    observer, attempt_ordinal, candidate
  )
}

stpd_candidate_lineage_stage4_replay_expected <- function(
    context, entry, raw, seed_runs, episode_roots, scientific_out) {
  if (is.null(context) || !identical(
      entry, stpd_candidate_lineage_stage4_entry_from_context(context))) {
    return(NULL)
  }
  params <- context$params
  vp <- context$vp
  dat <- context$dat
  train <- context$train
  min_isi_sec <- context$min_isi_sec
  n <- entry$n_interval_rows[[1L]]
  observer <- stpd_candidate_lineage_stage4_replay_observer(
    context, entry, seed_runs, episode_roots, raw
  )
  isi <- observer$isi
  valid <- observer$valid
  seed_flag <- rep(FALSE, n)
  if (nrow(raw)) {
    seed_flag[raw$detector_row_index] <- raw$seed_band_member
  }
  native_bridge <- entry$native_bridge_high_sec[[1L]]
  candidate_bridge <- entry$candidate_bridge_high_sec[[1L]]
  episode_upper <- entry$episode_upper_sec[[1L]]
  borrowed_run_max <- entry$borrowed_run_max[[1L]]
  intrusion <- stpd_event_grammar_burst_intrusion_mask(
    isi, valid, native_bridge, candidate_bridge, borrowed_run_max
  )
  extension_valid <- valid & !intrusion
  candidate_vp <- vp
  candidate_vp$bridge_high <- candidate_bridge

  # A. Seed-centred expansion: seed and dense attempts share one claim domain.
  if (nrow(seed_runs)) for (rr in seq_len(nrow(seed_runs))) {
    if (!isTRUE(seed_runs$min_seed_pass[[rr]])) next
    ss <- seed_runs$detector_start_row[[rr]]
    ee <- seed_runs$detector_end_row[[rr]]
    lefts <- stpd_event_core_left_extensions(
      ss, isi, extension_valid, candidate_bridge,
      entry$max_expand[[1L]]
    )
    rights <- stpd_event_core_right_extensions(
      ee, n, isi, extension_valid, candidate_bridge,
      entry$max_expand[[1L]]
    )
    for (s in lefts) for (e in rights) {
      if (length(observer$candidate_rows) >= entry$max_candidates[[1L]]) {
        stpd_candidate_lineage_stage4_note_cap(
          observer, "seed_expansion", rr,
          length(observer$candidate_rows)
        )
        break
      }
      attempt <- stpd_candidate_lineage_stage4_begin_attempt(
        observer, "seed_expansion", rr, ss, ee, s, e,
        length(observer$candidate_rows)
      )
      claim <- stpd_candidate_lineage_stage4_claim_geometry(
        observer, attempt, "seed_dense_shared_seen"
      )
      if (isTRUE(claim$duplicate)) {
        stpd_candidate_lineage_stage4_finish_duplicate(observer, attempt)
        next
      }
      idx <- stpd_event_grammar_safe_seq(s, e)
      if (!length(idx)) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "empty_safe_sequence"
        )
        next
      }
      if (sum(seed_flag[idx], na.rm = TRUE) <
          entry$min_seed_isi_count[[1L]]) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "insufficient_seed_support"
        )
        next
      }
      if (e - s + 2L < entry$min_spikes[[1L]]) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "insufficient_spikes"
        )
        next
      }
      metrics <- stpd_event_core_span_metrics(
        dat, s, e, params, candidate_vp, min_isi_sec, train,
        "event_grammar_burst_event"
      )
      if (is.null(metrics)) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "span_metrics_unavailable"
        )
        next
      }
      stpd_candidate_lineage_stage4_replay_emit(
        observer, attempt, scientific_out, "seed_expansion", s, e
      )
    }
    if (length(observer$candidate_rows) >= entry$max_candidates[[1L]]) {
      stpd_candidate_lineage_stage4_note_cap(
        observer, "seed_expansion", rr, length(observer$candidate_rows)
      )
      break
    }
  }

  eg <- params$event_grammar %||% list()
  burst_internal_gap_ratio_max <- stpd_event_grammar_num(
    params$burst$classic_burst_internal_outlier_ratio_max %||%
      params$burst$internal_outlier_ratio_max %||% 3.5,
    3.5
  )
  episode_seed_min <- max(1L, stpd_event_grammar_int(
    eg$structural_burst_episode_min_seed_isi %||% 1L, 1L
  ))
  episode_seed_frac_min <- max(0, min(1, stpd_event_grammar_num(
    eg$structural_burst_episode_seed_fraction_min %||% 0.18, 0.18
  )))
  episode_bridge_frac_min <- max(0, min(1, stpd_event_grammar_num(
    eg$structural_burst_episode_bridge_fraction_min %||% 0.55, 0.55
  )))
  episode_low_isi_frac_min <- max(
    episode_bridge_frac_min,
    min(1, stpd_event_grammar_num(
      eg$structural_burst_episode_low_isi_fraction_min %||% 0.85, 0.85
    ))
  )
  episode_cv_max <- stpd_event_grammar_num(
    eg$structural_burst_episode_cv_max %||% 0.55, 0.55
  )
  structural_rescue_min_ratio <- stpd_event_grammar_num(
    eg$structural_burst_rescue_compression_min %||% 3.0, 3.0
  )
  train_bg_ref <- stpd_event_grammar_q(
    valid_isi_values(isi[valid], min_isi_sec), 0.75, NA_real_
  )

  # B. Dense episode route; its trimmed geometry is claimed only after the
  # minimum-span filter, exactly as in the scientific loop.
  if (!observer$cap_check_triggered && nrow(episode_roots) > 0L &&
      length(observer$candidate_rows) < entry$max_candidates[[1L]]) {
    for (rr in seq_len(nrow(episode_roots))) {
      if (length(observer$candidate_rows) >= entry$max_candidates[[1L]]) {
        stpd_candidate_lineage_stage4_note_cap(
          observer, "dense_episode", rr,
          length(observer$candidate_rows)
        )
        break
      }
      s0 <- episode_roots$detector_start_row[[rr]]
      e0 <- episode_roots$detector_end_row[[rr]]
      s <- s0
      e <- e0
      attempt <- stpd_candidate_lineage_stage4_begin_attempt(
        observer, "dense_episode", rr, s0, e0,
        rows_before_attempt = length(observer$candidate_rows)
      )
      idx0 <- stpd_event_grammar_safe_seq(s0, e0)
      vals0 <- if (length(idx0)) isi[idx0][valid[idx0]] else numeric()
      edge_q90 <- stpd_event_grammar_q(vals0, 0.90, NA_real_)
      edge_upper <- max(c(native_bridge, edge_q90 * 1.10), na.rm = TRUE)
      edge_upper <- min(edge_upper, episode_upper, na.rm = TRUE)
      if (!is.finite(edge_upper) || edge_upper <= 0) {
        edge_upper <- episode_upper
      }
      while (s <= e && is.finite(isi[s]) && isi[s] > edge_upper) s <- s + 1L
      while (e >= s && is.finite(isi[e]) && isi[e] > edge_upper) e <- e - 1L
      stpd_candidate_lineage_stage4_set_geometry(observer, attempt, s, e)
      idx <- stpd_event_grammar_safe_seq(s, e)
      if (length(idx) < entry$episode_min_isi[[1L]]) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "dense_trimmed_span_too_short"
        )
        next
      }
      claim <- stpd_candidate_lineage_stage4_claim_geometry(
        observer, attempt, "seed_dense_shared_seen"
      )
      if (isTRUE(claim$duplicate)) {
        stpd_candidate_lineage_stage4_finish_duplicate(observer, attempt)
        next
      }
      vals <- isi[idx][valid[idx]]
      if (length(vals) < entry$episode_min_isi[[1L]]) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "dense_valid_support_too_short"
        )
        next
      }
      seed_count <- sum(
        vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE
      )
      seed_frac <- seed_count / length(vals)
      low_isi_frac <- mean(vals <= episode_upper, na.rm = TRUE)
      q90 <- stpd_event_grammar_q(vals, 0.90, NA_real_)
      cv <- stpd_event_core_cv(vals)
      compression <- if (is.finite(train_bg_ref) && is.finite(q90) &&
          q90 > 0) train_bg_ref / q90 else NA_real_
      seed_entry_pass <- seed_count >= episode_seed_min &&
        is.finite(seed_frac) && seed_frac >= episode_seed_frac_min
      low_isi_episode_pass <- is.finite(low_isi_frac) &&
        low_isi_frac >= episode_low_isi_frac_min &&
        is.finite(q90) && q90 <= episode_upper &&
        is.finite(compression) &&
        compression >= structural_rescue_min_ratio
      if (!seed_entry_pass && !low_isi_episode_pass) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "dense_entry_evidence_failed"
        )
        next
      }
      if (!is.finite(q90) || q90 > episode_upper) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "dense_q90_failed"
        )
        next
      }
      if (is.finite(cv) && cv > episode_cv_max) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "dense_cv_failed"
        )
        next
      }
      if (!is.finite(compression) ||
          compression < structural_rescue_min_ratio) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "dense_compression_failed"
        )
        next
      }
      n_spikes <- e - s + 2L
      if (n_spikes < entry$min_spikes[[1L]]) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "dense_insufficient_spikes"
        )
        next
      }
      extension_guard <- stpd_event_grammar_burst_extension_guard(
        vals, native_bridge, episode_upper, n_spikes, vp, params
      )
      if (!isTRUE(extension_guard$pass)) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt,
          paste0("dense_extension_guard:", extension_guard$reason)
        )
        next
      }
      internal_gap_guard <- stpd_event_grammar_burst_internal_gap_guard(
        vals, vp$seed_high, ratio_max = burst_internal_gap_ratio_max
      )
      if (!isTRUE(internal_gap_guard$pass)) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt,
          paste0("dense_internal_gap_guard:", internal_gap_guard$reason)
        )
        next
      }
      metrics <- stpd_event_core_span_metrics(
        dat, s, e, params, vp, min_isi_sec, train,
        "event_grammar_burst_episode"
      )
      if (is.null(metrics)) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "dense_span_metrics_unavailable"
        )
        next
      }
      stpd_candidate_lineage_stage4_replay_emit(
        observer, attempt, scientific_out, "dense_episode", s, e
      )
    }
  }

  # C. The optional single-connector route has its own duplicate domain.
  if (isTRUE(entry$single_connector_enabled[[1L]]) &&
      nrow(episode_roots) > 0L &&
      length(observer$candidate_rows) < entry$max_candidates[[1L]]) {
    for (rr in seq_len(nrow(episode_roots))) {
      if (length(observer$candidate_rows) >= entry$max_candidates[[1L]]) {
        stpd_candidate_lineage_stage4_note_cap(
          observer, "single_expanded_bridge", rr,
          length(observer$candidate_rows)
        )
        break
      }
      run_s <- episode_roots$detector_start_row[[rr]]
      run_e <- episode_roots$detector_end_row[[rr]]
      attempt <- stpd_candidate_lineage_stage4_begin_attempt(
        observer, "single_expanded_bridge", rr, run_s, run_e,
        rows_before_attempt = length(observer$candidate_rows)
      )
      run_idx <- stpd_event_grammar_safe_seq(run_s, run_e)
      if (length(run_idx) < 2L) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "connector_source_run_too_short"
        )
        next
      }
      connector <- run_idx[
        valid[run_idx] & is.finite(isi[run_idx]) &
          isi[run_idx] > native_bridge & isi[run_idx] <= episode_upper
      ]
      if (length(connector) != 1L) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "connector_count_not_one"
        )
        next
      }
      connector <- as.integer(connector[[1L]])
      normal_bridge <- valid & is.finite(isi) &
        isi >= vp$seed_low & isi <= native_bridge
      left <- integer()
      jj <- connector - 1L
      while (jj >= run_s && isTRUE(normal_bridge[jj])) {
        left <- c(jj, left)
        jj <- jj - 1L
      }
      right <- integer()
      jj <- connector + 1L
      while (jj <= run_e && isTRUE(normal_bridge[jj])) {
        right <- c(right, jj)
        jj <- jj + 1L
      }
      side_min <- entry$connector_side_seed_min[[1L]]
      if (length(left) < side_min || length(right) < side_min) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "connector_side_core_support_failed"
        )
        next
      }
      s <- min(left)
      e <- max(right)
      stpd_candidate_lineage_stage4_set_geometry(observer, attempt, s, e)
      claim <- stpd_candidate_lineage_stage4_claim_geometry(
        observer, attempt, "single_connector_bridge_seen"
      )
      if (isTRUE(claim$duplicate)) {
        stpd_candidate_lineage_stage4_finish_duplicate(observer, attempt)
        next
      }
      idx <- stpd_event_grammar_safe_seq(s, e)
      vals <- isi[idx][valid[idx]]
      if (length(vals) != length(idx) || length(vals) < 2L) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "connector_valid_support_failed"
        )
        next
      }
      core_vals <- isi[c(left, right)]
      core_median <- stpd_event_grammar_q(core_vals, 0.50, NA_real_)
      connector_ratio <- if (is.finite(core_median) && core_median > 0) {
        isi[connector] / core_median
      } else NA_real_
      q75 <- stpd_event_grammar_q(vals, 0.75, NA_real_)
      seed_count <- sum(
        vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE
      )
      n_spikes <- e - s + 2L
      if (n_spikes < entry$min_spikes[[1L]] ||
          seed_count < 2L * side_min || !is.finite(q75) ||
          q75 > native_bridge || !is.finite(connector_ratio) ||
          connector_ratio < 1.15 ||
          connector_ratio > burst_internal_gap_ratio_max) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "connector_compactness_or_size_failed"
        )
        next
      }
      metrics <- stpd_event_core_span_metrics(
        dat, s, e, params, vp, min_isi_sec, train,
        "event_grammar_single_expanded_bridge_burst"
      )
      if (is.null(metrics)) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "connector_span_metrics_unavailable"
        )
        next
      }
      if (isTRUE(metrics$manual_negative_veto[[1L]])) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "connector_manual_negative_veto"
        )
        next
      }
      final <- stpd_event_grammar_burst_subtype_from_spike_count(
        n_spikes, vp, review_label = "possible_burst"
      )
      if (!(final %in% c("burst", "long_burst"))) {
        stpd_candidate_lineage_stage4_finish_filter(
          observer, attempt, "connector_subtype_not_canonical"
        )
        next
      }
      stpd_candidate_lineage_stage4_replay_emit(
        observer, attempt, scientific_out,
        "single_expanded_bridge", s, e
      )
    }
  } else if (isTRUE(entry$single_connector_enabled[[1L]]) &&
      nrow(episode_roots) > 0L &&
      length(observer$candidate_rows) >= entry$max_candidates[[1L]]) {
    stpd_candidate_lineage_stage4_note_cap(
      observer, "single_expanded_bridge", 1L,
      length(observer$candidate_rows)
    )
  }
  if (length(observer$candidate_rows) != nrow(scientific_out)) return(NULL)
  list(
    attempts = stpd_candidate_lineage_stage4_attempt_payload(observer),
    cap_check_triggered = observer$cap_check_triggered,
    stop_subroute = observer$stop_subroute,
    stop_source_root_ordinal = observer$stop_source_root_ordinal,
    stop_rows_n = observer$stop_rows_n
  )
}

stpd_candidate_lineage_stage4_attempts_are_valid <- function(
    payload, train, dispatch, raw, seed_runs, episode_roots,
    entry = NULL, context = NULL, scientific_out = NULL) {
  if (!is.null(entry) && nrow(entry) == 1L && !is.null(context) &&
      !is.null(scientific_out)) {
    replay <- tryCatch(
      stpd_candidate_lineage_stage4_replay_expected(
        context, entry, raw, seed_runs, episode_roots, scientific_out
      ),
      error = function(e) NULL
    )
    return(!is.null(replay) && identical(payload, replay$attempts))
  }
  if (!nrow(payload)) return(TRUE)
  if (is.null(raw) || !nrow(raw) ||
      is.null(seed_runs) || is.null(episode_roots)) return(FALSE)
  expected_k <- if (!is.null(dispatch) && nrow(dispatch) == 1L) {
    dispatch$max_candidates[[1L]]
  } else {
    unique(payload$max_candidates)
  }
  n <- raw$detector_row_index[[nrow(raw)]]
  vectors <- stpd_candidate_lineage_stage4_raw_vectors(raw, n)
  allowed_routes <- c(
    "seed_expansion", "dense_episode", "single_expanded_bridge"
  )
  allowed_terminals <- c(
    "candidate_emitted", "duplicate_skipped", "filter_rejected"
  )
  expected_root_type <- c(
    seed_expansion = "seed_run", dense_episode = "dense_episode_run",
    single_expanded_bridge = "connector_episode_run"
  )
  essential <- c(
    "train", "attempt_ordinal", "subroute", "route_attempt_ordinal",
    "source_root_type", "source_root_ordinal", "source_root_start_row",
    "source_root_end_row", "source_root_start_isi", "source_root_end_isi",
    "source_root_sha256", "geometry_available", "geometry_key",
    "duplicate_domain", "geometry_claimed", "rows_before_attempt",
    "max_candidates", "terminal_status", "terminal_reason",
    "candidate_id", "candidate_source_sha256"
  )
  if (anyNA(payload[essential]) ||
      !identical(payload$attempt_ordinal,
                 as.integer(seq_len(nrow(payload)))) ||
      !all(payload$train == train) ||
      !all(payload$subroute %in% allowed_routes) ||
      !all(payload$terminal_status %in% allowed_terminals) ||
      !all(nzchar(payload$terminal_reason)) ||
      length(expected_k) != 1L || expected_k[[1L]] < 1L ||
      !all(payload$max_candidates == expected_k[[1L]]) ||
      any(diff(match(payload$subroute, allowed_routes)) < 0L)) {
    return(FALSE)
  }
  for (route in allowed_routes) {
    rows <- which(payload$subroute == route)
    if (length(rows) && !identical(
        payload$route_attempt_ordinal[rows], as.integer(seq_along(rows)))) {
      return(FALSE)
    }
  }
  shared_claims <- new.env(hash = TRUE, parent = emptyenv())
  bridge_claims <- new.env(hash = TRUE, parent = emptyenv())
  emitted_n <- 0L
  for (i in seq_len(nrow(payload))) {
    row <- payload[i, , drop = FALSE]
    route <- row$subroute[[1L]]
    if (!identical(row$source_root_type[[1L]],
                   unname(expected_root_type[[route]])) ||
        row$source_root_ordinal[[1L]] < 1L ||
        row$source_root_start_row[[1L]] < 2L ||
        row$source_root_end_row[[1L]] < row$source_root_start_row[[1L]] ||
        row$source_root_end_row[[1L]] > n ||
        row$source_root_start_row[[1L]] !=
          row$source_root_start_isi[[1L]] + 1L ||
        row$source_root_end_row[[1L]] !=
          row$source_root_end_isi[[1L]] + 1L ||
        row$rows_before_attempt[[1L]] != emitted_n) {
      return(FALSE)
    }
    if (identical(route, "seed_expansion")) {
      root <- row$source_root_ordinal[[1L]]
      if (root > nrow(seed_runs) ||
          !isTRUE(seed_runs$min_seed_pass[[root]]) ||
          row$source_root_start_row[[1L]] !=
            seed_runs$detector_start_row[[root]] ||
          row$source_root_end_row[[1L]] !=
            seed_runs$detector_end_row[[root]]) return(FALSE)
      root_hash <- stpd_candidate_lineage_stage4_support_hash(
        "seed_run", root, row$source_root_start_row[[1L]],
        row$source_root_end_row[[1L]], vectors$isi, vectors$valid
      )
    } else {
      root <- row$source_root_ordinal[[1L]]
      if (root > nrow(episode_roots) ||
          row$source_root_start_row[[1L]] !=
            episode_roots$detector_start_row[[root]] ||
          row$source_root_end_row[[1L]] !=
            episode_roots$detector_end_row[[root]]) return(FALSE)
      root_hash <- episode_roots$support_sha256[[root]]
    }
    if (!identical(row$source_root_sha256[[1L]], root_hash)) return(FALSE)
    geometry_available <- isTRUE(row$geometry_available[[1L]])
    if (geometry_available) {
      if (anyNA(row[c(
          "proposed_start_row", "proposed_end_row",
          "proposed_start_isi", "proposed_end_isi"
        )]) || row$proposed_start_row[[1L]] < 2L ||
          row$proposed_end_row[[1L]] < row$proposed_start_row[[1L]] ||
          row$proposed_end_row[[1L]] > n ||
          row$proposed_start_row[[1L]] !=
            row$proposed_start_isi[[1L]] + 1L ||
          row$proposed_end_row[[1L]] !=
            row$proposed_end_isi[[1L]] + 1L ||
          !identical(
            row$geometry_key[[1L]],
            paste0(row$proposed_start_row[[1L]], "_",
                   row$proposed_end_row[[1L]])
          )) return(FALSE)
    } else if (!all(is.na(unlist(row[c(
        "proposed_start_row", "proposed_end_row",
        "proposed_start_isi", "proposed_end_isi"
      )]))) || nzchar(row$geometry_key[[1L]]) ||
        isTRUE(row$geometry_claimed[[1L]])) {
      return(FALSE)
    }
    if (isTRUE(row$geometry_claimed[[1L]])) {
      expected_domain <- if (identical(route, "single_expanded_bridge")) {
        "single_connector_bridge_seen"
      } else {
        "seed_dense_shared_seen"
      }
      if (!identical(row$duplicate_domain[[1L]], expected_domain)) {
        return(FALSE)
      }
      claims <- if (identical(expected_domain,
                             "seed_dense_shared_seen")) {
        shared_claims
      } else bridge_claims
      key <- row$geometry_key[[1L]]
      duplicate_of <- if (exists(key, envir = claims, inherits = FALSE)) {
        get(key, envir = claims, inherits = FALSE)
      } else {
        assign(key, i, envir = claims)
        NA_integer_
      }
      if (!identical(row$duplicate_of_attempt_ordinal[[1L]],
                     as.integer(duplicate_of))) return(FALSE)
    } else if (nzchar(row$duplicate_domain[[1L]]) ||
        !is.na(row$duplicate_of_attempt_ordinal[[1L]])) {
      return(FALSE)
    }
    if (identical(row$terminal_status[[1L]], "duplicate_skipped")) {
      if (!isTRUE(row$geometry_claimed[[1L]]) ||
          is.na(row$duplicate_of_attempt_ordinal[[1L]]) ||
          !is.na(row$candidate_append_ordinal[[1L]]) ||
          nzchar(row$candidate_id[[1L]]) ||
          nzchar(row$candidate_source_sha256[[1L]])) return(FALSE)
    } else if (identical(row$terminal_status[[1L]],
                         "candidate_emitted")) {
      emitted_n <- emitted_n + 1L
      if (!isTRUE(row$geometry_claimed[[1L]]) ||
          !is.na(row$duplicate_of_attempt_ordinal[[1L]]) ||
          row$candidate_append_ordinal[[1L]] != emitted_n ||
          !nzchar(row$candidate_id[[1L]]) ||
          !grepl("^[0-9a-f]{64}$",
                 row$candidate_source_sha256[[1L]])) return(FALSE)
    } else if (!is.na(row$candidate_append_ordinal[[1L]]) ||
        nzchar(row$candidate_id[[1L]]) ||
        nzchar(row$candidate_source_sha256[[1L]])) {
      return(FALSE)
    }
  }
  TRUE
}

stpd_candidate_lineage_stage4_candidates_are_valid <- function(
    payload, train, attempts, scientific_out = NULL) {
  emitted <- if (is.null(attempts) || !nrow(attempts)) integer() else
    which(attempts$terminal_status == "candidate_emitted")
  if (!length(emitted)) {
    return(!nrow(payload) &&
      (is.null(scientific_out) || !nrow(scientific_out)))
  }
  hash_fields <- setdiff(names(payload), "candidate_observation_sha256")
  expected_observation_hash <- vapply(seq_len(nrow(payload)), function(i) {
    stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-candidate-observation-v1",
      payload[i, hash_fields, drop = FALSE]
    )
  }, character(1))
  if (nrow(payload) != length(emitted) || anyNA(payload) ||
      !all(payload$train == train) ||
      !identical(payload$candidate_append_ordinal,
                 as.integer(seq_len(nrow(payload)))) ||
      !identical(payload$source_attempt_ordinal,
                 attempts$attempt_ordinal[emitted]) ||
      !identical(payload$subroute, attempts$subroute[emitted]) ||
      !identical(payload$candidate_id, attempts$candidate_id[emitted]) ||
      !identical(payload$source_payload_sha256,
                 attempts$candidate_source_sha256[emitted]) ||
      !identical(payload$detector_start_row,
                 attempts$proposed_start_row[emitted]) ||
      !identical(payload$detector_end_row,
                 attempts$proposed_end_row[emitted]) ||
      !identical(payload$detector_start_row, payload$start_isi + 1L) ||
      !identical(payload$detector_end_row, payload$end_isi + 1L) ||
      any(!nzchar(payload$candidate_id)) ||
      any(!grepl("^[0-9a-f]{64}$", payload$source_payload_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", payload$candidate_observation_sha256)) ||
      !identical(
        payload$candidate_observation_sha256, expected_observation_hash
      )) {
    return(FALSE)
  }
  if (!is.null(scientific_out)) {
    expected <- tryCatch(
      stpd_candidate_lineage_stage4_candidate_payload_from_scientific(
        scientific_out, train, attempts
      ),
      error = function(e) NULL
    )
    if (is.null(expected) || !identical(payload, expected)) return(FALSE)
  }
  TRUE
}

stpd_candidate_lineage_stage4_receipt_is_valid <- function(
    payload, train, outer_entry, dispatch, raw, seed_runs, episode_roots,
    attempts, candidates, stage4_entry = NULL, context = NULL,
    scientific_out = NULL) {
  if (nrow(payload) != 1L || anyNA(payload[setdiff(
      names(payload), c("stop_source_root_ordinal")
    )]) || is.null(outer_entry) || is.null(raw) ||
      is.null(seed_runs) || is.null(episode_roots) || is.null(attempts) ||
      is.null(candidates)) return(FALSE)
  n <- outer_entry$n_interval_rows[[1L]]
  short <- n <= 2L
  expected_k <- if (!is.null(dispatch) && nrow(dispatch) == 1L) {
    dispatch$max_candidates[[1L]]
  } else if (nrow(attempts)) {
    unique(attempts$max_candidates)
  } else {
    payload$max_candidates[[1L]]
  }
  hashes_valid <- identical(
    payload$attempt_payload_sha256[[1L]],
    stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-attempts-v1", attempts
    )
  ) && identical(
    payload$candidate_payload_sha256[[1L]],
    stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-candidates-v1", candidates
    )
  ) && all(grepl("^[0-9a-f]{64}$", c(
    payload$proposal_intrusion_mask_sha256,
    payload$episode_support_sha256,
    payload$attempt_payload_sha256,
    payload$candidate_payload_sha256
  )))
  base_valid <- identical(payload$train[[1L]], train) &&
    identical(payload$burst_pipeline_id[[1L]], "final") &&
    identical(payload$n_interval_rows[[1L]], n) &&
    length(expected_k) == 1L &&
    identical(payload$max_candidates[[1L]], expected_k[[1L]]) &&
    payload$max_candidates[[1L]] >= 1L &&
    payload$max_expand[[1L]] >= 0L && hashes_valid
  if (!base_valid) return(FALSE)
  if (short) {
    short_hashes_valid <- identical(
      payload$proposal_intrusion_mask_sha256[[1L]],
      stpd_threshold_first_hash_domain(
        "stpd-burst-stage4-proposal-intrusion-mask-v1", logical()
      )
    ) && identical(
      payload$episode_support_sha256[[1L]],
      stpd_threshold_first_hash_domain(
        "stpd-burst-stage4-episode-support-v1", data.frame()
      )
    )
    return(!nrow(raw) && !nrow(seed_runs) && !nrow(episode_roots) &&
      !nrow(attempts) && !nrow(candidates) &&
      identical(payload$applicability_status[[1L]],
                "not_applicable_short_train") &&
      identical(payload$coverage_status[[1L]],
                "stage4_not_applicable_short_train") &&
      !isTRUE(payload$cap_check_triggered[[1L]]) &&
      !isTRUE(payload$stage4_search_exhausted[[1L]]) &&
      !nzchar(payload$stop_subroute[[1L]]) &&
      is.na(payload$stop_source_root_ordinal[[1L]]) &&
      identical(payload$seed_status[[1L]],
                "not_applicable_short_train") &&
      identical(payload$dense_status[[1L]],
                "not_applicable_short_train") &&
      identical(payload$connector_status[[1L]],
                "not_applicable_short_train") &&
      short_hashes_valid &&
      all(unlist(payload[c(
        "seed_root_n", "seed_eligible_root_n", "episode_root_n",
        "observed_attempt_n", "emitted_candidate_n", "duplicate_n",
        "filter_rejected_n", "seed_attempt_n", "dense_attempt_n",
        "connector_attempt_n", "seed_candidate_n", "dense_candidate_n",
        "connector_candidate_n", "stop_rows_n"
      )]) == 0L))
  }
  vectors <- stpd_candidate_lineage_stage4_raw_vectors(raw, n)
  native_bridge <- raw$native_bridge_high_sec[[1L]]
  proposal_intrusion <- stpd_event_grammar_burst_intrusion_mask(
    vectors$isi, vectors$valid, native_bridge,
    payload$candidate_bridge_high_sec[[1L]],
    payload$borrowed_run_max[[1L]]
  )
  proposal_hash <- stpd_threshold_first_hash_domain(
    "stpd-burst-stage4-proposal-intrusion-mask-v1",
    as.logical(proposal_intrusion)
  )
  expected_roots <-
    stpd_candidate_lineage_stage4_episode_roots_expected(
      raw, n, payload$episode_upper_sec[[1L]],
      payload$borrowed_run_max[[1L]]
    )
  episode_intrusion <- stpd_event_grammar_burst_intrusion_mask(
    vectors$isi, vectors$valid, native_bridge,
    payload$episode_upper_sec[[1L]], payload$borrowed_run_max[[1L]]
  )
  episode_flag <- vectors$valid &
    vectors$isi <= payload$episode_upper_sec[[1L]] & !episode_intrusion
  support_rows <- seq.int(2L, n)
  episode_support <- data.frame(
    detector_row_index = as.integer(support_rows),
    intrusion_mask = as.logical(episode_intrusion[support_rows]),
    episode_member = as.logical(episode_flag[support_rows]),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  episode_hash <- stpd_threshold_first_hash_domain(
    "stpd-burst-stage4-episode-support-v1", episode_support
  )
  count <- function(table, field, value) {
    as.integer(if (!nrow(table)) 0L else sum(table[[field]] == value))
  }
  counts_valid <-
    payload$seed_root_n[[1L]] == nrow(seed_runs) &&
    payload$seed_eligible_root_n[[1L]] == sum(seed_runs$min_seed_pass) &&
    payload$episode_root_n[[1L]] == nrow(episode_roots) &&
    identical(episode_roots, expected_roots) &&
    payload$observed_attempt_n[[1L]] == nrow(attempts) &&
    payload$emitted_candidate_n[[1L]] == nrow(candidates) &&
    payload$duplicate_n[[1L]] ==
      count(attempts, "terminal_status", "duplicate_skipped") &&
    payload$filter_rejected_n[[1L]] ==
      count(attempts, "terminal_status", "filter_rejected") &&
    payload$seed_attempt_n[[1L]] ==
      count(attempts, "subroute", "seed_expansion") &&
    payload$dense_attempt_n[[1L]] ==
      count(attempts, "subroute", "dense_episode") &&
    payload$connector_attempt_n[[1L]] ==
      count(attempts, "subroute", "single_expanded_bridge") &&
    payload$seed_candidate_n[[1L]] ==
      count(candidates, "subroute", "seed_expansion") &&
    payload$dense_candidate_n[[1L]] ==
      count(candidates, "subroute", "dense_episode") &&
    payload$connector_candidate_n[[1L]] ==
      count(candidates, "subroute", "single_expanded_bridge")
  route_order <- c(
    seed_expansion = 1L, dense_episode = 2L,
    single_expanded_bridge = 3L
  )
  status_for <- function(route, has_source, enabled = TRUE) {
    if (!enabled) return("not_applied_disabled")
    if (!has_source) return("not_applicable_no_source_roots")
    if (!isTRUE(payload$cap_check_triggered[[1L]])) {
      return("search_exhausted")
    }
    stop_route <- payload$stop_subroute[[1L]]
    if (!(stop_route %in% names(route_order))) return("invalid_stop_route")
    if (route_order[[route]] < route_order[[stop_route]]) {
      return("search_exhausted_before_shared_cap")
    }
    if (identical(route, stop_route)) return("truncated_by_shared_cap")
    "not_entered_shared_cap"
  }
  expected_statuses <- c(
    seed_status = status_for(
      "seed_expansion", payload$seed_eligible_root_n[[1L]] > 0L
    ),
    dense_status = status_for(
      "dense_episode", payload$episode_root_n[[1L]] > 0L
    ),
    connector_status = status_for(
      "single_expanded_bridge", payload$episode_root_n[[1L]] > 0L,
      isTRUE(payload$single_connector_enabled[[1L]])
    )
  )
  statuses_valid <- identical(
    unname(unlist(payload[c(
      "seed_status", "dense_status", "connector_status"
    )])), unname(expected_statuses)
  )
  settings_valid <- !is.null(stage4_entry) && nrow(stage4_entry) == 1L &&
    identical(payload$max_candidates[[1L]],
              stage4_entry$max_candidates[[1L]]) &&
    identical(payload$max_expand[[1L]], stage4_entry$max_expand[[1L]]) &&
    identical(payload$candidate_bridge_high_sec[[1L]],
              stage4_entry$candidate_bridge_high_sec[[1L]]) &&
    identical(payload$episode_upper_sec[[1L]],
              stage4_entry$episode_upper_sec[[1L]]) &&
    identical(payload$borrowed_run_max[[1L]],
              stage4_entry$borrowed_run_max[[1L]]) &&
    identical(payload$single_connector_enabled[[1L]],
              stage4_entry$single_connector_enabled[[1L]])
  replay <- if (settings_valid) tryCatch(
    stpd_candidate_lineage_stage4_replay_expected(
      context, stage4_entry, raw, seed_runs, episode_roots, scientific_out
    ),
    error = function(e) NULL
  ) else NULL
  replay_valid <- !is.null(replay) &&
    identical(attempts, replay$attempts) &&
    identical(payload$cap_check_triggered[[1L]],
              replay$cap_check_triggered) &&
    identical(payload$stop_subroute[[1L]], replay$stop_subroute) &&
    identical(payload$stop_source_root_ordinal[[1L]],
              replay$stop_source_root_ordinal) &&
    identical(payload$stop_rows_n[[1L]], replay$stop_rows_n)
  cap_valid <- if (isTRUE(payload$cap_check_triggered[[1L]])) {
    stop_root_limit <- switch(
      payload$stop_subroute[[1L]],
      seed_expansion = payload$seed_root_n[[1L]],
      dense_episode = payload$episode_root_n[[1L]],
      single_expanded_bridge = payload$episode_root_n[[1L]],
      0L
    )
    payload$stop_subroute[[1L]] %in% c(
      "seed_expansion", "dense_episode", "single_expanded_bridge"
    ) && !is.na(payload$stop_source_root_ordinal[[1L]]) &&
      payload$stop_source_root_ordinal[[1L]] >= 1L &&
      payload$stop_source_root_ordinal[[1L]] <= stop_root_limit &&
      payload$stop_rows_n[[1L]] == payload$max_candidates[[1L]] &&
      nrow(candidates) == payload$max_candidates[[1L]] &&
      !isTRUE(payload$stage4_search_exhausted[[1L]]) &&
      identical(payload$coverage_status[[1L]],
                "stage4_incomplete_early_stop")
  } else {
    !nzchar(payload$stop_subroute[[1L]]) &&
      is.na(payload$stop_source_root_ordinal[[1L]]) &&
      payload$stop_rows_n[[1L]] == 0L &&
      isTRUE(payload$stage4_search_exhausted[[1L]]) &&
      identical(payload$coverage_status[[1L]],
                "stage4_search_complete")
  }
  identical(payload$applicability_status[[1L]], "applied") &&
    identical(payload$proposal_intrusion_mask_sha256[[1L]], proposal_hash) &&
    identical(payload$episode_support_sha256[[1L]], episode_hash) &&
    counts_valid && statuses_valid && settings_valid && replay_valid &&
    cap_valid
}
