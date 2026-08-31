# Threshold-first Gate 1B Burst--Pause ownership observation -------------
#
# This module observes only the internal Pause-ownership support returned by
# stpd_event_core_freeze_burst_bridges().  The sidecar output is not a Burst
# Event result and is never authorized for Event materialization.

stpd_candidate_lineage_pause_ownership_candidate_hash <- function(row, stage) {
  stpd_candidate_lineage_candidate_source_hash(
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE),
    paste0("burst_pause_ownership_", as.character(stage)[1L])
  )
}

stpd_candidate_lineage_pause_ownership_set_hash <- function(
    ordinals, candidate_ids, payload_hashes, domain) {
  stpd_threshold_first_hash_domain(
    domain,
    data.frame(
      source_input_ordinal = as.integer(ordinals),
      source_candidate_id = as.character(candidate_ids),
      source_payload_sha256 = as.character(payload_hashes),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
}

stpd_candidate_lineage_pause_ownership_no_burst_branch_is_closed <- function(
    train_collector, input_candidates, boundaries,
    scientific_out = NULL, science_context = NULL) {
  context_name <- "pause_raw_canonical_context"
  raw_context <- if (exists(
      context_name, envir = train_collector, inherits = FALSE) &&
      bindingIsLocked(context_name, train_collector)) {
    get(context_name, envir = train_collector, inherits = FALSE)
  } else NULL
  raw_receipt <- train_collector$observations$pause_raw_receipt_v1
  patterns <- as.character(raw_context$patterns %||% character())
  burst_requested <- any(c("burst", "long_burst") %in% patterns)
  science_closed <- is.null(science_context) || (
    is.list(science_context) && !is.null(raw_context) &&
    identical(science_context$train, raw_context$train) &&
    identical(science_context$dat, raw_context$dat) &&
    identical(science_context$params, raw_context$params) &&
    identical(science_context$vp, raw_context$vp) &&
    identical(science_context$min_isi_sec, raw_context$min_isi_sec)
  )
  output_closed <- is.null(scientific_out) ||
    (is.data.frame(scientific_out) && nrow(scientific_out) == 0L)
  !is.null(raw_context) && !burst_requested &&
    is.data.frame(input_candidates) && nrow(input_candidates) == 0L &&
    is.data.frame(boundaries) && nrow(boundaries) == 0L &&
    isTRUE(output_closed) && isTRUE(science_closed) &&
    !exists("burst_refractory_context", envir = train_collector,
            inherits = FALSE) &&
    is.null(train_collector$observations$burst_refractory_output_v1) &&
    is.null(train_collector$observations$burst_refractory_receipt_v1) &&
    is.data.frame(raw_receipt) && nrow(raw_receipt) == 1L &&
    stpd_candidate_lineage_pause_raw_payload_is_valid(
      train_collector, "pause_raw_receipt_v1", raw_receipt
    )
}

stpd_candidate_lineage_pause_ownership_upstream_is_closed <- function(
    train_collector, input_candidates, boundaries) {
  context <- if (exists(
      "burst_refractory_context", envir = train_collector,
      inherits = FALSE)) {
    train_collector$burst_refractory_context
  } else NULL
  output_payload <-
    train_collector$observations$burst_refractory_output_v1
  receipt_payload <-
    train_collector$observations$burst_refractory_receipt_v1
  if (is.null(context) &&
      stpd_candidate_lineage_pause_ownership_no_burst_branch_is_closed(
        train_collector, input_candidates, boundaries
      )) {
    return(TRUE)
  }
  if (is.null(context) || !is.data.frame(context$scientific_out) ||
      is.null(output_payload) ||
      is.null(receipt_payload) ||
      !stpd_candidate_lineage_refractory_payload_is_valid(
        train_collector, "burst_refractory_output_v1", output_payload
      ) ||
      !stpd_candidate_lineage_refractory_payload_is_valid(
        train_collector, "burst_refractory_receipt_v1", receipt_payload
      ) ||
      !identical(input_candidates, context$scientific_out) ||
      nrow(boundaries) != 0L ||
      nrow(input_candidates) != nrow(output_payload)) {
    return(FALSE)
  }
  if (!nrow(input_candidates)) return(TRUE)
  ids <- stpd_candidate_lineage_candidate_column(
    input_candidates, "candidate_id", "character"
  )
  starts <- stpd_candidate_lineage_candidate_column(
    input_candidates, "start_isi", "integer"
  )
  ends <- stpd_candidate_lineage_candidate_column(
    input_candidates, "end_isi", "integer"
  )
  hashes <- vapply(seq_len(nrow(input_candidates)), function(i) {
    stpd_candidate_lineage_refractory_candidate_hash(
      input_candidates[i, , drop = FALSE], "refractory_policy_output"
    )
  }, character(1))
  "train" %in% names(input_candidates) &&
    !anyNA(input_candidates$train) &&
    all(input_candidates$train == train_collector$train) &&
    identical(ids, output_payload$output_candidate_id) &&
    identical(starts, output_payload$detector_start_row) &&
    identical(ends, output_payload$detector_end_row) &&
    identical(hashes, output_payload$output_payload_sha256)
}

stpd_candidate_lineage_pause_ownership_science_context_is_closed <- function(
    train_collector, context) {
  refractory <- if (exists(
      "burst_refractory_context", envir = train_collector,
      inherits = FALSE)) {
    train_collector$burst_refractory_context
  } else NULL
  if (is.null(refractory)) {
    return(
      stpd_candidate_lineage_pause_ownership_no_burst_branch_is_closed(
        train_collector, context$input_candidates, context$boundaries,
        scientific_out = context$scientific_out,
        science_context = context
      )
    )
  }
  is.list(refractory) &&
    identical(context$train, refractory$train) &&
    identical(context$dat, refractory$dat) &&
    identical(context$params, refractory$params) &&
    identical(context$vp, refractory$vp) &&
    identical(context$min_isi_sec, refractory$min_isi_sec)
}

stpd_candidate_lineage_pause_ownership_eligibility <- function(context) {
  input <- context$input_candidates
  if (!nrow(input)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_pause_ownership_eligibility_v1"
    ))
  }
  labels <- rep_len(
    as.character(input$final_label %||% ""), nrow(input)
  )
  actions <- rep_len(
    as.character(input$action %||% input$decision_action %||% ""),
    nrow(input)
  )
  eligible <- labels %in% c("burst", "long_burst") & actions == "accept"
  eligible[is.na(eligible)] <- FALSE
  reasons <- ifelse(
    !(labels %in% c("burst", "long_burst")),
    "excluded_non_burst_label",
    ifelse(actions != "accept" | is.na(actions),
           "excluded_non_accept_action", "eligible")
  )
  rows <- lapply(seq_len(nrow(input)), function(i) {
    row <- input[i, , drop = FALSE]
    source_hash <- stpd_candidate_lineage_refractory_candidate_hash(
      row, "refractory_policy_output"
    )
    data.frame(
      train = context$train, input_ordinal = as.integer(i),
      source_candidate_id = stpd_candidate_lineage_candidate_column(
        row, "candidate_id", "character"
      )[[1L]],
      source_payload_sha256 = source_hash,
      detector_start_row = stpd_candidate_lineage_candidate_column(
        row, "start_isi", "integer"
      )[[1L]],
      detector_end_row = stpd_candidate_lineage_candidate_column(
        row, "end_isi", "integer"
      )[[1L]],
      final_label = labels[[i]], action = actions[[i]],
      eligible = eligible[[i]], eligibility_reason = reasons[[i]],
      boundary_output_ordinal = as.integer(i),
      boundary_output_payload_sha256 = source_hash,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  out[, names(stpd_candidate_lineage_observation_hook_schema(
    "burst_pause_ownership_eligibility_v1"
  )), drop = FALSE]
}

stpd_candidate_lineage_pause_ownership_selection <- function(
    context, eligibility) {
  input <- context$input_candidates
  eligible_ordinals <- eligibility$input_ordinal[eligibility$eligible]
  if (!length(eligible_ordinals)) {
    return(list(
      payload = stpd_candidate_lineage_empty_hook_payload(
        "burst_pause_ownership_selection_v1"
      ),
      accepted_pool = input[0, , drop = FALSE],
      selected_rows = input[0, , drop = FALSE],
      selected_input_ordinals = integer()
    ))
  }
  accepted_pool <- input[eligible_ordinals, , drop = FALSE]
  selected_all <- stpd_event_core_weighted_select(
    accepted_pool, locked = NULL, patterns = c("burst", "long_burst")
  )
  selected <- as.logical(selected_all$selected_for_auto)
  selected[is.na(selected)] <- FALSE
  starts <- suppressWarnings(as.integer(selected_all$start_isi))
  ends <- suppressWarnings(as.integer(selected_all$end_isi))
  selector_order <- order(ends, starts)
  selector_rank <- integer(nrow(selected_all))
  selector_rank[selector_order] <- seq_along(selector_order)
  selected_pool_ordinals <- which(selected)
  selected_rows <- selected_all[selected_pool_ordinals, , drop = FALSE]
  selected_input_ordinals <- eligible_ordinals[selected_pool_ordinals]
  if (nrow(selected_rows)) {
    atom_order <- order(
      suppressWarnings(as.integer(selected_rows$start_isi)),
      suppressWarnings(as.integer(selected_rows$end_isi))
    )
    atom_ordinal <- rep(NA_integer_, nrow(selected_all))
    atom_ordinal[selected_pool_ordinals[atom_order]] <- seq_along(atom_order)
  } else {
    atom_ordinal <- rep(NA_integer_, nrow(selected_all))
  }
  rows <- lapply(seq_len(nrow(selected_all)), function(i) {
    row <- selected_all[i, , drop = FALSE]
    source <- accepted_pool[i, , drop = FALSE]
    data.frame(
      train = context$train, accepted_pool_ordinal = as.integer(i),
      source_input_ordinal = as.integer(eligible_ordinals[[i]]),
      source_candidate_id = stpd_candidate_lineage_candidate_column(
        source, "candidate_id", "character"
      )[[1L]],
      source_payload_sha256 =
        stpd_candidate_lineage_refractory_candidate_hash(
          source, "refractory_policy_output"
        ),
      detector_start_row = starts[[i]], detector_end_row = ends[[i]],
      selector_end_start_rank = as.integer(selector_rank[[i]]),
      candidate_value = as.numeric(
        stpd_event_core_candidate_value(source)
      ),
      selected_atom = selected[[i]],
      selection_status = as.character(row$selection_status[[1L]]),
      atom_ordinal = as.integer(atom_ordinal[[i]]),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  payload <- dplyr::bind_rows(rows)
  payload <- payload[, names(stpd_candidate_lineage_observation_hook_schema(
    "burst_pause_ownership_selection_v1"
  )), drop = FALSE]
  list(
    payload = payload, accepted_pool = accepted_pool,
    selected_rows = selected_rows,
    selected_input_ordinals = as.integer(selected_input_ordinals)
  )
}

stpd_candidate_lineage_pause_ownership_replay <- function(context) {
  if (is.null(context) || !is.list(context) ||
      !is.character(context$train) || length(context$train) != 1L ||
      is.na(context$train) || !nzchar(context$train) ||
      !is.data.frame(context$input_candidates) ||
      !is.data.frame(context$scientific_out) ||
      !is.data.frame(context$dat) || !is.list(context$params) ||
      !is.list(context$vp) ||
      !is.numeric(context$min_isi_sec) ||
      length(context$min_isi_sec) != 1L ||
      !is.finite(context$min_isi_sec) || context$min_isi_sec < 0 ||
      !is.data.frame(context$boundaries)) {
    return(NULL)
  }
  input <- context$input_candidates
  if (nrow(input) &&
      (!("train" %in% names(input)) || anyNA(input$train) ||
       !all(input$train == context$train))) {
    return(NULL)
  }
  eligibility <- stpd_candidate_lineage_pause_ownership_eligibility(context)
  selection <- stpd_candidate_lineage_pause_ownership_selection(
    context, eligibility
  )
  accepted_pool <- selection$accepted_pool
  atoms <- selection$selected_rows
  atom_input_ordinals <- selection$selected_input_ordinals
  if (nrow(atoms)) {
    atom_order <- order(
      suppressWarnings(as.integer(atoms$start_isi)),
      suppressWarnings(as.integer(atoms$end_isi))
    )
    atoms_ordered <- atoms[atom_order, , drop = FALSE]
    atom_input_ordered <- atom_input_ordinals[atom_order]
  } else {
    atoms_ordered <- atoms
    atom_input_ordered <- integer()
  }

  eg <- context$params$event_grammar %||% list()
  bridge_factor <- max(1, stpd_event_core_num(
    eg$structural_burst_episode_bridge_factor %||% 1.75, 1.75
  ))
  bridge_upper <- stpd_event_core_num(context$vp$bridge_high, NA_real_)
  bridge_ceiling <- if (is.finite(bridge_upper) && bridge_upper > 0) {
    max(bridge_upper, bridge_upper * bridge_factor)
  } else NA_real_
  max_gap_n <- max(1L, stpd_event_core_int(
    context$vp$max_bridge_n, 4L
  ))
  max_bridge_fraction <- min(max(stpd_event_core_num(
    context$vp$max_bridge_frac, 0.60
  ), 0), 1)
  min_seed_n <- as.integer(stpd_event_core_int(
    context$vp$min_seed_isi_n, 0L
  ))
  helper_invoked <- nrow(input) > 0L
  applicability <- if (!helper_invoked) {
    "not_invoked_empty_input"
  } else if (!nrow(accepted_pool)) {
    "invoked_no_eligible_candidates"
  } else if (nrow(atoms) < 2L) {
    "invoked_fewer_than_two_selected_atoms"
  } else if (!is.finite(bridge_upper) || bridge_upper <= 0) {
    "invoked_invalid_bridge_upper"
  } else {
    "invoked_one_pass_pair_scan"
  }
  evidence_origin <- if (helper_invoked) {
    "live_input_output_with_deterministic_branch_replay"
  } else {
    "live_control_flow_with_deterministic_not_invoked_replay"
  }
  entry <- data.frame(
    train = context$train, burst_pipeline_id = "final",
    applicability_status = applicability,
    input_candidate_n = as.integer(nrow(input)),
    eligible_candidate_n = as.integer(nrow(accepted_pool)),
    selected_atom_n = as.integer(nrow(atoms)),
    canonical_boundary_n = as.integer(nrow(context$boundaries)),
    bridge_upper_sec = as.numeric(bridge_upper),
    bridge_ceiling_sec = as.numeric(bridge_ceiling),
    bridge_factor = as.numeric(bridge_factor),
    seed_low_sec = as.numeric(context$vp$seed_low),
    seed_high_sec = as.numeric(context$vp$seed_high),
    min_seed_isi_n = min_seed_n,
    max_gap_n = as.integer(max_gap_n),
    max_bridge_n = as.numeric(context$vp$max_bridge_n),
    classic_max_spikes = as.integer(context$vp$classic_max_spikes),
    max_bridge_fraction = as.numeric(max_bridge_fraction),
    evidence_origin = evidence_origin,
    input_scope = if (isTRUE(context$burst_family_requested)) {
      "ordinary_post_refractory_burst_candidates"
    } else "typed_empty_burst_family_not_requested",
    upstream_boundary_status =
      "not_applicable_ownership_precedes_pause_boundary",
    upstream_pause_status =
      "not_applicable_ownership_precedes_contextual_pause",
    upstream_universe_status = "candidate_universe_unavailable",
    output_role = "pause_ownership_support_only",
    input_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-pause-ownership-input-v1", input
    ),
    boundary_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-pause-ownership-boundaries-v1", context$boundaries
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  attempts <- list()
  next_rows <- list()
  output_info <- list()
  counter <- 0L
  lone_tail_n <- 0L
  isi <- suppressWarnings(as.numeric(context$dat$ISI_sec))
  artifact <- is_artifact_isi(isi, context$min_isi_sec)
  valid <- is.finite(isi) & !artifact
  if (length(valid)) valid[[1L]] <- FALSE

  add_retained <- function(row, input_ordinal) {
    next_rows[[length(next_rows) + 1L]] <<- row
    output_info[[length(output_info) + 1L]] <<- list(
      relation = "retained_ownership_atom",
      source_input_ordinals = as.integer(input_ordinal),
      witness_input_ordinals = integer()
    )
    length(next_rows)
  }
  add_merged <- function(row, source_ordinals, witness_ordinals) {
    next_rows[[length(next_rows) + 1L]] <<- row
    output_info[[length(output_info) + 1L]] <<- list(
      relation = "merge_ownership_child",
      source_input_ordinals = as.integer(source_ordinals),
      witness_input_ordinals = as.integer(witness_ordinals)
    )
    length(next_rows)
  }

  if (!helper_invoked || !nrow(accepted_pool)) {
    replayed_out <- data.frame()
  } else if (nrow(atoms) < 2L ||
      !is.finite(bridge_upper) || bridge_upper <= 0) {
    replayed_out <- atoms
    if (nrow(atoms)) {
      output_info <- lapply(seq_len(nrow(atoms)), function(i) list(
        relation = "retained_ownership_atom",
        source_input_ordinals = as.integer(atom_input_ordinals[[i]]),
        witness_input_ordinals = integer()
      ))
      if (nrow(atoms) == 1L) lone_tail_n <- 1L
    }
  } else {
    ii <- 1L
    while (ii <= nrow(atoms_ordered)) {
      if (ii >= nrow(atoms_ordered)) {
        add_retained(
          atoms_ordered[ii, , drop = FALSE], atom_input_ordered[[ii]]
        )
        lone_tail_n <- lone_tail_n + 1L
        ii <- ii + 1L
        next
      }
      left <- atoms_ordered[ii, , drop = FALSE]
      right <- atoms_ordered[ii + 1L, , drop = FALSE]
      left_input <- as.integer(atom_input_ordered[[ii]])
      right_input <- as.integer(atom_input_ordered[[ii + 1L]])
      s <- suppressWarnings(as.integer(left$start_isi[[1L]]))
      e <- suppressWarnings(as.integer(right$end_isi[[1L]]))
      gates <- as.list(stats::setNames(
        rep("not_evaluated", 12L),
        c(
          "boundary_gate", "witness_gate", "gap_shape_gate",
          "gap_validity_gate", "support_gate", "ceiling_gate",
          "core_gate", "bridge_count_gate", "bridge_fraction_gate",
          "expanded_bridge_gate", "q90_gate", "construction_gate"
        )
      ))
      overlap <- if (nrow(context$boundaries)) which(
        as.integer(context$boundaries$start_isi) <= e &
          as.integer(context$boundaries$end_isi) >= s
      ) else integer()
      witness <- accepted_pool[0, , drop = FALSE]
      witness_input <- integer()
      gap <- integer()
      gap_value <- NA_real_
      gap_valid <- NA
      core_n <- NA_integer_
      bridge_n <- NA_integer_
      bridge_fraction <- NA_real_
      expanded_n <- NA_integer_
      q90 <- NA_real_
      terminal_reason <- ""
      terminal_status <- "merge_rejected"
      merged <- NULL

      gates$boundary_gate <- if (length(overlap)) "fail" else "pass"
      if (length(overlap)) {
        terminal_reason <- "canonical_pause_overlap"
      } else {
        witness_mask <-
          as.integer(accepted_pool$start_isi) <= s &
          as.integer(accepted_pool$end_isi) >= e
        witness <- accepted_pool[witness_mask, , drop = FALSE]
        witness_input <- eligibility$input_ordinal[eligibility$eligible][
          witness_mask
        ]
        gates$witness_gate <- if (nrow(witness)) "pass" else "fail"
        if (!nrow(witness)) {
          terminal_reason <- "no_spanning_accepted_witness"
        } else {
          gap <- stpd_event_core_safe_seq(
            suppressWarnings(as.integer(left$end_isi[[1L]])) + 1L,
            suppressWarnings(as.integer(right$start_isi[[1L]])) - 1L
          )
          gates$gap_shape_gate <- if (
            length(gap) == 1L && length(gap) <= max_gap_n
          ) "pass" else "fail"
          if (!identical(gates$gap_shape_gate, "pass")) {
            terminal_reason <- "gap_not_exactly_one_isi"
          } else {
            gap_valid <- !any(!valid[gap])
            gap_value <- as.numeric(isi[gap][[1L]])
            gates$gap_validity_gate <- if (gap_valid) "pass" else "fail"
            if (!gap_valid) {
              terminal_reason <- "gap_invalid_or_artifact"
            } else {
              idx <- stpd_event_core_safe_seq(s, e)
              vals <- isi[idx][valid[idx]]
              gates$support_gate <- if (length(vals)) "pass" else "fail"
              if (!length(vals)) {
                terminal_reason <- "no_valid_envelope_support"
              } else {
                gates$ceiling_gate <- if (
                  !any(isi[gap] > bridge_ceiling)
                ) "pass" else "fail"
                if (!identical(gates$ceiling_gate, "pass")) {
                  terminal_reason <- "expanded_gap_above_bridge_ceiling"
                } else {
                  core_n <- as.integer(sum(
                    vals >= context$vp$seed_low &
                      vals <= context$vp$seed_high, na.rm = TRUE
                  ))
                  bridge_n <- as.integer(sum(
                    vals > context$vp$seed_high &
                      vals <= bridge_ceiling, na.rm = TRUE
                  ))
                  bridge_fraction <- as.numeric(bridge_n / length(vals))
                  expanded_n <- as.integer(sum(
                    isi[gap] > bridge_upper, na.rm = TRUE
                  ))
                  q90 <- as.numeric(stpd_event_core_quantile(vals, 0.90))
                  gates$core_gate <- if (
                    core_n >= context$vp$min_seed_isi_n
                  ) "pass" else "fail"
                  if (!identical(gates$core_gate, "pass")) {
                    terminal_reason <- "insufficient_seed_core_support"
                  } else {
                    gates$bridge_count_gate <- if (
                      bridge_n <= context$vp$max_bridge_n
                    ) "pass" else "fail"
                    if (!identical(gates$bridge_count_gate, "pass")) {
                      terminal_reason <- "bridge_count_exceeded"
                    } else {
                      gates$bridge_fraction_gate <- if (
                        bridge_fraction <= max_bridge_fraction
                      ) "pass" else "fail"
                      if (!identical(gates$bridge_fraction_gate, "pass")) {
                        terminal_reason <- "bridge_fraction_exceeded"
                      } else {
                        gates$expanded_bridge_gate <- if (
                          expanded_n == 1L
                        ) "pass" else "fail"
                        if (!identical(
                            gates$expanded_bridge_gate, "pass")) {
                          terminal_reason <- "expanded_bridge_count_not_one"
                        } else {
                          gates$q90_gate <- if (
                            is.finite(q90) && q90 <= bridge_ceiling
                          ) "pass" else "fail"
                          if (!identical(gates$q90_gate, "pass")) {
                            terminal_reason <- if (!is.finite(q90)) {
                              "q90_unavailable"
                            } else "q90_above_bridge_ceiling"
                          } else {
                            n_spikes <- e - s + 2L
                            label <- if (
                              n_spikes <= context$vp$classic_max_spikes
                            ) "burst" else "long_burst"
                            merged <- stpd_event_core_candidate_from_run(
                              context$dat, s, e, context$params, context$vp,
                              context$min_isi_sec, context$train,
                              "event_core_burst_bridge_merge",
                              "event_core_burst_bridge_merge", label,
                              "event_core_burst_bridge_merge_pass",
                              paste0(
                                "burst_bridge_ownership_resolved_before_",
                                "pause_generation"
                              ),
                              "accept", 20 + n_spikes, 1400,
                              list(
                                candidate_id = paste0(
                                  "event_core_bridge_merged_burst_", counter + 1L
                                ),
                                bridge_band_upper_sec = bridge_upper,
                                burst_bridge_merge_ceiling_sec = bridge_ceiling,
                                burst_bridge_merge_factor = bridge_factor,
                                burst_bridge_merge_gap_isi_n = length(gap),
                                burst_bridge_merge_expanded_bridge_n = expanded_n,
                                burst_bridge_merge_core_isi_n = core_n,
                                burst_bridge_merge_bridge_isi_n = bridge_n,
                                burst_bridge_merge_source_candidate_ids = paste(
                                  as.character(left$candidate_id[[1L]] %||% ""),
                                  as.character(right$candidate_id[[1L]] %||% ""),
                                  sep = ";"
                                ),
                                burst_bridge_merge_witness_candidate_ids = paste(
                                  as.character(witness$candidate_id %||% ""),
                                  collapse = ";"
                                )
                              )
                            )
                            gates$construction_gate <- if (
                              !is.null(merged) && nrow(merged) > 0L
                            ) "pass" else "fail"
                            if (identical(gates$construction_gate, "pass")) {
                              terminal_status <- "merge_emitted"
                              terminal_reason <- "ownership_merge_emitted"
                            } else {
                              terminal_reason <- "candidate_construction_failed"
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      witness_ids <- stpd_candidate_lineage_candidate_column(
        witness, "candidate_id", "character"
      )
      witness_hashes <- if (nrow(witness)) vapply(
        seq_len(nrow(witness)), function(j) {
          stpd_candidate_lineage_refractory_candidate_hash(
            witness[j, , drop = FALSE], "refractory_policy_output"
          )
        }, character(1)
      ) else character()
      attempt_ordinal <- length(attempts) + 1L
      if (identical(terminal_status, "merge_emitted")) {
        counter <- counter + 1L
        output_ordinal <- add_merged(
          merged, c(left_input, right_input), witness_input
        )
      } else {
        output_ordinal <- add_retained(left, left_input)
      }
      attempts[[attempt_ordinal]] <- data.frame(
        train = context$train, attempt_ordinal = as.integer(attempt_ordinal),
        left_atom_ordinal = as.integer(ii),
        right_atom_ordinal = as.integer(ii + 1L),
        left_source_input_ordinal = left_input,
        right_source_input_ordinal = right_input,
        left_candidate_id = stpd_candidate_lineage_candidate_column(
          left, "candidate_id", "character"
        )[[1L]],
        right_candidate_id = stpd_candidate_lineage_candidate_column(
          right, "candidate_id", "character"
        )[[1L]],
        left_payload_sha256 = stpd_candidate_lineage_refractory_candidate_hash(
          input[left_input, , drop = FALSE], "refractory_policy_output"
        ),
        right_payload_sha256 = stpd_candidate_lineage_refractory_candidate_hash(
          input[right_input, , drop = FALSE], "refractory_policy_output"
        ),
        proposed_detector_start_row = as.integer(s),
        proposed_detector_end_row = as.integer(e),
        overlapping_boundary_ordinals = paste(overlap, collapse = ";"),
        spanning_witness_n = as.integer(nrow(witness)),
        spanning_witness_input_ordinals = paste(witness_input, collapse = ";"),
        spanning_witness_candidate_ids = paste(witness_ids, collapse = ";"),
        spanning_witness_payload_sha256 =
          stpd_candidate_lineage_pause_ownership_set_hash(
            witness_input, witness_ids, witness_hashes,
            "stpd-burst-pause-ownership-witness-set-v1"
          ),
        gap_start_row = if (length(gap)) as.integer(gap[[1L]]) else NA_integer_,
        gap_end_row = if (length(gap)) as.integer(gap[[length(gap)]]) else
          NA_integer_,
        gap_isi_n = as.integer(length(gap)), gap_isi_sec = gap_value,
        gap_valid = as.logical(gap_valid), core_isi_n = as.integer(core_n),
        bridge_isi_n = as.integer(bridge_n),
        bridge_fraction = as.numeric(bridge_fraction),
        expanded_bridge_isi_n = as.integer(expanded_n), q90_sec = q90,
        boundary_gate = gates$boundary_gate,
        witness_gate = gates$witness_gate,
        gap_shape_gate = gates$gap_shape_gate,
        gap_validity_gate = gates$gap_validity_gate,
        support_gate = gates$support_gate, ceiling_gate = gates$ceiling_gate,
        core_gate = gates$core_gate,
        bridge_count_gate = gates$bridge_count_gate,
        bridge_fraction_gate = gates$bridge_fraction_gate,
        expanded_bridge_gate = gates$expanded_bridge_gate,
        q90_gate = gates$q90_gate, construction_gate = gates$construction_gate,
        terminal_status = terminal_status, terminal_reason = terminal_reason,
        redetection_performed = FALSE, output_ordinal = as.integer(output_ordinal),
        output_candidate_id = "", output_payload_sha256 = "",
        stringsAsFactors = FALSE, check.names = FALSE
      )
      ii <- if (identical(terminal_status, "merge_emitted")) ii + 2L else ii + 1L
    }
    replayed_out <- dplyr::bind_rows(next_rows)
    replayed_out$selected_for_auto <- TRUE
    replayed_out$selection_status <-
      "burst_bridge_ownership_frozen_before_pause"
  }

  if (!identical(replayed_out, context$scientific_out)) return(NULL)

  if (length(attempts)) {
    attempts <- dplyr::bind_rows(attempts)
    for (i in seq_len(nrow(attempts))) {
      oi <- attempts$output_ordinal[[i]]
      target <- replayed_out[oi, , drop = FALSE]
      attempts$output_candidate_id[[i]] <-
        stpd_candidate_lineage_candidate_column(
          target, "candidate_id", "character"
        )[[1L]]
      attempts$output_payload_sha256[[i]] <-
        stpd_candidate_lineage_pause_ownership_candidate_hash(target, "output")
    }
    attempts <- attempts[, names(stpd_candidate_lineage_observation_hook_schema(
      "burst_pause_ownership_attempts_v1"
    )), drop = FALSE]
  } else {
    attempts <- stpd_candidate_lineage_empty_hook_payload(
      "burst_pause_ownership_attempts_v1"
    )
  }

  if (!nrow(replayed_out)) {
    output <- stpd_candidate_lineage_empty_hook_payload(
      "burst_pause_ownership_output_v1"
    )
  } else {
    output_rows <- lapply(seq_len(nrow(replayed_out)), function(i) {
      target <- replayed_out[i, , drop = FALSE]
      info <- output_info[[i]]
      source_ord <- as.integer(info$source_input_ordinals)
      source_rows <- input[source_ord, , drop = FALSE]
      source_ids <- stpd_candidate_lineage_candidate_column(
        source_rows, "candidate_id", "character"
      )
      source_hashes <- vapply(seq_len(nrow(source_rows)), function(j) {
        stpd_candidate_lineage_refractory_candidate_hash(
          source_rows[j, , drop = FALSE], "refractory_policy_output"
        )
      }, character(1))
      witness_ord <- as.integer(info$witness_input_ordinals)
      witness_rows <- input[witness_ord, , drop = FALSE]
      witness_ids <- stpd_candidate_lineage_candidate_column(
        witness_rows, "candidate_id", "character"
      )
      witness_hashes <- if (nrow(witness_rows)) vapply(
        seq_len(nrow(witness_rows)), function(j) {
          stpd_candidate_lineage_refractory_candidate_hash(
            witness_rows[j, , drop = FALSE], "refractory_policy_output"
          )
        }, character(1)
      ) else character()
      start_row <- stpd_candidate_lineage_candidate_column(
        target, "start_isi", "integer"
      )[[1L]]
      end_row <- stpd_candidate_lineage_candidate_column(
        target, "end_isi", "integer"
      )[[1L]]
      data.frame(
        train = context$train, output_ordinal = as.integer(i),
        relation = as.character(info$relation),
        source_input_ordinals = paste(source_ord, collapse = ";"),
        source_candidate_ids = paste(source_ids, collapse = ";"),
        source_payload_sha256 =
          stpd_candidate_lineage_pause_ownership_set_hash(
            source_ord, source_ids, source_hashes,
            "stpd-burst-pause-ownership-source-set-v1"
          ),
        witness_input_ordinals = paste(witness_ord, collapse = ";"),
        witness_candidate_ids = paste(witness_ids, collapse = ";"),
        witness_payload_sha256 =
          stpd_candidate_lineage_pause_ownership_set_hash(
            witness_ord, witness_ids, witness_hashes,
            "stpd-burst-pause-ownership-output-witness-set-v1"
          ),
        output_candidate_id = stpd_candidate_lineage_candidate_column(
          target, "candidate_id", "character"
        )[[1L]],
        output_payload_sha256 =
          stpd_candidate_lineage_pause_ownership_candidate_hash(target, "output"),
        detector_start_row = start_row, detector_end_row = end_row,
        start_isi = as.integer(start_row - 1L),
        end_isi = as.integer(end_row - 1L),
        final_label = stpd_candidate_lineage_candidate_column(
          target, "final_label", "character"
        )[[1L]],
        action = rep_len(as.character(
          target$action %||% target$decision_action %||% ""
        ), 1L)[[1L]],
        selected_for_auto = stpd_candidate_lineage_candidate_column(
          target, "selected_for_auto", "logical"
        )[[1L]],
        selection_status = stpd_candidate_lineage_candidate_column(
          target, "selection_status", "character"
        )[[1L]],
        ownership_role = "pause_ownership_support_only",
        event_materialization_status = "not_materialized_by_this_path",
        stringsAsFactors = FALSE, check.names = FALSE
      )
    })
    output <- dplyr::bind_rows(output_rows)
    output <- output[, names(stpd_candidate_lineage_observation_hook_schema(
      "burst_pause_ownership_output_v1"
    )), drop = FALSE]
  }

  pair_attempt_n <- nrow(attempts)
  success_n <- if (pair_attempt_n) {
    sum(attempts$terminal_status == "merge_emitted")
  } else 0L
  receipt <- data.frame(
    train = context$train, burst_pipeline_id = "final",
    applicability_status = applicability,
    input_candidate_n = as.integer(nrow(input)),
    eligible_candidate_n = as.integer(nrow(accepted_pool)),
    selector_winner_n = as.integer(nrow(atoms)),
    selector_loser_n = as.integer(nrow(accepted_pool) - nrow(atoms)),
    pair_attempt_n = as.integer(pair_attempt_n),
    successful_merge_n = as.integer(success_n),
    failed_merge_n = as.integer(pair_attempt_n - success_n),
    retained_atom_n = as.integer(sum(
      output$relation == "retained_ownership_atom"
    )),
    lone_tail_n = as.integer(lone_tail_n),
    output_support_n = as.integer(nrow(output)), redetection_n = 0L,
    one_pass_control_path_exhausted = identical(
      applicability, "invoked_one_pass_pair_scan"
    ),
    cap_applied = FALSE,
    evidence_origin = evidence_origin,
    input_scope = if (isTRUE(context$burst_family_requested)) {
      "ordinary_post_refractory_burst_candidates"
    } else "typed_empty_burst_family_not_requested",
    upstream_boundary_status =
      "not_applicable_ownership_precedes_pause_boundary",
    upstream_pause_status =
      "not_applicable_ownership_precedes_contextual_pause",
    upstream_universe_status = "candidate_universe_unavailable",
    output_role = "pause_ownership_support_only",
    coverage_status = switch(
      applicability,
      not_invoked_empty_input = "not_invoked_empty_input",
      invoked_no_eligible_candidates =
        "ownership_helper_control_path_exhausted_no_eligible_candidates",
      invoked_fewer_than_two_selected_atoms =
        "ownership_helper_control_path_exhausted_fewer_than_two_atoms",
      invoked_invalid_bridge_upper =
        "ownership_helper_control_path_exhausted_invalid_bridge_upper",
      invoked_one_pass_pair_scan = paste0(
        "greedy_one_pass_control_path_exhausted_over_",
        "frozen_selected_atoms"
      )
    ),
    entry_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-pause-ownership-entry-v1", entry
    ),
    eligibility_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-pause-ownership-eligibility-v1", eligibility
    ),
    selection_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-pause-ownership-selection-v1", selection$payload
    ),
    attempt_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-pause-ownership-attempts-v1", attempts
    ),
    output_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-pause-ownership-output-v1", output
    ),
    scientific_output_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-pause-ownership-scientific-output-v1", replayed_out
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  list(
    entry = entry, eligibility = eligibility,
    selection = selection$payload, attempts = attempts,
    output = output, receipt = receipt
  )
}

stpd_candidate_lineage_capture_burst_pause_ownership <- function(
    train_collector, train, input_candidates, scientific_out,
    dat, params, vp, min_isi_sec, boundaries) {
  if (is.null(train_collector)) return(invisible(NULL))
  train <- as.character(train)
  if (length(train) != 1L || is.na(train) || !nzchar(train)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      "Burst--Pause ownership observation requires one active train.",
      field = "train"
    )
  }
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", train = train,
    validate_observations = FALSE
  )
  input_candidates <- as.data.frame(
    input_candidates %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  boundaries <- as.data.frame(
    boundaries %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!stpd_candidate_lineage_pause_ownership_upstream_is_closed(
      train_collector, input_candidates, boundaries)) {
    stpd_candidate_lineage_abort(
      "collector_pause_ownership_upstream_not_closed",
      paste(
        "Burst--Pause ownership input does not close to the exact",
        "ordinary post-refractory Burst output with an empty boundary set."
      )
    )
  }
  context <- list(
    train = train, input_candidates = input_candidates,
    scientific_out = as.data.frame(
      scientific_out %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    dat = as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE),
    params = params, vp = vp, min_isi_sec = as.numeric(min_isi_sec)[[1L]],
    boundaries = boundaries,
    burst_family_requested = {
      raw_context <- get(
        "pause_raw_canonical_context", envir = train_collector,
        inherits = FALSE
      )
      any(c("burst", "long_burst") %in%
            as.character(raw_context$patterns %||% character()))
    }
  )
  if (!stpd_candidate_lineage_pause_ownership_science_context_is_closed(
      train_collector, context)) {
    stpd_candidate_lineage_abort(
      "collector_pause_ownership_science_context_not_closed",
      paste(
        "Burst--Pause ownership data and parameters do not close",
        "to the exact refractory scientific context."
      )
    )
  }
  replay <- stpd_candidate_lineage_pause_ownership_replay(context)
  if (is.null(replay)) {
    stpd_candidate_lineage_abort(
      "collector_pause_ownership_scientific_replay_failed",
      "Burst--Pause ownership output does not replay exactly."
    )
  }
  train_collector$burst_pause_ownership_context <- context
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_ownership_entry_v1", replay$entry,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_ownership_eligibility_v1",
    replay$eligibility, validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_ownership_selection_v1", replay$selection,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_ownership_attempts_v1", replay$attempts,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_ownership_output_v1", replay$output,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_ownership_receipt_v1", replay$receipt,
    validate_payload = FALSE
  )
  invisible(replay)
}

stpd_candidate_lineage_pause_ownership_payload_is_valid <- function(
    train_collector, hook_id, payload) {
  context <- if (exists(
      "burst_pause_ownership_context", envir = train_collector,
      inherits = FALSE)) {
    train_collector$burst_pause_ownership_context
  } else NULL
  if (is.null(context) || !is.list(context) ||
      !is.character(context$train) || length(context$train) != 1L ||
      is.na(context$train) || !nzchar(context$train) ||
      !identical(context$train, train_collector$train) ||
      !("train" %in% names(payload)) || anyNA(payload$train) ||
      !all(payload$train == train_collector$train) ||
      !stpd_candidate_lineage_pause_ownership_upstream_is_closed(
        train_collector, context$input_candidates, context$boundaries
      ) ||
      !stpd_candidate_lineage_pause_ownership_science_context_is_closed(
        train_collector, context
      )) {
    return(FALSE)
  }
  replay <- stpd_candidate_lineage_pause_ownership_replay(context)
  if (is.null(replay)) return(FALSE)
  hook_map <- c(
    burst_pause_ownership_entry_v1 = "entry",
    burst_pause_ownership_eligibility_v1 = "eligibility",
    burst_pause_ownership_selection_v1 = "selection",
    burst_pause_ownership_attempts_v1 = "attempts",
    burst_pause_ownership_output_v1 = "output",
    burst_pause_ownership_receipt_v1 = "receipt"
  )
  key <- unname(hook_map[[hook_id]])
  if (is.null(key) || !identical(payload, replay[[key]])) return(FALSE)
  prior <- switch(
    hook_id,
    burst_pause_ownership_entry_v1 = character(),
    burst_pause_ownership_eligibility_v1 =
      "burst_pause_ownership_entry_v1",
    burst_pause_ownership_selection_v1 = c(
      "burst_pause_ownership_entry_v1",
      "burst_pause_ownership_eligibility_v1"
    ),
    burst_pause_ownership_attempts_v1 = c(
      "burst_pause_ownership_entry_v1",
      "burst_pause_ownership_eligibility_v1",
      "burst_pause_ownership_selection_v1"
    ),
    burst_pause_ownership_output_v1 = c(
      "burst_pause_ownership_entry_v1",
      "burst_pause_ownership_eligibility_v1",
      "burst_pause_ownership_selection_v1",
      "burst_pause_ownership_attempts_v1"
    ),
    burst_pause_ownership_receipt_v1 = names(hook_map)[1:5],
    character()
  )
  if (!length(prior)) return(TRUE)
  all(vapply(prior, function(prior_hook) {
    prior_key <- unname(hook_map[[prior_hook]])
    stored <- train_collector$observations[[prior_hook]]
    !is.null(stored) && identical(stored, replay[[prior_key]])
  }, logical(1)))
}
