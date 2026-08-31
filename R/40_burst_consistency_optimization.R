# ============================================================
# event grammar burst-consistency optimization layer
# ============================================================
# This layer is loaded after the event grammar threshold-resolved detector and keeps the
# current event grammar architecture. It optimizes the burst core in three ways:
#   1. q95 bridge checking becomes a soft penalty by default, not a hard veto.
#   2. one-sided / edge-limited candidates with clean burst core are retained as
#      high-confidence possible_burst unless the user explicitly promotes them.
#   3. possible_burst receives dynamic interval-selection weight, so visually
#      similar burst-like clusters are not silently lost to pause/tonic layers.

stpd_event_grammar_bool <- function(x, default = FALSE) {
  if (length(x) == 0 || is.null(x)) return(isTRUE(default))
  isTRUE(x)
}

stpd_event_grammar_safe_col <- function(row, nm, default = NA_real_) {
  if (is.null(row) || nrow(row) == 0 || !(nm %in% names(row))) return(default)
  row[[nm]][1]
}

# Dynamic possible_burst value function retained as a named internal implementation.
stpd_event_core_candidate_value_possible_burst_dynamic <- function(row) {
  lab <- as.character(stpd_event_grammar_safe_col(row, "final_label", ""))
  sc <- suppressWarnings(as.numeric(stpd_event_grammar_safe_col(row, "score", 0)))
  if (!is.finite(sc)) sc <- 0
  n_isi <- suppressWarnings(as.numeric(stpd_event_grammar_safe_col(row, "n_isi", 0)))
  if (!is.finite(n_isi)) n_isi <- 0
  layer <- as.character(stpd_event_grammar_safe_col(row, "candidate_layer", ""))

  if (lab %in% c("burst", "long_burst") && identical(layer, "event_grammar_burst_episode")) {
    base <- if (identical(lab, "burst")) 1000000 else 900000
    return(base + 220000 * n_isi + 100 * sc)
  }

  if (identical(lab, "possible_burst")) {
    pri <- 260000
    if (grepl("burst", layer, ignore.case = TRUE)) pri <- 300000

    one <- stpd_event_grammar_bool(stpd_event_grammar_safe_col(row, "one_sided_boundary_pass", FALSE), FALSE)
    poss <- stpd_event_grammar_bool(stpd_event_grammar_safe_col(row, "possible_boundary_pass", FALSE), FALSE)
    q90p <- stpd_event_grammar_bool(stpd_event_grammar_safe_col(row, "q90_bridge_pass", FALSE), FALSE)
    q95p <- stpd_event_grammar_bool(stpd_event_grammar_safe_col(row, "q95_bridge_pass", TRUE), TRUE)
    sp <- suppressWarnings(as.numeric(stpd_event_grammar_safe_col(row, "seed_purity", NA_real_)))
    bf <- suppressWarnings(as.numeric(stpd_event_grammar_safe_col(row, "bridge_fraction", NA_real_)))
    contrast <- suppressWarnings(as.numeric(stpd_event_grammar_safe_col(row, "burst_contrast_score", NA_real_)))
    req <- suppressWarnings(as.numeric(stpd_event_grammar_safe_col(row, "burst_contrast_required", NA_real_)))

    if (isTRUE(poss)) pri <- pri + 80000
    if (isTRUE(one)) pri <- pri + 60000
    if (isTRUE(q90p)) pri <- pri + 40000
    if (is.finite(sp)) pri <- pri + 50000 * max(0, min(1, sp))
    if (is.finite(contrast) && is.finite(req) && req > 0) pri <- pri + 60000 * max(0, min(1, contrast / req))
    if (!isTRUE(q95p)) pri <- pri - 25000
    if (is.finite(bf)) pri <- pri - 45000 * max(0, min(1, bf))
    pri <- max(220000, min(480000, pri))  # below canonical HF spiking, above tonic/pause.
    return(pri + 100 * sc + n_isi)
  }

  pri <- switch(lab,
    burst = 1000000,
    long_burst = 900000,
    high_frequency_spiking = 500000,
    high_frequency_tonic = 250000,
    tonic = 160000,
    pause = 90000,
    0
  )
  pri + 100 * sc + n_isi
}

# Optimized event grammar burst detector kept as a named internal implementation.
# HF spiking/HF tonic/tonic/pause layers remain the existing event core/event grammar
# state detectors.
stpd_event_grammar_detect_burst_events_consistency_optimized <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); rows <- list()
  if (n <= 2) return(data.frame())

  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE

  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  runs <- stpd_event_grammar_bool_runs(seed_flag)
  if (nrow(runs) == 0) return(data.frame())

  eg <- params$event_grammar %||% list()
  strict_q95 <- stpd_event_grammar_bool(eg$strict_q95_bridge_gate, FALSE)
  one_sided_as_canonical <- stpd_event_grammar_bool(eg$allow_one_sided_burst_as_canonical, FALSE)
  one_sided_S <- stpd_event_grammar_num(eg$one_sided_burst_contrast_min %||% (vp$S + 0.5), vp$S + 0.5)
  seed_purity_min <- stpd_event_grammar_num(eg$one_sided_seed_purity_min %||% 0.65, 0.65)
  q95_penalty_weight <- stpd_event_grammar_num(eg$q95_soft_penalty_weight %||% 0.35, 0.35)

  seen <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    ss <- as.integer(runs$start_isi[rr]); ee <- as.integer(runs$end_isi[rr])
    if (sum(seed_flag[ss:ee], na.rm = TRUE) < vp$min_seed_isi_n) next

    lefts <- stpd_event_core_left_extensions(ss, isi, valid, vp$bridge_high, vp$max_expand)
    rights <- stpd_event_core_right_extensions(ee, n, isi, valid, vp$bridge_high, vp$max_expand)

    for (s in lefts) for (e in rights) {
      if (length(rows) >= vp$max_candidates) break
      key <- paste0(s, "_", e)
      if (!is.null(seen[[key]])) next
      seen[[key]] <- TRUE

      idx <- stpd_event_grammar_safe_seq(s, e)
      if (length(idx) == 0) next
      core_n <- sum(seed_flag[idx], na.rm = TRUE)
      if (core_n < vp$min_seed_isi_n) next

      n_spikes <- e - s + 2L
      if (n_spikes < vp$min_spikes) next

      m <- stpd_event_core_span_metrics(dat, s, e, params, vp, min_isi_sec, train, "event_grammar_burst_event")
      if (is.null(m)) next

      n_valid <- suppressWarnings(as.numeric(m$n_valid_isi[1] %||% NA_real_))
      if (!is.finite(n_valid) || n_valid <= 0) n_valid <- length(idx)
      seed_purity <- core_n / max(1, n_valid)

      bridge_count <- suppressWarnings(as.numeric(m$bridge_isi_count[1] %||% 0))
      bridge_fraction <- suppressWarnings(as.numeric(m$bridge_fraction[1] %||% 0))
      bridge_count_pass <- is.finite(bridge_count) && bridge_count <= vp$max_bridge_n
      bridge_fraction_pass <- is.finite(bridge_fraction) && bridge_fraction <= vp$max_bridge_frac

      intra_q90 <- suppressWarnings(as.numeric(m$intra_q90_sec[1] %||% NA_real_))
      intra_q95 <- suppressWarnings(as.numeric(m$intra_q95_sec[1] %||% NA_real_))
      q90_pass <- is.finite(intra_q90) && intra_q90 <= vp$bridge_high
      q95_pass <- is.finite(intra_q95) && intra_q95 <= vp$bridge_high
      q95_ratio <- if (is.finite(intra_q95) && is.finite(vp$bridge_high) && vp$bridge_high > 0) intra_q95 / vp$bridge_high else NA_real_
      q95_soft_penalty <- if (is.finite(q95_ratio)) max(0, q95_ratio - 1) else 0
      q95_hard_pass <- if (strict_q95) q95_pass else TRUE

      pre_ratio <- if (is.finite(m$pre_gap_sec[1]) && is.finite(intra_q90) && intra_q90 > 0) m$pre_gap_sec[1] / intra_q90 else NA_real_
      post_ratio <- if (is.finite(m$post_gap_sec[1]) && is.finite(intra_q90) && intra_q90 > 0) m$post_gap_sec[1] / intra_q90 else NA_real_
      two_sided <- is.finite(pre_ratio) && is.finite(post_ratio) && pre_ratio >= vp$S && post_ratio >= vp$S
      two_possible <- is.finite(pre_ratio) && is.finite(post_ratio) && pre_ratio >= vp$S_possible && post_ratio >= vp$S_possible
      one_sided <- (is.finite(pre_ratio) && pre_ratio >= one_sided_S) || (is.finite(post_ratio) && post_ratio >= one_sided_S)
      edge_limited <- (!is.finite(pre_ratio) || !is.finite(post_ratio)) && one_sided
      clean_one_sided <- isTRUE(one_sided) && q90_pass && q95_hard_pass && seed_purity >= seed_purity_min && bridge_fraction <= min(vp$max_bridge_frac, 0.45)

      neg <- isTRUE(m$manual_negative_veto[1])
      size_label <- "prolonged_burst_like"
      if (n_spikes <= vp$classic_max_spikes) size_label <- "burst"
      else if (n_spikes >= vp$long_min_spikes && (vp$long_max_spikes <= 0 || n_spikes <= vp$long_max_spikes)) size_label <- "long_burst"

      final <- "reject"; status <- "event_grammar_reject"; action <- "reject"; decision <- "event_grammar_reject"; priority <- 0
      base_ok <- !neg && bridge_count_pass && bridge_fraction_pass && q90_pass && q95_hard_pass

      if (base_ok && two_sided) {
        if (size_label %in% c("burst", "long_burst")) {
          final <- size_label
          status <- if (q95_pass) "event_grammar_two_sided_burst_event_pass" else "event_grammar_two_sided_burst_event_pass_q95_soft_penalty"
          action <- "accept"
          decision <- paste0(status, "__", size_label)
          priority <- if (final == "burst") 1200 else 1120
          if (!q95_pass) priority <- priority - 80
        } else {
          final <- "possible_burst"; status <- "event_grammar_prolonged_burst_like_review"; action <- "demote_to_possible"
          decision <- "two_sided_structure_but_spike_count_exceeds_long_burst_range"; priority <- 260
        }
      } else if (base_ok && clean_one_sided) {
        if (one_sided_as_canonical && size_label %in% c("burst", "long_burst")) {
          final <- size_label; status <- "event_grammar_clean_one_sided_burst_event_pass_user_allowed"; action <- "accept"
          decision <- paste0("clean_one_sided_event_grammar_pass__", size_label); priority <- if (final == "burst") 980 else 930
        } else {
          final <- "possible_burst"; status <- if (edge_limited) "event_grammar_clean_edge_limited_possible_burst" else "event_grammar_clean_one_sided_possible_burst"
          action <- "demote_to_possible"; decision <- "clean_one_sided_flank_contrast_pass_q90_core_pass_q95_soft"; priority <- 320
        }
      } else if (base_ok && two_possible) {
        final <- "possible_burst"; status <- "event_grammar_possible_two_sided_burst"; action <- "demote_to_possible"
        decision <- "two_sided_possible_contrast_pass"; priority <- 280
      } else {
        reasons <- c(
          if (neg) "manual_negative_veto",
          if (!bridge_count_pass) "too_many_bridge_isis",
          if (!bridge_fraction_pass) "bridge_fraction_too_high",
          if (!q90_pass) "intra_q90_exceeds_bridge_band",
          if (strict_q95 && !q95_pass) "intra_q95_exceeds_bridge_band_strict",
          if (!two_sided && !one_sided && !two_possible) "flank_contrast_fail"
        )
        decision <- paste(reasons, collapse = ";")
        if (!nzchar(decision)) decision <- "event_grammar_reject"
      }

      contrast <- suppressWarnings(as.numeric(m$burst_contrast_score[1] %||% 0)); if (!is.finite(contrast)) contrast <- 0
      score <- contrast + 0.18 * core_n - 0.20 * bridge_count - 0.45 * bridge_fraction + 0.35 * seed_purity - q95_penalty_weight * q95_soft_penalty

      counter <- counter + 1L
      rows[[length(rows) + 1L]] <- stpd_event_core_candidate_row(m, "event_grammar_burst_event", "event_grammar_seed_centered_burst", final, status, decision, action, score, priority,
        list(candidate_id = paste0("event_grammar_burst_opt_", counter), seed_run_start_isi = ss, seed_run_end_isi = ee,
             seed_band_lower_sec = vp$seed_low, seed_band_upper_sec = vp$seed_high, bridge_band_upper_sec = vp$bridge_high,
             burst_contrast_required = vp$S, one_sided_contrast_required = one_sided_S, possible_contrast_required = vp$S_possible,
             pre_ratio_q90 = pre_ratio, post_ratio_q90 = post_ratio,
             boundary_type = if (two_sided) "two_sided" else if (clean_one_sided) if (edge_limited) "edge_limited_clean_one_sided" else "clean_one_sided" else if (one_sided) "one_sided_weak_core" else "failed",
             strict_boundary_pass = two_sided, one_sided_boundary_pass = clean_one_sided, possible_boundary_pass = two_possible,
             bridge_count_pass = bridge_count_pass, bridge_fraction_pass = bridge_fraction_pass,
             q90_bridge_pass = q90_pass, q95_bridge_pass = q95_pass, q95_bridge_gate_mode = if (strict_q95) "hard" else "soft_penalty",
             q95_soft_penalty = q95_soft_penalty, seed_purity = seed_purity, failure_reason = if (final == "reject") decision else "",
             candidate_diagnostic_class = if (final == "reject") paste0("rejected__", decision) else paste0(final, "__", status),
             threshold_source_summary = paste0("seed=", vp$threshold_table$source[vp$threshold_table$pattern=="burst" & vp$threshold_table$field=="seed_upper_sec"][1] %||% "")))
    }
    if (length(rows) >= vp$max_candidates) break
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

# ============================================================
# event grammar final burst implementation after consistency layer
# ============================================================
# The final implementation below is selected explicitly by
# stpd_event_grammar_detect_burst_events().

stpd_event_grammar_possible_priority <- function(m, vp, two_possible = FALSE, one_sided = FALSE, q95_excess_ratio = 1) {
  contrast <- stpd_event_grammar_num(m$burst_contrast_score[1], 0)
  core_n <- stpd_event_grammar_num(m$core_isi_count[1], 0)
  bridge_frac <- stpd_event_grammar_num(m$bridge_fraction[1], 1)
  seed_purity <- stpd_event_grammar_num(m$seed_purity[1], NA_real_)
  if (!is.finite(seed_purity)) seed_purity <- core_n / max(1, stpd_event_grammar_num(m$n_valid_isi[1], 1))
  closeness <- if (is.finite(vp$S) && vp$S > 0) min(1.5, contrast / vp$S) else 0
  pri <- 260 + 180 * closeness + 120 * min(1, seed_purity) + 20 * min(5, core_n)
  if (two_possible) pri <- pri + 80
  if (one_sided) pri <- pri + 50
  if (is.finite(q95_excess_ratio) && q95_excess_ratio > 1) pri <- pri - 60 * min(2, q95_excess_ratio - 1)
  if (is.finite(bridge_frac)) pri <- pri - 120 * max(0, bridge_frac - 0.35)
  max(180, min(760, pri))
}

stpd_event_core_candidate_value_explicit_priority <- function(row) {
  explicit <- stpd_event_grammar_num(row$priority[1], NA_real_)
  lab <- as.character(row$final_label[1] %||% "")
  if (is.finite(explicit) && explicit > 0) {
    sc <- stpd_event_grammar_num(row$score[1], 0)
    n_isi <- stpd_event_grammar_num(row$n_isi[1], 0)
    return(explicit * 10000 + 100 * sc + n_isi)
  }
  pri <- switch(lab,
    burst = 1200,
    long_burst = 1120,
    high_frequency_spiking = 700,
    high_frequency_tonic = 560,
    tonic = 420,
    pause = 320,
    possible_burst = 280,
    0
  )
  sc <- stpd_event_grammar_num(row$score[1], 0)
  n_isi <- stpd_event_grammar_num(row$n_isi[1], 0)
  pri * 10000 + 100 * sc + n_isi
}

stpd_event_grammar_burst_subtype_from_spike_count <- function(n_spikes, vp, review_label = "possible_burst") {
  stpd_event_core_burst_subtype_from_spike_count(n_spikes, vp, review_label = review_label)
}

# Candidate-envelope guard for one conspicuously large internal ISI.  The
# relative limit is not an absolute Burst threshold: a value above the ratio
# limit is retained when it still lies inside the frozen Burst-left support.
# Conversely, an out-of-band value may not be hidden by an inflated bridge
# band.  Only one deterministic maximum is removed when estimating the local
# core, so tied maxima remain visible in the reference distribution.
stpd_event_grammar_burst_internal_gap_guard <- function(
    values, burst_left_upper, ratio_max = 3.5, min_isi_n = 4L) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values) & values > 0]
  min_isi_n <- max(2L, stpd_event_grammar_int(min_isi_n, 4L))
  ratio_max <- stpd_event_grammar_num(ratio_max, 3.5)
  if (!is.finite(ratio_max) || ratio_max <= 1) ratio_max <- 3.5

  empty <- list(
    pass = TRUE,
    reason = "insufficient_support_for_internal_gap_guard",
    evaluated = FALSE,
    max_isi_sec = if (length(values)) max(values) else NA_real_,
    reference_median_sec = NA_real_,
    reference_mean_sec = NA_real_,
    max_to_median_ratio = NA_real_,
    max_to_mean_ratio = NA_real_,
    ratio_max = ratio_max,
    burst_left_upper_sec = burst_left_upper,
    ratio_pass = NA,
    left_band_rescue = NA
  )
  if (length(values) < min_isi_n) return(empty)

  max_pos <- which.max(values)[1L]
  max_isi <- values[max_pos]
  reference <- values[-max_pos]
  reference_median <- stats::median(reference)
  reference_mean <- mean(reference)
  median_ratio <- if (is.finite(reference_median) && reference_median > 0) {
    max_isi / reference_median
  } else NA_real_
  mean_ratio <- if (is.finite(reference_mean) && reference_mean > 0) {
    max_isi / reference_mean
  } else NA_real_
  ratio_pass <- is.finite(median_ratio) && median_ratio <= ratio_max
  left_band_rescue <- is.finite(burst_left_upper) &&
    max_isi <= burst_left_upper
  pass <- isTRUE(ratio_pass) || isTRUE(left_band_rescue)

  list(
    pass = pass,
    reason = if (isTRUE(ratio_pass)) {
      "internal_gap_ratio_pass"
    } else if (isTRUE(left_band_rescue)) {
      "internal_gap_left_band_rescue"
    } else {
      "internal_gap_exceeds_ratio_and_burst_left_band"
    },
    evaluated = TRUE,
    max_isi_sec = max_isi,
    reference_median_sec = reference_median,
    reference_mean_sec = reference_mean,
    max_to_median_ratio = median_ratio,
    max_to_mean_ratio = mean_ratio,
    ratio_max = ratio_max,
    burst_left_upper_sec = burst_left_upper,
    ratio_pass = ratio_pass,
    left_band_rescue = left_band_rescue
  )
}

stpd_event_grammar_burst_extension_guard <- function(
    values, native_bridge_upper, candidate_bridge_upper, n_spikes, vp,
    params) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  eg <- params$event_grammar %||% list()
  native_fraction_min <- stpd_event_grammar_num(
    eg$burst_extension_native_support_fraction_min %||% 0.55, 0.55
  )
  native_fraction_min <- min(1, max(0.50, native_fraction_min))
  borrowed_run_max <- max(1L, stpd_event_grammar_int(
    eg$burst_extension_max_consecutive_borrowed_isi %||% 2L, 2L
  ))
  size_ceiling <- stpd_event_grammar_int(vp$long_max_spikes %||% 15L, 15L)
  proposal_active <- is.finite(candidate_bridge_upper) &&
    is.finite(native_bridge_upper) &&
    candidate_bridge_upper > native_bridge_upper
  if (!length(values) || !proposal_active) {
    return(list(
      pass = TRUE, reason = "no_boundary_extension",
      native_n = length(values), borrowed_n = 0L,
      native_fraction = 1, max_consecutive_borrowed = 0L,
      size_pass = TRUE
    ))
  }
  native <- values <= native_bridge_upper
  borrowed <- values > native_bridge_upper &
    values <= candidate_bridge_upper
  borrowed_runs <- stpd_event_grammar_bool_runs(borrowed)
  max_borrowed_run <- if (nrow(borrowed_runs)) {
    max(borrowed_runs$end_isi - borrowed_runs$start_isi + 1L)
  } else 0L
  native_n <- sum(native)
  borrowed_n <- sum(borrowed)
  native_fraction <- native_n / length(values)
  # The spike ceiling is an intrusion guard for threshold-borrowed expansion,
  # not an absolute definition of Burst.  A fully native compact episode has
  # no borrowed invasion point and remains reviewable as possible_burst; when
  # borrowing actually enlarges the support, crossing the ceiling rolls the
  # candidate back instead of accepting the invaded State tail.
  size_pass <- borrowed_n == 0L || size_ceiling <= 0L ||
    n_spikes <= size_ceiling
  fraction_pass <- native_fraction >= native_fraction_min
  run_pass <- max_borrowed_run <= borrowed_run_max
  reasons <- c(
    if (!size_pass) "candidate_exceeds_burst_spike_ceiling",
    if (!fraction_pass) "borrowed_support_dominates_native_burst_support",
    if (!run_pass) "sustained_borrowed_run_indicates_adjacent_state"
  )
  list(
    pass = size_pass && fraction_pass && run_pass,
    reason = if (length(reasons)) paste(reasons, collapse = ";") else
      "candidate_level_extension_guard_pass",
    native_n = native_n,
    borrowed_n = borrowed_n,
    native_fraction = native_fraction,
    native_fraction_min = native_fraction_min,
    max_consecutive_borrowed = max_borrowed_run,
    max_consecutive_borrowed_allowed = borrowed_run_max,
    size_pass = size_pass
  )
}

stpd_event_grammar_burst_intrusion_mask <- function(
    isi, valid, native_bridge_upper, candidate_bridge_upper,
    max_consecutive_borrowed = 2L) {
  isi <- suppressWarnings(as.numeric(isi))
  valid <- rep_len(as.logical(valid), length(isi))
  mask <- rep(FALSE, length(isi))
  if (!length(isi) || !is.finite(native_bridge_upper) ||
      !is.finite(candidate_bridge_upper) ||
      candidate_bridge_upper <= native_bridge_upper) {
    return(mask)
  }
  max_consecutive_borrowed <- max(
    1L, suppressWarnings(as.integer(max_consecutive_borrowed))[1L]
  )
  borrowed <- valid & is.finite(isi) & isi > native_bridge_upper &
    isi <= candidate_bridge_upper
  runs <- stpd_event_grammar_bool_runs(borrowed)
  if (!nrow(runs)) return(mask)
  sustained <- runs[
    runs$end_isi - runs$start_isi + 1L > max_consecutive_borrowed,
    , drop = FALSE
  ]
  if (!nrow(sustained)) return(mask)
  for (ii in seq_len(nrow(sustained))) {
    # Mark the complete sustained run, including its first borrowed ISI.  This
    # is deliberate rollback-to-onset semantics, not stop-after-detection.
    mask[seq.int(sustained$start_isi[ii], sustained$end_isi[ii])] <- TRUE
  }
  mask
}

stpd_event_grammar_detect_burst_events_final_impl <- function(
    dat, params, vp, min_isi_sec = 0.001, train = "",
    candidate_lineage_collector = NULL) {
  n <- nrow(dat); rows <- list()
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_collector_capture(
      candidate_lineage_collector,
      "burst_detector_final_entry_v1",
      data.frame(
        train = as.character(train)[1L],
        burst_pipeline_id = "final",
        stringsAsFactors = FALSE, check.names = FALSE
      )
    )
  }
  if (n <= 2) {
    if (!is.null(candidate_lineage_collector)) {
      stpd_candidate_lineage_collector_capture(
        candidate_lineage_collector,
        "burst_raw_threshold_support_v1",
        stpd_candidate_lineage_empty_hook_payload(
          "burst_raw_threshold_support_v1"
        )
      )
      stpd_candidate_lineage_collector_capture(
        candidate_lineage_collector,
        "burst_seed_runs_v1",
        stpd_candidate_lineage_empty_hook_payload("burst_seed_runs_v1")
      )
      stpd_candidate_lineage_stage4_short_receipt(
        candidate_lineage_collector, train, n, vp, params
      )
    }
    return(data.frame())
  }
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  if (!is.null(candidate_lineage_collector)) {
    support_rows <- seq.int(2L, n)
    native_bridge_extension_eligible <- valid & is.finite(isi) &
      isi <= vp$bridge_high
    stpd_candidate_lineage_collector_capture(
      candidate_lineage_collector,
      "burst_raw_threshold_support_v1",
      data.frame(
        train = rep(as.character(train)[1L], length(support_rows)),
        detector_row_index = as.integer(support_rows),
        isi_index = seq_along(support_rows),
        isi_sec = as.numeric(isi[support_rows]),
        artifact_isi = as.logical(art[support_rows]),
        valid_isi = as.logical(valid[support_rows]),
        seed_band_member = as.logical(seed_flag[support_rows]),
        native_bridge_extension_eligible = as.logical(
          native_bridge_extension_eligible[support_rows]
        ),
        min_valid_isi_sec = rep(
          as.numeric(min_isi_sec)[1L], length(support_rows)
        ),
        seed_low_sec = rep(
          as.numeric(vp$seed_low)[1L], length(support_rows)
        ),
        seed_high_sec = rep(
          as.numeric(vp$seed_high)[1L], length(support_rows)
        ),
        native_bridge_high_sec = rep(
          as.numeric(vp$bridge_high)[1L], length(support_rows)
        ),
        min_seed_isi_count = rep(
          as.integer(vp$min_seed_isi_n)[1L], length(support_rows)
        ),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    )
  }
  runs <- stpd_event_grammar_bool_runs(seed_flag)
  if (!is.null(candidate_lineage_collector)) {
    seed_run_payload <- stpd_candidate_lineage_empty_hook_payload(
      "burst_seed_runs_v1"
    )
    if (nrow(runs)) {
      seed_count <- as.integer(runs$end_isi - runs$start_isi + 1L)
      seed_run_payload <- data.frame(
        train = rep(as.character(train)[1L], nrow(runs)),
        run_ordinal = seq_len(nrow(runs)),
        detector_start_row = as.integer(runs$start_isi),
        detector_end_row = as.integer(runs$end_isi),
        start_isi = as.integer(runs$start_isi - 1L),
        end_isi = as.integer(runs$end_isi - 1L),
        seed_isi_count = seed_count,
        min_seed_isi_count = rep(
          as.integer(vp$min_seed_isi_n)[1L], nrow(runs)
        ),
        min_seed_pass = seed_count >= as.integer(vp$min_seed_isi_n)[1L],
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
    stpd_candidate_lineage_collector_capture(
      candidate_lineage_collector,
      "burst_seed_runs_v1",
      seed_run_payload
    )
  }

  native_bridge_high <- vp$bridge_high
  proposed_bridge_high <- stpd_event_grammar_num(
    vp$cross_train_borrowing_bridge_candidate_sec %||% native_bridge_high,
    native_bridge_high
  )
  candidate_bridge_high <- max(native_bridge_high, proposed_bridge_high)
  candidate_vp <- vp
  candidate_vp$bridge_high <- candidate_bridge_high
  borrowed_run_max <- max(1L, stpd_event_grammar_int(
    (params$event_grammar %||% list())$
      burst_extension_max_consecutive_borrowed_isi %||% 2L,
    2L
  ))
  proposal_intrusion_mask <- stpd_event_grammar_burst_intrusion_mask(
    isi, valid, native_bridge_high, candidate_bridge_high,
    borrowed_run_max
  )
  proposal_extension_valid <- valid & !proposal_intrusion_mask

  eg <- params$event_grammar %||% list()
  burst_internal_gap_ratio_max <- stpd_event_grammar_num(
    params$burst$classic_burst_internal_outlier_ratio_max %||%
      params$burst$internal_outlier_ratio_max %||% 3.5,
    3.5
  )
  strict_q95 <- isTRUE(eg$strict_q95_bridge_gate %||% FALSE)
  q95_severe_ratio <- stpd_event_grammar_num(eg$q95_soft_severe_ratio %||% 1.35, 1.35)
  q95_severe_ratio <- max(1.0, q95_severe_ratio)
  one_sided_as_canonical <- isTRUE(eg$allow_one_sided_burst_as_canonical %||% FALSE)
  one_sided_S <- stpd_event_grammar_num(eg$one_sided_burst_contrast_min %||% (vp$S + 0.5), vp$S + 0.5)
  one_sided_seed_purity_min <- stpd_event_grammar_num(eg$one_sided_seed_purity_min %||% 0.65, 0.65)
  one_sided_bridge_frac_max <- stpd_event_grammar_num(eg$one_sided_bridge_fraction_max %||% min(0.35, vp$max_bridge_frac), min(0.35, vp$max_bridge_frac))
  structural_rescue_min_ratio <- stpd_event_grammar_num(eg$structural_burst_rescue_compression_min %||% 3.0, 3.0)
  structural_rescue_strong_ratio <- stpd_event_grammar_num(eg$structural_burst_rescue_strong_compression_min %||% 4.0, 4.0)
  structural_rescue_seed_purity_min <- stpd_event_grammar_num(eg$structural_burst_rescue_seed_purity_min %||% 0.70, 0.70)
  structural_rescue_cv_max <- stpd_event_grammar_num(eg$structural_burst_rescue_cv_max %||% 0.80, 0.80)
  train_bg_ref <- stpd_event_grammar_q(valid_isi_values(isi[valid], min_isi_sec), 0.75, NA_real_)
  # This is a candidate envelope, not a committed threshold mutation.  It is
  # scale-relative, remains below canonical Pause, and every extension must
  # pass the candidate-level mode-intrusion guard below.
  episode_upper <- max(
    candidate_bridge_high,
    native_bridge_high * stpd_event_grammar_num(
      eg$structural_burst_episode_bridge_factor %||% 1.75, 1.75
    ),
    na.rm = TRUE
  )
  if (is.finite(vp$pause_thr) && vp$pause_thr > 0) {
    episode_upper <- min(episode_upper, vp$pause_thr * (1 - 1e-6))
  }
  if (!is.finite(episode_upper) || episode_upper <= native_bridge_high) {
    episode_upper <- native_bridge_high
  }
  episode_min_isi <- max(3L, stpd_event_grammar_int(eg$structural_burst_episode_min_isi %||% 3L, 3L))
  episode_seed_min <- max(1L, stpd_event_grammar_int(eg$structural_burst_episode_min_seed_isi %||% 1L, 1L))
  episode_seed_frac_min <- max(0, min(1, stpd_event_grammar_num(eg$structural_burst_episode_seed_fraction_min %||% 0.18, 0.18)))
  episode_bridge_frac_min <- max(0, min(1, stpd_event_grammar_num(eg$structural_burst_episode_bridge_fraction_min %||% 0.55, 0.55)))
  episode_low_isi_frac_min <- max(episode_bridge_frac_min, min(1, stpd_event_grammar_num(eg$structural_burst_episode_low_isi_fraction_min %||% 0.85, 0.85)))
  episode_cv_max <- stpd_event_grammar_num(eg$structural_burst_episode_cv_max %||% 0.55, 0.55)
  burst_contract <- (params$spiketrainpattern %||% list())$burst %||% list()
  threshold_elastic_min_spikes <- max(
    vp$min_spikes,
    stpd_event_grammar_int(
      burst_contract$threshold_elastic_min_spikes_without_contrast %||% 6L,
      6L
    )
  )

  stage4_observer <- if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_stage4_observer_new(
      candidate_lineage_collector, train, dat, vp, params, min_isi_sec,
      seed_run_payload, isi, valid, proposal_intrusion_mask,
      candidate_bridge_high, episode_upper, borrowed_run_max
    )
  } else {
    NULL
  }

  seen <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    ss <- as.integer(runs$start_isi[rr]); ee <- as.integer(runs$end_isi[rr])
    if (sum(seed_flag[ss:ee], na.rm = TRUE) < vp$min_seed_isi_n) next
    lefts <- stpd_event_core_left_extensions(
      ss, isi, proposal_extension_valid, candidate_bridge_high,
      vp$max_expand
    )
    rights <- stpd_event_core_right_extensions(
      ee, n, isi, proposal_extension_valid, candidate_bridge_high,
      vp$max_expand
    )
    for (s in lefts) for (e in rights) {
      if (length(rows) >= vp$max_candidates) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_note_cap(
            stage4_observer, "seed_expansion", rr, length(rows)
          )
        }
        break
      }
      stage4_attempt <- if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_begin_attempt(
          stage4_observer, "seed_expansion", rr, ss, ee,
          s, e, length(rows)
        )
      } else NA_integer_
      if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_claim_geometry(
          stage4_observer, stage4_attempt, "seed_dense_shared_seen"
        )
      }
      key <- paste0(s, "_", e)
      if (!is.null(seen[[key]])) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_duplicate(
            stage4_observer, stage4_attempt
          )
        }
        next
      }
      seen[[key]] <- TRUE
      idx <- stpd_event_grammar_safe_seq(s, e)
      if (length(idx) == 0) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt, "empty_safe_sequence"
          )
        }
        next
      }
      core_n <- sum(seed_flag[idx], na.rm = TRUE)
      if (core_n < vp$min_seed_isi_n) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt, "insufficient_seed_support"
          )
        }
        next
      }
      n_spikes <- e - s + 2L
      if (n_spikes < vp$min_spikes) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt, "insufficient_spikes"
          )
        }
        next
      }
      m <- stpd_event_core_span_metrics(dat, s, e, params, candidate_vp, min_isi_sec, train, "event_grammar_burst_event")
      if (is.null(m)) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt, "span_metrics_unavailable"
          )
        }
        next
      }

      extension_guard <- stpd_event_grammar_burst_extension_guard(
        isi[idx][valid[idx]], native_bridge_high, candidate_bridge_high,
        n_spikes, vp, params
      )
      internal_gap_guard <- stpd_event_grammar_burst_internal_gap_guard(
        isi[idx][valid[idx]], vp$seed_high,
        ratio_max = burst_internal_gap_ratio_max
      )

      valid_n <- max(1, stpd_event_grammar_num(m$n_valid_isi[1], length(idx)))
      seed_purity <- core_n / valid_n
      m$seed_purity <- seed_purity
      bridge_count_pass <- is.finite(m$bridge_isi_count[1]) && m$bridge_isi_count[1] <= vp$max_bridge_n
      bridge_fraction_pass <- is.finite(m$bridge_fraction[1]) && m$bridge_fraction[1] <= vp$max_bridge_frac
      q90_pass <- is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] <= candidate_bridge_high
      q95_raw_pass <- is.finite(m$intra_q95_sec[1]) && m$intra_q95_sec[1] <= candidate_bridge_high
      q95_excess_ratio <- if (is.finite(m$intra_q95_sec[1]) && is.finite(candidate_bridge_high) && candidate_bridge_high > 0) m$intra_q95_sec[1] / candidate_bridge_high else NA_real_
      q95_severe <- is.finite(q95_excess_ratio) && q95_excess_ratio > q95_severe_ratio
      q95_gate_pass <- q95_raw_pass || (!strict_q95 && !q95_severe)

      pre_ratio <- if (is.finite(m$pre_gap_sec[1]) && is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] > 0) m$pre_gap_sec[1] / m$intra_q90_sec[1] else NA_real_
      post_ratio <- if (is.finite(m$post_gap_sec[1]) && is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] > 0) m$post_gap_sec[1] / m$intra_q90_sec[1] else NA_real_
      two_sided <- is.finite(pre_ratio) && is.finite(post_ratio) && pre_ratio >= vp$S && post_ratio >= vp$S
      two_possible <- is.finite(pre_ratio) && is.finite(post_ratio) && pre_ratio >= vp$S_possible && post_ratio >= vp$S_possible
      one_sided <- (is.finite(pre_ratio) && pre_ratio >= one_sided_S) || (is.finite(post_ratio) && post_ratio >= one_sided_S)
      edge_limited <- (!is.finite(pre_ratio) || !is.finite(post_ratio)) && one_sided
      clean_one_sided <- one_sided && seed_purity >= one_sided_seed_purity_min && is.finite(m$bridge_fraction[1]) && m$bridge_fraction[1] <= one_sided_bridge_frac_max && q90_pass && q95_gate_pass
      neg <- isTRUE(m$manual_negative_veto[1])
      bg_vals <- c(train_bg_ref, m$pre_gap_sec[1], m$post_gap_sec[1])
      bg_vals <- bg_vals[is.finite(bg_vals)]
      structural_bg_ref <- if (length(bg_vals) > 0) max(bg_vals, na.rm = TRUE) else NA_real_
      structural_ratio <- if (is.finite(structural_bg_ref) && is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] > 0) structural_bg_ref / m$intra_q90_sec[1] else NA_real_
      cv_val <- stpd_event_grammar_num(if ("CV" %in% names(m)) m$CV[1] else NA_real_, NA_real_)
      cv_rescue_pass <- !is.finite(cv_val) || cv_val <= structural_rescue_cv_max

      size_label <- stpd_event_grammar_burst_subtype_from_spike_count(
        n_spikes,
        vp,
        review_label = "prolonged_burst_like"
      )

      final <- "reject"; status <- "event_grammar_reject"; action <- "reject"; decision <- "event_grammar_reject"; priority <- 0
      core_pass <- !neg && bridge_count_pass && bridge_fraction_pass &&
        q90_pass && q95_gate_pass && isTRUE(extension_guard$pass) &&
        isTRUE(internal_gap_guard$pass)
      structural_rescue <- core_pass &&
        seed_purity >= structural_rescue_seed_purity_min &&
        cv_rescue_pass &&
        is.finite(structural_ratio) &&
        structural_ratio >= structural_rescue_min_ratio
      structural_rescue_strong <- structural_rescue && structural_ratio >= structural_rescue_strong_ratio
      q95_note <- if (!q95_raw_pass && q95_gate_pass) ";q95_soft_exceeds_bridge" else ""
      contrast_evidence <- if (two_sided) {
        "strong_two_sided"
      } else if (clean_one_sided) {
        if (edge_limited) "clean_edge_limited" else "clean_one_sided"
      } else if (two_possible) {
        "moderate_two_sided"
      } else if (one_sided) {
        "weak_one_sided"
      } else {
        "weak_or_unavailable"
      }
      threshold_elastic_support_pass <- n_spikes >= threshold_elastic_min_spikes

      # The threshold-resolved seed/bridge path is the primary, Max-Interval-
      # like STPD burst route.  Flank contrast is useful evidence and the
      # structure-first screen remains an independent rescue route, but flank
      # contrast is not a universal acceptance gate once the complete elastic
      # ISI contract (seed, bridge, size, q90/q95, artifact and negative-label
      # guards) has passed and the contrast-independent support minimum is met.
      if (core_pass && size_label %in% c("burst", "long_burst") &&
          (two_sided || threshold_elastic_support_pass)) {
        final <- size_label
        action <- "accept"
        if (two_sided) {
          status <- "event_grammar_two_sided_burst_event_pass"
          decision <- paste0(
            "event_grammar_two_sided_event_grammar_pass__", size_label,
            q95_note
          )
          priority <- if (final == "burst") 1250 else 1160
        } else {
          status <- "event_grammar_threshold_elastic_burst_event_pass"
          decision <- paste0(
            "threshold_elastic_seed_bridge_contract_pass__", size_label,
            ";contrast_evidence=", contrast_evidence,
            q95_note
          )
          priority <- if (final == "burst") 1210 else 1140
        }
      } else if (core_pass && two_sided) {
        final <- "possible_burst"; status <- "event_grammar_prolonged_burst_like_review"; action <- "demote_to_possible"; decision <- paste0("two_sided_structure_but_spike_count_exceeds_long_burst_range", q95_note); priority <- stpd_event_grammar_possible_priority(m, vp, two_possible = TRUE, one_sided = FALSE, q95_excess_ratio = q95_excess_ratio)
      } else if (core_pass && clean_one_sided) {
        if (one_sided_as_canonical && size_label %in% c("burst", "long_burst")) {
          final <- size_label; status <- "event_grammar_clean_one_sided_burst_event_pass_user_allowed"; action <- "accept"; decision <- paste0("event_grammar_clean_one_sided_event_grammar_pass__", size_label, q95_note); priority <- if (final == "burst") 980 else 930
        } else {
          final <- "possible_burst"; status <- if (edge_limited) "event_grammar_clean_edge_limited_possible_burst" else "event_grammar_clean_one_sided_possible_burst"; action <- "demote_to_possible"; decision <- paste0("clean_one_sided_flank_contrast_pass_core_compact", q95_note); priority <- stpd_event_grammar_possible_priority(m, vp, two_possible = FALSE, one_sided = TRUE, q95_excess_ratio = q95_excess_ratio)
        }
      } else if (!neg && bridge_count_pass && bridge_fraction_pass &&
                 q90_pass && q95_gate_pass &&
                 isTRUE(internal_gap_guard$pass) && two_possible) {
        final <- "possible_burst"; status <- "event_grammar_possible_two_sided_burst"; action <- "demote_to_possible"; decision <- paste0("two_sided_possible_contrast_pass", q95_note); priority <- stpd_event_grammar_possible_priority(m, vp, two_possible = TRUE, one_sided = FALSE, q95_excess_ratio = q95_excess_ratio)
      } else if (structural_rescue) {
        if (structural_rescue_strong && size_label %in% c("burst", "long_burst")) {
          final <- size_label; status <- "event_grammar_structural_burst_rescue_pass"; action <- "accept"; decision <- paste0("compact_short_isi_cluster_rescued_by_train_scale_compression__", size_label, q95_note); priority <- if (final == "burst") 1010 else 960
        } else {
          final <- "possible_burst"; status <- "event_grammar_structural_possible_burst_rescue"; action <- "demote_to_possible"; decision <- paste0("compact_short_isi_cluster_rescued_for_review_by_train_scale_compression", q95_note); priority <- stpd_event_grammar_possible_priority(m, vp, two_possible = FALSE, one_sided = FALSE, q95_excess_ratio = q95_excess_ratio)
        }
      } else {
        reasons <- c(if (neg) "manual_negative_veto", if (!bridge_count_pass) "too_many_bridge_isis", if (!bridge_fraction_pass) "bridge_fraction_too_high", if (!q90_pass) "intra_q90_exceeds_bridge_band", if (strict_q95 && !q95_raw_pass) "intra_q95_exceeds_bridge_band_strict", if (!strict_q95 && q95_severe) "intra_q95_severely_exceeds_bridge_band", if (!isTRUE(extension_guard$pass)) extension_guard$reason, if (!isTRUE(internal_gap_guard$pass)) internal_gap_guard$reason, if (!two_sided && !one_sided && !two_possible) "flank_contrast_fail")
        decision <- paste(reasons, collapse = ";"); if (!nzchar(decision)) decision <- "event_grammar_reject"
      }
      q95_penalty <- if (is.finite(q95_excess_ratio) && q95_excess_ratio > 1) 0.45 * min(2, q95_excess_ratio - 1) else 0
      score <- (if (is.finite(m$burst_contrast_score[1])) m$burst_contrast_score[1] else 0) + 0.16 * m$core_isi_count[1] + 0.35 * seed_purity - 0.20 * m$bridge_isi_count[1] - 0.45 * m$bridge_fraction[1] - q95_penalty
      counter <- counter + 1L
      stage4_candidate <- stpd_event_core_candidate_row(m, "event_grammar_burst_event", "event_grammar_seed_centered_burst", final, status, decision, action, score, priority,
        list(candidate_id = paste0("event_grammar_burst_opt2_", counter), seed_run_start_isi = ss, seed_run_end_isi = ee,
             seed_band_lower_sec = vp$seed_low, seed_band_upper_sec = vp$seed_high, bridge_band_upper_sec = candidate_bridge_high,
             native_bridge_band_upper_sec = native_bridge_high,
             boundary_extension_proposal_sec = proposed_bridge_high,
             boundary_extension_guard_pass = extension_guard$pass,
             boundary_extension_guard_reason = extension_guard$reason,
             boundary_extension_native_isi_n = extension_guard$native_n,
             boundary_extension_borrowed_isi_n = extension_guard$borrowed_n,
             boundary_extension_native_fraction = extension_guard$native_fraction,
             boundary_extension_max_consecutive_borrowed_isi = extension_guard$max_consecutive_borrowed,
             boundary_extension_rollback_to_intrusion_onset = TRUE,
             internal_gap_guard_pass = internal_gap_guard$pass,
             internal_gap_guard_reason = internal_gap_guard$reason,
             internal_gap_guard_evaluated = internal_gap_guard$evaluated,
             internal_gap_max_isi_sec = internal_gap_guard$max_isi_sec,
             internal_gap_reference_median_sec = internal_gap_guard$reference_median_sec,
             internal_gap_reference_mean_sec = internal_gap_guard$reference_mean_sec,
             internal_gap_max_to_median_ratio = internal_gap_guard$max_to_median_ratio,
             internal_gap_max_to_mean_ratio = internal_gap_guard$max_to_mean_ratio,
             internal_gap_ratio_max = internal_gap_guard$ratio_max,
             internal_gap_burst_left_upper_sec = internal_gap_guard$burst_left_upper_sec,
             internal_gap_ratio_pass = internal_gap_guard$ratio_pass,
             internal_gap_left_band_rescue = internal_gap_guard$left_band_rescue,
             burst_contrast_required = vp$S, one_sided_contrast_required = one_sided_S, possible_contrast_required = vp$S_possible,
             pre_ratio_q90 = pre_ratio, post_ratio_q90 = post_ratio,
             seed_purity = seed_purity,
             structural_burst_rescue_pass = structural_rescue,
             structural_burst_rescue_strong = structural_rescue_strong,
             structural_compression_reference_sec = structural_bg_ref,
             structural_compression_ratio = structural_ratio,
             structural_rescue_compression_min = structural_rescue_min_ratio,
             structural_rescue_seed_purity_min = structural_rescue_seed_purity_min,
             structural_rescue_cv_max = structural_rescue_cv_max,
             threshold_elastic_contract_pass = core_pass,
             threshold_elastic_min_spikes_without_contrast = threshold_elastic_min_spikes,
             threshold_elastic_support_pass = threshold_elastic_support_pass,
             threshold_elastic_acceptance = core_pass &&
               final %in% c("burst", "long_burst") &&
               (two_sided || threshold_elastic_support_pass),
             contrast_evidence_status = contrast_evidence,
             q95_excess_ratio = q95_excess_ratio,
             q95_bridge_hard_gate = strict_q95,
             q95_bridge_soft_pass = q95_gate_pass,
             q95_bridge_severe = q95_severe,
             q95_bridge_penalty = q95_penalty,
             boundary_type = if (two_sided) "two_sided" else if (clean_one_sided) "clean_one_sided_or_edge_limited" else if (one_sided) "weak_one_sided" else "failed",
             strict_boundary_pass = two_sided, one_sided_boundary_pass = one_sided, clean_one_sided_pass = clean_one_sided, possible_boundary_pass = two_possible,
             bridge_count_pass = bridge_count_pass, bridge_fraction_pass = bridge_fraction_pass, q90_bridge_pass = q90_pass, q95_bridge_pass = q95_raw_pass,
             failure_reason = if (final == "reject") decision else "",
             candidate_diagnostic_class = if (final == "reject") paste0("rejected__", decision) else paste0(final, "__", status),
             threshold_source_summary = paste0("seed=", vp$threshold_table$source[vp$threshold_table$pattern=="burst" & vp$threshold_table$field=="seed_upper_sec"][1] %||% "")))
      rows[[length(rows) + 1L]] <- stage4_candidate
      if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_finish_candidate(
          stage4_observer, stage4_attempt, stage4_candidate
        )
      }
    }
    if (length(rows) >= vp$max_candidates) {
      if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_note_cap(
          stage4_observer, "seed_expansion", rr, length(rows)
        )
      }
      break
    }
  }
  episode_intrusion_mask <- stpd_event_grammar_burst_intrusion_mask(
    isi, valid, native_bridge_high, episode_upper, borrowed_run_max
  )
  episode_flag <- valid & isi <= episode_upper & !episode_intrusion_mask
  episode_runs <- stpd_event_grammar_bool_runs(episode_flag)
  if (!is.null(stage4_observer)) {
    stpd_candidate_lineage_stage4_set_episode_roots(
      stage4_observer, episode_runs, episode_intrusion_mask, episode_flag
    )
  }
  if (nrow(episode_runs) > 0 && length(rows) < vp$max_candidates) {
    for (rr in seq_len(nrow(episode_runs))) {
      if (length(rows) >= vp$max_candidates) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_note_cap(
            stage4_observer, "dense_episode", rr, length(rows)
          )
        }
        break
      }
      s <- as.integer(episode_runs$start_isi[rr]); e <- as.integer(episode_runs$end_isi[rr])
      s0 <- s; e0 <- e
      stage4_attempt <- if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_begin_attempt(
          stage4_observer, "dense_episode", rr, s0, e0,
          rows_before_attempt = length(rows)
        )
      } else NA_integer_
      idx0 <- stpd_event_grammar_safe_seq(s0, e0)
      vals0 <- if (length(idx0) > 0) isi[idx0][valid[idx0]] else numeric()
      edge_q90 <- stpd_event_grammar_q(vals0, 0.90, NA_real_)
      edge_upper <- max(c(vp$bridge_high, edge_q90 * 1.10), na.rm = TRUE)
      edge_upper <- min(edge_upper, episode_upper, na.rm = TRUE)
      if (!is.finite(edge_upper) || edge_upper <= 0) edge_upper <- episode_upper
      while (s <= e && is.finite(isi[s]) && isi[s] > edge_upper) s <- s + 1L
      while (e >= s && is.finite(isi[e]) && isi[e] > edge_upper) e <- e - 1L
      if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_set_geometry(
          stage4_observer, stage4_attempt, s, e
        )
      }
      idx <- stpd_event_grammar_safe_seq(s, e)
      if (length(idx) < episode_min_isi) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "dense_trimmed_span_too_short"
          )
        }
        next
      }
      if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_claim_geometry(
          stage4_observer, stage4_attempt, "seed_dense_shared_seen"
        )
      }
      key <- paste0(s, "_", e)
      if (!is.null(seen[[key]])) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_duplicate(
            stage4_observer, stage4_attempt
          )
        }
        next
      }
      seen[[key]] <- TRUE
      vals <- isi[idx][valid[idx]]
      if (length(vals) < episode_min_isi) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "dense_valid_support_too_short"
          )
        }
        next
      }
      seed_count <- sum(vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE)
      seed_frac <- seed_count / length(vals)
      bridge_frac <- mean(vals <= vp$bridge_high, na.rm = TRUE)
      low_isi_frac <- mean(vals <= episode_upper, na.rm = TRUE)
      q90 <- stpd_event_grammar_q(vals, 0.90, NA_real_)
      q95 <- stpd_event_grammar_q(vals, 0.95, NA_real_)
      cv <- stpd_event_core_cv(vals)
      compression <- if (is.finite(train_bg_ref) && is.finite(q90) && q90 > 0) train_bg_ref / q90 else NA_real_
      seed_entry_pass <- seed_count >= episode_seed_min &&
        is.finite(seed_frac) && seed_frac >= episode_seed_frac_min
      low_isi_episode_pass <- is.finite(low_isi_frac) &&
        low_isi_frac >= episode_low_isi_frac_min &&
        is.finite(q90) && q90 <= episode_upper &&
        is.finite(compression) &&
        compression >= structural_rescue_min_ratio
      if (!seed_entry_pass && !low_isi_episode_pass) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "dense_entry_evidence_failed"
          )
        }
        next
      }
      if (!is.finite(q90) || q90 > episode_upper) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt, "dense_q90_failed"
          )
        }
        next
      }
      if (is.finite(cv) && cv > episode_cv_max) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt, "dense_cv_failed"
          )
        }
        next
      }
      if (!is.finite(compression) ||
          compression < structural_rescue_min_ratio) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "dense_compression_failed"
          )
        }
        next
      }

      n_spikes <- e - s + 2L
      if (n_spikes < vp$min_spikes) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt, "dense_insufficient_spikes"
          )
        }
        next
      }
      extension_guard <- stpd_event_grammar_burst_extension_guard(
        vals, native_bridge_high, episode_upper, n_spikes, vp, params
      )
      if (!isTRUE(extension_guard$pass)) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            paste0("dense_extension_guard:", extension_guard$reason)
          )
        }
        next
      }
      internal_gap_guard <- stpd_event_grammar_burst_internal_gap_guard(
        vals, vp$seed_high, ratio_max = burst_internal_gap_ratio_max
      )
      if (!isTRUE(internal_gap_guard$pass)) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            paste0("dense_internal_gap_guard:", internal_gap_guard$reason)
          )
        }
        next
      }
      m <- stpd_event_core_span_metrics(dat, s, e, params, vp, min_isi_sec, train, "event_grammar_burst_episode")
      if (is.null(m)) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "dense_span_metrics_unavailable"
          )
        }
        next
      }
      final <- stpd_event_grammar_burst_subtype_from_spike_count(n_spikes, vp, review_label = "possible_burst")
      status <- if (final == "possible_burst") "event_grammar_structural_burst_episode_review" else "event_grammar_structural_burst_episode_pass"
      action <- if (final == "possible_burst") "demote_to_possible" else "accept"
      decision <- paste0("dense_short_isi_episode_rescued_by_train_scale_compression__", final)
      # Burst and long_burst are spike-count subtypes of the same accepted dense
      # episode.  Changing the subtype label must not, by itself, make an
      # otherwise identical candidate lose interval arbitration.
      priority <- switch(final, burst = 1035, long_burst = 1035, possible_burst = stpd_event_grammar_possible_priority(m, vp, two_possible = FALSE, one_sided = FALSE, q95_excess_ratio = NA_real_), 0)
      score <- 4 + 0.12 * length(vals) + 0.6 * seed_frac + 0.5 * bridge_frac +
        if (is.finite(compression)) min(4, 0.2 * compression) else 0 -
        if (is.finite(cv)) 0.4 * cv else 0
      counter <- counter + 1L
      stage4_candidate <- stpd_event_core_candidate_row(
        m, "event_grammar_burst_episode", "event_grammar_dense_short_isi_episode",
        final, status, decision, action, score, priority,
        list(candidate_id = paste0("event_grammar_burst_episode_", counter),
             seed_band_lower_sec = vp$seed_low,
             seed_band_upper_sec = vp$seed_high,
             bridge_band_upper_sec = vp$bridge_high,
             burst_episode_upper_sec = episode_upper,
             burst_episode_seed_count = seed_count,
             burst_episode_seed_fraction = seed_frac,
             burst_episode_bridge_fraction = bridge_frac,
             burst_episode_native_support_fraction = extension_guard$native_fraction,
             boundary_extension_guard_pass = extension_guard$pass,
             boundary_extension_guard_reason = extension_guard$reason,
             boundary_extension_borrowed_isi_n = extension_guard$borrowed_n,
             boundary_extension_max_consecutive_borrowed_isi = extension_guard$max_consecutive_borrowed,
             internal_gap_guard_pass = internal_gap_guard$pass,
             internal_gap_guard_reason = internal_gap_guard$reason,
             internal_gap_guard_evaluated = internal_gap_guard$evaluated,
             internal_gap_max_isi_sec = internal_gap_guard$max_isi_sec,
             internal_gap_reference_median_sec = internal_gap_guard$reference_median_sec,
             internal_gap_reference_mean_sec = internal_gap_guard$reference_mean_sec,
             internal_gap_max_to_median_ratio = internal_gap_guard$max_to_median_ratio,
             internal_gap_max_to_mean_ratio = internal_gap_guard$max_to_mean_ratio,
             internal_gap_ratio_max = internal_gap_guard$ratio_max,
             internal_gap_burst_left_upper_sec = internal_gap_guard$burst_left_upper_sec,
             internal_gap_ratio_pass = internal_gap_guard$ratio_pass,
             internal_gap_left_band_rescue = internal_gap_guard$left_band_rescue,
             burst_episode_low_isi_fraction = low_isi_frac,
             burst_episode_seed_entry_pass = seed_entry_pass,
             burst_episode_low_isi_episode_pass = low_isi_episode_pass,
             burst_episode_low_isi_fraction_min = episode_low_isi_frac_min,
             burst_episode_edge_upper_sec = edge_upper,
             burst_episode_start_isi_before_trim = s0,
             burst_episode_end_isi_before_trim = e0,
             burst_episode_cv = cv,
             structural_burst_rescue_pass = TRUE,
             structural_burst_rescue_strong = TRUE,
             structural_compression_reference_sec = train_bg_ref,
             structural_compression_ratio = compression,
             structural_rescue_compression_min = structural_rescue_min_ratio,
             boundary_type = "dense_episode",
             strict_boundary_pass = FALSE,
             one_sided_boundary_pass = FALSE,
             clean_one_sided_pass = FALSE,
             possible_boundary_pass = FALSE,
             q90_bridge_pass = q90 <= vp$bridge_high,
             q95_bridge_pass = q95 <= vp$bridge_high,
             q95_excess_ratio = if (is.finite(q95) && is.finite(vp$bridge_high) && vp$bridge_high > 0) q95 / vp$bridge_high else NA_real_,
             failure_reason = "",
             candidate_diagnostic_class = paste0(final, "__", status),
             threshold_source_summary = paste0("episode_upper=", signif(episode_upper, 4))))
      rows[[length(rows) + 1L]] <- stage4_candidate
      if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_finish_candidate(
          stage4_observer, stage4_attempt, stage4_candidate
        )
      }
    }
  }

  # A Burst may contain one moderately longer internal ISI that bridges two
  # compact seed-supported cores.  Generate that Burst candidate explicitly
  # before Pause candidates are considered.  This is deliberately stricter
  # than simply merging neighbouring Burst events: both sides must be
  # contiguous short-ISI cores, the connector is singular and bounded, and
  # the complete episode must remain compact on a robust (q75) scale.
  if (isTRUE(eg$single_expanded_bridge_enabled %||% FALSE) &&
      nrow(episode_runs) > 0 && length(rows) < vp$max_candidates) {
    bridge_seen <- list()
    side_seed_min <- max(1L, stpd_event_grammar_int(vp$min_seed_isi_n, 2L))
    connector_ratio_min <- 1.15
    connector_ratio_max <- burst_internal_gap_ratio_max
    for (rr in seq_len(nrow(episode_runs))) {
      if (length(rows) >= vp$max_candidates) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_note_cap(
            stage4_observer, "single_expanded_bridge", rr,
            length(rows)
          )
        }
        break
      }
      run_s <- as.integer(episode_runs$start_isi[rr])
      run_e <- as.integer(episode_runs$end_isi[rr])
      stage4_attempt <- if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_begin_attempt(
          stage4_observer, "single_expanded_bridge", rr, run_s, run_e,
          rows_before_attempt = length(rows)
        )
      } else NA_integer_
      run_idx <- stpd_event_grammar_safe_seq(run_s, run_e)
      if (length(run_idx) < 2L) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "connector_source_run_too_short"
          )
        }
        next
      }
      connector <- run_idx[
        valid[run_idx] & is.finite(isi[run_idx]) &
          isi[run_idx] > vp$bridge_high & isi[run_idx] <= episode_upper
      ]
      if (length(connector) != 1L) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "connector_count_not_one"
          )
        }
        next
      }
      connector <- as.integer(connector[1L])

      # Take the two maximal normal-bridge runs immediately adjacent to the
      # connector.  This avoids manufacturing a bridge across a genuine gap.
      normal_bridge <- valid & is.finite(isi) &
        isi >= vp$seed_low & isi <= vp$bridge_high
      left <- integer(); jj <- connector - 1L
      while (jj >= run_s && isTRUE(normal_bridge[jj])) {
        left <- c(jj, left); jj <- jj - 1L
      }
      right <- integer(); jj <- connector + 1L
      while (jj <= run_e && isTRUE(normal_bridge[jj])) {
        right <- c(right, jj); jj <- jj + 1L
      }
      if (length(left) < side_seed_min || length(right) < side_seed_min) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "connector_side_core_support_failed"
          )
        }
        next
      }
      s <- min(left); e <- max(right)
      if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_set_geometry(
          stage4_observer, stage4_attempt, s, e
        )
        stpd_candidate_lineage_stage4_claim_geometry(
          stage4_observer, stage4_attempt,
          "single_connector_bridge_seen"
        )
      }
      key <- paste0(s, "_", e)
      if (!is.null(bridge_seen[[key]])) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_duplicate(
            stage4_observer, stage4_attempt
          )
        }
        next
      }
      bridge_seen[[key]] <- TRUE
      idx <- stpd_event_grammar_safe_seq(s, e)
      vals <- isi[idx][valid[idx]]
      if (length(vals) != length(idx) || length(vals) < 2L) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "connector_valid_support_failed"
          )
        }
        next
      }
      core_vals <- isi[c(left, right)]
      core_median <- stpd_event_grammar_q(core_vals, 0.50, NA_real_)
      connector_ratio <- if (is.finite(core_median) && core_median > 0) {
        isi[connector] / core_median
      } else NA_real_
      q75 <- stpd_event_grammar_q(vals, 0.75, NA_real_)
      seed_count <- sum(vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE)
      n_spikes <- e - s + 2L
      if (n_spikes < vp$min_spikes || seed_count < 2L * side_seed_min ||
          !is.finite(q75) || q75 > vp$bridge_high ||
          !is.finite(connector_ratio) || connector_ratio < connector_ratio_min ||
          connector_ratio > connector_ratio_max) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "connector_compactness_or_size_failed"
          )
        }
        next
      }
      m <- stpd_event_core_span_metrics(
        dat, s, e, params, vp, min_isi_sec, train,
        "event_grammar_single_expanded_bridge_burst"
      )
      if (is.null(m)) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "connector_span_metrics_unavailable"
          )
        }
        next
      }
      if (isTRUE(m$manual_negative_veto[1L])) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "connector_manual_negative_veto"
          )
        }
        next
      }
      final <- stpd_event_grammar_burst_subtype_from_spike_count(
        n_spikes, vp, review_label = "possible_burst"
      )
      if (!final %in% c("burst", "long_burst")) {
        if (!is.null(stage4_observer)) {
          stpd_candidate_lineage_stage4_finish_filter(
            stage4_observer, stage4_attempt,
            "connector_subtype_not_canonical"
          )
        }
        next
      }
      counter <- counter + 1L
      stage4_candidate <- stpd_event_core_candidate_row(
        m, "event_grammar_single_expanded_bridge_burst",
        "event_grammar_single_expanded_bridge_burst", final,
        "event_grammar_single_expanded_bridge_burst_pass",
        "one_bounded_expanded_isi_connects_two_compact_burst_cores",
        "accept",
        5 + 0.18 * seed_count + 0.25 * connector_ratio,
        if (identical(final, "burst")) 1190 else 1145,
        list(
          candidate_id = paste0("event_grammar_expanded_bridge_burst_", counter),
          seed_band_lower_sec = vp$seed_low,
          seed_band_upper_sec = vp$seed_high,
          bridge_band_upper_sec = vp$bridge_high,
          burst_episode_upper_sec = episode_upper,
          expanded_bridge_isi_index = connector,
          expanded_bridge_isi_sec = isi[connector],
          expanded_bridge_connector_ratio = connector_ratio,
          expanded_bridge_connector_ratio_min = connector_ratio_min,
          expanded_bridge_connector_ratio_max = connector_ratio_max,
          expanded_bridge_left_core_isi_n = length(left),
          expanded_bridge_right_core_isi_n = length(right),
          expanded_bridge_q75_sec = q75,
          expanded_bridge_q75_pass = TRUE,
          expanded_bridge_count = 1L,
          candidate_diagnostic_class = paste0(final, "__single_expanded_bridge")
        )
      )
      rows[[length(rows) + 1L]] <- stage4_candidate
      if (!is.null(stage4_observer)) {
        stpd_candidate_lineage_stage4_finish_candidate(
          stage4_observer, stage4_attempt, stage4_candidate
        )
      }
    }
  } else if (isTRUE(eg$single_expanded_bridge_enabled %||% FALSE) &&
      nrow(episode_runs) > 0 && length(rows) >= vp$max_candidates &&
      !is.null(stage4_observer)) {
    stpd_candidate_lineage_stage4_note_cap(
      stage4_observer, "single_expanded_bridge", 1L, length(rows)
    )
  }
  stage4_out <- if (length(rows) == 0L) {
    data.frame()
  } else {
    dplyr::bind_rows(rows)
  }
  if (!is.null(stage4_observer)) {
    stpd_candidate_lineage_stage4_finalize(stage4_observer, stage4_out)
  }
  stage4_out
}

stpd_event_grammar_burst_detector_registry <- function() {
  list(
    threshold_resolved_base = stpd_event_grammar_detect_burst_events_threshold_resolved_base,
    threshold_resolved_optimized = stpd_event_grammar_detect_burst_events_threshold_resolved_optimized,
    consistency_optimized = stpd_event_grammar_detect_burst_events_consistency_optimized,
    final = stpd_event_grammar_detect_burst_events_final_impl
  )
}

stpd_event_grammar_burst_detector_default <- function(params = default_params_sec()) {
  pp <- effective_params_for_detector(params)
  requested <- as.character((pp$event_grammar %||% list())$burst_detector_pipeline %||% "final")[1]
  registry <- stpd_event_grammar_burst_detector_registry()
  if (!nzchar(requested) || !(requested %in% names(registry))) "final" else requested
}

stpd_event_grammar_detect_burst_events_dispatch <- function(
    dat, params, vp, min_isi_sec = 0.001, train = "", pipeline = NULL,
    candidate_lineage_collector = NULL) {
  params <- effective_params_for_detector(params)
  registry <- stpd_event_grammar_burst_detector_registry()
  pipeline <- as.character(pipeline %||% stpd_event_grammar_burst_detector_default(params))[1]
  if (!nzchar(pipeline) || !(pipeline %in% names(registry))) {
    stop("Unknown event grammar burst detector pipeline: ", pipeline, call. = FALSE)
  }
  direct_final_collector <- if (!is.null(candidate_lineage_collector) &&
      identical(pipeline, "final")) candidate_lineage_collector else NULL
  if (!is.null(direct_final_collector)) {
    stpd_candidate_lineage_collector_capture(
      direct_final_collector, "burst_dispatch_final_route_v1",
      data.frame(
        train = as.character(train)[1L], burst_pipeline_id = "final",
        max_candidates = as.integer(max(
          1L,
          stpd_event_core_int(
            (vp %||% list())$max_candidates %||% 3000L, 3000L
          )
        )),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    )
  }
  # Proposal route A is deliberately independent of the resolved Burst
  # seed/bridge band. The historical threshold-centred detector remains the
  # parallel proposal route B/fallback. These route names are distinct from
  # the frozen candidate-lineage stage numbering.
  structure_first <- stpd_event_core_structure_first_burst_candidates(
    dat, params, vp, min_isi_sec = min_isi_sec, train = train,
    candidate_lineage_collector = direct_final_collector
  )
  threshold_centred <- if (!is.null(direct_final_collector)) {
    registry[[pipeline]](
      dat, params, vp, min_isi_sec = min_isi_sec, train = train,
      candidate_lineage_collector = direct_final_collector
    )
  } else {
    registry[[pipeline]](
      dat, params, vp, min_isi_sec = min_isi_sec, train = train
    )
  }
  candidates <- stpd_event_core_cap_burst_candidate_union(
    structure_first, threshold_centred, vp,
    candidate_lineage_collector = direct_final_collector, train = train
  )
  refractory_out <- stpd_event_core_apply_refractory_suspect_policy(
    candidates, params, dat = dat, vp = vp, min_isi_sec = min_isi_sec
  )
  if (!is.null(direct_final_collector)) {
    stpd_candidate_lineage_capture_burst_refractory(
      direct_final_collector, train, candidates, refractory_out,
      dat, params, vp, min_isi_sec
    )
  }
  refractory_out
}

stpd_event_grammar_detect_burst_events <- function(
    dat, params, vp, min_isi_sec = 0.001, train = "",
    candidate_lineage_collector = NULL) {
  stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, min_isi_sec = min_isi_sec, train = train,
    candidate_lineage_collector = candidate_lineage_collector
  )
}
