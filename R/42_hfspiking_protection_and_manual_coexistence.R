# ============================================================
# event grammar HF-spiking state protection + manual coexistence fix
# ------------------------------------------------------------
# Rationale:
#   HF spiking is a long state.  The event grammar detector could still miss obvious
#   dense HF-spiking epochs when:
#     (1) the epoch was split by occasional moderate gaps before it reached the
#         minimum spike count;
#     (2) small possible_burst / tonic / pause candidates out-competed the long
#         HF-spiking state during weighted interval selection;
#     (3) manual labels inside a long HF candidate blocked the entire AUTO
#         candidate.  AUTO evidence should be computed independently; FINAL labels
#         can still be manual-dominant downstream.
# ============================================================

stpd_event_grammar_merge_hf_support_runs <- function(runs, isi, valid, tolerated_gap, max_gap_count,
                                                     hard_break = NA_real_, transparent_gap = NULL,
                                                     transparent_gap_short_side_n = NA_integer_) {
  if (is.null(runs) || nrow(runs) == 0) return(data.frame())
  if (!is.finite(tolerated_gap) || tolerated_gap <= 0) tolerated_gap <- 0.075
  max_gap_count <- max(0L, as.integer(max_gap_count %||% 3L))
  hard_break <- suppressWarnings(as.numeric(hard_break %||% NA_real_))[1]
  if (!is.finite(hard_break) || hard_break <= 0) hard_break <- NA_real_
  transparent_gap_short_side_n <- suppressWarnings(as.integer(transparent_gap_short_side_n %||% NA_integer_))[1]
  if (!is.finite(transparent_gap_short_side_n) || transparent_gap_short_side_n < 1L) {
    transparent_gap_short_side_n <- NA_integer_
  }
  if (is.null(transparent_gap)) {
    transparent_gap <- rep(FALSE, length(isi))
  } else {
    transparent_gap <- as.logical(transparent_gap)
    transparent_gap[is.na(transparent_gap)] <- FALSE
    if (length(transparent_gap) < length(isi)) {
      transparent_gap <- c(transparent_gap, rep(FALSE, length(isi) - length(transparent_gap)))
    }
    if (length(transparent_gap) > length(isi)) transparent_gap <- transparent_gap[seq_along(isi)]
  }

  rows <- list()
  cur_s <- as.integer(runs$start_isi[1])
  cur_e <- as.integer(runs$end_isi[1])

  flush <- function(s, e) data.frame(start_isi = as.integer(s), end_isi = as.integer(e), stringsAsFactors = FALSE)

  if (nrow(runs) >= 2) {
    for (i in 2:nrow(runs)) {
      ns <- as.integer(runs$start_isi[i])
      ne <- as.integer(runs$end_isi[i])
      gap_idx <- if (cur_e + 1L <= ns - 1L) (cur_e + 1L):(ns - 1L) else integer(0)
      gap_in_range <- gap_idx[gap_idx >= 1L & gap_idx <= length(isi)]
      gap_bridgeable <- gap_in_range[(valid[gap_in_range] | transparent_gap[gap_in_range]) & is.finite(isi[gap_in_range])]
      gap_ok <- length(gap_idx) == length(gap_bridgeable) && length(gap_bridgeable) <= max_gap_count
      if (gap_ok && length(gap_bridgeable) > 0 && is.finite(hard_break)) {
        gap_ok <- !any(isi[gap_bridgeable] >= hard_break, na.rm = TRUE)
      }
      if (gap_ok && length(gap_bridgeable) > 0) gap_ok <- all(isi[gap_bridgeable] <= tolerated_gap, na.rm = TRUE)
      if (gap_ok && length(gap_in_range) > 0 && any(transparent_gap[gap_in_range], na.rm = TRUE) &&
          is.finite(transparent_gap_short_side_n)) {
        prev_len <- cur_e - cur_s + 1L
        gap_ok <- is.finite(prev_len) && prev_len <= transparent_gap_short_side_n
      }
      if (gap_ok) {
        cur_e <- ne
      } else {
        rows[[length(rows) + 1L]] <- flush(cur_s, cur_e)
        cur_s <- ns; cur_e <- ne
      }
    }
  }
  rows[[length(rows) + 1L]] <- flush(cur_s, cur_e)
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

# Active event grammar HF-spiking state detector.  This is deliberately state-oriented:
# it builds candidates from sustained high-frequency support runs and merges
# adjacent support runs across a small number of moderate gaps.  It does NOT
# require every ISI in the epoch to be under the burst-core interval.
stpd_event_core_detect_hf_spiking <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat)
  if (n <= 2) return(data.frame())

  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  manual_lab <- if ("pattern_manual" %in% names(dat)) as.character(dat$pattern_manual) else rep("", n)
  manual_lab[is.na(manual_lab)] <- ""
  honor_manual <- isTRUE((params$detector %||% list())$honor_manual_lock_for_auto %||% TRUE)
  manual_burst_lock <- isTRUE(honor_manual) & manual_lab %in% c("burst", "long_burst", "possible_burst")
  valid_for_hf <- valid & !manual_burst_lock

  short_upper <- suppressWarnings(as.numeric(vp$hf_spiking_short_upper %||% vp$hf_spiking_q90_max %||% 0.020))
  q80_max <- suppressWarnings(as.numeric(vp$hf_spiking_q80_max %||% vp$hf_spiking_q90_max %||% 0.025))
  q90_max <- suppressWarnings(as.numeric(vp$hf_spiking_q90_max %||% 0.025))
  epoch_bridge <- suppressWarnings(as.numeric(vp$hf_spiking_epoch_bridge %||% 0.035))
  if (!is.finite(short_upper) || short_upper <= 0) short_upper <- 0.020
  if (!is.finite(q90_max) || q90_max <= 0) q90_max <- max(short_upper, 0.025)
  if (!is.finite(q80_max) || q80_max <= 0) q80_max <- q90_max
  if (!is.finite(epoch_bridge) || epoch_bridge <= 0) epoch_bridge <- max(0.035, q90_max)

  pause_break <- suppressWarnings(as.numeric(vp$pause_thr %||% NA_real_))[1]
  if (!is.finite(pause_break) || pause_break <= 0) pause_break <- NA_real_

  hp <- params$highfreq %||% list()
  tolerated_gap <- suppressWarnings(as.numeric(hp$spiking_tolerated_gap_ISI_sec %||% NA_real_))
  if (!is.finite(tolerated_gap) || tolerated_gap <= 0) {
    tolerated_gap <- max(0.060, min(0.120, max(2.0 * epoch_bridge, 2.5 * q90_max, na.rm = TRUE)), na.rm = TRUE)
  }
  hfs_lim <- stpd_pattern_isi_limits_for_label("high_frequency_spiking", params)
  hfs_max_sec <- suppressWarnings(as.numeric(hfs_lim$max_sec %||% NA_real_))[1]
  if (!is.finite(hfs_max_sec) || hfs_max_sec <= 0) hfs_max_sec <- NA_real_
  if (is.finite(hfs_max_sec)) {
    tolerated_gap <- min(tolerated_gap, hfs_max_sec)
  }
  vp_hard_break <- suppressWarnings(as.numeric(vp$hf_spiking_hard_break %||% vp$hf_spiking_break_isi %||% NA_real_))[1]
  hard_candidates <- c(pause_break, hfs_max_sec, vp_hard_break)
  hard_candidates <- hard_candidates[is.finite(hard_candidates) & hard_candidates > 0]
  hard_break <- if (length(hard_candidates) > 0) min(hard_candidates, na.rm = TRUE) else NA_real_
  max_gap_count <- max(1L, as.integer(vp$hf_spiking_max_consec_large %||% hp$spiking_max_consecutive_large_isi %||% 3L))
  min_spikes <- max(3L, as.integer(vp$hf_spiking_min_spikes %||% 30L))

  support_flag <- valid_for_hf & isi <= epoch_bridge
  if (is.finite(hfs_max_sec)) support_flag <- support_flag & isi <= hfs_max_sec
  if (is.finite(pause_break)) support_flag <- support_flag & isi < pause_break
  support_runs <- stpd_event_core_bool_runs(support_flag)
  if (nrow(support_runs) == 0) return(data.frame())
  artifact_gap <- is.finite(isi) & art & isi >= 0 & isi <= max(min_isi_sec, min_isi_sec + 1e-12)
  runs <- stpd_event_grammar_merge_hf_support_runs(
    support_runs, isi, valid_for_hf, tolerated_gap, max_gap_count,
    hard_break = hard_break, transparent_gap = artifact_gap,
    transparent_gap_short_side_n = min_spikes - 1L
  )
  if (nrow(runs) == 0) return(data.frame())

  min_duration <- suppressWarnings(as.numeric(vp$hf_spiking_min_duration %||% 0))
  if (!is.finite(min_duration)) min_duration <- 0
  short_frac_min <- min(max(suppressWarnings(as.numeric(vp$hf_spiking_short_fraction_min %||% 0.70)), 0.1), 1)
  allowed_large <- min(max(suppressWarnings(as.numeric(vp$hf_spiking_allowed_large_frac %||% 0.25)), 0), 1)
  # Be slightly tolerant by default; HF spiking is a long state and should allow
  # a minority of 20-25 ms or even moderate gaps when the majority remains fast.
  allowed_large_eff <- max(allowed_large, 0.30)
  max_consec_large_eff <- max(2L, as.integer(vp$hf_spiking_max_consec_large %||% 3L))

  rows <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    idx <- stpd_event_grammar_safe_seq(s, e)
    if (length(idx) == 0) next
    if (any(manual_burst_lock[idx], na.rm = TRUE)) next
    vals <- isi[idx][valid_for_hf[idx]]
    if (length(vals) == 0) next
    if (is.finite(hfs_max_sec) && any(vals > hfs_max_sec, na.rm = TRUE)) next

    n_spikes <- e - s + 2L
    if (n_spikes < min_spikes) next

    duration <- if ("timestamp_sec" %in% names(dat) && s > 1L && e <= n) {
      suppressWarnings(as.numeric(dat$timestamp_sec[e]) - as.numeric(dat$timestamp_sec[s - 1L]))
    } else NA_real_
    if (min_duration > 0 && (!is.finite(duration) || duration < min_duration)) next

    q50 <- stpd_event_core_quantile(vals, 0.50)
    q80 <- stpd_event_core_quantile(vals, 0.80)
    q90 <- stpd_event_core_quantile(vals, 0.90)
    q95 <- stpd_event_core_quantile(vals, 0.95)

    short_frac <- mean(vals <= short_upper, na.rm = TRUE)
    q90_short_frac <- mean(vals <= q90_max, na.rm = TRUE)
    bridge_frac <- mean(vals <= epoch_bridge, na.rm = TRUE)
    large_flag <- vals > epoch_bridge
    large_frac <- mean(large_flag, na.rm = TRUE)
    max_consec_large <- stpd_event_core_max_consecutive_true(large_flag)
    tolerated_frac <- mean(vals <= tolerated_gap, na.rm = TRUE)

    strict_q90_pass <- is.finite(q90) && q90 <= q90_max
    robust_q80_pass <- is.finite(q80) && q80 <= q80_max
    majority_pass <- (short_frac >= max(0.50, short_frac_min - 0.15)) ||
      (q90_short_frac >= max(0.60, short_frac_min - 0.10)) ||
      (bridge_frac >= max(0.75, short_frac_min))
    state_compact_pass <- strict_q90_pass || (robust_q80_pass && majority_pass)
    gap_tolerance_pass <- tolerated_frac >= 0.95
    large_pass <- large_frac <= allowed_large_eff && max_consec_large <= max_consec_large_eff

    pass <- state_compact_pass && gap_tolerance_pass && large_pass
    if (!pass) next

    score <- 24 + 0.08 * n_spikes + 2.5 * short_frac + 2.0 * q90_short_frac +
      1.5 * bridge_frac - 2.0 * large_frac + ifelse(strict_q90_pass, 1.0, 0.4)
    counter <- counter + 1L
    row <- stpd_event_core_candidate_from_run(
      dat, s, e, params, vp, min_isi_sec, train,
      "event_grammar_hf_spiking_state", "event_grammar_long_hf_spiking_epoch", "high_frequency_spiking",
      "event_grammar_hf_spiking_state_pass", "merged_support_run_long_high_frequency_state", "accept",
      score, 1040,
      list(
        candidate_id = paste0("event_grammar_hfs_state_", counter),
        hf_spiking_q50_sec = q50,
        hf_spiking_q80_sec = q80,
        hf_spiking_q90_sec = q90,
        hf_spiking_q95_sec = q95,
        hf_spiking_short_upper_sec = short_upper,
        hf_spiking_q80_max_sec = q80_max,
        hf_spiking_q90_max_sec = q90_max,
        hf_spiking_epoch_bridge_sec = epoch_bridge,
        hf_spiking_tolerated_gap_sec = tolerated_gap,
        hf_spiking_pattern_max_ISI_sec = hfs_max_sec,
        hf_spiking_pause_break_sec = pause_break,
        hf_spiking_short_fraction = short_frac,
        hf_spiking_q90_short_fraction = q90_short_frac,
        hf_spiking_bridge_fraction = bridge_frac,
        hf_spiking_large_fraction = large_frac,
        hf_spiking_tolerated_fraction = tolerated_frac,
        hf_spiking_max_consecutive_large_isi = max_consec_large,
        hf_spiking_min_spikes_required = min_spikes,
        hf_spiking_acceptance_route = if (strict_q90_pass) "strict_q90" else "robust_q80_majority_state"
      )
    )
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_event_grammar_interval_union_stats <- function(starts, ends, span_start, span_end) {
  starts <- suppressWarnings(as.integer(starts))
  ends <- suppressWarnings(as.integer(ends))
  span_start <- suppressWarnings(as.integer(span_start))
  span_end <- suppressWarnings(as.integer(span_end))
  if (!is.finite(span_start) || !is.finite(span_end) || span_end < span_start) {
    return(list(group_count = 0L, covered_n = 0L, coverage = 0))
  }
  ok <- is.finite(starts) & is.finite(ends) & ends >= starts
  starts <- pmax(starts[ok], span_start)
  ends <- pmin(ends[ok], span_end)
  ok <- is.finite(starts) & is.finite(ends) & ends >= starts
  starts <- starts[ok]
  ends <- ends[ok]
  if (length(starts) == 0) return(list(group_count = 0L, covered_n = 0L, coverage = 0))

  ord <- order(starts, ends)
  starts <- starts[ord]
  ends <- ends[ord]
  group_count <- 0L
  covered_n <- 0L
  cur_start <- starts[1]
  cur_end <- ends[1]
  if (length(starts) > 1L) {
    for (i in 2:length(starts)) {
      if (starts[i] <= cur_end) {
        cur_end <- max(cur_end, ends[i], na.rm = TRUE)
      } else {
        group_count <- group_count + 1L
        covered_n <- covered_n + cur_end - cur_start + 1L
        cur_start <- starts[i]
        cur_end <- ends[i]
      }
    }
  }
  group_count <- group_count + 1L
  covered_n <- covered_n + cur_end - cur_start + 1L
  span_n <- span_end - span_start + 1L
  list(group_count = group_count, covered_n = covered_n, coverage = covered_n / max(1L, span_n))
}

stpd_event_grammar_reject_burst_dominated_hf_spiking_states <- function(audit) {
  if (is.null(audit) || nrow(audit) == 0 || !("final_label" %in% names(audit))) return(audit)
  n <- nrow(audit)
  starts <- suppressWarnings(as.integer(audit$start_isi))
  ends <- suppressWarnings(as.integer(audit$end_isi))
  lab <- as.character(audit$final_label %||% rep("", n))
  lab[is.na(lab)] <- ""
  suppressed_lab <- as.character(audit$suppressed_original_label %||% rep("", n))
  suppressed_lab[is.na(suppressed_lab)] <- ""
  effective_lab <- ifelse(nzchar(suppressed_lab), suppressed_lab, lab)

  if (!("hf_spiking_burst_dominated" %in% names(audit))) audit$hf_spiking_burst_dominated <- FALSE
  if (!("hf_spiking_embedded_burst_count" %in% names(audit))) audit$hf_spiking_embedded_burst_count <- NA_integer_
  if (!("hf_spiking_embedded_burst_group_count" %in% names(audit))) audit$hf_spiking_embedded_burst_group_count <- NA_integer_
  if (!("hf_spiking_embedded_burst_coverage" %in% names(audit))) audit$hf_spiking_embedded_burst_coverage <- NA_real_
  if (!("hf_spiking_burst_packet_like" %in% names(audit))) audit$hf_spiking_burst_packet_like <- FALSE
  if (!("hf_spiking_burst_packet_neighbor" %in% names(audit))) audit$hf_spiking_burst_packet_neighbor <- FALSE
  if (!("candidate_diagnostic_class" %in% names(audit))) audit$candidate_diagnostic_class <- ""

  hfs_idx <- which(lab == "high_frequency_spiking")
  if (length(hfs_idx) == 0) return(audit)

  for (hi in hfs_idx) {
    hs <- starts[hi]
    he <- ends[hi]
    if (!is.finite(hs) || !is.finite(he) || he < hs) next
    h_n_isi <- he - hs + 1L
    embedded <- which(
      seq_len(n) != hi &
        effective_lab %in% c("burst", "long_burst") &
        is.finite(starts) & is.finite(ends) &
        starts >= hs & ends <= he
    )
    stats <- stpd_event_grammar_interval_union_stats(starts[embedded], ends[embedded], hs, he)
    burst_count <- length(embedded)
    burst_groups <- stats$group_count
    burst_coverage <- stats$coverage
    min_groups <- max(6L, as.integer(ceiling(0.055 * h_n_isi)))
    burst_dominated <- burst_groups >= min_groups && burst_coverage >= 0.25
    h_cv <- suppressWarnings(as.numeric(audit$CV[hi] %||% NA_real_))
    h_lv <- suppressWarnings(as.numeric(audit$LV[hi] %||% NA_real_))
    h_mm <- suppressWarnings(as.numeric(audit$MM[hi] %||% NA_real_))
    h_large <- suppressWarnings(as.numeric(audit$hf_spiking_large_fraction[hi] %||% NA_real_))
    variable_hf <- (is.finite(h_cv) && h_cv >= 0.65) ||
      (is.finite(h_lv) && h_lv >= 0.45) ||
      (is.finite(h_mm) && h_mm >= 3.0) ||
      (is.finite(h_large) && h_large >= 0.08)
    burst_packet_like <- isTRUE(variable_hf) &&
      ((burst_groups >= 2L && burst_coverage >= 0.08) ||
        (burst_groups >= 1L && burst_coverage >= 0.18))

    audit$hf_spiking_embedded_burst_count[hi] <- burst_count
    audit$hf_spiking_embedded_burst_group_count[hi] <- burst_groups
    audit$hf_spiking_embedded_burst_coverage[hi] <- burst_coverage
    audit$hf_spiking_burst_packet_like[hi] <- isTRUE(burst_packet_like)
    audit$hf_spiking_burst_dominated[hi] <- isTRUE(burst_dominated)

    if (isTRUE(burst_dominated) || isTRUE(burst_packet_like)) {
      rejection_tag <- if (isTRUE(burst_dominated)) {
        "reject_burst_dominated_hf_spiking_state"
      } else {
        "reject_burst_packet_like_hf_spiking_state"
      }
      audit$decision_path[hi] <- paste0(
        as.character(audit$decision_path[hi] %||% ""),
        ";", rejection_tag
      )
      audit$action[hi] <- rejection_tag
      audit$final_label[hi] <- "reject"
      audit$class[hi] <- "reject"
      audit$candidate_diagnostic_class[hi] <- paste0("rejected__", rejection_tag)
    }
  }

  packet_idx <- hfs_idx[as.logical(audit$hf_spiking_burst_packet_like[hfs_idx] %||% FALSE)]
  packet_idx <- packet_idx[!is.na(packet_idx)]
  remaining_hfs <- hfs_idx[as.character(audit$final_label[hfs_idx] %||% "") == "high_frequency_spiking"]
  if (length(packet_idx) > 0 && length(remaining_hfs) > 0) {
    for (hi in remaining_hfs) {
      hs <- starts[hi]
      he <- ends[hi]
      if (!is.finite(hs) || !is.finite(he)) next
      h_cv <- suppressWarnings(as.numeric(audit$CV[hi] %||% NA_real_))
      h_lv <- suppressWarnings(as.numeric(audit$LV[hi] %||% NA_real_))
      h_mm <- suppressWarnings(as.numeric(audit$MM[hi] %||% NA_real_))
      variable_hf <- (is.finite(h_cv) && h_cv >= 0.65) ||
        (is.finite(h_lv) && h_lv >= 0.45) ||
        (is.finite(h_mm) && h_mm >= 3.0)
      if (!isTRUE(variable_hf)) next
      neighbor <- FALSE
      for (pj in packet_idx) {
        ps <- starts[pj]
        pe <- ends[pj]
        if (!is.finite(ps) || !is.finite(pe)) next
        gap_n <- if (pe < hs) hs - pe - 1L else if (he < ps) ps - he - 1L else 0L
        if (!is.finite(gap_n) || gap_n < 0L || gap_n > 3L) next
        gap_sec <- if (pe < hs) {
          suppressWarnings(as.numeric(audit$pre_gap_sec[hi] %||% audit$post_gap_sec[pj] %||% NA_real_))
        } else if (he < ps) {
          suppressWarnings(as.numeric(audit$post_gap_sec[hi] %||% audit$pre_gap_sec[pj] %||% NA_real_))
        } else {
          0
        }
        tol <- suppressWarnings(max(as.numeric(c(
          audit$hf_spiking_tolerated_gap_sec[hi] %||% NA_real_,
          audit$hf_spiking_tolerated_gap_sec[pj] %||% NA_real_
        )), na.rm = TRUE))
        if (!is.finite(tol) || tol <= 0) tol <- 0.075
        if (is.finite(gap_sec) && gap_sec <= tol) {
          neighbor <- TRUE
          break
        }
      }
      if (isTRUE(neighbor)) {
        audit$hf_spiking_burst_packet_neighbor[hi] <- TRUE
        audit$decision_path[hi] <- paste0(
          as.character(audit$decision_path[hi] %||% ""),
          ";reject_burst_packet_neighbor_hf_spiking_state"
        )
        audit$action[hi] <- "reject_burst_packet_neighbor_hf_spiking_state"
        audit$final_label[hi] <- "reject"
        audit$class[hi] <- "reject"
        audit$candidate_diagnostic_class[hi] <- "rejected__reject_burst_packet_neighbor_hf_spiking_state"
      }
    }
  }
  audit
}

# Suppress subordinate labels inside an accepted long HF-spiking state before
# weighted interval selection. Compact burst-like kernels can occur inside a
# sustained HF state, but they should not delete the state-level annotation in a
# single-label AUTO layer. If the putative HF state is itself dominated by many
# independent canonical burst packets, the HF candidate is rejected first so the
# burst event layer remains visible.
stpd_event_grammar_protect_hf_spiking_states <- function(audit) {
  if (is.null(audit) || nrow(audit) == 0 || !("final_label" %in% names(audit))) return(audit)
  audit <- stpd_event_grammar_reject_burst_dominated_hf_spiking_states(audit)
  lab <- as.character(audit$final_label); lab[is.na(lab)] <- ""
  hfs_idx <- which(lab == "high_frequency_spiking")
  if (length(hfs_idx) == 0) return(audit)

  starts <- suppressWarnings(as.integer(audit$start_isi))
  ends <- suppressWarnings(as.integer(audit$end_isi))
  suppressible <- c("possible_burst", "high_frequency_tonic", "tonic", "pause")
  if (!("suppressed_by_hf_spiking_state" %in% names(audit))) audit$suppressed_by_hf_spiking_state <- FALSE
  if (!("suppressed_original_label" %in% names(audit))) audit$suppressed_original_label <- ""

  suppress_rows <- function(rows, suffix, action = "suppress_for_hf_spiking_state") {
    rows <- rows[is.finite(rows) & rows >= 1L & rows <= nrow(audit)]
    if (length(rows) == 0) return(invisible(NULL))
    audit$suppressed_by_hf_spiking_state[rows] <<- TRUE
    audit$suppressed_original_label[rows] <<- lab[rows]
    audit$decision_path[rows] <<- paste0(as.character(audit$decision_path[rows] %||% ""), suffix)
    audit$selection_status[rows] <<- "suppressed_by_hf_spiking_state_candidate"
    audit$final_label[rows] <<- "reject"
    audit$class[rows] <<- "reject"
    audit$action[rows] <<- action
    invisible(NULL)
  }

  for (hi in hfs_idx) {
    hs <- starts[hi]; he <- ends[hi]
    if (!is.finite(hs) || !is.finite(he)) next
    h_n_isi <- he - hs + 1L
    h_short <- suppressWarnings(as.numeric(audit$hf_spiking_short_fraction[hi] %||% NA_real_))
    h_bridge <- suppressWarnings(as.numeric(audit$hf_spiking_bridge_fraction[hi] %||% NA_real_))
    h_q90 <- suppressWarnings(as.numeric(audit$hf_spiking_q90_sec[hi] %||% NA_real_))
    h_q90_max <- suppressWarnings(as.numeric(audit$hf_spiking_q90_max_sec[hi] %||% NA_real_))
    sustained_hf_state <- is.finite(h_n_isi) && h_n_isi >= 80L &&
      (!is.finite(h_short) || h_short >= 0.70) &&
      (!is.finite(h_bridge) || h_bridge >= 0.90) &&
      (!is.finite(h_q90) || !is.finite(h_q90_max) || h_q90 <= h_q90_max)
    compact_pure_hf_state <- is.finite(h_n_isi) && h_n_isi >= 30L &&
      is.finite(h_short) && h_short >= 0.85 &&
      is.finite(h_bridge) && h_bridge >= 0.95 &&
      (!is.finite(h_q90) || !is.finite(h_q90_max) || h_q90 <= h_q90_max)
    strong_hf_state <- isTRUE(sustained_hf_state) || isTRUE(compact_pure_hf_state)

    ov <- which(seq_len(nrow(audit)) != hi & lab %in% suppressible & is.finite(starts) & is.finite(ends) & starts <= he & ends >= hs)
    suppress_rows(ov, ";suppressed_by_long_hf_spiking_state")

    if (isTRUE(strong_hf_state)) {
      embedded_events <- which(
        seq_len(nrow(audit)) != hi &
          lab %in% c("burst", "long_burst") &
          is.finite(starts) & is.finite(ends) &
          starts >= hs & ends <= he
      )
      if (length(embedded_events) > 0) {
        embedded_n <- ends[embedded_events] - starts[embedded_events] + 1L
        max_embedded_n <- max(8L, as.integer(ceiling(0.06 * h_n_isi)))
        embedded_duration <- suppressWarnings(as.numeric(audit$duration_sec[embedded_events] %||% NA_real_))
        compact <- is.finite(embedded_n) & embedded_n <= max_embedded_n
        if (length(embedded_duration) == length(embedded_events)) {
          compact <- compact & (!is.finite(embedded_duration) | embedded_duration <= 0.25)
        }
        suppress_rows(
          embedded_events[compact],
          ";compact_burst_kernel_suppressed_inside_long_hf_spiking_state",
          action = "suppress_embedded_burst_for_hf_spiking_state"
        )
      }
    }
  }
  audit
}

stpd_event_core_candidate_value_registry <- function() {
  list(
    label_base = stpd_event_core_candidate_value_label_base,
    priority_aware = stpd_event_core_candidate_value_priority_aware,
    possible_burst_dynamic = stpd_event_core_candidate_value_possible_burst_dynamic,
    explicit_priority = stpd_event_core_candidate_value_explicit_priority,
    hf_protected = stpd_event_core_candidate_value
  )
}

# Formal weighted-interval value function. Long HF-spiking states receive
# span-aware value so they are not fragmented into many low-specificity
# possible_burst/tonic/pause candidates.
stpd_event_core_candidate_value <- function(row) {
  explicit <- stpd_event_grammar_num(row$priority[1], NA_real_)
  lab <- as.character(row$final_label[1] %||% "")
  sc <- stpd_event_grammar_num(row$score[1], 0)
  n_isi <- stpd_event_grammar_num(row$n_isi[1], 0)
  if (!is.finite(sc)) sc <- 0
  if (!is.finite(n_isi)) n_isi <- 0

  layer <- as.character(row$candidate_layer[1] %||% "")
  if (grepl("^isi_profile_hard_threshold", layer)) {
    pri <- switch(lab,
      burst = 1800,
      # Hard burst thresholds should rescue compact burst packets, but a
      # valid long HF-spiking state should still win over a threshold-only
      # long_burst candidate on the same span.
      long_burst = 980,
      pause = 1500,
      tonic = 900,
      high_frequency_tonic = 900,
      high_frequency_spiking = 900,
      800
    )
    span_bonus <- if (identical(lab, "burst")) {
      2500000 * min(n_isi, 80)
    } else if (identical(lab, "long_burst")) {
      20000 * min(n_isi, 300)
    } else {
      5000 * min(n_isi, 300)
    }
    return(pri * 10000 + 180 * sc + span_bonus)
  }

  if (identical(lab, "high_frequency_spiking")) {
    # Span-aware value: protect long state candidates from being replaced by
    # many small review fragments.
    return(1040 * 10000 + 180 * sc + 45000 * min(n_isi, 300))
  }
  if (lab %in% c("burst", "long_burst") && identical(layer, "event_grammar_burst_episode")) {
    # Dense burst episodes should beat a tiling of several tiny burst fragments
    # inside the same visually coherent cluster.
    base <- if (identical(lab, "burst")) 1250 else 1160
    return(base * 10000 + 180 * sc + 2500000 * min(n_isi, 80))
  }
  if (identical(lab, "possible_burst")) {
    explicit <- if (is.finite(explicit) && explicit > 0) min(explicit, 520) else 320
  }
  if (is.finite(explicit) && explicit > 0) return(explicit * 10000 + 100 * sc + n_isi)

  pri <- switch(lab,
    burst = 1250,
    long_burst = 1160,
    high_frequency_tonic = 560,
    tonic = 420,
    pause = 320,
    possible_burst = 280,
    0
  )
  pri * 10000 + 100 * sc + n_isi
}

# Active event grammar detector entry point.  Manual labels no longer block AUTO evidence;
# pattern_auto is computed independently, while final-label logic elsewhere can
# remain manual-dominant.  This is essential for long HF states that contain a few
# manual annotations inside the same epoch.
stpd_detect_train_hf_protected_impl <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  n <- nrow(dat)
  if (!("pattern_manual_negative" %in% names(dat))) dat$pattern_manual_negative <- rep("", n)
  if (!("auto_score" %in% names(dat))) dat$auto_score <- rep(NA_real_, n)
  if (n <= 1) { dat$pattern_auto <- ""; dat$auto_score <- NA_real_; return(dat) }

  vp <- stpd_event_grammar_params_impl(dat, params, min_isi_sec, train = train)
  manual_for_lock <- if (!is.null(dat$pattern_manual)) as.character(dat$pattern_manual) else rep("", n)
  manual_for_lock[is.na(manual_for_lock)] <- ""
  patterns <- params$detector$patterns_to_run %||% stpd_default_patterns_to_run()

  profile <- stpd_event_core_train_profile_row(dat, params, vp, min_isi_sec, train)
  cand_rows <- list(profile)
  hard_thr <- stpd_event_core_detect_hard_isi_thresholds(dat, params, vp, min_isi_sec, train)
  if (nrow(hard_thr) > 0) cand_rows[[length(cand_rows) + 1L]] <- hard_thr
  if (any(c("burst", "long_burst") %in% patterns)) {
    b <- stpd_event_grammar_detect_burst_events(dat, params, vp, min_isi_sec, train)
    if (nrow(b) > 0) cand_rows[[length(cand_rows) + 1L]] <- b
  }
  if ("high_frequency_spiking" %in% patterns) {
    hfs <- stpd_event_core_detect_hf_spiking(dat, params, vp, min_isi_sec, train)
    if (nrow(hfs) > 0) cand_rows[[length(cand_rows) + 1L]] <- hfs
  }
  if ("high_frequency_tonic" %in% patterns) {
    hft <- stpd_event_core_detect_hf_tonic(dat, params, vp, min_isi_sec, train)
    if (nrow(hft) > 0) cand_rows[[length(cand_rows) + 1L]] <- hft
  }
  if ("tonic" %in% patterns) {
    ton <- stpd_event_core_detect_tonic(dat, params, vp, min_isi_sec, train)
    if (nrow(ton) > 0) cand_rows[[length(cand_rows) + 1L]] <- ton
  }
  if ("pause" %in% patterns) {
    pau <- stpd_event_core_detect_pause(dat, params, vp, min_isi_sec, train)
    if (nrow(pau) > 0) cand_rows[[length(cand_rows) + 1L]] <- pau
  }

  audit <- dplyr::bind_rows(cand_rows)
  audit <- stpd_event_grammar_protect_hf_spiking_states(audit)
  audit <- stpd_event_core_weighted_select(audit, locked = NULL, patterns = patterns)

  pat <- rep("", n); score <- rep(NA_real_, n)
  if (nrow(audit) > 0) {
    sel_flag <- as.logical(audit$selected_for_auto); sel_flag[is.na(sel_flag)] <- FALSE
    selected <- audit[sel_flag, , drop = FALSE]
    if (nrow(selected) > 0) {
      selected <- selected[order(suppressWarnings(as.integer(selected$start_isi))), , drop = FALSE]
      for (i in seq_len(nrow(selected))) {
        lab <- as.character(selected$final_label[i] %||% "")
        s <- suppressWarnings(as.integer(selected$start_isi[i])); e <- suppressWarnings(as.integer(selected$end_isi[i]))
        if (!nzchar(lab) || lab %in% c("reject", "profile") || !is.finite(s) || !is.finite(e) || e < s || s < 2L || e > n) next
        idx <- s:e
        idx <- idx[pat[idx] == ""]
        if (length(idx) == 0) next
        pat[idx] <- lab
        score[idx] <- suppressWarnings(as.numeric(selected$score[i] %||% NA_real_))
      }
    }
  }

  if ("others" %in% patterns && isTRUE(params$detector$fill_others_auto %||% FALSE)) {
    isi <- suppressWarnings(as.numeric(dat$ISI_sec)); art <- is_artifact_isi(isi, min_isi_sec)
    fill_idx <- which(seq_len(n) >= 2L & is.finite(isi) & !art & pat == "")
    pat[fill_idx] <- "others"
  }

  dat$pattern_auto <- pat
  dat$auto_score <- score
  # Validate AUTO fragments.  We pass lock_manual = FALSE because AUTO evidence is
  # now intentionally independent of manual labels; manual labels can still govern
  # FINAL labels downstream.
  dat <- stpd_post_validate_auto_event_sizes(dat, params, min_isi_sec = min_isi_sec, lock_manual = FALSE, train = train)
  attr(dat, "candidate_diagnostic_audit") <- audit
  attr(dat, "event_grammar_params") <- vp
  dat
}

stpd_detect_train_hf_protected <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  if (isTRUE((params$event_grammar %||% list())$enabled %||% TRUE)) {
    return(stpd_detect_train_product_hardened(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual))
  }
  stpd_train_pipeline_event_grammar_core(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}
