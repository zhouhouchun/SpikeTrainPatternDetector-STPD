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
#     (3) in label-blind mode, manual labels must be removed upstream so they cannot
#         block a long HF candidate.  The legacy manual-aware mode still honors its
#         generation-time manual lock; fully track-scoped manual coexistence is a
#         later migration phase, while FINAL labels remain manual-dominant downstream.
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
      if (gap_ok && length(gap_bridgeable) > 0) {
        connector_tolerance <- max(1e-12, abs(tolerated_gap) * 1e-9)
        gap_ok <- all(
          isi[gap_bridgeable] <= tolerated_gap + connector_tolerance,
          na.rm = TRUE
        )
      }
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

stpd_event_grammar_probability_faster <- function(candidate, background) {
  candidate <- suppressWarnings(as.numeric(candidate))
  background <- suppressWarnings(as.numeric(background))
  candidate <- candidate[is.finite(candidate)]
  background <- background[is.finite(background)]
  if (!length(candidate) || !length(background)) return(NA_real_)
  comparison <- outer(candidate, background, FUN = "-")
  mean(comparison < 0) + 0.5 * mean(comparison == 0)
}

# Expand q25-like fast-core anchors only across the contiguous resolved
# proposal envelope (stable lower-tail valley when identifiable, otherwise
# bounded q75). The fast core is evidence that an HFS candidate exists; the
# proposal envelope is its direct support. A failed boundary expansion is reverted in
# one step to the pre-expansion anchor, so the accepted boundary is the point at
# which invasion began rather than a later point inside the invaded State.
stpd_event_grammar_hf_seed_anchored_envelope_runs <- function(
    core_flag, envelope_flag, isi, valid, envelope_upper,
    min_core_count = 3L, min_core_fraction = 0.30,
    min_envelope_fraction = 0.75,
    q80_ratio_max = 1.00, q90_ratio_max = 1.50,
    max_unanchored_boundary_isi = 5L,
    tonic_like_flag = NULL, tonic_upshift_ratio_min = 1.35) {
  core_flag <- as.logical(core_flag)
  envelope_flag <- as.logical(envelope_flag)
  core_flag[is.na(core_flag)] <- FALSE
  envelope_flag[is.na(envelope_flag)] <- FALSE
  min_core_count <- max(1L, as.integer(min_core_count %||% 3L))
  min_core_fraction <- min(max(as.numeric(min_core_fraction %||% 0.30), 0), 1)
  min_envelope_fraction <- min(max(
    as.numeric(min_envelope_fraction %||% 0.75), 0), 1)
  q80_ratio_max <- max(1, as.numeric(q80_ratio_max %||% 1.00))
  q90_ratio_max <- max(q80_ratio_max, as.numeric(q90_ratio_max %||% 1.50))
  max_unanchored_boundary_isi <- max(
    0L, as.integer(max_unanchored_boundary_isi %||% 5L)
  )
  if (is.null(tonic_like_flag)) {
    tonic_like_flag <- rep(FALSE, length(core_flag))
  } else {
    tonic_like_flag <- as.logical(tonic_like_flag)
    tonic_like_flag[is.na(tonic_like_flag)] <- FALSE
    length(tonic_like_flag) <- length(core_flag)
    tonic_like_flag[is.na(tonic_like_flag)] <- FALSE
  }
  tonic_upshift_ratio_min <- suppressWarnings(as.numeric(
    tonic_upshift_ratio_min %||% 1.35
  ))[1L]
  if (!is.finite(tonic_upshift_ratio_min) ||
      tonic_upshift_ratio_min < 1) {
    tonic_upshift_ratio_min <- 1.35
  }
  if (!is.finite(envelope_upper) || envelope_upper <= 0) return(data.frame())

  envelope_runs <- stpd_event_core_bool_runs(envelope_flag)
  if (nrow(envelope_runs) == 0L) return(data.frame())

  evaluate_bounds <- function(s, e) {
    idx <- stpd_event_grammar_safe_seq(s, e)
    idx <- idx[idx >= 1L & idx <= length(isi) & valid[idx]]
    vals <- isi[idx]
    vals <- vals[is.finite(vals)]
    n_valid <- length(vals)
    core_count <- if (length(idx)) sum(core_flag[idx], na.rm = TRUE) else 0L
    core_fraction <- core_count / max(1L, n_valid)
    envelope_fraction <- if (n_valid) {
      mean(vals <= envelope_upper, na.rm = TRUE)
    } else 0
    q80 <- if (n_valid) stpd_event_core_quantile(vals, 0.80) else NA_real_
    q90 <- if (n_valid) stpd_event_core_quantile(vals, 0.90) else NA_real_
    q80_ratio <- if (is.finite(q80)) q80 / envelope_upper else Inf
    q90_ratio <- if (is.finite(q90)) q90 / envelope_upper else Inf
    pass <- n_valid > 0L && core_count >= min_core_count &&
      core_fraction >= min_core_fraction &&
      envelope_fraction >= min_envelope_fraction &&
      q80_ratio <= q80_ratio_max && q90_ratio <= q90_ratio_max
    list(
      pass = isTRUE(pass), n_valid = n_valid,
      core_count = as.integer(core_count), core_fraction = core_fraction,
      envelope_fraction = envelope_fraction,
      q80 = q80, q90 = q90,
      q80_ratio = q80_ratio, q90_ratio = q90_ratio
    )
  }

  rows <- list()
  for (rr in seq_len(nrow(envelope_runs))) {
    pre_s <- as.integer(envelope_runs$start_isi[rr])
    pre_e <- as.integer(envelope_runs$end_isi[rr])
    pre_idx <- stpd_event_grammar_safe_seq(pre_s, pre_e)
    core_idx <- pre_idx[core_flag[pre_idx]]
    if (!length(core_idx)) next
    split_after <- rep(FALSE, max(0L, length(core_idx) - 1L))
    split_start <- rep(NA_integer_, length(split_after))
    split_end <- rep(NA_integer_, length(split_after))
    if (length(core_idx) >= 2L) {
      for (ii in seq_len(length(core_idx) - 1L)) {
        gap_idx <- stpd_event_grammar_safe_seq(
          core_idx[ii] + 1L, core_idx[ii + 1L] - 1L
        )
        if (length(gap_idx) <= max_unanchored_boundary_isi) next
        tonic_fraction <- mean(tonic_like_flag[gap_idx], na.rm = TRUE)
        if (!is.finite(tonic_fraction) || tonic_fraction < 0.80) next
        ref_core <- unique(c(
          tail(core_idx[core_idx <= core_idx[ii]], min_core_count),
          head(core_idx[core_idx >= core_idx[ii + 1L]], min_core_count)
        ))
        gap_median <- stats::median(isi[gap_idx], na.rm = TRUE)
        reference_median <- stats::median(isi[ref_core], na.rm = TRUE)
        upshift_ratio <- gap_median / reference_median
        if (!is.finite(upshift_ratio) ||
            upshift_ratio < tonic_upshift_ratio_min) next
        split_after[ii] <- TRUE
        split_start[ii] <- min(gap_idx)
        split_end[ii] <- max(gap_idx)
      }
    }
    core_group <- cumsum(c(1L, as.integer(split_after)))
    group_ids <- unique(core_group)
    for (gg in seq_along(group_ids)) {
      group_core <- core_idx[core_group == group_ids[gg]]
      anchor_s <- min(group_core)
      anchor_e <- max(group_core)
      anchor_eval <- evaluate_bounds(anchor_s, anchor_e)
      if (!isTRUE(anchor_eval$pass)) next

      group_pre_s <- if (gg == 1L) pre_s else anchor_s
      group_pre_e <- if (gg == length(group_ids)) pre_e else anchor_e
      preceding_split <- if (gg > 1L) which(split_after)[gg - 1L] else NA_integer_
      following_split <- if (gg <= sum(split_after)) which(split_after)[gg] else NA_integer_
      internal_split <- if (is.finite(preceding_split)) {
        preceding_split
      } else following_split
      internal_intrusion_start <- if (is.finite(internal_split)) {
        split_start[internal_split]
      } else NA_integer_
      internal_intrusion_end <- if (is.finite(internal_split)) {
        split_end[internal_split]
      } else NA_integer_

      post_s <- anchor_s
      post_e <- anchor_e
      left_intrusion_end <- NA_integer_
      right_intrusion_start <- NA_integer_
      if (group_pre_s < anchor_s) {
        left_eval <- evaluate_bounds(group_pre_s, post_e)
        left_side_eval <- evaluate_bounds(group_pre_s, anchor_s - 1L)
        left_side_supported <- left_side_eval$n_valid <=
          max_unanchored_boundary_isi ||
          left_side_eval$core_fraction >= min_core_fraction
        if (isTRUE(left_eval$pass) && isTRUE(left_side_supported)) {
          post_s <- group_pre_s
        } else {
          left_intrusion_end <- anchor_s - 1L
        }
      }
      if (group_pre_e > anchor_e) {
        right_eval <- evaluate_bounds(post_s, group_pre_e)
        right_side_eval <- evaluate_bounds(anchor_e + 1L, group_pre_e)
        right_side_supported <- right_side_eval$n_valid <=
          max_unanchored_boundary_isi ||
          right_side_eval$core_fraction >= min_core_fraction
        if (isTRUE(right_eval$pass) && isTRUE(right_side_supported)) {
          post_e <- group_pre_e
        } else {
          right_intrusion_start <- anchor_e + 1L
        }
      }
      final_eval <- evaluate_bounds(post_s, post_e)
      if (!isTRUE(final_eval$pass)) next
      rows[[length(rows) + 1L]] <- data.frame(
        start_isi = as.integer(post_s),
        end_isi = as.integer(post_e),
        anchor_start_isi = as.integer(anchor_s),
        anchor_end_isi = as.integer(anchor_e),
        pre_rollback_start_isi = as.integer(pre_s),
        pre_rollback_end_isi = as.integer(pre_e),
        left_intrusion_end_isi = as.integer(left_intrusion_end),
        right_intrusion_start_isi = as.integer(right_intrusion_start),
        internal_intrusion_start_isi =
          as.integer(internal_intrusion_start),
        internal_intrusion_end_isi = as.integer(internal_intrusion_end),
        rollback_to_intrusion_onset =
          is.finite(left_intrusion_end) ||
          is.finite(right_intrusion_start) ||
          is.finite(internal_intrusion_start),
        fast_core_isi_count = as.integer(final_eval$core_count),
        fast_core_fraction = as.numeric(final_eval$core_fraction),
        envelope_fraction = as.numeric(final_eval$envelope_fraction),
        envelope_q80_ratio = as.numeric(final_eval$q80_ratio),
        envelope_q90_ratio = as.numeric(final_eval$q90_ratio),
        max_unanchored_boundary_isi =
          as.integer(max_unanchored_boundary_isi),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(data.frame())
  dplyr::bind_rows(rows)
}

# Active event grammar HF-spiking state detector.  This is deliberately state-oriented:
# it builds candidates from sustained high-frequency support runs and merges
# adjacent support runs across a small number of moderate gaps.  It does NOT
# require every ISI in the epoch to be under the burst-core interval.
stpd_event_core_detect_hf_spiking <- function(dat, params, vp, min_isi_sec = 0.001,
                                               train = "", hard_boundaries = NULL,
                                               tonic_candidates = NULL) {
  n <- nrow(dat)
  if (n <= 2) return(data.frame())

  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  # Burst is an orthogonal Event overlay. Manual or AUTO Burst annotations may
  # never suppress Broad-HFS candidate generation (D-021/D-022).
  manual_burst_lock <- rep(FALSE, n)
  canonical_boundary_mask <- rep(FALSE, n)
  if (!is.null(hard_boundaries) && nrow(hard_boundaries) > 0L) {
    for (ii in seq_len(nrow(hard_boundaries))) {
      idx <- stpd_event_grammar_safe_seq(
        max(2L, as.integer(hard_boundaries$start_isi[ii])),
        min(n, as.integer(hard_boundaries$end_isi[ii]))
      )
      if (length(idx) > 0L) canonical_boundary_mask[idx] <- TRUE
    }
  }
  tonic_like_flag <- rep(FALSE, n)
  if (is.data.frame(tonic_candidates) && nrow(tonic_candidates) > 0L &&
      all(c("start_isi", "end_isi") %in% names(tonic_candidates))) {
    for (ii in seq_len(nrow(tonic_candidates))) {
      idx <- stpd_event_grammar_safe_seq(
        max(2L, as.integer(tonic_candidates$start_isi[ii])),
        min(n, as.integer(tonic_candidates$end_isi[ii]))
      )
      if (length(idx)) tonic_like_flag[idx] <- TRUE
    }
  }
  seed_lower <- suppressWarnings(as.numeric(
    vp$hf_spiking_seed_lower %||% min_isi_sec
  ))[1L]
  if (!is.finite(seed_lower) || seed_lower < min_isi_sec) {
    seed_lower <- min_isi_sec
  }
  # The lower value is an audit/refractory proposal, not a State veto. ISIs
  # shorter than it can be embedded Burst support inside Broad HFS and must
  # remain bridgeable. Artifact handling is already carried by valid.
  valid_for_hf <- valid & !manual_burst_lock & !canonical_boundary_mask
  hfs_threshold_source <- as.character(
    vp$hf_spiking_threshold_source_mode %||% "auto"
  )[1L]
  auto_requires_local_background <- isTRUE(
    vp$hf_spiking_auto_requires_local_background %||%
      (hfs_threshold_source %in% c("histogram", "auto"))
  )

  short_upper <- suppressWarnings(as.numeric(
    vp$hf_spiking_short_upper %||% vp$hf_spiking_q90_max %||% NA_real_
  ))
  q80_max <- suppressWarnings(as.numeric(
    vp$hf_spiking_q80_max %||% vp$hf_spiking_q90_max %||% NA_real_
  ))
  q90_max <- suppressWarnings(as.numeric(
    vp$hf_spiking_q90_max %||% NA_real_
  ))
  epoch_bridge <- suppressWarnings(as.numeric(
    vp$hf_spiking_epoch_bridge %||% NA_real_
  ))
  fallback_direct <- stpd_seed_bridge_safe_quantile(
    isi[valid_for_hf], 0.25, default = min_isi_sec * 2
  )
  if (!is.finite(short_upper) || short_upper <= 0) {
    short_upper <- fallback_direct
  }
  if (!is.finite(q90_max) || q90_max <= 0) q90_max <- short_upper
  if (!is.finite(q80_max) || q80_max <= 0) q80_max <- q90_max
  if (!is.finite(epoch_bridge) || epoch_bridge <= 0) {
    epoch_bridge <- max(short_upper, q90_max) * 1.50
  }

  # The resolved Pause seed is an audit/calibration input, not a global HFS
  # ceiling.  Only accepted, instance-level canonical Pause boundaries above
  # are removed from direct HFS support.  Contextual inter-burst Pauses are
  # resolved later and therefore cannot shrink the whole train's HFS band.
  pause_seed_audit <- suppressWarnings(as.numeric(vp$pause_thr %||% NA_real_))[1]
  if (!is.finite(pause_seed_audit) || pause_seed_audit <= 0) {
    pause_seed_audit <- NA_real_
  }

  hp <- params$highfreq %||% list()
  homogeneous_q90_candidates <- suppressWarnings(as.numeric(c(
    hp$spiking_q90_max_ISI_sec %||% NA_real_,
    hp$spiking_max_ISI_abs %||% NA_real_
  )))
  homogeneous_q90_candidates <- homogeneous_q90_candidates[
    is.finite(homogeneous_q90_candidates) & homogeneous_q90_candidates > 0
  ]
  homogeneous_q90_ceiling <- if (length(homogeneous_q90_candidates)) {
    min(homogeneous_q90_candidates)
  } else {
    NA_real_
  }
  if (!is.finite(homogeneous_q90_ceiling) ||
      homogeneous_q90_ceiling <= 0) {
    homogeneous_q90_ceiling <- NA_real_
  }
  background_contrast_min <- suppressWarnings(as.numeric(
    hp$spiking_background_contrast_min %||% 1.35
  ))[1L]
  if (!is.finite(background_contrast_min) ||
      background_contrast_min < 1) {
    background_contrast_min <- 1.35
  }
  hfs_lim <- stpd_pattern_isi_limits_for_label("high_frequency_spiking", params)
  hfs_max_sec <- suppressWarnings(as.numeric(hfs_lim$max_sec %||% NA_real_))[1]
  if (!is.finite(hfs_max_sec) || hfs_max_sec <= 0) hfs_max_sec <- NA_real_
  connector_policy <- stpd_hfs_connector_policy(
    params,
    direct_max_sec = hfs_max_sec,
    pause_break_sec = NA_real_,
    epoch_bridge_sec = epoch_bridge,
    use_param_pause_fallback = FALSE
  )
  tolerated_gap <- connector_policy$effective_tolerated_gap_sec
  vp_hard_break <- suppressWarnings(as.numeric(vp$hf_spiking_hard_break %||% vp$hf_spiking_break_isi %||% NA_real_))[1]
  # Max_ISI and the legacy VP hard break describe direct HFS support, not a
  # Pause-derived global ceiling.  Canonical Pause boundaries are already
  # represented by invalid direct-support indices in `valid_for_hf`.  The
  # HFS-calibrated tolerated gap is the connector ceiling; applying the legacy
  # direct-support break here would make that connector route unreachable.
  hard_break <- NA_real_
  max_gap_count <- suppressWarnings(as.integer(
    vp$hf_spiking_max_consec_large %||%
      hp$spiking_max_consecutive_large_isi %||% 3L
  ))[1L]
  if (!is.finite(max_gap_count)) max_gap_count <- 3L
  max_gap_count <- max(0L, max_gap_count)
  min_spikes <- max(3L, as.integer(vp$hf_spiking_min_spikes %||% 30L))
  fast_core_min_count <- max(1L, as.integer(
    vp$hf_spiking_fast_core_min_count %||%
      hp$spiking_fast_core_min_isi_count %||%
      max(3L, ceiling(max(1L, min_spikes - 1L) * 0.15))
  ))
  fast_core_fraction_min <- min(max(suppressWarnings(as.numeric(
    vp$hf_spiking_fast_core_fraction_min %||%
      hp$spiking_fast_core_fraction_min %||% 0.10
  )), 0.05), 1)
  envelope_expansion_core_fraction_min <- min(max(suppressWarnings(as.numeric(
    vp$hf_spiking_envelope_expansion_core_fraction_min %||%
      hp$spiking_envelope_expansion_core_fraction_min %||% 0.10
  )), fast_core_fraction_min), 1)
  max_unanchored_boundary_isi <- max(0L, as.integer(
    hp$spiking_envelope_max_unanchored_boundary_isi %||%
      ceiling(max(1L, min_spikes - 1L) * 0.25)
  ))
  envelope_fraction_min <- min(max(suppressWarnings(as.numeric(
    vp$hf_spiking_envelope_fraction_min %||%
      hp$spiking_envelope_fraction_min %||% 0.75
  )), 0.50), 1)
  envelope_q80_ratio_max <- max(1, suppressWarnings(as.numeric(
    vp$hf_spiking_envelope_q80_ratio_max %||%
      hp$spiking_envelope_q80_ratio_max %||% 1.00
  )))
  envelope_q90_ratio_max <- max(
    envelope_q80_ratio_max,
    suppressWarnings(as.numeric(
      vp$hf_spiking_envelope_q90_ratio_max %||%
        hp$spiking_envelope_q90_ratio_max %||% 1.50
    )), na.rm = TRUE
  )
  # `hf_spiking_epoch_bridge` is the established public/runtime field and may
  # be overridden after parameter resolution (for example by the threshold
  # preview).  The new alias is metadata/fallback only; it must not make those
  # existing overrides ineffective.
  envelope_upper <- suppressWarnings(as.numeric(
    epoch_bridge %||% vp$hf_spiking_envelope_upper
  ))[1L]
  if (!is.finite(envelope_upper) || envelope_upper < short_upper) {
    envelope_upper <- max(short_upper, epoch_bridge, na.rm = TRUE)
  }
  epoch_bridge <- envelope_upper
  connector_upper <- suppressWarnings(as.numeric(
    vp$hf_spiking_connector_upper %||% envelope_upper
  ))[1L]
  if (!is.finite(connector_upper) || connector_upper < envelope_upper) {
    connector_upper <- envelope_upper
  }
  if (hfs_threshold_source %in% c("histogram", "auto")) {
    # For an automatic proposal the resolved connector value is an inclusive
    # ceiling, not a lower bound that a broader legacy policy may silently
    # override. Explicit user/manual/default contracts retain their established
    # precedence semantics below.
    policy_upper <- if (is.finite(tolerated_gap) && tolerated_gap > 0) {
      tolerated_gap
    } else connector_upper
    tolerated_gap <- max(
      envelope_upper, min(policy_upper, connector_upper, na.rm = TRUE),
      na.rm = TRUE
    )
  } else {
    tolerated_gap <- max(
      tolerated_gap, envelope_upper, connector_upper, na.rm = TRUE
    )
  }

  # q25 is a fast-core anchor only. Consecutive q25--proposal-envelope ISIs are
  # direct HFS support; only values above that envelope spend connector budget.
  # The anchor-density gate is applied both during boundary expansion and to the
  # final candidate, so a small fast seed cannot absorb a long Tonic tail.
  fast_core_flag <- valid_for_hf & isi <= short_upper
  envelope_flag <- valid_for_hf & isi <= envelope_upper
  anchored_support_runs <- stpd_event_grammar_hf_seed_anchored_envelope_runs(
    fast_core_flag, envelope_flag, isi, valid_for_hf, envelope_upper,
    # An envelope component may contribute only one q25 anchor before it is
    # joined across a bounded connector.  The authoritative minimum q25-core
    # count is evaluated on the final merged candidate below; applying it to
    # every component would recreate the scale-dependent fragmentation bug.
    min_core_count = 1L,
    min_core_fraction = envelope_expansion_core_fraction_min,
    min_envelope_fraction = envelope_fraction_min,
    q80_ratio_max = envelope_q80_ratio_max,
    q90_ratio_max = envelope_q90_ratio_max,
    max_unanchored_boundary_isi = max_unanchored_boundary_isi,
    tonic_like_flag = tonic_like_flag,
    tonic_upshift_ratio_min = background_contrast_min
  )
  # A proposal-envelope component between anchored components is direct
  # envelope
  # support even when that component contains no q25 value itself.  Retain it
  # for connector assembly, then enforce the q25 count/fraction on the complete
  # merged candidate.  This distinction prevents per-component fragmentation
  # while keeping q25 evidence mandatory at the parent-candidate level.
  envelope_runs <- stpd_event_core_bool_runs(envelope_flag)
  unanchored_runs <- if (nrow(envelope_runs)) {
    keep <- vapply(seq_len(nrow(envelope_runs)), function(i) {
      idx <- stpd_event_grammar_safe_seq(
        envelope_runs$start_isi[i], envelope_runs$end_isi[i]
      )
      !any(fast_core_flag[idx], na.rm = TRUE)
    }, logical(1))
    envelope_runs[keep, , drop = FALSE]
  } else data.frame()
  if (nrow(unanchored_runs)) {
    unanchored_width <- as.integer(
      unanchored_runs$end_isi - unanchored_runs$start_isi + 1L
    )
    unanchored_runs$unanchored_component_isi_count <- unanchored_width
    unanchored_runs$unanchored_component_requires_review <-
      !is.finite(unanchored_width) |
      unanchored_width > max_unanchored_boundary_isi
    unanchored_runs$anchor_start_isi <- NA_integer_
    unanchored_runs$anchor_end_isi <- NA_integer_
    unanchored_runs$pre_rollback_start_isi <- unanchored_runs$start_isi
    unanchored_runs$pre_rollback_end_isi <- unanchored_runs$end_isi
    unanchored_runs$left_intrusion_end_isi <- NA_integer_
    unanchored_runs$right_intrusion_start_isi <- NA_integer_
    unanchored_runs$rollback_to_intrusion_onset <- FALSE
    unanchored_runs$fast_core_isi_count <- 0L
    unanchored_runs$fast_core_fraction <- 0
    unanchored_runs$envelope_fraction <- 1
    unanchored_runs$envelope_q80_ratio <- vapply(
      seq_len(nrow(unanchored_runs)), function(i) {
        idx <- stpd_event_grammar_safe_seq(
          unanchored_runs$start_isi[i], unanchored_runs$end_isi[i]
        )
        stpd_event_core_quantile(isi[idx], 0.80) / envelope_upper
      }, numeric(1)
    )
    unanchored_runs$envelope_q90_ratio <- vapply(
      seq_len(nrow(unanchored_runs)), function(i) {
        idx <- stpd_event_grammar_safe_seq(
          unanchored_runs$start_isi[i], unanchored_runs$end_isi[i]
        )
        stpd_event_core_quantile(isi[idx], 0.90) / envelope_upper
      }, numeric(1)
    )
    unanchored_runs$max_unanchored_boundary_isi <-
      as.integer(max_unanchored_boundary_isi)
  }
  if (nrow(anchored_support_runs)) {
    anchored_support_runs$unanchored_component_isi_count <- 0L
    anchored_support_runs$unanchored_component_requires_review <- FALSE
  }
  support_runs <- dplyr::bind_rows(anchored_support_runs, unanchored_runs)
  if (nrow(support_runs)) {
    support_runs <- support_runs[
      order(support_runs$start_isi, support_runs$end_isi, method = "radix"),
      , drop = FALSE
    ]
  }
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
  # The resolved values are authoritative. In particular, an explicit zero
  # connector budget must remain zero rather than being raised internally.
  allowed_large_eff <- allowed_large
  max_consec_large_eff <- suppressWarnings(as.integer(
    vp$hf_spiking_max_consec_large %||% 3L
  ))[1L]
  if (!is.finite(max_consec_large_eff)) max_consec_large_eff <- 3L
  max_consec_large_eff <- max(0L, max_consec_large_eff)
  homogeneous_coverage_min <- suppressWarnings(as.numeric(
    hp$spiking_homogeneous_coverage_min %||% 0.80
  ))[1]
  if (!is.finite(homogeneous_coverage_min)) homogeneous_coverage_min <- 0.80
  homogeneous_coverage_min <- min(max(homogeneous_coverage_min, 0.50), 1)
  background_min_isi_count <- suppressWarnings(as.integer(
    hp$spiking_background_min_isi_count %||% max(10L, ceiling(min_spikes / 2))
  ))[1]
  if (!is.finite(background_min_isi_count) || background_min_isi_count < 3L) {
    background_min_isi_count <- max(10L, ceiling(min_spikes / 2))
  }
  background_auc_min <- suppressWarnings(as.numeric(
    hp$spiking_background_auc_min %||% 0.75
  ))[1L]
  if (!is.finite(background_auc_min)) background_auc_min <- 0.75
  background_auc_min <- min(max(background_auc_min, 0.50), 1)
  bilateral_background_min_isi_count <- background_min_isi_count
  background_window_isi_count <- suppressWarnings(as.integer(
    hp$spiking_background_window_isi_count %||%
      background_min_isi_count
  ))[1L]
  if (!is.finite(background_window_isi_count) ||
      background_window_isi_count < bilateral_background_min_isi_count) {
    background_window_isi_count <- bilateral_background_min_isi_count
  }
  rows <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    support_parts <- support_runs[
      support_runs$end_isi >= s & support_runs$start_isi <= e,
      , drop = FALSE
    ]
    rollback_to_intrusion_onset <- nrow(support_parts) > 0L &&
      any(support_parts$rollback_to_intrusion_onset, na.rm = TRUE)
    unanchored_component_requires_review <- nrow(support_parts) > 0L &&
      "unanchored_component_requires_review" %in% names(support_parts) &&
      any(support_parts$unanchored_component_requires_review, na.rm = TRUE)
    max_unanchored_component_isi_count <- if (nrow(support_parts) > 0L &&
        "unanchored_component_isi_count" %in% names(support_parts)) {
      max(support_parts$unanchored_component_isi_count, na.rm = TRUE)
    } else 0L
    pre_rollback_start <- if (nrow(support_parts)) {
      min(support_parts$pre_rollback_start_isi, na.rm = TRUE)
    } else NA_integer_
    pre_rollback_end <- if (nrow(support_parts)) {
      max(support_parts$pre_rollback_end_isi, na.rm = TRUE)
    } else NA_integer_
    left_intrusions <- if (nrow(support_parts)) {
      support_parts$left_intrusion_end_isi[
        is.finite(support_parts$left_intrusion_end_isi)
      ]
    } else integer(0)
    right_intrusions <- if (nrow(support_parts)) {
      support_parts$right_intrusion_start_isi[
        is.finite(support_parts$right_intrusion_start_isi)
      ]
    } else integer(0)
    left_intrusion_end <- if (length(left_intrusions)) {
      max(left_intrusions)
    } else NA_integer_
    right_intrusion_start <- if (length(right_intrusions)) {
      min(right_intrusions)
    } else NA_integer_
    internal_intrusion_starts <- if (nrow(support_parts) &&
        "internal_intrusion_start_isi" %in% names(support_parts)) {
      support_parts$internal_intrusion_start_isi[
        is.finite(support_parts$internal_intrusion_start_isi)
      ]
    } else integer(0)
    internal_intrusion_ends <- if (nrow(support_parts) &&
        "internal_intrusion_end_isi" %in% names(support_parts)) {
      support_parts$internal_intrusion_end_isi[
        is.finite(support_parts$internal_intrusion_end_isi)
      ]
    } else integer(0)
    internal_intrusion_start <- if (length(internal_intrusion_starts)) {
      min(internal_intrusion_starts)
    } else NA_integer_
    internal_intrusion_end <- if (length(internal_intrusion_ends)) {
      max(internal_intrusion_ends)
    } else NA_integer_
    idx <- stpd_event_grammar_safe_seq(s, e)
    if (length(idx) == 0) next
    if (any(manual_burst_lock[idx], na.rm = TRUE)) next
    vals <- isi[idx][valid_for_hf[idx]]
    if (length(vals) == 0) next

    n_spikes <- e - s + 2L
    if (n_spikes < min_spikes) next

    duration <- if ("timestamp_sec" %in% names(dat) && s > 1L && e <= n) {
      suppressWarnings(as.numeric(dat$timestamp_sec[e]) - as.numeric(dat$timestamp_sec[s - 1L]))
    } else NA_real_
    # The spike-count and compact-ISI gates already define sustained HFS.
    # Treat a configured minimum duration as advisory evidence instead of an
    # additional hard veto.  Learning min_spikes and min_duration independently
    # can otherwise create an impossible rule (for example, >=25 spikes with
    # ISI <= 22 ms but duration >= 0.68 s).  The configured value remains in the
    # audit so callers can review it without silently deleting an otherwise
    # valid high-frequency state.
    duration_gate_pass <- min_duration <= 0 ||
      (is.finite(duration) && duration >= min_duration)

    q50 <- stpd_event_core_quantile(vals, 0.50)
    q80 <- stpd_event_core_quantile(vals, 0.80)
    q90 <- stpd_event_core_quantile(vals, 0.90)
    q95 <- stpd_event_core_quantile(vals, 0.95)

    fast_core_count <- sum(vals <= short_upper, na.rm = TRUE)
    short_frac <- fast_core_count / max(1L, length(vals))
    q90_short_frac <- mean(vals <= q90_max, na.rm = TRUE)
    bridge_frac <- mean(vals <= envelope_upper, na.rm = TRUE)
    large_flag <- vals > envelope_upper
    large_frac <- mean(large_flag, na.rm = TRUE)
    max_consec_large <- stpd_event_core_max_consecutive_true(large_flag)
    tolerated_frac <- mean(vals <= tolerated_gap, na.rm = TRUE)
    connector_flag <- vals > envelope_upper
    connector_budget_count <- sum(connector_flag, na.rm = TRUE)
    connector_max <- if (connector_budget_count > 0L) {
      max(vals[connector_flag], na.rm = TRUE)
    } else NA_real_
    # Retain the historical support-role count consumed by the observer-only
    # Gate-1B lineage schema.  It is audit compatibility, not the connector
    # budget used by candidate generation or acceptance; the latter is the
    # envelope-based count above.
    legacy_direct_upper <- if (is.finite(hfs_max_sec)) {
      min(envelope_upper, hfs_max_sec)
    } else envelope_upper
    legacy_connector_role_flag <- vals > legacy_direct_upper
    connector_count <- sum(legacy_connector_role_flag, na.rm = TRUE)

    strict_q90_pass <- is.finite(q90) && q90 <= q90_max
    robust_q80_pass <- is.finite(q80) && q80 <= q80_max
    majority_pass <- (short_frac >= max(0.50, short_frac_min - 0.15)) ||
      (q90_short_frac >= max(0.60, short_frac_min - 0.10)) ||
      (bridge_frac >= max(0.75, short_frac_min))
    fast_core_gate_pass <- fast_core_count >= fast_core_min_count &&
      short_frac >= fast_core_fraction_min
    envelope_q80_ratio <- if (is.finite(q80)) q80 / envelope_upper else Inf
    envelope_q90_ratio <- if (is.finite(q90)) q90 / envelope_upper else Inf
    envelope_q80_pass <- envelope_q80_ratio <= envelope_q80_ratio_max
    envelope_q90_pass <- envelope_q90_ratio <= envelope_q90_ratio_max
    envelope_compactness_pass <- fast_core_gate_pass &&
      bridge_frac >= envelope_fraction_min &&
      envelope_q80_pass && envelope_q90_pass
    state_compact_pass <- strict_q90_pass ||
      (robust_q80_pass && majority_pass) || envelope_compactness_pass
    gap_tolerance_pass <- tolerated_frac >= 0.95
    large_pass <- large_frac <= allowed_large_eff && max_consec_large <= max_consec_large_eff

    # Dual-route HFS evidence:
    #   1. A localized candidate must be faster than a reliable non-HF
    #      background, which prevents ordinary short-scale background runs from
    #      becoming HFS merely because they contain >=20 spikes.
    #   2. If the candidate covers most of the train, the remaining ISIs are
    #      themselves HF-like, or too little background remains, use the
    #      absolute sustained-HFS route.  Thus a homogeneous HFS train never
    #      requires a contrast that cannot be estimated.
    baseline_mask <- valid_for_hf
    left_flank <- stpd_event_grammar_safe_seq(
      max(2L, s - background_window_isi_count), s - 1L
    )
    right_flank <- stpd_event_grammar_safe_seq(
      e + 1L, min(n, e + background_window_isi_count)
    )
    left_adjacent_flank <- stpd_event_grammar_safe_seq(
      max(2L, s - background_min_isi_count), s - 1L
    )
    right_adjacent_flank <- stpd_event_grammar_safe_seq(
      e + 1L, min(n, e + background_min_isi_count)
    )
    outside_idx <- unique(c(left_flank, right_flank))
    outside_idx <- outside_idx[
      outside_idx >= 2L & outside_idx <= n & baseline_mask[outside_idx]
    ]
    left_idx <- left_flank[
      left_flank >= 2L & left_flank <= n & baseline_mask[left_flank]
    ]
    right_idx <- right_flank[
      right_flank >= 2L & right_flank <= n & baseline_mask[right_flank]
    ]
    left_adjacent_idx <- left_adjacent_flank[
      left_adjacent_flank >= 2L & left_adjacent_flank <= n &
        baseline_mask[left_adjacent_flank]
    ]
    right_adjacent_idx <- right_adjacent_flank[
      right_adjacent_flank >= 2L & right_adjacent_flank <= n &
        baseline_mask[right_adjacent_flank]
    ]
    outside_vals <- isi[outside_idx]
    outside_vals <- outside_vals[is.finite(outside_vals)]
    left_vals <- isi[left_idx]
    left_vals <- left_vals[is.finite(left_vals)]
    right_vals <- isi[right_idx]
    right_vals <- right_vals[is.finite(right_vals)]
    left_adjacent_vals <- isi[left_adjacent_idx]
    left_adjacent_vals <- left_adjacent_vals[is.finite(left_adjacent_vals)]
    right_adjacent_vals <- isi[right_adjacent_idx]
    right_adjacent_vals <- right_adjacent_vals[is.finite(right_adjacent_vals)]
    candidate_coverage <- length(vals) / max(1L, sum(valid_for_hf, na.rm = TRUE))
    background_median <- if (length(outside_vals)) stats::median(outside_vals) else NA_real_
    background_short_fraction <- if (length(outside_vals)) {
      mean(outside_vals <= q90_max, na.rm = TRUE)
    } else NA_real_
    background_hf_like <- is.finite(background_short_fraction) &&
      background_short_fraction >= max(0.60, short_frac_min - 0.10)
    # Availability is a sampling/geometry question. Whether the adjacent
    # background is actually slower is decided by the candidate-specific median
    # contrast below. Using the broad proposal connector as a pre-veto here
    # would call genuinely slower flanks "HF-like" and make the contrast gate
    # unreachable whenever the q75 fallback is active.
    background_available <- length(outside_vals) >= background_min_isi_count &&
      candidate_coverage < homogeneous_coverage_min
    background_contrast <- if (is.finite(background_median) && is.finite(q50) && q50 > 0) {
      background_median / q50
    } else NA_real_
    left_background_median <- if (length(left_vals)) {
      stats::median(left_vals)
    } else NA_real_
    right_background_median <- if (length(right_vals)) {
      stats::median(right_vals)
    } else NA_real_
    left_background_contrast <- if (is.finite(left_background_median) &&
        is.finite(q50) && q50 > 0) {
      left_background_median / q50
    } else NA_real_
    right_background_contrast <- if (is.finite(right_background_median) &&
        is.finite(q50) && q50 > 0) {
      right_background_median / q50
    } else NA_real_
    left_adjacent_background_median <- if (length(left_adjacent_vals)) {
      stats::median(left_adjacent_vals)
    } else NA_real_
    right_adjacent_background_median <- if (length(right_adjacent_vals)) {
      stats::median(right_adjacent_vals)
    } else NA_real_
    left_adjacent_background_contrast <- if (
        is.finite(left_adjacent_background_median) &&
        is.finite(q50) && q50 > 0) {
      left_adjacent_background_median / q50
    } else NA_real_
    right_adjacent_background_contrast <- if (
        is.finite(right_adjacent_background_median) &&
        is.finite(q50) && q50 > 0) {
      right_adjacent_background_median / q50
    } else NA_real_
    left_background_auc <- stpd_event_grammar_probability_faster(
      vals, left_vals
    )
    right_background_auc <- stpd_event_grammar_probability_faster(
      vals, right_vals
    )
    bilateral_background_available <-
      length(left_vals) >= bilateral_background_min_isi_count &&
      length(right_vals) >= bilateral_background_min_isi_count &&
      candidate_coverage < homogeneous_coverage_min
    bilateral_background_contrast_pass <-
      bilateral_background_available &&
      is.finite(left_background_contrast) &&
      left_background_contrast >= background_contrast_min &&
      is.finite(right_background_contrast) &&
      right_background_contrast >= background_contrast_min &&
      length(left_adjacent_vals) >= background_min_isi_count &&
      length(right_adjacent_vals) >= background_min_isi_count &&
      is.finite(left_adjacent_background_contrast) &&
      left_adjacent_background_contrast >= background_contrast_min &&
      is.finite(right_adjacent_background_contrast) &&
      right_adjacent_background_contrast >= background_contrast_min &&
      is.finite(left_background_auc) &&
      left_background_auc >= background_auc_min &&
      is.finite(right_background_auc) &&
      right_background_auc >= background_auc_min
    background_gate_pass <- if (auto_requires_local_background) {
      bilateral_background_contrast_pass
    } else {
      !background_available ||
        (is.finite(background_contrast) &&
           background_contrast >= background_contrast_min)
    }
    # A train-spanning compact HF state has no external flank by definition.
    # Requiring bilateral background in that case makes a homogeneous 10-ms
    # HFS mathematically impossible to detect. Permit the absolute sustained
    # route only when the candidate covers the prespecified homogeneous share
    # and independently passes the frozen fast-core/q90 morphology; localized
    # automatic candidates still require bilateral separation.
    homogeneous_absolute_pass <-
      candidate_coverage >= homogeneous_coverage_min &&
      fast_core_gate_pass &&
      is.finite(homogeneous_q90_ceiling) &&
      # Without an external background, equality to the generic direct-HFS
      # ceiling is not enough to distinguish a homogeneous Tonic train from
      # HFS.  Reserve this route for a strictly faster, high-specificity state.
      is.finite(q90) && q90 < homogeneous_q90_ceiling
    identifiability_pass <- !auto_requires_local_background ||
      bilateral_background_contrast_pass || homogeneous_absolute_pass
    # The calibrated parent-state minimum is authoritative.  A HF-like
    # remainder is descriptive evidence that the train may contain several HFS
    # episodes; it must not silently raise the required size (historically from
    # 20 to 30 spikes) and delete every individually valid episode.  This also
    # prevents a remote accepted Pause from changing an otherwise identical
    # HFS decision merely by being removed from the background summary.
    route_min_spikes <- min_spikes
    route_spike_count_pass <- n_spikes >= route_min_spikes
    evidence_route <- if (auto_requires_local_background &&
                          homogeneous_absolute_pass) {
      "absolute_homogeneous_coverage"
    } else if (auto_requires_local_background &&
               identifiability_pass) {
      "histogram_proposal_with_supporting_local_background_contrast"
    } else if (auto_requires_local_background) {
      "histogram_proposal_abstained_without_local_background_separation"
    } else if (candidate_coverage >= homogeneous_coverage_min) {
      "absolute_homogeneous_coverage"
    } else if (background_hf_like) {
      "absolute_hf_like_remainder"
    } else if (background_available && background_gate_pass) {
      "absolute_with_supporting_local_background_contrast"
    } else if (background_available) {
      "absolute_despite_weak_local_background_contrast"
    } else {
      "absolute_background_not_estimable"
    }

    # A user/manual/default contract may authorize an absolute sustained-HFS
    # route. A histogram-derived q25/proposal-envelope band cannot: without slower local
    # background it is self-referential and would label homogeneous Tonic as
    # HFS. In that automatic path, local identifiability is therefore a gate.
    pass <- fast_core_gate_pass && state_compact_pass &&
      gap_tolerance_pass && large_pass &&
      route_spike_count_pass && identifiability_pass &&
      !unanchored_component_requires_review
    # Preserve scientifically plausible automatic abstentions in the candidate
    # audit.  They must never enter AUTO, but silently dropping them would make
    # the promised review workflow impossible to reproduce.  A long unanchored
    # proposal component and a record-edge candidate without two estimable
    # flanks are review evidence only; weak morphology remains an ordinary
    # rejection and is not promoted into the review queue.
    review_morphology_pass <- state_compact_pass && gap_tolerance_pass &&
      large_pass && route_spike_count_pass
    review_reasons <- c(
      if (unanchored_component_requires_review) {
        "proposal_envelope_component_exceeds_unanchored_auto_budget"
      },
      if (auto_requires_local_background &&
          !bilateral_background_available &&
          candidate_coverage < homogeneous_coverage_min) {
        "bilateral_local_background_not_estimable_record_edge_or_short_flank"
      }
    )
    review_only <- !pass && review_morphology_pass &&
      length(review_reasons) > 0L
    if (!pass && !review_only) next

    score <- 24 + 0.08 * n_spikes + 2.5 * short_frac + 2.0 * q90_short_frac +
      1.5 * bridge_frac - 2.0 * large_frac + ifelse(strict_q90_pass, 1.0, 0.4)
    counter <- counter + 1L
    row <- stpd_event_core_candidate_from_run(
      dat, s, e, params, vp, min_isi_sec, train,
      "event_grammar_hf_spiking_state", "event_grammar_long_hf_spiking_epoch", "high_frequency_spiking",
      if (review_only) {
        "event_grammar_hf_spiking_state_review_only"
      } else {
        "event_grammar_hf_spiking_state_pass"
      },
      if (review_only) {
        paste(c("automatic_hfs_abstention", review_reasons), collapse = ";")
      } else {
        "merged_support_run_long_high_frequency_state"
      },
      if (review_only) "abstain" else "accept",
      score, if (review_only) 0 else 1040,
      list(
        candidate_id = paste0(
          if (review_only) "event_grammar_hfs_review_" else
            "event_grammar_hfs_state_",
          counter
        ),
        hf_spiking_review_only = review_only,
        hf_spiking_review_reason = paste(review_reasons, collapse = ";"),
        hf_spiking_selected_for_auto_contract = !review_only,
        hf_spiking_q50_sec = q50,
        hf_spiking_q80_sec = q80,
        hf_spiking_q90_sec = q90,
        hf_spiking_q95_sec = q95,
        hf_spiking_seed_lower_sec = seed_lower,
        hf_spiking_seed_lower_role =
          "audit_only_shorter_embedded_burst_support_retained",
        hf_spiking_support_semantics =
          "q25_anchor_stable_valley_or_q75_proposal_envelope",
        hf_spiking_fast_core_upper_sec = short_upper,
        hf_spiking_fast_core_isi_count = as.integer(fast_core_count),
        hf_spiking_fast_core_fraction = short_frac,
        hf_spiking_fast_core_min_count = fast_core_min_count,
        hf_spiking_fast_core_fraction_min = fast_core_fraction_min,
        hf_spiking_envelope_expansion_core_fraction_min =
          envelope_expansion_core_fraction_min,
        hf_spiking_envelope_max_unanchored_boundary_isi =
          max_unanchored_boundary_isi,
        hf_spiking_max_unanchored_component_isi_count =
          as.integer(max_unanchored_component_isi_count),
        hf_spiking_unanchored_component_requires_review =
          unanchored_component_requires_review,
        hf_spiking_fast_core_gate_pass = fast_core_gate_pass,
        hf_spiking_short_upper_sec = short_upper,
        hf_spiking_q80_max_sec = q80_max,
        hf_spiking_q90_max_sec = q90_max,
        hf_spiking_epoch_bridge_sec = envelope_upper,
        hf_spiking_envelope_upper_sec = envelope_upper,
        hf_spiking_connector_upper_sec = connector_upper,
        hf_spiking_envelope_proposal_status = as.character(
          vp$hf_spiking_envelope_proposal_status %||% ""
        )[1L],
        hf_spiking_envelope_proposal_method = as.character(
          vp$hf_spiking_envelope_proposal_method %||% ""
        )[1L],
        hf_spiking_envelope_fraction = bridge_frac,
        hf_spiking_envelope_fraction_min = envelope_fraction_min,
        hf_spiking_envelope_q80_ratio = envelope_q80_ratio,
        hf_spiking_envelope_q80_ratio_max = envelope_q80_ratio_max,
        hf_spiking_envelope_q90_ratio = envelope_q90_ratio,
        hf_spiking_envelope_q90_ratio_max = envelope_q90_ratio_max,
        hf_spiking_envelope_compactness_pass =
          envelope_compactness_pass,
        hf_spiking_envelope_rollback_to_intrusion_onset =
          rollback_to_intrusion_onset,
        hf_spiking_envelope_pre_rollback_start_isi =
          as.integer(pre_rollback_start),
        hf_spiking_envelope_pre_rollback_end_isi =
          as.integer(pre_rollback_end),
        hf_spiking_envelope_left_intrusion_end_isi =
          as.integer(left_intrusion_end),
        hf_spiking_envelope_right_intrusion_start_isi =
          as.integer(right_intrusion_start),
        hf_spiking_envelope_internal_intrusion_start_isi =
          as.integer(internal_intrusion_start),
        hf_spiking_envelope_internal_intrusion_end_isi =
          as.integer(internal_intrusion_end),
        hf_spiking_connector_configured_sec = connector_policy$configured_tolerated_gap_sec,
        hf_spiking_tolerated_gap_sec = tolerated_gap,
        hf_spiking_legacy_direct_hard_break_audit_sec = vp_hard_break,
        hf_spiking_connector_pause_allowance_sec = connector_policy$pause_allowance_sec,
        hf_spiking_connector_count = connector_count,
        hf_spiking_connector_count_role =
          "legacy_support_role_audit_not_candidate_budget",
        hf_spiking_connector_budget_count = connector_budget_count,
        hf_spiking_connector_budget_threshold_sec = envelope_upper,
        hf_spiking_connector_budget_gate_role =
          "only_above_resolved_proposal_envelope_consumes_budget",
        hf_spiking_connector_max_sec = connector_max,
        hf_spiking_connector_role =
          "only_above_resolved_proposal_envelope_brief_interruption",
        hf_spiking_pattern_max_ISI_sec = hfs_max_sec,
        hf_spiking_pause_break_sec = NA_real_,
        hf_spiking_pause_seed_audit_sec = pause_seed_audit,
        hf_spiking_canonical_boundary_n = sum(canonical_boundary_mask),
        hf_spiking_pause_boundary_policy =
          "instance_level_canonical_boundary_not_global_pause_seed",
        hf_spiking_short_fraction = short_frac,
        hf_spiking_q90_short_fraction = q90_short_frac,
        hf_spiking_bridge_fraction = bridge_frac,
        hf_spiking_large_fraction = large_frac,
        hf_spiking_tolerated_fraction = tolerated_frac,
        hf_spiking_max_consecutive_large_isi = max_consec_large,
        hf_spiking_min_spikes_required = min_spikes,
        hf_spiking_min_duration_configured_sec = min_duration,
        hf_spiking_duration_gate_pass = duration_gate_pass,
        hf_spiking_duration_gate_role = "advisory_min_spikes_and_compactness_authoritative",
        hf_spiking_candidate_coverage = candidate_coverage,
        hf_spiking_background_isi_count = length(outside_vals),
        hf_spiking_background_scope = "immediately_adjacent_bilateral_flanks",
        hf_spiking_background_window_isi_count =
          background_window_isi_count,
        hf_spiking_bilateral_background_min_isi_count =
          bilateral_background_min_isi_count,
        hf_spiking_background_auc_min = background_auc_min,
        hf_spiking_left_background_auc = left_background_auc,
        hf_spiking_right_background_auc = right_background_auc,
        hf_spiking_background_median_sec = background_median,
        hf_spiking_left_background_isi_count = length(left_vals),
        hf_spiking_right_background_isi_count = length(right_vals),
        hf_spiking_left_background_median_sec = left_background_median,
        hf_spiking_right_background_median_sec = right_background_median,
        hf_spiking_left_background_contrast = left_background_contrast,
        hf_spiking_right_background_contrast = right_background_contrast,
        hf_spiking_left_adjacent_background_isi_count =
          length(left_adjacent_vals),
        hf_spiking_right_adjacent_background_isi_count =
          length(right_adjacent_vals),
        hf_spiking_left_adjacent_background_median_sec =
          left_adjacent_background_median,
        hf_spiking_right_adjacent_background_median_sec =
          right_adjacent_background_median,
        hf_spiking_left_adjacent_background_contrast =
          left_adjacent_background_contrast,
        hf_spiking_right_adjacent_background_contrast =
          right_adjacent_background_contrast,
        hf_spiking_bilateral_background_available =
          bilateral_background_available,
        hf_spiking_bilateral_background_contrast_pass =
          bilateral_background_contrast_pass,
        hf_spiking_background_short_fraction = background_short_fraction,
        hf_spiking_background_available = background_available,
        hf_spiking_background_contrast = background_contrast,
        hf_spiking_background_contrast_min = background_contrast_min,
        hf_spiking_background_gate_pass = background_gate_pass,
        hf_spiking_identifiability_pass = identifiability_pass,
        hf_spiking_auto_requires_local_background =
          auto_requires_local_background,
        hf_spiking_threshold_source_mode = hfs_threshold_source,
        hf_spiking_threshold_field_sources = as.character(
          vp$hf_spiking_threshold_field_sources %||% hfs_threshold_source
        )[1L],
        hf_spiking_histogram_available =
          isTRUE(vp$hf_spiking_histogram_available),
        hf_spiking_histogram_status = as.character(
          vp$hf_spiking_histogram_status %||% "not_applicable"
        )[1L],
        hf_spiking_histogram_reason = as.character(
          vp$hf_spiking_histogram_reason %||% ""
        )[1L],
        hf_spiking_route_min_spikes_required = route_min_spikes,
        hf_spiking_route_spike_count_pass = route_spike_count_pass,
        hf_spiking_evidence_route = evidence_route,
        hf_spiking_acceptance_route = paste(
          if (strict_q90_pass) {
            "strict_q90"
          } else if (robust_q80_pass && majority_pass) {
            "robust_q80_majority_state"
          } else {
            "q25_anchor_stable_valley_or_q75_proposal_envelope"
          },
          evidence_route, sep = ";"
        )
      )
    )
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

# Resolve only topologically separable Tonic/HFS State transitions. Tonic and
# Broad HFS are generated independently; an accepted Tonic candidate may cut
# an HFS candidate only when it occupies an edge of that HFS support and the
# remaining contiguous support independently re-passes the complete frozen HFS
# detector. Interior/covering conflicts retain the historical HFS veto.
stpd_event_grammar_resolve_edge_tonic_hfs_states <- function(
    dat, hfs, tonic, params, vp, min_isi_sec = 0.001, train = "",
    hard_boundaries = NULL,
    hfs_detector = stpd_event_core_detect_hf_spiking) {
  hfs <- as.data.frame(hfs %||% data.frame(), stringsAsFactors = FALSE)
  tonic <- as.data.frame(tonic %||% data.frame(), stringsAsFactors = FALSE)

  audit_columns <- list(
    state_overlap_resolution = "none",
    state_overlap_peer_candidate_id = "",
    state_overlap_n_isi = 0L,
    state_overlap_geometry = "none",
    pre_resolution_start_isi = NA_integer_,
    pre_resolution_end_isi = NA_integer_,
    post_resolution_start_isi = NA_integer_,
    post_resolution_end_isi = NA_integer_,
    residual_gate_pass = NA,
    residual_gate_status = "not_evaluated",
    residual_failed_checks = "",
    root_hfs_candidate_id = "",
    resolution_reason = ""
  )
  add_audit_columns <- function(x) {
    for (name in names(audit_columns)) {
      if (!(name %in% names(x))) {
        x[[name]] <- rep(audit_columns[[name]], nrow(x))
      }
    }
    x
  }
  hfs <- add_audit_columns(hfs)
  tonic <- add_audit_columns(tonic)
  if (nrow(hfs) == 0L || nrow(tonic) == 0L) {
    return(list(hfs = hfs, tonic = tonic))
  }

  # Resolve duplicate/overlapping Tonic proposals without consulting HFS. This
  # prevents an HFS candidate from helping a Tonic proposal enter arbitration.
  tonic_selected <- stpd_event_core_weighted_select(
    tonic, locked = NULL, patterns = "tonic"
  )
  selected_flag <- as.logical(tonic_selected$selected_for_auto)
  selected_flag[is.na(selected_flag)] <- FALSE
  selected_tonic <- tonic_selected[selected_flag, , drop = FALSE]
  if (nrow(selected_tonic) == 0L) {
    return(list(hfs = hfs, tonic = tonic))
  }

  interval_overlap <- function(a_start, a_end, b_start, b_end) {
    max(0L, min(a_end, b_end) - max(a_start, b_start) + 1L)
  }
  candidate_id <- function(x, fallback) {
    value <- as.character(x$candidate_id %||% fallback)[1]
    if (is.na(value) || !nzchar(value)) fallback else value
  }
  combine_boundaries <- function(base, extra) {
    cols <- c("start_isi", "end_isi")
    base <- as.data.frame(base %||% data.frame(), stringsAsFactors = FALSE)
    extra <- as.data.frame(extra %||% data.frame(), stringsAsFactors = FALSE)
    if (!all(cols %in% names(base))) {
      base <- data.frame(start_isi = integer(), end_isi = integer())
    }
    if (!all(cols %in% names(extra))) {
      extra <- data.frame(start_isi = integer(), end_isi = integer())
    }
    out <- dplyr::bind_rows(
      base[, cols, drop = FALSE], extra[, cols, drop = FALSE]
    )
    if (nrow(out) == 0L) return(out)
    out$start_isi <- suppressWarnings(as.integer(out$start_isi))
    out$end_isi <- suppressWarnings(as.integer(out$end_isi))
    out <- out[
      is.finite(out$start_isi) & is.finite(out$end_isi) &
        out$end_isi >= out$start_isi,
      , drop = FALSE
    ]
    unique(out)
  }

  resolved_rows <- list()
  for (hi in seq_len(nrow(hfs))) {
    parent <- hfs[hi, , drop = FALSE]
    hs <- suppressWarnings(as.integer(parent$start_isi[1]))
    he <- suppressWarnings(as.integer(parent$end_isi[1]))
    root_id <- candidate_id(parent, paste0("hfs_row_", hi))
    if (!is.finite(hs) || !is.finite(he) || he < hs) {
      resolved_rows[[length(resolved_rows) + 1L]] <- parent
      next
    }

    overlap_rows <- selected_tonic[
      suppressWarnings(as.integer(selected_tonic$start_isi)) <= he &
        suppressWarnings(as.integer(selected_tonic$end_isi)) >= hs,
      , drop = FALSE
    ]
    if (nrow(overlap_rows) == 0L) {
      resolved_rows[[length(resolved_rows) + 1L]] <- parent
      next
    }

    ts <- suppressWarnings(as.integer(overlap_rows$start_isi))
    te <- suppressWarnings(as.integer(overlap_rows$end_isi))
    covers_parent <- ts <= hs & te >= he
    left_edge <- ts <= hs & te >= hs & te < he
    right_edge <- ts > hs & ts <= he & te >= he
    edge <- !covers_parent & (left_edge | right_edge)
    interior <- !covers_parent & !edge
    if (any(interior | covers_parent)) {
      parent$state_overlap_resolution <- "unresolved_nonedge_state_conflict"
      parent$state_overlap_geometry <- if (any(covers_parent)) {
        "tonic_covers_hfs"
      } else {
        "tonic_inside_hfs"
      }
      parent$state_overlap_peer_candidate_id <- paste(
        sort(unique(as.character(overlap_rows$candidate_id)), method = "radix"),
        collapse = ";"
      )
      parent$resolution_reason <-
        "nonedge_tonic_conflict_retains_historical_hfs_veto"
      resolved_rows[[length(resolved_rows) + 1L]] <- parent
      next
    }

    cuts <- overlap_rows[edge, , drop = FALSE]
    cut_start <- suppressWarnings(as.integer(cuts$start_isi))
    cut_end <- suppressWarnings(as.integer(cuts$end_isi))
    left_values <- cut_end[cut_start <= hs]
    right_values <- cut_start[cut_end >= he]
    left_cut_end <- if (length(left_values)) max(left_values) else NA_integer_
    right_cut_start <- if (length(right_values)) min(right_values) else NA_integer_
    residual_start <- if (is.finite(left_cut_end)) left_cut_end + 1L else hs
    residual_end <- if (is.finite(right_cut_start)) right_cut_start - 1L else he
    peer_ids <- paste(
      sort(unique(as.character(cuts$candidate_id)), method = "radix"),
      collapse = ";"
    )
    overlap_n <- sum(vapply(seq_len(nrow(cuts)), function(i) {
      interval_overlap(hs, he, cut_start[i], cut_end[i])
    }, integer(1)))

    if (!is.finite(residual_start) || !is.finite(residual_end) ||
        residual_end < residual_start) {
      parent$state_overlap_resolution <-
        "edge_candidate_residual_empty_keep_parent"
      parent$state_overlap_peer_candidate_id <- peer_ids
      parent$state_overlap_n_isi <- as.integer(overlap_n)
      parent$state_overlap_geometry <- "edge_separable_tonic_transition"
      parent$residual_gate_pass <- FALSE
      parent$residual_gate_status <- "failed_empty_residual"
      parent$residual_failed_checks <- "empty_residual"
      parent$resolution_reason <- "edge_tonic_consumes_all_hfs_support"
      resolved_rows[[length(resolved_rows) + 1L]] <- parent
      next
    }

    boundaries <- combine_boundaries(hard_boundaries, cuts)
    redetected <- hfs_detector(
      dat, params, vp, min_isi_sec = min_isi_sec, train = train,
      hard_boundaries = boundaries
    )
    redetected <- as.data.frame(
      redetected %||% data.frame(), stringsAsFactors = FALSE
    )
    exact <- if (nrow(redetected) > 0L) {
      suppressWarnings(as.integer(redetected$start_isi)) == residual_start &
        suppressWarnings(as.integer(redetected$end_isi)) == residual_end
    } else {
      logical()
    }
    exact[is.na(exact)] <- FALSE
    if (any(exact)) {
      child <- redetected[which(exact)[1L], , drop = FALSE]
      child <- add_audit_columns(child)
      child_id <- candidate_id(child, paste0(root_id, "_edge_residual"))
      child$candidate_id <- paste0(
        child_id, "__edge_residual_of__", root_id
      )
      child$state_overlap_resolution <-
        "resolved_edge_tonic_hfs_transition"
      child$state_overlap_peer_candidate_id <- peer_ids
      child$state_overlap_n_isi <- as.integer(overlap_n)
      child$state_overlap_geometry <- "edge_separable_tonic_transition"
      child$pre_resolution_start_isi <- hs
      child$pre_resolution_end_isi <- he
      child$post_resolution_start_isi <- residual_start
      child$post_resolution_end_isi <- residual_end
      child$residual_gate_pass <- TRUE
      child$residual_gate_status <-
        "passed_complete_frozen_hfs_redetection"
      child$root_hfs_candidate_id <- root_id
      child$resolution_reason <-
        "independent_tonic_edge_boundary_and_hfs_residual_repassed"
      resolved_rows[[length(resolved_rows) + 1L]] <- child

      tonic_hit <- tonic$candidate_id %in% as.character(cuts$candidate_id)
      tonic$state_overlap_resolution[tonic_hit] <-
        "resolved_edge_tonic_hfs_transition"
      tonic$state_overlap_peer_candidate_id[tonic_hit] <- root_id
      tonic$state_overlap_n_isi[tonic_hit] <- as.integer(overlap_n)
      tonic$state_overlap_geometry[tonic_hit] <-
        "edge_separable_tonic_transition"
      tonic$pre_resolution_start_isi[tonic_hit] <- hs
      tonic$pre_resolution_end_isi[tonic_hit] <- he
      tonic$post_resolution_start_isi[tonic_hit] <-
        suppressWarnings(as.integer(tonic$start_isi[tonic_hit]))
      tonic$post_resolution_end_isi[tonic_hit] <-
        suppressWarnings(as.integer(tonic$end_isi[tonic_hit]))
      tonic$residual_gate_pass[tonic_hit] <- TRUE
      tonic$residual_gate_status[tonic_hit] <- "peer_hfs_residual_passed"
      tonic$root_hfs_candidate_id[tonic_hit] <- root_id
      tonic$resolution_reason[tonic_hit] <-
        "tonic_retained_as_independently_supported_edge_state"
    } else {
      parent$state_overlap_resolution <-
        "edge_candidate_residual_failed_keep_parent"
      parent$state_overlap_peer_candidate_id <- peer_ids
      parent$state_overlap_n_isi <- as.integer(overlap_n)
      parent$state_overlap_geometry <- "edge_separable_tonic_transition"
      parent$pre_resolution_start_isi <- hs
      parent$pre_resolution_end_isi <- he
      parent$post_resolution_start_isi <- residual_start
      parent$post_resolution_end_isi <- residual_end
      parent$residual_gate_pass <- FALSE
      parent$residual_gate_status <-
        "failed_complete_frozen_hfs_redetection"
      parent$residual_failed_checks <- "no_exact_residual_candidate"
      parent$root_hfs_candidate_id <- root_id
      parent$resolution_reason <-
        "residual_hfs_failed_so_historical_hfs_veto_retained"
      resolved_rows[[length(resolved_rows) + 1L]] <- parent
    }
  }

  resolved_hfs <- dplyr::bind_rows(resolved_rows)
  if (nrow(resolved_hfs) > 0L) rownames(resolved_hfs) <- NULL
  list(hfs = resolved_hfs, tonic = tonic)
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

stpd_event_grammar_annotate_burst_rich_hf_spiking_states <- function(audit) {
  if (is.null(audit) || nrow(audit) == 0 || !("final_label" %in% names(audit))) return(audit)
  n <- nrow(audit)
  starts <- suppressWarnings(as.integer(audit$start_isi))
  ends <- suppressWarnings(as.integer(audit$end_isi))
  lab <- as.character(audit$final_label %||% rep("", n))
  lab[is.na(lab)] <- ""
  suppressed_lab <- as.character(audit$suppressed_original_label %||% rep("", n))
  suppressed_lab[is.na(suppressed_lab)] <- ""
  effective_lab <- ifelse(nzchar(suppressed_lab), suppressed_lab, lab)
  layers <- as.character(audit$candidate_layer %||% rep("", n))
  layers[is.na(layers)] <- ""

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
        layers != "structure_first_burst_screen" &
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
      diagnostic_tag <- if (isTRUE(burst_dominated)) {
        "audit_burst_dominated_hf_spiking_state"
      } else {
        "audit_burst_packet_like_hf_spiking_state"
      }
      audit$decision_path[hi] <- paste0(
        as.character(audit$decision_path[hi] %||% ""),
        ";", diagnostic_tag
      )
      # Burst is an orthogonal Event. Its coverage can describe a burst-rich or
      # packetized HF episode, but it cannot veto a State that independently
      # passed the frequency/regularity gates.
      audit$candidate_diagnostic_class[hi] <- paste0("descriptive__", diagnostic_tag)
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
          ";audit_burst_packet_neighbor_hf_spiking_state"
        )
        audit$candidate_diagnostic_class[hi] <-
          "descriptive__audit_burst_packet_neighbor_hf_spiking_state"
      }
    }
  }
  audit
}

# Suppress subordinate labels inside an accepted long HF-spiking state before
# weighted interval selection. Compact burst-like kernels can occur inside a
# sustained HF state, but they should not delete the state-level annotation.
# Burst-rich/packetized coverage is retained as descriptive evidence only. The
# legacy single-label projection may still suppress an overlapping label for
# display compatibility; the multitrack product restores the original Event and
# State candidates as orthogonal axes.
stpd_event_grammar_protect_hf_spiking_states <- function(audit) {
  if (is.null(audit) || nrow(audit) == 0 || !("final_label" %in% names(audit))) return(audit)
  audit <- stpd_event_grammar_annotate_burst_rich_hf_spiking_states(audit)
  lab <- as.character(audit$final_label); lab[is.na(lab)] <- ""
  # A descriptive/review-only HFS proposal is not an accepted State and must
  # never veto Burst or Tonic candidates in the legacy projection.  This is
  # especially important when the histogram proposes a broad envelope but the
  # local-background identifiability gate explicitly abstains.
  hfs_review_only <- as.logical(
    audit$hf_spiking_review_only %||% rep(FALSE, nrow(audit))
  )
  hfs_review_only[is.na(hfs_review_only)] <- FALSE
  hfs_action <- as.character(audit$action %||% rep("", nrow(audit)))
  hfs_action[is.na(hfs_action)] <- ""
  hfs_idx <- which(
    lab == "high_frequency_spiking" &
      !hfs_review_only &
      hfs_action != "abstain"
  )
  if (length(hfs_idx) == 0) return(audit)

  starts <- suppressWarnings(as.integer(audit$start_isi))
  ends <- suppressWarnings(as.integer(audit$end_isi))
  layers <- as.character(audit$candidate_layer %||% rep("", nrow(audit)))
  strict_boundary <- as.logical(audit$strict_boundary_pass %||% rep(FALSE, nrow(audit)))
  strict_boundary[is.na(strict_boundary)] <- FALSE
  # Structure-first anchors are primary candidate evidence, but they must not
  # automatically dismantle a separately accepted strong HFS state. In that
  # case the single-label AUTO layer keeps HFS while the original burst label,
  # geometry and source remain explicit in the suppressed-candidate audit.
  structure_anchor <- layers == "structure_first_burst_screen" & strict_boundary
  # Pause is an orthogonal Gap and must remain in the ledger even when it lies
  # inside an accepted State episode. The legacy single-label projection may
  # still be unable to display both, but the multi-track product will subtract
  # Pause from direct State support without deleting the parent envelope.
  suppressible <- c("possible_burst", "high_frequency_tonic", "tonic")
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
    h_required_spikes <- suppressWarnings(as.integer(
      audit$hf_spiking_min_spikes_required[hi] %||% 20L
    ))
    if (!is.finite(h_required_spikes) || h_required_spikes < 3L) {
      h_required_spikes <- 20L
    }
    sustained_hf_state <- is.finite(h_n_isi) && h_n_isi >= 80L &&
      (!is.finite(h_short) || h_short >= 0.70) &&
      (!is.finite(h_bridge) || h_bridge >= 0.90) &&
      (!is.finite(h_q90) || !is.finite(h_q90_max) || h_q90 <= h_q90_max)
    compact_pure_hf_state <- is.finite(h_n_isi) &&
      h_n_isi >= (h_required_spikes - 1L) &&
      is.finite(h_short) && h_short >= 0.85 &&
      is.finite(h_bridge) && h_bridge >= 0.95 &&
      (!is.finite(h_q90) || !is.finite(h_q90_max) || h_q90 <= h_q90_max)
    strong_hf_state <- isTRUE(sustained_hf_state) || isTRUE(compact_pure_hf_state)

    ov <- which(seq_len(nrow(audit)) != hi & lab %in% suppressible &
                  is.finite(starts) & is.finite(ends) &
                  starts <= he & ends >= hs)
    suppress_rows(ov, ";suppressed_by_long_hf_spiking_state")

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
      is_structure <- structure_anchor[embedded_events]
      is_structure[is.na(is_structure)] <- FALSE
      threshold_elastic <- as.logical(
        audit$threshold_elastic_acceptance[embedded_events] %||% FALSE
      )
      threshold_elastic[is.na(threshold_elastic)] <- FALSE
      contrast_status <- as.character(
        audit$contrast_evidence_status[embedded_events] %||% ""
      )
      contrast_status[is.na(contrast_status)] <- ""
      homogeneous_elastic <- threshold_elastic &
        contrast_status == "weak_or_unavailable"
      # Every HFS row reaching this point has passed the detector and survived
      # the independent burst-dominated/packet-like veto. A tiny contained
      # structure anchor is therefore retained as evidence but cannot replace
      # the whole accepted state in the mutually exclusive AUTO track. Legacy
      # burst candidates keep their historical suppression rule (strong HFS
      # only), avoiding a silent change outside the new proposal layer.
      suppress_compact <-
        (compact & (is_structure | isTRUE(strong_hf_state))) |
        (isTRUE(strong_hf_state) & homogeneous_elastic)
      suppress_rows(
        embedded_events[suppress_compact],
        ";compact_burst_kernel_suppressed_inside_long_hf_spiking_state;accepted_hf_state_containment_guard",
        action = "suppress_embedded_burst_for_hf_spiking_state"
      )
      structure_suppressed <- embedded_events[suppress_compact & is_structure]
      if (length(structure_suppressed) > 0L) {
        audit$decision_path[structure_suppressed] <- paste0(
          as.character(audit$decision_path[structure_suppressed] %||% ""),
          ";structure_first_evidence_preserved_in_audit"
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
  strict_structure <- identical(layer, "structure_first_burst_screen") &&
    isTRUE(as.logical(row$strict_boundary_pass[1] %||% FALSE))
  if (strict_structure && lab %in% c("burst", "long_burst")) {
    # Preserve the same anti-fragmentation span value used by dense burst
    # episodes, with the structure-first priority deciding an otherwise equal
    # event-family contest. State candidates retain their own stronger rules.
    structure_priority <- if (is.finite(explicit) && explicit > 0) explicit else 1260
    return(structure_priority * 10000 + 180 * sc + 2500000 * min(n_isi, 80))
  }
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
  if (identical(lab, "high_frequency_tonic") &&
      identical(layer, "event_core_hf_tonic_state")) {
    # A candidate that passes the stable HFT gate is state evidence, not merely
    # a competing label.  It should outrank dense-rescue burst subtypes when
    # both describe the same interval, avoiding an artificial 15/16-spike
    # discontinuity in event-versus-state arbitration.
    return(1300 * 10000 + 180 * sc + 3000000 * min(n_isi, 80))
  }
  if (lab %in% c("burst", "long_burst") && identical(layer, "event_grammar_burst_episode")) {
    # Dense burst episodes should beat a tiling of several tiny burst fragments
    # inside the same visually coherent cluster. Burst and long_burst differ by
    # spike count, not by the evidence strength of this rescue route.
    base <- 1250
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

# Resolve the HFS-local Burst proposal rule from the already frozen Burst
# parameter contract.  No cross-dataset absolute ISI threshold is introduced:
# every numerical gate is a ratio, rank, or count.  These candidates remain
# review-only because calibration did not support automatic promotion.
stpd_nested_hfs_detector_settings <- function(params, vp = list()) {
  product_burst <- ((params$spiketrainpattern %||% list())$burst %||% list())
  burst <- params$burst %||% list()
  compactness_quantile <- stpd_event_core_num(
    product_burst$structure_first_compactness_quantile %||% 0.80, 0.80
  )
  pair_rank_max <- min(0.35, max(0.05, 1 - compactness_quantile))
  min_spikes <- max(4L, stpd_event_core_int(vp$min_spikes %||% 4L, 4L))
  max_spikes <- max(
    min_spikes,
    stpd_event_core_int(vp$long_max_spikes %||% 15L, 15L)
  )
  hfs_min_spikes <- max(
    3L, stpd_event_core_int(vp$hf_spiking_min_spikes %||% 20L, 20L)
  )
  list(
    context_isi_n = max(6L, min(12L, as.integer(ceiling(hfs_min_spikes / 2)))),
    guard_isi_n = 1L,
    min_background_isi_n = 3L,
    local_min_radius = max(
      3L,
      stpd_event_core_int(product_burst$structure_first_min_isi_count %||% 3L, 3L)
    ),
    seed_pair_rank_max = pair_rank_max,
    seed_side_ratio_min = max(
      1,
      stpd_event_core_num(
        burst$proposal_contrast_min %||% 1.20, 1.20
      )
    ),
    seed_geom_ratio_min = max(
      1,
      stpd_event_core_num(
        burst$proposal_contrast_geom_min %||% 1.30, 1.30
      )
    ),
    direct_expand_factor = max(
      1,
      stpd_event_core_num(
        product_burst$structure_first_max_internal_tail_ratio %||% 1.25,
        1.25
      )
    ),
    edge_side_ratio_min = max(
      1,
      stpd_event_core_num(burst$long_burst_edge_contrast_min %||% 1.45, 1.45)
    ),
    edge_geom_ratio_min = max(
      1,
      stpd_event_core_num(burst$long_burst_edge_contrast_geom %||% 1.50, 1.50)
    ),
    bridge_ratio_max = max(
      1,
      stpd_event_core_num(
        burst$classic_burst_internal_outlier_ratio_max %||%
          burst$internal_outlier_ratio_max %||% 3.5,
        3.5
      )
    ),
    bridge_max_count = 1L,
    max_consecutive_borrowed = 1L,
    native_fraction_min = min(
      1,
      max(0.5, stpd_event_core_num(
        (params$event_grammar %||% list())$
          burst_extension_native_support_fraction_min %||% 0.55,
        0.55
      ))
    ),
    min_spikes = min_spikes,
    max_spikes = max_spikes,
    max_expand_each_side = max(1L, max_spikes - 2L),
    max_candidates = max(
      1L,
      min(300L, stpd_event_core_int(vp$max_candidates %||% 3000L, 3000L))
    )
  )
}

stpd_nested_hfs_selected_parent_candidates <- function(
    hfs_candidates, patterns, params) {
  if (is.null(hfs_candidates) || !is.data.frame(hfs_candidates) ||
      nrow(hfs_candidates) == 0L) return(data.frame())
  shadow <- stpd_multitrack_shadow_select(
    hfs_candidates, patterns = patterns, params = params
  )
  candidates <- as.data.frame(shadow$candidates, stringsAsFactors = FALSE)
  if (nrow(candidates) == 0L) return(candidates)
  label <- stpd_multitrack_shadow_normalize_label(candidates$final_label)
  selected <- as.logical(candidates$selected_within_track)
  selected[is.na(selected)] <- FALSE
  candidates[
    label == "high_frequency_spiking" &
      candidates$semantic_track == "state" & selected,
    , drop = FALSE
  ]
}

stpd_nested_hfs_standardize_candidates <- function(
    dat, nested_candidates, params, vp, min_isi_sec = 0.001, train = "") {
  if (is.null(nested_candidates) || !is.data.frame(nested_candidates) ||
      nrow(nested_candidates) == 0L) return(data.frame())
  rows <- vector("list", nrow(nested_candidates))
  for (i in seq_len(nrow(nested_candidates))) {
    raw <- nested_candidates[i, , drop = FALSE]
    extra <- as.list(raw)
    extra$nested_hfs_rule_version <- "nested_hfs_local_rate_contrast_v1"
    extra$nested_hfs_gate_applied <- TRUE
    extra$nested_hfs_gate_status <- paste0(
      "review_only__",
      as.character(raw$review_evidence_strength %||% "local_rate_proposal")
    )
    extra$strict_boundary_pass <- FALSE
    extra$one_sided_boundary_pass <- FALSE
    extra$possible_boundary_pass <- TRUE
    extra$canonical_eligible <- FALSE
    extra$review_only <- TRUE
    extra$candidate_diagnostic_class <-
      "possible_burst__nested_hfs_local_rate_review_only"
    rows[[i]] <- stpd_event_core_candidate_from_run(
      dat,
      stpd_event_core_int(raw$start_isi, NA_integer_),
      stpd_event_core_int(raw$end_isi, NA_integer_),
      params, vp, min_isi_sec, train,
      "nested_hfs_local_rate_contrast",
      "nested_hfs_local_rate_review_proposal",
      "possible_burst",
      "nested_hfs_local_rate_review_only",
      as.character(raw$decision_path),
      "demote_to_possible",
      stpd_event_core_num(raw$score, 0),
      min(360, stpd_event_core_num(raw$priority, 360)),
      extra
    )
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(data.frame())
  out <- dplyr::bind_rows(rows)
  stpd_event_core_apply_refractory_suspect_policy(
    out, params, dat = dat, vp = vp, min_isi_sec = min_isi_sec
  )
}

stpd_nested_hfs_parent_signature <- function(shadow_candidates) {
  x <- as.data.frame(shadow_candidates %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(x) == 0L) return(digest::digest(data.frame(), algo = "sha256"))
  label <- stpd_multitrack_shadow_normalize_label(x$final_label %||% "")
  selected <- as.logical(x$selected_within_track %||% FALSE)
  selected[is.na(selected)] <- FALSE
  x <- x[label == "high_frequency_spiking" & selected, , drop = FALSE]
  if (nrow(x) == 0L) {
    return(digest::digest(data.frame(), algo = "sha256"))
  }
  evidence_columns <- grep("^hf_spiking_", names(x), value = TRUE)
  columns <- intersect(c(
    "candidate_id", "candidate_layer", "final_label",
    "start_isi", "end_isi", "n_isi", "score", "priority",
    evidence_columns, "semantic_track", "selected_within_track",
    "track_selection_status"
  ), names(x))
  x <- x[, columns, drop = FALSE]
  if (nrow(x) > 0L) {
    x <- x[order(x$candidate_id, x$start_isi, x$end_isi,
                 method = "radix", na.last = TRUE), , drop = FALSE]
  }
  rownames(x) <- NULL
  digest::digest(x, algo = "sha256")
}

# Active event grammar detector entry point.  A true label-blind call clears manual
# evidence before reaching this function, so its AUTO candidates are independent.
# Manual-aware calls deliberately retain the legacy generation-time lock for
# non-regression until Phase 3 moves manual annotations to track-scoped projection.
# FINAL-label logic elsewhere may remain manual-dominant.
stpd_detect_train_hf_protected_impl <- function(
    dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE,
    candidate_lineage_collector = NULL) {
  stpd_candidate_lineage_collector_capture(
    candidate_lineage_collector,
    "hf_protected_impl_entry_v1",
    data.frame(
      train = as.character(train)[1L],
      n_interval_rows = as.integer(nrow(dat)),
      min_isi_sec = as.numeric(min_isi_sec)[1L],
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
  params <- effective_params_for_detector(params)
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  n <- nrow(dat)
  if (!("pattern_manual_negative" %in% names(dat))) dat$pattern_manual_negative <- rep("", n)
  if (!("auto_score" %in% names(dat))) dat$auto_score <- rep(NA_real_, n)
  if (n <= 1) {
    dat$pattern_auto <- rep("", n)
    dat$auto_score <- rep(NA_real_, n)
    empty_multitrack_shadow <- stpd_multitrack_shadow_select(
      data.frame(), patterns = character(), params = params
    )
    attr(dat, "multitrack_shadow") <- empty_multitrack_shadow
    attr(dat, "multitrack_compatibility_shadow") <-
      stpd_multitrack_compatibility_shadow(empty_multitrack_shadow, params = params)
    attr(dat, "nested_hfs_burst_review_candidates") <- data.frame()
    attr(dat, "tonic_review_candidates") <-
      stpd_tonic_review_empty_candidates()
    return(dat)
  }

  vp <- stpd_event_grammar_params_impl(dat, params, min_isi_sec, train = train)
  manual_for_lock <- if (!is.null(dat$pattern_manual)) as.character(dat$pattern_manual) else rep("", n)
  manual_for_lock[is.na(manual_for_lock)] <- ""
  patterns <- params$detector$patterns_to_run %||% stpd_default_patterns_to_run()

  # The first Pause scan is an append-only diagnostic proposal ledger. It has
  # no boundary authority. Active Pause decisions are made only after Burst
  # support/bridge ownership is frozen below; this prevents a moderately long
  # internal Burst bridge from being pre-emptively relabelled as Pause.
  raw_pause_candidates <- data.frame()
  raw_pause_hard_boundaries <- stpd_event_core_pause_hard_boundaries(NULL)
  pause_hard_boundaries <- stpd_event_core_pause_hard_boundaries(NULL)
  burst_detection_dat <- dat
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_begin_pause_raw_canonical(
      candidate_lineage_collector, train,
      dat, params, vp, min_isi_sec, patterns
    )
  }
  if ("pause" %in% patterns) {
    raw_pause_candidates <- stpd_event_core_detect_pause(
      dat, params, vp, min_isi_sec, train,
      burst_candidates = NULL
    )
    raw_pause_hard_boundaries <- stpd_event_core_pause_hard_boundaries(
      raw_pause_candidates
    )
  }
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_capture_pause_raw_canonical(
      candidate_lineage_collector, train,
      dat, params, vp, min_isi_sec, patterns,
      raw_pause_candidates, raw_pause_hard_boundaries
    )
  }

  profile <- stpd_event_core_train_profile_row(dat, params, vp, min_isi_sec, train)
  cand_rows <- list(profile)
  b <- data.frame()
  hard_thr <- data.frame()
  hfs <- data.frame()
  selected_hfs <- data.frame()
  nested_raw <- stpd_nested_hfs_empty_candidates()
  nested_burst_candidates <- data.frame()
  hft <- data.frame()
  ton <- data.frame()
  burst_candidates <- data.frame()
  burst_pause_support <- data.frame()
  if (any(c("burst", "long_burst") %in% patterns)) {
    b <- stpd_event_grammar_detect_burst_events(
      burst_detection_dat, params, vp, min_isi_sec, train,
      candidate_lineage_collector = candidate_lineage_collector
    )
    burst_before_pause_boundary <- b
  }
  # Explicit hard-threshold candidates are an optional override layer, not the
  # initial burst screen.  They are generated now but appended only after the
  # HFS-local re-adjudication below.
  hard_thr_pre_boundary <- stpd_event_core_detect_hard_isi_thresholds(
    burst_detection_dat, params, vp, min_isi_sec, train
  )

  # Freeze ordinary Burst support before active Pause generation. HFS-local
  # Burst proposals remain review-only and never acquire Pause ownership.
  burst_candidates <- b
  burst_support_source <- b
  burst_route <- stpd_event_grammar_burst_detector_default(params)
  if (nrow(burst_support_source) > 0L) {
    burst_pause_support <- stpd_event_core_freeze_burst_bridges(
      dat, params, vp, burst_support_source,
      min_isi_sec = min_isi_sec, train = train,
      hard_boundaries = data.frame()
    )
  }
  # Close the ownership receipt immediately after the ordinary Burst output is
  # frozen.  Active Pause does not exist yet and therefore cannot be an input
  # to this ownership decision.
  if (!is.null(candidate_lineage_collector) &&
      identical(burst_route, "final")) {
    stpd_candidate_lineage_capture_burst_pause_ownership(
      candidate_lineage_collector, train,
      burst_support_source, burst_pause_support,
      dat, params, vp, min_isi_sec, data.frame()
    )
  }

  # Re-adjudicate the raw generic proposals against frozen Burst ownership,
  # then add contextual inter-Burst Pause. Only this post-ownership ledger may
  # create active canonical boundaries.
  pau <- data.frame()
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_begin_pause_contextual(
      candidate_lineage_collector, train, dat, params, vp, min_isi_sec,
      patterns, raw_pause_candidates, raw_pause_hard_boundaries,
      burst_pause_support, burst_route
    )
  }
  if ("pause" %in% patterns) {
    pau <- stpd_event_core_detect_pause(
      dat, params, vp, min_isi_sec, train,
      burst_candidates = burst_pause_support,
      generic_candidates = raw_pause_candidates
    )
    pause_hard_boundaries <- stpd_event_core_final_event_boundaries(pau)
  }
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_capture_pause_contextual(
      candidate_lineage_collector, train, dat, params, vp, min_isi_sec,
      patterns, raw_pause_candidates, raw_pause_hard_boundaries,
      burst_pause_support, burst_route, pau
    )
  }

  # Accepted post-ownership Pause support must be disjoint from the immutable
  # Burst support ledger.  A collision means the arbitration order or the
  # ownership mask has regressed; silently cutting the Burst would corrupt both
  # Event truth and the candidate-lineage receipt.
  if (nrow(pause_hard_boundaries) > 0L && nrow(burst_pause_support) > 0L) {
    burst_selected <- if ("selected_for_auto" %in% names(burst_pause_support)) {
      x <- as.logical(burst_pause_support$selected_for_auto)
      x[is.na(x)] <- FALSE
      x
    } else {
      rep(TRUE, nrow(burst_pause_support))
    }
    burst_labels <- as.character(burst_pause_support$final_label %||% "")
    owned <- burst_pause_support[
      burst_selected & burst_labels %in% c("burst", "long_burst"),
      , drop = FALSE
    ]
    if (nrow(owned) > 0L) {
      collision <- vapply(seq_len(nrow(pause_hard_boundaries)), function(i) {
        any(
          as.integer(owned$start_isi) <=
            as.integer(pause_hard_boundaries$end_isi[i]) &
          as.integer(owned$end_isi) >=
            as.integer(pause_hard_boundaries$start_isi[i])
        )
      }, logical(1))
      if (any(collision)) {
        stop(
          "Post-ownership Pause boundary overlaps frozen Burst support.",
          call. = FALSE
        )
      }
    }
  }

  # A post-ownership canonical Pause can only cut candidates that do not own
  # that ISI as Burst support. This is a consistency assertion, not a
  # Pause-first veto.
  b <- stpd_event_core_apply_hard_boundaries_to_burst_candidates(
    b, pause_hard_boundaries
  )
  hard_thr <- stpd_event_core_apply_hard_boundaries_to_burst_candidates(
    hard_thr_pre_boundary, pause_hard_boundaries
  )
  if (!is.null(candidate_lineage_collector) &&
      any(c("burst", "long_burst") %in% patterns) &&
      identical(stpd_event_grammar_burst_detector_default(params), "final")) {
    stpd_candidate_lineage_capture_burst_pause_boundary(
      candidate_lineage_collector, train,
      burst_before_pause_boundary, b,
      pau, pause_hard_boundaries
    )
  }
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_begin_burst_hard_threshold_root(
      candidate_lineage_collector, train,
      burst_detection_dat, params, vp, min_isi_sec, patterns,
      pau, pause_hard_boundaries
    )
    stpd_candidate_lineage_capture_burst_hard_threshold_root(
      candidate_lineage_collector, train,
      burst_detection_dat, params, vp, min_isi_sec, patterns,
      hard_thr_pre_boundary, hard_thr,
      pau, pause_hard_boundaries
    )
  }

  # State envelopes are generated from their own support evidence, without a
  # Burst or Pause veto. The multi-track materializer later subtracts accepted
  # Pause from direct support while preserving the accepted parent episode.
  hfs_generation_boundaries <- data.frame()
  # Tonic evidence is generated independently before Broad-HFS. Besides its
  # own State track, it may identify a sustained, clearly slower internal entry
  # inside an otherwise unbounded q25-envelope component. A q25 drought alone
  # never cuts HFS; both accepted Tonic evidence and a local median upshift are
  # required by the HFS rollback helper.
  tonic_guard <- stpd_event_core_detect_tonic(
    dat, params, vp, min_isi_sec, train
  )

  # HFT/HFI are subtype requests, not independent State-support generators. A
  # subtype request therefore always runs the unique Broad-HFS parent detector;
  # the legacy HFT candidate is retained below only as diagnostic concordance.
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_begin_broad_hfs_state(
      candidate_lineage_collector, train, dat, params, vp, min_isi_sec,
      patterns, hfs_generation_boundaries, pause_hard_boundaries
    )
  }
  if (any(c(
      "high_frequency_spiking", "high_frequency_tonic",
      "high_frequency_irregular_state"
    ) %in% patterns)) {
    hfs <- stpd_event_core_detect_hf_spiking(
      dat, params, vp, min_isi_sec, train,
      hard_boundaries = hfs_generation_boundaries,
      tonic_candidates = tonic_guard
    )
  }
  # Preserve the frozen observer dependency order: Broad-HFS begins before the
  # Tonic observer, although the label-blind Tonic evidence above was computed
  # independently and cannot inherit an HFS decision.
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_begin_tonic_state(
      candidate_lineage_collector, train, dat, params, vp, min_isi_sec,
      patterns, hfs_generation_boundaries
    )
  }
  if ("tonic" %in% patterns) ton <- tonic_guard
  state_resolution <- stpd_event_grammar_resolve_edge_tonic_hfs_states(
    dat, hfs, ton, params, vp,
    min_isi_sec = min_isi_sec, train = train,
    hard_boundaries = hfs_generation_boundaries
  )
  hfs <- state_resolution$hfs
  ton <- state_resolution$tonic

  # Broad HFS is frozen before its nested Event overlay is considered.  The
  # provisional parent selection uses the same Phase-1A State selector as the
  # final product, so Burst evidence can neither create nor remove a parent.
  if (nrow(hfs) > 0L) {
    selected_hfs <- stpd_nested_hfs_selected_parent_candidates(
      hfs, patterns = patterns, params = params
    )
  }
  provisional_hfs_signature <- stpd_nested_hfs_parent_signature(selected_hfs)

  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_begin_nested_hfs_review(
      candidate_lineage_collector, train, dat, params, vp, min_isi_sec,
      patterns, selected_hfs, pause_hard_boundaries
    )
  }

  if (nrow(selected_hfs) > 0L &&
      any(c("burst", "long_burst") %in% patterns)) {
    nested_raw <- stpd_event_core_detect_nested_hfs_bursts(
      dat,
      selected_hfs,
      hard_boundaries = pause_hard_boundaries,
      settings = stpd_nested_hfs_detector_settings(params, vp),
      min_isi_sec = min_isi_sec
    )
    nested_burst_candidates <- stpd_nested_hfs_standardize_candidates(
      dat, nested_raw, params, vp,
      min_isi_sec = min_isi_sec, train = train
    )
  }

  if (nrow(b) > 0L) cand_rows[[length(cand_rows) + 1L]] <- b
  if (nrow(hard_thr) > 0L) cand_rows[[length(cand_rows) + 1L]] <- hard_thr
  if (nrow(hfs) > 0L) cand_rows[[length(cand_rows) + 1L]] <- hfs
  if ("high_frequency_tonic" %in% patterns) {
    hft <- stpd_event_core_detect_hf_tonic(dat, params, vp, min_isi_sec, train)
  }
  if ("tonic" %in% patterns) {
    if (nrow(ton) > 0) cand_rows[[length(cand_rows) + 1L]] <- ton
  }
  if (nrow(pau) > 0) cand_rows[[length(cand_rows) + 1L]] <- pau

  audit <- dplyr::bind_rows(cand_rows)
  shadow_audit <- dplyr::bind_rows(audit, nested_burst_candidates)
  # Phase 1A shadow: select independently by semantic track from the intact
  # pre-protection pool.  HFS-local possible_burst proposals are visible only
  # on the Review track.  The legacy audit and AUTO labels continue through the
  # canonical candidate pool below and therefore cannot be changed by them.
  multitrack_shadow <- stpd_multitrack_shadow_select(
    shadow_audit, patterns = patterns, params = params
  )
  final_hfs_signature <- stpd_nested_hfs_parent_signature(
    multitrack_shadow$candidates
  )
  if (!identical(provisional_hfs_signature, final_hfs_signature)) {
    stop(
      "Nested-HFS Burst adjudication changed the selected Broad-HFS parent evidence.",
      call. = FALSE
    )
  }
  multitrack_compatibility_shadow <- stpd_multitrack_compatibility_shadow(
    multitrack_shadow,
    dat = dat,
    params = params,
    variable_params = vp,
    min_isi_sec = min_isi_sec
  )
  audit_pre_final_gap_arbitration <- audit
  audit <- stpd_event_grammar_protect_hf_spiking_states(audit)
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_begin_gap_final(
      candidate_lineage_collector, train,
      audit_pre_final_gap_arbitration, audit, patterns, dat, params,
      min_isi_sec
    )
  }
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
  # Materialize the review-only Tonic side channel once so both the returned
  # attribute and the final direct-observation product binding hash the exact
  # same immutable table.
  tonic_review_candidates <- stpd_tonic_review_candidates(
    dat, params, vp, min_isi_sec = min_isi_sec, train = train,
    canonical_tonic = ton, pause_boundaries = pause_hard_boundaries
  )
  # Freeze the exact object that will be returned before the observer-only
  # complete-universe release. The release hashes this full product, including
  # AUTO scores, both multitrack shadows, parameters, and review side channels.
  attr(dat, "tonic_review_candidates") <- tonic_review_candidates
  # Legacy HFT is diagnostic concordance only. Formal HFT/HFI products are
  # projected from a selected Broad-HFS parent on that parent's frozen support;
  # this side channel therefore has no AUTO-selection authority.
  attr(dat, "legacy_hft_diagnostic") <- hft
  attr(dat, "candidate_diagnostic_audit") <- audit
  attr(dat, "multitrack_shadow") <- multitrack_shadow
  attr(dat, "multitrack_compatibility_shadow") <- multitrack_compatibility_shadow
  attr(dat, "event_grammar_params") <- vp
  attr(dat, "nested_hfs_parent_signature") <- provisional_hfs_signature
  attr(dat, "nested_hfs_burst_review_candidates") <- nested_burst_candidates
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_capture_gap_final(
      candidate_lineage_collector, train, audit, dat$pattern_auto
    )
    # Broad-HFS is generated earlier, but its observer closes only after the
    # final Gap root so the Gate 1B dependency order is explicit. This delayed
    # capture is read-only and uses the already frozen parent signatures.
    stpd_candidate_lineage_capture_broad_hfs_state(
      candidate_lineage_collector, train, hfs, selected_hfs,
      provisional_hfs_signature, final_hfs_signature
    )
    # Tonic-State closes after Broad-HFS so the frozen Gate 1B dependency is
    # explicit. Capture is observer-only and reruns Pause-created fragments on
    # isolated support without altering the scientific candidates above.
    stpd_candidate_lineage_capture_tonic_state(
      candidate_lineage_collector, train, ton, hft, burst_support_source
    )
    stpd_candidate_lineage_capture_nested_hfs_review(
      candidate_lineage_collector, train, nested_raw,
      nested_burst_candidates, multitrack_shadow,
      provisional_hfs_signature, final_hfs_signature
    )
    # Gate 1B closes only after every family observer above has completed.
    # This release is internal and observer-only: it binds final products and
    # existing caps, but emits no candidate and grants no publication authority.
    if (stpd_candidate_lineage_complete_universe_release_eligible(
        candidate_lineage_collector)) {
      stpd_candidate_lineage_begin_complete_universe_release(
        candidate_lineage_collector, train, audit, dat$pattern_auto,
        multitrack_shadow, nested_burst_candidates, tonic_review_candidates,
        final_hfs_signature, dat, params, vp
      )
      stpd_candidate_lineage_capture_complete_universe_release(
        candidate_lineage_collector, train
      )
    }
  }
  dat
}

stpd_detect_train_hf_protected <- function(
    dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE,
    candidate_lineage_collector = NULL) {
  params <- effective_params_for_detector(params)
  if (isTRUE((params$event_grammar %||% list())$enabled %||% TRUE)) {
    return(stpd_detect_train_product_hardened(
      dat, params, min_isi_sec = min_isi_sec, train = train,
      lock_manual = lock_manual,
      candidate_lineage_collector = candidate_lineage_collector
    ))
  }
  stpd_train_pipeline_event_grammar_core(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}
