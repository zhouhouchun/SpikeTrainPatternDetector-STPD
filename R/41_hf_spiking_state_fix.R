# ============================================================
# event grammar HF-spiking state detector fix
# ------------------------------------------------------------
# Rationale:
#   High-frequency spiking is a long state/epoch, not a burst event.  It is
#   characterized by many spikes, long duration, and predominantly short ISIs,
#   while allowing occasional moderately larger ISIs.  The earlier event core/event grammar state
#   detector could miss such epochs because:
#     (1) the run-building break threshold could be inflated by pause settings
#         (e.g. 100 ms), causing a long run to include non-HF tails and fail q90;
#     (2) q90 was used as a hard gate even though a valid HF-spiking epoch may
#         have a small fraction of 20-25 ms ISIs;
#     (3) event grammar threshold bands were mapped too narrowly onto hf_spiking_q90_max.
#
# This patch keeps the event-grammar architecture and only replaces the
# HF-spiking state layer plus the event grammar detector parameter resolver.
# ============================================================

stpd_event_grammar_params_impl <- function(dat, params, min_isi_sec = 0.001, train = "") {
  if (is.null((params$event_grammar %||% list())$effective_bands)) {
    params <- stpd_attach_thresholds_to_params_impl(
      params,
      ds = list(trains = list(current = dat)),
      min_isi_sec = min_isi_sec
    )
  }

  # Start from the event-core parameter object, then apply resolved event-grammar bands.
  vp <- stpd_event_core_params_impl(dat, params, min_isi_sec)
  eg <- params$event_grammar %||% list()
  b <- eg$effective_bands %||% list()

  if (!is.null(b$burst)) {
    vp$seed_low <- b$burst$seed_lower_sec
    vp$seed_high <- b$burst$seed_upper_sec
    vp$bridge_high <- b$burst$bridge_upper_sec
    if (is.finite(b$burst$contrast_S)) vp$S <- b$burst$contrast_S
  }

  # HF spiking: keep separate concepts.
  #   - short_upper: ISIs considered strongly supportive of high-frequency state
  #   - q80/q90 max: robust epoch-level high-frequency compactness limits
  #   - epoch_bridge: moderate ISIs still allowed inside a long HF epoch
  #   - hard_break: split an epoch only at genuinely non-HF gaps
  if (!is.null(b$high_frequency_spiking)) {
    hp <- params$highfreq %||% list()
    seed_lower <- suppressWarnings(as.numeric(
      b$high_frequency_spiking$seed_lower_sec
    ))
    short_upper <- suppressWarnings(as.numeric(
      b$high_frequency_spiking$fast_core_upper_sec %||%
        b$high_frequency_spiking$seed_upper_sec
    ))
    epoch_bridge <- suppressWarnings(as.numeric(
      b$high_frequency_spiking$envelope_upper_sec %||%
        b$high_frequency_spiking$bridge_upper_sec
    ))
    connector_upper <- suppressWarnings(as.numeric(
      b$high_frequency_spiking$connector_upper_sec %||% epoch_bridge
    ))
    ui_q90 <- suppressWarnings(as.numeric(hp$spiking_q90_max_ISI_sec %||% 0.025))
    ui_bridge <- suppressWarnings(as.numeric(hp$spiking_epoch_bridge_ISI_sec %||% 0.035))

    # The threshold resolver has already applied the declared precedence
    # (user > manual > histogram > default).  Treat its effective band as the
    # authoritative detector input.  Taking max(..., ui_default) here silently
    # overrode valid resolved values and reintroduced fixed-millisecond
    # behavior into otherwise scale-equivariant automatic thresholds.
    if (!is.finite(short_upper) || short_upper <= 0) short_upper <- ui_q90
    if (!is.finite(epoch_bridge) || epoch_bridge <= 0) epoch_bridge <- ui_bridge
    if (!is.finite(connector_upper) || connector_upper < epoch_bridge) {
      connector_upper <- epoch_bridge
    }
    if (!is.finite(ui_q90) || ui_q90 <= 0) ui_q90 <- 0.025
    if (!is.finite(ui_bridge) || ui_bridge <= 0) ui_bridge <- 0.035

    if (!is.finite(seed_lower) || seed_lower < min_isi_sec) {
      seed_lower <- min_isi_sec
    }
    vp$hf_spiking_seed_lower <- seed_lower
    vp$hf_spiking_fast_core_upper <- max(min_isi_sec, short_upper)
    vp$hf_spiking_short_upper <- vp$hf_spiking_fast_core_upper
    # q80/q90 compactness must be consistent with the declared connector budget.
    # Requiring q80 <= envelope while simultaneously allowing up to 25% bounded
    # connectors is contradictory. The envelope fraction and connector ceiling
    # provide the dimensionless robust limits instead.
    vp$hf_spiking_q80_max <- max(epoch_bridge, vp$hf_spiking_short_upper)
    vp$hf_spiking_q90_max <- max(connector_upper, epoch_bridge)
    vp$hf_spiking_epoch_bridge <- max(
      epoch_bridge, vp$hf_spiking_short_upper, na.rm = TRUE
    )
    vp$hf_spiking_envelope_upper <- vp$hf_spiking_epoch_bridge
    vp$hf_spiking_connector_upper <- max(
      connector_upper, vp$hf_spiking_envelope_upper, na.rm = TRUE
    )
    # The automatic histogram proposal uses q25 only as an anchor.  A candidate
    # must retain a non-trivial amount of that anchor after it is expanded over
    # the stable-valley/q75 proposal envelope; this prevents a small fast seed from absorbing a
    # long Tonic-like tail.  These are dimensionless detector-shape defaults and
    # may be overridden by an explicit highfreq contract.
    vp$hf_spiking_fast_core_min_count <- max(
      3L,
      as.integer(hp$spiking_fast_core_min_isi_count %||%
        ceiling(max(1L, vp$hf_spiking_min_spikes - 1L) * 0.15))
    )
    vp$hf_spiking_fast_core_fraction_min <- min(max(
      suppressWarnings(as.numeric(
        hp$spiking_fast_core_fraction_min %||% 0.10
      )), 0.05), 1)
    vp$hf_spiking_envelope_expansion_core_fraction_min <- min(max(
      suppressWarnings(as.numeric(
        hp$spiking_envelope_expansion_core_fraction_min %||% 0.10
      )), vp$hf_spiking_fast_core_fraction_min), 1)
    vp$hf_spiking_envelope_fraction_min <- min(max(
      suppressWarnings(as.numeric(
        hp$spiking_envelope_fraction_min %||% 0.75
      )), 0.50), 1)
    proposal_connector_ratio <- vp$hf_spiking_connector_upper /
      vp$hf_spiking_envelope_upper
    vp$hf_spiking_envelope_q80_ratio_max <- max(1,
      suppressWarnings(as.numeric(
        hp$spiking_envelope_q80_ratio_max %||% proposal_connector_ratio
      )))
    vp$hf_spiking_envelope_q90_ratio_max <- max(
      vp$hf_spiking_envelope_q80_ratio_max,
      suppressWarnings(as.numeric(
        hp$spiking_envelope_q90_ratio_max %||% proposal_connector_ratio
      )), na.rm = TRUE
    )
    # Do NOT inherit pause-scale hard breaks.  HF-spiking epochs should be split
    # when the gap leaves the HF state, not only at pause-sized gaps.
    vp$hf_spiking_hard_break <- max(vp$hf_spiking_epoch_bridge, 1.5 * vp$hf_spiking_q90_max, na.rm = TRUE)
    vp$hf_spiking_break_isi <- vp$hf_spiking_hard_break

    hfs_sources <- as.character(c(
      b$high_frequency_spiking$seed_lower_sec_source %||% "",
      b$high_frequency_spiking$seed_upper_sec_source %||% "",
      b$high_frequency_spiking$bridge_upper_sec_source %||% ""
    ))
    hfs_sources <- unique(hfs_sources[!is.na(hfs_sources) &
      nzchar(hfs_sources)])
    if (!length(hfs_sources)) {
      hfs_sources <- as.character(eg$threshold_source_mode %||% "auto")[1L]
    }
    hfs_source <- if (any(hfs_sources %in% c("histogram", "auto"))) {
      "histogram"
    } else {
      as.character(
        b$high_frequency_spiking$seed_upper_sec_source %||% hfs_sources[1L]
      )[1L]
    }
    hfs_hist <- (eg$histogram_suggest %||%
      list())$high_frequency_spiking %||% list()
    vp$hf_spiking_threshold_source_mode <- hfs_source
    vp$hf_spiking_threshold_field_sources <- paste(
      sort(hfs_sources), collapse = ";"
    )
    cached_background_contract <- hfs_hist$requires_candidate_local_background
    automatic_hfs_source <- hfs_source %in% c("histogram", "auto")
    # An automatic proposal is self-referential unless candidate-local slower
    # background is demonstrated. A stale/malformed cache entry may not turn
    # that scientific gate off; explicit FALSE is retained only as an audit
    # inconsistency while the detector fails closed.
    vp$hf_spiking_auto_requires_local_background <- automatic_hfs_source
    vp$hf_spiking_background_contract_consistent <-
      !automatic_hfs_source ||
      !identical(cached_background_contract, FALSE)
    vp$hf_spiking_histogram_available <- isTRUE(hfs_hist$available)
    vp$hf_spiking_histogram_status <- as.character(
      hfs_hist$status %||% "not_applicable"
    )[1L]
    vp$hf_spiking_histogram_reason <- as.character(
      hfs_hist$reason %||% ""
    )[1L]
    vp$hf_spiking_envelope_proposal_status <- as.character(
      b$high_frequency_spiking$envelope_proposal_status %||%
        (hfs_hist$envelope_proposal %||% list())$status %||% ""
    )[1L]
    vp$hf_spiking_envelope_proposal_method <- as.character(
      b$high_frequency_spiking$envelope_proposal_method %||%
        (hfs_hist$envelope_proposal %||% list())$method %||% ""
    )[1L]
  }

  if (!is.null(b$high_frequency_tonic)) {
    vp$hf_tonic_floor <- b$high_frequency_tonic$seed_lower_sec
    vp$hf_tonic_high_max <- b$high_frequency_tonic$seed_upper_sec
    vp$hf_tonic_bridge_upper <- b$high_frequency_tonic$bridge_upper_sec
  }
  if (!is.null(b$tonic)) {
    vp$tonic_min <- b$tonic$seed_lower_sec
    vp$tonic_max <- b$tonic$seed_upper_sec
    vp$tonic_bridge_upper <- b$tonic$bridge_upper_sec
  }
  if (!is.null(b$pause)) {
    vp$pause_thr <- b$pause$seed_lower_sec
    vp$pause_entry_thr <- b$pause$seed_lower_sec
    vp$pause_strong_thr <- b$pause$seed_upper_sec
    pause_source <- as.character(
      b$pause$seed_upper_sec_source %||%
        (eg$threshold_source_mode %||% "auto")
    )[1L]
    pause_tail <- (eg$histogram_suggest %||% list())$pause$strong_tail %||%
      list()
    pause_status <- if (identical(pause_source, "histogram")) {
      as.character(pause_tail$status %||% "unresolved_histogram_tail")[1L]
    } else {
      paste0("resolved_", pause_source, "_strong_threshold")
    }
    vp$pause_strong_active <- !identical(pause_source, "histogram") ||
      identical(pause_status, "resolved_stable_log_kde_upper_tail")
    vp$pause_strong_status <- pause_status
    vp$pause_threshold_source_mode <- pause_source
  }
  vp <- stpd_apply_train_isi_thresholds_to_event_vp(vp, params, train = train, min_isi_sec = min_isi_sec)
  if (exists("stpd_apply_bounded_borrowing_to_event_vp", mode = "function")) {
    vp <- stpd_apply_bounded_borrowing_to_event_vp(
      vp, dat, params, train = train, min_isi_sec = min_isi_sec
    )
  }
  vp$tonic_burst_overlap_ref <- suppressWarnings(max(c(
    stpd_event_grammar_num(vp$tonic_burst_overlap_ref, NA_real_),
    stpd_event_grammar_num(vp$seed_high, NA_real_),
    stpd_event_grammar_num(vp$bridge_high, NA_real_)
  ), na.rm = TRUE))
  vp$threshold_table <- eg$threshold_table
  vp
}

# Robust long-HF-spiking detector.  This replaces the older event core state detector.
stpd_event_core_detect_hf_spiking <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat)
  if (n <= 2) return(data.frame())

  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE

  short_upper <- suppressWarnings(as.numeric(vp$hf_spiking_short_upper %||% vp$hf_spiking_q90_max))
  q80_max <- suppressWarnings(as.numeric(vp$hf_spiking_q80_max %||% vp$hf_spiking_q90_max))
  q90_max <- suppressWarnings(as.numeric(vp$hf_spiking_q90_max %||% 0.025))
  epoch_bridge <- suppressWarnings(as.numeric(vp$hf_spiking_epoch_bridge %||% 0.035))
  hard_break <- suppressWarnings(as.numeric(vp$hf_spiking_hard_break %||% vp$hf_spiking_break_isi %||% epoch_bridge))

  if (!is.finite(short_upper) || short_upper <= 0) short_upper <- q90_max
  if (!is.finite(q80_max) || q80_max <= 0) q80_max <- q90_max
  if (!is.finite(q90_max) || q90_max <= 0) q90_max <- 0.025
  if (!is.finite(epoch_bridge) || epoch_bridge <= 0) epoch_bridge <- max(0.035, q90_max)
  if (!is.finite(hard_break) || hard_break <= 0) hard_break <- max(epoch_bridge, 1.5 * q90_max)
  pause_break <- suppressWarnings(as.numeric(vp$pause_thr %||% NA_real_))[1]
  if (!is.finite(pause_break) || pause_break <= 0) pause_break <- NA_real_

  # Keep hard_break in the HF regime.  This prevents a pause/tonic tail from
  # being merged into the HF epoch and inflating q90 until the candidate fails.
  hard_break <- max(hard_break, epoch_bridge, 1.5 * q90_max, na.rm = TRUE)
  hard_break <- min(hard_break, max(0.060, 2.5 * q90_max, epoch_bridge * 1.5, na.rm = TRUE), na.rm = TRUE)

  base_flag <- valid & isi <= hard_break
  if (is.finite(pause_break)) base_flag <- base_flag & isi < pause_break
  runs <- stpd_event_core_bool_runs(base_flag)
  rows <- list(); counter <- 0L

  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    idx <- stpd_event_core_safe_seq(s, e)
    if (length(idx) == 0) next
    vals <- isi[idx][valid[idx]]
    if (length(vals) == 0) next

    n_spikes <- e - s + 2L
    if (n_spikes < vp$hf_spiking_min_spikes) next

    duration <- if ("timestamp_sec" %in% names(dat)) {
      suppressWarnings(as.numeric(dat$timestamp_sec[e]) - as.numeric(dat$timestamp_sec[s - 1L]))
    } else NA_real_
    if (is.finite(vp$hf_spiking_min_duration) && vp$hf_spiking_min_duration > 0 &&
        (!is.finite(duration) || duration < vp$hf_spiking_min_duration)) next

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

    # HF-spiking acceptance: predominantly short ISIs.  q90 can be slightly high
    # when there are legitimate occasional 20-25 ms ISIs, so a q80/short-fraction
    # route is accepted in addition to the strict q90 route.
    strict_q90_pass <- is.finite(q90) && q90 <= q90_max
    robust_majority_pass <- is.finite(q80) && q80 <= q80_max &&
      (short_frac >= vp$hf_spiking_short_fraction_min || q90_short_frac >= vp$hf_spiking_short_fraction_min)
    bridge_pass <- bridge_frac >= max(0.75, vp$hf_spiking_short_fraction_min)
    large_pass <- large_frac <= vp$hf_spiking_allowed_large_frac &&
      max_consec_large <= vp$hf_spiking_max_consec_large

    pass <- (strict_q90_pass || robust_majority_pass) && bridge_pass && large_pass
    if (!pass) next

    score <- 18 + 0.05 * n_spikes + 2.0 * short_frac + 1.5 * q90_short_frac +
      1.0 * bridge_frac - 2.5 * large_frac
    counter <- counter + 1L
    row <- stpd_event_core_candidate_from_run(
      dat, s, e, params, vp, min_isi_sec, train,
      "event_grammar_hf_spiking_state", "event_grammar_long_hf_spiking_epoch", "high_frequency_spiking",
      "event_grammar_hf_spiking_pass", "long_high_frequency_epoch_predominantly_short_isi_without_burst_event_requirement", "accept",
      score, 780,
      list(
        candidate_id = paste0("event_grammar_hfs_", counter),
        hf_spiking_q50_sec = q50,
        hf_spiking_q80_sec = q80,
        hf_spiking_q90_sec = q90,
        hf_spiking_q95_sec = q95,
        hf_spiking_short_upper_sec = short_upper,
        hf_spiking_q80_max_sec = q80_max,
        hf_spiking_q90_max_sec = q90_max,
        hf_spiking_epoch_bridge_sec = epoch_bridge,
        hf_spiking_hard_break_sec = hard_break,
        hf_spiking_pause_break_sec = pause_break,
        hf_spiking_short_fraction = short_frac,
        hf_spiking_q90_short_fraction = q90_short_frac,
        hf_spiking_bridge_fraction = bridge_frac,
        hf_spiking_large_fraction = large_frac,
        hf_spiking_max_consecutive_large_isi = max_consec_large,
        hf_spiking_min_spikes_required = vp$hf_spiking_min_spikes,
        hf_spiking_acceptance_route = if (strict_q90_pass) "strict_q90" else "robust_q80_majority"
      )
    )
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}
