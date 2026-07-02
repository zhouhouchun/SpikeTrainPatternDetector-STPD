# ============================================================
# dataset ISI Dataset-level ISI-band, seed-centered burst event detector
# ============================================================
# Biological semantics implemented here:
#   * Dataset-level ISI band defines the burst-core seed interval.
#   * Per-train percentiles are OUTPUT/AUDIT metrics, not the primary burst seed gate.
#   * Candidate generation is seed-centered: core seed runs are identified first;
#     bridge ISIs only expand a seed locally and are never allowed to define the event body.
#   * Canonical burst / long_burst still require the classical
#       large-ISI -- compact short-ISI packet -- large-ISI structure.
#   * HF spiking is a long high-frequency epoch and remains distinct from long_burst.

stpd_dataset_isi_clamp <- function(x, lo, hi, default = NA_real_) {
  y <- stpd_seed_bridge_num(x, default)
  if (!is.finite(y)) return(default)
  min(max(y, lo), hi)
}

stpd_dataset_isi_is_enabled <- function(params) {
  ec <- params$event_core %||% list()
  isTRUE(ec$dataset_seed_band_enabled %||% ec$dataset_isi_burst_enabled %||% TRUE)
}

stpd_dataset_isi_params <- function(params, min_isi_sec = 0.001) {
  ec <- params$event_core %||% list()
  seed_low <- stpd_seed_bridge_num(ec$seed_band_lower_sec %||% 0.001, 0.001)
  seed_high <- stpd_seed_bridge_num(ec$seed_band_upper_sec %||% 0.010, 0.010)
  if (!is.finite(seed_low) || seed_low < 0) seed_low <- 0
  if (!is.finite(seed_high) || seed_high <= 0) seed_high <- 0.010
  if (seed_low >= seed_high) seed_low <- max(0, min_isi_sec)

  bridge_high <- stpd_seed_bridge_num(ec$bridge_band_upper_sec %||% 0.015, 0.015)
  if (!is.finite(bridge_high) || bridge_high <= 0) bridge_high <- seed_high * 1.5
  bridge_high <- max(bridge_high, seed_high)

  boundary_floor <- stpd_seed_bridge_num(ec$boundary_floor_sec %||% 0.025, 0.025)
  if (!is.finite(boundary_floor) || boundary_floor < 0) boundary_floor <- 0

  list(
    seed_low = seed_low,
    seed_high = seed_high,
    bridge_high = bridge_high,
    boundary_floor = boundary_floor,
    classicity_min = stpd_seed_bridge_num(ec$burst_contrast_min %||% 3.0, 3.0),
    possible_classicity = stpd_seed_bridge_num(ec$possible_burst_contrast_min %||% 2.0, 2.0),
    min_seed_isi_n = max(1L, stpd_seed_bridge_int(ec$min_seed_isi_count %||% 2L, 2L)),
    max_bridge_n = max(0L, stpd_seed_bridge_int(ec$max_bridge_isi_count %||% 4L, 4L)),
    max_bridge_frac = stpd_dataset_isi_clamp(ec$max_bridge_isi_fraction %||% 0.60, 0, 1, 0.60),
    max_expansion_steps = max(0L, stpd_seed_bridge_int(ec$max_expansion_isi_each_side %||% 4L, 4L)),
    context_min = stpd_seed_bridge_num(ec$context_compression_min %||% 1.00, 1.00),
    edge_return_min = stpd_seed_bridge_num(ec$edge_return_min %||% 0.00, 0.00),
    use_pct_gate = isTRUE(ec$use_train_percentile_as_seed_gate %||% FALSE),
    pct_gate_max = stpd_dataset_isi_clamp(ec$seed_percentile_gate_max %||% 0, 0, 100, 0),
    max_candidates_per_train = max(100L, stpd_seed_bridge_int(ec$max_candidates_per_train %||% 2500L, 2500L)),
    allow_boundary_possible = isTRUE(ec$allow_boundary_possible_burst %||% TRUE)
  )
}

stpd_dataset_isi_train_seed_profile <- function(dat, params, min_isi_sec = 0.001, train = "") {
  vp <- stpd_dataset_isi_params(params, min_isi_sec)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  vals <- isi[valid]
  n_valid <- length(vals)
  seed_low_pct <- seed_high_pct <- seed_band_fraction <- median_isi <- pause_fraction <- NA_real_
  if (n_valid > 0) {
    seed_low_pct <- mean(vals <= vp$seed_low, na.rm = TRUE) * 100
    seed_high_pct <- mean(vals <= vp$seed_high, na.rm = TRUE) * 100
    seed_band_fraction <- mean(vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE)
    median_isi <- stats::median(vals, na.rm = TRUE)
    pause_thr <- stpd_seed_bridge_num((params$pause %||% list())$T_seed %||% 0.100, 0.100)
    pause_fraction <- mean(vals >= pause_thr, na.rm = TRUE)
  }
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  runs <- stpd_seed_bridge_bool_runs(seed_flag)
  max_run <- if (nrow(runs) > 0) max(runs$end_isi - runs$start_isi + 1L) else 0L
  phenotype <- "mixed"
  if (n_valid < 10) phenotype <- "low_spike_count_unreliable"
  else if (is.finite(seed_band_fraction) && seed_band_fraction >= 0.40 && max_run >= 10L) phenotype <- "hf_spiking_like_seed_dominant"
  else if (is.finite(seed_band_fraction) && seed_band_fraction <= 0.02) phenotype <- "tonic_or_slow_seed_sparse"
  else if (is.finite(pause_fraction) && pause_fraction >= 0.25) phenotype <- "pause_dominant"
  else if (nrow(runs) >= 2 && is.finite(seed_band_fraction) && seed_band_fraction > 0.02) phenotype <- "burst_capable"

  data.frame(
    train = as.character(train %||% ""),
    candidate_layer = "dataset_isi_train_seed_band_profile",
    candidate_class = "train_profile",
    final_label = "profile",
    gate_status = "profile",
    decision_path = "dataset_seed_band_percentiles_are_outputs_not_seed_gates",
    action = "audit_only",
    start_isi = NA_integer_,
    end_isi = NA_integer_,
    n_valid_isi = as.integer(n_valid),
    dataset_isi_seed_low_sec = vp$seed_low,
    dataset_isi_seed_high_sec = vp$seed_high,
    dataset_isi_bridge_high_sec = vp$bridge_high,
    dataset_isi_boundary_floor_sec = vp$boundary_floor,
    seed_low_percentile_in_train = seed_low_pct,
    seed_high_percentile_in_train = seed_high_pct,
    seed_band_fraction = seed_band_fraction,
    seed_run_count = as.integer(nrow(runs)),
    max_seed_run_length = as.integer(max_run),
    median_ISI_sec = median_isi,
    pause_fraction = pause_fraction,
    phenotype_prior = phenotype,
    stringsAsFactors = FALSE
  )
}

stpd_dataset_isi_thresholds <- function(dat, params, min_isi_sec = 0.001) {
  vp <- stpd_dataset_isi_params(params, min_isi_sec)
  profile <- stpd_dataset_isi_train_seed_profile(dat, params, min_isi_sec, train = "")
  list(
    core_thr = vp$seed_high,
    core_abs = vp$seed_high,
    core_pct_thr = NA_real_,
    core_pct_seed_thr = NA_real_,
    core_pct = NA_real_,
    bridge_thr = vp$bridge_high,
    bridge_factor = if (vp$seed_high > 0) vp$bridge_high / vp$seed_high else NA_real_,
    seed_low = vp$seed_low,
    boundary_floor = vp$boundary_floor,
    seed_high_percentile_in_train = profile$seed_high_percentile_in_train[1],
    seed_band_fraction = profile$seed_band_fraction[1],
    phenotype_prior = profile$phenotype_prior[1]
  )
}

stpd_seed_bridge_thresholds <- function(dat, params, min_isi_sec = 0.001) {
  if (stpd_dataset_isi_is_enabled(params)) {
    return(stpd_dataset_isi_thresholds(dat, params, min_isi_sec))
  }
  stpd_seed_bridge_thresholds_classicity(dat, params, min_isi_sec)
}

stpd_dataset_isi_left_extensions <- function(seed_s, isi, valid, bridge_high, max_steps) {
  starts <- as.integer(seed_s)
  cur <- as.integer(seed_s)
  steps <- 0L
  while (cur > 2L && steps < max_steps) {
    cand <- cur - 1L
    if (!isTRUE(valid[cand]) || !is.finite(isi[cand]) || isi[cand] > bridge_high) break
    starts <- c(starts, cand)
    cur <- cand
    steps <- steps + 1L
  }
  unique(as.integer(starts))
}

stpd_dataset_isi_right_extensions <- function(seed_e, n, isi, valid, bridge_high, max_steps) {
  ends <- as.integer(seed_e)
  cur <- as.integer(seed_e)
  steps <- 0L
  while (cur < n && steps < max_steps) {
    cand <- cur + 1L
    if (cand > n || !isTRUE(valid[cand]) || !is.finite(isi[cand]) || isi[cand] > bridge_high) break
    ends <- c(ends, cand)
    cur <- cand
    steps <- steps + 1L
  }
  unique(as.integer(ends))
}

stpd_dataset_isi_overlap_key <- function(s, e) paste0(as.integer(s), "_", as.integer(e))

stpd_dataset_isi_interval_overlaps_any <- function(s, e, chosen) {
  if (length(chosen) == 0) return(FALSE)
  for (z in chosen) {
    if (s <= z[2] && e >= z[1]) return(TRUE)
  }
  FALSE
}

stpd_dataset_isi_select_nonoverlapping <- function(cands) {
  if (is.null(cands) || nrow(cands) == 0) return(cands)
  cands$proposed_final_label <- as.character(cands$final_label %||% "")
  cands$selected_for_auto <- FALSE
  cands$selection_status <- "not_selected"
  # accepted burst-family first, then possible_burst. Rejected/audit rows remain unselected.
  priority <- ifelse(cands$final_label %in% c("burst", "long_burst"), 3L,
                     ifelse(cands$final_label == "possible_burst", 2L, 0L))
  sc <- suppressWarnings(as.numeric(cands$score))
  sc[!is.finite(sc)] <- 0
  ord <- order(-priority, -sc, suppressWarnings(as.numeric(cands$start_isi)), suppressWarnings(as.numeric(cands$end_isi)))
  chosen <- list()
  for (ii in ord) {
    if (priority[ii] <= 0) next
    s <- as.integer(cands$start_isi[ii]); e <- as.integer(cands$end_isi[ii])
    if (!is.finite(s) || !is.finite(e) || e < s) next
    if (!stpd_dataset_isi_interval_overlaps_any(s, e, chosen)) {
      cands$selected_for_auto[ii] <- TRUE
      cands$selection_status[ii] <- "selected"
      chosen[[length(chosen) + 1L]] <- c(s, e)
    } else {
      cands$selection_status[ii] <- "rejected_overlap_with_higher_score_seed_centered_candidate"
    }
  }
  demote <- !cands$selected_for_auto & cands$final_label %in% c("burst", "long_burst", "possible_burst")
  cands$final_label[demote] <- "reject"
  cands$action[demote] <- "reject_overlap"
  cands$gate_status[demote] <- cands$selection_status[demote]
  cands
}

stpd_dataset_isi_detect_burst_seed_centered <- function(dat, params, min_isi_sec = 0.001, train = "") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  n <- nrow(dat)
  empty <- data.frame()
  if (n <= 2) return(empty)
  vp <- stpd_dataset_isi_params(params, min_isi_sec)
  bp <- params$burst %||% list()
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  if (isTRUE(vp$use_pct_gate) && is.finite(vp$pct_gate_max) && vp$pct_gate_max > 0) {
    pct <- suppressWarnings(as.numeric(dat$ISI_pct))
    seed_flag <- seed_flag & is.finite(pct) & pct <= vp$pct_gate_max
  }
  seed_runs <- stpd_seed_bridge_bool_runs(seed_flag)
  profile <- stpd_dataset_isi_train_seed_profile(dat, params, min_isi_sec, train = train)
  if (nrow(seed_runs) == 0) return(profile)

  classic_max_spikes <- stpd_seed_bridge_int(bp$classic_burst_max_spikes %||% 10L, 10L)
  long_min_spikes <- stpd_seed_bridge_int(bp$long_burst_min_spikes %||% 11L, 11L)
  long_max_spikes <- stpd_seed_bridge_int(bp$long_burst_max_spikes %||% 15L, 15L)
  min_spikes <- stpd_seed_bridge_int(bp$G_min %||% 3L, 3L)
  max_rows <- vp$max_candidates_per_train

  rows <- list(profile)
  seen <- list()
  candidate_counter <- 0L
  for (ri in seq_len(nrow(seed_runs))) {
    seed_s <- as.integer(seed_runs$start_isi[ri]); seed_e <- as.integer(seed_runs$end_isi[ri])
    if (!is.finite(seed_s) || !is.finite(seed_e) || seed_s < 2L || seed_e > n || seed_e < seed_s) next
    seed_core_n <- sum(seed_flag[seed_s:seed_e], na.rm = TRUE)
    if (seed_core_n < vp$min_seed_isi_n) next
    lefts <- stpd_dataset_isi_left_extensions(seed_s, isi, valid, vp$bridge_high, vp$max_expansion_steps)
    rights <- stpd_dataset_isi_right_extensions(seed_e, n, isi, valid, vp$bridge_high, vp$max_expansion_steps)
    for (s in lefts) {
      for (e in rights) {
        if (length(rows) >= max_rows) break
        key <- stpd_dataset_isi_overlap_key(s, e)
        if (!is.null(seen[[key]])) next
        seen[[key]] <- TRUE
        idx <- as.integer(s):as.integer(e)
        vals <- isi[idx][valid[idx]]
        if (length(vals) == 0) next
        core_n <- sum(seed_flag[idx], na.rm = TRUE)
        if (core_n < vp$min_seed_isi_n) next
        n_spikes <- e - s + 2L
        if (n_spikes < min_spikes) next
        bridge_n <- sum(valid[idx] & !seed_flag[idx] & isi[idx] <= vp$bridge_high, na.rm = TRUE)
        bridge_frac <- bridge_n / max(1L, length(idx))
        bridge_count_pass <- bridge_n <= vp$max_bridge_n
        bridge_frac_pass <- bridge_frac <= vp$max_bridge_frac
        m <- stpd_arbitration_span_metrics(dat, s, e, params, min_isi_sec = min_isi_sec, train = train, candidate_class = "dataset_isi_seed_centered_burst")
        if (is.null(m)) next
        q90 <- suppressWarnings(as.numeric(m$intra_q90_sec[1]))
        pre_gap <- suppressWarnings(as.numeric(m$pre_gap_sec[1]))
        post_gap <- suppressWarnings(as.numeric(m$post_gap_sec[1]))
        has_two_edges <- is.finite(pre_gap) && is.finite(post_gap)
        min_edge <- if (has_two_edges) min(pre_gap, post_gap) else if (is.finite(pre_gap)) pre_gap else if (is.finite(post_gap)) post_gap else NA_real_
        classicity <- if (is.finite(min_edge) && is.finite(q90) && q90 > 0) min_edge / q90 else NA_real_
        strict_required_gap <- if (is.finite(q90) && q90 > 0) max(vp$classicity_min * q90, vp$boundary_floor) else NA_real_
        possible_required_gap <- if (is.finite(q90) && q90 > 0) max(vp$possible_classicity * q90, min(vp$boundary_floor, vp$classicity_min * q90)) else NA_real_
        strict_boundary_pass <- has_two_edges && is.finite(strict_required_gap) && pre_gap >= strict_required_gap && post_gap >= strict_required_gap
        possible_two_edge_pass <- has_two_edges && is.finite(possible_required_gap) && pre_gap >= possible_required_gap && post_gap >= possible_required_gap
        one_edge_possible <- !has_two_edges && isTRUE(vp$allow_boundary_possible) && is.finite(min_edge) && is.finite(possible_required_gap) && min_edge >= possible_required_gap
        context_active <- is.finite(vp$context_min) && vp$context_min > 1
        edge_return_active <- is.finite(vp$edge_return_min) && vp$edge_return_min > 0
        context_pass <- !context_active || !is.finite(m$context_compression) || suppressWarnings(as.numeric(m$context_compression[1])) >= vp$context_min
        edge_return_pass <- !edge_return_active || !is.finite(m$edge_return_ratio) || suppressWarnings(as.numeric(m$edge_return_ratio[1])) >= vp$edge_return_min
        neg <- isTRUE(m$manual_negative_veto) && isTRUE((params$detector %||% list())$manual_negative_labels_enabled %||% TRUE)
        q90_pass <- is.finite(q90) && q90 <= vp$bridge_high
        size_label <- "oversized_burst_family"
        if (n_spikes <= classic_max_spikes) size_label <- "burst"
        else if (n_spikes >= long_min_spikes && (long_max_spikes <= 0 || n_spikes <= long_max_spikes)) size_label <- "long_burst"
        proposed <- "reject"
        status <- "dataset_isi_reject"
        action <- "reject"
        decision <- "dataset_isi_reject"
        if (!neg && bridge_count_pass && bridge_frac_pass && q90_pass && context_pass && edge_return_pass && strict_boundary_pass) {
          if (size_label %in% c("burst", "long_burst")) {
            proposed <- size_label; status <- "dataset_isi_classic_pass"; action <- "accept"
            decision <- paste0("dataset_seed_band_seed_centered_strict_boundary_pass__", size_label)
          } else {
            proposed <- "possible_burst"; status <- "dataset_isi_prolonged_burst_like_review"; action <- "demote_to_possible"
            decision <- "strict_boundary_but_exceeds_long_burst_max_spikes"
          }
        } else if (!neg && bridge_count_pass && bridge_frac_pass && q90_pass && context_pass && (possible_two_edge_pass || one_edge_possible)) {
          proposed <- "possible_burst"; status <- if (one_edge_possible) "dataset_isi_boundary_possible" else "dataset_isi_possible"
          action <- "demote_to_possible"; decision <- "dataset_seed_band_possible_or_boundary_burst_review"
        } else {
          reasons <- c(
            if (neg) "manual_negative_veto",
            if (!bridge_count_pass) "too_many_bridge_isis",
            if (!bridge_frac_pass) "bridge_fraction_too_high",
            if (!q90_pass) "intra_q90_exceeds_bridge_band_upper",
            if (!context_pass) "context_compression_fail",
            if (!edge_return_pass) "edge_return_fail",
            if (!strict_boundary_pass && !possible_two_edge_pass && !one_edge_possible) "largeISI_burst_largeISI_boundary_fail"
          )
          decision <- paste(reasons, collapse = ";")
          if (!nzchar(decision)) decision <- "dataset_isi_reject"
        }
        # Score prioritizes the most classical seed-centered event and penalizes bridge dominance.
        score <- if (is.finite(classicity)) classicity else 0
        score <- score + 0.05 * core_n - 0.10 * bridge_n - 0.20 * bridge_frac
        candidate_counter <- candidate_counter + 1L
        extra <- list(
          train = as.character(train %||% ""),
          candidate_id = paste0("dataset_isi_", candidate_counter),
          candidate_class = "dataset_isi_seed_centered_burst",
          seed_run_start_isi = seed_s,
          seed_run_end_isi = seed_e,
          seed_band_lower_sec = vp$seed_low,
          seed_band_upper_sec = vp$seed_high,
          bridge_band_upper_sec = vp$bridge_high,
          boundary_floor_sec = vp$boundary_floor,
          seed_high_percentile_in_train = profile$seed_high_percentile_in_train[1],
          seed_band_fraction = profile$seed_band_fraction[1],
          phenotype_prior = profile$phenotype_prior[1],
          core_isi_count = as.integer(core_n),
          bridge_isi_count = as.integer(bridge_n),
          bridge_fraction = bridge_frac,
          bridge_count_pass = bridge_count_pass,
          bridge_fraction_pass = bridge_frac_pass,
          burst_classicity_score = classicity,
          burst_classicity_required = vp$classicity_min,
          possible_classicity_required = vp$possible_classicity,
          strict_required_gap_sec = strict_required_gap,
          possible_required_gap_sec = possible_required_gap,
          strict_boundary_pass = strict_boundary_pass,
          possible_boundary_pass = possible_two_edge_pass || one_edge_possible,
          boundary_floor_pass = if (is.finite(vp$boundary_floor) && vp$boundary_floor > 0 && has_two_edges) pre_gap >= vp$boundary_floor && post_gap >= vp$boundary_floor else TRUE,
          q90_bridge_pass = q90_pass,
          context_gate_pass = context_pass,
          edge_return_pass = edge_return_pass,
          manual_negative_veto = neg
        )
        rows[[length(rows) + 1L]] <- stpd_seed_bridge_enrich_candidate_metrics(m, "dataset_isi_seed_centered_burst", proposed, status, decision, action, score, extra)
      }
      if (length(rows) >= max_rows) break
    }
    if (length(rows) >= max_rows) break
  }
  if (length(rows) == 0) return(empty)
  out <- dplyr::bind_rows(rows)
  prof <- out[as.character(out$candidate_layer) == "dataset_isi_train_seed_band_profile", , drop = FALSE]
  cand <- out[as.character(out$candidate_layer) != "dataset_isi_train_seed_band_profile", , drop = FALSE]
  cand <- stpd_dataset_isi_select_nonoverlapping(cand)
  dplyr::bind_rows(prof, cand)
}

stpd_seed_bridge_detect_burst_seed_bridge <- function(dat, params, min_isi_sec = 0.001, train = "") {
  if (stpd_dataset_isi_is_enabled(params)) {
    return(stpd_dataset_isi_detect_burst_seed_centered(dat, params, min_isi_sec = min_isi_sec, train = train))
  }
  stpd_seed_bridge_detect_burst_seed_bridge_classicity(dat, params, min_isi_sec = min_isi_sec, train = train)
}

# dataset ISI safe event writer: never fragments a candidate when a stronger event has
# already occupied part of its interval.  This is critical for preventing
# possible_burst / tonic / pause residues after HF-spiking or burst-family events.
stpd_arbitration_write_candidates <- function(pat, score, locked, candidates, label_filter = NULL) {
  if (is.null(candidates) || nrow(candidates) == 0) return(list(pat = pat, score = score))
  for (i in seq_len(nrow(candidates))) {
    lab <- as.character(candidates$final_label[i] %||% candidates$class[i] %||% "")
    if (length(lab) == 0 || is.na(lab)) lab <- ""
    if (lab == "" || identical(lab, "reject") || identical(lab, "profile")) next
    if (!is.null(label_filter) && !(lab %in% label_filter)) next
    s <- suppressWarnings(as.integer(candidates$start_isi[i]))
    e <- suppressWarnings(as.integer(candidates$end_isi[i]))
    if (!is.finite(s) || !is.finite(e) || e < s || s < 2 || e > length(pat)) next
    idx <- s:e
    # Full-event integrity: do not write a residual fragment if any ISI in this
    # candidate is manual-locked or already occupied by a higher-priority event.
    if (any(locked[idx] | pat[idx] != "", na.rm = TRUE)) next
    pat[idx] <- lab
    if (!is.null(score)) score[idx] <- suppressWarnings(as.numeric(candidates$score[i] %||% NA_real_))
  }
  list(pat = pat, score = score)
}

# Programmatic helper for dataset-level ISI-band calibration.
# This is intentionally independent of AUTO labeling. It lets the user inspect
# pooled and train-balanced ISI histograms before choosing the dataset ISI seed/bridge/boundary bands.
stpd_dataset_isi_histogram <- function(trains, params = default_params_sec(), min_isi_sec = 0.001,
                                           bin_width_sec = NULL, x_max_sec = NULL,
                                           mode = c("raw_pooled", "train_balanced")) {
  mode <- match.arg(mode)
  if (is.null(bin_width_sec) || !is.finite(bin_width_sec) || bin_width_sec <= 0) {
    bin_width_sec <- stpd_seed_bridge_num((params$event_core %||% list())$histogram_bin_width_sec %||% 0.005, 0.005)
  }
  vals_by_train <- list()
  for (nm in names(trains)) {
    dat <- trains[[nm]]
    if (is.null(dat) || !("ISI_sec" %in% names(dat))) next
    isi <- suppressWarnings(as.numeric(dat$ISI_sec))
    art <- is_artifact_isi(isi, min_isi_sec)
    valid <- is.finite(isi) & !art
    if (length(valid) > 0) valid[1] <- FALSE
    vals <- isi[valid]
    vals <- vals[is.finite(vals) & vals >= min_isi_sec]
    if (length(vals) > 0) vals_by_train[[nm]] <- vals
  }
  if (length(vals_by_train) == 0) {
    return(data.frame(bin_left_sec = numeric(), bin_right_sec = numeric(), count = numeric(), density = numeric(), mode = character()))
  }
  all_vals <- unlist(vals_by_train, use.names = FALSE)
  if (is.null(x_max_sec) || !is.finite(x_max_sec) || x_max_sec <= 0) x_max_sec <- max(all_vals, na.rm = TRUE)
  x_max_sec <- max(x_max_sec, bin_width_sec)
  br <- seq(0, ceiling(x_max_sec / bin_width_sec) * bin_width_sec, by = bin_width_sec)
  if (length(br) < 2) br <- c(0, bin_width_sec)
  if (identical(mode, "raw_pooled")) {
    h <- hist(all_vals, breaks = br, plot = FALSE, include.lowest = TRUE, right = FALSE)
    cnt <- as.numeric(h$counts)
  } else {
    mat <- lapply(vals_by_train, function(v) hist(v, breaks = br, plot = FALSE, include.lowest = TRUE, right = FALSE)$counts)
    mat <- do.call(rbind, mat)
    rs <- rowSums(mat)
    mat_norm <- mat
    nz <- rs > 0
    mat_norm[nz, ] <- mat[nz, , drop = FALSE] / rs[nz]
    cnt <- colMeans(mat_norm, na.rm = TRUE)
  }
  total <- sum(cnt, na.rm = TRUE)
  data.frame(
    bin_left_sec = br[-length(br)],
    bin_right_sec = br[-1],
    count = cnt,
    density = if (is.finite(total) && total > 0) cnt / total else cnt,
    mode = mode,
    stringsAsFactors = FALSE
  )
}
