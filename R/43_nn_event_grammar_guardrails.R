# ============================================================
# event grammar NN event-grammar guardrails
# ------------------------------------------------------------
# Rationale:
#   The neural-network module is an ISI-level classifier.  Labels such as
#   high_frequency_spiking, high_frequency_tonic and burst are event/state
#   labels, not single-ISI labels.  Applying raw NN predictions directly to
#   AUTO can therefore create biologically impossible fragments, e.g. a
#   two-spike interval labelled as high_frequency_spiking.  This patch keeps
#   the neural network as an auxiliary classifier but enforces event-grammar
#   validation before writing NN predictions to AUTO.
# ============================================================

stpd_nn_bool_runs <- function(flag) {
  flag <- as.logical(flag)
  flag[is.na(flag)] <- FALSE
  if (length(flag) == 0 || !any(flag)) return(data.frame(start_isi = integer(), end_isi = integer()))
  d <- diff(c(FALSE, flag, FALSE))
  data.frame(start_isi = which(d == 1L), end_isi = which(d == -1L) - 1L, stringsAsFactors = FALSE)
}

stpd_nn_max_consecutive_true <- function(flag) {
  flag <- as.logical(flag)
  flag[is.na(flag)] <- FALSE
  if (length(flag) == 0 || !any(flag)) return(0L)
  max(rle(flag)$lengths[rle(flag)$values], na.rm = TRUE)
}

stpd_nn_quantile <- function(x, p) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, type = 7))
}

# Build NN-assisted long HF-spiking state candidates from prediction support.
# This does not use raw single-ISI labels as final output.  It first finds
# repeated HF-spiking support, merges across a small number of tolerated gaps,
# and then validates the whole epoch using the HF-state grammar.
stpd_nn_hf_spiking_events_from_predictions <- function(dat, pred_train, params, min_isi_sec = 0.001, train = "") {
  if (is.null(pred_train) || nrow(pred_train) == 0 || is.null(dat) || nrow(dat) <= 2) return(data.frame())
  n <- nrow(dat)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE

  hp <- params$highfreq %||% list()
  min_spikes <- max(3L, as.integer(hp$spiking_min_spikes %||% 30L))
  min_duration <- suppressWarnings(as.numeric(hp$spiking_min_duration %||% 0))
  if (!is.finite(min_duration)) min_duration <- 0
  short_upper <- suppressWarnings(as.numeric(hp$spiking_max_ISI_abs %||% hp$spiking_q90_max_ISI_sec %||% 0.020))
  q90_max <- suppressWarnings(as.numeric(hp$spiking_q90_max_ISI_sec %||% 0.025))
  q80_max <- suppressWarnings(as.numeric(hp$spiking_q80_max_ISI_sec %||% q90_max))
  epoch_bridge <- suppressWarnings(as.numeric(hp$spiking_epoch_bridge_ISI_sec %||% 0.035))
  tolerated_gap <- suppressWarnings(as.numeric(hp$spiking_tolerated_gap_ISI_sec %||% 0.075))
  allowed_large <- suppressWarnings(as.numeric(hp$spiking_allowed_large_isi_fraction %||% 0.25))
  max_consec_large <- max(1L, as.integer(hp$spiking_max_consecutive_large_isi %||% 3L))
  short_frac_min <- suppressWarnings(as.numeric(hp$spiking_short_fraction_min %||% 0.70))
  if (!is.finite(short_upper) || short_upper <= 0) short_upper <- 0.020
  if (!is.finite(q90_max) || q90_max <= 0) q90_max <- max(short_upper, 0.025)
  if (!is.finite(q80_max) || q80_max <= 0) q80_max <- q90_max
  if (!is.finite(epoch_bridge) || epoch_bridge <= 0) epoch_bridge <- max(0.035, q90_max)
  if (!is.finite(tolerated_gap) || tolerated_gap <= 0) tolerated_gap <- max(0.060, min(0.120, 2.5 * q90_max))
  allowed_large <- min(max(ifelse(is.finite(allowed_large), allowed_large, 0.25), 0), 1)
  allowed_large_eff <- max(allowed_large, 0.30)
  short_frac_min <- min(max(ifelse(is.finite(short_frac_min), short_frac_min, 0.70), 0.1), 1)

  # Prediction support. Use accepted HF-spiking predictions, plus a probability
  # column when present.  This makes long states robust to intermittent labels.
  prob_col <- "prob_high_frequency_spiking"
  hfs_prob <- if (prob_col %in% names(pred_train)) suppressWarnings(as.numeric(pred_train[[prob_col]])) else rep(NA_real_, nrow(pred_train))
  pred_lab <- normalize_pattern_label(pred_train$pred_label, fill_blank_others = FALSE)
  pred_conf <- suppressWarnings(as.numeric(pred_train$pred_confidence))
  pred_acc <- as.logical(pred_train$accepted); pred_acc[is.na(pred_acc)] <- FALSE
  hfs_support_rows <- (pred_lab == "high_frequency_spiking" & pred_acc) |
    (is.finite(hfs_prob) & hfs_prob >= max(0.50, min(0.85, (params$ml_hf_spiking_prob_support %||% 0.55))))

  support <- rep(FALSE, n)
  if (any(hfs_support_rows)) {
    idx <- suppressWarnings(as.integer(pred_train$isi_idx[hfs_support_rows]))
    idx <- idx[is.finite(idx) & idx >= 2L & idx <= n]
    support[idx] <- TRUE
  }
  # NN support cannot override biology: keep only valid ISIs that are at least
  # plausibly high-frequency or within a tolerated gap range.
  support <- support & valid & isi <= tolerated_gap
  if (!any(support)) return(data.frame())

  runs <- stpd_nn_bool_runs(support)
  if (nrow(runs) == 0) return(data.frame())
  if (exists("stpd_event_grammar_merge_hf_support_runs", mode = "function")) {
    runs <- stpd_event_grammar_merge_hf_support_runs(runs, isi, valid, tolerated_gap = tolerated_gap, max_gap_count = max_consec_large)
  }
  if (nrow(runs) == 0) return(data.frame())

  rows <- list()
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    if (!is.finite(s) || !is.finite(e) || e < s || s < 2L || e > n) next
    idx <- s:e
    vals <- isi[idx][valid[idx]]
    if (length(vals) == 0) next
    n_spikes <- e - s + 2L
    if (n_spikes < min_spikes) next
    start_t <- suppressWarnings(as.numeric(dat$timestamp_sec[s - 1L]))
    end_t <- suppressWarnings(as.numeric(dat$timestamp_sec[e]))
    duration <- end_t - start_t
    if (min_duration > 0 && (!is.finite(duration) || duration < min_duration)) next
    q50 <- stpd_nn_quantile(vals, 0.50)
    q80 <- stpd_nn_quantile(vals, 0.80)
    q90 <- stpd_nn_quantile(vals, 0.90)
    short_frac <- mean(vals <= short_upper, na.rm = TRUE)
    q90_short_frac <- mean(vals <= q90_max, na.rm = TRUE)
    bridge_frac <- mean(vals <= epoch_bridge, na.rm = TRUE)
    large_flag <- vals > epoch_bridge
    large_frac <- mean(large_flag, na.rm = TRUE)
    max_large <- stpd_nn_max_consecutive_true(large_flag)
    tolerated_frac <- mean(vals <= tolerated_gap, na.rm = TRUE)
    strict_q90 <- is.finite(q90) && q90 <= q90_max
    robust_q80 <- is.finite(q80) && q80 <= q80_max
    majority_pass <- (short_frac >= max(0.50, short_frac_min - 0.15)) ||
      (q90_short_frac >= max(0.60, short_frac_min - 0.10)) ||
      (bridge_frac >= max(0.75, short_frac_min))
    state_pass <- strict_q90 || (robust_q80 && majority_pass)
    gap_pass <- tolerated_frac >= 0.95
    large_pass <- large_frac <= allowed_large_eff && max_large <= max(2L, max_consec_large)
    if (!(state_pass && gap_pass && large_pass)) next

    mean_conf <- NA_real_
    ov <- pred_train$isi_idx >= s & pred_train$isi_idx <= e & hfs_support_rows
    if (any(ov, na.rm = TRUE)) mean_conf <- mean(pred_conf[ov], na.rm = TRUE)
    rows[[length(rows) + 1L]] <- data.frame(
      train = train,
      start_isi = s,
      end_isi = e,
      label = "high_frequency_spiking",
      mean_confidence = mean_conf,
      n_isi = length(idx),
      n_spikes = n_spikes,
      duration_sec = duration,
      q50_sec = q50,
      q80_sec = q80,
      q90_sec = q90,
      short_fraction = short_frac,
      large_fraction = large_frac,
      validation_path = ifelse(strict_q90, "nn_hfs_strict_q90", "nn_hfs_robust_q80_majority"),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

# Override: event validator with explicit HF-spiking and HF-tonic branches.
validate_nn_event_segment <- function(dat, idx, label, params, min_isi_sec = 0.001, train = "") {
  idx <- sort(unique(as.integer(idx)))
  idx <- idx[is.finite(idx) & idx >= 2 & idx <= nrow(dat)]
  if (length(idx) == 0) return(FALSE)
  isi <- dat$ISI_sec
  vals <- valid_isi_values(isi[idx], min_isi_sec)
  if (length(vals) == 0) return(FALSE)
  s <- min(idx); e <- max(idx)
  label <- normalize_pattern_label(label, fill_blank_others = FALSE)[1]
  if (label == "others") return(TRUE)

  if (label %in% c("burst", "long_burst", "possible_burst", "burst_family")) {
    n_spk <- e - s + 2L
    if (n_spk < (params$burst$G_min %||% 3L)) return(FALSE)
    edge <- calc_edge_contrast_stats(isi, s, e, min_isi_sec = min_isi_sec, robust_q = params$burst$contrast_q %||% 0.90)
    edge_min <- edge$contrast_min_q
    edge_geom <- edge$contrast_geom_q
    min_thr <- max(1.05, (params$burst$contrast_min_possible %||% 1.20) * 0.90)
    geom_thr <- max(1.10, (params$burst$contrast_geom_possible %||% 1.30) * 0.90)
    return(edge$n_flank >= 1L && is.finite(edge_min) && is.finite(edge_geom) && edge_min >= min_thr && edge_geom >= geom_thr)
  }

  if (label == "high_frequency_spiking") {
    hp <- params$highfreq %||% list()
    n_spk <- e - s + 2L
    min_spk <- max(3L, as.integer(hp$spiking_min_spikes %||% 30L))
    if (n_spk < min_spk) return(FALSE)
    start_t <- suppressWarnings(as.numeric(dat$timestamp_sec[s - 1L]))
    end_t <- suppressWarnings(as.numeric(dat$timestamp_sec[e]))
    dur <- end_t - start_t
    min_dur <- suppressWarnings(as.numeric(hp$spiking_min_duration %||% 0))
    if (is.finite(min_dur) && min_dur > 0 && (!is.finite(dur) || dur < min_dur)) return(FALSE)
    short_upper <- suppressWarnings(as.numeric(hp$spiking_max_ISI_abs %||% hp$spiking_q90_max_ISI_sec %||% 0.020))
    q90_max <- suppressWarnings(as.numeric(hp$spiking_q90_max_ISI_sec %||% 0.025))
    q80_max <- suppressWarnings(as.numeric(hp$spiking_q80_max_ISI_sec %||% q90_max))
    epoch_bridge <- suppressWarnings(as.numeric(hp$spiking_epoch_bridge_ISI_sec %||% 0.035))
    allowed_large <- min(max(suppressWarnings(as.numeric(hp$spiking_allowed_large_isi_fraction %||% 0.25)), 0), 1)
    short_frac_min <- min(max(suppressWarnings(as.numeric(hp$spiking_short_fraction_min %||% 0.70)), 0.1), 1)
    if (!is.finite(short_upper) || short_upper <= 0) short_upper <- 0.020
    if (!is.finite(q90_max) || q90_max <= 0) q90_max <- max(short_upper, 0.025)
    if (!is.finite(q80_max) || q80_max <= 0) q80_max <- q90_max
    if (!is.finite(epoch_bridge) || epoch_bridge <= 0) epoch_bridge <- max(0.035, q90_max)
    hfs_lim <- stpd_pattern_isi_limits_for_label("high_frequency_spiking", params)
    hfs_max_sec <- suppressWarnings(as.numeric(hfs_lim$max_sec %||% NA_real_))[1]
    if (is.finite(hfs_max_sec) && hfs_max_sec > 0 && any(vals > hfs_max_sec, na.rm = TRUE)) return(FALSE)
    q80 <- stpd_nn_quantile(vals, 0.80)
    q90 <- stpd_nn_quantile(vals, 0.90)
    short_frac <- mean(vals <= short_upper, na.rm = TRUE)
    q90_short_frac <- mean(vals <= q90_max, na.rm = TRUE)
    bridge_frac <- mean(vals <= epoch_bridge, na.rm = TRUE)
    large_flag <- vals > epoch_bridge
    large_frac <- mean(large_flag, na.rm = TRUE)
    max_large <- stpd_nn_max_consecutive_true(large_flag)
    strict_q90 <- is.finite(q90) && q90 <= q90_max
    robust_q80 <- is.finite(q80) && q80 <= q80_max
    majority_pass <- (short_frac >= max(0.50, short_frac_min - 0.15)) ||
      (q90_short_frac >= max(0.60, short_frac_min - 0.10)) ||
      (bridge_frac >= max(0.75, short_frac_min))
    large_pass <- large_frac <= max(allowed_large, 0.30) && max_large <= max(2L, as.integer(hp$spiking_max_consecutive_large_isi %||% 3L))
    return((strict_q90 || (robust_q80 && majority_pass)) && large_pass)
  }

  if (label == "high_frequency_tonic") {
    hp <- params$highfreq %||% list()
    n_spk <- e - s + 2L
    min_spk <- max(3L, as.integer(hp$G_min %||% 6L))
    if (n_spk < min_spk) return(FALSE)
    floor_sec <- suppressWarnings(as.numeric(hp$tonic_min_ISI_floor_sec %||% 0.010))
    low_tail_max <- min(max(suppressWarnings(as.numeric(hp$tonic_low_tail_fraction_max %||% 0.05)), 0), 1)
    q10 <- stpd_nn_quantile(vals, 0.10)
    q90 <- stpd_nn_quantile(vals, 0.90)
    low_tail <- mean(vals < floor_sec, na.rm = TRUE)
    cv <- calc_CV(vals)
    lv <- calc_LV(vals)
    mm <- max(vals, na.rm = TRUE) / mean(vals, na.rm = TRUE)
    max_isi <- suppressWarnings(as.numeric(hp$T_high_max %||% 0.020))
    if (!is.finite(max_isi) || max_isi <= 0) max_isi <- 0.030
    return(is.finite(q10) && q10 >= floor_sec * 0.85 && low_tail <= max(low_tail_max, 0.10) &&
             is.finite(q90) && q90 <= max_isi * 1.25 &&
             is.finite(cv) && cv <= max(0.45, (hp$stable_CV_max %||% 0.30) * 1.5) &&
             is.finite(lv) && lv <= max(0.50, (hp$stable_LV_max %||% 0.35) * 1.5) &&
             is.finite(mm) && mm <= max(1.35, (hp$stable_MM_max %||% 1.25) * 1.2))
  }

  if (label == "tonic") {
    n_spk <- e - s + 2L
    if (n_spk < (params$tonic$G_min %||% 5L)) return(FALSE)
    m <- mean(vals)
    lv <- calc_LV(vals)
    cv <- calc_CV(vals)
    mm <- max(vals) / mean(vals)
    rr <- get_train_tonic_range(params$tonic, train = train)
    range_eval <- stpd_range_anchor_support(m, rr = NULL)
    if (!is.null(rr) && isTRUE(params$tonic$adaptive_use_train_ranges %||% TRUE)) {
      range_eval <- stpd_range_anchor_support(
        m,
        value_pct = isi_percentile_scalar(m, isi, min_isi_sec),
        rr = rr,
        mode = params$tonic$adaptive_range_mode %||% "percentile_or_absolute",
        enforce_lower_sec = TRUE,
        hard_requested = isTRUE(params$tonic$adaptive_train_ranges_hard %||% FALSE)
      )
    }
    abs_ok <- is.finite(m) && m >= (params$tonic$T_min %||% 0) && m <= (params$tonic$T_max %||% Inf)
    mean_ok <- if (isTRUE(range_eval$policy$hard_allowed)) {
      abs_ok && isTRUE(range_eval$range_match)
    } else {
      abs_ok || isTRUE(range_eval$soft_support)
    }
    return(mean_ok && is.finite(lv) && lv <= (params$tonic$LV_core %||% 0.5) * 1.25 &&
             is.finite(cv) && cv <= max(0.10, (params$burst$final_tonic_like_cv_max %||% 0.30) * 1.50) &&
             is.finite(mm) && mm <= (params$tonic$tonic_mm_max %||% 1.25) * 1.15)
  }

  if (label == "pause") {
    ok <- FALSE
    for (j in idx) {
      loc <- get_local_median(isi, j, min_isi_sec = min_isi_sec)
      ratio <- if (is.finite(loc) && loc > 0) isi[j] / loc else NA_real_
      rr <- get_train_pause_range(params$pause, train = train)
      range_eval <- stpd_range_anchor_support(isi[j], rr = NULL)
      if (!is.null(rr) && isTRUE(params$pause$adaptive_use_train_ranges %||% TRUE)) {
        range_eval <- stpd_range_anchor_support(
          isi[j],
          value_pct = isi_percentile_scalar(isi[j], isi, min_isi_sec),
          rr = rr,
          mode = params$pause$adaptive_range_mode %||% "percentile_or_absolute",
          enforce_lower_sec = TRUE,
          default_low_pct = 75,
          default_high_pct = 100,
          hard_requested = isTRUE(params$pause$adaptive_train_ranges_hard %||% FALSE)
        )
      }
      abs_pause <- is.finite(isi[j]) && isi[j] >= (params$pause$T_seed %||% 0.100) * 0.90
      rel_pause <- is.finite(ratio) && ratio >= (params$pause$alpha %||% 2.2) * 0.80
      anchor_pause <- isTRUE(range_eval$policy$is_manual_anchor) && isTRUE(range_eval$anchor$soft_support) &&
        (isTRUE(abs_pause) || isTRUE(rel_pause))
      explicit_range <- isTRUE(range_eval$soft_support) && !isTRUE(range_eval$policy$is_manual_anchor)
      ok <- ok || (is.finite(ratio) && ratio >= (params$pause$alpha %||% 2.2) * 0.80) ||
        abs_pause || explicit_range || anchor_pause
    }
    return(ok)
  }
  FALSE
}

# Override: event postprocessor that reconstructs long NN HF-spiking states
# before applying regular same-label segment validation.
postprocess_nn_predictions_for_train <- function(dat, pred_train, params, min_isi_sec = 0.001,
                                                    train = "", apply_others = FALSE) {
  n <- nrow(dat)
  if (n <= 1 || is.null(pred_train) || nrow(pred_train) == 0) return(data.frame())
  lab <- rep("", n)
  conf <- rep(NA_real_, n)
  accepted <- pred_train[pred_train$accepted, , drop = FALSE]
  rows <- list()
  covered <- rep(FALSE, n)

  # 1) Long HF-spiking state reconstruction.  This prevents an ISI-level NN from
  # producing short, biologically invalid HF-spiking fragments, while still
  # allowing true long HF states to be written as a single event.
  hfs <- stpd_nn_hf_spiking_events_from_predictions(dat, pred_train, params, min_isi_sec = min_isi_sec, train = train)
  if (!is.null(hfs) && nrow(hfs) > 0) {
    for (ii in seq_len(nrow(hfs))) {
      idx <- as.integer(hfs$start_isi[ii]):as.integer(hfs$end_isi[ii])
      idx <- idx[idx >= 2L & idx <= n]
      if (length(idx) > 0) covered[idx] <- TRUE
    }
    rows[[length(rows) + 1L]] <- hfs[, c("train", "start_isi", "end_isi", "label", "mean_confidence", "n_isi"), drop = FALSE]
  }

  # 2) Accepted point predictions for other labels.  HF-spiking points outside a
  # validated long state are intentionally ignored.
  if (nrow(accepted) > 0) {
    for (ii in seq_len(nrow(accepted))) {
      idx <- safe_int(accepted$isi_idx[ii], NA_integer_)
      if (!is.finite(idx) || idx < 2 || idx > n) next
      if (covered[idx]) next
      if (!is.na(dat$pattern_manual[idx]) && dat$pattern_manual[idx] != "") next
      pl <- normalize_pattern_label(accepted$pred_label[ii], fill_blank_others = FALSE)[1]
      if (pl == "burst_family") pl <- "possible_burst"
      if (pl == "high_frequency_spiking") next
      if (pl == "others" && !isTRUE(apply_others)) next
      lab[idx] <- pl
      conf[idx] <- accepted$pred_confidence[ii]
    }
  }

  # Salt-and-pepper suppression. Single pause ISIs are valid; isolated state/event
  # calls are removed unless they are part of a same-label neighborhood.
  for (i in 2:n) {
    if (lab[i] == "" || lab[i] == "pause" || lab[i] == "others") next
    same_left <- i > 2 && lab[i - 1L] == lab[i]
    same_right <- i < n && lab[i + 1L] == lab[i]
    if (!same_left && !same_right) lab[i] <- ""
  }

  segs <- label_segments(lab, labels = c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others"))
  if (nrow(segs) > 0) {
    for (ii in seq_len(nrow(segs))) {
      idx <- segs$start_isi[ii]:segs$end_isi[ii]
      lbl <- segs$class[ii]
      if (!validate_nn_event_segment(dat, idx, lbl, params, min_isi_sec = min_isi_sec, train = train)) next
      rows[[length(rows) + 1L]] <- data.frame(
        train = train,
        start_isi = segs$start_isi[ii],
        end_isi = segs$end_isi[ii],
        label = lbl,
        mean_confidence = mean(conf[idx], na.rm = TRUE),
        n_isi = length(idx),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  out <- dplyr::bind_rows(rows)
  out <- out[order(out$start_isi, out$end_isi), , drop = FALSE]
  out
}

# Guardrail for raw NN application mode.  It removes biologically invalid tiny
# fragments after raw per-ISI NN labels are written to AUTO.  In normal use the
# event-grammar postprocessor above is preferred.
stpd_nn_guardrail_auto_predictions_for_train <- function(dat, params, min_isi_sec = 0.001, train = "") {
  if (is.null(dat) || nrow(dat) == 0 || !("pattern_auto" %in% names(dat))) return(dat)
  pat <- as.character(dat$pattern_auto); pat[is.na(pat)] <- ""
  labels <- c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
  segs <- label_segments(pat, labels = labels)
  if (nrow(segs) == 0) return(dat)
  for (ii in seq_len(nrow(segs))) {
    idx <- segs$start_isi[ii]:segs$end_isi[ii]
    lbl <- segs$class[ii]
    if (!validate_nn_event_segment(dat, idx, lbl, params, min_isi_sec = min_isi_sec, train = train)) {
      pat[idx] <- ""
      if ("auto_score" %in% names(dat)) dat$auto_score[idx] <- NA_real_
    }
  }
  dat$pattern_auto <- pat
  dat
}
