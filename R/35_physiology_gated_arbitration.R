# ============================================================
# event arbitration physiology-gated final arbitration
# ============================================================
# This layer keeps the existing Structure-Seed-Bridge burst detector as a
# candidate generator, then applies physiology-driven gates before AUTO labels
# are written.  It is intentionally conservative for human PD STN/GPe/GPi data:
# canonical burst must be a compact short-ISI packet with immediate edge
# isolation and local-context compression.  Sustained high-frequency states are
# generated as independent candidates and are no longer fragmented by weak burst
# candidates.

stpd_arbitration_empty_audit <- function() {
  data.frame(
    train = character(), candidate_layer = character(), candidate_class = character(), final_label = character(),
    start_isi = integer(), end_isi = integer(), n_spikes = integer(), duration_sec = numeric(),
    intra_q50_sec = numeric(), intra_q90_sec = numeric(), intra_q95_sec = numeric(), max_intra_ISI_sec = numeric(),
    pre_gap_sec = numeric(), post_gap_sec = numeric(), edge_ratio = numeric(), context_compression = numeric(),
    edge_return_ratio = numeric(), internal_q95_q50_ratio = numeric(), internal_max_q50_ratio = numeric(),
    CV = numeric(), LV = numeric(), MM = numeric(), abs_ceiling_sec = numeric(), abs_ceiling_pass = logical(),
    ceiling_fuzzy_level = character(), max_bridge_pass = logical(), edge_gate_pass = logical(),
    context_gate_pass = logical(), edge_return_pass = logical(), internal_coherence_pass = logical(),
    manual_negative_veto = logical(), manual_negative_overlap_fraction = numeric(), manual_negative_core_overlap = logical(),
    gate_status = character(), decision_path = character(), action = character(), score = numeric(),
    phenotype_prior = character(), stringsAsFactors = FALSE
  )
}

stpd_arbitration_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  if (length(y) == 0 || !is.finite(y[1])) return(default)
  y[1]
}

stpd_arbitration_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
  isTRUE(x[1])
}

stpd_arbitration_vec_label <- function(x, n) {
  y <- as.character(x %||% rep("", n))
  y[is.na(y)] <- ""
  if (length(y) != n) y <- rep("", n)
  y
}

stpd_arbitration_train_phenotype <- function(dat, min_isi_sec = 0.001) {
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  valid <- is.finite(isi) & !is_artifact_isi(isi, min_isi_sec)
  valid[1] <- FALSE
  x <- isi[valid]
  if (length(x) < 10) {
    return(list(prior = "low_spike_count_unreliable", median_ISI = NA_real_, fast_fraction = NA_real_, pause_fraction = NA_real_, cv2_median = NA_real_, lv_median = NA_real_))
  }
  med <- stats::median(x, na.rm = TRUE)
  q10 <- as.numeric(stats::quantile(x, 0.10, na.rm = TRUE, names = FALSE))
  q90 <- as.numeric(stats::quantile(x, 0.90, na.rm = TRUE, names = FALSE))
  fast_fraction <- mean(x <= 0.020, na.rm = TRUE)
  pause_fraction <- mean(x >= max(0.100, 2.5 * med), na.rm = TRUE)
  lv <- calc_LV(x)
  cv <- calc_CV(x)
  prior <- "mixed"
  if (length(x) < 30) prior <- "low_spike_count_unreliable"
  else if (is.finite(pause_fraction) && pause_fraction >= 0.12) prior <- "pause_dominant"
  else if (is.finite(fast_fraction) && fast_fraction >= 0.60 && is.finite(lv) && lv <= 0.35) prior <- "hf_tonic_dominant"
  else if (is.finite(lv) && lv <= 0.35 && is.finite(q90 / max(q10, .Machine$double.eps)) && (q90 / max(q10, .Machine$double.eps)) <= 2.5) prior <- "tonic_dominant"
  else if (is.finite(fast_fraction) && fast_fraction >= 0.15) prior <- "burst_capable"
  list(prior = prior, median_ISI = med, fast_fraction = fast_fraction, pause_fraction = pause_fraction, cv2_median = cv, lv_median = lv)
}

stpd_arbitration_effective_burst_ceiling <- function(params, train = "") {
  bp <- params$burst %||% list()
  vals <- numeric(0)
  global <- stpd_arbitration_num(bp$canonical_burst_abs_ceiling_sec %||% 0, 0)
  if (is.finite(global) && global > 0) vals <- c(vals, global)
  rr <- NULL
  if (!is.null(bp$adaptive_train_ranges) && nzchar(as.character(train %||% ""))) rr <- bp$adaptive_train_ranges[[as.character(train)]]
  if (!is.null(rr)) {
    hi <- stpd_arbitration_num(rr$high_sec %||% 0, 0)
    if (is.finite(hi) && hi > 0) vals <- c(vals, hi)
  }
  tman <- stpd_arbitration_num(bp$T_manual %||% NA_real_, NA_real_)
  if (is.finite(tman) && tman > 0 && isTRUE(bp$canonical_burst_use_T_manual %||% TRUE)) vals <- c(vals, tman)
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(vals) == 0) return(NA_real_)
  min(vals, na.rm = TRUE)
}

stpd_manual_negative_labels_overlap <- function(dat, s_isi, e_isi) {
  n <- nrow(dat)
  if (!is.finite(s_isi) || !is.finite(e_isi) || e_isi < s_isi || s_isi < 2 || e_isi > n) {
    return(list(frac = 0, core = FALSE, veto = FALSE))
  }
  idx <- s_isi:e_isi
  neg <- rep(FALSE, n)
  if ("pattern_manual_negative" %in% names(dat)) {
    v <- tolower(trimws(as.character(dat$pattern_manual_negative)))
    v[is.na(v)] <- ""
    neg <- neg | v %in% c("not_burst", "hard_negative_burst", "not burst", "not-burst")
  }
  # Backward-compatible escape hatch if an older file accidentally stored a negative label in pattern_manual.
  if ("pattern_manual" %in% names(dat)) {
    v <- tolower(trimws(as.character(dat$pattern_manual)))
    v[is.na(v)] <- ""
    neg <- neg | v %in% c("not_burst", "hard_negative_burst", "not burst", "not-burst")
  }
  frac <- if (length(idx) > 0) mean(neg[idx], na.rm = TRUE) else 0
  list(frac = frac, core = any(neg[idx], na.rm = TRUE), veto = any(neg[idx], na.rm = TRUE))
}

stpd_arbitration_span_metrics <- function(dat, s_isi, e_isi, params, min_isi_sec = 0.001, train = "", candidate_class = "") {
  n <- nrow(dat)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  if (!is.finite(s_isi) || !is.finite(e_isi) || s_isi < 2 || e_isi > n || e_isi < s_isi) return(NULL)
  vals <- valid_isi_values(isi[s_isi:e_isi], min_isi_sec)
  if (length(vals) == 0) return(NULL)
  q50 <- stats::median(vals, na.rm = TRUE)
  q90 <- as.numeric(stats::quantile(vals, 0.90, na.rm = TRUE, names = FALSE))
  q95 <- as.numeric(stats::quantile(vals, 0.95, na.rm = TRUE, names = FALSE))
  mx <- max(vals, na.rm = TRUE)
  pre <- if (s_isi > 2) isi[s_isi - 1L] else NA_real_
  post <- if (e_isi < n) isi[e_isi + 1L] else NA_real_
  pre <- if (is.finite(pre) && pre >= min_isi_sec) pre else NA_real_
  post <- if (is.finite(post) && post >= min_isi_sec) post else NA_real_
  bc <- calc_event_contrast_stats(
    isi, s_isi, e_isi,
    min_isi_sec = min_isi_sec,
    robust_q = params$burst$contrast_q %||% 0.90,
    context_k = params$burst$context_k %||% 5L
  )
  context_vals <- c(bc$context_pre_ISI_sec, bc$context_post_ISI_sec)
  context_med <- safe_median(context_vals[is.finite(context_vals)], default = NA_real_)
  edge_min <- safe_median(c(pre, post)[is.finite(c(pre, post))], default = NA_real_)
  if (is.finite(pre) && is.finite(post)) edge_min <- min(pre, post)
  edge_ratio <- if (is.finite(edge_min) && is.finite(q90) && q90 > 0) edge_min / q90 else NA_real_
  context_compression <- if (is.finite(context_med) && is.finite(q90) && q90 > 0) context_med / q90 else NA_real_
  edge_return <- if (is.finite(edge_min) && is.finite(context_med) && context_med > 0) edge_min / context_med else NA_real_
  neg <- stpd_manual_negative_labels_overlap(dat, s_isi, e_isi)
  data.frame(
    start_isi = as.integer(s_isi), end_isi = as.integer(e_isi), candidate_class = as.character(candidate_class),
    n_spikes = as.integer(e_isi - s_isi + 2L), n_isi = as.integer(e_isi - s_isi + 1L),
    duration_sec = suppressWarnings(as.numeric(dat$timestamp_sec[e_isi] - dat$timestamp_sec[s_isi - 1L])),
    intra_q50_sec = q50, intra_q90_sec = q90, intra_q95_sec = q95, max_intra_ISI_sec = mx,
    pre_gap_sec = pre, post_gap_sec = post, edge_ratio = edge_ratio,
    context_compression = context_compression, edge_return_ratio = edge_return,
    n_immediate_flanks = as.integer(bc$n_flank %||% 0L), n_context_flanks = as.integer(bc$n_flank_ctx %||% 0L),
    internal_q95_q50_ratio = if (is.finite(q50) && q50 > 0) q95 / q50 else NA_real_,
    internal_max_q50_ratio = if (is.finite(q50) && q50 > 0) mx / q50 else NA_real_,
    CV = calc_CV(vals), LV = calc_LV(vals), MM = if (length(vals) > 0 && is.finite(mean(vals)) && mean(vals) > 0) mx / mean(vals) else NA_real_,
    manual_negative_overlap_fraction = neg$frac,
    manual_negative_core_overlap = isTRUE(neg$core),
    manual_negative_veto = isTRUE(neg$veto),
    stringsAsFactors = FALSE
  )
}

stpd_arbitration_canonical_burst_gate <- function(dat, candidate, params, min_isi_sec = 0.001, train = "") {
  s <- suppressWarnings(as.integer(candidate$start_isi[1])); e <- suppressWarnings(as.integer(candidate$end_isi[1]))
  cls0 <- as.character(candidate$class[1] %||% "burst")
  if (length(cls0) == 0 || is.na(cls0) || cls0 == "") cls0 <- "burst"
  score0 <- suppressWarnings(as.numeric(candidate$score[1] %||% NA_real_))
  ph <- stpd_arbitration_train_phenotype(dat, min_isi_sec)
  m <- stpd_arbitration_span_metrics(dat, s, e, params, min_isi_sec = min_isi_sec, train = train, candidate_class = cls0)
  if (is.null(m)) return(NULL)
  if (!isTRUE(params$detector$manual_negative_labels_enabled %||% TRUE)) {
    m$manual_negative_veto <- FALSE
    m$manual_negative_core_overlap <- FALSE
    m$manual_negative_overlap_fraction <- 0
  }

  bp <- params$burst %||% list()
  base_edge <- stpd_arbitration_num(bp$canonical_burst_edge_multiplier %||% 3.0, 3.0)
  base_context <- stpd_arbitration_num(bp$canonical_burst_context_contrast_min %||% 2.5, 2.5)
  edge_return_min <- stpd_arbitration_num(bp$canonical_burst_edge_return_min %||% 0.60, 0.60)
  # Conservative train-level priors.  They alter gate strictness; they do not directly assign labels.
  if (ph$prior %in% c("tonic_dominant", "hf_tonic_dominant")) {
    base_edge <- max(base_edge, stpd_arbitration_num(bp$canonical_burst_edge_multiplier_tonic_prior %||% 3.5, 3.5))
    base_context <- max(base_context, stpd_arbitration_num(bp$canonical_burst_context_min_tonic_prior %||% 3.0, 3.0))
  }
  if (ph$prior %in% c("pause_dominant")) {
    base_context <- max(base_context, stpd_arbitration_num(bp$canonical_burst_context_min_pause_prior %||% 3.0, 3.0))
  }

  ceiling <- stpd_arbitration_effective_burst_ceiling(params, train = train)
  fuzzy_pct <- max(0, stpd_arbitration_num(bp$canonical_burst_abs_ceiling_fuzzy_pct %||% 0, 0))
  fuzzy_factor <- 1 + fuzzy_pct / 100
  abs_pass <- TRUE
  fuzzy_level <- "disabled"
  if (is.finite(ceiling) && ceiling > 0) {
    if (is.finite(m$intra_q90_sec) && m$intra_q90_sec <= ceiling) {
      abs_pass <- TRUE; fuzzy_level <- "strict"
    } else if (fuzzy_factor > 1 && is.finite(m$intra_q90_sec) && m$intra_q90_sec <= ceiling * fuzzy_factor) {
      abs_pass <- TRUE; fuzzy_level <- paste0("plus_", round(fuzzy_pct), "pct")
      if (fuzzy_pct <= 5) base_edge <- max(base_edge, 3.5) else base_edge <- max(base_edge, 4.0)
    } else {
      abs_pass <- FALSE; fuzzy_level <- "fail"
    }
  }

  max_bridge <- stpd_arbitration_num(bp$canonical_burst_max_bridge_ISI_sec %||% 0, 0)
  max_bridge_pass <- TRUE
  if (is.finite(max_bridge) && max_bridge > 0) max_bridge_pass <- is.finite(m$max_intra_ISI_sec) && m$max_intra_ISI_sec <= max_bridge

  edge_pass <- is.finite(m$edge_ratio) && m$edge_ratio >= base_edge
  context_pass <- is.finite(m$context_compression) && m$context_compression >= base_context
  edge_return_pass <- !is.finite(m$edge_return_ratio) || m$edge_return_ratio >= edge_return_min

  q95q50_max <- stpd_arbitration_num(bp$canonical_burst_internal_q95_q50_ratio_max %||% 3.5, 3.5)
  maxq50_max <- stpd_arbitration_num(bp$canonical_burst_internal_max_q50_ratio_max %||% 5.0, 5.0)
  cv_max <- stpd_arbitration_num(bp$canonical_burst_internal_cv_max %||% 1.5, 1.5)
  lv_max <- stpd_arbitration_num(bp$canonical_burst_internal_lv_max %||% 2.0, 2.0)
  internal_pass <- TRUE
  if (is.finite(m$internal_q95_q50_ratio)) internal_pass <- internal_pass && m$internal_q95_q50_ratio <= q95q50_max
  if (is.finite(m$internal_max_q50_ratio)) internal_pass <- internal_pass && m$internal_max_q50_ratio <= maxq50_max
  if (is.finite(m$CV)) internal_pass <- internal_pass && m$CV <= cv_max
  if (is.finite(m$LV)) internal_pass <- internal_pass && m$LV <= lv_max

  has_two_edges <- is.finite(m$pre_gap_sec) && is.finite(m$post_gap_sec)
  one_edge_ok <- (is.finite(m$pre_gap_sec) || is.finite(m$post_gap_sec)) && is.finite(m$edge_ratio) && m$edge_ratio >= max(base_edge, stpd_arbitration_num(bp$boundary_one_flank_ratio_min %||% 2.5, 2.5))
  boundary_ok <- !has_two_edges && one_edge_ok && context_pass && abs_pass && max_bridge_pass && internal_pass && !isTRUE(m$manual_negative_veto)

  strict_ok <- has_two_edges && abs_pass && max_bridge_pass && edge_pass && context_pass && edge_return_pass && internal_pass && !isTRUE(m$manual_negative_veto)
  final_label <- "reject"
  gate_status <- "reject"
  action <- "reject"
  decision <- "arbitration_reject"
  if (strict_ok) {
    if (!identical(fuzzy_level, "disabled") && !identical(fuzzy_level, "strict") && !isTRUE(bp$canonical_burst_allow_fuzzy_canonical %||% FALSE)) {
      final_label <- "possible_burst"; gate_status <- "fuzzy_possible"; action <- "demote_to_possible"; decision <- "fuzzy_abs_ceiling_pass_requires_review"
    } else {
      if (cls0 == "possible_burst") {
        final_label <- "possible_burst"
        gate_status <- "possible_review_pass"
        action <- "accept_review"
        decision <- "possible_burst_candidate_passed_arbitration_gates"
      } else {
        final_label <- if (cls0 == "long_burst") "long_burst" else "burst"
        gate_status <- "canonical_pass"; action <- "accept"; decision <- "canonical_burst_gate_pass"
      }
    }
  } else if (boundary_ok && isTRUE(bp$boundary_burst_mode %||% TRUE)) {
    final_label <- "possible_burst"; gate_status <- "boundary_possible"; action <- "demote_to_possible"; decision <- "one_sided_boundary_burst_review"
  } else if (!abs_pass) {
    decision <- "reject_abs_ceiling_fail"
  } else if (isTRUE(m$manual_negative_veto)) {
    decision <- "manual_negative_veto"
  } else if (!edge_pass || !context_pass) {
    # Weak eventness is not a burst; let independent HF/tonic candidates compete later.
    decision <- paste(c(if (!edge_pass) "edge_ratio_fail" else NULL, if (!context_pass) "context_compression_fail" else NULL), collapse = ";")
  } else if (!internal_pass || !max_bridge_pass) {
    decision <- paste(c(if (!internal_pass) "internal_coherence_fail" else NULL, if (!max_bridge_pass) "max_bridge_fail" else NULL), collapse = ";")
  }

  m$train <- as.character(train %||% "")
  m$candidate_layer <- "burst_candidate"
  m$final_label <- final_label
  m$abs_ceiling_sec <- ceiling
  m$abs_ceiling_pass <- abs_pass
  m$ceiling_fuzzy_level <- fuzzy_level
  m$max_bridge_pass <- max_bridge_pass
  m$edge_gate_pass <- edge_pass
  m$context_gate_pass <- context_pass
  m$edge_return_pass <- edge_return_pass
  m$internal_coherence_pass <- internal_pass
  m$gate_status <- gate_status
  m$decision_path <- decision
  m$action <- action
  m$score <- score0
  m$phenotype_prior <- as.character(ph$prior %||% "mixed")
  m
}

stpd_arbitration_candidate_row <- function(dat, row, params, layer, min_isi_sec = 0.001, train = "") {
  s <- suppressWarnings(as.integer(row$start_isi[1])); e <- suppressWarnings(as.integer(row$end_isi[1]))
  cls <- as.character(row$class[1] %||% layer)
  if (length(cls) == 0 || is.na(cls) || cls == "") cls <- as.character(layer)
  m <- stpd_arbitration_span_metrics(dat, s, e, params, min_isi_sec = min_isi_sec, train = train, candidate_class = cls)
  if (is.null(m)) return(NULL)
  ph <- stpd_arbitration_train_phenotype(dat, min_isi_sec)
  m$train <- as.character(train %||% "")
  m$candidate_layer <- layer
  m$final_label <- cls
  m$abs_ceiling_sec <- NA_real_
  m$abs_ceiling_pass <- NA
  m$ceiling_fuzzy_level <- "not_applicable"
  m$max_bridge_pass <- NA
  m$edge_gate_pass <- NA
  m$context_gate_pass <- NA
  m$edge_return_pass <- NA
  m$internal_coherence_pass <- NA
  m$gate_status <- "candidate"
  m$decision_path <- paste0(layer, "_candidate")
  m$action <- "candidate"
  m$score <- suppressWarnings(as.numeric(row$score[1] %||% NA_real_))
  m$phenotype_prior <- as.character(ph$prior %||% "mixed")
  m
}

stpd_arbitration_fill_interval <- function(pat, score, locked, s, e, label, val = NA_real_) {
  n <- length(pat)
  if (!is.finite(s) || !is.finite(e) || e < s || s < 2 || e > n) return(list(pat = pat, score = score))
  idx <- s:e
  idx <- idx[!locked[idx] & pat[idx] == ""]
  if (length(idx) > 0) {
    pat[idx] <- label
    if (!is.null(score)) score[idx] <- val
  }
  list(pat = pat, score = score)
}

stpd_arbitration_write_candidates <- function(pat, score, locked, candidates, label_filter = NULL) {
  if (is.null(candidates) || nrow(candidates) == 0) return(list(pat = pat, score = score))
  candidates <- candidates[order(suppressWarnings(as.numeric(candidates$start_isi)), -suppressWarnings(as.numeric(candidates$end_isi - candidates$start_isi))), , drop = FALSE]
  for (i in seq_len(nrow(candidates))) {
    lab <- as.character(candidates$final_label[i] %||% candidates$class[i] %||% "")
    if (length(lab) == 0 || is.na(lab)) lab <- ""
    if (lab == "" || identical(lab, "reject")) next
    if (!is.null(label_filter) && !(lab %in% label_filter)) next
    tmp <- stpd_arbitration_fill_interval(pat, score, locked, as.integer(candidates$start_isi[i]), as.integer(candidates$end_isi[i]), lab, suppressWarnings(as.numeric(candidates$score[i] %||% NA_real_)))
    pat <- tmp$pat; score <- tmp$score
  }
  list(pat = pat, score = score)
}

stpd_detect_train_arbitration_impl <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  burst_p <- params$burst
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  dat <- ensure_train_local_median_cache(dat, window = burst_p$local_window %||% 11L, min_isi_sec = min_isi_sec, force = TRUE)
  n <- nrow(dat)
  if (!("pattern_manual_negative" %in% names(dat))) dat$pattern_manual_negative <- rep("", n)
  if (n <= 1) {
    dat$pattern_auto <- ""; dat$auto_score <- NA_real_; return(dat)
  }

  manual_for_lock <- if (isTRUE(lock_manual)) stpd_arbitration_vec_label(dat$pattern_manual, n) else rep("", n)
  locked <- manual_for_lock != ""
  dat$pattern_auto <- rep("", n)
  dat$auto_score <- rep(NA_real_, n)
  patterns <- params$detector$patterns_to_run %||% stpd_default_patterns_to_run()
  fill_others <- isTRUE(params$detector$fill_others_auto)

  audit_rows <- list()
  seed_bridge_diag <- NULL
  burst_gated <- data.frame()

  # 1) Existing burst-family detector as candidate generator only.
  if (any(c("burst", "long_burst") %in% patterns)) {
    b <- detect_burst_train(dat, burst_p, min_isi_sec = min_isi_sec, train = train)
    seed_bridge_diag <- attr(b, "seed_bridge_diag")
    if (nrow(b) > 0) {
      keep_classes <- c("possible_burst")
      if ("burst" %in% patterns) keep_classes <- c(keep_classes, "burst")
      if ("long_burst" %in% patterns) keep_classes <- c(keep_classes, "long_burst")
      b <- b[as.character(b$class) %in% keep_classes, , drop = FALSE]
    }
    if (nrow(b) > 0) {
      gated <- list()
      for (i in seq_len(nrow(b))) {
        gi <- stpd_arbitration_canonical_burst_gate(dat, b[i, , drop = FALSE], params, min_isi_sec = min_isi_sec, train = train)
        if (is.null(gi)) next
        gated[[length(gated) + 1L]] <- gi
      }
      if (length(gated) > 0) {
        burst_gated <- dplyr::bind_rows(gated)
        audit_rows[[length(audit_rows) + 1L]] <- burst_gated
      }
    }
    attr(dat, "seed_bridge_diag") <- seed_bridge_diag
  }

  # 2) Independent high-frequency candidates. No burst occupancy mask is used here.
  hf_candidates <- data.frame()
  hf_patterns <- intersect(patterns, c("high_frequency_tonic", "high_frequency_spiking"))
  if (length(hf_patterns) > 0 && isTRUE(params$highfreq$enable %||% TRUE)) {
    hf <- detect_high_frequency_modes_train(dat, rep(FALSE, n), params$highfreq %||% default_params_sec()$highfreq, min_isi_sec = min_isi_sec, train = train)
    if (nrow(hf) > 0) hf <- hf[as.character(hf$class) %in% hf_patterns, , drop = FALSE]
    if (nrow(hf) > 0) {
      rows <- list()
      for (i in seq_len(nrow(hf))) rows[[length(rows) + 1L]] <- stpd_arbitration_candidate_row(dat, hf[i, , drop = FALSE], params, "hf_candidate", min_isi_sec, train)
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) > 0) {
        hf_candidates <- dplyr::bind_rows(rows)
        audit_rows[[length(audit_rows) + 1L]] <- hf_candidates
      }
    }
  }

  # 3) Independent tonic candidates.
  tonic_candidates <- data.frame()
  if ("tonic" %in% patterns) {
    tb <- detect_tonic_train(dat, rep(FALSE, n), params$tonic, burst_p$T_seed, min_isi_sec = min_isi_sec, train = train)
    if (nrow(tb) > 0) {
      tb$class <- "tonic"; tb$score <- NA_real_
      rows <- list()
      for (i in seq_len(nrow(tb))) rows[[length(rows) + 1L]] <- stpd_arbitration_candidate_row(dat, tb[i, , drop = FALSE], params, "tonic_candidate", min_isi_sec, train)
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) > 0) {
        tonic_candidates <- dplyr::bind_rows(rows)
        audit_rows[[length(audit_rows) + 1L]] <- tonic_candidates
      }
    }
  }

  # 4) Pause candidates are gap-level candidates and remain independent.
  pause_candidates <- data.frame()
  if ("pause" %in% patterns) {
    pb <- detect_pause_train(dat, rep(FALSE, n), params$pause, params$tonic, min_isi_sec = min_isi_sec, current_labels = rep("", n), train = train)
    attr(dat, "pause_diag") <- pb
    if (nrow(pb) > 0) {
      pb$class <- "pause"; if (!("score" %in% names(pb))) pb$score <- NA_real_
      rows <- list()
      for (i in seq_len(nrow(pb))) rows[[length(rows) + 1L]] <- stpd_arbitration_candidate_row(dat, pb[i, , drop = FALSE], params, "pause_candidate", min_isi_sec, train)
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) > 0) {
        pause_candidates <- dplyr::bind_rows(rows)
        audit_rows[[length(audit_rows) + 1L]] <- pause_candidates
      }
    }
  } else {
    attr(dat, "pause_diag") <- data.frame()
  }

  pat <- rep("", n); autoscore <- rep(NA_real_, n)

  # Event-level arbitration, conservative order:
  #   strict/fuzzy accepted burst-family first; HF candidates only if they do not overlap accepted burst-family;
  #   possible_burst for review; tonic; pause where still empty.
  if (nrow(burst_gated) > 0) {
    acc <- burst_gated[as.character(burst_gated$final_label) %in% c("burst", "long_burst"), , drop = FALSE]
    acc <- acc[order(-suppressWarnings(as.numeric(acc$edge_ratio)), -suppressWarnings(as.numeric(acc$context_compression))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, acc, label_filter = c("burst", "long_burst"))
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(hf_candidates) > 0) {
    # Do not fragment a continuous HF candidate around a canonical burst. If it overlaps an accepted burst,
    # skip the whole HF candidate rather than leaving small invalid HF residues.
    keep <- rep(TRUE, nrow(hf_candidates))
    for (i in seq_len(nrow(hf_candidates))) {
      idx <- as.integer(hf_candidates$start_isi[i]):as.integer(hf_candidates$end_isi[i])
      keep[i] <- !any(pat[idx] %in% c("burst", "long_burst"), na.rm = TRUE)
    }
    hf2 <- hf_candidates[keep, , drop = FALSE]
    hf2 <- hf2[order(-suppressWarnings(as.numeric(hf2$n_spikes)), suppressWarnings(as.numeric(hf2$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, hf2, label_filter = c("high_frequency_tonic", "high_frequency_spiking"))
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(burst_gated) > 0) {
    poss <- burst_gated[as.character(burst_gated$final_label) == "possible_burst", , drop = FALSE]
    # Do not let weak possible_burst split a validated HF state.
    poss <- poss[order(-suppressWarnings(as.numeric(poss$edge_ratio)), -suppressWarnings(as.numeric(poss$context_compression))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, poss, label_filter = "possible_burst")
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(tonic_candidates) > 0) {
    tonic_candidates <- tonic_candidates[order(-suppressWarnings(as.numeric(tonic_candidates$n_spikes)), suppressWarnings(as.numeric(tonic_candidates$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, tonic_candidates, label_filter = "tonic")
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(pause_candidates) > 0) {
    pause_candidates <- pause_candidates[order(-suppressWarnings(as.numeric(pause_candidates$max_intra_ISI_sec)), suppressWarnings(as.numeric(pause_candidates$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, pause_candidates, label_filter = "pause")
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if ("others" %in% patterns && fill_others) {
    art <- is_artifact_isi(dat$ISI_sec, min_isi_sec)
    fill_idx <- which(dat$idx >= 2 & !art & is.finite(dat$ISI_sec) & manual_for_lock == "" & pat == "")
    pat[fill_idx] <- "others"
  }

  dat$pattern_auto <- pat
  dat$auto_score <- autoscore
  dat <- stpd_post_validate_auto_event_sizes(dat, params, min_isi_sec = min_isi_sec, lock_manual = lock_manual, train = train)
  if (length(audit_rows) > 0) attr(dat, "candidate_diagnostic_audit") <- dplyr::bind_rows(audit_rows) else attr(dat, "candidate_diagnostic_audit") <- stpd_arbitration_empty_audit()
  dat
}

stpd_train_pipeline_near_miss_augmented <- stpd_detect_train_near_miss_augmented

stpd_detect_train_arbitrated <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  if (!isTRUE((params$arbitration %||% list())$enabled %||% TRUE)) {
    return(stpd_train_pipeline_near_miss_augmented(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual))
  }
  stpd_detect_train_arbitration_impl(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}
