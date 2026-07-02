# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# refined Machine-learning feature extraction and optional nnet model
# ============================================================


ml_label_levels <- function(ml_mode = c("strict_high_confidence", "candidate_family")) {
  ml_mode <- match.arg(ml_mode)
  if (ml_mode == "candidate_family") {
    return(c("burst_family", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others"))
  }
  c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
}

normalize_pattern_label <- function(x, fill_blank_others = FALSE, ml_mode = c("strict_high_confidence", "candidate_family")) {
  # strict ML/training mode keeps possible_burst separate. Candidate-family
  # mode explicitly merges burst + long_burst + possible_burst into burst_family
  # for candidate-recall tasks, not high-confidence classifier accuracy.
  ml_mode <- match.arg(ml_mode)
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x[x %in% c("possible burst", "possible-burst", "possible_burst")] <- "possible_burst"
  x[x %in% c("long burst", "long-burst", "long_burst", "longburst")] <- "long_burst"
  x[x %in% c("high-frequency tonic", "high frequency tonic", "high_frequency_tonic", "hf tonic", "hf_tonic", "hftonic")] <- "high_frequency_tonic"
  x[x %in% c("high-frequency spiking", "high frequency spiking", "high_frequency_spiking", "hf spiking", "hf_spiking", "hfspiking")] <- "high_frequency_spiking"
  x[x %in% c("other", "unclassified", "unlabeled")] <- "others"
  if (ml_mode == "candidate_family") {
    x[x %in% c("burst", "long_burst", "possible_burst")] <- "burst_family"
  }
  if (isTRUE(fill_blank_others)) x[x == ""] <- "others"
  x
}

extract_ml_features_for_train <- function(dat, train = "", source = c("audit_final", "final", "manual", "auto", "none"),
                                          auto_others = FALSE, fill_blank_others = FALSE,
                                          min_isi_sec = 0.001, context_n = 3L,
                                          ml_mode = c("strict_high_confidence", "candidate_family")) {
  ml_mode <- match.arg(ml_mode)
  source <- match.arg(source)
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec)
  n <- nrow(dat)
  if (n <= 1) return(data.frame())
  context_n <- max(1L, safe_int(context_n, 3L))
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  valid <- is.finite(isi) & isi >= min_isi_sec
  valid[1] <- FALSE
  pct <- suppressWarnings(as.numeric(dat$ISI_pct))
  
  label <- rep("", n)
  if (source == "manual") {
    label <- dat$pattern_manual
  } else if (source == "auto") {
    label <- dat$pattern_auto
  } else if (source == "audit_final") {
    label <- stpd_audit_final_labels(dat, min_isi_sec = min_isi_sec,
                                     auto_others = FALSE,
                                     prefer_stored = TRUE)
  } else if (source == "final") {
    # ML features should not silently turn every unlabeled interval into an
    # automatically learned negative class. `others` can still be learned when it
    # is explicitly present in manual/AUTO labels or when fill_blank_others is
    # intentionally enabled downstream.
    label <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                                   auto_others = FALSE,
                                   min_isi_sec = min_isi_sec)
  }
  label <- normalize_pattern_label(label, fill_blank_others = fill_blank_others, ml_mode = ml_mode)
  
  rows <- list()
  valid_vals <- valid_isi_values(isi, min_isi_sec)
  global_med <- if (length(valid_vals) > 0) median(valid_vals) else min_isi_sec
  
  for (i in which(valid)) {
    idxs <- (i - context_n):(i + context_n)
    vals <- rep(NA_real_, length(idxs))
    pcts <- rep(NA_real_, length(idxs))
    for (jj in seq_along(idxs)) {
      k <- idxs[jj]
      if (k >= 2 && k <= n && valid[k]) {
        vals[jj] <- isi[k]
        pcts[jj] <- pct[k]
      }
    }
    vals_fill <- vals
    vals_fill[!is.finite(vals_fill)] <- global_med
    pcts_fill <- pcts
    pcts_fill[!is.finite(pcts_fill)] <- 50
    local <- vals[is.finite(vals)]
    local_med <- if (length(local) > 0) median(local) else global_med
    prev <- if (i > 2 && valid[i - 1]) isi[i - 1] else NA_real_
    nextv <- if (i < n && valid[i + 1]) isi[i + 1] else NA_real_
    row <- data.frame(
      train = train,
      isi_idx = i,
      start_time_sec = dat$timestamp_sec[i - 1L],
      end_time_sec = dat$timestamp_sec[i],
      label = label[i],
      ISI_sec = isi[i],
      logISI = log10(max(isi[i], min_isi_sec)),
      ISI_pct = pct[i],
      local_median_sec = local_med,
      local_ratio = if (is.finite(local_med) && local_med > 0) isi[i] / local_med else NA_real_,
      prev_ratio = if (is.finite(prev) && isi[i] > 0) prev / isi[i] else NA_real_,
      next_ratio = if (is.finite(nextv) && isi[i] > 0) nextv / isi[i] else NA_real_,
      local_LV = calc_LV(local),
      local_CV = calc_CV(local),
      stringsAsFactors = FALSE
    )
    for (jj in seq_along(vals_fill)) {
      nm <- jj - context_n - 1L
      row[[paste0("logISI_w", ifelse(nm >= 0, "p", "m"), abs(nm))]] <- log10(max(vals_fill[jj], min_isi_sec))
      row[[paste0("pct_w", ifelse(nm >= 0, "p", "m"), abs(nm))]] <- pcts_fill[jj]
    }
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0) return(data.frame())
  bind_rows(rows)
}

extract_ml_feature_table <- function(trains, source = "final", auto_others = FALSE, fill_blank_others = FALSE,
                                     min_isi_sec = 0.001, context_n = 3L,
                                     ml_mode = c("strict_high_confidence", "candidate_family")) {
  ml_mode <- match.arg(ml_mode)
  rows <- list()
  for (tr in names(trains)) {
    x <- extract_ml_features_for_train(trains[[tr]], train = tr, source = source,
                                       auto_others = auto_others, fill_blank_others = fill_blank_others,
                                       min_isi_sec = min_isi_sec, context_n = context_n,
                                       ml_mode = ml_mode)
    if (nrow(x) > 0) rows[[length(rows) + 1L]] <- x
  }
  if (length(rows) == 0) return(data.frame())
  bind_rows(rows)
}

ml_feature_columns <- function(df) {
  exclude <- c("train", "isi_idx", "start_time_sec", "end_time_sec", "label")
  cols <- setdiff(names(df), exclude)
  cols[vapply(df[cols], function(x) is.numeric(x) || is.integer(x), logical(1))]
}

train_nnet_pattern_model <- function(df, hidden = 12L, decay = 0.001, maxit = 300L,
                                    ml_mode = c("strict_high_confidence", "candidate_family")) {
  ml_mode <- match.arg(ml_mode)
  if (!requireNamespace("nnet", quietly = TRUE)) stop("Package 'nnet' is required for neural-network training. Install it or use the ML feature CSV export.")
  if (is.null(df) || nrow(df) == 0) stop("No feature rows available for training.")
  df$label <- normalize_pattern_label(df$label, fill_blank_others = FALSE, ml_mode = ml_mode)
  classes <- ml_label_levels(ml_mode)
  df <- df[df$label %in% classes, , drop = FALSE]
  if (nrow(df) < 10) stop("Need at least 10 labeled ISIs for neural-network training.")
  y <- factor(df$label, levels = classes)
  y <- droplevels(y)
  if (nlevels(y) < 2) stop("Need at least two label classes for neural-network training.")
  cols <- ml_feature_columns(df)
  x <- as.data.frame(df[cols])
  for (cc in cols) {
    v <- suppressWarnings(as.numeric(x[[cc]]))
    med <- median(v[is.finite(v)], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    v[!is.finite(v)] <- med
    x[[cc]] <- v
  }
  mu <- vapply(x, mean, numeric(1), na.rm = TRUE)
  sdv <- vapply(x, stats::sd, numeric(1), na.rm = TRUE)
  sdv[!is.finite(sdv) | sdv == 0] <- 1
  xs <- sweep(sweep(as.matrix(x), 2, mu, "-"), 2, sdv, "/")
  yy <- nnet::class.ind(y)
  set.seed(1)
  mdl <- nnet::nnet(xs, yy, size = safe_int(hidden, 12L), decay = decay, maxit = safe_int(maxit, 300L), softmax = TRUE, trace = FALSE)
  list(
    model_type = paste0("nnet_multiclass_isi_window_", ml_mode),
    ml_label_mode = ml_mode,
    model = mdl,
    feature_cols = cols,
    mu = mu,
    sd = sdv,
    label_levels = colnames(yy),
    training_counts = as.data.frame(table(label = y), stringsAsFactors = FALSE),
    context_n = NA_integer_,
    created_at = as.character(Sys.time())
  )
}

predict_nnet_pattern_model <- function(model_bundle, df, confidence_cutoff = 0.60) {
  if (is.null(model_bundle) || is.null(model_bundle$model)) stop("No trained model loaded.")
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  cols <- model_bundle$feature_cols
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) stop("Feature table is missing columns required by the model: ", paste(missing_cols, collapse = ", "))
  x <- as.data.frame(df[cols])
  for (cc in cols) {
    v <- suppressWarnings(as.numeric(x[[cc]]))
    med <- model_bundle$mu[cc]
    if (!is.finite(med)) med <- 0
    v[!is.finite(v)] <- med
    x[[cc]] <- v
  }
  xs <- sweep(sweep(as.matrix(x), 2, model_bundle$mu[cols], "-"), 2, model_bundle$sd[cols], "/")
  prob <- predict(model_bundle$model, xs, type = "raw")
  if (is.vector(prob)) prob <- matrix(prob, ncol = length(model_bundle$label_levels))
  colnames(prob) <- model_bundle$label_levels
  pred_i <- max.col(prob, ties.method = "first")
  pred_label <- colnames(prob)[pred_i]
  pred_conf <- prob[cbind(seq_len(nrow(prob)), pred_i)]
  out <- df[, c("train", "isi_idx", "start_time_sec", "end_time_sec", "ISI_sec", "ISI_pct"), drop = FALSE]
  out$pred_label <- pred_label
  out$pred_confidence <- pred_conf
  out$accepted <- is.finite(pred_conf) & pred_conf >= confidence_cutoff
  for (cc in colnames(prob)) out[[paste0("prob_", cc)]] <- prob[, cc]
  out
}


classification_eval <- function(pred, truth, classes = ml_label_levels("strict_high_confidence"), ml_mode = NULL) {
  if (is.null(ml_mode)) {
    ml_mode <- if ("burst_family" %in% classes) "candidate_family" else "strict_high_confidence"
  }
  truth <- normalize_pattern_label(truth, fill_blank_others = FALSE, ml_mode = ml_mode)
  pred <- normalize_pattern_label(pred, fill_blank_others = FALSE, ml_mode = ml_mode)
  keep <- truth %in% classes & pred %in% classes
  truth <- truth[keep]
  pred <- pred[keep]
  if (length(truth) == 0) {
    return(list(
      accuracy = NA_real_, n = 0L,
      confusion = data.frame(truth = character(), pred_label = character(), n = integer()),
      metrics = data.frame(class = classes, precision = NA_real_, recall = NA_real_, F1 = NA_real_, support = 0L)
    ))
  }
  cm <- as.data.frame(table(truth = factor(truth, levels = classes), pred_label = factor(pred, levels = classes)), stringsAsFactors = FALSE)
  names(cm)[names(cm) == "Freq"] <- "n"
  cm <- cm[cm$n > 0, , drop = FALSE]
  metric_rows <- lapply(classes, function(cl) {
    tp <- sum(truth == cl & pred == cl)
    fp <- sum(truth != cl & pred == cl)
    fn <- sum(truth == cl & pred != cl)
    prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    rec <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    f1 <- if (is.finite(prec) && is.finite(rec) && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else NA_real_
    data.frame(class = cl, precision = prec, recall = rec, F1 = f1, support = sum(truth == cl), stringsAsFactors = FALSE)
  })
  list(
    accuracy = mean(pred == truth, na.rm = TRUE),
    n = length(truth),
    confusion = cm,
    metrics = bind_rows(metric_rows)
  )
}

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

  if (label %in% c("burst", "possible_burst", "burst_family")) {
    n_spk <- e - s + 2L
    if (n_spk < (params$burst$G_min %||% 3L)) return(FALSE)
    edge <- calc_edge_contrast_stats(isi, s, e, min_isi_sec = min_isi_sec, robust_q = params$burst$contrast_q %||% 0.90)
    edge_min <- edge$contrast_min_q
    edge_geom <- edge$contrast_geom_q
    # Use a review-friendly criterion for NN output: enforce burst structure, but
    # do not require the full deterministic final edge thresholds.
    min_thr <- max(1.05, (params$burst$contrast_min_possible %||% 1.20) * 0.90)
    geom_thr <- max(1.10, (params$burst$contrast_geom_possible %||% 1.30) * 0.90)
    return(edge$n_flank >= 1L && is.finite(edge_min) && is.finite(edge_geom) && edge_min >= min_thr && edge_geom >= geom_thr)
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

postprocess_nn_predictions_for_train <- function(dat, pred_train, params, min_isi_sec = 0.001,
                                                    train = "", apply_others = FALSE) {
  n <- nrow(dat)
  if (n <= 1 || is.null(pred_train) || nrow(pred_train) == 0) return(data.frame())
  lab <- rep("", n)
  conf <- rep(NA_real_, n)
  accepted <- pred_train[pred_train$accepted, , drop = FALSE]
  if (nrow(accepted) == 0) return(data.frame())
  for (ii in seq_len(nrow(accepted))) {
    idx <- safe_int(accepted$isi_idx[ii], NA_integer_)
    if (!is.finite(idx) || idx < 2 || idx > n) next
    if (!is.na(dat$pattern_manual[idx]) && dat$pattern_manual[idx] != "") next
    pl <- normalize_pattern_label(accepted$pred_label[ii], fill_blank_others = FALSE)[1]
    if (pl == "burst_family") pl <- "possible_burst"
    if (pl == "others" && !isTRUE(apply_others)) next
    lab[idx] <- pl
    conf[idx] <- accepted$pred_confidence[ii]
  }

  # Salt-and-pepper suppression. Single pause ISIs are valid; isolated burst or
  # tonic calls are removed unless they are part of a same-label neighborhood.
  for (i in 2:n) {
    if (lab[i] == "" || lab[i] == "pause" || lab[i] == "others") next
    same_left <- i > 2 && lab[i - 1L] == lab[i]
    same_right <- i < n && lab[i + 1L] == lab[i]
    if (!same_left && !same_right) lab[i] <- ""
  }

  segs <- label_segments(lab, labels = c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others"))
  if (nrow(segs) == 0) return(data.frame())
  rows <- list()
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
  if (length(rows) == 0) return(data.frame())
  bind_rows(rows)
}

