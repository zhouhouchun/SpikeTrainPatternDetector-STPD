# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Detector helpers
# ============================================================

merge_blocks <- function(blocks, gap_ok_fun) {
  if (nrow(blocks) <= 1) return(blocks)
  blocks <- blocks[order(blocks$start_isi), , drop = FALSE]
  out <- list()
  cur_s <- blocks$start_isi[1]
  cur_e <- blocks$end_isi[1]
  for (k in 2:nrow(blocks)) {
    s <- blocks$start_isi[k]
    e <- blocks$end_isi[k]
    if (gap_ok_fun(cur_s, cur_e, s, e)) {
      cur_e <- e
    } else {
      out[[length(out) + 1]] <- c(cur_s, cur_e)
      cur_s <- s
      cur_e <- e
    }
  }
  out[[length(out) + 1]] <- c(cur_s, cur_e)
  out <- as.data.frame(do.call(rbind, out))
  colnames(out) <- c("start_isi", "end_isi")
  out
}

anti_burst_veto <- function(isi_vec, T_B_seed) {
  x <- finite_num(isi_vec)
  if (length(x) == 0 || !is.finite(T_B_seed)) return(FALSE)
  cond1 <- sum(x <= T_B_seed, na.rm = TRUE) >= 2
  cond2 <- FALSE
  if (length(x) >= 2) {
    roll2 <- (x[-length(x)] + x[-1]) / 2
    cond2 <- any(roll2 <= T_B_seed, na.rm = TRUE)
  }
  cond1 || cond2
}

stpd_tonic_burst_overlap_guard_config <- function(p = NULL) {
  p <- p %||% list()
  enabled <- isTRUE(p$burst_overlap_guard %||% TRUE)
  guard_factor <- suppressWarnings(as.numeric(p$burst_overlap_guard_factor %||% 1.15))
  if (!is.finite(guard_factor) || guard_factor < 1) guard_factor <- 1.15
  lower_quantile <- suppressWarnings(as.numeric(p$burst_overlap_lower_quantile %||% 0.10))
  if (!is.finite(lower_quantile)) lower_quantile <- 0.10
  lower_quantile <- min(max(lower_quantile, 0), 0.50)
  low_fraction_max <- suppressWarnings(as.numeric(p$burst_overlap_low_fraction_max %||% 0.05))
  if (!is.finite(low_fraction_max)) low_fraction_max <- 0.05
  low_fraction_max <- min(max(low_fraction_max, 0), 1)
  reference_quantile <- suppressWarnings(as.numeric(p$burst_overlap_reference_quantile %||% 0.95))
  if (!is.finite(reference_quantile)) reference_quantile <- 0.95
  reference_quantile <- min(max(reference_quantile, 0.50), 0.999)
  list(
    enabled = enabled,
    guard_factor = guard_factor,
    lower_quantile = lower_quantile,
    low_fraction_max = low_fraction_max,
    reference_quantile = reference_quantile
  )
}

stpd_tonic_burst_overlap_reference <- function(dat = NULL, T_B_seed = NA_real_,
                                               min_isi_sec = 0.001, p = NULL,
                                               fallback_refs = numeric()) {
  cfg <- stpd_tonic_burst_overlap_guard_config(p)
  refs <- finite_num(c(T_B_seed, fallback_refs))
  if (!is.null(dat) && is.data.frame(dat) && all(c("ISI_sec", "pattern_auto") %in% names(dat))) {
    labs <- tolower(trimws(as.character(dat$pattern_auto)))
    labs[is.na(labs)] <- ""
    labs <- gsub("[ -]+", "_", labs)
    burst_like <- labs %in% c("burst", "long_burst", "possible_burst")
    burst_vals <- valid_isi_values(dat$ISI_sec[burst_like], min_isi_sec)
    if (length(burst_vals) > 0) {
      refs <- c(refs, safe_q(burst_vals, cfg$reference_quantile, default = NA_real_))
    }
  }
  refs <- finite_num(refs)
  if (length(refs) == 0) return(NA_real_)
  max(refs, na.rm = TRUE)
}

stpd_tonic_burst_overlap_ok <- function(isi_vec, burst_ref_sec, p = NULL,
                                        min_isi_sec = 0.001) {
  cfg <- stpd_tonic_burst_overlap_guard_config(p)
  if (!isTRUE(cfg$enabled)) return(TRUE)
  x <- valid_isi_values(isi_vec, min_isi_sec)
  if (length(x) == 0) return(FALSE)
  burst_ref_sec <- suppressWarnings(as.numeric(burst_ref_sec))
  if (!is.finite(burst_ref_sec) || burst_ref_sec <= 0) return(TRUE)

  center_floor <- burst_ref_sec * cfg$guard_factor
  mean_ok <- is.finite(mean(x)) && mean(x) > center_floor
  median_ok <- is.finite(stats::median(x)) && stats::median(x) > center_floor
  low_q <- safe_q(x, cfg$lower_quantile, default = NA_real_)[1]
  low_q_ok <- is.finite(low_q) && low_q > burst_ref_sec
  low_fraction <- mean(x <= burst_ref_sec, na.rm = TRUE)
  low_fraction_ok <- is.finite(low_fraction) && low_fraction <= cfg$low_fraction_max

  mean_ok && median_ok && low_q_ok && low_fraction_ok
}

anti_tonic_veto <- function(isi_vec, lv_core, T_t_min, T_t_max) {
  x <- finite_num(isi_vec)
  if (length(x) < 2) return(FALSE)
  lv <- calc_LV(x)
  m <- mean(x)
  is.finite(lv) && is.finite(m) && lv <= lv_core && m >= T_t_min && m <= T_t_max
}

candidate_metrics <- function(dat, s_isi, e_isi, p, min_isi_sec = 0.001) {
  isi <- dat$ISI_sec
  n <- nrow(dat)
  if (s_isi < 2 || e_isi > n || e_isi < s_isi) return(NULL)
  
  vals <- valid_isi_values(isi[s_isi:e_isi], min_isi_sec)
  if (length(vals) < max(1L, p$G_min - 1L)) return(NULL)
  
  n_spk <- e_isi - s_isi + 2L
  if (n_spk < p$G_min) return(NULL)
  
  dur <- dat$timestamp_sec[e_isi] - dat$timestamp_sec[s_isi - 1]
  if (!is.finite(dur) || dur < p$D_min) return(NULL)
  Dmax <- suppressWarnings(as.numeric(p$D_max %||% 0))
  if (is.finite(Dmax) && Dmax > 0 && dur > Dmax) return(NULL)
  
  bc <- calc_event_contrast_stats(
    isi, s_isi, e_isi,
    min_isi_sec = min_isi_sec,
    robust_q = p$contrast_q %||% 0.90,
    context_k = p$context_k %||% 5L
  )
  
  ref_min <- if (identical(p$contrast_ref %||% "q", "max")) bc$contrast_min_ctx_max else bc$contrast_min_ctx_q
  ref_geom <- if (identical(p$contrast_ref %||% "q", "max")) bc$contrast_geom_ctx_max else bc$contrast_geom_ctx_q
  if (!is.finite(ref_geom) || !is.finite(ref_min)) return(NULL)
  
  mm <- if (length(vals) > 0) max(vals) / mean(vals) else NA_real_
  lv <- calc_LV(vals)
  ctx_med <- safe_median(c(bc$context_pre_ISI_sec, bc$context_post_ISI_sec), default = NA_real_)
  comp <- if (is.finite(ctx_med) && is.finite(bc$core_q) && bc$core_q > 0) ctx_med / bc$core_q else NA_real_
  
  score <- log(max(ref_geom, 1e-9)) + 0.10 * log(max(n_spk, 1))
  if (is.finite(comp)) score <- score + 0.10 * log(max(comp, 1e-9))
  if (is.finite(mm)) score <- score - 0.10 * max(0, mm - (p$mm_penalty_start %||% 2.5))
  if (is.finite(lv)) score <- score - 0.05 * max(0, lv - (p$lv_penalty_start %||% 1.5))
  
  class <- "reject"
  min_flanks <- safe_int(p$contrast_min_flanks %||% 2L, 2L)
  if (bc$n_flank_ctx >= min_flanks) {
    if (ref_min >= (p$contrast_min_high %||% 1.8) &&
        ref_geom >= (p$contrast_geom_high %||% 1.8) &&
        score >= (p$score_high %||% 0.65)) {
      class <- "burst"
    } else if (ref_min >= (p$contrast_min_possible %||% 1.25) &&
               ref_geom >= (p$contrast_geom_possible %||% 1.35) &&
               score >= (p$score_possible %||% 0.35)) {
      class <- "possible_burst"
    }
  }
  # one-sided boundary burst support. If a recording starts/ends inside
  # a burst-like compressed cluster, one flank is unavailable; keep it reviewable.
  if (class == "reject" && isTRUE(p$boundary_burst_mode %||% FALSE)) {
    at_boundary <- (s_isi <= 2L) || (e_isi >= n)
    core_pct <- isi_percentile_scalar(bc$core_q, isi, min_isi_sec = min_isi_sec)
    boundary_pct_ok <- !is.finite(core_pct) || core_pct <= (p$boundary_core_pct_max %||% 30)
    boundary_ratio_ok <- bc$n_flank_ctx >= 1L && is.finite(ref_geom) && ref_geom >= (p$boundary_one_flank_ratio_min %||% 2.50)
    boundary_local_ok <- !is.finite(comp) || comp >= (p$boundary_local_ratio_min %||% 2.20)
    boundary_size_ok <- (safe_int(p$boundary_max_n_spikes %||% 8L, 8L) <= 0L || n_spk <= safe_int(p$boundary_max_n_spikes %||% 8L, 8L))
    boundary_dur_max <- suppressWarnings(as.numeric(p$boundary_max_duration %||% 0))
    boundary_dur_ok <- !is.finite(boundary_dur_max) || boundary_dur_max <= 0 || dur <= boundary_dur_max
    if (at_boundary && boundary_ratio_ok && boundary_pct_ok && boundary_local_ok && boundary_size_ok && boundary_dur_ok &&
        score >= (p$boundary_score_possible %||% p$score_possible %||% 0.25)) {
      class <- p$boundary_burst_label %||% "possible_burst"
      if (!class %in% c("burst", "possible_burst")) class <- "possible_burst"
      score <- score + 0.05
    }
  }
  
  data.frame(
    start_isi = as.integer(s_isi),
    end_isi = as.integer(e_isi),
    n_spikes = as.integer(n_spk),
    duration_sec = dur,
    score = score,
    class = class,
    contrast_min_ctx_q = bc$contrast_min_ctx_q,
    contrast_geom_ctx_q = bc$contrast_geom_ctx_q,
    contrast_min_ctx_max = bc$contrast_min_ctx_max,
    contrast_geom_ctx_max = bc$contrast_geom_ctx_max,
    core_q = bc$core_q,
    mm = mm,
    lv = lv,
    stringsAsFactors = FALSE
  )
}

optimize_burst_boundary <- function(dat, s_isi, e_isi, p, min_isi_sec = 0.001) {
  n <- nrow(dat)
  r <- safe_int(p$optimize_radius %||% 1L, 1L)
  s_grid <- seq(max(2L, s_isi - r), min(n, s_isi + r))
  e_grid <- seq(max(2L, e_isi - r), min(n, e_isi + r))
  
  best <- NULL
  best_score <- -Inf
  for (ss in s_grid) {
    for (ee in e_grid) {
      if (ee < ss) next
      m <- candidate_metrics(dat, ss, ee, p, min_isi_sec = min_isi_sec)
      if (is.null(m)) next
      if (is.finite(m$score[1]) && m$score[1] > best_score) {
        best <- m
        best_score <- m$score[1]
      }
    }
  }
  best
}

seed_blocks_burst <- function(dat, p, min_isi_sec = 0.001) {
  isi <- dat$ISI_sec
  n <- nrow(dat)
  valid <- is.finite(isi) & !is_artifact_isi(isi, min_isi_sec)
  valid[1] <- FALSE
  
  T_seed <- p$T_seed
  seed_abs <- valid & is.finite(T_seed) & (isi <= T_seed)
  
  loc_med <- rep(NA_real_, n)
  if (isTRUE(p$use_local_compression_seed)) {
    for (ii in 2:n) {
      loc_med[ii] <- get_local_median(
        isi, ii,
        window = p$local_window %||% 11L,
        exclude_idx = ii,
        min_isi_sec = min_isi_sec
      )
    }
  }
  seed_rel <- valid & isTRUE(p$use_local_compression_seed) &
    is.finite(loc_med) & loc_med > 0 &
    (isi <= loc_med / (p$local_compression_min %||% 1.4))
  
  seed <- seed_abs | seed_rel
  blocks <- list()
  i <- 2L
  while (i <= n) {
    if (seed[i]) {
      s <- i
      j <- i
      while (j + 1 <= n && (seed[j + 1] || is_artifact_isi(isi[j + 1], min_isi_sec))) {
        j <- j + 1L
      }
      blocks[[length(blocks) + 1L]] <- c(s, j)
      i <- j + 1L
    } else {
      i <- i + 1L
    }
  }
  if (length(blocks) == 0) {
    return(data.frame(start_isi = integer(0), end_isi = integer(0), source = character(0)))
  }
  blocks <- as.data.frame(do.call(rbind, blocks))
  colnames(blocks) <- c("start_isi", "end_isi")
  
  if (isTRUE(p$allow_bridge) && nrow(blocks) >= 2) {
    gap_ok_fun <- function(cur_s, cur_e, next_s, next_e) {
      between <- seq(cur_e + 1L, next_s - 1L)
      if (length(between) == 0) return(TRUE)
      non_art_between <- between[!is_artifact_isi(isi[between], min_isi_sec)]
      if (length(non_art_between) == 0) return(TRUE)
      if (length(non_art_between) > (p$connector_max_n %||% 1L)) return(FALSE)
      ok_abs <- is.finite(p$T_bridge) & all(is.finite(isi[non_art_between]) & isi[non_art_between] <= p$T_bridge)
      ok_rel <- FALSE
      if (isTRUE(p$use_local_compression_seed)) {
        ok_rel <- all(vapply(non_art_between, function(jj) {
          lm <- get_local_median(isi, jj, window = p$local_window %||% 11L,
                                 exclude_idx = jj, min_isi_sec = min_isi_sec)
          is.finite(lm) && lm > 0 && isi[jj] <= lm / max(1.10, (p$local_compression_min %||% 1.4) * 0.85)
        }, logical(1)))
      }
      ok_abs || ok_rel
    }
    blocks <- merge_blocks(blocks, gap_ok_fun)
  }
  
  blocks$source <- "seed"
  # Keep seed-derived blocks during candidate capping; context-window proposals
  # receive finite cheap pre-scores below.
  blocks$pre_score <- Inf
  blocks
}

context_window_proposals <- function(dat, p, min_isi_sec = 0.001) {
  if (!isTRUE(p$use_context_proposals)) {
    return(data.frame(start_isi = integer(0), end_isi = integer(0), source = character(0), pre_score = numeric(0)))
  }
  isi <- dat$ISI_sec
  n <- nrow(dat)
  if (n <= 3) return(data.frame(start_isi = integer(0), end_isi = integer(0), source = character(0), pre_score = numeric(0)))
  
  Lmin <- max(1L, safe_int(p$proposal_window_min_isi %||% 2L, 2L))
  Lmax <- max(Lmin, safe_int(p$proposal_window_max_isi %||% 8L, 8L))
  Lmax <- min(Lmax, n - 1L)
  
  # Performance guard: in fast mode, only evaluate windows containing at least
  # one ISI that is locally compressed. This prevents exhaustive context-window
  # scanning from blocking the Shiny session on long trains.
  fast_context <- isTRUE(p$fast_context_proposals %||% TRUE)
  comp_flag <- rep(TRUE, n)
  if (fast_context) {
    art <- is_artifact_isi(isi, min_isi_sec)
    valid <- is.finite(isi) & !art
    valid[1] <- FALSE
    loc_med <- rep(NA_real_, n)
    for (ii in 2:n) {
      loc_med[ii] <- get_local_median(
        isi, ii,
        window = p$local_window %||% 11L,
        exclude_idx = ii,
        min_isi_sec = min_isi_sec
      )
    }
    comp_ratio <- max(1.10, suppressWarnings(as.numeric(p$local_compression_min %||% 1.40)) * 0.75)
    comp_flag <- valid & is.finite(loc_med) & loc_med > 0 & (isi <= loc_med / comp_ratio)
  }
  
  out <- list()
  for (L in Lmin:Lmax) {
    last_s <- n - L + 1L
    if (last_s < 2) next
    for (s in 2:last_s) {
      e <- s + L - 1L
      if (fast_context && !any(comp_flag[s:e], na.rm = TRUE)) next
      vals <- valid_isi_values(isi[s:e], min_isi_sec)
      if (length(vals) < L) next
      bc <- calc_event_contrast_stats(
        isi, s, e,
        min_isi_sec = min_isi_sec,
        robust_q = p$contrast_q %||% 0.90,
        context_k = p$context_k %||% 5L
      )
      cmin <- if (identical(p$contrast_ref %||% "q", "max")) bc$contrast_min_ctx_max else bc$contrast_min_ctx_q
      cgeom <- if (identical(p$contrast_ref %||% "q", "max")) bc$contrast_geom_ctx_max else bc$contrast_geom_ctx_q
      if (!is.finite(cmin) || !is.finite(cgeom)) next
      if (bc$n_flank_ctx < (p$contrast_min_flanks %||% 2L)) next
      if (cmin >= (p$proposal_contrast_min %||% 1.20) && cgeom >= (p$proposal_contrast_geom_min %||% 1.30)) {
        # Cheap score used only for early capping. Full score is recomputed later.
        ps <- log(max(cgeom, 1e-9)) + 0.05 * log(max(L + 1L, 1L)) + 0.02 * log(max(cmin, 1e-9))
        out[[length(out) + 1L]] <- data.frame(
          start_isi = as.integer(s),
          end_isi = as.integer(e),
          source = "context",
          pre_score = ps,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(out) == 0) {
    return(data.frame(start_isi = integer(0), end_isi = integer(0), source = character(0), pre_score = numeric(0)))
  }
  bind_rows(out)
}

nms_burst_candidates <- function(cand, n, allow_possible = TRUE) {
  if (is.null(cand) || nrow(cand) == 0) return(cand)
  cand <- cand %>%
    filter(class %in% c("burst", "long_burst", "possible_burst")) %>%
    mutate(priority = dplyr::case_when(class == "long_burst" ~ 3L, class == "burst" ~ 3L, TRUE ~ 1L)) %>%
    arrange(desc(priority), desc(score), start_isi, end_isi)
  if (!isTRUE(allow_possible)) cand <- cand %>% filter(class %in% c("burst", "long_burst"))
  if (nrow(cand) == 0) return(cand)
  
  used <- rep(FALSE, n)
  keep <- logical(nrow(cand))
  for (i in seq_len(nrow(cand))) {
    idx <- cand$start_isi[i]:cand$end_isi[i]
    if (!any(used[idx])) {
      keep[i] <- TRUE
      used[idx] <- TRUE
    }
  }
  cand[keep, , drop = FALSE] %>% arrange(start_isi, end_isi)
}

burst_gap_is_stitchable <- function(dat, left_end_isi, right_start_isi, p, min_isi_sec = 0.001) {
  isi <- dat$ISI_sec
  n <- nrow(dat)
  if (right_start_isi <= left_end_isi + 1L) return(TRUE)
  
  gap <- seq(left_end_isi + 1L, right_start_isi - 1L)
  gap <- gap[gap >= 2L & gap <= n]
  if (length(gap) == 0) return(TRUE)
  
  non_art_gap <- gap[!is_artifact_isi(isi[gap], min_isi_sec)]
  if (length(non_art_gap) == 0) return(TRUE)
  
  max_gap_n <- safe_int(p$stitch_gap_max_n %||% 1L, 1L)
  if (length(non_art_gap) > max_gap_n) return(FALSE)
  
  vals <- isi[non_art_gap]
  if (!all(is.finite(vals))) return(FALSE)
  
  # Absolute bridge: keeps compatibility with the original adaptive-interval logic.
  t_bridge <- suppressWarnings(as.numeric(p$T_bridge %||% NA_real_))
  if (is.finite(t_bridge) && all(vals <= t_bridge)) return(TRUE)
  
  # Relative bridge: if the unlabeled gap ISI is still compressed relative to the local
  # background, it is probably an internal burst connector rather than a true boundary.
  frac <- suppressWarnings(as.numeric(p$stitch_gap_local_frac %||% 0.85))
  if (!is.finite(frac) || frac <= 0) frac <- 0.85
  
  all(vapply(non_art_gap, function(jj) {
    lm <- get_local_median(
      isi, jj,
      window = p$local_window %||% 11L,
      exclude_idx = jj,
      min_isi_sec = min_isi_sec
    )
    is.finite(lm) && lm > 0 && isi[jj] <= lm * frac
  }, logical(1)))
}

post_merge_burst_candidates <- function(cand, dat, p, min_isi_sec = 0.001) {
  if (!isTRUE(p$stitch_short_burst_gaps)) return(cand)
  if (is.null(cand) || nrow(cand) <= 1) return(cand)
  
  cand <- cand %>%
    filter(class %in% c("burst", "long_burst", "possible_burst")) %>%
    arrange(start_isi, end_isi, desc(score))
  if (nrow(cand) <= 1) return(cand)
  
  groups <- list()
  cur_rows <- cand[1, , drop = FALSE]
  cur_s <- cand$start_isi[1]
  cur_e <- cand$end_isi[1]
  
  for (i in 2:nrow(cand)) {
    ns <- cand$start_isi[i]
    ne <- cand$end_isi[i]
    can_join <- ns <= cur_e || burst_gap_is_stitchable(dat, cur_e, ns, p, min_isi_sec)
    if (can_join) {
      cur_rows <- bind_rows(cur_rows, cand[i, , drop = FALSE])
      cur_s <- min(cur_s, ns)
      cur_e <- max(cur_e, ne)
    } else {
      groups[[length(groups) + 1L]] <- list(rows = cur_rows, start_isi = cur_s, end_isi = cur_e)
      cur_rows <- cand[i, , drop = FALSE]
      cur_s <- ns
      cur_e <- ne
    }
  }
  groups[[length(groups) + 1L]] <- list(rows = cur_rows, start_isi = cur_s, end_isi = cur_e)
  
  merged <- lapply(groups, function(g) {
    rows <- g$rows
    target_class <- if (any(rows$class == "long_burst") && isTRUE(p$promote_mixed_burst_family %||% TRUE)) {
      "long_burst"
    } else if (any(rows$class == "burst") && isTRUE(p$promote_mixed_burst_family %||% TRUE)) {
      "burst"
    } else {
      "possible_burst"
    }
    max_score <- max(rows$score, na.rm = TRUE)
    src <- if ("source" %in% names(rows)) paste(unique(rows$source), collapse = "+") else "post_merge"
    
    m <- candidate_metrics(dat, g$start_isi, g$end_isi, p, min_isi_sec = min_isi_sec)
    if (is.null(m)) {
      # Fallback: keep the stitched span as reviewable, rather than displaying
      # multiple disconnected orange/red fragments around the same visual cluster.
      data.frame(
        start_isi = as.integer(g$start_isi),
        end_isi = as.integer(g$end_isi),
        n_spikes = as.integer(g$end_isi - g$start_isi + 2L),
        duration_sec = dat$timestamp_sec[g$end_isi] - dat$timestamp_sec[g$start_isi - 1L],
        score = max_score,
        class = target_class,
        contrast_min_ctx_q = NA_real_,
        contrast_geom_ctx_q = NA_real_,
        contrast_min_ctx_max = NA_real_,
        contrast_geom_ctx_max = NA_real_,
        core_q = NA_real_,
        mm = NA_real_,
        lv = NA_real_,
        source = src,
        stringsAsFactors = FALSE
      )
    } else {
      # keep the intended burst-family semantic class after stitching.
      # candidate_metrics() recomputes size/duration/edge metrics on the merged span,
      # but if fragments already indicate a long-burst family and promotion is enabled,
      # the merged candidate should not be silently forced back to generic burst.
      if (target_class %in% c("burst", "long_burst") && isTRUE(p$promote_mixed_burst_family %||% TRUE)) {
        m$class <- target_class
      } else if (!(m$class %in% c("burst", "long_burst", "possible_burst"))) {
        m$class <- target_class
      }
      m$score <- pmax(m$score, max_score, na.rm = TRUE)
      m$source <- src
      m$merged_reclassified <- TRUE
      m
    }
  })
  
  out_merged <- bind_rows(merged) %>% arrange(start_isi, end_isi)
  if (nrow(out_merged) > 0) {
    out_merged <- bind_rows(lapply(seq_len(nrow(out_merged)), function(ii) {
      reclassify_burst_family_candidate(out_merged[ii, , drop = FALSE], dat, p, min_isi_sec = min_isi_sec)
    })) %>% arrange(start_isi, end_isi)
  }
  out_merged
}

detect_burst_train_sliding_context <- function(dat, p, min_isi_sec = 0.001) {
  n <- nrow(dat)
  if (n <= 1) {
    return(data.frame(start_isi = integer(0), end_isi = integer(0), class = character(0), score = numeric(0)))
  }
  
  props <- bind_rows(
    seed_blocks_burst(dat, p, min_isi_sec = min_isi_sec),
    context_window_proposals(dat, p, min_isi_sec = min_isi_sec)
  )
  if (nrow(props) == 0) {
    return(data.frame(start_isi = integer(0), end_isi = integer(0), class = character(0), score = numeric(0)))
  }
  props <- props %>% distinct(start_isi, end_isi, .keep_all = TRUE)
  
  max_c <- safe_int(p$max_candidates_per_train %||% 800L, 800L)
  
  # Cheap pre-filter to protect Shiny responsiveness. Seed blocks receive Inf;
  # context proposals receive pre_score during proposal generation. This avoids
  # running the full boundary-optimized scorer on thousands of low-priority windows.
  if (!("pre_score" %in% names(props))) props$pre_score <- 0
  props$pre_score <- suppressWarnings(as.numeric(props$pre_score))
  seed_idx <- props$source == "seed"
  props$pre_score[!is.finite(props$pre_score) & !seed_idx] <- 0
  props$pre_score[seed_idx] <- Inf
  props <- props %>% arrange(desc(pre_score), start_isi, end_isi)
  if (nrow(props) > max_c) props <- props %>% head(max_c)
  
  use_opt <- isTRUE(p$use_boundary_optimization)
  max_opt <- safe_int(p$max_optimize_candidates_per_train %||% 200L, 200L)
  max_opt <- if (use_opt) min(max_opt, nrow(props)) else 0L
  
  cand_parts <- list()
  
  if (max_opt > 0L) {
    cand_parts <- c(cand_parts, lapply(seq_len(max_opt), function(i) {
      s <- props$start_isi[i]
      e <- props$end_isi[i]
      m <- optimize_burst_boundary(dat, s, e, p, min_isi_sec = min_isi_sec)
      if (is.null(m)) return(NULL)
      m$source <- props$source[i]
      m
    }))
  }
  
  if (nrow(props) > max_opt) {
    rest_idx <- (max_opt + 1L):nrow(props)
    cand_parts <- c(cand_parts, lapply(rest_idx, function(i) {
      s <- props$start_isi[i]
      e <- props$end_isi[i]
      m <- candidate_metrics(dat, s, e, p, min_isi_sec = min_isi_sec)
      if (is.null(m)) return(NULL)
      m$source <- props$source[i]
      m
    }))
  }
  
  cand <- bind_rows(cand_parts)
  if (nrow(cand) == 0) {
    return(data.frame(start_isi = integer(0), end_isi = integer(0), class = character(0), score = numeric(0)))
  }
  
  cand <- cand %>%
    group_by(start_isi, end_isi, class) %>%
    arrange(desc(score), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  
  cand <- apply_refractory_suspect_policy_burst_candidates(cand, dat, p, min_isi_sec = min_isi_sec)
  cand <- nms_burst_candidates(cand, n, allow_possible = TRUE)
  cand <- post_merge_burst_candidates(cand, dat, p, min_isi_sec = min_isi_sec)
  cand <- apply_refractory_suspect_policy_burst_candidates(cand, dat, p, min_isi_sec = min_isi_sec)
  nms_burst_candidates(cand, n, allow_possible = TRUE)
}

