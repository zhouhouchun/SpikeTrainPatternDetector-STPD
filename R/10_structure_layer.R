# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# structure structure-candidate layer and overrides
# ============================================================

empty_structure_candidates_tbl <- function() {
  tibble(
    structure_id = integer(), train = character(), structure_class = character(),
    start_isi = integer(), end_isi = integer(), start_spike_idx = integer(), end_spike_idx = integer(),
    n_isi = integer(), n_spikes = integer(), start_time_sec = numeric(), end_time_sec = numeric(), duration_sec = numeric(),
    pre_ISI_sec = numeric(), post_ISI_sec = numeric(),
    pre_ISI_pct = numeric(), post_ISI_pct = numeric(),
    core_min_ISI_sec = numeric(), core_median_ISI_sec = numeric(), core_q_ISI_sec = numeric(), core_max_ISI_sec = numeric(), core_mean_ISI_sec = numeric(),
    core_min_ISI_pct = numeric(), core_median_ISI_pct = numeric(), core_q_ISI_pct = numeric(), core_max_ISI_pct = numeric(),
    core_values_sec = character(), core_values_pct = character(),
    pre_ratio_q = numeric(), post_ratio_q = numeric(), edge_contrast_min_q = numeric(), edge_contrast_geom_q = numeric(),
    pre_ratio_max = numeric(), post_ratio_max = numeric(), edge_contrast_min_max = numeric(), edge_contrast_geom_max = numeric(),
    MM = numeric(), LV = numeric(), CV = numeric(), structure_score = numeric(),
    manual_anchor_active = logical(), manual_anchor_soft_support = logical(),
    manual_anchor_score = numeric(), manual_anchor_closeness = numeric(),
    manual_anchor_distance_log = numeric(), manual_anchor_center_sec = numeric(),
    manual_anchor_spread_log = numeric(), manual_anchor_confidence = numeric(),
    manual_anchor_n = integer(), manual_anchor_source = character(),
    train_range_support = logical(), train_range_hard_applied = logical(),
    tonic_like = logical(), manual_hint = character(), seed_decision = character(), reject_reason = character(),
    burst_sublabel = character(), burst_motif_type = character(), linked_burst_label = character(),
    linked_burst_start_isi = integer(), linked_burst_end_isi = integer(),
    linked_burst_start_time_sec = numeric(), linked_burst_end_time_sec = numeric(),
    motif_gap_isi_n = integer(), motif_gap_sec = numeric(),
    packet_to_burst_median_ratio = numeric(), packet_to_burst_q90_ratio = numeric()
  )
}

manual_hint_for_interval <- function(dat, s_isi, e_isi) {
  if (is.null(dat$pattern_manual) || s_isi > e_isi) return("")
  labs <- as.character(dat$pattern_manual[s_isi:e_isi])
  labs <- labs[labs != "" & !is.na(labs)]
  if (length(labs) == 0) return("")
  names(sort(table(labs), decreasing = TRUE))[1]
}


nms_seed_candidates <- function(seeds, p = NULL) {
  if (is.null(seeds) || nrow(seeds) <= 1) return(seeds)
  mode <- p$seed_nms_mode %||% "fractional"
  overlap_frac_cut <- suppressWarnings(as.numeric(p$seed_nms_overlap_frac %||% 0.75))
  if (!is.finite(overlap_frac_cut)) overlap_frac_cut <- 0.75
  overlap_frac_cut <- clamp(overlap_frac_cut, 0.05, 1.00)
  seeds <- seeds %>%
    mutate(priority = case_when(
      seed_source == "manual_structure" ~ 6L,
      seed_source == "structure" ~ 5L,
      seed_source == "possible_structure" ~ 4L,
      seed_source == "absolute" ~ 3L,
      seed_source == "mixed" ~ 2L,
      seed_source == "local_compression" ~ 1L,
      TRUE ~ 0L
    )) %>%
    arrange(desc(priority), desc(seed_score), start_isi, end_isi)
  if (identical(mode, "off")) {
    out <- seeds %>% arrange(start_isi, end_isi)
    out$seed_id <- seq_len(nrow(out)); out$priority <- NULL
    return(out)
  }
  keep <- rep(TRUE, nrow(seeds))
  chosen <- list()
  for (ii in seq_len(nrow(seeds))) {
    s0 <- suppressWarnings(as.integer(seeds$start_isi[ii])); e0 <- suppressWarnings(as.integer(seeds$end_isi[ii]))
    if (!is.finite(s0) || !is.finite(e0) || e0 < s0) { keep[ii] <- FALSE; next }
    suppress <- FALSE
    if (length(chosen) > 0) {
      for (rr in chosen) {
        ov_frac <- seed_interval_overlap_fraction(s0, e0, rr[1], rr[2])
        if (identical(mode, "hard")) {
          if (ov_frac > 0) { suppress <- TRUE; break }
        } else {
          if (ov_frac >= overlap_frac_cut) { suppress <- TRUE; break }
        }
      }
    }
    if (suppress) keep[ii] <- FALSE else chosen[[length(chosen) + 1L]] <- c(s0, e0)
  }
  out <- seeds[keep, , drop = FALSE] %>% arrange(start_isi, end_isi)
  out$seed_id <- seq_len(nrow(out))
  out$priority <- NULL
  out
}



structure_to_seed_windows <- function(structures, p) {
  if (is.null(structures) || nrow(structures) == 0) {
    return(tibble(start_isi = integer(), end_isi = integer(), source = character()))
  }
  ok_cls <- "structure_seed"
  if (isTRUE(p$structure_use_possible_as_seed %||% FALSE)) ok_cls <- c(ok_cls, "possible_structure")
  ss <- structures %>% filter(structure_class %in% ok_cls)
  if (nrow(ss) == 0) return(tibble(start_isi = integer(), end_isi = integer(), source = character()))

  ss2 <- ss %>%
    mutate(source = ifelse(structure_class == "structure_seed", "structure", "possible_structure")) %>%
    arrange(desc(structure_score), start_isi, end_isi)
  out <- ss2 %>%
    transmute(start_isi = as.integer(start_isi), end_isi = as.integer(end_isi), source = source)

  # refined: by default, do not run NMS inside this source-specific
  # conversion step. structure_to_seed_windows() now only proposes structure-derived
  # seed windows. mine_burst_seeds() performs a single NMS pass after all seed
  # sources have been merged and structure-primary source filtering has been applied.
  if (!isTRUE(p$structure_seed_pre_nms %||% FALSE)) {
    return(out %>% distinct(start_isi, end_isi, source, .keep_all = TRUE))
  }

  # Optional legacy pre-NMS, retained for users who explicitly enable it in params.
  pseudo <- ss2 %>%
    transmute(start_isi = as.integer(start_isi), end_isi = as.integer(end_isi),
              seed_source = source, seed_score = suppressWarnings(as.numeric(structure_score)))
  pseudo <- nms_seed_candidates(pseudo, p = p)
  pseudo %>% transmute(start_isi = as.integer(start_isi), end_isi = as.integer(end_isi), source = seed_source)
}


seed_bridge_diagnostics_train <- function(dat, p, min_isi_sec = 0.001, train = "") {
  seeds <- mine_burst_seeds(dat, p, min_isi_sec = min_isi_sec, train = train)
  structures <- attr(seeds, "structure_candidates")
  if (is.null(structures)) {
    structures <- if (isTRUE(p$use_structure_candidates %||% TRUE)) mine_structure_candidates(dat, p, min_isi_sec = min_isi_sec, train = train) else empty_structure_candidates_tbl()
  }
  bridges <- mine_seed_bridges(dat, seeds, p, min_isi_sec = min_isi_sec, train = train)
  candidates <- build_burst_candidates(dat, seeds, bridges, p, min_isi_sec = min_isi_sec, train = train)
  list(structures = structures, seeds = seeds, bridges = bridges, candidates = candidates)
}


empty_local_compression_candidates <- function() {
  tibble(start_isi = integer(), end_isi = integer(), class = character(), score = numeric(), source = character())
}

mine_local_compression_burst_candidates <- function(dat, p, min_isi_sec = 0.001, train = "", existing = NULL) {
  if (!isTRUE(p$local_compression_burst_mode %||% FALSE)) return(empty_local_compression_candidates())
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = FALSE)
  dat <- ensure_train_local_median_cache(dat, window = p$local_window %||% 11L, min_isi_sec = min_isi_sec, force = FALSE)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  n <- length(isi)
  if (n < 4) return(empty_local_compression_candidates())
  valid <- is.finite(isi) & !is_artifact_isi(isi, min_isi_sec)
  valid[1] <- FALSE
  pct <- suppressWarnings(as.numeric(dat$ISI_pct %||% rep(NA_real_, n)))
  w_min <- max(2L, safe_int(p$structure_core_min_isi_n %||% p$seed_min_isi_n %||% 2L, 2L))
  w_max <- max(w_min, safe_int(min(p$structure_core_max_isi_n %||% p$seed_max_isi_n %||% 8L, 8L), 8L))
  core_pct_max <- suppressWarnings(as.numeric(p$local_compression_core_pct_max %||% 30))
  local_ratio_min <- suppressWarnings(as.numeric(p$local_compression_local_ratio_min %||% 2.20))
  flank_min <- suppressWarnings(as.numeric(p$local_compression_flank_ratio_min %||% p$local_compression_edge_min %||% 1.80))
  flank_geom_min <- suppressWarnings(as.numeric(p$local_compression_flank_geom_min %||% p$local_compression_edge_geom %||% 2.50))
  cv_max <- suppressWarnings(as.numeric(p$local_compression_core_cv_max %||% 1.10))
  cand_class <- as.character(p$local_compression_candidate_class %||% p$local_compression_burst_label %||% "possible_burst")
  if (!cand_class %in% c("burst", "possible_burst")) cand_class <- "possible_burst"
  max_rows <- safe_int(p$local_compression_max_candidates %||% 300L, 300L)
  rows <- list()
  existing_ranges <- NULL
  if (!is.null(existing) && nrow(existing) > 0 && all(c("start_isi", "end_isi") %in% names(existing))) {
    existing_ranges <- as.data.frame(existing[, c("start_isi", "end_isi")])
  }
  overlaps_existing <- function(s0, e0) {
    if (is.null(existing_ranges) || nrow(existing_ranges) == 0) return(FALSE)
    any(!(e0 < existing_ranges$start_isi | s0 > existing_ranges$end_isi))
  }
  qprob <- clamp(p$contrast_q %||% 0.90, 0.50, 1.00)
  for (w in w_min:w_max) {
    max_s <- n - w
    if (max_s < 2) next
    for (s_isi in 2:max_s) {
      e_isi <- s_isi + w - 1L
      if (!valid_isi_interval(s_isi, e_isi, n, require_flanks = TRUE)) next
      if (overlaps_existing(s_isi, e_isi)) next
      idx <- s_isi:e_isi
      if (!all(valid[idx])) next
      vals <- isi[idx]
      if (length(vals) < w_min) next
      core_q <- as.numeric(stats::quantile(vals, qprob, na.rm = TRUE, names = FALSE))
      if (!is.finite(core_q) || core_q <= 0) next
      core_pct <- suppressWarnings(as.numeric(pct[idx]))
      core_q_pct <- isi_percentile_scalar(core_q, isi, min_isi_sec = min_isi_sec)
      core_med_pct <- if (any(is.finite(core_pct))) stats::median(core_pct, na.rm = TRUE) else core_q_pct
      if (is.finite(core_q_pct) && core_q_pct > core_pct_max) next
      if (is.finite(core_med_pct) && core_med_pct > min(100, core_pct_max + 10)) next
      edge <- calc_edge_contrast_stats(isi, s_isi, e_isi, min_isi_sec, qprob)
      if (!is.finite(edge$pre) || !is.finite(edge$post)) next
      pre_ratio <- edge$pre / core_q
      post_ratio <- edge$post / core_q
      flank_geom <- sqrt(max(pre_ratio, 0) * max(post_ratio, 0))
      if (!is.finite(pre_ratio) || !is.finite(post_ratio) || !is.finite(flank_geom)) next
      if (min(pre_ratio, post_ratio) < flank_min || flank_geom < flank_geom_min) next
      mid <- floor((s_isi + e_isi) / 2)
      loc_med <- NA_real_
      if ("local_median_ISI_sec" %in% names(dat)) loc_med <- suppressWarnings(as.numeric(dat$local_median_ISI_sec[mid]))
      if (!is.finite(loc_med)) loc_med <- get_local_median(isi, mid, min_isi_sec = min_isi_sec)
      local_ratio <- loc_med / core_q
      if (!is.finite(local_ratio) || local_ratio < local_ratio_min) next
      cv <- calc_CV(vals); lv <- calc_LV(vals); mm <- max(vals) / mean(vals)
      if (is.finite(cv) && cv > cv_max) next
      n_spk <- e_isi - s_isi + 2L
      if (n_spk < (p$G_min %||% 3L)) next
      score <- log(max(local_ratio, 1e-9)) + 0.25 * log(max(flank_geom, 1e-9)) + 0.05 * log(max(1L, n_spk))
      rows[[length(rows) + 1L]] <- tibble(start_isi = as.integer(s_isi), end_isi = as.integer(e_isi), class = cand_class, score = score, source = "local_compression_burst")
      if (length(rows) >= max_rows) break
    }
    if (length(rows) >= max_rows) break
  }
  if (length(rows) == 0) return(empty_local_compression_candidates())
  out <- bind_rows(rows) %>% arrange(desc(score), start_isi, end_isi)
  keep <- rep(TRUE, nrow(out)); chosen <- list()
  for (ii in seq_len(nrow(out))) {
    s0 <- out$start_isi[ii]; e0 <- out$end_isi[ii]
    suppress <- FALSE
    if (length(chosen) > 0) {
      for (rr in chosen) {
        if (seed_interval_overlap_fraction(s0, e0, rr[1], rr[2]) >= 0.50) { suppress <- TRUE; break }
      }
    }
    if (suppress) keep[ii] <- FALSE else chosen[[length(chosen) + 1L]] <- c(s0, e0)
  }
  out[keep, , drop = FALSE] %>% arrange(start_isi, end_isi)
}

empty_boundary_burst_candidates <- function() {
  tibble(start_isi = integer(), end_isi = integer(), class = character(), score = numeric(), source = character())
}

mine_boundary_burst_candidates <- function(dat, p, min_isi_sec = 0.001, train = "", existing = NULL) {
  if (!isTRUE(p$boundary_burst_mode %||% FALSE)) return(empty_boundary_burst_candidates())
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = FALSE)
  dat <- ensure_train_local_median_cache(dat, window = p$local_window %||% 11L, min_isi_sec = min_isi_sec, force = FALSE)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
  n <- length(isi)
  if (n < 4) return(empty_boundary_burst_candidates())
  valid <- is.finite(isi) & !is_artifact_isi(isi, min_isi_sec); valid[1] <- FALSE
  pct <- suppressWarnings(as.numeric(dat$ISI_pct %||% rep(NA_real_, n)))
  qprob <- clamp(p$contrast_q %||% 0.90, 0.50, 1.00)
  w_min <- max(2L, safe_int(p$structure_core_min_isi_n %||% p$seed_min_isi_n %||% 2L, 2L))
  w_max <- max(w_min, safe_int(min(p$structure_core_max_isi_n %||% p$seed_max_isi_n %||% 8L, 8L), 8L))
  core_pct_max <- suppressWarnings(as.numeric(p$boundary_core_pct_max %||% p$local_compression_core_pct_max %||% 30))
  local_ratio_min <- suppressWarnings(as.numeric(p$boundary_local_ratio_min %||% p$local_compression_local_ratio_min %||% 2.20))
  one_flank_min <- suppressWarnings(as.numeric(p$boundary_one_flank_ratio_min %||% 2.50))
  max_spk <- safe_int(p$boundary_max_n_spikes %||% p$local_compression_max_n_spikes %||% 8L, 8L)
  max_dur <- suppressWarnings(as.numeric(p$boundary_max_duration %||% p$local_compression_max_duration %||% 0))
  cand_class <- as.character(p$boundary_burst_label %||% "possible_burst")
  if (!cand_class %in% c("burst", "possible_burst")) cand_class <- "possible_burst"
  existing_ranges <- NULL
  if (!is.null(existing) && nrow(existing) > 0 && all(c("start_isi", "end_isi") %in% names(existing))) {
    existing_ranges <- as.data.frame(existing[, c("start_isi", "end_isi")])
  }
  overlaps_existing <- function(s0, e0) {
    if (is.null(existing_ranges) || nrow(existing_ranges) == 0) return(FALSE)
    any(!(e0 < existing_ranges$start_isi | s0 > existing_ranges$end_isi))
  }
  rows <- list()
  add_candidate <- function(s_isi, e_isi, side) {
    if (!valid_isi_interval(s_isi, e_isi, n, require_flanks = FALSE)) return(NULL)
    if (overlaps_existing(s_isi, e_isi)) return(NULL)
    idx <- s_isi:e_isi
    if (!all(valid[idx])) return(NULL)
    vals <- isi[idx]
    if (length(vals) < w_min) return(NULL)
    core_q <- as.numeric(stats::quantile(vals, qprob, na.rm = TRUE, names = FALSE))
    if (!is.finite(core_q) || core_q <= 0) return(NULL)
    core_q_pct <- isi_percentile_from_cache(core_q, attr(dat, "isi_cache"))
    if (train_percentile_reliable(dat, p, min_isi_sec) && is.finite(core_q_pct) && core_q_pct > core_pct_max) return(NULL)
    n_spk <- e_isi - s_isi + 2L
    dur <- ts[e_isi] - ts[s_isi - 1L]
    if (n_spk < (p$G_min %||% 3L)) return(NULL)
    if (max_spk > 0L && n_spk > max_spk) return(NULL)
    if (is.finite(max_dur) && max_dur > 0 && is.finite(dur) && dur > max_dur) return(NULL)
    mid <- floor((s_isi + e_isi) / 2)
    loc_med <- NA_real_
    if ("local_median_ISI_sec" %in% names(dat)) loc_med <- suppressWarnings(as.numeric(dat$local_median_ISI_sec[mid]))
    if (!is.finite(loc_med)) loc_med <- get_local_median(isi, mid, min_isi_sec = min_isi_sec)
    local_ratio <- if (is.finite(loc_med) && core_q > 0) loc_med / core_q else NA_real_
    if (!is.finite(local_ratio) || local_ratio < local_ratio_min) return(NULL)
    pre <- if (s_isi > 2L && is.finite(isi[s_isi - 1L]) && !is_artifact_isi(isi[s_isi - 1L], min_isi_sec)) isi[s_isi - 1L] else NA_real_
    post <- if (e_isi < n && is.finite(isi[e_isi + 1L]) && !is_artifact_isi(isi[e_isi + 1L], min_isi_sec)) isi[e_isi + 1L] else NA_real_
    flank <- if (identical(side, "start")) post else pre
    flank_ratio <- if (is.finite(flank) && core_q > 0) flank / core_q else NA_real_
    if (!is.finite(flank_ratio) || flank_ratio < one_flank_min) return(NULL)
    cv <- calc_CV(vals); lv <- calc_LV(vals); mm <- max(vals) / mean(vals)
    score <- log(max(local_ratio, 1e-9)) + 0.25 * log(max(flank_ratio, 1e-9)) + 0.05 * log(max(1L, n_spk))
    tibble(start_isi = as.integer(s_isi), end_isi = as.integer(e_isi), class = cand_class,
           score = score, source = paste0("boundary_", side, "_burst"),
           core_q_sec = core_q, core_q_pct = core_q_pct, local_median_core_ratio = local_ratio,
           one_flank_ratio = flank_ratio, n_spikes = as.integer(n_spk), duration_sec = dur,
           LV = lv, CV = cv, MM = mm)
  }
  # start-boundary candidates: core begins at the first real ISI (index 2), post flank only
  for (w in w_min:w_max) {
    s0 <- 2L; e0 <- s0 + w - 1L
    if (e0 < n) {
      r <- add_candidate(s0, e0, "start")
      if (!is.null(r)) rows[[length(rows) + 1L]] <- r
    }
  }
  # end-boundary candidates: core ends at the last ISI, pre flank only
  for (w in w_min:w_max) {
    e0 <- n; s0 <- e0 - w + 1L
    if (s0 > 2L) {
      r <- add_candidate(s0, e0, "end")
      if (!is.null(r)) rows[[length(rows) + 1L]] <- r
    }
  }
  if (length(rows) == 0) return(empty_boundary_burst_candidates())
  out <- bind_rows(rows) %>% arrange(desc(score), start_isi, end_isi)
  keep <- rep(TRUE, nrow(out)); chosen <- list()
  for (ii in seq_len(nrow(out))) {
    s0 <- out$start_isi[ii]; e0 <- out$end_isi[ii]
    suppress <- FALSE
    if (length(chosen) > 0) for (rr in chosen) if (seed_interval_overlap_fraction(s0, e0, rr[1], rr[2]) >= 0.50) { suppress <- TRUE; break }
    if (suppress) keep[ii] <- FALSE else chosen[[length(chosen) + 1L]] <- c(s0, e0)
  }
  out[keep, , drop = FALSE] %>% arrange(start_isi, end_isi)
}

detect_burst_train_seed_bridge <- function(dat, p, min_isi_sec = 0.001, train = "") {
  diag <- seed_bridge_diagnostics_train(dat, p, min_isi_sec = min_isi_sec, train = train)
  cand <- diag$candidates
  diag$raw_seed_bridge_candidates <- cand

  out_parts <- list()
  # High-confidence and explicitly enabled review candidates from the seed-bridge detector.
  if (!is.null(cand) && nrow(cand) > 0) {
    core_out <- cand %>%
      filter(class %in% c("burst", "long_burst") | (class == "possible_burst" & isTRUE(p$label_possible_burst)))
    if (nrow(core_out) > 0) {
      if (!("source" %in% names(core_out))) core_out$source <- "seed_bridge"
      out_parts[[length(out_parts) + 1L]] <- core_out
    }
  }

  # Local-compression candidates are generated separately and then pass through the
  # same post-policy path as seed-bridge candidates.
  lc <- mine_local_compression_burst_candidates(dat, p, min_isi_sec = min_isi_sec, train = train,
                                                            existing = if (length(out_parts) > 0) bind_rows(out_parts) else NULL)
  diag$local_compression_candidates <- lc
  if (!is.null(lc) && nrow(lc) > 0) {
    lc_out <- lc %>% filter(class %in% c("burst", "long_burst") | (class == "possible_burst" & isTRUE(p$label_possible_burst)))
    if (nrow(lc_out) > 0) out_parts[[length(out_parts) + 1L]] <- lc_out
  }

  # Boundary one-sided candidates are reviewable by default even when generic
  # possible_burst writing is disabled. This prevents true start/end bursts from
  # being generated only in diagnostics while staying blank in AUTO output.
  bc <- mine_boundary_burst_candidates(dat, p, min_isi_sec = min_isi_sec, train = train,
                                                   existing = if (length(out_parts) > 0) bind_rows(out_parts) else NULL)
  diag$boundary_burst_candidates <- bc
  if (!is.null(bc) && nrow(bc) > 0) {
    keep_boundary_possible <- isTRUE(p$label_boundary_possible_burst %||% TRUE)
    bc_out <- bc %>% filter(class %in% c("burst", "long_burst") | (class == "possible_burst" & (isTRUE(p$label_possible_burst) || keep_boundary_possible)))
    if (nrow(bc_out) > 0) out_parts[[length(out_parts) + 1L]] <- bc_out
  }

  raw_all <- if (length(out_parts) > 0) bind_rows(out_parts) %>% arrange(start_isi, end_isi) else empty_burst_candidates_tbl()
  if (!is.null(raw_all) && nrow(raw_all) > 0 && !("raw_class" %in% names(raw_all))) raw_all$raw_class <- raw_all$class
  diag$raw_burst_candidates <- raw_all
  diag$raw_candidates <- raw_all

  final_all <- raw_all
  if (!is.null(final_all) && nrow(final_all) > 0) {
    final_all <- apply_refractory_suspect_policy_burst_candidates(final_all, dat, p, min_isi_sec = min_isi_sec)
    final_all <- bind_rows(lapply(seq_len(nrow(final_all)), function(ii) {
      reclassify_burst_family_candidate(final_all[ii, , drop = FALSE], dat, p, min_isi_sec = min_isi_sec)
    }))
    final_all <- nms_burst_candidates(final_all, nrow(dat), allow_possible = TRUE)
    final_all <- post_merge_burst_candidates(final_all, dat, p, min_isi_sec = min_isi_sec)
    final_all <- apply_refractory_suspect_policy_burst_candidates(final_all, dat, p, min_isi_sec = min_isi_sec)
    final_all <- bind_rows(lapply(seq_len(nrow(final_all)), function(ii) {
      reclassify_burst_family_candidate(final_all[ii, , drop = FALSE], dat, p, min_isi_sec = min_isi_sec)
    }))
    final_all <- nms_burst_candidates(final_all, nrow(dat), allow_possible = TRUE)
  }
  diag$final_burst_candidates <- final_all
  diag$final_candidates <- final_all
  attr(final_all, "seed_bridge_diag") <- diag
  final_all
}

near_miss_from_structures <- function(structures, p) {
  if (is.null(structures) || nrow(structures) == 0) return(empty_near_miss_tbl())
  rows <- list()
  for (ii in seq_len(nrow(structures))) {
    st <- structures[ii, , drop = FALSE]
    if (st$structure_class == "structure_seed") next
    cand_ref <- paste0("structure:", st$structure_id)
    fail_count <- 0L
    if (!is.finite(st$edge_contrast_min_q) || st$edge_contrast_min_q < (p$structure_edge_min %||% 1.25)) fail_count <- fail_count + 1L
    if (!is.finite(st$edge_contrast_geom_q) || st$edge_contrast_geom_q < (p$structure_edge_geom_min %||% 1.35)) fail_count <- fail_count + 1L
    qmax <- p$structure_core_q_max %||% p$seed_q_max %||% 0
    if (is.finite(qmax) && qmax > 0 && is.finite(st$core_q_ISI_sec) && st$core_q_ISI_sec > qmax * (p$structure_core_q_loosen %||% 1.25)) fail_count <- fail_count + 1L
    if (fail_count == 0) fail_count <- 1L
    
    if (is.finite(st$edge_contrast_min_q) && st$edge_contrast_min_q < (p$structure_edge_min %||% 1.25)) {
      rows[[length(rows)+1L]] <- relax_row("burst", "structure", st$train, st$start_isi, st$end_isi, st$start_time_sec, st$end_time_sec,
                                              "burst_structure_edge_min", "decrease", p$structure_edge_min %||% 1.25, st$edge_contrast_min_q,
                                              score = st$structure_score, metric_value = st$edge_contrast_min_q, failure_count = fail_count,
                                              candidate_ref = cand_ref, reason = st$reject_reason,
                                              details = paste0("core_q=", signif(st$core_q_ISI_sec, 4), "; pre=", signif(st$pre_ISI_sec, 4), "; post=", signif(st$post_ISI_sec, 4)))
    }
    if (is.finite(st$edge_contrast_geom_q) && st$edge_contrast_geom_q < (p$structure_edge_geom_min %||% 1.35)) {
      rows[[length(rows)+1L]] <- relax_row("burst", "structure", st$train, st$start_isi, st$end_isi, st$start_time_sec, st$end_time_sec,
                                              "burst_structure_edge_geom", "decrease", p$structure_edge_geom_min %||% 1.35, st$edge_contrast_geom_q,
                                              score = st$structure_score, metric_value = st$edge_contrast_geom_q, failure_count = fail_count,
                                              candidate_ref = cand_ref, reason = st$reject_reason,
                                              details = paste0("core_q=", signif(st$core_q_ISI_sec, 4), "; LV=", signif(st$LV, 4), "; MM=", signif(st$MM, 4)))
    }
    if (is.finite(qmax) && qmax > 0 && is.finite(st$core_q_ISI_sec) && st$core_q_ISI_sec > qmax * (p$structure_core_q_loosen %||% 1.25)) {
      rows[[length(rows)+1L]] <- relax_row("burst", "structure", st$train, st$start_isi, st$end_isi, st$start_time_sec, st$end_time_sec,
                                              "burst_structure_core_q_max", "increase", qmax, st$core_q_ISI_sec / max(1, p$structure_core_q_loosen %||% 1.25),
                                              score = st$structure_score, metric_value = st$core_q_ISI_sec, failure_count = fail_count,
                                              candidate_ref = cand_ref, reason = st$reject_reason,
                                              details = paste0("core range=", signif(st$core_min_ISI_sec, 4), "-", signif(st$core_max_ISI_sec, 4)))
    }
  }
  out <- bind_rows(rows)
  if (nrow(out) == 0) return(empty_near_miss_tbl())
  out
}

# [refined] Removed duplicate earlier definition of build_near_miss_table; final definition retained below.
