# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# seed-bridge near-miss threshold preview helpers
# ============================================================

empty_near_miss_tbl <- function() {
  tibble(
    nm_id = integer(), pattern = character(), category = character(), train = character(),
    start_isi = integer(), end_isi = integer(), start_time_sec = numeric(), end_time_sec = numeric(),
    parameter = character(), direction = character(), current_value = numeric(), required_value = numeric(),
    absolute_change = numeric(), relative_change = numeric(), failure_count = integer(),
    score = numeric(), metric_value = numeric(), candidate_ref = character(), reason = character(), details = character()
  )
}

near_miss_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  if (length(y) == 0 || !is.finite(y[1])) return(default)
  y[1]
}

near_miss_relaxed_boundary <- function(value, direction = c("decrease", "increase"), margin = 1e-6) {
  direction <- match.arg(direction)
  value <- suppressWarnings(as.numeric(value))[1]
  margin <- suppressWarnings(as.numeric(margin))[1]
  if (!is.finite(value)) return(value)
  if (!is.finite(margin) || margin <= 0) margin <- 1e-6
  if (identical(direction, "increase")) value * (1 + margin) else value * (1 - margin)
}

relax_row <- function(pattern, category, train, start_isi, end_isi, start_time_sec, end_time_sec,
                         parameter, direction, current_value, required_value,
                         score = NA_real_, metric_value = NA_real_, failure_count = 1L,
                         candidate_ref = "", reason = "", details = "") {
  current_value <- suppressWarnings(as.numeric(current_value))
  required_value <- suppressWarnings(as.numeric(required_value))
  abs_change <- required_value - current_value
  rel <- if (is.finite(current_value) && current_value != 0) abs(abs_change) / abs(current_value) else abs(abs_change)
  tibble(
    nm_id = NA_integer_, pattern = pattern, category = category, train = train,
    start_isi = as.integer(start_isi), end_isi = as.integer(end_isi),
    start_time_sec = start_time_sec, end_time_sec = end_time_sec,
    parameter = parameter, direction = direction, current_value = current_value, required_value = required_value,
    absolute_change = abs_change, relative_change = rel, failure_count = as.integer(failure_count),
    score = score, metric_value = metric_value, candidate_ref = candidate_ref, reason = reason, details = details
  )
}

near_miss_burst_from_bridges <- function(bridges, p) {
  if (is.null(bridges) || nrow(bridges) == 0) return(empty_near_miss_tbl())
  rows <- list()
  max_rel <- p$near_miss_max_relax %||% 0.25
  
  for (ii in seq_len(nrow(bridges))) {
    br <- bridges[ii, , drop = FALSE]
    if (br$bridge_class == "accepted") next
    cand_ref <- paste0("bridge:", br$bridge_id)
    start_isi <- br$merged_start_isi
    end_isi <- br$merged_end_isi
    start_t <- NA_real_
    end_t <- NA_real_
    
    # Bridge ratio threshold: upper-limit parameter.
    thr_ratio <- p$bridge_ratio_max %||% 3.50
    val_ratio <- suppressWarnings(as.numeric(br$bridge_ratio_max_seed_q))
    fail_count <- 0L
    if (is.finite(val_ratio) && is.finite(thr_ratio) && val_ratio > thr_ratio) fail_count <- fail_count + 1L
    thr_raw <- p$bridge_raw_max %||% 0
    val_raw <- suppressWarnings(as.numeric(br$bridge_ISI_max_sec))
    if (is.finite(thr_raw) && thr_raw > 0 && is.finite(val_raw) && val_raw > thr_raw) fail_count <- fail_count + 1L
    thr_e <- p$bridge_merged_edge_min %||% 1.25
    val_e <- suppressWarnings(as.numeric(br$merged_edge_contrast_min_q))
    if (is.finite(val_e) && is.finite(thr_e) && val_e < thr_e) fail_count <- fail_count + 1L
    thr_g <- p$bridge_merged_edge_geom_min %||% 1.30
    val_g <- suppressWarnings(as.numeric(br$merged_edge_contrast_geom_q))
    if (is.finite(val_g) && is.finite(thr_g) && val_g < thr_g) fail_count <- fail_count + 1L
    fail_count <- max(1L, fail_count)
    
    details <- paste0(
      "bridge_ISI_max=", signif(val_raw, 4), " s; ",
      "ratio_inflated=", signif(val_ratio, 4), "; ",
      "ratio_raw=", signif(br$bridge_ratio_max_seed_q_raw, 4), "; ",
      "left_seed_q=", signif(br$left_seed_q_sec, 4), " s; ",
      "right_seed_q=", signif(br$right_seed_q_sec, 4), " s; ",
      "class=", br$bridge_class, "; reason=", br$bridge_reason
    )
    
    if (is.finite(val_ratio) && is.finite(thr_ratio) && val_ratio > thr_ratio) {
      r <- relax_row("burst", "bridge", br$train, start_isi, end_isi, start_t, end_t,
                        "burst_bridge_ratio_max", "increase", thr_ratio, val_ratio,
                        score = br$bridge_score, metric_value = val_ratio, failure_count = fail_count,
                        candidate_ref = cand_ref, reason = "bridge ratio exceeds threshold", details = details)
      rows[[length(rows) + 1L]] <- r
    }
    
    current_inflate <- p$bridge_core_inflate %||% 1.25
    req_inflate <- suppressWarnings(as.numeric(br$required_bridge_core_inflate))
    if (is.finite(req_inflate) && is.finite(current_inflate) && req_inflate > current_inflate) {
      r <- relax_row("burst", "bridge", br$train, start_isi, end_isi, start_t, end_t,
                        "burst_bridge_core_inflate", "increase", current_inflate, req_inflate,
                        score = br$bridge_score, metric_value = req_inflate, failure_count = fail_count,
                        candidate_ref = cand_ref, reason = "equivalent seed-core inflation needed", details = details)
      rows[[length(rows) + 1L]] <- r
    }
    
    if (is.finite(thr_raw) && thr_raw > 0 && is.finite(val_raw) && val_raw > thr_raw) {
      r <- relax_row("burst", "bridge", br$train, start_isi, end_isi, start_t, end_t,
                        "burst_bridge_raw_max", "increase", thr_raw, val_raw,
                        score = br$bridge_score, metric_value = val_raw, failure_count = fail_count,
                        candidate_ref = cand_ref, reason = "raw bridge ISI exceeds threshold", details = details)
      rows[[length(rows) + 1L]] <- r
    }
    
    if (is.finite(val_e) && is.finite(thr_e) && val_e < thr_e) {
      r <- relax_row("burst", "bridge", br$train, start_isi, end_isi, start_t, end_t,
                        "burst_bridge_edge_min", "decrease", thr_e, val_e,
                        score = br$bridge_score, metric_value = val_e, failure_count = fail_count,
                        candidate_ref = cand_ref, reason = "merged bridge edge contrast min below threshold", details = details)
      rows[[length(rows) + 1L]] <- r
    }
    
    if (is.finite(val_g) && is.finite(thr_g) && val_g < thr_g) {
      r <- relax_row("burst", "bridge", br$train, start_isi, end_isi, start_t, end_t,
                        "burst_bridge_edge_geom", "decrease", thr_g, val_g,
                        score = br$bridge_score, metric_value = val_g, failure_count = fail_count,
                        candidate_ref = cand_ref, reason = "merged bridge edge contrast geom below threshold", details = details)
      rows[[length(rows) + 1L]] <- r
    }
  }
  
  if (length(rows) == 0) return(empty_near_miss_tbl())
  out <- bind_rows(rows) %>% filter(is.finite(relative_change), relative_change <= max_rel)
  if (nrow(out) == 0) return(empty_near_miss_tbl())
  out
}


near_miss_burst_from_candidates <- function(candidates, p) {
  # robust near-miss generation with explicit separation of
  # local-compression and one-sided boundary candidates.  A column existing in
  # the whole data frame is not sufficient to classify a row as boundary; the
  # row must actually carry a finite one_flank_ratio or an explicit boundary
  # source/flag. This avoids misrouting local-compression candidates whenever a
  # candidate table also contains boundary columns.
  if (is.null(candidates) || nrow(candidates) == 0) return(empty_near_miss_tbl())
  rows <- list()
  max_rel <- p$near_miss_max_relax %||% 0.25
  scalar_num <- function(x, default = NA_real_) {
    z <- suppressWarnings(as.numeric(x))
    if (length(z) == 0 || !is.finite(z[1])) return(default)
    z[1]
  }
  scalar_chr <- function(x, default = "") {
    z <- as.character(x)
    if (length(z) == 0 || is.na(z[1])) return(default)
    z[1]
  }
  scalar_logical <- function(x) {
    z <- suppressWarnings(as.logical(x))
    length(z) > 0 && isTRUE(z[1])
  }
  safe_col <- function(df, nm, default = NA_real_) {
    if (nm %in% names(df)) df[[nm]] else default
  }
  add_row <- function(...) rows[[length(rows) + 1L]] <<- relax_row(...)

  for (ii in seq_len(nrow(candidates))) {
    ca <- candidates[ii, , drop = FALSE]
    cls <- scalar_chr(safe_col(ca, "class", ""))
    accepted <- if ("accepted" %in% names(ca)) isTRUE(ca$accepted[1]) else FALSE
    if (cls %in% c("burst", "long_burst") && accepted) next

    src <- tolower(scalar_chr(safe_col(ca, "source", safe_col(ca, "candidate_source", ""))))
    start_isi <- suppressWarnings(as.integer(safe_col(ca, "start_isi", NA_integer_)[1]))
    end_isi <- suppressWarnings(as.integer(safe_col(ca, "end_isi", NA_integer_)[1]))
    start_t <- scalar_num(safe_col(ca, "start_time_sec", NA_real_))
    end_t <- scalar_num(safe_col(ca, "end_time_sec", NA_real_))
    tr <- scalar_chr(safe_col(ca, "train", ""))
    cid <- scalar_chr(safe_col(ca, "candidate_id", paste0("candidate_", ii)))
    score <- scalar_num(safe_col(ca, "score", safe_col(ca, "candidate_score", NA_real_)))
    reject_reason <- scalar_chr(safe_col(ca, "reject_reason", safe_col(ca, "rejection_reason", "")))

    edge_min <- if ("edge_contrast_min_seed_q" %in% names(ca)) scalar_num(ca$edge_contrast_min_seed_q) else scalar_num(safe_col(ca, "edge_contrast_min_q", safe_col(ca, "edge_contrast_min", NA_real_)))
    edge_geom <- if ("edge_contrast_geom_seed_q" %in% names(ca)) scalar_num(ca$edge_contrast_geom_seed_q) else scalar_num(safe_col(ca, "edge_contrast_geom_q", safe_col(ca, "edge_contrast_geom", NA_real_)))
    thr_min <- p$final_edge_contrast_min %||% p$contrast_min_high %||% 1.45
    thr_geom <- p$final_edge_contrast_geom_min %||% p$contrast_geom_high %||% 1.50
    thr_score <- p$score_high %||% 0.65

    one_flank <- scalar_num(safe_col(ca, "one_flank_ratio", NA_real_))
    local_ratio <- scalar_num(safe_col(ca, "local_median_core_ratio", NA_real_))
    core_pct <- scalar_num(safe_col(ca, "core_q_pct", safe_col(ca, "seed_core_q_pct", safe_col(ca, "q_ISI_pct", NA_real_))))

    local_like <- grepl("local_compression", src) || scalar_logical(safe_col(ca, "local_compression_burst", FALSE))
    boundary_like <- !local_like && (
      grepl("boundary", src) ||
      scalar_logical(safe_col(ca, "boundary_burst", FALSE)) ||
      is.finite(one_flank)
    )

    # Local-compression candidates are two-sided micro-burst proposals. They can
    # have local_median_core_ratio but should not be treated as one-sided
    # boundary candidates.
    if (local_like) {
      thr_local <- p$local_compression_local_ratio_min %||% 2.20
      thr_core_pct <- p$local_compression_core_pct_max %||% 30
      thr_edge_min <- p$local_compression_edge_min %||% p$local_compression_flank_ratio_min %||% 1.80
      thr_edge_geom <- p$local_compression_edge_geom %||% p$local_compression_flank_geom_min %||% 2.50
      details <- paste0("class=", cls, "; source=", src,
                        "; local/core=", signif(local_ratio, 4),
                        "; core_q_pct=", signif(core_pct, 4),
                        "; edge_min=", signif(edge_min, 4),
                        "; edge_geom=", signif(edge_geom, 4),
                        "; score=", signif(score, 4), "; reason=", reject_reason)
      fail_count <- 0L
      if (is.finite(local_ratio) && is.finite(thr_local) && local_ratio < thr_local) fail_count <- fail_count + 1L
      if (is.finite(core_pct) && is.finite(thr_core_pct) && core_pct > thr_core_pct) fail_count <- fail_count + 1L
      if (is.finite(edge_min) && is.finite(thr_edge_min) && edge_min < thr_edge_min) fail_count <- fail_count + 1L
      if (is.finite(edge_geom) && is.finite(thr_edge_geom) && edge_geom < thr_edge_geom) fail_count <- fail_count + 1L
      if (fail_count == 0L && !cls %in% c("burst", "long_burst", "possible_burst")) fail_count <- 1L
      if (is.finite(local_ratio) && is.finite(thr_local) && local_ratio < thr_local) {
        add_row("burst", "local_compression", tr, start_isi, end_isi, start_t, end_t,
                "local_compression_local_ratio_min", "decrease", thr_local, local_ratio,
                score = score, metric_value = local_ratio, failure_count = fail_count,
                candidate_ref = paste0("candidate:", cid), reason = "local-compression median/core ratio below threshold", details = details)
      }
      if (is.finite(core_pct) && is.finite(thr_core_pct) && core_pct > thr_core_pct) {
        add_row("burst", "local_compression", tr, start_isi, end_isi, start_t, end_t,
                "local_compression_core_pct_max", "increase", thr_core_pct, core_pct,
                score = score, metric_value = core_pct, failure_count = fail_count,
                candidate_ref = paste0("candidate:", cid), reason = "local-compression core q90 percentile above threshold", details = details)
      }
      if (is.finite(edge_min) && is.finite(thr_edge_min) && edge_min < thr_edge_min) {
        add_row("burst", "local_compression", tr, start_isi, end_isi, start_t, end_t,
                "local_compression_edge_min", "decrease", thr_edge_min, edge_min,
                score = score, metric_value = edge_min, failure_count = fail_count,
                candidate_ref = paste0("candidate:", cid), reason = "local-compression edge contrast min below threshold", details = details)
      }
      if (is.finite(edge_geom) && is.finite(thr_edge_geom) && edge_geom < thr_edge_geom) {
        add_row("burst", "local_compression", tr, start_isi, end_isi, start_t, end_t,
                "local_compression_edge_geom", "decrease", thr_edge_geom, edge_geom,
                score = score, metric_value = edge_geom, failure_count = fail_count,
                candidate_ref = paste0("candidate:", cid), reason = "local-compression edge contrast geom below threshold", details = details)
      }
      next
    }

    # One-sided boundary candidates use one available flank and local context.
    if (boundary_like) {
      thr_flank <- p$boundary_one_flank_ratio_min %||% 2.50
      thr_local <- p$boundary_local_ratio_min %||% 2.20
      thr_core_pct <- p$boundary_core_pct_max %||% p$local_compression_core_pct_max %||% 30
      details <- paste0("class=", cls, "; source=", src,
                        "; one_flank_ratio=", signif(one_flank, 4),
                        "; local/core=", signif(local_ratio, 4),
                        "; core_q_pct=", signif(core_pct, 4),
                        "; score=", signif(score, 4), "; reason=", reject_reason)
      fail_count <- 0L
      if (is.finite(one_flank) && is.finite(thr_flank) && one_flank < thr_flank) fail_count <- fail_count + 1L
      if (is.finite(local_ratio) && is.finite(thr_local) && local_ratio < thr_local) fail_count <- fail_count + 1L
      if (is.finite(core_pct) && is.finite(thr_core_pct) && core_pct > thr_core_pct) fail_count <- fail_count + 1L
      if (fail_count == 0L && !cls %in% c("burst", "long_burst", "possible_burst")) fail_count <- 1L
      if (is.finite(one_flank) && is.finite(thr_flank) && one_flank < thr_flank) {
        add_row("burst", "boundary", tr, start_isi, end_isi, start_t, end_t,
                "boundary_one_flank_ratio_min", "decrease", thr_flank, one_flank,
                score = score, metric_value = one_flank, failure_count = fail_count,
                candidate_ref = paste0("candidate:", cid), reason = "boundary one-sided flank/core ratio below threshold", details = details)
      }
      if (is.finite(local_ratio) && is.finite(thr_local) && local_ratio < thr_local) {
        add_row("burst", "boundary", tr, start_isi, end_isi, start_t, end_t,
                "boundary_local_ratio_min", "decrease", thr_local, local_ratio,
                score = score, metric_value = local_ratio, failure_count = fail_count,
                candidate_ref = paste0("candidate:", cid), reason = "boundary local median/core ratio below threshold", details = details)
      }
      if (is.finite(core_pct) && is.finite(thr_core_pct) && core_pct > thr_core_pct) {
        add_row("burst", "boundary", tr, start_isi, end_isi, start_t, end_t,
                "boundary_core_pct_max", "increase", thr_core_pct, core_pct,
                score = score, metric_value = core_pct, failure_count = fail_count,
                candidate_ref = paste0("candidate:", cid), reason = "boundary core q90 percentile above threshold", details = details)
      }
      next
    }

    # Two-sided candidates require finite two-sided edge metrics or score for near-miss.
    if (!is.finite(edge_min) && !is.finite(edge_geom) && !is.finite(score)) next
    fail_count <- 0L
    if (is.finite(edge_min) && is.finite(thr_min) && edge_min < thr_min) fail_count <- fail_count + 1L
    if (is.finite(edge_geom) && is.finite(thr_geom) && edge_geom < thr_geom) fail_count <- fail_count + 1L
    if (is.finite(score) && is.finite(thr_score) && score < thr_score) fail_count <- fail_count + 1L
    if (fail_count == 0L && !cls %in% c("burst", "long_burst")) fail_count <- 1L
    details <- paste0(
      "class=", cls, "; seeds=", scalar_chr(safe_col(ca, "seed_ids", "")), "; bridges=", scalar_chr(safe_col(ca, "bridge_ids", "")),
      "; seed_core_q=", signif(scalar_num(safe_col(ca, "seed_core_q_sec", NA_real_)), 4), " s; ",
      "edge_min_seed=", signif(edge_min, 4), "; edge_geom_seed=", signif(edge_geom, 4),
      "; score=", signif(score, 4), "; reason=", reject_reason
    )
    if (is.finite(edge_min) && is.finite(thr_min) && edge_min < thr_min) {
      add_row("burst", "final", tr, start_isi, end_isi, start_t, end_t,
              "burst_final_edge_min", "decrease", thr_min, edge_min,
              score = score, metric_value = edge_min, failure_count = fail_count,
              candidate_ref = paste0("candidate:", cid),
              reason = "final seed-core edge contrast min below threshold", details = details)
    }
    if (is.finite(edge_geom) && is.finite(thr_geom) && edge_geom < thr_geom) {
      add_row("burst", "final", tr, start_isi, end_isi, start_t, end_t,
              "burst_final_edge_geom", "decrease", thr_geom, edge_geom,
              score = score, metric_value = edge_geom, failure_count = fail_count,
              candidate_ref = paste0("candidate:", cid),
              reason = "final seed-core edge contrast geom below threshold", details = details)
    }
    if (is.finite(score) && is.finite(thr_score) && score < thr_score) {
      add_row("burst", "final", tr, start_isi, end_isi, start_t, end_t,
              "burst_score_high", "decrease", thr_score, score,
              score = score, metric_value = score, failure_count = fail_count,
              candidate_ref = paste0("candidate:", cid),
              reason = "final score below high-confidence cutoff", details = details)
    }
    dur <- scalar_num(safe_col(ca, "duration_sec", NA_real_))
    if (is.finite(p$final_max_duration %||% 0) && (p$final_max_duration %||% 0) > 0 && is.finite(dur) && dur > (p$final_max_duration %||% Inf)) {
      add_row("burst", "final", tr, start_isi, end_isi, start_t, end_t,
              "burst_final_duration_max", "increase", p$final_max_duration, dur,
              score = score, metric_value = dur, failure_count = fail_count,
              candidate_ref = paste0("candidate:", cid),
              reason = "final duration exceeds threshold", details = details)
    }
  }
  if (length(rows) == 0) return(empty_near_miss_tbl())
  out <- bind_rows(rows) %>% filter(is.finite(relative_change), relative_change <= max_rel)
  if (nrow(out) == 0) return(empty_near_miss_tbl())
  out
}

mine_tonic_near_miss_train <- function(dat, p_tonic, min_isi_sec = 0.001, train = "", max_relax = 0.25, max_rows = 200L) {
  n <- nrow(dat)
  if (n <= 4) return(empty_near_miss_tbl())
  p_tonic <- p_tonic %||% list()
  isi <- dat$ISI_sec
  final <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, isi, auto_others = FALSE, min_isi_sec = min_isi_sec)
  valid <- is.finite(isi) & !is_artifact_isi(isi, min_isi_sec)
  valid[1] <- FALSE
  
  len_min <- max(2L, safe_int(p_tonic$G_min %||% 5L, 5L) - 1L)
  len_max <- max(len_min, min(12L, len_min + 4L))
  t_min <- near_miss_num(p_tonic$T_min, 0.020)
  t_max <- near_miss_num(p_tonic$T_max, 0.060)
  lv_core <- near_miss_num(p_tonic$LV_core, 0.50)
  seed_ratio <- near_miss_num(p_tonic$seed_ratio, 1.20)
  mm_max <- near_miss_num(p_tonic$tonic_mm_max, 1.25)
  mm_min <- near_miss_num(p_tonic$tonic_mm_min, 0.85)
  rows <- list()
  for (L in len_min:len_max) {
    if (n < L + 1L) next
    for (s in 2:(n - L + 1L)) {
      e <- s + L - 1L
      if (!all(valid[s:e])) next
      if (any(final[s:e] != "", na.rm = TRUE)) next
      vals <- isi[s:e]
      m <- mean(vals); lv <- calc_LV(vals)
      ratio <- max(vals) / min(vals)
      mm <- max(vals) / m
      mmn <- min(vals) / m
      
      fail_rows <- list()
      fail_count <- 0L
      if (is.finite(m) && is.finite(t_min) && m < t_min) fail_count <- fail_count + 1L
      if (is.finite(m) && is.finite(t_max) && t_max > 0 && m > t_max) fail_count <- fail_count + 1L
      if (is.finite(lv) && is.finite(lv_core) && lv > lv_core) fail_count <- fail_count + 1L
      if (is.finite(ratio) && is.finite(seed_ratio) && ratio > seed_ratio) fail_count <- fail_count + 1L
      if (is.finite(mm) && is.finite(mm_max) && mm > mm_max) fail_count <- fail_count + 1L
      if (is.finite(mmn) && is.finite(mm_min) && mmn < mm_min) fail_count <- fail_count + 1L
      if (fail_count == 0L || fail_count > 2L) next
      
      details <- paste0("mean=", signif(m,4), " s; LV=", signif(lv,4), "; seed_ratio=", signif(ratio,4),
                        "; max/mean=", signif(mm,4), "; min/mean=", signif(mmn,4))
      
      if (is.finite(m) && is.finite(t_min) && m < t_min) fail_rows[[length(fail_rows)+1L]] <- relax_row("tonic","tonic_window",train,s,e,dat$timestamp_sec[s-1L],dat$timestamp_sec[e],"tonic_T_min","decrease",t_min,m,score=-lv,metric_value=m,failure_count=fail_count,candidate_ref=paste0("tonic:",s,"-",e),reason="mean ISI below tonic_T_min",details=details)
      if (is.finite(m) && is.finite(t_max) && t_max > 0 && m > t_max) fail_rows[[length(fail_rows)+1L]] <- relax_row("tonic","tonic_window",train,s,e,dat$timestamp_sec[s-1L],dat$timestamp_sec[e],"tonic_T_max","increase",t_max,m,score=-lv,metric_value=m,failure_count=fail_count,candidate_ref=paste0("tonic:",s,"-",e),reason="mean ISI above tonic_T_max",details=details)
      if (is.finite(lv) && is.finite(lv_core) && lv > lv_core) fail_rows[[length(fail_rows)+1L]] <- relax_row("tonic","tonic_window",train,s,e,dat$timestamp_sec[s-1L],dat$timestamp_sec[e],"tonic_LV_core","increase",lv_core,lv,score=-lv,metric_value=lv,failure_count=fail_count,candidate_ref=paste0("tonic:",s,"-",e),reason="LV above tonic_LV_core",details=details)
      if (is.finite(ratio) && is.finite(seed_ratio) && ratio > seed_ratio) fail_rows[[length(fail_rows)+1L]] <- relax_row("tonic","tonic_window",train,s,e,dat$timestamp_sec[s-1L],dat$timestamp_sec[e],"tonic_seed_ratio","increase",seed_ratio,ratio,score=-lv,metric_value=ratio,failure_count=fail_count,candidate_ref=paste0("tonic:",s,"-",e),reason="seed ratio above threshold",details=details)
      if (is.finite(mm) && is.finite(mm_max) && mm > mm_max) fail_rows[[length(fail_rows)+1L]] <- relax_row("tonic","tonic_window",train,s,e,dat$timestamp_sec[s-1L],dat$timestamp_sec[e],"tonic_mm_max","increase",mm_max,mm,score=-lv,metric_value=mm,failure_count=fail_count,candidate_ref=paste0("tonic:",s,"-",e),reason="max/mean above threshold",details=details)
      if (is.finite(mmn) && is.finite(mm_min) && mmn < mm_min) fail_rows[[length(fail_rows)+1L]] <- relax_row("tonic","tonic_window",train,s,e,dat$timestamp_sec[s-1L],dat$timestamp_sec[e],"tonic_mm_min","decrease",mm_min,mmn,score=-lv,metric_value=mmn,failure_count=fail_count,candidate_ref=paste0("tonic:",s,"-",e),reason="min/mean below threshold",details=details)
      
      if (length(fail_rows) > 0) {
        tmp <- bind_rows(fail_rows) %>% filter(relative_change <= max_relax)
        if (nrow(tmp) > 0) rows[[length(rows)+1L]] <- tmp
      }
      if (length(rows) >= max_rows) break
    }
    if (length(rows) >= max_rows) break
  }
  if (length(rows) == 0) return(empty_near_miss_tbl())
  out <- bind_rows(rows) %>% arrange(relative_change, failure_count) %>% head(max_rows)
  out
}

mine_pause_near_miss_train <- function(dat, p_pause, min_isi_sec = 0.001, train = "", max_relax = 0.25, max_rows = 200L) {
  n <- nrow(dat)
  if (n <= 2) return(empty_near_miss_tbl())
  p_pause <- p_pause %||% list()
  isi <- dat$ISI_sec
  final <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, isi, auto_others = FALSE, min_isi_sec = min_isi_sec)
  valid <- is.finite(isi) & !is_artifact_isi(isi, min_isi_sec)
  valid[1] <- FALSE

  active_context_labels <- c("burst", "long_burst", "possible_burst", "high_frequency_tonic", "high_frequency_spiking", "tonic")
  exclude_context_idx <- if (isTRUE(p_pause$exclude_occupied_context %||% TRUE)) which(final %in% active_context_labels) else integer(0)
  valid_global_vals <- valid_isi_values(isi, min_isi_sec)
  global_med <- safe_median(valid_global_vals, default = NA_real_)
  global_factor <- near_miss_num(p_pause$global_median_factor, 2.5)
  if (!is.finite(global_factor) || global_factor <= 0) global_factor <- 2.5
  use_global_guard <- isTRUE(p_pause$global_median_guard %||% TRUE)
  global_guard_thr <- if (isTRUE(use_global_guard) && is.finite(global_med) && global_med > 0) global_factor * global_med else NA_real_

  rr_pause <- get_train_pause_range(p_pause, train = train)
  pause_range_mode <- p_pause$adaptive_range_mode %||% "percentile_or_absolute"
  pause_range_eval <- function(value_sec) {
    if (!isTRUE(p_pause$adaptive_use_train_ranges %||% TRUE) || is.null(rr_pause)) {
      return(stpd_range_anchor_support(value_sec, rr = NULL))
    }
    value_pct <- isi_percentile_scalar(value_sec, isi, min_isi_sec = min_isi_sec)
    stpd_range_anchor_support(
      value_sec = value_sec,
      value_pct = value_pct,
      rr = rr_pause,
      mode = pause_range_mode,
      enforce_lower_sec = TRUE,
      default_low_pct = 75,
      default_high_pct = 100,
      hard_requested = isTRUE(p_pause$adaptive_train_ranges_hard %||% FALSE)
    )
  }

  alpha_thr <- near_miss_num(p_pause$alpha, 2.2)
  seed_thr <- near_miss_num(p_pause$T_seed, 0.1)
  strong_thr <- near_miss_num(p_pause$T_strong, 0.15)
  d_min <- near_miss_num(p_pause$D_min, 0)
  g_min <- safe_int(p_pause$G_min %||% 2L, 2L)
  if (!is.finite(d_min) || d_min < 0) d_min <- 0
  if (!is.finite(g_min) || g_min < 1L) g_min <- 2L

  rows <- list()
  for (i in 2:n) {
    if (!valid[i] || final[i] != "") next
    loc <- get_local_median(isi, i, exclude_idx = c(exclude_context_idx, i), min_isi_sec = min_isi_sec)
    if (!is.finite(loc)) loc <- get_local_median(isi, i, min_isi_sec = min_isi_sec)
    if (!is.finite(loc) || loc <= 0) next
    neigh <- c(if (i > 2) final[i - 1L] else "", if (i < n) final[i + 1L] else "")
    ctx_factor <- if (any(neigh %in% c("burst", "long_burst", "possible_burst", "tonic"))) {
      near_miss_num(p_pause$context_relax, 0.9)
    } else {
      near_miss_num(p_pause$context_tight, 1.1)
    }
    if (!is.finite(ctx_factor) || ctx_factor <= 0) ctx_factor <- 1
    alpha_metric <- isi[i] / loc
    seed_metric <- isi[i] / ctx_factor
    if (!is.finite(alpha_metric) || !is.finite(seed_metric)) next

    seed_abs_thr <- seed_thr * ctx_factor
    alpha_abs_thr <- alpha_thr * loc
    seed_gate_thr <- max(
      seed_abs_thr,
      alpha_abs_thr,
      if (isTRUE(use_global_guard) && is.finite(global_guard_thr)) global_guard_thr else -Inf,
      na.rm = TRUE
    )
    strong_gate_thr <- max(
      strong_thr,
      alpha_abs_thr,
      if (isTRUE(use_global_guard) && is.finite(global_guard_thr)) global_guard_thr else -Inf,
      na.rm = TRUE
    )
    baseline_seed <- is.finite(seed_gate_thr) && isi[i] >= seed_gate_thr
    range_eval <- pause_range_eval(isi[i])
    range_seed <- isTRUE(range_eval$soft_support)
    hard_range <- isTRUE(range_eval$policy$hard_allowed)
    relaxed_rel <- is.finite(alpha_metric) && alpha_metric >= max(1.05, alpha_thr * 0.80)
    relaxed_abs <- is.finite(seed_abs_thr) && isi[i] >= seed_abs_thr * 0.80
    anchor_seed <- isTRUE(range_eval$policy$is_manual_anchor) && isTRUE(range_eval$anchor$soft_support) &&
      (isTRUE(relaxed_rel) || isTRUE(relaxed_abs))
    explicit_range_seed <- isTRUE(range_seed) && !isTRUE(range_eval$policy$is_manual_anchor)
    if (isTRUE(hard_range) && !isTRUE(range_eval$range_match)) next
    needs_seed_thresholds <- !isTRUE(baseline_seed) && !isTRUE(anchor_seed) && !(isTRUE(explicit_range_seed) && !isTRUE(hard_range))

    basic_without_strong <- isTRUE((is.finite(isi[i]) && isi[i] >= d_min) || (2L >= g_min))
    strong_pass <- isTRUE(is.finite(strong_gate_thr) && isi[i] >= strong_gate_thr) ||
      isTRUE(explicit_range_seed) || (isTRUE(anchor_seed) && isTRUE(relaxed_rel))
    needs_strong_thresholds <- !isTRUE(basic_without_strong) && !isTRUE(strong_pass)

    candidate_rows <- list()
    unrelaxable <- FALSE
    details <- paste0(
      "ISI=", signif(isi[i], 4), " s; local_median=", signif(loc, 4), " s; ISI/local=", signif(alpha_metric, 4),
      "; context_factor=", signif(ctx_factor, 4), "; seed_threshold=", signif(seed_gate_thr, 4),
      " s; strong_threshold=", signif(strong_gate_thr, 4), " s; global_guard=",
      if (is.finite(global_guard_thr)) paste0(signif(global_guard_thr, 4), " s") else "off"
    )
    add_needed_row <- function(parameter, current_value, required_boundary, metric_value, reason) {
      current_value <- suppressWarnings(as.numeric(current_value))[1]
      required_value <- near_miss_relaxed_boundary(required_boundary, "decrease")
      if (!is.finite(current_value) || !is.finite(required_value) || current_value <= required_value) return(invisible(FALSE))
      rel <- if (current_value != 0) abs(required_value - current_value) / abs(current_value) else abs(required_value - current_value)
      if (!is.finite(rel) || rel > max_relax) {
        unrelaxable <<- TRUE
        return(invisible(FALSE))
      }
      r <- relax_row(
        "pause", "single_isi", train, i, i, dat$timestamp_sec[i - 1L], dat$timestamp_sec[i],
        parameter, "decrease", current_value, required_value,
        score = alpha_metric, metric_value = metric_value, failure_count = 1L,
        candidate_ref = paste0("pause:", i), reason = reason, details = details
      )
      existing <- which(vapply(candidate_rows, function(x) identical(as.character(x$parameter[1]), parameter), logical(1)))
      if (length(existing) > 0) {
        old <- candidate_rows[[existing[1L]]]
        if (required_value < old$required_value[1]) candidate_rows[[existing[1L]]] <<- r
      } else {
        candidate_rows[[length(candidate_rows) + 1L]] <<- r
      }
      invisible(TRUE)
    }

    if (isTRUE(needs_seed_thresholds)) {
      if (is.finite(seed_abs_thr) && seed_abs_thr > isi[i]) {
        add_needed_row("pause_T_seed", seed_thr, seed_metric, seed_metric, "effective ISI below pause seed threshold")
      }
      if (is.finite(alpha_abs_thr) && alpha_abs_thr > isi[i]) {
        add_needed_row("pause_alpha", alpha_thr, alpha_metric, alpha_metric, "ISI/local_median below pause alpha")
      }
      if (isTRUE(use_global_guard) && is.finite(global_guard_thr) && global_guard_thr > isi[i] && is.finite(global_med) && global_med > 0) {
        add_needed_row("pause_global_median_factor", global_factor, isi[i] / global_med, isi[i], "global median pause guard above candidate")
      }
    }

    if (isTRUE(needs_strong_thresholds)) {
      if (is.finite(strong_thr) && strong_thr > isi[i]) {
        add_needed_row("pause_T_strong", strong_thr, isi[i], isi[i], "ISI below pause strong threshold")
      }
      if (is.finite(alpha_abs_thr) && alpha_abs_thr > isi[i]) {
        add_needed_row("pause_alpha", alpha_thr, alpha_metric, alpha_metric, "ISI/local_median below pause alpha")
      }
      if (isTRUE(use_global_guard) && is.finite(global_guard_thr) && global_guard_thr > isi[i] && is.finite(global_med) && global_med > 0) {
        add_needed_row("pause_global_median_factor", global_factor, isi[i] / global_med, isi[i], "global median pause guard above candidate")
      }
    }

    if (isTRUE(unrelaxable) || length(candidate_rows) == 0) next
    tmp <- bind_rows(candidate_rows)
    tmp$failure_count <- nrow(tmp)
    rows[[length(rows) + 1L]] <- tmp
    if (length(rows) >= max_rows) break
  }
  if (length(rows) == 0) return(empty_near_miss_tbl())
  bind_rows(rows) %>% arrange(relative_change, desc(score)) %>% head(max_rows)
}

build_near_miss_table <- function(ds, params, min_isi_sec = 0.001, target_trains = NULL) {
  if (is.null(ds) || is.null(params)) return(empty_near_miss_tbl())
  target_trains <- target_trains %||% names(ds$trains)
  if (exists("stpd_seed_bridge_diagnostics_for_dataset", mode = "function")) {
    needs_seed_bridge <- nrow(ds$results$seed_candidates %||% empty_seed_candidates_tbl()) == 0 &&
      nrow(ds$results$bridge_candidates %||% empty_bridge_candidates_tbl()) == 0
    if (isTRUE(needs_seed_bridge)) {
      sb_diag <- tryCatch(
        stpd_seed_bridge_diagnostics_for_dataset(ds, params, min_isi_sec = min_isi_sec, target_trains = target_trains),
        error = function(e) NULL
      )
      if (!is.null(sb_diag)) {
        if (nrow(ds$results$structure_candidates %||% empty_structure_candidates_tbl()) == 0) ds$results$structure_candidates <- sb_diag$structures
        ds$results$seed_candidates <- sb_diag$seeds
        ds$results$bridge_candidates <- sb_diag$bridges
        if (nrow(ds$results$burst_candidates %||% empty_burst_candidates_tbl()) == 0) ds$results$burst_candidates <- sb_diag$candidates
      }
    }
  }
  max_rel <- params$burst$near_miss_max_relax %||% 0.25
  max_rows <- safe_int(params$burst$near_miss_max_rows %||% 600L, 600L)
  
  rows <- list()
  rows[[length(rows)+1L]] <- near_miss_from_structures(ds$results$structure_candidates %||% empty_structure_candidates_tbl(), params$burst)
  rows[[length(rows)+1L]] <- near_miss_burst_from_bridges(ds$results$bridge_candidates %||% empty_bridge_candidates_tbl(), params$burst)
  rows[[length(rows)+1L]] <- near_miss_burst_from_candidates(ds$results$burst_candidates %||% empty_burst_candidates_tbl(), params$burst)
  
  # Tonic/pause preview is deliberately lightweight and does not change labels.
  for (tr in intersect(target_trains, names(ds$trains))) {
    rows[[length(rows)+1L]] <- tryCatch(
      mine_tonic_near_miss_train(ds$trains[[tr]], params$tonic, min_isi_sec = min_isi_sec, train = tr, max_relax = max_rel, max_rows = min(200L, max_rows)),
      error = function(e) empty_near_miss_tbl()
    )
    rows[[length(rows)+1L]] <- tryCatch(
      mine_pause_near_miss_train(ds$trains[[tr]], params$pause, min_isi_sec = min_isi_sec, train = tr, max_relax = max_rel, max_rows = min(200L, max_rows)),
      error = function(e) empty_near_miss_tbl()
    )
  }
  
  out <- bind_rows(rows)
  if (nrow(out) == 0) return(empty_near_miss_tbl())
  out <- out %>%
    filter(is.finite(relative_change), relative_change <= max_rel) %>%
    arrange(failure_count, relative_change, desc(score), train, start_isi) %>%
    head(max_rows)
  if (nrow(out) == 0) return(empty_near_miss_tbl())
  out$nm_id <- seq_len(nrow(out))
  out
}

detect_tonic_train <- function(dat, occupied_idx, p, T_B_seed, min_isi_sec = 0.001, train = "") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec)
  isi <- dat$ISI_sec
  n <- nrow(dat)
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  rr_tonic <- get_train_tonic_range(p, train = train)
  burst_overlap_ref <- stpd_tonic_burst_overlap_reference(
    dat = dat,
    T_B_seed = T_B_seed,
    min_isi_sec = min_isi_sec,
    p = p
  )
  tonic_range_mode <- p$adaptive_range_mode %||% "percentile_or_absolute"
  tonic_range_value_eval <- function(value_sec) {
    if (!isTRUE(p$adaptive_use_train_ranges %||% TRUE) || is.null(rr_tonic)) {
      return(stpd_range_anchor_support(value_sec, rr = NULL))
    }
    value_pct <- isi_percentile_scalar(value_sec, isi, min_isi_sec = min_isi_sec)
    stpd_range_anchor_support(
      value_sec = value_sec,
      value_pct = value_pct,
      rr = rr_tonic,
      mode = tonic_range_mode,
      enforce_lower_sec = TRUE,
      default_low_pct = 0,
      default_high_pct = 100,
      hard_requested = isTRUE(p$adaptive_train_ranges_hard %||% FALSE)
    )
  }
  tonic_mean_ok <- function(vals) {
    vals <- valid_isi_values(vals, min_isi_sec)
    if (length(vals) == 0) return(FALSE)
    if (!stpd_tonic_burst_overlap_ok(vals, burst_overlap_ref, p = p, min_isi_sec = min_isi_sec)) {
      return(FALSE)
    }
    m <- mean(vals)
    abs_ok <- is.finite(m) && m >= (p$T_min %||% 0) && m <= (p$T_max %||% Inf)
    range_eval <- tonic_range_value_eval(m)
    if (isTRUE(range_eval$policy$hard_allowed)) return(abs_ok && isTRUE(range_eval$range_match))
    abs_ok || isTRUE(range_eval$soft_support)
  }
  
  seed <- rep(FALSE, n)
  if (n >= 3) {
    for (i in 2:(n - 1)) {
      if (!valid[i] || !valid[i + 1]) next
      if (occupied_idx[i] || occupied_idx[i + 1]) next
      x <- c(isi[i], isi[i + 1])
      ratio <- max(x) / min(x)
      if (ratio <= p$seed_ratio && tonic_mean_ok(x)) {
        seed[i] <- TRUE
        seed[i + 1] <- TRUE
      }
    }
  }
  
  blocks <- find_segments(ifelse(seed, "seed", ""), "seed")
  if (nrow(blocks) == 0) return(blocks)
  
  if (nrow(blocks) >= 2) {
    gap_ok_fun <- function(cur_s, cur_e, next_s, next_e) {
      between <- seq(cur_e + 1L, next_s - 1L)
      if (length(between) == 0) return(TRUE)
      connectors <- between[!art[between]]
      if (length(connectors) == 0) return(TRUE)
      kmax <- max(1L, floor(p$connector_budget_frac * sum(valid[between])))
      kmax <- min(kmax, p$connector_max_n)
      if (length(connectors) > kmax) return(FALSE)
      
      left_idx <- cur_s:cur_e
      right_idx <- next_s:next_e
      m1 <- stats::median(isi[left_idx][valid[left_idx]], na.rm = TRUE)
      m2 <- stats::median(isi[right_idx][valid[right_idx]], na.rm = TRUE)
      conn_vals <- isi[connectors]
      all_small <- all(conn_vals < min(m1, m2))
      all_large <- all(conn_vals > max(m1, m2))
      if (!(all_small || all_large)) return(FALSE)
      
      cand_idx <- cur_s:next_e
      cand_vals <- isi[cand_idx][valid[cand_idx]]
      if (length(cand_vals) < 2) return(FALSE)
      if (!tonic_mean_ok(cand_vals)) return(FALSE)
      mm <- max(cand_vals) / mean(cand_vals)
      mmn <- min(cand_vals) / mean(cand_vals)
      if (mm > p$tonic_mm_max || mmn < p$tonic_mm_min) return(FALSE)
      lv <- calc_LV(cand_vals)
      if (!is.finite(lv) || lv > p$LV_core) return(FALSE)
      
      loc_mid <- get_local_median(isi, floor(mean(c(cur_s, next_e))), min_isi_sec = min_isi_sec)
      if (is.finite(loc_mid)) {
        ratio_local <- mean(cand_vals) / loc_mid
        if (ratio_local < p$local_ratio_min || ratio_local > p$local_ratio_max) return(FALSE)
      }
      
      if (isTRUE(p$anti_burst_veto) && anti_burst_veto(cand_vals, T_B_seed)) return(FALSE)
      TRUE
    }
    blocks <- merge_blocks(blocks, gap_ok_fun)
  }
  
  for (k in seq_len(nrow(blocks))) {
    repeat {
      changed <- FALSE
      s <- blocks$start_isi[k]
      e <- blocks$end_isi[k]
      core_vals <- isi[s:e][valid[s:e]]
      if (length(core_vals) < 2) break
      lv_old <- calc_LV(core_vals)
      
      if (s > 2 && !occupied_idx[s - 1] && !art[s - 1] && valid[s - 1]) {
        cand_idx <- (s - 1):e
        cand_vals <- isi[cand_idx][valid[cand_idx]]
        if (length(cand_vals) >= 2) {
          lv_new <- calc_LV(cand_vals)
          mnew <- mean(cand_vals)
          mm <- max(cand_vals) / mnew
          mmn <- min(cand_vals) / mnew
          if (is.finite(lv_new) && lv_new <= p$LV_pre && lv_new <= lv_old * (1 + p$lv_delta) &&
              tonic_mean_ok(cand_vals) && mm <= p$tonic_mm_max && mmn >= p$tonic_mm_min) {
            blocks$start_isi[k] <- s - 1
            changed <- TRUE
          }
        }
      }
      
      s2 <- blocks$start_isi[k]
      e2 <- blocks$end_isi[k]
      core_vals <- isi[s2:e2][valid[s2:e2]]
      if (e2 < n && !occupied_idx[e2 + 1] && !art[e2 + 1] && valid[e2 + 1]) {
        cand_idx <- s2:(e2 + 1)
        cand_vals <- isi[cand_idx][valid[cand_idx]]
        lv_old2 <- calc_LV(core_vals)
        if (length(cand_vals) >= 2) {
          lv_new <- calc_LV(cand_vals)
          mnew <- mean(cand_vals)
          mm <- max(cand_vals) / mnew
          mmn <- min(cand_vals) / mnew
          if (is.finite(lv_new) && lv_new <= p$LV_post && lv_new <= lv_old2 * (1 + p$lv_delta) &&
              tonic_mean_ok(cand_vals) && mm <= p$tonic_mm_max && mmn >= p$tonic_mm_min) {
            blocks$end_isi[k] <- e2 + 1
            changed <- TRUE
          }
        }
      }
      
      if (!changed) break
    }
  }
  
  keep <- logical(nrow(blocks))
  for (k in seq_len(nrow(blocks))) {
    s <- blocks$start_isi[k]
    e <- blocks$end_isi[k]
    vals <- isi[s:e][valid[s:e]]
    if (length(vals) < 2) next
    n_spk <- e - s + 2L
    dur <- dat$timestamp_sec[e] - dat$timestamp_sec[s - 1]
    keep[k] <- (n_spk >= p$G_min) && (dur >= p$D_min) &&
      stpd_tonic_burst_overlap_ok(vals, burst_overlap_ref, p = p, min_isi_sec = min_isi_sec)
  }
  out <- blocks[keep, , drop = FALSE]
  if (nrow(out) > 0) {
    out$tonic_burst_overlap_ref_sec <- burst_overlap_ref
    out$tonic_burst_overlap_guard <- isTRUE((p %||% list())$burst_overlap_guard %||% TRUE)
  }
  out
}


detect_high_frequency_modes_train <- function(dat, occupied_idx, p, min_isi_sec = 0.001, train = "") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  pct <- suppressWarnings(as.numeric(dat$ISI_pct))
  n <- nrow(dat)
  empty <- tibble(
    start_isi = integer(), end_isi = integer(), class = character(), score = numeric(), LV = numeric(), CV = numeric(), MM = numeric(),
    manual_anchor_active = logical(), manual_anchor_soft_support = logical(),
    manual_anchor_score = numeric(), manual_anchor_closeness = numeric(),
    manual_anchor_center_sec = numeric(), manual_anchor_spread_log = numeric(),
    manual_anchor_confidence = numeric(), manual_anchor_n = integer(),
    train_range_support = logical(), train_range_hard_applied = logical()
  )
  if (n <= 2) return(empty)
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  occupied_idx <- as.logical(occupied_idx %||% rep(FALSE, n))
  if (length(occupied_idx) != n) occupied_idx <- rep(FALSE, n)

  # General high-frequency seed thresholds. For high-frequency spiking, users can
  # define a separate absolute and/or train-percentile max ISI; the seed set uses
  # the union of tonic-family and spiking-family short-ISI gates so that candidate
  # generation is not accidentally narrower than the spiking criteria.
  pct_max_base <- suppressWarnings(as.numeric(p$ISI_pct_max %||% p$pct_max %||% 35))
  if (!is.finite(pct_max_base)) pct_max_base <- 35
  pct_max_base <- clamp(pct_max_base, 1, 100)
  abs_max_base <- suppressWarnings(as.numeric(p$ISI_abs_max %||% p$T_high_max %||% 0))
  use_abs_base <- is.finite(abs_max_base) && abs_max_base > 0

  sp_use_abs <- isTRUE(p$spiking_use_abs_max %||% TRUE)
  sp_use_pct <- isTRUE(p$spiking_use_pct_max %||% TRUE)
  sp_abs_max <- suppressWarnings(as.numeric(p$spiking_max_ISI_abs %||% p$ISI_abs_max %||% p$T_high_max %||% 0))
  sp_pct_max <- suppressWarnings(as.numeric(p$spiking_max_ISI_pct %||% p$ISI_pct_max %||% p$pct_max %||% 35))
  if (!is.finite(sp_abs_max)) sp_abs_max <- 0
  if (!is.finite(sp_pct_max)) sp_pct_max <- pct_max_base
  sp_pct_max <- clamp(sp_pct_max, 1, 100)
  sp_logic <- as.character(p$spiking_gate_logic %||% "either")
  if (!sp_logic %in% c("either", "both")) sp_logic <- "either"

  rr_hf <- get_train_highfreq_range(p, train = train)
  hf_range_mode <- p$adaptive_range_mode %||% "percentile_or_absolute"
  hf_anchor_eval <- function(value_sec, value_pct = NA_real_) {
    if (!isTRUE(p$adaptive_use_train_ranges %||% FALSE) || is.null(rr_hf)) {
      return(stpd_range_anchor_support(value_sec, value_pct = value_pct, rr = NULL))
    }
    stpd_range_anchor_support(
      value_sec = value_sec,
      value_pct = value_pct,
      rr = rr_hf,
      mode = hf_range_mode,
      enforce_lower_sec = FALSE,
      default_low_pct = 0,
      default_high_pct = max(35, pct_max_base),
      hard_requested = isTRUE(p$adaptive_train_ranges_hard %||% FALSE)
    )
  }

  seed_abs_flag <- if (use_abs_base) is.finite(isi) & isi <= abs_max_base else rep(FALSE, n)
  seed_pct_flag <- is.finite(pct) & pct <= pct_max_base
  sp_abs_flag <- if (sp_use_abs && sp_abs_max > 0) is.finite(isi) & isi <= sp_abs_max else rep(FALSE, n)
  sp_pct_flag <- if (sp_use_pct) is.finite(pct) & pct <= sp_pct_max else rep(FALSE, n)
  sp_short_flag <- if (sp_use_abs && sp_use_pct && sp_logic == "both") {
    sp_abs_flag & sp_pct_flag
  } else if (sp_use_abs && sp_use_pct) {
    sp_abs_flag | sp_pct_flag
  } else if (sp_use_abs) {
    sp_abs_flag
  } else if (sp_use_pct) {
    sp_pct_flag
  } else {
    seed_abs_flag | seed_pct_flag
  }

  hf_anchor_flag <- rep(FALSE, n)
  if (isTRUE(p$adaptive_use_train_ranges %||% FALSE) && !is.null(rr_hf)) {
    for (ii in seq_len(n)) {
      if (!valid[ii] || occupied_idx[ii]) next
      ev <- hf_anchor_eval(isi[ii], if (is.finite(pct[ii])) pct[ii] else NA_real_)
      hf_anchor_flag[ii] <- isTRUE(ev$soft_support)
    }
  }

  seed <- valid & !occupied_idx & (seed_abs_flag | seed_pct_flag | sp_short_flag | hf_anchor_flag)
  seed[1] <- FALSE
  blocks <- find_segments(ifelse(seed, "hf", ""), "hf")
  if (nrow(blocks) == 0) return(empty)

  if (nrow(blocks) >= 2) {
    gap_ok_fun <- function(cur_s, cur_e, next_s, next_e) {
      between <- seq(cur_e + 1L, next_s - 1L)
      if (length(between) == 0) return(TRUE)
      connectors <- between[valid[between] & !occupied_idx[between]]
      if (length(connectors) == 0) return(TRUE)
      max_n <- safe_int(p$connector_max_n %||% 1L, 1L)
      if (length(connectors) > max_n) return(FALSE)
      margin <- suppressWarnings(as.numeric(p$connector_pct_margin %||% 10))
      if (!is.finite(margin)) margin <- 10
      all(is.finite(pct[connectors]) & pct[connectors] <= min(100, max(pct_max_base, sp_pct_max) + margin)) ||
        (isTRUE(use_abs_base) && all(is.finite(isi[connectors]) & isi[connectors] <= max(abs_max_base, sp_abs_max))) ||
        all(hf_anchor_flag[connectors], na.rm = TRUE)
    }
    blocks <- merge_blocks(blocks, gap_ok_fun)
  }

  rows <- list()
  for (k in seq_len(nrow(blocks))) {
    s0 <- suppressWarnings(as.integer(blocks$start_isi[k])); e0 <- suppressWarnings(as.integer(blocks$end_isi[k]))
    if (!valid_isi_interval(s0, e0, n, require_flanks = FALSE)) next
    idx <- s0:e0
    use_idx <- valid[idx] & !occupied_idx[idx]
    vals <- isi[idx][use_idx]
    if (length(vals) < 2) next

    hf_support_flag <- seed_abs_flag | seed_pct_flag | hf_anchor_flag
    sp_support_flag <- sp_short_flag | hf_anchor_flag
    seed_short_frac <- mean(hf_support_flag[idx][use_idx], na.rm = TRUE)
    if (!is.finite(seed_short_frac)) seed_short_frac <- 0
    sp_short_frac <- mean(sp_support_flag[idx][use_idx], na.rm = TRUE)
    if (!is.finite(sp_short_frac)) sp_short_frac <- 0
    n_spk <- e0 - s0 + 2L
    dur <- dat$timestamp_sec[e0] - dat$timestamp_sec[s0 - 1L]
    if (!is.finite(dur)) next
    if (n_spk < safe_int(p$G_min %||% ((p$min_isi_n %||% 5L) + 1L), 8L)) next
    if (dur < (p$D_min %||% 0)) next

    lv <- calc_LV(vals); cv <- calc_CV(vals); mm <- max(vals) / mean(vals)
    med_vals <- stats::median(vals, na.rm = TRUE)
    med_pct <- isi_percentile_scalar(med_vals, isi, min_isi_sec = min_isi_sec)
    anchor_eval <- hf_anchor_eval(med_vals, med_pct)
    if (isTRUE(anchor_eval$policy$hard_allowed) && !isTRUE(anchor_eval$range_match)) next

    stable <- is.finite(lv) && is.finite(cv) && is.finite(mm) &&
      lv <= (p$LV_stable_max %||% p$stable_LV_max %||% 0.35) &&
      cv <= (p$CV_stable_max %||% p$stable_CV_max %||% 0.30) &&
      mm <= (p$MM_stable_max %||% p$stable_MM_max %||% 1.25)
    irregular <- is.finite(lv) && is.finite(cv) && is.finite(mm) &&
      (lv >= (p$irregular_LV_min %||% 0.50) || cv >= (p$irregular_CV_min %||% 0.35) || mm >= (p$irregular_MM_min %||% 1.50))

    cls <- ""
    if (stable && seed_short_frac >= (p$short_fraction_min %||% 0.80)) {
      cls <- "high_frequency_tonic"
    } else if (irregular) {
      sp_min_spk <- safe_int(p$spiking_min_spikes %||% p$G_min %||% 30L, 30L)
      sp_min_dur <- suppressWarnings(as.numeric(p$spiking_min_duration %||% 0))
      sp_frac_min <- suppressWarnings(as.numeric(p$spiking_short_fraction_min %||% 0.70))
      sp_size_ok <- n_spk >= sp_min_spk && (!is.finite(sp_min_dur) || sp_min_dur <= 0 || dur >= sp_min_dur)
      if (sp_size_ok && sp_short_frac >= sp_frac_min) cls <- "high_frequency_spiking"
    }
    if (cls == "") next

    score <- 0
    if (is.finite(lv)) score <- score - lv
    if (is.finite(cv)) score <- score - cv
    if (is.finite(mm)) score <- score - max(0, mm - 1)
    if (isTRUE(anchor_eval$anchor$active) && is.finite(anchor_eval$anchor$score)) score <- score + anchor_eval$anchor$score
    rows[[length(rows) + 1L]] <- tibble(
      start_isi = as.integer(s0), end_isi = as.integer(e0), class = cls,
      score = score, LV = lv, CV = cv, MM = mm,
      n_spikes = as.integer(n_spk), duration_sec = dur,
      mean_ISI_sec = mean(vals), median_ISI_sec = median(vals),
      mean_ISI_pct = mean(pct[idx], na.rm = TRUE),
      short_fraction = if (cls == "high_frequency_spiking") sp_short_frac else seed_short_frac,
      manual_anchor_active = isTRUE(anchor_eval$anchor$active),
      manual_anchor_soft_support = isTRUE(anchor_eval$anchor$soft_support),
      manual_anchor_score = anchor_eval$anchor$score,
      manual_anchor_closeness = anchor_eval$anchor$closeness,
      manual_anchor_center_sec = anchor_eval$anchor$center_sec,
      manual_anchor_spread_log = anchor_eval$anchor$spread_log,
      manual_anchor_confidence = anchor_eval$anchor$confidence,
      manual_anchor_n = as.integer(anchor_eval$anchor$n),
      train_range_support = isTRUE(anchor_eval$soft_support),
      train_range_hard_applied = isTRUE(anchor_eval$policy$hard_allowed)
    )
  }
  if (length(rows) == 0) return(empty)
  bind_rows(rows) %>% arrange(start_isi, end_isi)
}

detect_pause_train <- function(dat, occupied_idx, p, tonic_p, min_isi_sec = 0.001, current_labels = NULL, train = "") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec)
  isi <- dat$ISI_sec
  n <- nrow(dat)
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  current_labels <- current_labels %||% rep("", n)
  current_labels <- as.character(current_labels); current_labels[is.na(current_labels)] <- ""
  rr_pause <- get_train_pause_range(p, train = train)
  pause_range_mode <- p$adaptive_range_mode %||% "percentile_or_absolute"
  pause_range_eval <- function(value_sec) {
    if (!isTRUE(p$adaptive_use_train_ranges %||% TRUE) || is.null(rr_pause)) {
      return(stpd_range_anchor_support(value_sec, rr = NULL))
    }
    value_pct <- isi_percentile_scalar(value_sec, isi, min_isi_sec = min_isi_sec)
    stpd_range_anchor_support(
      value_sec = value_sec,
      value_pct = value_pct,
      rr = rr_pause,
      mode = pause_range_mode,
      enforce_lower_sec = TRUE,
      default_low_pct = 75,
      default_high_pct = 100,
      hard_requested = isTRUE(p$adaptive_train_ranges_hard %||% FALSE)
    )
  }

  # pause local baseline excludes already identified active firing states.
  active_context_labels <- c("burst", "long_burst", "possible_burst", "high_frequency_tonic", "high_frequency_spiking", "tonic")
  exclude_context_idx <- if (isTRUE(p$exclude_occupied_context %||% TRUE)) which(current_labels %in% active_context_labels) else integer(0)
  valid_global_vals <- valid_isi_values(isi, min_isi_sec)
  global_med <- safe_median(valid_global_vals, default = NA_real_)
  global_factor <- suppressWarnings(as.numeric(p$global_median_factor %||% 2.5))
  if (!is.finite(global_factor) || global_factor <= 0) global_factor <- 2.5
  use_global_guard <- isTRUE(p$global_median_guard %||% TRUE)
  global_guard_thr <- if (isTRUE(use_global_guard) && is.finite(global_med) && global_med > 0) global_factor * global_med else NA_real_

  seed <- rep(FALSE, n)
  strong <- rep(FALSE, n)
  local_med_vec <- rep(NA_real_, n)
  eff_thr_vec <- rep(NA_real_, n)
  global_thr_vec <- rep(global_guard_thr, n)
  baseline_seed_vec <- rep(FALSE, n)
  range_seed_vec <- rep(FALSE, n)

  for (i in 2:n) {
    if (!valid[i] || occupied_idx[i]) next
    loc <- get_local_median(isi, i, exclude_idx = c(exclude_context_idx, i), min_isi_sec = min_isi_sec)
    if (!is.finite(loc)) loc <- get_local_median(isi, i, min_isi_sec = min_isi_sec)
    if (!is.finite(loc)) next
    local_med_vec[i] <- loc

    neigh <- c(if (i > 2) current_labels[i - 1] else "", if (i < n) current_labels[i + 1] else "")
    ctx_factor <- if (any(neigh %in% active_context_labels)) p$context_relax else p$context_tight

    thr_eff <- max(p$T_seed * ctx_factor, p$alpha * loc)
    if (isTRUE(use_global_guard) && is.finite(global_guard_thr)) thr_eff <- max(thr_eff, global_guard_thr)
    eff_thr_vec[i] <- thr_eff
    baseline_seed <- isi[i] >= thr_eff
    range_eval <- pause_range_eval(isi[i])
    range_seed <- isTRUE(range_eval$soft_support)
    hard_range <- isTRUE(range_eval$policy$hard_allowed)
    rel_ratio <- if (is.finite(loc) && loc > 0) isi[i] / loc else NA_real_
    relaxed_rel <- is.finite(rel_ratio) && rel_ratio >= max(1.05, (p$alpha %||% 2.2) * 0.80)
    relaxed_abs <- is.finite(p$T_seed %||% NA_real_) && isi[i] >= (p$T_seed %||% 0.100) * ctx_factor * 0.80
    anchor_seed <- isTRUE(range_eval$policy$is_manual_anchor) && isTRUE(range_eval$anchor$soft_support) &&
      (isTRUE(relaxed_rel) || isTRUE(relaxed_abs))
    explicit_range_seed <- isTRUE(range_seed) && !isTRUE(range_eval$policy$is_manual_anchor)
    baseline_seed_vec[i] <- baseline_seed
    range_seed_vec[i] <- range_seed
    if (isTRUE(hard_range)) {
      seed[i] <- baseline_seed && isTRUE(range_eval$range_match)
    } else {
      seed[i] <- baseline_seed || explicit_range_seed || anchor_seed
    }
    strong_thr <- max(p$T_strong, p$alpha * loc, if (isTRUE(use_global_guard) && is.finite(global_guard_thr)) global_guard_thr else -Inf)
    if (isi[i] >= strong_thr || explicit_range_seed || (anchor_seed && relaxed_rel)) strong[i] <- TRUE
  }

  blocks <- find_segments(ifelse(seed, "pause", ""), "pause")
  if (nrow(blocks) == 0) return(blocks)

  if (nrow(blocks) >= 2) {
    gap_ok_fun <- function(cur_s, cur_e, next_s, next_e) {
      between <- seq(cur_e + 1L, next_s - 1L)
      if (length(between) != 1) return(FALSE)
      j <- between[1]
      if (!valid[j] || occupied_idx[j]) return(FALSE)
      loc <- get_local_median(isi, j, exclude_idx = c(exclude_context_idx, j), min_isi_sec = min_isi_sec)
      if (!is.finite(loc)) loc <- get_local_median(isi, j, min_isi_sec = min_isi_sec)
      if (!is.finite(loc)) return(FALSE)
      merged_idx <- cur_s:next_e
      merged_vals <- isi[merged_idx][valid[merged_idx]]
      if (length(merged_vals) == 0) return(FALSE)
      guard_ok <- TRUE
      if (isTRUE(use_global_guard) && is.finite(global_guard_thr)) guard_ok <- isi[j] >= global_guard_thr
      isi[j] >= p$beta * loc && guard_ok && sum(merged_vals) >= p$D_min
    }
    blocks <- merge_blocks(blocks, gap_ok_fun)
  }

  keep <- logical(nrow(blocks))
  for (k in seq_len(nrow(blocks))) {
    s <- blocks$start_isi[k]
    e <- blocks$end_isi[k]
    idx <- s:e
    vals <- isi[idx][valid[idx]]
    dur <- sum(vals, na.rm = TRUE)
    n_spk <- e - s + 2L
    has_strong <- any(strong[idx], na.rm = TRUE)
    pass_basic <- has_strong || (dur >= p$D_min) || (n_spk >= p$G_min)

    veto <- FALSE
    if (isTRUE(p$anti_tonic_veto) && length(vals) >= 2) {
      veto <- anti_tonic_veto(vals, tonic_p$LV_core, tonic_p$T_min, tonic_p$T_max)
    }
    keep[k] <- pass_basic && !veto
  }
  out <- blocks[keep, , drop = FALSE]
  if (nrow(out) > 0) {
    out$pause_local_median_sec <- vapply(seq_len(nrow(out)), function(ii) safe_median(local_med_vec[out$start_isi[ii]:out$end_isi[ii]], default = NA_real_), numeric(1))
    out$pause_effective_threshold_sec <- vapply(seq_len(nrow(out)), function(ii) safe_median(eff_thr_vec[out$start_isi[ii]:out$end_isi[ii]], default = NA_real_), numeric(1))
    out$pause_global_median_sec <- global_med
    out$pause_global_guard_threshold_sec <- global_guard_thr
    out$pause_global_threshold_sec <- global_guard_thr
    out$pause_global_median_factor <- global_factor
    out$pause_excluded_context_labels <- paste(active_context_labels, collapse = ";")
    out$pause_excluded_context_n <- length(exclude_context_idx)
    out$pause_global_guard_used <- isTRUE(use_global_guard)
    out$pause_context_excluded <- isTRUE(p$exclude_occupied_context %||% TRUE)
    out$pause_alpha <- suppressWarnings(as.numeric(p$alpha %||% NA_real_))
    out$pause_context_factor <- NA_real_
  }
  out
}

bridge_close_auto_burst_gaps <- function(dat, p, min_isi_sec = 0.001) {
  # Anti-fragmentation pass for context-window proposals.
  # It fills very small unlabeled gaps between adjacent burst/possible_burst AUTO blocks
  # when the combined block still has acceptable context contrast.
  n <- nrow(dat)
  if (n <= 2 || !isTRUE(p$merge_candidate_fragments)) return(dat)
  
  pat <- as.character(dat$pattern_auto)
  pat[is.na(pat)] <- ""
  locked <- as.character(dat$pattern_manual) != ""
  max_gap <- max(0L, safe_int(p$merge_gap_max_n %||% 2L, 2L))
  candidate_labels <- c("burst", "long_burst", "possible_burst")
  
  get_candidate_segments <- function(x) {
    idx <- which(x %in% candidate_labels)
    if (length(idx) == 0) return(data.frame(start_isi = integer(0), end_isi = integer(0), class = character(0)))
    cuts <- c(1, which(diff(idx) != 1) + 1)
    starts <- idx[cuts]
    ends <- idx[c(cuts[-1] - 1, length(idx))]
    cls <- vapply(seq_along(starts), function(ii) {
      vals <- x[starts[ii]:ends[ii]]
      if (any(vals == "long_burst")) "long_burst" else if (any(vals == "burst")) "burst" else "possible_burst"
    }, character(1))
    data.frame(start_isi = starts, end_isi = ends, class = cls, stringsAsFactors = FALSE)
  }
  
  combined_passes_weak_context <- function(s_isi, e_isi) {
    if (s_isi < 2 || e_isi > n || e_isi < s_isi) return(FALSE)
    vals <- valid_isi_values(dat$ISI_sec[s_isi:e_isi], min_isi_sec)
    if (length(vals) < max(1L, (p$G_min %||% 3L) - 1L)) return(FALSE)
    bc <- calc_event_contrast_stats(
      dat$ISI_sec, s_isi, e_isi,
      min_isi_sec = min_isi_sec,
      robust_q = p$contrast_q %||% 0.90,
      context_k = p$context_k %||% 5L
    )
    cmin <- if (identical(p$contrast_ref %||% "q", "max")) bc$contrast_min_ctx_max else bc$contrast_min_ctx_q
    cgeom <- if (identical(p$contrast_ref %||% "q", "max")) bc$contrast_geom_ctx_max else bc$contrast_geom_ctx_q
    is.finite(cmin) && is.finite(cgeom) &&
      bc$n_flank_ctx >= (p$contrast_min_flanks %||% 2L) &&
      cmin >= (p$proposal_contrast_min %||% 1.20) &&
      cgeom >= (p$proposal_contrast_geom_min %||% 1.30)
  }
  
  changed_any <- TRUE
  iter <- 0L
  while (changed_any && iter < 20L) {
    iter <- iter + 1L
    changed_any <- FALSE
    seg <- get_candidate_segments(pat)
    if (nrow(seg) <= 1) break
    
    i <- 1L
    while (i < nrow(seg)) {
      gap_s <- seg$end_isi[i] + 1L
      gap_e <- seg$start_isi[i + 1L] - 1L
      gap_n <- gap_e - gap_s + 1L
      if (gap_n >= 0L && gap_n <= max_gap) {
        gap_idx <- if (gap_n == 0L) integer(0) else gap_s:gap_e
        no_manual_conflict <- length(gap_idx) == 0L || !any(locked[gap_idx])
        no_other_auto_conflict <- length(gap_idx) == 0L || all(pat[gap_idx] == "")
        merged_s <- seg$start_isi[i]
        merged_e <- seg$end_isi[i + 1L]
        
        if (no_manual_conflict && no_other_auto_conflict && combined_passes_weak_context(merged_s, merged_e)) {
          m <- candidate_metrics(dat, merged_s, merged_e, p, min_isi_sec = min_isi_sec)
          new_class <- "possible_burst"
          if (!is.null(m) && identical(as.character(m$class[1]), "burst")) {
            new_class <- "burst"
          } else if (seg$class[i] == "burst" && seg$class[i + 1L] == "burst") {
            # Keep strong fragments strong only if both sides were already high confidence.
            new_class <- "burst"
          }
          fill_idx <- merged_s:merged_e
          fill_idx <- fill_idx[!locked[fill_idx] & pat[fill_idx] %in% c("", "burst", "long_burst", "possible_burst")]
          if (length(fill_idx) > 0) {
            pat[fill_idx] <- new_class
            if ("auto_score" %in% names(dat) && !is.null(m) && is.finite(m$score[1])) {
              dat$auto_score[fill_idx] <- m$score[1]
            }
            changed_any <- TRUE
          }
        }
      }
      i <- i + 1L
    }
  }
  
  dat$pattern_auto <- pat
  dat
}





# post-overlap post-overlap event-size validation.
# Detectors are run in a priority order, so a long candidate can be fragmented
# when a higher-priority class occupies the middle. User-facing FINAL/AUTO
# labels must nevertheless obey each pattern's declared minimum size after the
# priority/overlap resolution step. This guard removes orphan fragments such as
# a two-spike high_frequency_spiking strip when hf_spiking_min_spikes = 15.
stpd_min_spikes_for_label <- function(label, params) {
  label <- as.character(label %||% "")
  if (label == "high_frequency_spiking") {
    return(safe_int(params$highfreq$spiking_min_spikes %||% params$highfreq$G_min %||% 30L, 30L))
  }
  if (label == "high_frequency_tonic") {
    return(safe_int(params$highfreq$G_min %||% ((params$highfreq$min_isi_n %||% 5L) + 1L), 6L))
  }
  if (label == "tonic") {
    return(safe_int(params$tonic$G_min %||% 5L, 5L))
  }
  if (label == "pause") {
    return(safe_int(params$pause$G_min %||% 2L, 2L))
  }
  if (label == "long_burst") {
    return(safe_int(params$burst$long_burst_min_spikes %||% params$burst$G_min %||% 11L, 11L))
  }
  if (label %in% c("burst", "possible_burst")) {
    return(safe_int(params$burst$G_min %||% 3L, 3L))
  }
  1L
}

stpd_min_duration_for_label <- function(label, params) {
  label <- as.character(label %||% "")
  if (label == "high_frequency_spiking") return(suppressWarnings(as.numeric(params$highfreq$spiking_min_duration %||% 0)))
  if (label == "high_frequency_tonic") return(suppressWarnings(as.numeric(params$highfreq$D_min %||% 0)))
  if (label == "tonic") return(suppressWarnings(as.numeric(params$tonic$D_min %||% 0)))
  if (label == "pause") return(suppressWarnings(as.numeric(params$pause$D_min %||% 0)))
  if (label == "long_burst") return(suppressWarnings(as.numeric(params$burst$long_burst_min_duration %||% params$burst$D_min %||% 0)))
  if (label %in% c("burst", "possible_burst")) return(suppressWarnings(as.numeric(params$burst$D_min %||% 0)))
  0
}

# event arbitration optional per-pattern ISI gates. These are final event-level guards.
# Values of 0 disable the corresponding Min_ISI or Max_ISI gate.
stpd_pattern_isi_limits_for_label <- function(label, params) {
  label <- as.character(label %||% "")
  lims <- params$detector$pattern_isi_limits %||% list()
  lookup_label <- if (label == "possible_burst") "burst" else label
  lim <- lims[[lookup_label]] %||% list(min_sec = 0, max_sec = 0)
  min_s <- suppressWarnings(as.numeric(lim$min_sec %||% 0))
  max_s <- suppressWarnings(as.numeric(lim$max_sec %||% 0))
  if (!is.finite(min_s) || min_s < 0) min_s <- 0
  if (!is.finite(max_s) || max_s < 0) max_s <- 0
  list(min_sec = min_s, max_sec = max_s)
}

stpd_pattern_isi_gate_pass <- function(vals, label, params, min_isi_sec = 0.001) {
  lim <- stpd_pattern_isi_limits_for_label(label, params)
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[is.finite(vals) & vals >= min_isi_sec]
  min_active <- is.finite(lim$min_sec) && lim$min_sec > 0
  max_active <- is.finite(lim$max_sec) && lim$max_sec > 0
  if (!min_active && !max_active) {
    return(list(pass = TRUE, min_sec = lim$min_sec, max_sec = lim$max_sec, reason = "pattern_isi_gate_disabled"))
  }
  if (length(vals) == 0) {
    return(list(pass = FALSE, min_sec = lim$min_sec, max_sec = lim$max_sec, reason = "no_valid_isi_for_pattern_isi_gate"))
  }
  min_pass <- !min_active || all(vals >= lim$min_sec, na.rm = TRUE)
  max_pass <- !max_active || all(vals <= lim$max_sec, na.rm = TRUE)
  reason <- paste(c(if (!min_pass) "below_pattern_Min_ISI", if (!max_pass) "above_pattern_Max_ISI"), collapse = ";")
  if (!nzchar(reason)) reason <- "pattern_isi_gate_pass"
  list(pass = isTRUE(min_pass && max_pass), min_sec = lim$min_sec, max_sec = lim$max_sec, reason = reason)
}

stpd_post_validate_auto_event_sizes <- function(dat, params, min_isi_sec = 0.001, lock_manual = TRUE, train = "") {
  if (is.null(dat) || nrow(dat) == 0 || is.null(dat$pattern_auto)) return(dat)
  pat <- as.character(dat$pattern_auto)
  pat[is.na(pat)] <- ""
  if (length(pat) != nrow(dat)) pat <- rep("", nrow(dat))
  if (length(pat) > 0) pat[1] <- ""

  locked <- rep(FALSE, nrow(dat))
  if (isTRUE(lock_manual) && !is.null(dat$pattern_manual)) {
    locked <- as.character(dat$pattern_manual) != ""
    locked[is.na(locked)] <- FALSE
  }

  labels <- c("burst", "long_burst", "possible_burst", "pause", "tonic", "high_frequency_tonic", "high_frequency_spiking", "others")
  removed_rows <- list()
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)

  # For HF-spiking, a user-provided pattern Max_ISI is a hard final-label
  # ceiling: tolerated-gap logic may connect neighboring HF epochs, but the
  # over-limit ISI itself should not be colored or exported as HF spiking.
  hard_gate_labels <- c("high_frequency_spiking")
  for (lab in hard_gate_labels) {
    lim <- stpd_pattern_isi_limits_for_label(lab, params)
    min_active <- is.finite(lim$min_sec) && lim$min_sec > 0
    max_active <- is.finite(lim$max_sec) && lim$max_sec > 0
    if (!min_active && !max_active) next
    bad <- pat == lab & !locked & is.finite(isi) & !art
    if (min_active) bad <- bad & isi >= lim$min_sec else bad <- bad
    if (max_active) bad <- bad & isi <= lim$max_sec else bad <- bad
    bad <- which(pat == lab & !locked & is.finite(isi) & !art & !bad)
    if (length(bad) == 0) next
    bad_pat <- rep("", nrow(dat))
    bad_pat[bad] <- "bad"
    bad_seg <- find_segments(bad_pat, "bad")
    for (kk in seq_len(nrow(bad_seg))) {
      s_bad <- suppressWarnings(as.integer(bad_seg$start_isi[kk]))
      e_bad <- suppressWarnings(as.integer(bad_seg$end_isi[kk]))
      if (!is.finite(s_bad) || !is.finite(e_bad) || e_bad < s_bad) next
      bad_idx <- s_bad:e_bad
      removed_rows[[length(removed_rows) + 1L]] <- data.frame(
        train = as.character(train %||% ""),
        pattern = lab,
        start_isi = s_bad,
        end_isi = e_bad,
        n_spikes_final = e_bad - s_bad + 2L,
        n_isi_final = e_bad - s_bad + 1L,
        n_valid_isi_final = sum(is.finite(isi[bad_idx]) & !art[bad_idx], na.rm = TRUE),
        required_min_spikes = NA_integer_,
        required_min_isi = NA_integer_,
        duration_sec = NA_real_,
        required_min_duration_sec = NA_real_,
        required_pattern_min_ISI_sec = lim$min_sec,
        required_pattern_max_ISI_sec = lim$max_sec,
        pattern_isi_gate_pass = FALSE,
        pattern_isi_gate_reason = "hf_spiking_individual_isi_outside_pattern_gate",
        action = "trimmed_auto_isi_by_pattern_hard_gate",
        stringsAsFactors = FALSE
      )
    }
    pat[bad] <- ""
    if (!is.null(dat$auto_score)) dat$auto_score[bad] <- NA_real_
  }

  for (lab in labels) {
    seg <- find_segments(pat, lab)
    if (nrow(seg) == 0) next
    min_spk <- max(1L, stpd_min_spikes_for_label(lab, params))
    min_isi_n <- max(0L, min_spk - 1L)
    min_dur <- stpd_min_duration_for_label(lab, params)
    if (!is.finite(min_dur)) min_dur <- 0

    for (k in seq_len(nrow(seg))) {
      s0 <- suppressWarnings(as.integer(seg$start_isi[k]))
      e0 <- suppressWarnings(as.integer(seg$end_isi[k]))
      if (!is.finite(s0) || !is.finite(e0) || e0 < s0) next
      if (s0 < 2L || e0 > nrow(dat)) next
      idx <- s0:e0
      idx_unlocked <- idx[!locked[idx]]
      if (length(idx_unlocked) == 0) next

      n_spikes_final <- e0 - s0 + 2L
      n_isi_final <- e0 - s0 + 1L
      n_valid_isi_final <- sum(is.finite(isi[idx]) & !art[idx], na.rm = TRUE)
      start_t <- suppressWarnings(as.numeric(dat$timestamp_sec[s0 - 1L]))
      end_t <- suppressWarnings(as.numeric(dat$timestamp_sec[e0]))
      dur <- end_t - start_t
      dur_ok <- !is.finite(min_dur) || min_dur <= 0 || (is.finite(dur) && dur >= min_dur)
      size_ok <- n_spikes_final >= min_spk && n_isi_final >= min_isi_n && n_valid_isi_final >= min_isi_n
      isi_gate <- stpd_pattern_isi_gate_pass(isi[idx], lab, params, min_isi_sec = min_isi_sec)
      isi_gate_ok <- isTRUE(isi_gate$pass)

      if (!size_ok || !dur_ok || !isi_gate_ok) {
        pat[idx_unlocked] <- ""
        if (!is.null(dat$auto_score)) dat$auto_score[idx_unlocked] <- NA_real_
        removed_rows[[length(removed_rows) + 1L]] <- data.frame(
          train = as.character(train %||% ""),
          pattern = lab,
          start_isi = s0,
          end_isi = e0,
          n_spikes_final = n_spikes_final,
          n_isi_final = n_isi_final,
          n_valid_isi_final = n_valid_isi_final,
          required_min_spikes = min_spk,
          required_min_isi = min_isi_n,
          duration_sec = dur,
          required_min_duration_sec = min_dur,
          required_pattern_min_ISI_sec = isi_gate$min_sec,
          required_pattern_max_ISI_sec = isi_gate$max_sec,
          pattern_isi_gate_pass = isi_gate_ok,
          pattern_isi_gate_reason = isi_gate$reason,
          action = ifelse(!isi_gate_ok, "removed_auto_event_by_pattern_isi_gate", "removed_auto_fragment_after_overlap_resolution"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  dat$pattern_auto <- pat
  if (length(removed_rows) > 0) attr(dat, "posthoc_fragment_audit") <- dplyr::bind_rows(removed_rows)
  dat
}

stpd_detect_train_near_miss_augmented <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  burst_p <- params$burst
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  dat <- ensure_train_local_median_cache(dat, window = burst_p$local_window %||% 11L, min_isi_sec = min_isi_sec, force = TRUE)
  n <- nrow(dat)
  if (n <= 1) {
    dat$pattern_auto <- ""
    dat$auto_score <- NA_real_
    return(dat)
  }
  manual_for_lock <- if (isTRUE(lock_manual)) as.character(dat$pattern_manual) else rep("", n)
  manual_for_lock[is.na(manual_for_lock)] <- ""
  dat$pattern_auto <- ""
  dat$auto_score <- NA_real_
  patterns <- params$detector$patterns_to_run %||% stpd_default_patterns_to_run()
  fill_others <- isTRUE(params$detector$fill_others_auto)
  locked <- manual_for_lock != ""

  # 1) Burst-family first. `long_burst` is not a separate detector; it is a
  # burst-family subclass and therefore triggers the same burst detector chain.
  run_burst_family <- any(c("burst", "long_burst") %in% patterns)
  if (run_burst_family) {
    b <- detect_burst_train(dat, burst_p, min_isi_sec = min_isi_sec, train = train)
    seed_bridge_diag <- attr(b, "seed_bridge_diag")
    if (nrow(b) > 0) {
      keep_classes <- character(0)
      if ("burst" %in% patterns) keep_classes <- c(keep_classes, "burst")
      if ("long_burst" %in% patterns) keep_classes <- c(keep_classes, "long_burst")
      keep_classes <- c(keep_classes, "possible_burst")
      b <- b[b$class %in% keep_classes, , drop = FALSE]
    }
    if (nrow(b) > 0) {
      for (k in seq_len(nrow(b))) {
        idx <- b$start_isi[k]:b$end_isi[k]
        idx <- idx[!locked[idx]]
        if (length(idx) == 0) next
        dat$pattern_auto[idx] <- b$class[k]
        dat$auto_score[idx] <- b$score[k]
      }
    }
    if (!isTRUE(burst_p$use_seed_bridge_model %||% TRUE)) {
      dat <- bridge_close_auto_burst_gaps(dat, burst_p, min_isi_sec = min_isi_sec)
    }
    attr(dat, "seed_bridge_diag") <- seed_bridge_diag
  }

  # 2) High-frequency modes. These run after burst and before classical tonic so
  # stable high-rate segments can be explicitly labeled as high_frequency_tonic
  # instead of being absorbed into generic tonic.
  current_labels <- compute_final_pattern(manual_for_lock, dat$pattern_auto, dat$ISI_sec,
                                          auto_others = FALSE, min_isi_sec = min_isi_sec)
  occupied <- current_labels != ""
  hf_patterns <- intersect(patterns, c("high_frequency_tonic", "high_frequency_spiking"))
  if (length(hf_patterns) > 0 && isTRUE(params$highfreq$enable %||% TRUE)) {
    hfblocks <- detect_high_frequency_modes_train(dat, occupied, params$highfreq %||% default_params_sec()$highfreq,
                                                  min_isi_sec = min_isi_sec, train = train)
    if (nrow(hfblocks) > 0) {
      for (k in seq_len(nrow(hfblocks))) {
        cls <- as.character(hfblocks$class[k])
        if (!(cls %in% hf_patterns)) next
        idx <- hfblocks$start_isi[k]:hfblocks$end_isi[k]
        idx <- idx[!locked[idx] & dat$pattern_auto[idx] == ""]
        if (length(idx) == 0) next
        dat$pattern_auto[idx] <- cls
        dat$auto_score[idx] <- suppressWarnings(as.numeric(hfblocks$score[k]))
      }
    }
  }

  # 3) Classical tonic: stable firing in the tonic ISI range not already labeled
  # as burst or high-frequency tonic/spiking.
  current_labels <- compute_final_pattern(manual_for_lock, dat$pattern_auto, dat$ISI_sec,
                                          auto_others = FALSE, min_isi_sec = min_isi_sec)
  occupied <- current_labels != ""
  if ("tonic" %in% patterns) {
    tblocks <- detect_tonic_train(dat, occupied, params$tonic, burst_p$T_seed, min_isi_sec = min_isi_sec, train = train)
    if (nrow(tblocks) > 0) {
      for (k in seq_len(nrow(tblocks))) {
        idx <- tblocks$start_isi[k]:tblocks$end_isi[k]
        idx <- idx[!locked[idx] & dat$pattern_auto[idx] == ""]
        dat$pattern_auto[idx] <- "tonic"
      }
    }
  }

  # 4) Pause after burst/high-frequency/tonic occupation.
  current_labels <- compute_final_pattern(manual_for_lock, dat$pattern_auto, dat$ISI_sec,
                                          auto_others = FALSE, min_isi_sec = min_isi_sec)
  occupied <- current_labels != ""
  if ("pause" %in% patterns) {
    pblocks <- detect_pause_train(dat, occupied, params$pause, params$tonic,
                                  min_isi_sec = min_isi_sec, current_labels = current_labels, train = train)
    attr(dat, "pause_diag") <- pblocks
    if (nrow(pblocks) > 0) {
      for (k in seq_len(nrow(pblocks))) {
        idx <- pblocks$start_isi[k]:pblocks$end_isi[k]
        idx <- idx[!locked[idx] & dat$pattern_auto[idx] == ""]
        dat$pattern_auto[idx] <- "pause"
      }
    }
  } else {
    attr(dat, "pause_diag") <- data.frame()
  }

  # 5) Optional explicit others fill.
  if ("others" %in% patterns && fill_others) {
    art <- is_artifact_isi(dat$ISI_sec, min_isi_sec)
    fill_idx <- which(dat$idx >= 2 & !art & is.finite(dat$ISI_sec) &
                        manual_for_lock == "" & dat$pattern_auto == "")
    dat$pattern_auto[fill_idx] <- "others"
  }

  # 6) Final guard after all overlap/priority decisions. This is deliberately
  # post hoc: it validates what the user will actually see/export, not the
  # larger pre-overlap candidates. It prevents invalid visible fragments such as
  # a two-spike high_frequency_spiking event when the UI minimum is eight spikes.
  dat <- stpd_post_validate_auto_event_sizes(dat, params, min_isi_sec = min_isi_sec, lock_manual = lock_manual, train = train)
  dat
}
