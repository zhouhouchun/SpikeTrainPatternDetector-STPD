# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# refined semantic-consistency helpers
# ============================================================

effective_burst_params <- function(params) {
  # Single source of truth: detector-level refractory settings are authoritative.
  bp <- params$burst %||% default_params_sec()$burst
  det <- params$detector %||% list()
  bp$refractory_suspect_sec <- det$refractory_suspect_sec %||% bp$refractory_suspect_sec %||% 0.0010
  bp$refractory_suspect_action <- det$refractory_suspect_action %||% bp$refractory_suspect_action %||% "demote_to_possible"
  bp
}

effective_pause_params <- function(params) {
  pp <- params$pause %||% default_params_sec()$pause
  pp$exclude_occupied_context <- pp$exclude_occupied_context %||% TRUE
  pp$global_median_guard <- pp$global_median_guard %||% TRUE
  pp$global_median_factor <- pp$global_median_factor %||% 2.50
  pp
}

effective_params_for_detector_core <- function(params) {
  # Return a fully resolved parameter object used by both Shiny and batch/API paths.
  pp <- params
  pp$burst <- effective_burst_params(params)
  pp$pause <- effective_pause_params(params)
  if (is.null(pp$detector)) pp$detector <- list()
  pp$detector$refractory_suspect_sec <- pp$burst$refractory_suspect_sec
  pp$detector$refractory_suspect_action <- pp$burst$refractory_suspect_action
  pp$detector$analysis_role <- pp$detector$analysis_role %||% "candidate_event_generator_plus_review"
  pp$detector$preset_name <- pp$detector$preset_name %||% "balanced_single_unit"
  pp$detector$require_human_or_model_review_for_publication <- pp$detector$require_human_or_model_review_for_publication %||% TRUE
  pp
}

effective_params_for_detector <- effective_params_for_detector_core

add_run_metadata_cols <- function(df, run_id = "", params_hash = "") {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!("run_id" %in% names(df))) df$run_id <- run_id
  if (!("params_hash" %in% names(df))) df$params_hash <- params_hash
  df
}

candidate_col_first <- function(row, names_vec, default = NA_real_) {
  for (nm in names_vec) {
    if (nm %in% names(row)) {
      v <- row[[nm]][1]
      if (is.numeric(v) || is.integer(v)) {
        v <- suppressWarnings(as.numeric(v))
        if (is.finite(v)) return(v)
      } else {
        return(as.character(v))
      }
    }
  }
  default
}

is_long_burst_span <- function(dat, s_isi, e_isi, p, min_isi_sec = 0.001,
                                            edge_min = NA_real_, edge_geom = NA_real_, n_spikes = NA_integer_, duration_sec = NA_real_) {
  if (!isTRUE(p$long_burst_enable %||% FALSE)) return(FALSE)
  n <- nrow(dat)
  if (!is.finite(s_isi) || !is.finite(e_isi) || s_isi < 2L || e_isi > n || e_isi < s_isi) return(FALSE)
  vals <- valid_isi_values(dat$ISI_sec[s_isi:e_isi], min_isi_sec)
  if (length(vals) == 0) return(FALSE)
  if (!is.finite(n_spikes)) n_spikes <- e_isi - s_isi + 2L
  if (!is.finite(duration_sec)) duration_sec <- dat$timestamp_sec[e_isi] - dat$timestamp_sec[s_isi - 1L]
  lb_min_spk <- safe_int(p$long_burst_min_spikes %||% 11L, 11L)
  lb_max_spk <- safe_int(p$long_burst_max_spikes %||% 0L, 0L)
  lb_min_dur <- suppressWarnings(as.numeric(p$long_burst_min_duration %||% 0))
  lb_max_dur <- suppressWarnings(as.numeric(p$long_burst_max_duration %||% 0))
  if (n_spikes < lb_min_spk) return(FALSE)
  if (lb_max_spk > 0L && n_spikes > lb_max_spk) return(FALSE)
  if (is.finite(lb_min_dur) && lb_min_dur > 0 && duration_sec < lb_min_dur) return(FALSE)
  if (is.finite(lb_max_dur) && lb_max_dur > 0 && duration_sec > lb_max_dur) return(FALSE)
  if (!is.finite(edge_min) || !is.finite(edge_geom)) {
    ed <- calc_edge_contrast_stats(dat$ISI_sec, s_isi, e_isi, min_isi_sec, p$contrast_q %||% 0.90)
    edge_min <- ed$contrast_min_q
    edge_geom <- ed$contrast_geom_q
  }
  lb_edge_min <- suppressWarnings(as.numeric(p$long_burst_edge_contrast_min %||% p$final_edge_contrast_min %||% 1.45))
  lb_edge_geom <- suppressWarnings(as.numeric(p$long_burst_edge_contrast_geom %||% p$final_edge_contrast_geom %||% 1.50))
  if (!is.finite(edge_min) || !is.finite(edge_geom) || edge_min < lb_edge_min || edge_geom < lb_edge_geom) return(FALSE)
  lb_pct <- suppressWarnings(as.numeric(p$long_burst_core_pct_max %||% 35))
  lb_frac_min <- suppressWarnings(as.numeric(p$long_burst_short_fraction_min %||% 0.65))
  pct_vec <- if ("ISI_pct" %in% names(dat)) suppressWarnings(as.numeric(dat$ISI_pct[s_isi:e_isi])) else numeric(0)
  if (length(pct_vec) > 0 && any(is.finite(pct_vec))) {
    frac <- mean(is.finite(pct_vec) & pct_vec <= lb_pct, na.rm = TRUE)
  } else {
    qv <- suppressWarnings(as.numeric(stats::quantile(vals, probs = p$contrast_q %||% 0.90, na.rm = TRUE, type = 7)))
    frac <- if (is.finite(qv) && qv > 0) mean(vals <= qv * 1.50, na.rm = TRUE) else NA_real_
  }
  is.finite(frac) && frac >= lb_frac_min
}

reclassify_burst_family_candidate <- function(cand, dat, p, min_isi_sec = 0.001) {
  if (is.null(cand) || nrow(cand) == 0) return(cand)
  for (ii in seq_len(nrow(cand))) {
    cls0 <- as.character(cand$class[ii] %||% "")
    if (!cls0 %in% c("burst", "long_burst")) next
    s0 <- suppressWarnings(as.integer(cand$start_isi[ii])); e0 <- suppressWarnings(as.integer(cand$end_isi[ii]))
    nsp <- candidate_col_first(cand[ii, , drop = FALSE], c("n_spikes", "merged_n_spikes"), default = e0 - s0 + 2L)
    dur <- candidate_col_first(cand[ii, , drop = FALSE], c("duration_sec", "merged_duration_sec"), default = NA_real_)
    emin <- candidate_col_first(cand[ii, , drop = FALSE], c("edge_contrast_min_q", "edge_contrast_min_seed_q", "contrast_min_ctx_q", "merged_edge_contrast_min_q"), default = NA_real_)
    egeom <- candidate_col_first(cand[ii, , drop = FALSE], c("edge_contrast_geom_q", "edge_contrast_geom_seed_q", "contrast_geom_ctx_q", "merged_edge_contrast_geom_q"), default = NA_real_)
    if (is_long_burst_span(dat, s0, e0, p, min_isi_sec, edge_min = emin, edge_geom = egeom, n_spikes = nsp, duration_sec = dur)) {
      cand$class[ii] <- as.character(p$long_burst_output_class %||% "long_burst")
      if (!"reject_reason" %in% names(cand)) cand$reject_reason <- ""
      cand$reject_reason[ii] <- trimws(paste(as.character(cand$reject_reason[ii] %||% ""), "long_burst_reclassified_after_merge"))
    } else if (cls0 == "long_burst") {
      cand$class[ii] <- "burst"
      if (!"reject_reason" %in% names(cand)) cand$reject_reason <- ""
      cand$reject_reason[ii] <- trimws(paste(as.character(cand$reject_reason[ii] %||% ""), "long_burst_reclassified_to_burst_after_merge"))
    }
  }
  cand
}

mode_nonempty_label <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""; x <- x[x != ""]
  if (length(x) == 0) return("")
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

possible_burst_reason <- function(row) {
  row_chr <- function(names_vec, default = "") {
    for (nm in names_vec) {
      if (nm %in% names(row)) {
        v <- row[[nm]][1]
        if (!is.null(v) && length(v) > 0 && !is.na(v)) return(as.character(v))
      }
    }
    default
  }
  cls <- row_chr(c("class", "raw_candidate_class"))
  src <- tolower(row_chr(c("source", "candidate_source")))
  reasons <- character()
  if (cls == "possible_burst") reasons <- c(reasons, "review_candidate")
  if (grepl("boundary_start", src)) reasons <- c(reasons, "boundary_missing_pre_flank")
  if (grepl("boundary_end", src)) reasons <- c(reasons, "boundary_missing_post_flank")
  if (grepl("local", src)) reasons <- c(reasons, "local_compression_review")
  if ("refractory_suspect_n" %in% names(row) && is.finite(suppressWarnings(as.numeric(row$refractory_suspect_n))) && suppressWarnings(as.numeric(row$refractory_suspect_n)) > 0) reasons <- c(reasons, "contains_refractory_suspect_ISI")
  if ("tonic_like" %in% names(row) && isTRUE(as.logical(row$tonic_like))) reasons <- c(reasons, "tonic_like_regular_high_frequency")
  rr <- row_chr(c("reject_reason", "rejection_reason", "uncertainty_reason"))
  if (nzchar(rr)) reasons <- c(reasons, rr)
  reasons <- unique(trimws(reasons)); reasons <- reasons[nzchar(reasons)]
  if (length(reasons) == 0) "" else paste(reasons, collapse = ";")
}


build_candidate_ledger_from_result_tables <- function(ds, params, selected_trains = NULL, run_id = NULL, params_hash = NULL) {
  # candidate ledger contains candidates only. Final extracted events are
  # stored separately in build_event_ledger_internal(). This prevents final
  # events from inflating candidate counts or candidate-feature audits.
  if (is.null(ds) || is.null(ds$trains)) return(tibble())
  run_id <- run_id %||% paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  params_hash <- params_hash %||% compute_params_hash(params)
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(trains, names(ds$trains))
  rows <- list()
  row_chr <- function(row, names_vec, default = "") {
    for (nm in names_vec) {
      if (nm %in% names(row)) {
        v <- row[[nm]][1]
        if (!is.null(v) && length(v) > 0 && !is.na(v)) return(as.character(v))
      }
    }
    default
  }
  row_num <- function(row, names_vec, default = NA_real_) {
    for (nm in names_vec) {
      if (nm %in% names(row)) {
        v <- suppressWarnings(as.numeric(row[[nm]][1]))
        if (is.finite(v)) return(v)
      }
    }
    default
  }
  row_int <- function(row, names_vec, default = NA_integer_) {
    v <- row_num(row, names_vec, default = NA_real_)
    if (is.finite(v)) as.integer(v) else default
  }
  add_candidate_row <- function(r, tr, dat, ii, source_default = "candidate", class_default = "not_written_to_auto") {
    s0 <- row_int(r, "start_isi"); e0 <- row_int(r, "end_isi")
    if (!is.finite(s0) || !is.finite(e0) || s0 < 2 || e0 > nrow(dat) || e0 < s0) return(NULL)
    auto_vals <- as.character(dat$pattern_auto[s0:e0]); auto_vals[is.na(auto_vals)] <- ""
    final_vals <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                                        auto_others = FALSE,
                                        min_isi_sec = params$detector$min_valid_isi_sec %||% 0.0009)[s0:e0]
    raw_cls <- row_chr(r, c("raw_class", "class"), class_default)
    auto_major <- mode_nonempty_label(auto_vals)
    final_major <- mode_nonempty_label(final_vals)
    src <- row_chr(r, c("source", "candidate_source"), source_default)
    written <- nzchar(auto_major)
    tibble(
      candidate_id = row_chr(r, "candidate_id", paste0(run_id, ":", tr, ":", source_default, ":", ii)),
      run_id = run_id, params_hash = params_hash,
      train = tr, start_isi = s0, end_isi = e0,
      start_time_sec = dat$timestamp_sec[s0 - 1L], end_time_sec = dat$timestamp_sec[e0],
      candidate_source = src, raw_candidate_class = raw_cls,
      final_candidate_class = if (nzchar(auto_major)) auto_major else row_chr(r, "class", class_default),
      final_label_majority = if (nzchar(final_major)) final_major else "unlabeled",
      written_to_auto = written, visible_in_raster = written,
      score = row_num(r, c("score", "candidate_score")),
      possible_burst_subtype = if (raw_cls == "possible_burst" || auto_major == "possible_burst" || row_chr(r, "class") == "possible_burst") possible_burst_reason(r) else "",
      uncertainty_reason = possible_burst_reason(r),
      policy_action = row_chr(r, c("refractory_suspect_action", "policy_action")),
      rejection_reason = row_chr(r, c("reject_reason", "rejection_reason")),
      refractory_suspect_n = row_num(r, "refractory_suspect_n"),
      pause_local_median_sec = row_num(r, "pause_local_median_sec"),
      pause_effective_threshold_sec = row_num(r, "pause_effective_threshold_sec"),
      pause_global_median_sec = row_num(r, "pause_global_median_sec"),
      pause_global_threshold_sec = row_num(r, "pause_global_threshold_sec"),
      stringsAsFactors = FALSE
    )
  }
  bc <- ds$results$burst_candidates_final %||% ds$results$burst_candidates %||% data.frame()
  if (!is.null(bc) && nrow(bc) > 0) {
    for (ii in seq_len(nrow(bc))) {
      r <- bc[ii, , drop = FALSE]; tr <- row_chr(r, "train")
      if (!tr %in% trains || !tr %in% names(ds$trains)) next
      row <- add_candidate_row(r, tr, ds$trains[[tr]], ii, source_default = "burst_candidate_final", class_default = "not_written_to_auto")
      if (!is.null(row)) rows[[length(rows)+1L]] <- row
    }
  }
  pc <- ds$results$pause_candidates %||% data.frame()
  if (!is.null(pc) && nrow(pc) > 0 && all(c("train", "start_isi", "end_isi") %in% names(pc))) {
    for (ii in seq_len(nrow(pc))) {
      r <- pc[ii, , drop = FALSE]; tr <- row_chr(r, "train")
      if (!tr %in% trains || !tr %in% names(ds$trains)) next
      if (!"class" %in% names(r)) r$class <- "pause"
      if (!"source" %in% names(r)) r$source <- "pause_candidate"
      row <- add_candidate_row(r, tr, ds$trains[[tr]], ii, source_default = "pause_candidate", class_default = "pause")
      if (!is.null(row)) rows[[length(rows)+1L]] <- row
    }
  }
  if (length(rows) == 0) tibble() else bind_rows(rows) %>% arrange(train, start_isi, end_isi, candidate_source)
}

build_candidate_ledger_internal <- build_candidate_ledger_from_result_tables

build_event_ledger_internal <- function(ds, params, selected_trains = NULL, run_id = NULL, params_hash = NULL) {
  # Final events only. This is intentionally separate from candidate_ledger.
  if (is.null(ds) || is.null(ds$results)) return(tibble())
  ev <- ds$results$events %||% data.frame()
  if (is.null(ev) || nrow(ev) == 0) return(tibble())
  run_id <- run_id %||% as.character((ds$results$run_metadata$run_id %||% paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S")))[1])
  params_hash <- params_hash %||% as.character((ds$results$run_metadata$params_hash %||% compute_params_hash(params))[1])
  trains <- selected_trains %||% unique(as.character(ev$train))
  ev <- ev[as.character(ev$train) %in% trains, , drop = FALSE]
  if (nrow(ev) == 0) return(tibble())
  ev$event_id <- paste0(run_id, ":event:", seq_len(nrow(ev)))
  ev$run_id <- if ("run_id" %in% names(ev)) ev$run_id else run_id
  ev$params_hash <- if ("params_hash" %in% names(ev)) ev$params_hash else params_hash
  ev$event_layer <- dplyr::case_when(
    as.character(ev$pattern) == "possible_burst" ~ "review_candidate",
    as.character(ev$pattern) %in% c("burst", "long_burst") ~ "high_confidence_burst_family_member",
    TRUE ~ "high_confidence_nonburst_or_other"
  )
  ev$pattern_family <- ifelse(as.character(ev$pattern) %in% c("burst", "long_burst", "possible_burst"), "burst_family", as.character(ev$pattern))
  ev
}


result_layers_from_events <- function(events) {
  # Events_burst_family means burst-family only, not all events with
  # a family-mapping column. This avoids inflating burst-family summaries with
  # pause/tonic/high-frequency events.
  if (is.null(events) || nrow(events) == 0) {
    empty <- data.frame()
    return(list(high_confidence = empty, review_candidates = empty, burst_family = empty, all_event_family_map = empty))
  }
  ev <- events
  high <- ev %>% filter(pattern != "possible_burst")
  review <- ev %>% filter(pattern == "possible_burst") %>% mutate(review_family = "burst_review_candidate")
  fam <- ev %>% filter(pattern %in% c("burst", "long_burst", "possible_burst")) %>% mutate(pattern_family = "burst_family")
  all_map <- ev %>% mutate(pattern_family = ifelse(pattern %in% c("burst", "long_burst", "possible_burst"), "burst_family", as.character(pattern)))
  list(high_confidence = high, review_candidates = review, burst_family = fam, all_event_family_map = all_map)
}

add_run_columns <- function(df, run_id = "", params_hash = "") {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!("run_id" %in% names(df))) df$run_id <- run_id
  if (!("params_hash" %in% names(df))) df$params_hash <- params_hash
  df
}

stpd_seed_bridge_diagnostics_for_dataset <- function(ds, params, min_isi_sec = 0.001,
                                                     target_trains = NULL, run_id = "", params_hash = "") {
  empty <- list(
    structures = add_run_columns(empty_structure_candidates_tbl(), run_id, params_hash),
    seeds = add_run_columns(empty_seed_candidates_tbl(), run_id, params_hash),
    bridges = add_run_columns(empty_bridge_candidates_tbl(), run_id, params_hash),
    candidates = add_run_columns(empty_burst_candidates_tbl(), run_id, params_hash)
  )
  if (is.null(ds) || is.null(ds$trains) || !exists("seed_bridge_diagnostics_train", mode = "function")) return(empty)
  params <- effective_params_for_detector(params)
  bp <- effective_burst_params(params)
  if (!isTRUE(bp$use_seed_bridge_model %||% TRUE)) return(empty)
  target_trains <- target_trains %||% names(ds$trains)
  target_trains <- intersect(target_trains, names(ds$trains))
  if (length(target_trains) == 0) return(empty)

  parts <- list(structures = list(), seeds = list(), bridges = list(), candidates = list())
  add_part <- function(key, tbl, tr) {
    if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(invisible(NULL))
    if (!("train" %in% names(tbl))) tbl$train <- tr
    parts[[key]][[length(parts[[key]]) + 1L]] <<- tbl
    invisible(NULL)
  }

  for (tr in target_trains) {
    dg <- tryCatch(
      seed_bridge_diagnostics_train(ds$trains[[tr]], bp, min_isi_sec = min_isi_sec, train = tr),
      error = function(e) NULL
    )
    if (is.null(dg)) next
    add_part("structures", dg$structures, tr)
    add_part("seeds", dg$seeds, tr)
    add_part("bridges", dg$bridges, tr)
    add_part("candidates", dg$candidates, tr)
  }

  list(
    structures = add_run_columns(if (length(parts$structures) > 0) bind_rows(parts$structures) else empty_structure_candidates_tbl(), run_id, params_hash),
    seeds = add_run_columns(if (length(parts$seeds) > 0) bind_rows(parts$seeds) else empty_seed_candidates_tbl(), run_id, params_hash),
    bridges = add_run_columns(if (length(parts$bridges) > 0) bind_rows(parts$bridges) else empty_bridge_candidates_tbl(), run_id, params_hash),
    candidates = add_run_columns(if (length(parts$candidates) > 0) bind_rows(parts$candidates) else empty_burst_candidates_tbl(), run_id, params_hash)
  )
}

stpd_csv_escape_formula <- function(x) {
  out <- as.character(x)
  hit <- !is.na(out) & grepl("^[[:space:]]*[=+@-]", out)
  out[hit] <- paste0("'", out[hit])
  out
}

csv_safe_df <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  out <- df
  for (nm in names(out)) {
    if (!is.list(out[[nm]])) next
    out[[nm]] <- vapply(out[[nm]], function(x) {
      if (is.null(x) || length(x) == 0) return("")
      if (is.data.frame(x)) return(paste(utils::capture.output(str(x, give.attr = FALSE)), collapse = " "))
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
      if (length(x) == 0) return("")
      paste(as.character(x), collapse = ";")
    }, character(1))
  }
  for (nm in names(out)) {
    if (is.factor(out[[nm]]) || is.character(out[[nm]])) {
      out[[nm]] <- stpd_csv_escape_formula(out[[nm]])
    }
  }
  out
}

stpd_column_is_export_empty <- function(x) {
  if (is.null(x) || length(x) == 0) return(TRUE)
  if (is.list(x) && !is.data.frame(x)) {
    empty <- vapply(x, function(xx) {
      if (is.null(xx) || length(xx) == 0) return(TRUE)
      xx <- unlist(xx, recursive = TRUE, use.names = FALSE)
      if (length(xx) == 0) return(TRUE)
      yy <- as.character(xx)
      all(is.na(yy) | !nzchar(trimws(yy)))
    }, logical(1))
    return(all(empty))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(all(is.na(x) | !nzchar(trimws(x))))
  all(is.na(x))
}

stpd_drop_empty_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(df)
  keep <- !vapply(df, stpd_column_is_export_empty, logical(1))
  df[, keep, drop = FALSE]
}

write_csv_safe <- function(df, file, row.names = FALSE, fileEncoding = "UTF-8", ...) {
  utils::write.csv(csv_safe_df(df), file, row.names = row.names, fileEncoding = fileEncoding, ...)
}

enrich_events_with_pause_thresholds <- function(events, trains, run_id = "", params_hash = "") {
  if (is.null(events) || nrow(events) == 0 || is.null(trains)) return(events)
  cols <- c("pause_local_median_sec", "pause_effective_threshold_sec", "pause_global_median_sec",
            "pause_global_threshold_sec", "pause_excluded_context_n", "pause_global_guard_used",
            "pause_context_excluded", "pause_alpha", "pause_context_factor")
  for (cc in cols) if (!(cc %in% names(events))) events[[cc]] <- NA
  for (tr in intersect(unique(as.character(events$train)), names(trains))) {
    pb <- attr(trains[[tr]], "pause_diag")
    if (is.null(pb) || nrow(pb) == 0) next
    ev_idx <- which(as.character(events$train) == tr & as.character(events$pattern) == "pause")
    if (length(ev_idx) == 0) next
    for (ii in ev_idx) {
      s0 <- suppressWarnings(as.integer(events$start_isi[ii])); e0 <- suppressWarnings(as.integer(events$end_isi[ii]))
      hit <- which(suppressWarnings(as.integer(pb$start_isi)) == s0 & suppressWarnings(as.integer(pb$end_isi)) == e0)
      if (length(hit) == 0) {
        hit <- which(suppressWarnings(as.integer(pb$start_isi)) <= s0 & suppressWarnings(as.integer(pb$end_isi)) >= e0)
      }
      if (length(hit) == 0) next
      h <- hit[1]
      for (cc in intersect(cols, names(pb))) events[[cc]][ii] <- pb[[cc]][h]
    }
  }
  add_run_columns(events, run_id = run_id, params_hash = params_hash)
}


write_tiered_result_exports <- function(ds, params, out_dir) {
  params <- effective_params_for_detector(params)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(ds$results)) ds$results <- list()
  rid <- as.character((ds$results$run_metadata$run_id %||% paste0("export_run_", format(Sys.time(), "%Y%m%d_%H%M%S")))[1])
  ph <- as.character((ds$results$run_metadata$params_hash %||% compute_params_hash(params))[1])
  if (is.null(ds$results$candidate_ledger) || nrow(ds$results$candidate_ledger %||% data.frame()) == 0) {
    ds$results$candidate_ledger <- build_candidate_ledger_internal(ds, params, selected_trains = names(ds$trains), run_id = rid, params_hash = ph)
  }
  if (is.null(ds$results$event_ledger) || nrow(ds$results$event_ledger %||% data.frame()) == 0) {
    ds$results$event_ledger <- build_event_ledger_internal(ds, params, selected_trains = names(ds$trains), run_id = rid, params_hash = ph)
    ds$results$event_audit <- ds$results$event_ledger
  }
  layers <- result_layers_from_events(ds$results$events %||% data.frame())
  if (nrow(layers$high_confidence) > 0) write_csv_safe(layers$high_confidence, file.path(out_dir, "Events_high_confidence.csv"))
  if (nrow(layers$review_candidates) > 0) write_csv_safe(layers$review_candidates, file.path(out_dir, "Events_review_candidates.csv"))
  if (nrow(layers$burst_family) > 0) {
    write_csv_safe(layers$burst_family, file.path(out_dir, "Events_burst_family_candidates.csv"))
    write_csv_safe(layers$burst_family, file.path(out_dir, "Events_burst_family.csv")) # backward-compatible alias
  }
  if (nrow(layers$all_event_family_map) > 0) {
    write_csv_safe(layers$all_event_family_map, file.path(out_dir, "Events_all_with_pattern_family.csv"))
    write_csv_safe(layers$all_event_family_map, file.path(out_dir, "Events_all_family_map.csv")) # backward-compatible alias
  }
  if (!is.null(ds$results$candidate_ledger) && nrow(ds$results$candidate_ledger) > 0) {
    write_csv_safe(ds$results$candidate_ledger, file.path(out_dir, "Candidate_ledger.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    write_csv_safe(ds$results$candidate_ledger, file.path(out_dir, "Candidates_ledger.csv"), row.names = FALSE, fileEncoding = "UTF-8") # backward-compatible alias
  }
  # Backward-compatible filename, now explicitly candidate-only.
  if (!is.null(ds$results$candidate_ledger) && nrow(ds$results$candidate_ledger) > 0) write_csv_safe(ds$results$candidate_ledger, file.path(out_dir, "Events_all_candidates_ledger.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$event_ledger) && nrow(ds$results$event_ledger) > 0) {
    write_csv_safe(ds$results$event_ledger, file.path(out_dir, "Event_audit.csv"))
    write_csv_safe(ds$results$event_ledger, file.path(out_dir, "Events_final_event_ledger.csv")) # backward-compatible alias
  }
  if (!is.null(ds$results$burst_candidates_raw) && nrow(ds$results$burst_candidates_raw) > 0) write_csv_safe(ds$results$burst_candidates_raw, file.path(out_dir, "Burst_candidates_raw.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$burst_candidates_final) && nrow(ds$results$burst_candidates_final) > 0) write_csv_safe(ds$results$burst_candidates_final, file.path(out_dir, "Burst_candidates_final.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$pause_candidates) && nrow(ds$results$pause_candidates) > 0) write_csv_safe(ds$results$pause_candidates, file.path(out_dir, "Pause_candidates_with_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$posthoc_fragment_audit) && nrow(ds$results$posthoc_fragment_audit) > 0) write_csv_safe(ds$results$posthoc_fragment_audit, file.path(out_dir, "Posthoc_fragment_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$candidate_diagnostic_audit) && nrow(ds$results$candidate_diagnostic_audit) > 0) write_csv_safe(ds$results$candidate_diagnostic_audit, file.path(out_dir, "Candidate_diagnostic_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$final_audit_summary) && nrow(ds$results$final_audit_summary) > 0) write_csv_safe(ds$results$final_audit_summary, file.path(out_dir, "Final_audit_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$final_audit_events) && nrow(ds$results$final_audit_events) > 0) write_csv_safe(ds$results$final_audit_events, file.path(out_dir, "Final_audit_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$final_audit_history) && nrow(ds$results$final_audit_history) > 0) write_csv_safe(ds$results$final_audit_history, file.path(out_dir, "Final_audit_history.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$final_audit_event_history) && nrow(ds$results$final_audit_event_history) > 0) write_csv_safe(ds$results$final_audit_event_history, file.path(out_dir, "Final_audit_event_history.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  task_events_out <- stpd_normalize_task_events(ds$task_events %||% data.frame(), source = ds$meta$display_name %||% "")
	  if (nrow(task_events_out) > 0) write_csv_safe(task_events_out, file.path(out_dir, "Task_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$possible_burst_promotion_audit) && nrow(ds$results$possible_burst_promotion_audit) > 0) write_csv_safe(ds$results$possible_burst_promotion_audit, file.path(out_dir, "Possible_burst_promotion_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$possible_burst_promotion_summary) && nrow(ds$results$possible_burst_promotion_summary) > 0) write_csv_safe(ds$results$possible_burst_promotion_summary, file.path(out_dir, "Possible_burst_promotion_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  write_governance_exports(ds, params, out_dir)
  if (!is.null(ds$results$scientific_validation)) {
    stpd_write_scientific_validation_exports(ds$results$scientific_validation, out_dir)
  }
  invisible(ds)
}

stpd_call_progress <- function(progress_callback, phase, ..., detail = NULL) {
  if (!is.function(progress_callback)) return(invisible(NULL))
  tryCatch(
    progress_callback(phase = phase, ..., detail = detail),
    error = function(e) NULL
  )
  invisible(NULL)
}

run_detector_dataset_internal <- function(ds, params, selected_trains = NULL, lock_manual = TRUE, collect_diagnostics = TRUE,
                                          progress_callback = NULL) {
  # Unified detector API for Shiny, batch, and command-line use.
  params <- effective_params_for_detector(params)
  if (!is.null(ds) && is.null(ds$trains) && is.list(ds) && length(ds) > 0 &&
      all(vapply(ds, function(x) is.data.frame(x) && all(c("idx", "timestamp_sec", "ISI_sec") %in% names(x)), logical(1)))) {
    ds <- make_dataset(name = "dataset", source = "trains_list", trains = ds, unit_in = "s")
  }
  if (is.null(ds) || is.null(ds$trains)) stop("Dataset has no trains.", call. = FALSE)
  if (is.null(ds$results)) ds$results <- list()
  if (is.null(ds$meta)) ds$meta <- list(display_name = "dataset", unit_in = "s")
  if (is.null(ds$train_settings)) ds$train_settings <- list(burst_isi_ranges = list(), tonic_isi_ranges = list(), pause_isi_ranges = list(), highfreq_isi_ranges = list(), isi_thresholds = list())
  if (is.null(ds$train_settings$isi_thresholds)) ds$train_settings$isi_thresholds <- list()
  params <- merge_train_isi_thresholds_into_params(params, ds$train_settings$isi_thresholds)
  td <- ds$trains
  target_trains <- selected_trains %||% names(td)
  target_trains <- intersect(target_trains, names(td))
  if (length(target_trains) == 0) stop("No target trains found.", call. = FALSE)

  structure_diag_parts <- list(); seed_diag_parts <- list(); bridge_diag_parts <- list()
  burst_raw_parts <- list(); burst_final_parts <- list(); pause_diag_parts <- list(); posthoc_fragments_parts <- list(); candidate_audit_parts <- list()
  min_isi <- params$detector$min_valid_isi_sec %||% 0.0009
  run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  phash <- compute_params_hash(params)

  total_trains <- length(target_trains)
  for (ii in seq_along(target_trains)) {
    tr <- target_trains[[ii]]
    stpd_call_progress(
      progress_callback,
      "train_start",
      train = tr,
      index = ii,
      total = total_trains,
      detail = paste0("Detecting train ", ii, "/", total_trains, ": ", tr)
    )
    td[[tr]] <- run_detector_one_train(td[[tr]], params, min_isi_sec = min_isi, train = tr, lock_manual = lock_manual)
    pf <- attr(td[[tr]], "posthoc_fragment_audit")
    if (!is.null(pf) && nrow(pf) > 0) { pf$train <- tr; posthoc_fragments_parts[[length(posthoc_fragments_parts) + 1L]] <- pf }
    va <- attr(td[[tr]], "candidate_diagnostic_audit")
    if (!is.null(va) && nrow(va) > 0) { va$train <- tr; candidate_audit_parts[[length(candidate_audit_parts) + 1L]] <- va }
    attr(td[[tr]], "auto_run_id") <- run_id
    attr(td[[tr]], "auto_params_hash") <- phash
    if (collect_diagnostics) {
      dg <- attr(td[[tr]], "seed_bridge_diag")
      if (!is.null(dg)) {
        if (!is.null(dg$structures) && nrow(dg$structures) > 0) { tmp <- dg$structures; tmp$train <- tr; structure_diag_parts[[length(structure_diag_parts) + 1L]] <- tmp }
        if (!is.null(dg$seeds) && nrow(dg$seeds) > 0) { tmp <- dg$seeds; tmp$train <- tr; seed_diag_parts[[length(seed_diag_parts) + 1L]] <- tmp }
        if (!is.null(dg$bridges) && nrow(dg$bridges) > 0) { tmp <- dg$bridges; tmp$train <- tr; bridge_diag_parts[[length(bridge_diag_parts) + 1L]] <- tmp }
        if (!is.null(dg$raw_burst_candidates) && nrow(dg$raw_burst_candidates) > 0) { tmp <- dg$raw_burst_candidates; tmp$train <- tr; burst_raw_parts[[length(burst_raw_parts) + 1L]] <- tmp }
        if (!is.null(dg$final_burst_candidates) && nrow(dg$final_burst_candidates) > 0) { tmp <- dg$final_burst_candidates; tmp$train <- tr; burst_final_parts[[length(burst_final_parts) + 1L]] <- tmp }
      }
      pb <- attr(td[[tr]], "pause_diag")
      if (!is.null(pb) && nrow(pb) > 0) { tmp <- pb; tmp$train <- tr; pause_diag_parts[[length(pause_diag_parts) + 1L]] <- tmp }
    }
    stpd_call_progress(
      progress_callback,
      "train_done",
      train = tr,
      index = ii,
      total = total_trains,
      detail = paste0("Finished train ", ii, "/", total_trains, ": ", tr)
    )
  }

  stpd_call_progress(progress_callback, "assemble_events", detail = "Rebuilding event tables")
  ds$trains <- td
  ds$params_last <- params
  ds$results$run_metadata <- data.frame(
    run_id = run_id,
    run_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    params_hash = phash,
    run_scope = if (length(target_trains) < length(td)) "selected_trains_only" else "all_trains",
    selected_trains = paste(target_trains, collapse = ";"),
    refractory_suspect_action = params$detector$refractory_suspect_action %||% params$burst$refractory_suspect_action %||% "",
    tonic_like_policy = params$burst$final_tonic_like_action %||% "",
    possible_burst_policy = paste0("label_possible_burst=", isTRUE(params$burst$label_possible_burst),
                                   "; label_boundary_possible_burst=", isTRUE(params$burst$label_boundary_possible_burst)),
    analysis_role = params$detector$analysis_role %||% "candidate_event_generator_plus_review",
    preset_name = params$detector$preset_name %||% "balanced_single_unit",
    review_required_for_publication = isTRUE(params$detector$require_human_or_model_review_for_publication),
    stringsAsFactors = FALSE
  )
  ds$quality <- validate_dataset_quality_impl(td, min_isi_sec = min_isi, unit_hint = ds$meta$unit_in %||% "s",
                                                 refractory_suspect_sec = params$detector$refractory_suspect_sec %||% 0.0010)
  event_trains <- td[target_trains]
  ev <- derive_interval_tables(event_trains, source = "final", auto_others = FALSE,
                               dataset_map = setNames(rep(ds$meta$display_name %||% "dataset", length(event_trains)), names(event_trains)),
                               min_isi_sec = min_isi,
                               contrast_q = params$burst$contrast_q %||% 0.90,
                               context_k = params$burst$context_k %||% 5L)$events
  if (length(target_trains) < length(td) && nrow(ev) > 0) ev$run_scope <- "selected_trains_only"
  ev <- enrich_events_with_pause_thresholds(ev, td, run_id = run_id, params_hash = phash)
  ds$results$events <- ev

  if (collect_diagnostics) {
    stpd_call_progress(progress_callback, "diagnostics", detail = "Collecting diagnostics")
    ds$results$structure_candidates <- add_run_columns(if (length(structure_diag_parts) > 0) bind_rows(structure_diag_parts) else empty_structure_candidates_tbl(), run_id, phash)
    ds$results$seed_candidates <- add_run_columns(if (length(seed_diag_parts) > 0) bind_rows(seed_diag_parts) else empty_seed_candidates_tbl(), run_id, phash)
    ds$results$bridge_candidates <- add_run_columns(if (length(bridge_diag_parts) > 0) bind_rows(bridge_diag_parts) else empty_bridge_candidates_tbl(), run_id, phash)
    ds$results$burst_candidates_raw <- add_run_columns(if (length(burst_raw_parts) > 0) bind_rows(burst_raw_parts) else empty_burst_candidates_tbl(), run_id, phash)
    ds$results$burst_candidates_final <- add_run_columns(if (length(burst_final_parts) > 0) bind_rows(burst_final_parts) else empty_burst_candidates_tbl(), run_id, phash)
    ds$results$burst_candidates <- ds$results$burst_candidates_final
    if (nrow(ds$results$seed_candidates) == 0 && nrow(ds$results$bridge_candidates) == 0) {
      sb_diag <- stpd_seed_bridge_diagnostics_for_dataset(
        ds, params,
        min_isi_sec = min_isi,
        target_trains = target_trains,
        run_id = run_id,
        params_hash = phash
      )
      if (nrow(ds$results$structure_candidates) == 0) ds$results$structure_candidates <- sb_diag$structures
      ds$results$seed_candidates <- sb_diag$seeds
      ds$results$bridge_candidates <- sb_diag$bridges
      if (nrow(ds$results$burst_candidates_raw) == 0) ds$results$burst_candidates_raw <- sb_diag$candidates
      if (nrow(ds$results$burst_candidates) == 0) ds$results$burst_candidates <- sb_diag$candidates
    }
    if (exists("stpd_append_burst_sublabel_structures_for_dataset", mode = "function")) {
      ds$results$structure_candidates <- stpd_append_burst_sublabel_structures_for_dataset(
        ds$results$structure_candidates,
        td,
        effective_burst_params(params),
        min_isi_sec = min_isi,
        target_trains = target_trains,
        run_id = run_id,
        params_hash = phash
      )
    }
    ds$results$pause_candidates <- add_run_columns(if (length(pause_diag_parts) > 0) bind_rows(pause_diag_parts) else data.frame(), run_id, phash)
    ds$results$posthoc_fragment_audit <- add_run_columns(if (length(posthoc_fragments_parts) > 0) bind_rows(posthoc_fragments_parts) else data.frame(), run_id, phash)
    ds$results$candidate_diagnostic_audit <- add_run_columns(if (length(candidate_audit_parts) > 0) bind_rows(candidate_audit_parts) else data.frame(), run_id, phash)
    # Keep diagnostic candidate windows separate from the public candidate ledger.
    ds$results$near_miss_candidates <- add_run_columns(build_near_miss_table(ds, params, min_isi_sec = min_isi, target_trains = target_trains), run_id, phash)
  }

  stpd_call_progress(progress_callback, "ledger", detail = "Rebuilding candidate and event ledgers")
  ds$results$candidate_ledger <- build_candidate_ledger_internal(ds, params, selected_trains = target_trains, run_id = run_id, params_hash = phash)
  ds$results$event_ledger <- build_event_ledger_internal(ds, params, selected_trains = target_trains, run_id = run_id, params_hash = phash)
  ds$results$event_audit <- ds$results$event_ledger
  layers <- result_layers_from_events(ds$results$events)
  ds$results$events_high_confidence <- add_run_columns(layers$high_confidence, run_id, phash)
  ds$results$events_review_candidates <- add_run_columns(layers$review_candidates, run_id, phash)
  ds$results$events_burst_family <- add_run_columns(layers$burst_family, run_id, phash)
  ds$results$events_all_family_map <- add_run_columns(layers$all_event_family_map, run_id, phash)

  # schema: candidate-feature-table is the single post-processing input.
  # Candidate ledger is still retained for audit, but downstream final-classification
  # audit, biological warnings and reporting layers read candidate_features_internal rather
  # than recomputing candidate metrics ad hoc.
  stpd_call_progress(progress_callback, "features", detail = "Computing candidate features")
  cf <- compute_candidate_feature_table(ds, candidates = ds$results$candidate_ledger, params = params, selected_trains = target_trains)
  ds$results$candidate_features_internal <- add_run_columns(cf, run_id, phash)
  ds$results$candidate_features <- ds$results$candidate_features_internal
  stpd_call_progress(progress_callback, "final_audits", detail = "Computing final classification audits")
  ds$results$final_decisions_internal <- add_run_columns(final_classify_candidates(ds$results$candidate_features_internal, params), run_id, phash)
  ds$results$final_classification_audit <- ds$results$final_decisions_internal
  ds$results$post_processing_input <- "candidate_features_internal"
  ds <- stpd_add_distributional_results(ds, params = params, selected_trains = target_trains, candidates = ds$results$candidate_features_internal)
  stpd_call_progress(progress_callback, "report_tables", detail = "Building validation and report tables")
  ds$results$validation_guidance <- validation_guidance(ds, params)
  ds$results$consistency_audit <- consistency_audit(ds, params)
  ds$results$semantic_consistency_report <- ds$results$consistency_audit
  ds$results$governance_summary <- params_governance_summary(params)
  ds$results$parameters_report <- parameter_report_table(params)
  ds$results$stationarity_qc <- tryCatch(stationarity_qc(td, min_isi_sec = min_isi), error = function(e) data.frame())
  ds$results$overfit_warning_report <- overfit_warning_report(ds, params)
  ds$results$development_roadmap_table <- development_roadmap()
  stpd_call_progress(progress_callback, "complete", detail = "Detector result layers are synchronized")
  ds
}

run_detector_file <- function(input_csv, params = default_params_sec(), output_dir = tempdir(),
                                      mode = c("raw", "labeled"), unit_in = c("s", "ms"), header = TRUE,
                                      lock_manual = TRUE, collect_diagnostics = TRUE,
                                      duplicate_policy = c("error_keep", "warn_keep", "collapse_exact")) {
  mode <- match.arg(mode)
  unit_in <- match.arg(unit_in)
  duplicate_policy <- match.arg(duplicate_policy)
  base <- tools::file_path_sans_ext(basename(input_csv))
  trains <- if (mode == "raw") {
    build_trains_from_raw(input_csv, header = header, unit_in = unit_in, duplicate_policy = duplicate_policy)
  } else {
    build_trains_from_annot(input_csv, unit_in = unit_in, duplicate_policy = duplicate_policy)
  }
  task_events <- if (mode == "raw") {
    tryCatch(stpd_extract_task_events_from_raw(input_csv, header = header, unit_in = unit_in), error = function(e) stpd_empty_task_events())
  } else {
    tryCatch(stpd_extract_task_events_from_raw(input_csv, header = TRUE, unit_in = unit_in), error = function(e) stpd_empty_task_events())
  }
  min_isi <- params$detector$min_valid_isi_sec %||% 0.0009
  trains <- precompute_trains_isi_percentiles(trains, min_isi_sec = min_isi, force = TRUE)
  ds <- make_dataset(name = base, source = mode, trains = trains, unit_in = unit_in, task_events = task_events)
  ds$quality <- validate_dataset_quality_impl(trains, min_isi_sec = min_isi, unit_hint = unit_in, refractory_suspect_sec = params$detector$refractory_suspect_sec %||% 0.0010)
  ds <- run_detector_dataset_internal(ds, params, selected_trains = names(ds$trains), lock_manual = lock_manual, collect_diagnostics = collect_diagnostics)
  export_detection_results_simple(ds, params, out_dir = file.path(output_dir, base), dataset_name = base, time_unit = "ms")
  ds
}

batch_run_detector <- function(file_list, params = default_params_sec(), output_dir = tempdir(),
                                       mode = c("raw", "labeled"), unit_in = c("s", "ms"), header = TRUE,
                                       duplicate_policy = c("error_keep", "warn_keep", "collapse_exact")) {
  mode <- match.arg(mode); unit_in <- match.arg(unit_in); duplicate_policy <- match.arg(duplicate_policy)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  res <- lapply(file_list, function(f) run_detector_file(f, params = params, output_dir = output_dir, mode = mode, unit_in = unit_in, header = header, duplicate_policy = duplicate_policy))
  names(res) <- tools::file_path_sans_ext(basename(file_list))
  invisible(res)
}

evaluate_detector_against_manual <- function(ds, params, selected_trains = NULL, min_isi_sec = NULL, use_learned_ranges = TRUE, metric_mode = c("strict_high_confidence", "candidate_family", "review_assisted")) {
  metric_mode <- match.arg(metric_mode)
  eval_mode <- if (isTRUE(use_learned_ranges)) "current_calibrated_detector_with_learned_ranges" else "range_blinded_detector_without_learned_train_specific_ranges"
  eval_meta <- data.frame(
    evaluation_mode = eval_mode,
    metric_mode = metric_mode,
    learned_ranges_used = isTRUE(use_learned_ranges),
    possible_burst_handling = if (metric_mode == "candidate_family") "burst and possible_burst are merged into burst_family; this estimates candidate-generation sensitivity, not high-confidence classifier performance." else "possible_burst is kept as a separate review class and is not counted as high-confidence burst.",
    interpretation = if (isTRUE(use_learned_ranges))
      "Calibration-style report: the shadow detector uses current learned train-specific ranges. This is appropriate for parameter tuning, not an unbiased held-out validation."
    else
      "Range-blinded report: learned train-specific burst/tonic/pause ranges are disabled during the shadow detector pass.",
    stringsAsFactors = FALSE
  )
  if (is.null(ds) || is.null(ds$trains)) return(list(confusion = data.frame(), metrics = data.frame(), events = data.frame(), meta = eval_meta))
  min_isi <- min_isi_sec %||% params$detector$min_valid_isi_sec %||% 0.0009
  params_eval <- if (isTRUE(use_learned_ranges)) params else strip_learned_ranges_for_eval(params)
  td <- ds$trains
  target <- selected_trains %||% names(td)
  target <- intersect(target, names(td))
  classes <- setdiff(result_metric_classes(metric_mode), "unlabeled")
  conf_parts <- list(); event_parts <- list()
  for (tr in target) {
    dat <- td[[tr]]
    if (is.null(dat) || nrow(dat) <= 1 || is.null(dat$pattern_manual)) next
    truth <- pattern_eval_normalize(dat$pattern_manual, metric_mode = metric_mode)
    valid <- is.finite(dat$ISI_sec) & dat$ISI_sec >= min_isi & dat$idx >= 2 & truth != "unlabeled"
    if (!any(valid)) next
    shadow <- run_detector_one_train(dat, params_eval, min_isi_sec = min_isi, train = tr, lock_manual = FALSE)
    pred <- pattern_eval_normalize(shadow$pattern_auto, metric_mode = metric_mode)
    conf_parts[[length(conf_parts) + 1L]] <- tibble(train = tr, truth = truth[valid], prediction = pred[valid])
    event_parts[[length(event_parts) + 1L]] <- manual_event_overlap(truth, pred, train = tr, metric_mode = metric_mode)
  }
  conf_long <- if (length(conf_parts) > 0) bind_rows(conf_parts) else tibble(train = character(), truth = character(), prediction = character())
  if (nrow(conf_long) == 0) return(list(confusion = data.frame(), metrics = data.frame(), events = data.frame(), meta = eval_meta))
  confusion <- conf_long %>% count(truth, prediction, name = "n") %>% arrange(truth, prediction)
  metrics <- lapply(classes, function(cls) {
    tp <- sum(conf_long$truth == cls & conf_long$prediction == cls, na.rm = TRUE)
    truth_n <- sum(conf_long$truth == cls, na.rm = TRUE)
    pred_n <- sum(conf_long$prediction == cls, na.rm = TRUE)
    recall <- if (truth_n > 0) tp / truth_n else NA_real_
    precision <- if (pred_n > 0) tp / pred_n else NA_real_
    f1 <- if (is.finite(recall) && is.finite(precision) && (recall + precision) > 0) 2 * recall * precision / (recall + precision) else NA_real_
    tibble(pattern = cls, truth_n = truth_n, predicted_n_on_manual_subset = pred_n, true_positive_n = tp,
           recall_on_manual_subset = recall, precision_on_manual_subset = precision, F1_on_manual_subset = f1)
  }) %>% bind_rows()
  events <- if (length(event_parts) > 0) bind_rows(event_parts) else data.frame()
  list(confusion = confusion, metrics = metrics, events = events, meta = eval_meta)
}

export_detection_results_simple <- function(ds, params, out_dir, dataset_name = "dataset", time_unit = "ms") {
  params <- effective_params_for_detector(params)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(ds$results)) ds$results <- list()
  td <- ds$trains
  min_isi <- params$detector$min_valid_isi_sec %||% 0.0009
  bundle <- derive_interval_tables(td, source = "final", auto_others = FALSE,
                                   dataset_map = setNames(rep(dataset_name, length(td)), names(td)),
                                   min_isi_sec = min_isi,
                                   contrast_q = params$burst$contrast_q %||% 0.90,
                                   context_k = params$burst$context_k %||% 5L)
  ev_final <- if (!is.null(ds$results$events) && nrow(ds$results$events) > 0) ds$results$events else enrich_events_with_pause_thresholds(bundle$events, td, run_id = (ds$results$run_metadata$run_id %||% "export_run")[1], params_hash = (ds$results$run_metadata$params_hash %||% compute_params_hash(params))[1])
  write_csv_safe(ev_final, file.path(out_dir, "Events_final.csv"))
  if (!is.null(ev_final) && nrow(ev_final) > 0 && "pattern" %in% names(ev_final)) {
    lb_ev <- ev_final[as.character(ev_final$pattern) == "long_burst", , drop = FALSE]
    if (nrow(lb_ev) > 0) write_csv_safe(lb_ev, file.path(out_dir, "Long_burst_events.csv"))
  }
  write_csv_safe(bundle$labels, file.path(out_dir, "ISI_labels_final.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$quality) && nrow(ds$quality) > 0) write_csv_safe(ds$quality, file.path(out_dir, "Data_quality_QC.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  dup_details <- tryCatch(duplicate_timestamp_details(ds$trains, display_unit = time_unit), error = function(e) data.frame())
  if (!is.null(dup_details) && nrow(dup_details) > 0) write_csv_safe(dup_details, file.path(out_dir, "Duplicate_timestamp_details.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  art_details <- tryCatch(artifact_isi_details(ds$trains, min_isi_sec = params$detector$min_valid_isi_sec %||% 0.0009), error = function(e) data.frame())
  if (!is.null(art_details) && nrow(art_details) > 0) write_csv_safe(art_details, file.path(out_dir, "Artifact_ISI_details.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$structure_candidates) && nrow(ds$results$structure_candidates) > 0) write_csv_safe(ds$results$structure_candidates, file.path(out_dir, "Structure_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$seed_candidates) && nrow(ds$results$seed_candidates) > 0) write_csv_safe(ds$results$seed_candidates, file.path(out_dir, "Seed_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$bridge_candidates) && nrow(ds$results$bridge_candidates) > 0) write_csv_safe(ds$results$bridge_candidates, file.path(out_dir, "Bridge_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$burst_candidates) && nrow(ds$results$burst_candidates) > 0) write_csv_safe(ds$results$burst_candidates, file.path(out_dir, "Burst_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$burst_candidates_raw) && nrow(ds$results$burst_candidates_raw) > 0) write_csv_safe(ds$results$burst_candidates_raw, file.path(out_dir, "Burst_candidates_raw.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$burst_candidates_final) && nrow(ds$results$burst_candidates_final) > 0) write_csv_safe(ds$results$burst_candidates_final, file.path(out_dir, "Burst_candidates_final.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$pause_candidates) && nrow(ds$results$pause_candidates) > 0) write_csv_safe(ds$results$pause_candidates, file.path(out_dir, "Pause_candidates_with_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$posthoc_fragment_audit) && nrow(ds$results$posthoc_fragment_audit) > 0) write_csv_safe(ds$results$posthoc_fragment_audit, file.path(out_dir, "Posthoc_fragment_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$candidate_diagnostic_audit) && nrow(ds$results$candidate_diagnostic_audit) > 0) write_csv_safe(ds$results$candidate_diagnostic_audit, file.path(out_dir, "Candidate_diagnostic_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$final_audit_summary) && nrow(ds$results$final_audit_summary) > 0) write_csv_safe(ds$results$final_audit_summary, file.path(out_dir, "Final_audit_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$final_audit_events) && nrow(ds$results$final_audit_events) > 0) write_csv_safe(ds$results$final_audit_events, file.path(out_dir, "Final_audit_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$final_audit_history) && nrow(ds$results$final_audit_history) > 0) write_csv_safe(ds$results$final_audit_history, file.path(out_dir, "Final_audit_history.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$final_audit_event_history) && nrow(ds$results$final_audit_event_history) > 0) write_csv_safe(ds$results$final_audit_event_history, file.path(out_dir, "Final_audit_event_history.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  task_events_out <- stpd_normalize_task_events(ds$task_events %||% data.frame(), source = ds$meta$display_name %||% "")
	  if (nrow(task_events_out) > 0) write_csv_safe(task_events_out, file.path(out_dir, "Task_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$possible_burst_promotion_audit) && nrow(ds$results$possible_burst_promotion_audit) > 0) write_csv_safe(ds$results$possible_burst_promotion_audit, file.path(out_dir, "Possible_burst_promotion_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$possible_burst_promotion_summary) && nrow(ds$results$possible_burst_promotion_summary) > 0) write_csv_safe(ds$results$possible_burst_promotion_summary, file.path(out_dir, "Possible_burst_promotion_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	  if (!is.null(ds$results$near_miss_candidates) && nrow(ds$results$near_miss_candidates) > 0) write_csv_safe(ds$results$near_miss_candidates, file.path(out_dir, "Near_miss_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$train_settings$burst_isi_ranges) && length(ds$train_settings$burst_isi_ranges) > 0) {
    write_csv_safe(train_range_dataframe(ds$train_settings$burst_isi_ranges, pattern = "burst", factor = 1, unit = "s"), file.path(out_dir, "Train_burst_ISI_ranges.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$train_settings$tonic_isi_ranges) && length(ds$train_settings$tonic_isi_ranges) > 0) {
    write_csv_safe(train_range_dataframe(ds$train_settings$tonic_isi_ranges, pattern = "tonic", factor = 1, unit = "s"), file.path(out_dir, "Train_tonic_ISI_ranges.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$train_settings$pause_isi_ranges) && length(ds$train_settings$pause_isi_ranges) > 0) {
    write_csv_safe(train_range_dataframe(ds$train_settings$pause_isi_ranges, pattern = "pause", factor = 1, unit = "s"), file.path(out_dir, "Train_pause_ISI_ranges.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$train_settings$highfreq_isi_ranges) && length(ds$train_settings$highfreq_isi_ranges) > 0) {
    write_csv_safe(train_range_dataframe(ds$train_settings$highfreq_isi_ranges, pattern = "highfreq", factor = 1, unit = "s"), file.path(out_dir, "Train_highfreq_ISI_anchors.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$train_settings$isi_thresholds) && length(ds$train_settings$isi_thresholds) > 0) {
    write_csv_safe(train_isi_threshold_dataframe(ds$train_settings$isi_thresholds, factor = if (identical(time_unit, "ms")) 1000 else 1, unit = time_unit), file.path(out_dir, "Train_specific_ISI_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  write_tiered_result_exports(ds, params, out_dir)
  if (!is.null(ds$results$run_metadata) && nrow(ds$results$run_metadata) > 0) write_csv_safe(ds$results$run_metadata, file.path(out_dir, "Detector_run_metadata.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$event_ledger) && nrow(ds$results$event_ledger %||% data.frame()) > 0) {
    write_csv_safe(ds$results$event_ledger, file.path(out_dir, "Event_audit.csv"))
    write_csv_safe(ds$results$event_ledger, file.path(out_dir, "Events_final_event_ledger.csv"))
  }
  if (!is.null(ds$results$events_burst_family) && nrow(ds$results$events_burst_family %||% data.frame()) > 0) {
    write_csv_safe(ds$results$events_burst_family, file.path(out_dir, "Events_burst_family_candidates.csv"))
  }
  if (!is.null(ds$results$events_all_family_map) && nrow(ds$results$events_all_family_map %||% data.frame()) > 0) {
    write_csv_safe(ds$results$events_all_family_map, file.path(out_dir, "Events_all_with_pattern_family.csv"))
    write_csv_safe(ds$results$events_all_family_map, file.path(out_dir, "Events_all_family_map.csv"))
  }
  writeLines(capture.output(str(params)), file.path(out_dir, "Detector_params.txt"))
  invisible(out_dir)
}
