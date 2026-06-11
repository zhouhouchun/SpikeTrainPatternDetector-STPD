# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# seed-bridge seed-bridge burst detector
# ============================================================

empty_burst_candidates_tbl <- function() {
  tibble(
    candidate_id = integer(), train = character(), class = character(), start_isi = integer(), end_isi = integer(),
    start_spike_idx = integer(), end_spike_idx = integer(), n_isi = integer(), n_spikes = integer(),
    start_time_sec = numeric(), end_time_sec = numeric(), duration_sec = numeric(),
    seed_count = integer(), bridge_count = integer(), seed_ids = character(), bridge_ids = character(),
    core_q_sec = numeric(), seed_core_q_sec = numeric(), max_ISI_sec = numeric(), mean_ISI_sec = numeric(), MM = numeric(), LV = numeric(), CV = numeric(),
    pre_edge_ISI_sec = numeric(), post_edge_ISI_sec = numeric(),
    edge_contrast_min_q = numeric(), edge_contrast_geom_q = numeric(),
    edge_pre_ratio_seed_q = numeric(), edge_post_ratio_seed_q = numeric(),
    edge_contrast_min_seed_q = numeric(), edge_contrast_geom_seed_q = numeric(),
    score = numeric(), accepted = logical(), reject_reason = character(),
    final_tonic_like = logical(), final_tonic_action = character(),
    required_final_edge_min = numeric(), required_final_edge_geom = numeric(), required_score_high = numeric(),
    local_compression_burst = logical(), local_median_core_ratio = numeric(), seed_core_q_pct = numeric()
  )
}

calc_edge_contrast_stats <- function(isi, s_isi, e_isi, min_isi_sec = 0.001, robust_q = 0.90) {
  n <- length(isi)
  empty <- list(
    core_max = NA_real_, core_q = NA_real_, pre = NA_real_, post = NA_real_,
    pre_ratio_q = NA_real_, post_ratio_q = NA_real_, contrast_min_q = NA_real_,
    contrast_geom_q = NA_real_, contrast_pct_q = NA_real_, n_flank = 0L,
    pre_ratio_max = NA_real_, post_ratio_max = NA_real_, contrast_min_max = NA_real_, contrast_geom_max = NA_real_
  )
  if (!is.finite(s_isi) || !is.finite(e_isi) || s_isi < 2 || e_isi > n || e_isi < s_isi) return(empty)
  vals <- valid_isi_values(isi[s_isi:e_isi], min_isi_sec)
  if (length(vals) == 0) return(empty)
  robust_q <- clamp(robust_q, 0.50, 1.00)
  core_max <- max(vals)
  core_q <- as.numeric(stats::quantile(vals, robust_q, na.rm = TRUE, names = FALSE))
  pre <- if (s_isi > 2) isi[s_isi - 1L] else NA_real_
  post <- if (e_isi < n) isi[e_isi + 1L] else NA_real_
  pre <- if (is.finite(pre) && pre >= min_isi_sec) pre else NA_real_
  post <- if (is.finite(post) && post >= min_isi_sec) post else NA_real_
  qsum <- calc_ratio_summary(pre, post, core_q)
  msum <- calc_ratio_summary(pre, post, core_max)
  list(
    core_max = core_max, core_q = core_q, pre = pre, post = post,
    pre_ratio_q = qsum$pre_ratio, post_ratio_q = qsum$post_ratio,
    contrast_min_q = qsum$contrast_min, contrast_geom_q = qsum$contrast_geom,
    contrast_pct_q = qsum$contrast_pct, n_flank = qsum$n_flank,
    pre_ratio_max = msum$pre_ratio, post_ratio_max = msum$post_ratio,
    contrast_min_max = msum$contrast_min, contrast_geom_max = msum$contrast_geom
  )
}

split_seed_block <- function(isi, s_isi, e_isi, p, min_isi_sec = 0.001) {
  if (e_isi < s_isi) return(data.frame(start_isi = integer(0), end_isi = integer(0)))
  min_n <- max(1L, safe_int(p$seed_min_isi_n %||% 2L, 2L))
  max_n <- max(min_n, safe_int(p$seed_max_isi_n %||% 8L, 8L))
  idx <- s_isi:e_isi
  vals <- valid_isi_values(isi[idx], min_isi_sec)
  if (length(vals) == 0) return(data.frame(start_isi = integer(0), end_isi = integer(0)))
  split_ratio <- suppressWarnings(as.numeric(p$seed_internal_bridge_split_ratio %||% 1.80))
  split_ratio <- max(1.10, split_ratio)
  block_vals <- isi[idx]
  med <- safe_median(block_vals[is.finite(block_vals) & block_vals >= min_isi_sec], default = NA_real_)
  split_points <- integer(0)
  if (is.finite(med) && med > 0 && length(idx) >= (2L * min_n + 1L)) {
    cand <- idx[is.finite(block_vals) & block_vals >= min_isi_sec & block_vals >= med * split_ratio]
    for (j in cand) {
      left_n <- j - s_isi
      right_n <- e_isi - j
      if (left_n >= min_n && right_n >= min_n) split_points <- c(split_points, j)
    }
  }
  # Split at putative internal bridge ISIs; those ISIs become bridge candidates later.
  parts <- list()
  cur_s <- s_isi
  for (sp in sort(unique(split_points))) {
    cur_e <- sp - 1L
    if (cur_e >= cur_s && (cur_e - cur_s + 1L) >= min_n) parts[[length(parts) + 1L]] <- c(cur_s, cur_e)
    cur_s <- sp + 1L
  }
  if (e_isi >= cur_s && (e_isi - cur_s + 1L) >= min_n) parts[[length(parts) + 1L]] <- c(cur_s, e_isi)
  if (length(parts) == 0) {
    if ((e_isi - s_isi + 1L) >= min_n) parts[[1]] <- c(s_isi, e_isi)
  }
  if (length(parts) == 0) return(data.frame(start_isi = integer(0), end_isi = integer(0)))
  raw <- as.data.frame(do.call(rbind, parts))
  colnames(raw) <- c("start_isi", "end_isi")
  
  # If a very long candidate remains, tile it into high-recall seed windows.
  tiled <- list()
  for (ii in seq_len(nrow(raw))) {
    a <- raw$start_isi[ii]; b <- raw$end_isi[ii]
    len <- b - a + 1L
    if (len <= max_n) {
      tiled[[length(tiled) + 1L]] <- c(a, b)
    } else {
      starts <- seq(a, b - min_n + 1L, by = max(1L, max_n))
      for (st in starts) {
        en <- min(b, st + max_n - 1L)
        if ((en - st + 1L) >= min_n) tiled[[length(tiled) + 1L]] <- c(st, en)
      }
    }
  }
  out <- as.data.frame(do.call(rbind, tiled))
  colnames(out) <- c("start_isi", "end_isi")
  unique(out)
}

# [refined] Removed duplicate earlier definition of mine_burst_seeds; final definition retained below.

mine_seed_bridges <- function(dat, seeds, p, min_isi_sec = 0.001, train = "") {
  if (nrow(seeds) <= 1) return(empty_bridge_candidates_tbl())
  seeds <- seeds %>% arrange(start_isi, end_isi)
  rows <- list()
  bid <- 1L
  for (ii in seq_len(nrow(seeds) - 1L)) {
    br <- bridge_row(dat, seeds, ii, ii + 1L, bridge_id = bid, train = train, p = p, min_isi_sec = min_isi_sec)
    if (nrow(br) > 0) {
      rows[[length(rows) + 1L]] <- br
      bid <- bid + 1L
    }
  }
  if (length(rows) == 0) return(empty_bridge_candidates_tbl())
  out <- bind_rows(rows)
  max_keep <- safe_int(p$seed_bridge_max_bridge_candidates %||% 1200L, 1200L)
  if (nrow(out) > max_keep) {
    out <- out %>% arrange(desc(bridge_score), bridge_start_isi) %>% head(max_keep) %>% arrange(bridge_start_isi)
    out$bridge_id <- seq_len(nrow(out))
  }
  out
}

union_find_components <- function(seed_ids, bridges) {
  parent <- setNames(as.integer(seed_ids), as.character(seed_ids))
  find <- function(x) {
    x <- as.integer(x)
    while (parent[as.character(x)] != x) {
      parent[as.character(x)] <<- parent[as.character(parent[as.character(x)])]
      x <- parent[as.character(x)]
    }
    x
  }
  unite <- function(a, b) {
    ra <- find(a); rb <- find(b)
    if (ra != rb) parent[as.character(rb)] <<- ra
  }
  if (nrow(bridges) > 0) {
    acc <- bridges %>% filter(bridge_class == "accepted")
    if (nrow(acc) > 0) {
      for (ii in seq_len(nrow(acc))) unite(acc$left_seed_id[ii], acc$right_seed_id[ii])
    }
  }
  comp <- vapply(seed_ids, find, integer(1))
  split(as.integer(seed_ids), comp)
}

build_burst_candidates <- function(dat, seeds, bridges, p, min_isi_sec = 0.001, train = "") {
  if (nrow(seeds) == 0) return(empty_burst_candidates_tbl())
  comps <- union_find_components(seeds$seed_id, bridges)
  rows <- list()
  cid <- 1L
  for (ids in comps) {
    row <- candidate_row_from_component(dat, seeds, bridges, ids, candidate_id = cid, train = train, p = p, min_isi_sec = min_isi_sec)
    if (nrow(row) > 0) {
      rows[[length(rows) + 1L]] <- row
      cid <- cid + 1L
    }
  }
  if (length(rows) == 0) return(empty_burst_candidates_tbl())
  cand <- bind_rows(rows) %>% arrange(start_isi, end_isi)
  # Non-maximum suppression among overlapping components; accepted high-confidence burst has priority.
  if (nrow(cand) <= 1) return(cand)
  cand <- cand %>% mutate(priority = case_when(class == "long_burst" ~ 4L, class == "burst" ~ 3L, class == "possible_burst" ~ 2L, TRUE ~ 1L)) %>%
    arrange(desc(priority), desc(score), start_isi)
  keep <- rep(TRUE, nrow(cand))
  accepted_ranges <- list()
  for (ii in seq_len(nrow(cand))) {
    s <- cand$start_isi[ii]; e <- cand$end_isi[ii]
    overlap <- FALSE
    if (length(accepted_ranges) > 0) {
      for (rr in accepted_ranges) {
        if (!(e < rr[1] || s > rr[2])) { overlap <- TRUE; break }
      }
    }
    if (overlap) {
      keep[ii] <- FALSE
    } else {
      accepted_ranges[[length(accepted_ranges) + 1L]] <- c(s, e)
    }
  }
  cand <- cand[keep, , drop = FALSE] %>% arrange(start_isi, end_isi)
  cand$candidate_id <- seq_len(nrow(cand))
  cand$priority <- NULL
  cand
}

# [refined] Removed duplicate earlier definition of seed_bridge_diagnostics_train; final definition retained below.

# [refined] Removed duplicate earlier definition of detect_burst_train_seed_bridge; final definition retained below.

detect_burst_train <- function(dat, p, min_isi_sec = 0.001, train = "") {
  if (isTRUE(p$use_seed_bridge_model %||% TRUE)) {
    return(detect_burst_train_seed_bridge(dat, p, min_isi_sec = min_isi_sec, train = train))
  }
  detect_burst_train_sliding_context(dat, p, min_isi_sec = min_isi_sec)
}



