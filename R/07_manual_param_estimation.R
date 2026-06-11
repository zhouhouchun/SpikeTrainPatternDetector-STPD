# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Parameter estimation from manual labels
# ============================================================

estimate_params_from_manual_pool <- function(trains_pool,
                                             dataset_map,
                                             min_isi_sec = 0.001,
                                             logisi_mcv_sec = 0.1) {
  base <- default_params_sec()
  dd <- derive_interval_tables(
    trains_pool,
    source = "manual",
    auto_others = FALSE,
    dataset_map = dataset_map,
    min_isi_sec = min_isi_sec,
    contrast_q = base$burst$contrast_q,
    context_k = base$burst$context_k
  )
  ev <- dd$events
  ints <- dd$intervals
  
  burst_intra <- ints$intra_burst$value_sec %||% numeric(0)
  tonic_intra <- ints$intra_tonic$value_sec %||% numeric(0)
  pause_isi <- ints$pause_isi$value_sec %||% numeric(0)
  burst_pre <- ints$pre_burst$value_sec %||% numeric(0)
  burst_post <- ints$after_burst$value_sec %||% numeric(0)
  tonic_pre <- ints$pre_tonic$value_sec %||% numeric(0)
  tonic_post <- ints$after_tonic$value_sec %||% numeric(0)
  
  burst_ev <- ev %>% filter(pattern == "burst")
  tonic_ev <- ev %>% filter(pattern == "tonic")
  pause_ev <- ev %>% filter(pattern == "pause")
  
  ML_r <- vapply(trains_pool, function(dat) {
    estimate_mean_isi_threshold_train(dat$ISI_sec, min_isi_sec = min_isi_sec)
  }, numeric(1))
  ML_r <- ML_r[is.finite(ML_r)]
  
  LOG_details <- lapply(trains_pool, function(dat) {
    estimate_logisi_threshold_train_result(dat$ISI_sec, min_isi_sec = min_isi_sec, mcv_sec = logisi_mcv_sec)
  })
  LOG_all <- vapply(LOG_details, function(x) as.numeric(x$threshold_sec %||% NA_real_), numeric(1))
  LOG_unresolved_n <- sum(!vapply(LOG_details, function(x) isTRUE(x$accepted), logical(1)), na.rm = TRUE)
  LOG_r <- LOG_all[is.finite(LOG_all)]
  
  T_B_manual <- safe_q(burst_intra, 0.95, default = NA_real_)
  T_B_bridge0 <- safe_q(burst_intra, 0.98, default = NA_real_)
  prepost_bridge <- finite_num(c(burst_pre, burst_post))
  T_B_bridge <- suppressWarnings(max(c(T_B_bridge0, safe_q(prepost_bridge, 0.10, default = NA_real_)), na.rm = TRUE))
  if (!is.finite(T_B_bridge)) T_B_bridge <- T_B_manual
  edge_fallback <- if (is.finite(T_B_bridge)) T_B_bridge else if (is.finite(T_B_manual)) T_B_manual else 0.025
  T_B_edge_pre <- safe_q(burst_pre, 0.10, default = edge_fallback)
  T_B_edge_post <- safe_q(burst_post, 0.10, default = edge_fallback)
  
  T_B_MI <- if (length(ML_r) > 0) median(ML_r, na.rm = TRUE) else NA_real_
  T_B_log <- if (length(LOG_r) > 0) median(LOG_r, na.rm = TRUE) else NA_real_
  seed_candidates <- finite_num(c(T_B_manual, T_B_MI, T_B_log))
  T_B_seed <- if (length(seed_candidates) > 0) median(seed_candidates) else 0.020
  
  # High-recall protections: manual examples are often too typical.
  # Keep seed reasonably permissive, and use context score to recover precision.
  if (is.finite(T_B_seed) && is.finite(T_B_manual)) {
    T_B_seed <- max(T_B_seed, safe_q(burst_intra, 0.75, default = T_B_seed))
  }
  
  burst_ctx_min <- finite_num(burst_ev$contrast_min_ctx_q)
  burst_ctx_geom <- finite_num(burst_ev$contrast_geom_ctx_q)
  
  c_possible <- safe_q(burst_ctx_min, 0.05, default = base$burst$contrast_min_possible)
  c_high <- safe_q(burst_ctx_min, 0.10, default = base$burst$contrast_min_high)
  g_possible <- safe_q(burst_ctx_geom, 0.05, default = base$burst$contrast_geom_possible)
  g_high <- safe_q(burst_ctx_geom, 0.10, default = base$burst$contrast_geom_high)
  
  c_possible <- clamp(c_possible, 1.20, 1.60)
  c_high <- clamp(c_high, 1.45, 1.90)
  g_possible <- clamp(g_possible, 1.25, 1.75)
  g_high <- clamp(g_high, 1.45, 2.10)
  
  local_comp <- safe_q(burst_ctx_min, 0.20, default = base$burst$local_compression_min)
  local_comp <- clamp(local_comp, 1.25, 1.60)
  
  G_B_min <- 3L
  D_B_min <- safe_q(burst_ev$duration_sec, 0.02, default = 0)
  
  pause_fallback <- max(c(T_B_bridge, T_B_seed, 0.100), na.rm = TRUE)
  if (!is.finite(pause_fallback)) pause_fallback <- 0.100
  T_P_strong <- safe_q(pause_isi, 0.50, default = pause_fallback)
  T_P_seed <- safe_q(pause_isi, 0.10, default = pause_fallback)
  D_P_min <- safe_q(pause_ev$duration_sec, 0.05, default = 0)
  G_P_min <- if (nrow(pause_ev) > 0) max(2L, floor(as.numeric(stats::quantile(pause_ev$n_spikes, 0.05, na.rm = TRUE)))) else 2L
  
  T_T_min <- safe_q(tonic_intra, 0.05, default = 0.020)
  T_T_max <- safe_q(tonic_intra, 0.95, default = 0.060)
  T_LV_core <- safe_q(tonic_ev$LV, 0.95, default = 0.5)
  T_LV_pre <- safe_q(tonic_ev$Pre_LV, 0.95, default = T_LV_core)
  T_LV_post <- safe_q(tonic_ev$After_LV, 0.95, default = T_LV_core)
  G_T_min <- if (nrow(tonic_ev) > 0) max(3L, floor(as.numeric(stats::quantile(tonic_ev$n_spikes, 0.05, na.rm = TRUE)))) else 5L
  D_T_min <- safe_q(tonic_ev$duration_sec, 0.05, default = 0)
  
  base$burst$T_manual <- T_B_manual
  base$burst$T_MI <- T_B_MI
  base$burst$T_log <- T_B_log
  base$burst$T_log_method <- "pasquale_logisi"
  base$burst$T_log_status <- if (is.finite(T_B_log)) "resolved" else "threshold_unresolved"
  base$burst$T_log_resolved_n <- as.integer(length(LOG_r))
  base$burst$T_log_unresolved_n <- as.integer(LOG_unresolved_n)
  base$burst$T_seed <- T_B_seed
  base$burst$T_bridge <- T_B_bridge
  base$burst$T_edge_pre <- T_B_edge_pre
  base$burst$T_edge_post <- T_B_edge_post
  base$burst$G_min <- as.integer(G_B_min)
  base$burst$D_min <- D_B_min
  base$burst$local_compression_min <- local_comp
  base$burst$contrast_min_possible <- c_possible
  base$burst$contrast_min_high <- c_high
  base$burst$contrast_geom_possible <- g_possible
  base$burst$contrast_geom_high <- g_high
  
  # seed-bridge seed-bridge estimates. Use immediate edge contrast, not fixed-k context median,
  # as the primary burst boundary reference. Raw intra-burst ISI remains only a weak
  # seed reference so low-frequency bursts do not corrupt high-frequency seed logic.
  burst_edge_min <- finite_num(burst_ev$contrast_min_q)
  burst_edge_geom <- finite_num(burst_ev$contrast_geom_q)
  burst_core_q <- finite_num(burst_ev$core_q_ISI_sec)
  burst_core_q_pct <- numeric(0)
  if (nrow(burst_ev) > 0) {
    burst_core_q_pct <- suppressWarnings(vapply(seq_len(nrow(burst_ev)), function(ii) {
      tr <- as.character(burst_ev$train[ii])
      if (!(tr %in% names(trains_pool))) return(NA_real_)
      isi_percentile_scalar(burst_ev$core_q_ISI_sec[ii], trains_pool[[tr]]$ISI_sec, min_isi_sec = min_isi_sec)
    }, numeric(1)))
    burst_core_q_pct <- finite_num(burst_core_q_pct)
  }
  burst_mm <- finite_num(burst_ev$MM)
  base$burst$use_seed_bridge_model <- TRUE
  base$burst$use_structure_candidates <- TRUE
  seed_q_est <- safe_q(burst_core_q, 0.90, default = if (is.finite(T_B_seed)) T_B_seed else 0.035)
  base$burst$seed_q_max <- clamp(seed_q_est, 0.003, 0.120)
  base$burst$structure_core_q_max <- clamp(seed_q_est * 1.35, 0.005, 0.160)
  base$burst$structure_core_q_loosen <- 1.25
  base$burst$adaptive_apply_core_pct_to_structure <- FALSE
  base$burst$adaptive_core_pct_seed_max <- clamp(safe_q(burst_core_q_pct, 0.90, default = base$burst$adaptive_core_pct_seed_max %||% 25), 5, 45)
  base$burst$adaptive_core_pct_possible_max <- clamp(safe_q(burst_core_q_pct, 0.98, default = max(base$burst$adaptive_core_pct_seed_max + 10, 35)), base$burst$adaptive_core_pct_seed_max, 60)
  base$burst$structure_edge_min <- clamp(safe_q(burst_edge_min, 0.05, default = 1.25), 1.05, 1.70)
  base$burst$structure_edge_geom_min <- clamp(safe_q(burst_edge_geom, 0.05, default = 1.35), 1.10, 1.90)
  base$burst$seed_q_loosen <- 1.35
  base$burst$seed_internal_bridge_split_ratio <- clamp(safe_q(burst_mm, 0.50, default = 1.80), 1.35, 2.50)
  base$burst$seed_edge_contrast_min <- clamp(safe_q(burst_edge_min, 0.02, default = 1.05), 1.00, 1.35)
  base$burst$bridge_ratio_max <- clamp(safe_q(burst_mm, 0.85, default = 3.50) + 0.50, 2.00, 6.00)
  base$burst$bridge_ratio_possible_max <- max(base$burst$bridge_ratio_max + 1.00, 4.00)
  base$burst$bridge_raw_max <- clamp(safe_q(burst_intra, 0.99, default = base$burst$T_bridge) * 1.50, 0.010, 0.200)
  base$burst$final_edge_contrast_min <- clamp(safe_q(burst_edge_min, 0.10, default = 1.45), 1.20, 1.90)
  base$burst$final_edge_contrast_geom_min <- clamp(safe_q(burst_edge_geom, 0.10, default = 1.50), 1.25, 2.20)
  base$burst$bridge_merged_edge_min <- clamp(base$burst$final_edge_contrast_min * 0.85, 1.10, 1.60)
  base$burst$bridge_merged_edge_geom_min <- clamp(base$burst$final_edge_contrast_geom_min * 0.85, 1.15, 1.80)
  base$burst$seed_min_isi_n <- 2L
  base$burst$G_min <- 3L
  
  base$tonic$T_min <- T_T_min
  base$tonic$T_max <- T_T_max
  base$tonic$LV_core <- T_LV_core
  base$tonic$LV_pre <- T_LV_pre
  base$tonic$LV_post <- T_LV_post
  base$tonic$G_min <- as.integer(G_T_min)
  base$tonic$D_min <- D_T_min
  
  base$pause$T_strong <- T_P_strong
  base$pause$T_seed <- T_P_seed
  base$pause$D_min <- D_P_min
  base$pause$G_min <- as.integer(G_P_min)
  
  base$detector$min_valid_isi_sec <- min_isi_sec
  base$detector$logisi_mcv_sec <- logisi_mcv_sec
  base$stats <- list(
    n_manual_burst_events = nrow(burst_ev),
    n_manual_tonic_events = nrow(tonic_ev),
    n_manual_pause_events = nrow(pause_ev),
    n_ML = length(ML_r),
    n_LOG = length(LOG_r),
    n_LOG_unresolved = LOG_unresolved_n,
    n_burst_context_values = length(burst_ctx_min),
    n_burst_edge_values = length(burst_edge_min),
    n_burst_core_pct_values = length(burst_core_q_pct)
  )
  base
}
