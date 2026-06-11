# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Event extraction and interval derivation
# ============================================================

empty_events_tbl <- function() {
  tibble(
    event_id = integer(),
    dataset = character(),
    train = character(),
    pattern = character(),
    start_isi = integer(),
    end_isi = integer(),
    start_spike_idx = integer(),
    end_spike_idx = integer(),
    n_spikes = integer(),
    n_isi = integer(),
    start_time_sec = numeric(),
    end_time_sec = numeric(),
    duration_sec = numeric(),
    pre_ISI_sec = numeric(),
    post_ISI_sec = numeric(),
    context_pre_ISI_sec = numeric(),
    context_post_ISI_sec = numeric(),
    mean_ISI_sec = numeric(),
    median_ISI_sec = numeric(),
    min_ISI_sec = numeric(),
    max_ISI_sec = numeric(),
    core_q_ISI_sec = numeric(),
    MM = numeric(),
    LV = numeric(),
    CV = numeric(),
    Pre_LV = numeric(),
    After_LV = numeric(),
    n_flank = integer(),
    n_flank_ctx = integer(),
    pre_ratio_q = numeric(),
    post_ratio_q = numeric(),
    contrast_min_q = numeric(),
    contrast_geom_q = numeric(),
    contrast_pct_q = numeric(),
    contrast_min_max = numeric(),
    contrast_geom_max = numeric(),
    contrast_pct_max = numeric(),
    context_pre_ratio_q = numeric(),
    context_post_ratio_q = numeric(),
    contrast_min_ctx_q = numeric(),
    contrast_geom_ctx_q = numeric(),
    contrast_pct_ctx_q = numeric(),
    contrast_min_ctx_max = numeric(),
    contrast_geom_ctx_max = numeric(),
    contrast_pct_ctx_max = numeric(),
    label_source = character(),
    n_user_promoted_isi = integer(),
    user_promoted_possible_burst = logical(),
    auto_pattern_majority = character(),
    user_override_reason = character(),
    auto_score = numeric(),
    isi_values_sec = list()
  )
}

stpd_event_span_label_audit <- function(dat, s_isi, e_isi, source = "final") {
  n <- nrow(dat)
  idx <- seq(max(2L, s_isi), min(n, e_isi))
  if (length(idx) == 0) {
    return(list(
      label_source = source,
      n_user_promoted_isi = 0L,
      user_promoted_possible_burst = FALSE,
      auto_pattern_majority = "",
      user_override_reason = ""
    ))
  }
  manual <- stpd_chr_vec(dat$pattern_manual, n)
  auto <- stpd_chr_vec(dat$pattern_auto, n)
  user_override <- stpd_chr_vec(dat$pattern_user_override, n)
  user_from <- stpd_chr_vec(dat$pattern_user_override_from, n)
  user_reason <- stpd_chr_vec(dat$pattern_user_override_reason, n)
  promoted <- user_override[idx] == "burst" & user_from[idx] == "possible_burst"
  label_source <- source
  if (identical(source, "final")) {
    label_source <- if (any(promoted, na.rm = TRUE)) "user_promoted_possible_burst"
    else if (any(manual[idx] != "", na.rm = TRUE)) "manual"
    else if (any(auto[idx] != "", na.rm = TRUE)) "auto"
    else "none"
  }
  list(
    label_source = label_source,
    n_user_promoted_isi = sum(promoted, na.rm = TRUE),
    user_promoted_possible_burst = any(promoted, na.rm = TRUE),
    auto_pattern_majority = mode_nonempty_label(auto[idx]),
    user_override_reason = mode_nonempty_label(user_reason[idx])
  )
}

extract_events_for_train <- function(dat,
                                     source = c("manual", "auto", "final", "audit_final"),
                                     auto_others = FALSE,
                                     dataset = "",
                                     train = "",
                                     min_isi_sec = 0.001,
                                     contrast_q = 0.90,
                                     context_k = 5L) {
  source <- match.arg(source)
  n <- nrow(dat)
  if (n <= 1) return(empty_events_tbl())
  
  pat <- switch(
    source,
    manual = dat$pattern_manual,
    auto   = dat$pattern_auto,
    final  = compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                                   auto_others = auto_others,
                                   min_isi_sec = min_isi_sec),
    audit_final = stpd_audit_final_labels(dat,
                                          min_isi_sec = min_isi_sec,
                                          auto_others = auto_others,
                                          prefer_stored = TRUE)
  )
  
  pat <- as.character(pat)
  pat[is.na(pat)] <- ""
  pat[1] <- ""
  art <- is_artifact_isi(dat$ISI_sec, min_isi_sec)
  scores <- suppressWarnings(as.numeric(dat$auto_score %||% rep(NA_real_, n)))
  
  out <- list()
  evt_id <- 1L
  for (p in c("burst", "long_burst", "possible_burst", "pause", "tonic", "high_frequency_tonic", "high_frequency_spiking", "others")) {
    pat_use <- pat
    if (p == "pause") pat_use[art] <- ""
    seg <- find_segments(pat_use, p)
    if (nrow(seg) == 0) next
    
    for (k in seq_len(nrow(seg))) {
      s_isi <- seg$start_isi[k]
      e_isi <- seg$end_isi[k]
      s_spk <- s_isi - 1
      e_spk <- e_isi
      if (s_spk < 1 || e_spk > n) next
      
      isi_vec_all <- dat$ISI_sec[s_isi:e_isi]
      isi_vec_valid <- valid_isi_values(isi_vec_all, min_isi_sec)
      pre <- if (s_isi > 2) dat$ISI_sec[s_isi - 1] else NA_real_
      post <- if (e_isi < n) dat$ISI_sec[e_isi + 1] else NA_real_
      pre <- if (is.finite(pre) && pre >= min_isi_sec) pre else NA_real_
      post <- if (is.finite(post) && post >= min_isi_sec) post else NA_real_
      
      start_t <- dat$timestamp_sec[s_spk]
      end_t <- dat$timestamp_sec[e_spk]
      dur <- end_t - start_t
      
      lv <- cv <- pre_lv <- after_lv <- NA_real_
      if (length(isi_vec_valid) >= 2) {
        lv <- calc_LV(isi_vec_valid)
        cv <- calc_CV(isi_vec_valid)
      }
      if (p %in% c("tonic", "high_frequency_tonic", "high_frequency_spiking")) {
        if (is.finite(pre)) pre_lv <- calc_LV(c(pre, isi_vec_valid))
        if (is.finite(post)) after_lv <- calc_LV(c(isi_vec_valid, post))
      }
      
      bc <- calc_event_contrast_stats(
        dat$ISI_sec, s_isi, e_isi,
        min_isi_sec = min_isi_sec,
        robust_q = contrast_q,
        context_k = context_k
      )
      label_audit <- stpd_event_span_label_audit(dat, s_isi, e_isi, source = source)
      
      out[[length(out) + 1]] <- tibble(
        event_id = evt_id,
        dataset = dataset,
        train = train,
        pattern = p,
        start_isi = s_isi,
        end_isi = e_isi,
        start_spike_idx = s_spk,
        end_spike_idx = e_spk,
        n_spikes = e_spk - s_spk + 1,
        n_isi = e_isi - s_isi + 1,
        start_time_sec = start_t,
        end_time_sec = end_t,
        duration_sec = dur,
        pre_ISI_sec = pre,
        post_ISI_sec = post,
        context_pre_ISI_sec = bc$context_pre_ISI_sec,
        context_post_ISI_sec = bc$context_post_ISI_sec,
        mean_ISI_sec = if (length(isi_vec_valid) > 0) mean(isi_vec_valid) else NA_real_,
        median_ISI_sec = if (length(isi_vec_valid) > 0) median(isi_vec_valid) else NA_real_,
        min_ISI_sec = if (length(isi_vec_valid) > 0) min(isi_vec_valid) else NA_real_,
        max_ISI_sec = if (length(isi_vec_valid) > 0) max(isi_vec_valid) else NA_real_,
        core_q_ISI_sec = bc$core_q,
        MM = if (length(isi_vec_valid) > 0) max(isi_vec_valid) / mean(isi_vec_valid) else NA_real_,
        LV = lv,
        CV = cv,
        Pre_LV = pre_lv,
        After_LV = after_lv,
        n_flank = bc$n_flank,
        n_flank_ctx = bc$n_flank_ctx,
        pre_ratio_q = bc$pre_ratio_q,
        post_ratio_q = bc$post_ratio_q,
        contrast_min_q = bc$contrast_min_q,
        contrast_geom_q = bc$contrast_geom_q,
        contrast_pct_q = bc$contrast_pct_q,
        contrast_min_max = bc$contrast_min_max,
        contrast_geom_max = bc$contrast_geom_max,
        contrast_pct_max = bc$contrast_pct_max,
        context_pre_ratio_q = bc$context_pre_ratio_q,
        context_post_ratio_q = bc$context_post_ratio_q,
        contrast_min_ctx_q = bc$contrast_min_ctx_q,
        contrast_geom_ctx_q = bc$contrast_geom_ctx_q,
        contrast_pct_ctx_q = bc$contrast_pct_ctx_q,
        contrast_min_ctx_max = bc$contrast_min_ctx_max,
        contrast_geom_ctx_max = bc$contrast_geom_ctx_max,
        contrast_pct_ctx_max = bc$contrast_pct_ctx_max,
        label_source = label_audit$label_source,
        n_user_promoted_isi = label_audit$n_user_promoted_isi,
        user_promoted_possible_burst = label_audit$user_promoted_possible_burst,
        auto_pattern_majority = label_audit$auto_pattern_majority,
        user_override_reason = label_audit$user_override_reason,
        auto_score = if (all(is.na(scores[s_isi:e_isi]))) NA_real_ else max(scores[s_isi:e_isi], na.rm = TRUE),
        isi_values_sec = list(as.numeric(isi_vec_all))
      )
      evt_id <- evt_id + 1L
    }
  }
  
  if (length(out) == 0) return(empty_events_tbl())
  bind_rows(out) %>% arrange(train, start_time_sec, pattern)
}

derive_interval_tables <- function(trains_pool,
                                   source = c("manual", "auto", "final", "audit_final"),
                                   auto_others = FALSE,
                                   dataset_map = NULL,
                                   min_isi_sec = 0.001,
                                   contrast_q = 0.90,
                                   context_k = 5L) {
  source <- match.arg(source)
  dataset_map <- dataset_map %||% stats::setNames(rep("dataset", length(trains_pool)), names(trains_pool))
  dataset_name_for_train <- function(nm) {
    val <- NULL
    map_names <- names(dataset_map)
    if (!is.null(map_names) && nm %in% map_names) {
      val <- dataset_map[[nm]]
    } else {
      pos <- match(nm, names(trains_pool))
      if (is.finite(pos) && !is.na(pos) && pos >= 1L && pos <= length(dataset_map)) val <- dataset_map[[pos]]
    }
    val <- as.character(val %||% "dataset")[1]
    if (is.na(val) || !nzchar(val)) "dataset" else val
  }
  if (length(trains_pool) == 0) {
    return(list(events = data.frame(), intervals = list(), labels = data.frame(), logisi = data.frame()))
  }
  
  events <- bind_rows(imap(trains_pool, function(dat, nm) {
    extract_events_for_train(
      dat,
      source = source,
      auto_others = auto_others,
      dataset = dataset_name_for_train(nm),
      train = nm,
      min_isi_sec = min_isi_sec,
      contrast_q = contrast_q,
      context_k = context_k
    )
  }))
  
  labels <- bind_rows(imap(trains_pool, function(dat, nm) {
    manual <- dat$pattern_manual
    auto <- dat$pattern_auto
    n <- nrow(dat)
    final <- compute_final_pattern(manual, auto, dat$ISI_sec,
                                   auto_others = auto_others,
                                   min_isi_sec = min_isi_sec)
    audit_final <- stpd_audit_final_labels(dat,
                                           min_isi_sec = min_isi_sec,
                                           auto_others = auto_others,
                                           prefer_stored = TRUE)
    tibble(
      dataset = dataset_name_for_train(nm),
      train = nm,
      idx = dat$idx,
      timestamp_sec = dat$timestamp_sec,
      ISI_sec = dat$ISI_sec,
      is_artifact = is_artifact_isi(dat$ISI_sec, min_isi_sec),
      manual_label = manual,
      manual_negative_label = stpd_chr_vec(dat$pattern_manual_negative, n),
      auto_label = auto,
      auto_label_original = stpd_chr_vec(dat$pattern_auto_original, n),
      final_label = final,
      audit_final_label = audit_final,
      audit_base_final_label = stpd_chr_vec(dat$pattern_audit_base_final, n),
      audit_from_label = stpd_chr_vec(dat$pattern_audit_from, n),
      audit_to_label = stpd_chr_vec(dat$pattern_audit_to, n),
      audit_action = stpd_chr_vec(dat$pattern_audit_action, n),
      audit_source = stpd_chr_vec(dat$pattern_audit_source, n),
      audit_reason = stpd_chr_vec(dat$pattern_audit_reason, n),
      audit_id = stpd_chr_vec(dat$pattern_audit_id, n),
      audit_time = stpd_chr_vec(dat$pattern_audit_time, n),
      user_override_label = stpd_chr_vec(dat$pattern_user_override, n),
      user_override_from = stpd_chr_vec(dat$pattern_user_override_from, n),
      user_override_to = stpd_chr_vec(dat$pattern_user_override_to, n),
      user_override_reason = stpd_chr_vec(dat$pattern_user_override_reason, n),
      user_override_source = stpd_chr_vec(dat$pattern_user_override_source, n),
      user_override_time = stpd_chr_vec(dat$pattern_user_override_time, n),
      user_override_id = stpd_chr_vec(dat$pattern_user_override_id, n),
      auto_score = suppressWarnings(as.numeric(dat$auto_score %||% NA_real_))
    )
  }))
  
  make_interval_df <- function(evt, pattern_name, field, new_name) {
    if (nrow(evt) == 0 || !(field %in% colnames(evt))) return(tibble())
    evt %>%
      filter(pattern == pattern_name, is.finite(.data[[field]])) %>%
      transmute(dataset, train, pattern = pattern_name, event_id, start_time_sec, end_time_sec,
                value_sec = .data[[field]], interval_type = new_name)
  }
  
  make_value_df <- function(evt, pattern_name, field, new_name) {
    if (nrow(evt) == 0 || !(field %in% colnames(evt))) return(tibble())
    evt %>%
      filter(pattern == pattern_name, is.finite(.data[[field]])) %>%
      transmute(dataset, train, pattern = pattern_name, event_id, start_time_sec, end_time_sec,
                value = .data[[field]], interval_type = new_name)
  }
  
  intervals <- list()
  intervals$pre_burst <- make_interval_df(events, "burst", "pre_ISI_sec", "pre_burst")
  intervals$after_burst <- make_interval_df(events, "burst", "post_ISI_sec", "after_burst")
  intervals$pre_tonic <- make_interval_df(events, "tonic", "pre_ISI_sec", "pre_tonic")
  intervals$after_tonic <- make_interval_df(events, "tonic", "post_ISI_sec", "after_tonic")
  
  if (nrow(events) > 0) {
    intra_parts <- lapply(seq_len(nrow(events)), function(i) {
      vals <- unlist(events$isi_values_sec[[i]], use.names = FALSE)
      vals <- valid_isi_values(vals, min_isi_sec)
      if (length(vals) == 0) return(NULL)
      tibble(
        dataset = events$dataset[i],
        train = events$train[i],
        pattern = events$pattern[i],
        event_id = events$event_id[i],
        start_time_sec = events$start_time_sec[i],
        end_time_sec = events$end_time_sec[i],
        value_sec = vals,
        interval_type = dplyr::case_when(
          events$pattern[i] == "burst" ~ "intra_burst",
          events$pattern[i] == "long_burst" ~ "intra_long_burst",
          events$pattern[i] == "possible_burst" ~ "intra_possible_burst",
          events$pattern[i] == "tonic" ~ "intra_tonic",
          events$pattern[i] == "high_frequency_tonic" ~ "intra_high_frequency_tonic",
          events$pattern[i] == "high_frequency_spiking" ~ "intra_high_frequency_spiking",
          events$pattern[i] == "pause" ~ "pause_isi",
          events$pattern[i] == "others" ~ "others_isi",
          TRUE ~ "other"
        )
      )
    })
    intra_long <- bind_rows(intra_parts)
    intervals$intra_burst <- intra_long %>% filter(interval_type == "intra_burst")
    intervals$intra_long_burst <- intra_long %>% filter(interval_type == "intra_long_burst")
    intervals$intra_possible_burst <- intra_long %>% filter(interval_type == "intra_possible_burst")
    intervals$intra_tonic <- intra_long %>% filter(interval_type == "intra_tonic")
    intervals$intra_high_frequency_tonic <- intra_long %>% filter(interval_type == "intra_high_frequency_tonic")
    intervals$intra_high_frequency_spiking <- intra_long %>% filter(interval_type == "intra_high_frequency_spiking")
    intervals$pause_isi <- intra_long %>% filter(interval_type == "pause_isi")
    intervals$others_isi <- intra_long %>% filter(interval_type == "others_isi")
  } else {
    intervals$intra_burst <- tibble()
    intervals$intra_long_burst <- tibble()
    intervals$intra_possible_burst <- tibble()
    intervals$intra_tonic <- tibble()
    intervals$intra_high_frequency_tonic <- tibble()
    intervals$intra_high_frequency_spiking <- tibble()
    intervals$pause_isi <- tibble()
    intervals$others_isi <- tibble()
  }
  
  make_inter_event <- function(evt, pattern_name, new_name) {
    if (nrow(evt) == 0) return(tibble())
    evt %>%
      filter(pattern == pattern_name) %>%
      arrange(dataset, train, start_time_sec) %>%
      group_by(dataset, train) %>%
      mutate(next_start = lead(start_time_sec), value_sec = next_start - end_time_sec) %>%
      ungroup() %>%
      filter(is.finite(value_sec) & value_sec >= 0) %>%
      transmute(dataset, train, pattern = pattern_name, event_id, start_time_sec, end_time_sec, value_sec, interval_type = new_name)
  }
  intervals$inter_burst <- make_inter_event(events, "burst", "inter_burst")
  intervals$inter_possible_burst <- make_inter_event(events, "possible_burst", "inter_possible_burst")
  intervals$inter_tonic <- make_inter_event(events, "tonic", "inter_tonic")
  intervals$inter_pause <- make_inter_event(events, "pause", "inter_pause")
  
  intervals$pause_duration <- events %>%
    filter(pattern == "pause", is.finite(duration_sec)) %>%
    transmute(dataset, train, pattern, event_id, start_time_sec, end_time_sec,
              value_sec = duration_sec, interval_type = "pause_duration")
  
  intervals$tonic_lv <- make_value_df(events, "tonic", "LV", "tonic_lv")
  intervals$tonic_pre_lv <- make_value_df(events, "tonic", "Pre_LV", "tonic_pre_lv")
  intervals$tonic_after_lv <- make_value_df(events, "tonic", "After_LV", "tonic_after_lv")
  
  # Burst contrast distributions; these are dimensionless, except pct.
  contrast_fields <- c(
    burst_contrast_min_q = "contrast_min_q",
    burst_contrast_geom_q = "contrast_geom_q",
    burst_contrast_pct_q = "contrast_pct_q",
    burst_contrast_min_max = "contrast_min_max",
    burst_contrast_geom_max = "contrast_geom_max",
    burst_context_min_q = "contrast_min_ctx_q",
    burst_context_geom_q = "contrast_geom_ctx_q",
    burst_context_pct_q = "contrast_pct_ctx_q",
    burst_context_min_max = "contrast_min_ctx_max",
    burst_context_geom_max = "contrast_geom_ctx_max",
    possible_context_min_q = "contrast_min_ctx_q",
    possible_context_geom_q = "contrast_geom_ctx_q"
  )
  for (nm in names(contrast_fields)) {
    pat <- if (startsWith(nm, "possible_")) "possible_burst" else "burst"
    intervals[[nm]] <- make_value_df(events, pat, contrast_fields[[nm]], nm)
  }
  
  source_label <- switch(
    source,
    manual = labels$manual_label,
    auto = labels$auto_label,
    final = labels$final_label
  )
  logisi <- labels
  logisi$source_label <- source_label
  logisi <- logisi %>%
    filter(idx >= 2, is.finite(ISI_sec), !is_artifact) %>%
    mutate(log10_ISI = log10(ISI_sec))
  
  list(events = events, intervals = intervals, labels = labels, logisi = logisi)
}
