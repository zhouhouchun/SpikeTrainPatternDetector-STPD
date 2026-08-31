`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

component_seed <- function(template_id, component, component_slot, replicate = 1L) {
  allowed_seed_components <- unique(c(parameters$rng_contract$component_types,
                                      parameters$rng_contract$streams))
  if (length(component) != 1L || is.na(component) || !component %in% allowed_seed_components)
    stop("The component or stream is not declared by the frozen RNG contract.", call. = FALSE)
  if (length(replicate) != 1L || is.na(replicate) || replicate < 1L || replicate != as.integer(replicate))
    stop("The component replicate must be a positive integer.", call. = FALSE)
  rng_seed_from_key(c(parameters$rng_contract$namespace, template_id, component,
                      component_slot, as.integer(replicate)))
}

with_component_rng <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

local_isi_sha256 <- function(x) {
  # Hash a publication-stable decimal representation rather than in-memory
  # binary doubles, so a CSV write/read round trip cannot change provenance.
  digest::digest(paste(sprintf("%.10f", as.numeric(x)), collapse = "|"),
                 algo = "sha256", serialize = FALSE)
}

frozen_hfs_rate_overlap_stratum <- function(template_index, regime) {
  regimes <- names(parameters$broad_hfs$regularity_regimes)
  strata <- parameters$broad_hfs$rate_overlap_latin_square
  regime_index <- match(regime, regimes)
  if (is.na(regime_index)) stop("Unknown HFS regularity regime.", call. = FALSE)
  strata[((regime_index - 1L + template_index - 1L) %% length(strata)) + 1L]
}

make_frozen_burst_quota_table <- function() {
  regimes <- names(parameters$broad_hfs$regularity_regimes)
  strengths <- names(parameters$burst_pulse$pulse_contrast_strata)
  standalone <- do.call(rbind, lapply(seq_len(parameters$n_templates), function(i) {
    data.frame(
      Template_ID = sprintf("TPL_%03d", i), Burst_Slot = paste0("standalone_", seq_along(strengths)),
      Context = "standalone", Strength = strengths, HFS_Regime = "not_applicable",
      stringsAsFactors = FALSE
    )
  }))

  development <- data.frame(
    Template_ID = rep(sprintf("TPL_%03d", 1:5), each = 2L),
    Burst_Slot = rep(c("hfs_1", "hfs_2"), 5L), Context = "hfs",
    Strength = c("weak", "moderate", "weak", "strong", "moderate", "strong",
                 "moderate", "weak", "strong", "moderate"),
    HFS_Regime = c("regular", "intermediate", "intermediate", "irregular", "irregular", "regular",
                   "regular", "irregular", "intermediate", "regular"),
    stringsAsFactors = FALSE
  )

  cell_grid <- expand.grid(HFS_Regime = regimes, Strength = strengths, stringsAsFactors = FALSE)
  holdout_pool <- rbind(cell_grid, cell_grid, cell_grid,
                        data.frame(HFS_Regime = regimes, Strength = strengths, stringsAsFactors = FALSE))
  set.seed(parameters$quota_seed)
  paired <- NULL
  for (attempt in seq_len(1000L)) {
    remaining <- seq_len(nrow(holdout_pool))
    chosen <- integer()
    while (length(remaining) >= 2L) {
      first <- sample(remaining, 1L)
      compatible <- setdiff(remaining[
        holdout_pool$HFS_Regime[remaining] != holdout_pool$HFS_Regime[first] &
          holdout_pool$Strength[remaining] != holdout_pool$Strength[first]
      ], first)
      if (!length(compatible)) break
      second <- sample(compatible, 1L)
      chosen <- c(chosen, first, second)
      remaining <- setdiff(remaining, c(first, second))
    }
    if (!length(remaining)) {
      paired <- holdout_pool[chosen, , drop = FALSE]
      break
    }
  }
  if (is.null(paired)) stop("Unable to construct the frozen holdout HFS quota pairing.", call. = FALSE)
  holdout <- data.frame(
    Template_ID = rep(sprintf("TPL_%03d", 6:20), each = 2L),
    Burst_Slot = rep(c("hfs_1", "hfs_2"), 15L), Context = "hfs",
    Strength = paired$Strength, HFS_Regime = paired$HFS_Regime,
    stringsAsFactors = FALSE
  )

  quota <- rbind(standalone, development, holdout)
  quota$Burst_Quota_ID <- sprintf("BQ_%03d", seq_len(nrow(quota)))
  quota$Is_Four_Spike_Boundary <- FALSE
  for (i in seq_len(nrow(parameters$four_spike_assignments))) {
    assignment <- parameters$four_spike_assignments[i, ]
    hit <- which(quota$Template_ID == assignment$Template_ID &
                   quota$Context == assignment$Context & quota$Strength == assignment$Strength)
    if (length(hit) != 1L) stop("A four-spike assignment does not resolve to exactly one quota row.", call. = FALSE)
    quota$Is_Four_Spike_Boundary[hit] <- TRUE
  }
  quota$Target_Spikes <- parameters$burst_pulse$boundary_realized_spikes
  standard_levels <- parameters$burst_pulse$standard_realized_spikes
  for (context in c("standalone", "hfs")) for (strength_index in seq_along(strengths)) {
    idx <- which(!quota$Is_Four_Spike_Boundary & quota$Context == context &
                   quota$Strength == strengths[strength_index])
    idx <- idx[order(quota$Template_ID[idx], quota$Burst_Slot[idx])]
    offset <- (strength_index - 1L + ifelse(context == "hfs", 2L, 0L)) %% length(standard_levels)
    quota$Target_Spikes[idx] <- rep(standard_levels[((seq_along(standard_levels) - 1L + offset) %%
                                                       length(standard_levels)) + 1L], length.out = length(idx))
  }
  quota$Split <- ifelse(quota$Template_ID %in% parameters$development_templates, "development", "holdout")
  template_index <- as.integer(sub("TPL_", "", quota$Template_ID))
  quota$HFS_Rate_Overlap_Stratum <- "not_applicable"
  hfs_rows <- quota$Context == "hfs"
  quota$HFS_Rate_Overlap_Stratum[hfs_rows] <- vapply(which(hfs_rows), function(i) {
    frozen_hfs_rate_overlap_stratum(template_index[i], quota$HFS_Regime[i])
  }, character(1))
  quota <- quota[, c("Burst_Quota_ID", "Template_ID", "Split", "Burst_Slot", "Context",
                     "Strength", "HFS_Regime", "HFS_Rate_Overlap_Stratum",
                     "Target_Spikes", "Is_Four_Spike_Boundary")]
  rownames(quota) <- NULL
  quota
}

empty_interval_frame <- function() {
  data.frame(
    Template_ID = character(), Interval_Index = integer(), Left_Spike_Index = integer(),
    Right_Spike_Index = integer(), Start_u = numeric(), End_u = numeric(), ISI_u = numeric(),
    Run_ID = character(), State_Label = character(), Event_Label = character(),
    State_Envelope_ID = character(), Event_ID = character(), Pause_Subtype = character(),
    Contextual_Separator_Label = character(), Contextual_Separator_ID = character(),
    Borderline_Gap_Label = character(), Borderline_Gap_ID = character(),
    Secondary_Event_Label = character(), Secondary_Event_ID = character(),
    Secondary_Event_Subtype = character(), Secondary_Scoring_Role = character(),
    Product_Level_Mapping = character(),
    Generator_Provenance = character(), Renewal_Family = character(), Regularity_Regime = character(),
    Tonic_Subtype = character(), Burst_Role = character(), HFS_Role = character(),
    Direct_HFS_Support = logical(), HFS_Interruption_Role = character(),
    HFS_Support_Contract_Version = character(),
    Legacy_V2_2_Direct_HFS_Support_Audit = logical(),
    Composite_HFS_Regime_ID = character(), Hard_Cut_Reason = character(),
    Tonic_Intended_Stratum = character(), Tonic_Rate_Overlap_Stratum = character(),
    HFS_Rate_Overlap_Stratum = character(), Latent_Pulse_ID = character(),
    Burst_Strength_Stratum = character(), Frozen_Burst_Target_Spikes = integer(),
    Burst_Quota_ID = character(), Burst_Contrast_Target = numeric(),
    Pulse_Refractory_Limited = logical(), Latent_Window_Truncated = logical(),
    stringsAsFactors = FALSE
  )
}

generate_template_v23 <- function(template_index, burst_quota) {
  template_id <- sprintf("TPL_%03d", template_index)
  template_quota <- burst_quota[burst_quota$Template_ID == template_id, , drop = FALSE]
  standalone_quota <- template_quota[template_quota$Context == "standalone", , drop = FALSE]
  hfs_quota <- template_quota[template_quota$Context == "hfs", , drop = FALSE]
  if (nrow(standalone_quota) != 3L || nrow(hfs_quota) != 2L) stop("Frozen Burst quota mismatch.", call. = FALSE)
  plain_hfs_regime <- setdiff(names(parameters$broad_hfs$regularity_regimes), hfs_quota$HFS_Regime)
  if (length(plain_hfs_regime) != 1L) stop("Each template must use two distinct HFS overlay regimes.", call. = FALSE)

  interval_rows <- empty_interval_frame()
  event_rows <- list(); state_rows <- list(); run_rows <- list()
  separator_rows <- list(); borderline_rows <- list(); contextual_rows <- list(); regime_rows <- list()
  current_time <- 0; interval_index <- 0L; run_counter <- 0L
  event_counter <- 0L; state_counter <- 0L; pulse_counter <- 0L; separator_counter <- 0L
  borderline_counter <- 0L; regime_counter <- 0L

  append_intervals <- function(run, state_label = "none", event_mask = NULL,
                               event_label = "none", pause_subtype = "none",
                               separator_mask = NULL, borderline_mask = NULL,
                               provenance = "background_renewal",
                               regime = "background", tonic_subtype = "none",
                               burst_role = NULL, hfs_role = NULL, latent = NULL,
                               event_context = "none", burst_quota_row = NULL,
                               component_type = "background", component_slot = "unspecified",
                               component_seeds = NULL, tonic_intended_stratum = "not_applicable",
                               tonic_rate_overlap_stratum = "not_applicable",
                               hfs_rate_overlap_stratum = "not_applicable") {
    n_isi <- length(run$isis)
    if (n_isi < 1L) stop("A run must contain at least one observed ISI.", call. = FALSE)
    run_counter <<- run_counter + 1L
    run_id <- sprintf("%s_RUN_%03d", template_id, run_counter)
    start_time <- current_time
    starts <- current_time + run$times[-length(run$times)]
    ends <- current_time + run$times[-1L]
    if (is.null(event_mask)) event_mask <- rep(FALSE, n_isi)
    if (is.null(separator_mask)) separator_mask <- rep(FALSE, n_isi)
    if (is.null(borderline_mask)) borderline_mask <- rep(FALSE, n_isi)
    if (sum(c(any(event_mask), any(separator_mask), any(borderline_mask))) > 1L)
      stop("Primary and secondary Event mechanisms cannot share an ISI.", call. = FALSE)
    if (is.null(burst_role)) burst_role <- rep("none", n_isi)
    if (is.null(hfs_role)) hfs_role <- ifelse(state_label == "Broad_HFS", "baseline", "none")

    state_id <- "none"
    if (state_label != "none") {
      state_counter <<- state_counter + 1L
      state_id <- sprintf("%s_STATE_%03d", template_id, state_counter)
    }
    event_id <- "none"; latent_id <- "none"
    if (any(event_mask)) {
      event_counter <<- event_counter + 1L
      event_id <- sprintf("%s_EVENT_%03d", template_id, event_counter)
      if (event_label == "Burst") {
        pulse_counter <<- pulse_counter + 1L
        latent_id <- sprintf("%s_PULSE_%03d", template_id, pulse_counter)
      }
    }
    separator_id <- "none"
    if (any(separator_mask)) {
      separator_counter <<- separator_counter + 1L
      separator_id <- sprintf("%s_SEPARATOR_%03d", template_id, separator_counter)
    }
    borderline_id <- "none"
    if (any(borderline_mask)) {
      borderline_counter <<- borderline_counter + 1L
      borderline_id <- sprintf("%s_BORDERLINE_%03d", template_id, borderline_counter)
    }
    strength <- if (!is.null(burst_quota_row)) burst_quota_row$Strength else "none"
    target_spikes <- if (!is.null(burst_quota_row)) burst_quota_row$Target_Spikes else NA_integer_
    quota_id <- if (!is.null(burst_quota_row)) burst_quota_row$Burst_Quota_ID else "none"
    burst_contrast <- if (!is.null(latent)) latent$contrast else NA_real_
    pulse_refractory_limited <- if (!is.null(latent)) latent$refractory_limited else FALSE
    latent_window_truncated <- if (!is.null(latent)) latent$truncated else FALSE
    hfs_baseline_role <- ifelse(run$isis <= parameters$broad_hfs$direct_support_max_isi_B,
                                "direct", ifelse(run$isis <= parameters$broad_hfs$tolerated_interruption_max_isi_B,
                                                  "tolerated_short_interruption", "outside_direct_support"))
    row_hfs_role <- if (state_label == "Broad_HFS") hfs_baseline_role else rep("none", n_isi)
    if (state_label == "Broad_HFS" && event_label == "Burst") row_hfs_role[event_mask] <- "nested_burst_event"
    direct_hfs <- state_label == "Broad_HFS" & row_hfs_role == "direct"
    legacy_v2_2_direct_hfs <- state_label == "Broad_HFS" & !event_mask &
      run$isis <= parameters$broad_hfs$legacy_v2_2_direct_support_max_isi_B
    new_rows <- data.frame(
      Template_ID = template_id,
      Interval_Index = interval_index + seq_len(n_isi),
      Left_Spike_Index = interval_index + seq_len(n_isi),
      Right_Spike_Index = interval_index + seq_len(n_isi) + 1L,
      Start_u = starts, End_u = ends, ISI_u = run$isis,
      Run_ID = run_id, State_Label = state_label,
      Event_Label = ifelse(event_mask, event_label, "none"),
      State_Envelope_ID = ifelse(state_label == "none", "none", state_id),
      Event_ID = ifelse(event_mask, event_id, "none"),
      Pause_Subtype = ifelse(event_mask & event_label == "Pause", pause_subtype, "none"),
      Contextual_Separator_Label = ifelse(separator_mask, parameters$contextual_separator$secondary_label, "none"),
      Contextual_Separator_ID = ifelse(separator_mask, separator_id, "none"),
      Borderline_Gap_Label = ifelse(borderline_mask, parameters$borderline_gap$secondary_label, "none"),
      Borderline_Gap_ID = ifelse(borderline_mask, borderline_id, "none"),
      Secondary_Event_Label = ifelse(separator_mask, parameters$contextual_separator$secondary_label,
                                     ifelse(borderline_mask, parameters$borderline_gap$secondary_label, "none")),
      Secondary_Event_ID = ifelse(separator_mask, separator_id,
                                  ifelse(borderline_mask, borderline_id, "none")),
      Secondary_Event_Subtype = ifelse(separator_mask, "contextual_interburst_gap_separator",
                                       ifelse(borderline_mask, "tonic_tail_overlap_gap", "none")),
      Secondary_Scoring_Role = ifelse(separator_mask | borderline_mask,
                                      "secondary_target_excluded_from_primary_pause", "none"),
      Product_Level_Mapping = ifelse(separator_mask | borderline_mask, "Pause", "none"),
      Generator_Provenance = ifelse(event_mask & event_label == "Burst", "injected_rate_pulse",
                                    ifelse(separator_mask, "contextual_gap_mechanism",
                                           ifelse(borderline_mask, "borderline_gap_mechanism", provenance))),
      Renewal_Family = ifelse(event_label == "Pause" | any(separator_mask) | any(borderline_mask),
                              "explicit_gap", parameters$renewal_family),
      Regularity_Regime = regime, Tonic_Subtype = tonic_subtype,
      Burst_Role = ifelse(event_mask & event_label == "Burst", burst_role, "none"),
      HFS_Role = row_hfs_role,
      Direct_HFS_Support = direct_hfs,
      HFS_Interruption_Role = if (state_label == "Broad_HFS") row_hfs_role else rep("none", n_isi),
      HFS_Support_Contract_Version = ifelse(state_label == "Broad_HFS",
                                             parameters$broad_hfs$support_contract_version,
                                             "not_applicable"),
      Legacy_V2_2_Direct_HFS_Support_Audit = legacy_v2_2_direct_hfs,
      Composite_HFS_Regime_ID = "none", Hard_Cut_Reason = "none",
      Tonic_Intended_Stratum = ifelse(state_label == "Tonic", tonic_intended_stratum, "not_applicable"),
      Tonic_Rate_Overlap_Stratum = ifelse(state_label == "Tonic", tonic_rate_overlap_stratum, "not_applicable"),
      HFS_Rate_Overlap_Stratum = ifelse(state_label == "Broad_HFS", hfs_rate_overlap_stratum, "not_applicable"),
      Latent_Pulse_ID = ifelse(event_mask & event_label == "Burst", latent_id, "none"),
      Burst_Strength_Stratum = ifelse(event_mask & event_label == "Burst", strength, "none"),
      Frozen_Burst_Target_Spikes = ifelse(event_mask & event_label == "Burst", target_spikes, NA_integer_),
      Burst_Quota_ID = ifelse(event_mask & event_label == "Burst", quota_id, "none"),
      Burst_Contrast_Target = ifelse(event_mask & event_label == "Burst", burst_contrast, NA_real_),
      Pulse_Refractory_Limited = ifelse(event_mask & event_label == "Burst", pulse_refractory_limited, FALSE),
      Latent_Window_Truncated = ifelse(event_mask & event_label == "Burst", latent_window_truncated, FALSE),
      stringsAsFactors = FALSE
    )
    interval_rows <<- rbind(interval_rows, new_rows)
    metrics <- regularity_metrics(run$isis)
    primary_descriptor <- if (any(event_mask)) {
      if (event_label == "Pause") paste0("Pause:", pause_subtype) else event_label
    } else if (state_label != "none") {
      if (state_label == "Tonic") paste0("Tonic:", tonic_subtype) else state_label
    } else if (any(separator_mask)) parameters$contextual_separator$secondary_label
    else if (any(borderline_mask)) parameters$borderline_gap$secondary_label else "Background"
    run_rows[[length(run_rows) + 1L]] <<- data.frame(
      Template_ID = template_id, Run_ID = run_id, Run_Ordinal = run_counter,
      Start_u = start_time, End_u = tail(ends, 1), Source_Type = provenance,
      Primary_Descriptor = primary_descriptor, State_Label = state_label,
      Event_Label = if (any(event_mask)) event_label else "none",
      Pause_Subtype = if (any(event_mask) && event_label == "Pause") pause_subtype else "none",
      Contextual_Separator_Label = if (any(separator_mask)) parameters$contextual_separator$secondary_label else "none",
      Borderline_Gap_Label = if (any(borderline_mask)) parameters$borderline_gap$secondary_label else "none",
      Secondary_Event_Label = if (any(separator_mask)) parameters$contextual_separator$secondary_label else
        if (any(borderline_mask)) parameters$borderline_gap$secondary_label else "none",
      Secondary_Event_Subtype = if (any(separator_mask)) "contextual_interburst_gap_separator" else
        if (any(borderline_mask)) "tonic_tail_overlap_gap" else "none",
      Secondary_Scoring_Role = if (any(separator_mask) || any(borderline_mask))
        "secondary_target_excluded_from_primary_pause" else "none",
      Product_Level_Mapping = if (any(separator_mask) || any(borderline_mask)) "Pause" else "none",
      Renewal_Family = if (event_label == "Pause" || any(separator_mask) || any(borderline_mask)) "explicit_gap" else parameters$renewal_family,
      Regularity_Regime = regime, Tonic_Subtype = tonic_subtype,
      Tonic_Rate_Overlap_Stratum = if (state_label == "Tonic") tonic_rate_overlap_stratum else "not_applicable",
      HFS_Rate_Overlap_Stratum = if (state_label == "Broad_HFS") hfs_rate_overlap_stratum else "not_applicable",
      HFS_Support_Contract_Version = if (state_label == "Broad_HFS")
        parameters$broad_hfs$support_contract_version else "not_applicable",
      Burst_Strength_Stratum = strength, Burst_Quota_ID = quota_id,
      Burst_Contrast_Target = if (!is.null(latent)) latent$contrast else NA_real_,
      Component_Type = component_type, Component_Slot = component_slot,
      RNG_Seed = if (is.null(component_seeds)) component_seed(template_id, component_type, component_slot) else component_seeds[["point_process"]] %||% component_seeds[[1]],
      RNG_Stream_Registry = if (is.null(component_seeds)) "single_component_stream" else paste(names(component_seeds), unlist(component_seeds), sep = "=", collapse = ";"),
      Local_ISI_SHA256 = local_isi_sha256(run$isis),
      Mean_ISI_Parameter_u = run$mean_isi %||% NA_real_, Gamma_Shape = run$shape %||% NA_real_,
      Refractory_u = if (event_label == "Pause" || any(separator_mask) || any(borderline_mask))
        NA_real_ else parameters$shared_refractory_B,
      N_ISIs = n_isi, Duration_u = sum(run$isis), Observed_Mean_ISI_u = metrics["mean"],
      CV = metrics["cv"], CV2 = metrics["cv2"], LV = metrics["lv"],
      Generation_Attempts = run$attempts %||% 1L, stringsAsFactors = FALSE
    )
    if (state_label != "none") {
      state_rows[[length(state_rows) + 1L]] <<- data.frame(
        Template_ID = template_id, State_Envelope_ID = state_id, Run_ID = run_id,
        State_Label = state_label, Tonic_Subtype = tonic_subtype,
        Start_u = start_time, End_u = tail(ends, 1), Duration_u = tail(ends, 1) - start_time,
        N_Boundary_Spikes = n_isi + 1L, N_ISIs = n_isi,
        Renewal_Family = parameters$renewal_family, Regularity_Regime = regime,
        Mean_ISI_Parameter_u = run$mean_isi %||% NA_real_, Gamma_Shape = run$shape %||% NA_real_,
        Refractory_u = parameters$shared_refractory_B, Observed_Mean_ISI_u = metrics["mean"],
        CV = metrics["cv"], CV2 = metrics["cv2"], LV = metrics["lv"],
        Contains_Injected_Burst = any(event_mask & event_label == "Burst"),
        Burst_Strength_Stratum = if (any(event_mask & event_label == "Burst")) strength else "none",
        Tonic_Intended_Stratum = if (state_label == "Tonic") tonic_intended_stratum else "not_applicable",
        Tonic_Rate_Overlap_Stratum = if (state_label == "Tonic") tonic_rate_overlap_stratum else "not_applicable",
        HFS_Rate_Overlap_Stratum = if (state_label == "Broad_HFS") hfs_rate_overlap_stratum else "not_applicable",
        HFS_Support_Contract_Version = if (state_label == "Broad_HFS")
          parameters$broad_hfs$support_contract_version else "not_applicable",
        Composite_HFS_Regime_ID = "none", Boundary_Observability = "pending_audit",
        Start_Boundary_Observability = "pending_audit", End_Boundary_Observability = "pending_audit",
        Start_Boundary_Contrast = NA_real_, End_Boundary_Contrast = NA_real_,
        Start_Hard_Cut_Reason = "none", End_Hard_Cut_Reason = "none",
        stringsAsFactors = FALSE
      )
    }
    if (any(event_mask)) {
      event_pos <- which(event_mask)
      latent_start <- if (!is.null(latent)) start_time + latent$start else NA_real_
      latent_end <- if (!is.null(latent)) start_time + latent$end else NA_real_
      event_rows[[length(event_rows) + 1L]] <<- data.frame(
        Template_ID = template_id, Event_ID = event_id, Run_ID = run_id,
        State_Envelope_ID = state_id, Event_Label = event_label,
        Pause_Subtype = pause_subtype, Event_Context = event_context,
        Latent_Pulse_ID = latent_id, Latent_Start_u = latent_start, Latent_End_u = latent_end,
        Latent_Duration_u = latent_end - latent_start,
        Realized_Start_u = starts[min(event_pos)], Realized_End_u = ends[max(event_pos)],
        Realized_Duration_u = ends[max(event_pos)] - starts[min(event_pos)],
        Realized_Left_Spike_Index = new_rows$Left_Spike_Index[min(event_pos)],
        Realized_Right_Spike_Index = new_rows$Right_Spike_Index[max(event_pos)],
        N_Realized_Spikes = length(event_pos) + 1L, N_Realized_ISIs = length(event_pos),
        Rate_Multiplier = if (!is.null(latent)) latent$multiplier else NA_real_,
        Target_Mean_ISI_Contrast = if (!is.null(latent)) latent$contrast else NA_real_,
        Pulse_Refractory_Limited = if (!is.null(latent)) latent$refractory_limited else FALSE,
        Latent_Window_Truncated = if (!is.null(latent)) latent$truncated else FALSE,
        Pulse_Strength_Stratum = if (!is.null(latent)) latent$stratum else "none",
        Frozen_Target_Spikes = target_spikes, Burst_Quota_ID = quota_id,
        Is_Four_Spike_Boundary = event_label == "Burst" && target_spikes == 4L,
        Observed_Left_Context_ISIs = min(event_pos) - 1L,
        Observed_Right_Context_ISIs = n_isi - max(event_pos),
        Composite_HFS_Regime_ID = "none",
        Boundary_Definition = if (event_label == "Burst") "first_to_last_pulse_exposed_spike" else "observed_first_to_last_pause_boundary_spike",
        stringsAsFactors = FALSE
      )
    }
    if (any(separator_mask)) {
      pos <- which(separator_mask)
      separator_rows[[length(separator_rows) + 1L]] <<- data.frame(
        Template_ID = template_id, Contextual_Separator_ID = separator_id, Run_ID = run_id,
        Secondary_Label = parameters$contextual_separator$secondary_label,
        Primary_Event_Label = "none", Start_u = starts[min(pos)], End_u = ends[max(pos)],
        Duration_u = sum(run$isis[pos]), N_ISIs = length(pos), N_Boundary_Spikes = length(pos) + 1L,
        Secondary_Event_Subtype = "contextual_interburst_gap_separator",
        Product_Level_Mapping = "Pause", Secondary_Scoring_Role = "secondary_target_excluded_from_primary_pause",
        stringsAsFactors = FALSE
      )
    }
    if (any(borderline_mask)) {
      pos <- which(borderline_mask)
      borderline_rows[[length(borderline_rows) + 1L]] <<- data.frame(
        Template_ID = template_id, Borderline_Gap_ID = borderline_id, Run_ID = run_id,
        Secondary_Label = parameters$borderline_gap$secondary_label,
        Primary_Event_Label = "none", Start_u = starts[min(pos)], End_u = ends[max(pos)],
        Duration_u = sum(run$isis[pos]), N_ISIs = length(pos), N_Boundary_Spikes = length(pos) + 1L,
        Canonical_Pause_Truth = FALSE, Secondary_Event_Subtype = "tonic_tail_overlap_gap",
        Product_Level_Mapping = "Pause", Secondary_Scoring_Role = "secondary_target_excluded_from_primary_pause",
        stringsAsFactors = FALSE
      )
    }
    current_time <<- tail(ends, 1)
    interval_index <<- interval_index + n_isi
    list(run_id = run_id, event_id = event_id, state_id = state_id,
         separator_id = separator_id, borderline_id = borderline_id)
  }

  trim_pulse_run <- function(run) {
    exposed <- run$exposed_indices
    lo <- min(exposed); hi <- max(exposed)
    original_start <- run$times[lo]
    run$times <- run$times[lo:hi] - original_start
    run$isis <- diff(run$times)
    run$pulse_start <- run$pulse_start - original_start
    run$pulse_end <- run$pulse_end - original_start
    run$latent_window_truncated <- run$pulse_start < 0 || run$pulse_end > tail(run$times, 1)
    run$pulse_start <- max(0, run$pulse_start)
    run$pulse_end <- min(tail(run$times, 1), run$pulse_end)
    run$pulse_exposed <- rep(TRUE, length(run$times))
    run$exposed_indices <- seq_along(run$times)
    run
  }

  slice_pulse_run <- function(run, first_spike, last_spike) {
    old_exposed <- run$exposed_indices
    shift <- run$times[first_spike]
    run$times <- run$times[first_spike:last_spike] - shift
    run$isis <- diff(run$times)
    run$pulse_start <- run$pulse_start - shift
    run$pulse_end <- run$pulse_end - shift
    run$latent_window_truncated <- run$pulse_start < 0 || run$pulse_end > tail(run$times, 1)
    run$pulse_start <- max(0, run$pulse_start)
    run$pulse_end <- min(tail(run$times, 1), run$pulse_end)
    run$pulse_exposed <- run$pulse_exposed[first_spike:last_spike]
    run$exposed_indices <- old_exposed[old_exposed >= first_spike & old_exposed <= last_spike] - first_spike + 1L
    run
  }

  retain_pre_and_pulse <- function(run) slice_pulse_run(run, 1L, max(run$exposed_indices))
  retain_pulse_and_post <- function(run) slice_pulse_run(run, min(run$exposed_indices), length(run$times))

  seeds_for <- function(context, slot, replicate = 1L) {
    context <- match.arg(context, c("standalone", "hfs"))
    streams <- parameters$rng_contract$pulse_streams[[context]]
    if (is.null(streams) || !length(streams) || !all(streams %in% parameters$rng_contract$streams))
      stop("Pulse RNG streams are inconsistent with the frozen RNG contract.", call. = FALSE)
    stats::setNames(lapply(streams, function(stream)
      component_seed(template_id, stream, slot, replicate)), streams)
  }

  append_pulse_quota <- function(quota_row,
                                 trim_mode = c("none", "realized", "pre_and_pulse", "pulse_and_post"),
                                 maximum_realized_median = Inf) {
    trim_mode <- match.arg(trim_mode)
    context <- quota_row$Context
    regime <- if (context == "hfs") quota_row$HFS_Regime else NA_character_
    hfs_overlap <- if (context == "hfs") frozen_hfs_rate_overlap_stratum(template_index, regime) else "not_applicable"
    slot <- quota_row$Burst_Quota_ID
    component <- if (context == "hfs") "hfs_with_nested_burst" else "burst"
    run <- NULL; seeds <- NULL
    for (replicate in seq_len(parameters$acceptance_contract$maximum_attempts_per_run)) {
      seeds <- seeds_for(context, slot, replicate)
      candidate <- generate_pulse_run(context, regime, quota_row$Strength, quota_row$Target_Spikes,
                                      hfs_rate_overlap_stratum = hfs_overlap, rng_seeds = seeds)
      if (trim_mode == "realized") candidate <- trim_pulse_run(candidate)
      if (trim_mode == "pre_and_pulse") candidate <- retain_pre_and_pulse(candidate)
      if (trim_mode == "pulse_and_post") candidate <- retain_pulse_and_post(candidate)
      exposed <- candidate$exposed_indices
      realized_isis <- candidate$isis[min(exposed):(max(exposed) - 1L)]
      if (stats::median(realized_isis) <= maximum_realized_median) {
        run <- candidate
        break
      }
    }
    if (is.null(run)) stop("Pulse component could not meet its pre-frozen local contract.", call. = FALSE)
    exposed <- run$exposed_indices
    mask <- rep(FALSE, length(run$isis))
    mask[min(exposed):(max(exposed) - 1L)] <- TRUE
    latent <- list(start = run$pulse_start, end = run$pulse_end,
                   multiplier = run$multiplier, stratum = run$pulse_strength_stratum,
                   contrast = run$target_mean_isi_contrast,
                   refractory_limited = run$pulse_refractory_limited,
                   truncated = run$latent_window_truncated)
    append_intervals(
      run, state_label = if (context == "hfs") "Broad_HFS" else "none",
      event_mask = mask, event_label = "Burst",
      provenance = if (context == "hfs") "broad_hfs_shifted_gamma" else "standalone_background_shifted_gamma",
      regime = run$regime, burst_role = ifelse(mask, "pulse_realized_support", "none"),
      hfs_role = ifelse(context == "hfs", "baseline", "none"), latent = latent,
      event_context = if (context == "hfs") "burst_in_hfs" else "standalone_burst",
      burst_quota_row = quota_row, component_type = component, component_slot = slot,
      component_seeds = seeds, hfs_rate_overlap_stratum = hfs_overlap
    )
  }

  append_background <- function(after_macro_id) {
    slot <- paste0("after_", after_macro_id)
    seed <- component_seed(template_id, "background", slot)
    run <- with_component_rng(seed, generate_background_run())
    append_intervals(run, provenance = "background_shifted_gamma", component_type = "background",
                     component_slot = slot, component_seeds = list(point_process = seed))
  }
  append_tonic <- function(subtype) {
    slot <- paste0("tonic_", subtype)
    intended <- parameters$tonic$intended_stratum_by_template[template_index]
    overlap_row <- parameters$tonic$rate_overlap_stratum_by_template[
      parameters$tonic$rate_overlap_stratum_by_template$Template_ID == template_id, , drop = FALSE]
    rate_overlap <- overlap_row[[subtype]]
    seed <- component_seed(template_id, "tonic", slot)
    run <- with_component_rng(seed, generate_tonic_run(subtype, intended, rate_overlap))
    append_intervals(run, state_label = "Tonic", provenance = paste0("tonic_shifted_gamma_", subtype),
                     regime = subtype, tonic_subtype = subtype, component_type = "tonic",
                     component_slot = slot, component_seeds = list(point_process = seed),
                     tonic_intended_stratum = intended,
                     tonic_rate_overlap_stratum = rate_overlap)
  }
  append_hfs_plain <- function(regime) {
    slot <- paste0("plain_hfs_", regime)
    rate_overlap <- frozen_hfs_rate_overlap_stratum(template_index, regime)
    seed <- component_seed(template_id, "hfs", slot)
    run <- with_component_rng(seed, generate_hfs_run(regime, rate_overlap))
    append_intervals(run, state_label = "Broad_HFS", provenance = "broad_hfs_shifted_gamma", regime = regime,
                     component_type = "hfs", component_slot = slot,
                     component_seeds = list(point_process = seed),
                     hfs_rate_overlap_stratum = rate_overlap)
  }
  append_pause <- function(subtype = c("canonical", "complex_multi_gap"), semantic_slot) {
    subtype <- match.arg(subtype)
    slot <- paste0(subtype, "_", semantic_slot)
    seed <- component_seed(template_id, "pause", slot)
    if (subtype == "canonical") {
      gap <- with_component_rng(seed, stats::runif(1, parameters$canonical_pause$gap_B[1], parameters$canonical_pause$gap_B[2]))
      run <- list(times = c(0, gap), isis = gap, pulse_exposed = c(FALSE, FALSE),
                  mean_isi = gap, shape = NA_real_, regime = "canonical_gap", attempts = 1L)
    } else {
      run <- with_component_rng(seed, generate_complex_pause_run())
    }
    append_intervals(run, event_mask = rep(TRUE, length(run$isis)), event_label = "Pause",
                     pause_subtype = subtype, provenance = paste0(subtype, "_mechanism"),
                     regime = subtype, event_context = subtype, component_type = "pause",
                     component_slot = slot, component_seeds = list(point_process = seed))
  }

  append_borderline_gap <- function() {
    slot <- "borderline_gap_1"
    seed <- component_seed(template_id, "borderline_gap", slot)
    gap <- with_component_rng(seed, stats::runif(1, parameters$borderline_gap$gap_B[1],
                                                 parameters$borderline_gap$gap_B[2]))
    if (gap >= parameters$borderline_gap$canonical_pause_lower_B)
      stop("Borderline gap entered the canonical Pause range.", call. = FALSE)
    run <- list(times = c(0, gap), isis = gap, pulse_exposed = c(FALSE, FALSE),
                mean_isi = gap, shape = NA_real_, regime = "borderline_gap", attempts = 1L)
    append_intervals(run, borderline_mask = TRUE, provenance = "borderline_gap_mechanism",
                     regime = "borderline_gap", component_type = "borderline_gap",
                     component_slot = slot, component_seeds = list(point_process = seed))
  }

  append_contextual_macro <- function(left_quota, right_quota) {
    left <- append_pulse_quota(left_quota, trim_mode = "pre_and_pulse",
                               maximum_realized_median = parameters$contextual_separator$gap_B[2] /
                                 parameters$contextual_separator$local_median_multiplier)
    left_rows <- interval_rows[interval_rows$Event_ID == left$event_id, , drop = FALSE]
    left_median <- stats::median(left_rows$ISI_u)
    right_run <- NULL; separator <- NA_real_; right_median <- NA_real_
    for (attempt in seq_len(parameters$acceptance_contract$maximum_attempts_per_run)) {
      seeds <- seeds_for("standalone", right_quota$Burst_Quota_ID, attempt)
      candidate <- generate_pulse_run("standalone", NA_character_, right_quota$Strength,
                                      right_quota$Target_Spikes, rng_seeds = seeds)
      candidate <- retain_pulse_and_post(candidate)
      exposed <- candidate$exposed_indices
      right_median <- stats::median(candidate$isis[min(exposed):(max(exposed) - 1L)])
      lower <- max(parameters$contextual_separator$gap_B[1],
                   parameters$contextual_separator$local_median_multiplier * left_median,
                   parameters$contextual_separator$local_median_multiplier * right_median)
      upper <- min(parameters$contextual_separator$gap_B[2],
                   parameters$contextual_separator$canonical_pause_lower_B - 1e-9)
      if (lower < upper) {
        sep_seed <- component_seed(template_id, "contextual_separator", "separator_1", attempt)
        separator <- with_component_rng(sep_seed, stats::runif(1, lower, upper))
        right_run <- candidate
        break
      }
    }
    if (is.null(right_run)) stop("Contextual separator contract could not be satisfied.", call. = FALSE)
    sep_run <- list(times = c(0, separator), isis = separator, pulse_exposed = c(FALSE, FALSE),
                    mean_isi = separator, shape = NA_real_, regime = "contextual_separator", attempts = attempt)
    sep <- append_intervals(sep_run, separator_mask = TRUE,
                            provenance = "contextual_gap_mechanism", regime = "contextual_separator",
                            component_type = "contextual_separator", component_slot = "separator_1",
                            component_seeds = list(point_process = sep_seed))
    exposed <- right_run$exposed_indices
    mask <- rep(FALSE, length(right_run$isis))
    mask[min(exposed):(max(exposed) - 1L)] <- TRUE
    latent <- list(start = right_run$pulse_start, end = right_run$pulse_end,
                   multiplier = right_run$multiplier, stratum = right_run$pulse_strength_stratum,
                   contrast = right_run$target_mean_isi_contrast,
                   refractory_limited = right_run$pulse_refractory_limited,
                   truncated = right_run$latent_window_truncated)
    right <- append_intervals(
      right_run, event_mask = mask, event_label = "Burst",
      provenance = "standalone_background_shifted_gamma", regime = "standalone_background",
      burst_role = ifelse(mask, "pulse_realized_support", "none"), latent = latent,
      event_context = "standalone_burst", burst_quota_row = right_quota,
      component_type = "burst", component_slot = right_quota$Burst_Quota_ID,
      component_seeds = seeds
    )
    contextual_rows[[length(contextual_rows) + 1L]] <<- data.frame(
      Template_ID = template_id, Left_Burst_Event_ID = left$event_id,
      Contextual_Separator_ID = sep$separator_id, Right_Burst_Event_ID = right$event_id,
      Left_Local_Median_ISI_u = left_median, Right_Local_Median_ISI_u = right_median,
      Separator_ISI_u = separator, Primary_Pause_Truth = FALSE,
      Secondary_Scoring_Target = TRUE, stringsAsFactors = FALSE
    )
  }

  strength_cycle <- c("weak", "moderate", "strong")
  strength_offset <- (template_index - 1L) %% length(strength_cycle)
  rotated_strengths <- strength_cycle[((seq_along(strength_cycle) - 1L + strength_offset) %%
                                         length(strength_cycle)) + 1L]
  context_left <- standalone_quota[match(rotated_strengths[1], standalone_quota$Strength), , drop = FALSE]
  context_right <- standalone_quota[match(rotated_strengths[2], standalone_quota$Strength), , drop = FALSE]
  direct_burst <- standalone_quota[match(rotated_strengths[3], standalone_quota$Strength), , drop = FALSE]
  has_complex <- template_id %in% parameters$complex_pause_templates
  tonic_pause_subtype <- if (template_index %% 2L) "generic_stress" else "stn_like_empirical"
  remaining_tonic_subtype <- setdiff(c("generic_stress", "stn_like_empirical"), tonic_pause_subtype)

  has_composite_regime <- template_id %in% parameters$composite_hfs_regime_templates
  macros <- if (has_composite_regime) list(
    list(type = "contextual_macro"), list(type = "direct_burst_hfs_macro"),
    list(type = "composite_hfs_pause_hfs_macro"), list(type = "tonic_canonical_pause_macro"),
    list(type = if (has_complex) "tonic_complex_pause_macro" else "tonic_only"),
    list(type = "borderline_gap_only")
  ) else list(
    list(type = "contextual_macro"), list(type = "direct_burst_hfs_macro"),
    list(type = "hfs_overlay", slot = "hfs_1"), list(type = "hfs_overlay", slot = "hfs_2"),
    list(type = "tonic_canonical_pause_macro"),
    list(type = if (has_complex) "tonic_complex_pause_macro" else "tonic_only"),
    list(type = "canonical_pause_only"), list(type = "borderline_gap_only")
  )
  for (m in seq_along(macros)) {
    macros[[m]]$id <- if (macros[[m]]$type == "hfs_overlay")
      paste0("hfs_overlay_", macros[[m]]$slot) else macros[[m]]$type
  }
  macro_start_state <- function(macro) {
    if (macro$type %in% c("hfs_overlay", "composite_hfs_pause_hfs_macro") ||
        (macro$type == "direct_burst_hfs_macro" && template_index %% 2L == 0L)) return("Broad_HFS")
    if (macro$type %in% c("tonic_canonical_pause_macro", "tonic_complex_pause_macro", "tonic_only")) return("Tonic")
    "none"
  }
  macro_end_state <- function(macro) {
    if (macro$type %in% c("hfs_overlay", "composite_hfs_pause_hfs_macro") ||
        (macro$type == "direct_burst_hfs_macro" && template_index %% 2L == 1L)) return("Broad_HFS")
    if (macro$type == "tonic_only") return("Tonic")
    "none"
  }
  schedule_seed <- component_seed(template_id, "schedule", "macro_order")
  order_idx <- with_component_rng(schedule_seed, sample(seq_along(macros)))
  macros <- macros[order_idx]
  background_flags <- vapply(macros, function(macro) {
    with_component_rng(component_seed(template_id, "schedule", paste0("background_after_", macro$id)),
                       stats::runif(1) < parameters$background$between_macro_insertion_probability)
  }, logical(1))
  forced_same_state_boundary_separator <- rep(FALSE, length(macros))
  if (length(macros) > 1L) for (m in seq_len(length(macros) - 1L)) {
    # Two separately scored States with the same primary label must not touch
    # across a macro boundary: without an observed separator that boundary is
    # not identifiable and a defensible detector merge would be mislabeled.
    left_state <- macro_end_state(macros[[m]])
    right_state <- macro_start_state(macros[[m + 1L]])
    if (left_state != "none" && identical(left_state, right_state)) {
      background_flags[m] <- TRUE
      forced_same_state_boundary_separator[m] <- TRUE
    }
  }
  macro_order <- character()
  for (m in seq_along(macros)) {
    macro <- macros[[m]]
    macro_order <- c(macro_order, macro$id)
    if (macro$type == "contextual_macro") append_contextual_macro(context_left, context_right)
    if (macro$type == "direct_burst_hfs_macro") {
      if (template_index %% 2L) {
        append_pulse_quota(direct_burst, trim_mode = "none")
        append_hfs_plain(plain_hfs_regime)
      } else {
        append_hfs_plain(plain_hfs_regime)
        append_pulse_quota(direct_burst, trim_mode = "none")
      }
    }
    if (macro$type == "hfs_overlay") {
      q <- hfs_quota[hfs_quota$Burst_Slot == macro$slot, , drop = FALSE]
      append_pulse_quota(q, trim_mode = "none")
    }
    if (macro$type == "composite_hfs_pause_hfs_macro") {
      q1 <- hfs_quota[hfs_quota$Burst_Slot == "hfs_1", , drop = FALSE]
      q2 <- hfs_quota[hfs_quota$Burst_Slot == "hfs_2", , drop = FALSE]
      left <- append_pulse_quota(q1, trim_mode = "none")
      pause <- append_pause("canonical", "composite_hfs_connector")
      right <- append_pulse_quota(q2, trim_mode = "none")
      regime_counter <- regime_counter + 1L
      regime_rows[[length(regime_rows) + 1L]] <- data.frame(
        Template_ID = template_id,
        Composite_HFS_Regime_ID = sprintf("%s_REGIME_%02d", template_id, regime_counter),
        Left_State_Envelope_ID = left$state_id, Canonical_Pause_Event_ID = pause$event_id,
        Right_State_Envelope_ID = right$state_id, Start_u = NA_real_, End_u = NA_real_,
        Relationship = "HFS_State--canonical_Pause--HFS_State",
        Continuous_HFS_State = FALSE, stringsAsFactors = FALSE)
    }
    if (macro$type == "tonic_canonical_pause_macro") {
      append_tonic(tonic_pause_subtype)
      append_pause("canonical", "after_tonic")
    }
    if (macro$type == "tonic_complex_pause_macro") {
      append_tonic(remaining_tonic_subtype)
      append_pause("complex_multi_gap", "after_tonic")
    }
    if (macro$type == "tonic_only") append_tonic(remaining_tonic_subtype)
    if (macro$type == "canonical_pause_only") append_pause("canonical", "standalone")
    if (macro$type == "borderline_gap_only") append_borderline_gap()
    if (m < length(macros) && background_flags[m]) append_background(macro$id)
  }

  intervals <- interval_rows
  spikes <- data.frame(Template_ID = template_id, Spike_Index = seq_len(nrow(intervals) + 1L),
                       Time_u = c(0, intervals$End_u), stringsAsFactors = FALSE)
  events <- do.call(rbind, event_rows); states <- do.call(rbind, state_rows); runs <- do.call(rbind, run_rows)
  separators <- do.call(rbind, separator_rows); borderline <- do.call(rbind, borderline_rows)
  contextual <- do.call(rbind, contextual_rows)
  regimes <- if (length(regime_rows)) do.call(rbind, regime_rows) else data.frame(
    Template_ID = character(), Composite_HFS_Regime_ID = character(), Left_State_Envelope_ID = character(),
    Canonical_Pause_Event_ID = character(), Right_State_Envelope_ID = character(), Start_u = numeric(),
    End_u = numeric(), Relationship = character(), Continuous_HFS_State = logical(), stringsAsFactors = FALSE)
  if (nrow(regimes)) {
    for (j in seq_len(nrow(regimes))) {
      rid <- regimes$Composite_HFS_Regime_ID[j]
      state_ids <- c(regimes$Left_State_Envelope_ID[j], regimes$Right_State_Envelope_ID[j])
      pause_id <- regimes$Canonical_Pause_Event_ID[j]
      states$Composite_HFS_Regime_ID[states$State_Envelope_ID %in% state_ids] <- rid
      events$Composite_HFS_Regime_ID[events$Event_ID == pause_id] <- rid
      interval_rows$Composite_HFS_Regime_ID[interval_rows$State_Envelope_ID %in% state_ids |
                                            interval_rows$Event_ID == pause_id] <- rid
      interval_rows$Hard_Cut_Reason[interval_rows$Event_ID == pause_id] <- "canonical_pause"
      states$End_Hard_Cut_Reason[states$State_Envelope_ID == state_ids[1]] <- "canonical_pause"
      states$Start_Hard_Cut_Reason[states$State_Envelope_ID == state_ids[2]] <- "canonical_pause"
      regimes$Start_u[j] <- states$Start_u[match(state_ids[1], states$State_Envelope_ID)]
      regimes$End_u[j] <- states$End_u[match(state_ids[2], states$State_Envelope_ID)]
    }
  }
  intervals <- interval_rows
  transitions <- do.call(rbind, lapply(seq_len(nrow(runs) - 1L), function(i) {
    left <- runs[i, ]; right <- runs[i + 1L, ]
    data.frame(
      Template_ID = template_id, Transition_ID = sprintf("%s_TRANS_%03d", template_id, i),
      Boundary_u = left$End_u, Left_Run_ID = left$Run_ID, Right_Run_ID = right$Run_ID,
      Left_Descriptor = left$Primary_Descriptor, Right_Descriptor = right$Primary_Descriptor,
      Direct_Target_Boundary = left$Primary_Descriptor != "Background" && right$Primary_Descriptor != "Background",
      Transition_Type = paste(left$Primary_Descriptor, right$Primary_Descriptor, sep = " -> "),
      stringsAsFactors = FALSE
    )
  }))
  quota_row <- data.frame(
    Template_ID = template_id, Split = if (template_id %in% parameters$development_templates) "development" else "holdout",
    Standalone_Burst = 3L, Burst_in_HFS = 2L, Burst_Total = 5L,
    Four_Spike_Burst = sum(template_quota$Is_Four_Spike_Boundary),
    Canonical_Pause = 2L, Complex_Pause = as.integer(has_complex),
    Contextual_Separator_secondary = 1L, Borderline_Gap_secondary = 1L,
    Tonic_generic = 1L, Tonic_STN_like = 1L,
    Broad_HFS = 3L, Composite_HFS_Regime = as.integer(has_composite_regime),
    Forced_Same_State_Boundary_Separators = sum(forced_same_state_boundary_separator),
    Direct_Target_Boundaries = sum(transitions$Direct_Target_Boundary),
    Macro_Order = paste(macro_order, collapse = " > "),
    Template_Duration_u = tail(spikes$Time_u, 1), N_Spikes = nrow(spikes), stringsAsFactors = FALSE
  )
  list(spikes = spikes, intervals = intervals, events = events, states = states,
       runs = runs, separators = separators, borderline = borderline,
       contextual = contextual, regimes = regimes,
       transitions = transitions, quota = quota_row)
}
