#!/usr/bin/env Rscript

# Clean four-pattern synthetic mechanism benchmark, version 1.0.0.
#
# This standalone benchmark wrapper intentionally does not call the general
# simulator's automatic time-proportion scheduler or its single-track labeling
# system. It generates dimensionless paired templates with dual state/event
# truth, then scales every timestamp exactly by 1, 4, and 10.

options(stringsAsFactors = FALSE, warn = 1)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) stop("Run with Rscript.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
output_dir <- dirname(script_path)
reference_simulator <- file.path(output_dir, "reference", "Spike_train_simulator_V13_5_0_HF_modes.R")

required_packages <- c("ggplot2", "jsonlite", "yaml", "digest", "openxlsx")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
if (!file.exists(reference_simulator)) stop("Frozen reference simulator is missing.", call. = FALSE)

for (subdir in c("detector_inputs", "ground_truth", "calibration", "tables", "metadata", "figures")) {
  dir.create(file.path(output_dir, subdir), recursive = TRUE, showWarnings = FALSE)
}

parameters <- list(
  dataset_id = "clean_synthetic_mechanism_benchmark_v1_0_0",
  n_templates = 20L,
  calibration_templates = sprintf("TPL_%03d", 1:5),
  holdout_templates = sprintf("TPL_%03d", 6:20),
  template_seed_base = 2026082500L,
  sample_blinding_seed = 2026082591L,
  calibration_selection_seed_base = 2026082600L,
  anchor_B_s = 0.100,
  scale_factors = c(1, 4, 10),
  workbook_blind_batches = list(Batch_A = 4, Batch_B = 10, Batch_C = 1),
  target_labels = c("Burst", "Pause", "Tonic", "Broad_HFS"),
  state_labels = c("none", "Tonic", "Broad_HFS"),
  event_labels = c("none", "Burst", "Pause"),
  quota_ranges = list(
    Burst = c(3L, 5L),
    Pause = c(3L, 4L),
    Canonical_Pause = c(2L, 3L),
    Contextual_Separator = c(0L, 1L),
    Tonic = c(2L, 4L),
    Broad_HFS = c(2L, 4L),
    Burst_in_HFS = c(0L, 1L)
  ),
  burst = list(
    boundary_spikes = c(5L, 7L),
    core_isi_B = c(0.04, 0.18),
    bridge_probability = 0.40,
    bridge_max_count = 1L,
    bridge_median_multiplier = 3.5,
    bridge_cap_B = 0.45,
    minimum_core_isis_after_bridge = 3L
  ),
  broad_hfs = list(
    boundary_spikes = c(20L, 35L),
    direct_isi_B = c(0.08, 0.40),
    connector_isi_B = c(0.40, 0.60),
    connector_fraction_max = 0.10,
    max_consecutive_connectors = 1L,
    minimum_duration_B = 3.0
  ),
  tonic = list(
    boundary_spikes = c(8L, 12L),
    isi_B = c(0.70, 1.30),
    cv_max = 0.30,
    cv2_max = 0.38,
    lv_max = 0.28
  ),
  canonical_pause = list(isi_B = c(2.8, 5.0), interval_count = 1L),
  contextual_separator = list(
    isi_B = c(0.45, 0.65),
    burst_median_multiplier = 3.5,
    canonical_pause_lower_B = 2.8
  ),
  background = list(isi_B = c(0.45, 0.65), interval_count = c(1L, 2L)),
  stimulation_enabled = FALSE,
  observation_noise_enabled = FALSE,
  drift_enabled = FALSE
)

uniform_open_closed <- function(n, lower, upper) {
  lower + (upper - lower) * stats::runif(n, min = .Machine$double.eps, max = 1)
}

regularity_metrics <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  mean_x <- mean(x)
  cv <- if (n > 1L && mean_x > 0) stats::sd(x) / mean_x else 0
  cv2 <- 0
  lv <- 0
  if (n > 1L) {
    denom <- x[-n] + x[-1]
    delta <- x[-1] - x[-n]
    cv2 <- mean(2 * abs(delta) / denom)
    lv <- mean(3 * (delta / denom)^2)
  }
  c(mean = mean_x, cv = cv, cv2 = cv2, lv = lv)
}

max_consecutive_true <- function(x) {
  rr <- rle(as.logical(x))
  if (!any(rr$values)) return(0L)
  as.integer(max(rr$lengths[rr$values]))
}

generate_burst <- function() {
  cfg <- parameters$burst
  n_spikes <- sample(seq.int(cfg$boundary_spikes[1], cfg$boundary_spikes[2]), 1)
  n_isi <- n_spikes - 1L
  use_bridge <- stats::runif(1) < cfg$bridge_probability

  if (use_bridge) {
    core <- stats::runif(n_isi - 1L, cfg$core_isi_B[1], cfg$core_isi_B[2])
    core_median <- stats::median(core)
    bridge_upper <- min(cfg$bridge_median_multiplier * core_median, cfg$bridge_cap_B)
    if (bridge_upper > cfg$core_isi_B[2] + 1e-12) {
      bridge_position <- sample(2:(n_isi - 1L), 1)
      bridge <- uniform_open_closed(1, cfg$core_isi_B[2], bridge_upper)
      isi <- append(core, bridge, after = bridge_position - 1L)
      role <- rep("core", n_isi)
      role[bridge_position] <- "bridge"
    } else {
      isi <- stats::runif(n_isi, cfg$core_isi_B[1], cfg$core_isi_B[2])
      role <- rep("core", n_isi)
    }
  } else {
    isi <- stats::runif(n_isi, cfg$core_isi_B[1], cfg$core_isi_B[2])
    role <- rep("core", n_isi)
  }

  list(isi = isi, role = role, core_median = stats::median(isi[role == "core"]))
}

generate_tonic <- function() {
  cfg <- parameters$tonic
  for (attempt in 1:1000) {
    n_spikes <- sample(seq.int(cfg$boundary_spikes[1], cfg$boundary_spikes[2]), 1)
    n_isi <- n_spikes - 1L
    center <- stats::runif(1, 0.82, 1.18)
    sd_target <- center * stats::runif(1, 0.04, 0.10)
    isi <- numeric(n_isi)
    for (i in seq_len(n_isi)) {
      accepted <- FALSE
      for (draw in 1:100) {
        value <- stats::rnorm(1, center, sd_target)
        if (value >= cfg$isi_B[1] && value <= cfg$isi_B[2]) {
          isi[i] <- value
          accepted <- TRUE
          break
        }
      }
      if (!accepted) isi[i] <- min(max(center, cfg$isi_B[1]), cfg$isi_B[2])
    }
    metrics <- regularity_metrics(isi)
    if (metrics["cv"] <= cfg$cv_max && metrics["cv2"] <= cfg$cv2_max && metrics["lv"] <= cfg$lv_max) {
      return(list(isi = isi, metrics = metrics))
    }
  }
  stop("Unable to generate a valid Tonic state.", call. = FALSE)
}

choose_nonadjacent_positions <- function(candidates, count) {
  if (count <= 0L) return(integer(0))
  for (attempt in 1:500) {
    picked <- sort(sample(candidates, count))
    if (length(picked) <= 1L || all(diff(picked) > 1L)) return(picked)
  }
  integer(0)
}

generate_hfs <- function(with_overlay = FALSE) {
  cfg <- parameters$broad_hfs
  for (attempt in 1:2000) {
    n_spikes <- sample(seq.int(cfg$boundary_spikes[1], cfg$boundary_spikes[2]), 1)
    n_isi <- n_spikes - 1L
    overlay <- if (with_overlay) generate_burst() else NULL
    overlay_positions <- integer(0)
    if (with_overlay) {
      overlay_n <- length(overlay$isi)
      if (n_isi - overlay_n < 8L) next
      overlay_start <- sample(5:(n_isi - overlay_n - 3L), 1)
      overlay_positions <- overlay_start:(overlay_start + overlay_n - 1L)
    }

    available <- setdiff(seq_len(n_isi), overlay_positions)
    max_connectors <- floor(cfg$connector_fraction_max * n_isi + 1e-12)
    connector_count <- if (max_connectors > 0L) sample(0:max_connectors, 1) else 0L
    connector_positions <- choose_nonadjacent_positions(available, connector_count)
    if (length(connector_positions) != connector_count) next

    isi <- stats::runif(n_isi, cfg$direct_isi_B[1], cfg$direct_isi_B[2])
    hfs_role <- rep("direct", n_isi)
    burst_role <- rep("none", n_isi)
    if (connector_count > 0L) {
      isi[connector_positions] <- uniform_open_closed(connector_count, cfg$connector_isi_B[1], cfg$connector_isi_B[2])
      hfs_role[connector_positions] <- "connector"
    }
    if (with_overlay) {
      isi[overlay_positions] <- overlay$isi
      hfs_role[overlay_positions] <- "overlay"
      burst_role[overlay_positions] <- overlay$role
    }
    if (sum(isi) < cfg$minimum_duration_B) next
    if (max_consecutive_true(hfs_role == "connector") > cfg$max_consecutive_connectors) next

    return(list(
      isi = isi,
      hfs_role = hfs_role,
      burst_role = burst_role,
      overlay_positions = overlay_positions,
      overlay_core_median = if (with_overlay) overlay$core_median else NA_real_
    ))
  }
  stop("Unable to generate a valid Broad HFS state.", call. = FALSE)
}

sample_quota <- function(template_index) {
  set.seed(parameters$template_seed_base + template_index)
  contextual <- sample(0:1, 1)
  canonical <- if (contextual == 1L) sample(2:3, 1) else 3L
  burst_total <- sample(3:5, 1)
  overlay <- sample(0:1, 1)
  if (burst_total - overlay < 2L * contextual) overlay <- 0L
  c(
    Burst = burst_total,
    Pause = canonical + contextual,
    Canonical_Pause = canonical,
    Contextual_Separator = contextual,
    Tonic = sample(2:4, 1),
    Broad_HFS = sample(2:4, 1),
    Burst_in_HFS = overlay
  )
}

shuffle_units <- function(units) {
  types <- vapply(units, function(x) x$type, character(1))
  for (attempt in 1:2000) {
    order_idx <- sample(seq_along(units))
    ordered_types <- types[order_idx]
    if (length(ordered_types) < 2L || all(ordered_types[-1] != ordered_types[-length(ordered_types)])) {
      return(units[order_idx])
    }
  }
  units[sample(seq_along(units))]
}

make_units <- function(quota) {
  units <- list()
  add_unit <- function(type, overlay = FALSE) {
    units[[length(units) + 1L]] <<- list(type = type, overlay = overlay)
  }
  if (quota["Contextual_Separator"] == 1L) add_unit("Contextual_Macro")
  standalone_bursts <- quota["Burst"] - quota["Burst_in_HFS"] - 2L * quota["Contextual_Separator"]
  for (i in seq_len(standalone_bursts)) add_unit("Burst")
  for (i in seq_len(quota["Canonical_Pause"])) add_unit("Canonical_Pause")
  for (i in seq_len(quota["Tonic"])) add_unit("Tonic")
  for (i in seq_len(quota["Broad_HFS"])) add_unit("Broad_HFS", overlay = i <= quota["Burst_in_HFS"])
  shuffle_units(units)
}

generate_template <- function(template_index) {
  template_id <- sprintf("TPL_%03d", template_index)
  seed <- parameters$template_seed_base + template_index
  set.seed(seed)
  quota <- sample_quota(template_index)
  set.seed(seed + 100000L)
  units <- make_units(quota)

  interval_rows <- list()
  run_counter <- 0L
  event_counter <- 0L
  state_counter <- 0L
  macro_counter <- 0L
  interval_counter <- 0L
  current_time <- stats::runif(1, parameters$background$isi_B[1], parameters$background$isi_B[2])
  initial_latency <- current_time
  contextual_links <- list()

  next_run_id <- function() {
    run_counter <<- run_counter + 1L
    sprintf("%s_R%03d", template_id, run_counter)
  }
  next_event_id <- function() {
    event_counter <<- event_counter + 1L
    sprintf("%s_E%03d", template_id, event_counter)
  }
  next_state_id <- function() {
    state_counter <<- state_counter + 1L
    sprintf("%s_S%03d", template_id, state_counter)
  }
  next_macro_id <- function() {
    macro_counter <<- macro_counter + 1L
    sprintf("%s_M%03d", template_id, macro_counter)
  }

  append_intervals <- function(isi, run_id, macro_id = "none", state_label = "none",
                               event_label = "none", event_id = "none", state_id = "none",
                               direct_hfs = NULL, hfs_role = NULL, burst_role = NULL,
                               pause_subtype = "none", source_component) {
    n <- length(isi)
    if (is.null(direct_hfs)) direct_hfs <- rep(FALSE, n)
    if (is.null(hfs_role)) hfs_role <- rep("none", n)
    if (is.null(burst_role)) burst_role <- rep("none", n)
    for (j in seq_len(n)) {
      interval_counter <<- interval_counter + 1L
      start <- current_time
      current_time <<- current_time + isi[j]
      interval_rows[[interval_counter]] <<- data.frame(
        Template_ID = template_id,
        Interval_Index = interval_counter,
        Left_Spike_Index = interval_counter,
        Right_Spike_Index = interval_counter + 1L,
        Start_u = start,
        End_u = current_time,
        ISI_u = isi[j],
        Run_ID = run_id,
        Macro_Unit_ID = macro_id,
        State_Label = state_label,
        Event_Label = event_label,
        Event_ID = event_id,
        State_Envelope_ID = state_id,
        Direct_HFS_Support = direct_hfs[j],
        HFS_ISI_Role = hfs_role[j],
        Burst_Role = burst_role[j],
        Pause_Subtype = pause_subtype,
        Source_Component = source_component,
        stringsAsFactors = FALSE
      )
    }
  }

  add_background <- function() {
    n <- sample(seq.int(parameters$background$interval_count[1], parameters$background$interval_count[2]), 1)
    append_intervals(
      stats::runif(n, parameters$background$isi_B[1], parameters$background$isi_B[2]),
      run_id = next_run_id(),
      source_component = "background"
    )
  }
  add_burst <- function(macro_id = "none", state_label = "none", state_id = "none", source = "burst") {
    burst <- generate_burst()
    event_id <- next_event_id()
    append_intervals(
      burst$isi,
      run_id = next_run_id(),
      macro_id = macro_id,
      state_label = state_label,
      event_label = "Burst",
      event_id = event_id,
      state_id = state_id,
      burst_role = burst$role,
      source_component = source
    )
    list(event_id = event_id, core_median = burst$core_median)
  }
  add_canonical_pause <- function() {
    event_id <- next_event_id()
    append_intervals(
      stats::runif(1, parameters$canonical_pause$isi_B[1], parameters$canonical_pause$isi_B[2]),
      run_id = next_run_id(),
      event_label = "Pause",
      event_id = event_id,
      pause_subtype = "canonical",
      source_component = "canonical_pause"
    )
  }
  add_contextual_macro <- function() {
    for (attempt in 1:1000) {
      left <- generate_burst()
      right <- generate_burst()
      lower <- max(
        parameters$contextual_separator$isi_B[1],
        parameters$burst$core_isi_B[2],
        parameters$contextual_separator$burst_median_multiplier * left$core_median,
        parameters$contextual_separator$burst_median_multiplier * right$core_median
      )
      if (lower < parameters$contextual_separator$isi_B[2] - 1e-12) break
    }
    if (lower >= parameters$contextual_separator$isi_B[2] - 1e-12) {
      stop("Unable to generate contextual separator.", call. = FALSE)
    }
    macro_id <- next_macro_id()
    left_event <- next_event_id()
    append_intervals(
      left$isi, next_run_id(), macro_id = macro_id,
      event_label = "Burst", event_id = left_event,
      burst_role = left$role, source_component = "contextual_left_burst"
    )
    pause_event <- next_event_id()
    gap <- uniform_open_closed(1, lower, parameters$contextual_separator$isi_B[2])
    append_intervals(
      gap, next_run_id(), macro_id = macro_id,
      event_label = "Pause", event_id = pause_event,
      pause_subtype = "contextual_interburst_gap",
      source_component = "contextual_separator"
    )
    right_event <- next_event_id()
    append_intervals(
      right$isi, next_run_id(), macro_id = macro_id,
      event_label = "Burst", event_id = right_event,
      burst_role = right$role, source_component = "contextual_right_burst"
    )
    contextual_links[[length(contextual_links) + 1L]] <<- data.frame(
      Template_ID = template_id,
      Macro_Unit_ID = macro_id,
      Left_Burst_Event_ID = left_event,
      Separator_Event_ID = pause_event,
      Right_Burst_Event_ID = right_event,
      Left_Core_Median_u = left$core_median,
      Right_Core_Median_u = right$core_median,
      Separator_ISI_u = gap,
      stringsAsFactors = FALSE
    )
  }
  add_tonic <- function() {
    tonic <- generate_tonic()
    append_intervals(
      tonic$isi,
      run_id = next_run_id(),
      state_label = "Tonic",
      state_id = next_state_id(),
      source_component = "tonic"
    )
  }
  add_hfs <- function(with_overlay) {
    hfs <- generate_hfs(with_overlay)
    state_id <- next_state_id()
    event_id <- if (with_overlay) next_event_id() else "none"
    event_labels <- rep("none", length(hfs$isi))
    event_ids <- rep("none", length(hfs$isi))
    if (with_overlay) {
      event_labels[hfs$overlay_positions] <- "Burst"
      event_ids[hfs$overlay_positions] <- event_id
    }
    run_id <- next_run_id()
    for (j in seq_along(hfs$isi)) {
      append_intervals(
        hfs$isi[j],
        run_id = run_id,
        state_label = "Broad_HFS",
        event_label = event_labels[j],
        event_id = event_ids[j],
        state_id = state_id,
        direct_hfs = hfs$hfs_role[j] == "direct",
        hfs_role = hfs$hfs_role[j],
        burst_role = hfs$burst_role[j],
        source_component = if (hfs$hfs_role[j] == "overlay") "burst_in_hfs" else "broad_hfs"
      )
    }
  }

  add_background()
  for (unit_index in seq_along(units)) {
    unit <- units[[unit_index]]
    if (unit$type == "Burst") add_burst()
    if (unit$type == "Canonical_Pause") add_canonical_pause()
    if (unit$type == "Contextual_Macro") add_contextual_macro()
    if (unit$type == "Tonic") add_tonic()
    if (unit$type == "Broad_HFS") add_hfs(unit$overlay)
    add_background()
  }

  intervals <- do.call(rbind, interval_rows)
  spike_times <- c(initial_latency, intervals$End_u)
  spikes <- data.frame(
    Template_ID = template_id,
    Spike_Index = seq_along(spike_times),
    Time_u = spike_times,
    stringsAsFactors = FALSE
  )
  links <- if (length(contextual_links) > 0L) do.call(rbind, contextual_links) else data.frame(
    Template_ID = character(0), Macro_Unit_ID = character(0),
    Left_Burst_Event_ID = character(0), Separator_Event_ID = character(0),
    Right_Burst_Event_ID = character(0), Left_Core_Median_u = numeric(0),
    Right_Core_Median_u = numeric(0), Separator_ISI_u = numeric(0)
  )

  event_idx <- intervals$Event_ID != "none"
  event_groups <- split(intervals[event_idx, , drop = FALSE], intervals$Event_ID[event_idx])
  events <- do.call(rbind, lapply(event_groups, function(x) {
    data.frame(
      Template_ID = template_id,
      Event_ID = x$Event_ID[1],
      Event_Label = x$Event_Label[1],
      Pause_Subtype = x$Pause_Subtype[1],
      Macro_Unit_ID = x$Macro_Unit_ID[1],
      State_Envelope_ID = if (all(x$State_Envelope_ID == "none")) "none" else unique(x$State_Envelope_ID)[1],
      Event_Context = if (x$Event_Label[1] == "Burst" && any(x$State_Label == "Broad_HFS")) "burst_in_hfs" else "standalone",
      Start_u = min(x$Start_u),
      End_u = max(x$End_u),
      Duration_u = max(x$End_u) - min(x$Start_u),
      N_ISIs = nrow(x),
      N_Boundary_Spikes = nrow(x) + 1L,
      N_Core_ISIs = sum(x$Burst_Role == "core"),
      N_Bridge_ISIs = sum(x$Burst_Role == "bridge"),
      Core_Median_ISI_u = if (any(x$Burst_Role == "core")) stats::median(x$ISI_u[x$Burst_Role == "core"]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(events) <- NULL

  state_idx <- intervals$State_Envelope_ID != "none"
  state_groups <- split(intervals[state_idx, , drop = FALSE], intervals$State_Envelope_ID[state_idx])
  states <- do.call(rbind, lapply(state_groups, function(x) {
    direct <- x$HFS_ISI_Role == "direct"
    connector <- x$HFS_ISI_Role == "connector"
    overlay_event <- unique(x$Event_ID[x$Event_ID != "none"])
    metrics <- regularity_metrics(x$ISI_u)
    data.frame(
      Template_ID = template_id,
      State_Envelope_ID = x$State_Envelope_ID[1],
      State_Label = x$State_Label[1],
      Run_ID = x$Run_ID[1],
      Start_u = min(x$Start_u),
      End_u = max(x$End_u),
      Duration_u = max(x$End_u) - min(x$Start_u),
      N_ISIs = nrow(x),
      N_Boundary_Spikes = nrow(x) + 1L,
      N_Direct_HFS_ISIs = sum(direct),
      N_Connector_ISIs = sum(connector),
      Connector_Fraction = sum(connector) / nrow(x),
      Max_Consecutive_Connectors = max_consecutive_true(connector),
      Overlay_Burst_Event_ID = if (length(overlay_event) > 0L) overlay_event[1] else "none",
      Mean_ISI_u = metrics["mean"],
      CV = metrics["cv"],
      CV2 = metrics["cv2"],
      LV = metrics["lv"],
      stringsAsFactors = FALSE
    )
  }))
  rownames(states) <- NULL

  list(
    template_id = template_id,
    seed = seed,
    quota = quota,
    units = units,
    intervals = intervals,
    spikes = spikes,
    events = events,
    states = states,
    contextual_links = links,
    initial_latency_u = initial_latency,
    duration_u = max(spike_times)
  )
}

templates <- lapply(seq_len(parameters$n_templates), generate_template)
names(templates) <- vapply(templates, function(x) x$template_id, character(1))

quota_manifest <- do.call(rbind, lapply(templates, function(x) {
  data.frame(
    Template_ID = x$template_id,
    Split = if (x$template_id %in% parameters$calibration_templates) "calibration" else "holdout",
    Seed = x$seed,
    Burst = unname(x$quota["Burst"]),
    Pause = unname(x$quota["Pause"]),
    Canonical_Pause = unname(x$quota["Canonical_Pause"]),
    Contextual_Separator = unname(x$quota["Contextual_Separator"]),
    Tonic = unname(x$quota["Tonic"]),
    Broad_HFS = unname(x$quota["Broad_HFS"]),
    Burst_in_HFS = unname(x$quota["Burst_in_HFS"]),
    Unit_Order = paste(vapply(x$units, function(u) if (u$overlay) "Broad_HFS+Burst" else u$type, character(1)), collapse = "|"),
    Initial_Latency_u = x$initial_latency_u,
    Template_Duration_u = x$duration_u,
    N_Spikes = nrow(x$spikes),
    stringsAsFactors = FALSE
  )
}))

dimensionless_intervals <- do.call(rbind, lapply(templates, `[[`, "intervals"))
dimensionless_spikes <- do.call(rbind, lapply(templates, `[[`, "spikes"))
dimensionless_events <- do.call(rbind, lapply(templates, `[[`, "events"))
dimensionless_states <- do.call(rbind, lapply(templates, `[[`, "states"))
contextual_links <- do.call(rbind, lapply(templates, `[[`, "contextual_links"))
rownames(dimensionless_intervals) <- NULL
rownames(dimensionless_spikes) <- NULL
rownames(dimensionless_events) <- NULL
rownames(dimensionless_states) <- NULL
rownames(contextual_links) <- NULL

sample_key <- expand.grid(
  Template_ID = names(templates),
  Scale_Factor = parameters$scale_factors,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
sample_key <- sample_key[order(sample_key$Template_ID, sample_key$Scale_Factor), ]
set.seed(parameters$sample_blinding_seed)
sample_key$Sample_ID <- sprintf("S%04d", sample(seq_len(nrow(sample_key))))
sample_key$B_s <- parameters$anchor_B_s * sample_key$Scale_Factor
batch_scale <- unlist(parameters$workbook_blind_batches, use.names = TRUE)
sample_key$Workbook_Batch <- names(batch_scale)[match(sample_key$Scale_Factor, as.numeric(batch_scale))]
sample_key$Split <- ifelse(sample_key$Template_ID %in% parameters$calibration_templates, "calibration", "holdout")
sample_key$Template_Duration_u <- quota_manifest$Template_Duration_u[match(sample_key$Template_ID, quota_manifest$Template_ID)]
sample_key$Recording_Duration_s <- sample_key$Template_Duration_u * sample_key$B_s

scale_table <- function(data, time_columns) {
  rows <- lapply(seq_len(nrow(sample_key)), function(i) {
    key <- sample_key[i, ]
    x <- data[data$Template_ID == key$Template_ID, , drop = FALSE]
    if (nrow(x) == 0L) return(NULL)
    x$Sample_ID <- key$Sample_ID
    x$Scale_Factor <- key$Scale_Factor
    x$B_s <- key$B_s
    x$Split <- key$Split
    for (column in time_columns) x[[sub("_u$", "_s", column)]] <- x[[column]] * key$B_s
    x
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

scaled_spikes <- scale_table(dimensionless_spikes, c("Time_u"))
scaled_intervals <- scale_table(dimensionless_intervals, c("Start_u", "End_u", "ISI_u"))
scaled_events <- scale_table(dimensionless_events, c("Start_u", "End_u", "Duration_u", "Core_Median_ISI_u"))
scaled_states <- scale_table(dimensionless_states, c("Start_u", "End_u", "Duration_u", "Mean_ISI_u"))
scaled_links <- scale_table(contextual_links, c("Left_Core_Median_u", "Right_Core_Median_u", "Separator_ISI_u"))

detector_input <- scaled_spikes[, c("Sample_ID", "Spike_Index", "Time_s")]
sample_order <- sample_key$Sample_ID[order(sample_key$Sample_ID)]
detector_input$Sample_ID <- factor(detector_input$Sample_ID, levels = sample_order)
detector_input <- detector_input[order(detector_input$Sample_ID, detector_input$Spike_Index), ]
detector_input$Sample_ID <- as.character(detector_input$Sample_ID)
rownames(detector_input) <- NULL

select_calibration <- function(scale_factor) {
  set.seed(parameters$calibration_selection_seed_base + scale_factor)
  e <- scaled_events[scaled_events$Scale_Factor == scale_factor & scaled_events$Split == "calibration", ]
  s <- scaled_states[scaled_states$Scale_Factor == scale_factor & scaled_states$Split == "calibration", ]
  bursts <- e[e$Event_Label == "Burst", ]
  pauses_context <- e[e$Event_Label == "Pause" & e$Pause_Subtype == "contextual_interburst_gap", ]
  pauses_canonical <- e[e$Event_Label == "Pause" & e$Pause_Subtype == "canonical", ]
  tonic <- s[s$State_Label == "Tonic", ]
  hfs <- s[s$State_Label == "Broad_HFS", ]
  pick <- function(x, n) x[sample(seq_len(nrow(x)), n), , drop = FALSE]
  burst_pick <- pick(bursts, 10)
  n_context <- min(2L, nrow(pauses_context))
  pause_pick <- rbind(
    if (n_context > 0L) pick(pauses_context, n_context) else NULL,
    pick(pauses_canonical, 10L - n_context)
  )
  tonic_pick <- pick(tonic, 10)
  hfs_pick <- pick(hfs, 10)
  event_rows <- rbind(
    data.frame(Calibration_Mode = "Burst", Episode_ID = burst_pick$Event_ID, burst_pick[, c("Sample_ID", "Template_ID", "Scale_Factor", "B_s", "Start_s", "End_s", "Duration_s")]),
    data.frame(Calibration_Mode = "Pause", Episode_ID = pause_pick$Event_ID, pause_pick[, c("Sample_ID", "Template_ID", "Scale_Factor", "B_s", "Start_s", "End_s", "Duration_s")])
  )
  state_rows <- rbind(
    data.frame(Calibration_Mode = "Tonic", Episode_ID = tonic_pick$State_Envelope_ID, tonic_pick[, c("Sample_ID", "Template_ID", "Scale_Factor", "B_s", "Start_s", "End_s", "Duration_s")]),
    data.frame(Calibration_Mode = "Broad_HFS", Episode_ID = hfs_pick$State_Envelope_ID, hfs_pick[, c("Sample_ID", "Template_ID", "Scale_Factor", "B_s", "Start_s", "End_s", "Duration_s")])
  )
  out <- rbind(event_rows, state_rows)
  out$Calibration_Row <- seq_len(nrow(out))
  out[, c("Calibration_Row", "Calibration_Mode", "Episode_ID", "Sample_ID", "Template_ID", "Scale_Factor", "B_s", "Start_s", "End_s", "Duration_s")]
}
calibration_selection <- do.call(rbind, lapply(parameters$scale_factors, select_calibration))
rownames(calibration_selection) <- NULL

write.csv(detector_input, file.path(output_dir, "detector_inputs", "spike_timestamps_blinded.csv"), row.names = FALSE)
write.csv(scaled_intervals, file.path(output_dir, "ground_truth", "interval_truth_dual_track.csv"), row.names = FALSE, na = "")
write.csv(scaled_events, file.path(output_dir, "ground_truth", "event_episodes.csv"), row.names = FALSE, na = "")
write.csv(scaled_states, file.path(output_dir, "ground_truth", "state_envelopes.csv"), row.names = FALSE, na = "")
write.csv(scaled_links, file.path(output_dir, "ground_truth", "contextual_separator_links.csv"), row.names = FALSE, na = "")
write.csv(sample_key, file.path(output_dir, "ground_truth", "sample_template_scale_key.csv"), row.names = FALSE)
write.csv(calibration_selection, file.path(output_dir, "calibration", "calibration_10_episodes_per_mode_per_scale.csv"), row.names = FALSE)
write.csv(dimensionless_spikes, file.path(output_dir, "tables", "dimensionless_spike_templates.csv"), row.names = FALSE)
write.csv(dimensionless_intervals, file.path(output_dir, "tables", "dimensionless_interval_templates.csv"), row.names = FALSE)
write.csv(dimensionless_events, file.path(output_dir, "tables", "dimensionless_event_episodes.csv"), row.names = FALSE, na = "")
write.csv(dimensionless_states, file.path(output_dir, "tables", "dimensionless_state_envelopes.csv"), row.names = FALSE, na = "")
write.csv(quota_manifest, file.path(output_dir, "metadata", "template_quota_and_order_manifest.csv"), row.names = FALSE)

scale_summary <- do.call(rbind, lapply(parameters$scale_factors, function(scale_factor) {
  scale_key <- sample_key[sample_key$Scale_Factor == scale_factor, ]
  spike_counts <- table(detector_input$Sample_ID[detector_input$Sample_ID %in% scale_key$Sample_ID])
  data.frame(
    Scale_Factor = scale_factor,
    B_s = parameters$anchor_B_s * scale_factor,
    N_Samples = nrow(scale_key),
    Total_Spikes = sum(spike_counts),
    Min_Spikes_Per_Sample = min(spike_counts),
    Median_Spikes_Per_Sample = stats::median(as.numeric(spike_counts)),
    Max_Spikes_Per_Sample = max(spike_counts),
    Min_Duration_s = min(scale_key$Recording_Duration_s),
    Median_Duration_s = stats::median(scale_key$Recording_Duration_s),
    Max_Duration_s = max(scale_key$Recording_Duration_s),
    stringsAsFactors = FALSE
  )
}))
episode_summary <- data.frame(
  Truth_Component = c("Burst", "Pause:canonical", "Pause:contextual_interburst_gap", "Tonic", "Broad_HFS", "Burst_in_HFS"),
  Dimensionless_Episode_Count = c(
    sum(dimensionless_events$Event_Label == "Burst"),
    sum(dimensionless_events$Pause_Subtype == "canonical"),
    sum(dimensionless_events$Pause_Subtype == "contextual_interburst_gap"),
    sum(dimensionless_states$State_Label == "Tonic"),
    sum(dimensionless_states$State_Label == "Broad_HFS"),
    sum(dimensionless_events$Event_Context == "burst_in_hfs")
  ),
  Scaled_Row_Count = c(
    sum(scaled_events$Event_Label == "Burst"),
    sum(scaled_events$Pause_Subtype == "canonical"),
    sum(scaled_events$Pause_Subtype == "contextual_interburst_gap"),
    sum(scaled_states$State_Label == "Tonic"),
    sum(scaled_states$State_Label == "Broad_HFS"),
    sum(scaled_events$Event_Context == "burst_in_hfs")
  ),
  stringsAsFactors = FALSE
)
write.csv(scale_summary, file.path(output_dir, "metadata", "scale_summary.csv"), row.names = FALSE)
write.csv(episode_summary, file.path(output_dir, "metadata", "episode_summary.csv"), row.names = FALSE)
jsonlite::write_json(parameters, file.path(output_dir, "metadata", "generation_parameters.json"), auto_unbox = TRUE, pretty = TRUE, digits = 15)
yaml::write_yaml(parameters, file.path(output_dir, "metadata", "generation_parameters.yaml"))

workbook <- openxlsx::createWorkbook(creator = "R clean mechanism benchmark generator")
header_style <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#1F4E78", textDecoration = "bold", halign = "center")
time_style <- openxlsx::createStyle(numFmt = "0.000000000000000")
for (batch_name in names(batch_scale)) {
  batch_ids <- sample_key$Sample_ID[sample_key$Workbook_Batch == batch_name]
  batch_data <- detector_input[detector_input$Sample_ID %in% batch_ids, , drop = FALSE]
  openxlsx::addWorksheet(workbook, batch_name, gridLines = TRUE)
  openxlsx::writeDataTable(workbook, batch_name, batch_data, tableStyle = "TableStyleMedium2", withFilter = TRUE)
  openxlsx::addStyle(workbook, batch_name, header_style, rows = 1, cols = 1:3, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(workbook, batch_name, time_style, rows = 2:(nrow(batch_data) + 1L), cols = 3, gridExpand = TRUE)
  openxlsx::freezePane(workbook, batch_name, firstRow = TRUE)
  openxlsx::setColWidths(workbook, batch_name, 1:3, c(14, 14, 23))
}
openxlsx::saveWorkbook(workbook, file.path(output_dir, "detector_inputs", "spike_timestamps_blinded.xlsx"), overwrite = TRUE)

plot_intervals <- scaled_intervals
plot_intervals$State_Rail <- ifelse(
  plot_intervals$State_Label == "Broad_HFS", "State: Broad HFS",
  paste0("State: ", plot_intervals$State_Label)
)
plot_intervals$Event_Rail <- ifelse(
  plot_intervals$Event_Label == "Pause" & plot_intervals$Pause_Subtype == "canonical", "Event: Pause (canonical)",
  ifelse(
    plot_intervals$Event_Label == "Pause" & plot_intervals$Pause_Subtype == "contextual_interburst_gap",
    "Event: Pause (contextual)",
    paste0("Event: ", plot_intervals$Event_Label)
  )
)
rail_colors <- c(
  "State: none" = "#D9D9D9",
  "State: Tonic" = "#009E73",
  "State: Broad HFS" = "#CC6677",
  "Event: none" = "#D9D9D9",
  "Event: Burst" = "#E69F00",
  "Event: Pause (canonical)" = "#0072B2",
  "Event: Pause (contextual)" = "#56B4E9"
)
rail_legend_breaks <- c(
  "State: Tonic", "State: Broad HFS", "Event: Burst",
  "Event: Pause (canonical)", "Event: Pause (contextual)"
)

paired_templates <- sprintf("TPL_%03d", 1:6)
plot_subset <- plot_intervals[plot_intervals$Template_ID %in% paired_templates, ]
paired_y <- stats::setNames(rev(seq_along(paired_templates)), paired_templates)
plot_subset$Y <- unname(paired_y[plot_subset$Template_ID])
paired_spikes <- scaled_spikes[scaled_spikes$Template_ID %in% paired_templates, ]
paired_spikes$Y <- unname(paired_y[paired_spikes$Template_ID])

paired_plot <- ggplot2::ggplot(plot_subset) +
  ggplot2::geom_segment(
    ggplot2::aes(x = Start_s, xend = End_s, y = Y + 0.11, yend = Y + 0.11, color = State_Rail),
    linewidth = 2.0, lineend = "butt"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = Start_s, xend = End_s, y = Y - 0.11, yend = Y - 0.11, color = Event_Rail),
    linewidth = 2.0, lineend = "butt"
  ) +
  ggplot2::geom_segment(
    data = paired_spikes,
    ggplot2::aes(x = Time_s, xend = Time_s, y = Y - 0.31, yend = Y + 0.31),
    inherit.aes = FALSE, color = "#111111", linewidth = 0.22
  ) +
  ggplot2::scale_color_manual(values = rail_colors, breaks = rail_legend_breaks, na.translate = FALSE) +
  ggplot2::scale_y_continuous(breaks = unname(paired_y), labels = names(paired_y), expand = ggplot2::expansion(add = 0.55)) +
  ggplot2::facet_wrap(~Scale_Factor, scales = "free_x", ncol = 1, labeller = ggplot2::label_both) +
  ggplot2::labs(
    x = "Time (s)", y = NULL, color = "Truth label",
    title = "Paired spike-train templates across exact 1/4/10 time scales",
    subtitle = "Black ticks: spikes | upper rail: State | lower rail: Event | grey: none"
  ) +
  ggplot2::theme_classic(base_size = 10) +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
    legend.position = "top", legend.box = "vertical", panel.spacing.y = grid::unit(0.7, "lines"),
    plot.title.position = "plot"
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE, override.aes = list(linewidth = 2.5)))
ggplot2::ggsave(file.path(output_dir, "figures", "paired_scale_dual_track_overview.png"), paired_plot, width = 15, height = 13, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(output_dir, "figures", "paired_scale_dual_track_overview.pdf"), paired_plot, width = 15, height = 13, device = grDevices::pdf)

anchor <- plot_intervals[plot_intervals$Scale_Factor == 1, ]
anchor_templates <- sprintf("TPL_%03d", 1:20)
anchor_y <- stats::setNames(rev(seq_along(anchor_templates)), anchor_templates)
anchor$Y <- unname(anchor_y[anchor$Template_ID])
anchor_spikes <- scaled_spikes[scaled_spikes$Scale_Factor == 1, ]
anchor_spikes$Y <- unname(anchor_y[anchor_spikes$Template_ID])
anchor_plot <- ggplot2::ggplot(anchor) +
  ggplot2::geom_segment(
    ggplot2::aes(x = Start_u, xend = End_u, y = Y + 0.11, yend = Y + 0.11, color = State_Rail),
    linewidth = 2.0, lineend = "butt"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = Start_u, xend = End_u, y = Y - 0.11, yend = Y - 0.11, color = Event_Rail),
    linewidth = 2.0, lineend = "butt"
  ) +
  ggplot2::geom_segment(
    data = anchor_spikes,
    ggplot2::aes(x = Time_u, xend = Time_u, y = Y - 0.31, yend = Y + 0.31),
    inherit.aes = FALSE, color = "#111111", linewidth = 0.20
  ) +
  ggplot2::scale_color_manual(values = rail_colors, breaks = rail_legend_breaks, na.translate = FALSE) +
  ggplot2::scale_y_continuous(breaks = unname(anchor_y), labels = names(anchor_y), expand = ggplot2::expansion(add = 0.55)) +
  ggplot2::labs(
    x = "Dimensionless time (B units)", y = NULL, color = "Truth label",
    title = "Twenty frozen dimensionless spike-train templates",
    subtitle = "Black ticks: spikes | upper rail: State | lower rail: Event | grey: none"
  ) +
  ggplot2::theme_classic(base_size = 10) +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
    legend.position = "top", legend.box = "vertical", plot.title.position = "plot"
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE, override.aes = list(linewidth = 2.5)))
ggplot2::ggsave(file.path(output_dir, "figures", "all_dimensionless_templates_dual_track.png"), anchor_plot, width = 16, height = 12, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(output_dir, "figures", "all_dimensionless_templates_dual_track.pdf"), anchor_plot, width = 16, height = 12, device = grDevices::pdf)

all_scaled_intervals <- plot_intervals
all_scaled_intervals$Y <- unname(anchor_y[all_scaled_intervals$Template_ID])
all_scaled_spikes <- scaled_spikes
all_scaled_spikes$Y <- unname(anchor_y[all_scaled_spikes$Template_ID])
all_scaled_plot <- ggplot2::ggplot(all_scaled_intervals) +
  ggplot2::geom_segment(
    ggplot2::aes(x = Start_s, xend = End_s, y = Y + 0.11, yend = Y + 0.11, color = State_Rail),
    linewidth = 1.55, lineend = "butt"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = Start_s, xend = End_s, y = Y - 0.11, yend = Y - 0.11, color = Event_Rail),
    linewidth = 1.55, lineend = "butt"
  ) +
  ggplot2::geom_segment(
    data = all_scaled_spikes,
    ggplot2::aes(x = Time_s, xend = Time_s, y = Y - 0.31, yend = Y + 0.31),
    inherit.aes = FALSE, color = "#111111", linewidth = 0.15
  ) +
  ggplot2::scale_color_manual(values = rail_colors, breaks = rail_legend_breaks, na.translate = FALSE) +
  ggplot2::scale_y_continuous(breaks = unname(anchor_y), labels = names(anchor_y), expand = ggplot2::expansion(add = 0.55)) +
  ggplot2::facet_wrap(~Scale_Factor, scales = "free_x", ncol = 1, labeller = ggplot2::label_both) +
  ggplot2::labs(
    x = "Time (s)", y = NULL, color = "Truth label",
    title = "All 60 paired spike trains across exact 1/4/10 time scales",
    subtitle = "Twenty paired templates per scale | black ticks: spikes | upper rail: State | lower rail: Event | grey: none"
  ) +
  ggplot2::theme_classic(base_size = 9) +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
    legend.position = "top", legend.box = "vertical", panel.spacing.y = grid::unit(0.8, "lines"),
    plot.title.position = "plot"
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE, override.aes = list(linewidth = 2.5)))
ggplot2::ggsave(file.path(output_dir, "figures", "all_60_scaled_spike_trains_dual_track.png"), all_scaled_plot, width = 16, height = 25, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(output_dir, "figures", "all_60_scaled_spike_trains_dual_track.pdf"), all_scaled_plot, width = 16, height = 25, device = grDevices::pdf)

canonical_files <- c(
  "detector_inputs/spike_timestamps_blinded.csv",
  "ground_truth/interval_truth_dual_track.csv",
  "ground_truth/event_episodes.csv",
  "ground_truth/state_envelopes.csv",
  "ground_truth/contextual_separator_links.csv",
  "ground_truth/sample_template_scale_key.csv",
  "calibration/calibration_10_episodes_per_mode_per_scale.csv",
  "tables/dimensionless_spike_templates.csv",
  "tables/dimensionless_interval_templates.csv",
  "metadata/template_quota_and_order_manifest.csv",
  "metadata/scale_summary.csv",
  "metadata/episode_summary.csv",
  "metadata/generation_parameters.json"
)
checksums <- data.frame(
  File = canonical_files,
  SHA256 = vapply(canonical_files, function(path) digest::digest(file = file.path(output_dir, path), algo = "sha256"), character(1)),
  stringsAsFactors = FALSE
)
write.csv(checksums, file.path(output_dir, "metadata", "canonical_output_checksums_sha256.csv"), row.names = FALSE)

manifest <- list(
  dataset_id = parameters$dataset_id,
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  generator_file = basename(script_path),
  generator_sha256 = digest::digest(file = script_path, algo = "sha256"),
  reference_simulator_file = file.path("reference", basename(reference_simulator)),
  reference_simulator_sha256 = digest::digest(file = reference_simulator, algo = "sha256"),
  reference_simulator_used_for_generation = FALSE,
  r_version = R.version.string,
  rng_kind = RNGkind(),
  package_versions = as.list(vapply(required_packages, function(pkg) as.character(utils::packageVersion(pkg)), character(1))),
  n_dimensionless_templates = length(templates),
  n_scaled_samples = nrow(sample_key),
  calibration_template_count = length(parameters$calibration_templates),
  holdout_template_count = length(parameters$holdout_templates),
  detector_input_columns = names(detector_input),
  stimulation_enabled = FALSE,
  observation_noise_enabled = FALSE,
  drift_enabled = FALSE,
  oracle_detector_performance_reported = FALSE
)
jsonlite::write_json(manifest, file.path(output_dir, "metadata", "reproduction_manifest.json"), auto_unbox = TRUE, pretty = TRUE, digits = 15)

cat("Generated", length(templates), "dimensionless templates and", nrow(sample_key), "paired scale copies.\n")
cat("Detector-input spikes:", nrow(detector_input), "\n")
cat("Output:", output_dir, "\n")
