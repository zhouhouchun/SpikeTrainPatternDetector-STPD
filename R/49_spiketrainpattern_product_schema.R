# ============================================================
# SpikeTrainPattern product schema layer
# ------------------------------------------------------------
# Purpose:
#   Provide a stable, version-neutral parameter namespace for product use.
#   The active detector core consumes version-neutral runtime
#   namespaces: event_core, event_grammar, and arbitration.
# ============================================================

stpd_path_get <- function(x, path, default = NULL) {
  if (is.null(x) || !nzchar(path)) return(default)
  cur <- x
  for (part in strsplit(path, "\\.", fixed = FALSE)[[1]]) {
    if (is.null(cur) || !(part %in% names(cur))) return(default)
    cur <- cur[[part]]
  }
  if (is.null(cur)) default else cur
}

stpd_path_set <- function(x, path, value) {
  parts <- strsplit(path, "\\.", fixed = FALSE)[[1]]
  if (length(parts) == 0) return(x)
  if (length(parts) == 1) { x[[parts[1]]] <- value; return(x) }
  head <- parts[1]
  rest <- paste(parts[-1], collapse = ".")
  child <- x[[head]]
  if (is.null(child) || !is.list(child)) child <- list()
  x[[head]] <- stpd_path_set(child, rest, value)
  x
}

stpd_first_nonnull <- function(...) {
  vals <- list(...)
  for (v in vals) if (!is.null(v)) return(v)
  NULL
}

stpd_normalize_threshold_source_mode <- function(x) {
  mode <- as.character(x %||% "auto")
  if (!length(mode) || is.na(mode[1]) || !nzchar(mode[1])) mode <- "auto"
  mode <- mode[1]
  if (identical(mode, "auto_priority")) "auto" else mode
}

stpd_fill_defaults <- function(x, defaults) {
  if (is.null(x) || !is.list(x)) x <- list()
  if (is.null(defaults) || !is.list(defaults)) return(x)
  for (nm in names(defaults)) {
    if (!(nm %in% names(x)) || is.null(x[[nm]])) {
      x[[nm]] <- defaults[[nm]]
    } else if (is.list(x[[nm]]) && is.list(defaults[[nm]])) {
      x[[nm]] <- stpd_fill_defaults(x[[nm]], defaults[[nm]])
    }
  }
  x
}

stpd_scrub_versioned_runtime_params <- function(params) {
  if (!is.null(params$burst) && is.list(params$burst)) {
    old_burst <- names(params$burst)[grepl(paste0("^", "v", "1[123]_"), names(params$burst), ignore.case = TRUE)]
    params$burst[old_burst] <- NULL
  }
  if (!is.null(params$detector) && is.list(params$detector)) {
    old_detector <- names(params$detector)[grepl("^use_v[[:digit:]]+[[:alpha:]]?_arbitration$", names(params$detector), ignore.case = TRUE)]
    params$detector[old_detector] <- NULL
  }
  old_top <- names(params)[grepl("^v[[:digit:]]+[[:alpha:]]?$", names(params), ignore.case = TRUE)]
  params[old_top] <- NULL
  params
}

stpd_product_schema_defaults <- function() {
  stpd_parameter_config_section("product_defaults", default = list())
}

stpd_product_alias_map <- function() {
  data.frame(
    canonical_path = c(
      "engine.enabled",
      "engine.threshold_source_mode",
      "engine.use_manual_calibration",
      "engine.manual_can_expand_seed_band",
      "engine.manual_can_expand_bridge_band",
      "engine.stop_on_qc_error",
      "engine.freeze_dataset_thresholds",
      "engine.honor_manual_lock_for_auto",
      "qc.artifact_min_valid_isi_sec",
      "qc.refractory_suspect_isi_sec",
      "qc.refractory_suspect_action",
      "burst.seed_lower_sec",
      "burst.seed_upper_sec",
      "burst.bridge_upper_sec",
      "burst.boundary_floor_sec",
      "burst.boundary_floor_hard",
      "burst.contrast_min",
      "burst.possible_contrast_min",
      "burst.min_seed_isi_count",
      "burst.max_bridge_isi_count",
      "burst.max_bridge_isi_fraction",
      "burst.max_expansion_isi_each_side",
      "burst.max_candidates_per_train",
      "burst.allow_one_sided_as_canonical",
      "burst.allow_one_sided_possible",
      "burst.one_sided_seed_purity_min",
      "burst.strict_q95_bridge_gate",
      "burst.q95_soft_penalty_weight",
      "burst.dynamic_possible_priority",
      "burst.classic_max_spikes",
      "burst.long_min_spikes",
      "burst.long_max_spikes",
      "burst.prolonged_min_spikes",
      "burst.prolonged_max_spikes",
      "high_frequency_spiking.min_spikes",
      "high_frequency_spiking.min_duration_sec",
      "high_frequency_spiking.short_isi_upper_sec",
      "high_frequency_spiking.q90_isi_max_sec",
      "high_frequency_spiking.epoch_bridge_isi_sec",
      "high_frequency_spiking.tolerated_gap_isi_sec",
      "high_frequency_spiking.allowed_large_isi_fraction",
      "high_frequency_spiking.max_consecutive_large_isi",
      "high_frequency_tonic.min_isi_floor_sec",
      "high_frequency_tonic.max_isi_sec",
      "high_frequency_tonic.bridge_upper_sec",
      "high_frequency_tonic.low_tail_fraction_max",
      "high_frequency_tonic.veto_burst_core_run",
      "tonic.min_isi_sec",
      "tonic.max_isi_sec",
      "pause.min_isi_sec",
      "pause.max_isi_sec",
      "pause.bridge_upper_sec"
    ),
    legacy_path = c(
      "event_grammar.enabled",
      "burst.event_grammar_threshold_source_mode",
      "burst.event_core_use_manual_isi_calibration",
      "burst.event_core_manual_can_expand_seed_band",
      "burst.event_core_manual_can_expand_bridge_band",
      "detector.stop_on_qc_error",
      "detector.freeze_dataset_thresholds",
      "detector.honor_manual_lock_for_auto",
      "detector.artifact_min_valid_isi_sec",
      "detector.refractory_suspect_sec",
      "detector.refractory_suspect_action",
      "burst.event_core_seed_band_lower_sec",
      "burst.event_core_seed_band_upper_sec",
      "burst.event_core_bridge_band_upper_sec",
      "burst.event_core_boundary_floor_sec",
      "burst.event_core_boundary_floor_hard",
      "burst.event_core_burst_contrast_min",
      "burst.event_core_possible_burst_contrast_min",
      "burst.event_grammar_min_seed_isi_count",
      "burst.event_grammar_max_bridge_isi_count",
      "burst.event_grammar_max_bridge_isi_fraction",
      "burst.event_grammar_max_expansion_isi_each_side",
      "burst.event_grammar_max_candidates_per_train",
      "burst.event_grammar_allow_one_sided_as_canonical",
      "burst.event_grammar_allow_one_sided_possible",
      "burst.event_grammar_one_sided_seed_purity_min",
      "burst.event_grammar_strict_q95_bridge_gate",
      "burst.event_grammar_q95_soft_penalty_weight",
      "burst.event_grammar_dynamic_possible_priority",
      "burst.classic_burst_max_spikes",
      "burst.long_burst_min_spikes",
      "burst.long_burst_max_spikes",
      "burst.event_core_prolonged_min_spikes",
      "burst.event_core_prolonged_max_spikes",
      "highfreq.spiking_min_spikes",
      "highfreq.spiking_min_duration",
      "highfreq.spiking_max_ISI_abs",
      "highfreq.spiking_q90_max_ISI_sec",
      "highfreq.spiking_epoch_bridge_ISI_sec",
      "highfreq.spiking_tolerated_gap_ISI_sec",
      "highfreq.spiking_allowed_large_isi_fraction",
      "highfreq.spiking_max_consecutive_large_isi",
      "highfreq.tonic_min_ISI_floor_sec",
      "highfreq.tonic_max_ISI_sec",
      "highfreq.tonic_bridge_upper_sec",
      "highfreq.tonic_low_tail_fraction_max",
      "highfreq.tonic_veto_burst_core_run",
      "tonic.T_min",
      "tonic.T_max",
      "pause.T_seed",
      "pause.T_strong",
      "pause.bridge_upper_sec"
    ),
    stringsAsFactors = FALSE
  )
}

stpd_product_runtime_value <- function(params, canonical_path) {
  switch(as.character(canonical_path),
    "engine.enabled" = stpd_first_nonnull(
      stpd_path_get(params, "event_grammar.enabled", NULL),
      stpd_path_get(params, "event_core.enabled", NULL)
    ),
    "engine.threshold_source_mode" = stpd_path_get(params, "event_grammar.threshold_source_mode", NULL),
    "engine.use_manual_calibration" = stpd_path_get(params, "event_core.use_manual_isi_calibration", NULL),
    "engine.manual_can_expand_seed_band" = stpd_path_get(params, "event_core.manual_can_expand_seed_band", NULL),
    "engine.manual_can_expand_bridge_band" = stpd_path_get(params, "event_core.manual_can_expand_bridge_band", NULL),
    "burst.seed_lower_sec" = stpd_path_get(params, "event_core.seed_band_lower_sec", NULL),
    "burst.seed_upper_sec" = stpd_path_get(params, "event_core.seed_band_upper_sec", NULL),
    "burst.bridge_upper_sec" = stpd_path_get(params, "event_core.bridge_band_upper_sec", NULL),
    "burst.boundary_floor_sec" = stpd_path_get(params, "event_core.boundary_floor_sec", NULL),
    "burst.boundary_floor_hard" = stpd_path_get(params, "event_core.boundary_floor_hard", NULL),
    "burst.contrast_min" = stpd_path_get(params, "event_core.burst_contrast_min", NULL),
    "burst.possible_contrast_min" = stpd_path_get(params, "event_core.possible_burst_contrast_min", NULL),
    "burst.min_seed_isi_count" = stpd_path_get(params, "event_core.min_seed_isi_count", NULL),
    "burst.max_bridge_isi_count" = stpd_path_get(params, "event_core.max_bridge_isi_count", NULL),
    "burst.max_bridge_isi_fraction" = stpd_path_get(params, "event_core.max_bridge_isi_fraction", NULL),
    "burst.max_expansion_isi_each_side" = stpd_path_get(params, "event_core.max_expansion_isi_each_side", NULL),
    "burst.max_candidates_per_train" = stpd_path_get(params, "event_core.max_candidates_per_train", NULL),
    "burst.allow_one_sided_as_canonical" = stpd_path_get(params, "event_grammar.allow_one_sided_burst_as_canonical", NULL),
    "burst.allow_one_sided_possible" = stpd_path_get(params, "event_grammar.allow_one_sided_possible", NULL),
    "burst.one_sided_seed_purity_min" = stpd_path_get(params, "event_grammar.one_sided_seed_purity_min", NULL),
    "burst.strict_q95_bridge_gate" = stpd_path_get(params, "event_grammar.strict_q95_bridge_gate", NULL),
    "burst.q95_soft_penalty_weight" = stpd_path_get(params, "event_grammar.q95_soft_penalty_weight", NULL),
    "burst.dynamic_possible_priority" = stpd_path_get(params, "event_grammar.dynamic_possible_priority", NULL),
    "burst.classic_max_spikes" = stpd_path_get(params, "event_core.classic_max_spikes", NULL),
    "burst.long_min_spikes" = stpd_path_get(params, "event_core.long_min_spikes", NULL),
    "burst.long_max_spikes" = stpd_path_get(params, "event_core.long_max_spikes", NULL),
    "burst.prolonged_min_spikes" = stpd_path_get(params, "event_core.prolonged_min_spikes", NULL),
    "burst.prolonged_max_spikes" = stpd_path_get(params, "event_core.prolonged_max_spikes", NULL),
    "qc.refractory_suspect_action" = stpd_first_nonnull(
      stpd_path_get(params, "detector.refractory_suspect_action", NULL),
      stpd_path_get(params, "burst.refractory_suspect_action", NULL)
    ),
    NULL
  )
}

stpd_productize_params <- function(params, prefer = c("canonical", "legacy")) {
  prefer <- match.arg(prefer)
  if (is.null(params)) params <- list()
  defs <- stpd_product_schema_defaults()
  sp <- params$spiketrainpattern %||% list()
  amap <- stpd_product_alias_map()

  for (i in seq_len(nrow(amap))) {
    cpath <- amap$canonical_path[i]
    lpath <- amap$legacy_path[i]
    cval <- stpd_path_get(sp, cpath, NULL)
    rval <- stpd_product_runtime_value(params, cpath)
    lval <- stpd_path_get(params, lpath, NULL)
    dval <- stpd_path_get(defs, cpath, NULL)
    val <- if (identical(prefer, "canonical")) stpd_first_nonnull(cval, rval, lval, dval) else stpd_first_nonnull(rval, lval, cval, dval)
    sp <- stpd_path_set(sp, cpath, val)
  }

  sp <- stpd_fill_defaults(sp, defs)
  sp$schema_version <- sp$schema_version %||% defs$schema_version
  params$spiketrainpattern <- sp

  params$burst <- params$burst %||% list()
  params$detector <- params$detector %||% list()
  params$highfreq <- params$highfreq %||% list()
  params$pause <- params$pause %||% list()
  params$tonic <- params$tonic %||% list()
  params$metadata <- params$metadata %||% list()

  params$event_core <- params$event_core %||% list()
  params$event_grammar <- params$event_grammar %||% list()
  params$arbitration <- params$arbitration %||% list()

  ec <- params$event_core
  eg <- params$event_grammar
  ar <- params$arbitration

  ec$enabled <- sp$engine$enabled
  ec$dataset_seed_band_enabled <- sp$engine$enabled
  ec$dataset_isi_burst_enabled <- sp$engine$enabled
  ec$use_manual_isi_calibration <- sp$engine$use_manual_calibration
  ec$manual_can_expand_seed_band <- sp$engine$manual_can_expand_seed_band
  ec$manual_can_expand_bridge_band <- sp$engine$manual_can_expand_bridge_band
  ec$manual_min_burst_isi_count <- stpd_first_nonnull(ec$manual_min_burst_isi_count, stpd_path_get(params, "burst.event_core_manual_min_burst_isi_n", NULL), 3L)
  ec$manual_can_set_hf_tonic_floor <- stpd_first_nonnull(ec$manual_can_set_hf_tonic_floor, stpd_path_get(params, "burst.event_core_manual_can_set_hf_tonic_floor", NULL), FALSE)
  ec$manual_can_expand_hf_spiking_q90 <- stpd_first_nonnull(ec$manual_can_expand_hf_spiking_q90, stpd_path_get(params, "burst.event_core_manual_can_expand_hf_spiking_q90", NULL), TRUE)
  ec$manual_can_set_tonic_band <- stpd_first_nonnull(ec$manual_can_set_tonic_band, stpd_path_get(params, "burst.event_core_manual_can_set_tonic_band", NULL), FALSE)
  ec$seed_band_lower_sec <- sp$burst$seed_lower_sec
  ec$seed_band_upper_sec <- sp$burst$seed_upper_sec
  ec$bridge_band_upper_sec <- sp$burst$bridge_upper_sec
  ec$boundary_floor_sec <- sp$burst$boundary_floor_sec
  ec$boundary_floor_hard <- sp$burst$boundary_floor_hard
  ec$burst_contrast_min <- sp$burst$contrast_min
  ec$possible_burst_contrast_min <- sp$burst$possible_contrast_min
  ec$min_seed_isi_count <- sp$burst$min_seed_isi_count
  ec$max_bridge_isi_count <- sp$burst$max_bridge_isi_count
  ec$max_bridge_isi_fraction <- sp$burst$max_bridge_isi_fraction
  ec$max_expansion_isi_each_side <- sp$burst$max_expansion_isi_each_side
  ec$max_candidates_per_train <- sp$burst$max_candidates_per_train
  ec$min_spikes <- stpd_first_nonnull(ec$min_spikes, stpd_path_get(params, "burst.G_min", NULL), 3L)
  ec$classic_max_spikes <- sp$burst$classic_max_spikes
  ec$long_min_spikes <- sp$burst$long_min_spikes
  ec$long_max_spikes <- sp$burst$long_max_spikes
  ec$prolonged_min_spikes <- sp$burst$prolonged_min_spikes
  ec$prolonged_max_spikes <- sp$burst$prolonged_max_spikes
  ec$allow_boundary_possible_burst <- stpd_first_nonnull(ec$allow_boundary_possible_burst, TRUE)
  ec$context_compression_min <- stpd_first_nonnull(ec$context_compression_min, stpd_path_get(params, "burst.dataset_isi_context_compression_min", NULL), stpd_path_get(params, "burst.seed_bridge_context_compression_min", NULL), 1.00)
  ec$edge_return_min <- stpd_first_nonnull(ec$edge_return_min, stpd_path_get(params, "burst.dataset_isi_edge_return_min", NULL), stpd_path_get(params, "burst.seed_bridge_edge_return_min", NULL), 0.00)
  ec$use_train_percentile_as_seed_gate <- stpd_first_nonnull(ec$use_train_percentile_as_seed_gate, stpd_path_get(params, "burst.dataset_isi_use_train_percentile_as_seed_gate", NULL), FALSE)
  ec$seed_percentile_gate_max <- stpd_first_nonnull(ec$seed_percentile_gate_max, stpd_path_get(params, "burst.dataset_isi_seed_percentile_gate_max", NULL), 0)
  ec$histogram_bin_width_sec <- stpd_first_nonnull(ec$histogram_bin_width_sec, stpd_path_get(params, "burst.dataset_isi_histogram_bin_width_sec", NULL), 0.005)

  eg$enabled <- sp$engine$enabled
  eg$threshold_source_mode <- stpd_normalize_threshold_source_mode(sp$engine$threshold_source_mode)
  eg$histogram_bin_width_sec <- stpd_first_nonnull(eg$histogram_bin_width_sec, ec$histogram_bin_width_sec, 0.005)
  eg$user <- eg$user %||% list()
  eg$allow_one_sided_burst_as_canonical <- sp$burst$allow_one_sided_as_canonical
  eg$allow_one_sided_possible <- sp$burst$allow_one_sided_possible
  eg$one_sided_seed_purity_min <- sp$burst$one_sided_seed_purity_min
  eg$strict_q95_bridge_gate <- sp$burst$strict_q95_bridge_gate
  eg$q95_soft_penalty_weight <- sp$burst$q95_soft_penalty_weight
  eg$dynamic_possible_priority <- sp$burst$dynamic_possible_priority
  eg$q95_soft_severe_ratio <- stpd_first_nonnull(eg$q95_soft_severe_ratio, 1.35)
  eg$one_sided_burst_contrast_min <- stpd_first_nonnull(eg$one_sided_burst_contrast_min, NULL)
  eg$one_sided_bridge_fraction_max <- stpd_first_nonnull(eg$one_sided_bridge_fraction_max, NULL)

  ar$enabled <- stpd_first_nonnull(ar$enabled, sp$engine$enabled)

  params$event_core <- ec
  params$event_grammar <- eg
  params$arbitration <- ar

  params$detector$min_valid_isi_sec <- sp$qc$artifact_min_valid_isi_sec
  params$detector$artifact_min_valid_isi_sec <- sp$qc$artifact_min_valid_isi_sec
  params$burst$refractory_suspect_sec <- sp$qc$refractory_suspect_isi_sec
  refractory_action <- sp$qc$refractory_suspect_action %||% params$detector$refractory_suspect_action %||% params$burst$refractory_suspect_action %||% "demote_to_possible"
  params$detector$refractory_suspect_action <- refractory_action
  params$burst$refractory_suspect_action <- refractory_action

  # Non-versioned detector families still keep their historical namespaces until
  # their own UI/schema migration is complete.
  params$pause$T_seed <- sp$pause$min_isi_sec
  params$pause$T_strong <- sp$pause$max_isi_sec
  params$pause$T_pause <- sp$pause$min_isi_sec
  params$pause$bridge_upper_sec <- sp$pause$bridge_upper_sec
  params$highfreq$spiking_max_ISI_abs <- sp$high_frequency_spiking$short_isi_upper_sec
  params$highfreq$spiking_short_upper_sec <- sp$high_frequency_spiking$short_isi_upper_sec
  params$highfreq$spiking_q90_max_ISI_sec <- sp$high_frequency_spiking$q90_isi_max_sec
  params$highfreq$spiking_epoch_bridge_ISI_sec <- sp$high_frequency_spiking$epoch_bridge_isi_sec
  params$highfreq$T_high_max <- sp$high_frequency_tonic$max_isi_sec
  params$highfreq$ISI_abs_max <- sp$high_frequency_tonic$max_isi_sec
  params$highfreq$tonic_max_ISI_sec <- sp$high_frequency_tonic$max_isi_sec
  params$highfreq$tonic_bridge_upper_sec <- sp$high_frequency_tonic$bridge_upper_sec
  params$tonic$burst_overlap_guard <- stpd_first_nonnull(sp$tonic$burst_overlap_guard, params$tonic$burst_overlap_guard, TRUE)
  params$tonic$burst_overlap_guard_factor <- stpd_first_nonnull(sp$tonic$burst_overlap_guard_factor, params$tonic$burst_overlap_guard_factor, 1.15)
  params$tonic$burst_overlap_lower_quantile <- stpd_first_nonnull(sp$tonic$burst_overlap_lower_quantile, params$tonic$burst_overlap_lower_quantile, 0.10)
  params$tonic$burst_overlap_low_fraction_max <- stpd_first_nonnull(sp$tonic$burst_overlap_low_fraction_max, params$tonic$burst_overlap_low_fraction_max, 0.05)
  params$tonic$burst_overlap_reference_quantile <- stpd_first_nonnull(sp$tonic$burst_overlap_reference_quantile, params$tonic$burst_overlap_reference_quantile, 0.95)
  params$detector$stop_on_qc_error <- sp$engine$stop_on_qc_error
  params$detector$freeze_dataset_thresholds <- sp$engine$freeze_dataset_thresholds
  params$detector$honor_manual_lock_for_auto <- sp$engine$honor_manual_lock_for_auto

  params$metadata$parameter_schema <- "spiketrainpattern"
  params$metadata$deprecated_parameter_aliases <- amap
  params <- stpd_scrub_versioned_runtime_params(params)
  params
}

stpd_public_parameter_table <- function(params = NULL) {
  params <- stpd_productize_params(params %||% default_params_sec(), prefer = "canonical")
  sp <- params$spiketrainpattern
  rows <- list()
  add <- function(section, name, value, unit = "") {
    rows[[length(rows) + 1L]] <<- data.frame(section = section, parameter = name, value = as.character(value), unit = unit, stringsAsFactors = FALSE)
  }
  add("QC", "artifact_min_valid_isi", sp$qc$artifact_min_valid_isi_sec, "sec")
  add("QC", "refractory_suspect_isi", sp$qc$refractory_suspect_isi_sec, "sec")
  add("QC", "refractory_suspect_action", sp$qc$refractory_suspect_action)
  add("Engine", "stop_on_qc_error", sp$engine$stop_on_qc_error)
  add("Engine", "freeze_dataset_thresholds", sp$engine$freeze_dataset_thresholds)
  add("Engine", "honor_manual_lock_for_auto", sp$engine$honor_manual_lock_for_auto)
  add("Burst", "seed_lower", sp$burst$seed_lower_sec, "sec")
  add("Burst", "seed_upper", sp$burst$seed_upper_sec, "sec")
  add("Burst", "bridge_upper", sp$burst$bridge_upper_sec, "sec")
  add("Burst", "contrast_min", sp$burst$contrast_min, "ratio")
  add("Burst", "possible_contrast_min", sp$burst$possible_contrast_min, "ratio")
  add("HF spiking", "min_spikes", sp$high_frequency_spiking$min_spikes, "spikes")
  add("HF spiking", "short_isi_upper", sp$high_frequency_spiking$short_isi_upper_sec, "sec")
  add("HF spiking", "q90_isi_max", sp$high_frequency_spiking$q90_isi_max_sec, "sec")
  add("HF spiking", "tolerated_gap_isi", sp$high_frequency_spiking$tolerated_gap_isi_sec, "sec")
  add("HF tonic", "min_isi_floor", sp$high_frequency_tonic$min_isi_floor_sec, "sec")
  add("HF tonic", "max_isi", sp$high_frequency_tonic$max_isi_sec, "sec")
  add("Tonic", "min_isi", sp$tonic$min_isi_sec, "sec")
  add("Tonic", "max_isi", sp$tonic$max_isi_sec, "sec")
  add("Tonic", "burst_overlap_guard", sp$tonic$burst_overlap_guard)
  add("Tonic", "burst_overlap_guard_factor", sp$tonic$burst_overlap_guard_factor, "ratio")
  add("Pause", "min_isi", sp$pause$min_isi_sec, "sec")
  add("Pause", "max_isi", sp$pause$max_isi_sec, "sec")
  do.call(rbind, rows)
}

stpd_parameter_deprecation_notes <- function() {
  data.frame(
    policy = c(
      "Public configuration should use params$spiketrainpattern.",
      "Internal runtime namespaces mirror public configuration into event_core, event_grammar, and arbitration.",
      "The detector resolves params$spiketrainpattern first, then mirrors values into internal aliases before running.",
      "UI labels and exported parameter reports use function-based names such as burst.seed_upper_sec and high_frequency_spiking.min_spikes."
    ),
    stringsAsFactors = FALSE
  )
}

# Final parameter constructors materialize YAML defaults first, then mirror the
# public product namespace into the detector runtime fields.
default_params_sec <- function() {
  stpd_productize_params(stpd_runtime_default_params(), prefer = "legacy")
}

effective_params_for_detector <- function(params) {
  stpd_productize_params(effective_params_for_detector_core(params), prefer = "canonical")
}

# Version-neutral policy notes supersede older text while retaining the function name.
stpd_csv_input_policy_notes <- function() {
  data.frame(
    topic = c("raw_spike_csv", "derived_csv_hard_block", "override", "rejection_reason", "audit_separation", "duplicate_timestamps", "parameter_namespace"),
    policy = c(
      "Raw CSV files should contain spike timestamp columns. Each numeric column is interpreted as one spike train after NA removal and sorting.",
      "High-confidence derived outputs such as Sliding_*, ISI_base, tonic_summary, threshold/threshould, Candidate_ledger, Eventness_audit, Events_final, diagnostic candidate audits, summary/result/features/audit files are blocked by default.",
      "Use allow_derived_csv=TRUE only when intentionally bypassing the derived-table guard. This is not recommended for routine batch processing.",
      "rejection_reason is reserved for diagnostic/rejected rows. Public selected candidates should leave it blank; neutral strings such as 'no rejection' and 'not applicable' are accepted.",
      "Diagnostic candidate audit keeps all rejected/profile windows. Candidate_ledger, candidate_features, Eventness_audit and Final_classification contain only selected public candidates.",
      "Exact duplicate timestamps should be reported. For formal statistics, consider duplicate_policy='collapse_exact' or report duplicate impact on ISI, burst/pause and LV/CV/MM metrics.",
      "Public parameter configuration should use params$spiketrainpattern; internal runtime fields are derived during detector setup."
    ),
    stringsAsFactors = FALSE
  )
}

# Version-neutral public wrappers.  Older suffixed functions remain available as
# deprecated compatibility aliases, but new code should prefer these names.
stpd_run_detector <- function(ds, params = default_params_sec(), selected_trains = NULL,
                              lock_manual = TRUE, collect_diagnostics = TRUE,
                              strict_params = FALSE, progress_callback = NULL) {
  stpd_detect(ds, params = params, selected_trains = selected_trains,
              lock_manual = lock_manual, collect_diagnostics = collect_diagnostics,
              strict_params = strict_params, progress_callback = progress_callback)
}

stpd_detect_patterns <- stpd_run_detector

stpd_generate_candidate_events <- function(ds, params = default_params_sec(), selected_trains = NULL) {
  stpd_generate_candidates(ds, params = params, selected_trains = selected_trains)
}

stpd_compute_candidate_features <- function(ds, candidates = NULL, params = default_params_sec(), selected_trains = NULL) {
  stpd_compute_features(ds, candidates = candidates, params = params, selected_trains = selected_trains)
}

stpd_parameter_report <- function(params = default_params_sec(), baseline = default_params_sec(), preset = NULL) {
  fallback <- stpd_parameter_report_flat(params, baseline = baseline, preset = preset)
  product <- tryCatch(stpd_public_parameter_table(params), error = function(e) data.frame())
  if (nrow(product) > 0) product else fallback
}

stpd_params_hash <- function(params) {
  digest::digest(stpd_public_parameter_table(stpd_productize_params(params)), algo = "sha256")
}

stpd_method_readme <- function() {
  paste(
    "SpikeTrainPatternDetector uses a productized event-grammar detector.",
    "Public configuration lives in params$spiketrainpattern.",
    "Burst events are seed-centered short-ISI packets with flank contrast.",
    "High-frequency spiking is a long high-frequency state, not a long burst.",
    "Diagnostic candidate windows are kept separate from public final results.",
    sep = "\n"
  )
}

if (exists("stpd_engine_prepare_params", mode = "function") && !exists("stpd_engine_prepare_params_unproductized", mode = "function")) {
  stpd_engine_prepare_params_unproductized <- stpd_engine_prepare_params
  stpd_engine_prepare_params <- function(params = default_params_sec(), strict = FALSE) {
    pp <- stpd_productize_params(params, prefer = "canonical")
    out <- stpd_engine_prepare_params_unproductized(pp, strict = strict)
    out <- stpd_productize_params(out, prefer = "canonical")
    out$meta <- out$meta %||% list()
    out$meta$params_hash <- stpd_params_hash(out)
    out
  }
}

if (exists("stpd_export_results", mode = "function") && !exists("stpd_export_results_core_exporter", mode = "function")) {
  stpd_export_results_core_exporter <- stpd_export_results
  stpd_export_results <- function(ds, params = default_params_sec(), out_dir, dataset_name = "dataset", time_unit = "ms") {
    if (exists("stpd_canonicalize_result_names", mode = "function")) ds <- stpd_canonicalize_result_names(ds)
    stpd_export_results_core_exporter(ds, params = params, out_dir = out_dir, dataset_name = dataset_name, time_unit = time_unit)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    if (!is.null(ds$results$parameter_report)) write_csv_safe(ds$results$parameter_report, file.path(out_dir, "Parameters_report.csv"))
    if (!is.null(ds$results$run_metadata_public)) write_csv_safe(ds$results$run_metadata_public, file.path(out_dir, "Detector_run_metadata.csv"))
    if (!is.null(ds$results$result_consistency)) write_csv_safe(ds$results$result_consistency, file.path(out_dir, "Result_consistency_check.csv"))
    if (!is.null(ds$results$scientific_validation_summary)) write_csv_safe(ds$results$scientific_validation_summary, file.path(out_dir, "Scientific_validation_summary.csv"))
    if (!is.null(ds$results$event_level_validation_strict)) write_csv_safe(ds$results$event_level_validation_strict, file.path(out_dir, "Event_level_validation_strict.csv"))
    if (!is.null(ds$results$event_level_validation_candidate_family)) write_csv_safe(ds$results$event_level_validation_candidate_family, file.path(out_dir, "Event_level_validation_candidate_family.csv"))
    if (!is.null(ds$results$candidate_diagnostic_audit) && nrow(ds$results$candidate_diagnostic_audit) > 0) write_csv_safe(ds$results$candidate_diagnostic_audit, file.path(out_dir, "Candidate_diagnostic_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    writeLines(stpd_method_readme(), con = file.path(out_dir, "README_results.txt"), useBytes = TRUE)
    invisible(out_dir)
  }
}

stpd_product_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  if (length(y) == 0 || !is.finite(y[1])) return(default)
  y[1]
}

stpd_event_grammar_clear_threshold_resolution <- function(params) {
  if (is.null(params$event_grammar) || !is.list(params$event_grammar)) return(params)
  params$event_grammar$threshold_table <- NULL
  params$event_grammar$effective_bands <- NULL
  params$event_grammar$manual_event_table <- NULL
  params$event_grammar$manual_suggest <- NULL
  params$event_grammar$histogram_suggest <- NULL
  params
}

stpd_product_target_trains <- function(ds, selected_trains = NULL) {
  if (is.null(ds) || is.null(ds$trains)) return(character())
  target <- selected_trains %||% names(ds$trains)
  intersect(target, names(ds$trains))
}

stpd_product_pre_detection_qc <- function(ds, params, target_trains) {
  if (length(target_trains) == 0) return(data.frame())
  validate_dataset_quality_impl(
    ds$trains[target_trains],
    min_isi_sec = params$detector$min_valid_isi_sec %||% 0.0009,
    unit_hint = ds$meta$unit_in %||% "s",
    refractory_suspect_sec = params$detector$refractory_suspect_sec %||% 0.0010
  )
}

stpd_product_qc_error_message <- function(qc) {
  if (is.null(qc) || nrow(qc) == 0 || !("warning_level" %in% names(qc))) return("")
  bad <- qc[as.character(qc$warning_level) == "error", , drop = FALSE]
  if (nrow(bad) == 0) return("")
  detail <- paste(
    utils::head(paste0(bad$train, ": ", bad$warning_message), 5L),
    collapse = " | "
  )
  paste0("Pre-detection QC found ", nrow(bad), " train(s) with data-integrity errors. ", detail)
}

stpd_qc_errors_are_exact_duplicate_only <- function(qc) {
  if (is.null(qc) || nrow(qc) == 0 || !("warning_level" %in% names(qc))) return(FALSE)
  bad <- qc[as.character(qc$warning_level) == "error", , drop = FALSE]
  if (nrow(bad) == 0) return(FALSE)
  get_num <- function(nm, default = 0) {
    if (nm %in% names(bad)) suppressWarnings(as.numeric(bad[[nm]])) else rep(default, nrow(bad))
  }
  get_chr <- function(nm, default = "") {
    if (nm %in% names(bad)) as.character(bad[[nm]]) else rep(default, nrow(bad))
  }
  dup_n <- get_num("n_duplicate_timestamps", 0)
  nonpos_n <- get_num("n_zero_or_negative_ISI", 0)
  art_n <- get_num("n_artifact_ISI", 0)
  msg <- get_chr("warning_message", "")
  has_dup <- is.finite(dup_n) & dup_n > 0
  nonpos_ok <- !is.finite(nonpos_n) | nonpos_n <= dup_n
  artifact_ok <- !is.finite(art_n) | art_n <= dup_n
  no_other_hard_error <- !grepl("invalid_duration|timestamp_ISI_mismatch", msg)
  all(has_dup & nonpos_ok & artifact_ok & no_other_hard_error, na.rm = TRUE)
}

stpd_product_attach_dataset_thresholds <- function(params, ds, target_trains) {
  if (!isTRUE((params$event_grammar %||% list())$enabled %||% TRUE)) return(params)
  if (!exists("stpd_attach_thresholds_to_params_impl", mode = "function")) return(params)
  params <- stpd_event_grammar_clear_threshold_resolution(params)
  min_isi <- params$detector$min_valid_isi_sec %||% 0.0009
  bin_width <- params$event_grammar$histogram_bin_width_sec %||% params$event_core$histogram_bin_width_sec %||% 0.005
  scoped_ds <- ds
  scoped_ds$trains <- scoped_ds$trains[target_trains]
  params <- stpd_attach_thresholds_to_params_impl(params, ds = scoped_ds, min_isi_sec = min_isi, bin_width_sec = bin_width)
  params$event_grammar$threshold_resolution_scope <- "dataset_pre_detection"
  params
}

stpd_product_apply_manual_lock_to_audit <- function(audit, locked) {
  if (is.null(audit) || nrow(audit) == 0 || !any(locked, na.rm = TRUE)) return(audit)
  starts <- suppressWarnings(as.integer(audit$start_isi))
  ends <- suppressWarnings(as.integer(audit$end_isi))
  locked_idx <- which(locked)
  blocked <- vapply(seq_len(nrow(audit)), function(i) {
    is.finite(starts[i]) && is.finite(ends[i]) && any(locked_idx >= starts[i] & locked_idx <= ends[i])
  }, logical(1))
  lab <- as.character(audit$final_label %||% audit$class %||% "")
  lab[is.na(lab)] <- ""
  blocked <- blocked & !(lab %in% c("profile", "reject", "unlabeled", "not_selected"))
  if (!any(blocked, na.rm = TRUE)) return(audit)

  if (!("manual_lock_overlap" %in% names(audit))) audit$manual_lock_overlap <- rep(FALSE, nrow(audit))
  audit$manual_lock_overlap <- blocked
  if (!("auto_write_blocked_by_manual_lock" %in% names(audit))) audit$auto_write_blocked_by_manual_lock <- rep(FALSE, nrow(audit))
  audit$auto_write_blocked_by_manual_lock[blocked] <- TRUE
  audit$manual_lock_policy <- ifelse(blocked, "trim_auto_on_manual_label", "")

  if ("selected_for_auto" %in% names(audit) && !("selected_for_auto_before_manual_lock" %in% names(audit))) {
    audit$selected_for_auto_before_manual_lock <- audit$selected_for_auto
  }
  if ("selected_for_auto" %in% names(audit)) {
    audit$selected_for_auto[blocked] <- FALSE
  }
  if (!("written_to_auto" %in% names(audit))) audit$written_to_auto <- rep(NA, nrow(audit))
  audit$written_to_auto[blocked] <- FALSE
  if ("auto_written" %in% names(audit)) audit$auto_written[blocked] <- FALSE
  if ("selection_status" %in% names(audit) && !("selection_status_before_manual_lock" %in% names(audit))) {
    audit$selection_status_before_manual_lock <- audit$selection_status
  }
  if (!("selection_status" %in% names(audit))) audit$selection_status <- rep("", nrow(audit))
  audit$selection_status[blocked] <- "blocked_by_manual_label"

  if ("decision_path" %in% names(audit)) {
    old <- as.character(audit$decision_path[blocked])
    old[is.na(old)] <- ""
    audit$decision_path[blocked] <- ifelse(nzchar(old), paste0(old, ";manual_lock_trimmed"), "manual_lock_trimmed")
  }
  audit
}

if (exists("stpd_event_core_params_impl", mode = "function") && !exists("stpd_event_core_params_impl_base", mode = "function")) {
  stpd_event_core_params_impl_base <- stpd_event_core_params_impl
  stpd_event_core_params_impl <- function(dat, params, min_isi_sec = 0.001) {
    pp <- stpd_productize_params(params, prefer = "canonical")
    out <- stpd_event_core_params_impl_base(dat, pp, min_isi_sec = min_isi_sec)
    hp <- pp$highfreq %||% list()
    short_upper <- stpd_product_num(hp$spiking_max_ISI_abs %||% hp$spiking_short_upper_sec, out$hf_spiking_q90_max %||% 0.020)
    q80_max <- stpd_product_num(hp$spiking_q80_max_ISI_sec, max(short_upper, out$hf_spiking_q90_max %||% short_upper, na.rm = TRUE))
    out$hf_spiking_short_upper <- short_upper
    out$hf_spiking_q80_max <- q80_max
    out
  }
}

stpd_detect_train_product_hardened <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  pp <- effective_params_for_detector(params)
  out <- stpd_detect_train_hf_protected_impl(dat, pp, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
  honor <- isTRUE((pp$detector %||% list())$honor_manual_lock_for_auto %||% TRUE)
  if (!isTRUE(lock_manual) || !honor || is.null(out) || nrow(out) == 0 || !("pattern_manual" %in% names(out)) || !("pattern_auto" %in% names(out))) return(out)

  manual <- as.character(out$pattern_manual)
  manual[is.na(manual)] <- ""
  locked <- nzchar(manual)
  if (!any(locked, na.rm = TRUE)) return(out)

  out$pattern_auto[locked] <- ""
  if ("auto_score" %in% names(out)) out$auto_score[locked] <- NA_real_
  audit <- attr(out, "candidate_diagnostic_audit")
  if (!is.null(audit) && nrow(audit) > 0) {
    attr(out, "candidate_diagnostic_audit") <- stpd_product_apply_manual_lock_to_audit(audit, locked)
  }
  attr(out, "manual_lock_applied_to_auto") <- TRUE
  out
}

if (exists("run_detector_dataset_internal", mode = "function") && !exists("run_detector_dataset_internal_base", mode = "function")) {
  run_detector_dataset_internal_base <- run_detector_dataset_internal
  run_detector_dataset_internal <- function(ds, params, selected_trains = NULL, lock_manual = TRUE, collect_diagnostics = TRUE,
                                            progress_callback = NULL) {
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

    target_trains <- stpd_product_target_trains(ds, selected_trains)
    if (length(target_trains) == 0) stop("No target trains found.", call. = FALSE)

    stpd_call_progress(progress_callback, "prepare", detail = "Running pre-detection QC")
    pre_qc <- stpd_product_pre_detection_qc(ds, params, target_trains)
    qc_msg <- stpd_product_qc_error_message(pre_qc)
    if (isTRUE((params$detector %||% list())$stop_on_qc_error %||% TRUE) && nzchar(qc_msg)) {
      stop(qc_msg, call. = FALSE)
    }

    if (isTRUE((params$detector %||% list())$freeze_dataset_thresholds %||% TRUE)) {
      stpd_call_progress(progress_callback, "thresholds", detail = "Resolving dataset/manual thresholds")
      params <- stpd_product_attach_dataset_thresholds(params, ds, target_trains)
    }

    out <- run_detector_dataset_internal_base(
      ds,
      params = params,
      selected_trains = target_trains,
      lock_manual = lock_manual,
      collect_diagnostics = collect_diagnostics,
      progress_callback = progress_callback
    )
    out$quality_pre_detection <- pre_qc
    out$results$pre_detection_quality <- pre_qc
    if (!is.null((params$event_grammar %||% list())$threshold_table)) out$results$threshold_table <- params$event_grammar$threshold_table
    out
  }
}
