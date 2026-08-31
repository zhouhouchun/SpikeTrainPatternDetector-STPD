# Threshold-first Phase 0 contracts -----------------------------------------
#
# This module is deliberately dormant. It freezes schemas, normalization,
# run-mode semantics, and deterministic identity rules for the future
# threshold-first path without changing default parameters or detector calls.

STPD_THRESHOLD_FIRST_CONTRACT_VERSION <- "stpd_threshold_first_contract_v1"
STPD_THRESHOLD_FIRST_RUN_CONFIG_VERSION <- "stpd_threshold_first_run_config_v1"
STPD_THRESHOLD_FIRST_TRAIN_VIEW_VERSION <- "stpd_normalized_train_view_v1"
STPD_THRESHOLD_FIRST_LABEL_FREE_VERSION <- "stpd_label_free_train_view_v1"
STPD_THRESHOLD_FIRST_INPUT_PROVENANCE_VERSION <- "stpd_input_provenance_v1"
STPD_THRESHOLD_FIRST_NUMERIC_VERSION <- "stpd_threshold_first_numeric_primitives_v1"
STPD_THRESHOLD_FIRST_SCIENTIFIC_HASH_VERSION <- "stpd_threshold_first_scientific_projection_v1"
STPD_THRESHOLD_FIRST_SCIENTIFIC_PARAMS_VERSION <- "stpd_threshold_first_scientific_params_v1"
STPD_THRESHOLD_FIRST_PROVIDER_INTERFACE_VERSION <- "stpd_threshold_first_provider_interface_v1"
STPD_THRESHOLD_FIRST_PROVIDER_IDENTITY_VERSION <- "stpd_threshold_first_provider_identity_v1"
STPD_THRESHOLD_FIRST_PROVIDER_MAPPING_VERSION <- "stpd_threshold_first_provider_mapping_v1"
STPD_THRESHOLD_FIRST_FROZEN_PROVIDER_CONTRACT_SHA256 <-
  "29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b"

stpd_threshold_first_abort <- function(code, message, field = NA_character_) {
  condition <- structure(
    list(
      message = as.character(message)[1L], call = NULL,
      code = as.character(code)[1L], field = as.character(field)[1L]
    ),
    class = c(
      paste0("stpd_threshold_first_", as.character(code)[1L]),
      "stpd_threshold_first_error", "error", "condition"
    )
  )
  stop(condition)
}

stpd_threshold_first_scalar_character <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stpd_threshold_first_abort(
      "field_invalid", paste0(field, " must be one non-empty character value."),
      field = field
    )
  }
  x
}

stpd_threshold_first_scalar_logical <- function(x, field) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stpd_threshold_first_abort(
      "field_invalid", paste0(field, " must be one non-missing logical value."),
      field = field
    )
  }
  x
}

stpd_threshold_first_numeric_vector <- function(
    x, field, allow_empty = FALSE) {
  extra_attributes <- setdiff(names(attributes(x)), "names")
  if (!(is.double(x) || is.integer(x)) || length(extra_attributes) ||
      (!allow_empty && !length(x))) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      paste0(field, " must be an unclassed real numeric vector."), field = field
    )
  }
  as.double(x)
}

stpd_threshold_first_integer_scalar <- function(x, field, minimum = 0L) {
  value <- stpd_threshold_first_numeric_vector(x, field)
  if (length(value) != 1L || !is.finite(value) || value != round(value) ||
      value < minimum || value > .Machine$integer.max) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      paste0(field, " must be one integer >= ", minimum, "."), field = field
    )
  }
  as.integer(value)
}

stpd_threshold_first_choice <- function(x, choices, field) {
  x <- stpd_threshold_first_scalar_character(x, field)
  if (!(x %in% choices)) {
    stpd_threshold_first_abort(
      "enum_invalid",
      paste0(field, " must be one of: ", paste(choices, collapse = ", "), "."),
      field = field
    )
  }
  x
}

stpd_threshold_first_validate_named_container <- function(x, field) {
  if (!is.list(x)) {
    stpd_threshold_first_abort(
      "legacy_config_invalid", paste0(field, " must be a list."), field = field
    )
  }
  if (length(x) && (is.null(names(x)) || anyNA(names(x)) ||
                    any(!nzchar(names(x))) || anyDuplicated(names(x)))) {
    stpd_threshold_first_abort(
      "legacy_config_invalid",
      paste0(field, " requires unique non-empty field names."), field = field
    )
  }
  invisible(TRUE)
}

stpd_threshold_first_exact_legacy_get <- function(x, name, field) {
  stpd_threshold_first_validate_named_container(x, field)
  fields <- names(x) %||% character()
  partial <- (startsWith(fields, name) | startsWith(name, fields)) &
    fields != name
  if (any(partial)) {
    stpd_threshold_first_abort(
      "legacy_config_unknown_field",
      paste0(
        field, " contains a partial-match lookalike for '", name, "': ",
        paste(fields[partial], collapse = ", "), "."
      ),
      field = field
    )
  }
  if (!(name %in% fields)) return(NULL)
  x[[name, exact = TRUE]]
}

stpd_threshold_first_run_config_enums <- function() {
  list(
    engine_algorithm = c(
      "legacy", "threshold_first_shadow", "threshold_first_experimental"
    ),
    threshold_source = c(
      "user", "provider", "train_adaptive", "ordered_fallback"
    ),
    manual_policy = c("ignore", "evidence_only", "lock", "override"),
    audit_level = c("off", "summary", "full")
  )
}

stpd_threshold_first_validate_run_config <- function(x) {
  required <- c(
    "schema_version", "engine_algorithm", "threshold_source",
    "manual_policy", "audit_level"
  )
  if (!is.list(x) || is.null(names(x))) {
    stpd_threshold_first_abort(
      "run_config_invalid", "Threshold-first run config must be a named list."
    )
  }
  if (anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    stpd_threshold_first_abort(
      "run_config_invalid",
      "Threshold-first run config fields require unique non-empty names."
    )
  }
  unknown <- setdiff(names(x), required)
  missing <- setdiff(required, names(x))
  if (length(unknown)) {
    stpd_threshold_first_abort(
      "unknown_field",
      paste0("Unknown threshold-first run config field(s): ",
             paste(unknown, collapse = ", "), ".")
    )
  }
  if (length(missing)) {
    stpd_threshold_first_abort(
      "required_field_missing",
      paste0("Missing threshold-first run config field(s): ",
             paste(missing, collapse = ", "), ".")
    )
  }
  if (!identical(
    stpd_threshold_first_scalar_character(x$schema_version, "schema_version"),
    STPD_THRESHOLD_FIRST_RUN_CONFIG_VERSION
  )) {
    stpd_threshold_first_abort(
      "schema_version_unsupported",
      "Unsupported threshold-first run config schema_version.",
      field = "schema_version"
    )
  }
  enums <- stpd_threshold_first_run_config_enums()
  out <- list(
    schema_version = STPD_THRESHOLD_FIRST_RUN_CONFIG_VERSION,
    engine_algorithm = stpd_threshold_first_choice(
      x$engine_algorithm, enums$engine_algorithm, "engine_algorithm"
    ),
    threshold_source = stpd_threshold_first_choice(
      x$threshold_source, enums$threshold_source, "threshold_source"
    ),
    manual_policy = stpd_threshold_first_choice(
      x$manual_policy, enums$manual_policy, "manual_policy"
    ),
    audit_level = stpd_threshold_first_choice(
      x$audit_level, enums$audit_level, "audit_level"
    )
  )
  if (identical(out$engine_algorithm, "legacy") &&
      !identical(out$threshold_source, "ordered_fallback")) {
    stpd_threshold_first_abort(
      "mode_combination_invalid",
      "engine_algorithm='legacy' requires threshold_source='ordered_fallback'."
    )
  }
  if (!identical(out$engine_algorithm, "legacy") &&
      identical(out$threshold_source, "ordered_fallback")) {
    stpd_threshold_first_abort(
      "mode_combination_invalid",
      "threshold_source='ordered_fallback' is reserved for legacy compatibility."
    )
  }
  out
}

stpd_threshold_first_run_config <- function(
    engine_algorithm = "legacy", threshold_source = "ordered_fallback",
    manual_policy = "lock", audit_level = "off") {
  stpd_threshold_first_validate_run_config(list(
    schema_version = STPD_THRESHOLD_FIRST_RUN_CONFIG_VERSION,
    engine_algorithm = engine_algorithm, threshold_source = threshold_source,
    manual_policy = manual_policy, audit_level = audit_level
  ))
}

stpd_threshold_first_migrate_legacy_config <- function(
    legacy_config = list(), label_blind = FALSE, lock_manual = TRUE,
    collect_diagnostics = FALSE) {
  label_blind <- stpd_threshold_first_scalar_logical(label_blind, "label_blind")
  lock_manual <- stpd_threshold_first_scalar_logical(lock_manual, "lock_manual")
  collect_diagnostics <- stpd_threshold_first_scalar_logical(
    collect_diagnostics, "collect_diagnostics"
  )
  if (!is.list(legacy_config)) {
    stpd_threshold_first_abort(
      "legacy_config_invalid", "legacy_config must be a list."
    )
  }
  stpd_threshold_first_validate_named_container(legacy_config, "legacy_config")
  nested <- stpd_threshold_first_exact_legacy_get(
    legacy_config, "threshold_first", "legacy_config"
  )
  new_fields <- c(
    "schema_version", "engine_algorithm", "threshold_source",
    "manual_policy", "audit_level"
  )
  top_present <- intersect(names(legacy_config), new_fields)
  if (!is.null(nested) && length(top_present)) {
    stpd_threshold_first_abort(
      "migration_conflict",
      "New run-config fields cannot appear both at top level and under threshold_first."
    )
  }
  candidate <- if (!is.null(nested)) nested else legacy_config[top_present]
  if (length(candidate)) {
    if (length(candidate) != length(new_fields) ||
        !setequal(names(candidate), new_fields)) {
      stpd_threshold_first_abort(
        "partial_new_config",
        "A partially specified threshold-first run config cannot be migrated silently."
      )
    }
    validated <- stpd_threshold_first_validate_run_config(candidate)
    if (isTRUE(label_blind) &&
        !identical(validated$manual_policy, "ignore")) {
      stpd_threshold_first_abort(
        "label_blind_conflict",
        paste0(
          "label_blind=TRUE requires manual_policy='ignore'; ",
          "current configuration contains manual authority."
        )
      )
    }
    return(list(
      config = validated,
      migration = data.frame(
        migration_version = STPD_THRESHOLD_FIRST_RUN_CONFIG_VERSION,
        source_format = "already_current",
        legacy_threshold_source_mode = NA_character_,
        label_blind = isTRUE(label_blind), lock_manual = isTRUE(lock_manual),
        changed = FALSE, stringsAsFactors = FALSE
      )
    ))
  }
  event_grammar <- stpd_threshold_first_exact_legacy_get(
    legacy_config, "event_grammar", "legacy_config"
  )
  spiketrainpattern <- stpd_threshold_first_exact_legacy_get(
    legacy_config, "spiketrainpattern", "legacy_config"
  )
  if (!is.null(event_grammar)) {
    event_grammar_source <- stpd_threshold_first_exact_legacy_get(
      event_grammar, "threshold_source_mode", "legacy_config$event_grammar"
    )
  } else {
    event_grammar_source <- NULL
  }
  if (!is.null(spiketrainpattern)) {
    engine <- stpd_threshold_first_exact_legacy_get(
      spiketrainpattern, "engine", "legacy_config$spiketrainpattern"
    )
  } else {
    engine <- NULL
  }
  if (!is.null(engine)) {
    engine_source <- stpd_threshold_first_exact_legacy_get(
      engine, "threshold_source_mode",
      "legacy_config$spiketrainpattern$engine"
    )
  } else {
    engine_source <- NULL
  }
  legacy_source <- event_grammar_source %||% engine_source %||% "auto"
  legacy_source <- tolower(stpd_threshold_first_scalar_character(
    legacy_source, "legacy_threshold_source_mode"
  ))
  allowed_legacy_sources <- c(
    "auto", "auto_priority", "user", "manual", "histogram", "default"
  )
  if (!(legacy_source %in% allowed_legacy_sources)) {
    stpd_threshold_first_abort(
      "legacy_source_unknown",
      paste0("Unknown legacy threshold_source_mode: ", legacy_source, "."),
      field = "legacy_threshold_source_mode"
    )
  }
  manual_policy <- if (isTRUE(label_blind)) {
    "ignore"
  } else if (isTRUE(lock_manual)) {
    "lock"
  } else {
    "evidence_only"
  }
  config <- stpd_threshold_first_run_config(
    engine_algorithm = "legacy", threshold_source = "ordered_fallback",
    manual_policy = manual_policy,
    audit_level = if (isTRUE(collect_diagnostics)) "full" else "off"
  )
  list(
    config = config,
    migration = data.frame(
      migration_version = STPD_THRESHOLD_FIRST_RUN_CONFIG_VERSION,
      source_format = "legacy_missing_threshold_first_fields",
      legacy_threshold_source_mode = legacy_source,
      label_blind = isTRUE(label_blind), lock_manual = isTRUE(lock_manual),
      changed = TRUE, stringsAsFactors = FALSE
    )
  )
}

stpd_threshold_first_numeric_contract <- function() {
  list(
    schema_version = STPD_THRESHOLD_FIRST_NUMERIC_VERSION,
    definition_status = "definition_frozen",
    engineering_defaults_status = "shadow_development_preregistered",
    engineering_defaults_provenance = paste0(
      "Phase 0 shadow defaults; not universal neurophysiological constants; ",
      "must undergo predeclared sensitivity analysis before promotion"
    ),
    canonical_time_unit = "s", log_base = "e",
    local_reference_valid_isi_each_side = 50L,
    guard_valid_isi_each_side = 2L,
    minimum_reference_valid_isi = 30L,
    robust_center = "median_log_isi",
    robust_scale = "mad_log_isi_constant_1.4826",
    zero_mad_status = "unresolved_zero_mad",
    empirical_tail_correction = "(1+inclusive_tail_count)/(n_reference+1)",
    empirical_tail_ties = "inclusive",
    multiplicity_adjustment = "BH",
    multiplicity_family = "train_x_analysis_block_x_proposer",
    multiplicity_completeness_status =
      "unverified_until_candidate_lineage_gate",
    stable_tail_fdr_q_max = 0.05,
    gray_zone_tail_fdr_q_max = 0.10,
    engineering_sensitivity_grid = list(
      local_reference_valid_isi_each_side = c(30L, 50L, 80L),
      guard_valid_isi_each_side = c(1L, 2L, 4L),
      minimum_reference_valid_isi = c(20L, 30L, 40L),
      stable_tail_fdr_q_max = c(0.025, 0.05, 0.075),
      gray_zone_tail_fdr_q_max = c(0.075, 0.10, 0.15)
    ),
    run_surprise = "sum(-log10(raw_empirical_tail_p))",
    ratio_role = "secondary_evidence_not_universal_hard_gate",
    absolute_floor_role = "domain_qc_guard_not_universal_pattern_threshold",
    threshold_interval_closure = "closed",
    contrast_seed_min_spikes = 4L,
    contrast_seed_scope = "initial_contrast_proposer_only_not_final_burst_gate",
    strict_single_bridge_ratio_max = 3.5,
    strict_bridge_scope = "single_internal_bridge_only_not_general_burst_gate",
    strict_bridge_direct_support_requirement =
      "two_sided_contiguous_upstream_verified_direct_support",
    strict_bridge_min_flank_isi = 2L,
    floating_tolerance = sqrt(.Machine$double.eps),
    rng_contract = "deterministic_no_rng"
  )
}

stpd_threshold_first_robust_log_reference <- function(
    isi_sec, minimum_n = stpd_threshold_first_numeric_contract()$
      minimum_reference_valid_isi) {
  values <- stpd_threshold_first_numeric_vector(isi_sec, "isi_sec")
  minimum_n <- stpd_threshold_first_integer_scalar(
    minimum_n, "minimum_n", minimum = 2L
  )
  if (any(!is.finite(values) | values <= 0)) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      "Reference ISIs must all be finite and positive; filter only through the validated QC view."
    )
  }
  if (length(values) < minimum_n) {
    return(list(
      status = "unresolved_insufficient_reference",
      n_reference = length(values), center_log_isi = NA_real_,
      scale_log_isi = NA_real_
    ))
  }
  logged <- log(values)
  center <- stats::median(logged)
  scale <- stats::mad(logged, center = center, constant = 1.4826)
  if (!is.finite(scale) ||
      scale <= stpd_threshold_first_numeric_contract()$floating_tolerance) {
    return(list(
      status = "unresolved_zero_mad", n_reference = length(values),
      center_log_isi = center, scale_log_isi = scale
    ))
  }
  list(
    status = "resolved", n_reference = length(values),
    center_log_isi = center, scale_log_isi = scale
  )
}

stpd_threshold_first_log_residual <- function(isi_sec, center_sec) {
  isi_sec <- stpd_threshold_first_numeric_vector(isi_sec, "isi_sec")
  center_sec <- stpd_threshold_first_numeric_vector(center_sec, "center_sec")
  if (length(center_sec) != 1L || !is.finite(center_sec) || center_sec <= 0 ||
      any(!is.finite(isi_sec) | isi_sec <= 0)) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      "isi_sec and center_sec must contain finite positive values."
    )
  }
  log(isi_sec / center_sec)
}

stpd_threshold_first_empirical_tail_p <- function(
    query, reference, tail = "lower") {
  tail <- stpd_threshold_first_choice(tail, c("lower", "upper"), "tail")
  query <- stpd_threshold_first_numeric_vector(query, "query")
  reference <- stpd_threshold_first_numeric_vector(reference, "reference")
  if (any(!is.finite(query)) || any(!is.finite(reference))) {
    stpd_threshold_first_abort(
      "numeric_input_invalid", "query and reference must be finite numeric vectors."
    )
  }
  n <- length(reference)
  vapply(query, function(value) {
    count <- if (identical(tail, "lower")) {
      sum(reference <= value)
    } else {
      sum(reference >= value)
    }
    (1 + count) / (n + 1)
  }, numeric(1))
}

stpd_threshold_first_bh_adjust <- function(p) {
  p <- stpd_threshold_first_numeric_vector(p, "p", allow_empty = TRUE)
  if (any(!is.finite(p) | p < 0 | p > 1)) {
    stpd_threshold_first_abort(
      "numeric_input_invalid", "p must contain finite probabilities in [0, 1]."
    )
  }
  stats::p.adjust(p, method = "BH")
}

stpd_threshold_first_run_surprise <- function(raw_tail_p) {
  raw_tail_p <- stpd_threshold_first_numeric_vector(raw_tail_p, "raw_tail_p")
  if (any(!is.finite(raw_tail_p) |
                                raw_tail_p <= 0 | raw_tail_p > 1)) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      "raw_tail_p must contain one or more finite probabilities in (0, 1]."
    )
  }
  sum(-log10(raw_tail_p))
}

stpd_threshold_first_local_reference_indices <- function(
    valid_isi, analysis_block_id, query_index,
    each_side = stpd_threshold_first_numeric_contract()$
      local_reference_valid_isi_each_side,
    guard = stpd_threshold_first_numeric_contract()$guard_valid_isi_each_side,
    minimum_n = stpd_threshold_first_numeric_contract()$
      minimum_reference_valid_isi) {
  n <- length(valid_isi)
  if (!is.logical(valid_isi) ||
      length(setdiff(names(attributes(valid_isi)), "names")) ||
      !is.character(analysis_block_id) ||
      length(setdiff(names(attributes(analysis_block_id)), "names")) ||
      anyNA(valid_isi) || length(analysis_block_id) != n ||
      anyNA(analysis_block_id) || any(!nzchar(analysis_block_id))) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      "valid_isi and analysis_block_id must be complete aligned vectors."
    )
  }
  query_index <- stpd_threshold_first_integer_scalar(
    query_index, "query_index", 1L
  )
  each_side <- stpd_threshold_first_integer_scalar(each_side, "each_side", 1L)
  guard <- stpd_threshold_first_integer_scalar(guard, "guard", 0L)
  minimum_n <- stpd_threshold_first_integer_scalar(minimum_n, "minimum_n", 2L)
  if (query_index > n) {
    stpd_threshold_first_abort(
      "numeric_input_invalid", "query_index lies outside the aligned vectors.",
      field = "query_index"
    )
  }
  if (!valid_isi[[query_index]]) {
    return(list(
      status = "unresolved_query_ineligible", indices = integer(),
      left_indices = integer(), right_indices = integer(), n_reference = 0L,
      analysis_block_id = as.character(analysis_block_id[[query_index]])
    ))
  }
  same_block <- as.character(analysis_block_id) ==
    as.character(analysis_block_id[[query_index]])
  left <- which(valid_isi & same_block & seq_len(n) < query_index)
  right <- which(valid_isi & same_block & seq_len(n) > query_index)
  left <- if (length(left) > guard) {
    left[seq_len(length(left) - guard)]
  } else {
    integer()
  }
  right <- if (length(right) > guard) {
    right[seq.int(guard + 1L, length(right))]
  } else {
    integer()
  }
  left <- tail(left, each_side)
  right <- head(right, each_side)
  indices <- as.integer(c(left, right))
  list(
    status = if (length(indices) >= minimum_n) "resolved" else
      "unresolved_insufficient_reference",
    indices = indices, left_indices = as.integer(left),
    right_indices = as.integer(right), n_reference = length(indices),
    analysis_block_id = as.character(analysis_block_id[[query_index]])
  )
}

stpd_threshold_first_local_ratio <- function(query_isi_sec, center_log_isi) {
  query_isi_sec <- stpd_threshold_first_numeric_vector(
    query_isi_sec, "query_isi_sec"
  )
  center_log_isi <- stpd_threshold_first_numeric_vector(
    center_log_isi, "center_log_isi"
  )
  if (length(query_isi_sec) != 1L || length(center_log_isi) != 1L ||
      !is.finite(query_isi_sec) || query_isi_sec <= 0 ||
      !is.finite(center_log_isi)) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      "query_isi_sec must be positive and center_log_isi must be finite."
    )
  }
  query_isi_sec / exp(center_log_isi)
}

stpd_threshold_first_adjust_tail_by_family <- function(raw_tail_p, family_id) {
  raw_tail_p <- stpd_threshold_first_numeric_vector(raw_tail_p, "raw_tail_p")
  if (!is.character(family_id) ||
      length(setdiff(names(attributes(family_id)), "names")) ||
      length(family_id) != length(raw_tail_p) ||
      anyNA(family_id) || any(!nzchar(family_id)) ||
      any(!is.finite(raw_tail_p) | raw_tail_p < 0 | raw_tail_p > 1)) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      "raw_tail_p and family_id must be complete aligned vectors."
    )
  }
  out <- numeric(length(raw_tail_p))
  families <- as.character(family_id)
  for (family in unique(families)) {
    rows <- which(families == family)
    out[rows] <- stats::p.adjust(raw_tail_p[rows], method = "BH")
  }
  out
}

stpd_threshold_first_q_zone <- function(
    q, stable_max = stpd_threshold_first_numeric_contract()$
      stable_tail_fdr_q_max,
    gray_max = stpd_threshold_first_numeric_contract()$
      gray_zone_tail_fdr_q_max) {
  q <- stpd_threshold_first_numeric_vector(q, "q")
  stable_max <- stpd_threshold_first_numeric_vector(stable_max, "stable_max")
  gray_max <- stpd_threshold_first_numeric_vector(gray_max, "gray_max")
  if (any(!is.finite(q) | q < 0 | q > 1) ||
      length(stable_max) != 1L || length(gray_max) != 1L ||
      !is.finite(stable_max) || !is.finite(gray_max) ||
      stable_max < 0 || gray_max > 1 || stable_max > gray_max) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      "q-zone bounds must satisfy 0 <= stable <= gray <= 1."
    )
  }
  ifelse(q <= stable_max, "stable_support",
         ifelse(q <= gray_max, "gray_zone", "outside"))
}

stpd_threshold_first_qc_floor_status <- function(isi_sec, min_valid_isi_sec) {
  isi_sec <- stpd_threshold_first_numeric_vector(
    isi_sec, "isi_sec", allow_empty = TRUE
  )
  floor_sec <- stpd_threshold_first_numeric_vector(
    min_valid_isi_sec, "min_valid_isi_sec"
  )
  if (length(floor_sec) != 1L || !is.finite(floor_sec) || floor_sec <= 0) {
    stpd_threshold_first_abort(
      "numeric_input_invalid", "min_valid_isi_sec must be finite and positive."
    )
  }
  ifelse(!is.finite(isi_sec), "nonfinite",
         ifelse(isi_sec <= 0, "nonpositive",
                ifelse(isi_sec < floor_sec, "below_qc_floor", "eligible")))
}

stpd_threshold_first_bridge_ratio <- function(
    candidate_isi_sec, bridge_index, direct_support_mask,
    minimum_flank_isi = stpd_threshold_first_numeric_contract()$
      strict_bridge_min_flank_isi,
    ratio_max = stpd_threshold_first_numeric_contract()$
      strict_single_bridge_ratio_max) {
  if (missing(direct_support_mask)) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      "direct_support_mask is required; the bridge helper cannot infer a Burst core."
    )
  }
  values <- stpd_threshold_first_numeric_vector(
    candidate_isi_sec, "candidate_isi_sec"
  )
  bridge_index <- stpd_threshold_first_integer_scalar(
    bridge_index, "bridge_index", 2L
  )
  minimum_flank_isi <- stpd_threshold_first_integer_scalar(
    minimum_flank_isi, "minimum_flank_isi",
    stpd_threshold_first_numeric_contract()$strict_bridge_min_flank_isi
  )
  ratio_max <- stpd_threshold_first_numeric_vector(ratio_max, "ratio_max")
  if (!is.logical(direct_support_mask) || anyNA(direct_support_mask) ||
      length(direct_support_mask) != length(values) ||
      length(setdiff(names(attributes(direct_support_mask)), "names")) ||
      length(values) < 3L || any(!is.finite(values) | values <= 0) ||
      bridge_index >= length(values) || direct_support_mask[[bridge_index]] ||
      length(ratio_max) != 1L ||
      !is.finite(ratio_max) || ratio_max <= 0 ||
      ratio_max >
        stpd_threshold_first_numeric_contract()$strict_single_bridge_ratio_max) {
    stpd_threshold_first_abort(
      "numeric_input_invalid",
      paste0(
        "Bridge ratio requires positive ISIs and one internal bridge ",
        "with direct support on both sides."
      )
    )
  }
  left_walk <- rev(seq_len(bridge_index - 1L))
  right_walk <- seq.int(bridge_index + 1L, length(values))
  left_indices <- sort(left_walk[
    cumprod(as.integer(direct_support_mask[left_walk])) == 1L
  ])
  right_indices <- right_walk[
    cumprod(as.integer(direct_support_mask[right_walk])) == 1L
  ]
  if (length(left_indices) < minimum_flank_isi ||
      length(right_indices) < minimum_flank_isi) {
    return(list(
      status = "bridge_rejected_insufficient_direct_flank",
      bridge_ratio = NA_real_, left_core_ratio = NA_real_,
      right_core_ratio = NA_real_, core_mean_isi_sec = NA_real_,
      left_direct_isi_count = as.integer(length(left_indices)),
      right_direct_isi_count = as.integer(length(right_indices)),
      minimum_flank_isi = minimum_flank_isi, ratio_max = ratio_max
    ))
  }
  left_mean <- mean(values[left_indices])
  right_mean <- mean(values[right_indices])
  core_mean <- mean(c(values[left_indices], values[right_indices]))
  left_ratio <- values[[bridge_index]] / left_mean
  right_ratio <- values[[bridge_index]] / right_mean
  ratio <- max(left_ratio, right_ratio)
  tolerance <- stpd_threshold_first_numeric_contract()$floating_tolerance *
    max(1, ratio_max)
  list(
    status = if (ratio <= ratio_max + tolerance) {
      "bridge_supported"
    } else {
      "bridge_rejected"
    },
    bridge_ratio = ratio,
    left_core_ratio = left_ratio, right_core_ratio = right_ratio,
    core_mean_isi_sec = core_mean,
    left_direct_isi_count = as.integer(length(left_indices)),
    right_direct_isi_count = as.integer(length(right_indices)),
    minimum_flank_isi = minimum_flank_isi, ratio_max = ratio_max
  )
}

stpd_threshold_first_resolution_action <- function(status) {
  status <- stpd_threshold_first_choice(
    status,
    c(
      "resolved", "unresolved_insufficient_reference",
      "unresolved_zero_mad", "unresolved_query_ineligible"
    ),
    "status"
  )
  if (identical(status, "resolved")) {
    "use_local_evidence"
  } else {
    "abstain_no_borrowing"
  }
}

stpd_threshold_first_as_numeric <- function(x, field) {
  if (!(is.double(x) || is.integer(x)) || is.complex(x) ||
      !is.null(attributes(x))) {
    stpd_threshold_first_abort(
      "field_invalid",
      paste0(
        field,
        " must be one unclassed one-dimensional real numeric vector."
      ),
      field = field
    )
  }
  as.double(x)
}

stpd_threshold_first_strict_logical <- function(x, n, field, default) {
  if (is.null(x)) return(rep(as.logical(default), n))
  if (!is.logical(x) || length(x) != n || anyNA(x)) {
    stpd_threshold_first_abort(
      "field_invalid",
      paste0(field, " must be a non-missing logical vector of length ", n, "."),
      field = field
    )
  }
  x
}

stpd_threshold_first_input_policy_contract <- function() {
  boundary <- list(
    policy_id = "raw_acquisition_boundary_policy_v1",
    definition = paste0(
      "Use caller-declared acquisition segment labels, explicit hard-boundary ",
      "flags, and any dropped nonfinite timestamp as boundaries; never use ",
      "manual, truth, or predicted pattern labels."
    )
  )
  qc <- list(
    policy_id = "raw_acquisition_qc_mask_policy_v1",
    definition = paste0(
      "Use only acquisition/artifact QC to form a right-spike-aligned ISI ",
      "mask; never derive the mask from manual, truth, or predicted patterns."
    )
  )
  list(
    schema_version = STPD_THRESHOLD_FIRST_INPUT_PROVENANCE_VERSION,
    boundary_policy = c(
      boundary,
      list(policy_sha256 = stpd_threshold_first_hash_domain(
        "stpd-threshold-first-boundary-policy-v1", boundary
      ))
    ),
    qc_mask_policy = c(
      qc,
      list(policy_sha256 = stpd_threshold_first_hash_domain(
        "stpd-threshold-first-qc-mask-policy-v1", qc
      ))
    ),
    valid_isi_mask_alignment = "right_spike_isi_row"
  )
}

stpd_threshold_first_input_provenance <- function(
    information_access = "unknown",
    boundary_policy_id = NULL, boundary_policy_sha256 = NULL,
    qc_mask_policy_id = NULL, qc_mask_policy_sha256 = NULL) {
  information_access <- stpd_threshold_first_choice(
    information_access, c("unknown", "label_blind", "manual_aware"),
    "information_access"
  )
  contract <- stpd_threshold_first_input_policy_contract()
  boundary_policy_id <- boundary_policy_id %||%
    contract$boundary_policy$policy_id
  boundary_policy_sha256 <- boundary_policy_sha256 %||%
    contract$boundary_policy$policy_sha256
  qc_mask_policy_id <- qc_mask_policy_id %||%
    contract$qc_mask_policy$policy_id
  qc_mask_policy_sha256 <- qc_mask_policy_sha256 %||%
    contract$qc_mask_policy$policy_sha256
  out <- structure(
    list(
      schema_version = STPD_THRESHOLD_FIRST_INPUT_PROVENANCE_VERSION,
      information_access = information_access,
      boundary_policy_id = stpd_threshold_first_scalar_character(
        boundary_policy_id, "boundary_policy_id"
      ),
      boundary_policy_sha256 = stpd_threshold_first_sha256_or_null(
        boundary_policy_sha256, "boundary_policy_sha256"
      ),
      qc_mask_policy_id = stpd_threshold_first_scalar_character(
        qc_mask_policy_id, "qc_mask_policy_id"
      ),
      qc_mask_policy_sha256 = stpd_threshold_first_sha256_or_null(
        qc_mask_policy_sha256, "qc_mask_policy_sha256"
      ),
      valid_isi_mask_alignment = contract$valid_isi_mask_alignment
    ),
    class = c("stpd_threshold_first_input_provenance_v1", "list")
  )
  stpd_threshold_first_validate_input_provenance(out)
  out
}

stpd_threshold_first_validate_input_provenance <- function(x) {
  required <- c(
    "schema_version", "information_access", "boundary_policy_id",
    "boundary_policy_sha256", "qc_mask_policy_id", "qc_mask_policy_sha256",
    "valid_isi_mask_alignment"
  )
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
      !setequal(names(x), required) || length(x) != length(required)) {
    stpd_threshold_first_abort(
      "input_provenance_invalid",
      "Input provenance must contain the exact frozen fields."
    )
  }
  if (!identical(x$schema_version,
                 STPD_THRESHOLD_FIRST_INPUT_PROVENANCE_VERSION)) {
    stpd_threshold_first_abort(
      "schema_version_unsupported", "Input provenance schema is unsupported."
    )
  }
  stpd_threshold_first_choice(
    x$information_access, c("label_blind", "manual_aware", "unknown"),
    "information_access"
  )
  stpd_threshold_first_scalar_character(
    x$boundary_policy_id, "boundary_policy_id"
  )
  stpd_threshold_first_sha256_or_null(
    x$boundary_policy_sha256, "boundary_policy_sha256"
  )
  stpd_threshold_first_scalar_character(x$qc_mask_policy_id, "qc_mask_policy_id")
  stpd_threshold_first_sha256_or_null(
    x$qc_mask_policy_sha256, "qc_mask_policy_sha256"
  )
  if (!identical(x$valid_isi_mask_alignment, "right_spike_isi_row")) {
    stpd_threshold_first_abort(
      "input_provenance_invalid",
      "valid_isi_mask_alignment must be right_spike_isi_row."
    )
  }
  if (identical(x$information_access, "label_blind")) {
    contract <- stpd_threshold_first_input_policy_contract()
    expected <- c(
      boundary_policy_id = contract$boundary_policy$policy_id,
      boundary_policy_sha256 = contract$boundary_policy$policy_sha256,
      qc_mask_policy_id = contract$qc_mask_policy$policy_id,
      qc_mask_policy_sha256 = contract$qc_mask_policy$policy_sha256
    )
    actual <- unlist(x[names(expected)], use.names = TRUE)
    if (!identical(actual, expected)) {
      stpd_threshold_first_abort(
        "label_blind_provenance_invalid",
        "Label-blind input requires the frozen acquisition/QC-only policies."
      )
    }
  }
  invisible(TRUE)
}

stpd_threshold_first_normalize_train_view <- function(
    dat, train_key, time_unit = "s",
    duplicate_policy = "error",
    min_valid_isi_sec = 0.0009, segment_id = NULL,
    hard_boundary_before = NULL, valid_isi_mask = NULL,
    input_provenance = stpd_threshold_first_input_provenance()) {
  time_unit <- stpd_threshold_first_choice(time_unit, c("s", "ms"), "time_unit")
  duplicate_policy <- stpd_threshold_first_choice(
    duplicate_policy, c("error", "collapse_exact"), "duplicate_policy"
  )
  train_key <- stpd_threshold_first_scalar_character(train_key, "train_key")
  stpd_threshold_first_validate_input_provenance(input_provenance)
  if (!is.data.frame(dat) || !("timestamp_sec" %in% names(dat)) || !nrow(dat)) {
    stpd_threshold_first_abort(
      "train_input_invalid",
      "dat must be a non-empty data frame containing timestamp_sec."
    )
  }
  if (!is.double(min_valid_isi_sec) || length(min_valid_isi_sec) != 1L ||
      !is.null(attributes(min_valid_isi_sec)) ||
      !is.finite(min_valid_isi_sec) || min_valid_isi_sec <= 0) {
    stpd_threshold_first_abort(
      "field_invalid",
      "min_valid_isi_sec must be one unclassed finite double > 0.",
      field = "min_valid_isi_sec"
    )
  }
  n_raw <- nrow(dat)
  timestamp <- stpd_threshold_first_as_numeric(dat$timestamp_sec, "timestamp_sec")
  if (length(timestamp) != n_raw) {
    stpd_threshold_first_abort(
      "field_invalid", "timestamp_sec length does not equal nrow(dat).",
      field = "timestamp_sec"
    )
  }
  if (identical(time_unit, "ms")) timestamp <- timestamp / 1000
  if (is.null(segment_id)) {
    segment_id <- if ("segment_id" %in% names(dat)) dat$segment_id else
      rep("segment_1", n_raw)
  }
  if (length(segment_id) != n_raw || anyNA(segment_id)) {
    stpd_threshold_first_abort(
      "field_invalid",
      "segment_id must be non-missing and have length nrow(dat).",
      field = "segment_id"
    )
  }
  segment_id <- as.character(segment_id)
  if (any(!nzchar(segment_id))) {
    stpd_threshold_first_abort(
      "field_invalid", "segment_id cannot contain empty values.",
      field = "segment_id"
    )
  }
  if (is.null(hard_boundary_before) &&
      "hard_boundary_before" %in% names(dat)) {
    hard_boundary_before <- dat$hard_boundary_before
  }
  hard_boundary_before <- stpd_threshold_first_strict_logical(
    hard_boundary_before, n_raw, "hard_boundary_before", FALSE
  )
  if (is.null(valid_isi_mask)) {
    if ("valid_isi_mask" %in% names(dat)) {
      valid_isi_mask <- dat$valid_isi_mask
    } else if ("valid_isi" %in% names(dat)) {
      valid_isi_mask <- dat$valid_isi
    }
  }
  valid_isi_mask <- stpd_threshold_first_strict_logical(
    valid_isi_mask, n_raw, "valid_isi_mask", TRUE
  )
  raw_row <- seq_len(n_raw)
  source_row_key <- if ("idx" %in% names(dat)) {
    if (anyNA(dat$idx)) {
      stpd_threshold_first_abort(
        "source_row_key_invalid", "idx cannot contain missing values."
      )
    }
    as.character(dat$idx)
  } else {
    as.character(raw_row)
  }
  if (anyNA(source_row_key) || any(!nzchar(source_row_key)) ||
      anyDuplicated(source_row_key)) {
    stpd_threshold_first_abort(
      "source_row_key_invalid",
      "Source row keys must be unique, non-missing, and non-empty."
    )
  }
  finite <- is.finite(timestamp)
  segment_run_start <- c(
    TRUE, segment_id[-1L] != segment_id[-length(segment_id)]
  )
  segment_run_labels <- segment_id[segment_run_start]
  if (anyDuplicated(segment_run_labels)) {
    stpd_threshold_first_abort(
      "segment_reentry",
      "A segment_id cannot leave and later re-enter the raw acquisition order."
    )
  }
  raw_acquisition_block <- rep(NA_integer_, n_raw)
  block_number <- 0L
  for (i in raw_row) {
    if (!finite[[i]]) next
    begins <- i == 1L || !finite[[i - 1L]] ||
      !identical(segment_id[[i]], segment_id[[i - 1L]]) ||
      isTRUE(hard_boundary_before[[i]])
    if (begins) block_number <- block_number + 1L
    raw_acquisition_block[[i]] <- block_number
  }
  finite_rows <- raw_row[finite]
  if (!length(finite_rows)) {
    stpd_threshold_first_abort(
      "timestamp_unavailable", "No finite timestamps remain after normalization."
    )
  }
  order_finite <- finite_rows[order(
    raw_acquisition_block[finite_rows], timestamp[finite_rows],
    raw_row[finite_rows],
    method = "radix"
  )]
  sorted_block <- raw_acquisition_block[order_finite]
  sorted_segment <- segment_id[order_finite]
  sorted_timestamp <- timestamp[order_finite]
  group_start <- c(
    TRUE,
    sorted_block[-1L] != sorted_block[-length(sorted_block)] |
      sorted_timestamp[-1L] != sorted_timestamp[-length(sorted_timestamp)]
  )
  group_number <- cumsum(group_start)
  group_sizes <- tabulate(group_number)
  duplicate_groups <- which(group_sizes > 1L)
  if (length(duplicate_groups) && identical(duplicate_policy, "error")) {
    stpd_threshold_first_abort(
      "duplicate_timestamp",
      "Duplicate timestamps were found within a segment; use collapse_exact explicitly to collapse them."
    )
  }
  representative_pos <- which(group_start)
  representative_raw <- order_finite[representative_pos]
  n_spikes <- length(representative_raw)
  map_norm <- rep(NA_integer_, n_raw)
  map_group <- rep(NA_character_, n_raw)
  map_status <- rep("dropped_nonfinite_timestamp", n_raw)
  for (pos in seq_along(order_finite)) {
    rr <- order_finite[[pos]]
    group <- group_number[[pos]]
    map_norm[[rr]] <- group
    map_group[[rr]] <- sprintf("collapse_%06d", group)
    map_status[[rr]] <- if (pos == representative_pos[[group]]) {
      "retained"
    } else {
      "collapsed_duplicate"
    }
  }
  collapsed_valid_mask <- vapply(seq_len(n_spikes), function(group) {
    all(valid_isi_mask[order_finite[group_number == group]])
  }, logical(1))
  spike_acquisition_block <- raw_acquisition_block[representative_raw]
  spike_segment <- segment_id[representative_raw]
  spike_timestamp <- timestamp[representative_raw]
  segment_spike_index <- ave(seq_len(n_spikes), spike_segment, FUN = seq_along)
  first_in_acquisition_block <- !duplicated(spike_acquisition_block)
  # A raw hard-boundary flag starts an acquisition block. If timestamps inside
  # that block are sorted, boundary authority belongs only to the resulting
  # first normalized spike, not to the original raw row at its sorted position.
  collapsed_hard_boundary <- first_in_acquisition_block
  isi <- rep(NA_real_, n_spikes)
  left_spike <- rep(NA_integer_, n_spikes)
  for (i in seq_len(n_spikes)) {
    if (i > 1L && !collapsed_hard_boundary[[i]] &&
        identical(spike_segment[[i]], spike_segment[[i - 1L]])) {
      left_spike[[i]] <- i - 1L
      isi[[i]] <- spike_timestamp[[i]] - spike_timestamp[[i - 1L]]
    }
  }
  valid_isi <- !collapsed_hard_boundary & collapsed_valid_mask &
    is.finite(isi) & isi >= min_valid_isi_sec
  invalid_reason <- rep("", n_spikes)
  invalid_reason[collapsed_hard_boundary] <- "hard_boundary"
  invalid_reason[!collapsed_hard_boundary & !collapsed_valid_mask] <-
    "caller_masked"
  invalid_reason[
    !collapsed_hard_boundary & collapsed_valid_mask &
      is.finite(isi) & isi < min_valid_isi_sec
  ] <- "below_min_valid_isi"
  invalid_reason[
    !collapsed_hard_boundary & collapsed_valid_mask & !is.finite(isi)
  ] <- "nonfinite_recomputed_isi"
  analysis_block_start <- collapsed_hard_boundary | !valid_isi
  analysis_block_number <- cumsum(analysis_block_start)
  analysis_block_id <- sprintf("analysis_%06d", analysis_block_number)
  acquisition_block_id <- sprintf(
    "acquisition_%06d", spike_acquisition_block
  )
  analysis_block_spike_index <- ave(
    seq_len(n_spikes), analysis_block_id, FUN = seq_along
  )
  spikes <- data.frame(
    train_key = rep(train_key, n_spikes),
    normalized_spike_index = as.integer(seq_len(n_spikes)),
    normalized_segment_spike_index = as.integer(segment_spike_index),
    normalized_analysis_block_spike_index = as.integer(
      analysis_block_spike_index
    ),
    timestamp_sec = as.double(spike_timestamp),
    segment_id = as.character(spike_segment),
    acquisition_block_id = as.character(acquisition_block_id),
    analysis_block_id = as.character(analysis_block_id),
    representative_raw_row_index = as.integer(representative_raw),
    hard_boundary_before = as.logical(collapsed_hard_boundary),
    input_valid_isi_mask = as.logical(collapsed_valid_mask),
    stringsAsFactors = FALSE
  )
  isis <- data.frame(
    train_key = rep(train_key, n_spikes),
    normalized_isi_index = as.integer(seq_len(n_spikes)),
    left_spike_index = as.integer(left_spike),
    right_spike_index = as.integer(seq_len(n_spikes)),
    segment_id = as.character(spike_segment),
    acquisition_block_id = as.character(acquisition_block_id),
    analysis_block_id = as.character(analysis_block_id),
    ISI_sec = as.double(isi),
    input_valid_isi_mask = as.logical(collapsed_valid_mask),
    valid_isi = as.logical(valid_isi),
    invalid_reason = as.character(invalid_reason),
    hard_boundary_before = as.logical(collapsed_hard_boundary),
    stringsAsFactors = FALSE
  )
  index_map <- data.frame(
    raw_row_index = as.integer(raw_row),
    source_row_key = source_row_key,
    segment_id = as.character(segment_id), timestamp_sec = as.double(timestamp),
    hard_boundary_before = as.logical(hard_boundary_before),
    valid_isi_mask = as.logical(valid_isi_mask),
    acquisition_block_id = ifelse(
      is.na(raw_acquisition_block), NA_character_,
      sprintf("acquisition_%06d", raw_acquisition_block)
    ),
    normalized_spike_index = as.integer(map_norm),
    analysis_block_id = ifelse(
      is.na(map_norm), NA_character_, analysis_block_id[map_norm]
    ),
    mapping_status = as.character(map_status),
    collapse_group_id = as.character(map_group), stringsAsFactors = FALSE
  )
  metadata <- list(
    schema_version = STPD_THRESHOLD_FIRST_TRAIN_VIEW_VERSION,
    label_free_schema_version = STPD_THRESHOLD_FIRST_LABEL_FREE_VERSION,
    train_key = train_key, canonical_time_unit = "s",
    input_time_unit = time_unit,
    timestamp_order_policy = "stable_sort_within_acquisition_block",
    timestamp_authority = "timestamp_sec_recompute_isi",
    duplicate_policy = duplicate_policy, interval_closure = "closed",
    valid_isi_mask_alignment = "right_spike_isi_row",
    boundary_semantics = paste0(
      "segment_change_or_explicit_hard_boundary_or_nonfinite_gap_creates_",
      "acquisition_block; every invalid_ISI starts_an_analysis_block"
    ),
    min_valid_isi_sec = min_valid_isi_sec, raw_row_count = as.integer(n_raw),
    normalized_spike_count = as.integer(n_spikes),
    normalized_analysis_block_count = as.integer(
      length(unique(analysis_block_id))
    ),
    rejected_raw_row_count = as.integer(sum(!finite)),
    collapsed_duplicate_row_count = as.integer(
      sum(map_status == "collapsed_duplicate")
    ),
    fit_eligible = as.logical(all(finite)),
    input_provenance = input_provenance
  )
  out <- structure(
    list(metadata = metadata, spikes = spikes, isis = isis,
         index_map = index_map),
    class = c("stpd_normalized_train_view_v1", "list")
  )
  stpd_threshold_first_validate_train_view(out)
  out
}

stpd_threshold_first_train_view_columns <- function() {
  lapply(stpd_threshold_first_train_view_column_types(), names)
}

stpd_threshold_first_train_view_column_types <- function() {
  list(
    spikes = c(
      train_key = "character", normalized_spike_index = "integer",
      normalized_segment_spike_index = "integer",
      normalized_analysis_block_spike_index = "integer",
      timestamp_sec = "double", segment_id = "character",
      acquisition_block_id = "character", analysis_block_id = "character",
      representative_raw_row_index = "integer",
      hard_boundary_before = "logical", input_valid_isi_mask = "logical"
    ),
    isis = c(
      train_key = "character", normalized_isi_index = "integer",
      left_spike_index = "integer", right_spike_index = "integer",
      segment_id = "character", acquisition_block_id = "character",
      analysis_block_id = "character", ISI_sec = "double",
      input_valid_isi_mask = "logical", valid_isi = "logical",
      invalid_reason = "character", hard_boundary_before = "logical"
    ),
    index_map = c(
      raw_row_index = "integer", source_row_key = "character",
      segment_id = "character", timestamp_sec = "double",
      hard_boundary_before = "logical", valid_isi_mask = "logical",
      acquisition_block_id = "character",
      normalized_spike_index = "integer", analysis_block_id = "character",
      mapping_status = "character", collapse_group_id = "character"
    )
  )
}

stpd_threshold_first_validate_train_view <- function(view) {
  if (!is.list(view) ||
      !identical(names(view), c("metadata", "spikes", "isis", "index_map"))) {
    stpd_threshold_first_abort(
      "train_view_invalid",
      "Normalized train view must contain the exact ordered components."
    )
  }
  metadata <- view$metadata
  metadata_fields <- c(
    "schema_version", "label_free_schema_version", "train_key",
    "canonical_time_unit", "input_time_unit", "timestamp_order_policy",
    "timestamp_authority", "duplicate_policy", "interval_closure",
    "valid_isi_mask_alignment", "boundary_semantics", "min_valid_isi_sec",
    "raw_row_count", "normalized_spike_count",
    "normalized_analysis_block_count", "rejected_raw_row_count",
    "collapsed_duplicate_row_count", "fit_eligible", "input_provenance"
  )
  if (!is.list(metadata) || !identical(names(metadata), metadata_fields) ||
      !identical(metadata$schema_version,
                 STPD_THRESHOLD_FIRST_TRAIN_VIEW_VERSION) ||
      !identical(metadata$label_free_schema_version,
                 STPD_THRESHOLD_FIRST_LABEL_FREE_VERSION)) {
    stpd_threshold_first_abort(
      "train_view_schema_invalid",
      "Normalized train-view metadata does not match the frozen schema."
    )
  }
  stpd_threshold_first_scalar_character(metadata$train_key, "train_key")
  stpd_threshold_first_choice(
    metadata$input_time_unit, c("s", "ms"), "input_time_unit"
  )
  stpd_threshold_first_choice(
    metadata$duplicate_policy, c("error", "collapse_exact"),
    "duplicate_policy"
  )
  if (!identical(metadata$canonical_time_unit, "s") ||
      !identical(metadata$timestamp_order_policy,
                 "stable_sort_within_acquisition_block") ||
      !identical(metadata$timestamp_authority,
                 "timestamp_sec_recompute_isi") ||
      !identical(metadata$interval_closure, "closed") ||
      !identical(metadata$valid_isi_mask_alignment,
                 "right_spike_isi_row") ||
      !identical(
        metadata$boundary_semantics,
        paste0(
          "segment_change_or_explicit_hard_boundary_or_nonfinite_gap_creates_",
          "acquisition_block; every invalid_ISI starts_an_analysis_block"
        )
      )) {
    stpd_threshold_first_abort(
      "train_view_schema_invalid",
      "Normalized train-view policy metadata is invalid."
    )
  }
  if (!is.double(metadata$min_valid_isi_sec) ||
      length(metadata$min_valid_isi_sec) != 1L ||
      !is.null(attributes(metadata$min_valid_isi_sec)) ||
      !is.finite(metadata$min_valid_isi_sec) ||
      metadata$min_valid_isi_sec <= 0) {
    stpd_threshold_first_abort(
      "train_view_schema_invalid", "min_valid_isi_sec is invalid."
    )
  }
  integer_metadata <- c(
    "raw_row_count", "normalized_spike_count",
    "normalized_analysis_block_count", "rejected_raw_row_count",
    "collapsed_duplicate_row_count"
  )
  if (any(vapply(integer_metadata, function(field) {
    value <- metadata[[field]]
    !is.integer(value) || length(value) != 1L || is.na(value) || value < 0L
  }, logical(1)))) {
    stpd_threshold_first_abort(
      "train_view_schema_invalid", "Train-view count metadata is invalid."
    )
  }
  stpd_threshold_first_scalar_logical(metadata$fit_eligible, "fit_eligible")
  stpd_threshold_first_validate_input_provenance(metadata$input_provenance)

  types <- stpd_threshold_first_train_view_column_types()
  for (table in names(types)) {
    value <- view[[table]]
    if (!is.data.frame(value) || !identical(names(value), names(types[[table]]))) {
      stpd_threshold_first_abort(
        "train_view_schema_invalid",
        paste0(table, " does not match the frozen column schema."), field = table
      )
    }
    for (column in names(types[[table]])) {
      if (!is.atomic(value[[column]]) ||
          length(value[[column]]) != nrow(value) ||
          !is.null(attributes(value[[column]])) ||
          !identical(typeof(value[[column]]), unname(types[[table]][[column]]))) {
        stpd_threshold_first_abort(
          "train_view_schema_invalid",
          paste0(table, "$", column, " has the wrong exact type."), field = table
        )
      }
    }
  }
  spikes <- view$spikes; isis <- view$isis; index_map <- view$index_map
  n <- nrow(spikes)
  if (n < 1L || nrow(isis) != n ||
      nrow(index_map) != metadata$raw_row_count ||
      n != metadata$normalized_spike_count ||
      !identical(spikes$normalized_spike_index, as.integer(seq_len(n))) ||
      !identical(isis$normalized_isi_index, as.integer(seq_len(n))) ||
      !identical(isis$right_spike_index, as.integer(seq_len(n)))) {
    stpd_threshold_first_abort(
      "coordinate_invalid", "Normalized spike/ISI coordinates are inconsistent."
    )
  }
  if (any(!is.finite(spikes$timestamp_sec)) || anyNA(spikes$segment_id) ||
      any(!nzchar(spikes$segment_id)) || anyNA(spikes$acquisition_block_id) ||
      any(!nzchar(spikes$acquisition_block_id)) ||
      anyNA(spikes$analysis_block_id) ||
      any(!nzchar(spikes$analysis_block_id)) ||
      anyNA(spikes$hard_boundary_before) ||
      anyNA(spikes$input_valid_isi_mask)) {
    stpd_threshold_first_abort(
      "timestamp_invalid",
      "Normalized spikes require finite timestamps and complete block fields."
    )
  }
  if (any(spikes$train_key != metadata$train_key) ||
      any(isis$train_key != metadata$train_key) ||
      !identical(isis$segment_id, spikes$segment_id) ||
      !identical(isis$acquisition_block_id, spikes$acquisition_block_id) ||
      !identical(isis$analysis_block_id, spikes$analysis_block_id) ||
      !identical(isis$hard_boundary_before, spikes$hard_boundary_before) ||
      !identical(isis$input_valid_isi_mask, spikes$input_valid_isi_mask)) {
    stpd_threshold_first_abort(
      "train_view_schema_invalid",
      "Spike and ISI rows disagree on train, segment, block, boundary, or mask."
    )
  }
  contiguous_runs <- function(x) {
    starts <- c(TRUE, x[-1L] != x[-length(x)])
    x[starts]
  }
  if (anyDuplicated(contiguous_runs(spikes$segment_id)) ||
      anyDuplicated(contiguous_runs(spikes$acquisition_block_id)) ||
      anyDuplicated(contiguous_runs(spikes$analysis_block_id))) {
    stpd_threshold_first_abort(
      "boundary_invalid", "Segment and block identifiers cannot re-enter."
    )
  }
  expected_segment_index <- as.integer(ave(
    seq_len(n), spikes$segment_id, FUN = seq_along
  ))
  expected_analysis_index <- as.integer(ave(
    seq_len(n), spikes$analysis_block_id, FUN = seq_along
  ))
  if (!identical(spikes$normalized_segment_spike_index,
                 expected_segment_index) ||
      !identical(spikes$normalized_analysis_block_spike_index,
                 expected_analysis_index)) {
    stpd_threshold_first_abort(
      "coordinate_invalid", "Within-segment or within-block indices are invalid."
    )
  }
  first_in_acquisition_block <- !duplicated(spikes$acquisition_block_id)
  analysis_block_start <- c(
    TRUE,
    spikes$analysis_block_id[-1L] !=
      spikes$analysis_block_id[-length(spikes$analysis_block_id)]
  )
  expected_analysis_start <- spikes$hard_boundary_before | !isis$valid_isi
  expected_acquisition_id <- sprintf(
    "acquisition_%06d", cumsum(spikes$hard_boundary_before)
  )
  expected_analysis_id <- sprintf(
    "analysis_%06d", cumsum(expected_analysis_start)
  )
  if (any(!spikes$hard_boundary_before[first_in_acquisition_block]) ||
      !identical(spikes$acquisition_block_id, expected_acquisition_id) ||
      !identical(spikes$analysis_block_id, expected_analysis_id) ||
      !identical(analysis_block_start, expected_analysis_start) ||
      any(isis$valid_isi[isis$hard_boundary_before]) ||
      length(unique(spikes$analysis_block_id)) !=
        metadata$normalized_analysis_block_count) {
    stpd_threshold_first_abort(
      "boundary_invalid",
      "Acquisition/analysis blocks do not close over boundaries and invalid ISIs."
    )
  }
  tolerance <- stpd_threshold_first_numeric_contract()$floating_tolerance
  for (i in seq_len(n)) {
    if (isis$hard_boundary_before[[i]]) {
      if (!is.na(isis$left_spike_index[[i]]) || !is.na(isis$ISI_sec[[i]])) {
        stpd_threshold_first_abort(
          "boundary_invalid", "Hard-boundary ISI rows must have no left endpoint or ISI."
        )
      }
    } else {
      if (i <= 1L || !identical(isis$left_spike_index[[i]], i - 1L) ||
          !identical(spikes$segment_id[[i]], spikes$segment_id[[i - 1L]]) ||
          !identical(spikes$acquisition_block_id[[i]],
                     spikes$acquisition_block_id[[i - 1L]])) {
        stpd_threshold_first_abort(
          "coordinate_invalid", "Non-boundary ISI endpoints are inconsistent."
        )
      }
      expected <- spikes$timestamp_sec[[i]] - spikes$timestamp_sec[[i - 1L]]
      allowed <- tolerance * max(1, abs(expected))
      if (!is.finite(expected) || expected <= 0 ||
          !is.finite(isis$ISI_sec[[i]]) ||
          abs(isis$ISI_sec[[i]] - expected) > allowed) {
        stpd_threshold_first_abort(
          "timestamp_non_monotonic",
          "Timestamps must be strictly increasing within each normalized segment."
        )
      }
    }
  }
  expected_valid <- !isis$hard_boundary_before & isis$input_valid_isi_mask &
    is.finite(isis$ISI_sec) &
    isis$ISI_sec >= metadata$min_valid_isi_sec
  expected_reason <- rep("", n)
  expected_reason[isis$hard_boundary_before] <- "hard_boundary"
  expected_reason[!isis$hard_boundary_before &
                    !isis$input_valid_isi_mask] <- "caller_masked"
  expected_reason[!isis$hard_boundary_before &
                    isis$input_valid_isi_mask &
                    is.finite(isis$ISI_sec) &
                    isis$ISI_sec < metadata$min_valid_isi_sec] <-
    "below_min_valid_isi"
  expected_reason[!isis$hard_boundary_before &
                    isis$input_valid_isi_mask &
                    !is.finite(isis$ISI_sec)] <- "nonfinite_recomputed_isi"
  if (!identical(isis$valid_isi, expected_valid) ||
      !identical(isis$invalid_reason, expected_reason)) {
    stpd_threshold_first_abort(
      "validity_closure_invalid",
      "valid_isi and invalid_reason do not match the frozen closure."
    )
  }

  if (!identical(index_map$raw_row_index,
                 as.integer(seq_len(nrow(index_map)))) ||
      anyNA(index_map$source_row_key) ||
      any(!nzchar(index_map$source_row_key)) ||
      anyDuplicated(index_map$source_row_key) ||
      anyNA(index_map$segment_id) || any(!nzchar(index_map$segment_id)) ||
      anyNA(index_map$hard_boundary_before) ||
      anyNA(index_map$valid_isi_mask)) {
    stpd_threshold_first_abort(
      "index_map_invalid", "Raw index-map coordinates are invalid."
    )
  }
  allowed_status <- c(
    "retained", "collapsed_duplicate", "dropped_nonfinite_timestamp"
  )
  if (anyNA(index_map$mapping_status) ||
      any(!(index_map$mapping_status %in% allowed_status))) {
    stpd_threshold_first_abort(
      "index_map_invalid", "index_map contains an unknown mapping_status."
    )
  }
  dropped <- index_map$mapping_status == "dropped_nonfinite_timestamp"
  if (any(is.finite(index_map$timestamp_sec[dropped])) ||
      any(!is.na(index_map$normalized_spike_index[dropped])) ||
      any(!is.na(index_map$acquisition_block_id[dropped])) ||
      any(!is.na(index_map$analysis_block_id[dropped])) ||
      any(!is.na(index_map$collapse_group_id[dropped])) ||
      any(!is.finite(index_map$timestamp_sec[!dropped]))) {
    stpd_threshold_first_abort(
      "index_map_invalid",
      "Dropped and mapped timestamp rows do not satisfy the mapping closure."
    )
  }
  raw_segment_start <- c(
    TRUE,
    index_map$segment_id[-1L] !=
      index_map$segment_id[-length(index_map$segment_id)]
  )
  if (anyDuplicated(index_map$segment_id[raw_segment_start])) {
    stpd_threshold_first_abort(
      "boundary_invalid", "Raw segment identifiers cannot leave and re-enter."
    )
  }
  expected_raw_acquisition_number <- rep(NA_integer_, nrow(index_map))
  raw_block_number <- 0L
  for (i in seq_len(nrow(index_map))) {
    if (dropped[[i]]) next
    begins <- i == 1L || dropped[[i - 1L]] ||
      !identical(index_map$segment_id[[i]], index_map$segment_id[[i - 1L]]) ||
      isTRUE(index_map$hard_boundary_before[[i]])
    if (begins) raw_block_number <- raw_block_number + 1L
    expected_raw_acquisition_number[[i]] <- raw_block_number
  }
  expected_raw_acquisition_id <- ifelse(
    is.na(expected_raw_acquisition_number), NA_character_,
    sprintf("acquisition_%06d", expected_raw_acquisition_number)
  )
  if (!identical(
    index_map$acquisition_block_id, expected_raw_acquisition_id
  )) {
    stpd_threshold_first_abort(
      "boundary_invalid",
      paste0(
        "Acquisition blocks must be reconstructed from raw segment changes, ",
        "explicit boundaries, and dropped nonfinite gaps."
      )
    )
  }
  if (anyNA(index_map$acquisition_block_id[!dropped]) ||
      any(!nzchar(index_map$acquisition_block_id[!dropped])) ||
      anyNA(index_map$analysis_block_id[!dropped]) ||
      any(!nzchar(index_map$analysis_block_id[!dropped])) ||
      anyNA(index_map$collapse_group_id[!dropped]) ||
      any(!nzchar(index_map$collapse_group_id[!dropped]))) {
    stpd_threshold_first_abort(
      "index_map_invalid", "Mapped rows require complete block and collapse IDs."
    )
  }
  mapped <- index_map$normalized_spike_index[!dropped]
  if (anyNA(mapped) || any(mapped < 1L | mapped > n)) {
    stpd_threshold_first_abort(
      "coordinate_invalid", "index_map points outside normalized spikes."
    )
  }
  for (i in seq_len(n)) {
    rows <- which(index_map$normalized_spike_index == i)
    retained_rows <- rows[index_map$mapping_status[rows] == "retained"]
    if (!length(rows) || length(retained_rows) != 1L ||
        !identical(retained_rows[[1L]],
                   spikes$representative_raw_row_index[[i]]) ||
        any(index_map$mapping_status[setdiff(rows, retained_rows)] !=
              "collapsed_duplicate") ||
        !identical(index_map$timestamp_sec[rows],
                   rep(spikes$timestamp_sec[[i]], length(rows))) ||
        any(index_map$segment_id[rows] != spikes$segment_id[[i]]) ||
        any(index_map$acquisition_block_id[rows] !=
              spikes$acquisition_block_id[[i]]) ||
        any(index_map$analysis_block_id[rows] !=
              spikes$analysis_block_id[[i]]) ||
        !identical(index_map$collapse_group_id[rows],
                   rep(sprintf("collapse_%06d", i), length(rows))) ||
        !identical(all(index_map$valid_isi_mask[rows]),
                   spikes$input_valid_isi_mask[[i]]) ||
        !identical(
          isTRUE(first_in_acquisition_block[[i]]),
          spikes$hard_boundary_before[[i]]
        )) {
      stpd_threshold_first_abort(
        "index_map_invalid",
        "A normalized spike does not have a closed retained/collapsed mapping."
      )
    }
  }
  if (metadata$rejected_raw_row_count != sum(dropped) ||
      metadata$collapsed_duplicate_row_count !=
        sum(index_map$mapping_status == "collapsed_duplicate") ||
      (identical(metadata$duplicate_policy, "error") &&
       any(index_map$mapping_status == "collapsed_duplicate")) ||
      !identical(metadata$fit_eligible, !any(dropped))) {
    stpd_threshold_first_abort(
      "train_view_schema_invalid",
      "Train-view count or fit-eligibility metadata does not match index_map."
    )
  }
  invisible(TRUE)
}

stpd_threshold_first_label_free_view <- function(view) {
  stpd_threshold_first_validate_train_view(view)
  if (!identical(view$metadata$input_provenance$information_access,
                 "label_blind")) {
    stpd_threshold_first_abort(
      "label_blind_provenance_required",
      "A label-free train view requires declared and validated label-blind provenance."
    )
  }
  if (!isTRUE(view$metadata$fit_eligible)) {
    stpd_threshold_first_abort(
      "train_view_fit_ineligible",
      "A train containing dropped nonfinite timestamps cannot enter scientific fitting."
    )
  }
  columns <- stpd_threshold_first_train_view_columns()
  metadata_fields <- c(
    "schema_version", "label_free_schema_version", "train_key",
    "canonical_time_unit", "input_time_unit", "timestamp_order_policy",
    "timestamp_authority", "duplicate_policy", "interval_closure",
    "valid_isi_mask_alignment", "boundary_semantics", "min_valid_isi_sec",
    "raw_row_count", "normalized_spike_count",
    "normalized_analysis_block_count", "rejected_raw_row_count",
    "collapsed_duplicate_row_count", "fit_eligible", "input_provenance"
  )
  structure(
    list(
      metadata = view$metadata[metadata_fields],
      spikes = as.data.frame(view$spikes[columns$spikes], stringsAsFactors = FALSE),
      isis = as.data.frame(view$isis[columns$isis], stringsAsFactors = FALSE),
      index_map = as.data.frame(
        view$index_map[columns$index_map], stringsAsFactors = FALSE
      )
    ),
    class = c("stpd_label_free_train_view_v1", "list")
  )
}

stpd_threshold_first_scientific_train_projection <- function(view) {
  label_free <- stpd_threshold_first_label_free_view(view)
  structure(
    list(
      metadata = label_free$metadata[c(
        "schema_version", "label_free_schema_version", "train_key",
        "canonical_time_unit", "timestamp_order_policy", "timestamp_authority",
        "duplicate_policy", "interval_closure", "valid_isi_mask_alignment",
        "boundary_semantics", "min_valid_isi_sec", "normalized_spike_count",
        "normalized_analysis_block_count", "input_provenance"
      )],
      spikes = label_free$spikes[c(
        "train_key", "normalized_spike_index",
        "normalized_segment_spike_index",
        "normalized_analysis_block_spike_index", "timestamp_sec", "segment_id",
        "acquisition_block_id", "analysis_block_id", "hard_boundary_before",
        "input_valid_isi_mask"
      )],
      isis = label_free$isis[c(
        "train_key", "normalized_isi_index", "left_spike_index",
        "right_spike_index", "segment_id", "acquisition_block_id",
        "analysis_block_id", "ISI_sec", "input_valid_isi_mask", "valid_isi",
        "hard_boundary_before"
      )]
    ),
    class = c("stpd_threshold_first_scientific_train_projection_v1", "list")
  )
}

stpd_threshold_first_atomic_payload <- function(x) {
  encode_double <- function(value) {
    if (is.nan(value)) return(list(kind = "NaN"))
    if (is.na(value)) return(list(kind = "NA"))
    if (is.infinite(value)) return(list(kind = if (value > 0) "Inf" else "-Inf"))
    list(kind = "value", value = sprintf("%a", value))
  }
  encode_character <- function(value) {
    if (is.na(value)) list(kind = "NA") else
      list(kind = "value", value = enc2utf8(value))
  }
  encode_logical <- function(value) {
    if (is.na(value)) list(kind = "NA") else
      list(kind = "value", value = if (isTRUE(value)) "TRUE" else "FALSE")
  }
  encode_integer <- function(value) {
    if (is.na(value)) list(kind = "NA") else
      list(kind = "value", value = as.character(value))
  }
  nms <- names(x)
  if (!is.null(nms)) {
    if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
      stpd_threshold_first_abort(
        "hash_names_invalid",
        "Named atomic vectors in canonical hashes require unique non-empty names."
      )
    }
    ordering <- order(nms, method = "radix")
    x <- x[ordering]
    nms <- nms[ordering]
  }
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "POSIXt")) x <- format(x, tz = "UTC", usetz = TRUE)
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  if (is.double(x)) {
    values <- lapply(x, encode_double); type <- "double"
  } else if (is.integer(x)) {
    values <- lapply(x, encode_integer); type <- "integer"
  } else if (is.logical(x)) {
    values <- lapply(x, encode_logical); type <- "logical"
  } else if (is.character(x)) {
    values <- lapply(x, encode_character); type <- "character"
  } else if (is.raw(x)) {
    values <- as.list(sprintf("%02x", as.integer(x))); type <- "raw"
  } else if (is.null(x)) {
    return(list(`__stpd_type__` = "NULL"))
  } else {
    stpd_threshold_first_abort(
      "hash_type_unsupported",
      paste0("Unsupported type in canonical hash: ", paste(class(x), collapse = "/"), ".")
    )
  }
  list(
    `__stpd_type__` = type,
    names = if (is.null(nms)) NULL else as.character(nms), values = values
  )
}

stpd_threshold_first_canonical_payload <- function(x) {
  if (is.data.frame(x)) {
    columns <- sort(names(x), method = "radix")
    values <- lapply(columns, function(column) {
      stpd_threshold_first_canonical_payload(unname(x[[column]]))
    })
    names(values) <- columns
    return(list(
      `__stpd_type__` = "data.frame", nrow = as.character(nrow(x)), columns = values
    ))
  }
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) {
      return(list(
        `__stpd_type__` = "list",
        values = lapply(x, stpd_threshold_first_canonical_payload)
      ))
    }
    if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
      stpd_threshold_first_abort(
        "hash_names_invalid",
        "Named lists in canonical hashes require unique non-empty names."
      )
    }
    order_names <- order(nms, method = "radix")
    values <- lapply(x[order_names], stpd_threshold_first_canonical_payload)
    names(values) <- nms[order_names]
    return(list(`__stpd_type__` = "named_list", values = values))
  }
  stpd_threshold_first_atomic_payload(x)
}

stpd_threshold_first_hash_domain <- function(domain, payload) {
  domain <- stpd_threshold_first_scalar_character(domain, "hash_domain")
  canonical <- stpd_threshold_first_canonical_payload(payload)
  json <- jsonlite::toJSON(
    canonical, auto_unbox = TRUE, null = "null", na = "null",
    digits = NA, pretty = FALSE
  )
  bytes <- c(
    charToRaw(enc2utf8(domain)), as.raw(0L),
    charToRaw(enc2utf8(as.character(json)))
  )
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

stpd_threshold_first_train_view_hash <- function(view) {
  scientific <- stpd_threshold_first_scientific_train_projection(view)
  stpd_threshold_first_hash_domain(
    "stpd-threshold-first-scientific-train-input-v1", unclass(scientific)
  )
}

stpd_threshold_first_train_view_audit_hash <- function(view) {
  stpd_threshold_first_validate_train_view(view)
  stpd_threshold_first_hash_domain(
    "stpd-threshold-first-full-train-audit-view-v1", unclass(view)
  )
}

stpd_threshold_first_sha256_or_null <- function(x, field) {
  if (is.null(x)) return(NULL)
  x <- stpd_threshold_first_scalar_character(x, field)
  if (!grepl("^[0-9a-f]{64}$", x)) {
    stpd_threshold_first_abort(
      "sha256_invalid", paste0(field, " must be a lowercase SHA-256 value."),
      field = field
    )
  }
  x
}

stpd_threshold_first_sha256_required <- function(x, field) {
  value <- stpd_threshold_first_sha256_or_null(x, field)
  if (is.null(value)) {
    stpd_threshold_first_abort(
      "required_field_missing", paste0(field, " is required."), field = field
    )
  }
  value
}

stpd_threshold_first_validate_parameter_payload <- function(
    x, path = "effective_params") {
  extra_attributes <- setdiff(names(attributes(x)), "names")
  if (length(extra_attributes)) {
    stpd_threshold_first_abort(
      "scientific_params_invalid",
      paste0(
        path, " contains unsupported semantic attributes: ",
        paste(extra_attributes, collapse = ", "),
        ". Encode units or dimensions as explicit named fields."
      ),
      field = path
    )
  }
  if (is.list(x)) {
    nms <- names(x)
    labels <- if (is.null(nms)) as.character(seq_along(x)) else nms
    for (i in seq_along(x)) {
      stpd_threshold_first_validate_parameter_payload(
        x[[i]], paste0(path, "[[", labels[[i]], "]]" )
      )
    }
    return(invisible(TRUE))
  }
  if (!(is.null(x) || is.double(x) || is.integer(x) || is.logical(x) ||
        is.character(x) || is.raw(x))) {
    stpd_threshold_first_abort(
      "scientific_params_invalid",
      paste0(path, " has unsupported type ", typeof(x), "."),
      field = path
    )
  }
  invisible(TRUE)
}

stpd_threshold_first_scientific_params <- function(effective_params) {
  if (!is.list(effective_params) || is.null(names(effective_params)) ||
      !length(effective_params) || anyNA(names(effective_params)) ||
      any(!nzchar(names(effective_params))) ||
      anyDuplicated(names(effective_params))) {
    stpd_threshold_first_abort(
      "scientific_params_invalid",
      "effective_params must be a non-empty named list with unique fields."
    )
  }
  stpd_threshold_first_validate_parameter_payload(effective_params)
  active_fields <- sort(names(effective_params), method = "radix")
  effective_hash <- stpd_threshold_first_hash_domain(
    "stpd-threshold-first-effective-scientific-params-v1", effective_params
  )
  numeric_hash <- stpd_threshold_first_hash_domain(
    "stpd-threshold-first-numeric-contract-v1",
    stpd_threshold_first_numeric_contract()
  )
  structure(
    list(
      schema_version = STPD_THRESHOLD_FIRST_SCIENTIFIC_PARAMS_VERSION,
      payload_role = "caller_supplied_contract_payload",
      completeness_status = "unverified_until_engine_parameter_resolver_wiring",
      active_top_level_fields = active_fields,
      effective_params = effective_params,
      effective_params_sha256 = effective_hash,
      numeric_contract_sha256 = numeric_hash
    ),
    class = c("stpd_threshold_first_scientific_params_v1", "list")
  )
}

stpd_threshold_first_validate_scientific_params <- function(x) {
  required <- c(
    "schema_version", "payload_role", "completeness_status",
    "active_top_level_fields", "effective_params",
    "effective_params_sha256", "numeric_contract_sha256"
  )
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
      !identical(names(x), required) ||
      !identical(x$schema_version,
                 STPD_THRESHOLD_FIRST_SCIENTIFIC_PARAMS_VERSION) ||
      !identical(x$payload_role, "caller_supplied_contract_payload") ||
      !identical(
        x$completeness_status,
        "unverified_until_engine_parameter_resolver_wiring"
      )) {
    stpd_threshold_first_abort(
      "scientific_params_invalid",
      "Scientific parameters do not match the frozen versioned schema."
    )
  }
  rebuilt <- stpd_threshold_first_scientific_params(x$effective_params)
  if (!identical(unclass(x), unclass(rebuilt))) {
    stpd_threshold_first_abort(
      "scientific_params_invalid",
      "Scientific parameter fields or hashes are incomplete or inconsistent."
    )
  }
  invisible(TRUE)
}

stpd_threshold_first_provider_identity <- function(
    provider_id, provider_version, provider_state_sha256,
    fold_manifest_sha256, train_view_sha256,
    information_access = "label_blind",
    performance_use = "label_blind_detector_performance",
    group_separation_status = "not_required",
    labels_used = FALSE, reference_annotations_used = FALSE,
    calibration_group_ids = character(), evaluation_group_ids) {
  provider_id <- stpd_threshold_first_scalar_character(provider_id, "provider_id")
  provider_version <- stpd_threshold_first_scalar_character(
    provider_version, "provider_version"
  )
  provider_state_sha256 <- stpd_threshold_first_sha256_required(
    provider_state_sha256, "provider_state_sha256"
  )
  fold_manifest_sha256 <- stpd_threshold_first_sha256_required(
    fold_manifest_sha256, "fold_manifest_sha256"
  )
  train_view_sha256 <- stpd_threshold_first_sha256_required(
    train_view_sha256, "train_view_sha256"
  )
  information_access <- stpd_threshold_first_choice(
    information_access, c("label_blind", "manual_aware", "unknown"),
    "information_access"
  )
  performance_use <- stpd_threshold_first_choice(
    performance_use,
    c(
      "label_blind_detector_performance",
      "heldout_detector_performance_only",
      "adjudicated_agreement_only", "not_eligible"
    ),
    "performance_use"
  )
  group_separation_status <- stpd_threshold_first_choice(
    group_separation_status,
    c(
      "not_required", "verified_disjoint", "overlap_rejected",
      "unverifiable_rejected"
    ),
    "group_separation_status"
  )
  labels_used <- stpd_threshold_first_scalar_logical(labels_used, "labels_used")
  reference_annotations_used <- stpd_threshold_first_scalar_logical(
    reference_annotations_used, "reference_annotations_used"
  )
  normalize_groups <- function(x, field, allow_empty) {
    if (!is.character(x) || anyNA(x) || any(!nzchar(x)) ||
        anyDuplicated(x) || (!allow_empty && !length(x))) {
      stpd_threshold_first_abort(
        "provider_identity_invalid",
        paste0(field, " must contain unique non-empty group identifiers."),
        field = field
      )
    }
    sort(enc2utf8(x), method = "radix")
  }
  calibration_group_ids <- normalize_groups(
    calibration_group_ids, "calibration_group_ids", allow_empty = TRUE
  )
  evaluation_group_ids <- normalize_groups(
    evaluation_group_ids, "evaluation_group_ids", allow_empty = FALSE
  )
  overlap <- intersect(calibration_group_ids, evaluation_group_ids)
  if (identical(information_access, "label_blind") &&
      (labels_used || reference_annotations_used)) {
    stpd_threshold_first_abort(
      "provider_identity_invalid",
      "label_blind information access cannot declare label or reference use."
    )
  }
  if (identical(group_separation_status, "verified_disjoint") &&
      (!length(calibration_group_ids) || length(overlap))) {
    stpd_threshold_first_abort(
      "provider_identity_invalid",
      "verified_disjoint requires non-empty calibration groups and zero overlap."
    )
  }
  if (identical(group_separation_status, "not_required") &&
      length(calibration_group_ids)) {
    stpd_threshold_first_abort(
      "provider_identity_invalid",
      "not_required requires an explicit empty calibration group set."
    )
  }
  if (identical(group_separation_status, "overlap_rejected") &&
      !length(overlap)) {
    stpd_threshold_first_abort(
      "provider_identity_invalid",
      "overlap_rejected requires a non-zero observed group overlap."
    )
  }
  if (identical(performance_use, "label_blind_detector_performance") &&
      (!identical(information_access, "label_blind") || labels_used ||
       reference_annotations_used || length(overlap) ||
       !(group_separation_status %in%
           c("not_required", "verified_disjoint")))) {
    stpd_threshold_first_abort(
      "provider_performance_ineligible",
      "Fully automatic performance requires label-blind, label-free, non-overlapping provenance."
    )
  }
  if (identical(performance_use, "heldout_detector_performance_only") &&
      (!identical(group_separation_status, "verified_disjoint") ||
       length(overlap))) {
    stpd_threshold_first_abort(
      "provider_performance_ineligible",
      "Held-out provider-assisted performance requires verified disjoint groups."
    )
  }
  if ((identical(information_access, "unknown") ||
       group_separation_status %in%
         c("overlap_rejected", "unverifiable_rejected")) &&
      !(performance_use %in%
          c("adjudicated_agreement_only", "not_eligible"))) {
    stpd_threshold_first_abort(
      "provider_performance_ineligible",
      "Unknown or rejected separation provenance is not detector-performance eligible."
    )
  }
  structure(
    list(
      schema_version = STPD_THRESHOLD_FIRST_PROVIDER_IDENTITY_VERSION,
      provider_contract_sha256 =
        STPD_THRESHOLD_FIRST_FROZEN_PROVIDER_CONTRACT_SHA256,
      provider_id = provider_id, provider_version = provider_version,
      provider_state_sha256 = provider_state_sha256,
      fold_manifest_sha256 = fold_manifest_sha256,
      train_view_sha256 = train_view_sha256,
      information_access = information_access,
      performance_use = performance_use,
      group_separation_status = group_separation_status,
      labels_used = labels_used,
      reference_annotations_used = reference_annotations_used,
      calibration_group_ids = calibration_group_ids,
      evaluation_group_ids = evaluation_group_ids,
      calibration_groups_sha256 = stpd_threshold_first_hash_domain(
        "stpd-threshold-first-calibration-groups-v1", calibration_group_ids
      ),
      evaluation_groups_sha256 = stpd_threshold_first_hash_domain(
        "stpd-threshold-first-evaluation-groups-v1", evaluation_group_ids
      ),
      group_overlap_count = as.integer(length(overlap))
    ),
    class = c("stpd_threshold_first_provider_identity_v1", "list")
  )
}

stpd_threshold_first_validate_provider_identity <- function(x) {
  required <- c(
    "schema_version", "provider_contract_sha256", "provider_id",
    "provider_version", "provider_state_sha256", "fold_manifest_sha256",
    "train_view_sha256", "information_access", "performance_use",
    "group_separation_status", "labels_used", "reference_annotations_used",
    "calibration_group_ids", "evaluation_group_ids",
    "calibration_groups_sha256", "evaluation_groups_sha256",
    "group_overlap_count"
  )
  if (!is.list(x) || !identical(names(x), required) ||
      !identical(x$schema_version,
                 STPD_THRESHOLD_FIRST_PROVIDER_IDENTITY_VERSION) ||
      !identical(x$provider_contract_sha256,
                 STPD_THRESHOLD_FIRST_FROZEN_PROVIDER_CONTRACT_SHA256)) {
    stpd_threshold_first_abort(
      "provider_identity_invalid",
      "Provider identity does not match the frozen schema or R82 contract."
    )
  }
  rebuilt <- stpd_threshold_first_provider_identity(
    provider_id = x$provider_id, provider_version = x$provider_version,
    provider_state_sha256 = x$provider_state_sha256,
    fold_manifest_sha256 = x$fold_manifest_sha256,
    train_view_sha256 = x$train_view_sha256,
    information_access = x$information_access,
    performance_use = x$performance_use,
    group_separation_status = x$group_separation_status,
    labels_used = x$labels_used,
    reference_annotations_used = x$reference_annotations_used,
    calibration_group_ids = x$calibration_group_ids,
    evaluation_group_ids = x$evaluation_group_ids
  )
  if (!identical(unclass(x), unclass(rebuilt))) {
    stpd_threshold_first_abort(
      "provider_identity_invalid",
      "Provider identity group proof or digest is inconsistent."
    )
  }
  invisible(TRUE)
}

stpd_threshold_first_scientific_hash <- function(
    train_view, run_config, scientific_params, provider_identity = NULL,
    rng_kind = "deterministic_no_rng", rng_seed = NULL) {
  config <- stpd_threshold_first_validate_run_config(run_config)
  train_view_sha256 <- stpd_threshold_first_train_view_hash(train_view)
  stpd_threshold_first_validate_scientific_params(scientific_params)
  if (!identical(config$manual_policy, "ignore")) {
    stpd_threshold_first_abort(
      "label_blind_manual_policy_conflict",
      paste0(
        "Scientific hashing uses a label-blind train view and therefore ",
        "requires manual_policy='ignore'."
      )
    )
  }
  if (identical(config$threshold_source, "provider") &&
      is.null(provider_identity)) {
    stpd_threshold_first_abort(
      "provider_state_required",
      "Provider threshold source requires a complete provider identity."
    )
  }
  if (!is.null(provider_identity)) {
    stpd_threshold_first_validate_provider_identity(provider_identity)
    if (!identical(provider_identity$train_view_sha256, train_view_sha256)) {
      stpd_threshold_first_abort(
        "provider_identity_invalid",
        "Provider identity was not predicted against this scientific train view."
      )
    }
  }
  if (!identical(config$threshold_source, "provider") &&
      !is.null(provider_identity)) {
    stpd_threshold_first_abort(
      "provider_state_unexpected",
      "Provider identity is only valid when threshold_source='provider'."
    )
  }
  rng_kind <- stpd_threshold_first_choice(
    rng_kind,
    c("deterministic_no_rng", "Mersenne-Twister", "L'Ecuyer-CMRG"),
    "rng_kind"
  )
  if (identical(rng_kind, "deterministic_no_rng")) {
    if (!is.null(rng_seed)) {
      stpd_threshold_first_abort(
        "rng_contract_invalid",
        "rng_seed must be NULL when rng_kind='deterministic_no_rng'."
      )
    }
  } else {
    seed_numeric <- stpd_threshold_first_numeric_vector(rng_seed, "rng_seed")
    if (length(seed_numeric) != 1L || !is.finite(seed_numeric) ||
        seed_numeric != round(seed_numeric) || seed_numeric < 0 ||
        seed_numeric > .Machine$integer.max) {
      stpd_threshold_first_abort(
        "rng_contract_invalid", "A finite integer rng_seed is required."
      )
    }
    rng_seed <- as.integer(seed_numeric)
  }
  scientific_config <- config[c(
    "schema_version", "engine_algorithm", "threshold_source", "manual_policy"
  )]
  payload <- list(
    schema_version = STPD_THRESHOLD_FIRST_SCIENTIFIC_HASH_VERSION,
    contract_version = STPD_THRESHOLD_FIRST_CONTRACT_VERSION,
    train_view_sha256 = train_view_sha256,
    run_config = scientific_config, scientific_params = scientific_params,
    provider_identity = provider_identity,
    rng = list(kind = rng_kind, seed = rng_seed)
  )
  stpd_threshold_first_hash_domain(
    "stpd-threshold-first-scientific-projection-v1", payload
  )
}

stpd_threshold_first_provider_interface_contract <- function() {
  list(
    schema_version = STPD_THRESHOLD_FIRST_PROVIDER_INTERFACE_VERSION,
    mapping_version = STPD_THRESHOLD_FIRST_PROVIDER_MAPPING_VERSION,
    frozen_r82_contract_sha256 =
      STPD_THRESHOLD_FIRST_FROZEN_PROVIDER_CONTRACT_SHA256,
    fit_signature = "fit(calibration_view)->frozen_provider_state",
    predict_signature = paste0(
      "predict(label_free_train_view,frozen_provider_state)",
      "->provider_evidence"
    ),
    status = c("resolved", "unsupported", "unresolved", "error"),
    resolved_outcome = c("support", "no_support"),
    nonresolved_outcome = "not_applicable", boundary_closure = "closed",
    required_identity = c(
      "provider_id", "provider_version", "provider_state_sha256",
      "fold_manifest_sha256", "train_view_sha256", "information_access",
      "performance_use", "group_separation_status",
      "calibration_groups_sha256", "evaluation_groups_sha256"
    ),
    required_evidence = c(
      "pattern", "evidence_type", "threshold_direction", "estimate",
      "threshold_stability", "model_adequacy_status", "gray_zone",
      "coverage_status", "explicit_scope_outcome", "candidate_coordinates",
      "fallback_provenance", "deterministic"
    ),
    leakage_rule = "calibration_and_evaluation_groups_must_be_disjoint",
    mapping_granularity = "provider_x_train_x_pattern_x_requested_scope",
    negative_rule = paste0(
      "only status=resolved,outcome=no_support is valid negative evidence; ",
      "unsupported/unresolved/error are never negatives"
    ),
    r82_mapping = data.frame(
      condition = c(
        "complete_positive_normalized_adequate",
        "complete_explicit_negative_complete_coverage_adequate",
        "positive_with_no_scope_coverage", "capability_absent",
        "insufficient_or_partial_evidence",
        "contract_normalization_nondeterminism_or_exception"
      ),
      status = c(
        "resolved", "resolved", "error", "unsupported", "unresolved", "error"
      ),
      outcome = c(
        "support", "no_support", "not_applicable", "not_applicable",
        "not_applicable",
        "not_applicable"
      ),
      stringsAsFactors = FALSE
    )
  )
}

stpd_threshold_first_map_provider_status <- function(
    run_status = "complete",
    provider_decision = "positive",
    capability_supported = TRUE,
    coverage_status = "complete",
    model_adequacy_status = "adequate",
    normalization_status = "normalized",
    deterministic = TRUE, execution_error = FALSE) {
  run_status <- stpd_threshold_first_choice(
    run_status, c("complete", "rejected"), "run_status"
  )
  provider_decision <- stpd_threshold_first_choice(
    provider_decision, c("positive", "negative", "indeterminate"),
    "provider_decision"
  )
  coverage_status <- stpd_threshold_first_choice(
    coverage_status, c("complete", "partial", "none"), "coverage_status"
  )
  model_adequacy_status <- stpd_threshold_first_choice(
    model_adequacy_status, c("adequate", "inadequate", "unresolved"),
    "model_adequacy_status"
  )
  normalization_status <- stpd_threshold_first_choice(
    normalization_status, c("normalized", "rejected"),
    "normalization_status"
  )
  capability_supported <- stpd_threshold_first_scalar_logical(
    capability_supported, "capability_supported"
  )
  deterministic <- stpd_threshold_first_scalar_logical(
    deterministic, "deterministic"
  )
  execution_error <- stpd_threshold_first_scalar_logical(
    execution_error, "execution_error"
  )
  mapped <- if (execution_error ||
                identical(normalization_status, "rejected") ||
                !deterministic) {
    list(status = "error", outcome = "not_applicable")
  } else if (!capability_supported) {
    list(status = "unsupported", outcome = "not_applicable")
  } else if (!identical(model_adequacy_status, "adequate")) {
    list(status = "unresolved", outcome = "not_applicable")
  } else if (identical(run_status, "rejected")) {
    list(status = "error", outcome = "not_applicable")
  } else if (identical(provider_decision, "positive") &&
             identical(coverage_status, "none")) {
    list(status = "error", outcome = "not_applicable")
  } else if (identical(provider_decision, "positive")) {
    list(status = "resolved", outcome = "support")
  } else if (identical(provider_decision, "negative") &&
             identical(coverage_status, "complete")) {
    list(status = "resolved", outcome = "no_support")
  } else {
    list(status = "unresolved", outcome = "not_applicable")
  }
  stpd_threshold_first_validate_provider_status(
    mapped$status, mapped$outcome
  )
}

stpd_threshold_first_validate_provider_status <- function(status, outcome) {
  contract <- stpd_threshold_first_provider_interface_contract()
  status <- stpd_threshold_first_choice(status, contract$status, "status")
  outcome <- stpd_threshold_first_choice(
    outcome, c(contract$resolved_outcome, contract$nonresolved_outcome), "outcome"
  )
  if (identical(status, "resolved") && !(outcome %in% contract$resolved_outcome)) {
    stpd_threshold_first_abort(
      "provider_status_invalid",
      "Resolved provider evidence requires support or no_support outcome."
    )
  }
  if (!identical(status, "resolved") &&
      !identical(outcome, contract$nonresolved_outcome)) {
    stpd_threshold_first_abort(
      "provider_status_invalid",
      "Unsupported, unresolved, and error provider states require not_applicable outcome."
    )
  }
  list(status = status, outcome = outcome)
}

stpd_threshold_first_phase0_manifest <- function() {
  data.frame(
    field = c(
      "contract_version", "run_config_schema", "train_view_schema",
      "label_free_schema", "input_provenance_schema", "numeric_schema",
      "scientific_hash_schema", "scientific_params_schema",
      "provider_interface_schema", "provider_identity_schema",
      "provider_mapping_schema", "legacy_default_engine",
      "legacy_default_threshold_source", "detector_wiring_status",
      "frozen_legacy_scientific_payload_sha256",
      "frozen_provider_contract_sha256",
      "phase0_golden_scientific_train_input_sha256",
      "phase0_golden_scientific_run_sha256"
    ),
    value = c(
      STPD_THRESHOLD_FIRST_CONTRACT_VERSION,
      STPD_THRESHOLD_FIRST_RUN_CONFIG_VERSION,
      STPD_THRESHOLD_FIRST_TRAIN_VIEW_VERSION,
      STPD_THRESHOLD_FIRST_LABEL_FREE_VERSION,
      STPD_THRESHOLD_FIRST_INPUT_PROVENANCE_VERSION,
      STPD_THRESHOLD_FIRST_NUMERIC_VERSION,
      STPD_THRESHOLD_FIRST_SCIENTIFIC_HASH_VERSION,
      STPD_THRESHOLD_FIRST_SCIENTIFIC_PARAMS_VERSION,
      STPD_THRESHOLD_FIRST_PROVIDER_INTERFACE_VERSION,
      STPD_THRESHOLD_FIRST_PROVIDER_IDENTITY_VERSION,
      STPD_THRESHOLD_FIRST_PROVIDER_MAPPING_VERSION,
      "legacy", "ordered_fallback", "dormant_not_connected",
      "9af37b989edc848c9d7a5c9eedc160ae080eed131b53c9846010c402fd23a545",
      STPD_THRESHOLD_FIRST_FROZEN_PROVIDER_CONTRACT_SHA256,
      "62d14090ffebf7663152122436496c4670cbf9271937b962f076cbe7116c4021",
      "8863cc1675888e1bacc389c422f1867981ad02bc88ee8f6b0b2822f739383763"
    ),
    stringsAsFactors = FALSE
  )
}
