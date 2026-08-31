# Parameter registry and validation layer.
# This module is intended to replace hand-written parameter plumbing gradually.
# It keeps the existing detector behavior but makes parameter origin, defaults,
# and reporting explicit and machine-checkable.

stpd_flatten_params <- function(x, prefix = "") {
  rows <- list()
  walk <- function(obj, pref) {
    if (is.list(obj) && !is.data.frame(obj)) {
      nms <- names(obj)
      if (is.null(nms)) nms <- as.character(seq_along(obj))
      for (ii in seq_along(obj)) {
        nm <- nms[ii]
        val <- obj[[ii]]
        path <- if (nzchar(pref)) paste0(pref, ".", nm) else nm
        if (is.list(val) && !is.data.frame(val) && !grepl("adaptive_train_ranges|train_.*ranges|ranges$", path)) {
          walk(val, path)
        } else {
          rows[[length(rows) + 1L]] <<- list(path = path, value = val)
        }
      }
    } else {
      rows[[length(rows) + 1L]] <<- list(path = pref, value = obj)
    }
  }
  walk(x, prefix)
  if (length(rows) == 0) return(data.frame(path = character(), value_string = character(), value_type = character(), stringsAsFactors = FALSE))
  dplyr::bind_rows(lapply(rows, function(r) {
    v <- r$value
    value_type <- if (is.null(v)) "null" else if (is.logical(v)) "logical" else if (is.integer(v)) "integer" else if (is.numeric(v)) "numeric" else if (is.character(v)) "character" else if (is.list(v)) "list" else class(v)[1]
    value_string <- if (is.null(v)) "<NULL>" else if (is.list(v) && !is.data.frame(v)) paste0("<list:", length(v), ">") else paste(as.character(v), collapse = ",")
    data.frame(path = r$path, value_string = value_string, value_type = value_type, stringsAsFactors = FALSE)
  }))
}

stpd_parameter_registry <- function(defaults = default_params_sec(), key_schema = stpd_parameter_schema(scope = "key")) {
  flat <- stpd_flatten_params(defaults)
  contract <- stpd_parameter_contract()
  if (nrow(contract) == 0) return(key_schema)
  out <- merge(contract, flat, by = "path", all.x = TRUE, sort = FALSE)
  out$value_string[is.na(out$value_string) | !nzchar(out$value_string)] <- out$default[is.na(out$value_string) | !nzchar(out$value_string)]
  out$value_type[is.na(out$value_type) | !nzchar(out$value_type)] <- out$type[is.na(out$value_type) | !nzchar(out$value_type)]
  out$is_key <- out$path %in% key_schema$path
  out$registry_scope <- ifelse(out$schema_scope == "key_ui", "key_ui",
                               ifelse(out$schema_scope == "eventness_audit", "eventness_audit", "full_contract"))
  extra <- flat[!(flat$path %in% contract$path), , drop = FALSE]
  if (nrow(extra) > 0) {
    extra$group <- sub("\\..*$", "", extra$path)
    extra$is_key <- FALSE
    extra$input_id <- gsub("[^A-Za-z0-9_]+", "__", extra$path)
    extra$type <- extra$value_type
    extra$default <- extra$value_string
    extra$label <- extra$path
    extra$choices <- ""
    extra$min <- ""
    extra$max <- ""
    extra$step <- ""
    extra$unit <- ""
    extra$scientific_note <- "Extension parameter not present in YAML contract; retained for compatibility."
    extra$schema_scope <- "extension"
    extra$required <- "FALSE"
    extra$ui_level <- "expert"
    extra$ui_order <- ""
    extra$section <- extra$group
    extra$section_order <- "500"
    extra$advanced <- "TRUE"
    extra$expert_only <- "TRUE"
    extra$visible_if <- ""
    extra$help_text <- "Extension parameter retained for expert compatibility/audit."
    extra$control_type <- stpd_contract_control_type(extra$type)
    extra$registry_scope <- "extension"
    out <- dplyr::bind_rows(out, extra[, names(out), drop = FALSE])
  }
  out[order(suppressWarnings(as.numeric(out$section_order)), suppressWarnings(as.numeric(out$ui_order)), out$group, out$path), , drop = FALSE]
}

stpd_contract_default_allows_missing <- function(rr) {
  if (is.null(rr) || nrow(rr) == 0) return(FALSE)
  typ <- as.character(rr$type[1] %||% "")
  default <- trimws(as.character(rr$default[1] %||% ""))
  minv <- suppressWarnings(as.numeric(rr$min[1]))
  maxv <- suppressWarnings(as.numeric(rr$max[1]))
  typ == "numeric" && identical(toupper(default), "NA") &&
    !is.finite(minv) && !is.finite(maxv)
}

# Cross-parameter scientific constraints cannot be represented by the scalar
# type/range contract above.  Always evaluate them on the productized canonical
# namespace so a stale legacy alias cannot veto (or conceal) the public value.
stpd_validate_param_relations <- function(params = default_params_sec()) {
  if (is.null(params)) params <- list()
  if (!is.list(params)) {
    stop("stpd_validate_param_relations(): params must be a list.", call. = FALSE)
  }
  if (!exists("stpd_productize_params", mode = "function")) {
    stop("stpd_validate_param_relations(): canonical parameter productizer is unavailable.", call. = FALSE)
  }

  canonical <- stpd_productize_params(params, prefer = "canonical")
  rows <- list()
  messages <- c(
    burst_seed_lower_not_below_upper =
      "burst seed lower bound must be strictly less than seed upper bound",
    burst_seed_upper_above_bridge =
      "burst seed upper bound must not exceed bridge upper bound",
    possible_burst_contrast_above_canonical =
      "possible burst contrast must not exceed canonical burst contrast",
    burst_classic_min_above_max =
      "classic burst minimum spikes must not exceed classic burst maximum spikes",
    burst_classic_max_not_below_long_min =
      "classic burst maximum spikes must be strictly less than long burst minimum spikes",
    burst_long_max_unbounded_sentinel =
      "long burst maximum spikes set to 0 enables the legacy unbounded sentinel; prolonged-burst sizing may be unreachable",
    burst_long_min_above_max =
      "long burst minimum spikes must not exceed long burst maximum spikes",
    burst_long_max_not_below_prolonged_min =
      "long burst maximum spikes must be strictly less than prolonged burst minimum spikes",
    burst_prolonged_min_above_max =
      "prolonged burst minimum spikes must not exceed prolonged burst maximum spikes",
    tonic_min_not_below_max =
      "tonic minimum ISI must be strictly less than tonic maximum ISI",
    tonic_max_above_bridge =
      "tonic maximum ISI must not exceed tonic bridge upper bound",
    tonic_mm_min_above_max =
      "tonic MM minimum must not exceed the ordinary MM maximum",
    tonic_mm_max_above_relaxed_max =
      "tonic ordinary MM maximum must not exceed the guarded relaxed MM maximum",
    pause_min_above_max =
      "pause minimum ISI must not exceed pause maximum ISI",
    pause_max_above_bridge =
      "pause maximum ISI must not exceed pause bridge upper bound",
    hfs_short_above_q90 =
      "high-frequency spiking short-ISI upper bound must not exceed its q90 bound",
    hfs_q90_above_bridge =
      "high-frequency spiking q90 bound must not exceed its epoch bridge bound",
    hfs_bridge_above_tolerated_gap =
      "high-frequency spiking epoch bridge bound must not exceed its tolerated-gap bound",
    pause_below_hfs_bridge =
      "pause seed below the high-frequency spiking connector requires instance-level Pause boundary resolution",
    active_hfs_cap_above_bridge =
      "an active high-frequency spiking direct-support maximum-ISI cap must not exceed the epoch bridge bound",
    event_grammar_user_band_value_invalid =
      "enabled event-grammar user band values must be finite and strictly positive",
    event_grammar_user_seed_lower_not_below_upper =
      "enabled event-grammar user seed lower bound must be strictly less than its seed upper bound",
    event_grammar_user_seed_upper_above_bridge =
      "enabled event-grammar user seed upper bound must not exceed its bridge upper bound",
    event_grammar_user_burst_contrast_invalid =
      "enabled event-grammar user burst contrast must be finite and at least 1",
    event_grammar_user_burst_contrast_below_possible =
      "enabled event-grammar user burst contrast must not be below the canonical possible-burst contrast"
  )
  add_issue <- function(severity, path, issue_code, args = list()) {
    row <- data.frame(
      severity = as.character(severity),
      path = as.character(path),
      issue = unname(messages[[issue_code]]),
      issue_code = as.character(issue_code),
      stringsAsFactors = FALSE
    )
    row$args <- I(list(args))
    rows[[length(rows) + 1L]] <<- row
  }
  number_at <- function(path) {
    value <- suppressWarnings(as.numeric(stpd_get_param(canonical, path, NULL)))
    if (length(value) != 1L || !is.finite(value[1])) return(NA_real_)
    value[1]
  }
  compare_args <- function(...) {
    values <- list(...)
    lapply(values, function(value) {
      value <- suppressWarnings(as.numeric(value))
      if (length(value) == 1L && is.finite(value)) value else NA_real_
    })
  }

  burst_seed_lower <- number_at("spiketrainpattern.burst.seed_lower_sec")
  burst_seed_upper <- number_at("spiketrainpattern.burst.seed_upper_sec")
  burst_bridge <- number_at("spiketrainpattern.burst.bridge_upper_sec")
  if (all(is.finite(c(burst_seed_lower, burst_seed_upper))) && burst_seed_lower >= burst_seed_upper) {
    add_issue(
      "error", "spiketrainpattern.burst.seed_upper_sec",
      "burst_seed_lower_not_below_upper",
      compare_args(seed_lower_sec = burst_seed_lower, seed_upper_sec = burst_seed_upper)
    )
  }
  if (all(is.finite(c(burst_seed_upper, burst_bridge))) && burst_seed_upper > burst_bridge) {
    add_issue(
      "error", "spiketrainpattern.burst.bridge_upper_sec",
      "burst_seed_upper_above_bridge",
      compare_args(seed_upper_sec = burst_seed_upper, bridge_upper_sec = burst_bridge)
    )
  }

  burst_contrast <- number_at("spiketrainpattern.burst.contrast_min")
  possible_contrast <- number_at("spiketrainpattern.burst.possible_contrast_min")
  if (all(is.finite(c(burst_contrast, possible_contrast))) && possible_contrast > burst_contrast) {
    add_issue(
      "error", "spiketrainpattern.burst.possible_contrast_min",
      "possible_burst_contrast_above_canonical",
      compare_args(canonical_contrast = burst_contrast, possible_contrast = possible_contrast)
    )
  }

  classic_min <- number_at("spiketrainpattern.burst.classic_min_spikes")
  classic_max <- number_at("spiketrainpattern.burst.classic_max_spikes")
  long_min <- number_at("spiketrainpattern.burst.long_min_spikes")
  long_max <- number_at("spiketrainpattern.burst.long_max_spikes")
  prolonged_min <- number_at("spiketrainpattern.burst.prolonged_min_spikes")
  prolonged_max <- number_at("spiketrainpattern.burst.prolonged_max_spikes")
  if (all(is.finite(c(classic_min, classic_max))) && classic_min > classic_max) {
    add_issue(
      "error", "spiketrainpattern.burst.classic_max_spikes",
      "burst_classic_min_above_max",
      compare_args(classic_min_spikes = classic_min, classic_max_spikes = classic_max)
    )
  }
  if (all(is.finite(c(classic_max, long_min))) && classic_max >= long_min) {
    add_issue(
      "error", "spiketrainpattern.burst.long_min_spikes",
      "burst_classic_max_not_below_long_min",
      compare_args(classic_max_spikes = classic_max, long_min_spikes = long_min)
    )
  }
  if (is.finite(long_max) && identical(long_max, 0)) {
    add_issue(
      "warning", "spiketrainpattern.burst.long_max_spikes",
      "burst_long_max_unbounded_sentinel",
      compare_args(long_max_spikes = long_max, prolonged_min_spikes = prolonged_min)
    )
  } else if (is.finite(long_max)) {
    if (is.finite(long_min) && long_min > long_max) {
      add_issue(
        "error", "spiketrainpattern.burst.long_max_spikes",
        "burst_long_min_above_max",
        compare_args(long_min_spikes = long_min, long_max_spikes = long_max)
      )
    }
    if (is.finite(prolonged_min) && long_max >= prolonged_min) {
      add_issue(
        "error", "spiketrainpattern.burst.prolonged_min_spikes",
        "burst_long_max_not_below_prolonged_min",
        compare_args(long_max_spikes = long_max, prolonged_min_spikes = prolonged_min)
      )
    }
  }
  if (all(is.finite(c(prolonged_min, prolonged_max))) && prolonged_min > prolonged_max) {
    add_issue(
      "error", "spiketrainpattern.burst.prolonged_max_spikes",
      "burst_prolonged_min_above_max",
      compare_args(prolonged_min_spikes = prolonged_min, prolonged_max_spikes = prolonged_max)
    )
  }

  tonic_min <- number_at("spiketrainpattern.tonic.min_isi_sec")
  tonic_max <- number_at("spiketrainpattern.tonic.max_isi_sec")
  tonic_bridge <- number_at("spiketrainpattern.tonic.bridge_upper_sec")
  if (all(is.finite(c(tonic_min, tonic_max))) && tonic_min >= tonic_max) {
    add_issue(
      "error", "spiketrainpattern.tonic.max_isi_sec",
      "tonic_min_not_below_max",
      compare_args(min_isi_sec = tonic_min, max_isi_sec = tonic_max)
    )
  }
  if (all(is.finite(c(tonic_max, tonic_bridge))) && tonic_max > tonic_bridge) {
    add_issue(
      "error", "spiketrainpattern.tonic.bridge_upper_sec",
      "tonic_max_above_bridge",
      compare_args(max_isi_sec = tonic_max, bridge_upper_sec = tonic_bridge)
    )
  }
  tonic_mm_min <- number_at("spiketrainpattern.tonic.mm_min")
  tonic_mm_max <- number_at("spiketrainpattern.tonic.mm_max")
  tonic_mm_relaxed_max <- number_at("spiketrainpattern.tonic.mm_relaxed_max")
  if (all(is.finite(c(tonic_mm_min, tonic_mm_max))) &&
      tonic_mm_min > tonic_mm_max) {
    add_issue(
      "error", "spiketrainpattern.tonic.mm_max",
      "tonic_mm_min_above_max",
      compare_args(mm_min = tonic_mm_min, mm_max = tonic_mm_max)
    )
  }
  if (all(is.finite(c(tonic_mm_max, tonic_mm_relaxed_max))) &&
      tonic_mm_max > tonic_mm_relaxed_max) {
    add_issue(
      "error", "spiketrainpattern.tonic.mm_relaxed_max",
      "tonic_mm_max_above_relaxed_max",
      compare_args(mm_max = tonic_mm_max,
                   mm_relaxed_max = tonic_mm_relaxed_max)
    )
  }

  pause_min <- number_at("spiketrainpattern.pause.min_isi_sec")
  pause_max <- number_at("spiketrainpattern.pause.max_isi_sec")
  pause_bridge <- number_at("spiketrainpattern.pause.bridge_upper_sec")
  if (all(is.finite(c(pause_min, pause_max))) && pause_min > pause_max) {
    add_issue(
      "error", "spiketrainpattern.pause.max_isi_sec",
      "pause_min_above_max",
      compare_args(min_isi_sec = pause_min, max_isi_sec = pause_max)
    )
  }
  # Keep this relation visible in validation reports for users, but the engine
  # treats it as advisory: pause_max is the terminal classification bound and
  # pause bridge is a separate continuity aid that may legitimately be
  # narrower during a dry-run.
  if (all(is.finite(c(pause_max, pause_bridge))) && pause_max > pause_bridge) {
    add_issue(
      "error", "spiketrainpattern.pause.bridge_upper_sec",
      "pause_max_above_bridge",
      compare_args(max_isi_sec = pause_max, bridge_upper_sec = pause_bridge)
    )
  }

  hfs_short <- number_at("spiketrainpattern.high_frequency_spiking.short_isi_upper_sec")
  hfs_q90 <- number_at("spiketrainpattern.high_frequency_spiking.q90_isi_max_sec")
  hfs_bridge <- number_at("spiketrainpattern.high_frequency_spiking.epoch_bridge_isi_sec")
  hfs_tolerated <- number_at("spiketrainpattern.high_frequency_spiking.tolerated_gap_isi_sec")
  if (all(is.finite(c(hfs_short, hfs_q90))) && hfs_short > hfs_q90) {
    add_issue(
      "error", "spiketrainpattern.high_frequency_spiking.q90_isi_max_sec",
      "hfs_short_above_q90",
      compare_args(short_isi_upper_sec = hfs_short, q90_isi_max_sec = hfs_q90)
    )
  }
  if (all(is.finite(c(hfs_q90, hfs_bridge))) && hfs_q90 > hfs_bridge) {
    add_issue(
      "error", "spiketrainpattern.high_frequency_spiking.epoch_bridge_isi_sec",
      "hfs_q90_above_bridge",
      compare_args(q90_isi_max_sec = hfs_q90, epoch_bridge_isi_sec = hfs_bridge)
    )
  }
  if (all(is.finite(c(hfs_bridge, hfs_tolerated))) && hfs_bridge > hfs_tolerated) {
    add_issue(
      "error", "spiketrainpattern.high_frequency_spiking.tolerated_gap_isi_sec",
      "hfs_bridge_above_tolerated_gap",
      compare_args(epoch_bridge_isi_sec = hfs_bridge, tolerated_gap_isi_sec = hfs_tolerated)
    )
  }
  if (all(is.finite(c(pause_min, hfs_bridge))) && pause_min < hfs_bridge) {
    add_issue(
      "warning", "spiketrainpattern.pause.min_isi_sec",
      "pause_below_hfs_bridge",
      compare_args(pause_min_isi_sec = pause_min, hfs_epoch_bridge_isi_sec = hfs_bridge)
    )
  }

  hfs_cap <- number_at("detector.pattern_isi_limits.high_frequency_spiking.max_sec")
  if (is.finite(hfs_cap) && hfs_cap > 0) {
    if (is.finite(hfs_bridge) && hfs_cap > hfs_bridge) {
      add_issue(
        "error", "detector.pattern_isi_limits.high_frequency_spiking.max_sec",
        "active_hfs_cap_above_bridge",
        compare_args(max_sec = hfs_cap, epoch_bridge_isi_sec = hfs_bridge)
      )
    }
  }

  event_grammar_user <- (canonical$event_grammar %||% list())$user %||% list()
  for (pattern in c("burst", "high_frequency_spiking", "high_frequency_tonic", "tonic", "pause")) {
    user_band <- event_grammar_user[[pattern]] %||% list()
    if (!isTRUE(user_band$enable %||% FALSE)) next
    band_path <- paste0("event_grammar.user.", pattern)
    lower <- suppressWarnings(as.numeric(user_band$seed_lower_sec %||% NA_real_))
    upper <- suppressWarnings(as.numeric(user_band$seed_upper_sec %||% NA_real_))
    bridge <- suppressWarnings(as.numeric(user_band$bridge_upper_sec %||% NA_real_))
    band <- c(lower = lower[1], upper = upper[1], bridge = bridge[1])
    if (length(lower) != 1L || length(upper) != 1L || length(bridge) != 1L ||
        any(!is.finite(band)) || any(band <= 0)) {
      add_issue(
        "error", band_path, "event_grammar_user_band_value_invalid",
        compare_args(seed_lower_sec = lower[1], seed_upper_sec = upper[1], bridge_upper_sec = bridge[1])
      )
    } else {
      if (band[["lower"]] >= band[["upper"]]) {
        add_issue(
          "error", paste0(band_path, ".seed_upper_sec"),
          "event_grammar_user_seed_lower_not_below_upper",
          compare_args(seed_lower_sec = band[["lower"]], seed_upper_sec = band[["upper"]])
        )
      }
      if (band[["upper"]] > band[["bridge"]]) {
        add_issue(
          "error", paste0(band_path, ".bridge_upper_sec"),
          "event_grammar_user_seed_upper_above_bridge",
          compare_args(seed_upper_sec = band[["upper"]], bridge_upper_sec = band[["bridge"]])
        )
      }
    }

    if (identical(pattern, "burst")) {
      contrast <- suppressWarnings(as.numeric(user_band$contrast_S %||% NA_real_))
      contrast <- if (length(contrast) == 1L) contrast[1] else NA_real_
      if (!is.finite(contrast) || contrast < 1) {
        add_issue(
          "error", paste0(band_path, ".contrast_S"),
          "event_grammar_user_burst_contrast_invalid",
          compare_args(contrast_S = contrast)
        )
      } else if (is.finite(possible_contrast) && contrast < possible_contrast) {
        add_issue(
          "error", paste0(band_path, ".contrast_S"),
          "event_grammar_user_burst_contrast_below_possible",
          compare_args(
            contrast_S = contrast,
            canonical_possible_contrast = possible_contrast
          )
        )
      }
      # event_grammar.user.burst.one_sided_contrast_S is a legacy UI payload
      # not consumed by the threshold resolver.  The active one-sided rule is
      # event_grammar.one_sided_burst_contrast_min, so this relation validator
      # must not fail a run on the inactive nested value.
    }
  }

  if (length(rows) == 0L) {
    out <- data.frame(
      severity = character(), path = character(), issue = character(),
      issue_code = character(), stringsAsFactors = FALSE
    )
    out$args <- I(vector("list", 0L))
    return(out)
  }
  dplyr::bind_rows(rows)
}

stpd_validate_params <- function(params = default_params_sec(), registry = stpd_parameter_registry(), strict = FALSE) {
  flat <- stpd_flatten_params(params)
  reg_paths <- registry$path
  issues <- list()
  missing_key <- registry$path[registry$registry_scope == "key_ui" & !(registry$path %in% flat$path)]
  if (length(missing_key)) {
    issues[[length(issues) + 1L]] <- data.frame(severity = "warning", path = missing_key, issue = "key parameter missing from params; schema default will be applied", stringsAsFactors = FALSE)
  }
  extra <- flat$path[!(flat$path %in% reg_paths)]
  if (length(extra)) {
    issues[[length(issues) + 1L]] <- data.frame(severity = "info", path = extra, issue = "parameter not present in registry; retained as legacy/extension parameter", stringsAsFactors = FALSE)
  }
  add_issue <- function(severity, path, issue) {
    issues[[length(issues) + 1L]] <<- data.frame(severity = severity, path = path, issue = issue, stringsAsFactors = FALSE)
  }
  split_choices <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) return(character())
    strsplit(as.character(x[1]), "|", fixed = TRUE)[[1]]
  }
  type_ok <- function(value, expected) {
    expected <- as.character(expected %||% "")
    if (!nzchar(expected) || is.null(value)) return(TRUE)
    if (expected == "numeric") return(is.numeric(value))
    if (expected == "integer") return(is.integer(value) || (is.numeric(value) && all(is.na(value) | abs(value - round(value)) < .Machine$double.eps^0.5)))
    if (expected == "logical") return(is.logical(value))
    if (expected %in% c("choice", "choice_vector", "text", "character")) return(is.character(value))
    if (expected == "list") return(is.list(value) && !is.data.frame(value))
    if (expected == "data_frame") return(is.data.frame(value))
    TRUE
  }
  for (ii in seq_len(nrow(flat))) {
    rr <- registry[registry$path == flat$path[ii], , drop = FALSE]
    if (nrow(rr) == 0) next
    value <- stpd_get_param(params, flat$path[ii], NULL)
    expected_type <- as.character(rr$type[1] %||% "")
    if (!type_ok(value, expected_type)) {
      add_issue("error", flat$path[ii], paste0("type mismatch: expected ", expected_type, ", got ", flat$value_type[ii]))
      next
    }
    if (expected_type == "logical") {
      if (length(value) == 0 || any(is.na(value))) {
        add_issue("error", flat$path[ii], "logical parameter has missing or invalid value")
        next
      }
    }
    if (expected_type %in% c("numeric", "integer")) {
      vals_all <- suppressWarnings(as.numeric(value))
      missing_or_invalid <- length(vals_all) == 0 || any(is.na(vals_all))
      if (missing_or_invalid && !stpd_contract_default_allows_missing(rr)) {
        add_issue("error", flat$path[ii], paste0(expected_type, " parameter has missing or invalid value"))
        next
      }
      if (expected_type == "integer") {
        finite_vals <- vals_all[is.finite(vals_all)]
        if (any(!is.finite(vals_all))) {
          add_issue("error", flat$path[ii], "integer parameter must be finite")
          next
        }
        if (length(finite_vals) > 0 && any(abs(finite_vals - round(finite_vals)) >= .Machine$double.eps^0.5)) {
          add_issue("error", flat$path[ii], "integer parameter contains a fractional value")
          next
        }
      }
    }
    choices <- split_choices(rr$choices)
    if (length(choices) > 0 && expected_type %in% c("choice", "choice_vector", "text", "character")) {
      vv <- as.character(value)
      bad <- setdiff(vv[!is.na(vv) & nzchar(vv)], choices)
      if (length(bad) > 0) add_issue("error", flat$path[ii], paste0("value outside choices: ", paste(bad, collapse = ",")))
    }
    if (expected_type %in% c("numeric", "integer")) {
      vals <- suppressWarnings(as.numeric(value))
      vals <- vals[!is.na(vals)]
      minv <- suppressWarnings(as.numeric(rr$min[1]))
      maxv <- suppressWarnings(as.numeric(rr$max[1]))
      if (length(vals) > 0 && is.finite(minv) && any(vals < minv)) {
        add_issue("error", flat$path[ii], paste0("value below contract minimum ", minv))
      }
      if (length(vals) > 0 && is.finite(maxv) && any(vals > maxv)) {
        add_issue("error", flat$path[ii], paste0("value above contract maximum ", maxv))
      }
    }
  }
  tonic_short_contract <- stpd_tonic_short_route_contract(params)
  if (isTRUE(tonic_short_contract$requested) &&
      !isTRUE(tonic_short_contract$valid)) {
    add_issue(
      "error",
      "spiketrainpattern.tonic.short_regular_route_enabled",
      paste0(
        "enabled short Tonic route has an invalid calibration-frozen ",
        "contract: ", tonic_short_contract$reason
      )
    )
  }
  tonic_mm_contract <- stpd_tonic_mm_relaxation_contract(params)
  if (isTRUE(tonic_mm_contract$required) &&
      !isTRUE(tonic_mm_contract$valid)) {
    add_issue(
      "error",
      "spiketrainpattern.tonic.mm_relaxed_max",
      paste0(
        "Tonic MM ceiling above 1.40 has an invalid calibration-frozen ",
        "contract: ", tonic_mm_contract$reason
      )
    )
  }
  out <- if (length(issues)) dplyr::bind_rows(issues) else data.frame(severity = character(), path = character(), issue = character(), stringsAsFactors = FALSE)
  relation_issues <- stpd_validate_param_relations(params)
  if (nrow(relation_issues) > 0L) {
    # Preserve the historical public three-column validator schema.  The
    # dedicated relation validator exposes issue_code/args for programmatic use.
    out <- dplyr::bind_rows(out, relation_issues[, c("severity", "path", "issue"), drop = FALSE])
  }
  if (strict && any(out$severity == "error")) stop(paste(out$path[out$severity == "error"], out$issue[out$severity == "error"], collapse = "; "), call. = FALSE)
  out
}

stpd_parameter_issue_table <- function(issues, registry = stpd_parameter_registry(), ui_level = "all") {
  if (is.null(issues) || nrow(issues) == 0) {
    return(data.frame(severity = character(), path = character(), issue = character(), ui_level = character(), section = character(), stringsAsFactors = FALSE))
  }
  meta_cols <- intersect(names(registry), c("path", "ui_level", "section", "section_order", "ui_order", "registry_scope"))
  out <- merge(issues, registry[, meta_cols, drop = FALSE], by = "path", all.x = TRUE, sort = FALSE)
  out$ui_level[is.na(out$ui_level) | !nzchar(out$ui_level)] <- "unclassified"
  out$section[is.na(out$section) | !nzchar(out$section)] <- "Unclassified"
  level <- as.character(ui_level %||% "all")[1]
  if (!identical(level, "all")) {
    out <- out[out$severity == "error" | out$ui_level == level, , drop = FALSE]
  }
  out[order(out$severity != "error", suppressWarnings(as.numeric(out$section_order)), suppressWarnings(as.numeric(out$ui_order)), out$path), , drop = FALSE]
}

stpd_parameter_report_flat <- function(params = default_params_sec(), baseline = default_params_sec(), preset = NULL) {
  cur <- stpd_flatten_params(params)
  base <- stpd_flatten_params(baseline)
  names(base)[names(base) == "value_string"] <- "default_value"
  out <- merge(cur[, c("path", "value_string", "value_type")], base[, c("path", "default_value")], by = "path", all = TRUE, sort = FALSE)
  out$current_value <- out$value_string
  out$value_string <- NULL
  out$changed_from_default <- !is.na(out$current_value) & !is.na(out$default_value) & out$current_value != out$default_value
  if (!is.null(preset)) {
    pre <- stpd_flatten_params(preset)
    names(pre)[names(pre) == "value_string"] <- "preset_value"
    out <- merge(out, pre[, c("path", "preset_value")], by = "path", all.x = TRUE, sort = FALSE)
    out$changed_from_preset <- !is.na(out$current_value) & !is.na(out$preset_value) & out$current_value != out$preset_value
  } else {
    out$preset_value <- NA_character_
    out$changed_from_preset <- NA
  }
  reg <- stpd_parameter_registry(baseline)
  reg <- reg[, intersect(names(reg), c("path", "group", "label", "scientific_note", "registry_scope", "ui_level", "section", "section_order", "ui_order", "advanced", "expert_only", "help_text", "control_type")), drop = FALSE]
  out <- merge(out, reg, by = "path", all.x = TRUE, sort = FALSE)
  out[order(suppressWarnings(as.numeric(out$section_order)), suppressWarnings(as.numeric(out$ui_order)), out$group, out$path), , drop = FALSE]
}

stpd_parameter_change_preview <- function(params = default_params_sec(), baseline = default_params_sec(),
                                          include_non_ui = FALSE) {
  report <- parameter_report_table(params, defaults = baseline)
  if (is.null(report) || nrow(report) == 0) {
    return(data.frame(message = "No parameters available for change preview.", stringsAsFactors = FALSE))
  }
  changed_col <- if ("changed_from_default" %in% names(report)) "changed_from_default" else "differs_from_default"
  changed <- report[isTRUE(report[[changed_col]]) | (!is.na(report[[changed_col]]) & report[[changed_col]]), , drop = FALSE]
  if (!isTRUE(include_non_ui) && nrow(changed) > 0) {
    key_paths <- tryCatch(stpd_parameter_schema(scope = "key")$path, error = function(e) character())
    contract_paths <- tryCatch(stpd_contract_ui_schema(ui_level = "all")$path, error = function(e) character())
    ui_paths <- unique(c(as.character(key_paths), as.character(contract_paths)))
    changed <- changed[as.character(changed$path) %in% ui_paths, , drop = FALSE]
  }
  if (nrow(changed) == 0) {
    return(data.frame(message = "No UI-visible parameters differ from defaults.", stringsAsFactors = FALSE))
  }
  for (nm in c("label", "ui_level", "section", "section_order", "ui_order", "help_text", "scientific_note", "value", "default_value", "preset_value")) {
    if (!(nm %in% names(changed))) changed[[nm]] <- ""
  }
  changed$ui_level[is.na(changed$ui_level) | !nzchar(as.character(changed$ui_level))] <- "unclassified"
  changed$section[is.na(changed$section) | !nzchar(as.character(changed$section))] <- "Unclassified"
  changed$label[is.na(changed$label) | !nzchar(as.character(changed$label))] <- changed$path[is.na(changed$label) | !nzchar(as.character(changed$label))]
  impact <- as.character(changed$help_text %||% "")
  blank_impact <- is.na(impact) | !nzchar(impact)
  impact[blank_impact] <- as.character(changed$scientific_note %||% "")[blank_impact]
  impact[is.na(impact) | !nzchar(impact)] <- "Changed from default; review detector output after rerun."
  changed$impact_preview <- impact
  changed$level_order <- match(as.character(changed$ui_level), c("basic", "advanced", "expert", "unclassified"))
  changed$level_order[is.na(changed$level_order)] <- 99L
  changed <- changed[order(
    changed$level_order,
    suppressWarnings(as.numeric(changed$section_order)),
    suppressWarnings(as.numeric(changed$ui_order)),
    changed$path
  ), , drop = FALSE]
  out <- changed[, c("path", "label", "ui_level", "section", "value", "default_value", "impact_preview"), drop = FALSE]
  names(out) <- c("path", "label", "ui_level", "section", "current_value", "default_value", "impact")
  rownames(out) <- NULL
  out
}

stpd_params_hash_flat <- function(params) {
  if (exists("stpd_params_hash", mode = "function")) {
    return(stpd_params_hash(params))
  }
  canonical <- if (exists("stpd_productize_params", mode = "function")) {
    stpd_productize_params(params, prefer = "canonical")
  } else {
    params
  }
  digest::digest(
    stpd_flatten_params(canonical)[, c("path", "value_string")],
    algo = "sha256"
  )
}
