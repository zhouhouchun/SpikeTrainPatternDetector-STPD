# ============================================================
# event grammar Codex third-review fixes
# ------------------------------------------------------------
# 1) Hard-block high-confidence derived CSV files unless the caller explicitly
#    sets allow_derived_csv = TRUE.  The second-review guard was too permissive:
#    long numeric columns in Sliding/summary/threshold tables could still be
#    treated as raw spike timestamps.
# 2) Ensure final_classify_candidates() returns a zero-row, schema-preserving
#    table when all candidate rows are filtered out as rejected/unwritten.
# ============================================================

stpd_event_grammar_hard_derived_csv_filename <- function(path) {
  nm <- tolower(basename(as.character(path)))
  # High-confidence derived outputs.  These are never raw spike timestamp files
  # in this package's workflow unless explicitly overridden by allow_derived_csv.
  grepl("^sliding([_ .-]|$)", nm) ||
    grepl("^isi[_ .-]?base([_ .-]|\\.csv$)", nm) ||
    grepl("^tonic[_ .-]?summary([_ .-]|\\.csv$)", nm) ||
    grepl("(^|[_ .-])burst[_ .-]?isi[_ .-]?(threshold|threshould|thresh)([_ .-]|\\.csv$)", nm) ||
    grepl("(^|[_ .-])(threshold|thresholds|threshould|thresh)([_ .-]|\\.csv$)", nm) ||
    grepl("^(candidate[_ .-]?ledger|eventness[_ .-]?audit|final[_ .-]?classification|events[_ .-]?final|event_grammar[_ .-]?candidate|arbitration[_ .-]?candidate)([_ .-]|\\.csv$)", nm) ||
    grepl("^(misi|logisi|support|qc|validation|manual[_ .-]?vs[_ .-]?detector|stationarity|duplicate|artifact|near[_ .-]?miss)([_ .-]|$)", nm) ||
    grepl("(^|[_ .-])(summary|summaries|audit|audits|output|outputs|result|results|metrics|features|parameters|params|candidate|candidates)([_ .-]|\\.csv$)", nm)
}

stpd_event_grammar_strong_derived_csv_schema <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  cn <- tolower(gsub("[^a-z0-9]+", "_", colnames(df)))
  cn <- gsub("^_+|_+$", "", cn)

  # Columns that are characteristic of derived tables, not independent raw
  # spike-train timestamp columns.  This catches Sliding_* and threshold/summary
  # files even when the filename is not informative.
  strong_patterns <- c(
    "^spike_train$", "^fragment(_number|_id)?$", "^start(_time|_sec|_ms)?$", "^end(_time|_sec|_ms)?$",
    "^max_isi", "^min_isi", "^median_isi", "^mean_isi", "^q[0-9]+_?isi", "^isi_(mean|median|max|min|q[0-9]+)",
    "^mm$", "^cv$", "^lv$", "^duration", "^n_spikes$", "^count$", "^fraction$",
    "^threshold$", "^threshould$", "^candidate", "^event", "^score$", "^decision$", "^status$",
    "^pattern$", "^label$", "^final_label$", "^source$", "^family$", "^summary$", "^metric"
  )
  hits <- vapply(cn, function(x) any(grepl(paste(strong_patterns, collapse = "|"), x)), logical(1))
  hit_n <- sum(hits, na.rm = TRUE)
  hit_frac <- hit_n / max(length(cn), 1L)

  # A derived schema is indicated by several analysis/summary columns, or by a
  # smaller number of very specific derived names such as Spike_train + Start/End.
  hit_n >= 3L || (hit_n >= 2L && any(cn %in% c("spike_train", "fragment_number", "start_time", "end_time"))) || hit_frac >= 0.35
}

stpd_event_grammar_csv_shape_is_too_summary_like <- function(df) {
  # Raw spike timestamp CSVs can have short columns, but most package-derived
  # threshold/summary tables have many named metric columns and few rows.  Use
  # this only as an extra guard after filename/schema suspicion, not alone.
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(FALSE)
  n_num <- 0L
  long_num <- 0L
  for (j in seq_len(ncol(df))) {
    x <- suppressWarnings(as.numeric(df[[j]]))
    x <- x[is.finite(x)]
    if (length(x) == 0) next
    n_num <- n_num + 1L
    if (length(x) >= 20L && length(unique(x)) >= min(10L, length(x))) long_num <- long_num + 1L
  }
  nrow(df) < 20L && ncol(df) >= 3L && n_num >= 2L && long_num == 0L
}

build_trains_from_raw <- function(path, header = TRUE, unit_in = c("s", "ms"), duplicate_policy = c("error_keep", "warn_keep", "collapse_exact"), allow_derived_csv = FALSE) {
  unit_in <- match.arg(unit_in)
  duplicate_policy <- match.arg(duplicate_policy)

  if (!isTRUE(allow_derived_csv)) {
    hard_name <- stpd_event_grammar_hard_derived_csv_filename(path)
    df_probe <- tryCatch(read_csv_fast(path, header = header), error = function(e) NULL)
    strong_schema <- !is.null(df_probe) && stpd_event_grammar_strong_derived_csv_schema(df_probe)
    summary_shape <- !is.null(df_probe) && stpd_event_grammar_csv_shape_is_too_summary_like(df_probe)

    if (hard_name) {
      stop(
        "This CSV filename is a high-confidence derived output table, not a raw spike timestamp file: ", basename(path),
        ". Examples include Sliding_*, ISI_base, tonic_summary, threshold/threshould, candidate/eventness/audit/output files. ",
        "Set allow_derived_csv=TRUE only if you intentionally want to bypass this guard.",
        call. = FALSE
      )
    }
    if (strong_schema || summary_shape) {
      stop(
        "This CSV schema/data shape looks like a derived analysis/summary table, not raw spike timestamps: ", basename(path),
        ". Raw spike CSVs should contain timestamp columns, not columns such as Spike_train, Fragment_number, Start_time, End_time, Max_ISI, Mean_ISI, thresholds, candidates or audit metrics. ",
        "Set allow_derived_csv=TRUE only if this is intentional.",
        call. = FALSE
      )
    }
  }

  # Call the canonical timestamp parser after the hard derived-table guard.
  build_trains_from_raw_impl(path, header = header, unit_in = unit_in, duplicate_policy = duplicate_policy)
}

build_spike_dataset <- function(path, mode = c("raw", "labeled"), unit_in = c("s", "ms"), header = TRUE, name = NULL, duplicate_policy = c("error_keep", "warn_keep", "collapse_exact"), allow_derived_csv = FALSE) {
  mode <- match.arg(mode)
  unit_in <- match.arg(unit_in)
  duplicate_policy <- match.arg(duplicate_policy)
  trains <- if (mode == "raw") {
    build_trains_from_raw(path, header = header, unit_in = unit_in, duplicate_policy = duplicate_policy, allow_derived_csv = allow_derived_csv)
  } else {
    build_trains_from_annot(path, unit_in = unit_in, duplicate_policy = duplicate_policy)
  }
  task_events <- if (mode == "raw") {
    tryCatch(stpd_extract_task_events_from_raw(path, header = header, unit_in = unit_in), error = function(e) stpd_empty_task_events())
  } else {
    tryCatch(stpd_extract_task_events_from_raw(path, header = TRUE, unit_in = unit_in), error = function(e) stpd_empty_task_events())
  }
  make_dataset(name = name %||% tools::file_path_sans_ext(basename(path)), source = mode, trains = trains, unit_in = unit_in, task_events = task_events)
}

stpd_event_grammar_filter_public_candidate_features <- function(features) {
  f <- tibble::as_tibble(features %||% stpd_event_grammar_empty_candidate_features())
  if (nrow(f) == 0) return(f)
  keep <- rep(TRUE, nrow(f))
  if ("written_to_auto" %in% names(f)) keep <- keep & (is.na(f$written_to_auto) | f$written_to_auto == TRUE)
  if ("selected_for_auto" %in% names(f)) keep <- keep & (is.na(f$selected_for_auto) | f$selected_for_auto == TRUE)
  if ("final_candidate_class" %in% names(f)) {
    cls <- tolower(trimws(as.character(f$final_candidate_class)))
    bad <- cls %in% stpd_event_grammar_review_reject_labels() | grepl("reject|profile|diagnostic|not[_ -]?selected|unlabeled", cls)
    keep <- keep & !bad
  }
  if ("selection_status" %in% names(f)) {
    ss <- tolower(trimws(as.character(f$selection_status)))
    keep <- keep & !(ss %in% c("not_selected", "rejected", "reject", "diagnostic", "profile") | grepl("reject|not[_ -]?selected|diagnostic|profile", ss))
  }
  if ("gate_status" %in% names(f)) {
    gs <- tolower(trimws(as.character(f$gate_status)))
    keep <- keep & !(gs %in% c("reject", "rejected", "diagnostic", "profile") | grepl("reject|diagnostic|profile", gs))
  }
  f[keep, , drop = FALSE]
}
