# Phase 0 UI provenance and staleness model.
#
# These helpers are deliberately pure: they inspect datasets, parameters, and
# validation artifacts and return immutable identity/status records.  They do
# not add fields to a scientific dataset.  A Shiny caller should capture the
# run identity immediately after a successful detector run and retain it in UI
# state (for example, keyed by the session dataset id).

stpd_ui_state_schema_version <- function() {
  "stpd_ui_run_state_v1"
}

stpd_ui_state_chr <- function(x, default = "") {
  value <- as.character(x %||% default)
  if (length(value) == 0L || is.na(value[1])) default else value[1]
}

stpd_ui_state_int <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(x %||% default))
  if (length(value) == 0L || is.na(value[1])) default else value[1]
}

stpd_ui_state_sha256 <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) return("")
  tryCatch(
    digest::digest(x, algo = "sha256", serialize = TRUE),
    error = function(e) ""
  )
}

stpd_ui_state_valid_sha256 <- function(x) {
  value <- tolower(trimws(stpd_ui_state_chr(x)))
  if (grepl("^[0-9a-f]{64}$", value)) value else ""
}

stpd_ui_state_params_sha256 <- function(params) {
  if (is.null(params) || !is.list(params)) return("")
  tryCatch(
    stpd_ui_state_valid_sha256(stpd_params_hash(params)),
    error = function(e) ""
  )
}

stpd_ui_state_normalize_vector <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "POSIXt")) x <- format(x, tz = "UTC", usetz = TRUE)
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  attributes(x) <- NULL
  x
}

stpd_ui_state_normalize_table <- function(x, sort_rows = FALSE) {
  if (!is.data.frame(x)) return(data.frame())
  columns <- sort(names(x), method = "radix")
  out <- x[, columns, drop = FALSE]
  out[] <- lapply(out, stpd_ui_state_normalize_vector)
  if (isTRUE(sort_rows) && nrow(out) > 1L && ncol(out) > 0L) {
    keys <- lapply(out, function(value) {
      if (is.numeric(value)) {
        key <- sprintf("%+.17e", as.numeric(value))
        key[is.na(value)] <- "<NA>"
        key
      } else {
        key <- as.character(value)
        key[is.na(key)] <- "<NA>"
        key
      }
    })
    ord <- do.call(order, c(keys, list(na.last = TRUE, method = "radix")))
    out <- out[ord, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

stpd_ui_state_train_names <- function(ds) {
  trains <- if (is.list(ds) && is.list(ds$trains)) ds$trains else list()
  names_in <- names(trains)
  if (is.null(names_in) || length(names_in) != length(trains) ||
      anyNA(names_in) || any(!nzchar(names_in)) || anyDuplicated(names_in)) {
    return(character())
  }
  sort(as.character(names_in), method = "radix")
}

stpd_ui_state_parse_trains <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character())
  value <- as.character(x)
  value <- unlist(strsplit(value, ";", fixed = TRUE), use.names = FALSE)
  value <- trimws(value)
  value <- value[!is.na(value) & nzchar(value)]
  sort(unique(value), method = "radix")
}

stpd_ui_state_data_payload <- function(ds, train_names) {
  trains <- ds$trains
  train_payload <- lapply(train_names, function(train) {
    dat <- trains[[train]]
    if (!is.data.frame(dat)) {
      return(list(train = train, valid = FALSE, n = NA_integer_))
    }
    # Detector input identity deliberately excludes AUTO, MANUAL, Review, and
    # audit columns.  Manual/Review truth has its own independent fingerprint.
    input_columns <- intersect(c("idx", "timestamp_sec", "ISI_sec"), names(dat))
    values <- lapply(input_columns, function(name) {
      stpd_ui_state_normalize_vector(dat[[name]])
    })
    names(values) <- input_columns
    list(train = train, valid = TRUE, n = as.integer(nrow(dat)), values = values)
  })
  names(train_payload) <- train_names
  list(
    trains = train_payload,
    task_events = stpd_ui_state_normalize_table(
      (ds %||% list())$task_events %||% data.frame(), sort_rows = TRUE
    )
  )
}

stpd_ui_state_manual_payload <- function(ds, train_names) {
  trains <- ds$trains
  manual_columns <- c("pattern_manual", "pattern_manual_negative")
  payload <- lapply(train_names, function(train) {
    dat <- trains[[train]]
    if (!is.data.frame(dat)) {
      return(list(train = train, valid = FALSE, n = NA_integer_))
    }
    n <- nrow(dat)
    values <- lapply(manual_columns, function(name) {
      value <- if (name %in% names(dat)) dat[[name]] else rep("", n)
      value <- as.character(value)
      value[is.na(value)] <- ""
      unname(value)
    })
    names(values) <- manual_columns
    list(train = train, valid = TRUE, n = as.integer(n), values = values)
  })
  names(payload) <- train_names
  payload
}

stpd_ui_state_auto_payload <- function(ds, train_names) {
  trains <- ds$trains
  payload <- lapply(train_names, function(train) {
    dat <- trains[[train]]
    if (!is.data.frame(dat)) {
      return(list(train = train, valid = FALSE, n = NA_integer_))
    }
    n <- nrow(dat)
    label <- if ("pattern_auto" %in% names(dat)) dat$pattern_auto else rep("", n)
    label <- as.character(label)
    label[is.na(label)] <- ""
    score <- if ("auto_score" %in% names(dat)) {
      suppressWarnings(as.numeric(dat$auto_score))
    } else {
      rep(NA_real_, n)
    }
    list(
      train = train, valid = TRUE, n = as.integer(n),
      pattern_auto = unname(label), auto_score = unname(score)
    )
  })
  names(payload) <- train_names
  payload
}

stpd_ui_state_review_payload <- function(ds) {
  product <- if (is.list(ds) && is.list(ds$results)) {
    ds$results$multitrack_review %||% NULL
  } else {
    NULL
  }
  if (is.null(product)) return(list(has_state = FALSE))
  table_names <- c(
    "metadata", "transition_history", "current_decisions",
    "manual_intervals", "final_intervals", "final_relationships"
  )
  tables <- lapply(table_names, function(name) {
    stpd_ui_state_normalize_table(product[[name]] %||% data.frame())
  })
  names(tables) <- table_names
  list(has_state = TRUE, tables = tables)
}

stpd_ui_state_detector_context_payload <- function(ds, train_names) {
  settings <- if (is.list(ds) && is.list(ds$train_settings)) {
    ds$train_settings
  } else {
    list()
  }
  per_train_fields <- c(
    "burst_isi_ranges", "tonic_isi_ranges", "pause_isi_ranges",
    "highfreq_isi_ranges", "isi_thresholds"
  )
  for (field in per_train_fields) {
    values <- settings[[field]] %||% list()
    if (!is.list(values)) values <- list()
    keep <- intersect(train_names, names(values) %||% character())
    settings[[field]] <- if (length(keep) > 0L) values[keep] else list()
  }
  settings <- settings[sort(names(settings), method = "radix")]
  list(train_settings = settings)
}

stpd_ui_state_scope_hashes <- function(ds, train_names) {
  all_trains <- stpd_ui_state_train_names(ds)
  requested <- stpd_ui_state_parse_trains(train_names)
  valid <- is.list(ds) && is.list(ds$trains) &&
    length(all_trains) == length(ds$trains) &&
    all(requested %in% all_trains)
  if (!valid) {
    return(list(
      valid = FALSE, train_names = requested, data_sha256 = "",
      auto_output_sha256 = "", manual_truth_sha256 = "",
      detector_context_sha256 = ""
    ))
  }
  list(
    valid = TRUE,
    train_names = requested,
    data_sha256 = stpd_ui_state_sha256(
      stpd_ui_state_data_payload(ds, requested)
    ),
    auto_output_sha256 = stpd_ui_state_sha256(
      stpd_ui_state_auto_payload(ds, requested)
    ),
    manual_truth_sha256 = stpd_ui_state_sha256(
      stpd_ui_state_manual_payload(ds, requested)
    ),
    detector_context_sha256 = stpd_ui_state_sha256(
      stpd_ui_state_detector_context_payload(ds, requested)
    )
  )
}

stpd_ui_dataset_identity <- function(ds, dataset_id = NULL) {
  train_names <- stpd_ui_state_train_names(ds)
  valid <- is.list(ds) && is.list(ds$trains) &&
    length(train_names) == length(ds$trains)
  meta <- if (is.list(ds) && is.list(ds$meta)) ds$meta else list()
  id <- stpd_ui_state_chr(dataset_id)
  if (!nzchar(id)) id <- stpd_ui_state_chr(meta$dataset_id %||% meta$id)
  input_sha <- stpd_ui_state_valid_sha256(meta$input_sha256)
  scope <- stpd_ui_state_scope_hashes(ds, train_names)
  data_sha <- scope$data_sha256
  manual_sha <- scope$manual_truth_sha256
  auto_sha <- scope$auto_output_sha256
  review_sha <- if (valid) {
    stpd_ui_state_sha256(stpd_ui_state_review_payload(ds))
  } else {
    ""
  }
  detector_context_sha <- scope$detector_context_sha256
  structure(
    list(
      schema_version = stpd_ui_state_schema_version(),
      dataset_id = id,
      dataset_name = stpd_ui_state_chr(meta$display_name %||% meta$name),
      input_sha256 = input_sha,
      data_sha256 = data_sha,
      auto_output_sha256 = auto_sha,
      detector_context_sha256 = detector_context_sha,
      manual_truth_sha256 = manual_sha,
      review_truth_sha256 = review_sha,
      total_train_n = as.integer(length(train_names)),
      train_names = train_names,
      verifiable = isTRUE(valid) && nzchar(data_sha)
    ),
    class = c("stpd_ui_dataset_identity", "list")
  )
}

stpd_ui_state_run_metadata <- function(ds) {
  if (!is.list(ds) || !is.list(ds$results)) return(data.frame())
  public <- ds$results$run_metadata_public %||% data.frame()
  internal <- ds$results$run_metadata %||% data.frame()
  if (is.data.frame(public) && nrow(public) > 0L) return(public[1L, , drop = FALSE])
  if (is.data.frame(internal) && nrow(internal) > 0L) return(internal[1L, , drop = FALSE])
  data.frame()
}

stpd_ui_state_metadata_value <- function(metadata, name, default = "") {
  if (!is.data.frame(metadata) || nrow(metadata) == 0L ||
      !(name %in% names(metadata))) return(default)
  metadata[[name]][1]
}

stpd_ui_state_has_detector_output <- function(ds) {
  metadata <- stpd_ui_state_run_metadata(ds)
  if (nrow(metadata) > 0L) return(TRUE)
  if (!is.list(ds) || !is.list(ds$results)) return(FALSE)
  result_names <- intersect(
    names(ds$results),
    c(
      "event_ledger", "event_audit", "candidate_ledger", "threshold_table",
      "result_consistency", "scientific_validation_summary", "detection_mode"
    )
  )
  any(vapply(result_names, function(name) !is.null(ds$results[[name]]), logical(1)))
}

stpd_ui_run_identity <- function(
    ds, params = NULL, dataset_id = NULL, selected_trains = NULL,
    dataset_identity = NULL) {
  identity <- dataset_identity %||% stpd_ui_dataset_identity(ds, dataset_id)
  metadata <- stpd_ui_state_run_metadata(ds)
  meta_selected <- stpd_ui_state_parse_trains(
    stpd_ui_state_metadata_value(metadata, "selected_trains")
  )
  selected <- stpd_ui_state_parse_trains(selected_trains)
  if (length(selected) == 0L) selected <- meta_selected
  selected_n <- stpd_ui_state_int(
    stpd_ui_state_metadata_value(metadata, "selected_train_n")
  )
  if (length(selected) > 0L) selected_n <- length(selected)
  total_n <- stpd_ui_state_int(
    stpd_ui_state_metadata_value(metadata, "total_train_n"),
    identity$total_train_n
  )
  if (is.na(total_n) || total_n < 0L) total_n <- identity$total_train_n
  if (is.na(selected_n) && stpd_ui_state_has_detector_output(ds) &&
      total_n > 0L && length(selected) == 0L) {
    # Old metadata without an explicit scope is not silently treated as a
    # whole-dataset run; the state evaluator will mark it unverifiable.
    selected_n <- NA_integer_
  }
  scope <- stpd_ui_state_scope_hashes(ds, selected)
  effective_hash <- stpd_ui_state_valid_sha256(
    stpd_ui_state_metadata_value(metadata, "params_hash")
  )
  if (!nzchar(effective_hash) && is.list(ds$params_effective)) {
    effective_hash <- stpd_ui_state_params_sha256(ds$params_effective)
  }
  ui_hash <- stpd_ui_state_params_sha256(params)
  if (!nzchar(ui_hash)) ui_hash <- effective_hash
  run_input_sha <- stpd_ui_state_valid_sha256(
    stpd_ui_state_metadata_value(metadata, "input_sha256")
  )
  if (!nzchar(run_input_sha)) run_input_sha <- identity$input_sha256
  run_dataset_name <- stpd_ui_state_chr(
    stpd_ui_state_metadata_value(metadata, "dataset_name"),
    identity$dataset_name
  )
  has_run <- stpd_ui_state_has_detector_output(ds)
  detector_output_identity <- if (
      exists("stpd_ui_detector_output_identity", mode = "function",
             inherits = TRUE) && has_run) {
    tryCatch(
      stpd_ui_detector_output_identity(ds, selected_trains = selected),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  structure(
    list(
      schema_version = stpd_ui_state_schema_version(),
      has_run = has_run,
      run_id = stpd_ui_state_chr(
        stpd_ui_state_metadata_value(metadata, "run_id")
      ),
      dataset_id = stpd_ui_state_chr(identity$dataset_id),
      dataset_name = run_dataset_name,
      input_sha256 = run_input_sha,
      scope_hash_version = "selected_trains_v1",
      data_sha256 = stpd_ui_state_chr(scope$data_sha256),
      auto_output_sha256 = stpd_ui_state_chr(scope$auto_output_sha256),
      detector_context_sha256 = stpd_ui_state_chr(
        scope$detector_context_sha256
      ),
      effective_params_sha256 = effective_hash,
      ui_params_sha256 = ui_hash,
      manual_truth_sha256 = stpd_ui_state_chr(scope$manual_truth_sha256),
      review_truth_sha256 = stpd_ui_state_chr(identity$review_truth_sha256),
      selected_train_n = as.integer(selected_n),
      selected_trains = selected,
      total_train_n = as.integer(total_n),
      detector_output_identity = detector_output_identity,
      verifiable = isTRUE(identity$verifiable) &&
        isTRUE(scope$valid) && nzchar(stpd_ui_state_chr(scope$data_sha256)) &&
        nzchar(ui_hash) &&
        !is.na(selected_n) && selected_n >= 0L &&
        (!has_run || (selected_n > 0L && length(selected) == selected_n))
    ),
    class = c("stpd_ui_run_identity", "list")
  )
}

stpd_ui_status_record <- function(
    code, level, title, detail, scope_n = NA_integer_,
    scope_total = NA_integer_, selected_trains = character(),
    reason_codes = code, verifiable = TRUE) {
  scope_n <- stpd_ui_state_int(scope_n)
  scope_total <- stpd_ui_state_int(scope_total)
  scope_label <- if (!is.na(scope_n) && !is.na(scope_total) &&
      scope_n >= 0L && scope_total >= 0L) {
    paste0(scope_n, "/", scope_total)
  } else {
    "?/?"
  }
  structure(
    list(
      code = stpd_ui_state_chr(code),
      level = stpd_ui_state_chr(level),
      title = stpd_ui_state_chr(title),
      detail = stpd_ui_state_chr(detail),
      scope_n = as.integer(scope_n),
      scope_total = as.integer(scope_total),
      scope_label = scope_label,
      selected_trains = stpd_ui_state_parse_trains(selected_trains),
      reason_codes = unique(as.character(reason_codes)),
      verifiable = isTRUE(verifiable)
    ),
    class = c("stpd_ui_status", "list")
  )
}

stpd_ui_run_state <- function(
    ds, params = NULL, dataset_id = NULL, run_identity = NULL,
    dataset_identity = NULL) {
  current <- dataset_identity %||% stpd_ui_dataset_identity(ds, dataset_id)
  run <- run_identity %||% stpd_ui_run_identity(
    ds, params = params, dataset_id = dataset_id,
    dataset_identity = current
  )
  scope_n <- stpd_ui_state_int(run$selected_train_n)
  scope_total <- stpd_ui_state_int(current$total_train_n)
  selected <- stpd_ui_state_parse_trains(run$selected_trains)
  current_scope <- stpd_ui_state_scope_hashes(ds, selected)
  if (!isTRUE(run$has_run)) {
    return(stpd_ui_status_record(
      "run_not_started", "neutral", "\u5C1A\u672A\u8FD0\u884C",
      "\u5F53\u524D\u6570\u636E\u96C6\u8FD8\u6CA1\u6709\u68C0\u6D4B\u5668\u8FD0\u884C\u7ED3\u679C\u3002",
      0L, scope_total, character(), "not_started", FALSE
    ))
  }

  current_id <- stpd_ui_state_chr(current$dataset_id)
  run_id <- stpd_ui_state_chr(run$dataset_id)
  current_name <- stpd_ui_state_chr(current$dataset_name)
  run_name <- stpd_ui_state_chr(run$dataset_name)
  explicit_foreign <- nzchar(current_id) && nzchar(run_id) &&
    !identical(current_id, run_id)
  inferred_foreign <- (!nzchar(current_id) || !nzchar(run_id)) &&
    nzchar(current_name) && nzchar(run_name) && !identical(current_name, run_name)
  if (explicit_foreign || inferred_foreign) {
    return(stpd_ui_status_record(
      "run_foreign_dataset", "danger", "\u6765\u81EA\u5176\u4ED6\u6570\u636E\u96C6",
      "\u8BE5\u7ED3\u679C\u4E0D\u5C5E\u4E8E\u5F53\u524D\u6570\u636E\u96C6\uFF0C\u4E0D\u80FD\u4F5C\u4E3A\u5F53\u524D\u7ED3\u679C\u4F7F\u7528\u3002",
      scope_n, scope_total, selected, "foreign_dataset", TRUE
    ))
  }

  # Validate the run scope before computing or comparing scoped payloads.
  # A detector run cannot legitimately cover zero trains; accepting such a
  # snapshot would make an empty scope hash look like a current partial run.
  scope_identity_invalid <- !isTRUE(run$verifiable) ||
    is.na(scope_n) || is.na(scope_total) || scope_n <= 0L ||
    scope_total <= 0L || scope_n > scope_total ||
    length(selected) != scope_n
  if (scope_identity_invalid) {
    return(stpd_ui_status_record(
      "run_identity_unverifiable", "warning", "\u65e0\u6cd5\u9a8c\u8bc1\u7ed3\u679c\u8eab\u4efd",
      "\u7ed3\u679c\u7f3a\u5c11\u5b8c\u6574\u7684\u6570\u636e\u3001\u53c2\u6570\u6216 train \u8303\u56f4\u5feb\u7167\uff0c\u4e0d\u80fd\u786e\u8ba4\u5b83\u662f\u5426\u4ecd\u4e3a\u5f53\u524d\u7ed3\u679c\u3002",
      scope_n, scope_total, selected, "identity_unverifiable", FALSE
    ))
  }

  data_reasons <- character()
  current_input <- stpd_ui_state_chr(current$input_sha256)
  run_input <- stpd_ui_state_chr(run$input_sha256)
  if (nzchar(current_input) && nzchar(run_input) &&
      !identical(current_input, run_input)) {
    data_reasons <- c(data_reasons, "input_sha256_changed")
  }
  current_data <- stpd_ui_state_chr(current_scope$data_sha256)
  run_data <- stpd_ui_state_chr(run$data_sha256)
  if (nzchar(current_data) && nzchar(run_data) &&
      !identical(current_data, run_data)) {
    data_reasons <- c(data_reasons, "spike_data_changed")
  }
  current_params <- stpd_ui_state_params_sha256(params)
  if (!nzchar(current_params) && is.list(ds$params_effective)) {
    current_params <- stpd_ui_state_params_sha256(ds$params_effective)
  }
  run_params <- stpd_ui_state_chr(run$ui_params_sha256)
  params_changed <- nzchar(current_params) && nzchar(run_params) &&
    !identical(current_params, run_params)
  if (length(data_reasons) > 0L) {
    reasons <- c("data_changed", data_reasons)
    detail <- "\u8F93\u5165\u6570\u636E\u5728\u68C0\u6D4B\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u3002"
    if (params_changed) {
      reasons <- c(reasons, "params_changed")
      detail <- paste0(detail, " \u5F53\u524D\u53C2\u6570\u4E5F\u5DF2\u6539\u53D8\u3002")
    }
    return(stpd_ui_status_record(
      "run_data_changed", "warning", "\u6570\u636E\u5DF2\u6539\u53D8", detail,
      scope_n, scope_total, selected, reasons, TRUE
    ))
  }
  if (params_changed) {
    return(stpd_ui_status_record(
      "run_params_changed", "warning", "\u53C2\u6570\u5DF2\u6539\u53D8",
      "\u5F53\u524D\u53C2\u6570\u4E0E\u8BE5\u6B21\u8FD0\u884C\u7684\u53C2\u6570\u5FEB\u7167\u4E0D\u540C\uFF1B\u73B0\u6709\u7ED3\u679C\u4FDD\u7559\uFF0C\u4F46\u9700\u8981\u91CD\u65B0\u8FD0\u884C\u3002",
      scope_n, scope_total, selected, "params_changed", TRUE
    ))
  }

  current_context <- stpd_ui_state_chr(
    current_scope$detector_context_sha256
  )
  run_context <- stpd_ui_state_chr(run$detector_context_sha256)
  if (nzchar(current_context) && nzchar(run_context) &&
      !identical(current_context, run_context)) {
    return(stpd_ui_status_record(
      "run_detector_context_changed", "warning", "Train \u7EA7\u68C0\u6D4B\u8BBE\u7F6E\u5DF2\u6539\u53D8",
      "Train-specific \u9608\u503C\u6216\u8303\u56F4\u5728\u8FD0\u884C\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u8FD0\u884C\u3002",
      scope_n, scope_total, selected, "detector_context_changed", TRUE
    ))
  }

  current_auto <- stpd_ui_state_chr(current_scope$auto_output_sha256)
  run_auto <- stpd_ui_state_chr(run$auto_output_sha256)
  if (nzchar(current_auto) && nzchar(run_auto) &&
      !identical(current_auto, run_auto)) {
    return(stpd_ui_status_record(
      "run_output_modified", "warning", "AUTO \u7ED3\u679C\u5DF2\u624B\u52A8\u6539\u52A8",
      "AUTO \u6807\u7B7E\u6216\u5206\u6570\u5728\u68C0\u6D4B\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u4E8B\u4EF6\u8868\u548C\u8BCA\u65AD\u53EF\u80FD\u5DF2\u4E0D\u540C\u6B65\u3002\u8BF7\u91CD\u65B0\u8FD0\u884C\u3002",
      scope_n, scope_total, selected, "auto_output_changed", TRUE
    ))
  }

  current_manual <- stpd_ui_state_chr(current_scope$manual_truth_sha256)
  run_manual <- stpd_ui_state_chr(run$manual_truth_sha256)
  if (nzchar(current_manual) && nzchar(run_manual) &&
      !identical(current_manual, run_manual)) {
    return(stpd_ui_status_record(
      "run_manual_context_changed", "warning", "MANUAL \u8BC1\u636E\u5DF2\u6539\u53D8",
      "MANUAL/NOT-burst \u6807\u7B7E\u5728\u68C0\u6D4B\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u5F53\u524D AUTO \u4ECD\u4FDD\u7559\uFF0C\u4F46 manual-aware \u8FD0\u884C\u4E0E\u4E0B\u6E38\u5BA1\u8BA1\u9700\u8981\u91CD\u65B0\u540C\u6B65\u3002",
      scope_n, scope_total, selected, "manual_context_changed", TRUE
    ))
  }

  identity_missing <- !nzchar(stpd_ui_state_chr(run$run_id)) ||
    !isTRUE(current$verifiable) || !isTRUE(current_scope$valid) ||
    !nzchar(run_data) || !nzchar(run_auto) ||
    !nzchar(run_context) || !nzchar(run_manual) ||
    !nzchar(current_params) || !nzchar(run_params) ||
    is.na(scope_n) || is.na(scope_total) || scope_n < 0L ||
    scope_total < 0L || scope_n > scope_total ||
    scope_n == 0L || length(selected) != scope_n
  if (identity_missing) {
    return(stpd_ui_status_record(
      "run_identity_unverifiable", "warning", "\u65E0\u6CD5\u9A8C\u8BC1\u7ED3\u679C\u8EAB\u4EFD",
      "\u7ED3\u679C\u7F3A\u5C11\u5B8C\u6574\u7684\u6570\u636E\u3001\u53C2\u6570\u6216 train \u8303\u56F4\u5FEB\u7167\uFF0C\u4E0D\u80FD\u786E\u8BA4\u5B83\u662F\u5426\u4ECD\u4E3A\u5F53\u524D\u7ED3\u679C\u3002",
      scope_n, scope_total, selected, "identity_unverifiable", FALSE
    ))
  }
  if (scope_n < scope_total) {
    return(stpd_ui_status_record(
      "run_partial_current", "info", "\u90E8\u5206 train \u7ED3\u679C\u4E3A\u5F53\u524D",
      paste0("\u5F53\u524D\u7ED3\u679C\u8986\u76D6 ", scope_n, "/", scope_total,
             " \u6761 train\uFF1B\u672A\u8FD0\u884C\u7684 train \u4E0D\u5E94\u663E\u793A\u4E3A\u9634\u6027\u3002"),
      scope_n, scope_total, selected, "partial_scope", TRUE
    ))
  }
  stpd_ui_status_record(
    "run_current", "success", "\u7ED3\u679C\u4E3A\u5F53\u524D",
    paste0("\u6570\u636E\u3001\u53C2\u6570\u548C\u8FD0\u884C\u8303\u56F4\u4E00\u81F4\uFF08", scope_n, "/", scope_total, " \u6761 train\uFF09\u3002"),
    scope_n, scope_total, selected, character(), TRUE
  )
}

stpd_ui_formal_export_state <- function(
    ds, params = NULL, dataset_id = NULL, run_identity = NULL,
    dataset_identity = NULL, expected_detector_output_identity = NULL) {
  current <- dataset_identity %||% stpd_ui_dataset_identity(ds, dataset_id)
  stored <- run_identity %||% stpd_ui_run_identity(
    ds, params = params, dataset_id = dataset_id,
    dataset_identity = current
  )
  run_state <- stpd_ui_run_state(
    ds, params = params, dataset_id = dataset_id,
    run_identity = stored, dataset_identity = current
  )
  scope_n <- stpd_ui_state_int(run_state$scope_n)
  scope_total <- stpd_ui_state_int(run_state$scope_total)
  selected <- stpd_ui_state_parse_trains(run_state$selected_trains)
  block <- function(code, detail) {
    structure(
      list(
        eligible = FALSE,
        code = stpd_ui_state_chr(code),
        title = "\u6B63\u5F0F\u7ED3\u679C ZIP \u6682\u4E0D\u53EF\u5BFC\u51FA",
        detail = stpd_ui_state_chr(detail),
        run_state = run_state,
        run_id = stpd_ui_state_chr(stored$run_id),
        params_sha256 = stpd_ui_state_chr(stored$effective_params_sha256),
        selected_train_n = as.integer(scope_n),
        total_train_n = as.integer(scope_total),
        selected_trains = selected
      ),
      class = c("stpd_ui_formal_export_state", "list")
    )
  }

  if (is.null(params) || !is.list(params)) {
    return(block(
      "formal_export_params_unavailable",
      "\u5F53\u524D\u754C\u9762\u53C2\u6570\u65E0\u6CD5\u5F62\u6210\u6709\u6548\u5FEB\u7167\u3002\u8BF7\u68C0\u67E5\u53C2\u6570\u5E76\u91CD\u65B0\u8FD0\u884C\u5168\u90E8 trains \u540E\u518D\u5BFC\u51FA\u3002"
    ))
  }

  if (!identical(run_state$code, "run_current")) {
    instruction <- if (identical(run_state$code, "run_partial_current")) {
      "\u5F53\u524D\u53EA\u8FD0\u884C\u4E86\u90E8\u5206 trains\u3002\u8BF7\u53D6\u6D88\u201C\u4EC5\u68C0\u6D4B\u5F53\u524D\u53EF\u89C1 trains\u201D\uFF0C\u91CD\u65B0\u8FD0\u884C\u5168\u90E8 trains \u540E\u518D\u5BFC\u51FA\u3002"
    } else {
      paste0(
        "\u5F53\u524D\u68C0\u6D4B\u72B6\u6001\u4E3A\u201C", stpd_ui_state_chr(run_state$title, run_state$code),
        "\u201D\u3002\u8BF7\u91CD\u65B0\u8FD0\u884C\u5168\u90E8 trains\uFF0C\u786E\u8BA4\u72B6\u6001\u53D8\u4E3A\u201C\u7ED3\u679C\u4E3A\u5F53\u524D\u201D\u540E\u518D\u5BFC\u51FA\u3002"
      )
    }
    return(block(paste0("formal_export_", run_state$code), instruction))
  }

  # Re-read the persisted run metadata rather than trusting only the UI's
  # captured snapshot.  This prevents a foreign/restored result object from
  # being combined with an otherwise current-looking in-session identity.
  live <- stpd_ui_run_identity(
    ds, params = params, dataset_id = dataset_id,
    dataset_identity = current
  )
  same_scope <- identical(
    stpd_ui_state_parse_trains(live$selected_trains),
    stpd_ui_state_parse_trains(stored$selected_trains)
  ) && identical(
    stpd_ui_state_int(live$selected_train_n),
    stpd_ui_state_int(stored$selected_train_n)
  )
  identity_fields <- c(
    "run_id", "dataset_id", "dataset_name", "input_sha256",
    "data_sha256", "auto_output_sha256", "detector_context_sha256",
    "manual_truth_sha256", "effective_params_sha256", "ui_params_sha256"
  )
  identity_match <- all(vapply(identity_fields, function(field) {
    identical(
      stpd_ui_state_chr(live[[field]]),
      stpd_ui_state_chr(stored[[field]])
    )
  }, logical(1)))
  actual_effective_sha <- if (is.list(ds$params_effective)) {
    stpd_ui_state_params_sha256(ds$params_effective)
  } else {
    ""
  }
  current_params_sha <- stpd_ui_state_params_sha256(params)
  metadata_current <- nzchar(stpd_ui_state_chr(live$run_id)) &&
    nzchar(stpd_ui_state_chr(live$effective_params_sha256)) &&
    nzchar(actual_effective_sha) && nzchar(current_params_sha) &&
    identity_match && same_scope &&
    identical(stpd_ui_state_int(live$total_train_n),
              stpd_ui_state_int(stored$total_train_n)) &&
    identical(actual_effective_sha,
              stpd_ui_state_chr(stored$effective_params_sha256)) &&
    identical(current_params_sha,
              stpd_ui_state_chr(stored$ui_params_sha256))
  if (!metadata_current) {
    return(block(
      "formal_export_run_metadata_mismatch",
      "\u7ED3\u679C\u5BF9\u8C61\u7684\u8FD0\u884C\u8EAB\u4EFD\u4E0E\u5F53\u524D\u754C\u9762\u5FEB\u7167\u4E0D\u4E00\u81F4\u3002\u8BF7\u91CD\u65B0\u8FD0\u884C\u5168\u90E8 trains \u540E\u518D\u5BFC\u51FA\u3002"
    ))
  }

  expected_output <- expected_detector_output_identity %||%
    stored$detector_output_identity %||% NULL
  if (!exists("stpd_ui_detector_output_state", mode = "function",
              inherits = TRUE)) {
    return(block(
      "formal_export_detector_output_validator_unavailable",
      "\u5F53\u524D\u5305\u7F3A\u5C11\u68C0\u6D4B\u4EA7\u7269\u5B8C\u6574\u6027\u9A8C\u8BC1\u5668\uFF0C\u4E0D\u80FD\u751F\u6210\u6B63\u5F0F\u5BFC\u51FA\u3002"
    ))
  }
  output_state <- tryCatch(
    stpd_ui_detector_output_state(
      ds, expected_identity = expected_output,
      selected_trains = stored$selected_trains
    ),
    error = function(e) list(
      current = FALSE,
      code = "detector_output_identity_unverifiable",
      detail = conditionMessage(e)
    )
  )
  if (!isTRUE(output_state$current)) {
    return(block(
      paste0(
        "formal_export_",
        stpd_ui_state_chr(
          output_state$code, "detector_output_identity_unverifiable"
        )
      ),
      paste0(
        "\u68C0\u6D4B\u4EA7\u7269\u8868\u4E0E\u8FD0\u884C\u5FEB\u7167\u4E0D\u4E00\u81F4\uFF1A",
        stpd_ui_state_chr(
          output_state$detail,
          "\u65E0\u6CD5\u9A8C\u8BC1 candidate/event \u7ED3\u679C\u8868\u3002"
        ),
        " \u8BF7\u91CD\u65B0\u8FD0\u884C\u5168\u90E8 trains \u540E\u518D\u5BFC\u51FA\u3002"
      )
    ))
  }

  structure(
    list(
      eligible = TRUE,
      code = "formal_export_ready",
      title = "\u53EF\u5BFC\u51FA\u5F53\u524D\u5168\u91CF\u7ED3\u679C",
      detail = paste0(
        "\u5C06\u5BFC\u51FA run ", stpd_ui_state_chr(stored$run_id), "\uFF0C\u8303\u56F4 ",
        scope_n, "/", scope_total, " \u6761 train\u3002"
      ),
      run_state = run_state,
      run_id = stpd_ui_state_chr(stored$run_id),
      params_sha256 = stpd_ui_state_chr(stored$effective_params_sha256),
      detector_output_sha256 = stpd_ui_state_chr(
        output_state$current_sha256
      ),
      selected_train_n = as.integer(scope_n),
      total_train_n = as.integer(scope_total),
      selected_trains = selected
    ),
    class = c("stpd_ui_formal_export_state", "list")
  )
}

stpd_ui_state_validation_metadata <- function(validation) {
  if (!is.list(validation)) return(data.frame())
  metadata <- validation$metadata %||% validation$meta %||% data.frame()
  if (is.data.frame(metadata) && nrow(metadata) > 0L) {
    metadata[1L, , drop = FALSE]
  } else {
    data.frame()
  }
}

stpd_ui_validation_report_sha256 <- function(validation) {
  if (!is.list(validation) || length(validation) == 0L) return("")
  report <- validation
  # The identity is attached after the report is created. Excluding it keeps
  # the report hash acyclic and lets export-time verification recompute the
  # exact same payload from a persisted report.
  report[["ui_identity"]] <- NULL
  tryCatch(
    stpd_ui_state_valid_sha256(stpd_ui_state_sha256(report)),
    error = function(e) ""
  )
}

stpd_ui_validation_identity <- function(
    validation, dataset_identity = NULL, run_identity = NULL,
    truth_sha256 = NULL, selected_trains = NULL,
    truth_dependencies = NULL,
    source_kind = c("run_bound", "shadow_independent"),
    source_params_sha256 = NULL, dataset = NULL) {
  source_kind <- match.arg(source_kind)
  requires_active_run <- identical(source_kind, "run_bound")
  has_validation <- is.list(validation) && length(validation) > 0L
  dataset_identity <- dataset_identity %||% list()
  run_identity <- run_identity %||% list()
  metadata <- stpd_ui_state_validation_metadata(validation)
  selected_supplied <- !is.null(selected_trains)
  selected <- stpd_ui_state_parse_trains(selected_trains)
  if (!selected_supplied && length(selected) == 0L) {
    selected <- stpd_ui_state_parse_trains(
      stpd_ui_state_metadata_value(metadata, "selected_trains")
    )
  }
  if (!selected_supplied && length(selected) == 0L && is.list(validation) &&
      is.data.frame(validation$split) && "train" %in% names(validation$split)) {
    selected <- stpd_ui_state_parse_trains(validation$split$train)
  }
  if (!selected_supplied && length(selected) == 0L) {
    selected <- stpd_ui_state_parse_trains(run_identity$selected_trains)
  }
  scoped_snapshot <- is.list(dataset) && is.list(dataset$trains)
  scope <- if (scoped_snapshot) {
    stpd_ui_state_scope_hashes(dataset, selected)
  } else {
    list(
      valid = isTRUE(dataset_identity$verifiable),
      data_sha256 = stpd_ui_state_chr(dataset_identity$data_sha256),
      manual_truth_sha256 = stpd_ui_state_chr(
        dataset_identity$manual_truth_sha256
      ),
      detector_context_sha256 = stpd_ui_state_chr(
        dataset_identity$detector_context_sha256
      )
    )
  }
  inferred_dependencies <- if ("prediction_source" %in% names(metadata)) {
    "external"
  } else {
    "manual"
  }
  dependencies <- tolower(as.character(truth_dependencies %||% inferred_dependencies))
  dependencies <- sort(unique(intersect(
    dependencies, c("manual", "review", "external")
  )), method = "radix")
  external_truth <- stpd_ui_state_valid_sha256(truth_sha256)
  if (!nzchar(external_truth)) {
    external_truth <- stpd_ui_state_valid_sha256(
      stpd_ui_state_metadata_value(metadata, "truth_sha256")
    )
  }
  report_sha256 <- stpd_ui_validation_report_sha256(validation)
  validation_id <- stpd_ui_state_chr(
    stpd_ui_state_metadata_value(metadata, "validation_run_id")
  )
  if (!nzchar(validation_id)) {
    validation_id <- stpd_ui_state_chr(
      stpd_ui_state_metadata_value(metadata, "schema_version")
    )
  }
  if (!nzchar(validation_id) && has_validation) {
    validation_id <- paste0(
      "validation_", substr(report_sha256, 1L, 24L)
    )
  }
  source_run_id <- if (requires_active_run) {
    value <- stpd_ui_state_chr(
      stpd_ui_state_metadata_value(metadata, "run_id")
    )
    if (!nzchar(value)) value <- stpd_ui_state_chr(run_identity$run_id)
    value
  } else {
    ""
  }
  source_params <- stpd_ui_state_valid_sha256(source_params_sha256)
  if (!nzchar(source_params)) {
    source_params <- stpd_ui_state_valid_sha256(
      stpd_ui_state_metadata_value(metadata, "params_hash")
    )
  }
  if (!nzchar(source_params)) {
    source_params <- stpd_ui_state_chr(if (requires_active_run) {
      run_identity$effective_params_sha256
    } else {
      run_identity$ui_params_sha256
    })
  }
  total_n <- stpd_ui_state_int(dataset_identity$total_train_n)
  selected_n <- stpd_ui_state_int(
    stpd_ui_state_metadata_value(metadata, "selected_train_n")
  )
  if (selected_supplied || length(selected) > 0L) selected_n <- length(selected)
  structure(
    list(
      schema_version = stpd_ui_state_schema_version(),
      has_validation = has_validation,
      validation_id = validation_id,
      report_sha256 = report_sha256,
      dataset_id = stpd_ui_state_chr(dataset_identity$dataset_id),
      dataset_name = stpd_ui_state_chr(dataset_identity$dataset_name),
      data_sha256 = stpd_ui_state_chr(scope$data_sha256),
      scope_hash_version = if (scoped_snapshot) {
        "selected_trains_v1"
      } else {
        "full_dataset_legacy"
      },
      source_kind = source_kind,
      requires_active_run = requires_active_run,
      source_run_id = source_run_id,
      source_params_sha256 = source_params,
      source_detector_context_sha256 = stpd_ui_state_chr(
        scope$detector_context_sha256
      ),
      prediction_artifact_sha256 = stpd_ui_state_valid_sha256(
        stpd_ui_state_metadata_value(metadata, "prediction_artifact_sha256")
      ),
      manual_truth_sha256 = stpd_ui_state_chr(
        scope$manual_truth_sha256
      ),
      review_truth_sha256 = stpd_ui_state_chr(
        dataset_identity$review_truth_sha256
      ),
      external_truth_sha256 = external_truth,
      truth_dependencies = dependencies,
      selected_train_n = as.integer(selected_n),
      selected_trains = selected,
      total_train_n = as.integer(total_n),
      verifiable = has_validation && nzchar(validation_id) &&
        nzchar(report_sha256) &&
        isTRUE(scope$valid) && nzchar(stpd_ui_state_chr(scope$data_sha256)) &&
        nzchar(source_params) &&
        !is.na(selected_n)
    ),
    class = c("stpd_ui_validation_identity", "list")
  )
}

stpd_ui_validation_state <- function(
    ds, params = NULL, dataset_id = NULL, run_identity = NULL,
    validation_identity = NULL, current_truth_sha256 = NULL,
    dataset_identity = NULL) {
  current <- dataset_identity %||% stpd_ui_dataset_identity(ds, dataset_id)
  run <- run_identity %||% stpd_ui_run_identity(
    ds, params = params, dataset_id = dataset_id,
    dataset_identity = current
  )
  validation <- validation_identity %||% list(has_validation = FALSE)
  scope_n <- stpd_ui_state_int(validation$selected_train_n)
  scope_total <- stpd_ui_state_int(current$total_train_n)
  selected <- stpd_ui_state_parse_trains(validation$selected_trains)
  current_scope <- if (identical(
      validation$scope_hash_version, "selected_trains_v1")) {
    stpd_ui_state_scope_hashes(ds, selected)
  } else {
    list(
      valid = isTRUE(current$verifiable),
      data_sha256 = stpd_ui_state_chr(current$data_sha256),
      manual_truth_sha256 = stpd_ui_state_chr(current$manual_truth_sha256),
      detector_context_sha256 = stpd_ui_state_chr(
        current$detector_context_sha256
      )
    )
  }
  if (!isTRUE(validation$has_validation)) {
    return(stpd_ui_status_record(
      "validation_not_started", "neutral", "\u5C1A\u672A\u9A8C\u8BC1",
      "\u5F53\u524D\u6570\u636E\u96C6\u8FD8\u6CA1\u6709\u9A8C\u8BC1\u7ED3\u679C\u3002",
      0L, scope_total, character(), "not_started", FALSE
    ))
  }
  current_id <- stpd_ui_state_chr(current$dataset_id)
  validation_id <- stpd_ui_state_chr(validation$dataset_id)
  current_name <- stpd_ui_state_chr(current$dataset_name)
  validation_name <- stpd_ui_state_chr(validation$dataset_name)
  foreign <- (nzchar(current_id) && nzchar(validation_id) &&
    !identical(current_id, validation_id)) ||
    ((!nzchar(current_id) || !nzchar(validation_id)) &&
      nzchar(current_name) && nzchar(validation_name) &&
      !identical(current_name, validation_name))
  if (foreign) {
    return(stpd_ui_status_record(
      "validation_foreign_dataset", "danger", "\u9A8C\u8BC1\u6765\u81EA\u5176\u4ED6\u6570\u636E\u96C6",
      "\u8BE5\u9A8C\u8BC1\u62A5\u544A\u4E0D\u5C5E\u4E8E\u5F53\u524D\u6570\u636E\u96C6\u3002",
      scope_n, scope_total, selected, "foreign_dataset", TRUE
    ))
  }
  current_data <- stpd_ui_state_chr(current_scope$data_sha256)
  validation_data <- stpd_ui_state_chr(validation$data_sha256)
  if (nzchar(current_data) && nzchar(validation_data) &&
      !identical(current_data, validation_data)) {
    return(stpd_ui_status_record(
      "validation_data_changed", "warning", "\u9A8C\u8BC1\u6240\u7528\u6570\u636E\u5DF2\u6539\u53D8",
      "\u5F53\u524D spike \u6570\u636E\u4E0E\u9A8C\u8BC1\u65F6\u7684\u6570\u636E\u5FEB\u7167\u4E0D\u540C\uFF1B\u8BE5\u62A5\u544A\u5DF2\u8FC7\u671F\u3002",
      scope_n, scope_total, selected, "data_changed", TRUE
    ))
  }
  requires_active_run <- !identical(validation$requires_active_run, FALSE)
  current_run_state <- if (requires_active_run) {
    stpd_ui_run_state(
      ds, params = params, dataset_id = dataset_id, run_identity = run,
      dataset_identity = current
    )
  } else {
    NULL
  }
  source_run <- stpd_ui_state_chr(validation$source_run_id)
  active_run <- stpd_ui_state_chr(run$run_id)
  source_params <- stpd_ui_state_chr(validation$source_params_sha256)
  active_params <- stpd_ui_state_params_sha256(params)
  if (!nzchar(active_params) && is.list(ds$params_effective)) {
    active_params <- stpd_ui_state_params_sha256(ds$params_effective)
  }
  source_context <- stpd_ui_state_chr(
    validation$source_detector_context_sha256
  )
  active_context <- stpd_ui_state_chr(
    current_scope$detector_context_sha256
  )
  parameter_or_context_mismatch <-
    (nzchar(source_params) && nzchar(active_params) &&
      !identical(source_params, active_params)) ||
    (nzchar(source_context) && nzchar(active_context) &&
      !identical(source_context, active_context))
  run_mismatch <- if (requires_active_run) {
    (nzchar(source_run) && nzchar(active_run) &&
      !identical(source_run, active_run)) ||
      parameter_or_context_mismatch ||
      current_run_state$code %in% c(
        "run_not_started", "run_data_changed", "run_params_changed",
        "run_detector_context_changed", "run_output_modified",
        "run_foreign_dataset"
      )
  } else {
    parameter_or_context_mismatch
  }
  if (run_mismatch) {
    return(stpd_ui_status_record(
      "validation_run_changed", "warning",
      if (requires_active_run) "\u68C0\u6D4B\u8FD0\u884C\u5DF2\u6539\u53D8" else "\u9A8C\u8BC1\u8BBE\u7F6E\u5DF2\u6539\u53D8",
      if (requires_active_run) {
        "\u9A8C\u8BC1\u62A5\u544A\u7ED1\u5B9A\u7684\u68C0\u6D4B\u8FD0\u884C\u3001\u53C2\u6570\u6216 Train \u7EA7\u8BBE\u7F6E\u5DF2\u4E0D\u518D\u662F\u5F53\u524D\u7248\u672C\u3002"
      } else {
        "\u72EC\u7ACB\u9A8C\u8BC1\u6240\u7528\u7684\u53C2\u6570\u6216 Train \u7EA7\u68C0\u6D4B\u8BBE\u7F6E\u5DF2\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u9A8C\u8BC1\u3002"
      },
      scope_n, scope_total, selected, "run_changed", TRUE
    ))
  }

  dependencies <- as.character(validation$truth_dependencies %||% character())
  manual_changed <- "manual" %in% dependencies &&
    nzchar(stpd_ui_state_chr(validation$manual_truth_sha256)) &&
    nzchar(stpd_ui_state_chr(current_scope$manual_truth_sha256)) &&
    !identical(
      stpd_ui_state_chr(validation$manual_truth_sha256),
      stpd_ui_state_chr(current_scope$manual_truth_sha256)
    )
  if (manual_changed) {
    return(stpd_ui_status_record(
      "validation_manual_truth_changed", "warning", "\u4EBA\u5DE5\u771F\u503C\u5DF2\u6539\u53D8",
      "MANUAL/NOT-burst \u771F\u503C\u5728\u9A8C\u8BC1\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u9A8C\u8BC1\u3002",
      scope_n, scope_total, selected, "manual_truth_changed", TRUE
    ))
  }
  review_changed <- "review" %in% dependencies &&
    nzchar(stpd_ui_state_chr(validation$review_truth_sha256)) &&
    nzchar(stpd_ui_state_chr(current$review_truth_sha256)) &&
    !identical(
      stpd_ui_state_chr(validation$review_truth_sha256),
      stpd_ui_state_chr(current$review_truth_sha256)
    )
  if (review_changed) {
    return(stpd_ui_status_record(
      "validation_review_truth_changed", "warning", "Review \u771F\u503C\u5DF2\u6539\u53D8",
      "Review \u786E\u8BA4\u6216\u64A4\u9500\u72B6\u6001\u5728\u9A8C\u8BC1\u540E\u53D1\u751F\u4E86\u6539\u53D8\uFF1B\u8BF7\u91CD\u65B0\u9A8C\u8BC1\u3002",
      scope_n, scope_total, selected, "review_truth_changed", TRUE
    ))
  }
  snapshot_truth <- stpd_ui_state_chr(validation$external_truth_sha256)
  current_truth <- stpd_ui_state_valid_sha256(current_truth_sha256)
  external_changed <- "external" %in% dependencies &&
    nzchar(snapshot_truth) && nzchar(current_truth) &&
    !identical(snapshot_truth, current_truth)
  if (external_changed) {
    return(stpd_ui_status_record(
      "validation_truth_changed", "warning", "\u9A8C\u8BC1\u771F\u503C\u5DF2\u6539\u53D8",
      "\u5916\u90E8\u53C2\u8003\u771F\u503C\u4E0E\u9A8C\u8BC1\u65F6\u7684\u771F\u503C\u5FEB\u7167\u4E0D\u540C\uFF1B\u8BF7\u91CD\u65B0\u9A8C\u8BC1\u3002",
      scope_n, scope_total, selected, "external_truth_changed", TRUE
    ))
  }

  dependency_unverifiable <- ("manual" %in% dependencies &&
      (!nzchar(stpd_ui_state_chr(validation$manual_truth_sha256)) ||
       !nzchar(stpd_ui_state_chr(current_scope$manual_truth_sha256)))) ||
    ("review" %in% dependencies &&
      (!nzchar(stpd_ui_state_chr(validation$review_truth_sha256)) ||
       !nzchar(stpd_ui_state_chr(current$review_truth_sha256)))) ||
    ("external" %in% dependencies &&
      (!nzchar(snapshot_truth) || !nzchar(current_truth)))
  identity_missing <- !isTRUE(validation$verifiable) ||
    !isTRUE(current$verifiable) || !isTRUE(current_scope$valid) ||
    (requires_active_run &&
      current_run_state$code == "run_identity_unverifiable") ||
    dependency_unverifiable || is.na(scope_n) || is.na(scope_total) ||
    scope_n < 0L || scope_total < 0L || scope_n > scope_total
  if (identity_missing) {
    return(stpd_ui_status_record(
      "validation_identity_unverifiable", "warning", "\u65E0\u6CD5\u9A8C\u8BC1\u62A5\u544A\u8EAB\u4EFD",
      "\u62A5\u544A\u7F3A\u5C11\u5B8C\u6574\u7684\u6570\u636E\u3001\u8FD0\u884C\u3001\u771F\u503C\u6216 train \u8303\u56F4\u5FEB\u7167\u3002",
      scope_n, scope_total, selected, "identity_unverifiable", FALSE
    ))
  }
  if (scope_n < scope_total) {
    return(stpd_ui_status_record(
      "validation_partial_current", "info", "\u90E8\u5206 train \u9A8C\u8BC1\u4E3A\u5F53\u524D",
      paste0("\u8BE5\u62A5\u544A\u8986\u76D6 ", scope_n, "/", scope_total, " \u6761 train\u3002"),
      scope_n, scope_total, selected, "partial_scope", TRUE
    ))
  }
  stpd_ui_status_record(
    "validation_current", "success", "\u9A8C\u8BC1\u4E3A\u5F53\u524D",
    paste0("\u6570\u636E\u3001\u8FD0\u884C\u3001\u771F\u503C\u548C\u9A8C\u8BC1\u8303\u56F4\u4E00\u81F4\uFF08", scope_n, "/",
           scope_total, " \u6761 train\uFF09\u3002"),
    scope_n, scope_total, selected, character(), TRUE
  )
}

stpd_ui_state_model <- function(
    ds, params = NULL, dataset_id = NULL, run_identity = NULL,
    validation_identity = NULL, current_truth_sha256 = NULL,
    dataset_identity = NULL) {
  current <- dataset_identity %||% stpd_ui_dataset_identity(ds, dataset_id)
  list(
    run = stpd_ui_run_state(
      ds, params = params, dataset_id = dataset_id,
      run_identity = run_identity, dataset_identity = current
    ),
    validation = stpd_ui_validation_state(
      ds, params = params, dataset_id = dataset_id,
      run_identity = run_identity,
      validation_identity = validation_identity,
      current_truth_sha256 = current_truth_sha256,
      dataset_identity = current
    )
  )
}
