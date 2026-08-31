# Descriptive recurrent parent-State product.
#
# This is a fifth, orthogonal post-detection layer.  It consumes canonical
# FINAL Events/Gaps and carrier States without changing any of those products.
# Recurrent regimes are algorithmic candidates, not biological ground truth.

stpd_event_regime_schema_version <- function() {
  "stpd_recurrent_parent_state_candidates_v2"
}

stpd_event_regime_result_key <- function() {
  "event_regimes"
}

stpd_event_regime_abort <- function(code, message) {
  condition <- structure(
    list(message = as.character(message)[1], call = NULL,
         code = as.character(code)[1]),
    class = c("stpd_event_regime_error", "error", "condition")
  )
  stop(condition)
}

stpd_event_regime_window_policy <- function() {
  list(
    max_interruption_isi_fraction = 0.90,
    max_interruption_duration_fraction = 0.95,
    max_single_interruption_isi_fraction = 0.50,
    max_single_interruption_duration_fraction = 0.70
  )
}

stpd_event_regime_policy_hash <- function() {
  window_policy <- stpd_event_regime_window_policy()
  stpd_multitrack_auto_hash(list(
    schema_version = stpd_event_regime_schema_version(),
    source = "validated_multitrack_final_only",
    burst_trigger = "three_consecutive_canonical_burst_events",
    pause_triggers = c(
      "three_high_specificity_long_pause_entities",
      "five_ordinary_accepted_pause_entities"
    ),
    selection = paste(
      "minimum_trigger_anchor|left_to_right_one_pass|fixed_anchor_cadence|",
      "child_used_at_most_once_within_regime_class|no_recursive_budget_growth"
    ),
    finite_window_policy = window_policy,
    boundaries = paste(
      "qc_or_acquisition_hard_only|canonical_pause_is_not_a_regime_boundary"
    ),
    carrier_states = "orthogonal_non_destructive_context",
    absolute_isi_threshold_used = FALSE,
    parent_semantics = paste(
      "state_domain|recurrent_bursting_state_or_recurrent_pause_state|",
      "canonical_children_preserved"
    ),
    authority = "descriptive_unvalidated_parent_state_candidate_only"
  ))
}

stpd_event_regime_empty_metadata <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(),
    authoritative = logical(), authority_scope = character(),
    biological_ground_truth = logical(), validation_status = character(),
    materialization_status = character(),
    parent_final_schema_version = character(),
    parent_final_product_sha256 = character(),
    absolute_isi_threshold_used = logical(),
    one_pass_nonrecursive = logical(), regimes_n = integer(),
    memberships_n = integer(), carrier_relationships_n = integer(),
    boundaries_n = integer(), product_sha256 = character(),
    stringsAsFactors = FALSE
  )
}

stpd_event_regime_empty_boundaries <- function() {
  data.frame(
    schema_version = character(), run_id = character(),
    params_hash = character(), train = character(),
    boundary_isi = integer(), boundary_class = character(),
    boundary_source = character(), stringsAsFactors = FALSE
  )
}

stpd_event_regime_empty_regimes <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(),
    authoritative = logical(), biological_ground_truth = logical(),
    train = character(), regime_id = character(), regime_class = character(),
    semantic_domain = character(), parent_state_class = character(),
    candidate_status = character(), validation_status = character(),
    trigger_route = character(), child_domain = character(),
    qc_segment_id = character(), start_isi = integer(), end_isi = integer(),
    envelope_n_isi = integer(), direct_support_isi_n = integer(),
    interruption_isi_n = integer(), interruption_fraction = double(),
    child_n = integer(), trigger_child_n = integer(),
    long_pause_child_n = integer(), accepted_pause_child_n = integer(),
    start_time_sec = double(), end_time_sec = double(), duration_sec = double(),
    direct_support_duration_sec = double(),
    interruption_duration_sec = double(),
    interruption_duration_fraction = double(),
    max_single_interruption_isi_fraction = double(),
    max_single_interruption_duration_fraction = double(),
    temporal_budget_pass = logical(),
    adjacent_child_gap_median_sec = double(),
    adjacent_child_gap_max_sec = double(), carrier_state_n = integer(),
    carrier_context = character(), carrier_state_classes = character(),
    absolute_isi_threshold_used = logical(),
    one_pass_nonrecursive = logical(), candidate_layer = character(),
    candidate_source = character(), stringsAsFactors = FALSE
  )
}

stpd_event_regime_empty_memberships <- function() {
  data.frame(
    schema_version = character(), run_id = character(),
    params_hash = character(), authoritative = logical(),
    train = character(), regime_id = character(), regime_class = character(),
    parent_state_class = character(),
    child_domain = character(), child_id = character(), child_order = integer(),
    child_role = character(), trigger_contributor = logical(),
    child_start_isi = integer(), child_end_isi = integer(),
    child_support_isi_n = integer(), non_destructive = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_event_regime_empty_carrier_relationships <- function() {
  data.frame(
    schema_version = character(), run_id = character(),
    params_hash = character(), authoritative = logical(),
    train = character(), regime_id = character(), state_id = character(),
    state_class = character(), state_family = character(),
    state_subtype = character(), relationship_type = character(),
    overlap_start_isi = integer(), overlap_end_isi = integer(),
    overlap_isi_n = integer(), regime_overlap_fraction = double(),
    state_overlap_fraction = double(), non_destructive = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_event_regime_empty_invariants <- function() {
  data.frame(
    schema_version = character(), run_id = character(),
    params_hash = character(), check_name = character(), status = character(),
    message = character(), stringsAsFactors = FALSE
  )
}

stpd_event_regime_empty_manifest <- function() {
  data.frame(
    schema_version = character(), run_id = character(),
    params_hash = character(), table_name = character(),
    row_count = integer(), column_count = integer(), column_types = character(),
    table_sha256 = character(), stringsAsFactors = FALSE
  )
}

stpd_event_regime_bind <- function(prototype, rows) {
  if (length(rows) == 0L) return(prototype)
  dplyr::bind_rows(c(list(prototype), rows))
}

stpd_event_regime_manifest <- function(tables, run_id, params_hash) {
  data.frame(
    schema_version = stpd_event_regime_schema_version(),
    run_id = run_id, params_hash = params_hash,
    table_name = names(tables),
    row_count = vapply(tables, nrow, integer(1)),
    column_count = vapply(tables, ncol, integer(1)),
    column_types = vapply(tables, function(x) paste(
      paste(names(x), vapply(x, typeof, character(1)), sep = ":"),
      collapse = ";"
    ), character(1)),
    table_sha256 = vapply(tables, stpd_multitrack_auto_hash, character(1)),
    stringsAsFactors = FALSE
  )
}

stpd_event_regime_final_parent <- function(x) {
  final <- if (inherits(x, "stpd_multitrack_final_product")) {
    stpd_multitrack_final_validate(x)
    x
  } else {
    stpd_multitrack_final(x)
  }
  if (!identical(as.character(final$metadata$materialization_status[1]),
                 "materialized")) {
    stpd_event_regime_abort(
      "parent_final_unavailable",
      "Recurrent parent-State candidates require a materialized canonical FINAL product."
    )
  }
  final
}

stpd_event_regime_normalize_boundaries <- function(
    final, hard_boundaries = NULL) {
  run_id <- as.character(final$metadata$run_id[1])
  params_hash <- as.character(final$metadata$params_hash[1])
  rows <- list()
  per_isi <- final$per_isi
  if (nrow(per_isi) > 0L) {
    trains <- sort(unique(as.character(per_isi$train)), method = "radix")
    for (train in trains) {
      x <- per_isi[per_isi$train == train, , drop = FALSE]
      x <- x[order(x$isi_index, method = "radix"), , drop = FALSE]
      if (nrow(x) == 0L) next
      first_index <- min(as.integer(x$isi_index), na.rm = TRUE)
      invalid_timestamp <- !is.finite(as.numeric(x$timestamp_sec))
      invalid_isi <- as.integer(x$isi_index) != first_index &
        (!is.finite(as.numeric(x$ISI_sec)) | as.numeric(x$ISI_sec) <= 0)
      missing_predecessor <- c(FALSE, diff(as.integer(x$isi_index)) != 1L)
      bad <- invalid_timestamp | invalid_isi | missing_predecessor
      if (!any(bad)) next
      for (i in which(bad)) {
        cls <- if (invalid_timestamp[i]) {
          "qc_invalid_timestamp"
        } else if (missing_predecessor[i]) {
          "acquisition_index_discontinuity"
        } else {
          "qc_invalid_isi"
        }
        rows[[length(rows) + 1L]] <- data.frame(
          schema_version = stpd_event_regime_schema_version(),
          run_id = run_id, params_hash = params_hash, train = train,
          boundary_isi = as.integer(x$isi_index[i]), boundary_class = cls,
          boundary_source = "final_per_isi_qc", stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!is.null(hard_boundaries) && nrow(hard_boundaries) > 0L) {
    if (!is.data.frame(hard_boundaries) ||
        !"train" %in% names(hard_boundaries)) {
      stpd_event_regime_abort(
        "hard_boundary_schema_invalid",
        "Explicit hard boundaries require train and boundary_isi/isi_index."
      )
    }
    index_name <- if ("boundary_isi" %in% names(hard_boundaries)) {
      "boundary_isi"
    } else if ("isi_index" %in% names(hard_boundaries)) {
      "isi_index"
    } else {
      stpd_event_regime_abort(
        "hard_boundary_index_missing",
        "Explicit hard boundaries require boundary_isi or isi_index."
      )
    }
    for (i in seq_len(nrow(hard_boundaries))) {
      boundary_index <- suppressWarnings(
        as.integer(hard_boundaries[[index_name]][i])
      )
      train <- as.character(hard_boundaries$train[i])
      if (is.na(boundary_index) || boundary_index < 1L ||
          is.na(train) || !nzchar(train)) {
        stpd_event_regime_abort(
          "hard_boundary_value_invalid",
          "Explicit hard-boundary train/index values must be valid."
        )
      }
      train_index <- suppressWarnings(as.integer(
        per_isi$isi_index[as.character(per_isi$train) == train]
      ))
      train_index <- train_index[is.finite(train_index)]
      if (length(train_index) == 0L ||
          boundary_index < min(train_index) ||
          boundary_index > max(train_index)) {
        stpd_event_regime_abort(
          "hard_boundary_scope_invalid",
          paste(
            "Explicit hard-boundary train/index must resolve inside the",
            "corresponding canonical FINAL per-ISI support."
          )
        )
      }
      cls <- if ("boundary_class" %in% names(hard_boundaries)) {
        as.character(hard_boundaries$boundary_class[i])
      } else {
        "acquisition_hard"
      }
      source <- if ("boundary_source" %in% names(hard_boundaries)) {
        as.character(hard_boundaries$boundary_source[i])
      } else {
        "explicit_input"
      }
      allowed_class <- c("qc_hard", "acquisition_hard", "artifact_hard")
      if (is.na(cls) || !cls %in% allowed_class) {
        stpd_event_regime_abort(
          "hard_boundary_class_invalid",
          paste(
            "Explicit recurrent-State boundaries must be QC/acquisition/artifact",
            "hard boundaries; Pause is never a parent-regime hard boundary."
          )
        )
      }
      if (!is.na(source) && identical(source, "final_per_isi_qc")) {
        stpd_event_regime_abort(
          "hard_boundary_source_reserved",
          "The final_per_isi_qc boundary source is reserved for derived QC evidence."
        )
      }
      rows[[length(rows) + 1L]] <- data.frame(
        schema_version = stpd_event_regime_schema_version(),
        run_id = run_id, params_hash = params_hash, train = train,
        boundary_isi = boundary_index,
        boundary_class = cls,
        boundary_source = ifelse(is.na(source) || !nzchar(source),
                                 "explicit_input", source),
        stringsAsFactors = FALSE
      )
    }
  }
  out <- stpd_event_regime_bind(stpd_event_regime_empty_boundaries(), rows)
  if (nrow(out) > 0L) {
    key <- paste(out$train, out$boundary_isi, out$boundary_class,
                 out$boundary_source, sep = "\u001f")
    out <- out[!duplicated(key), , drop = FALSE]
    out <- out[order(out$train, out$boundary_isi, out$boundary_class,
                     out$boundary_source, method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

stpd_event_regime_assign_segments <- function(children, boundaries) {
  if (nrow(children) == 0L) {
    children$qc_segment_id <- character()
    children$hard_boundary_crossed <- logical()
    return(children)
  }
  segment <- character(nrow(children))
  crossed <- logical(nrow(children))
  for (i in seq_len(nrow(children))) {
    boundary_rows <- boundaries[
      boundaries$train == children$train[i], , drop = FALSE
    ]
    b <- as.integer(boundary_rows$boundary_isi)
    valid <- is.finite(b)
    b <- b[valid]
    boundary_rows <- boundary_rows[valid, , drop = FALSE]
    contaminates_support <- boundary_rows$boundary_class %in% c(
      "qc_invalid_timestamp", "qc_invalid_isi"
    )
    child_start <- as.integer(children$start_isi[i])
    child_end <- as.integer(children$end_isi[i])
    crossed[i] <- any(
      (contaminates_support & child_start <= b & b <= child_end) |
        (!contaminates_support & child_start < b & b <= child_end)
    )
    segment[i] <- paste0(
      children$train[i], "::segment_",
      sum(b <= child_start) + 1L
    )
  }
  children$qc_segment_id <- segment
  children$hard_boundary_crossed <- crossed
  children
}

stpd_event_regime_attach_support_geometry <- function(children, per_isi) {
  if (nrow(children) == 0L) {
    children$.support_start_time_sec <- double()
    children$.support_end_time_sec <- double()
    children$.support_duration_sec <- double()
    return(children)
  }
  support_start <- support_end <- support_duration <- rep(NA_real_, nrow(children))
  per_train <- split(
    per_isi, as.character(per_isi$train), drop = TRUE
  )
  empty_support <- per_isi[FALSE, , drop = FALSE]
  child_train <- as.character(children$train)
  for (train in unique(child_train)) {
    child_rows <- which(child_train == train)
    train_support <- per_train[[train]]
    if (!is.null(train_support) && nrow(train_support) > 0L) {
      train_support <- train_support[order(
        as.integer(train_support$isi_index), method = "radix"
      ), , drop = FALSE]
      support_index <- as.integer(train_support$isi_index)
    }
    for (i in child_rows) {
      x <- empty_support
      if (!is.null(train_support) && nrow(train_support) > 0L) {
        left <- findInterval(
          as.integer(children$start_isi[i]) - 1L, support_index
        ) + 1L
        right <- findInterval(
          as.integer(children$end_isi[i]), support_index
        )
        if (left <= right && left >= 1L && right <= nrow(train_support)) {
          x <- train_support[seq.int(left, right), , drop = FALSE]
        }
      }
      valid <- is.finite(as.numeric(x$ISI_sec)) &
        as.numeric(x$ISI_sec) > 0 &
        is.finite(as.numeric(x$timestamp_sec))
      if (any(valid)) {
        isi <- as.numeric(x$ISI_sec[valid])
        timestamp <- as.numeric(x$timestamp_sec[valid])
        support_start[i] <- min(timestamp - isi)
        support_end[i] <- max(timestamp)
        support_duration[i] <- sum(isi)
      } else {
        support_start[i] <- as.numeric(children$start_time_sec[i])
        support_end[i] <- as.numeric(children$end_time_sec[i])
        support_duration[i] <- max(
          0, support_end[i] - support_start[i], na.rm = TRUE
        )
      }
    }
  }
  children$.support_start_time_sec <- support_start
  children$.support_end_time_sec <- support_end
  children$.support_duration_sec <- support_duration
  children
}

stpd_event_regime_interval_union_n <- function(start, end) {
  if (length(start) == 0L) return(0L)
  x <- data.frame(start = as.integer(start), end = as.integer(end))
  x <- x[order(x$start, x$end, method = "radix"), , drop = FALSE]
  total <- 0L
  current_start <- x$start[1]
  current_end <- x$end[1]
  if (nrow(x) > 1L) {
    for (i in 2:nrow(x)) {
      if (x$start[i] <= current_end + 1L) {
        current_end <- max(current_end, x$end[i])
      } else {
        total <- total + current_end - current_start + 1L
        current_start <- x$start[i]
        current_end <- x$end[i]
      }
    }
  }
  as.integer(total + current_end - current_start + 1L)
}

stpd_event_regime_id <- function(
    parent_hash, train, regime_class, trigger_route, child_ids) {
  payload <- paste(
    stpd_event_regime_schema_version(), stpd_event_regime_policy_hash(),
    parent_hash, train, regime_class, trigger_route,
    paste(sort(as.character(child_ids), method = "radix"), collapse = "|"),
    sep = "\u001f"
  )
  paste0("event_regime_", digest::digest(
    payload, algo = "sha256", serialize = FALSE
  ))
}

stpd_event_regime_window_budget <- function(children) {
  children <- children[order(children$start_isi, children$end_isi,
                             children$.child_id, method = "radix"), , drop = FALSE]
  envelope_n <- as.integer(
    max(children$end_isi) - min(children$start_isi) + 1L
  )
  direct_n <- stpd_event_regime_interval_union_n(
    children$start_isi, children$end_isi
  )
  interruption_n <- max(0L, envelope_n - direct_n)
  isi_gaps <- integer()
  time_gaps <- numeric()
  if (nrow(children) > 1L) {
    isi_gaps <- pmax(
      0L,
      as.integer(children$start_isi[-1L]) -
        as.integer(children$end_isi[-nrow(children)]) - 1L
    )
    time_gaps <- pmax(
      0,
      as.numeric(children$.support_start_time_sec[-1L]) -
        as.numeric(children$.support_end_time_sec[-nrow(children)])
    )
    time_gaps <- time_gaps[is.finite(time_gaps)]
  }
  start_time <- suppressWarnings(min(
    as.numeric(children$.support_start_time_sec), na.rm = TRUE
  ))
  end_time <- suppressWarnings(max(
    as.numeric(children$.support_end_time_sec), na.rm = TRUE
  ))
  if (!is.finite(start_time)) start_time <- NA_real_
  if (!is.finite(end_time)) end_time <- NA_real_
  duration <- if (is.finite(start_time) && is.finite(end_time)) {
    max(0, end_time - start_time)
  } else {
    NA_real_
  }
  child_duration <- pmax(0, as.numeric(children$.support_duration_sec))
  child_duration <- child_duration[is.finite(child_duration)]
  direct_duration <- if (length(child_duration)) sum(child_duration) else 0
  interruption_duration <- if (is.finite(duration)) {
    max(0, duration - direct_duration)
  } else {
    NA_real_
  }
  interruption_isi_fraction <- if (envelope_n > 0L) {
    interruption_n / envelope_n
  } else {
    0
  }
  interruption_duration_fraction <- if (is.finite(duration) && duration > 0) {
    interruption_duration / duration
  } else if (isTRUE(interruption_duration == 0)) {
    0
  } else {
    1
  }
  max_single_isi_fraction <- if (envelope_n > 0L && length(isi_gaps)) {
    max(isi_gaps) / envelope_n
  } else {
    0
  }
  max_single_duration_fraction <- if (is.finite(duration) && duration > 0 &&
                                      length(time_gaps)) {
    max(time_gaps) / duration
  } else {
    0
  }
  policy <- stpd_event_regime_window_policy()
  pass <- interruption_isi_fraction <=
    policy$max_interruption_isi_fraction &
    interruption_duration_fraction <=
      policy$max_interruption_duration_fraction &
    max_single_isi_fraction <=
      policy$max_single_interruption_isi_fraction &
    max_single_duration_fraction <=
      policy$max_single_interruption_duration_fraction
  list(
    pass = isTRUE(pass), envelope_n_isi = envelope_n,
    direct_support_isi_n = as.integer(direct_n),
    interruption_isi_n = as.integer(interruption_n),
    interruption_isi_fraction = interruption_isi_fraction,
    start_time_sec = start_time, end_time_sec = end_time,
    duration_sec = duration, direct_support_duration_sec = direct_duration,
    interruption_duration_sec = interruption_duration,
    interruption_duration_fraction = interruption_duration_fraction,
    max_single_interruption_isi_fraction = max_single_isi_fraction,
    max_single_interruption_duration_fraction = max_single_duration_fraction,
    adjacent_isi_gaps = isi_gaps, adjacent_time_gaps = time_gaps
  )
}

stpd_event_regime_can_expand <- function(children, next_child, anchor_budget) {
  last <- children[nrow(children), , drop = FALSE]
  next_isi_gap <- max(
    0L,
    as.integer(next_child$start_isi[1]) - as.integer(last$end_isi[1]) - 1L
  )
  next_time_gap <- max(
    0,
    as.numeric(next_child$.support_start_time_sec[1]) -
      as.numeric(last$.support_end_time_sec[1])
  )
  anchor_isi_limit <- if (length(anchor_budget$adjacent_isi_gaps)) {
    max(anchor_budget$adjacent_isi_gaps)
  } else {
    0L
  }
  anchor_time_limit <- if (length(anchor_budget$adjacent_time_gaps)) {
    max(anchor_budget$adjacent_time_gaps)
  } else {
    0
  }
  cadence_pass <- next_isi_gap <= anchor_isi_limit &&
    (!is.finite(next_time_gap) || next_time_gap <= anchor_time_limit)
  proposed <- dplyr::bind_rows(children, next_child)
  isTRUE(cadence_pass) &&
    isTRUE(stpd_event_regime_window_budget(proposed)$pass)
}

stpd_event_regime_window_rows <- function(
    children, trigger_contributor, regime_class, trigger_route,
    run_id, params_hash, parent_hash) {
  children <- children[order(children$start_isi, children$end_isi,
                             children$.child_id, method = "radix"), , drop = FALSE]
  budget <- stpd_event_regime_window_budget(children)
  if (!isTRUE(budget$pass)) {
    stpd_event_regime_abort(
      "temporal_budget_failed",
      "A recurrent parent-State row cannot be materialized outside its finite interruption budget."
    )
  }
  start_isi <- min(as.integer(children$start_isi))
  end_isi <- max(as.integer(children$end_isi))
  envelope_n <- budget$envelope_n_isi
  direct_n <- budget$direct_support_isi_n
  interruption_n <- budget$interruption_isi_n
  start_time <- budget$start_time_sec
  end_time <- budget$end_time_sec
  duration <- budget$duration_sec
  adjacent_gap <- budget$adjacent_time_gaps
  gap_median <- if (length(adjacent_gap)) stats::median(adjacent_gap) else NA_real_
  gap_max <- if (length(adjacent_gap)) max(adjacent_gap) else NA_real_
  regime_id <- stpd_event_regime_id(
    parent_hash, children$train[1], regime_class, trigger_route,
    children$.child_id
  )
  is_pause <- identical(children$.child_domain[1], "gap")
  parent_state_class <- switch(
    regime_class,
    recurrent_bursting = "recurrent_bursting_state",
    recurrent_pausing = "recurrent_pause_state",
    stpd_event_regime_abort(
      "parent_state_class_invalid",
      "A recurrent parent State requires a registered State class."
    )
  )
  regime <- data.frame(
    schema_version = stpd_event_regime_schema_version(),
    policy_hash = stpd_event_regime_policy_hash(), run_id = run_id,
    params_hash = params_hash, authoritative = FALSE,
    biological_ground_truth = FALSE, train = children$train[1],
    regime_id = regime_id, regime_class = regime_class,
    semantic_domain = "state", parent_state_class = parent_state_class,
    candidate_status = "descriptive_state_candidate",
    validation_status = "unvalidated_descriptive_state",
    trigger_route = trigger_route,
    child_domain = children$.child_domain[1],
    qc_segment_id = children$qc_segment_id[1], start_isi = start_isi,
    end_isi = end_isi, envelope_n_isi = envelope_n,
    direct_support_isi_n = direct_n, interruption_isi_n = interruption_n,
    interruption_fraction = if (envelope_n > 0L) interruption_n / envelope_n else 0,
    child_n = as.integer(nrow(children)),
    trigger_child_n = as.integer(sum(trigger_contributor)),
    long_pause_child_n = as.integer(if (is_pause) sum(children$.is_long) else 0L),
    accepted_pause_child_n = as.integer(if (is_pause) nrow(children) else 0L),
    start_time_sec = start_time, end_time_sec = end_time,
    duration_sec = duration,
    direct_support_duration_sec = budget$direct_support_duration_sec,
    interruption_duration_sec = budget$interruption_duration_sec,
    interruption_duration_fraction = budget$interruption_duration_fraction,
    max_single_interruption_isi_fraction =
      budget$max_single_interruption_isi_fraction,
    max_single_interruption_duration_fraction =
      budget$max_single_interruption_duration_fraction,
    temporal_budget_pass = TRUE,
    adjacent_child_gap_median_sec = gap_median,
    adjacent_child_gap_max_sec = gap_max, carrier_state_n = 0L,
    carrier_context = "none", carrier_state_classes = "",
    absolute_isi_threshold_used = FALSE, one_pass_nonrecursive = TRUE,
    candidate_layer = "post_final_recurrent_parent_state",
    candidate_source = "canonical_final_child_entities",
    stringsAsFactors = FALSE
  )
  membership <- lapply(seq_len(nrow(children)), function(i) {
    data.frame(
      schema_version = stpd_event_regime_schema_version(), run_id = run_id,
      params_hash = params_hash, authoritative = FALSE,
      train = children$train[i], regime_id = regime_id,
      regime_class = regime_class, parent_state_class = parent_state_class,
      child_domain = children$.child_domain[i],
      child_id = children$.child_id[i], child_order = as.integer(i),
      child_role = if (identical(children$.child_domain[i], "event")) {
        "canonical_burst_event"
      } else if (isTRUE(children$.is_long[i])) {
        "high_specificity_long_pause"
      } else {
        "accepted_pause"
      },
      trigger_contributor = as.logical(trigger_contributor[i]),
      child_start_isi = as.integer(children$start_isi[i]),
      child_end_isi = as.integer(children$end_isi[i]),
      child_support_isi_n = as.integer(
        children$end_isi[i] - children$start_isi[i] + 1L
      ),
      non_destructive = TRUE, stringsAsFactors = FALSE
    )
  })
  list(regime = regime, memberships = membership)
}

stpd_event_regime_burst_windows <- function(
    children, run_id, params_hash, parent_hash) {
  rows <- list()
  links <- list()
  if (nrow(children) == 0L) return(list(regimes = rows, memberships = links))
  groups <- split(children, paste(children$train, children$qc_segment_id,
                                  sep = "\u001f"), drop = TRUE)
  groups <- groups[order(names(groups), method = "radix")]
  for (x in groups) {
    x <- x[!x$hard_boundary_crossed, , drop = FALSE]
    x <- x[order(x$start_isi, x$end_isi, x$.child_id,
                 method = "radix"), , drop = FALSE]
    i <- 1L
    while (i + 2L <= nrow(x)) {
      end <- i + 2L
      selected <- x[i:end, , drop = FALSE]
      anchor_budget <- stpd_event_regime_window_budget(selected)
      if (!isTRUE(anchor_budget$pass)) {
        i <- i + 1L
        next
      }
      while (end < nrow(x) && stpd_event_regime_can_expand(
        selected, x[end + 1L, , drop = FALSE], anchor_budget
      )) {
        end <- end + 1L
        selected <- x[i:end, , drop = FALSE]
      }
      trigger <- seq_len(nrow(selected)) <= 3L
      window <- stpd_event_regime_window_rows(
        selected, trigger, "recurrent_bursting",
        "three_canonical_burst_events", run_id, params_hash, parent_hash
      )
      rows[[length(rows) + 1L]] <- window$regime
      links <- c(links, window$memberships)
      i <- end + 1L
    }
  }
  list(regimes = rows, memberships = links)
}

stpd_event_regime_pause_windows <- function(
    children, run_id, params_hash, parent_hash) {
  rows <- list()
  links <- list()
  if (nrow(children) == 0L) return(list(regimes = rows, memberships = links))
  groups <- split(children, paste(children$train, children$qc_segment_id,
                                  sep = "\u001f"), drop = TRUE)
  groups <- groups[order(names(groups), method = "radix")]
  for (x in groups) {
    x <- x[!x$hard_boundary_crossed, , drop = FALSE]
    x <- x[order(x$start_isi, x$end_isi, x$.child_id,
                 method = "radix"), , drop = FALSE]
    i <- 1L
    while (i <= nrow(x)) {
      remaining <- seq.int(i, nrow(x))
      long_position <- remaining[x$.is_long[remaining]]
      ordinary_position <- remaining[!x$.is_long[remaining]]
      raw_candidate_available <- length(long_position) >= 3L ||
        length(ordinary_position) >= 5L
      if (!raw_candidate_available) break

      # The two trigger routes are a logical OR.  Evaluate their temporal
      # budgets independently before choosing a route; otherwise an earlier
      # 3-long anchor that fails its budget can mask a valid 5-accepted anchor.
      options <- list()
      if (length(long_position) >= 3L) {
        selected_index <- seq.int(long_position[1L], long_position[3L])
        selected <- x[selected_index, , drop = FALSE]
        anchor_budget <- stpd_event_regime_window_budget(selected)
        if (isTRUE(anchor_budget$pass)) {
          options[[length(options) + 1L]] <- list(
            selected_index = selected_index,
            trigger = x$.is_long[selected_index],
            route = "three_high_specificity_long_pauses",
            anchor_budget = anchor_budget,
            end = max(selected_index), start = min(selected_index),
            priority = 1L
          )
        }
      }
      if (length(ordinary_position) >= 5L) {
        selected_index <- seq.int(
          ordinary_position[1L], ordinary_position[5L]
        )
        selected <- x[selected_index, , drop = FALSE]
        anchor_budget <- stpd_event_regime_window_budget(selected)
        if (isTRUE(anchor_budget$pass)) {
          options[[length(options) + 1L]] <- list(
            selected_index = selected_index,
            trigger = !x$.is_long[selected_index],
            route = "five_ordinary_accepted_pause_events",
            anchor_budget = anchor_budget,
            end = max(selected_index), start = min(selected_index),
            priority = 2L
          )
        }
      }
      if (length(options) == 0L) {
        i <- i + 1L
        next
      }

      option_order <- order(
        vapply(options, `[[`, integer(1), "end"),
        vapply(options, `[[`, integer(1), "start"),
        vapply(options, `[[`, integer(1), "priority"),
        method = "radix"
      )
      chosen <- options[[option_order[1L]]]
      selected_index <- chosen$selected_index
      trigger <- chosen$trigger
      route <- chosen$route
      anchor_budget <- chosen$anchor_budget
      selected <- x[selected_index, , drop = FALSE]
      end <- max(selected_index)
      trigger_full <- trigger
      while (end < nrow(x) && stpd_event_regime_can_expand(
        selected, x[end + 1L, , drop = FALSE], anchor_budget
      )) {
        end <- end + 1L
        selected <- x[seq.int(min(selected_index), end), , drop = FALSE]
        trigger_full <- c(trigger_full, FALSE)
      }
      window <- stpd_event_regime_window_rows(
        selected, trigger_full, "recurrent_pausing", route,
        run_id, params_hash, parent_hash
      )
      rows[[length(rows) + 1L]] <- window$regime
      links <- c(links, window$memberships)
      i <- end + 1L
    }
  }
  list(regimes = rows, memberships = links)
}

stpd_event_regime_carriers <- function(regimes, states, run_id, params_hash) {
  rows <- list()
  if (nrow(regimes) == 0L || nrow(states) == 0L) {
    return(stpd_event_regime_empty_carrier_relationships())
  }
  states <- states[order(states$train, states$start_isi, states$end_isi,
                         states$state_id, method = "radix"), , drop = FALSE]
  for (i in seq_len(nrow(regimes))) {
    hit <- which(
      states$train == regimes$train[i] &
        states$start_isi <= regimes$end_isi[i] &
        states$end_isi >= regimes$start_isi[i]
    )
    for (j in hit) {
      overlap_start <- max(regimes$start_isi[i], states$start_isi[j])
      overlap_end <- min(regimes$end_isi[i], states$end_isi[j])
      overlap_n <- as.integer(overlap_end - overlap_start + 1L)
      rows[[length(rows) + 1L]] <- data.frame(
        schema_version = stpd_event_regime_schema_version(), run_id = run_id,
        params_hash = params_hash, authoritative = FALSE,
        train = regimes$train[i], regime_id = regimes$regime_id[i],
        state_id = as.character(states$state_id[j]),
        state_class = as.character(states$state_class[j]),
        state_family = as.character(states$state_family[j]),
        state_subtype = as.character(states$state_subtype[j]),
        relationship_type = "carrier_state_context",
        overlap_start_isi = as.integer(overlap_start),
        overlap_end_isi = as.integer(overlap_end), overlap_isi_n = overlap_n,
        regime_overlap_fraction = overlap_n / regimes$envelope_n_isi[i],
        state_overlap_fraction = overlap_n /
          as.integer(states$end_isi[j] - states$start_isi[j] + 1L),
        non_destructive = TRUE, stringsAsFactors = FALSE
      )
    }
  }
  out <- stpd_event_regime_bind(
    stpd_event_regime_empty_carrier_relationships(), rows
  )
  if (nrow(out) > 0L) {
    out <- out[order(out$train, out$regime_id, out$state_id,
                     method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

stpd_event_regime_materialize <- function(final, hard_boundaries = NULL) {
  run_id <- as.character(final$metadata$run_id[1])
  params_hash <- as.character(final$metadata$params_hash[1])
  parent_hash <- as.character(final$metadata$product_sha256[1])
  boundaries <- stpd_event_regime_normalize_boundaries(final, hard_boundaries)

  events <- final$events
  events <- events[events$event_family == "burst", , drop = FALSE]
  events$.child_id <- as.character(events$event_id)
  events$.child_domain <- rep("event", nrow(events))
  events$.is_long <- rep(FALSE, nrow(events))
  events <- stpd_event_regime_assign_segments(events, boundaries)
  events <- stpd_event_regime_attach_support_geometry(events, final$per_isi)

  # Post-FINAL aggregation only: recurrent-pausing candidates are derived from
  # already accepted canonical Pause entities.  Count triggers never discover,
  # accept, reject, or retype an individual Pause.
  gaps <- final$gaps
  gaps <- gaps[
    gaps$gap_class == "pause" & gaps$candidate_decision == "accepted",
    , drop = FALSE
  ]
  gaps$.child_id <- as.character(gaps$gap_id)
  gaps$.child_domain <- rep("gap", nrow(gaps))
  gaps$.is_long <- as.character(gaps$candidate_reason_code) ==
    "high_specificity_absolute_and_relative_long_isi_anchor"
  gaps <- stpd_event_regime_assign_segments(gaps, boundaries)
  gaps <- stpd_event_regime_attach_support_geometry(gaps, final$per_isi)

  burst <- stpd_event_regime_burst_windows(
    events, run_id, params_hash, parent_hash
  )
  pause <- stpd_event_regime_pause_windows(
    gaps, run_id, params_hash, parent_hash
  )
  regimes <- stpd_event_regime_bind(
    stpd_event_regime_empty_regimes(), c(burst$regimes, pause$regimes)
  )
  memberships <- stpd_event_regime_bind(
    stpd_event_regime_empty_memberships(),
    c(burst$memberships, pause$memberships)
  )
  if (nrow(regimes) > 0L) {
    regimes <- regimes[order(regimes$train, regimes$start_isi,
                             regimes$regime_class, regimes$regime_id,
                             method = "radix"), , drop = FALSE]
    rownames(regimes) <- NULL
  }
  if (nrow(memberships) > 0L) {
    memberships <- memberships[order(
      memberships$train, memberships$regime_id, memberships$child_order,
      method = "radix"
    ), , drop = FALSE]
    rownames(memberships) <- NULL
  }
  carriers <- stpd_event_regime_carriers(
    regimes, final$states, run_id, params_hash
  )
  if (nrow(regimes) > 0L && nrow(carriers) > 0L) {
    for (i in seq_len(nrow(regimes))) {
      x <- carriers[carriers$regime_id == regimes$regime_id[i], , drop = FALSE]
      regimes$carrier_state_n[i] <- as.integer(nrow(x))
      if (nrow(x) == 0L) next
      classes <- sort(unique(as.character(x$state_class)), method = "radix")
      regimes$carrier_state_classes[i] <- paste(classes, collapse = "|")
      broad <- any(x$state_family == "broad_high_frequency_state")
      tonic <- any(x$state_family == "tonic" | x$state_class == "tonic")
      regimes$carrier_context[i] <- if (broad && tonic) {
        "mixed_broad_hfs_and_tonic"
      } else if (broad) {
        "broad_hfs"
      } else if (tonic) {
        "tonic"
      } else {
        "other_state_context"
      }
    }
  }
  list(
    boundaries = boundaries, regimes = regimes, memberships = memberships,
    carrier_relationships = carriers
  )
}

stpd_event_regime_build <- function(ds, hard_boundaries = NULL) {
  final <- stpd_event_regime_final_parent(ds)
  materialized <- stpd_event_regime_materialize(final, hard_boundaries)
  run_id <- as.character(final$metadata$run_id[1])
  params_hash <- as.character(final$metadata$params_hash[1])
  invariants <- data.frame(
    schema_version = rep(stpd_event_regime_schema_version(), 6L),
    run_id = rep(run_id, 6L), params_hash = rep(params_hash, 6L),
    check_name = c(
      "canonical_children_preserved", "carrier_states_orthogonal",
      "pause_not_recurrence_boundary", "one_pass_nonrecursive",
      "no_absolute_isi_threshold", "post_final_pause_aggregation_only"
    ),
    status = rep("pass", 6L),
    message = c(
      "Canonical FINAL Event and Gap children are referenced, never retyped or deleted.",
      "Tonic and Broad HFS remain carrier States and may overlap a recurrent parent State.",
      "Accepted Pause may occur in A between Burst Events; only QC/acquisition boundaries split regimes.",
      "Minimum-count anchors expand only under frozen local cadence and finite interruption budgets; accepted parents are never recursively merged.",
      "Regime triggering uses entity counts and upstream typed evidence, never a fixed absolute ISI threshold.",
      "Three-long/Five-ordinary-accepted Pause triggers run only after canonical FINAL Pause detection and cannot modify child decisions."
    ), stringsAsFactors = FALSE
  )
  payload <- c(materialized, list(invariants = invariants))
  parent_hash <- as.character(final$metadata$product_sha256[1])
  policy_hash <- stpd_event_regime_policy_hash()
  product_hash <- stpd_multitrack_auto_hash(list(
    parent_final_product_sha256 = parent_hash,
    policy_hash = policy_hash,
    tables = payload
  ))
  metadata <- data.frame(
    schema_version = stpd_event_regime_schema_version(),
    policy_hash = policy_hash, run_id = run_id,
    params_hash = params_hash, authoritative = FALSE,
    authority_scope = "descriptive_parent_state_candidate_only",
    biological_ground_truth = FALSE,
    validation_status = "unvalidated_descriptive_state",
    materialization_status = "materialized",
    parent_final_schema_version = as.character(final$metadata$schema_version[1]),
    parent_final_product_sha256 = parent_hash,
    absolute_isi_threshold_used = FALSE, one_pass_nonrecursive = TRUE,
    regimes_n = as.integer(nrow(materialized$regimes)),
    memberships_n = as.integer(nrow(materialized$memberships)),
    carrier_relationships_n = as.integer(nrow(materialized$carrier_relationships)),
    boundaries_n = as.integer(nrow(materialized$boundaries)),
    product_sha256 = product_hash, stringsAsFactors = FALSE
  )
  tables <- c(list(metadata = metadata), payload)
  product <- structure(
    c(tables, list(manifest = stpd_event_regime_manifest(
      tables, run_id, params_hash
    ))),
    class = c("stpd_event_regime_product", "list")
  )
  stpd_event_regime_validate(product, parent = final, rematerialize = FALSE)
  product
}

stpd_event_regime_validate <- function(
    product, parent = NULL, rematerialize = TRUE) {
  prototypes <- list(
    metadata = stpd_event_regime_empty_metadata(),
    boundaries = stpd_event_regime_empty_boundaries(),
    regimes = stpd_event_regime_empty_regimes(),
    memberships = stpd_event_regime_empty_memberships(),
    carrier_relationships = stpd_event_regime_empty_carrier_relationships(),
    invariants = stpd_event_regime_empty_invariants(),
    manifest = stpd_event_regime_empty_manifest()
  )
  if (!inherits(product, "stpd_event_regime_product") ||
      !identical(names(product), names(prototypes))) {
    stpd_event_regime_abort(
      "product_schema_invalid", "Event-regime product tables are incomplete."
    )
  }
  for (name in names(prototypes)) {
    if (!is.data.frame(product[[name]]) ||
        !identical(names(product[[name]]), names(prototypes[[name]])) ||
        !identical(vapply(product[[name]], typeof, character(1)),
                   vapply(prototypes[[name]], typeof, character(1)))) {
      stpd_event_regime_abort(
        "table_schema_invalid",
        paste0("Event-regime table '", name, "' has an invalid schema.")
      )
    }
  }
  metadata <- product$metadata
  if (nrow(metadata) != 1L ||
      !identical(metadata$schema_version, stpd_event_regime_schema_version()) ||
      !identical(metadata$policy_hash, stpd_event_regime_policy_hash()) ||
      !identical(metadata$authority_scope,
                 "descriptive_parent_state_candidate_only") ||
      !identical(metadata$validation_status,
                 "unvalidated_descriptive_state") ||
      metadata$authoritative || metadata$biological_ground_truth ||
      metadata$absolute_isi_threshold_used ||
      !metadata$one_pass_nonrecursive) {
    stpd_event_regime_abort(
      "metadata_semantics_invalid",
      "Recurrent parent-State metadata must remain non-authoritative and threshold-free."
    )
  }
  identity_tables <- c(
    "boundaries", "regimes", "memberships", "carrier_relationships",
    "invariants", "manifest"
  )
  for (name in identity_tables) {
    table <- product[[name]]
    if (nrow(table) == 0L) next
    if (any(table$schema_version != stpd_event_regime_schema_version()) ||
        any(table$run_id != metadata$run_id) ||
        any(table$params_hash != metadata$params_hash)) {
      stpd_event_regime_abort(
        "cross_table_identity_invalid",
        paste0("Event-regime table '", name,
               "' is outside the metadata run/parameter identity.")
      )
    }
  }
  if (nrow(product$regimes) > 0L &&
      any(product$regimes$policy_hash != metadata$policy_hash)) {
    stpd_event_regime_abort(
      "cross_table_policy_invalid",
      "Event-regime rows do not share the frozen product policy."
    )
  }
  internal_boundary_class <- c(
    "qc_invalid_timestamp", "qc_invalid_isi",
    "acquisition_index_discontinuity"
  )
  explicit_boundary_class <- c("qc_hard", "acquisition_hard", "artifact_hard")
  boundaries <- product$boundaries
  if (nrow(boundaries) > 0L) {
    internal <- boundaries$boundary_source == "final_per_isi_qc"
    if (any(internal & !boundaries$boundary_class %in% internal_boundary_class) ||
        any(!internal & !boundaries$boundary_class %in% explicit_boundary_class)) {
      stpd_event_regime_abort(
        "boundary_ontology_invalid",
        "Event-regime boundaries must be typed QC/acquisition/artifact evidence; Pause is forbidden."
      )
    }
  }
  payload_names <- c(
    "boundaries", "regimes", "memberships", "carrier_relationships",
    "invariants"
  )
  expected_product_hash <- stpd_multitrack_auto_hash(list(
    parent_final_product_sha256 = metadata$parent_final_product_sha256,
    policy_hash = metadata$policy_hash,
    tables = product[payload_names]
  ))
  if (!identical(metadata$product_sha256, expected_product_hash)) {
    stpd_event_regime_abort(
      "product_hash_invalid", "Event-regime payload hash is stale."
    )
  }
  expected_manifest <- stpd_event_regime_manifest(
    product[setdiff(names(product), "manifest")],
    metadata$run_id, metadata$params_hash
  )
  if (!identical(product$manifest, expected_manifest)) {
    stpd_event_regime_abort(
      "manifest_invalid", "Event-regime manifest does not match its tables."
    )
  }
  regimes <- product$regimes
  memberships <- product$memberships
  if (nrow(regimes) != metadata$regimes_n ||
      nrow(memberships) != metadata$memberships_n ||
      nrow(product$carrier_relationships) !=
        metadata$carrier_relationships_n ||
      nrow(product$boundaries) != metadata$boundaries_n) {
    stpd_event_regime_abort(
      "metadata_count_invalid", "Event-regime metadata counts are stale."
    )
  }
  if (nrow(regimes) > 0L) {
    if (anyDuplicated(regimes$regime_id) ||
        any(regimes$authoritative) || any(regimes$biological_ground_truth) ||
        any(regimes$absolute_isi_threshold_used) ||
        any(!regimes$one_pass_nonrecursive) || any(!regimes$temporal_budget_pass) ||
        any(regimes$semantic_domain != "state") ||
        any(regimes$candidate_status != "descriptive_state_candidate") ||
        any(regimes$validation_status != "unvalidated_descriptive_state") ||
        any(regimes$candidate_layer != "post_final_recurrent_parent_state") ||
        any(regimes$candidate_source != "canonical_final_child_entities") ||
        any(!regimes$regime_class %in% c(
          "recurrent_bursting", "recurrent_pausing"
        )) || any(regimes$parent_state_class != ifelse(
          regimes$regime_class == "recurrent_bursting",
          "recurrent_bursting_state", "recurrent_pause_state"
        ))) {
      stpd_event_regime_abort(
        "regime_semantics_invalid", "Event-regime candidate semantics are invalid."
      )
    }
    if (!setequal(regimes$regime_id, unique(memberships$regime_id))) {
      stpd_event_regime_abort(
        "membership_expected_set_invalid",
        "Every Event regime requires exactly its canonical child-membership set."
      )
    }
    for (i in seq_len(nrow(regimes))) {
      x <- memberships[memberships$regime_id == regimes$regime_id[i], , drop = FALSE]
      x <- x[order(x$child_order), , drop = FALSE]
      direct_n <- stpd_event_regime_interval_union_n(
        x$child_start_isi, x$child_end_isi
      )
      envelope_n <- regimes$end_isi[i] - regimes$start_isi[i] + 1L
      if (nrow(x) != regimes$child_n[i] ||
          !identical(x$child_order, seq_len(nrow(x))) ||
          min(x$child_start_isi) != regimes$start_isi[i] ||
          max(x$child_end_isi) != regimes$end_isi[i] ||
          direct_n != regimes$direct_support_isi_n[i] ||
          envelope_n != regimes$envelope_n_isi[i] ||
          envelope_n - direct_n != regimes$interruption_isi_n[i] ||
          any(!x$non_destructive) ||
          any(x$parent_state_class != regimes$parent_state_class[i])) {
        stpd_event_regime_abort(
          "regime_geometry_invalid",
          "Event-regime envelope/direct-support closure failed."
        )
      }
    }
    duplicate_child <- paste(
      memberships$regime_class, memberships$train,
      memberships$child_domain, memberships$child_id, sep = "\u001f"
    )
    if (anyDuplicated(duplicate_child)) {
      stpd_event_regime_abort(
        "recursive_or_overlapping_membership",
        "A canonical child cannot feed more than one same-class regime candidate."
      )
    }
  } else if (nrow(memberships) > 0L) {
    stpd_event_regime_abort(
      "orphan_membership", "Event-regime memberships cannot be orphaned."
    )
  }
  if (nrow(product$carrier_relationships) > 0L &&
      (any(!product$carrier_relationships$regime_id %in% regimes$regime_id) ||
       any(!product$carrier_relationships$non_destructive) ||
       any(product$carrier_relationships$authoritative))) {
    stpd_event_regime_abort(
      "carrier_relationship_invalid",
      "Carrier relationships must resolve and remain non-destructive."
    )
  }
  if (!is.null(parent)) {
    final <- stpd_event_regime_final_parent(parent)
    if (!identical(metadata$parent_final_product_sha256,
                   as.character(final$metadata$product_sha256[1]))) {
      stpd_event_regime_abort(
        "parent_final_hash_mismatch",
        "Event-regime product is stale relative to canonical FINAL."
      )
    }
    source_events <- final$events
    source_gaps <- final$gaps
    if (nrow(memberships) > 0L) {
      for (i in seq_len(nrow(memberships))) {
        source <- if (memberships$child_domain[i] == "event") {
          source_events[source_events$event_id == memberships$child_id[i], , drop = FALSE]
        } else {
          source_gaps[source_gaps$gap_id == memberships$child_id[i], , drop = FALSE]
        }
        if (nrow(source) != 1L ||
            as.character(source$train[1]) != memberships$train[i] ||
            as.integer(source$start_isi[1]) != memberships$child_start_isi[i] ||
            as.integer(source$end_isi[1]) != memberships$child_end_isi[i] ||
            (memberships$child_domain[i] == "event" &&
             as.character(source$event_family[1]) != "burst") ||
            (memberships$child_domain[i] == "gap" &&
             (as.character(source$gap_class[1]) != "pause" ||
              as.character(source$candidate_decision[1]) != "accepted"))) {
          stpd_event_regime_abort(
            "child_parent_binding_invalid",
            "Event-regime child membership does not bind canonical FINAL."
          )
        }
      }
    }
    if (isTRUE(rematerialize)) {
      explicit <- product$boundaries[
        product$boundaries$boundary_source != "final_per_isi_qc", , drop = FALSE
      ]
      expected <- stpd_event_regime_materialize(final, explicit)
      for (name in c(
        "boundaries", "regimes", "memberships", "carrier_relationships"
      )) {
        if (!identical(product[[name]], expected[[name]])) {
          stpd_event_regime_abort(
            "deterministic_rematerialization_mismatch",
            paste0("Event-regime table '", name,
                   "' differs from deterministic rematerialization.")
          )
        }
      }
    }
  }
  invisible(TRUE)
}

stpd_event_regime_strip <- function(ds) {
  if (is.list(ds) && is.list(ds$results) &&
      stpd_event_regime_result_key() %in% names(ds$results)) {
    ds$results[[stpd_event_regime_result_key()]] <- NULL
  }
  ds
}

stpd_event_regime_attach <- function(ds, hard_boundaries = NULL) {
  final_candidate <- (ds$results %||% list())[[
    stpd_multitrack_final_result_key()
  ]] %||% NULL
  if (is.null(final_candidate) ||
      !identical(
        as.character((final_candidate$metadata %||% data.frame())$
          materialization_status[1]),
        "materialized"
      )) {
    # This additive descriptive layer must never make a legacy/fail-soft
    # detector route fail merely because canonical FINAL was unavailable.
    return(stpd_event_regime_strip(ds))
  }
  before_trains <- ds$trains
  before_final <- final_candidate
  product <- stpd_event_regime_build(ds, hard_boundaries)
  out <- ds
  if (is.null(out$results) || !is.list(out$results)) out$results <- list()
  out$results[[stpd_event_regime_result_key()]] <- product
  if (!identical(before_trains, out$trains) ||
      !identical(before_final, out$results[[stpd_multitrack_final_result_key()]])) {
    stpd_event_regime_abort(
      "parent_mutated",
      "Attaching Event regimes changed canonical trains or FINAL tables."
    )
  }
  out
}

#' Access descriptive recurrent parent-State candidates
#'
#' Recurrent Burst/Pause parent States are a non-authoritative layer over
#' preserved canonical child Events/Gaps.  They are not biological truth and
#' do not replace Tonic or Broad-HFS carrier States.
#' @param ds A detector dataset or `stpd_event_regime_product`.
#' @return A strictly validated descriptive recurrent parent-State product.
#' @export
stpd_event_regimes <- function(ds) {
  product <- if (inherits(ds, "stpd_event_regime_product")) {
    ds
  } else if (is.list(ds) && is.list(ds$results)) {
    ds$results[[stpd_event_regime_result_key()]] %||% NULL
  } else {
    NULL
  }
  if (is.null(product)) {
    stpd_event_regime_abort(
      "product_missing",
      "No recurrent parent-State product is attached; run the detector again."
    )
  }
  stpd_event_regime_validate(
    product,
    parent = if (inherits(ds, "stpd_event_regime_product")) NULL else ds,
    rematerialize = !inherits(ds, "stpd_event_regime_product")
  )
  product
}
