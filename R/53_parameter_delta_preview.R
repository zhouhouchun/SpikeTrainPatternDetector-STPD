# Parameter delta dry-run preview.
# These helpers rerun a small train subset with baseline and current parameters,
# compare AUTO Event and State products (including direct support and overlap
# arbitration), and return only audit tables. They never mutate the input dataset,
# adopt a previewed threshold, or write into ds$results.

stpd_delta_event_table <- function(ds, params, selected_trains, source = c("auto", "final")) {
  source <- match.arg(source)
  if (is.null(ds) || is.null(ds$trains)) return(empty_events_tbl())
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  if (length(target) == 0) return(empty_events_tbl())
  p <- effective_params_for_detector(params)
  min_isi <- suppressWarnings(as.numeric(p$detector$min_valid_isi_sec %||% 0.0009))
  if (!is.finite(min_isi)) min_isi <- 0.0009
  out <- derive_interval_tables(
    ds$trains[target],
    source = source,
    auto_others = FALSE,
    dataset_map = stats::setNames(rep(ds$meta$display_name %||% "dataset", length(target)), target),
    min_isi_sec = min_isi,
    contrast_q = p$burst$contrast_q %||% 0.90,
    context_k = p$burst$context_k %||% 5L
  )$events
  if (is.null(out) || nrow(out) == 0) return(empty_events_tbl())
  out$event_row_id <- seq_len(nrow(out))
  out
}

stpd_delta_empty_state_episodes <- function() {
  data.frame(
    episode_row_id = integer(), train = character(),
    state_class = character(), state_subtype = character(),
    episode_key = character(), source_state_ids = character(),
    start_isi = integer(), end_isi = integer(), n_isi = integer(),
    direct_support_isi_n = integer(), support_fragment_n = integer(),
    stringsAsFactors = FALSE
  )
}

stpd_delta_empty_state_support <- function() {
  data.frame(
    support_row_id = integer(), train = character(), isi_index = integer(),
    state_class = character(), state_subtype = character(),
    state_episode_key = character(), state_support_role = character(),
    stringsAsFactors = FALSE
  )
}

stpd_delta_empty_overlap_resolution <- function() {
  data.frame(
    resolution_row_id = integer(), train = character(),
    root_hfs_candidate_id = character(), peer_candidate_ids = character(),
    state_overlap_resolution = character(), state_overlap_geometry = character(),
    start_isi = integer(), end_isi = integer(),
    post_start_isi = integer(), post_end_isi = integer(),
    overlap_n_isi = integer(), residual_gate_pass = logical(),
    residual_gate_status = character(), residual_failed_checks = character(),
    resolution_reason = character(), stringsAsFactors = FALSE
  )
}

stpd_delta_multitrack_product <- function(ds) {
  if (inherits(ds, "stpd_multitrack_auto_product")) return(ds)
  product <- if (is.list(ds) && is.list(ds$results)) {
    ds$results[[stpd_multitrack_auto_result_key()]] %||% NULL
  } else {
    NULL
  }
  if (is.null(product)) return(NULL)
  stpd_multitrack_auto(product)
}

stpd_delta_interval_union_n <- function(starts, ends) {
  starts <- suppressWarnings(as.integer(starts))
  ends <- suppressWarnings(as.integer(ends))
  ok <- is.finite(starts) & is.finite(ends) & ends >= starts
  if (!any(ok)) return(0L)
  starts <- starts[ok]
  ends <- ends[ok]
  ord <- order(starts, ends, method = "radix")
  starts <- starts[ord]
  ends <- ends[ord]
  total <- 0L
  current_start <- starts[1L]
  current_end <- ends[1L]
  if (length(starts) > 1L) {
    for (ii in 2:length(starts)) {
      if (starts[ii] <= current_end + 1L) {
        current_end <- max(current_end, ends[ii])
      } else {
        total <- total + current_end - current_start + 1L
        current_start <- starts[ii]
        current_end <- ends[ii]
      }
    }
  }
  as.integer(total + current_end - current_start + 1L)
}

# Collapse the automatic multi-track State table to one Tonic or Broad-HFS
# episode row. Broad-HFS direct-support fragments retain their common episode
# envelope; a Tonic candidate is already one State episode.
stpd_delta_state_episode_table <- function(product) {
  empty <- stpd_delta_empty_state_episodes()
  states <- as.data.frame((product %||% list())$states %||% data.frame(),
                          stringsAsFactors = FALSE)
  required <- c("train", "state_class", "start_isi", "end_isi")
  if (nrow(states) == 0L || !all(required %in% names(states))) return(empty)
  cls <- stpd_multitrack_shadow_normalize_label(states$state_class)
  keep <- cls %in% c("tonic", "high_frequency_spiking")
  keep[is.na(keep)] <- FALSE
  states <- states[keep, , drop = FALSE]
  cls <- cls[keep]
  if (nrow(states) == 0L) return(empty)

  state_id <- as.character(states$state_id %||% rep("", nrow(states)))
  episode_id <- as.character(states$state_episode_id %||% rep("", nrow(states)))
  episode_id[is.na(episode_id)] <- ""
  state_id[is.na(state_id)] <- ""
  start <- suppressWarnings(as.integer(states$start_isi))
  end <- suppressWarnings(as.integer(states$end_isi))
  episode_start <- suppressWarnings(as.integer(
    states$episode_start_isi %||% rep(NA_integer_, nrow(states))
  ))
  episode_end <- suppressWarnings(as.integer(
    states$episode_end_isi %||% rep(NA_integer_, nrow(states))
  ))
  is_hfs <- cls == "high_frequency_spiking"
  start[is_hfs & is.finite(episode_start)] <-
    episode_start[is_hfs & is.finite(episode_start)]
  end[is_hfs & is.finite(episode_end)] <-
    episode_end[is_hfs & is.finite(episode_end)]
  episode_key <- state_id
  use_episode <- is_hfs & nzchar(episode_id)
  episode_key[use_episode] <- episode_id[use_episode]
  missing_key <- !nzchar(episode_key)
  episode_key[missing_key] <- paste(
    as.character(states$train[missing_key]), cls[missing_key],
    start[missing_key], end[missing_key], sep = "|"
  )
  group_key <- paste(as.character(states$train), cls, episode_key, sep = "\r")
  groups <- split(seq_len(nrow(states)), factor(
    group_key, levels = unique(group_key)
  ))
  rows <- lapply(groups, function(idx) {
    subtype <- sort(unique(as.character(
      states$state_subtype[idx] %||% rep("", length(idx))
    )), method = "radix")
    subtype <- subtype[!is.na(subtype) & nzchar(subtype)]
    ids <- sort(unique(state_id[idx]), method = "radix")
    ids <- ids[!is.na(ids) & nzchar(ids)]
    data.frame(
      episode_row_id = 0L,
      train = as.character(states$train[idx[1L]]),
      state_class = cls[idx[1L]],
      state_subtype = paste(subtype, collapse = ";"),
      episode_key = episode_key[idx[1L]],
      source_state_ids = paste(ids, collapse = ";"),
      start_isi = min(start[idx], na.rm = TRUE),
      end_isi = max(end[idx], na.rm = TRUE),
      n_isi = max(end[idx], na.rm = TRUE) - min(start[idx], na.rm = TRUE) + 1L,
      direct_support_isi_n = stpd_delta_interval_union_n(
        states$start_isi[idx], states$end_isi[idx]
      ),
      support_fragment_n = length(idx), stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  out <- out[order(out$train, out$start_isi, out$end_isi, out$state_class,
                   method = "radix"), , drop = FALSE]
  out$episode_row_id <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

# Materialize only direct State support. Tonic has no envelope connector role,
# while Broad HFS explicitly marks direct support versus Pause/connector rows.
stpd_delta_state_support_table <- function(product) {
  empty <- stpd_delta_empty_state_support()
  per_isi <- as.data.frame((product %||% list())$per_isi %||% data.frame(),
                           stringsAsFactors = FALSE)
  required <- c("train", "isi_index", "state_class")
  if (nrow(per_isi) == 0L || !all(required %in% names(per_isi))) return(empty)
  cls <- stpd_multitrack_shadow_normalize_label(per_isi$state_class)
  role <- tolower(trimws(as.character(
    per_isi$state_support_role %||% rep("", nrow(per_isi))
  )))
  role[is.na(role)] <- ""
  keep <- cls == "tonic" |
    (cls == "high_frequency_spiking" &
       (role == "direct_support" | !("state_support_role" %in% names(per_isi))))
  keep[is.na(keep)] <- FALSE
  per_isi <- per_isi[keep, , drop = FALSE]
  cls <- cls[keep]
  role <- role[keep]
  if (nrow(per_isi) == 0L) return(empty)
  role[cls == "tonic"] <- "direct_support"
  state_id <- as.character(per_isi$state_id %||% rep("", nrow(per_isi)))
  episode_id <- as.character(
    per_isi$state_episode_id %||% rep("", nrow(per_isi))
  )
  state_id[is.na(state_id)] <- ""
  episode_id[is.na(episode_id)] <- ""
  episode_key <- state_id
  use_episode <- cls == "high_frequency_spiking" & nzchar(episode_id)
  episode_key[use_episode] <- episode_id[use_episode]
  subtype <- as.character(
    per_isi$state_subtype %||% rep("", nrow(per_isi))
  )
  subtype[is.na(subtype)] <- ""
  out <- data.frame(
    support_row_id = 0L,
    train = as.character(per_isi$train),
    isi_index = suppressWarnings(as.integer(per_isi$isi_index)),
    state_class = cls,
    state_subtype = subtype,
    state_episode_key = episode_key,
    state_support_role = role,
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$isi_index), , drop = FALSE]
  out <- out[!duplicated(paste(out$train, out$isi_index, sep = "\r")),
             , drop = FALSE]
  out <- out[order(out$train, out$isi_index, method = "radix"), , drop = FALSE]
  out$support_row_id <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

stpd_delta_match_events_any_label <- function(current_events, baseline_events, iou_min = 0.25) {
  if (is.null(current_events) || is.null(baseline_events) || nrow(current_events) == 0 || nrow(baseline_events) == 0) {
    return(data.frame(pred_index = integer(), truth_index = integer(), train = character(), pattern = character(), iou = numeric(), stringsAsFactors = FALSE))
  }
  cur <- as.data.frame(current_events, stringsAsFactors = FALSE)
  base <- as.data.frame(baseline_events, stringsAsFactors = FALSE)
  cur$.delta_any_label <- "event"
  base$.delta_any_label <- "event"
  stpd_match_events_greedy(cur, base, class_col = ".delta_any_label", iou_min = iou_min)
}

stpd_parameter_delta_count_table <- function(baseline_events, current_events) {
  patterns <- sort(unique(c(as.character(baseline_events$pattern %||% character()), as.character(current_events$pattern %||% character()))))
  patterns <- patterns[nzchar(patterns) & !is.na(patterns)]
  if (length(patterns) == 0) {
    return(data.frame(pattern = character(), baseline_n = integer(), current_n = integer(), delta_n = integer(), direction = character(), stringsAsFactors = FALSE))
  }
  base_tab <- table(factor(as.character(baseline_events$pattern %||% character()), levels = patterns))
  cur_tab <- table(factor(as.character(current_events$pattern %||% character()), levels = patterns))
  out <- data.frame(
    pattern = patterns,
    baseline_n = as.integer(base_tab),
    current_n = as.integer(cur_tab),
    stringsAsFactors = FALSE
  )
  out$delta_n <- out$current_n - out$baseline_n
  out$direction <- ifelse(out$delta_n > 0, "increased", ifelse(out$delta_n < 0, "decreased", "unchanged"))
  out
}

stpd_parameter_delta_event_diff <- function(baseline_events, current_events, iou_min = 0.25) {
  empty <- data.frame(
    status = character(),
    train = character(),
    baseline_pattern = character(),
    current_pattern = character(),
    baseline_start_isi = integer(),
    baseline_end_isi = integer(),
    current_start_isi = integer(),
    current_end_isi = integer(),
    iou = numeric(),
    baseline_event_id = integer(),
    current_event_id = integer(),
    stringsAsFactors = FALSE
  )
  base <- as.data.frame(baseline_events %||% empty_events_tbl(), stringsAsFactors = FALSE)
  cur <- as.data.frame(current_events %||% empty_events_tbl(), stringsAsFactors = FALSE)
  if (nrow(base) > 0 && !("event_row_id" %in% names(base))) base$event_row_id <- seq_len(nrow(base))
  if (nrow(cur) > 0 && !("event_row_id" %in% names(cur))) cur$event_row_id <- seq_len(nrow(cur))

  matches <- stpd_delta_match_events_any_label(cur, base, iou_min = iou_min)
  rows <- list()
  used_cur <- integer()
  used_base <- integer()
  if (!is.null(matches) && nrow(matches) > 0) {
    for (ii in seq_len(nrow(matches))) {
      ci <- as.integer(matches$pred_index[ii])
      bi <- as.integer(matches$truth_index[ii])
      if (!ci %in% seq_len(nrow(cur)) || !bi %in% seq_len(nrow(base))) next
      used_cur <- c(used_cur, ci)
      used_base <- c(used_base, bi)
      same_label <- identical(as.character(cur$pattern[ci]), as.character(base$pattern[bi]))
      same_span <- identical(as.integer(cur$start_isi[ci]), as.integer(base$start_isi[bi])) &&
        identical(as.integer(cur$end_isi[ci]), as.integer(base$end_isi[bi]))
      status <- if (!same_label) "label_changed" else if (!same_span) "boundary_changed" else "unchanged_event"
      rows[[length(rows) + 1L]] <- data.frame(
        status = status,
        train = as.character(cur$train[ci] %||% base$train[bi] %||% ""),
        baseline_pattern = as.character(base$pattern[bi] %||% ""),
        current_pattern = as.character(cur$pattern[ci] %||% ""),
        baseline_start_isi = as.integer(base$start_isi[bi] %||% NA_integer_),
        baseline_end_isi = as.integer(base$end_isi[bi] %||% NA_integer_),
        current_start_isi = as.integer(cur$start_isi[ci] %||% NA_integer_),
        current_end_isi = as.integer(cur$end_isi[ci] %||% NA_integer_),
        iou = as.numeric(matches$iou[ii] %||% NA_real_),
        baseline_event_id = as.integer(base$event_id[bi] %||% bi),
        current_event_id = as.integer(cur$event_id[ci] %||% ci),
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(cur) > 0) {
    for (ci in setdiff(seq_len(nrow(cur)), used_cur)) {
      rows[[length(rows) + 1L]] <- data.frame(
        status = "added_event",
        train = as.character(cur$train[ci] %||% ""),
        baseline_pattern = "",
        current_pattern = as.character(cur$pattern[ci] %||% ""),
        baseline_start_isi = NA_integer_,
        baseline_end_isi = NA_integer_,
        current_start_isi = as.integer(cur$start_isi[ci] %||% NA_integer_),
        current_end_isi = as.integer(cur$end_isi[ci] %||% NA_integer_),
        iou = NA_real_,
        baseline_event_id = NA_integer_,
        current_event_id = as.integer(cur$event_id[ci] %||% ci),
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(base) > 0) {
    for (bi in setdiff(seq_len(nrow(base)), used_base)) {
      rows[[length(rows) + 1L]] <- data.frame(
        status = "removed_event",
        train = as.character(base$train[bi] %||% ""),
        baseline_pattern = as.character(base$pattern[bi] %||% ""),
        current_pattern = "",
        baseline_start_isi = as.integer(base$start_isi[bi] %||% NA_integer_),
        baseline_end_isi = as.integer(base$end_isi[bi] %||% NA_integer_),
        current_start_isi = NA_integer_,
        current_end_isi = NA_integer_,
        iou = NA_real_,
        baseline_event_id = as.integer(base$event_id[bi] %||% bi),
        current_event_id = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(empty)
  out <- dplyr::bind_rows(rows)
  out[order(out$status == "unchanged_event", out$train, out$baseline_start_isi, out$current_start_isi, out$baseline_pattern, out$current_pattern), , drop = FALSE]
}

stpd_parameter_delta_state_count_table <- function(baseline_states, current_states) {
  classes <- sort(unique(c(
    as.character(baseline_states$state_class %||% character()),
    as.character(current_states$state_class %||% character())
  )), method = "radix")
  classes <- classes[!is.na(classes) & nzchar(classes)]
  if (length(classes) == 0L) {
    return(data.frame(
      state_class = character(), baseline_n = integer(),
      current_n = integer(), delta_n = integer(), direction = character(),
      stringsAsFactors = FALSE
    ))
  }
  base_tab <- table(factor(
    as.character(baseline_states$state_class %||% character()),
    levels = classes
  ))
  cur_tab <- table(factor(
    as.character(current_states$state_class %||% character()),
    levels = classes
  ))
  out <- data.frame(
    state_class = classes, baseline_n = as.integer(base_tab),
    current_n = as.integer(cur_tab), stringsAsFactors = FALSE
  )
  out$delta_n <- out$current_n - out$baseline_n
  out$direction <- ifelse(
    out$delta_n > 0L, "increased",
    ifelse(out$delta_n < 0L, "decreased", "unchanged")
  )
  out
}

stpd_parameter_delta_state_episode_diff <- function(
    baseline_states, current_states, iou_min = 0.25) {
  empty <- data.frame(
    status = character(), train = character(),
    baseline_state_class = character(), current_state_class = character(),
    baseline_state_subtype = character(), current_state_subtype = character(),
    baseline_start_isi = integer(), baseline_end_isi = integer(),
    current_start_isi = integer(), current_end_isi = integer(),
    baseline_direct_support_isi_n = integer(),
    current_direct_support_isi_n = integer(), iou = numeric(),
    baseline_episode_key = character(), current_episode_key = character(),
    stringsAsFactors = FALSE
  )
  base <- as.data.frame(
    baseline_states %||% stpd_delta_empty_state_episodes(),
    stringsAsFactors = FALSE
  )
  cur <- as.data.frame(
    current_states %||% stpd_delta_empty_state_episodes(),
    stringsAsFactors = FALSE
  )
  matches <- if (nrow(cur) == 0L || nrow(base) == 0L) {
    data.frame(
      pred_index = integer(), truth_index = integer(), train = character(),
      state_class = character(), iou = numeric(), stringsAsFactors = FALSE
    )
  } else {
    stpd_match_events_greedy(
      cur, base, class_col = "state_class", iou_min = iou_min
    )
  }
  rows <- list()
  used_cur <- integer()
  used_base <- integer()
  add_row <- function(status, bi = NA_integer_, ci = NA_integer_, overlap = NA_real_) {
    has_base <- is.finite(bi) && bi %in% seq_len(nrow(base))
    has_cur <- is.finite(ci) && ci %in% seq_len(nrow(cur))
    data.frame(
      status = status,
      train = if (has_cur) as.character(cur$train[ci]) else if (has_base) as.character(base$train[bi]) else "",
      baseline_state_class = if (has_base) as.character(base$state_class[bi]) else "",
      current_state_class = if (has_cur) as.character(cur$state_class[ci]) else "",
      baseline_state_subtype = if (has_base) as.character(base$state_subtype[bi] %||% "") else "",
      current_state_subtype = if (has_cur) as.character(cur$state_subtype[ci] %||% "") else "",
      baseline_start_isi = if (has_base) as.integer(base$start_isi[bi]) else NA_integer_,
      baseline_end_isi = if (has_base) as.integer(base$end_isi[bi]) else NA_integer_,
      current_start_isi = if (has_cur) as.integer(cur$start_isi[ci]) else NA_integer_,
      current_end_isi = if (has_cur) as.integer(cur$end_isi[ci]) else NA_integer_,
      baseline_direct_support_isi_n = if (has_base) as.integer(base$direct_support_isi_n[bi] %||% NA_integer_) else NA_integer_,
      current_direct_support_isi_n = if (has_cur) as.integer(cur$direct_support_isi_n[ci] %||% NA_integer_) else NA_integer_,
      iou = as.numeric(overlap),
      baseline_episode_key = if (has_base) as.character(base$episode_key[bi] %||% "") else "",
      current_episode_key = if (has_cur) as.character(cur$episode_key[ci] %||% "") else "",
      stringsAsFactors = FALSE
    )
  }
  if (nrow(matches) > 0L) {
    for (ii in seq_len(nrow(matches))) {
      ci <- as.integer(matches$pred_index[ii])
      bi <- as.integer(matches$truth_index[ii])
      if (!(ci %in% seq_len(nrow(cur))) || !(bi %in% seq_len(nrow(base)))) next
      used_cur <- c(used_cur, ci)
      used_base <- c(used_base, bi)
      same_class <- identical(as.character(cur$state_class[ci]), as.character(base$state_class[bi]))
      same_subtype <- identical(as.character(cur$state_subtype[ci] %||% ""), as.character(base$state_subtype[bi] %||% ""))
      same_span <- identical(as.integer(cur$start_isi[ci]), as.integer(base$start_isi[bi])) &&
        identical(as.integer(cur$end_isi[ci]), as.integer(base$end_isi[bi]))
      status <- if (!same_class) {
        "state_class_changed"
      } else if (!same_span) {
        "state_boundary_changed"
      } else if (!same_subtype) {
        "state_subtype_changed"
      } else {
        "unchanged_state_episode"
      }
      rows[[length(rows) + 1L]] <- add_row(status, bi, ci, matches$iou[ii])
    }
  }
  for (ci in setdiff(seq_len(nrow(cur)), used_cur)) {
    rows[[length(rows) + 1L]] <- add_row("added_state_episode", ci = ci)
  }
  for (bi in setdiff(seq_len(nrow(base)), used_base)) {
    rows[[length(rows) + 1L]] <- add_row("removed_state_episode", bi = bi)
  }
  if (length(rows) == 0L) return(empty)
  out <- dplyr::bind_rows(rows)
  out[order(
    out$status == "unchanged_state_episode", out$train,
    out$baseline_start_isi, out$current_start_isi,
    out$baseline_state_class, out$current_state_class, method = "radix"
  ), , drop = FALSE]
}

stpd_parameter_delta_state_support_diff <- function(
    baseline_support, current_support) {
  empty <- data.frame(
    status = character(), train = character(), isi_index = integer(),
    baseline_state_class = character(), current_state_class = character(),
    baseline_state_subtype = character(), current_state_subtype = character(),
    baseline_support_role = character(), current_support_role = character(),
    stringsAsFactors = FALSE
  )
  base <- as.data.frame(
    baseline_support %||% stpd_delta_empty_state_support(),
    stringsAsFactors = FALSE
  )
  cur <- as.data.frame(
    current_support %||% stpd_delta_empty_state_support(),
    stringsAsFactors = FALSE
  )
  if (nrow(base) == 0L && nrow(cur) == 0L) return(empty)
  base_key <- paste(base$train, base$isi_index, sep = "\r")
  cur_key <- paste(cur$train, cur$isi_index, sep = "\r")
  keys <- sort(unique(c(base_key, cur_key)), method = "radix")
  rows <- lapply(keys, function(key) {
    bi <- match(key, base_key)
    ci <- match(key, cur_key)
    has_base <- !is.na(bi)
    has_cur <- !is.na(ci)
    base_class <- if (has_base) as.character(base$state_class[bi]) else ""
    cur_class <- if (has_cur) as.character(cur$state_class[ci]) else ""
    base_subtype <- if (has_base) as.character(base$state_subtype[bi] %||% "") else ""
    cur_subtype <- if (has_cur) as.character(cur$state_subtype[ci] %||% "") else ""
    status <- if (!has_base) {
      "added_direct_support"
    } else if (!has_cur) {
      "removed_direct_support"
    } else if (!identical(base_class, cur_class)) {
      "direct_support_state_class_changed"
    } else if (!identical(base_subtype, cur_subtype)) {
      "direct_support_subtype_changed"
    } else {
      "unchanged_direct_support"
    }
    data.frame(
      status = status,
      train = if (has_cur) as.character(cur$train[ci]) else as.character(base$train[bi]),
      isi_index = if (has_cur) as.integer(cur$isi_index[ci]) else as.integer(base$isi_index[bi]),
      baseline_state_class = base_class, current_state_class = cur_class,
      baseline_state_subtype = base_subtype, current_state_subtype = cur_subtype,
      baseline_support_role = if (has_base) as.character(base$state_support_role[bi] %||% "") else "",
      current_support_role = if (has_cur) as.character(cur$state_support_role[ci] %||% "") else "",
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  out[order(out$status == "unchanged_direct_support", out$train,
            out$isi_index, method = "radix"), , drop = FALSE]
}

stpd_delta_candidate_audit <- function(ds, selected_trains = NULL) {
  if (is.null(ds) || !is.list(ds$trains)) return(data.frame())
  target <- intersect(
    as.character(selected_trains %||% names(ds$trains)), names(ds$trains)
  )
  rows <- list()
  for (train in target) {
    audit <- attr(ds$trains[[train]], "candidate_diagnostic_audit")
    if (is.null(audit) || nrow(audit) == 0L) next
    audit <- as.data.frame(audit, stringsAsFactors = FALSE)
    if (!("train" %in% names(audit))) audit$train <- train
    rows[[length(rows) + 1L]] <- audit
  }
  if (length(rows) > 0L) return(dplyr::bind_rows(rows))
  audit <- as.data.frame(
    (ds$results %||% list())$candidate_diagnostic_audit %||% data.frame(),
    stringsAsFactors = FALSE
  )
  if (nrow(audit) == 0L || !("train" %in% names(audit))) return(audit)
  audit[audit$train %in% target, , drop = FALSE]
}

# One row per HFS-side State-overlap decision. Tonic peer rows contain the same
# resolution receipt, so retaining the HFS root avoids double-counting.
stpd_delta_overlap_resolution_table <- function(candidate_audit) {
  empty <- stpd_delta_empty_overlap_resolution()
  audit <- as.data.frame(candidate_audit %||% data.frame(),
                         stringsAsFactors = FALSE)
  required <- c("train", "state_overlap_resolution", "start_isi", "end_isi")
  if (nrow(audit) == 0L || !all(required %in% names(audit))) return(empty)
  resolution <- as.character(audit$state_overlap_resolution)
  resolution[is.na(resolution)] <- ""
  final_label <- stpd_multitrack_shadow_normalize_label(
    audit$final_label %||% rep("", nrow(audit))
  )
  original_label <- stpd_multitrack_shadow_normalize_label(
    audit$suppressed_original_label %||% rep("", nrow(audit))
  )
  effective_label <- final_label
  use_original <- effective_label %in% c("", "reject") & nzchar(original_label)
  effective_label[use_original] <- original_label[use_original]
  keep <- resolution != "" & resolution != "none" &
    effective_label == "high_frequency_spiking"
  keep[is.na(keep)] <- FALSE
  audit <- audit[keep, , drop = FALSE]
  resolution <- resolution[keep]
  if (nrow(audit) == 0L) return(empty)
  col_chr <- function(name, default = "") {
    value <- if (name %in% names(audit)) as.character(audit[[name]]) else rep(default, nrow(audit))
    value[is.na(value)] <- default
    value
  }
  col_int <- function(name) {
    if (name %in% names(audit)) suppressWarnings(as.integer(audit[[name]])) else rep(NA_integer_, nrow(audit))
  }
  col_logical <- function(name) {
    if (name %in% names(audit)) as.logical(audit[[name]]) else rep(NA, nrow(audit))
  }
  start <- col_int("pre_resolution_start_isi")
  end <- col_int("pre_resolution_end_isi")
  raw_start <- col_int("start_isi")
  raw_end <- col_int("end_isi")
  start[!is.finite(start)] <- raw_start[!is.finite(start)]
  end[!is.finite(end)] <- raw_end[!is.finite(end)]
  post_start <- col_int("post_resolution_start_isi")
  post_end <- col_int("post_resolution_end_isi")
  post_start[!is.finite(post_start)] <- raw_start[!is.finite(post_start)]
  post_end[!is.finite(post_end)] <- raw_end[!is.finite(post_end)]
  root_id <- col_chr("root_hfs_candidate_id")
  candidate_id <- col_chr("candidate_id")
  root_id[!nzchar(root_id)] <- candidate_id[!nzchar(root_id)]
  out <- data.frame(
    resolution_row_id = 0L, train = col_chr("train"),
    root_hfs_candidate_id = root_id,
    peer_candidate_ids = col_chr("state_overlap_peer_candidate_id"),
    state_overlap_resolution = resolution,
    state_overlap_geometry = col_chr("state_overlap_geometry"),
    start_isi = start, end_isi = end,
    post_start_isi = post_start, post_end_isi = post_end,
    overlap_n_isi = col_int("state_overlap_n_isi"),
    residual_gate_pass = col_logical("residual_gate_pass"),
    residual_gate_status = col_chr("residual_gate_status"),
    residual_failed_checks = col_chr("residual_failed_checks"),
    resolution_reason = col_chr("resolution_reason"),
    stringsAsFactors = FALSE
  )
  key <- paste(
    out$train, out$root_hfs_candidate_id, out$state_overlap_resolution,
    out$start_isi, out$end_isi, sep = "\r"
  )
  out <- out[!duplicated(key), , drop = FALSE]
  out <- out[order(out$train, out$start_isi, out$end_isi,
                   out$state_overlap_resolution, method = "radix"), , drop = FALSE]
  out$resolution_row_id <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

stpd_parameter_delta_overlap_resolution_diff <- function(
    baseline_resolution, current_resolution, iou_min = 0.25) {
  empty <- data.frame(
    status = character(), train = character(),
    baseline_root_hfs_candidate_id = character(),
    current_root_hfs_candidate_id = character(),
    baseline_peer_candidate_ids = character(),
    current_peer_candidate_ids = character(),
    baseline_resolution = character(), current_resolution = character(),
    baseline_geometry = character(), current_geometry = character(),
    baseline_start_isi = integer(), baseline_end_isi = integer(),
    current_start_isi = integer(), current_end_isi = integer(),
    baseline_post_start_isi = integer(), baseline_post_end_isi = integer(),
    current_post_start_isi = integer(), current_post_end_isi = integer(),
    baseline_overlap_n_isi = integer(), current_overlap_n_isi = integer(),
    baseline_residual_gate_pass = logical(),
    current_residual_gate_pass = logical(),
    baseline_residual_gate_status = character(),
    current_residual_gate_status = character(),
    baseline_residual_failed_checks = character(),
    current_residual_failed_checks = character(),
    baseline_resolution_reason = character(),
    current_resolution_reason = character(), iou = numeric(),
    stringsAsFactors = FALSE
  )
  base <- as.data.frame(
    baseline_resolution %||% stpd_delta_empty_overlap_resolution(),
    stringsAsFactors = FALSE
  )
  cur <- as.data.frame(
    current_resolution %||% stpd_delta_empty_overlap_resolution(),
    stringsAsFactors = FALSE
  )
  matches <- stpd_delta_match_events_any_label(cur, base, iou_min = iou_min)
  rows <- list()
  used_cur <- integer()
  used_base <- integer()
  add_row <- function(status, bi = NA_integer_, ci = NA_integer_, overlap = NA_real_) {
    has_base <- is.finite(bi) && bi %in% seq_len(nrow(base))
    has_cur <- is.finite(ci) && ci %in% seq_len(nrow(cur))
    data.frame(
      status = status,
      train = if (has_cur) as.character(cur$train[ci]) else if (has_base) as.character(base$train[bi]) else "",
      baseline_root_hfs_candidate_id = if (has_base) as.character(base$root_hfs_candidate_id[bi] %||% "") else "",
      current_root_hfs_candidate_id = if (has_cur) as.character(cur$root_hfs_candidate_id[ci] %||% "") else "",
      baseline_peer_candidate_ids = if (has_base) as.character(base$peer_candidate_ids[bi] %||% "") else "",
      current_peer_candidate_ids = if (has_cur) as.character(cur$peer_candidate_ids[ci] %||% "") else "",
      baseline_resolution = if (has_base) as.character(base$state_overlap_resolution[bi]) else "",
      current_resolution = if (has_cur) as.character(cur$state_overlap_resolution[ci]) else "",
      baseline_geometry = if (has_base) as.character(base$state_overlap_geometry[bi] %||% "") else "",
      current_geometry = if (has_cur) as.character(cur$state_overlap_geometry[ci] %||% "") else "",
      baseline_start_isi = if (has_base) as.integer(base$start_isi[bi]) else NA_integer_,
      baseline_end_isi = if (has_base) as.integer(base$end_isi[bi]) else NA_integer_,
      current_start_isi = if (has_cur) as.integer(cur$start_isi[ci]) else NA_integer_,
      current_end_isi = if (has_cur) as.integer(cur$end_isi[ci]) else NA_integer_,
      baseline_post_start_isi = if (has_base) as.integer(base$post_start_isi[bi]) else NA_integer_,
      baseline_post_end_isi = if (has_base) as.integer(base$post_end_isi[bi]) else NA_integer_,
      current_post_start_isi = if (has_cur) as.integer(cur$post_start_isi[ci]) else NA_integer_,
      current_post_end_isi = if (has_cur) as.integer(cur$post_end_isi[ci]) else NA_integer_,
      baseline_overlap_n_isi = if (has_base) as.integer(base$overlap_n_isi[bi]) else NA_integer_,
      current_overlap_n_isi = if (has_cur) as.integer(cur$overlap_n_isi[ci]) else NA_integer_,
      baseline_residual_gate_pass = if (has_base) as.logical(base$residual_gate_pass[bi]) else NA,
      current_residual_gate_pass = if (has_cur) as.logical(cur$residual_gate_pass[ci]) else NA,
      baseline_residual_gate_status = if (has_base) as.character(base$residual_gate_status[bi] %||% "") else "",
      current_residual_gate_status = if (has_cur) as.character(cur$residual_gate_status[ci] %||% "") else "",
      baseline_residual_failed_checks = if (has_base) as.character(base$residual_failed_checks[bi] %||% "") else "",
      current_residual_failed_checks = if (has_cur) as.character(cur$residual_failed_checks[ci] %||% "") else "",
      baseline_resolution_reason = if (has_base) as.character(base$resolution_reason[bi] %||% "") else "",
      current_resolution_reason = if (has_cur) as.character(cur$resolution_reason[ci] %||% "") else "",
      iou = as.numeric(overlap), stringsAsFactors = FALSE
    )
  }
  if (nrow(matches) > 0L) {
    for (ii in seq_len(nrow(matches))) {
      ci <- as.integer(matches$pred_index[ii])
      bi <- as.integer(matches$truth_index[ii])
      if (!(ci %in% seq_len(nrow(cur))) || !(bi %in% seq_len(nrow(base)))) next
      used_cur <- c(used_cur, ci)
      used_base <- c(used_base, bi)
      same_resolution <- identical(
        as.character(cur$state_overlap_resolution[ci]),
        as.character(base$state_overlap_resolution[bi])
      )
      same_post <- identical(as.integer(cur$post_start_isi[ci]), as.integer(base$post_start_isi[bi])) &&
        identical(as.integer(cur$post_end_isi[ci]), as.integer(base$post_end_isi[bi]))
      status <- if (!same_resolution) {
        "overlap_resolution_changed"
      } else if (!same_post) {
        "overlap_resolution_boundary_changed"
      } else {
        "unchanged_overlap_resolution"
      }
      rows[[length(rows) + 1L]] <- add_row(status, bi, ci, matches$iou[ii])
    }
  }
  for (ci in setdiff(seq_len(nrow(cur)), used_cur)) {
    rows[[length(rows) + 1L]] <- add_row("added_overlap_resolution", ci = ci)
  }
  for (bi in setdiff(seq_len(nrow(base)), used_base)) {
    rows[[length(rows) + 1L]] <- add_row("removed_overlap_resolution", bi = bi)
  }
  if (length(rows) == 0L) return(empty)
  out <- dplyr::bind_rows(rows)
  out[order(out$status == "unchanged_overlap_resolution", out$train,
            out$baseline_start_isi, out$current_start_isi, method = "radix"),
      , drop = FALSE]
}

stpd_parameter_delta_summary <- function(
    event_diff, count_table, selected_trains, parameter_changes, source = "auto",
    state_episode_diff = NULL, state_counts = NULL,
    baseline_state_support = NULL, current_state_support = NULL,
    state_support_diff = NULL, baseline_overlap_resolution = NULL,
    current_overlap_resolution = NULL, overlap_resolution_diff = NULL) {
  event_diff <- event_diff %||% data.frame(status = character(), stringsAsFactors = FALSE)
  count_table <- count_table %||% data.frame()
  status_n <- table(factor(as.character(event_diff$status %||% character()), levels = c("added_event", "removed_event", "label_changed", "boundary_changed", "unchanged_event")))
  changed_n <- sum(event_diff$status %in% c("added_event", "removed_event", "label_changed", "boundary_changed"), na.rm = TRUE)
  legacy <- data.frame(
    metric = c(
      "source_compared", "selected_train_n", "selected_trains", "parameter_change_n",
      "baseline_event_n", "current_event_n", "changed_event_n",
      "added_event_n", "removed_event_n", "label_changed_n", "boundary_changed_n", "unchanged_event_n"
    ),
    value = c(
      source,
      as.character(length(selected_trains)),
      paste(selected_trains, collapse = ";"),
      as.character(if (is.null(parameter_changes) || !("path" %in% names(parameter_changes))) 0L else nrow(parameter_changes)),
      as.character(sum(count_table$baseline_n %||% 0L, na.rm = TRUE)),
      as.character(sum(count_table$current_n %||% 0L, na.rm = TRUE)),
      as.character(changed_n),
      as.character(status_n[["added_event"]]),
      as.character(status_n[["removed_event"]]),
      as.character(status_n[["label_changed"]]),
      as.character(status_n[["boundary_changed"]]),
      as.character(status_n[["unchanged_event"]])
    ),
    stringsAsFactors = FALSE
  )
  multitrack_supplied <- !is.null(state_episode_diff) || !is.null(state_counts) ||
    !is.null(baseline_state_support) || !is.null(current_state_support) ||
    !is.null(state_support_diff) || !is.null(baseline_overlap_resolution) ||
    !is.null(current_overlap_resolution) || !is.null(overlap_resolution_diff)
  if (!multitrack_supplied) return(legacy)

  state_episode_diff <- state_episode_diff %||% data.frame(status = character())
  state_counts <- state_counts %||% data.frame(
    state_class = character(), baseline_n = integer(), current_n = integer(),
    delta_n = integer()
  )
  baseline_state_support <- baseline_state_support %||%
    stpd_delta_empty_state_support()
  current_state_support <- current_state_support %||%
    stpd_delta_empty_state_support()
  state_support_diff <- state_support_diff %||% data.frame(status = character())
  baseline_overlap_resolution <- baseline_overlap_resolution %||%
    stpd_delta_empty_overlap_resolution()
  current_overlap_resolution <- current_overlap_resolution %||%
    stpd_delta_empty_overlap_resolution()
  overlap_resolution_diff <- overlap_resolution_diff %||%
    data.frame(status = character())

  state_count <- function(class, column) {
    hit <- as.character(state_counts$state_class %||% character()) == class
    if (!any(hit) || !(column %in% names(state_counts))) return(0L)
    as.integer(sum(state_counts[[column]][hit], na.rm = TRUE))
  }
  support_count <- function(x, class) {
    as.integer(sum(
      as.character(x$state_class %||% character()) == class, na.rm = TRUE
    ))
  }
  changed_state_n <- sum(!(
    as.character(state_episode_diff$status %||% character()) %in%
      "unchanged_state_episode"
  ), na.rm = TRUE)
  changed_support_n <- sum(!(
    as.character(state_support_diff$status %||% character()) %in%
      "unchanged_direct_support"
  ), na.rm = TRUE)
  changed_resolution_n <- sum(!(
    as.character(overlap_resolution_diff$status %||% character()) %in%
      "unchanged_overlap_resolution"
  ), na.rm = TRUE)
  multitrack <- data.frame(
    metric = c(
      "baseline_tonic_state_episode_n", "current_tonic_state_episode_n",
      "delta_tonic_state_episode_n", "baseline_broad_hfs_state_episode_n",
      "current_broad_hfs_state_episode_n", "delta_broad_hfs_state_episode_n",
      "changed_state_episode_n", "baseline_tonic_direct_support_isi_n",
      "current_tonic_direct_support_isi_n",
      "baseline_broad_hfs_direct_support_isi_n",
      "current_broad_hfs_direct_support_isi_n",
      "changed_state_direct_support_isi_n",
      "baseline_overlap_resolution_n", "current_overlap_resolution_n",
      "changed_overlap_resolution_n"
    ),
    value = as.character(c(
      state_count("tonic", "baseline_n"),
      state_count("tonic", "current_n"),
      state_count("tonic", "delta_n"),
      state_count("high_frequency_spiking", "baseline_n"),
      state_count("high_frequency_spiking", "current_n"),
      state_count("high_frequency_spiking", "delta_n"),
      changed_state_n,
      support_count(baseline_state_support, "tonic"),
      support_count(current_state_support, "tonic"),
      support_count(baseline_state_support, "high_frequency_spiking"),
      support_count(current_state_support, "high_frequency_spiking"),
      changed_support_n,
      nrow(baseline_overlap_resolution), nrow(current_overlap_resolution),
      changed_resolution_n
    )),
    stringsAsFactors = FALSE
  )
  dplyr::bind_rows(legacy, multitrack)
}

stpd_parameter_delta_overlay_rows <- function(preview, trains, selected_trains = NULL) {
  diff <- preview$event_diff %||% data.frame()
  if (is.null(diff) || nrow(diff) == 0 || is.null(trains) || length(trains) == 0) {
    return(data.frame(
      delta_row_index = integer(),
      status = character(),
      train = character(),
      baseline_pattern = character(),
      current_pattern = character(),
      start_isi = integer(),
      end_isi = integer(),
      start_align_sec = numeric(),
      end_align_sec = numeric(),
      iou = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  target <- as.character(selected_trains %||% names(trains))
  rows <- list()
  for (ii in seq_len(nrow(diff))) {
    tr <- as.character(diff$train[ii] %||% "")
    if (!nzchar(tr) || !(tr %in% names(trains)) || !(tr %in% target)) next
    dat <- trains[[tr]]
    if (is.null(dat) || nrow(dat) == 0 || !("timestamp_sec" %in% names(dat))) next
    starts <- suppressWarnings(as.integer(c(diff$baseline_start_isi[ii], diff$current_start_isi[ii])))
    ends <- suppressWarnings(as.integer(c(diff$baseline_end_isi[ii], diff$current_end_isi[ii])))
    starts <- starts[is.finite(starts)]
    ends <- ends[is.finite(ends)]
    if (length(starts) == 0 || length(ends) == 0) next
    s_isi <- max(1L, min(starts, na.rm = TRUE))
    e_isi <- max(ends, na.rm = TRUE)
    if (!is.finite(s_isi) || !is.finite(e_isi) || e_isi < s_isi) next
    s_spk <- max(1L, s_isi - 1L)
    e_spk <- min(nrow(dat), e_isi)
    if (s_spk > nrow(dat) || e_spk < 1L || e_spk < s_spk) next
    t0 <- suppressWarnings(as.numeric(dat$timestamp_sec[s_spk] - dat$timestamp_sec[1]))
    t1 <- suppressWarnings(as.numeric(dat$timestamp_sec[e_spk] - dat$timestamp_sec[1]))
    if (!is.finite(t0) || !is.finite(t1)) next
    rows[[length(rows) + 1L]] <- data.frame(
      delta_row_index = ii,
      status = as.character(diff$status[ii] %||% ""),
      train = tr,
      baseline_pattern = as.character(diff$baseline_pattern[ii] %||% ""),
      current_pattern = as.character(diff$current_pattern[ii] %||% ""),
      start_isi = s_isi,
      end_isi = e_isi,
      start_align_sec = min(t0, t1),
      end_align_sec = max(t0, t1),
      iou = suppressWarnings(as.numeric(diff$iou[ii] %||% NA_real_)),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(
      delta_row_index = integer(),
      status = character(),
      train = character(),
      baseline_pattern = character(),
      current_pattern = character(),
      start_isi = integer(),
      end_isi = integer(),
      start_align_sec = numeric(),
      end_align_sec = numeric(),
      iou = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  out <- dplyr::bind_rows(rows)
  for (nm in c("status", "train", "baseline_pattern", "current_pattern")) {
    out[[nm]][is.na(out[[nm]])] <- ""
  }
  out
}

stpd_parameter_delta_export <- function(preview, out_dir) {
  if (is.null(preview) || !is.list(preview)) stop("No parameter delta preview is available to export.", call. = FALSE)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- preview$summary %||% data.frame(message = "No parameter delta preview summary.", stringsAsFactors = FALSE)
  counts <- preview$counts %||% data.frame(message = "No parameter delta preview counts.", stringsAsFactors = FALSE)
  events <- preview$event_diff %||% data.frame(message = "No parameter delta preview changed events.", stringsAsFactors = FALSE)
  state_counts <- preview$state_counts %||% data.frame(
    message = "No parameter delta preview State counts.",
    stringsAsFactors = FALSE
  )
  state_episodes <- preview$state_episode_diff %||% data.frame(
    message = "No parameter delta preview changed State episodes.",
    stringsAsFactors = FALSE
  )
  state_support <- preview$state_direct_support_diff %||% data.frame(
    message = "No parameter delta preview changed direct State support.",
    stringsAsFactors = FALSE
  )
  overlap_resolution <- preview$overlap_resolution_diff %||% data.frame(
    message = "No parameter delta preview changed State-overlap resolutions.",
    stringsAsFactors = FALSE
  )
  parameter_changes <- preview$parameter_changes %||% data.frame(
    message = "No parameter changes were supplied for this preview.",
    stringsAsFactors = FALSE
  )
  write_csv_safe(summary, file.path(out_dir, "Parameter_delta_preview_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(counts, file.path(out_dir, "Parameter_delta_preview_counts.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(events, file.path(out_dir, "Parameter_delta_preview_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(state_counts, file.path(out_dir, "Parameter_delta_preview_state_counts.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(state_episodes, file.path(out_dir, "Parameter_delta_preview_state_episodes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(state_support, file.path(out_dir, "Parameter_delta_preview_state_direct_support.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(overlap_resolution, file.path(out_dir, "Parameter_delta_preview_overlap_resolution.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(parameter_changes, file.path(out_dir, "Parameter_delta_preview_parameter_changes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out_dir)
}

stpd_parameter_delta_preview <- function(ds, params_current = default_params_sec(),
                                         params_baseline = default_params_sec(),
                                         selected_trains = NULL,
                                         max_trains = 3L,
                                         iou_min = 0.25,
                                         source = c("auto", "final"),
                                         lock_manual = TRUE,
                                         collect_diagnostics = FALSE,
                                         label_blind = FALSE) {
  source <- match.arg(source)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_parameter_delta_preview(): ds must be a dataset with a trains list.", call. = FALSE)
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  if (length(target) == 0) stop("No target trains found for parameter delta preview.", call. = FALSE)
  max_trains <- suppressWarnings(as.integer(max_trains %||% 3L))
  if (!is.finite(max_trains) || max_trains < 1L) max_trains <- 3L
  target <- head(target, max_trains)
  iou_min <- suppressWarnings(as.numeric(iou_min %||% 0.25))
  if (!is.finite(iou_min)) iou_min <- 0.25
  iou_min <- max(0.01, min(1, iou_min))

  baseline_run <- stpd_detect(
    ds, params_baseline, selected_trains = target, lock_manual = lock_manual,
    collect_diagnostics = collect_diagnostics, label_blind = label_blind
  )
  current_run <- stpd_detect(
    ds, params_current, selected_trains = target, lock_manual = lock_manual,
    collect_diagnostics = collect_diagnostics, label_blind = label_blind
  )
  baseline_events <- stpd_delta_event_table(baseline_run, params_baseline, target, source = source)
  current_events <- stpd_delta_event_table(current_run, params_current, target, source = source)
  event_diff <- stpd_parameter_delta_event_diff(baseline_events, current_events, iou_min = iou_min)
  count_table <- stpd_parameter_delta_count_table(baseline_events, current_events)

  baseline_product <- stpd_delta_multitrack_product(baseline_run)
  current_product <- stpd_delta_multitrack_product(current_run)
  baseline_state_episodes <- stpd_delta_state_episode_table(baseline_product)
  current_state_episodes <- stpd_delta_state_episode_table(current_product)
  state_episode_diff <- stpd_parameter_delta_state_episode_diff(
    baseline_state_episodes, current_state_episodes, iou_min = iou_min
  )
  state_count_table <- stpd_parameter_delta_state_count_table(
    baseline_state_episodes, current_state_episodes
  )
  baseline_state_support <- stpd_delta_state_support_table(baseline_product)
  current_state_support <- stpd_delta_state_support_table(current_product)
  state_support_diff <- stpd_parameter_delta_state_support_diff(
    baseline_state_support, current_state_support
  )
  baseline_overlap_resolution <- stpd_delta_overlap_resolution_table(
    stpd_delta_candidate_audit(baseline_run, target)
  )
  current_overlap_resolution <- stpd_delta_overlap_resolution_table(
    stpd_delta_candidate_audit(current_run, target)
  )
  overlap_resolution_diff <- stpd_parameter_delta_overlap_resolution_diff(
    baseline_overlap_resolution, current_overlap_resolution, iou_min = iou_min
  )

  parameter_changes <- stpd_parameter_change_preview(params_current, baseline = params_baseline)
  if ("message" %in% names(parameter_changes)) parameter_changes <- data.frame()
  summary <- stpd_parameter_delta_summary(
    event_diff, count_table, target, parameter_changes, source = source,
    state_episode_diff = state_episode_diff,
    state_counts = state_count_table,
    baseline_state_support = baseline_state_support,
    current_state_support = current_state_support,
    state_support_diff = state_support_diff,
    baseline_overlap_resolution = baseline_overlap_resolution,
    current_overlap_resolution = current_overlap_resolution,
    overlap_resolution_diff = overlap_resolution_diff
  )
  list(
    summary = summary,
    counts = count_table,
    event_diff = event_diff[event_diff$status != "unchanged_event", , drop = FALSE],
    event_diff_all = event_diff,
    state_counts = state_count_table,
    state_episode_diff = state_episode_diff[
      state_episode_diff$status != "unchanged_state_episode", , drop = FALSE
    ],
    state_episode_diff_all = state_episode_diff,
    state_direct_support_diff = state_support_diff[
      state_support_diff$status != "unchanged_direct_support", , drop = FALSE
    ],
    state_direct_support_diff_all = state_support_diff,
    overlap_resolution_diff = overlap_resolution_diff[
      overlap_resolution_diff$status != "unchanged_overlap_resolution",
      , drop = FALSE
    ],
    overlap_resolution_diff_all = overlap_resolution_diff,
    parameter_changes = parameter_changes,
    baseline_events = baseline_events,
    current_events = current_events,
    baseline_state_episodes = baseline_state_episodes,
    current_state_episodes = current_state_episodes,
    baseline_state_direct_support = baseline_state_support,
    current_state_direct_support = current_state_support,
    baseline_overlap_resolution = baseline_overlap_resolution,
    current_overlap_resolution = current_overlap_resolution,
    selected_trains = target,
    iou_min = iou_min,
    source = source
  )
}
