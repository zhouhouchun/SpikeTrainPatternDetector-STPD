# Controlled possible_burst -> burst promotion.
#
# possible_burst is an algorithmic review label. These helpers implement a
# user-review layer that can promote selected possible_burst intervals to burst
# while preserving the original automatic label and a reversible audit trail.

stpd_chr_vec <- function(x, n, default = "") {
  if (is.null(x)) x <- rep(default, n)
  x <- as.character(x)
  if (length(x) < n) x <- c(x, rep(default, n - length(x)))
  if (length(x) > n) x <- x[seq_len(n)]
  x[is.na(x)] <- default
  x
}

stpd_possible_to_real_map <- function() {
  c(
    possible_burst = "burst",
    possible_long_burst = "long_burst",
    possible_tonic = "tonic",
    possible_pause = "pause",
    possible_high_frequency_tonic = "high_frequency_tonic",
    possible_hf_tonic = "high_frequency_tonic",
    possible_high_frequency_spiking = "high_frequency_spiking",
    possible_hf_spiking = "high_frequency_spiking",
    possible_others = "others"
  )
}

stpd_promote_possible_labels <- function(labels, promote_possible = TRUE, promotion_map = NULL) {
  x <- as.character(labels)
  x[is.na(x)] <- ""
  if (!isTRUE(promote_possible)) return(x)
  map <- promotion_map %||% stpd_possible_to_real_map()
  map <- as.character(map)
  names(map) <- as.character(names(map))
  hit <- match(x, names(map))
  use <- !is.na(hit)
  if (any(use)) x[use] <- unname(map[hit[use]])

  real_labels <- c(
    "burst", "long_burst", "tonic", "pause", "high_frequency_tonic",
    "high_frequency_spiking", "others"
  )
  possible <- grepl("^possible_", x)
  suffix <- sub("^possible_", "", x)
  fallback <- possible & suffix %in% real_labels
  if (any(fallback)) x[fallback] <- suffix[fallback]
  x
}

stpd_audit_final_labels <- function(dat,
                                    min_isi_sec = 0.001,
                                    auto_others = FALSE,
                                    prefer_stored = TRUE,
                                    promote_possible = FALSE,
                                    promotion_map = NULL) {
  n <- if (is.null(dat)) 0L else nrow(dat)
  if (n == 0L) return(character(0))
  isi <- suppressWarnings(as.numeric(dat$ISI_sec %||% rep(NA_real_, n)))
  if (isTRUE(prefer_stored) && "pattern_audit_final" %in% names(dat)) {
    out <- stpd_chr_vec(dat$pattern_audit_final, n)
  } else {
    out <- compute_final_pattern(
      dat$pattern_manual %||% rep("", n),
      dat$pattern_auto %||% rep("", n),
      isi,
      auto_others = FALSE,
      min_isi_sec = min_isi_sec
    )
    out <- stpd_promote_possible_labels(out, promote_possible = promote_possible, promotion_map = promotion_map)
  }
  if (isTRUE(auto_others)) out <- fill_unlabeled_others_for_display(out, isi, min_isi_sec = min_isi_sec)
  out
}

stpd_empty_final_audit_summary <- function() {
  data.frame(
    train = character(),
    n_isi = integer(),
    n_labeled_final = integer(),
    n_labeled_audit_final = integer(),
    n_possible_before = integer(),
    n_possible_after = integer(),
    n_promoted_isi = integer(),
    n_promoted_events = integer(),
    promote_possible = logical(),
    audit_id = character(),
    reason = character(),
    user = character(),
    time = character(),
    stringsAsFactors = FALSE
  )
}

stpd_empty_final_audit_events <- function() {
  data.frame(
    train = character(),
    start_isi = integer(),
    end_isi = integer(),
    start_spike_idx = integer(),
    end_spike_idx = integer(),
    n_isi = integer(),
    n_spikes = integer(),
    start_time_sec = numeric(),
    end_time_sec = numeric(),
    duration_sec = numeric(),
    from_label = character(),
    to_label = character(),
    audit_id = character(),
    action = character(),
    reason = character(),
    user = character(),
    time = character(),
    stringsAsFactors = FALSE
  )
}

stpd_final_audit_latest_by_train <- function(df, empty_df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || !("train" %in% names(df))) return(empty_df)
  train <- as.character(df$train)
  keep <- !duplicated(train, fromLast = TRUE)
  df[keep, , drop = FALSE]
}

stpd_final_audit_replace_train_rows <- function(old, new, trains, empty_df, dedupe_train = FALSE) {
  trains <- as.character(trains %||% character(0))
  old <- old %||% empty_df
  new <- new %||% empty_df
  if (!is.data.frame(old) || nrow(old) == 0L || !("train" %in% names(old))) old <- empty_df
  if (!is.data.frame(new) || nrow(new) == 0L || !("train" %in% names(new))) new <- empty_df
  if (isTRUE(dedupe_train)) old <- stpd_final_audit_latest_by_train(old, empty_df)
  if (nrow(old) > 0L && length(trains) > 0L) {
    old <- old[!(as.character(old$train) %in% trains), , drop = FALSE]
  }
  if (nrow(old) > 0L && nrow(new) > 0L) {
    out <- dplyr::bind_rows(old, new)
  } else if (nrow(new) > 0L) {
    out <- new
  } else {
    out <- old
  }
  if (is.null(out) || nrow(out) == 0L) empty_df else out
}

stpd_final_audit_append_history <- function(old, new, empty_df) {
  old <- old %||% empty_df
  new <- new %||% empty_df
  if (!is.data.frame(old) || nrow(old) == 0L) old <- empty_df
  if (!is.data.frame(new) || nrow(new) == 0L) new <- empty_df
  if (nrow(old) > 0L && nrow(new) > 0L) {
    dplyr::bind_rows(old, new)
  } else if (nrow(new) > 0L) {
    new
  } else {
    old
  }
}

stpd_final_audit_event_rows <- function(dat, train, base_labels, audit_labels,
                                        audit_id = "", reason = "", user = "",
                                        time_chr = "") {
  n <- nrow(dat)
  base_labels <- stpd_chr_vec(base_labels, n)
  audit_labels <- stpd_chr_vec(audit_labels, n)
  changed <- which(seq_len(n) >= 2L & nzchar(base_labels) & nzchar(audit_labels) & base_labels != audit_labels)
  if (length(changed) == 0L) return(stpd_empty_final_audit_events())
  event_rows <- list()
  for (lab in unique(audit_labels[changed])) {
    idx <- changed[audit_labels[changed] == lab]
    tmp <- rep("", n)
    tmp[idx] <- lab
    seg <- label_segments(tmp)
    if (nrow(seg) == 0L) next
    for (ii in seq_len(nrow(seg))) {
      s <- as.integer(seg$start_isi[ii])
      e <- as.integer(seg$end_isi[ii])
      idx2 <- seq(max(2L, s), min(n, e))
      from_lab <- mode_nonempty_label(base_labels[idx2])
      if (!nzchar(from_lab)) from_lab <- base_labels[idx2][1] %||% ""
      start_t <- suppressWarnings(as.numeric(dat$timestamp_sec[max(1L, s - 1L)]))
      end_t <- suppressWarnings(as.numeric(dat$timestamp_sec[min(n, e)]))
      event_rows[[length(event_rows) + 1L]] <- data.frame(
        train = train,
        start_isi = s,
        end_isi = e,
        start_spike_idx = max(1L, s - 1L),
        end_spike_idx = e,
        n_isi = e - s + 1L,
        n_spikes = e - s + 2L,
        start_time_sec = start_t,
        end_time_sec = end_t,
        duration_sec = end_t - start_t,
        from_label = from_lab,
        to_label = lab,
        audit_id = audit_id,
        action = "promote_possible_to_real",
        reason = reason,
        user = user,
        time = time_chr,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(event_rows) == 0L) stpd_empty_final_audit_events() else do.call(rbind, event_rows)
}

stpd_apply_final_audit <- function(ds,
                                   selected_trains = NULL,
                                   promote_possible = FALSE,
                                   promotion_map = NULL,
                                   min_isi_sec = 0.001,
                                   audit_id = NULL,
                                   reason = "final_audit_rebuild",
                                   user = NA_character_) {
  if (is.null(ds) || is.null(ds$trains)) stop("Dataset has no trains.", call. = FALSE)
  if (is.null(ds$results)) ds$results <- list()
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(as.character(trains), names(ds$trains))
  if (length(trains) == 0L) stop("No selected trains found.", call. = FALSE)
  time_chr <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  if (is.null(audit_id) || !nzchar(as.character(audit_id)[1])) {
    audit_id <- paste0("final_audit_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  audit_id <- as.character(audit_id)[1]
  reason <- as.character(reason %||% "final_audit_rebuild")[1]
  user <- as.character(user %||% NA_character_)[1]
  summary_rows <- list()
  event_rows <- list()

  for (tr in trains) {
    dat <- ds$trains[[tr]]
    n <- nrow(dat)
    if (n == 0L) next
    dat$pattern_manual <- stpd_chr_vec(dat$pattern_manual, n)
    dat$pattern_auto <- stpd_chr_vec(dat$pattern_auto, n)
    base <- compute_final_pattern(
      dat$pattern_manual,
      dat$pattern_auto,
      dat$ISI_sec,
      auto_others = FALSE,
      min_isi_sec = min_isi_sec
    )
    audit <- stpd_promote_possible_labels(base, promote_possible = promote_possible, promotion_map = promotion_map)
    changed <- nzchar(base) & nzchar(audit) & base != audit
    dat$pattern_audit_final <- audit
    dat$pattern_audit_base_final <- base
    dat$pattern_audit_from <- ifelse(changed, base, "")
    dat$pattern_audit_to <- ifelse(changed, audit, "")
    dat$pattern_audit_action <- ifelse(changed, "promote_possible_to_real",
                                       ifelse(nzchar(audit), "base_final", "unlabeled"))
    dat$pattern_audit_source <- ifelse(changed, "user_final_audit",
                                       ifelse(nzchar(dat$pattern_manual), "manual",
                                              ifelse(nzchar(dat$pattern_auto), "auto", "none")))
    dat$pattern_audit_reason <- ifelse(changed, reason, "")
    dat$pattern_audit_id <- ifelse(changed | nzchar(audit), audit_id, "")
    dat$pattern_audit_time <- ifelse(changed | nzchar(audit), time_chr, "")
    ds$trains[[tr]] <- dat

    ev <- stpd_final_audit_event_rows(dat, tr, base, audit,
                                      audit_id = audit_id, reason = reason,
                                      user = user, time_chr = time_chr)
    if (nrow(ev) > 0L) event_rows[[length(event_rows) + 1L]] <- ev
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      train = tr,
      n_isi = n,
      n_labeled_final = sum(nzchar(base), na.rm = TRUE),
      n_labeled_audit_final = sum(nzchar(audit), na.rm = TRUE),
      n_possible_before = sum(grepl("^possible_", base), na.rm = TRUE),
      n_possible_after = sum(grepl("^possible_", audit), na.rm = TRUE),
      n_promoted_isi = sum(changed, na.rm = TRUE),
      n_promoted_events = nrow(ev),
      promote_possible = isTRUE(promote_possible),
      audit_id = audit_id,
      reason = reason,
      user = user,
      time = time_chr,
      stringsAsFactors = FALSE
    )
  }

  summary <- if (length(summary_rows) > 0L) do.call(rbind, summary_rows) else stpd_empty_final_audit_summary()
  events <- if (length(event_rows) > 0L) do.call(rbind, event_rows) else stpd_empty_final_audit_events()
  ds$results$final_audit_policy <- list(
    promote_possible = isTRUE(promote_possible),
    selected_trains = trains,
    audit_id = audit_id,
    reason = reason,
    user = user,
    time = time_chr
  )
  old_summary <- ds$results$final_audit_summary %||% data.frame()
  old_events <- ds$results$final_audit_events %||% data.frame()
  old_current_summary <- ds$results$final_audit_current_summary %||% old_summary
  old_current_events <- ds$results$final_audit_current_events %||% old_events
  old_history <- ds$results$final_audit_history %||% old_summary
  old_event_history <- ds$results$final_audit_event_history %||% old_events

  current_summary <- stpd_final_audit_replace_train_rows(
    old_current_summary, summary, trains, stpd_empty_final_audit_summary(), dedupe_train = TRUE
  )
  current_events <- stpd_final_audit_replace_train_rows(
    old_current_events, events, trains, stpd_empty_final_audit_events(), dedupe_train = FALSE
  )
  history <- stpd_final_audit_append_history(old_history, summary, stpd_empty_final_audit_summary())
  event_history <- stpd_final_audit_append_history(old_event_history, events, stpd_empty_final_audit_events())

  ds$results$final_audit_current_summary <- current_summary
  ds$results$final_audit_current_events <- current_events
  ds$results$final_audit_history <- history
  ds$results$final_audit_event_history <- event_history
  # Backward-compatible aliases: these now represent the current audit state,
  # while final_audit_history/final_audit_event_history retain operation logs.
  ds$results$final_audit_summary <- current_summary
  ds$results$final_audit_events <- current_events
  list(dataset = ds, summary = summary, events = events, policy = ds$results$final_audit_policy)
}

stpd_clear_final_audit <- function(ds, selected_trains = NULL) {
  if (is.null(ds) || is.null(ds$trains)) stop("Dataset has no trains.", call. = FALSE)
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(as.character(trains), names(ds$trains))
  audit_cols <- c(
    "pattern_audit_final", "pattern_audit_base_final", "pattern_audit_from",
    "pattern_audit_to", "pattern_audit_action", "pattern_audit_source",
    "pattern_audit_reason", "pattern_audit_id", "pattern_audit_time"
  )
  for (tr in trains) {
    dat <- ds$trains[[tr]]
    dat[intersect(audit_cols, names(dat))] <- NULL
    ds$trains[[tr]] <- dat
  }
  if (is.null(ds$results)) ds$results <- list()
  clearing_all <- length(setdiff(names(ds$trains), trains)) == 0L
  if (isTRUE(clearing_all)) {
    ds$results$final_audit_policy <- NULL
    ds$results$final_audit_summary <- NULL
    ds$results$final_audit_events <- NULL
    ds$results$final_audit_current_summary <- NULL
    ds$results$final_audit_current_events <- NULL
  } else {
    for (nm in c("final_audit_summary", "final_audit_current_summary")) {
      if (!is.null(ds$results[[nm]]) && nrow(ds$results[[nm]]) > 0L &&
          "train" %in% names(ds$results[[nm]])) {
        ds$results[[nm]] <- ds$results[[nm]][
          !(as.character(ds$results[[nm]]$train) %in% trains),
          , drop = FALSE
        ]
      }
    }
    for (nm in c("final_audit_events", "final_audit_current_events")) {
      if (!is.null(ds$results[[nm]]) && nrow(ds$results[[nm]]) > 0L &&
          "train" %in% names(ds$results[[nm]])) {
        ds$results[[nm]] <- ds$results[[nm]][
          !(as.character(ds$results[[nm]]$train) %in% trains),
          , drop = FALSE
        ]
      }
    }
    if (!is.null(ds$results$final_audit_policy$selected_trains)) {
      ds$results$final_audit_policy$selected_trains <- setdiff(
        as.character(ds$results$final_audit_policy$selected_trains),
        trains
      )
    }
  }
  ds
}

stpd_final_audit_summary <- function(ds) {
  if (is.null(ds) || is.null(ds$results)) return(stpd_empty_final_audit_summary())
  ds$results$final_audit_current_summary %||%
    ds$results$final_audit_summary %||%
    stpd_empty_final_audit_summary()
}

stpd_final_audit_events <- function(ds) {
  if (is.null(ds) || is.null(ds$results)) return(stpd_empty_final_audit_events())
  ds$results$final_audit_current_events %||%
    ds$results$final_audit_events %||%
    stpd_empty_final_audit_events()
}

stpd_final_audit_history <- function(ds) {
  if (is.null(ds) || is.null(ds$results)) return(stpd_empty_final_audit_summary())
  ds$results$final_audit_history %||% stpd_empty_final_audit_summary()
}

stpd_final_audit_event_history <- function(ds) {
  if (is.null(ds) || is.null(ds$results)) return(stpd_empty_final_audit_events())
  ds$results$final_audit_event_history %||% stpd_empty_final_audit_events()
}

stpd_ensure_possible_burst_promotion_columns <- function(dat) {
  n <- nrow(dat)
  dat$pattern_manual <- stpd_chr_vec(dat$pattern_manual, n)
  dat$pattern_auto <- stpd_chr_vec(dat$pattern_auto, n)
  dat$pattern_manual_negative <- stpd_chr_vec(dat$pattern_manual_negative, n)
  if (!("pattern_auto_original" %in% names(dat))) {
    dat$pattern_auto_original <- dat$pattern_auto
  } else {
    dat$pattern_auto_original <- stpd_chr_vec(dat$pattern_auto_original, n)
    fill <- dat$pattern_auto_original == "" & dat$pattern_auto != ""
    dat$pattern_auto_original[fill] <- dat$pattern_auto[fill]
  }
  for (nm in c(
    "pattern_user_override",
    "pattern_user_override_from",
    "pattern_user_override_to",
    "pattern_user_override_reason",
    "pattern_user_override_source",
    "pattern_user_override_time",
    "pattern_user_override_id",
    "pattern_manual_before_user_override",
    "pattern_manual_negative_before_user_override"
  )) {
    dat[[nm]] <- stpd_chr_vec(dat[[nm]], n)
  }
  dat
}

stpd_possible_burst_empty_events <- function() {
  data.frame(
    train = character(),
    start_isi = integer(),
    end_isi = integer(),
    start_spike_idx = integer(),
    end_spike_idx = integer(),
    n_isi = integer(),
    n_spikes = integer(),
    start_time_sec = numeric(),
    end_time_sec = numeric(),
    duration_sec = numeric(),
    auto_score = numeric(),
    stringsAsFactors = FALSE
  )
}

stpd_possible_burst_event_rows <- function(dat, train, idx) {
  idx <- sort(unique(suppressWarnings(as.integer(idx))))
  idx <- idx[is.finite(idx) & idx >= 2L & idx <= nrow(dat)]
  if (length(idx) == 0) return(stpd_possible_burst_empty_events())
  lab <- rep("", nrow(dat))
  lab[idx] <- "possible_burst"
  seg <- find_segments(lab, "possible_burst")
  if (nrow(seg) == 0) return(stpd_possible_burst_empty_events())
  score <- suppressWarnings(as.numeric(dat$auto_score %||% rep(NA_real_, nrow(dat))))
  rows <- lapply(seq_len(nrow(seg)), function(i) {
    s <- as.integer(seg$start_isi[i])
    e <- as.integer(seg$end_isi[i])
    sc <- score[s:e]
    data.frame(
      train = train,
      start_isi = s,
      end_isi = e,
      start_spike_idx = s - 1L,
      end_spike_idx = e,
      n_isi = e - s + 1L,
      n_spikes = e - s + 2L,
      start_time_sec = suppressWarnings(as.numeric(dat$timestamp_sec[s - 1L])),
      end_time_sec = suppressWarnings(as.numeric(dat$timestamp_sec[e])),
      duration_sec = suppressWarnings(as.numeric(dat$timestamp_sec[e] - dat$timestamp_sec[s - 1L])),
      auto_score = if (all(is.na(sc))) NA_real_ else max(sc, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

stpd_possible_burst_empty_labels <- function() {
  data.frame(
    train = character(),
    idx = integer(),
    timestamp_sec = numeric(),
    ISI_sec = numeric(),
    auto_label = character(),
    manual_label = character(),
    manual_negative_label = character(),
    stringsAsFactors = FALSE
  )
}

#' Preview bulk promotion of possible_burst labels
#'
#' Returns per-train counts and event intervals that would be promoted by
#' [stpd_promote_possible_burst()]. Existing manual labels and manual negative
#' labels are protected by default.
#'
#' @param ds SpikeTrainPatternDetector dataset.
#' @param selected_trains Optional character vector of train names. Defaults to
#'   all trains.
#' @param overwrite_manual If `TRUE`, existing manual labels and manual-negative
#'   labels inside selected possible_burst intervals may be overwritten.
#' @return A list with `summary`, `events`, `labels`, `total_eligible_isi`, and
#'   `total_eligible_events`.
#' @export
stpd_possible_burst_promotion_preview <- function(ds, selected_trains = NULL, overwrite_manual = FALSE) {
  if (is.null(ds) || is.null(ds$trains)) stop("Dataset has no trains.", call. = FALSE)
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(as.character(trains), names(ds$trains))
  if (length(trains) == 0) stop("No selected trains found.", call. = FALSE)

  summary_rows <- list()
  event_rows <- list()
  label_rows <- list()
  for (tr in trains) {
    dat <- stpd_ensure_possible_burst_promotion_columns(ds$trains[[tr]])
    n <- nrow(dat)
    auto <- stpd_chr_vec(dat$pattern_auto, n)
    man <- stpd_chr_vec(dat$pattern_manual, n)
    neg <- stpd_chr_vec(dat$pattern_manual_negative, n)
    possible_idx <- which(seq_len(n) >= 2L & auto == "possible_burst")
    blocked_manual <- possible_idx[man[possible_idx] != ""]
    blocked_negative <- possible_idx[neg[possible_idx] != ""]
    eligible <- possible_idx
    if (!isTRUE(overwrite_manual)) {
      eligible <- eligible[man[eligible] == "" & neg[eligible] == ""]
    }
    possible_events <- stpd_possible_burst_event_rows(dat, tr, possible_idx)
    eligible_events <- stpd_possible_burst_event_rows(dat, tr, eligible)
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      train = tr,
      n_possible_burst_isi = length(possible_idx),
      n_possible_burst_events = nrow(possible_events),
      n_eligible_isi = length(eligible),
      n_eligible_events = nrow(eligible_events),
      n_blocked_by_manual_isi = length(blocked_manual),
      n_blocked_by_manual_negative_isi = length(blocked_negative),
      overwrite_manual = isTRUE(overwrite_manual),
      stringsAsFactors = FALSE
    )
    if (nrow(eligible_events) > 0) event_rows[[length(event_rows) + 1L]] <- eligible_events
    if (length(eligible) > 0) {
      label_rows[[length(label_rows) + 1L]] <- data.frame(
        train = tr,
        idx = eligible,
        timestamp_sec = suppressWarnings(as.numeric(dat$timestamp_sec[eligible])),
        ISI_sec = suppressWarnings(as.numeric(dat$ISI_sec[eligible])),
        auto_label = auto[eligible],
        manual_label = man[eligible],
        manual_negative_label = neg[eligible],
        stringsAsFactors = FALSE
      )
    }
  }
  summary <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
  events <- if (length(event_rows) > 0) do.call(rbind, event_rows) else stpd_possible_burst_empty_events()
  labels <- if (length(label_rows) > 0) do.call(rbind, label_rows) else stpd_possible_burst_empty_labels()
  out <- list(
    summary = summary,
    events = events,
    labels = labels,
    selected_trains = trains,
    overwrite_manual = isTRUE(overwrite_manual),
    total_eligible_isi = sum(summary$n_eligible_isi %||% 0L),
    total_eligible_events = nrow(events)
  )
  class(out) <- c("stpd_possible_burst_promotion_preview", "list")
  out
}

#' Promote possible_burst labels to burst in the user-review layer
#'
#' This function does not rewrite the detector's automatic label. It writes a
#' user-review override (`pattern_manual = "burst"`) so final labels become
#' `burst`, while retaining `pattern_auto_original`, `pattern_user_override_*`,
#' and an event-level audit table under
#' `ds$results$possible_burst_promotion_audit`.
#'
#' @param ds SpikeTrainPatternDetector dataset.
#' @param selected_trains Optional character vector of train names.
#' @param overwrite_manual If `TRUE`, existing manual labels may be overwritten.
#' @param reason Audit reason stored on modified ISIs.
#' @param user Optional user/operator name for the audit table.
#' @param audit_id Optional stable operation id. Generated when omitted.
#' @return A list with `dataset`, `preview`, `summary`, and `audit`.
#' @export
stpd_promote_possible_burst <- function(ds,
                                        selected_trains = NULL,
                                        overwrite_manual = FALSE,
                                        reason = "user_promoted_possible_burst",
                                        user = NA_character_,
                                        audit_id = NULL) {
  preview <- stpd_possible_burst_promotion_preview(ds, selected_trains = selected_trains, overwrite_manual = overwrite_manual)
  if (is.null(ds$results)) ds$results <- list()
  time_chr <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  if (is.null(audit_id) || !nzchar(as.character(audit_id)[1])) {
    audit_id <- paste0("pb_promote_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  audit_id <- as.character(audit_id)[1]
  reason <- as.character(reason %||% "user_promoted_possible_burst")[1]
  user <- as.character(user %||% NA_character_)[1]

  labels <- preview$labels
  if (nrow(labels) > 0) {
    by_train <- split(labels, labels$train)
    for (tr in names(by_train)) {
      dat <- stpd_ensure_possible_burst_promotion_columns(ds$trains[[tr]])
      idx <- sort(unique(suppressWarnings(as.integer(by_train[[tr]]$idx))))
      idx <- idx[is.finite(idx) & idx >= 2L & idx <= nrow(dat)]
      if (length(idx) == 0) next
      dat$pattern_auto_original[idx] <- dat$pattern_auto[idx]
      dat$pattern_manual_before_user_override[idx] <- dat$pattern_manual[idx]
      dat$pattern_manual_negative_before_user_override[idx] <- dat$pattern_manual_negative[idx]
      dat$pattern_user_override[idx] <- "burst"
      dat$pattern_user_override_from[idx] <- dat$pattern_auto[idx]
      dat$pattern_user_override_to[idx] <- "burst"
      dat$pattern_user_override_reason[idx] <- reason
      dat$pattern_user_override_source[idx] <- "bulk_possible_burst_promotion"
      dat$pattern_user_override_time[idx] <- time_chr
      dat$pattern_user_override_id[idx] <- audit_id
      dat$pattern_manual[idx] <- "burst"
      dat$pattern_manual_negative[idx] <- ""
      ds$trains[[tr]] <- dat
    }
  }

  audit <- preview$events
  if (nrow(audit) > 0) {
    audit$audit_id <- audit_id
    audit$action <- "promote_possible_burst_to_burst"
    audit$from_label <- "possible_burst"
    audit$to_label <- "burst"
    audit$reason <- reason
    audit$user <- user
    audit$time <- time_chr
    audit$overwrite_manual <- isTRUE(overwrite_manual)
  }
  summary <- preview$summary
  if (nrow(summary) > 0) {
    summary$audit_id <- audit_id
    summary$action <- "promote_possible_burst_to_burst"
    summary$reason <- reason
    summary$user <- user
    summary$time <- time_chr
  }
  old_audit <- ds$results$possible_burst_promotion_audit %||% data.frame()
  old_summary <- ds$results$possible_burst_promotion_summary %||% data.frame()
  ds$results$possible_burst_promotion_audit <- if (nrow(old_audit) > 0 && nrow(audit) > 0) {
    dplyr::bind_rows(old_audit, audit)
  } else if (nrow(audit) > 0) audit else old_audit
  ds$results$possible_burst_promotion_summary <- if (nrow(old_summary) > 0 && nrow(summary) > 0) {
    dplyr::bind_rows(old_summary, summary)
  } else if (nrow(summary) > 0) summary else old_summary
  list(dataset = ds, preview = preview, summary = summary, audit = audit)
}

#' Revert user-review possible_burst promotions
#'
#' Restores the manual label state that was present before
#' [stpd_promote_possible_burst()] for matching selected trains. By default it
#' protects later manual edits: only rows still carrying the promoted `burst`
#' manual label are reverted.
#'
#' @param ds SpikeTrainPatternDetector dataset.
#' @param selected_trains Optional character vector of train names.
#' @param protect_manual_edits If `TRUE`, do not revert rows whose manual label
#'   has been changed after promotion.
#' @param reason Optional audit reason filter.
#' @return A list with `dataset`, `summary`, and `audit`.
#' @export
stpd_revert_possible_burst_promotions <- function(ds,
                                                  selected_trains = NULL,
                                                  protect_manual_edits = TRUE,
                                                  reason = NULL) {
  if (is.null(ds) || is.null(ds$trains)) stop("Dataset has no trains.", call. = FALSE)
  if (is.null(ds$results)) ds$results <- list()
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(as.character(trains), names(ds$trains))
  if (length(trains) == 0) stop("No selected trains found.", call. = FALSE)
  time_chr <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  audit_id <- paste0("pb_revert_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  summary_rows <- list()
  audit_rows <- list()
  for (tr in trains) {
    dat <- stpd_ensure_possible_burst_promotion_columns(ds$trains[[tr]])
    n <- nrow(dat)
    idx <- which(
      seq_len(n) >= 2L &
        dat$pattern_user_override == "burst" &
        dat$pattern_user_override_from == "possible_burst" &
        dat$pattern_user_override_source == "bulk_possible_burst_promotion"
    )
    if (!is.null(reason)) idx <- idx[dat$pattern_user_override_reason[idx] %in% as.character(reason)]
    if (isTRUE(protect_manual_edits)) idx <- idx[dat$pattern_manual[idx] == dat$pattern_user_override_to[idx]]
    ev <- stpd_possible_burst_event_rows(dat, tr, idx)
    if (length(idx) > 0) {
      dat$pattern_manual[idx] <- dat$pattern_manual_before_user_override[idx]
      dat$pattern_manual_negative[idx] <- dat$pattern_manual_negative_before_user_override[idx]
      for (nm in c(
        "pattern_user_override",
        "pattern_user_override_from",
        "pattern_user_override_to",
        "pattern_user_override_reason",
        "pattern_user_override_source",
        "pattern_user_override_time",
        "pattern_user_override_id",
        "pattern_manual_before_user_override",
        "pattern_manual_negative_before_user_override"
      )) {
        dat[[nm]][idx] <- ""
      }
      ds$trains[[tr]] <- dat
    }
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      train = tr,
      n_reverted_isi = length(idx),
      n_reverted_events = nrow(ev),
      action = "revert_possible_burst_promotion",
      audit_id = audit_id,
      time = time_chr,
      protect_manual_edits = isTRUE(protect_manual_edits),
      stringsAsFactors = FALSE
    )
    if (nrow(ev) > 0) {
      ev$audit_id <- audit_id
      ev$action <- "revert_possible_burst_promotion"
      ev$from_label <- "burst"
      ev$to_label <- "possible_burst"
      ev$reason <- "revert_user_promoted_possible_burst"
      ev$time <- time_chr
      audit_rows[[length(audit_rows) + 1L]] <- ev
    }
  }
  summary <- do.call(rbind, summary_rows)
  audit <- if (length(audit_rows) > 0) do.call(rbind, audit_rows) else stpd_possible_burst_empty_events()
  old_audit <- ds$results$possible_burst_promotion_audit %||% data.frame()
  old_summary <- ds$results$possible_burst_promotion_summary %||% data.frame()
  ds$results$possible_burst_promotion_audit <- if (nrow(old_audit) > 0 && nrow(audit) > 0) dplyr::bind_rows(old_audit, audit) else if (nrow(audit) > 0) audit else old_audit
  ds$results$possible_burst_promotion_summary <- if (nrow(old_summary) > 0 && nrow(summary) > 0) dplyr::bind_rows(old_summary, summary) else summary
  list(dataset = ds, summary = summary, audit = audit)
}

stpd_possible_burst_promotion_audit <- function(ds) {
  if (is.null(ds) || is.null(ds$results)) return(data.frame())
  ds$results$possible_burst_promotion_audit %||% data.frame()
}
