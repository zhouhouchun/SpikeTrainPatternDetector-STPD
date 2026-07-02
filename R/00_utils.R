# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# Utility functions
# ============================================================

to_sec <- function(x, unit_in = c("s", "ms")) {
  unit_in <- match.arg(unit_in)
  x <- suppressWarnings(as.numeric(x))
  if (unit_in == "ms") x / 1000 else x
}

from_sec <- function(x, unit_out = c("s", "ms")) {
  unit_out <- match.arg(unit_out)
  x <- suppressWarnings(as.numeric(x))
  if (unit_out == "ms") x * 1000 else x
}

finite_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

safe_q <- function(x, probs, default = NA_real_) {
  x <- finite_num(x)
  if (length(x) == 0) return(rep(default, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
}

safe_median <- function(x, default = NA_real_) {
  x <- finite_num(x)
  if (length(x) == 0) return(default)
  stats::median(x, na.rm = TRUE)
}

clamp <- function(x, lo, hi) {
  x <- suppressWarnings(as.numeric(x))
  pmin(pmax(x, lo), hi)
}

safe_int <- function(x, default = 0L) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- suppressWarnings(as.integer(round(as.numeric(x)[1])))
  if (!is.finite(x)) return(default)
  x
}

stpd_valid_xrange_window <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  length(x) == 2L && all(is.finite(x)) && x[2] > x[1]
}

stpd_html_escape <- function(x) {
  out <- as.character(x)
  out[is.na(out)] <- ""
  out <- gsub("&", "&amp;", out, fixed = TRUE)
  out <- gsub("<", "&lt;", out, fixed = TRUE)
  out <- gsub(">", "&gt;", out, fixed = TRUE)
  out <- gsub("\"", "&quot;", out, fixed = TRUE)
  out <- gsub("'", "&#39;", out, fixed = TRUE)
  out
}

calc_LV <- function(Ti) {
  Ti <- finite_num(Ti)
  if (length(Ti) < 2) return(NA_real_)
  a <- head(Ti, -1)
  b <- tail(Ti, -1)
  denom <- a + b
  ok <- is.finite(denom) & denom > 0
  if (!any(ok)) return(NA_real_)
  mean(3 * (a[ok] - b[ok])^2 / denom[ok]^2, na.rm = TRUE)
}

calc_CV <- function(Ti) {
  Ti <- finite_num(Ti)
  if (length(Ti) < 2) return(NA_real_)
  m <- mean(Ti)
  if (!is.finite(m) || m == 0) return(NA_real_)
  stats::sd(Ti) / m
}

is_artifact_isi <- function(isi_sec, min_isi_sec = 0.001) {
  # Treat values infinitesimally below the threshold as non-artifact. This
  # avoids false QC/detector artifact calls from floating-point conversion
  # of values that are effectively equal to the user threshold, e.g. 1 ms.
  min_isi_sec <- suppressWarnings(as.numeric(min_isi_sec))
  tol <- max(1e-12, abs(min_isi_sec) * 1e-6)
  !is.na(isi_sec) & is.finite(isi_sec) & (isi_sec < (min_isi_sec - tol))
}

is_refractory_suspect_isi <- function(isi_sec, min_isi_sec = 0.001, refractory_suspect_sec = 0.0010) {
  # Softer single-unit QC threshold. These ISIs are not hard artifacts, but
  # they can indicate refractory-period violations, duplicate spike detection,
  # timestamp problems, or multi-unit contamination. They can optionally demote
  # or exclude burst candidates while remaining distinct from hard artifacts.
  min_isi_sec <- suppressWarnings(as.numeric(min_isi_sec))
  refractory_suspect_sec <- suppressWarnings(as.numeric(refractory_suspect_sec))
  if (!is.finite(refractory_suspect_sec) || refractory_suspect_sec <= min_isi_sec) {
    return(rep(FALSE, length(isi_sec)))
  }
  tol_min <- max(1e-12, abs(min_isi_sec) * 1e-6)
  tol_ref <- max(1e-12, abs(refractory_suspect_sec) * 1e-6)
  !is.na(isi_sec) & is.finite(isi_sec) &
    (isi_sec >= (min_isi_sec - tol_min)) &
    (isi_sec < (refractory_suspect_sec - tol_ref))
}

candidate_refractory_summary <- function(cand, dat, p, min_isi_sec = 0.001) {
  if (is.null(cand) || nrow(cand) == 0) return(cand)
  ref_thr <- suppressWarnings(as.numeric(p$refractory_suspect_sec %||% NA_real_))
  if (!is.finite(ref_thr)) ref_thr <- suppressWarnings(as.numeric(p$refractory_suspect_threshold_sec %||% NA_real_))
  if (!is.finite(ref_thr)) ref_thr <- 0.0015
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  n <- length(isi)
  counts <- integer(nrow(cand)); fracs <- rep(NA_real_, nrow(cand)); mins <- rep(NA_real_, nrow(cand))
  for (ii in seq_len(nrow(cand))) {
    s0 <- suppressWarnings(as.integer(cand$start_isi[ii])); e0 <- suppressWarnings(as.integer(cand$end_isi[ii]))
    idx <- seq(max(2L, s0), min(n, e0))
    if (length(idx) == 0) next
    vals <- isi[idx]
    r <- is_refractory_suspect_isi(vals, min_isi_sec = min_isi_sec, refractory_suspect_sec = ref_thr)
    counts[ii] <- sum(r, na.rm = TRUE)
    fracs[ii] <- counts[ii] / max(1L, length(idx))
    if (counts[ii] > 0) mins[ii] <- min(vals[r], na.rm = TRUE)
  }
  cand$refractory_suspect_n <- counts
  cand$refractory_suspect_fraction <- fracs
  cand$refractory_suspect_min_ISI_sec <- mins
  cand
}

apply_refractory_suspect_policy_burst_candidates <- function(cand, dat, p, min_isi_sec = 0.001) {
  if (is.null(cand) || nrow(cand) == 0) return(cand)
  cand <- candidate_refractory_summary(cand, dat, p, min_isi_sec = min_isi_sec)
  action <- as.character(p$refractory_suspect_action %||% "warn_only")
  action <- tolower(trimws(action))
  action <- gsub("-", "_", action)
  if (action %in% c("demote", "demote_burst", "demote_to_possible_burst", "review")) action <- "demote_to_possible"
  if (action %in% c("exclude", "reject", "drop", "reject_candidate")) action <- "exclude_candidate"
  if (action %in% c("split", "split_candidate", "split_at_refractory")) action <- "split_at_suspect"
  if (action %in% c("exclude_suspect", "exclude_suspect_isi", "reevaluate_fragments")) action <- "exclude_suspect_isi_and_reevaluate"
  if (action %in% c("multiunit", "mark_multiunit")) action <- "mark_multiunit_contamination"
  valid_actions <- c("warn_only", "demote_to_possible", "split_at_suspect", "exclude_suspect_isi_and_reevaluate", "exclude_candidate", "mark_multiunit_contamination")
  if (!action %in% valid_actions) action <- "warn_only"
  has_ref <- is.finite(cand$refractory_suspect_n) & cand$refractory_suspect_n > 0
  if (!any(has_ref)) return(cand)
  if (!("refractory_suspect_action" %in% names(cand))) cand$refractory_suspect_action <- ""
  if (!("refractory_suspect_warning" %in% names(cand))) cand$refractory_suspect_warning <- FALSE
  if (!("uncertainty_reason" %in% names(cand))) cand$uncertainty_reason <- ""
  if (!("reject_reason" %in% names(cand))) cand$reject_reason <- ""
  cand$refractory_suspect_warning[has_ref] <- TRUE
  cand$uncertainty_reason[has_ref] <- trimws(paste(cand$uncertainty_reason[has_ref], "contains_refractory_suspect_ISI"))

  if (action == "warn_only") {
    cand$refractory_suspect_action[has_ref] <- "warn_only"
    return(cand)
  }
  if (action == "demote_to_possible") {
    demote <- has_ref & cand$class %in% c("burst", "long_burst")
    cand$class[demote] <- "possible_burst"
    cand$refractory_suspect_action[has_ref] <- ifelse(demote[has_ref], "demoted_to_possible_burst", "already_possible_or_nonburst")
    cand$reject_reason[has_ref] <- trimws(paste(cand$reject_reason[has_ref], "refractory_suspect_review"))
    return(cand)
  }
  if (action == "mark_multiunit_contamination") {
    demote <- has_ref & cand$class %in% c("burst", "long_burst")
    cand$class[demote] <- "possible_burst"
    cand$refractory_suspect_action[has_ref] <- "marked_possible_multiunit_contamination"
    cand$uncertainty_reason[has_ref] <- trimws(paste(cand$uncertainty_reason[has_ref], "possible_multiunit_contamination"))
    cand$reject_reason[has_ref] <- trimws(paste(cand$reject_reason[has_ref], "possible_multiunit_contamination"))
    return(cand)
  }
  if (action == "exclude_candidate") {
    cand$refractory_suspect_action[has_ref] <- "excluded_entire_candidate"
    return(cand[!has_ref, , drop = FALSE])
  }

  # Conservative fragment strategy: split at refractory-suspect ISIs and keep only
  # contiguous non-suspect fragments with enough spikes. These fragments are
  # written as possible_burst by default because one suspicious ISI disrupted the
  # original event structure; a user can later promote them after review.
  if (action %in% c("split_at_suspect", "exclude_suspect_isi_and_reevaluate")) {
    ref_thr <- suppressWarnings(as.numeric(p$refractory_suspect_sec %||% p$refractory_suspect_threshold_sec %||% NA_real_))
    if (!is.finite(ref_thr)) ref_thr <- min_isi_sec
    rows <- list()
    g_min <- safe_int(p$G_min %||% 3L, 3L)
    d_max <- suppressWarnings(as.numeric(p$D_max %||% p$final_max_duration %||% 0))
    for (ii in seq_len(nrow(cand))) {
      row <- cand[ii, , drop = FALSE]
      if (!isTRUE(has_ref[ii])) {
        rows[[length(rows) + 1L]] <- row
        next
      }
      s0 <- suppressWarnings(as.integer(row$start_isi[1])); e0 <- suppressWarnings(as.integer(row$end_isi[1]))
      if (!is.finite(s0) || !is.finite(e0) || s0 > e0 || e0 > nrow(dat)) next
      idx <- s0:e0
      suspect <- is_refractory_suspect_isi(dat$ISI_sec[idx], min_isi_sec = min_isi_sec, refractory_suspect_sec = ref_thr)
      keep_idx <- idx[!suspect]
      if (length(keep_idx) == 0) next
      grp <- split(keep_idx, cumsum(c(TRUE, diff(keep_idx) != 1L)))
      for (gg in grp) {
        if (length(gg) + 1L < g_min) next
        ns <- min(gg); ne <- max(gg)
        dur <- if (ne <= nrow(dat) && ns > 1) dat$timestamp_sec[ne] - dat$timestamp_sec[ns - 1L] else NA_real_
        if (is.finite(d_max) && d_max > 0 && is.finite(dur) && dur > d_max) next
        frag <- row
        frag$start_isi <- as.integer(ns); frag$end_isi <- as.integer(ne)
        if ("class" %in% names(frag)) frag$class <- "possible_burst"
        if ("score" %in% names(frag)) frag$score <- suppressWarnings(as.numeric(frag$score)) * 0.95
        frag$refractory_suspect_action <- action
        frag$refractory_suspect_warning <- TRUE
        frag$uncertainty_reason <- trimws(paste(as.character(frag$uncertainty_reason %||% ""), "refractory_suspect_split_fragment"))
        frag$reject_reason <- trimws(paste(as.character(frag$reject_reason %||% ""), "refractory_suspect_split_fragment"))
        rows[[length(rows) + 1L]] <- frag
      }
    }
    if (length(rows) == 0) return(cand[0, , drop = FALSE])
    out <- bind_rows(rows) %>% arrange(start_isi, end_isi)
    return(out)
  }
  cand
}

bridge_artifacts_in_pattern <- function(pat, isi_sec, min_isi_sec = 0.001) {
  pat <- as.character(pat)
  pat[is.na(pat)] <- ""
  art <- is_artifact_isi(isi_sec, min_isi_sec)
  n <- length(pat)
  if (n <= 2) return(pat)
  
  for (i in which(art)) {
    if (i <= 1 || i >= n) next
    left <- pat[i - 1]
    right <- pat[i + 1]
    if (left == right && left %in% c("burst", "long_burst", "possible_burst", "tonic")) {
      pat[i] <- left
    }
  }
  pat
}


compute_final_pattern_base <- function(manual, auto, isi_sec, min_isi_sec = 0.001) {
  manual <- as.character(manual); manual[is.na(manual)] <- ""
  auto <- as.character(auto); auto[is.na(auto)] <- ""
  out <- ifelse(manual != "", manual, auto)
  bridge_artifacts_in_pattern(out, isi_sec, min_isi_sec = min_isi_sec)
}

fill_unlabeled_others_for_display <- function(pattern, isi_sec, min_isi_sec = 0.001) {
  out <- as.character(pattern); out[is.na(out)] <- ""
  art <- is_artifact_isi(isi_sec, min_isi_sec)
  fill <- (out == "") & !art & is.finite(isi_sec)
  if (length(fill) > 0) fill[1] <- FALSE
  out[fill] <- "others"
  out
}

# Backward-compatible wrapper. Use `auto_others = TRUE` only for final DISPLAY
# or EXPORT. Detector occupancy, cached events, and ML should pass FALSE so that
# unlabeled intervals do not become a hidden negative class prematurely.
compute_final_pattern <- function(manual, auto, isi_sec, auto_others = FALSE, min_isi_sec = 0.001) {
  out <- compute_final_pattern_base(manual, auto, isi_sec, min_isi_sec = min_isi_sec)
  if (isTRUE(auto_others)) out <- fill_unlabeled_others_for_display(out, isi_sec, min_isi_sec = min_isi_sec)
  out
}

find_segments <- function(pat_vec, target) {
  idx <- which(pat_vec == target)
  if (length(idx) == 0) {
    return(data.frame(start_isi = integer(0), end_isi = integer(0)))
  }
  cuts <- c(1, which(diff(idx) != 1) + 1)
  starts <- idx[cuts]
  ends <- idx[c(cuts[-1] - 1, length(idx))]
  data.frame(start_isi = starts, end_isi = ends)
}

label_segments <- function(pat_vec, labels = NULL) {
  x <- as.character(pat_vec)
  x[is.na(x)] <- ""
  if (!is.null(labels)) x[!(x %in% labels)] <- ""
  idx <- which(x != "")
  if (length(idx) == 0) {
    return(data.frame(start_isi = integer(0), end_isi = integer(0), class = character(0), stringsAsFactors = FALSE))
  }
  cuts <- c(1, which(diff(idx) != 1 | x[idx][-1] != x[idx][-length(idx)]) + 1)
  starts <- idx[cuts]
  ends <- idx[c(cuts[-1] - 1, length(idx))]
  cls <- x[starts]
  data.frame(start_isi = starts, end_isi = ends, class = cls, stringsAsFactors = FALSE)
}


# ============================================================
# Train-name metadata parsing
# ============================================================

stpd_guess_structure_from_text <- function(x, default = NA_character_) {
  x <- paste(x %||% "", collapse = " ")
  xu <- toupper(as.character(x))
  has_token <- function(tok) grepl(paste0("(^|[^A-Z0-9])", tok, "([^A-Z0-9]|$)"), xu, perl = TRUE)
  if (has_token("STN")) return("STN")
  if (has_token("GPE")) return("GPe")
  if (has_token("GPI")) return("GPi")
  if (has_token("GP")) return("GP")
  if (is.null(default) || length(default) == 0 || is.na(default[1]) || !nzchar(as.character(default[1]))) return(NA_character_)
  as.character(default[1])
}

parse_spike_train_column_metadata <- function(train_names, dataset_name = "", default_structure = NULL) {
  train_names <- as.character(train_names %||% character(0))
  if (length(train_names) == 0) {
    return(data.frame(
      train = character(0), raw_train_name = character(0), train_base = character(0), parse_ok = logical(0),
      structure = character(0), side = character(0), hemisphere = character(0), trajectory = character(0),
      recording_depth = numeric(0), recording_depth_label = character(0), channel_type = character(0),
      wire = integer(0), unit_id = character(0), flag = integer(0), duplicate_name_suffix = integer(0),
      recording_group = character(0), depth_group = character(0), stringsAsFactors = FALSE
    ))
  }
  ds_structure <- stpd_guess_structure_from_text(dataset_name, default = default_structure %||% NA_character_)
  rows <- lapply(train_names, function(tr) {
    raw <- as.character(tr)
    x <- trimws(raw)

    duplicate_suffix <- NA_integer_
    m_dup <- regexec("\\.([0-9]+)$", x, perl = TRUE)
    r_dup <- regmatches(x, m_dup)[[1]]
    if (length(r_dup) == 2L) {
      duplicate_suffix <- suppressWarnings(as.integer(r_dup[2]))
      x <- substr(x, 1L, nchar(x) - nchar(r_dup[1]))
      x <- trimws(x)
    }

    flag <- NA_integer_
    m_flag <- regexec("\\s*\\(flag\\s+([0-9]+)\\)\\s*$", x, ignore.case = TRUE, perl = TRUE)
    r_flag <- regmatches(x, m_flag)[[1]]
    if (length(r_flag) == 2L) {
      flag <- suppressWarnings(as.integer(r_flag[2]))
      x <- substr(x, 1L, nchar(x) - nchar(r_flag[1]))
      x <- trimws(x)
    }

    structure <- stpd_guess_structure_from_text(x, default = ds_structure)
    core <- x
    core <- sub("^(STN|GPE|GPI|GP)[_-]+", "", core, ignore.case = TRUE, perl = TRUE)

    side <- NA_character_; hemi <- NA_character_; trajectory <- NA_character_; depth <- NA_real_
    depth_label <- NA_character_; channel_type <- NA_character_; wire <- NA_integer_; unit_id <- NA_character_
    parse_ok <- FALSE

    m <- regexec("^([LR])(T[0-9]+)D([+-]?[0-9]+(?:[_.][0-9]+)?)_(s?nw)([+-]?[0-9]+)(?:_([A-Za-z0-9]+))?$", core, ignore.case = TRUE, perl = TRUE)
    rr <- regmatches(core, m)[[1]]
    if (length(rr) >= 6L) {
      parse_ok <- TRUE
      side <- toupper(rr[2])
      hemi <- if (identical(side, "L")) "left" else if (identical(side, "R")) "right" else NA_character_
      trajectory <- toupper(rr[3])
      depth_label <- rr[4]
      depth <- suppressWarnings(as.numeric(gsub("_", ".", depth_label, fixed = TRUE)))
      channel_type <- tolower(rr[5])
      wire <- suppressWarnings(as.integer(rr[6]))
      if (length(rr) >= 7L && !is.na(rr[7]) && nzchar(rr[7])) unit_id <- rr[7]
    } else {
      m_fon <- regexec(
        "^([LR])(T[0-9]+)D([+-]?[0-9]+(?:[_.][0-9]+)?)[_-]([A-Za-z]+[0-9]*)(?:_([0-9]+))?(?:_(.*))?$",
        core, ignore.case = TRUE, perl = TRUE
      )
      rf <- regmatches(core, m_fon)[[1]]
      if (length(rf) >= 5L) {
        parse_ok <- TRUE
        side <- toupper(rf[2])
        hemi <- if (identical(side, "L")) "left" else if (identical(side, "R")) "right" else NA_character_
        trajectory <- toupper(rf[3])
        depth_label <- rf[4]
        depth <- suppressWarnings(as.numeric(gsub("_", ".", depth_label, fixed = TRUE)))
        channel_token <- tolower(rf[5])
        mc <- regexec("^([a-z]+)([0-9]*)$", channel_token, perl = TRUE)
        rc <- regmatches(channel_token, mc)[[1]]
        if (length(rc) >= 2L) {
          channel_type <- rc[2]
          if (length(rc) >= 3L && nzchar(rc[3])) wire <- suppressWarnings(as.integer(rc[3]))
        } else {
          channel_type <- channel_token
        }
        if (!is.finite(wire) && length(rf) >= 6L && !is.na(rf[6]) && nzchar(rf[6])) {
          wire <- suppressWarnings(as.integer(rf[6]))
        }
        tail <- character(0)
        if (length(rf) >= 6L && !is.na(rf[6]) && nzchar(rf[6])) tail <- c(tail, rf[6])
        if (length(rf) >= 7L && !is.na(rf[7]) && nzchar(rf[7])) tail <- c(tail, rf[7])
        if (length(tail) > 0L) unit_id <- paste(tail, collapse = "_")
      } else {
        # Fallbacks for less strict names: side and structure are often still useful.
        if (grepl("^L", core, ignore.case = TRUE)) {
          side <- "L"; hemi <- "left"
        } else if (grepl("^R", core, ignore.case = TRUE)) {
          side <- "R"; hemi <- "right"
        } else if (grepl("(^|[_-])(LEFT|LT)([_-]|$)", core, ignore.case = TRUE, perl = TRUE)) {
          side <- "L"; hemi <- "left"
        } else if (grepl("(^|[_-])(RIGHT|RT)([_-]|$)", core, ignore.case = TRUE, perl = TRUE)) {
          side <- "R"; hemi <- "right"
        }
        mt <- regexec("(T[0-9]+)", core, ignore.case = TRUE, perl = TRUE)
        rt <- regmatches(core, mt)[[1]]
        if (length(rt) >= 2L) trajectory <- toupper(rt[2])
        md <- regexec("D([+-]?[0-9]+(?:[_.][0-9]+)?)", core, ignore.case = TRUE, perl = TRUE)
        rd <- regmatches(core, md)[[1]]
        if (length(rd) >= 2L) {
          depth_label <- rd[2]
          depth <- suppressWarnings(as.numeric(gsub("_", ".", depth_label, fixed = TRUE)))
        }
      }
    }

    if (is.na(structure) || !nzchar(as.character(structure))) structure <- "unknown"
    group_bits <- c(structure, side, trajectory)
    group_bits <- group_bits[!is.na(group_bits) & nzchar(group_bits)]
    recording_group <- if (length(group_bits) > 0) paste(group_bits, collapse = "_") else "unknown"
    depth_group <- if (is.finite(depth)) paste0(recording_group, "_D", formatC(depth, format = "f", digits = 3)) else recording_group

    data.frame(
      train = raw,
      raw_train_name = raw,
      train_base = core,
      parse_ok = parse_ok,
      structure = as.character(structure),
      side = side,
      hemisphere = hemi,
      trajectory = trajectory,
      recording_depth = depth,
      recording_depth_label = depth_label,
      channel_type = channel_type,
      wire = wire,
      unit_id = unit_id,
      flag = flag,
      duplicate_name_suffix = duplicate_suffix,
      recording_group = recording_group,
      depth_group = depth_group,
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  out
}

attach_train_metadata <- function(trains, metadata) {
  if (is.null(trains) || length(trains) == 0 || is.null(metadata) || nrow(metadata) == 0) return(trains)
  md <- metadata
  for (tr in names(trains)) {
    dat <- trains[[tr]]
    if (is.null(dat) || nrow(dat) == 0) next
    row <- md[as.character(md$train) == as.character(tr), , drop = FALSE]
    if (nrow(row) == 0) next
    dat$train_name <- as.character(tr)
    dat$meta_structure <- as.character(row$structure[1])
    dat$meta_side <- as.character(row$side[1])
    dat$meta_hemisphere <- as.character(row$hemisphere[1])
    dat$meta_trajectory <- as.character(row$trajectory[1])
    dat$meta_recording_depth <- suppressWarnings(as.numeric(row$recording_depth[1]))
    dat$meta_channel_type <- as.character(row$channel_type[1])
    dat$meta_wire <- suppressWarnings(as.integer(row$wire[1]))
    dat$meta_unit_id <- as.character(row$unit_id[1])
    dat$meta_flag <- suppressWarnings(as.integer(row$flag[1]))
    dat$meta_recording_group <- as.character(row$recording_group[1])
    dat$meta_depth_group <- as.character(row$depth_group[1])
    trains[[tr]] <- dat
  }
  trains
}

make_dataset <- function(name, source, trains, unit_in = "s", task_events = NULL) {
  train_metadata <- tryCatch(
    parse_spike_train_column_metadata(names(trains), dataset_name = name),
    error = function(e) data.frame(train = names(trains), stringsAsFactors = FALSE)
  )
  trains <- tryCatch(attach_train_metadata(trains, train_metadata), error = function(e) trains)
  list(
    meta = list(
      display_name = name,
      source = source,
      unit_in = unit_in,
      created_at = Sys.time(),
      train_metadata = train_metadata
    ),
    trains = trains,
    task_events = stpd_normalize_task_events(task_events, source = name),
    params_est = NULL,
    params_last = NULL,
    train_settings = list(
      burst_isi_ranges = list(),
      tonic_isi_ranges = list(),
      pause_isi_ranges = list(),
      highfreq_isi_ranges = list(),
      isi_thresholds = list()
    ),
    ml = list(
      last_feature_table = data.frame(),
      last_prediction_table = data.frame(),
      last_eval_table = data.frame(),
      last_eval_metrics = data.frame()
    ),
    results = list(events = data.frame(), structure_candidates = data.frame(), seed_candidates = data.frame(), bridge_candidates = data.frame(), burst_candidates = data.frame(), near_miss_candidates = data.frame())
  )
}
