#!/usr/bin/env Rscript

# Reproduce two prespecified real-data tonic-like review spans under the current
# calibration sampler and threshold-freezing code.  This script is diagnostic:
# it does not change detector parameters or canonical decisions.

options(stringsAsFactors = FALSE, warn = 1)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Unable to resolve the diagnostic script path.", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(
  file.path(dirname(script_path), "..", "..", "..", ".."), mustWork = TRUE
)
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L && nzchar(args[[1L]])) {
  normalizePath(args[[1L]], mustWork = FALSE)
} else {
  file.path(
    repo, "test-results", "publication_validation", "diagnostics",
    "real_tonic_miss_trace"
  )
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pkgload::load_all(repo, quiet = TRUE)
source(
  file.path(
    repo, "evaluation", "publication_validation",
    "publication_validation_helpers.R"
  ),
  local = FALSE
)

xlsx_contract <- stpd_pub_resolve_authoritative_workbook(
  repo = repo,
  configured_path = Sys.getenv("STPD_MANUAL_REFERENCE_XLSX", unset = "")
)
xlsx <- xlsx_contract$path
raw_path <- Sys.getenv("STPD_REAL_SPIKE_CSV", unset = "")
if (!nzchar(raw_path) || !file.exists(raw_path)) {
  stop("Set STPD_REAL_SPIKE_CSV to the raw timestamp CSV.", call. = FALSE)
}
raw_path <- normalizePath(path.expand(raw_path), mustWork = TRUE)

book <- do.call(rbind, lapply(readxl::excel_sheets(xlsx), function(sheet) {
  z <- as.data.frame(readxl::read_excel(xlsx, sheet = sheet),
                     stringsAsFactors = FALSE)
  z$sheet <- sheet
  z
}))
book$train_id <- as.character(book$train_id)
book$Group_ID <- sub("_[Ff]on.*$", "", book$train_id)
book$Right_Spike_Index <- as.integer(book$isi_index) + 1L
book$ISI_s <- as.numeric(book$isi_us) / 1e6

# Match the reference-eligible real validation protocol exactly: eligibility is
# axis-specific, while a train enters the calibration pool if either axis has at
# least ten reviewed positive ISIs.
reference_profile <- do.call(rbind, lapply(split(book, book$train_id), function(z) {
  event_labeled <- !is.na(z$event_pattern) &
    nzchar(trimws(as.character(z$event_pattern)))
  raw_state_labeled <- !is.na(z$state_pattern) &
    nzchar(trimws(as.character(z$state_pattern)))
  state_labeled <- raw_state_labeled &
    stpd_pub_normalize_pattern(z$state_pattern) != "tonic"
  data.frame(
    train_id = z$train_id[[1L]],
    event_reference_eligible = sum(event_labeled) >= 10L,
    state_reference_eligible = sum(state_labeled) >= 10L,
    reference_eligible = sum(event_labeled) >= 10L ||
      sum(state_labeled) >= 10L,
    stringsAsFactors = FALSE
  )
}))
book <- book[
  book$train_id %in% reference_profile$train_id[reference_profile$reference_eligible],
  , drop = FALSE
]

raw <- read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE)
spikes <- lapply(names(raw), function(name) {
  x <- suppressWarnings(as.numeric(raw[[name]]))
  x[is.finite(x)]
})
names(spikes) <- names(raw)
spikes <- spikes[names(spikes) %in% unique(book$train_id)]
if (!setequal(names(spikes), unique(book$train_id))) {
  stop("Raw/workbook train identities do not match.", call. = FALSE)
}

event_rows <- !is.na(book$event_pattern) &
  nzchar(trimws(as.character(book$event_pattern)))
state_rows <- !is.na(book$state_pattern) &
  nzchar(trimws(as.character(book$state_pattern)))
other_rows <- !event_rows & !state_rows
base_cols <- c(
  "train_id", "Group_ID", "Right_Spike_Index", "ISI_s", "review_status"
)
add_axis <- function(index, axis, values) {
  z <- book[index, base_cols, drop = FALSE]
  names(z)[1L] <- "Train_ID"
  z$Axis <- axis
  z$Pattern <- stpd_pub_normalize_pattern(values[index])
  z
}
intervals <- rbind(
  add_axis(event_rows, "event", book$event_pattern),
  add_axis(state_rows, "state", book$state_pattern),
  add_axis(other_rows, "other", rep("other", nrow(book)))
)
intervals <- intervals[
  order(
    intervals$Train_ID, intervals$Right_Spike_Index, intervals$Axis,
    method = "radix"
  ),
  , drop = FALSE
]
tonic_reference <- stpd_pub_partition_tonic_reference(
  intervals, tonic_reference_role = "tonic_like_review"
)
tonic_like_review <- stpd_pub_tonic_like_review_audit(
  tonic_reference$review_intervals
)
intervals <- tonic_reference$formal_intervals
episodes <- stpd_pub_contiguous_episodes(intervals)
intervals <- stpd_pub_assign_episode_ids(intervals, episodes)
train_group <- unique(book[c("train_id", "Group_ID")])
names(train_group)[1L] <- "Train_ID"
groups <- sort(unique(book$Group_ID), method = "radix")

targets <- data.frame(
  fold_id = c(7L, 8L),
  heldout_group = c("RT1D00.82", "RT2D00.44"),
  start_isi = c(67L, 80L),
  end_isi = c(70L, 84L),
  stringsAsFactors = FALSE
)
targets$train <- vapply(targets$heldout_group, function(group) {
  z <- sort(
    unique(train_group$Train_ID[train_group$Group_ID == group]),
    method = "radix"
  )
  if (length(z) != 1L) {
    stop("Expected one target train for ", group, "; found ", length(z), ".",
         call. = FALSE)
  }
  z[[1L]]
}, character(1))
if (!identical(groups[targets$fold_id], targets$heldout_group)) {
  stop("Prespecified fold IDs no longer match the held-out groups.", call. = FALSE)
}

num_or_na <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  if (!length(y) || !is.finite(y[[1L]])) NA_real_ else y[[1L]]
}
int_or_na <- function(x) {
  y <- suppressWarnings(as.integer(x))
  if (!length(y) || !is.finite(y[[1L]])) NA_integer_ else y[[1L]]
}
chr_or_empty <- function(x) {
  y <- as.character(x)
  if (!length(y) || is.na(y[[1L]])) "" else y[[1L]]
}
candidate_labels <- function(x) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0L) return(character())
  for (name in c(
      "final_label", "review_label", "label", "state_class", "final_candidate_class",
      "raw_candidate_class"
  )) {
    if (name %in% names(x)) return(as.character(x[[name]]))
  }
  rep("", nrow(x))
}
overlap_rows <- function(x, train, start_isi, end_isi) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0L ||
      !all(c("start_isi", "end_isi") %in% names(x))) return(data.frame())
  keep <- as.integer(x$start_isi) <= end_isi &
    as.integer(x$end_isi) >= start_isi
  if ("train" %in% names(x)) keep <- keep & as.character(x$train) == train
  keep[is.na(keep)] <- FALSE
  x[keep, , drop = FALSE]
}
candidate_trace_rows <- function(x, stage, target) {
  x <- overlap_rows(x, target$train, target$start_isi, target$end_isi)
  if (nrow(x) == 0L) {
    return(data.frame(
      fold_id = target$fold_id,
      heldout_group = target$heldout_group,
      train = target$train,
      target_start_isi = target$start_isi,
      target_end_isi = target$end_isi,
      stage = stage,
      candidate_id = "", label = "", candidate_layer = "",
      candidate_source = "", start_isi = NA_integer_,
      end_isi = NA_integer_, overlap_isi_n = 0L, selected = NA,
      selection_status = "no_overlapping_candidate", score = NA_real_,
      priority = NA_real_, stringsAsFactors = FALSE
    ))
  }
  get_col <- function(name, default) {
    if (name %in% names(x)) x[[name]] else rep(default, nrow(x))
  }
  label <- candidate_labels(x)
  selected <- if ("selected_within_track" %in% names(x)) {
    as.logical(x$selected_within_track)
  } else if ("selected_for_auto" %in% names(x)) {
    as.logical(x$selected_for_auto)
  } else {
    rep(NA, nrow(x))
  }
  status <- if ("track_selection_status" %in% names(x)) {
    as.character(x$track_selection_status)
  } else if ("selection_status" %in% names(x)) {
    as.character(x$selection_status)
  } else {
    rep("", nrow(x))
  }
  data.frame(
    fold_id = target$fold_id,
    heldout_group = target$heldout_group,
    train = target$train,
    target_start_isi = target$start_isi,
    target_end_isi = target$end_isi,
    stage = stage,
    candidate_id = as.character(get_col(
      "candidate_id", get_col("source_candidate_id", "")
    )),
    label = as.character(label),
    candidate_layer = as.character(get_col("candidate_layer", "")),
    candidate_source = as.character(get_col("candidate_source", "")),
    start_isi = as.integer(x$start_isi),
    end_isi = as.integer(x$end_isi),
    overlap_isi_n = pmax(
      0L,
      pmin(as.integer(x$end_isi), target$end_isi) -
        pmax(as.integer(x$start_isi), target$start_isi) + 1L
    ),
    selected = selected,
    selection_status = status,
    score = suppressWarnings(as.numeric(get_col("score", NA_real_))),
    priority = suppressWarnings(as.numeric(get_col("priority", NA_real_))),
    stringsAsFactors = FALSE
  )
}

summaries <- list()
candidate_rows <- list()
review_rows <- list()
per_isi_rows <- list()
selected_rows <- list()
manifests <- list()

for (tt in seq_len(nrow(targets))) {
  target <- targets[tt, , drop = FALSE]
  fold_id <- target$fold_id[[1L]]
  heldout_group <- target$heldout_group[[1L]]
  target_train <- target$train[[1L]]
  validation_trains <- sort(
    train_group$Train_ID[train_group$Group_ID == heldout_group],
    method = "radix"
  )
  calibration_trains <- sort(
    setdiff(train_group$Train_ID, validation_trains), method = "radix"
  )
  patterns <- c("burst", "pause", "high_frequency_spiking", "other")
  selected <- stpd_pub_balanced_sample(
    episodes[episodes$Train_ID %in% calibration_trains, , drop = FALSE],
    patterns = patterns, n_each = 10L, seed = 910000L + fold_id
  )
  selected_key <- paste(
    selected$Pattern, selected$Train_ID, selected$Episode, sep = "\r"
  )
  if (nrow(selected) != length(patterns) * 10L ||
      any(table(selected$Pattern) != 10L) || anyDuplicated(selected_key)) {
    stop("Calibration sampling was not unique and balanced in fold ", fold_id,
         ".", call. = FALSE)
  }
  selected_rows[[tt]] <- transform(selected, fold_id = fold_id)
  learned <- stpd_pub_learn_params(
    spikes, calibration_trains, selected, intervals,
    dataset_name = paste0("Grechishnikova_2017_LOGO_", heldout_group),
    bounded_borrowing = TRUE,
    tonic_reference_role = "tonic_like_review"
  )
  validation_pool <- stpd_pub_make_trains(spikes, validation_trains)
  ds <- SpikeTrainPatternDetector:::make_dataset(
    name = paste0("Grechishnikova_2017_LOGO_", heldout_group),
    source = "heldout_label_blind_tonic_trace",
    trains = validation_pool, unit_in = "s"
  )
  detected <- stpd_detect(
    ds, params = learned$params, selected_trains = validation_trains,
    lock_manual = FALSE, collect_diagnostics = FALSE, label_blind = TRUE
  )
  dat <- detected$trains[[target_train]]
  min_isi_sec <- learned$min_valid_isi_sec
  vp <- attr(dat, "event_grammar_params", exact = TRUE)
  if (is.null(vp)) {
    vp <- SpikeTrainPatternDetector:::stpd_event_grammar_params_impl(
      dat, learned$params, min_isi_sec = min_isi_sec, train = target_train
    )
  }
  raw_tonic <- SpikeTrainPatternDetector:::stpd_event_core_detect_tonic(
    dat, learned$params, vp, min_isi_sec = min_isi_sec, train = target_train
  )
  tonic_review <- attr(dat, "tonic_review_candidates", exact = TRUE)
  if (is.null(tonic_review)) tonic_review <- data.frame()
  ledger <- detected$results$candidate_ledger
  ledger_train <- if (is.data.frame(ledger) && "train" %in% names(ledger)) {
    ledger[as.character(ledger$train) == target_train, , drop = FALSE]
  } else {
    ledger
  }
  shadow <- attr(dat, "multitrack_shadow", exact = TRUE)
  if (is.null(shadow)) {
    shadow <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
      ledger_train,
      patterns = learned$params$detector$patterns_to_run,
      params = learned$params
    )
  }
  product <- stpd_multitrack_auto(detected)
  product_states <- product$states[
    as.character(product$states$train) == target_train, , drop = FALSE
  ]
  product_per_isi <- product$per_isi[
    as.character(product$per_isi$train) == target_train, , drop = FALSE
  ]

  start_isi <- target$start_isi[[1L]]
  end_isi <- target$end_isi[[1L]]
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  valid <- is.finite(isi) & !is_artifact_isi(isi, min_isi_sec)
  if (length(valid) > 0L) valid[[1L]] <- FALSE
  bounds <- SpikeTrainPatternDetector:::stpd_event_core_tonic_adaptive_bounds(
    isi, valid, vp, min_isi_sec
  )
  band_flag <- valid & isi >= bounds$lower & isi <= bounds$upper
  band_runs <- SpikeTrainPatternDetector:::stpd_event_core_bool_runs(band_flag)
  overlapping_runs <- overlap_rows(
    band_runs, target_train, start_isi, end_isi
  )
  if (nrow(overlapping_runs) > 0L) {
    overlapping_runs$overlap_n <- pmax(
      0L,
      pmin(overlapping_runs$end_isi, end_isi) -
        pmax(overlapping_runs$start_isi, start_isi) + 1L
    )
    run <- overlapping_runs[which.max(overlapping_runs$overlap_n), , drop = FALSE]
    run_idx <- seq.int(run$start_isi[[1L]], run$end_isi[[1L]])
  } else {
    run <- data.frame(start_isi = NA_integer_, end_isi = NA_integer_)
    run_idx <- integer()
  }
  target_idx <- seq.int(start_isi, end_isi)
  target_vals <- isi[target_idx]
  run_vals <- isi[run_idx]
  duration_for <- function(start, end, values) {
    if (length(values) == 0L || !is.finite(start) || !is.finite(end)) {
      return(NA_real_)
    }
    duration <- suppressWarnings(
      as.numeric(dat$timestamp_sec[end]) -
        as.numeric(dat$timestamp_sec[start - 1L])
    )
    if (!is.finite(duration)) sum(values, na.rm = TRUE) else duration
  }
  target_duration <- duration_for(start_isi, end_isi, target_vals)
  run_duration <- duration_for(
    int_or_na(run$start_isi), int_or_na(run$end_isi), run_vals
  )
  target_lv <- SpikeTrainPatternDetector:::stpd_event_core_lv(target_vals)
  target_mm <- SpikeTrainPatternDetector:::stpd_event_core_mm(target_vals)
  target_cv <- SpikeTrainPatternDetector:::stpd_event_core_cv(target_vals)
  run_lv <- SpikeTrainPatternDetector:::stpd_event_core_lv(run_vals)
  run_mm <- SpikeTrainPatternDetector:::stpd_event_core_mm(run_vals)
  run_cv <- SpikeTrainPatternDetector:::stpd_event_core_cv(run_vals)
  target_mm_effective_max <- vp$tonic_mm_max
  if (is.finite(target_lv) &&
      target_lv <= min(vp$tonic_lv_max, 0.15, na.rm = TRUE) &&
      is.finite(target_cv) && target_cv <= 0.30) {
    target_mm_effective_max <- max(target_mm_effective_max, 1.40, na.rm = TRUE)
  }
  run_mm_effective_max <- vp$tonic_mm_max
  if (is.finite(run_lv) &&
      run_lv <= min(vp$tonic_lv_max, 0.15, na.rm = TRUE) &&
      is.finite(run_cv) && run_cv <= 0.30) {
    run_mm_effective_max <- max(run_mm_effective_max, 1.40, na.rm = TRUE)
  }
  raw_tonic_overlap <- overlap_rows(
    raw_tonic, target_train, start_isi, end_isi
  )
  tonic_review_overlap <- overlap_rows(
    tonic_review, target_train, start_isi, end_isi
  )
  if (nrow(tonic_review) > 0L) {
    review_export <- tonic_review
    review_export$fold_id <- fold_id
    review_export$heldout_group <- heldout_group
    review_export$target_start_isi <- start_isi
    review_export$target_end_isi <- end_isi
    review_export$target_overlap <-
      as.integer(review_export$start_isi) <= end_isi &
      as.integer(review_export$end_isi) >= start_isi
    review_export$target_overlap_isi_n <- pmax(
      0L,
      pmin(as.integer(review_export$end_isi), end_isi) -
        pmax(as.integer(review_export$start_isi), start_isi) + 1L
    )
    review_rows[[length(review_rows) + 1L]] <- review_export
  }
  ledger_overlap <- overlap_rows(
    ledger_train, target_train, start_isi, end_isi
  )
  ledger_tonic <- ledger_overlap[
    candidate_labels(ledger_overlap) == "tonic",
    , drop = FALSE
  ]
  ledger_hfs <- ledger_overlap[
    candidate_labels(ledger_overlap) %in% c(
      "high_frequency_spiking", "high_frequency_tonic",
      "high_frequency_irregular_state"
    ),
    , drop = FALSE
  ]
  shadow_overlap <- overlap_rows(
    shadow$candidates, target_train, start_isi, end_isi
  )
  shadow_tonic <- shadow_overlap[
    candidate_labels(shadow_overlap) == "tonic",
    , drop = FALSE
  ]
  shadow_hfs_selected <- shadow_overlap[
    candidate_labels(shadow_overlap) ==
      "high_frequency_spiking" &
      as.logical(shadow_overlap$selected_within_track %||% FALSE),
    , drop = FALSE
  ]
  shadow_tonic_selected <- shadow_tonic[
    as.logical(shadow_tonic$selected_within_track %||% FALSE),
    , drop = FALSE
  ]
  product_state_overlap <- overlap_rows(
    product_states, target_train, start_isi, end_isi
  )
  product_target <- product_per_isi[
    as.integer(product_per_isi$isi_index) %in% target_idx, , drop = FALSE
  ]

  failure_stage <- if (nrow(raw_tonic_overlap) == 0L) {
    "candidate_not_generated_or_gate_rejected"
  } else if (nrow(ledger_tonic) == 0L) {
    "raw_candidate_missing_from_candidate_ledger"
  } else if (nrow(shadow_tonic_selected) == 0L &&
             nrow(shadow_hfs_selected) > 0L) {
    "tonic_blocked_by_selected_broad_hfs_parent"
  } else if (nrow(shadow_tonic_selected) == 0L) {
    "tonic_not_selected_within_state_track"
  } else if (!any(as.character(product_target$state_class) == "tonic")) {
    "selected_tonic_lost_during_product_materialization"
  } else {
    "tonic_materialized"
  }
  gate_detail <- if (nrow(raw_tonic_overlap) > 0L) {
    "raw_tonic_candidate_overlaps_target"
  } else if (!all(band_flag[target_idx])) {
    "target_split_by_frozen_tonic_band"
  } else if (length(run_idx) + 1L < vp$tonic_min_spikes) {
    "band_run_below_minimum_spike_support"
  } else if (is.finite(run_lv) && run_lv > vp$tonic_lv_max) {
    "band_run_fails_lv"
  } else if (is.finite(run_mm) && run_mm > run_mm_effective_max) {
    "band_run_fails_mm_upper"
  } else if (is.finite(run_mm) && run_mm < vp$tonic_mm_min) {
    "band_run_fails_mm_lower"
  } else if (is.finite(vp$tonic_min_duration) &&
             vp$tonic_min_duration > 0 &&
             (!is.finite(run_duration) ||
                run_duration < vp$tonic_min_duration)) {
    "band_run_fails_calibration_duration"
  } else {
    "requires_candidate_generator_trace"
  }

  summaries[[tt]] <- data.frame(
    fold_id = fold_id,
    heldout_group = heldout_group,
    train = target_train,
    target_start_isi = start_isi,
    target_end_isi = end_isi,
    target_isi_n = length(target_idx),
    target_spike_n = length(target_idx) + 1L,
    target_duration_sec = target_duration,
    target_cv = target_cv,
    target_lv = target_lv,
    target_mm = target_mm,
    tonic_lower_sec = bounds$lower,
    tonic_upper_sec = bounds$upper,
    tonic_core_lower_sec = bounds$core_lower,
    tonic_threshold_source_mode = bounds$tonic_threshold_source_mode,
    tonic_train_lower_adaptation_applied =
      isTRUE(bounds$tonic_train_lower_adaptation_applied),
    tonic_train_upper_adaptation_applied =
      isTRUE(bounds$tonic_train_upper_adaptation_applied),
    tonic_min_spikes = vp$tonic_min_spikes,
    tonic_min_duration_sec = vp$tonic_min_duration,
    tonic_lv_max = vp$tonic_lv_max,
    tonic_mm_min = vp$tonic_mm_min,
    tonic_mm_max = vp$tonic_mm_max,
    target_mm_effective_max = target_mm_effective_max,
    target_band_pass_n = sum(band_flag[target_idx]),
    target_band_all_pass = all(band_flag[target_idx]),
    target_lv_pass = !is.finite(target_lv) || target_lv <= vp$tonic_lv_max,
    target_mm_pass = !is.finite(target_mm) ||
      (target_mm >= vp$tonic_mm_min && target_mm <= target_mm_effective_max),
    target_duration_pass = !is.finite(vp$tonic_min_duration) ||
      vp$tonic_min_duration <= 0 || target_duration >= vp$tonic_min_duration,
    containing_band_run_start_isi = int_or_na(run$start_isi),
    containing_band_run_end_isi = int_or_na(run$end_isi),
    containing_band_run_isi_n = length(run_idx),
    containing_band_run_duration_sec = run_duration,
    containing_band_run_cv = run_cv,
    containing_band_run_lv = run_lv,
    containing_band_run_mm = run_mm,
    containing_band_run_mm_effective_max = run_mm_effective_max,
    raw_tonic_overlap_n = nrow(raw_tonic_overlap),
    tonic_review_candidate_total_n = nrow(tonic_review),
    tonic_review_candidate_total_isi_n = if (nrow(tonic_review) > 0L) {
      sum(as.integer(tonic_review$n_isi), na.rm = TRUE)
    } else 0L,
    tonic_review_target_overlap_n = nrow(tonic_review_overlap),
    tonic_review_target_overlap_isi_n = if (nrow(tonic_review_overlap) > 0L) {
      sum(pmax(
        0L,
        pmin(as.integer(tonic_review_overlap$end_isi), end_isi) -
          pmax(as.integer(tonic_review_overlap$start_isi), start_isi) + 1L
      ))
    } else 0L,
    ledger_tonic_overlap_n = nrow(ledger_tonic),
    ledger_hfs_overlap_n = nrow(ledger_hfs),
    shadow_tonic_selected_overlap_n = nrow(shadow_tonic_selected),
    shadow_hfs_selected_overlap_n = nrow(shadow_hfs_selected),
    product_state_overlap_n = nrow(product_state_overlap),
    product_target_tonic_isi_n = sum(
      as.character(product_target$state_class) == "tonic"
    ),
    product_target_hfs_isi_n = sum(
      as.character(product_target$state_class) == "high_frequency_spiking"
    ),
    failure_stage = failure_stage,
    gate_detail = gate_detail,
    stringsAsFactors = FALSE
  )

  candidate_rows[[length(candidate_rows) + 1L]] <- candidate_trace_rows(
    raw_tonic, "raw_tonic_generator", target
  )
  candidate_rows[[length(candidate_rows) + 1L]] <- candidate_trace_rows(
    tonic_review, "review_only_possible_tonic", target
  )
  candidate_rows[[length(candidate_rows) + 1L]] <- candidate_trace_rows(
    ledger_train, "candidate_ledger", target
  )
  candidate_rows[[length(candidate_rows) + 1L]] <- candidate_trace_rows(
    shadow$candidates, "multitrack_shadow", target
  )
  candidate_rows[[length(candidate_rows) + 1L]] <- candidate_trace_rows(
    product_states, "automatic_product_state", target
  )

  context_idx <- seq.int(
    max(2L, start_isi - 6L), min(nrow(dat), end_isi + 6L)
  )
  truth_rows <- book[
    book$train_id == target_train & book$Right_Spike_Index %in% context_idx,
    , drop = FALSE
  ]
  truth_state <- stats::setNames(
    as.character(truth_rows$state_pattern), truth_rows$Right_Spike_Index
  )
  product_context <- product_per_isi[
    as.integer(product_per_isi$isi_index) %in% context_idx, , drop = FALSE
  ]
  product_state <- stats::setNames(
    as.character(product_context$state_class), product_context$isi_index
  )
  per_isi_rows[[tt]] <- data.frame(
    fold_id = fold_id,
    heldout_group = heldout_group,
    train = target_train,
    isi_index = context_idx,
    is_target = context_idx %in% target_idx,
    ISI_sec = isi[context_idx],
    truth_state = unname(truth_state[as.character(context_idx)]),
    frozen_band_pass = band_flag[context_idx],
    raw_tonic_candidate_support = vapply(context_idx, function(index) {
      nrow(raw_tonic_overlap) > 0L && any(
        raw_tonic_overlap$start_isi <= index & raw_tonic_overlap$end_isi >= index
      )
    }, logical(1)),
    tonic_review_candidate_support = vapply(context_idx, function(index) {
      nrow(tonic_review) > 0L && any(
        tonic_review$start_isi <= index & tonic_review$end_isi >= index
      )
    }, logical(1)),
    selected_shadow_tonic_support = vapply(context_idx, function(index) {
      nrow(shadow_tonic_selected) > 0L && any(
        shadow_tonic_selected$start_isi <= index &
          shadow_tonic_selected$end_isi >= index
      )
    }, logical(1)),
    selected_shadow_hfs_support = vapply(context_idx, function(index) {
      nrow(shadow_hfs_selected) > 0L && any(
        shadow_hfs_selected$start_isi <= index &
          shadow_hfs_selected$end_isi >= index
      )
    }, logical(1)),
    product_state = unname(product_state[as.character(context_idx)]),
    stringsAsFactors = FALSE
  )
  manifests[[tt]] <- transform(learned$manifest, fold_id = fold_id)
}

summary_table <- do.call(rbind, summaries)
candidate_table <- dplyr::bind_rows(candidate_rows)
review_table <- if (length(review_rows) > 0L) {
  dplyr::bind_rows(review_rows)
} else {
  SpikeTrainPatternDetector:::stpd_tonic_review_empty_candidates()
}
per_isi_table <- do.call(rbind, per_isi_rows)
selected_table <- do.call(rbind, selected_rows)
manifest_table <- do.call(rbind, manifests)

write.csv(summary_table, file.path(out_dir, "target_gate_summary.csv"),
          row.names = FALSE)
write.csv(candidate_table, file.path(out_dir, "candidate_stage_trace.csv"),
          row.names = FALSE)
write.csv(review_table, file.path(out_dir, "tonic_review_candidates.csv"),
          row.names = FALSE)
write.csv(per_isi_table, file.path(out_dir, "target_per_isi_trace.csv"),
          row.names = FALSE)
write.csv(selected_table, file.path(out_dir, "calibration_examples.csv"),
          row.names = FALSE)
write.csv(manifest_table, file.path(out_dir, "learned_parameters.csv"),
          row.names = FALSE)
write.csv(
  data.frame(
    artifact = c("manual_workbook", "raw_timestamps", "detector_core",
                 "tonic_review_proposer", "validation_helpers",
                 "diagnostic_script"),
    path = c(
      paste0("external:", basename(xlsx)),
      paste0("external:", basename(raw_path)),
      "R/38_event_grammar_core.R",
      "R/42b_tonic_review_candidates.R",
      "evaluation/publication_validation/publication_validation_helpers.R",
      paste0(
        "evaluation/publication_validation/diagnostics/",
        "real_tonic_miss_trace/run_trace.R"
      )
    ),
    sha256 = vapply(c(
      xlsx, raw_path, file.path(repo, "R", "38_event_grammar_core.R"),
      file.path(repo, "R", "42b_tonic_review_candidates.R"),
      file.path(repo, "evaluation", "publication_validation",
                "publication_validation_helpers.R"),
      file.path(repo, "evaluation", "publication_validation",
                "diagnostics", "real_tonic_miss_trace", "run_trace.R")
    ), digest::digest, character(1), algo = "sha256", file = TRUE),
    stringsAsFactors = FALSE
  ),
  file.path(out_dir, "reproducibility_manifest.csv"), row.names = FALSE
)

cat("Wrote Tonic miss trace to", out_dir, "\n")
print(summary_table, row.names = FALSE)
