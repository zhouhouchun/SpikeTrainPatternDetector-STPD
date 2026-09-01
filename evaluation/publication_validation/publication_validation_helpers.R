# Publication-grade validation helpers for SpikeTrainPatternDetector.
#
# The functions in this file deliberately keep calibration and validation
# separated at train/group level.  They never expose held-out labels to the
# detector and they preserve State and Event as orthogonal axes.

stpd_pub_normalize_pattern <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x[is.na(x) | !nzchar(x) | x %in% c("others", "unlabeled", "none")] <- "other"
  x[x %in% c("possible_burst", "long_burst")] <- "burst"
  x
}

# The manual workbook uses HFS as a broad high-frequency State.  Gate B v3
# deliberately resolves that family into frequency x regularity subtypes.  A
# broad-HFS estimand therefore pools both high-frequency irregular State and
# high-frequency tonic while leaving ordinary tonic outside the family.
stpd_pub_broad_hf_pattern <- function(x) {
  x <- stpd_pub_normalize_pattern(x)
  x[x %in% c(
    "broad_high_frequency_state", "high_frequency_irregular_state",
    "high_frequency_tonic", "hf_unresolved"
  )] <-
    "high_frequency_spiking"
  x
}

stpd_pub_resolve_authoritative_workbook <- function(
    repo, configured_path = "",
    expected_sha256 = "42d246192b9051b82fc979e696619d804051503a21ca65183fd59abaa781d30e") {
  canonical <- file.path(
    normalizePath(repo, mustWork = TRUE), "data", "real", "STN",
    "PD_STN_manual_isi_labels.xlsx"
  )
  if (!file.exists(canonical)) {
    stop("The authoritative PD_STN workbook is missing.", call. = FALSE)
  }
  canonical <- normalizePath(canonical, mustWork = TRUE)
  candidate <- if (nzchar(configured_path)) {
    normalizePath(path.expand(configured_path), mustWork = TRUE)
  } else {
    canonical
  }
  if (!identical(candidate, canonical)) {
    stop(
      "Real publication validation requires the canonical PD_STN path; ",
      "alternate workbooks are not accepted.",
      call. = FALSE
    )
  }
  expected_sha256 <- tolower(trimws(as.character(expected_sha256)[1L]))
  if (!grepl("^[0-9a-f]{64}$", expected_sha256)) {
    stop("The authoritative workbook SHA-256 contract is invalid.",
         call. = FALSE)
  }
  observed_sha256 <- digest::digest(candidate, algo = "sha256", file = TRUE)
  if (!identical(observed_sha256, expected_sha256)) {
    stop(
      "Authoritative workbook SHA-256 mismatch: expected ",
      expected_sha256, ", observed ", observed_sha256, ".",
      call. = FALSE
    )
  }
  list(path = candidate, sha256 = observed_sha256)
}

stpd_pub_partition_tonic_reference <- function(
    intervals,
    tonic_reference_role = c("formal_state", "tonic_like_review")) {
  intervals <- as.data.frame(intervals, stringsAsFactors = FALSE)
  tonic_reference_role <- match.arg(tonic_reference_role)
  required <- c(
    "Train_ID", "Group_ID", "Right_Spike_Index", "ISI_s", "Axis", "Pattern"
  )
  if (!all(required %in% names(intervals))) {
    stop("Tonic reference partition input is incomplete.", call. = FALSE)
  }
  is_tonic <- as.character(intervals$Axis) == "state" &
    stpd_pub_normalize_pattern(intervals$Pattern) == "tonic"
  review <- intervals[is_tonic, , drop = FALSE]
  review$Source_Axis <- rep("state", nrow(review))
  review$Source_Pattern <- rep("tonic", nrow(review))
  review$Axis <- rep("tonic_like_review", nrow(review))
  review$Pattern <- rep("tonic_like_review", nrow(review))
  review$Reporting_Role <- rep(
    "descriptive_review_only_not_state_endpoint", nrow(review)
  )
  formal <- if (identical(tonic_reference_role, "tonic_like_review")) {
    intervals[!is_tonic, , drop = FALSE]
  } else {
    intervals
  }
  # A formal Tonic endpoint must not also be returned as a scoring exclusion.
  # Keeping the descriptive copy here caused otherwise valid GPi Tonic truth
  # rows to be removed by `stpd_pub_truth_axes()`.
  if (identical(tonic_reference_role, "formal_state")) {
    review <- review[FALSE, , drop = FALSE]
  }
  rownames(formal) <- NULL
  rownames(review) <- NULL
  list(
    formal_intervals = formal,
    review_intervals = review,
    tonic_reference_role = tonic_reference_role
  )
}

stpd_pub_tonic_like_review_audit <- function(review_intervals) {
  review <- as.data.frame(review_intervals, stringsAsFactors = FALSE)
  if (!nrow(review)) {
    episodes <- data.frame(
      Train_ID = character(), Group_ID = character(), Review_Label = character(),
      Episode = character(), start_isi = integer(), end_isi = integer(),
      n_isi = integer(), n_spikes = integer(), duration_sec = numeric(),
      median_isi_sec = numeric(), reporting_role = character(),
      stringsAsFactors = FALSE
    )
    return(list(intervals = review, episodes = episodes,
                summary = data.frame(
                  review_label = "tonic_like_review", interval_n = 0L,
                  episode_n = 0L, train_n = 0L,
                  reporting_role = "descriptive_review_only_not_state_endpoint",
                  stringsAsFactors = FALSE)))
  }
  required <- c(
    "Train_ID", "Group_ID", "Right_Spike_Index", "ISI_s", "Axis", "Pattern"
  )
  if (!all(required %in% names(review)) ||
      any(as.character(review$Axis) != "tonic_like_review") ||
      any(as.character(review$Pattern) != "tonic_like_review")) {
    stop("Tonic-like review audit input is invalid.", call. = FALSE)
  }
  episode_table <- stpd_pub_contiguous_episodes(review)
  review <- stpd_pub_assign_episode_ids(review, episode_table)
  groups <- split(
    review,
    paste(review$Train_ID, review$Episode, sep = "\r")
  )
  episodes <- do.call(rbind, lapply(groups, function(z) data.frame(
    Train_ID = as.character(z$Train_ID[1L]),
    Group_ID = as.character(z$Group_ID[1L]),
    Review_Label = "tonic_like_review",
    Episode = as.character(z$Episode[1L]),
    start_isi = min(as.integer(z$Right_Spike_Index)),
    end_isi = max(as.integer(z$Right_Spike_Index)),
    n_isi = nrow(z), n_spikes = nrow(z) + 1L,
    duration_sec = sum(as.numeric(z$ISI_s), na.rm = TRUE),
    median_isi_sec = stats::median(as.numeric(z$ISI_s), na.rm = TRUE),
    reporting_role = "descriptive_review_only_not_state_endpoint",
    stringsAsFactors = FALSE
  )))
  episodes <- episodes[order(
    episodes$Train_ID, episodes$start_isi, method = "radix"
  ), , drop = FALSE]
  rownames(episodes) <- NULL
  review <- review[order(
    review$Train_ID, review$Right_Spike_Index, method = "radix"
  ), , drop = FALSE]
  rownames(review) <- NULL
  list(
    intervals = review,
    episodes = episodes,
    summary = data.frame(
      review_label = "tonic_like_review", interval_n = nrow(review),
      episode_n = nrow(episodes),
      train_n = length(unique(as.character(review$Train_ID))),
      median_episode_isi_n = stats::median(episodes$n_isi),
      median_episode_duration_sec = stats::median(episodes$duration_sec),
      reporting_role = "descriptive_review_only_not_state_endpoint",
      stringsAsFactors = FALSE
    )
  )
}

stpd_pub_resolve_frozen_result_dir <- function(
    env_var, required_files = character(), configured_path = "") {
  env_var <- trimws(as.character(env_var)[1L])
  if (!nzchar(env_var)) {
    stop("A result-directory environment variable name is required.",
         call. = FALSE)
  }
  configured_path <- trimws(as.character(configured_path)[1L])
  if (!nzchar(configured_path)) {
    stop(
      "Set ", env_var,
      " to the explicit frozen result directory; canonical-directory fallback is disabled.",
      call. = FALSE
    )
  }
  path <- normalizePath(path.expand(configured_path), mustWork = TRUE)
  if (!dir.exists(path)) {
    stop(env_var, " does not resolve to a directory.", call. = FALSE)
  }
  required_files <- unique(as.character(required_files))
  required_files <- required_files[nzchar(required_files)]
  missing <- required_files[!file.exists(file.path(path, required_files))]
  if (length(missing)) {
    stop(
      "Frozen result directory from ", env_var,
      " is incomplete: ", paste(missing, collapse = " | "), ".",
      call. = FALSE
    )
  }
  path
}

stpd_pub_validate_real_result_bundle <- function(
    configured_path,
    env_var = "STPD_REAL_VALIDATION_DIR") {
  required <- c(
    "pooled_observed_metrics.csv",
    "group_cluster_bootstrap_95ci.csv",
    "group_cluster_bootstrap_draws.rds",
    "runtime_and_candidate_status.csv",
    "sampled_10_per_pattern_by_fold.csv",
    "learned_parameters_by_fold.csv",
    "confirmatory_event_iou_metrics.csv",
    "confirmatory_isi_support_metrics.csv",
    "explicit_manual_positive_recall_sensitivity.csv",
    "reference_eligibility_by_train.csv",
    "real_data_scope.csv",
    "data_quality_and_protocol_checks.csv",
    "tonic_like_review_intervals.csv",
    "tonic_like_review_episodes.csv",
    "tonic_like_review_summary.csv",
    "publication_validation_result.rds"
  )
  path <- stpd_pub_resolve_frozen_result_dir(
    env_var = env_var, required_files = required,
    configured_path = configured_path
  )
  read_table <- function(file) utils::read.csv(
    file.path(path, file), check.names = FALSE, stringsAsFactors = FALSE
  )
  runtime <- read_table("runtime_and_candidate_status.csv")
  if (!nrow(runtime) || !"tonic_reference_role" %in% names(runtime) ||
      anyNA(runtime$tonic_reference_role) ||
      any(as.character(runtime$tonic_reference_role) != "tonic_like_review")) {
    stop(
      "Frozen real result is not a tonic_like_review run in every fold.",
      call. = FALSE
    )
  }
  selected <- read_table("sampled_10_per_pattern_by_fold.csv")
  if (!nrow(selected) ||
      !all(c("Pattern", "fold_id") %in% names(selected))) {
    stop("Frozen real calibration sample is incomplete.", call. = FALSE)
  }
  selected_pattern <- stpd_pub_normalize_pattern(selected$Pattern)
  expected_calibration <- c(
    "burst", "pause", "high_frequency_spiking", "other"
  )
  selected_by_fold <- split(
    selected_pattern, as.character(selected$fold_id), drop = TRUE
  )
  if (any(selected_pattern == "tonic") || !length(selected_by_fold) ||
      any(!vapply(selected_by_fold, setequal, logical(1),
                  y = expected_calibration))) {
    stop(
      "Frozen real calibration must contain Burst, Pause, Broad HFS, and Other only; Tonic is review-only.",
      call. = FALSE
    )
  }
  learned <- read_table("learned_parameters_by_fold.csv")
  if (!nrow(learned) ||
      !all(c("Parameter", "Source", "fold_id") %in% names(learned))) {
    stop("Frozen real learned-parameter audit is incomplete.", call. = FALSE)
  }
  tonic_parameter <- startsWith(as.character(learned$Parameter), "tonic.")
  if (!any(tonic_parameter) || anyNA(learned$Source[tonic_parameter]) ||
      any(as.character(learned$Source[tonic_parameter]) !=
            "predeclared_default_no_real_tonic_reference")) {
    stop(
      "Frozen real Tonic parameters must use predeclared defaults and no real Tonic calibration evidence.",
      call. = FALSE
    )
  }
  confirmatory_event <- read_table("confirmatory_event_iou_metrics.csv")
  confirmatory_support <- read_table("confirmatory_isi_support_metrics.csv")
  allowed_confirmatory <- c("burst", "pause", "broad_hfs")
  validate_confirmatory <- function(table, file) {
    if (!nrow(table) || !"pattern" %in% names(table)) {
      stop("Frozen ", file, " is missing its confirmatory pattern column.",
           call. = FALSE)
    }
    pattern <- tolower(trimws(as.character(table$pattern)))
    if (anyNA(pattern) || any(!nzchar(pattern)) ||
        !setequal(unique(pattern), allowed_confirmatory)) {
      stop(
        "Frozen ", file,
        " must contain confirmatory Burst, Pause, and Broad HFS only.",
        call. = FALSE
      )
    }
  }
  validate_confirmatory(
    confirmatory_event, "confirmatory_event_iou_metrics.csv"
  )
  validate_confirmatory(
    confirmatory_support, "confirmatory_isi_support_metrics.csv"
  )
  pooled <- read_table("pooled_observed_metrics.csv")
  if (!nrow(pooled) || !"label" %in% names(pooled) ||
      any(stpd_pub_normalize_pattern(pooled$label) == "tonic")) {
    stop("Frozen real formal metrics contain or cannot exclude Tonic.",
         call. = FALSE)
  }
  tonic_summary <- read_table("tonic_like_review_summary.csv")
  required_tonic_summary <- c(
    "review_label", "interval_n", "episode_n", "train_n",
    "median_episode_isi_n", "median_episode_duration_sec", "reporting_role"
  )
  if (nrow(tonic_summary) != 1L ||
      !all(required_tonic_summary %in% names(tonic_summary)) ||
      as.character(tonic_summary$review_label[1L]) != "tonic_like_review" ||
      as.character(tonic_summary$reporting_role[1L]) !=
        "descriptive_review_only_not_state_endpoint") {
    stop("Frozen real Tonic-like descriptive audit is invalid.",
         call. = FALSE)
  }
  quality <- read_table("data_quality_and_protocol_checks.csv")
  if (!all(c("check", "value") %in% names(quality)) ||
      anyDuplicated(as.character(quality$check))) {
    stop("Frozen real protocol checks are invalid.", call. = FALSE)
  }
  quality_value <- function(key, numeric = FALSE) {
    value <- quality$value[as.character(quality$check) == key]
    if (length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
      stop("Frozen real protocol check is missing: ", key, ".",
           call. = FALSE)
    }
    if (!numeric) return(as.character(value))
    number <- suppressWarnings(as.numeric(value))
    if (!is.finite(number)) {
      stop("Frozen real protocol check is non-numeric: ", key, ".",
           call. = FALSE)
    }
    number
  }
  required_quality <- c(
    "patient_n", "hemisphere_n", "recording_group_n", "train_n", "isi_n",
    "manual_labeled_n", "blank_as_other_n", "multi_axis_interval_n",
    "group_overlap_max", "state_reference_eligible_train_n", "bootstrap_n"
  )
  invisible(lapply(required_quality, quality_value, numeric = TRUE))
  if (quality_value("group_overlap_max", numeric = TRUE) != 0) {
    stop("Frozen real result has calibration-validation group overlap.",
         call. = FALSE)
  }
  reference_eligibility <- read_table("reference_eligibility_by_train.csv")
  if (!nrow(reference_eligibility) ||
      !all(c("train_id", "reference_eligible") %in%
           names(reference_eligibility))) {
    stop("Frozen real reference-eligibility table is incomplete.",
         call. = FALSE)
  }
  list(
    path = path,
    runtime = runtime,
    selected = selected,
    learned = learned,
    pooled = pooled,
    confirmatory_event = confirmatory_event,
    confirmatory_support = confirmatory_support,
    tonic_summary = tonic_summary,
    quality = quality,
    quality_value = quality_value,
    reference_eligibility = reference_eligibility,
    required_files = required
  )
}

stpd_pub_filter_axis_reference <- function(x, reference_eligibility = NULL) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (is.null(reference_eligibility) || !nrow(x)) return(x)
  eligibility <- as.data.frame(reference_eligibility, stringsAsFactors = FALSE)
  required <- c(
    "Train_ID", "event_reference_eligible", "state_reference_eligible",
    "coexistence_reference_eligible"
  )
  if (!all(required %in% names(eligibility)) ||
      !all(c("Train_ID", "Axis") %in% names(x))) {
    stop("Axis-specific reference eligibility is incomplete.", call. = FALSE)
  }
  rownames(eligibility) <- as.character(eligibility$Train_ID)
  train <- as.character(x$Train_ID)
  axis <- as.character(x$Axis)
  lookup <- function(column) {
    value <- as.logical(eligibility[train, column])
    value[is.na(value)] <- FALSE
    value
  }
  keep <- ifelse(
    axis == "event", lookup("event_reference_eligible"),
    ifelse(
      axis %in% c("state", "state_strict", "state_hf_family"),
      lookup("state_reference_eligible"),
      ifelse(axis == "coexistence",
             lookup("coexistence_reference_eligible"), TRUE)
    )
  )
  x[as.logical(keep), , drop = FALSE]
}

stpd_pub_contiguous_episodes <- function(intervals) {
  intervals <- as.data.frame(intervals, stringsAsFactors = FALSE)
  required <- c("Train_ID", "Group_ID", "Axis", "Pattern", "Right_Spike_Index")
  stopifnot(all(required %in% names(intervals)))
  intervals <- intervals[order(intervals$Train_ID, intervals$Axis,
                               intervals$Right_Spike_Index, method = "radix"), , drop = FALSE]
  key <- paste(intervals$Train_ID, intervals$Axis, intervals$Pattern, sep = "\r")
  new_episode <- !duplicated(intervals$Train_ID) |
    c(TRUE, diff(intervals$Right_Spike_Index) != 1L)[seq_len(nrow(intervals))] |
    c(TRUE, key[-1L] != key[-length(key)])
  # The first expression above catches train boundaries; explicitly include
  # axis/pattern changes while retaining deterministic row order.
  new_episode <- c(TRUE,
    intervals$Train_ID[-1L] != intervals$Train_ID[-nrow(intervals)] |
      intervals$Axis[-1L] != intervals$Axis[-nrow(intervals)] |
      intervals$Pattern[-1L] != intervals$Pattern[-nrow(intervals)] |
      intervals$Right_Spike_Index[-1L] != intervals$Right_Spike_Index[-nrow(intervals)] + 1L)
  episode_number <- ave(cumsum(new_episode), intervals$Train_ID, intervals$Axis,
                        FUN = function(z) match(z, unique(z)))
  intervals$Episode <- paste(intervals$Axis, intervals$Pattern,
                             sprintf("%05d", episode_number), sep = "__")
  groups <- split(intervals, paste(intervals$Train_ID, intervals$Axis,
                                   intervals$Episode, sep = "\r"))
  do.call(rbind, lapply(groups, function(z) {
    data.frame(
      Train_ID = z$Train_ID[1L], Group_ID = z$Group_ID[1L], Axis = z$Axis[1L],
      Pattern = z$Pattern[1L], Episode = z$Episode[1L],
      Start_Right_Spike_Index = min(z$Right_Spike_Index),
      End_Right_Spike_Index = max(z$Right_Spike_Index),
      N_ISI = nrow(z),
      N_Spikes = nrow(z) + 1L,
      Episode_Duration = sum(as.numeric(z$ISI_s), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

stpd_pub_assign_episode_ids <- function(intervals, episodes) {
  intervals <- as.data.frame(intervals, stringsAsFactors = FALSE)
  episodes <- as.data.frame(episodes, stringsAsFactors = FALSE)
  intervals$Episode <- NA_character_
  for (ii in seq_len(nrow(episodes))) {
    hit <- as.character(intervals$Train_ID) == as.character(episodes$Train_ID[ii]) &
      as.character(intervals$Axis) == as.character(episodes$Axis[ii]) &
      as.character(intervals$Pattern) == as.character(episodes$Pattern[ii]) &
      as.integer(intervals$Right_Spike_Index) >= as.integer(episodes$Start_Right_Spike_Index[ii]) &
      as.integer(intervals$Right_Spike_Index) <= as.integer(episodes$End_Right_Spike_Index[ii])
    intervals$Episode[hit] <- as.character(episodes$Episode[ii])
  }
  if (anyNA(intervals$Episode) || any(!nzchar(intervals$Episode))) {
    stop("Failed to assign every interval to exactly one contiguous episode.", call. = FALSE)
  }
  intervals
}

stpd_pub_balanced_sample <- function(episodes, patterns, n_each = 10L, seed = 1L) {
  episodes <- as.data.frame(episodes, stringsAsFactors = FALSE)
  n_each <- as.integer(n_each)
  out <- list()
  for (ii in seq_along(patterns)) {
    pat <- patterns[[ii]]
    z <- episodes[as.character(episodes$Pattern) == pat, , drop = FALSE]
    if (nrow(z) < n_each) {
      stop("Fewer than ", n_each, " calibration episodes for ", pat,
           " (available: ", nrow(z), ").", call. = FALSE)
    }
    # Pattern-specific salts make the non-Tonic calibration sample invariant to
    # excluding review-only real-data Tonic references.  The values preserve the
    # historical run-split order exactly.
    pattern_salt <- c(
      burst = 1L, pause = 2L, tonic = 3L,
      high_frequency_spiking = 4L, other = 5L
    )
    salt <- unname(pattern_salt[pat])
    if (!length(salt) || is.na(salt)) salt <- ii
    set.seed(as.integer(seed) + as.integer(salt) * 1009L)
    group_names <- sample(sort(unique(as.character(z$Group_ID)), method = "radix"))
    queues <- lapply(group_names, function(g) {
      q <- which(as.character(z$Group_ID) == g)
      q[sample.int(length(q), size = length(q), replace = FALSE)]
    })
    selected <- integer()
    cursor <- rep(1L, length(queues))
    while (length(selected) < n_each) {
      progressed <- FALSE
      for (jj in seq_along(queues)) {
        if (cursor[jj] <= length(queues[[jj]])) {
          selected <- c(selected, queues[[jj]][cursor[jj]])
          cursor[jj] <- cursor[jj] + 1L
          progressed <- TRUE
          if (length(selected) == n_each) break
        }
      }
      if (!progressed) break
    }
    out[[pat]] <- z[selected, , drop = FALSE]
  }
  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  ans$Calibration_Role <- ifelse(ans$Pattern == "other", "negative_control", "parameter_estimation")
  ans
}

stpd_pub_make_trains <- function(spikes, train_ids, selected = NULL, intervals = NULL) {
  train_ids <- intersect(as.character(train_ids), names(spikes))
  out <- lapply(train_ids, function(train_id) {
    timestamp <- sort(unique(as.numeric(spikes[[train_id]])))
    timestamp <- timestamp[is.finite(timestamp)]
    dat <- data.frame(
      idx = seq_along(timestamp), timestamp_sec = timestamp,
      ISI_sec = c(NA_real_, diff(timestamp)),
      pattern_manual = rep("", length(timestamp)),
      pattern_manual_negative = rep("", length(timestamp)),
      pattern_auto = rep("", length(timestamp)), stringsAsFactors = FALSE
    )
    if (!is.null(selected) && !is.null(intervals)) {
      chosen <- selected[as.character(selected$Train_ID) == train_id, , drop = FALSE]
      # Others are applied first, then State, then Event.  This ensures that an
      # Event example remains available to the legacy estimator if orthogonal
      # State/Event labels overlap on the same ISI. HFS gates are learned from
      # the orthogonal interval table below, so no State evidence is lost.
      priority <- c("other", "high_frequency_spiking", "tonic", "pause", "burst")
      chosen <- chosen[order(match(chosen$Pattern, priority)), , drop = FALSE]
      for (jj in seq_len(nrow(chosen))) {
        rows <- intervals[
          as.character(intervals$Train_ID) == train_id &
            as.character(intervals$Axis) == as.character(chosen$Axis[jj]) &
            as.character(intervals$Episode) == as.character(chosen$Episode[jj]) &
            as.character(intervals$Pattern) == as.character(chosen$Pattern[jj]), , drop = FALSE]
        index <- as.integer(rows$Right_Spike_Index)
        index <- index[is.finite(index) & index >= 1L & index <= nrow(dat)]
        label <- if (chosen$Pattern[jj] == "other") "others" else as.character(chosen$Pattern[jj])
        dat$pattern_manual[index] <- label
      }
    }
    dat
  })
  names(out) <- train_ids
  out
}

stpd_pub_safe_band <- function(values, min_valid_isi_sec,
                               lower_p = 0.01, upper_p = 0.99, margin = 0.02) {
  values <- as.numeric(values)
  values <- values[is.finite(values) & values >= min_valid_isi_sec]
  if (length(values) < 2L) stop("Insufficient calibration ISIs.", call. = FALSE)
  q <- stats::quantile(values, c(lower_p, upper_p), na.rm = TRUE,
                       names = FALSE, type = 7)
  c(lower = max(min_valid_isi_sec, q[1L] * (1 - margin)), upper = q[2L] * (1 + margin))
}

stpd_pub_resolve_hfs_pause_bands <- function(hfs_values, pause_values,
                                              min_valid_isi_sec) {
  # HFS direct support, HFS connectors, and Pause have different roles.  A
  # manually labelled inter-burst Pause may be shorter than HFS ISIs, and ISI
  # bands can overlap across trains; neither fact may turn the pooled Pause
  # lower tail into a global HFS ceiling.  Learn both distributions
  # independently.  Instance-level accepted Pause boundaries control geometry
  # at run time.
  hfs_band <- stpd_pub_safe_band(
    hfs_values, min_valid_isi_sec,
    lower_p = 0.01, upper_p = 0.90, margin = 0.01
  )
  pause_observed_band <- stpd_pub_safe_band(
    pause_values, min_valid_isi_sec
  )
  q <- function(x, p) as.numeric(stats::quantile(
    as.numeric(x), p, na.rm = TRUE, names = FALSE, type = 7
  ))
  hfs_bridge <- min(
    max(
      q(hfs_values, 0.99) * 1.01,
      hfs_band[["upper"]] * 1.05,
      na.rm = TRUE
    ),
    hfs_band[["upper"]] * 1.50
  )
  # Separate the ordinary one-sided Pause entry from the short contextual
  # lower tail.  The lower quartile is a deliberately simple calibration
  # statistic for canonical Pause; shorter labelled gaps remain available to
  # the post-Burst inter-burst route and never lower the generic threshold.
  canonical_pause_entry <- max(
    min_valid_isi_sec,
    q(pause_values, 0.25)
  )
  list(
    hfs_band = hfs_band,
    hfs_bridge = hfs_bridge,
    pause_band = c(
      lower = canonical_pause_entry,
      upper = pause_observed_band[["upper"]]
    ),
    contextual_pause_observed_lower = pause_observed_band[["lower"]]
  )
}

stpd_pub_learn_params <- function(spikes, calibration_trains, selected, intervals,
                                  dataset_name, min_examples = 10L,
                                  bounded_borrowing = TRUE,
                                  tonic_reference_role = c(
                                    "formal_state", "tonic_like_review"
                                  )) {
  tonic_reference_role <- match.arg(tonic_reference_role)
  selected <- as.data.frame(selected, stringsAsFactors = FALSE)
  intervals <- as.data.frame(intervals, stringsAsFactors = FALSE)
  if (identical(tonic_reference_role, "tonic_like_review")) {
    selected <- selected[
      stpd_pub_normalize_pattern(selected$Pattern) != "tonic",
      , drop = FALSE
    ]
    intervals <- intervals[!(
      as.character(intervals$Axis) == "state" &
        stpd_pub_normalize_pattern(intervals$Pattern) == "tonic"
    ), , drop = FALSE]
  }
  cal_times <- lapply(spikes[calibration_trains], function(x) sort(as.numeric(x)))
  min_isi <- min(unlist(lapply(cal_times, diff)), na.rm = TRUE)
  min_valid_isi_sec <- max(1e-6, min(0.009, min_isi * 0.99))
  pool <- stpd_pub_make_trains(spikes, calibration_trains, selected, intervals)
  dataset_map <- stats::setNames(rep(dataset_name, length(pool)), names(pool))
  estimated_legacy <- estimate_params_from_manual_pool(
    pool, dataset_map = dataset_map, min_isi_sec = min_valid_isi_sec,
    logisi_mcv_sec = max(0.1, min_valid_isi_sec * 2)
  )
  learned <- stpd_productize_params(estimated_legacy, prefer = "legacy")

  selected_keys <- selected[c("Train_ID", "Axis", "Episode", "Pattern")]
  selected_intervals <- merge(intervals, selected_keys,
    by = c("Train_ID", "Axis", "Episode", "Pattern"), all = FALSE)
  values <- function(pattern) as.numeric(selected_intervals$ISI_s[selected_intervals$Pattern == pattern])
  tonic_support_rows <- selected_intervals[
    selected_intervals$Pattern == "tonic",
    c("Train_ID", "Group_ID", "Episode", "Right_Spike_Index"), drop = FALSE
  ]
  tonic_support_by_episode <- if (nrow(tonic_support_rows)) {
    stats::aggregate(
      Right_Spike_Index ~ Train_ID + Group_ID + Episode,
      data = tonic_support_rows, FUN = length
    )
  } else {
    data.frame()
  }
  tonic_duration_by_episode <- if (nrow(tonic_support_rows)) {
    stats::aggregate(
      ISI_s ~ Train_ID + Group_ID + Episode,
      data = selected_intervals[selected_intervals$Pattern == "tonic", , drop = FALSE],
      FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
    )
  } else {
    data.frame()
  }
  tonic_min_duration <- if (nrow(tonic_duration_by_episode)) {
    as.numeric(stats::quantile(
      as.numeric(tonic_duration_by_episode$ISI_s), 0.05,
      na.rm = TRUE, names = FALSE, type = 7
    ))
  } else {
    0
  }
  tonic_short_support <- if (nrow(tonic_support_by_episode)) {
    stpd_manual_tonic_short_route_estimates(
      data.frame(
        n_spikes = as.integer(tonic_support_by_episode$Right_Spike_Index) + 1L,
        stringsAsFactors = FALSE
      ),
      group = tonic_support_by_episode$Group_ID
    )
  } else {
    stpd_manual_tonic_short_route_estimates(data.frame())
  }
  tonic_episode_rows <- selected_intervals[
    selected_intervals$Pattern == "tonic", , drop = FALSE
  ]
  tonic_mm_episode_table <- if (nrow(tonic_episode_rows)) {
    episode_key <- paste(
      tonic_episode_rows$Train_ID,
      tonic_episode_rows$Group_ID,
      tonic_episode_rows$Episode,
      sep = "\r"
    )
    dplyr::bind_rows(lapply(
      split(tonic_episode_rows, episode_key),
      function(z) {
        values <- as.numeric(z$ISI_s)
        data.frame(
          Train_ID = as.character(z$Train_ID[1L]),
          Group_ID = as.character(z$Group_ID[1L]),
          Episode = as.character(z$Episode[1L]),
          CV = stpd_event_core_cv(values),
          LV = stpd_event_core_lv(values),
          MM = stpd_event_core_mm(values),
          stringsAsFactors = FALSE
        )
      }
    ))
  } else {
    data.frame()
  }
  tonic_mm_relaxation <- stpd_manual_tonic_mm_relaxation_estimates(
    tonic_mm_episode_table,
    group = if (nrow(tonic_mm_episode_table)) {
      tonic_mm_episode_table$Group_ID
    } else {
      character()
    }
  )
  if (identical(tonic_reference_role, "tonic_like_review")) {
    tonic_short_support$enabled <- FALSE
    tonic_short_support$mode <- "not_calibrated"
    tonic_short_support$status <- "disabled_real_tonic_review_only"
    tonic_mm_relaxation$enabled <- FALSE
    tonic_mm_relaxation$status <- "disabled_real_tonic_review_only"
  }
  tonic_short_finite_or_zero <- function(x) {
    z <- suppressWarnings(as.numeric(x))[1L]
    if (length(z) && is.finite(z)) z else 0
  }
  tonic_short_param <- list(
    min_isi_count = as.integer(tonic_short_finite_or_zero(
      tonic_short_support$min_isi_count
    )),
    evidence_n = as.integer(tonic_short_finite_or_zero(
      tonic_short_support$evidence_n
    )),
    group_n = as.integer(tonic_short_finite_or_zero(
      tonic_short_support$group_n
    )),
    count_q10_full = tonic_short_finite_or_zero(
      tonic_short_support$count_q10_full
    ),
    logo_q10_min = tonic_short_finite_or_zero(
      tonic_short_support$logo_q10_min
    ),
    logo_q10_max = tonic_short_finite_or_zero(
      tonic_short_support$logo_q10_max
    ),
    structural_floor_spikes = as.integer(tonic_short_finite_or_zero(
      tonic_short_support$structural_floor_spikes
    ))
  )
  learned$tonic$short_regular_route_enabled <-
    isTRUE(tonic_short_support$enabled)
  learned$tonic$short_regular_min_isi_count <-
    tonic_short_param$min_isi_count
  learned$tonic$short_regular_evidence_n <-
    tonic_short_param$evidence_n
  learned$tonic$short_regular_group_n <-
    tonic_short_param$group_n
  learned$tonic$short_regular_count_q10_full <-
    tonic_short_param$count_q10_full
  learned$tonic$short_regular_logo_q10_min <-
    tonic_short_param$logo_q10_min
  learned$tonic$short_regular_logo_q10_max <-
    tonic_short_param$logo_q10_max
  learned$tonic$short_regular_structural_floor_spikes <-
    tonic_short_param$structural_floor_spikes
  learned$tonic$short_regular_mode <- as.character(tonic_short_support$mode)
  learned$tonic$short_regular_status <-
    as.character(tonic_short_support$status)
  learned$tonic$tonic_mm_relax_lv_max <-
    as.numeric(tonic_mm_relaxation$relax_lv_max)
  learned$tonic$tonic_mm_relax_cv_max <-
    as.numeric(tonic_mm_relaxation$relax_cv_max)
  learned$tonic$tonic_mm_relaxed_max <-
    as.numeric(tonic_mm_relaxation$relaxed_max)
  learned$tonic$mm_relaxation_contract <- tonic_mm_relaxation
  # Freeze State duration from the orthogonal Tonic episode envelope itself.
  # The legacy flattened label pool can split an episode when Event labels
  # coexist, which would make the duration floor depend on materialization
  # order rather than on the declared State reference.
  learned$tonic$D_min <- tonic_min_duration
  burst_band <- stpd_pub_safe_band(values("burst"), min_valid_isi_sec)
  tonic_band <- if (identical(tonic_reference_role, "formal_state")) {
    stpd_pub_safe_band(values("tonic"), min_valid_isi_sec)
  } else {
    c(
      lower = as.numeric(learned$spiketrainpattern$tonic$min_isi_sec),
      upper = as.numeric(learned$spiketrainpattern$tonic$max_isi_sec)
    )
  }
  interval_key <- paste(
    selected_intervals$Train_ID, selected_intervals$Right_Spike_Index,
    sep = "\r"
  )
  pause_key <- paste(
    intervals$Train_ID[intervals$Pattern == "pause"],
    intervals$Right_Spike_Index[intervals$Pattern == "pause"],
    sep = "\r"
  )
  # Canonical Pause is not Broad-HFS direct support even when the manual
  # workbook records both axes at the same ISI.  Learn the HF support band only
  # from non-Pause ISIs; the Pause rows remain available to learn the Gap gate.
  hfs_values <- as.numeric(selected_intervals$ISI_s[
    selected_intervals$Pattern == "high_frequency_spiking" &
      !(interval_key %in% pause_key)
  ])
  resolved_hfs_pause <- stpd_pub_resolve_hfs_pause_bands(
    hfs_values = hfs_values,
    pause_values = values("pause"),
    min_valid_isi_sec = min_valid_isi_sec
  )
  hfs_band <- resolved_hfs_pause$hfs_band
  hfs_bridge <- resolved_hfs_pause$hfs_bridge
  pause_band <- resolved_hfs_pause$pause_band
  contextual_pause_observed_lower <-
    resolved_hfs_pause$contextual_pause_observed_lower
  # Broad manual HFS episodes may contain Pause boundaries and therefore must
  # not independently determine both a spike-count floor and a duration floor
  # for the shorter direct-support segments.  Twenty spikes is the operational
  # support-fragment floor.  Full-episode counts are intentionally not copied
  # into this field because Pause/boundary splitting shortens valid fragments.
  hfs_min_spikes <- 20L
  hfs_min_duration <- 0

  sp <- learned$spiketrainpattern
  sp$burst$seed_lower_sec <- burst_band[["lower"]]
  sp$burst$seed_upper_sec <- burst_band[["upper"]]
  sp$burst$bridge_upper_sec <- max(burst_band[["upper"]],
    as.numeric(estimated_legacy$burst$T_bridge %||% NA_real_), na.rm = TRUE)
  if (identical(tonic_reference_role, "formal_state")) {
    sp$tonic$min_isi_sec <- tonic_band[["lower"]]
    sp$tonic$max_isi_sec <- tonic_band[["upper"]]
    sp$tonic$bridge_upper_sec <- tonic_band[["upper"]]
    sp$tonic$lv_max <- max(as.numeric(sp$tonic$lv_max %||% 0.5) * 1.10,
                           as.numeric(estimated_legacy$tonic$LV_core %||% 0.5))
    sp$tonic$min_duration_sec <- tonic_min_duration
  }
  sp$tonic$short_regular_route_enabled <-
    isTRUE(tonic_short_support$enabled)
  sp$tonic$short_regular_min_isi_count <-
    tonic_short_param$min_isi_count
  sp$tonic$short_regular_evidence_n <-
    tonic_short_param$evidence_n
  sp$tonic$short_regular_group_n <-
    tonic_short_param$group_n
  sp$tonic$short_regular_count_q10_full <-
    tonic_short_param$count_q10_full
  sp$tonic$short_regular_logo_q10_min <-
    tonic_short_param$logo_q10_min
  sp$tonic$short_regular_logo_q10_max <-
    tonic_short_param$logo_q10_max
  sp$tonic$short_regular_structural_floor_spikes <-
    tonic_short_param$structural_floor_spikes
  sp$tonic$short_regular_mode <- as.character(tonic_short_support$mode)
  sp$tonic$short_regular_status <- as.character(tonic_short_support$status)
  sp$tonic$mm_relax_lv_max <-
    as.numeric(tonic_mm_relaxation$relax_lv_max)
  sp$tonic$mm_relax_cv_max <-
    as.numeric(tonic_mm_relaxation$relax_cv_max)
  sp$tonic$mm_relaxed_max <-
    as.numeric(tonic_mm_relaxation$relaxed_max)
  sp$tonic$mm_relaxation_contract <- tonic_mm_relaxation
  sp$pause$min_isi_sec <- pause_band[["lower"]]
  sp$pause$max_isi_sec <- pause_band[["upper"]]
  sp$pause$bridge_upper_sec <- pause_band[["upper"]]
  sp$high_frequency_spiking$min_spikes <- hfs_min_spikes
  sp$high_frequency_spiking$min_duration_sec <- hfs_min_duration
  sp$high_frequency_spiking$short_isi_upper_sec <- hfs_band[["upper"]]
  sp$high_frequency_spiking$q90_isi_max_sec <- hfs_band[["upper"]]
  sp$high_frequency_spiking$epoch_bridge_isi_sec <- hfs_bridge
  sp$high_frequency_spiking$tolerated_gap_isi_sec <- hfs_bridge
  sp$high_frequency_spiking$allowed_large_isi_fraction <- 0.15
  sp$high_frequency_spiking$max_consecutive_large_isi <- 2L
  sp$engine$threshold_source_mode <- "user"
  sp$engine$use_manual_calibration <- FALSE
  sp$tonic$bridge_upper_sec <- max(sp$tonic$max_isi_sec, sp$tonic$bridge_upper_sec)
  sp$pause$bridge_upper_sec <- max(sp$pause$max_isi_sec, sp$pause$bridge_upper_sec)
  learned$spiketrainpattern <- sp
  learned$event_grammar$threshold_source_mode <- "user"
  learned$event_core$use_manual_isi_calibration <- FALSE
  learned$event_grammar$user <- list(
    burst = list(enable = TRUE, seed_lower_sec = sp$burst$seed_lower_sec,
      seed_upper_sec = sp$burst$seed_upper_sec, bridge_upper_sec = sp$burst$bridge_upper_sec,
      contrast_S = sp$burst$contrast_min),
    high_frequency_spiking = list(enable = TRUE, seed_lower_sec = hfs_band[["lower"]],
      seed_upper_sec = hfs_band[["upper"]], bridge_upper_sec = hfs_bridge,
      contrast_S = NA_real_),
    high_frequency_tonic = list(enable = FALSE),
    tonic = list(enable = TRUE, seed_lower_sec = sp$tonic$min_isi_sec,
      seed_upper_sec = sp$tonic$max_isi_sec, bridge_upper_sec = sp$tonic$bridge_upper_sec,
      contrast_S = NA_real_),
    pause = list(enable = TRUE, seed_lower_sec = sp$pause$min_isi_sec,
      seed_upper_sec = sp$pause$max_isi_sec, bridge_upper_sec = sp$pause$bridge_upper_sec,
      contrast_S = NA_real_)
  )
  learned$detector$min_valid_isi_sec <- min_valid_isi_sec
  learned$spiketrainpattern$qc$artifact_min_valid_isi_sec <- min_valid_isi_sec
  learned$detector$pattern_isi_limits$burst <- list(
    min_sec = sp$burst$seed_lower_sec, max_sec = sp$burst$bridge_upper_sec)
  learned$detector$pattern_isi_limits$tonic <- list(
    min_sec = sp$tonic$min_isi_sec, max_sec = sp$tonic$bridge_upper_sec)
  learned$detector$pattern_isi_limits$pause <- list(
    min_sec = sp$pause$min_isi_sec, max_sec = sp$pause$max_isi_sec)
  learned$detector$pattern_isi_limits$high_frequency_spiking <- list(
    min_sec = hfs_band[["lower"]], max_sec = hfs_band[["upper"]])
  learned <- effective_params_for_detector(learned)
  # Freeze the cross-train bridge-only allowance on calibration trains.  The
  # validation detector may read its own unlabeled ISI distribution for bounded
  # scale alignment, but no validation label can enter this contract.
  learned <- stpd_attach_thresholds_to_params_impl(
    learned, ds = list(trains = pool), min_isi_sec = min_valid_isi_sec
  )
  learned <- stpd_freeze_bounded_borrowing_contract(
    learned, pool, min_isi_sec = min_valid_isi_sec
  )
  if (!isTRUE(bounded_borrowing)) {
    learned$event_grammar$bounded_borrowing_contract$enabled <- FALSE
    learned$event_grammar$bounded_borrowing_contract$status <-
      "disabled_prespecified_ablation"
  }
  borrowing_contract <- learned$event_grammar$bounded_borrowing_contract
  issues <- stpd_validate_params(learned)
  if (any(issues$severity == "error")) {
    stop("Learned parameters failed validation: ",
         paste(issues$issue[issues$severity == "error"], collapse = "; "), call. = FALSE)
  }
  tonic_parameter_source <- if (
      identical(tonic_reference_role, "formal_state")) {
    "balanced_manual_examples"
  } else {
    "predeclared_default_no_real_tonic_reference"
  }
  tonic_duration_source <- if (
      identical(tonic_reference_role, "formal_state")) {
    "calibration_tonic_state_episode_duration_q05"
  } else {
    "predeclared_default_no_real_tonic_reference"
  }
  manifest <- data.frame(
    Parameter = c("qc.min_valid_isi_sec", "burst.seed_lower_sec", "burst.seed_upper_sec",
      "burst.bridge_upper_sec", "burst.contrast_min", "burst.structure_contrast_min",
      "burst.structure_geom_contrast_min", "burst.structure_min_isi_count",
      "burst.structure_max_isi_count", "tonic.min_isi_sec", "tonic.max_isi_sec",
      "tonic.lv_max", "tonic.min_duration_sec",
      "pause.contextual_observed_lower_sec",
      "pause.min_isi_sec", "pause.max_isi_sec",
      "hfs.short_lower_sec", "hfs.short_upper_sec", "hfs.bridge_isi_sec",
      "hfs.min_spikes", "hfs.min_duration_sec",
      "burst.bounded_borrowing_enabled",
      "burst.bounded_borrowing_max_expansion_ratio",
      "burst.bounded_borrowing_calibration_train_n",
      "burst.bounded_borrowing_evidence_train_n",
      "burst.bounded_borrowing_consensus_bridge_upper_sec"),
    Value = c(min_valid_isi_sec, sp$burst$seed_lower_sec, sp$burst$seed_upper_sec,
      sp$burst$bridge_upper_sec, sp$burst$contrast_min,
      sp$burst$structure_first_contrast_min, sp$burst$structure_first_geom_contrast_min,
      sp$burst$structure_first_min_isi_count, sp$burst$structure_first_max_isi_count,
      sp$tonic$min_isi_sec, sp$tonic$max_isi_sec, sp$tonic$lv_max,
      as.numeric(learned$tonic$D_min %||% 0),
      contextual_pause_observed_lower,
      sp$pause$min_isi_sec, sp$pause$max_isi_sec, hfs_band[["lower"]],
      hfs_band[["upper"]], hfs_bridge, hfs_min_spikes, hfs_min_duration,
      as.numeric(isTRUE(borrowing_contract$enabled)),
      as.numeric(borrowing_contract$max_expansion_ratio),
      as.numeric(borrowing_contract$calibration_train_n),
      as.numeric(borrowing_contract$manual_burst_evidence_train_n),
      as.numeric(borrowing_contract$consensus_bridge_upper_sec)),
    Source = c(rep("balanced_manual_examples", 9L),
      rep(tonic_parameter_source, 3L), tonic_duration_source,
      "manual_pause_lower_tail_retained_for_contextual_interburst_route",
      "balanced_manual_pause_q25_canonical_entry",
      "balanced_manual_examples",
      rep("balanced_hfs_direct_support_excluding_canonical_pause", 2L),
      "calibrated_hfs_tail_relative_connector_no_pause_minimum_cap",
      "prespecified_operational_hfs_minimum_20_spikes",
      "duration_advisory_not_hard_gate",
      "calibration_frozen_bridge_only_policy",
      "prespecified_relative_safety_cap",
      "calibration_only_no_validation_labels",
      "equal_train_weighted_manual_burst_consensus",
      "median_trainwise_burst_upper_quantile"),
    stringsAsFactors = FALSE
  )
  manifest <- rbind(
    manifest,
    data.frame(
      Parameter = c(
        "tonic.short_regular_route_enabled",
        "tonic.short_regular_min_isi_count",
        "tonic.short_regular_calibration_episode_n",
        "tonic.short_regular_calibration_group_n",
        "tonic.short_regular_q10_full",
        "tonic.short_regular_logo_q10_min",
        "tonic.short_regular_logo_q10_max",
        "tonic.short_regular_structural_floor_spikes"
      ),
      Value = c(
        as.numeric(isTRUE(tonic_short_support$enabled)),
        as.numeric(tonic_short_support$min_isi_count),
        as.numeric(tonic_short_support$evidence_n),
        as.numeric(tonic_short_support$group_n),
        as.numeric(tonic_short_support$count_q10_full),
        as.numeric(tonic_short_support$logo_q10_min),
        as.numeric(tonic_short_support$logo_q10_max),
        as.numeric(tonic_short_support$structural_floor_spikes)
      ),
      Source = rep(
        if (identical(tonic_reference_role, "formal_state")) {
          paste0(
            "calibration_group_logo_q10_v1__",
            as.character(tonic_short_support$status)
          )
        } else {
          "predeclared_default_no_real_tonic_reference"
        },
        8L
      ),
      stringsAsFactors = FALSE
    )
  )
  manifest <- rbind(
    manifest,
    data.frame(
      Parameter = c(
        "tonic.mm_relax_lv_max",
        "tonic.mm_relax_cv_max",
        "tonic.mm_relaxed_max",
        "tonic.mm_relaxation_evidence_n",
        "tonic.mm_relaxation_group_n",
        "tonic.mm_relaxation_q95_full",
        "tonic.mm_relaxation_logo_q95_min",
        "tonic.mm_relaxation_logo_q95_median",
        "tonic.mm_relaxation_logo_q95_max"
      ),
      Value = c(
        as.numeric(tonic_mm_relaxation$relax_lv_max),
        as.numeric(tonic_mm_relaxation$relax_cv_max),
        as.numeric(tonic_mm_relaxation$relaxed_max),
        as.numeric(tonic_mm_relaxation$evidence_n),
        as.numeric(tonic_mm_relaxation$group_n),
        as.numeric(tonic_mm_relaxation$q95_full),
        as.numeric(tonic_mm_relaxation$logo_q95_min),
        as.numeric(tonic_mm_relaxation$logo_q95_median),
        as.numeric(tonic_mm_relaxation$logo_q95_max)
      ),
      Source = rep(
        if (identical(tonic_reference_role, "formal_state")) {
          paste0(
            "calibration_episode_group_logo_q95_v1__",
            as.character(tonic_mm_relaxation$status)
          )
        } else {
          "predeclared_default_no_real_tonic_reference"
        },
        9L
      ),
      stringsAsFactors = FALSE
    )
  )
  list(params = learned, manifest = manifest, issues = issues,
       min_valid_isi_sec = min_valid_isi_sec,
       tonic_reference_role = tonic_reference_role)
}

stpd_pub_candidate_provenance <- function(metadata) {
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  required <- c("schema_version", "policy_hash", "product_sha256")
  if (nrow(metadata) != 1L || !all(required %in% names(metadata))) {
    stop("Multitrack candidate provenance is incomplete.", call. = FALSE)
  }
  schema_version <- as.character(metadata$schema_version[1L])
  policy_hash <- as.character(metadata$policy_hash[1L])
  product_sha256 <- as.character(metadata$product_sha256[1L])
  if (!nzchar(schema_version) ||
      !grepl("^[0-9a-f]{64}$", policy_hash) ||
      !grepl("^[0-9a-f]{64}$", product_sha256)) {
    stop("Multitrack candidate provenance is invalid.", call. = FALSE)
  }
  data.frame(
    multitrack_schema_version = schema_version,
    multitrack_policy_hash = policy_hash,
    multitrack_product_sha256 = product_sha256,
    stringsAsFactors = FALSE
  )
}

stpd_pub_extract_predictions <- function(detected, validation_trains) {
  product <- stpd_multitrack_auto(detected)
  if (is.null(product) || !identical(as.character(product$metadata$materialization_status[1]),
                                     "materialized")) {
    stop("Orthogonal multitrack candidate was not materialized.", call. = FALSE)
  }
  p <- product$per_isi
  required_per_isi <- c(
    "train", "isi_index", "state_class", "event_family", "gap_class",
    "state_episode_id", "state_episode_class"
  )
  if (!all(required_per_isi %in% names(p))) {
    stop(
      "Orthogonal multitrack candidate lacks State-episode projection columns.",
      call. = FALSE
    )
  }
  p <- p[as.character(p$train) %in% validation_trains & as.integer(p$isi_index) >= 2L, , drop = FALSE]
  state <- stpd_pub_normalize_pattern(p$state_class)
  event <- ifelse(nzchar(as.character(p$gap_class)),
                  stpd_pub_normalize_pattern(p$gap_class),
                  stpd_pub_normalize_pattern(p$event_family))
  # Fail closed at the evaluation boundary as well as in the strict v3
  # accessor: a canonical Pause is episode context, never direct State support.
  state[event == "pause"] <- "other"
  state_hf <- stpd_pub_broad_hf_pattern(state)
  episode_hf <- if ("state_episode_class" %in% names(p)) {
    stpd_pub_broad_hf_pattern(p$state_episode_class)
  } else {
    state_hf
  }
  joint <- ifelse(state_hf == "high_frequency_spiking" & event == "burst",
                  "high_frequency_spiking+burst",
           ifelse(episode_hf == "high_frequency_spiking" & event == "pause",
                  "high_frequency_spiking+pause", "other"))
  interval <- rbind(
    data.frame(Train_ID = p$train, Right_Spike_Index = p$isi_index,
               Axis = "event", Prediction = event),
    data.frame(Train_ID = p$train, Right_Spike_Index = p$isi_index,
               Axis = "state_strict", Prediction = state),
    data.frame(Train_ID = p$train, Right_Spike_Index = p$isi_index,
               Axis = "state_hf_family", Prediction = state_hf),
    data.frame(Train_ID = p$train, Right_Spike_Index = p$isi_index,
               Axis = "coexistence", Prediction = joint)
  )
  # Direct State support remains strict: canonical Pause rows are not HFS.
  # Event geometry is different.  Once a Broad-HFS parent has been accepted,
  # its stable state_episode_id defines one biological episode envelope, so a
  # related canonical Pause does not re-fragment it for event-level scoring.
  pred_events <- stpd_pub_axis_runs(
    interval, value_col = "Prediction",
    axes = c("event", "state_strict", "state_hf_family")
  )
  pred_events <- pred_events[!(
    pred_events$Axis == "state_hf_family" &
      pred_events$Pattern == "high_frequency_spiking"
  ), , drop = FALSE]
  episode_class <- stpd_pub_broad_hf_pattern(p$state_episode_class)
  episode_id <- trimws(as.character(p$state_episode_id))
  episode_rows <- p[
    episode_class == "high_frequency_spiking" & nzchar(episode_id),
    c("train", "isi_index", "state_episode_id"), drop = FALSE
  ]
  if (nrow(episode_rows)) {
    episode_key <- paste(
      as.character(episode_rows$train),
      as.character(episode_rows$state_episode_id), sep = "\r"
    )
    groups <- split(seq_len(nrow(episode_rows)), episode_key)
    parent_events <- do.call(rbind, lapply(groups, function(index) {
      data.frame(
        Train_ID = as.character(episode_rows$train[index[1L]]),
        Axis = "state_hf_family", Pattern = "high_frequency_spiking",
        start_isi = min(as.integer(episode_rows$isi_index[index])),
        end_isi = max(as.integer(episode_rows$isi_index[index])),
        stringsAsFactors = FALSE
      )
    }))
    rownames(parent_events) <- NULL
    pred_events <- rbind(pred_events, parent_events)
  }
  pred_events <- pred_events[order(
    pred_events$Train_ID, pred_events$Axis, pred_events$Pattern,
    pred_events$start_isi, pred_events$end_isi, method = "radix"
  ), , drop = FALSE]
  rownames(pred_events) <- NULL
  list(interval = interval, events = pred_events,
       candidate_metadata = product$metadata)
}

stpd_pub_truth_axes <- function(intervals, validation_trains,
                                state_review_exclusions = NULL) {
  identity_columns <- c(
    "Train_ID", "Group_ID", "Right_Spike_Index", "ISI_s"
  )
  review <- as.data.frame(
    state_review_exclusions %||% data.frame(), stringsAsFactors = FALSE
  )
  base_source <- intervals[identity_columns]
  if (nrow(review)) {
    if (!all(identity_columns %in% names(review))) {
      stop("State review exclusions are missing interval identity columns.",
           call. = FALSE)
    }
    # Review-only State rows still belong to the independent Event universe.
    # Restore their interval identity here; they are removed only from the
    # State/Coexistence axes below and therefore cannot enter State scoring.
    base_source <- rbind(base_source, review[identity_columns])
  }
  base <- unique(base_source)
  base <- base[base$Train_ID %in% validation_trains, , drop = FALSE]
  get_pattern <- function(axis_name, default = "other") {
    z <- intervals[intervals$Train_ID %in% validation_trains & intervals$Axis == axis_name,
                   c("Train_ID", "Right_Spike_Index", "Pattern"), drop = FALSE]
    key <- paste(z$Train_ID, z$Right_Spike_Index, sep = "\r")
    ans <- setNames(as.character(z$Pattern), key)
    out <- ans[paste(base$Train_ID, base$Right_Spike_Index, sep = "\r")]
    out[is.na(out) | !nzchar(out)] <- default
    stpd_pub_normalize_pattern(out)
  }
  event <- get_pattern("event")
  state_envelope <- get_pattern("state")
  episode_hf <- stpd_pub_broad_hf_pattern(state_envelope)
  # Gap and State are disjoint support axes.  A Pause may remain related to the
  # surrounding HF episode, but its own ISI is not scored as HF direct support.
  state <- state_envelope
  state[event == "pause"] <- "other"
  state_hf <- stpd_pub_broad_hf_pattern(state)
  joint <- ifelse(state_hf == "high_frequency_spiking" & event == "burst",
                  "high_frequency_spiking+burst",
           ifelse(episode_hf == "high_frequency_spiking" & event == "pause",
                  "high_frequency_spiking+pause", "other"))
  out <- rbind(
    data.frame(base, Axis = "event", Truth = event),
    data.frame(base, Axis = "state_strict", Truth = state),
    data.frame(base, Axis = "state_hf_family", Truth = state_hf),
    data.frame(base, Axis = "coexistence", Truth = joint)
  )
  if (nrow(review)) {
    review <- review[as.character(review$Train_ID) %in% validation_trains,
                     , drop = FALSE]
    review_key <- paste(
      review$Train_ID, review$Right_Spike_Index, sep = "\r"
    )
    out_key <- paste(out$Train_ID, out$Right_Spike_Index, sep = "\r")
    formal_state_axis <- out$Axis %in%
      c("state_strict", "state_hf_family", "coexistence")
    out <- out[!(formal_state_axis & out_key %in% review_key), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

stpd_pub_axis_runs <- function(interval, value_col,
                               axes = c("event", "state_strict", "state_hf_family")) {
  interval <- as.data.frame(interval, stringsAsFactors = FALSE)
  required <- c("Train_ID", "Right_Spike_Index", "Axis", value_col)
  if (!all(required %in% names(interval))) {
    stop("Axis-run input is missing required columns.", call. = FALSE)
  }
  value <- stpd_pub_normalize_pattern(interval[[value_col]])
  z <- data.frame(
    Train_ID = as.character(interval$Train_ID),
    Right_Spike_Index = as.integer(interval$Right_Spike_Index),
    Axis = as.character(interval$Axis), Pattern = value,
    stringsAsFactors = FALSE
  )
  z <- z[z$Axis %in% axes & z$Pattern != "other", , drop = FALSE]
  if (!nrow(z)) return(data.frame(
    Train_ID = character(), Axis = character(), Pattern = character(),
    start_isi = integer(), end_isi = integer(), stringsAsFactors = FALSE
  ))
  z <- z[order(z$Train_ID, z$Axis, z$Pattern, z$Right_Spike_Index,
               method = "radix"), , drop = FALSE]
  key <- paste(z$Train_ID, z$Axis, z$Pattern, sep = "\r")
  new_run <- c(TRUE,
    key[-1L] != key[-nrow(z)] |
      z$Right_Spike_Index[-1L] != z$Right_Spike_Index[-nrow(z)] + 1L)
  run_id <- cumsum(new_run)
  rows <- lapply(split(seq_len(nrow(z)), run_id), function(index) {
    data.frame(
      Train_ID = z$Train_ID[index[1L]], Axis = z$Axis[index[1L]],
      Pattern = z$Pattern[index[1L]],
      start_isi = min(z$Right_Spike_Index[index]),
      end_isi = max(z$Right_Spike_Index[index]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

stpd_pub_confirmatory_evaluation_events <- function(events) {
  events <- as.data.frame(events, stringsAsFactors = FALSE)
  required <- c("Train_ID", "Axis", "Pattern", "start_isi", "end_isi")
  if (!all(required %in% names(events))) {
    stop("Confirmatory event evaluation input is missing required columns.",
         call. = FALSE)
  }
  keep <- (events$Axis == "event" & events$Pattern %in% c("burst", "pause")) |
    (events$Axis == "state_hf_family" &
       events$Pattern == "high_frequency_spiking")
  out <- events[keep, required, drop = FALSE]
  out$pattern <- ifelse(
    out$Axis == "state_hf_family", "broad_hfs", as.character(out$Pattern)
  )
  out$train <- as.character(out$Train_ID)
  out <- out[c("train", "pattern", "start_isi", "end_isi")]
  out$start_isi <- as.integer(out$start_isi)
  out$end_isi <- as.integer(out$end_isi)
  out <- unique(out)
  out <- out[order(
    out$train, out$pattern, out$start_isi, out$end_isi, method = "radix"
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_pub_truth_events <- function(intervals, validation_trains,
                                  state_review_exclusions = NULL) {
  truth <- stpd_pub_truth_axes(
    intervals, validation_trains,
    state_review_exclusions = state_review_exclusions
  )
  events <- stpd_pub_axis_runs(
    truth, value_col = "Truth",
    axes = c("event", "state_strict", "state_hf_family")
  )
  events <- events[!(
    events$Axis == "state_hf_family" &
      events$Pattern == "high_frequency_spiking"
  ), , drop = FALSE]
  envelope <- intervals[
    as.character(intervals$Train_ID) %in% validation_trains &
      as.character(intervals$Axis) == "state",
    c("Train_ID", "Right_Spike_Index", "Pattern"), drop = FALSE
  ]
  envelope$Pattern <- stpd_pub_broad_hf_pattern(envelope$Pattern)
  # Some held-out groups legitimately contain no manually labelled State row.
  # Preserve the typed zero-row table instead of assigning a length-one scalar.
  envelope$Axis <- rep("state_hf_family", nrow(envelope))
  envelope <- envelope[envelope$Pattern == "high_frequency_spiking", , drop = FALSE]
  if (nrow(envelope)) {
    parent_events <- stpd_pub_axis_runs(
      envelope, value_col = "Pattern", axes = "state_hf_family"
    )
    events <- rbind(events, parent_events)
  }
  events <- events[order(
    events$Train_ID, events$Axis, events$Pattern,
    events$start_isi, events$end_isi, method = "radix"
  ), , drop = FALSE]
  rownames(events) <- NULL
  events
}

stpd_pub_counts <- function(joined, repeat_id, group_map) {
  axes <- sort(unique(as.character(joined$Axis)), method = "radix")
  out <- list()
  for (axis in axes) {
    z <- joined[joined$Axis == axis, , drop = FALSE]
    labels <- sort(unique(c(as.character(z$Truth), as.character(z$Prediction))), method = "radix")
    for (train in sort(unique(as.character(z$Train_ID)), method = "radix")) {
      q <- z[z$Train_ID == train, , drop = FALSE]
      for (label in labels) {
        tp <- sum(q$Truth == label & q$Prediction == label)
        fp <- sum(q$Truth != label & q$Prediction == label)
        fn <- sum(q$Truth == label & q$Prediction != label)
        out[[length(out) + 1L]] <- data.frame(
          repeat_id = repeat_id, cluster_id = as.character(group_map[[train]]), train = train,
          level = "interval", axis = axis, label = label,
          tp = tp, fp = fp, fn = fn, support = sum(q$Truth == label),
          correct = sum(q$Truth == q$Prediction), total = nrow(q), stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, out)
}

stpd_pub_event_counts <- function(pred, truth, repeat_id, group_map, iou_min = 0.25) {
  axes <- sort(unique(c(as.character(pred$Axis), as.character(truth$Axis))), method = "radix")
  out <- list()
  for (axis in axes) {
    p <- pred[pred$Axis == axis, , drop = FALSE]
    t <- truth[truth$Axis == axis, , drop = FALSE]
    labels <- sort(unique(c(as.character(p$Pattern), as.character(t$Pattern))), method = "radix")
    trains <- sort(unique(c(as.character(p$Train_ID), as.character(t$Train_ID))), method = "radix")
    for (train in trains) {
      pp <- p[p$Train_ID == train, , drop = FALSE]
      tt <- t[t$Train_ID == train, , drop = FALSE]
      names(pp)[names(pp) == "Train_ID"] <- "train"
      names(tt)[names(tt) == "Train_ID"] <- "train"
      for (label in labels) {
        px <- pp[pp$Pattern == label, , drop = FALSE]
        tx <- tt[tt$Pattern == label, , drop = FALSE]
        matches <- if (nrow(px) && nrow(tx)) stpd_match_events_optimal(
          px, tx, class_col = "Pattern", iou_min = iou_min) else data.frame()
        tp <- nrow(matches)
        out[[length(out) + 1L]] <- data.frame(
          repeat_id = repeat_id, cluster_id = as.character(group_map[[train]]), train = train,
          level = "event", axis = axis, label = label,
          tp = tp, fp = nrow(px) - tp, fn = nrow(tx) - tp, support = nrow(tx),
          correct = NA_integer_, total = NA_integer_, stringsAsFactors = FALSE)
      }
    }
  }
  if (length(out)) do.call(rbind, out) else data.frame()
}

stpd_pub_metric_from_counts <- function(counts) {
  counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  keys <- unique(counts[c("level", "axis", "label")])
  do.call(rbind, lapply(seq_len(nrow(keys)), function(ii) {
    k <- keys[ii, ]
    z <- counts[counts$level == k$level & counts$axis == k$axis & counts$label == k$label, ]
    tp <- sum(z$tp); fp <- sum(z$fp); fn <- sum(z$fn)
    precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
    recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
    f1_denominator <- 2 * tp + fp + fn
    f1 <- if (f1_denominator > 0) 2 * tp / f1_denominator else NA_real_
    data.frame(k, tp = tp, fp = fp, fn = fn, support = sum(z$support),
      precision = precision, recall = recall, F1 = f1,
      accuracy = if (k$level == "interval") sum(z$correct) / sum(z$total) else NA_real_)
  }))
}

stpd_pub_bootstrap <- function(counts, n_bootstrap = 1000L, seed = 1L) {
  counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  clusters <- sort(unique(as.character(counts$cluster_id)), method = "radix")
  set.seed(seed)
  draws <- vector("list", n_bootstrap)
  for (bb in seq_len(n_bootstrap)) {
    sampled <- sample(clusters, length(clusters), replace = TRUE)
    pieces <- lapply(seq_along(sampled), function(ii) {
      z <- counts[counts$cluster_id == sampled[ii], , drop = FALSE]
      repeats <- sort(unique(z$repeat_id))
      chosen <- repeats[sample.int(length(repeats), 1L)]
      z <- z[z$repeat_id == chosen, , drop = FALSE]
      z$cluster_id <- paste0(z$cluster_id, "__boot", ii)
      z
    })
    m <- stpd_pub_metric_from_counts(do.call(rbind, pieces))
    m$bootstrap_replicate <- bb
    draws[[bb]] <- m
  }
  boot <- do.call(rbind, draws)
  observed <- stpd_pub_metric_from_counts(counts)
  summary <- do.call(rbind, lapply(seq_len(nrow(observed)), function(ii) {
    k <- observed[ii, ]
    z <- boot[boot$level == k$level & boot$axis == k$axis & boot$label == k$label, ]
    rows <- lapply(c("precision", "recall", "F1", "accuracy"), function(metric) {
      values <- as.numeric(z[[metric]]); values <- values[is.finite(values)]
      ci <- if (length(values)) stats::quantile(values, c(.025, .975), names = FALSE, type = 6) else c(NA, NA)
      data.frame(level = k$level, axis = k$axis, label = k$label, metric = metric,
        observed = as.numeric(k[[metric]]), bootstrap_n = length(values),
        bootstrap_mean = if (length(values)) mean(values) else NA_real_,
        ci_low = ci[1L], ci_high = ci[2L],
        ci_method = "cluster percentile bootstrap; one held-out repeat sampled per cluster",
        stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  }))
  list(observed = observed, bootstrap = boot, summary = summary)
}

stpd_pub_run_split <- function(spikes, intervals, episodes, calibration_trains,
                               validation_trains, repeat_id, seed, dataset_name,
                               bounded_borrowing = TRUE,
                               reference_eligibility = NULL,
                               tonic_reference_role = c(
                                 "formal_state", "tonic_like_review"
                               ),
                               state_review_exclusions = NULL) {
  tonic_reference_role <- match.arg(tonic_reference_role)
  patterns <- c("burst", "pause", "tonic", "high_frequency_spiking", "other")
  if (identical(tonic_reference_role, "tonic_like_review")) {
    patterns <- patterns[patterns != "tonic"]
  }
  selected <- stpd_pub_balanced_sample(
    episodes[episodes$Train_ID %in% calibration_trains, , drop = FALSE],
    patterns = patterns, n_each = 10L, seed = seed)
  learned <- stpd_pub_learn_params(spikes, calibration_trains, selected, intervals,
                                   dataset_name = dataset_name,
                                   bounded_borrowing = bounded_borrowing,
                                   tonic_reference_role = tonic_reference_role)
  validation_pool <- stpd_pub_make_trains(spikes, validation_trains)
  ds <- SpikeTrainPatternDetector:::make_dataset(
    name = dataset_name, source = "heldout_label_blind_validation",
    trains = validation_pool, unit_in = "s")
  started <- Sys.time()
  detected <- stpd_detect(ds, params = learned$params, selected_trains = validation_trains,
    lock_manual = FALSE, collect_diagnostics = FALSE, label_blind = TRUE)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  borrowing_runtime <- lapply(validation_trains, function(train) {
    vp <- attr(detected$trains[[train]], "event_grammar_params")
    data.frame(
      train = train,
      borrowing_applied = isTRUE(vp$cross_train_borrowing_applied),
      borrowing_status = as.character(
        vp$cross_train_borrowing_status %||% "not_configured"
      ),
      stringsAsFactors = FALSE
    )
  })
  borrowing_runtime <- do.call(rbind, borrowing_runtime)
  prediction <- stpd_pub_extract_predictions(detected, validation_trains)
  provenance <- stpd_pub_candidate_provenance(prediction$candidate_metadata)
  truth <- stpd_pub_truth_axes(
    intervals, validation_trains,
    state_review_exclusions = state_review_exclusions
  )
  truth <- stpd_pub_filter_axis_reference(truth, reference_eligibility)
  prediction$interval <- stpd_pub_filter_axis_reference(
    prediction$interval, reference_eligibility
  )
  prediction$events <- stpd_pub_filter_axis_reference(
    prediction$events, reference_eligibility
  )
  joined <- merge(truth, prediction$interval,
    by = c("Train_ID", "Right_Spike_Index", "Axis"), all.x = TRUE, sort = FALSE)
  joined$Prediction[is.na(joined$Prediction)] <- "other"
  truth_events <- stpd_pub_truth_events(
    intervals, validation_trains,
    state_review_exclusions = state_review_exclusions
  )
  truth_events <- stpd_pub_filter_axis_reference(
    truth_events, reference_eligibility
  )
  group_map <- stats::setNames(unique(intervals[c("Train_ID", "Group_ID")])$Group_ID,
                              unique(intervals[c("Train_ID", "Group_ID")])$Train_ID)
  counts <- rbind(
    stpd_pub_counts(joined, repeat_id, group_map),
    stpd_pub_event_counts(prediction$events, truth_events, repeat_id, group_map)
  )
  if (identical(tonic_reference_role, "tonic_like_review")) {
    counts <- counts[counts$label != "tonic", , drop = FALSE]
  }
  list(counts = counts, joined = joined, selected = selected,
       predicted_events = prediction$events, truth_events = truth_events,
       parameter_manifest = learned$manifest, parameter_issues = learned$issues,
       split = data.frame(repeat_id = repeat_id,
         Role = c(rep("calibration", length(calibration_trains)),
                  rep("validation", length(validation_trains))),
         Train_ID = c(calibration_trains, validation_trains),
         Group_ID = unname(group_map[c(calibration_trains, validation_trains)]),
         stringsAsFactors = FALSE),
       runtime = cbind(
         data.frame(repeat_id = repeat_id, elapsed_seconds = elapsed,
           label_blind = isTRUE(detected$results$label_blind),
           multitrack_authoritative = isTRUE(prediction$candidate_metadata$authoritative[1]),
           multitrack_promotion_status = as.character(prediction$candidate_metadata$promotion_status[1]),
           bounded_borrowing_variant = if (isTRUE(bounded_borrowing))
             "enabled" else "disabled_ablation",
           tonic_reference_role = tonic_reference_role,
           bounded_borrowing_schema_version = as.character(
             learned$params$event_grammar$bounded_borrowing_contract$schema_version
           ),
           bounded_borrowing_calibration_input_sha256 = as.character(
             learned$params$event_grammar$bounded_borrowing_contract$calibration_input_sha256
           ),
           bounded_borrowing_validation_train_n = nrow(borrowing_runtime),
           bounded_borrowing_applied_train_n = sum(
             borrowing_runtime$borrowing_applied
           ),
           stringsAsFactors = FALSE),
         provenance
       ))
}
