# Calibration-frozen, bounded cross-train borrowing
#
# Borrowing is deliberately restricted to Burst bridge/boundary proposals.  It
# never changes the seed/core band or the committed per-train bridge threshold,
# and it never turns an isolated ISI into Burst.  A proposal is adjudicated at
# candidate level against size, native-support and mode-intrusion guards.

stpd_calibration_train_isi_summary <- function(
    trains, min_isi_sec = 0.001, anchor_probability = 0.20,
    burst_upper_probability = 0.95) {
  if (is.null(trains) || !length(trains)) return(data.frame())
  rows <- lapply(sort(names(trains), method = "radix"), function(train) {
    dat <- trains[[train]]
    isi_all <- suppressWarnings(as.numeric(dat$ISI_sec %||% numeric()))
    manual <- as.character(dat$pattern_manual %||% rep("", nrow(dat)))
    manual[is.na(manual)] <- ""
    if (length(manual) != length(isi_all)) {
      manual <- rep_len(manual, length(isi_all))
    }
    valid <- is.finite(isi_all) & isi_all >= min_isi_sec
    isi <- isi_all[valid]
    if (!length(isi)) return(NULL)
    burst_label <- manual %in% c("burst", "long_burst", "possible_burst")
    manual_burst_isi <- isi_all[valid & burst_label]
    burst_quantile <- function(probability) {
      if (!length(manual_burst_isi)) return(NA_real_)
      as.numeric(stats::quantile(
        manual_burst_isi, probability, names = FALSE, type = 7
      ))
    }
    input_payload <- list(
      train = train,
      isi_sec = as.list(isi),
      pattern_manual = as.list(manual)
    )
    input_sha256 <- digest::digest(
      jsonlite::toJSON(input_payload, auto_unbox = TRUE, null = "null",
                       digits = 17),
      algo = "sha256", serialize = FALSE
    )
    data.frame(
      train = train,
      n_valid_isi = length(isi),
      anchor_sec = as.numeric(stats::quantile(
        isi, anchor_probability, names = FALSE, type = 7
      )),
      median_sec = stats::median(isi),
      manual_burst_isi_n = length(manual_burst_isi),
      manual_burst_q50_sec = burst_quantile(0.50),
      manual_burst_q90_sec = burst_quantile(0.90),
      manual_burst_upper_sec = burst_quantile(burst_upper_probability),
      manual_burst_max_sec = if (length(manual_burst_isi)) {
        max(manual_burst_isi)
      } else NA_real_,
      calibration_train_input_sha256 = input_sha256,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

stpd_freeze_bounded_borrowing_contract <- function(
    params, calibration_trains, min_isi_sec = 0.001,
    anchor_probability = 0.20, min_calibration_trains = 2L,
    min_target_isi = 30L, max_expansion_ratio = 1.25,
    min_burst_evidence_per_train = 3L,
    min_burst_evidence_trains = 2L,
    burst_upper_probability = 0.95,
    consensus_margin = 0.02) {
  if (is.null(params$event_grammar)) params$event_grammar <- list()
  summary <- stpd_calibration_train_isi_summary(
    calibration_trains, min_isi_sec, anchor_probability,
    burst_upper_probability
  )
  max_expansion_ratio <- suppressWarnings(as.numeric(max_expansion_ratio))
  if (!is.finite(max_expansion_ratio) || max_expansion_ratio < 1) {
    max_expansion_ratio <- 1.25
  }
  min_calibration_trains <- max(2L, as.integer(min_calibration_trains))
  min_target_isi <- max(8L, as.integer(min_target_isi))
  min_burst_evidence_per_train <- max(
    2L, as.integer(min_burst_evidence_per_train)
  )
  min_burst_evidence_trains <- max(
    2L, as.integer(min_burst_evidence_trains)
  )
  consensus_margin <- suppressWarnings(as.numeric(consensus_margin))
  if (!is.finite(consensus_margin) || consensus_margin < 0 ||
      consensus_margin > 0.10) {
    consensus_margin <- 0.02
  }
  reference_anchor <- if (nrow(summary)) {
    stats::median(summary$anchor_sec[is.finite(summary$anchor_sec)])
  } else NA_real_
  threshold_bridge <- suppressWarnings(as.numeric(
    (params$event_grammar$effective_bands$burst %||% list())$bridge_upper_sec
  ))
  evidence_rows <- if (nrow(summary)) {
    summary$manual_burst_isi_n >= min_burst_evidence_per_train &
      is.finite(summary$manual_burst_upper_sec) &
      summary$manual_burst_upper_sec > 0
  } else logical()
  evidence_train_n <- sum(evidence_rows)
  # Each calibration train contributes one upper quantile, so a densely
  # annotated train cannot dominate the borrowed boundary.  The median is a
  # cross-train consensus, not a pooled-tail maximum.
  consensus_bridge <- if (evidence_train_n >= min_burst_evidence_trains) {
    stats::median(summary$manual_burst_upper_sec[evidence_rows]) *
      (1 + consensus_margin)
  } else NA_real_
  calibration_payload <- list(
    schema = "stpd_calibration_frozen_borrowing_v2",
    trains = if (nrow(summary)) as.list(summary$train) else list(),
    n_valid_isi = if (nrow(summary)) as.list(summary$n_valid_isi) else list(),
    anchor_sec = if (nrow(summary)) as.list(summary$anchor_sec) else list(),
    median_sec = if (nrow(summary)) as.list(summary$median_sec) else list(),
    manual_burst_isi_n = if (nrow(summary)) {
      as.list(summary$manual_burst_isi_n)
    } else list(),
    manual_burst_upper_sec = if (nrow(summary)) {
      as.list(summary$manual_burst_upper_sec)
    } else list(),
    calibration_train_input_sha256 = if (nrow(summary)) {
      as.list(summary$calibration_train_input_sha256)
    } else list(),
    base_bridge_upper_sec = if (length(threshold_bridge) &&
      is.finite(threshold_bridge[[1L]])) threshold_bridge[[1L]] else NULL,
    anchor_probability = anchor_probability,
    burst_upper_probability = burst_upper_probability,
    consensus_margin = consensus_margin,
    min_isi_sec = min_isi_sec
  )
  input_hash <- digest::digest(
    jsonlite::toJSON(calibration_payload, auto_unbox = TRUE, null = "null",
                     digits = 17, dataframe = "rows"),
    algo = "sha256", serialize = FALSE
  )
  enabled <- nrow(summary) >= min_calibration_trains &&
    evidence_train_n >= min_burst_evidence_trains &&
    is.finite(consensus_bridge) && consensus_bridge > 0 &&
    length(threshold_bridge) && is.finite(threshold_bridge[[1L]]) &&
    threshold_bridge[[1L]] > 0
  params$event_grammar$bounded_borrowing_contract <- list(
    schema_version = "stpd_calibration_frozen_borrowing_v2",
    enabled = isTRUE(enabled),
    status = if (enabled) "calibration_frozen" else
      "disabled_insufficient_calibration_evidence",
    calibration_only = TRUE,
    validation_labels_read = FALSE,
    permitted_role = "burst_bridge_or_boundary_extension_only",
    seed_band_mutation_allowed = FALSE,
    calibration_trains = if (nrow(summary)) summary$train else character(),
    calibration_train_n = nrow(summary),
    calibration_summary = summary,
    calibration_input_sha256 = input_hash,
    anchor_probability = anchor_probability,
    reference_anchor_sec = reference_anchor,
    manual_burst_evidence_train_n = evidence_train_n,
    min_burst_evidence_per_train = min_burst_evidence_per_train,
    min_burst_evidence_trains = min_burst_evidence_trains,
    burst_upper_probability = burst_upper_probability,
    consensus_margin = consensus_margin,
    consensus_bridge_upper_sec = consensus_bridge,
    base_bridge_upper_sec = if (length(threshold_bridge))
      threshold_bridge[[1L]] else NA_real_,
    min_target_valid_isi = min_target_isi,
    max_expansion_ratio = max_expansion_ratio,
    canonical_pause_precedence = TRUE,
    frozen_before_validation = TRUE
  )
  params
}

stpd_apply_bounded_borrowing_to_event_vp <- function(
    vp, dat, params, train = "", min_isi_sec = 0.001) {
  contract <- (params$event_grammar %||% list())$bounded_borrowing_contract
  vp$cross_train_borrowing_applied <- FALSE
  vp$cross_train_borrowing_status <- "not_configured"
  vp$cross_train_borrowing_seed_unchanged <- TRUE
  vp$cross_train_borrowing_bridge_original_sec <- vp$bridge_high
  vp$cross_train_borrowing_bridge_effective_sec <- vp$bridge_high
  vp$cross_train_borrowing_bridge_candidate_sec <- vp$bridge_high
  if (is.null(contract) || !isTRUE(contract$enabled)) {
    if (!is.null(contract)) vp$cross_train_borrowing_status <- contract$status
    return(vp)
  }

  # Explicit per-train hard thresholds are user constraints, not soft evidence.
  hard_range <- get_train_burst_range(params$burst %||% list(), train = train)
  if (!is.null(hard_range) && isTRUE(stpd_train_isi_threshold_is_hard(hard_range))) {
    vp$cross_train_borrowing_status <- "blocked_by_explicit_train_hard_threshold"
    return(vp)
  }
  isi <- suppressWarnings(as.numeric(dat$ISI_sec %||% numeric()))
  isi <- isi[is.finite(isi) & isi >= min_isi_sec]
  min_n <- max(8L, as.integer(contract$min_target_valid_isi %||% 30L))
  if (length(isi) < min_n) {
    vp$cross_train_borrowing_status <- "insufficient_target_distribution"
    return(vp)
  }
  base_bridge <- suppressWarnings(as.numeric(contract$base_bridge_upper_sec))
  consensus_bridge <- suppressWarnings(as.numeric(
    contract$consensus_bridge_upper_sec
  ))
  max_ratio <- suppressWarnings(as.numeric(contract$max_expansion_ratio))
  if (!all(is.finite(c(consensus_bridge, base_bridge, max_ratio))) ||
      consensus_bridge <= 0 || base_bridge <= 0 || max_ratio < 1) {
    vp$cross_train_borrowing_status <- "invalid_frozen_contract"
    return(vp)
  }
  proposed_bridge <- min(consensus_bridge, base_bridge * max_ratio)
  pause_ceiling <- suppressWarnings(as.numeric(vp$pause_thr))
  if (is.finite(pause_ceiling) && pause_ceiling > 0) {
    proposed_bridge <- min(proposed_bridge, pause_ceiling * (1 - 1e-6))
  }
  original_bridge <- vp$bridge_high
  candidate_bridge <- max(original_bridge, proposed_bridge)
  vp$cross_train_borrowing_applied <- isTRUE(candidate_bridge > original_bridge)
  vp$cross_train_borrowing_status <- if (vp$cross_train_borrowing_applied) {
    "proposal_frozen_for_candidate_level_adjudication"
  } else "eligible_consensus_already_covered"
  vp$cross_train_borrowing_consensus_bridge_upper_sec <- consensus_bridge
  vp$cross_train_borrowing_applied_ratio <- candidate_bridge / original_bridge
  vp$cross_train_borrowing_bridge_original_sec <- original_bridge
  vp$cross_train_borrowing_bridge_effective_sec <- original_bridge
  vp$cross_train_borrowing_bridge_candidate_sec <- candidate_bridge
  vp$cross_train_borrowing_contract_sha256 <-
    as.character(contract$calibration_input_sha256 %||% "")
  vp
}
