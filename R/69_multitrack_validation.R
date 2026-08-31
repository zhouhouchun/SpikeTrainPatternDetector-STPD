# Pure, track-aware validation for the Phase 2A automatic Preview and the
# Phase 2B adjudicated final product.  This module deliberately has no detector,
# review-transition, UI, or export side effects.

stpd_multitrack_validation_schema_version <- function() {
  "stpd_multitrack_validation_v1"
}

stpd_multitrack_validation_abort <- function(code, message, source_code = "") {
  condition <- structure(
    list(
      message = as.character(message)[1],
      call = NULL,
      code = as.character(code)[1],
      source_code = as.character(source_code %||% "")[1]
    ),
    class = c("stpd_multitrack_validation_error", "error", "condition")
  )
  stop(condition)
}

stpd_multitrack_validation_scalar <- function(x, name) {
  if (length(x) != 1L || is.na(x) || !is.character(x) || !nzchar(x)) {
    stpd_multitrack_validation_abort(
      paste0(name, "_missing"),
      paste0("A single non-empty explicit ", name, " is required.")
    )
  }
  x
}

stpd_multitrack_validation_primary_tracks <- function() {
  c("event", "state", "gap")
}

stpd_multitrack_validation_ontology <- function() {
  ontology <- stpd_multitrack_shadow_track_ontology()
  ontology[stpd_multitrack_validation_primary_tracks()]
}

stpd_multitrack_validation_empty_predictions <- function() {
  data.frame(
    prediction_interval_id = character(),
    train = character(),
    semantic_track = character(),
    label = character(),
    start_isi = integer(),
    end_isi = integer(),
    n_isi = integer(),
    prediction_source = character(),
    prediction_origin = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_validation_empty_review <- function() {
  data.frame(
    review_interval_id = character(),
    train = character(),
    label = character(),
    start_isi = integer(),
    end_isi = integer(),
    n_isi = integer(),
    target_track = character(),
    target_label = character(),
    active_in_automatic_preview = logical(),
    review_status = character(),
    prediction_source = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_validation_empty_matches <- function() {
  data.frame(
    match_id = character(),
    train = character(),
    semantic_track = character(),
    label = character(),
    prediction_interval_id = character(),
    truth_interval_id = character(),
    prediction_start_isi = integer(),
    prediction_end_isi = integer(),
    truth_start_isi = integer(),
    truth_end_isi = integer(),
    intersection_isi_n = integer(),
    union_isi_n = integer(),
    iou = double(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_validation_empty_metrics_by_label <- function() {
  data.frame(
    semantic_track = character(),
    label = character(),
    predicted_n = integer(),
    truth_n = integer(),
    matched_n = integer(),
    false_positive_n = integer(),
    false_negative_n = integer(),
    precision = double(),
    recall = double(),
    f1 = double(),
    mean_iou = double(),
    median_iou = double(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_validation_empty_metrics_by_track <- function() {
  data.frame(
    semantic_track = character(),
    predicted_n = integer(),
    truth_n = integer(),
    matched_n = integer(),
    false_positive_n = integer(),
    false_negative_n = integer(),
    precision = double(),
    recall = double(),
    f1 = double(),
    mean_iou = double(),
    median_iou = double(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_validation_empty_coverage <- function() {
  data.frame(
    semantic_track = character(),
    label = character(),
    predicted_isi_n = integer(),
    truth_isi_n = integer(),
    overlap_isi_n = integer(),
    union_isi_n = integer(),
    isi_precision = double(),
    isi_recall = double(),
    isi_f1 = double(),
    isi_iou = double(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_validation_empty_review_summary <- function() {
  data.frame(
    target_track = character(),
    target_label = character(),
    review_candidate_n = integer(),
    active_candidate_n = integer(),
    inactive_candidate_n = integer(),
    pending_candidate_n = integer(),
    confirmed_candidate_n = integer(),
    revoked_candidate_n = integer(),
    truth_target_n = integer(),
    truth_target_reached_by_any_review_n = integer(),
    truth_target_reached_by_active_review_n = integer(),
    truth_target_reached_by_pending_review_n = integer(),
    truth_target_at_iou_threshold_by_pending_n = integer(),
    mean_truth_best_pending_iou = double(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_validation_truth <- function(
    truth_intervals, ds, selected_trains) {
  required <- c(
    "truth_interval_id", "train", "semantic_track", "label",
    "start_isi", "end_isi"
  )
  required_types <- c(
    "character", "character", "character", "character", "integer", "integer"
  )
  if (!is.data.frame(truth_intervals) ||
      !identical(names(truth_intervals), required) ||
      !identical(unname(vapply(truth_intervals, typeof, character(1))),
                 required_types)) {
    stpd_multitrack_validation_abort(
      "truth_schema_invalid",
      paste(
        "truth_intervals must be a data.frame with the exact ordered typed",
        "schema: truth_interval_id/train/semantic_track/label as character",
        "and start_isi/end_isi as integer."
      )
    )
  }
  truth <- truth_intervals
  character_fields <- required[seq_len(4L)]
  bad_character <- vapply(character_fields, function(field) {
    anyNA(truth[[field]]) || any(!nzchar(truth[[field]]))
  }, logical(1))
  if (any(bad_character) || anyDuplicated(truth$truth_interval_id)) {
    stpd_multitrack_validation_abort(
      "truth_identity_invalid",
      "Truth IDs, trains, tracks, and labels must be non-empty; truth IDs must be unique."
    )
  }
  tracks <- stpd_multitrack_validation_primary_tracks()
  if (any(!truth$semantic_track %in% tracks)) {
    stpd_multitrack_validation_abort(
      "truth_track_invalid",
      "Primary truth may contain only Event, State, and Gap track intervals."
    )
  }
  ontology <- stpd_multitrack_validation_ontology()
  ontology_ok <- vapply(seq_len(nrow(truth)), function(i) {
    truth$label[i] %in% ontology[[truth$semantic_track[i]]]
  }, logical(1))
  if (any(!ontology_ok)) {
    stpd_multitrack_validation_abort(
      "truth_label_track_mismatch",
      "At least one truth label does not belong to its declared semantic track."
    )
  }
  if (!is.list(ds) || !is.list(ds$trains)) {
    stpd_multitrack_validation_abort(
      "parent_dataset_invalid", "The validation parent has no train collection."
    )
  }
  if (any(!truth$train %in% names(ds$trains))) {
    stpd_multitrack_validation_abort(
      "truth_parent_train_missing",
      "At least one truth interval names a train absent from the parent dataset."
    )
  }
  if (any(!truth$train %in% selected_trains)) {
    stpd_multitrack_validation_abort(
      "truth_outside_selected_scope",
      "Truth contains a train outside the explicit validation train scope."
    )
  }
  geometry_ok <- vapply(seq_len(nrow(truth)), function(i) {
    dat <- ds$trains[[truth$train[i]]]
    is.data.frame(dat) && truth$start_isi[i] >= 2L &&
      truth$end_isi[i] >= truth$start_isi[i] &&
      truth$end_isi[i] <= nrow(dat)
  }, logical(1))
  if (anyNA(truth$start_isi) || anyNA(truth$end_isi) || any(!geometry_ok)) {
    stpd_multitrack_validation_abort(
      "truth_parent_geometry_invalid",
      paste(
        "Truth spans must use parent ISI-row coordinates: start_isi >= 2,",
        "end_isi >= start_isi, and end_isi within the named train."
      )
    )
  }
  if (nrow(truth) > 1L) {
    groups <- split(
      seq_len(nrow(truth)),
      paste(truth$train, truth$semantic_track, sep = "\u001f")
    )
    for (indices in groups) {
      part <- truth[indices, , drop = FALSE]
      part <- part[order(
        part$start_isi, part$end_isi, part$truth_interval_id,
        method = "radix"
      ), , drop = FALSE]
      if (nrow(part) > 1L &&
          any(part$start_isi[-1L] <= part$end_isi[-nrow(part)])) {
        stpd_multitrack_validation_abort(
          "truth_within_track_overlap",
          paste0(
            "Truth intervals overlap within train '", part$train[1],
            "' and track '", part$semantic_track[1], "'."
          )
        )
      }
    }
  }
  truth <- truth[order(
    truth$train,
    match(truth$semantic_track, tracks),
    truth$label, truth$start_isi, truth$end_isi, truth$truth_interval_id,
    method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rownames(truth) <- NULL
  truth
}

stpd_multitrack_validation_standardize_predictions <- function(
    intervals, prediction_source, id_field, origin) {
  if (nrow(intervals) == 0L) {
    return(stpd_multitrack_validation_empty_predictions())
  }
  out <- data.frame(
    prediction_interval_id = as.character(intervals[[id_field]]),
    train = as.character(intervals$train),
    semantic_track = as.character(intervals$semantic_track),
    label = as.character(intervals$label),
    start_isi = as.integer(intervals$start_isi),
    end_isi = as.integer(intervals$end_isi),
    n_isi = as.integer(intervals$end_isi - intervals$start_isi + 1L),
    prediction_source = rep(prediction_source, nrow(intervals)),
    prediction_origin = as.character(origin),
    stringsAsFactors = FALSE
  )
  out <- out[order(
    out$train,
    match(out$semantic_track, stpd_multitrack_validation_primary_tracks()),
    out$label, out$start_isi, out$end_isi, out$prediction_interval_id,
    method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_multitrack_validation_standardize_review <- function(
    intervals, prediction_source, id_field, target_track, target_label,
    active_in_automatic_preview, review_status) {
  if (nrow(intervals) == 0L) return(stpd_multitrack_validation_empty_review())
  out <- data.frame(
    review_interval_id = as.character(intervals[[id_field]]),
    train = as.character(intervals$train),
    label = as.character(intervals$label),
    start_isi = as.integer(intervals$start_isi),
    end_isi = as.integer(intervals$end_isi),
    n_isi = as.integer(intervals$end_isi - intervals$start_isi + 1L),
    target_track = as.character(target_track),
    target_label = as.character(target_label),
    active_in_automatic_preview = as.logical(active_in_automatic_preview),
    review_status = as.character(review_status),
    prediction_source = rep(prediction_source, nrow(intervals)),
    stringsAsFactors = FALSE
  )
  out <- out[order(
    out$train, out$start_isi, out$end_isi, out$review_interval_id,
    method = "radix"
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_multitrack_validation_extract_automatic <- function(ds) {
  preview <- if (is.list(ds) && is.list(ds$results)) {
    ds$results$multitrack_preview %||% NULL
  } else NULL
  if (is.null(preview)) {
    stpd_multitrack_validation_abort(
      "automatic_preview_missing",
      "prediction_source='automatic_preview' requires a persisted Phase 2A Preview."
    )
  }
  validated <- tryCatch(
    stpd_multitrack_preview_validate_for_export(preview, parent = ds),
    error = function(e) e
  )
  if (inherits(validated, "error")) {
    stpd_multitrack_validation_abort(
      "automatic_preview_invalid",
      paste0("The automatic Preview failed strict validation: ",
             conditionMessage(validated)),
      source_code = validated$code %||% ""
    )
  }
  metadata <- preview$metadata
  if (!identical(as.character(metadata$materialization_status[1]), "materialized")) {
    stpd_multitrack_validation_abort(
      "automatic_preview_not_materialized",
      "Automatic validation requires a successfully materialized Preview."
    )
  }
  if (!identical(as.logical(metadata$label_blind[1]), TRUE) ||
      !identical(as.character(metadata$detection_mode[1]), "label_blind")) {
    stpd_multitrack_validation_abort(
      "automatic_preview_not_label_blind",
      paste(
        "Detector-performance validation requires a label-blind automatic",
        "Preview; manual-aware automatic output is not an eligible estimand."
      )
    )
  }
  intervals <- preview$intervals
  active <- intervals$active_in_preview %in% TRUE
  primary_rows <- intervals[
    active & intervals$semantic_track %in%
      stpd_multitrack_validation_primary_tracks(),
    , drop = FALSE
  ]
  primary <- stpd_multitrack_validation_standardize_predictions(
    primary_rows, "automatic_preview", "interval_id",
    rep("automatic_preview", nrow(primary_rows))
  )
  review_rows <- intervals[intervals$semantic_track == "review", , drop = FALSE]
  review_status <- ifelse(
    review_rows$active_in_preview %in% TRUE,
    "pending_review", "inactive_review"
  )
  review <- stpd_multitrack_validation_standardize_review(
    review_rows, "automatic_preview", "interval_id",
    review_rows$review_target_track, review_rows$review_target_label,
    review_rows$active_in_preview, review_status
  )
  artifact_scope <- stpd_multitrack_preview_selected_train_values(
    metadata$selected_trains[1]
  )
  list(
    primary = primary,
    review = review,
    run_id = as.character(metadata$run_id[1]),
    params_hash = as.character(metadata$params_hash[1]),
    source_schema_version = as.character(metadata$schema_version[1]),
    artifact_sha256 = stpd_multitrack_preview_table_hash(
      preview$table_manifest
    ),
    artifact_scope = artifact_scope,
    label_blind = TRUE,
    authoritative = FALSE
  )
}

stpd_multitrack_validation_extract_final <- function(ds) {
  raw <- if (is.list(ds) && is.list(ds$results)) {
    ds$results[[stpd_multitrack_review_result_key()]] %||% NULL
  } else NULL
  if (is.null(raw)) {
    stpd_multitrack_validation_abort(
      "phase2b_final_missing",
      "prediction_source='phase2b_final' requires a persisted Phase 2B product."
    )
  }
  raw_metadata <- raw$metadata %||% data.frame()
  lifecycle <- if (is.data.frame(raw_metadata) && nrow(raw_metadata) == 1L &&
                   "lifecycle_status" %in% names(raw_metadata)) {
    as.character(raw_metadata$lifecycle_status[1])
  } else ""
  authoritative <- if (is.data.frame(raw_metadata) && nrow(raw_metadata) == 1L &&
                       "authoritative" %in% names(raw_metadata)) {
    as.logical(raw_metadata$authoritative[1])
  } else FALSE
  if (!identical(lifecycle, "active") || !identical(authoritative, TRUE)) {
    stpd_multitrack_validation_abort(
      "phase2b_final_not_active",
      "Final agreement requires a current, active, authoritative Phase 2B product."
    )
  }
  validated <- tryCatch(
    stpd_multitrack_review_validate_for_export(ds),
    error = function(e) e
  )
  if (inherits(validated, "error")) {
    stpd_multitrack_validation_abort(
      "phase2b_final_invalid",
      paste0("The Phase 2B product failed strict validation: ",
             conditionMessage(validated)),
      source_code = validated$code %||% ""
    )
  }
  product <- validated$product
  metadata <- product$metadata
  intervals <- product$final_intervals
  primary_rows <- intervals[
    intervals$semantic_track %in% stpd_multitrack_validation_primary_tracks(),
    , drop = FALSE
  ]
  primary <- stpd_multitrack_validation_standardize_predictions(
    primary_rows, "phase2b_final", "final_interval_id",
    primary_rows$final_source
  )
  preview <- ds$results$multitrack_preview
  review_rows <- preview$intervals[
    preview$intervals$semantic_track == "review", , drop = FALSE
  ]
  target_track <- as.character(review_rows$review_target_track)
  target_label <- as.character(review_rows$review_target_label)
  status <- ifelse(
    review_rows$active_in_preview %in% TRUE,
    "pending_review", "inactive_review"
  )
  if (nrow(review_rows) > 0L) {
    decisions <- product$current_decisions
    if (nrow(decisions) > 0L) {
      review_key <- paste(
        review_rows$train, review_rows$interval_id, sep = "\u001f"
      )
      decision_key <- paste(
        decisions$train, decisions$source_review_interval_id, sep = "\u001f"
      )
      decision_hit <- match(
        review_key, decision_key
      )
      resolved <- which(!is.na(decision_hit))
      if (length(resolved) > 0L) {
        status[resolved] <- as.character(
          decisions$current_status[decision_hit[resolved]]
        )
      }
    }
  }
  review <- stpd_multitrack_validation_standardize_review(
    review_rows, "phase2b_final", "interval_id",
    target_track, target_label, review_rows$active_in_preview, status
  )
  artifact_scope <- stpd_multitrack_preview_selected_train_values(
    preview$metadata$selected_trains[1]
  )
  list(
    primary = primary,
    review = review,
    run_id = as.character(metadata$run_id[1]),
    params_hash = as.character(metadata$params_hash[1]),
    source_schema_version = as.character(metadata$schema_version[1]),
    artifact_sha256 = as.character(metadata$product_sha256[1]),
    artifact_scope = artifact_scope,
    label_blind = as.logical(preview$metadata$label_blind[1]),
    authoritative = TRUE
  )
}

stpd_multitrack_validation_extract_v2 <- function(ds, reviewed = FALSE) {
  source_name <- if (isTRUE(reviewed)) "reviewed_v2" else "automatic_v2"
  product <- tryCatch(
    if (isTRUE(reviewed)) stpd_multitrack_final(ds) else stpd_multitrack_auto(ds),
    error = function(e) e
  )
  if (inherits(product, "error")) {
    stpd_multitrack_validation_abort(
      paste0(source_name, "_invalid"), conditionMessage(product)
    )
  }
  auto <- stpd_multitrack_auto(ds)
  if (!isTRUE(reviewed) &&
      (!isTRUE(auto$metadata$label_blind_execution) ||
       !identical(auto$metadata$detection_mode, "label_blind"))) {
    stpd_multitrack_validation_abort(
      "automatic_v2_not_label_blind",
      paste(
        "Exploratory automatic comparison requires a label-blind",
        "Gate B AUTO candidate record."
      )
    )
  }
  interval_rows <- list()
  if (nrow(product$events) > 0L) interval_rows[[length(interval_rows) + 1L]] <- data.frame(
    interval_id = product$events$event_id, train = product$events$train,
    semantic_track = "event", label = product$events$event_family,
    start_isi = product$events$start_isi, end_isi = product$events$end_isi,
    origin = paste0(
      "extent=", product$events$extent_class,
      "|frequency=", product$events$frequency_class
    ), stringsAsFactors = FALSE
  )
  if (nrow(product$states) > 0L) interval_rows[[length(interval_rows) + 1L]] <- data.frame(
    interval_id = product$states$state_id, train = product$states$train,
    semantic_track = "state", label = product$states$state_class,
    start_isi = product$states$start_isi, end_isi = product$states$end_isi,
    origin = product$states$candidate_source, stringsAsFactors = FALSE
  )
  if (nrow(product$gaps) > 0L) interval_rows[[length(interval_rows) + 1L]] <- data.frame(
    interval_id = product$gaps$gap_id, train = product$gaps$train,
    semantic_track = "gap", label = product$gaps$gap_class,
    start_isi = product$gaps$start_isi, end_isi = product$gaps$end_isi,
    origin = product$gaps$candidate_source, stringsAsFactors = FALSE
  )
  intervals <- if (length(interval_rows) == 0L) data.frame() else {
    dplyr::bind_rows(interval_rows)
  }
  primary <- stpd_multitrack_validation_standardize_predictions(
    intervals, source_name, "interval_id",
    if (nrow(intervals) == 0L) character() else intervals$origin
  )
  reviews <- product$review_candidates
  review_input <- if (nrow(reviews) == 0L) data.frame() else data.frame(
    review_candidate_id = reviews$review_candidate_id,
    train = reviews$train, label = reviews$review_label,
    start_isi = reviews$start_isi, end_isi = reviews$end_isi,
    stringsAsFactors = FALSE
  )
  review_status <- rep("pending_review", nrow(reviews))
  active <- rep(TRUE, nrow(reviews))
  if (isTRUE(reviewed) && nrow(reviews) > 0L &&
      nrow(product$current_decisions) > 0L) {
    hit <- match(
      paste(reviews$train, reviews$review_candidate_id, sep = "\u001f"),
      paste(product$current_decisions$train,
            product$current_decisions$review_candidate_id, sep = "\u001f")
    )
    resolved <- !is.na(hit)
    review_status[resolved] <- product$current_decisions$current_status[hit[resolved]]
  }
  review <- stpd_multitrack_validation_standardize_review(
    review_input, source_name, "review_candidate_id",
    reviews$target_track, reviews$target_label, active, review_status
  )
  list(
    primary = primary, review = review,
    run_id = as.character(product$metadata$run_id),
    params_hash = as.character(product$metadata$params_hash),
    source_schema_version = as.character(product$metadata$schema_version),
    artifact_sha256 = as.character(product$metadata$product_sha256),
    artifact_scope = stpd_multitrack_preview_selected_train_values(
      auto$metadata$selected_trains
    ),
    label_blind = isTRUE(auto$metadata$label_blind_execution),
    authoritative = FALSE
  )
}

stpd_multitrack_validation_iou_payload <- function(ps, pe, ts, te) {
  intersection <- max(0L, min(pe, te) - max(ps, ts) + 1L)
  union <- (pe - ps + 1L) + (te - ts + 1L) - intersection
  list(
    intersection = as.integer(intersection),
    union = as.integer(union),
    iou = as.numeric(if (union > 0L) intersection / union else 0)
  )
}

stpd_multitrack_validation_dp_better <- function(a, b, predictions, truth) {
  if (a$count != b$count) return(if (a$count > b$count) a else b)
  tolerance <- .Machine$double.eps * 64 *
    max(1, abs(a$total_iou), abs(b$total_iou))
  if (abs(a$total_iou - b$total_iou) > tolerance) {
    return(if (a$total_iou > b$total_iou) a else b)
  }
  signature <- function(x) {
    if (nrow(x$pairs) == 0L) return("")
    paste(
      paste(
        predictions$prediction_interval_id[x$pairs[, 1]],
        truth$truth_interval_id[x$pairs[, 2]], sep = "\u001f"
      ),
      collapse = "\u001e"
    )
  }
  sa <- signature(a)
  sb <- signature(b)
  if (identical(sa, sb)) return(a)
  first <- sort(c(sa, sb), method = "radix")[1]
  if (identical(sa, first)) a else b
}

stpd_multitrack_validation_ordered_dp <- function(
    predictions, truth, iou_threshold) {
  m <- nrow(predictions)
  n <- nrow(truth)
  if (m == 0L || n == 0L) return(matrix(integer(), ncol = 2L))
  empty <- list(
    count = 0L, total_iou = 0,
    pairs = matrix(integer(), ncol = 2L)
  )
  dp <- matrix(vector("list", (m + 1L) * (n + 1L)),
               nrow = m + 1L, ncol = n + 1L)
  for (i in seq_len(m + 1L)) dp[[i, n + 1L]] <- empty
  for (j in seq_len(n + 1L)) dp[[m + 1L, j]] <- empty
  for (i in seq.int(m, 1L)) {
    for (j in seq.int(n, 1L)) {
      best <- stpd_multitrack_validation_dp_better(
        dp[[i + 1L, j]], dp[[i, j + 1L]], predictions, truth
      )
      payload <- stpd_multitrack_validation_iou_payload(
        predictions$start_isi[i], predictions$end_isi[i],
        truth$start_isi[j], truth$end_isi[j]
      )
      if (payload$iou + 1e-15 >= iou_threshold) {
        downstream <- dp[[i + 1L, j + 1L]]
        candidate <- list(
          count = downstream$count + 1L,
          total_iou = downstream$total_iou + payload$iou,
          pairs = rbind(c(i, j), downstream$pairs)
        )
        best <- stpd_multitrack_validation_dp_better(
          best, candidate, predictions, truth
        )
      }
      dp[[i, j]] <- best
    }
  }
  dp[[1L, 1L]]$pairs
}

stpd_multitrack_validation_match <- function(
    predictions, truth, iou_threshold) {
  if (nrow(predictions) == 0L || nrow(truth) == 0L) {
    return(stpd_multitrack_validation_empty_matches())
  }
  groups <- unique(rbind(
    predictions[c("train", "semantic_track", "label")],
    truth[c("train", "semantic_track", "label")]
  ))
  groups <- groups[order(
    groups$train,
    match(groups$semantic_track, stpd_multitrack_validation_primary_tracks()),
    groups$label, method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rows <- list()
  k <- 0L
  for (g in seq_len(nrow(groups))) {
    key <- groups[g, , drop = FALSE]
    p <- predictions[
      predictions$train == key$train &
        predictions$semantic_track == key$semantic_track &
        predictions$label == key$label,
      , drop = FALSE
    ]
    t <- truth[
      truth$train == key$train &
        truth$semantic_track == key$semantic_track &
        truth$label == key$label,
      , drop = FALSE
    ]
    p <- p[order(
      p$start_isi, p$end_isi, p$prediction_interval_id, method = "radix"
    ), , drop = FALSE]
    t <- t[order(
      t$start_isi, t$end_isi, t$truth_interval_id, method = "radix"
    ), , drop = FALSE]
    pairs <- stpd_multitrack_validation_ordered_dp(p, t, iou_threshold)
    if (nrow(pairs) == 0L) next
    for (i in seq_len(nrow(pairs))) {
      pred <- p[pairs[i, 1], , drop = FALSE]
      ref <- t[pairs[i, 2], , drop = FALSE]
      payload <- stpd_multitrack_validation_iou_payload(
        pred$start_isi, pred$end_isi, ref$start_isi, ref$end_isi
      )
      match_payload <- paste(
        pred$prediction_interval_id, ref$truth_interval_id,
        format(payload$iou, digits = 17, scientific = FALSE), sep = "\u001f"
      )
      k <- k + 1L
      rows[[k]] <- data.frame(
        match_id = paste0(
          "mtvm_", digest::digest(match_payload, algo = "sha256", serialize = FALSE)
        ),
        train = pred$train,
        semantic_track = pred$semantic_track,
        label = pred$label,
        prediction_interval_id = pred$prediction_interval_id,
        truth_interval_id = ref$truth_interval_id,
        prediction_start_isi = pred$start_isi,
        prediction_end_isi = pred$end_isi,
        truth_start_isi = ref$start_isi,
        truth_end_isi = ref$end_isi,
        intersection_isi_n = payload$intersection,
        union_isi_n = payload$union,
        iou = payload$iou,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) return(stpd_multitrack_validation_empty_matches())
  out <- dplyr::bind_rows(stpd_multitrack_validation_empty_matches(), rows)
  out <- out[order(
    out$train,
    match(out$semantic_track, stpd_multitrack_validation_primary_tracks()),
    out$label, out$prediction_start_isi, out$truth_start_isi,
    out$prediction_interval_id, out$truth_interval_id,
    method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_multitrack_validation_rates <- function(tp, predicted_n, truth_n) {
  fp <- predicted_n - tp
  fn <- truth_n - tp
  precision <- if (predicted_n > 0L) tp / predicted_n else NA_real_
  recall <- if (truth_n > 0L) tp / truth_n else NA_real_
  denominator <- 2L * tp + fp + fn
  f1 <- if (denominator > 0L) 2 * tp / denominator else NA_real_
  list(fp = as.integer(fp), fn = as.integer(fn), precision = precision,
       recall = recall, f1 = f1)
}

stpd_multitrack_validation_metrics_by_label <- function(
    predictions, truth, matches) {
  groups <- unique(rbind(
    predictions[c("semantic_track", "label")],
    truth[c("semantic_track", "label")]
  ))
  if (nrow(groups) == 0L) {
    return(stpd_multitrack_validation_empty_metrics_by_label())
  }
  groups <- groups[order(
    match(groups$semantic_track, stpd_multitrack_validation_primary_tracks()),
    groups$label, method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rows <- lapply(seq_len(nrow(groups)), function(i) {
    track <- groups$semantic_track[i]
    label <- groups$label[i]
    predicted_n <- sum(
      predictions$semantic_track == track & predictions$label == label
    )
    truth_n <- sum(truth$semantic_track == track & truth$label == label)
    hit <- matches$semantic_track == track & matches$label == label
    matched_n <- sum(hit)
    rates <- stpd_multitrack_validation_rates(
      matched_n, predicted_n, truth_n
    )
    values <- matches$iou[hit]
    data.frame(
      semantic_track = track,
      label = label,
      predicted_n = as.integer(predicted_n),
      truth_n = as.integer(truth_n),
      matched_n = as.integer(matched_n),
      false_positive_n = rates$fp,
      false_negative_n = rates$fn,
      precision = rates$precision,
      recall = rates$recall,
      f1 = rates$f1,
      mean_iou = if (length(values) > 0L) mean(values) else NA_real_,
      median_iou = if (length(values) > 0L) stats::median(values) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(stpd_multitrack_validation_empty_metrics_by_label(), rows)
}

stpd_multitrack_validation_metrics_by_track <- function(
    predictions, truth, matches) {
  rows <- lapply(stpd_multitrack_validation_primary_tracks(), function(track) {
    predicted_n <- sum(predictions$semantic_track == track)
    truth_n <- sum(truth$semantic_track == track)
    hit <- matches$semantic_track == track
    matched_n <- sum(hit)
    rates <- stpd_multitrack_validation_rates(
      matched_n, predicted_n, truth_n
    )
    values <- matches$iou[hit]
    data.frame(
      semantic_track = track,
      predicted_n = as.integer(predicted_n),
      truth_n = as.integer(truth_n),
      matched_n = as.integer(matched_n),
      false_positive_n = rates$fp,
      false_negative_n = rates$fn,
      precision = rates$precision,
      recall = rates$recall,
      f1 = rates$f1,
      mean_iou = if (length(values) > 0L) mean(values) else NA_real_,
      median_iou = if (length(values) > 0L) stats::median(values) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(stpd_multitrack_validation_empty_metrics_by_track(), rows)
}

stpd_multitrack_validation_coverage <- function(predictions, truth) {
  groups <- unique(rbind(
    predictions[c("semantic_track", "label")],
    truth[c("semantic_track", "label")]
  ))
  if (nrow(groups) == 0L) return(stpd_multitrack_validation_empty_coverage())
  groups <- groups[order(
    match(groups$semantic_track, stpd_multitrack_validation_primary_tracks()),
    groups$label, method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rows <- lapply(seq_len(nrow(groups)), function(i) {
    track <- groups$semantic_track[i]
    label <- groups$label[i]
    p <- predictions[
      predictions$semantic_track == track & predictions$label == label,
      , drop = FALSE
    ]
    t <- truth[truth$semantic_track == track & truth$label == label,
               , drop = FALSE]
    predicted_n <- sum(p$end_isi - p$start_isi + 1L)
    truth_n <- sum(t$end_isi - t$start_isi + 1L)
    overlap <- 0L
    if (nrow(p) > 0L && nrow(t) > 0L) {
      for (pi in seq_len(nrow(p))) {
        hits <- which(t$train == p$train[pi])
        if (length(hits) == 0L) next
        overlap <- overlap + sum(vapply(hits, function(ti) {
          stpd_multitrack_validation_iou_payload(
            p$start_isi[pi], p$end_isi[pi],
            t$start_isi[ti], t$end_isi[ti]
          )$intersection
        }, integer(1)))
      }
    }
    union <- predicted_n + truth_n - overlap
    denom_f1 <- predicted_n + truth_n
    data.frame(
      semantic_track = track,
      label = label,
      predicted_isi_n = as.integer(predicted_n),
      truth_isi_n = as.integer(truth_n),
      overlap_isi_n = as.integer(overlap),
      union_isi_n = as.integer(union),
      isi_precision = if (predicted_n > 0L) overlap / predicted_n else NA_real_,
      isi_recall = if (truth_n > 0L) overlap / truth_n else NA_real_,
      isi_f1 = if (denom_f1 > 0L) 2 * overlap / denom_f1 else NA_real_,
      isi_iou = if (union > 0L) overlap / union else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(stpd_multitrack_validation_empty_coverage(), rows)
}

stpd_multitrack_validation_review_summary <- function(
    review, truth, iou_threshold) {
  targets <- unique(review[c("target_track", "target_label")])
  targets <- targets[
    nzchar(targets$target_track) & nzchar(targets$target_label), , drop = FALSE
  ]
  # D-012 defines Event/Burst as the target even when every Review interval was
  # confirmed and therefore no unresolved Review row remains in the final layer.
  targets <- unique(rbind(
    data.frame(target_track = "event", target_label = "burst",
               stringsAsFactors = FALSE),
    targets
  ))
  targets <- targets[order(
    match(targets$target_track, stpd_multitrack_validation_primary_tracks()),
    targets$target_label, method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rows <- lapply(seq_len(nrow(targets)), function(i) {
    track <- targets$target_track[i]
    label <- targets$target_label[i]
    candidates <- review[
      review$target_track == track & review$target_label == label,
      , drop = FALSE
    ]
    active <- candidates[candidates$active_in_automatic_preview %in% TRUE,
                         , drop = FALSE]
    pending <- candidates[candidates$review_status == "pending_review",
                          , drop = FALSE]
    refs <- truth[truth$semantic_track == track & truth$label == label,
                  , drop = FALSE]
    best_iou <- function(candidate_rows) {
      best <- numeric(nrow(refs))
      if (nrow(refs) == 0L || nrow(candidate_rows) == 0L) return(best)
      for (ri in seq_len(nrow(refs))) {
        hits <- which(candidate_rows$train == refs$train[ri])
        if (length(hits) == 0L) next
        best[ri] <- max(vapply(hits, function(ci) {
          stpd_multitrack_validation_iou_payload(
            candidate_rows$start_isi[ci], candidate_rows$end_isi[ci],
            refs$start_isi[ri], refs$end_isi[ri]
          )$iou
        }, double(1)))
      }
      best
    }
    best_any <- best_iou(candidates)
    best_active <- best_iou(active)
    best_pending <- best_iou(pending)
    truth_n <- nrow(refs)
    threshold_hits <- sum(best_pending + 1e-15 >= iou_threshold)
    data.frame(
      target_track = track,
      target_label = label,
      review_candidate_n = as.integer(nrow(candidates)),
      active_candidate_n = as.integer(nrow(active)),
      inactive_candidate_n = as.integer(sum(
        !(candidates$active_in_automatic_preview %in% TRUE)
      )),
      pending_candidate_n = as.integer(nrow(pending)),
      confirmed_candidate_n = as.integer(sum(
        candidates$review_status == "confirmed"
      )),
      revoked_candidate_n = as.integer(sum(candidates$review_status == "revoked")),
      truth_target_n = as.integer(truth_n),
      truth_target_reached_by_any_review_n = as.integer(sum(best_any > 0)),
      truth_target_reached_by_active_review_n = as.integer(sum(best_active > 0)),
      truth_target_reached_by_pending_review_n = as.integer(sum(best_pending > 0)),
      truth_target_at_iou_threshold_by_pending_n = as.integer(threshold_hits),
      mean_truth_best_pending_iou = if (truth_n > 0L) {
        mean(best_pending)
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(stpd_multitrack_validation_empty_review_summary(), rows)
}

#' Compare validated prediction intervals with an explicit reference
#'
#' This pure scoring interface keeps Event, State, and Gap intervals separate.
#' Before Gate C, automatic comparisons are exploratory technical agreement,
#' even when the source is label-blind and the truth table is independent. A
#' Phase 2B final product is always reported
#' as adjudicated agreement; its reference may be independent or adjudicated,
#' but the result is never relabelled as detector performance.
#'
#' @param ds A detector dataset containing the requested multi-track product.
#' @param truth_intervals Exact typed long-form truth interval table.
#' @param prediction_source Explicitly `"automatic_preview"` or `"phase2b_final"`.
#' @param truth_role Explicitly `"independent_reference"` for automatic scoring.
#'   Final agreement accepts an independent or adjudicated reference, but is
#'   never reported as detector performance.
#' @param selected_trains Optional explicit train subset. It must be contained
#'   in the validated source artifact, and truth rows outside it are rejected.
#' @param iou_threshold Inclusive interval-IoU match threshold in `(0, 1]`.
#' @return A fixed list with `metadata`, `primary_predictions`,
#'   `truth_intervals`, `matches`, `metrics_by_label`, `metrics_by_track`,
#'   `coverage_metrics`, `review_candidates`, and `review_summary`.
#'   V1 deliberately has no split-table input; predeclared split-aware scoring
#'   is deferred, while explicit train-subset scoring is supported.
#'   The metadata estimand is `exploratory_technical_agreement` for automatic
#'   sources and `adjudicated_agreement` for `phase2b_final`.
#'
#'   Phase 2B adjudicated agreement is not label-blind detector accuracy and does
#'   not by itself establish biological validity. Review target coverage is not
#'   Review precision, and interval rows are not independent sampling units.
#'   `selected_trains` declares the scope for which annotation is complete,
#'   including fully annotated trains with zero positive truth intervals. The
#'   function neither infers annotation completeness nor narrows the scope from
#'   truth rows; unmatched predictions in every selected train count as false
#'   positives.
stpd_multitrack_validation <- function(
    ds, truth_intervals, prediction_source = NULL, truth_role = NULL,
    selected_trains = NULL, iou_threshold = 0.5) {
  prediction_source <- stpd_multitrack_validation_scalar(
    prediction_source, "prediction_source"
  )
  truth_role <- stpd_multitrack_validation_scalar(truth_role, "truth_role")
  if (!prediction_source %in% c(
      "automatic_v2", "reviewed_v2", "automatic_preview", "phase2b_final"
  )) {
    stpd_multitrack_validation_abort(
      "prediction_source_invalid",
      paste(
        "prediction_source must be 'automatic_v2', 'reviewed_v2',",
        "'automatic_preview', or 'phase2b_final'."
      )
    )
  }
  automatic_source <- prediction_source %in% c(
    "automatic_v2", "automatic_preview"
  )
  truth_roles <- if (automatic_source) {
    "independent_reference"
  } else c("independent_reference", "adjudicated_reference")
  if (!truth_role %in% truth_roles) {
    stpd_multitrack_validation_abort(
      "validation_truth_role_contract_invalid",
      paste0(
        "prediction_source='", prediction_source,
        "' does not permit truth_role='", truth_role, "'."
      )
    )
  }
  if (length(iou_threshold) != 1L || is.na(iou_threshold) ||
      !is.numeric(iou_threshold) || !is.finite(iou_threshold) ||
      iou_threshold <= 0 || iou_threshold > 1) {
    stpd_multitrack_validation_abort(
      "iou_threshold_invalid", "iou_threshold must be one finite number in (0, 1]."
    )
  }
  source <- if (identical(prediction_source, "automatic_v2")) {
    stpd_multitrack_validation_extract_v2(ds, reviewed = FALSE)
  } else if (identical(prediction_source, "reviewed_v2")) {
    stpd_multitrack_validation_extract_v2(ds, reviewed = TRUE)
  } else if (identical(prediction_source, "automatic_preview")) {
    stpd_multitrack_validation_extract_automatic(ds)
  } else {
    stpd_multitrack_validation_extract_final(ds)
  }
  artifact_scope <- sort(unique(as.character(source$artifact_scope)), method = "radix")
  if (length(artifact_scope) == 0L || anyNA(artifact_scope) ||
      any(!nzchar(artifact_scope))) {
    stpd_multitrack_validation_abort(
      "prediction_artifact_scope_invalid",
      "The validated prediction artifact has no usable train scope."
    )
  }
  if (is.null(selected_trains)) {
    selected_trains <- artifact_scope
  } else {
    if (!is.character(selected_trains) || length(selected_trains) == 0L ||
        anyNA(selected_trains) || any(!nzchar(selected_trains)) ||
        anyDuplicated(selected_trains)) {
      stpd_multitrack_validation_abort(
        "selected_train_scope_invalid",
        "selected_trains must be a non-empty unique character vector."
      )
    }
    selected_trains <- sort(selected_trains, method = "radix")
    if (any(!selected_trains %in% artifact_scope)) {
      stpd_multitrack_validation_abort(
        "selected_train_scope_invalid",
        "selected_trains contains a train outside the validated prediction artifact."
      )
    }
  }
  source$primary <- source$primary[
    source$primary$train %in% selected_trains, , drop = FALSE
  ]
  source$review <- source$review[
    source$review$train %in% selected_trains, , drop = FALSE
  ]
  rownames(source$primary) <- NULL
  rownames(source$review) <- NULL
  truth <- stpd_multitrack_validation_truth(
    truth_intervals, ds, selected_trains = selected_trains
  )
  estimand <- if (automatic_source) {
    "exploratory_technical_agreement"
  } else "adjudicated_agreement"
  matches <- stpd_multitrack_validation_match(
    source$primary, truth, as.numeric(iou_threshold)
  )
  metrics_by_label <- stpd_multitrack_validation_metrics_by_label(
    source$primary, truth, matches
  )
  metrics_by_track <- stpd_multitrack_validation_metrics_by_track(
    source$primary, truth, matches
  )
  coverage <- stpd_multitrack_validation_coverage(source$primary, truth)
  review_summary <- stpd_multitrack_validation_review_summary(
    source$review, truth, as.numeric(iou_threshold)
  )
  metadata <- data.frame(
    schema_version = stpd_multitrack_validation_schema_version(),
    prediction_source = prediction_source,
    estimand = estimand,
    truth_role = truth_role,
    iou_threshold = as.numeric(iou_threshold),
    matching_rule = "ordered_dp_max_cardinality_then_total_iou_v1",
    split_policy = "split_table_deferred_v1__explicit_train_subset_only",
    metric_aggregation = "track_separate__never_pool_event_state_gap",
    uncertainty_policy = "raw_counts_only__cluster_ci_deferred",
    review_handling = paste0(
      "target_coverage_only__excluded_from_primary_",
      "precision_recall_f1"
    ),
    primary_tracks = paste(stpd_multitrack_validation_primary_tracks(), collapse = ";"),
    selected_train_n = as.integer(length(selected_trains)),
    selected_trains = paste(selected_trains, collapse = ";"),
    run_id = source$run_id,
    params_hash = source$params_hash,
    source_schema_version = source$source_schema_version,
    prediction_artifact_sha256 = source$artifact_sha256,
    truth_sha256 = stpd_multitrack_review_table_hash(truth),
    source_label_blind = as.logical(source$label_blind),
    source_authoritative = as.logical(source$authoritative),
    prediction_interval_n = as.integer(nrow(source$primary)),
    truth_interval_n = as.integer(nrow(truth)),
    matched_interval_n = as.integer(nrow(matches)),
    review_candidate_n = as.integer(nrow(source$review)),
    review_excluded_from_primary = TRUE,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      metadata = metadata,
      primary_predictions = source$primary,
      truth_intervals = truth,
      matches = matches,
      metrics_by_label = metrics_by_label,
      metrics_by_track = metrics_by_track,
      coverage_metrics = coverage,
      review_candidates = source$review,
      review_summary = review_summary
    ),
    class = c("stpd_multitrack_validation", "list")
  )
}
