# Event-level validation helpers. These functions do not alter the detector;
# they provide clean, explicit evaluation primitives for held-out workflows.

stpd_event_iou <- function(a_start, a_end, b_start, b_end) {
  a_start <- as.integer(a_start); a_end <- as.integer(a_end); b_start <- as.integer(b_start); b_end <- as.integer(b_end)
  lo <- pmax(a_start, b_start); hi <- pmin(a_end, b_end)
  ov <- pmax(0L, hi - lo + 1L)
  union <- pmax(a_end, b_end) - pmin(a_start, b_start) + 1L
  ifelse(union > 0, ov / union, 0)
}

stpd_match_events_optimal_empty <- function(include_train = TRUE) {
  out <- tibble::tibble(
    pred_index = integer(), truth_index = integer(),
    train = character(), pattern = character(), iou = numeric()
  )
  if (!isTRUE(include_train)) out$train <- NULL
  out
}

stpd_event_matching_rule <- function() {
  "one_to_one_max_cardinality_then_max_total_iou_v1"
}

stpd_hungarian_min_cost <- function(cost) {
  cost <- as.matrix(cost)
  n <- nrow(cost)
  m <- ncol(cost)
  if (n == 0L) return(integer())
  if (m < n) stop("stpd_hungarian_min_cost(): cost matrix must have at least as many columns as rows.", call. = FALSE)
  if (any(!is.finite(cost))) stop("stpd_hungarian_min_cost(): cost matrix must be finite.", call. = FALSE)

  # Shortest augmenting-path Hungarian algorithm. Logical row/column zero is
  # stored at R index one; scanning columns in ascending order provides a stable
  # deterministic choice whenever reduced costs are tied.
  u <- numeric(n + 1L)
  v <- numeric(m + 1L)
  p <- integer(m + 1L)
  way <- integer(m + 1L)
  tolerance <- .Machine$double.eps * 128

  for (i in seq_len(n)) {
    p[1L] <- i
    j0 <- 0L
    minv <- rep(Inf, m)
    used <- rep(FALSE, m + 1L)
    repeat {
      used[j0 + 1L] <- TRUE
      i0 <- p[j0 + 1L]
      delta <- Inf
      j1 <- 0L
      for (j in seq_len(m)) {
        if (used[j + 1L]) next
        cur <- cost[i0, j] - u[i0 + 1L] - v[j + 1L]
        scale <- max(1, abs(cur), if (is.finite(minv[j])) abs(minv[j]) else 0)
        if (!is.finite(minv[j]) || cur < minv[j] - tolerance * scale) {
          minv[j] <- cur
          way[j + 1L] <- j0
        }
        delta_scale <- max(1, abs(minv[j]), if (is.finite(delta)) abs(delta) else 0)
        if (!is.finite(delta) || minv[j] < delta - tolerance * delta_scale ||
            (abs(minv[j] - delta) <= tolerance * delta_scale && (j1 == 0L || j < j1))) {
          delta <- minv[j]
          j1 <- j
        }
      }
      if (!is.finite(delta) || j1 == 0L) stop("stpd_hungarian_min_cost(): assignment failed.", call. = FALSE)
      used_columns <- which(used) - 1L
      for (j in used_columns) {
        row <- p[j + 1L]
        u[row + 1L] <- u[row + 1L] + delta
        v[j + 1L] <- v[j + 1L] - delta
      }
      unused_columns <- which(!used[-1L])
      minv[unused_columns] <- minv[unused_columns] - delta
      j0 <- j1
      if (p[j0 + 1L] == 0L) break
    }
    repeat {
      j1 <- way[j0 + 1L]
      p[j0 + 1L] <- p[j1 + 1L]
      j0 <- j1
      if (j0 == 0L) break
    }
  }

  assignment <- integer(n)
  for (j in seq_len(m)) {
    if (p[j + 1L] > 0L) assignment[p[j + 1L]] <- j
  }
  assignment
}

stpd_match_events_optimal <- function(pred, truth, class_col = "pattern", iou_min = 0.25) {
  empty <- stpd_match_events_optimal_empty(include_train = TRUE)
  if (is.null(pred) || is.null(truth) || nrow(pred) == 0L || nrow(truth) == 0L) return(empty)
  pred <- as.data.frame(pred, stringsAsFactors = FALSE)
  truth <- as.data.frame(truth, stringsAsFactors = FALSE)
  required <- c("train", class_col, "start_isi", "end_isi")
  if (!all(required %in% names(pred)) || !all(required %in% names(truth))) {
    stop("Event tables must contain train, class, start_isi, and end_isi columns.", call. = FALSE)
  }
  iou_min <- suppressWarnings(as.numeric(iou_min))
  if (length(iou_min) != 1L || is.na(iou_min) || !is.finite(iou_min) ||
      iou_min <= 0 || iou_min > 1) {
    stop("iou_min must be one finite number in (0, 1].", call. = FALSE)
  }

  pred_train <- as.character(pred$train)
  truth_train <- as.character(truth$train)
  pred_class <- as.character(pred[[class_col]])
  truth_class <- as.character(truth[[class_col]])
  trains <- sort(intersect(unique(pred_train[!is.na(pred_train)]), unique(truth_train[!is.na(truth_train)])), method = "radix")
  rows <- list()

  for (tr in trains) {
    classes <- sort(intersect(
      unique(pred_class[!is.na(pred_train) & pred_train == tr & !is.na(pred_class)]),
      unique(truth_class[!is.na(truth_train) & truth_train == tr & !is.na(truth_class)])
    ), method = "radix")
    for (cls in classes) {
      pred_index <- which(pred_train == tr & !is.na(pred_class) & pred_class == cls)
      truth_index <- which(truth_train == tr & !is.na(truth_class) & truth_class == cls)
      if (length(pred_index) == 0L || length(truth_index) == 0L) next

      pred_index <- pred_index[order(
        suppressWarnings(as.integer(pred$start_isi[pred_index])),
        suppressWarnings(as.integer(pred$end_isi[pred_index])), pred_index,
        method = "radix", na.last = TRUE
      )]
      truth_index <- truth_index[order(
        suppressWarnings(as.integer(truth$start_isi[truth_index])),
        suppressWarnings(as.integer(truth$end_isi[truth_index])), truth_index,
        method = "radix", na.last = TRUE
      )]
      np <- length(pred_index)
      nt <- length(truth_index)
      iou <- matrix(0, nrow = np, ncol = nt)
      for (ii in seq_len(np)) {
        iou[ii, ] <- stpd_event_iou(
          pred$start_isi[pred_index[ii]], pred$end_isi[pred_index[ii]],
          truth$start_isi[truth_index], truth$end_isi[truth_index]
        )
      }
      eligible <- is.finite(iou) & iou >= iou_min
      if (!any(eligible)) next

      # A cardinality bonus larger than the greatest possible aggregate IoU
      # difference makes the assignment objective lexicographic: first maximize
      # match count, then maximize total IoU. Dummy/ineligible assignments score
      # zero and are discarded after solving the padded square assignment.
      size <- max(np, nt)
      cardinality_bonus <- min(np, nt) + 1
      weight <- matrix(0, nrow = size, ncol = size)
      edge <- which(eligible, arr.ind = TRUE)
      weight[edge] <- cardinality_bonus + iou[edge]
      max_weight <- max(weight)
      assignment <- stpd_hungarian_min_cost(max_weight - weight)
      keep_rows <- seq_len(np)
      keep_columns <- assignment[keep_rows]
      keep <- keep_columns >= 1L & keep_columns <= nt
      keep[keep] <- eligible[cbind(keep_rows[keep], keep_columns[keep])]
      if (!any(keep)) next

      matched_rows <- keep_rows[keep]
      matched_columns <- keep_columns[keep]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        pred_index = as.integer(pred_index[matched_rows]),
        truth_index = as.integer(truth_index[matched_columns]),
        train = rep(tr, length(matched_rows)),
        pattern = rep(cls, length(matched_rows)),
        iou = as.numeric(iou[cbind(matched_rows, matched_columns)])
      )
    }
  }
  if (length(rows) == 0L) return(empty)
  out <- dplyr::bind_rows(rows)
  out[order(out$train, out$pattern, out$pred_index, out$truth_index, method = "radix"), , drop = FALSE]
}

stpd_match_events <- function(pred, truth, class_col = "pattern", iou_min = 0.25) {
  out <- stpd_match_events_optimal(pred, truth, class_col = class_col, iou_min = iou_min)
  out[, c("pred_index", "truth_index", "iou", "pattern"), drop = FALSE]
}

stpd_event_level_metrics <- function(pred, truth, class_col = "pattern", iou_min = 0.25) {
  classes <- sort(unique(c(pred[[class_col]] %||% character(), truth[[class_col]] %||% character())))
  if (length(classes) == 0) return(tibble::tibble())
  match <- stpd_match_events(pred, truth, class_col = class_col, iou_min = iou_min)
  rows <- lapply(classes, function(cls) {
    pidx <- which(pred[[class_col]] == cls)
    tidx <- which(truth[[class_col]] == cls)
    midx <- match$pattern == cls
    tp <- sum(midx)
    fp <- max(0L, length(pidx) - tp)
    fn <- max(0L, length(tidx) - tp)
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    f1_denominator <- 2L * tp + fp + fn
    f1 <- if (f1_denominator > 0L) 2 * tp / f1_denominator else NA_real_
    data.frame(pattern = cls, tp = tp, fp = fp, fn = fn, precision = precision, recall = recall, f1 = f1, stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# Standalone detection-result evaluation. This layer deliberately accepts only
# event tables: it cannot read detector parameters, manual-learning state, or a
# dataset object, and therefore cannot feed reference labels back into detection.
stpd_detection_evaluation_validate_events <- function(x, object_name, class_col) {
  if (is.null(x)) {
    out <- data.frame(train = character(), start_isi = integer(), end_isi = integer(),
                      stringsAsFactors = FALSE)
    out[[class_col]] <- character()
    return(out)
  }
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("train", class_col, "start_isi", "end_isi")
  missing <- setdiff(required, names(out))
  if (length(missing) > 0L) {
    stop(sprintf("%s must contain: %s.", object_name, paste(required, collapse = ", ")), call. = FALSE)
  }
  if (nrow(out) == 0L) return(out)

  train <- as.character(out$train)
  pattern <- as.character(out[[class_col]])
  start_raw <- suppressWarnings(as.numeric(out$start_isi))
  end_raw <- suppressWarnings(as.numeric(out$end_isi))
  valid <- !is.na(train) & nzchar(train) & !is.na(pattern) & nzchar(pattern) &
    is.finite(start_raw) & is.finite(end_raw) & start_raw >= 1 & end_raw >= start_raw &
    start_raw <= .Machine$integer.max & end_raw <= .Machine$integer.max &
    start_raw == floor(start_raw) & end_raw == floor(end_raw)
  if (!all(valid)) {
    stop(sprintf(
      "%s contains invalid train/class values or non-integer ISI bounds (start_isi >= 1 and end_isi >= start_isi are required).",
      object_name
    ), call. = FALSE)
  }
  out$train <- train
  out[[class_col]] <- pattern
  out$start_isi <- as.integer(start_raw)
  out$end_isi <- as.integer(end_raw)
  out
}

stpd_detection_evaluation_iou_grid <- function(iou_thresholds) {
  out <- suppressWarnings(as.numeric(iou_thresholds))
  if (length(out) == 0L || any(!is.finite(out)) || any(out <= 0 | out > 1)) {
    stop("iou_thresholds must contain finite, unique values in (0, 1].", call. = FALSE)
  }
  if (anyDuplicated(out)) {
    stop("iou_thresholds must contain finite, unique values in (0, 1].", call. = FALSE)
  }
  out <- sort(unique(out))
  tier <- vapply(out, function(x) {
    if (isTRUE(all.equal(x, 0.25))) return("approximate_detection")
    if (isTRUE(all.equal(x, 0.50))) return("substantial_overlap")
    if (isTRUE(all.equal(x, 0.75))) return("high_overlap")
    if (isTRUE(all.equal(x, 1.00))) return("complete_overlap")
    "custom_overlap"
  }, character(1))
  data.frame(
    iou_threshold = out,
    evaluation_tier = tier,
    definition = paste0("one-to-one event match with ISI IoU >= ", format(out, trim = TRUE)),
    stringsAsFactors = FALSE
  )
}

stpd_detection_evaluation_match_details <- function(pred, truth, class_col, iou_threshold) {
  matches <- stpd_match_events_optimal(pred, truth, class_col = class_col, iou_min = iou_threshold)
  if (nrow(matches) == 0L) {
    return(data.frame(
      iou_threshold = numeric(), pred_index = integer(), truth_index = integer(),
      train = character(), pattern = character(), iou = numeric(),
      truth_start_isi = integer(), truth_end_isi = integer(),
      predicted_start_isi = integer(), predicted_end_isi = integer(),
      overlap_isi_n = integer(), truth_isi_n = integer(), predicted_isi_n = integer(),
      truth_isi_recovery = numeric(), predicted_isi_precision = numeric(),
      start_boundary_error_isi = integer(), end_boundary_error_isi = integer(),
      mean_absolute_boundary_error_isi = numeric(), exact_boundary_match = logical(),
      stringsAsFactors = FALSE
    ))
  }
  pi <- as.integer(matches$pred_index)
  ti <- as.integer(matches$truth_index)
  ps <- pred$start_isi[pi]
  pe <- pred$end_isi[pi]
  ts <- truth$start_isi[ti]
  te <- truth$end_isi[ti]
  overlap <- pmax(0L, pmin(pe, te) - pmax(ps, ts) + 1L)
  pred_n <- pe - ps + 1L
  truth_n <- te - ts + 1L
  start_error <- ps - ts
  end_error <- pe - te
  data.frame(
    iou_threshold = rep(as.numeric(iou_threshold), nrow(matches)),
    pred_index = pi,
    truth_index = ti,
    train = as.character(matches$train),
    pattern = as.character(matches$pattern),
    iou = as.numeric(matches$iou),
    truth_start_isi = ts,
    truth_end_isi = te,
    predicted_start_isi = ps,
    predicted_end_isi = pe,
    overlap_isi_n = as.integer(overlap),
    truth_isi_n = as.integer(truth_n),
    predicted_isi_n = as.integer(pred_n),
    truth_isi_recovery = overlap / truth_n,
    predicted_isi_precision = overlap / pred_n,
    start_boundary_error_isi = as.integer(start_error),
    end_boundary_error_isi = as.integer(end_error),
    mean_absolute_boundary_error_isi = (abs(start_error) + abs(end_error)) / 2,
    exact_boundary_match = ps == ts & pe == te,
    stringsAsFactors = FALSE
  )
}

stpd_detection_evaluation_union <- function(start, end) {
  if (length(start) == 0L) return(data.frame(start = integer(), end = integer()))
  ord <- order(start, end, method = "radix")
  start <- as.integer(start[ord]); end <- as.integer(end[ord])
  out_start <- integer(); out_end <- integer()
  current_start <- start[1L]; current_end <- end[1L]
  if (length(start) > 1L) {
    for (ii in 2:length(start)) {
      if (start[ii] <= current_end + 1L) {
        current_end <- max(current_end, end[ii])
      } else {
        out_start <- c(out_start, current_start); out_end <- c(out_end, current_end)
        current_start <- start[ii]; current_end <- end[ii]
      }
    }
  }
  data.frame(start = c(out_start, current_start), end = c(out_end, current_end))
}

stpd_detection_evaluation_intersection_n <- function(a, b) {
  if (nrow(a) == 0L || nrow(b) == 0L) return(0)
  ii <- 1L; jj <- 1L; total <- 0
  while (ii <= nrow(a) && jj <= nrow(b)) {
    total <- total + max(0, min(a$end[ii], b$end[jj]) - max(a$start[ii], b$start[jj]) + 1)
    if (a$end[ii] < b$end[jj]) ii <- ii + 1L else jj <- jj + 1L
  }
  total
}

stpd_detection_evaluation_isi_by_train <- function(pred, truth, class_col) {
  keys <- unique(rbind(
    data.frame(train = pred$train, pattern = pred[[class_col]], stringsAsFactors = FALSE),
    data.frame(train = truth$train, pattern = truth[[class_col]], stringsAsFactors = FALSE)
  ))
  if (nrow(keys) == 0L) {
    return(data.frame(
      train = character(), pattern = character(), truth_isi_n = numeric(),
      predicted_isi_n = numeric(), overlap_isi_n = numeric(), truth_isi_recall = numeric(),
      predicted_isi_precision = numeric(), isi_dice = numeric(), stringsAsFactors = FALSE
    ))
  }
  keys <- keys[order(keys$train, keys$pattern, method = "radix"), , drop = FALSE]
  rows <- lapply(seq_len(nrow(keys)), function(ii) {
    tr <- keys$train[ii]; cls <- keys$pattern[ii]
    p <- pred[pred$train == tr & pred[[class_col]] == cls, , drop = FALSE]
    t <- truth[truth$train == tr & truth[[class_col]] == cls, , drop = FALSE]
    pu <- stpd_detection_evaluation_union(p$start_isi, p$end_isi)
    tu <- stpd_detection_evaluation_union(t$start_isi, t$end_isi)
    pn <- sum(pu$end - pu$start + 1)
    tn <- sum(tu$end - tu$start + 1)
    ov <- stpd_detection_evaluation_intersection_n(pu, tu)
    data.frame(
      train = tr, pattern = cls, truth_isi_n = tn, predicted_isi_n = pn,
      overlap_isi_n = ov,
      truth_isi_recall = if (tn > 0) ov / tn else NA_real_,
      predicted_isi_precision = if (pn > 0) ov / pn else NA_real_,
      isi_dice = if ((tn + pn) > 0) 2 * ov / (tn + pn) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

stpd_detection_evaluation_empty_fragmentation_by_reference <- function() {
  data.frame(
    truth_index = integer(), train = character(), pattern = character(),
    truth_start_isi = integer(), truth_end_isi = integer(), truth_isi_n = integer(),
    overlapping_predicted_n = integer(), overlapping_predicted_indices = character(),
    recovered_truth_isi_n = integer(), truth_isi_recovery = numeric(),
    detected_any_overlap = logical(), fragmented = logical(),
    excess_fragment_n = integer(), stringsAsFactors = FALSE
  )
}

stpd_detection_evaluation_fragmentation_by_reference <- function(
    pred, truth, class_col) {
  empty <- stpd_detection_evaluation_empty_fragmentation_by_reference()
  if (nrow(truth) == 0L) return(empty)

  rows <- lapply(seq_len(nrow(truth)), function(truth_index) {
    tr <- as.character(truth$train[truth_index])
    cls <- as.character(truth[[class_col]][truth_index])
    truth_start <- as.integer(truth$start_isi[truth_index])
    truth_end <- as.integer(truth$end_isi[truth_index])
    pred_index <- which(
      pred$train == tr & pred[[class_col]] == cls &
        pred$start_isi <= truth_end & pred$end_isi >= truth_start
    )
    if (length(pred_index) > 1L) {
      pred_index <- pred_index[order(
        pred$start_isi[pred_index], pred$end_isi[pred_index], pred_index,
        method = "radix"
      )]
    }
    recovered_n <- 0L
    if (length(pred_index) > 0L) {
      clipped <- stpd_detection_evaluation_union(
        pmax(truth_start, pred$start_isi[pred_index]),
        pmin(truth_end, pred$end_isi[pred_index])
      )
      recovered_n <- as.integer(sum(clipped$end - clipped$start + 1L))
    }
    predicted_n <- as.integer(length(pred_index))
    truth_n <- as.integer(truth_end - truth_start + 1L)
    data.frame(
      truth_index = as.integer(truth_index), train = tr, pattern = cls,
      truth_start_isi = truth_start, truth_end_isi = truth_end,
      truth_isi_n = truth_n, overlapping_predicted_n = predicted_n,
      overlapping_predicted_indices = paste(pred_index, collapse = ";"),
      recovered_truth_isi_n = recovered_n,
      truth_isi_recovery = recovered_n / truth_n,
      detected_any_overlap = predicted_n >= 1L,
      fragmented = predicted_n >= 2L,
      excess_fragment_n = as.integer(max(0L, predicted_n - 1L)),
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(c(list(empty), unname(rows)))
  out <- out[order(
    out$train, out$pattern, out$truth_start_isi, out$truth_end_isi,
    out$truth_index, method = "radix"
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_detection_evaluation_empty_fragmentation_metrics <- function() {
  data.frame(
    pattern = character(), truth_n = integer(), detected_truth_n = integer(),
    fragmented_truth_n = integer(), total_excess_fragment_n = integer(),
    false_split_rate_all_truth = numeric(),
    false_split_rate_detected_truth = numeric(),
    excess_fragments_per_truth = numeric(),
    mean_fragments_per_detected_truth = numeric(),
    stringsAsFactors = FALSE
  )
}

stpd_detection_evaluation_fragmentation_metrics <- function(by_reference) {
  empty <- stpd_detection_evaluation_empty_fragmentation_metrics()
  if (nrow(by_reference) == 0L) return(empty)

  rows <- lapply(split(by_reference, by_reference$pattern), function(x) {
    truth_n <- as.integer(nrow(x))
    detected_truth_n <- as.integer(sum(x$detected_any_overlap))
    fragmented_truth_n <- as.integer(sum(x$fragmented))
    total_excess_fragment_n <- as.integer(sum(x$excess_fragment_n))
    data.frame(
      pattern = as.character(x$pattern[1L]), truth_n = truth_n,
      detected_truth_n = detected_truth_n,
      fragmented_truth_n = fragmented_truth_n,
      total_excess_fragment_n = total_excess_fragment_n,
      false_split_rate_all_truth = fragmented_truth_n / truth_n,
      false_split_rate_detected_truth = if (detected_truth_n > 0L) {
        fragmented_truth_n / detected_truth_n
      } else NA_real_,
      excess_fragments_per_truth = total_excess_fragment_n / truth_n,
      mean_fragments_per_detected_truth = if (detected_truth_n > 0L) {
        sum(x$overlapping_predicted_n[x$detected_any_overlap]) / detected_truth_n
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(c(list(empty), unname(rows)))
  out <- out[order(out$pattern, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Evaluate detected events independently of the detector
#'
#' Compares a fixed prediction table with a fixed reference table. The function
#' performs deterministic one-to-one event matching at several ISI-IoU levels,
#' and separately reports pooled ISI coverage, boundary errors, and reference-
#' event fragmentation. It never runs or modifies the detector and accepts
#' neither detector parameters nor labels used for parameter learning.
#'
#' @param predicted Event table with `train`, class, `start_isi`, and `end_isi`.
#' @param reference Reference event table with the same required columns.
#' @param class_col Name of the event-class column.
#' @param iou_thresholds Event IoU thresholds. Defaults to 0.25, 0.50, and 0.75.
#' @return A `stpd_detection_evaluation` list containing protocol,
#'   event_metrics, matches, isi_metrics, isi_metrics_by_train,
#'   fragmentation_by_reference, and fragmentation_metrics.
#' @export
stpd_evaluate_detection_results <- function(predicted, reference, class_col = "pattern",
                                            iou_thresholds = c(0.25, 0.50, 0.75)) {
  if (length(class_col) != 1L || is.na(class_col) || !nzchar(class_col)) {
    stop("class_col must be one non-empty column name.", call. = FALSE)
  }
  pred <- stpd_detection_evaluation_validate_events(predicted, "predicted", class_col)
  truth <- stpd_detection_evaluation_validate_events(reference, "reference", class_col)
  protocol <- stpd_detection_evaluation_iou_grid(iou_thresholds)
  classes <- sort(unique(c(as.character(pred[[class_col]]), as.character(truth[[class_col]]))), method = "radix")

  match_list <- lapply(protocol$iou_threshold, function(threshold) {
    stpd_detection_evaluation_match_details(pred, truth, class_col, threshold)
  })
  matches <- dplyr::bind_rows(match_list)
  metric_rows <- list()
  for (threshold in protocol$iou_threshold) {
    current <- matches[matches$iou_threshold == threshold, , drop = FALSE]
    for (cls in classes) {
      predicted_n <- sum(pred[[class_col]] == cls)
      truth_n <- sum(truth[[class_col]] == cls)
      selected <- current$pattern == cls
      tp <- sum(selected)
      fp <- predicted_n - tp
      fn <- truth_n - tp
      precision <- if ((tp + fp) > 0L) tp / (tp + fp) else NA_real_
      recall <- if ((tp + fn) > 0L) tp / (tp + fn) else NA_real_
      f1_denominator <- 2L * tp + fp + fn
      metric_rows[[length(metric_rows) + 1L]] <- data.frame(
        iou_threshold = threshold,
        evaluation_tier = protocol$evaluation_tier[match(threshold, protocol$iou_threshold)],
        pattern = cls,
        truth_n = truth_n,
        predicted_n = predicted_n,
        true_positive_n = tp,
        false_positive_n = fp,
        false_negative_n = fn,
        precision = precision,
        recall = recall,
        F1 = if (f1_denominator > 0L) 2 * tp / f1_denominator else NA_real_,
        exact_boundary_match_n = sum(current$exact_boundary_match[selected]),
        mean_iou_matched = if (tp > 0L) mean(current$iou[selected]) else NA_real_,
        mean_absolute_boundary_error_isi = if (tp > 0L) {
          mean(current$mean_absolute_boundary_error_isi[selected])
        } else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  event_metrics <- if (length(metric_rows) > 0L) dplyr::bind_rows(metric_rows) else data.frame()
  isi_by_train <- stpd_detection_evaluation_isi_by_train(pred, truth, class_col)
  if (nrow(isi_by_train) > 0L) {
    isi_metrics <- lapply(split(isi_by_train, isi_by_train$pattern), function(x) {
      tn <- sum(x$truth_isi_n); pn <- sum(x$predicted_isi_n); ov <- sum(x$overlap_isi_n)
      data.frame(
        pattern = x$pattern[1L], truth_isi_n = tn, predicted_isi_n = pn, overlap_isi_n = ov,
        truth_isi_recall = if (tn > 0) ov / tn else NA_real_,
        predicted_isi_precision = if (pn > 0) ov / pn else NA_real_,
        isi_dice = if ((tn + pn) > 0) 2 * ov / (tn + pn) else NA_real_,
        stringsAsFactors = FALSE
      )
    })
    isi_metrics <- dplyr::bind_rows(isi_metrics)
    isi_metrics <- isi_metrics[order(isi_metrics$pattern, method = "radix"), , drop = FALSE]
  } else {
    isi_metrics <- data.frame()
  }
  fragmentation_by_reference <-
    stpd_detection_evaluation_fragmentation_by_reference(pred, truth, class_col)
  fragmentation_metrics <-
    stpd_detection_evaluation_fragmentation_metrics(fragmentation_by_reference)

  structure(list(
    protocol = protocol,
    event_metrics = event_metrics,
    matches = matches,
    isi_metrics = isi_metrics,
    isi_metrics_by_train = isi_by_train,
    fragmentation_by_reference = fragmentation_by_reference,
    fragmentation_metrics = fragmentation_metrics,
    matching_rule = stpd_event_matching_rule(),
    fragmentation_rule = paste(
      "reference_event_overlap_count",
      "same_train_and_class_positive_isi_overlap",
      "distinct_prediction_rows_v1", sep = "__"
    ),
    coordinate_system = "inclusive right-spike-index ISI intervals"
  ), class = c("stpd_detection_evaluation", "list"))
}

stpd_holdout_split_by_train <- function(train_names, fraction = 0.25, seed = 1L) {
  train_names <- unique(as.character(train_names))
  set.seed(seed)
  n_val <- max(1L, floor(length(train_names) * fraction))
  validation <- sample(train_names, n_val)
  data.frame(train = train_names, split = ifelse(train_names %in% validation, "validation", "calibration"), stringsAsFactors = FALSE)
}

stpd_overfit_report <- function(calibration_metrics, validation_metrics) {
  if (is.null(calibration_metrics) || is.null(validation_metrics)) return(tibble::tibble())
  m <- merge(calibration_metrics, validation_metrics, by = "pattern", suffixes = c("_calibration", "_validation"), all = TRUE)
  m$recall_gap <- m$recall_calibration - m$recall_validation
  m$precision_gap <- m$precision_calibration - m$precision_validation
  m$interpretation <- ifelse(is.finite(m$recall_gap) & m$recall_gap > 0.2, "possible overfit: calibration recall much higher than validation", "no large recall gap detected")
  m
}
