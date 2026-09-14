# Multi-method patrol layer ---------------------------------------------------
#
# This layer compares immutable detector outputs. It does not vote on, replace,
# or silently alter any detector label. Its product is a review queue plus the
# complete method-level evidence used to construct that queue.

STPD_PATROL_VERSION <- "stpd_multi_method_patrol_v1"

stpd_patrol_family_map <- function() {
  c(
    native_stpd = "native_grammar",
    mean_isi = "isi_threshold",
    mean_isi_calibrated = "isi_threshold_calibrated",
    mean_isi_article = "isi_threshold",
    logisi_newbd = "isi_threshold",
    logisi_newbd_calibrated = "isi_threshold_calibrated",
    pasquale_logisi_newBD = "isi_threshold",
    poisson_surprise = "surprise",
    robust_gaussian_surprise = "surprise"
  )
}

stpd_patrol_label_family <- function(label) {
  x <- tolower(trimws(as.character(label)))
  x[x %in% c("burst", "long_burst", "possible_burst", "burst_family")] <- "burst"
  x[x %in% c("broad_hfs", "hfs", "hft", "hf_irregular",
             "high_frequency_spiking", "high_frequency_tonic")] <- "broad_hfs"
  x
}

stpd_patrol_hash <- function(domain, fields) {
  payload <- paste(c(domain, enc2utf8(as.character(fields))), collapse = "\034")
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}

stpd_patrol_empty_evidence <- function() {
  data.frame(
    evidence_id = character(), provider_key = character(),
    evidence_family = character(), source_record_id = character(),
    train_key = character(), semantic_track = character(),
    proposed_label = character(), target_family = character(),
    start_isi = integer(), end_isi = integer(),
    start_spike = integer(), end_spike = integer(),
    start_time_sec = numeric(), end_time_sec = numeric(),
    authority_scope = character(), score_name = character(),
    score_value = numeric(), stringsAsFactors = FALSE
  )
}

stpd_patrol_validate_evidence <- function(evidence) {
  prototype <- stpd_patrol_empty_evidence()
  if (!is.data.frame(evidence)) {
    stop("Patrol evidence must be a data.frame.", call. = FALSE)
  }
  missing <- setdiff(names(prototype), names(evidence))
  if (length(missing)) {
    stop("Patrol evidence is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  evidence <- evidence[, names(prototype), drop = FALSE]
  if (!nrow(evidence)) return(prototype)

  character_fields <- names(prototype)[vapply(prototype, is.character, logical(1))]
  numeric_fields <- c("start_isi", "end_isi", "start_spike", "end_spike",
                      "start_time_sec", "end_time_sec", "score_value")
  for (field in character_fields) evidence[[field]] <- as.character(evidence[[field]])
  for (field in numeric_fields) evidence[[field]] <- suppressWarnings(as.numeric(evidence[[field]]))
  evidence$start_isi <- as.integer(evidence$start_isi)
  evidence$end_isi <- as.integer(evidence$end_isi)
  evidence$start_spike <- as.integer(evidence$start_spike)
  evidence$end_spike <- as.integer(evidence$end_spike)

  required_text <- c("evidence_id", "provider_key", "evidence_family",
                     "source_record_id", "train_key", "semantic_track",
                     "proposed_label", "target_family", "authority_scope")
  bad_text <- vapply(required_text, function(field) {
    anyNA(evidence[[field]]) || any(!nzchar(trimws(evidence[[field]])))
  }, logical(1))
  if (any(bad_text)) {
    stop("Patrol evidence has missing/empty required text in: ",
         paste(required_text[bad_text], collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(evidence$evidence_id)) {
    stop("Patrol evidence_id values must be unique.", call. = FALSE)
  }
  if (any(!evidence$semantic_track %in% c("event", "state", "gap"))) {
    stop("semantic_track must be event, state, or gap.", call. = FALSE)
  }
  if (any(!is.finite(evidence$start_isi)) || any(!is.finite(evidence$end_isi)) ||
      any(evidence$start_isi < 1L) || any(evidence$end_isi < evidence$start_isi)) {
    stop("Patrol ISI coordinates must be finite, positive, closed intervals.",
         call. = FALSE)
  }
  spike_present <- is.finite(evidence$start_spike) | is.finite(evidence$end_spike)
  if (any(spike_present & (!is.finite(evidence$start_spike) |
                           !is.finite(evidence$end_spike) |
                           evidence$end_spike < evidence$start_spike))) {
    stop("Spike coordinates must be both absent or a valid closed interval.",
         call. = FALSE)
  }
  time_present <- is.finite(evidence$start_time_sec) | is.finite(evidence$end_time_sec)
  if (any(time_present & (!is.finite(evidence$start_time_sec) |
                          !is.finite(evidence$end_time_sec) |
                          evidence$end_time_sec < evidence$start_time_sec))) {
    stop("Time coordinates must be both absent or a valid closed interval.",
         call. = FALSE)
  }
  evidence <- evidence[order(
    evidence$train_key, evidence$semantic_track, evidence$target_family,
    evidence$start_isi, evidence$end_isi, evidence$provider_key,
    evidence$evidence_id, method = "radix"
  ), , drop = FALSE]
  rownames(evidence) <- NULL
  evidence
}

stpd_patrol_evidence_from_provider_bundle <- function(bundle) {
  stpd_validate_provider_bundle(bundle)
  candidates <- bundle$candidate_intervals
  candidates <- candidates[candidates$provider_decision == "positive", , drop = FALSE]
  if (!nrow(candidates)) return(stpd_patrol_empty_evidence())
  run_index <- match(candidates$provider_run_id, bundle$provider_runs$provider_run_id)
  provider <- as.character(bundle$provider_runs$provider_kind[run_index])
  family_map <- stpd_patrol_family_map()
  family <- unname(family_map[provider])
  family[is.na(family)] <- paste0("provider:", provider[is.na(family)])
  out <- data.frame(
    evidence_id = as.character(candidates$candidate_id),
    provider_key = provider,
    evidence_family = family,
    source_record_id = as.character(candidates$source_record_key),
    train_key = as.character(candidates$train_key),
    semantic_track = as.character(candidates$semantic_track),
    proposed_label = as.character(candidates$proposed_label),
    target_family = stpd_patrol_label_family(candidates$proposed_label),
    start_isi = as.integer(candidates$canonical_start_isi),
    end_isi = as.integer(candidates$canonical_end_isi),
    start_spike = as.integer(candidates$canonical_start_spike),
    end_spike = as.integer(candidates$canonical_end_spike),
    start_time_sec = as.numeric(candidates$canonical_start_time_sec),
    end_time_sec = as.numeric(candidates$canonical_end_time_sec),
    authority_scope = as.character(bundle$provider_runs$authority_scope[run_index]),
    score_name = as.character(candidates$score_name),
    score_value = as.numeric(candidates$score_value),
    stringsAsFactors = FALSE
  )
  out$score_name[is.na(out$score_name)] <- ""
  stpd_patrol_validate_evidence(out)
}

stpd_patrol_evidence_from_native_events <- function(events) {
  if (!is.data.frame(events)) stop("events must be a data.frame.", call. = FALSE)
  if (!nrow(events)) return(stpd_patrol_empty_evidence())
  required <- c("train", "pattern", "start_isi", "end_isi")
  missing <- setdiff(required, names(events))
  if (length(missing)) {
    stop("Native event table is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  value_or <- function(options, default) {
    hit <- options[options %in% names(events)]
    if (length(hit)) return(events[[hit[[1L]]]])
    rep(default, nrow(events))
  }
  label <- tolower(trimws(as.character(events$pattern)))
  track <- ifelse(label == "pause", "gap",
                  ifelse(stpd_patrol_label_family(label) == "broad_hfs" |
                           label == "tonic", "state", "event"))
  source <- as.character(value_or("event_id", seq_len(nrow(events))))
  source[is.na(source) | !nzchar(source)] <- as.character(which(is.na(source) | !nzchar(source)))
  evidence_id <- vapply(seq_len(nrow(events)), function(i) {
    stpd_patrol_hash("stpd-patrol-native-event-v1", c(
      source[[i]], as.character(events$train[[i]]), label[[i]],
      as.integer(events$start_isi[[i]]), as.integer(events$end_isi[[i]])
    ))
  }, character(1))
  score <- suppressWarnings(as.numeric(value_or("auto_score", NA_real_)))
  stpd_patrol_validate_evidence(data.frame(
    evidence_id = evidence_id, provider_key = "native_stpd",
    evidence_family = "native_grammar",
    source_record_id = paste0("native_stpd:events:", source),
    train_key = as.character(events$train), semantic_track = track,
    proposed_label = label, target_family = stpd_patrol_label_family(label),
    start_isi = as.integer(events$start_isi),
    end_isi = as.integer(events$end_isi),
    start_spike = as.integer(value_or(c("start_spike_idx", "start_spike_index",
                                        "start_spike"), NA_integer_)),
    end_spike = as.integer(value_or(c("end_spike_idx", "end_spike_index",
                                      "end_spike"), NA_integer_)),
    start_time_sec = as.numeric(value_or("start_time_sec", NA_real_)),
    end_time_sec = as.numeric(value_or("end_time_sec", NA_real_)),
    authority_scope = "automatic_prediction_record",
    score_name = ifelse(is.finite(score), "auto_score", ""),
    score_value = score, stringsAsFactors = FALSE
  ))
}

stpd_patrol_infer_provider <- function(support, provider_key = NULL) {
  if (!is.null(provider_key) && length(provider_key) == 1L && nzchar(provider_key)) {
    return(as.character(provider_key))
  }
  method <- as.character(support$method %||% "")
  if (identical(method, "poisson_surprise")) return("poisson_surprise")
  if (identical(method, "robust_gaussian_surprise")) return("robust_gaussian_surprise")
  report_method <- unique(as.character((support$support_report %||% data.frame())$method %||% ""))
  report_method <- report_method[nzchar(report_method)]
  if (length(report_method) == 1L) return(report_method)
  stop("provider_key could not be inferred from the support result.", call. = FALSE)
}

stpd_patrol_support_table <- function(support, table, provider, label) {
  events <- support[[table]]
  if (!is.data.frame(events) || !nrow(events)) return(stpd_patrol_empty_evidence())
  required <- c("train", "start_isi", "end_isi")
  missing <- setdiff(required, names(events))
  if (length(missing)) {
    stop(provider, " ", table, " table is missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  value_or <- function(name, default) {
    if (name %in% names(events)) return(events[[name]])
    if (length(default) == 1L) return(rep(default, nrow(events)))
    if (length(default) == nrow(events)) return(default)
    stop("Internal patrol default length mismatch for ", name, ".",
         call. = FALSE)
  }
  source <- value_or("event_id", value_or("burst_id", seq_len(nrow(events))))
  start_spike <- value_or("start_spike_index", value_or("start_spike", NA_integer_))
  end_spike <- value_or("end_spike_index", value_or("end_spike", NA_integer_))
  start_time <- value_or("start_time_sec", NA_real_)
  end_time <- value_or("end_time_sec", NA_real_)
  score_name <- rep("", nrow(events))
  score_value <- rep(NA_real_, nrow(events))
  if ("surprise" %in% names(events)) {
    score_name <- rep("surprise", nrow(events)); score_value <- events$surprise
  } else if ("adjusted_p" %in% names(events)) {
    score_name <- rep("adjusted_p", nrow(events)); score_value <- events$adjusted_p
  }
  family_map <- stpd_patrol_family_map()
  family <- unname(family_map[provider])
  if (is.na(family)) family <- paste0("provider:", provider)
  semantic_track <- if (identical(label, "pause")) "gap" else "event"
  source <- paste0(provider, ":", table, ":", as.character(source))
  evidence_id <- vapply(seq_len(nrow(events)), function(i) {
    stpd_patrol_hash("stpd-patrol-evidence-v1", c(
      provider, source[[i]], as.character(events$train[[i]]), label,
      as.integer(events$start_isi[[i]]), as.integer(events$end_isi[[i]])
    ))
  }, character(1))
  stpd_patrol_validate_evidence(data.frame(
    evidence_id = evidence_id, provider_key = provider,
    evidence_family = family, source_record_id = source,
    train_key = as.character(events$train), semantic_track = semantic_track,
    proposed_label = label, target_family = stpd_patrol_label_family(label),
    # Support modules index diff(timestamp) from one. The provider/patrol
    # canonical coordinate is the train-row ISI index, whose first valid ISI
    # is row two; therefore the exact mapping is source + 1.
    start_isi = as.integer(events$start_isi) + 1L,
    end_isi = as.integer(events$end_isi) + 1L,
    start_spike = as.integer(start_spike), end_spike = as.integer(end_spike),
    start_time_sec = as.numeric(start_time), end_time_sec = as.numeric(end_time),
    authority_scope = "support_evidence_only", score_name = score_name,
    score_value = as.numeric(score_value), stringsAsFactors = FALSE
  ))
}

stpd_patrol_evidence_from_support <- function(support, provider_key = NULL,
                                              include_pauses = TRUE) {
  if (!is.list(support)) stop("support must be a list.", call. = FALSE)
  provider <- stpd_patrol_infer_provider(support, provider_key)
  pieces <- list(stpd_patrol_support_table(support, "bursts", provider, "burst"))
  if (isTRUE(include_pauses) && "pauses" %in% names(support)) {
    pieces[[length(pieces) + 1L]] <- stpd_patrol_support_table(
      support, "pauses", provider, "pause"
    )
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  stpd_patrol_validate_evidence(out)
}

stpd_patrol_bind_evidence <- function(...) {
  inputs <- list(...)
  if (length(inputs) == 1L && is.list(inputs[[1L]]) &&
      !is.data.frame(inputs[[1L]]) && is.null(inputs[[1L]]$candidate_intervals)) {
    inputs <- inputs[[1L]]
  }
  pieces <- lapply(inputs, function(x) {
    if (is.data.frame(x)) return(stpd_patrol_validate_evidence(x))
    if (is.list(x) && all(c("provider_runs", "candidate_intervals") %in% names(x))) {
      return(stpd_patrol_evidence_from_provider_bundle(x))
    }
    stpd_patrol_evidence_from_support(x)
  })
  if (!length(pieces)) return(stpd_patrol_empty_evidence())
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  stpd_patrol_validate_evidence(out)
}

stpd_patrol_interval_iou <- function(a0, a1, b0, b1) {
  intersection <- max(0L, min(a1, b1) - max(a0, b0) + 1L)
  union <- max(a1, b1) - min(a0, b0) + 1L
  if (union > 0L) intersection / union else 0
}

stpd_patrol_components <- function(evidence, max_gap_isi = 0L) {
  max_gap_isi <- as.integer(max_gap_isi)
  if (length(max_gap_isi) != 1L || is.na(max_gap_isi) || max_gap_isi < 0L) {
    stop("max_gap_isi must be one non-negative integer.", call. = FALSE)
  }
  if (!nrow(evidence)) return(integer())
  group <- interaction(evidence$train_key, evidence$semantic_track,
                       evidence$target_family, drop = TRUE, lex.order = TRUE)
  component <- integer(nrow(evidence))
  next_id <- 0L
  for (indices in split(seq_len(nrow(evidence)), group)) {
    indices <- indices[order(evidence$start_isi[indices], evidence$end_isi[indices],
                             evidence$evidence_id[indices], method = "radix")]
    current_end <- NA_integer_
    for (i in indices) {
      if (is.na(current_end) || evidence$start_isi[[i]] > current_end + max_gap_isi + 1L) {
        next_id <- next_id + 1L
        current_end <- evidence$end_isi[[i]]
      } else {
        current_end <- max(current_end, evidence$end_isi[[i]])
      }
      component[[i]] <- next_id
    }
  }
  component
}

stpd_patrol_pairwise <- function(evidence, cluster_ids) {
  rows <- list()
  for (cluster_id in unique(cluster_ids)) {
    idx <- which(cluster_ids == cluster_id)
    if (length(idx) < 2L) next
    pairs <- utils::combn(idx, 2L)
    for (j in seq_len(ncol(pairs))) {
      a <- evidence[pairs[1L, j], , drop = FALSE]
      b <- evidence[pairs[2L, j], , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        cluster_index = cluster_id,
        evidence_id_a = a$evidence_id, evidence_id_b = b$evidence_id,
        provider_a = a$provider_key, provider_b = b$provider_key,
        same_evidence_family = identical(a$evidence_family, b$evidence_family),
        intersection_isi = max(0L, min(a$end_isi, b$end_isi) -
                                 max(a$start_isi, b$start_isi) + 1L),
        iou = stpd_patrol_interval_iou(a$start_isi, a$end_isi,
                                      b$start_isi, b$end_isi),
        start_delta_isi = abs(a$start_isi - b$start_isi),
        end_delta_isi = abs(a$end_isi - b$end_isi),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(data.frame(
    cluster_index = integer(), evidence_id_a = character(), evidence_id_b = character(),
    provider_a = character(), provider_b = character(), same_evidence_family = logical(),
    intersection_isi = integer(), iou = numeric(), start_delta_isi = integer(),
    end_delta_isi = integer(), stringsAsFactors = FALSE
  ))
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

stpd_patrol_context_for_cluster <- function(cluster, context) {
  empty <- list(labels = character(), nested_hfs = FALSE, semantic_conflict = FALSE)
  if (is.null(context) || !nrow(context)) return(empty)
  hit <- context$train_key == cluster$train_key &
    context$start_isi <= cluster$end_isi & context$end_isi >= cluster$start_isi
  if (!any(hit)) return(empty)
  labels <- sort(unique(stpd_patrol_label_family(context$proposed_label[hit])),
                 method = "radix")
  burst <- identical(cluster$target_family, "burst")
  list(
    labels = labels,
    nested_hfs = burst && "broad_hfs" %in% labels,
    semantic_conflict = burst && "pause" %in% labels
  )
}

#' Build a non-authoritative multi-method patrol report
#'
#' @param evidence Canonical patrol evidence, a provider bundle, a support
#'   result, or a list containing those objects.
#' @param context_intervals Optional canonical evidence used only to flag state
#'   and gap context. A Burst overlapping Broad HFS is nested context, whereas
#'   overlap with Pause is a semantic conflict requiring review.
#' @param agreement_iou Minimum pairwise IoU for aligned boundaries.
#' @param boundary_tolerance_isi Maximum accepted endpoint difference in ISIs.
#' @param max_gap_isi Gap allowed when constructing a review component.
#' @return A `stpd_patrol_report`. It contains no final biological label.
stpd_build_multi_method_patrol <- function(
    evidence,
    context_intervals = NULL,
    agreement_iou = 0.50,
    boundary_tolerance_isi = 1L,
    max_gap_isi = 0L) {
  if (is.data.frame(evidence)) {
    ev <- stpd_patrol_validate_evidence(evidence)
  } else if (is.list(evidence) && all(c("provider_runs", "candidate_intervals") %in% names(evidence))) {
    ev <- stpd_patrol_evidence_from_provider_bundle(evidence)
  } else if (is.list(evidence) && !is.null(evidence$bursts)) {
    ev <- stpd_patrol_evidence_from_support(evidence)
  } else if (is.list(evidence)) {
    ev <- stpd_patrol_bind_evidence(evidence)
  } else {
    stop("Unsupported evidence input.", call. = FALSE)
  }
  agreement_iou <- as.numeric(agreement_iou)
  boundary_tolerance_isi <- as.integer(boundary_tolerance_isi)
  if (length(agreement_iou) != 1L || !is.finite(agreement_iou) ||
      agreement_iou < 0 || agreement_iou > 1) {
    stop("agreement_iou must be in [0, 1].", call. = FALSE)
  }
  if (length(boundary_tolerance_isi) != 1L || is.na(boundary_tolerance_isi) ||
      boundary_tolerance_isi < 0L) {
    stop("boundary_tolerance_isi must be non-negative.", call. = FALSE)
  }
  context <- if (is.null(context_intervals)) NULL else
    stpd_patrol_validate_evidence(context_intervals)
  component <- stpd_patrol_components(ev, max_gap_isi = max_gap_isi)
  pairwise <- stpd_patrol_pairwise(ev, component)
  clusters <- list()
  memberships <- list()
  for (index in unique(component)) {
    z <- ev[component == index, , drop = FALSE]
    provider <- sort(unique(z$provider_key), method = "radix")
    families <- sort(unique(z$evidence_family), method = "radix")
    start <- min(z$start_isi); end <- max(z$end_isi)
    cluster_stub <- list(train_key = z$train_key[[1L]],
                         target_family = z$target_family[[1L]],
                         start_isi = start, end_isi = end)
    ctx <- stpd_patrol_context_for_cluster(cluster_stub, context)
    pw <- pairwise[pairwise$cluster_index == index, , drop = FALSE]
    boundary_disagreement <- nrow(pw) > 0L && any(
      pw$iou < agreement_iou |
        pw$start_delta_isi > boundary_tolerance_isi |
        pw$end_delta_isi > boundary_tolerance_isi
    )
    native <- "native_grammar" %in% families
    source_class <- if (native && length(provider) > 1L) "native_and_auxiliary" else
      if (native) "native_only" else "auxiliary_only"
    status <- if (ctx$semantic_conflict) "semantic_conflict" else
      if (boundary_disagreement) "boundary_disagreement" else
      if (length(provider) == 1L) "single_method_only" else
      if (length(families) == 1L) "within_family_agreement" else
        "cross_family_agreement"
    priority <- if (ctx$semantic_conflict) "critical" else
      if (boundary_disagreement || identical(source_class, "auxiliary_only")) "high" else
      if (length(provider) == 1L) "medium" else "routine"
    lengths <- z$end_isi - z$start_isi + 1L
    chain_warning <- nrow(z) >= 3L && (end - start + 1L) > 2 * stats::median(lengths)
    cluster_id <- stpd_patrol_hash("stpd-patrol-cluster-v1", c(
      z$train_key[[1L]], z$semantic_track[[1L]], z$target_family[[1L]],
      start, end, sort(z$evidence_id, method = "radix")
    ))
    clusters[[length(clusters) + 1L]] <- data.frame(
      patrol_version = STPD_PATROL_VERSION, cluster_id = cluster_id,
      train_key = z$train_key[[1L]], semantic_track = z$semantic_track[[1L]],
      target_family = z$target_family[[1L]], start_isi = start, end_isi = end,
      intersection_start_isi = max(z$start_isi),
      intersection_end_isi = min(z$end_isi),
      method_count = length(provider), evidence_family_count = length(families),
      providers = paste(provider, collapse = ";"),
      evidence_families = paste(families, collapse = ";"),
      source_class = source_class, patrol_status = status,
      review_priority = priority,
      nested_hfs_context = ctx$nested_hfs,
      semantic_conflict = ctx$semantic_conflict,
      context_labels = paste(ctx$labels, collapse = ";"),
      chain_expansion_warning = chain_warning,
      stringsAsFactors = FALSE
    )
    memberships[[length(memberships) + 1L]] <- data.frame(
      cluster_id = cluster_id, evidence_id = z$evidence_id,
      provider_key = z$provider_key, evidence_family = z$evidence_family,
      stringsAsFactors = FALSE
    )
  }
  cluster_table <- if (length(clusters)) do.call(rbind, clusters) else data.frame()
  membership_table <- if (length(memberships)) do.call(rbind, memberships) else data.frame()
  if (nrow(cluster_table)) {
    order_priority <- match(cluster_table$review_priority,
                            c("critical", "high", "medium", "routine"))
    cluster_table <- cluster_table[order(order_priority, cluster_table$train_key,
                                         cluster_table$start_isi,
                                         cluster_table$cluster_id,
                                         method = "radix"), , drop = FALSE]
    rownames(cluster_table) <- NULL
  }
  review_queue <- if (nrow(cluster_table)) {
    cluster_table[cluster_table$review_priority != "routine", , drop = FALSE]
  } else cluster_table
  summary <- data.frame(
    patrol_version = STPD_PATROL_VERSION,
    evidence_n = nrow(ev), cluster_n = nrow(cluster_table),
    review_n = nrow(review_queue),
    critical_n = sum(cluster_table$review_priority == "critical"),
    high_n = sum(cluster_table$review_priority == "high"),
    cross_family_agreement_n = sum(cluster_table$patrol_status == "cross_family_agreement"),
    stringsAsFactors = FALSE
  )
  structure(list(
    version = STPD_PATROL_VERSION,
    scientific_authority = "review_prioritization_only",
    automatic_final_label = FALSE,
    evidence = ev,
    candidate_clusters = cluster_table,
    cluster_membership = membership_table,
    pairwise_disagreement = pairwise,
    review_queue = review_queue,
    summary = summary
  ), class = c("stpd_patrol_report", "list"))
}

stpd_patrol_call_args <- function(base, extra, reserved, method) {
  if (is.null(extra)) extra <- list()
  invalid_names <- length(extra) > 0L &&
    (is.null(names(extra)) || anyNA(names(extra)) ||
       any(!nzchar(names(extra))) || anyDuplicated(names(extra)))
  if (!is.list(extra) || is.object(extra) || invalid_names) {
    stop(method, " arguments must be a named plain list.", call. = FALSE)
  }
  collision <- intersect(names(extra), reserved)
  if (length(collision)) {
    stop(method, " arguments cannot replace orchestration fields: ",
         paste(collision, collapse = ", "), call. = FALSE)
  }
  c(base, extra)
}

#' Run auxiliary methods and construct a patrol report
#'
#' This orchestration function executes existing support methods without
#' changing their parameters or outputs. A failed method contributes no
#' evidence and is explicitly retained in `method_status`.
stpd_run_multi_method_patrol <- function(
    ds,
    params = default_params_sec(),
    selected_trains = NULL,
    native_bundle = NULL,
    native_events = NULL,
    methods = c("mean_isi", "logisi_newbd", "poisson_surprise",
                "robust_gaussian_surprise"),
    method_args = list(),
    include_rgs_pauses = TRUE,
    strict = FALSE,
    retain_support_results = TRUE,
    agreement_iou = 0.50,
    boundary_tolerance_isi = 1L,
    max_gap_isi = 0L) {
  allowed <- c("mean_isi", "logisi_newbd", "poisson_surprise",
               "robust_gaussian_surprise")
  methods <- unique(as.character(methods))
  if (!length(methods) || any(!methods %in% allowed)) {
    stop("methods must be a non-empty subset of: ",
         paste(allowed, collapse = ", "), call. = FALSE)
  }
  if (!is.list(method_args) || is.object(method_args)) {
    stop("method_args must be a plain named list.", call. = FALSE)
  }
  if (is.null(names(method_args))) names(method_args) <- rep("", length(method_args))
  unknown_args <- setdiff(names(method_args)[nzchar(names(method_args))], allowed)
  if (length(unknown_args)) {
    stop("Unknown method_args entries: ", paste(unknown_args, collapse = ", "),
         call. = FALSE)
  }
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict) ||
      !is.logical(retain_support_results) || length(retain_support_results) != 1L ||
      is.na(retain_support_results)) {
    stop("strict and retain_support_results must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(native_bundle) && !is.null(native_events)) {
    stop("Supply native_bundle or native_events, not both.", call. = FALSE)
  }

  runners <- list(
    mean_isi = function(extra) do.call(
      stpd_misi_support_dataset,
      stpd_patrol_call_args(
        list(ds = ds, params = params, selected_trains = selected_trains), extra,
        c("ds", "params", "selected_trains"), "mean_isi"
      )
    ),
    logisi_newbd = function(extra) do.call(
      stpd_logisi_support_dataset,
      stpd_patrol_call_args(
        list(ds = ds, params = params, selected_trains = selected_trains), extra,
        c("ds", "params", "selected_trains"), "logisi_newbd"
      )
    ),
    poisson_surprise = function(extra) do.call(
      stpd_poisson_surprise_support_dataset,
      stpd_patrol_call_args(
        list(ds = ds, selected_trains = selected_trains), extra,
        c("ds", "selected_trains"), "poisson_surprise"
      )
    ),
    robust_gaussian_surprise = function(extra) do.call(
      stpd_rgs_support_dataset,
      stpd_patrol_call_args(
        list(ds = ds, selected_trains = selected_trains), extra,
        c("ds", "selected_trains"), "robust_gaussian_surprise"
      )
    )
  )
  supports <- list()
  evidence_parts <- list()
  status_rows <- list()
  if (!is.null(native_bundle)) {
    native_evidence <- stpd_patrol_evidence_from_provider_bundle(native_bundle)
    evidence_parts[["native_stpd"]] <- native_evidence
    status_rows[["native_stpd"]] <- data.frame(
      method = "native_stpd", status = "complete", error_message = "",
      evidence_n = nrow(native_evidence), stringsAsFactors = FALSE
    )
  } else if (!is.null(native_events)) {
    native_evidence <- stpd_patrol_evidence_from_native_events(native_events)
    evidence_parts[["native_stpd"]] <- native_evidence
    status_rows[["native_stpd"]] <- data.frame(
      method = "native_stpd", status = "complete", error_message = "",
      evidence_n = nrow(native_evidence), stringsAsFactors = FALSE
    )
  }
  for (method in methods) {
    extra <- method_args[[method]] %||% list()
    result <- tryCatch(runners[[method]](extra), error = identity)
    if (inherits(result, "error")) {
      status_rows[[method]] <- data.frame(
        method = method, status = "rejected",
        error_message = conditionMessage(result), evidence_n = 0L,
        stringsAsFactors = FALSE
      )
      if (isTRUE(strict)) {
        stop("Patrol method ", method, " failed: ", conditionMessage(result),
             call. = FALSE)
      }
      next
    }
    supports[[method]] <- result
    ev <- stpd_patrol_evidence_from_support(
      result, provider_key = method,
      include_pauses = isTRUE(include_rgs_pauses)
    )
    evidence_parts[[method]] <- ev
    status_rows[[method]] <- data.frame(
      method = method, status = "complete", error_message = "",
      evidence_n = nrow(ev), stringsAsFactors = FALSE
    )
  }
  evidence <- if (length(evidence_parts)) {
    do.call(stpd_patrol_bind_evidence, unname(evidence_parts))
  } else stpd_patrol_empty_evidence()
  context <- evidence[evidence$semantic_track %in% c("state", "gap"), , drop = FALSE]
  report <- stpd_build_multi_method_patrol(
    evidence, context_intervals = context,
    agreement_iou = agreement_iou,
    boundary_tolerance_isi = boundary_tolerance_isi,
    max_gap_isi = max_gap_isi
  )
  report$method_status <- do.call(rbind, status_rows)
  rownames(report$method_status) <- NULL
  report$requested_methods <- methods
  report$support_results <- if (isTRUE(retain_support_results)) supports else NULL
  report
}
