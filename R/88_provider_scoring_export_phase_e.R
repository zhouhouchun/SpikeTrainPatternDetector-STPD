# Phase E: normalized provider scoring and transactional export --------------

STPD_PROVIDER_REFERENCE_VERSION <- "stpd_provider_reference_bundle_v1"
STPD_PROVIDER_SCORE_VERSION <- "stpd_provider_score_product_v1"

stpd_provider_reference_empty <- function() {
  list(
    metadata = data.frame(
      schema_version = character(), reference_id = character(),
      dataset_snapshot_sha256 = character(), authority_scope = character(),
      information_access = character(), blinded_to_provider = logical(),
      annotation_provenance = character(), created_utc = character(),
      interval_n = integer(), product_sha256 = character(),
      stringsAsFactors = FALSE
    ),
    intervals = data.frame(
      reference_interval_id = character(), train_key = character(),
      semantic_track = character(), label = character(),
      canonical_start_isi = integer(), canonical_end_isi = integer(),
      stringsAsFactors = FALSE
    ),
    manifest = data.frame(
      schema_version = character(), table_name = character(),
      row_count = integer(), column_count = integer(),
      column_types = character(), table_sha256 = character(),
      stringsAsFactors = FALSE
    )
  )
}

stpd_provider_scoring_manifest <- function(tables, schema_version, domain) {
  data.frame(
    schema_version = schema_version, table_name = names(tables),
    row_count = vapply(tables, nrow, integer(1)),
    column_count = vapply(tables, ncol, integer(1)),
    column_types = vapply(tables, function(x) paste(
      paste(names(x), vapply(x, typeof, character(1)), sep = ":"),
      collapse = ";"
    ), character(1)),
    table_sha256 = vapply(tables, function(x) stpd_provider_hash_domain(
      domain, x
    ), character(1)), stringsAsFactors = FALSE
  )
}

stpd_provider_reference_validate_intervals <- function(intervals) {
  prototype <- stpd_provider_reference_empty()$intervals
  if (!is.data.frame(intervals) || !identical(names(intervals), names(prototype)) ||
      any(vapply(names(prototype), function(name) {
        !identical(typeof(intervals[[name]]), typeof(prototype[[name]]))
      }, logical(1)))) {
    stpd_provider_workbench_abort(
      "provider_reference_schema_invalid",
      "Reference intervals must use the exact ordered typed v1 schema."
    )
  }
  text_fields <- c(
    "reference_interval_id", "train_key", "semantic_track", "label"
  )
  if (anyDuplicated(intervals$reference_interval_id) ||
      any(vapply(text_fields, function(name) {
        anyNA(intervals[[name]]) || any(!nzchar(intervals[[name]]))
      }, logical(1))) || anyNA(intervals$canonical_start_isi) ||
      anyNA(intervals$canonical_end_isi) ||
      any(intervals$canonical_start_isi < 1L) ||
      any(intervals$canonical_end_isi < intervals$canonical_start_isi)) {
    stpd_provider_workbench_abort(
      "provider_reference_geometry_invalid",
      "Reference identity or canonical ISI geometry is invalid."
    )
  }
  allowed <- list(
    event = c("burst", "long_burst"),
    state = c("broad_hfs", "hft", "hf_irregular", "tonic"),
    gap = "pause"
  )
  ok <- vapply(seq_len(nrow(intervals)), function(i) {
    track <- intervals$semantic_track[[i]]
    track %in% names(allowed) && intervals$label[[i]] %in% allowed[[track]]
  }, logical(1))
  if (any(!ok)) stpd_provider_workbench_abort(
    "provider_reference_ontology_invalid",
    "A reference label does not belong to its Event/State/Gap track."
  )
  groups <- split(
    seq_len(nrow(intervals)),
    paste(intervals$train_key, intervals$semantic_track, intervals$label,
          sep = "\u001f")
  )
  for (index in groups) {
    x <- intervals[index, , drop = FALSE]
    x <- x[order(x$canonical_start_isi, x$canonical_end_isi,
                 x$reference_interval_id, method = "radix"), , drop = FALSE]
    if (nrow(x) > 1L && any(
      x$canonical_start_isi[-1L] <=
        x$canonical_end_isi[-nrow(x)]
    )) stpd_provider_workbench_abort(
      "provider_reference_within_label_overlap",
      "Reference intervals cannot overlap within one train/track/label."
    )
  }
  out <- intervals[order(
    intervals$train_key, intervals$semantic_track, intervals$label,
    intervals$canonical_start_isi, intervals$canonical_end_isi,
    intervals$reference_interval_id, method = "radix"
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Create a sealed provider-scoring reference bundle
#'
#' @param intervals Exact typed reference interval table.
#' @param reference_id A stable pseudonymous reference identifier.
#' @param dataset_snapshot_sha256 Exact provider dataset snapshot hash.
#' @param authority_scope `independent_reference_standard` or
#'   `adjudicated_reference_record`.
#' @param information_access Reference access declaration.
#' @param blinded_to_provider Whether annotation was blinded to provider output.
#' @param annotation_provenance Bounded disclosure of annotation provenance.
#' @param created_utc Explicit RFC3339 UTC creation time.
#' @return A hashed reference bundle v1.
#' @export
stpd_provider_reference_bundle <- function(
    intervals, reference_id, dataset_snapshot_sha256,
    authority_scope = c("independent_reference_standard",
                        "adjudicated_reference_record"),
    information_access = c("blinded_to_predictions",
                           "not_blinded_to_predictions", "unknown"),
    blinded_to_provider, annotation_provenance, created_utc) {
  intervals <- stpd_provider_reference_validate_intervals(intervals)
  reference_id <- stpd_provider_composer_text(reference_id, "reference_id", 256L)
  dataset_snapshot_sha256 <- stpd_provider_composer_text(
    dataset_snapshot_sha256, "dataset_snapshot_sha256", 64L
  )
  if (!grepl("^[0-9a-f]{64}$", dataset_snapshot_sha256)) {
    stpd_provider_workbench_abort(
      "provider_reference_identity_invalid",
      "dataset_snapshot_sha256 must be a lowercase SHA-256."
    )
  }
  authority_scope <- match.arg(authority_scope)
  information_access <- match.arg(information_access)
  if (!is.logical(blinded_to_provider) || length(blinded_to_provider) != 1L ||
      is.na(blinded_to_provider)) stpd_provider_workbench_abort(
    "provider_reference_authority_invalid",
    "blinded_to_provider must be one explicit logical value."
  )
  annotation_provenance <- stpd_provider_composer_text(
    annotation_provenance, "annotation_provenance", 4096L
  )
  created_utc <- stpd_provider_composer_text(created_utc, "created_utc", 64L)
  if (!grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$",
    created_utc
  )) stpd_provider_workbench_abort(
    "provider_reference_identity_invalid", "created_utc must be RFC3339 UTC."
  )
  metadata <- data.frame(
    schema_version = STPD_PROVIDER_REFERENCE_VERSION,
    reference_id = reference_id,
    dataset_snapshot_sha256 = dataset_snapshot_sha256,
    authority_scope = authority_scope,
    information_access = information_access,
    blinded_to_provider = blinded_to_provider,
    annotation_provenance = annotation_provenance, created_utc = created_utc,
    interval_n = as.integer(nrow(intervals)), product_sha256 = NA_character_,
    stringsAsFactors = FALSE
  )
  metadata$product_sha256 <- stpd_provider_hash_domain(
    "stpd-provider-reference-product-v1",
    list(metadata = metadata[setdiff(names(metadata), "product_sha256")],
         intervals = intervals)
  )
  tables <- list(metadata = metadata, intervals = intervals)
  structure(
    c(tables, list(manifest = stpd_provider_scoring_manifest(
      tables, STPD_PROVIDER_REFERENCE_VERSION,
      "stpd-provider-reference-table-v1"
    ))),
    class = c("stpd_provider_reference_bundle_v1", "list")
  )
}

#' Validate a sealed provider-scoring reference bundle
#'
#' @param reference A sealed provider reference bundle v1.
#' @return Invisibly `TRUE` or a typed fail-closed error.
#' @export
stpd_validate_provider_reference_bundle <- function(reference) {
  prototype <- stpd_provider_reference_empty()
  if (!inherits(reference, "stpd_provider_reference_bundle_v1") ||
      !identical(names(reference), names(prototype))) {
    stpd_provider_workbench_abort(
      "provider_reference_schema_invalid", "Reference bundle is incomplete."
    )
  }
  for (name in names(prototype)) {
    if (!is.data.frame(reference[[name]]) ||
        !identical(names(reference[[name]]), names(prototype[[name]])) ||
        any(vapply(names(prototype[[name]]), function(column) {
          !identical(typeof(reference[[name]][[column]]),
                     typeof(prototype[[name]][[column]]))
        }, logical(1)))) stpd_provider_workbench_abort(
      "provider_reference_schema_invalid",
      paste0("Invalid reference table: ", name, ".")
    )
  }
  metadata <- reference$metadata
  if (nrow(metadata) != 1L ||
      !identical(metadata$schema_version[[1L]], STPD_PROVIDER_REFERENCE_VERSION) ||
      metadata$interval_n[[1L]] != nrow(reference$intervals)) {
    stpd_provider_workbench_abort(
      "provider_reference_identity_invalid", "Reference metadata is stale."
    )
  }
  if (!(metadata$authority_scope[[1L]] %in% c(
        "independent_reference_standard", "adjudicated_reference_record"
      )) || !(metadata$information_access[[1L]] %in% c(
        "blinded_to_predictions", "not_blinded_to_predictions", "unknown"
      )) || !is.logical(metadata$blinded_to_provider) ||
      length(metadata$blinded_to_provider) != 1L ||
      is.na(metadata$blinded_to_provider[[1L]]) ||
      !grepl("^[0-9a-f]{64}$", metadata$dataset_snapshot_sha256[[1L]]) ||
      !grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$",
        metadata$created_utc[[1L]]
      ) || is.na(suppressWarnings(as.POSIXct(
        sub("Z$", "", metadata$created_utc[[1L]]),
        format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
      ))) || any(!nzchar(c(metadata$reference_id[[1L]],
                           metadata$annotation_provenance[[1L]])))) {
    stpd_provider_workbench_abort(
      "provider_reference_authority_invalid",
      "Reference authority, provenance, dataset identity, or audit time is invalid."
    )
  }
  normalized <- stpd_provider_reference_validate_intervals(reference$intervals)
  if (!identical(normalized, reference$intervals)) stpd_provider_workbench_abort(
    "provider_reference_geometry_invalid", "Reference ordering is noncanonical."
  )
  expected_hash <- stpd_provider_hash_domain(
    "stpd-provider-reference-product-v1",
    list(metadata = metadata[setdiff(names(metadata), "product_sha256")],
         intervals = reference$intervals)
  )
  tables <- reference[c("metadata", "intervals")]
  if (!identical(metadata$product_sha256[[1L]], expected_hash) ||
      !identical(reference$manifest, stpd_provider_scoring_manifest(
        tables, STPD_PROVIDER_REFERENCE_VERSION,
        "stpd-provider-reference-table-v1"
      ))) stpd_provider_workbench_abort(
    "provider_reference_manifest_invalid", "Reference hashes are stale."
  )
  invisible(TRUE)
}

stpd_provider_score_validate_authority <- function(view, reference, estimand) {
  if (!is.character(estimand) || length(estimand) != 1L || is.na(estimand) ||
      !(estimand %in% c("detector_performance", "adjudicated_agreement"))) {
    stpd_provider_workbench_abort(
      "provider_score_estimand_authority_invalid", "Unknown scoring estimand."
    )
  }
  if (estimand == "detector_performance") {
    eligible <- view$metadata$source_mode[[1L]] == "auto" &&
      view$metadata$information_access[[1L]] == "label_blind" &&
      view$metadata$authority_scope[[1L]] == "automatic_prediction_record" &&
      reference$metadata$authority_scope[[1L]] ==
        "independent_reference_standard" &&
      isTRUE(reference$metadata$blinded_to_provider[[1L]]) &&
      reference$metadata$information_access[[1L]] == "blinded_to_predictions"
    if (!eligible) stpd_provider_workbench_abort(
      "provider_score_estimand_authority_invalid",
      "Detector performance requires label-blind AUTO and an independent blinded reference."
    )
  } else {
    eligible <- view$metadata$source_mode[[1L]] == "adjudicated" &&
      view$metadata$performance_use[[1L]] == "adjudicated_agreement_only"
    if (!eligible) stpd_provider_workbench_abort(
      "provider_score_estimand_authority_invalid",
      "Adjudicated agreement requires an adjudicated prediction source."
    )
  }
  invisible(TRUE)
}

stpd_provider_score_empty <- function() {
  list(
    metadata = data.frame(
      schema_version = character(), provider_run_id = character(),
      source_mode = character(), prediction_product_sha256 = character(),
      reference_id = character(), reference_product_sha256 = character(),
      dataset_snapshot_sha256 = character(), estimand = character(),
      iou_threshold = double(), matching_rule = character(),
      label_pooling = logical(), provider_pooling = logical(),
      prediction_n = integer(), reference_n = integer(), match_n = integer(),
      product_sha256 = character(), stringsAsFactors = FALSE
    ),
    predictions = stpd_multitrack_validation_empty_predictions(),
    references = data.frame(
      truth_interval_id = character(), train = character(),
      semantic_track = character(), label = character(),
      start_isi = integer(), end_isi = integer(), stringsAsFactors = FALSE
    ),
    matches = stpd_multitrack_validation_empty_matches(),
    metrics_by_label = stpd_multitrack_validation_empty_metrics_by_label(),
    coverage_by_label = stpd_multitrack_validation_empty_coverage(),
    fragmentation = data.frame(
      truth_interval_id = character(), train = character(),
      semantic_track = character(), label = character(),
      overlapping_prediction_n = integer(), false_split = logical(),
      best_iou = double(), stringsAsFactors = FALSE
    ),
    manifest = data.frame(
      schema_version = character(), table_name = character(),
      row_count = integer(), column_count = integer(),
      column_types = character(), table_sha256 = character(),
      stringsAsFactors = FALSE
    )
  )
}

stpd_provider_score_inputs <- function(view, reference) {
  predictions <- view$selected_intervals
  predictions <- if (!nrow(predictions)) {
    stpd_multitrack_validation_empty_predictions()
  } else data.frame(
    prediction_interval_id = predictions$selected_interval_id,
    train = predictions$train_key,
    semantic_track = predictions$semantic_track, label = predictions$label,
    start_isi = predictions$canonical_start_isi,
    end_isi = predictions$canonical_end_isi,
    n_isi = predictions$canonical_end_isi - predictions$canonical_start_isi + 1L,
    prediction_source = rep(view$metadata$source_mode[[1L]], nrow(predictions)),
    prediction_origin = predictions$geometry_origin, stringsAsFactors = FALSE
  )
  truth <- reference$intervals
  truth <- data.frame(
    truth_interval_id = truth$reference_interval_id, train = truth$train_key,
    semantic_track = truth$semantic_track, label = truth$label,
    start_isi = truth$canonical_start_isi, end_isi = truth$canonical_end_isi,
    stringsAsFactors = FALSE
  )
  list(predictions = predictions, truth = truth)
}

stpd_provider_score_fragmentation <- function(predictions, truth) {
  prototype <- stpd_provider_score_empty()$fragmentation
  if (!nrow(truth)) return(prototype)
  rows <- lapply(seq_len(nrow(truth)), function(i) {
    ref <- truth[i, , drop = FALSE]
    candidates <- predictions[
      predictions$train == ref$train[[1L]] &
        predictions$semantic_track == ref$semantic_track[[1L]] &
        predictions$label == ref$label[[1L]], , drop = FALSE
    ]
    iou <- if (!nrow(candidates)) numeric() else vapply(
      seq_len(nrow(candidates)), function(j) {
        stpd_multitrack_validation_iou_payload(
          candidates$start_isi[[j]], candidates$end_isi[[j]],
          ref$start_isi[[1L]], ref$end_isi[[1L]]
        )$iou
      }, double(1)
    )
    overlapping <- sum(iou > 0)
    data.frame(
      truth_interval_id = ref$truth_interval_id[[1L]],
      train = ref$train[[1L]], semantic_track = ref$semantic_track[[1L]],
      label = ref$label[[1L]], overlapping_prediction_n = as.integer(overlapping),
      false_split = overlapping > 1L,
      best_iou = if (length(iou)) max(iou) else 0,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(prototype, rows)
}

stpd_provider_score_build <- function(view, reference, estimand,
                                      iou_threshold) {
  inputs <- stpd_provider_score_inputs(view, reference)
  matches <- stpd_multitrack_validation_match(
    inputs$predictions, inputs$truth, iou_threshold
  )
  metrics <- stpd_multitrack_validation_metrics_by_label(
    inputs$predictions, inputs$truth, matches
  )
  coverage <- stpd_multitrack_validation_coverage(
    inputs$predictions, inputs$truth
  )
  fragmentation <- stpd_provider_score_fragmentation(
    inputs$predictions, inputs$truth
  )
  metadata <- data.frame(
    schema_version = STPD_PROVIDER_SCORE_VERSION,
    provider_run_id = view$metadata$provider_run_id[[1L]],
    source_mode = view$metadata$source_mode[[1L]],
    prediction_product_sha256 = view$metadata$product_sha256[[1L]],
    reference_id = reference$metadata$reference_id[[1L]],
    reference_product_sha256 = reference$metadata$product_sha256[[1L]],
    dataset_snapshot_sha256 = reference$metadata$dataset_snapshot_sha256[[1L]],
    estimand = estimand, iou_threshold = as.double(iou_threshold),
    matching_rule = "ordered_dp_max_cardinality_then_total_iou_v1",
    label_pooling = FALSE, provider_pooling = FALSE,
    prediction_n = as.integer(nrow(inputs$predictions)),
    reference_n = as.integer(nrow(inputs$truth)),
    match_n = as.integer(nrow(matches)), product_sha256 = NA_character_,
    stringsAsFactors = FALSE
  )
  payload <- list(
    predictions = inputs$predictions, references = inputs$truth,
    matches = matches, metrics_by_label = metrics,
    coverage_by_label = coverage, fragmentation = fragmentation
  )
  metadata$product_sha256 <- stpd_provider_hash_domain(
    "stpd-provider-score-product-v1",
    list(metadata = metadata[setdiff(names(metadata), "product_sha256")],
         payload = payload)
  )
  tables <- c(list(metadata = metadata), payload)
  structure(
    c(tables, list(manifest = stpd_provider_scoring_manifest(
      tables, STPD_PROVIDER_SCORE_VERSION, "stpd-provider-score-table-v1"
    ))), class = c("stpd_provider_score_product_v1", "list")
  )
}

#' Score one normalized provider view against one explicit reference
#'
#' @param provider_bundle,composition,view Exact Phase B2, D, and E parents.
#' @param reference A sealed provider reference bundle v1.
#' @param estimand `detector_performance` or `adjudicated_agreement`.
#' @param iou_threshold Event matching IoU in (0, 1].
#' @param adjudication Exact Phase C parent when applicable.
#' @return A track-and-label-specific score product without provider pooling.
#' @export
stpd_score_provider_view <- function(
    provider_bundle, composition, view, reference,
    estimand = c("detector_performance", "adjudicated_agreement"),
    iou_threshold = 0.5, adjudication = NULL) {
  stpd_validate_provider_review_view(
    provider_bundle, composition, view, adjudication
  )
  stpd_validate_provider_reference_bundle(reference)
  estimand <- match.arg(estimand)
  if (!is.numeric(iou_threshold) || length(iou_threshold) != 1L ||
      is.na(iou_threshold) || !is.finite(iou_threshold) ||
      iou_threshold <= 0 || iou_threshold > 1) stpd_provider_workbench_abort(
    "provider_score_iou_invalid", "iou_threshold must be in (0, 1]."
  )
  selected <- view$provider_catalog[view$provider_catalog$selected, , drop = FALSE]
  if (!identical(selected$dataset_snapshot_sha256[[1L]],
                 reference$metadata$dataset_snapshot_sha256[[1L]])) {
    stpd_provider_workbench_abort(
      "provider_score_dataset_mismatch",
      "Prediction and reference dataset snapshots differ."
    )
  }
  stpd_provider_score_validate_authority(view, reference, estimand)
  out <- stpd_provider_score_build(
    view, reference, estimand, as.double(iou_threshold)
  )
  stpd_validate_provider_score(
    provider_bundle, composition, view, reference, out,
    adjudication = adjudication
  )
  out
}

#' Validate a normalized provider score product
#' @inheritParams stpd_score_provider_view
#' @param score A provider score product v1.
#' @return Invisibly `TRUE` or a typed error.
#' @export
stpd_validate_provider_score <- function(
    provider_bundle, composition, view, reference, score,
    adjudication = NULL) {
  stpd_validate_provider_review_view(
    provider_bundle, composition, view, adjudication
  )
  stpd_validate_provider_reference_bundle(reference)
  prototype <- stpd_provider_score_empty()
  if (!inherits(score, "stpd_provider_score_product_v1") ||
      !identical(names(score), names(prototype))) stpd_provider_workbench_abort(
    "provider_score_schema_invalid", "Score product is incomplete."
  )
  for (name in names(prototype)) {
    if (!is.data.frame(score[[name]]) ||
        !identical(names(score[[name]]), names(prototype[[name]])) ||
        any(vapply(names(prototype[[name]]), function(column) {
          !identical(typeof(score[[name]][[column]]),
                     typeof(prototype[[name]][[column]]))
        }, logical(1)))) stpd_provider_workbench_abort(
      "provider_score_schema_invalid", paste0("Invalid score table: ", name, ".")
    )
  }
  metadata <- score$metadata
  if (nrow(metadata) != 1L || metadata$label_pooling[[1L]] ||
      metadata$provider_pooling[[1L]] ||
      !identical(metadata$provider_run_id[[1L]],
                 view$metadata$provider_run_id[[1L]]) ||
      !identical(metadata$prediction_product_sha256[[1L]],
                 view$metadata$product_sha256[[1L]]) ||
      !identical(metadata$reference_product_sha256[[1L]],
                 reference$metadata$product_sha256[[1L]])) {
    stpd_provider_workbench_abort(
      "provider_score_identity_invalid", "Score identity or pooling flags are invalid."
    )
  }
  stpd_provider_score_validate_authority(
    view, reference, metadata$estimand[[1L]]
  )
  expected <- stpd_provider_score_build(
    view, reference, metadata$estimand[[1L]], metadata$iou_threshold[[1L]]
  )
  if (!identical(score, expected)) stpd_provider_workbench_abort(
    "provider_score_manifest_invalid",
    "Score product failed deterministic rematerialization."
  )
  invisible(TRUE)
}

#' Transactionally export a provider workbench view and optional score
#'
#' @param out_dir A new output directory; existing paths fail closed.
#' @param score Optional validated score product.
#' @inheritParams stpd_score_provider_view
#' @return Invisibly, the normalized finalized directory.
#' @export
stpd_write_provider_workbench <- function(
    provider_bundle, composition, view, out_dir, adjudication = NULL,
    score = NULL, reference = NULL) {
  stpd_validate_provider_review_view(
    provider_bundle, composition, view, adjudication
  )
  if (xor(is.null(score), is.null(reference))) stpd_provider_workbench_abort(
    "provider_export_score_reference_pair_invalid",
    "score and reference must be supplied together."
  )
  if (!is.null(score)) stpd_validate_provider_score(
    provider_bundle, composition, view, reference, score, adjudication
  )
  if (!is.character(out_dir) || length(out_dir) != 1L || is.na(out_dir) ||
      !nzchar(out_dir)) stpd_provider_workbench_abort(
    "provider_export_path_invalid", "out_dir must be one path."
  )
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  if (file.exists(out_dir) || dir.exists(out_dir)) stpd_provider_workbench_abort(
    "provider_export_path_exists", "out_dir already exists."
  )
  parent <- dirname(out_dir)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE)) {
    stpd_provider_workbench_abort(
      "provider_export_parent_failed", "Could not create export parent."
    )
  }
  stage <- tempfile("stpd-provider-workbench-", tmpdir = parent)
  if (!dir.create(stage)) stpd_provider_workbench_abort(
    "provider_export_stage_failed", "Could not create export staging directory."
  )
  complete <- FALSE
  on.exit(if (!complete && dir.exists(stage)) unlink(stage, recursive = TRUE),
          add = TRUE)
  saveRDS(provider_bundle, file.path(stage, "provider_bundle.rds"), version = 3L)
  saveRDS(composition, file.path(stage, "provider_composition.rds"), version = 3L)
  if (!is.null(adjudication)) saveRDS(
    adjudication, file.path(stage, "provider_adjudication.rds"), version = 3L
  )
  saveRDS(view, file.path(stage, "provider_workbench_view.rds"), version = 3L)
  view_tables <- setdiff(names(view), "manifest")
  for (name in view_tables) utils::write.csv(
    view[[name]], file.path(stage, paste0("view_", name, ".csv")),
    row.names = FALSE, na = ""
  )
  utils::write.csv(view$manifest, file.path(stage, "view_manifest.csv"),
                   row.names = FALSE, na = "")
  if (!is.null(score)) {
    saveRDS(score, file.path(stage, "provider_score.rds"), version = 3L)
    saveRDS(reference, file.path(stage, "provider_reference.rds"), version = 3L)
    utils::write.csv(reference$metadata,
                     file.path(stage, "reference_metadata.csv"),
                     row.names = FALSE, na = "")
    for (name in setdiff(names(score), "manifest")) utils::write.csv(
      score[[name]], file.path(stage, paste0("score_", name, ".csv")),
      row.names = FALSE, na = ""
    )
    utils::write.csv(score$manifest, file.path(stage, "score_manifest.csv"),
                     row.names = FALSE, na = "")
  }
  files <- sort(list.files(stage), method = "radix")
  file_hashes <- setNames(vapply(files, function(name) digest::digest(
    file.path(stage, name), algo = "sha256", file = TRUE, serialize = FALSE
  ), character(1)), files)
  completion <- list(
    schema_version = "stpd_provider_workbench_export_v1",
    provider_run_id = view$metadata$provider_run_id[[1L]],
    source_mode = view$metadata$source_mode[[1L]],
    view_product_sha256 = view$metadata$product_sha256[[1L]],
    score_product_sha256 = if (is.null(score)) NULL else
      score$metadata$product_sha256[[1L]],
    completion_status = "complete_after_all_payloads_verified",
    files = as.list(file_hashes)
  )
  writeLines(jsonlite::toJSON(completion, auto_unbox = TRUE, pretty = TRUE),
             file.path(stage, "completion_manifest.json"), useBytes = TRUE)
  if (!file.rename(stage, out_dir)) stpd_provider_workbench_abort(
    "provider_export_finalize_failed", "Could not atomically finalize export."
  )
  complete <- TRUE
  invisible(normalizePath(out_dir, mustWork = TRUE))
}
