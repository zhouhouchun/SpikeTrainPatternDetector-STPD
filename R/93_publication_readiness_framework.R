# Publication-readiness 12-stage framework --------------------------------
#
# This module is governance-only. It validates sequencing, declared outputs
# and review gates; it cannot grant scientific, Gate B, Gate C or publication
# authority and it never calls the detector.

stpd_publication_readiness_framework_path <- function() {
  installed <- system.file(
    "config", "publication_readiness_framework_v1.json",
    package = "SpikeTrainPatternDetector"
  )
  if (nzchar(installed)) return(installed)
  file.path("inst", "config", "publication_readiness_framework_v1.json")
}

stpd_publication_readiness_packets_path <- function() {
  installed <- system.file(
    "config", "publication_readiness_stage_packets_v1.json",
    package = "SpikeTrainPatternDetector"
  )
  if (nzchar(installed)) return(installed)
  file.path("inst", "config", "publication_readiness_stage_packets_v1.json")
}

stpd_publication_readiness_receipts_path <- function() {
  installed <- system.file(
    "config", "publication_readiness_review_receipts_v1.json",
    package = "SpikeTrainPatternDetector"
  )
  if (nzchar(installed)) return(installed)
  file.path("inst", "config", "publication_readiness_review_receipts_v1.json")
}

stpd_publication_readiness_required_stage_ids <- function() {
  sprintf("PR-%02d", seq_len(12L))
}

stpd_publication_readiness_required_gate1b_roots <- function() {
  c(
    "final_gap_arbitration_materialization",
    "recurrent_pause_state",
    "broad_hfs_state",
    "tonic_state",
    "nested_hfs_review",
    "complete_candidate_universe_release"
  )
}

stpd_publication_readiness_scalar <- function(x, field) {
  value <- x[[field]]
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    stop("Publication-readiness field is not one non-empty string: ", field,
         call. = FALSE)
  }
  value
}

stpd_publication_readiness_strings <- function(x, field, allow_empty = FALSE) {
  value <- unlist(x[[field]], use.names = FALSE)
  if (length(value) == 0L && isTRUE(allow_empty)) return(character())
  if (!is.character(value) || anyNA(value) || any(!nzchar(value)) ||
      (!allow_empty && length(value) == 0L)) {
    stop("Publication-readiness field is not a valid string list: ", field,
         call. = FALSE)
  }
  value
}

stpd_publication_readiness_unique_strings <- function(x, field,
                                                       allow_empty = FALSE,
                                                       minimum = 1L) {
  value <- stpd_publication_readiness_strings(x, field, allow_empty)
  if (length(value) < minimum || anyDuplicated(value)) {
    stop("Publication-readiness field is too short or duplicated: ", field,
         call. = FALSE)
  }
  value
}

stpd_publication_readiness_validate <- function(framework) {
  required_top <- c(
    "schema", "framework_version", "authority_scope", "stage_count",
    "current_stage_id", "completed_foundations", "stages",
    "gate1b_remaining_roots"
  )
  if (!is.list(framework) ||
      !identical(sort(names(framework)), sort(required_top))) {
    stop("Publication-readiness framework is structurally incomplete.",
         call. = FALSE)
  }
  if (!identical(framework$schema,
                 "stpd_publication_readiness_framework_v1") ||
      !identical(framework$framework_version, "1.2.0") ||
      !identical(framework$authority_scope,
                 "framework_only_no_scientific_or_publication_authority") ||
      !identical(as.integer(framework$stage_count), 12L)) {
    stop("Publication-readiness framework identity or authority is invalid.",
         call. = FALSE)
  }

  stages <- framework$stages
  expected_ids <- stpd_publication_readiness_required_stage_ids()
  if (!is.list(stages) || length(stages) != length(expected_ids)) {
    stop("Publication-readiness framework must contain exactly 12 stages.",
         call. = FALSE)
  }
  required_stage <- c(
    "stage_id", "title", "depends_on", "framework_status",
    "review_status", "scientific_completion_status", "authority_scope",
    "required_outputs", "stop_checks", "deferred_details"
  )
  if (!all(vapply(stages, function(stage) {
    is.list(stage) && identical(sort(names(stage)), sort(required_stage))
  }, logical(1)))) {
    stop("At least one publication-readiness stage is incomplete.",
         call. = FALSE)
  }
  stage_ids <- vapply(stages, stpd_publication_readiness_scalar, character(1),
                      field = "stage_id")
  if (!identical(stage_ids, expected_ids) || anyDuplicated(stage_ids)) {
    stop("Publication-readiness stage IDs or order are invalid.", call. = FALSE)
  }

  allowed_framework <- c(
    "framework_pending", "ready_for_review", "reviewed_framework"
  )
  allowed_review <- c("not_started", "pending", "passed")
  allowed_science <- c("pending", "partial_existing_foundation")
  for (i in seq_along(stages)) {
    stage <- stages[[i]]
    title <- stpd_publication_readiness_scalar(stage, "title")
    status <- stpd_publication_readiness_scalar(stage, "framework_status")
    review <- stpd_publication_readiness_scalar(stage, "review_status")
    science <- stpd_publication_readiness_scalar(
      stage, "scientific_completion_status")
    authority <- stpd_publication_readiness_scalar(stage, "authority_scope")
    if (!(status %in% allowed_framework) || !(review %in% allowed_review) ||
        !(science %in% allowed_science) ||
        !identical(authority,
                   "none_framework_only_no_completion_claim")) {
      stop("Publication-readiness status or authority is invalid for ", title,
           call. = FALSE)
    }
    if ((identical(status, "reviewed_framework") &&
         !identical(review, "passed")) ||
        (identical(status, "ready_for_review") &&
         !identical(review, "pending")) ||
        (identical(status, "framework_pending") &&
         !identical(review, "not_started"))) {
      stop("Publication-readiness framework/review states disagree for ",
           stage_ids[[i]], call. = FALSE)
    }
    dependencies <- stpd_publication_readiness_strings(
      stage, "depends_on", allow_empty = TRUE)
    expected_dependency <- if (i == 1L) character() else stage_ids[[i - 1L]]
    if (!identical(dependencies, expected_dependency)) {
      stop("Publication-readiness stages must follow the frozen serial order.",
           call. = FALSE)
    }
    stpd_publication_readiness_unique_strings(
      stage, "required_outputs", minimum = 3L)
    stpd_publication_readiness_unique_strings(
      stage, "stop_checks", minimum = 3L)
    stpd_publication_readiness_unique_strings(
      stage, "deferred_details", minimum = 2L)
  }

  current <- stpd_publication_readiness_scalar(framework, "current_stage_id")
  if (identical(current, "FRAMEWORK_COMPLETE")) {
    if (!all(vapply(stages, function(stage) {
      identical(stage$framework_status, "reviewed_framework") &&
        identical(stage$review_status, "passed")
    }, logical(1)))) {
      stop("Completed publication-readiness framework has an unreviewed stage.",
           call. = FALSE)
    }
    current_index <- length(stages) + 1L
  } else {
  current_index <- match(current, stage_ids)
  if (is.na(current_index) ||
      !identical(stages[[current_index]]$framework_status,
                 "ready_for_review")) {
    stop("Current publication-readiness stage is not ready for review.",
         call. = FALSE)
  }
  }
  before <- if (current_index > 1L) seq_len(current_index - 1L) else integer()
  after <- if (current_index <= length(stages) &&
               current_index < length(stages)) {
    seq.int(current_index + 1L, length(stages))
  } else integer()
  if (length(before) && !all(vapply(stages[before], function(stage) {
    identical(stage$framework_status, "reviewed_framework") &&
      identical(stage$review_status, "passed")
  }, logical(1)))) {
    stop("An earlier publication-readiness framework lacks passed review.",
         call. = FALSE)
  }
  if (length(after) && !all(vapply(stages[after], function(stage) {
    identical(stage$framework_status, "framework_pending") &&
      identical(stage$review_status, "not_started")
  }, logical(1)))) {
    stop("A later publication-readiness stage started before its dependency.",
         call. = FALSE)
  }

  foundations <- framework$completed_foundations
  required_foundation <- c(
    "foundation_id", "record_type", "status", "authority_note")
  if (!is.list(foundations) || !length(foundations) ||
      !all(vapply(foundations, function(item) {
        is.list(item) &&
          identical(sort(names(item)), sort(required_foundation)) &&
          identical(item$record_type, "historical_foundation") &&
          is.character(item$status) && length(item$status) == 1L &&
          identical(item$status, "completed_before_framework") &&
          is.character(item$authority_note) &&
          identical(item$authority_note,
                    "does_not_complete_any_publication_readiness_stage")
      }, logical(1)))) {
    stop("Historical foundations are missing or overclaim current completion.",
         call. = FALSE)
  }
  foundation_ids <- vapply(
    foundations, stpd_publication_readiness_scalar, character(1),
    field = "foundation_id")
  if (anyDuplicated(foundation_ids)) {
    stop("Historical foundation IDs are duplicated.", call. = FALSE)
  }

  roots <- framework$gate1b_remaining_roots
  required_root <- c(
    "root_id", "depends_on", "framework_status",
    "scientific_closure_status", "publication_authority",
    "closure_record_path", "closure_record_sha256",
    "minimum_review_checks")
  expected_roots <- stpd_publication_readiness_required_gate1b_roots()
  if (!is.list(roots) || length(roots) != length(expected_roots) ||
      !all(vapply(roots, function(root) {
        is.list(root) && identical(sort(names(root)), sort(required_root))
      }, logical(1)))) {
    stop("Gate 1B remaining-root registry is incomplete.", call. = FALSE)
  }
  root_ids <- vapply(roots, stpd_publication_readiness_scalar, character(1),
                     field = "root_id")
  if (!identical(root_ids, expected_roots) || anyDuplicated(root_ids)) {
    stop("Gate 1B remaining roots or order are invalid.", call. = FALSE)
  }
  for (i in seq_along(roots)) {
    root <- roots[[i]]
    expected_dependency <- if (i == 1L) {
      "post_ownership_pause_proposal_root"
    } else root_ids[[i - 1L]]
    if (!identical(stpd_publication_readiness_scalar(root, "depends_on"),
                   expected_dependency) ||
        !identical(stpd_publication_readiness_scalar(
          root, "framework_status"), "implementation_reviewed") ||
        !identical(stpd_publication_readiness_scalar(
          root, "scientific_closure_status"),
          "bounded_observer_root_closed_no_performance_claim") ||
        !identical(root$publication_authority, FALSE) ||
        !grepl(
          "^validation/threshold_first_phase1/GATE1B_[A-Z0-9_]+_CLOSURE[.]md$",
          stpd_publication_readiness_scalar(root, "closure_record_path")
        ) ||
        !grepl(
          "^[0-9a-f]{64}$",
          stpd_publication_readiness_scalar(root, "closure_record_sha256")
        )) {
      stop("Gate 1B root closure, dependency, or authority is invalid.",
           call. = FALSE)
    }
    stpd_publication_readiness_unique_strings(
      root, "minimum_review_checks", minimum = 4L)
  }
  invisible(TRUE)
}

stpd_publication_readiness_validate_packets <- function(packets,
                                                         framework = NULL) {
  required_top <- c("schema", "packet_version", "authority_scope", "packets")
  if (!is.list(packets) ||
      !identical(sort(names(packets)), sort(required_top)) ||
      !identical(packets$schema,
                 "stpd_publication_readiness_stage_packets_v1") ||
      !identical(packets$packet_version, "1.1.0") ||
      !identical(packets$authority_scope,
                 "framework_only_no_scientific_or_release_authority")) {
    stop("Publication-readiness stage packets have invalid identity or scope.",
         call. = FALSE)
  }
  items <- packets$packets
  ids <- stpd_publication_readiness_required_stage_ids()
  if (!is.list(items) || length(items) != length(ids) ||
      !identical(vapply(items, stpd_publication_readiness_scalar, character(1),
                        field = "stage_id"), ids)) {
    stop("Publication-readiness stage packets are incomplete or unordered.",
         call. = FALSE)
  }
  required_packet <- c(
    "stage_id", "framework_objective", "artifact_contracts",
    "review_evidence", "deferred_work", "authority_granted"
  )
  if (is.null(framework)) {
    framework <- jsonlite::read_json(
      stpd_publication_readiness_framework_path(), simplifyVector = FALSE)
    stpd_publication_readiness_validate(framework)
  }
  for (i in seq_along(items)) {
    item <- items[[i]]
    stage <- framework$stages[[i]]
    if (!identical(sort(names(item)), sort(required_packet)) ||
        !identical(item$authority_granted, FALSE)) {
      stop("A stage packet is incomplete or grants authority.", call. = FALSE)
    }
    stpd_publication_readiness_scalar(item, "framework_objective")
    artifact_contracts <- stpd_publication_readiness_unique_strings(
      item, "artifact_contracts", minimum = 3L)
    review_evidence <- stpd_publication_readiness_unique_strings(
      item, "review_evidence", minimum = 3L)
    deferred_work <- stpd_publication_readiness_unique_strings(
      item, "deferred_work", minimum = 2L)
    if (!identical(
          artifact_contracts,
          stpd_publication_readiness_strings(stage, "required_outputs")) ||
        !identical(
          review_evidence,
          stpd_publication_readiness_strings(stage, "stop_checks")) ||
        !identical(
          deferred_work,
          stpd_publication_readiness_strings(stage, "deferred_details"))) {
      stop("Stage packet does not exactly bind its framework stage: ",
           item$stage_id, call. = FALSE)
    }
  }
  invisible(TRUE)
}

stpd_publication_readiness_sha256_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("Publication-readiness hash target is missing.", call. = FALSE)
  }
  digest::digest(path, algo = "sha256", file = TRUE)
}

stpd_publication_readiness_validate_gate1b_closure_records <- function(
    framework, framework_path, verify = c("auto", "always", "never")) {
  verify <- match.arg(verify)
  source_root <- normalizePath(
    file.path(dirname(framework_path), "..", ".."), mustWork = FALSE
  )
  for (root in framework$gate1b_remaining_roots) {
    relative <- root$closure_record_path
    candidates <- unique(c(
      file.path(getwd(), relative), file.path(source_root, relative)
    ))
    existing <- candidates[file.exists(candidates)]
    must_verify <- identical(verify, "always") ||
      (identical(verify, "auto") && length(existing) > 0L)
    if (must_verify && length(existing) == 0L) {
      stop("A Gate 1B closure record is missing.", call. = FALSE)
    }
    if (length(existing) > 0L && !any(vapply(existing, function(path) {
      identical(
        stpd_publication_readiness_sha256_file(path),
        root$closure_record_sha256
      )
    }, logical(1)))) {
      stop("A Gate 1B closure record hash differs from its registry.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

stpd_publication_readiness_validate_receipts <- function(
    receipts, framework, packets, framework_path, packets_path,
    verify_source_reviews = c("auto", "always", "never")) {
  verify_source_reviews <- match.arg(verify_source_reviews)
  required_top <- c(
    "schema", "receipt_version", "authority_scope", "framework_sha256",
    "packets_sha256", "remediation_record_path",
    "remediation_record_sha256", "receipts"
  )
  if (!is.list(receipts) ||
      !identical(sort(names(receipts)), sort(required_top)) ||
      !identical(receipts$schema,
                 "stpd_publication_readiness_review_receipts_v1") ||
      !identical(receipts$receipt_version, "1.0.0") ||
      !identical(
        receipts$authority_scope,
        "bounded_framework_review_only_no_scientific_or_release_authority")) {
    stop("Publication-readiness review receipts have invalid identity or scope.",
         call. = FALSE)
  }
  is_sha256 <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      grepl("^[0-9a-f]{64}$", value)
  }
  if (!is_sha256(receipts$framework_sha256) ||
      !is_sha256(receipts$packets_sha256) ||
      !identical(receipts$framework_sha256,
                 stpd_publication_readiness_sha256_file(framework_path)) ||
      !identical(receipts$packets_sha256,
                 stpd_publication_readiness_sha256_file(packets_path))) {
    stop("Publication-readiness framework or packet hash is not receipt-bound.",
         call. = FALSE)
  }

  items <- receipts$receipts
  ids <- stpd_publication_readiness_required_stage_ids()
  required_receipt <- c(
    "stage_id", "review_status", "review_record_path",
    "review_record_sha256", "bound_output_ids", "passed_stop_check_ids",
    "deferred_work_ids", "authority_granted"
  )
  if (!is.list(items) || length(items) != length(ids) ||
      !identical(vapply(items, stpd_publication_readiness_scalar, character(1),
                        field = "stage_id"), ids)) {
    stop("Publication-readiness review receipts are incomplete or unordered.",
         call. = FALSE)
  }

  source_root <- normalizePath(
    file.path(dirname(framework_path), "..", ".."),
    mustWork = FALSE)
  verify_review_path <- function(relative_path, expected_sha256) {
    if (!is_sha256(expected_sha256) ||
        !is.character(relative_path) || length(relative_path) != 1L ||
        is.na(relative_path) || !nzchar(relative_path) ||
        grepl("^/|(^|/)\\.\\.(/|$)", relative_path)) {
      stop("Publication-readiness review path or hash is invalid.",
           call. = FALSE)
    }
    candidates <- unique(c(
      file.path(getwd(), relative_path),
      file.path(source_root, relative_path)
    ))
    existing <- candidates[file.exists(candidates)]
    must_verify <- identical(verify_source_reviews, "always") ||
      (identical(verify_source_reviews, "auto") && length(existing) > 0L)
    if (must_verify && length(existing) == 0L) {
      stop("Publication-readiness source review record is missing.",
           call. = FALSE)
    }
    if (length(existing) > 0L &&
        !any(vapply(existing, function(path) {
          identical(stpd_publication_readiness_sha256_file(path),
                    expected_sha256)
        }, logical(1)))) {
      stop("Publication-readiness source review hash differs from receipt.",
           call. = FALSE)
    }
    invisible(TRUE)
  }

  for (i in seq_along(items)) {
    item <- items[[i]]
    stage <- framework$stages[[i]]
    packet <- packets$packets[[i]]
    expected_path <- sprintf(
      "validation/publication_readiness/STAGE%02d_FRAMEWORK_REVIEW.md", i)
    if (!identical(sort(names(item)), sort(required_receipt)) ||
        !identical(item$review_status,
                   "bounded_framework_review_passed") ||
        !identical(item$authority_granted, FALSE) ||
        !identical(item$review_record_path, expected_path)) {
      stop("A publication-readiness review receipt is incomplete or unsafe.",
           call. = FALSE)
    }
    outputs <- stpd_publication_readiness_unique_strings(
      item, "bound_output_ids", minimum = 3L)
    checks <- stpd_publication_readiness_unique_strings(
      item, "passed_stop_check_ids", minimum = 3L)
    deferred <- stpd_publication_readiness_unique_strings(
      item, "deferred_work_ids", minimum = 2L)
    if (!identical(
          outputs, unlist(packet$artifact_contracts, use.names = FALSE)) ||
        !identical(
          checks, unlist(packet$review_evidence, use.names = FALSE)) ||
        !identical(
          deferred, unlist(packet$deferred_work, use.names = FALSE)) ||
        !identical(outputs, unlist(stage$required_outputs, use.names = FALSE)) ||
        !identical(checks, unlist(stage$stop_checks, use.names = FALSE)) ||
        !identical(deferred,
                   unlist(stage$deferred_details, use.names = FALSE))) {
      stop("Review receipt does not exactly close its stage packet: ",
           item$stage_id, call. = FALSE)
    }
    verify_review_path(item$review_record_path, item$review_record_sha256)
  }
  if (!identical(
        receipts$remediation_record_path,
        "validation/publication_readiness/FRAMEWORK_RECHECK_REMEDIATION.md")) {
    stop("Publication-readiness remediation record path is invalid.",
         call. = FALSE)
  }
  verify_review_path(receipts$remediation_record_path,
                     receipts$remediation_record_sha256)
  invisible(TRUE)
}

#' Load the 12-stage publication-readiness framework
#' @param path Optional explicit JSON path.
#' @return A validated governance-only framework list.
#' @export
stpd_publication_readiness_framework <- function(path = NULL) {
  path <- path %||% stpd_publication_readiness_framework_path()
  if (!file.exists(path)) {
    stop("Publication-readiness framework file is missing.", call. = FALSE)
  }
  framework <- jsonlite::read_json(path, simplifyVector = FALSE)
  stpd_publication_readiness_validate(framework)
  stpd_publication_readiness_validate_gate1b_closure_records(
    framework, path, "auto"
  )
  if (identical(framework$current_stage_id, "FRAMEWORK_COMPLETE")) {
    packets_path <- stpd_publication_readiness_packets_path()
    packets <- jsonlite::read_json(packets_path, simplifyVector = FALSE)
    stpd_publication_readiness_validate_packets(packets, framework)
    receipts <- jsonlite::read_json(
      stpd_publication_readiness_receipts_path(), simplifyVector = FALSE)
    stpd_publication_readiness_validate_receipts(
      receipts, framework, packets, path, packets_path)
  }
  framework
}

#' Load the minimum framework packet for each publication-readiness stage
#' @param path Optional explicit JSON path.
#' @return A validated governance-only packet registry.
#' @export
stpd_publication_readiness_stage_packets <- function(path = NULL) {
  path <- path %||% stpd_publication_readiness_packets_path()
  if (!file.exists(path)) {
    stop("Publication-readiness stage packet file is missing.", call. = FALSE)
  }
  packets <- jsonlite::read_json(path, simplifyVector = FALSE)
  stpd_publication_readiness_validate_packets(packets)
  packets
}

#' Load hash-bound publication-readiness framework review receipts
#' @param path Optional explicit receipt JSON path.
#' @param verify_source_reviews Whether source review Markdown must be checked:
#'   `"auto"`, `"always"`, or `"never"`.
#' @return A validated non-authoritative review receipt registry.
#' @export
stpd_publication_readiness_review_receipts <- function(
    path = NULL, verify_source_reviews = c("auto", "always", "never")) {
  verify_source_reviews <- match.arg(verify_source_reviews)
  path <- path %||% stpd_publication_readiness_receipts_path()
  framework_path <- stpd_publication_readiness_framework_path()
  packets_path <- stpd_publication_readiness_packets_path()
  framework <- jsonlite::read_json(framework_path, simplifyVector = FALSE)
  packets <- jsonlite::read_json(packets_path, simplifyVector = FALSE)
  receipts <- jsonlite::read_json(path, simplifyVector = FALSE)
  stpd_publication_readiness_validate(framework)
  stpd_publication_readiness_validate_packets(packets, framework)
  stpd_publication_readiness_validate_receipts(
    receipts, framework, packets, framework_path, packets_path,
    verify_source_reviews)
  receipts
}
