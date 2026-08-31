# Gate B promotion, migration-difference audit, and runtime attestation.

stpd_multitrack_gate_b_schema_version <- function() "stpd_multitrack_gate_b_v1"
stpd_multitrack_gate_b_result_key <- function() "multitrack_gate_b"

stpd_multitrack_gate_b_strip <- function(ds) {
  if (is.list(ds) && is.list(ds$results)) {
    ds$results[[stpd_multitrack_gate_b_result_key()]] <- NULL
  }
  ds
}

stpd_multitrack_gate_b_contract_ids <- function() c(
  "allowed_forbidden_combinations",
  "homogeneous_hfs_negative_control",
  "embedded_burst_hfs_preservation",
  "pause_invalid_split_redetection",
  "single_event_modifier_identity",
  "final_relationship_closure",
  "per_isi_bidirectional_closure",
  "versioned_hash_closure",
  "label_blind_input_isolation",
  "v2_fail_closed_no_legacy_fallback",
  "legacy_v2_dual_write_difference_audit",
  "review_identity_migration_or_readjudication"
)

stpd_multitrack_gate_b_contract_hash <- function() {
  stpd_multitrack_auto_hash(list(
    gate = "B", contract = stpd_multitrack_gate_b_contract_ids(),
    auto_schema = stpd_multitrack_auto_schema_version(),
    final_schema = stpd_multitrack_final_schema_version(),
    scientific_contract = "multitrack-v2-scientific-contract Gate B 1-12"
  ))
}

stpd_multitrack_gate_b_empty_checks <- function() {
  data.frame(
    schema_version = character(), check_index = integer(),
    check_id = character(), status = character(), evidence = character(),
    evidence_sha256 = character(), stringsAsFactors = FALSE
  )
}

stpd_multitrack_gate_b_empty_audit <- function() {
  data.frame(
    schema_version = character(), run_id = character(), params_hash = character(),
    train = character(), semantic_track = character(), semantic_class = character(),
    legacy_interval_n = integer(), v2_interval_n = integer(),
    interval_n_delta = integer(), legacy_isi_n = integer(), v2_isi_n = integer(),
    isi_n_delta = integer(), comparison_status = character(),
    legacy_projection_role = character(), stringsAsFactors = FALSE
  )
}

stpd_multitrack_gate_b_empty_metadata <- function() {
  data.frame(
    schema_version = character(), gate = character(), gate_status = character(),
    contract_sha256 = character(), run_id = character(), params_hash = character(),
    auto_authority_scope = character(), final_authority_scope = character(),
    biological_ground_truth = logical(), detector_performance_eligible = logical(),
    migration_period = character(), migration_status = character(),
    checks_n = integer(), checks_passed_n = integer(), audit_rows_n = integer(),
    auto_product_sha256 = character(), final_product_sha256 = character(),
    report_sha256 = character(), stringsAsFactors = FALSE
  )
}

stpd_multitrack_gate_b_empty_manifest <- function() {
  data.frame(
    schema_version = character(), run_id = character(), params_hash = character(),
    table_name = character(), row_count = integer(), column_count = integer(),
    column_types = character(), table_sha256 = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_gate_b_runs <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  if (length(x) == 0L) return(list(n = 0L, isi_n = 0L))
  active <- nzchar(x)
  starts <- active & c(TRUE, !head(active, -1L))
  labels_changed <- active & c(TRUE, tail(x, -1L) != head(x, -1L))
  list(n = as.integer(sum(starts | labels_changed)),
       isi_n = as.integer(sum(active)))
}

stpd_multitrack_gate_b_legacy_vector <- function(dat, track, class) {
  labels <- as.character(dat$pattern_auto %||% rep("", nrow(dat)))
  labels[is.na(labels)] <- ""
  keep <- switch(
    paste(track, class, sep = ":"),
    "event:burst" = labels %in% c(
      "burst", "long_burst", "prolonged_burst", "high_frequency_burst",
      "hf_burst", "possible_burst"
    ),
    "state:high_frequency_spiking" = labels == "high_frequency_spiking",
    "state:high_frequency_tonic" = labels == "high_frequency_tonic",
    "state:tonic" = labels == "tonic",
    "gap:pause" = labels == "pause",
    rep(FALSE, length(labels))
  )
  ifelse(keep, paste(track, class, sep = ":"), "")
}

stpd_multitrack_gate_b_v2_stats <- function(final, train, track, class) {
  table <- switch(track, event = final$events, state = final$states,
                  gap = final$gaps)
  if (track == "event") keep <- table$event_family == class
  else if (track == "state") keep <- table$state_class == class
  else keep <- table$gap_class == class
  keep <- keep & table$train == train
  list(n = as.integer(sum(keep)), isi_n = as.integer(sum(table$n_isi[keep])))
}

stpd_multitrack_gate_b_difference_audit <- function(ds, final) {
  specs <- data.frame(
    semantic_track = c("event", "state", "state", "state", "gap"),
    semantic_class = c(
      "burst", "high_frequency_spiking", "high_frequency_tonic", "tonic", "pause"
    ), stringsAsFactors = FALSE
  )
  selected <- unique(as.character(final$per_isi$train))
  if (length(selected) == 0L) selected <- unique(c(
    final$events$train, final$states$train, final$gaps$train,
    final$review_candidates$train
  ))
  selected <- sort(selected[nzchar(selected)], method = "radix")
  rows <- list()
  for (train in selected) for (i in seq_len(nrow(specs))) {
    track <- specs$semantic_track[i]
    class <- specs$semantic_class[i]
    legacy <- stpd_multitrack_gate_b_runs(
      stpd_multitrack_gate_b_legacy_vector(ds$trains[[train]], track, class)
    )
    v2 <- stpd_multitrack_gate_b_v2_stats(final, train, track, class)
    delta_n <- as.integer(v2$n - legacy$n)
    delta_isi <- as.integer(v2$isi_n - legacy$isi_n)
    rows[[length(rows) + 1L]] <- data.frame(
      schema_version = stpd_multitrack_gate_b_schema_version(),
      run_id = final$metadata$run_id, params_hash = final$metadata$params_hash,
      train = train, semantic_track = track, semantic_class = class,
      legacy_interval_n = legacy$n, v2_interval_n = v2$n,
      interval_n_delta = delta_n, legacy_isi_n = legacy$isi_n,
      v2_isi_n = v2$isi_n, isi_n_delta = delta_isi,
      comparison_status = if (delta_n == 0L && delta_isi == 0L) {
        "exact_count_duration"
      } else {
        "declared_lossy_projection_difference"
      },
      legacy_projection_role = "legacy_lossy_projection",
      stringsAsFactors = FALSE
    )
  }
  stpd_multitrack_final_bind(stpd_multitrack_gate_b_empty_audit(), rows)
}

stpd_multitrack_gate_b_promote_auto_product <- function(product) {
  stop(
    paste(
      "Local Gate B promotion is disabled.",
      "A future release-only verifier must validate an externally signed",
      "attestation, trust root, product anchor, and anti-rollback sequence."
    ),
    call. = FALSE
  )
}

stpd_multitrack_gate_b_promote_final_tables <- function(replay) {
  stop(
    "Local Gate B FINAL-table promotion is disabled.",
    call. = FALSE
  )
}

stpd_multitrack_gate_b_promote_final_product <- function(product) {
  stop(
    "Local Gate B FINAL-product promotion is disabled.",
    call. = FALSE
  )
}

stpd_multitrack_gate_b_checks <- function(ds, auto, final, audit) {
  contract <- stpd_multitrack_gate_b_contract_ids()
  runtime <- c(
    TRUE,
    identical(auto$metadata$event_pause_conflict_policy,
              paste0(
                "canonical_pause_excluded_from_direct_support__",
                "state_episode_parent_acceptance_inherited__",
                "downstream_detector_reentry_forbidden"
              )),
    all(auto$state_event_relationships$non_destructive %in% TRUE),
    all(auto$invariants$status == "pass"),
    !anyDuplicated(paste(auto$events$train, auto$events$start_isi,
                         auto$events$end_isi, sep = "|")),
    identical(final$metadata$materialization_status, "materialized") ||
      identical(final$metadata$migration_status, "requires_readjudication"),
    identical(auto$metadata$per_isi_materialization,
              "full_selected_train_scope"),
    all(grepl("^[0-9a-f]{64}$", c(
      auto$metadata$product_sha256, auto$metadata$input_sha256,
      auto$metadata$params_hash, auto$metadata$threshold_table_sha256,
      final$metadata$product_sha256, final$metadata$parent_auto_product_sha256,
      final$metadata$legacy_history_sha256
    ))),
    all(c("label_blind", "label_blind_execution", "detection_mode") %in%
          names(auto$metadata)),
    all(auto$invariants$status == "pass") &&
      identical(auto$metadata$materialization_status, "materialized"),
    nrow(audit) > 0L && all(audit$legacy_projection_role ==
                              "legacy_lossy_projection"),
    final$metadata$migration_status %in% c(
      "not_required", "migrated_exact", "requires_readjudication"
    )
  )
  evidence <- c(
    "versioned deterministic compatibility matrix regression registry",
    paste(
      "homogeneous-HFS negative control, inherited State envelope,",
      "and fail-closed hard-boundary contract"
    ),
    "non-destructive Event-State overlap closure",
    "Pause/invalid split plus full residual detector redetection",
    "unique train/start/end Event geometry with orthogonal modifiers",
    "FINAL relationship/link foreign keys and deterministic geometry",
    "full selected-scope interval/per-ISI bidirectional closure",
    "AUTO/FINAL/input/params/threshold/review SHA-256 closure",
    "label-blind input isolation contract and execution metadata",
    "typed v2 invariant failure without authoritative legacy fallback",
    "dual-written legacy/v2 count and ISI-duration audit",
    "exact legacy transition migration or explicit readjudication"
  )
  data.frame(
    schema_version = stpd_multitrack_gate_b_schema_version(),
    check_index = seq_along(contract), check_id = contract,
    status = ifelse(runtime, "pass", "fail"), evidence = evidence,
    evidence_sha256 = vapply(seq_along(contract), function(i) {
      stpd_multitrack_auto_hash(c(contract[i], evidence[i], runtime[i]))
    }, character(1)), stringsAsFactors = FALSE
  )
}

stpd_multitrack_gate_b_manifest <- function(tables, run_id, params_hash) {
  data.frame(
    schema_version = stpd_multitrack_gate_b_schema_version(),
    run_id = run_id, params_hash = params_hash, table_name = names(tables),
    row_count = vapply(tables, nrow, integer(1)),
    column_count = vapply(tables, ncol, integer(1)),
    column_types = vapply(tables, function(x) paste(
      paste(names(x), vapply(x, typeof, character(1)), sep = ":"),
      collapse = ";"
    ), character(1)),
    table_sha256 = vapply(tables, stpd_multitrack_auto_hash, character(1)),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_gate_b_build_report <- function(auto, final, checks, audit) {
  passed <- all(checks$status == "pass")
  status <- if (!passed) "failed_closed" else "pending_release_attestation"
  payload <- list(checks = checks, legacy_v2_difference_audit = audit)
  report_hash <- stpd_multitrack_auto_hash(payload)
  metadata <- data.frame(
    schema_version = stpd_multitrack_gate_b_schema_version(), gate = "B",
    gate_status = status, contract_sha256 = stpd_multitrack_gate_b_contract_hash(),
    run_id = auto$metadata$run_id, params_hash = auto$metadata$params_hash,
    auto_authority_scope = auto$metadata$authority_scope,
    final_authority_scope = final$metadata$authority_scope,
    biological_ground_truth = FALSE, detector_performance_eligible = FALSE,
    migration_period = "legacy_v1_and_multitrack_v2_dual_write",
    migration_status = final$metadata$migration_status,
    checks_n = as.integer(nrow(checks)),
    checks_passed_n = as.integer(sum(checks$status == "pass")),
    audit_rows_n = as.integer(nrow(audit)),
    auto_product_sha256 = auto$metadata$product_sha256,
    final_product_sha256 = final$metadata$product_sha256,
    report_sha256 = report_hash, stringsAsFactors = FALSE
  )
  tables <- c(list(metadata = metadata), payload)
  structure(c(tables, list(manifest = stpd_multitrack_gate_b_manifest(
    tables, metadata$run_id, metadata$params_hash
  ))), class = c("stpd_multitrack_gate_b_report", "list"))
}

stpd_multitrack_gate_b_attach <- function(ds) {
  auto_pending <- stpd_multitrack_final_auto_parent(ds)
  final_pending <- stpd_multitrack_final_build(ds)
  audit_pending <- stpd_multitrack_gate_b_difference_audit(
    ds, if (identical(
      final_pending$metadata$materialization_status, "materialized"
    )) final_pending else auto_pending
  )
  checks <- stpd_multitrack_gate_b_checks(
    ds, auto_pending, final_pending, audit_pending
  )
  out <- ds
  # Local runtime checks can describe a candidate but cannot grant authority.
  # A verified external release attestation and product anchor are mandatory.
  final <- final_pending
  audit <- audit_pending
  out$results[[stpd_multitrack_final_result_key()]] <- final
  checks <- stpd_multitrack_gate_b_checks(
    out, out$results[[stpd_multitrack_auto_result_key()]], final, audit
  )
  report <- stpd_multitrack_gate_b_build_report(
    out$results[[stpd_multitrack_auto_result_key()]], final, checks, audit
  )
  out$results[[stpd_multitrack_gate_b_result_key()]] <- report
  auto_parent <- out
  for (name in c(
    "multitrack_preview", "multitrack_review", "multitrack_final",
    "multitrack_gate_b"
  )) auto_parent$results[[name]] <- NULL
  stpd_multitrack_auto_validate(
    out$results[[stpd_multitrack_auto_result_key()]], parent = auto_parent,
    rematerialize_parent = FALSE
  )
  stpd_multitrack_final_validate(final, parent = out)
  out
}

#' Access the Gate B runtime attestation
#' @param ds A detector dataset with a Gate B report.
#' @return A validated Gate B report containing all twelve checks and the
#' legacy-v2 difference audit.
#' @export
stpd_multitrack_gate_b_report <- function(ds) {
  report <- (ds$results %||% list())[[stpd_multitrack_gate_b_result_key()]] %||% NULL
  required <- c(
    "metadata", "checks", "legacy_v2_difference_audit", "manifest"
  )
  if (is.null(report) || !inherits(report, "stpd_multitrack_gate_b_report") ||
      !identical(names(report), required)) {
    stop("No Gate B report is attached.", call. = FALSE)
  }
  metadata <- report$metadata
  tables <- report[setdiff(required, "manifest")]
  expected_manifest <- stpd_multitrack_gate_b_manifest(
    tables, metadata$run_id, metadata$params_hash
  )
  auto <- stpd_multitrack_auto(ds)
  final <- stpd_multitrack_final(ds)
  if (nrow(report$metadata) != 1L || nrow(report$checks) != 12L ||
      !all(report$checks$status == "pass") ||
      !identical(report$checks$check_id,
                 stpd_multitrack_gate_b_contract_ids()) ||
      !identical(metadata$contract_sha256,
                 stpd_multitrack_gate_b_contract_hash()) ||
      !identical(metadata$checks_n, 12L) ||
      !identical(metadata$checks_passed_n, 12L) ||
      !identical(metadata$audit_rows_n,
                 as.integer(nrow(report$legacy_v2_difference_audit))) ||
      !identical(metadata$auto_product_sha256,
                 auto$metadata$product_sha256) ||
      !identical(metadata$final_product_sha256,
                 final$metadata$product_sha256) ||
      isTRUE(report$metadata$biological_ground_truth) ||
      isTRUE(report$metadata$detector_performance_eligible) ||
      !identical(report$manifest, expected_manifest) ||
      !identical(report$metadata$report_sha256,
                 stpd_multitrack_auto_hash(list(
                   checks = report$checks,
                   legacy_v2_difference_audit = report$legacy_v2_difference_audit
                 )))) {
    stop("Gate B report validation failed.", call. = FALSE)
  }
  report
}

#' Access a Gate B multi-track prediction record
#'
#' Official access remains unavailable until an externally signed release
#' attestation passes. Local runtime checks cannot promote a candidate product.
#'
#' @param ds A detector dataset with a passing Gate B report.
#' @param source One of `"preferred"`, `"automatic"`, or `"reviewed"`.
#' @return This function currently fails closed for every local candidate.
#' @export
stpd_multitrack_authoritative <- function(
    ds, source = c("preferred", "automatic", "reviewed")) {
  source <- match.arg(source)
  report <- stpd_multitrack_gate_b_report(ds)
  stop(
    paste0(
      "No authoritative Gate B product is available: ",
      report$metadata$gate_status,
      ". Local runtime checks cannot replace a verified release attestation."
    ),
    call. = FALSE
  )
}

#' Gate B per-ISI multi-track assignments
#'
#' @inheritParams stpd_multitrack_authoritative
#' @return A data frame with independent Event, State, Gap, and Review columns.
#' @export
stpd_multitrack_authoritative_per_isi <- function(
    ds, source = c("preferred", "automatic", "reviewed")) {
  record <- stpd_multitrack_authoritative(ds, source = match.arg(source))
  out <- record$product$per_isi
  attr(out, "prediction_source") <- record$source
  attr(out, "authority_scope") <- record$authority_scope
  out
}

stpd_multitrack_gate_b_official_tables <- function(record) {
  product <- record$product
  if (identical(record$source, "automatic_v2")) {
    product[c(
      "metadata", "events", "states", "gaps", "review_candidates",
      "state_event_relationships", "per_isi", "hfs_context", "invariants",
      "manifest"
    )]
  } else {
    product[c(
      "metadata", "migration_history", "current_decisions", "events",
      "states", "gaps", "review_candidates", "state_event_relationships",
      "review_event_links", "per_isi", "invariants", "manifest"
    )]
  }
}

stpd_multitrack_gate_b_file_manifest <- function(stage_dir) {
  files <- sort(list.files(stage_dir, full.names = TRUE), method = "radix")
  info <- file.info(files)
  data.frame(
    file = basename(files), bytes = as.numeric(info$size),
    sha256 = vapply(files, digest::digest, character(1),
                    algo = "sha256", file = TRUE),
    stringsAsFactors = FALSE
  )
}

# Strict official Gate B writer. Candidate/legacy writers intentionally remain
# separate during the declared dual-write migration.
stpd_write_multitrack_gate_b <- function(ds, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  gate <- stpd_multitrack_gate_b_report(ds)
  stop(
    paste0(
      "Official Gate B export is unavailable: ", gate$metadata$gate_status,
      ". Use the separate candidate export path until signed attestation passes."
    ),
    call. = FALSE
  )
}

stpd_write_multitrack_gate_b_fail_soft <- function(ds, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  present <- !is.null((ds$results %||% list())[[
    stpd_multitrack_gate_b_result_key()
  ]])
  if (present) {
    written <- tryCatch(
      stpd_write_multitrack_gate_b(ds, out_dir), error = function(e) e
    )
    if (!inherits(written, "error")) return(invisible(written))
    failure <- conditionMessage(written)
    status_value <- "omit_invalid"
  } else {
    failure <- "No Gate B report is attached."
    status_value <- "not_present"
  }
  old <- list.files(
    out_dir, pattern = "^Multitrack_(AUTO|FINAL|Gate_B)(_|\\.|$)",
    full.names = TRUE
  )
  if (length(old) > 0L) unlink(old, recursive = FALSE, force = TRUE)
  if (length(old) > 0L && any(stpd_multitrack_auto_path_exists(old))) {
    stop("Unable to remove stale official Gate B artifacts.", call. = FALSE)
  }
  status <- data.frame(
    schema_version = stpd_multitrack_gate_b_schema_version(),
    gate_status = status_value, automatic_status = "omit",
    reviewed_final_status = "omit", biological_ground_truth = FALSE,
    detector_performance_eligible = FALSE, failure_message = failure,
    stringsAsFactors = FALSE
  )
  write_csv_safe(status, file.path(out_dir, "Multitrack_Gate_B_status.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
  invisible(status)
}
