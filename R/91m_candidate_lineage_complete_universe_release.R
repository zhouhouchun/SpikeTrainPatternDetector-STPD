# Gate 1B complete direct-observation universe release -------------------
#
# This observer closes the internal hf_protected observation universe only.
# It binds the frozen family-root observations to the already materialized
# train products and freezes the existing resource caps.  It does not build
# the canonical public lineage adapter, emit scientific candidates, or grant
# publication authority.

stpd_candidate_lineage_complete_universe_release_hooks <- function() c(
  candidate_universe_release_manifest_v1 = "manifest",
  candidate_universe_release_products_v1 = "products",
  candidate_universe_release_receipt_v1 = "receipt"
)

stpd_candidate_lineage_complete_universe_release_hash <- function(x, stage) {
  stpd_threshold_first_hash_domain(
    paste0("stpd-candidate-universe-release-", stage, "-v1"), x
  )
}

stpd_candidate_lineage_complete_universe_root_audit <- function(observations) {
  receipt <- function(hook) observations[[hook]]
  scalar <- function(x, field, default = NA) {
    if (is.null(x) || !is.data.frame(x) || nrow(x) != 1L ||
        !(field %in% names(x))) return(default)
    x[[field]][[1L]]
  }
  row <- function(root_id, hook, complete, cap_or_early_stop, status) {
    data.frame(
      root_id = root_id, receipt_hook = hook,
      closure_complete = isTRUE(complete),
      cap_or_early_stop = isTRUE(cap_or_early_stop),
      closure_status = as.character(status)[1L],
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  pause_raw <- receipt("pause_raw_receipt_v1")
  structure <- receipt("burst_structure_first_scan_receipt_v1")
  stage4 <- receipt("burst_threshold_stage4_receipt_v1")
  union <- receipt("burst_union_rank_receipt_v1")
  refractory <- receipt("burst_refractory_receipt_v1")
  boundary <- receipt("burst_pause_boundary_receipt_v1")
  hard <- receipt("burst_hard_threshold_receipt_v1")
  ownership <- receipt("burst_pause_ownership_receipt_v1")
  contextual <- receipt("contextual_pause_receipt_v1")
  gap <- receipt("gap_final_receipt_v1")
  hfs <- receipt("hfs_state_receipt_v1")
  tonic <- receipt("tonic_state_receipt_v1")
  nested <- receipt("nested_hfs_review_receipt_v1")

  pause_requested <- isTRUE(scalar(
    pause_raw, "pause_pattern_requested", FALSE
  ))
  structure_enabled <- isTRUE(scalar(
    structure, "structure_first_enabled", FALSE
  ))
  stage4_not_applicable <- identical(
    scalar(stage4, "coverage_status", ""),
    "stage4_not_applicable_short_train"
  )
  hard_not_applicable <- scalar(hard, "applicability_status", "") %in% c(
    "not_reached_insufficient_input", "not_applicable_empty_train",
    "invoked_no_train_range", "invoked_non_hard_threshold_range",
    "invoked_invalid_hard_upper"
  )
  rows <- list(
    row(
      "raw_canonical_pause", "pause_raw_receipt_v1",
      (!pause_requested || isTRUE(scalar(pause_raw, "scan_exhausted", FALSE))) &&
        !isTRUE(scalar(pause_raw, "cap_applied", TRUE)),
      isTRUE(scalar(pause_raw, "cap_applied", FALSE)),
      scalar(pause_raw, "coverage_status", "missing_receipt")
    ),
    row(
      "burst_structure_first", "burst_structure_first_scan_receipt_v1",
      !structure_enabled || isTRUE(scalar(structure, "scan_exhausted", FALSE)),
      structure_enabled && (
        !isTRUE(scalar(structure, "scan_exhausted", FALSE)) ||
          scalar(structure, "truncated_n", 0L) > 0L
      ),
      scalar(structure, "structure_cap_status", "missing_receipt")
    ),
    row(
      "burst_threshold_stage4", "burst_threshold_stage4_receipt_v1",
      stage4_not_applicable || (
        isTRUE(scalar(stage4, "stage4_search_exhausted", FALSE)) &&
          !isTRUE(scalar(stage4, "cap_check_triggered", TRUE)) &&
          identical(scalar(stage4, "coverage_status", ""),
                    "stage4_search_complete")
      ),
      isTRUE(scalar(stage4, "cap_check_triggered", FALSE)) ||
        identical(scalar(stage4, "coverage_status", ""),
                  "stage4_incomplete_early_stop"),
      scalar(stage4, "coverage_status", "missing_receipt")
    ),
    row(
      "burst_union_rank", "burst_union_rank_receipt_v1",
      isTRUE(scalar(union, "ranking_exhausted", FALSE)) &&
        identical(scalar(union, "rank_scope_status", ""),
                  "complete_over_observed_inputs") &&
        scalar(union, "structure_upstream_status", "") %in% c(
          "stage3_scan_complete", "stage3_not_applicable_or_unavailable"
        ) &&
        scalar(union, "threshold_upstream_status", "") %in% c(
          "stage4_search_complete", "stage4_not_applicable_short_train"
        ),
      !isTRUE(scalar(union, "ranking_exhausted", FALSE)) ||
        scalar(union, "truncated_n", 0L) > 0L ||
        identical(scalar(union, "threshold_upstream_status", ""),
                  "stage4_incomplete_early_stop"),
      paste(
        scalar(union, "rank_scope_status", "missing_receipt"),
        scalar(union, "union_cap_status", "missing_receipt"), sep = "|"
      )
    ),
    row(
      "burst_refractory", "burst_refractory_receipt_v1",
      isTRUE(scalar(refractory, "replay_exhausted", FALSE)), FALSE,
      scalar(refractory, "coverage_status", "missing_receipt")
    ),
    row(
      "burst_pause_boundary", "burst_pause_boundary_receipt_v1",
      isTRUE(scalar(boundary, "replay_exhausted", FALSE)), FALSE,
      scalar(boundary, "coverage_status", "missing_receipt")
    ),
    row(
      "burst_hard_threshold", "burst_hard_threshold_receipt_v1",
      (hard_not_applicable ||
        isTRUE(scalar(hard, "run_scan_exhausted", FALSE))) &&
        !isTRUE(scalar(hard, "cap_applied", TRUE)),
      isTRUE(scalar(hard, "cap_applied", FALSE)),
      scalar(hard, "coverage_status", "missing_receipt")
    ),
    row(
      "burst_pause_ownership", "burst_pause_ownership_receipt_v1",
      !isTRUE(scalar(ownership, "cap_applied", TRUE)) &&
        grepl("exhausted|not_invoked", scalar(
          ownership, "coverage_status", "missing_receipt"
        )),
      isTRUE(scalar(ownership, "cap_applied", FALSE)),
      scalar(ownership, "coverage_status", "missing_receipt")
    ),
    row(
      "contextual_pause", "contextual_pause_receipt_v1",
      isTRUE(scalar(contextual, "scan_exhausted", FALSE)) &&
        !isTRUE(scalar(contextual, "audit_capture_cap_applied", TRUE)),
      isTRUE(scalar(contextual, "audit_capture_cap_applied", FALSE)),
      scalar(contextual, "coverage_status", "missing_receipt")
    ),
    row(
      "final_gap", "gap_final_receipt_v1",
      isTRUE(scalar(gap, "exact_candidate_input_binding", FALSE)) &&
        identical(scalar(gap, "arbitration_replay_status", ""),
                  "validated_exact") &&
        identical(scalar(gap, "materialized_gap_identity_status", ""),
                  "validated_exact_prevalidation_identity"),
      FALSE, scalar(gap, "materialized_gap_identity_status", "missing_receipt")
    ),
    row(
      "broad_hfs_state", "hfs_state_receipt_v1",
      identical(scalar(hfs, "connector_identity_status", ""),
                "validated_exact") &&
        identical(scalar(hfs, "support_envelope_closure_status", ""),
                  "validated_separate") &&
        identical(scalar(hfs, "parent_signature_status", ""),
                  "validated_unchanged"),
      FALSE, scalar(hfs, "support_envelope_closure_status", "missing_receipt")
    ),
    row(
      "tonic_state", "tonic_state_receipt_v1",
      !any(c(
        scalar(tonic, "sparse_evidence_abstention_status", "mismatch"),
        scalar(tonic, "burst_overlay_status", "mismatch"),
        scalar(tonic, "fragment_redetection_status", "mismatch")
      ) == "mismatch"),
      FALSE, scalar(tonic, "fragment_redetection_status", "missing_receipt")
    ),
    row(
      "nested_hfs_review", "nested_hfs_review_receipt_v1",
      identical(scalar(nested, "review_identity_status", ""),
                "validated_review_not_event") &&
        identical(scalar(nested, "local_contrast_status", ""),
                  "validated_local_hfs_background") &&
        identical(scalar(nested, "parent_invariance_status", ""),
                  "validated_no_state_effect") &&
        identical(scalar(nested, "promotion_authority_status", ""),
                  "separate_review_transition_required"),
      FALSE, scalar(nested, "promotion_authority_status", "missing_receipt")
    )
  )
  out <- dplyr::bind_rows(rows)
  rownames(out) <- NULL
  out
}

stpd_candidate_lineage_complete_universe_resource_audit <- function(
    observations, detector_candidate_cap, nested_review_candidate_cap) {
  scalar <- function(hook, field, default = NA) {
    x <- observations[[hook]]
    if (is.null(x) || !is.data.frame(x) || nrow(x) != 1L ||
        !(field %in% names(x))) return(default)
    x[[field]][[1L]]
  }
  row <- function(scope, hook, cap, observed, retained, cap_triggered,
                  pre_cap_observed, closure_complete) {
    data.frame(
      resource_scope = scope, receipt_hook = hook,
      candidate_cap = as.integer(cap), observed_candidate_n = as.integer(observed),
      retained_candidate_n = as.integer(retained),
      cap_triggered = isTRUE(cap_triggered),
      pre_cap_universe_observed = isTRUE(pre_cap_observed),
      closure_complete = isTRUE(closure_complete),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  nested_raw_n <- scalar(
    "nested_hfs_review_receipt_v1", "raw_candidate_n", 0L
  )
  rows <- list(
    row(
      "burst_dispatch", "burst_dispatch_final_route_v1",
      scalar("burst_dispatch_final_route_v1", "max_candidates", NA_integer_),
      scalar("burst_union_rank_receipt_v1", "observed_input_n", 0L),
      scalar("burst_union_rank_receipt_v1", "retained_n", 0L),
      scalar("burst_union_rank_receipt_v1", "truncated_n", 0L) > 0L,
      TRUE, TRUE
    ),
    row(
      "burst_structure_first", "burst_structure_first_scan_receipt_v1",
      scalar("burst_structure_first_scan_receipt_v1", "max_candidates",
             NA_integer_),
      scalar("burst_structure_first_scan_receipt_v1", "pre_cap_proposal_n", 0L),
      scalar("burst_structure_first_scan_receipt_v1", "retained_n", 0L),
      scalar("burst_structure_first_scan_receipt_v1", "truncated_n", 0L) > 0L,
      TRUE, TRUE
    ),
    row(
      "burst_threshold_stage4", "burst_threshold_stage4_receipt_v1",
      scalar("burst_threshold_stage4_receipt_v1", "max_candidates", NA_integer_),
      scalar("burst_threshold_stage4_receipt_v1", "emitted_candidate_n", 0L),
      scalar("burst_threshold_stage4_receipt_v1", "emitted_candidate_n", 0L),
      scalar("burst_threshold_stage4_receipt_v1", "cap_check_triggered", FALSE),
      !isTRUE(scalar(
        "burst_threshold_stage4_receipt_v1", "cap_check_triggered", TRUE
      )),
      !isTRUE(scalar(
        "burst_threshold_stage4_receipt_v1", "cap_check_triggered", TRUE
      ))
    ),
    row(
      "burst_union_rank", "burst_union_rank_receipt_v1",
      scalar("burst_union_rank_receipt_v1", "max_candidates", NA_integer_),
      scalar("burst_union_rank_receipt_v1", "observed_input_n", 0L),
      scalar("burst_union_rank_receipt_v1", "retained_n", 0L),
      scalar("burst_union_rank_receipt_v1", "truncated_n", 0L) > 0L,
      TRUE, TRUE
    ),
    row(
      "nested_hfs_review", "nested_hfs_review_receipt_v1",
      nested_review_candidate_cap, nested_raw_n,
      scalar("nested_hfs_review_receipt_v1", "standardized_candidate_n", 0L),
      nested_raw_n >= nested_review_candidate_cap,
      nested_raw_n < nested_review_candidate_cap,
      nested_raw_n < nested_review_candidate_cap
    )
  )
  out <- dplyr::bind_rows(rows)
  valid_caps <- is.finite(out$candidate_cap) & out$candidate_cap >= 1L
  detector_scopes <- out$resource_scope != "nested_hfs_review"
  expected_caps <- ifelse(
    detector_scopes, detector_candidate_cap, nested_review_candidate_cap
  )
  out$closure_complete <- out$closure_complete & valid_caps &
    out$candidate_cap == expected_caps
  rownames(out) <- NULL
  out
}

stpd_candidate_lineage_complete_universe_release_eligible <- function(shard) {
  if (is.null(shard)) return(FALSE)
  registry <- stpd_candidate_lineage_observation_hook_registry()$hook_id
  release_hooks <- names(
    stpd_candidate_lineage_complete_universe_release_hooks()
  )
  expected_prior <- setdiff(registry, release_hooks)
  hooks_complete <- identical(
    shard$observation_index$hook_id, expected_prior
  ) && identical(names(shard$observations), expected_prior)
  if (!hooks_complete) return(FALSE)
  audit <- stpd_candidate_lineage_complete_universe_root_audit(
    shard$observations
  )
  nrow(audit) > 0L && all(audit$closure_complete)
}

stpd_candidate_lineage_complete_universe_release_replay <- function(context) {
  prior_index <- context$prior_index
  manifest <- data.frame(
    train = context$train, root_id = "complete_candidate_universe_release",
    expected_prior_hook_n = as.integer(length(context$expected_prior_hooks)),
    observed_prior_hook_n = as.integer(nrow(prior_index)),
    release_hook_n = as.integer(length(
      stpd_candidate_lineage_complete_universe_release_hooks()
    )),
    max_observation_hook_n = as.integer(nrow(
      stpd_candidate_lineage_observation_hook_registry()
    )),
    prior_observation_record_n = as.integer(sum(prior_index$record_n)),
    root_receipt_n = as.integer(nrow(context$root_audit)),
    prior_hook_order_status = "exact_registry_order",
    root_closure_status = "all_roots_closed_with_explicit_cap_accounting",
    incomplete_root_ids = "",
    cap_or_early_stop_root_ids = paste(
      context$root_audit$root_id[context$root_audit$cap_or_early_stop],
      collapse = ","
    ),
    prior_hook_ids_sha256 = stpd_candidate_lineage_complete_universe_release_hash(
      prior_index$hook_id, "prior-hook-ids"
    ),
    prior_index_sha256 = stpd_candidate_lineage_complete_universe_release_hash(
      prior_index, "prior-index"
    ),
    root_audit_sha256 = stpd_candidate_lineage_complete_universe_release_hash(
      context$root_audit, "root-audit"
    ),
    all_family_roots_closed = TRUE,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  products <- data.frame(
    train = context$train,
    audit_candidate_n = as.integer(nrow(context$audit)),
    pattern_auto_labeled_isi_n = as.integer(sum(
      !is.na(context$pattern_auto) & nzchar(as.character(context$pattern_auto))
    )),
    auto_score_nonmissing_n = as.integer(sum(is.finite(context$auto_score))),
    shadow_candidate_n = as.integer(nrow(context$shadow_candidates)),
    nested_review_candidate_n = as.integer(nrow(context$nested_review)),
    tonic_review_candidate_n = as.integer(nrow(context$tonic_review)),
    audit_sha256 = stpd_candidate_lineage_complete_universe_release_hash(
      context$audit, "scientific-audit"
    ),
    pattern_auto_sha256 = stpd_candidate_lineage_complete_universe_release_hash(
      context$pattern_auto, "scientific-pattern-auto"
    ),
    auto_score_sha256 = stpd_candidate_lineage_complete_universe_release_hash(
      context$auto_score, "scientific-auto-score"
    ),
    shadow_candidates_sha256 =
      stpd_candidate_lineage_complete_universe_release_hash(
        context$shadow_candidates, "multitrack-shadow-candidates"
      ),
    nested_review_sha256 = stpd_candidate_lineage_complete_universe_release_hash(
      context$nested_review, "nested-review"
    ),
    tonic_review_sha256 = stpd_candidate_lineage_complete_universe_release_hash(
      context$tonic_review, "tonic-review"
    ),
    multitrack_shadow_sha256 =
      stpd_candidate_lineage_complete_universe_release_hash(
        context$multitrack_shadow, "multitrack-shadow"
      ),
    multitrack_compatibility_shadow_sha256 =
      stpd_candidate_lineage_complete_universe_release_hash(
        context$multitrack_compatibility_shadow,
        "multitrack-compatibility-shadow"
      ),
    event_grammar_params_sha256 =
      stpd_candidate_lineage_complete_universe_release_hash(
        context$event_grammar_params, "event-grammar-params"
      ),
    final_train_product_sha256 =
      stpd_candidate_lineage_complete_universe_release_hash(
        context$final_train_product, "final-returned-train-product"
      ),
    final_hfs_parent_signature = context$final_hfs_parent_signature,
    binding_status = "exact_returned_train_product_bound",
    stringsAsFactors = FALSE, check.names = FALSE
  )

  receipt <- data.frame(
    train = context$train, root_id = "complete_candidate_universe_release",
    observation_universe_status = "direct_complete",
    all_family_roots_status = "all_frozen_roots_closed",
    final_products_binding_status = "exact_returned_train_product_bound",
    resource_budget_status =
      "exact_all_candidate_caps_bound_no_unobserved_early_stop",
    canonical_adapter_status =
      "not_materialized_adapter_mapping_unavailable",
    max_observation_hook_n = as.integer(nrow(
      stpd_candidate_lineage_observation_hook_registry()
    )),
    detector_candidate_cap = context$detector_candidate_cap,
    nested_review_candidate_cap = context$nested_review_candidate_cap,
    resource_scope_n = as.integer(nrow(context$resource_audit)),
    resource_cap_triggered_n = as.integer(sum(
      context$resource_audit$cap_triggered
    )),
    release_detector_invocation_n = 0L,
    release_scientific_candidate_n = 0L,
    publication_authority = FALSE,
    scientific_result_influence = "none_observer_release_only",
    manifest_payload_sha256 =
      stpd_candidate_lineage_complete_universe_release_hash(
        manifest, "manifest"
      ),
    products_payload_sha256 =
      stpd_candidate_lineage_complete_universe_release_hash(
        products, "products"
      ),
    resource_audit_sha256 =
      stpd_candidate_lineage_complete_universe_release_hash(
        context$resource_audit, "resource-audit"
      ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  list(manifest = manifest, products = products, receipt = receipt)
}

stpd_candidate_lineage_begin_complete_universe_release <- function(
    shard, train, audit, pattern_auto, multitrack_shadow,
    nested_review, tonic_review, final_hfs_parent_signature,
    final_train_product, params, vp) {
  if (is.null(shard)) return(invisible(NULL))
  stpd_candidate_lineage_validate_train_collector(
    shard, "open", train, validate_observations = FALSE
  )
  registry <- stpd_candidate_lineage_observation_hook_registry()$hook_id
  release_hooks <- names(
    stpd_candidate_lineage_complete_universe_release_hooks()
  )
  expected_prior <- setdiff(registry, release_hooks)
  actual_prior <- shard$observation_index$hook_id
  if (!identical(actual_prior, expected_prior) ||
      !identical(names(shard$observations), expected_prior)) {
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_prior_incomplete",
      paste(
        "Complete-universe release requires all prior frozen hooks exactly",
        "once and in registry order before any release observation is written."
      )
    )
  }
  root_audit <- stpd_candidate_lineage_complete_universe_root_audit(
    shard$observations
  )
  if (!nrow(root_audit) || any(!root_audit$closure_complete)) {
    failed <- root_audit$root_id[!root_audit$closure_complete]
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_root_incomplete",
      paste(
        "Complete-universe release requires exhaustive or explicitly",
        "not-applicable root receipts; incomplete roots:",
        paste(failed, collapse = ", ")
      )
    )
  }
  shadow_candidates <- as.data.frame(
    multitrack_shadow$candidates %||% data.frame(), stringsAsFactors = FALSE
  )
  nested_settings <- stpd_nested_hfs_detector_settings(params, vp)
  detector_cap <- suppressWarnings(as.integer(vp$max_candidates))[1L]
  nested_cap <- suppressWarnings(as.integer(nested_settings$max_candidates))[1L]
  if (!is.finite(detector_cap) || detector_cap < 1L ||
      !is.finite(nested_cap) || nested_cap < 1L) {
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_resource_budget_invalid",
      "Existing detector candidate caps must be finite positive integers."
    )
  }
  if (!is.data.frame(final_train_product) ||
      !(all(c("pattern_auto", "auto_score") %in%
            names(final_train_product))) ||
      !identical(as.character(final_train_product$pattern_auto),
                 as.character(pattern_auto)) ||
      !identical(attr(final_train_product, "candidate_diagnostic_audit",
                      exact = TRUE), audit) ||
      !identical(attr(final_train_product, "multitrack_shadow", exact = TRUE),
                 multitrack_shadow) ||
      !identical(attr(final_train_product, "tonic_review_candidates",
                      exact = TRUE), tonic_review) ||
      !identical(attr(final_train_product,
                      "nested_hfs_burst_review_candidates", exact = TRUE),
                 nested_review) ||
      !identical(attr(final_train_product, "event_grammar_params", exact = TRUE),
                 vp)) {
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_product_binding_invalid",
      paste(
        "Final train product and all release components must be exact",
        "identical objects before observer-only release."
      )
    )
  }
  compatibility_shadow <- attr(
    final_train_product, "multitrack_compatibility_shadow", exact = TRUE
  )
  if (is.null(compatibility_shadow)) {
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_product_binding_invalid",
      "Final train product is missing its compatibility shadow."
    )
  }
  resource_audit <- stpd_candidate_lineage_complete_universe_resource_audit(
    shard$observations, detector_cap, nested_cap
  )
  if (!nrow(resource_audit) || any(!resource_audit$closure_complete)) {
    failed <- resource_audit$resource_scope[!resource_audit$closure_complete]
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_resource_incomplete",
      paste(
        "Complete-universe release cannot certify capped resources:",
        paste(failed, collapse = ", ")
      )
    )
  }
  context <- unserialize(serialize(list(
    train = as.character(train)[1L],
    expected_prior_hooks = expected_prior,
    prior_index = shard$observation_index,
    root_audit = root_audit,
    audit = as.data.frame(audit, stringsAsFactors = FALSE),
    pattern_auto = as.character(pattern_auto),
    auto_score = as.numeric(final_train_product$auto_score),
    shadow_candidates = shadow_candidates,
    multitrack_shadow = multitrack_shadow,
    multitrack_compatibility_shadow = compatibility_shadow,
    event_grammar_params = vp,
    final_train_product = final_train_product,
    nested_review = as.data.frame(nested_review, stringsAsFactors = FALSE),
    tonic_review = as.data.frame(tonic_review, stringsAsFactors = FALSE),
    final_hfs_parent_signature =
      as.character(final_hfs_parent_signature)[1L],
    detector_candidate_cap = as.integer(detector_cap),
    nested_review_candidate_cap = as.integer(nested_cap),
    resource_audit = resource_audit
  ), NULL, version = 3L))
  name <- "complete_universe_release_context"
  if (exists(name, shard, inherits = FALSE)) {
    if (!bindingIsLocked(name, shard) ||
        !identical(get(name, shard), context)) {
      stpd_candidate_lineage_abort(
        "collector_candidate_universe_release_begin_conflict",
        "Complete-universe release context conflicts with its first capture."
      )
    }
  } else {
    assign(name, context, shard)
    lockBinding(name, shard)
  }
  invisible(context)
}

stpd_candidate_lineage_capture_complete_universe_release <- function(
    shard, train, .test_fail_after_hook = getOption(
      "stpd.test.candidate_universe_release_fail_after_hook", NULL
    )) {
  if (is.null(shard)) return(invisible(NULL))
  name <- "complete_universe_release_context"
  if (!exists(name, shard, inherits = FALSE) ||
      !bindingIsLocked(name, shard)) {
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_release_begin_missing",
      "Complete-universe release requires its locked live begin context."
    )
  }
  context <- get(name, shard)
  if (!identical(context$train, as.character(train)[1L])) {
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_release_train_conflict",
      "Complete-universe release train differs from its begin context."
    )
  }
  if (!is.null(.test_fail_after_hook)) {
    .test_fail_after_hook <- suppressWarnings(as.integer(
      .test_fail_after_hook
    ))[1L]
    if (!is.finite(.test_fail_after_hook) ||
        !(.test_fail_after_hook %in% seq_along(
          stpd_candidate_lineage_complete_universe_release_hooks()
        ))) {
      stpd_candidate_lineage_abort(
        "collector_candidate_universe_release_fault_invalid",
        "Internal release fault injection must identify one release hook."
      )
    }
  }
  replay <- stpd_candidate_lineage_complete_universe_release_replay(context)
  cache_name <- "complete_universe_release_replay_cache"
  if (exists(cache_name, shard, inherits = FALSE)) {
    stpd_candidate_lineage_abort(
      "collector_candidate_universe_release_cache_conflict",
      "Complete-universe release cache already exists before transaction."
    )
  }
  before <- unserialize(serialize(list(
    observation_index = shard$observation_index,
    observations = shard$observations,
    instrumentation_status = shard$instrumentation_status
  ), NULL, version = 3L))
  rollback <- function() {
    if (exists(cache_name, shard, inherits = FALSE)) {
      rm(list = cache_name, envir = shard)
    }
    shard$observation_index <- before$observation_index
    shard$observations <- before$observations
    shard$instrumentation_status <- before$instrumentation_status
    invisible(NULL)
  }
  committed <- FALSE
  on.exit({
    if (!isTRUE(committed)) rollback()
  }, add = TRUE)
  tryCatch({
    assign(cache_name, replay, shard)
    hooks <- stpd_candidate_lineage_complete_universe_release_hooks()
    for (position in seq_along(hooks)) {
      hook <- names(hooks)[[position]]
      stpd_candidate_lineage_collector_capture(
        shard, hook, replay[[hooks[[hook]]]]
      )
      if (!is.null(.test_fail_after_hook) &&
          identical(position, .test_fail_after_hook)) {
        stpd_candidate_lineage_abort(
          "collector_candidate_universe_release_fault_injected",
          paste("Injected release fault after hook", position)
        )
      }
    }
    expected <- stpd_candidate_lineage_observation_hook_registry()$hook_id
    if (!identical(shard$observation_index$hook_id, expected) ||
        !identical(names(shard$observations), expected)) {
      stpd_candidate_lineage_abort(
        "collector_candidate_universe_release_incomplete",
        "Release hooks did not close the exact frozen observation registry."
      )
    }
    shard$instrumentation_status <- "direct_complete"
    stpd_candidate_lineage_validate_observation_storage(shard)
  }, error = function(error) {
    stop(error)
  })
  committed <- TRUE
  invisible(replay)
}

stpd_candidate_lineage_complete_universe_release_payload_is_valid <- function(
    shard, hook, payload) {
  context_name <- "complete_universe_release_context"
  cache_name <- "complete_universe_release_replay_cache"
  if (!exists(context_name, shard, inherits = FALSE) ||
      !bindingIsLocked(context_name, shard) ||
      !exists(cache_name, shard, inherits = FALSE)) return(FALSE)
  context <- get(context_name, shard)
  fresh <- stpd_candidate_lineage_complete_universe_release_replay(context)
  cached <- get(cache_name, shard)
  if (is.null(fresh) || !identical(fresh, cached)) return(FALSE)
  map <- stpd_candidate_lineage_complete_universe_release_hooks()
  key <- unname(map[[hook]])
  if (is.null(key) || !identical(payload, cached[[key]])) return(FALSE)
  position <- match(hook, names(map))
  prior <- if (position <= 1L) character() else
    names(map)[seq_len(position - 1L)]
  all(vapply(prior, function(previous) identical(
    shard$observations[[previous]], cached[[map[[previous]]]]
  ), logical(1)))
}
