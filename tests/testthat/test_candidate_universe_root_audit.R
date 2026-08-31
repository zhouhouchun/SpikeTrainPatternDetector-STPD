root_audit_receipts <- function() {
  list(
    burst_dispatch_final_route_v1 = data.frame(max_candidates = 500L),
    pause_raw_receipt_v1 = data.frame(
      pause_pattern_requested = TRUE, scan_exhausted = TRUE,
      cap_applied = FALSE, coverage_status = "complete"
    ),
    burst_structure_first_scan_receipt_v1 = data.frame(
      structure_first_enabled = TRUE, scan_exhausted = TRUE,
      max_candidates = 500L, pre_cap_proposal_n = 4L, retained_n = 4L,
      truncated_n = 0L, structure_cap_status = "not_applied_within_limit"
    ),
    burst_threshold_stage4_receipt_v1 = data.frame(
      stage4_search_exhausted = TRUE, cap_check_triggered = FALSE,
      max_candidates = 500L, emitted_candidate_n = 3L,
      coverage_status = "stage4_search_complete"
    ),
    burst_union_rank_receipt_v1 = data.frame(
      ranking_exhausted = TRUE,
      rank_scope_status = "complete_over_observed_inputs",
      structure_upstream_status = "stage3_scan_complete",
      threshold_upstream_status = "stage4_search_complete",
      max_candidates = 500L, observed_input_n = 7L, retained_n = 7L,
      truncated_n = 0L, union_cap_status = "not_applied_within_limit"
    ),
    burst_refractory_receipt_v1 = data.frame(
      replay_exhausted = TRUE, coverage_status = "complete"
    ),
    burst_pause_boundary_receipt_v1 = data.frame(
      replay_exhausted = TRUE, coverage_status = "complete"
    ),
    burst_hard_threshold_receipt_v1 = data.frame(
      applicability_status = "invoked_no_train_range",
      run_scan_exhausted = FALSE, cap_applied = FALSE,
      coverage_status = "hard_threshold_invoked_no_train_range"
    ),
    burst_pause_ownership_receipt_v1 = data.frame(
      cap_applied = FALSE, coverage_status = "not_invoked_empty_input"
    ),
    contextual_pause_receipt_v1 = data.frame(
      scan_exhausted = TRUE, audit_capture_cap_applied = FALSE,
      coverage_status = "complete"
    ),
    gap_final_receipt_v1 = data.frame(
      exact_candidate_input_binding = TRUE,
      arbitration_replay_status = "validated_exact",
      materialized_gap_identity_status =
        "validated_exact_prevalidation_identity"
    ),
    hfs_state_receipt_v1 = data.frame(
      connector_identity_status = "validated_exact",
      support_envelope_closure_status = "validated_separate",
      parent_signature_status = "validated_unchanged"
    ),
    tonic_state_receipt_v1 = data.frame(
      sparse_evidence_abstention_status = "not_applicable",
      burst_overlay_status = "validated",
      fragment_redetection_status = "not_applicable"
    ),
    nested_hfs_review_receipt_v1 = data.frame(
      raw_candidate_n = 2L, standardized_candidate_n = 2L,
      review_identity_status = "validated_review_not_event",
      local_contrast_status = "validated_local_hfs_background",
      parent_invariance_status = "validated_no_state_effect",
      promotion_authority_status = "separate_review_transition_required"
    )
  )
}

test_that("root audit distinguishes terminal non-applicability from truncation", {
  observations <- root_audit_receipts()
  audit <- stpd_candidate_lineage_complete_universe_root_audit(observations)
  expect_identical(nrow(audit), 13L)
  expect_true(all(audit$closure_complete))
  expect_false(any(audit$cap_or_early_stop))

  observations$burst_structure_first_scan_receipt_v1$truncated_n <- 3L
  observations$burst_union_rank_receipt_v1$truncated_n <- 2L
  capped <- stpd_candidate_lineage_complete_universe_root_audit(observations)
  expect_true(all(capped$closure_complete))
  expect_setequal(
    capped$root_id[capped$cap_or_early_stop],
    c("burst_structure_first", "burst_union_rank")
  )
})

test_that("resource audit binds every active candidate cap and fails closed", {
  observations <- root_audit_receipts()
  audit <- stpd_candidate_lineage_complete_universe_resource_audit(
    observations, 500L, 300L
  )
  expect_identical(nrow(audit), 5L)
  expect_true(all(audit$closure_complete))
  expect_false(any(audit$cap_triggered))

  observations$burst_structure_first_scan_receipt_v1$truncated_n <- 1L
  observations$burst_union_rank_receipt_v1$truncated_n <- 1L
  capped <- stpd_candidate_lineage_complete_universe_resource_audit(
    observations, 500L, 300L
  )
  expect_true(all(capped$closure_complete))
  expect_setequal(
    capped$resource_scope[capped$cap_triggered],
    c("burst_dispatch", "burst_structure_first", "burst_union_rank")
  )

  observations <- root_audit_receipts()
  observations$nested_hfs_review_receipt_v1$raw_candidate_n <- 300L
  incomplete <- stpd_candidate_lineage_complete_universe_resource_audit(
    observations, 500L, 300L
  )
  expect_false(incomplete$closure_complete[
    incomplete$resource_scope == "nested_hfs_review"
  ])

  observations <- root_audit_receipts()
  observations$burst_union_rank_receipt_v1$max_candidates <- 499L
  mismatch <- stpd_candidate_lineage_complete_universe_resource_audit(
    observations, 500L, 300L
  )
  expect_false(mismatch$closure_complete[
    mismatch$resource_scope == "burst_union_rank"
  ])
})

test_that("root audit rejects early stop and observer-side capture caps", {
  observations <- root_audit_receipts()
  observations$burst_threshold_stage4_receipt_v1$stage4_search_exhausted <- FALSE
  observations$burst_threshold_stage4_receipt_v1$cap_check_triggered <- TRUE
  observations$burst_threshold_stage4_receipt_v1$coverage_status <-
    "stage4_incomplete_early_stop"
  observations$burst_union_rank_receipt_v1$threshold_upstream_status <-
    "stage4_incomplete_early_stop"
  audit <- stpd_candidate_lineage_complete_universe_root_audit(observations)
  expect_false(audit$closure_complete[audit$root_id ==
    "burst_threshold_stage4"])
  expect_false(audit$closure_complete[audit$root_id == "burst_union_rank"])

  observations <- root_audit_receipts()
  observations$contextual_pause_receipt_v1$audit_capture_cap_applied <- TRUE
  audit <- stpd_candidate_lineage_complete_universe_root_audit(observations)
  expect_false(audit$closure_complete[audit$root_id == "contextual_pause"])
})
