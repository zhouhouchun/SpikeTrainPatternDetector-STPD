test_that("Gate B runtime checks remain pending without external attestation", {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"), params,
    selected_trains = "train_1", collect_diagnostics = FALSE,
    label_blind = TRUE
  )
  auto <- stpd_multitrack_auto(out)
  final <- stpd_multitrack_final(out)
  gate <- stpd_multitrack_gate_b_report(out)

  expect_false(auto$metadata$authoritative)
  expect_identical(auto$metadata$authority_scope, "none_preview")
  expect_identical(auto$metadata$promotion_status, "pending")
  expect_false(auto$metadata$detector_performance_eligible)
  expect_identical(auto$metadata$detector_performance_block_reason,
                   "gate_b_pending")
  expect_false(final$metadata$authoritative)
  expect_identical(final$metadata$authority_scope, "none_preview")
  expect_identical(final$metadata$promotion_status, "pending")
  expect_false(final$metadata$biological_ground_truth)
  expect_identical(gate$metadata$gate_status, "pending_release_attestation")
  expect_equal(nrow(gate$checks), 12L)
  expect_true(all(gate$checks$status == "pass"))
  expect_false(gate$metadata$biological_ground_truth)
  expect_false(gate$metadata$detector_performance_eligible)
  expect_identical(gate$metadata$migration_period,
                   "legacy_v1_and_multitrack_v2_dual_write")
})

test_that("Gate B difference audit declares legacy projection loss explicitly", {
  params <- default_params()
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"), params,
    selected_trains = "train_1", collect_diagnostics = FALSE,
    label_blind = TRUE
  )
  audit <- stpd_multitrack_gate_b_report(out)$legacy_v2_difference_audit

  expect_equal(nrow(audit), 5L)
  expect_true(all(audit$legacy_projection_role == "legacy_lossy_projection"))
  expect_true(all(audit$comparison_status %in% c(
    "exact_count_duration", "declared_lossy_projection_difference"
  )))
  expect_identical(audit$interval_n_delta,
                   audit$v2_interval_n - audit$legacy_interval_n)
  expect_identical(audit$isi_n_delta, audit$v2_isi_n - audit$legacy_isi_n)
  expect_true(all(grepl("^[0-9a-f]{64}$",
                        stpd_multitrack_gate_b_report(out)$checks$evidence_sha256)))
})

test_that("Gate B report rejects coordinated self-resealing", {
  params <- default_params()
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"), params,
    selected_trains = "train_1", collect_diagnostics = FALSE,
    label_blind = TRUE
  )
  out$results$multitrack_gate_b$checks$status[1] <- "fail"
  out$results$multitrack_gate_b$metadata$checks_passed_n <- 11L
  out$results$multitrack_gate_b$metadata$report_sha256 <-
    SpikeTrainPatternDetector:::stpd_multitrack_auto_hash(list(
      checks = out$results$multitrack_gate_b$checks,
      legacy_v2_difference_audit =
        out$results$multitrack_gate_b$legacy_v2_difference_audit
    ))
  error <- tryCatch(stpd_multitrack_gate_b_report(out), error = function(e) e)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "validation failed")
})

test_that("v2 typed tables remain non-authoritative while Gate B is pending", {
  params <- default_params()
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"), params,
    selected_trains = "train_1", collect_diagnostics = FALSE,
    label_blind = TRUE
  )
  auto <- stpd_multitrack_auto(out)
  final <- stpd_multitrack_final(out)
  auto_tables <- c(
    "events", "states", "gaps", "review_candidates",
    "state_event_relationships", "per_isi", "hfs_context", "invariants"
  )
  final_tables <- c(
    "events", "states", "gaps", "review_candidates",
    "state_event_relationships", "review_event_links", "per_isi"
  )
  expect_true(all(vapply(auto[auto_tables], function(x) {
    nrow(x) == 0L || !any(x$authoritative)
  }, logical(1))))
  expect_true(all(vapply(final[final_tables], function(x) {
    nrow(x) == 0L || !any(x$authoritative)
  }, logical(1))))
  expect_true(all(!auto$manifest$authoritative))
})
