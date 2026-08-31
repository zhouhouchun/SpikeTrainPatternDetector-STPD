test_that("collect_diagnostics FALSE retains the core public candidate chain", {
  out <- stpd_detect(
    stpd_golden_test_dataset("middle_burst"),
    default_params(),
    selected_trains = "train_1",
    collect_diagnostics = FALSE,
    label_blind = TRUE
  )

  core_tables <- c(
    "events", "candidate_diagnostic_audit", "candidate_ledger",
    "candidate_features", "final_decisions"
  )
  expect_true(all(core_tables %in% names(out$results)))
  for (table_name in core_tables) {
    expect_true(
      nrow(as.data.frame(out$results[[table_name]])) > 0L,
      info = table_name
    )
  }

  # These remain optional diagnostics and must stay empty when their collection
  # is explicitly disabled (some public schema layers retain zero-row tables).
  optional_diagnostics <- c(
    "structure_candidates", "seed_candidates", "bridge_candidates",
    "burst_candidates_raw", "burst_candidates_final", "pause_candidates",
    "posthoc_fragment_audit", "near_miss_candidates"
  )
  for (table_name in intersect(optional_diagnostics, names(out$results))) {
    expect_equal(nrow(as.data.frame(out$results[[table_name]])), 0L)
  }

  core_audit <- as.data.frame(out$results$candidate_diagnostic_audit)
  ledger <- as.data.frame(out$results$candidate_ledger)
  selected <- as.logical(core_audit$selected_for_auto)
  selected[is.na(selected)] <- FALSE
  expect_true(all(selected))
  expect_false(any(as.character(core_audit$final_label) %in% c("reject", "profile")))
  expect_setequal(as.character(core_audit$candidate_id), as.character(ledger$candidate_id))

  metadata <- out$results$run_metadata_public
  for (table_name in core_tables) {
    table <- as.data.frame(out$results[[table_name]])
    expect_true(
      all(as.character(table$run_id) == metadata$run_id),
      info = table_name
    )
    expect_true(
      all(as.character(table$params_hash) == metadata$params_hash),
      info = table_name
    )
  }
  expect_false(metadata$diagnostics_collected)
  expect_equal(metadata$candidate_count, nrow(out$results$candidate_ledger))
  expect_equal(metadata$feature_count, nrow(out$results$candidate_features))
})
