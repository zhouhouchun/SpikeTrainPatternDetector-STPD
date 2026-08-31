phase2b_legacy_guard_dataset <- function() {
  timestamp <- c(0, 0.10, 0.105, 0.110, 0.30, 0.305, 0.50)
  dat <- data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, diff(timestamp)),
    pattern_manual = rep("", length(timestamp)),
    pattern_manual_negative = rep("", length(timestamp)),
    pattern_auto = rep("", length(timestamp)),
    auto_score = seq_along(timestamp) / 10,
    stringsAsFactors = FALSE
  )
  dat$pattern_auto[2:4] <- "possible_burst"
  list(
    trains = list(train_1 = dat),
    results = list(),
    meta = list(display_name = "phase2b_legacy_guard", unit_in = "s")
  )
}

phase2b_legacy_guard_state <- function(ds, lifecycle_status = "active") {
  ds$results$multitrack_review <- list(
    metadata = data.frame(
      lifecycle_status = lifecycle_status,
      stringsAsFactors = FALSE
    ),
    transition_history = data.frame(
      transition_sequence = 1L,
      transition_action = "confirm",
      stringsAsFactors = FALSE
    )
  )
  ds
}

phase2b_legacy_guard_error <- function(expr) {
  tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
}

test_that("every legacy bulk promotion entry point fails atomically with Phase 2B state", {
  ds <- phase2b_legacy_guard_state(phase2b_legacy_guard_dataset())
  before <- serialize(ds, NULL, version = 3L)

  calls <- list(
    stpd_apply_final_audit = function() {
      stpd_apply_final_audit(
        ds,
        selected_trains = "train_1",
        promote_possible = TRUE,
        audit_id = "must_not_be_written"
      )
    },
    stpd_possible_burst_promotion_preview = function() {
      stpd_possible_burst_promotion_preview(ds, selected_trains = "train_1")
    },
    stpd_promote_possible_burst = function() {
      stpd_promote_possible_burst(
        ds,
        selected_trains = "train_1",
        audit_id = "must_not_be_written"
      )
    },
    stpd_revert_possible_burst_promotions = function() {
      stpd_revert_possible_burst_promotions(ds, selected_trains = "train_1")
    }
  )

  for (api in names(calls)) {
    err <- phase2b_legacy_guard_error(calls[[api]]())
    expect_s3_class(err, "stpd_phase2b_legacy_promotion_error")
    expect_identical(err$code, "phase2b_legacy_promotion_blocked", info = api)
    expect_identical(err$api, api, info = api)
    expect_identical(serialize(ds, NULL, version = 3L), before, info = api)
  }
})

test_that("inactive or stale Phase 2B history still closes legacy promotion paths", {
  ds <- phase2b_legacy_guard_state(
    phase2b_legacy_guard_dataset(),
    lifecycle_status = "stale_requires_revalidation"
  )
  expect_true(SpikeTrainPatternDetector:::stpd_phase2b_has_state(ds))
  expect_true(SpikeTrainPatternDetector:::stpd_phase2b_legacy_has_state(ds))

  err <- phase2b_legacy_guard_error(
    stpd_possible_burst_promotion_preview(ds, selected_trains = "train_1")
  )
  expect_identical(err$code, "phase2b_legacy_promotion_blocked")
})

test_that("an unreadable Phase 2B state predicate fails closed", {
  testthat::local_mocked_bindings(
    stpd_phase2b_has_state = function(ds) stop("corrupt Phase 2B state"),
    .package = "SpikeTrainPatternDetector"
  )
  ds <- phase2b_legacy_guard_dataset()

  err <- phase2b_legacy_guard_error(
    stpd_possible_burst_promotion_preview(ds, selected_trains = "train_1")
  )
  expect_s3_class(err, "stpd_phase2b_legacy_promotion_error")
  expect_identical(err$code, "phase2b_legacy_promotion_blocked")
})

test_that("non-promoting final audit remains available and preserves Phase 2B exactly", {
  ds <- phase2b_legacy_guard_state(phase2b_legacy_guard_dataset())
  phase2b_before <- serialize(
    ds$results$multitrack_review,
    NULL,
    version = 3L
  )

  out <- stpd_apply_final_audit(
    ds,
    selected_trains = "train_1",
    promote_possible = FALSE,
    audit_id = "legacy_non_promoting_audit"
  )$dataset

  expect_identical(
    serialize(out$results$multitrack_review, NULL, version = 3L),
    phase2b_before
  )
  expect_identical(out$results$final_audit_policy$scope, "legacy_single_label")
  expect_false(out$results$final_audit_policy$promote_possible)
  expect_identical(
    out$trains$train_1$pattern_audit_final[2:4],
    rep("possible_burst", 3L)
  )
})

test_that("Phase 2A Preview alone never blocks legacy behavior", {
  plain <- phase2b_legacy_guard_dataset()
  preview_only <- plain
  preview_only$results$multitrack_preview <- list(
    sentinel = "immutable_phase2a_preview"
  )
  preview_before <- serialize(
    preview_only$results$multitrack_preview,
    NULL,
    version = 3L
  )

  expect_false(SpikeTrainPatternDetector:::stpd_phase2b_has_state(preview_only))
  expect_false(SpikeTrainPatternDetector:::stpd_phase2b_legacy_has_state(preview_only))

  plain_preview <- stpd_possible_burst_promotion_preview(
    plain,
    selected_trains = "train_1"
  )
  phase2a_preview <- stpd_possible_burst_promotion_preview(
    preview_only,
    selected_trains = "train_1"
  )
  expect_identical(phase2a_preview$summary, plain_preview$summary)
  expect_identical(phase2a_preview$events, plain_preview$events)
  expect_identical(phase2a_preview$labels, plain_preview$labels)

  promoted <- stpd_promote_possible_burst(
    preview_only,
    selected_trains = "train_1",
    audit_id = "phase2a_only_legacy_promotion"
  )$dataset
  reverted <- stpd_revert_possible_burst_promotions(
    promoted,
    selected_trains = "train_1"
  )$dataset
  audited <- stpd_apply_final_audit(
    preview_only,
    selected_trains = "train_1",
    promote_possible = TRUE,
    audit_id = "phase2a_only_final_audit"
  )$dataset

  for (out in list(promoted, reverted, audited)) {
    expect_identical(
      serialize(out$results$multitrack_preview, NULL, version = 3L),
      preview_before
    )
  }
  expect_identical(audited$results$final_audit_policy$scope, "legacy_single_label")
  expect_true(audited$results$final_audit_policy$promote_possible)
})

test_that("saved legacy TRUE policy is never replayed after Phase 2B begins", {
  plain <- phase2b_legacy_guard_dataset()
  phase2b <- phase2b_legacy_guard_state(plain)
  old_true_policy <- list(promote_possible = TRUE)

  expect_true(
    SpikeTrainPatternDetector:::stpd_legacy_final_audit_promote_from_policy(
      plain,
      old_true_policy
    )
  )
  expect_false(
    SpikeTrainPatternDetector:::stpd_legacy_final_audit_promote_from_policy(
      phase2b,
      old_true_policy
    )
  )
  expect_false(
    SpikeTrainPatternDetector:::stpd_legacy_final_audit_promote_from_policy(
      plain,
      list(promote_possible = FALSE)
    )
  )
})
