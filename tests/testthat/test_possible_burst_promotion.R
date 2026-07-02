test_that("possible_burst promotion previews, promotes, and reverts with audit", {
  t <- c(0, 0.10, 0.105, 0.110, 0.30, 0.305, 0.50)
  dat <- data.frame(
    idx = seq_along(t),
    timestamp_sec = t,
    ISI_sec = c(NA_real_, diff(t)),
    pattern_manual = rep("", length(t)),
    pattern_manual_negative = rep("", length(t)),
    pattern_auto = rep("", length(t)),
    auto_score = seq_along(t) / 10,
    stringsAsFactors = FALSE
  )
  dat$pattern_auto[2:4] <- "possible_burst"
  dat$pattern_auto[6] <- "possible_burst"
  dat$pattern_manual[6] <- "pause"
  ds <- make_dataset("pb_promote", "synthetic", list(train_1 = dat), unit_in = "s")

  pr <- stpd_possible_burst_promotion_preview(ds, selected_trains = "train_1")
  expect_equal(pr$total_eligible_isi, 3L)
  expect_equal(pr$total_eligible_events, 1L)
  expect_equal(pr$summary$n_blocked_by_manual_isi, 1L)

  res <- stpd_promote_possible_burst(ds, selected_trains = "train_1", audit_id = "audit_1")
  out <- res$dataset
  expect_equal(as.character(out$trains$train_1$pattern_auto[2:4]), rep("possible_burst", 3))
  expect_equal(as.character(out$trains$train_1$pattern_manual[2:4]), rep("burst", 3))
  expect_equal(as.character(out$trains$train_1$pattern_user_override[2:4]), rep("burst", 3))
  expect_true(nrow(out$results$possible_burst_promotion_audit) == 1L)

  ev <- derive_interval_tables(out$trains, source = "final", min_isi_sec = 0.0009)$events
  burst_ev <- ev[as.character(ev$pattern) == "burst", , drop = FALSE]
  expect_true(nrow(burst_ev) >= 1L)
  expect_true(any(burst_ev$user_promoted_possible_burst))
  expect_true("user_override_reason" %in% names(ev))

  rev <- stpd_revert_possible_burst_promotions(out, selected_trains = "train_1")
  restored <- rev$dataset$trains$train_1
  expect_equal(as.character(restored$pattern_manual[2:4]), rep("", 3))
  expect_equal(as.character(restored$pattern_auto[2:4]), rep("possible_burst", 3))
  expect_equal(as.character(restored$pattern_manual[6]), "pause")
})

test_that("possible_burst promotion can overwrite manual labels only when requested", {
  t <- c(0, 0.10, 0.105, 0.110, 0.30)
  dat <- data.frame(
    idx = seq_along(t),
    timestamp_sec = t,
    ISI_sec = c(NA_real_, diff(t)),
    pattern_manual = rep("", length(t)),
    pattern_manual_negative = rep("", length(t)),
    pattern_auto = rep("", length(t)),
    stringsAsFactors = FALSE
  )
  dat$pattern_auto[2:4] <- "possible_burst"
  dat$pattern_manual[3] <- "tonic"
  ds <- make_dataset("pb_overwrite", "synthetic", list(train_1 = dat), unit_in = "s")

  protected <- stpd_possible_burst_promotion_preview(ds, "train_1", overwrite_manual = FALSE)
  overwritten <- stpd_possible_burst_promotion_preview(ds, "train_1", overwrite_manual = TRUE)
  expect_equal(protected$total_eligible_isi, 2L)
  expect_equal(overwritten$total_eligible_isi, 3L)

  res <- stpd_promote_possible_burst(ds, "train_1", overwrite_manual = TRUE)
  expect_equal(as.character(res$dataset$trains$train_1$pattern_manual[2:4]), rep("burst", 3))
  rev <- stpd_revert_possible_burst_promotions(res$dataset, "train_1")
  expect_equal(as.character(rev$dataset$trains$train_1$pattern_manual[3]), "tonic")
})
