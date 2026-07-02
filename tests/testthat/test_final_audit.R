test_that("final audit promotes possible labels without rewriting auto or manual labels", {
  ts <- c(0, cumsum(c(0.02, 0.021, 0.019, 0.20, 0.45, 0.10, 0.11)))
  dat <- data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, diff(ts)),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  dat$pattern_auto[2:4] <- "possible_burst"
  dat$pattern_auto[6] <- "pause"
  ds <- list(
    trains = list(train_1 = dat),
    results = list(),
    meta = list(display_name = "audit_test")
  )

  res <- stpd_apply_final_audit(ds, selected_trains = "train_1",
                                promote_possible = TRUE,
                                min_isi_sec = 0.001,
                                audit_id = "audit_unit_test",
                                reason = "unit_test")
  out <- res$dataset$trains$train_1

  expect_equal(out$pattern_auto[2:4], rep("possible_burst", 3))
  expect_true(all(out$pattern_manual[2:4] == ""))
  expect_equal(out$pattern_audit_final[2:4], rep("burst", 3))
  expect_equal(out$pattern_audit_from[2:4], rep("possible_burst", 3))
  expect_equal(out$pattern_audit_to[2:4], rep("burst", 3))
  expect_equal(res$summary$n_promoted_isi, 3)
  expect_equal(res$summary$n_promoted_events, 1)
})

test_that("audit_final label source feeds state space, events, and ML features", {
  ts <- c(0, cumsum(c(0.02, 0.021, 0.019, 0.20, 0.45, 0.10, 0.11)))
  dat <- data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, diff(ts)),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  dat$pattern_auto[2:4] <- "possible_burst"
  dat$pattern_auto[6] <- "pause"
  ds <- list(
    trains = list(train_1 = dat),
    results = list(),
    meta = list(display_name = "audit_test")
  )
  ds <- stpd_apply_final_audit(ds, promote_possible = TRUE,
                               min_isi_sec = 0.001,
                               audit_id = "audit_unit_test")$dataset
  dat2 <- ds$trains$train_1

  labs <- stpd_state_space_pattern_labels(dat2, label_source = "audit_final",
                                          min_isi_sec = 0.001)
  expect_true("burst" %in% labs)
  expect_false("possible_burst" %in% labs)

  ev <- derive_interval_tables(ds$trains, source = "audit_final",
                               min_isi_sec = 0.001)$events
  expect_true("burst" %in% ev$pattern)
  expect_false("possible_burst" %in% ev$pattern)

  ml <- extract_ml_feature_table(ds$trains, source = "audit_final",
                                 min_isi_sec = 0.001,
                                 fill_blank_others = FALSE)
  expect_true("burst" %in% ml$label)
  expect_false("possible_burst" %in% ml$label)
})

test_that("clearing selected final audit trains preserves other audit records", {
  make_dat <- function(label) {
    ts <- c(0, cumsum(c(0.02, 0.021, 0.019, 0.20)))
    dat <- data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = "",
      pattern_auto = "",
      stringsAsFactors = FALSE
    )
    dat$pattern_auto[2:4] <- label
    dat
  }
  ds <- list(
    trains = list(train_1 = make_dat("possible_burst"),
                  train_2 = make_dat("possible_burst")),
    results = list(),
    meta = list(display_name = "audit_clear_test")
  )
  ds <- stpd_apply_final_audit(ds, promote_possible = TRUE,
                               audit_id = "audit_clear_test")$dataset

  cleared <- stpd_clear_final_audit(ds, selected_trains = "train_1")

  expect_false("pattern_audit_final" %in% names(cleared$trains$train_1))
  expect_true("pattern_audit_final" %in% names(cleared$trains$train_2))
  expect_true(all(stpd_final_audit_summary(cleared)$train == "train_2"))
  expect_true(all(stpd_final_audit_events(cleared)$train == "train_2"))
  expect_true(nrow(stpd_final_audit_history(cleared)) >= 2L)
})

test_that("final audit current summary is distinct from append-only history", {
  make_dat <- function() {
    ts <- c(0, cumsum(c(0.02, 0.021, 0.019, 0.20)))
    dat <- data.frame(
      idx = seq_along(ts),
      timestamp_sec = ts,
      ISI_sec = c(NA_real_, diff(ts)),
      pattern_manual = "",
      pattern_auto = "",
      stringsAsFactors = FALSE
    )
    dat$pattern_auto[2:4] <- "possible_burst"
    dat
  }
  ds <- list(
    trains = list(train_1 = make_dat(), train_2 = make_dat()),
    results = list(),
    meta = list(display_name = "audit_history_test")
  )

  ds <- stpd_apply_final_audit(ds, selected_trains = "train_1",
                               promote_possible = TRUE,
                               audit_id = "audit_history_1")$dataset
  ds <- stpd_apply_final_audit(ds, selected_trains = "train_1",
                               promote_possible = FALSE,
                               audit_id = "audit_history_2")$dataset
  ds <- stpd_apply_final_audit(ds, selected_trains = "train_2",
                               promote_possible = TRUE,
                               audit_id = "audit_history_3")$dataset

  cur <- stpd_final_audit_summary(ds)
  hist <- stpd_final_audit_history(ds)
  ev_cur <- stpd_final_audit_events(ds)
  ev_hist <- stpd_final_audit_event_history(ds)

  expect_equal(nrow(cur), 2L)
  expect_equal(sum(cur$train == "train_1"), 1L)
  expect_equal(cur$n_possible_after[cur$train == "train_1"], 3L)
  expect_equal(cur$n_possible_after[cur$train == "train_2"], 0L)
  expect_gte(nrow(hist), 3L)
  expect_true(all(c("audit_history_1", "audit_history_2", "audit_history_3") %in% hist$audit_id))
  expect_false(any(ev_cur$train == "train_1"))
  expect_true(any(ev_cur$train == "train_2"))
  expect_gte(nrow(ev_hist), nrow(ev_cur))
})
