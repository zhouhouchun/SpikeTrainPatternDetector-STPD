patrol_row <- function(provider, family, id, start, end,
                       label = "burst", track = "event", train = "T1") {
  data.frame(
    evidence_id = id, provider_key = provider, evidence_family = family,
    source_record_id = paste0("source-", id), train_key = train,
    semantic_track = track, proposed_label = label,
    target_family = stpd_patrol_label_family(label),
    start_isi = as.integer(start), end_isi = as.integer(end),
    start_spike = as.integer(start), end_spike = as.integer(end + 1L),
    start_time_sec = start / 100, end_time_sec = (end + 1L) / 100,
    authority_scope = if (provider == "native_stpd")
      "automatic_prediction_record" else "support_evidence_only",
    score_name = "", score_value = NA_real_, stringsAsFactors = FALSE
  )
}

test_that("patrol distinguishes methods from evidence families", {
  evidence <- rbind(
    patrol_row("native_stpd", "native_grammar", "n", 10, 15),
    patrol_row("mean_isi", "isi_threshold", "m", 10, 15),
    patrol_row("logisi_newbd", "isi_threshold", "l", 10, 15)
  )
  out <- stpd_build_multi_method_patrol(evidence)
  expect_equal(out$candidate_clusters$method_count, 3L)
  expect_equal(out$candidate_clusters$evidence_family_count, 2L)
  expect_identical(out$candidate_clusters$patrol_status,
                   "cross_family_agreement")
  expect_false(out$automatic_final_label)
  expect_false("final_label" %in% names(out$candidate_clusters))
})

test_that("calibrated ISI providers remain explicit auxiliary evidence", {
  family <- stpd_patrol_family_map()
  expect_identical(unname(family[["mean_isi_calibrated"]]),
                   "isi_threshold_calibrated")
  expect_identical(unname(family[["logisi_newbd_calibrated"]]),
                   "isi_threshold_calibrated")
})

test_that("correlated surprise methods are not treated as two families", {
  evidence <- rbind(
    patrol_row("poisson_surprise", "surprise", "p", 20, 25),
    patrol_row("robust_gaussian_surprise", "surprise", "r", 20, 25)
  )
  out <- stpd_build_multi_method_patrol(evidence)
  expect_equal(out$candidate_clusters$method_count, 2L)
  expect_equal(out$candidate_clusters$evidence_family_count, 1L)
  expect_identical(out$candidate_clusters$patrol_status,
                   "within_family_agreement")
})

test_that("boundary disagreement and auxiliary-only evidence are prioritized", {
  evidence <- rbind(
    patrol_row("native_stpd", "native_grammar", "n", 30, 40),
    patrol_row("mean_isi", "isi_threshold", "m", 38, 55),
    patrol_row("poisson_surprise", "surprise", "single", 80, 84)
  )
  out <- stpd_build_multi_method_patrol(evidence, agreement_iou = 0.5,
                                        boundary_tolerance_isi = 1L)
  disputed <- out$candidate_clusters[out$candidate_clusters$start_isi == 30L, ]
  lone <- out$candidate_clusters[out$candidate_clusters$start_isi == 80L, ]
  expect_identical(disputed$patrol_status, "boundary_disagreement")
  expect_identical(disputed$review_priority, "high")
  expect_identical(lone$source_class, "auxiliary_only")
  expect_identical(lone$review_priority, "high")
})

test_that("Broad HFS is nested context but Pause is a conflict for Burst", {
  burst <- patrol_row("native_stpd", "native_grammar", "b", 100, 106)
  hfs <- patrol_row("native_stpd", "native_grammar", "h", 90, 120,
                    label = "broad_hfs", track = "state")
  pause <- patrol_row("robust_gaussian_surprise", "surprise", "g", 104, 104,
                      label = "pause", track = "gap")
  nested <- stpd_build_multi_method_patrol(burst, context_intervals = hfs)
  expect_true(nested$candidate_clusters$nested_hfs_context)
  expect_false(nested$candidate_clusters$semantic_conflict)
  conflict <- stpd_build_multi_method_patrol(burst,
                                             context_intervals = rbind(hfs, pause))
  expect_true(conflict$candidate_clusters$nested_hfs_context)
  expect_true(conflict$candidate_clusters$semantic_conflict)
  expect_identical(conflict$candidate_clusters$review_priority, "critical")
})

test_that("patrol output is deterministic under input permutation", {
  evidence <- rbind(
    patrol_row("native_stpd", "native_grammar", "a", 1, 4),
    patrol_row("mean_isi", "isi_threshold", "b", 2, 4),
    patrol_row("poisson_surprise", "surprise", "c", 10, 12)
  )
  a <- stpd_build_multi_method_patrol(evidence)
  b <- stpd_build_multi_method_patrol(evidence[c(3, 1, 2), ])
  expect_identical(a$candidate_clusters, b$candidate_clusters)
  expect_identical(a$cluster_membership, b$cluster_membership)
})

test_that("support-result adapter accepts PS and RGS event tables", {
  ps <- list(method = "poisson_surprise", bursts = data.frame(
    train = "T1", burst_id = 1L, start_isi = 5L, end_isi = 7L,
    start_spike_index = 5L, end_spike_index = 8L,
    start_time_sec = 0.5, end_time_sec = 0.8, surprise = 12
  ))
  rgs <- list(method = "robust_gaussian_surprise", bursts = data.frame(
    train = "T1", event_id = "r1", start_isi = 5L, end_isi = 7L,
    start_spike_index = 5L, end_spike_index = 8L,
    start_time_sec = 0.5, end_time_sec = 0.8, adjusted_p = 0.001
  ), pauses = data.frame())
  evidence <- stpd_patrol_bind_evidence(ps, rgs)
  expect_setequal(evidence$provider_key,
                  c("poisson_surprise", "robust_gaussian_surprise"))
  out <- stpd_build_multi_method_patrol(evidence)
  expect_identical(out$candidate_clusters$patrol_status,
                   "within_family_agreement")
})

test_that("train-local event numbering remains globally identifiable", {
  ps <- list(method = "poisson_surprise", bursts = data.frame(
    train = c("T1", "T2"), burst_id = c(1L, 1L),
    start_isi = c(5L, 5L), end_isi = c(7L, 7L),
    start_spike = c(5L, 5L), end_spike = c(8L, 8L),
    start_time_sec = c(0.5, 0.5), end_time_sec = c(0.8, 0.8),
    surprise = c(12, 12)
  ))
  evidence <- stpd_patrol_evidence_from_support(ps)
  expect_equal(nrow(evidence), 2L)
  expect_equal(length(unique(evidence$evidence_id)), 2L)
  expect_setequal(evidence$train_key, c("T1", "T2"))
  expect_true(all(evidence$start_isi == 6L))
  expect_true(all(evidence$end_isi == 8L))
})

test_that("native event ledgers preserve Event Gap and State axes", {
  events <- data.frame(
    event_id = c("b", "p", "h"), train = "T1",
    pattern = c("possible_burst", "pause", "high_frequency_spiking"),
    start_isi = c(5L, 8L, 10L), end_isi = c(6L, 8L, 20L),
    start_spike_idx = c(5L, 8L, 10L), end_spike_idx = c(7L, 9L, 21L),
    start_time_sec = c(0.5, 0.8, 1), end_time_sec = c(0.7, 0.9, 2.1),
    auto_score = c(0.8, NA, 0.9)
  )
  evidence <- stpd_patrol_evidence_from_native_events(events)
  expect_identical(evidence$target_family[evidence$proposed_label == "possible_burst"],
                   "burst")
  expect_identical(evidence$semantic_track[evidence$proposed_label == "pause"],
                   "gap")
  expect_identical(evidence$semantic_track[evidence$proposed_label ==
                                             "high_frequency_spiking"], "state")
})

test_that("reviewer bundle validation is read-only and strict", {
  evidence <- patrol_row("native_stpd", "native_grammar", "n", 2, 4)
  report <- stpd_build_multi_method_patrol(evidence)
  bundle <- list(regions = list(GPE = list(
    spikes = list(T1 = seq(0, 1, length.out = 10)), patrol = report,
    truth_segments = data.frame(
      train_key = "T1", proposed_label = "burst", start_isi = 2L,
      end_isi = 4L, start_time_sec = 0.1, end_time_sec = 0.4
    )
  )))
  expect_identical(stpd_patrol_reviewer_validate_bundle(bundle), bundle)
  expect_error(stpd_patrol_reviewer_validate_bundle(list(regions = list())),
               "non-empty regions")
})

test_that("patrol annotations are append-only and retain current state", {
  root <- tempfile("patrol-annotations-")
  dir.create(root)
  history_path <- file.path(root, "history.csv")
  current_path <- file.path(root, "current.csv")
  make_record <- function(label, note, created) {
    data.frame(
      action_id = "", previous_action_id = "", transaction_id = "",
      bundle_sha256 = strrep("a", 64),
      region = "GPE", train_key = "T1", cluster_id = "c1",
      decision = "accept_as_label", assigned_label = label,
      semantic_track = if (label == "pause") "gap" else "event",
      start_isi = 2L, end_isi = 4L, start_spike = 1L, end_spike = 4L,
      start_time_sec = 0.1, end_time_sec = 0.4, reviewer = "Reviewer A",
      note = note, created_at_utc = created, stringsAsFactors = FALSE
    )
  }
  first <- stpd_patrol_annotation_append(
    history_path, current_path, make_record("burst", "first", "2026-01-01 UTC"))
  second <- stpd_patrol_annotation_append(
    history_path, current_path, make_record("pause", "corrected", "2026-01-02 UTC"))
  expect_equal(nrow(first), 1L)
  expect_equal(nrow(second), 2L)
  expect_identical(second$previous_action_id[[2L]], second$action_id[[1L]])
  expect_true(all(nchar(second$action_id) == 64L))
  current <- stpd_patrol_annotation_read(current_path, verify_chain = FALSE)
  expect_equal(nrow(current), 1L)
  expect_identical(current$assigned_label, "pause")
  expect_identical(current$note, "corrected")
})

test_that("review clicks and saved geometry use canonical train-row ISI indices", {
  expect_identical(stpd_patrol_diff_to_canonical_isi(c(1L, 3L)), c(2L, 4L))
  geometry <- stpd_patrol_annotation_geometry(c(0, 0.01, 0.03, 0.08), 2L, 4L)
  expect_identical(geometry$start_spike, 1L)
  expect_identical(geometry$end_spike, 4L)
  expect_equal(geometry$start_time_sec, 0)
  expect_equal(geometry$end_time_sec, 0.08)
  expect_error(
    stpd_patrol_annotation_geometry(c(0, 0.01), 1L, 2L),
    "canonical train-row"
  )
})

test_that("free annotations, erasure, batches, and transaction undo are auditable", {
  root <- tempfile("patrol-editor-"); dir.create(root)
  history_path <- file.path(root, "history.csv")
  current_path <- file.path(root, "current.csv")
  bundle_hash <- strrep("b", 64)
  spikes <- c(0, 0.01, 0.03, 0.06, 0.1, 0.15)
  a <- stpd_patrol_annotation_record(
    bundle_hash, "GPI", "T1", "manual_a", "accept_as_label", "burst",
    spikes, 2L, 3L, "Reviewer A", "free interval", "2026-01-01 UTC")
  b <- stpd_patrol_annotation_record(
    bundle_hash, "GPI", "T1", "manual_b", "accept_as_label", "pause",
    spikes, 5L, 6L, "Reviewer A", "free interval", "2026-01-01 UTC")
  history <- stpd_patrol_annotation_append_many(
    history_path, current_path, rbind(a, b), transaction_id = strrep("c", 64))
  expect_equal(nrow(stpd_patrol_annotation_active(history)), 2L)
  expect_true(all(history$transaction_id == strrep("c", 64)))

  undo <- stpd_patrol_annotation_undo_records(
    history, "Reviewer A", "2026-01-02 UTC")
  expect_equal(nrow(undo), 2L)
  history <- stpd_patrol_annotation_append_many(
    history_path, current_path, undo, transaction_id = strrep("d", 64))
  expect_equal(nrow(stpd_patrol_annotation_active(history)), 0L)
  expect_true(all(tail(history$decision, 2L) == "reject_as_other"))

  history <- stpd_patrol_annotation_append(
    history_path, current_path, a)
  erased <- stpd_patrol_annotation_record(
    bundle_hash, "GPI", "T1", "manual_a", "reject_as_other", "other",
    spikes, 2L, 3L, "Reviewer A", "erase", "2026-01-03 UTC")
  history <- stpd_patrol_annotation_append(history_path, current_path, erased)
  expect_equal(nrow(stpd_patrol_annotation_active(history)), 0L)
  restored <- stpd_patrol_annotation_undo_records(
    history, "Reviewer A", "2026-01-04 UTC")
  history <- stpd_patrol_annotation_append(history_path, current_path, restored)
  expect_identical(stpd_patrol_annotation_active(history)$assigned_label, "burst")
})

test_that("one-call orchestration executes existing support methods", {
  set.seed(8)
  isi <- c(rexp(80, 10), rexp(15, 90), rexp(80, 10),
           rexp(15, 75), rexp(80, 10))
  ds <- list(
    trains = list(T1 = data.frame(timestamp_sec = cumsum(c(0, isi)))),
    results = list()
  )
  out <- stpd_run_multi_method_patrol(
    ds, methods = c("mean_isi", "logisi_newbd", "poisson_surprise"),
    retain_support_results = FALSE
  )
  expect_s3_class(out, "stpd_patrol_report")
  expect_setequal(out$method_status$method,
                  c("mean_isi", "logisi_newbd", "poisson_surprise"))
  expect_true(all(out$method_status$status == "complete"))
  expect_gt(nrow(out$evidence), 0L)
  expect_null(out$support_results)
  expect_false(out$automatic_final_label)
})

test_that("orchestration records failed methods without inventing evidence", {
  ds <- list(trains = list(T1 = data.frame(timestamp_sec = c(0, 0, 1))),
             results = list())
  out <- stpd_run_multi_method_patrol(
    ds, methods = "poisson_surprise", strict = FALSE
  )
  expect_identical(out$method_status$status, "rejected")
  expect_equal(out$method_status$evidence_n, 0L)
  expect_equal(nrow(out$evidence), 0L)
  expect_error(
    stpd_run_multi_method_patrol(ds, methods = "poisson_surprise", strict = TRUE),
    "Duplicate spike timestamps"
  )
})

test_that("current STPD datasets can be bundled for the integrated reviewer", {
  set.seed(91)
  isi <- c(rexp(40, 10), rexp(7, 80), rexp(40, 10))
  ds <- list(
    trains = list(T1 = data.frame(timestamp_sec = cumsum(c(0, isi)))),
    results = list(), meta = list(display_name = "Integrated smoke")
  )
  bundle <- stpd_patrol_reviewer_bundle_from_dataset(
    ds, methods = "mean_isi"
  )
  expect_identical(bundle$schema_version,
                   "stpd_multi_method_patrol_reviewer_v1")
  expect_identical(names(bundle$regions), "INTEGRATED_SMOKE")
  expect_identical(names(bundle$regions[[1L]]$spikes), "T1")
  expect_s3_class(bundle$regions[[1L]]$patrol, "stpd_patrol_report")
  expect_identical(bundle$regions[[1L]]$patrol$method_status$method,
                   "mean_isi")
  expect_silent(stpd_patrol_reviewer_validate_bundle(bundle))
})

test_that("the reusable patrol panel exposes full manual editing controls", {
  html <- paste(as.character(stpd_patrol_reviewer_panel_ui()), collapse = "")
  expect_match(html, "patrol_new", fixed = TRUE)
  expect_match(html, "patrol_split", fixed = TRUE)
  expect_match(html, "patrol_delete", fixed = TRUE)
  expect_match(html, "patrol_merge", fixed = TRUE)
  expect_match(html, "patrol_undo", fixed = TRUE)
})
