
# Canonical public API. Compatibility behavior is kept behind semantic
# implementation names; the supported user-facing API does not expose suffixes.

default_params <- function() apply_schema_defaults(default_params_sec())
validate_dataset_quality <- validate_dataset_quality_impl
run_detector_train <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  run_detector_one_train(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}
run_detector_dataset <- function(ds, params = default_params(), selected_trains = NULL, lock_manual = TRUE, collect_diagnostics = TRUE,
                                 progress_callback = NULL) {
  stpd_detect(ds, apply_schema_defaults(params), selected_trains = selected_trains, lock_manual = lock_manual,
              collect_diagnostics = collect_diagnostics, progress_callback = progress_callback)
}
export_detection_results <- export_detection_results_simple
build_candidate_ledger <- build_candidate_ledger_internal
build_event_audit <- build_event_ledger_internal

stpd_golden_test_dataset <- function(case = c("middle_burst", "boundary_start", "refractory_doublet", "stable_high_frequency")) {
  case <- match.arg(case)
  if (case == "middle_burst") t <- c(0, .10, .20, .205, .210, .215, .40, .55)
  else if (case == "boundary_start") t <- c(0, .005, .010, .015, .20, .35, .50)
  else if (case == "refractory_doublet") t <- c(0, .10, .1012, .1025, .30, .50)
  else t <- cumsum(c(0, rep(.010, 12), .20, .25, .30))
  dat <- data.frame(idx = seq_along(t), timestamp_sec = t, ISI_sec = c(NA_real_, diff(t)), pattern_manual = rep("", length(t)), pattern_auto = rep("", length(t)), stringsAsFactors = FALSE)
  make_dataset(name = case, source = "synthetic", trains = list(train_1 = dat), unit_in = "s")
}
