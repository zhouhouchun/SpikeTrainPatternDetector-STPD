
# canonical single-table candidate feature extraction.
# All downstream classification, ledger audit, cluster comparison and reporting
# should consume this table instead of recomputing features repeatedly.

compute_candidate_feature_table <- function(ds, candidates = NULL, params = default_params_sec(), selected_trains = NULL) {
  candidates <- candidates %||% (if (!is.null(ds) && !is.null(ds$results)) ds$results$candidate_ledger else data.frame())
  base <- compute_candidate_features_internal(ds, candidates, params = params, selected_trains = selected_trains)
  stpd_enhance_candidate_features_eventness(ds, base, params = params, selected_trains = selected_trains)
}

candidate_features_from_results <- function(ds, params = NULL) {
  compute_candidate_feature_table(ds, candidates = ds$results$candidate_ledger %||% data.frame(), params = params %||% (ds$params_last %||% default_params_sec()))
}
