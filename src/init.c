#ifdef __clang__
# pragma clang diagnostic ignored "-Wunknown-warning-option"
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP stpd_isi_percentiles_c(SEXP isi_sexp, SEXP min_sexp);
SEXP stpd_local_median_cache_c(SEXP isi_sexp, SEXP window_sexp, SEXP min_sexp);
SEXP stpd_structure_scan_c(SEXP isi_sexp, SEXP pct_sexp, SEXP min_w_sexp, SEXP max_w_sexp, SEXP qmax_sexp, SEXP pctmax_sexp, SEXP edge_min_sexp, SEXP edge_geom_sexp, SEXP min_isi_sexp);
SEXP stpd_interval_best_overlap_c(SEXP qs_sexp, SEXP qe_sexp, SEXP ts_sexp, SEXP te_sexp);
SEXP stpd_short_runs_c(SEXP isi_sexp, SEXP pct_sexp, SEXP max_abs_sexp, SEXP max_pct_sexp, SEXP min_run_sexp, SEXP min_isi_sexp, SEXP gate_both_sexp);

static const R_CallMethodDef CallEntries[] = {
    {"stpd_isi_percentiles_c", (DL_FUNC) &stpd_isi_percentiles_c, 2},
    {"stpd_local_median_cache_c", (DL_FUNC) &stpd_local_median_cache_c, 3},
    {"stpd_structure_scan_c", (DL_FUNC) &stpd_structure_scan_c, 9},
    {"stpd_interval_best_overlap_c", (DL_FUNC) &stpd_interval_best_overlap_c, 4},
    {"stpd_short_runs_c", (DL_FUNC) &stpd_short_runs_c, 7},
    {NULL, NULL, 0}
};

void R_init_SpikeTrainPatternDetector(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
