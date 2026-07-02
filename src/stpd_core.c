#ifdef __clang__
# pragma clang diagnostic ignored "-Wunknown-warning-option"
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>

static int cmp_double(const void *a, const void *b) {
    double da = *(const double*)a;
    double db = *(const double*)b;
    if (da < db) return -1;
    if (da > db) return 1;
    return 0;
}

static int upper_bound_double(const double *arr, int n, double x) {
    int lo = 0, hi = n;
    while (lo < hi) {
        int mid = lo + (hi - lo) / 2;
        if (arr[mid] <= x) lo = mid + 1;
        else hi = mid;
    }
    return lo;
}

static int upper_threshold_ok(double value, double upper) {
    if (!R_FINITE(value)) return 0;
    if (R_FINITE(upper)) return value <= upper;
    return isinf(upper) && upper > 0.0;
}

SEXP stpd_isi_percentiles_c(SEXP isi_sexp, SEXP min_sexp) {
    int nprotect = 0;
    if (!isReal(isi_sexp)) { isi_sexp = PROTECT(coerceVector(isi_sexp, REALSXP)); nprotect++; }
    R_xlen_t n0 = XLENGTH(isi_sexp);
    if (n0 > INT_MAX) error("stpd_isi_percentiles_c: input is too long");
    double min_isi = asReal(min_sexp);
    const double *x = REAL(isi_sexp);
    int nvalid = 0;
    for (R_xlen_t i = 0; i < n0; ++i) {
        if (R_FINITE(x[i]) && x[i] >= min_isi) nvalid++;
    }
    SEXP out = PROTECT(allocVector(REALSXP, n0)); nprotect++;
    for (R_xlen_t i = 0; i < n0; ++i) REAL(out)[i] = NA_REAL;
    if (nvalid <= 0) { UNPROTECT(nprotect); return out; }
    double *vals = (double*) R_alloc(nvalid, sizeof(double));
    int k = 0;
    for (R_xlen_t i = 0; i < n0; ++i) {
        if (R_FINITE(x[i]) && x[i] >= min_isi) vals[k++] = x[i];
    }
    qsort(vals, nvalid, sizeof(double), cmp_double);
    for (R_xlen_t i = 0; i < n0; ++i) {
        if (R_FINITE(x[i]) && x[i] >= min_isi) {
            int ub = upper_bound_double(vals, nvalid, x[i]);
            REAL(out)[i] = 100.0 * ((double) ub) / ((double) nvalid);
        }
    }
    UNPROTECT(nprotect);
    return out;
}

static double median_of_sorted(double *vals, int n) {
    if (n <= 0) return NA_REAL;
    qsort(vals, n, sizeof(double), cmp_double);
    if (n % 2 == 1) return vals[n/2];
    return 0.5 * (vals[n/2 - 1] + vals[n/2]);
}

SEXP stpd_local_median_cache_c(SEXP isi_sexp, SEXP window_sexp, SEXP min_sexp) {
    int nprotect = 0;
    if (!isReal(isi_sexp)) { isi_sexp = PROTECT(coerceVector(isi_sexp, REALSXP)); nprotect++; }
    R_xlen_t n0 = XLENGTH(isi_sexp);
    if (n0 > INT_MAX) error("stpd_local_median_cache_c: input is too long");
    int n = (int)n0;
    int window = asInteger(window_sexp);
    if (window < 3) window = 3;
    if (window > 10001) window = 10001;
    int max_window = n > 3 ? n : 3;
    if (max_window % 2 == 0 && max_window > 3) max_window--;
    if (window > max_window) window = max_window;
    if (window % 2 == 0) window++;
    if (window > 10001) window = 10001;
    if (window > max_window) window = max_window;
    int half = window / 2;
    double min_isi = asReal(min_sexp);
    const double *isi = REAL(isi_sexp);
    SEXP out = PROTECT(allocVector(REALSXP, n0)); nprotect++;
    for (R_xlen_t ii = 0; ii < n0; ++ii) REAL(out)[ii] = NA_REAL;
    if (n0 <= 0) { UNPROTECT(nprotect); return out; }
    double *buf = (double*) R_alloc(window + 2, sizeof(double));
    for (R_xlen_t ii = 0; ii < n0; ++ii) {
        int lo = (int)ii - half;
        int hi = (int)ii + half;
        if (lo < 1) lo = 1; // R code excludes first ISI index by starting at 2 (1-based); C index 1.
        if (hi >= n) hi = n - 1;
        int k = 0;
        for (int jj = lo; jj <= hi; ++jj) {
            if (jj == (int)ii) continue;
            double v = isi[jj];
            if (R_FINITE(v) && v >= min_isi) buf[k++] = v;
        }
        if (k > 0) REAL(out)[ii] = median_of_sorted(buf, k);
    }
    UNPROTECT(nprotect);
    return out;
}

static double q90_small(const double *x, int start, int end, double min_isi, double *buf) {
    int k = 0;
    for (int i = start; i <= end; ++i) {
        double v = x[i];
        if (R_FINITE(v) && v >= min_isi) buf[k++] = v;
    }
    if (k <= 0) return NA_REAL;
    qsort(buf, k, sizeof(double), cmp_double);
    if (k == 1) return buf[0];
    double h = 1.0 + ((double)k - 1.0) * 0.90; // R quantile(type = 7), 1-based position.
    int j = (int)floor(h);
    double gamma = h - (double)j;
    if (j <= 0) return buf[0];
    if (j >= k) return buf[k - 1];
    return buf[j - 1] + gamma * (buf[j] - buf[j - 1]);
}

SEXP stpd_structure_scan_c(SEXP isi_sexp, SEXP pct_sexp, SEXP min_w_sexp, SEXP max_w_sexp,
                           SEXP qmax_sexp, SEXP pctmax_sexp, SEXP edge_min_sexp,
                           SEXP edge_geom_sexp, SEXP min_isi_sexp) {
    int nprotect = 0;
    if (!isReal(isi_sexp)) { isi_sexp = PROTECT(coerceVector(isi_sexp, REALSXP)); nprotect++; }
    if (!isReal(pct_sexp)) { pct_sexp = PROTECT(coerceVector(pct_sexp, REALSXP)); nprotect++; }
    if (XLENGTH(isi_sexp) > INT_MAX) error("stpd_structure_scan_c: input is too long");
    int n = (int) XLENGTH(isi_sexp);
    if (XLENGTH(pct_sexp) != XLENGTH(isi_sexp)) {
        error("stpd_structure_scan_c: pct_sexp must have the same length as isi_sexp");
    }
    const double *isi = REAL(isi_sexp);
    const double *pct = REAL(pct_sexp);
    int min_w = asInteger(min_w_sexp);
    int max_w = asInteger(max_w_sexp);
    if (min_w < 1) min_w = 1;
    if (max_w < min_w) max_w = min_w;
    if (n > 2 && max_w > n - 2) max_w = n - 2;
    if (max_w > 10000) error("stpd_structure_scan_c: max_core_isi_n is too large");
    double qmax = asReal(qmax_sexp);
    double pctmax = asReal(pctmax_sexp);
    double edge_min = asReal(edge_min_sexp);
    double edge_geom = asReal(edge_geom_sexp);
    double min_isi = asReal(min_isi_sexp);
    int width_count = max_w >= min_w ? max_w - min_w + 1 : 0;
    long long cap_ll = (long long)n * (long long)width_count;
    if (cap_ll > INT_MAX) error("stpd_structure_scan_c: scan allocation would be too large");
    int cap = (int)cap_ll;
    if (cap < 1) cap = 1;
    int *starts = (int*) R_alloc(cap, sizeof(int));
    int *ends = (int*) R_alloc(cap, sizeof(int));
    double *qv = (double*) R_alloc(cap, sizeof(double));
    double *mp = (double*) R_alloc(cap, sizeof(double));
    double *emin = (double*) R_alloc(cap, sizeof(double));
    double *egeom = (double*) R_alloc(cap, sizeof(double));
    int buf_width = max_w >= min_w ? max_w : min_w;
    if (buf_width < 1) buf_width = 1;
    double *buf = (double*) R_alloc(buf_width + 2, sizeof(double));
    int k = 0;
    for (int w = min_w; w <= max_w; ++w) {
        for (int s = 1; s + w < n; ++s) { // zero-based; 1-based start_isi = s + 1. Need both flanks.
            int e = s + w - 1;
            double pre = isi[s - 1];
            double post = isi[e + 1];
            if (!R_FINITE(pre) || !R_FINITE(post)) continue;
            double q = q90_small(isi, s, e, min_isi, buf);
            if (!R_FINITE(q) || q <= 0.0) continue;
            double maxpct = NA_REAL;
            for (int ii = s; ii <= e; ++ii) {
                double pv = pct[ii];
                if (R_FINITE(pv)) {
                    if (!R_FINITE(maxpct) || pv > maxpct) maxpct = pv;
                }
            }
            double rpre = pre / q;
            double rpost = post / q;
            double emn = rpre < rpost ? rpre : rpost;
            double eg = sqrt(rpre * rpost);
            int qok = upper_threshold_ok(q, qmax);
            int pok = upper_threshold_ok(maxpct, pctmax);
            if ((qok || pok) && emn >= edge_min && eg >= edge_geom) {
                if (k >= cap) break;
                starts[k] = s + 1; // R 1-based ISI index
                ends[k] = e + 1;
                qv[k] = q;
                mp[k] = maxpct;
                emin[k] = emn;
                egeom[k] = eg;
                k++;
            }
        }
    }
    SEXP startv = PROTECT(allocVector(INTSXP, k)); nprotect++;
    SEXP endv = PROTECT(allocVector(INTSXP, k)); nprotect++;
    SEXP qvR = PROTECT(allocVector(REALSXP, k)); nprotect++;
    SEXP mpR = PROTECT(allocVector(REALSXP, k)); nprotect++;
    SEXP emR = PROTECT(allocVector(REALSXP, k)); nprotect++;
    SEXP egR = PROTECT(allocVector(REALSXP, k)); nprotect++;
    for (int i=0; i<k; ++i) {
        INTEGER(startv)[i] = starts[i];
        INTEGER(endv)[i] = ends[i];
        REAL(qvR)[i] = qv[i];
        REAL(mpR)[i] = mp[i];
        REAL(emR)[i] = emin[i];
        REAL(egR)[i] = egeom[i];
    }
    SEXP out = PROTECT(allocVector(VECSXP, 6)); nprotect++;
    SET_VECTOR_ELT(out, 0, startv); SET_VECTOR_ELT(out, 1, endv); SET_VECTOR_ELT(out, 2, qvR);
    SET_VECTOR_ELT(out, 3, mpR); SET_VECTOR_ELT(out, 4, emR); SET_VECTOR_ELT(out, 5, egR);
    SEXP names = PROTECT(allocVector(STRSXP, 6)); nprotect++;
    SET_STRING_ELT(names, 0, mkChar("start_isi")); SET_STRING_ELT(names, 1, mkChar("end_isi"));
    SET_STRING_ELT(names, 2, mkChar("core_q90_ISI_sec")); SET_STRING_ELT(names, 3, mkChar("core_max_pct"));
    SET_STRING_ELT(names, 4, mkChar("edge_contrast_min")); SET_STRING_ELT(names, 5, mkChar("edge_contrast_geom"));
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(nprotect);
    return out;
}

SEXP stpd_interval_best_overlap_c(SEXP qs_sexp, SEXP qe_sexp, SEXP ts_sexp, SEXP te_sexp) {
    int nprotect = 0;
    if (!isInteger(qs_sexp)) { qs_sexp = PROTECT(coerceVector(qs_sexp, INTSXP)); nprotect++; }
    if (!isInteger(qe_sexp)) { qe_sexp = PROTECT(coerceVector(qe_sexp, INTSXP)); nprotect++; }
    if (!isInteger(ts_sexp)) { ts_sexp = PROTECT(coerceVector(ts_sexp, INTSXP)); nprotect++; }
    if (!isInteger(te_sexp)) { te_sexp = PROTECT(coerceVector(te_sexp, INTSXP)); nprotect++; }
    if (XLENGTH(qe_sexp) != XLENGTH(qs_sexp)) {
        error("stpd_interval_best_overlap_c: qe_sexp must have the same length as qs_sexp");
    }
    if (XLENGTH(te_sexp) != XLENGTH(ts_sexp)) {
        error("stpd_interval_best_overlap_c: te_sexp must have the same length as ts_sexp");
    }
    if (XLENGTH(qs_sexp) > INT_MAX || XLENGTH(ts_sexp) > INT_MAX) {
        error("stpd_interval_best_overlap_c: input is too long");
    }
    int nq = (int) XLENGTH(qs_sexp);
    int nt = (int) XLENGTH(ts_sexp);
    SEXP bi = PROTECT(allocVector(INTSXP, nq)); nprotect++;
    SEXP ov = PROTECT(allocVector(INTSXP, nq)); nprotect++;
    SEXP iou = PROTECT(allocVector(REALSXP, nq)); nprotect++;
    for (int i = 0; i < nq; ++i) {
        int qs = INTEGER(qs_sexp)[i];
        int qe = INTEGER(qe_sexp)[i];
        int best = NA_INTEGER;
        int bestov = 0;
        double bestiou = NA_REAL;
        for (int j = 0; j < nt; ++j) {
            int ts = INTEGER(ts_sexp)[j];
            int te = INTEGER(te_sexp)[j];
            int lo = qs > ts ? qs : ts;
            int hi = qe < te ? qe : te;
            int o = hi >= lo ? hi - lo + 1 : 0;
            if (o <= 0) continue;
            int ulo = qs < ts ? qs : ts;
            int uhi = qe > te ? qe : te;
            double u = (double)(uhi - ulo + 1);
            double io = u > 0 ? ((double)o) / u : 0.0;
            if (best == NA_INTEGER || io > bestiou) { best = j + 1; bestov = o; bestiou = io; }
        }
        INTEGER(bi)[i] = best;
        INTEGER(ov)[i] = bestov;
        REAL(iou)[i] = bestiou;
    }
    SEXP out = PROTECT(allocVector(VECSXP, 3)); nprotect++;
    SET_VECTOR_ELT(out, 0, bi); SET_VECTOR_ELT(out, 1, ov); SET_VECTOR_ELT(out, 2, iou);
    SEXP names = PROTECT(allocVector(STRSXP, 3)); nprotect++;
    SET_STRING_ELT(names, 0, mkChar("best_index")); SET_STRING_ELT(names, 1, mkChar("overlap")); SET_STRING_ELT(names, 2, mkChar("iou"));
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(nprotect);
    return out;
}

SEXP stpd_short_runs_c(SEXP isi_sexp, SEXP pct_sexp, SEXP max_abs_sexp, SEXP max_pct_sexp,
                       SEXP min_run_sexp, SEXP min_isi_sexp, SEXP gate_both_sexp) {
    int nprotect = 0;
    if (!isReal(isi_sexp)) { isi_sexp = PROTECT(coerceVector(isi_sexp, REALSXP)); nprotect++; }
    if (!isReal(pct_sexp)) { pct_sexp = PROTECT(coerceVector(pct_sexp, REALSXP)); nprotect++; }
    if (XLENGTH(isi_sexp) > INT_MAX) error("stpd_short_runs_c: input is too long");
    int n = (int) XLENGTH(isi_sexp);
    if (XLENGTH(pct_sexp) != XLENGTH(isi_sexp)) {
        error("stpd_short_runs_c: pct_sexp must have the same length as isi_sexp");
    }
    const double *isi = REAL(isi_sexp);
    const double *pct = REAL(pct_sexp);
    double max_abs = asReal(max_abs_sexp);
    double max_pct = asReal(max_pct_sexp);
    int min_run = asInteger(min_run_sexp);
    if (min_run < 1) min_run = 1;
    double min_isi = asReal(min_isi_sexp);
    int gate_both = asInteger(gate_both_sexp);
    int cap = n > 1 ? n : 1;
    int *starts = (int*) R_alloc(cap, sizeof(int));
    int *ends = (int*) R_alloc(cap, sizeof(int));
    int *nisi = (int*) R_alloc(cap, sizeof(int));
    double *meanisi = (double*) R_alloc(cap, sizeof(double));
    double *maxisi = (double*) R_alloc(cap, sizeof(double));
    double *meanpct = (double*) R_alloc(cap, sizeof(double));
    double *maxpctout = (double*) R_alloc(cap, sizeof(double));
    int k = 0;
    int i = 0;
    while (i < n) {
        double v = isi[i];
        double p = pct[i];
        int ok_abs = R_FINITE(v) && v >= min_isi && upper_threshold_ok(v, max_abs);
        int ok_pct = upper_threshold_ok(p, max_pct);
        int ok = gate_both ? (ok_abs && ok_pct) : (ok_abs || ok_pct);
        if (!ok) { i++; continue; }
        int s = i;
        double sumv = 0.0, sump = 0.0, mxv = R_NegInf, mxp = R_NegInf;
        int nv = 0, np = 0;
        while (i < n) {
            v = isi[i]; p = pct[i];
            ok_abs = R_FINITE(v) && v >= min_isi && upper_threshold_ok(v, max_abs);
            ok_pct = upper_threshold_ok(p, max_pct);
            ok = gate_both ? (ok_abs && ok_pct) : (ok_abs || ok_pct);
            if (!ok) break;
            if (R_FINITE(v)) { sumv += v; if (v > mxv) mxv = v; nv++; }
            if (R_FINITE(p)) { sump += p; if (p > mxp) mxp = p; np++; }
            i++;
        }
        int e = i - 1;
        int len = e - s + 1;
        if (len >= min_run && k < cap) {
            starts[k] = s + 1;
            ends[k] = e + 1;
            nisi[k] = len;
            meanisi[k] = nv > 0 ? sumv / (double)nv : NA_REAL;
            maxisi[k] = nv > 0 ? mxv : NA_REAL;
            meanpct[k] = np > 0 ? sump / (double)np : NA_REAL;
            maxpctout[k] = np > 0 ? mxp : NA_REAL;
            k++;
        }
    }
    SEXP startv = PROTECT(allocVector(INTSXP, k)); nprotect++;
    SEXP endv = PROTECT(allocVector(INTSXP, k)); nprotect++;
    SEXP nrun = PROTECT(allocVector(INTSXP, k)); nprotect++;
    SEXP meanv = PROTECT(allocVector(REALSXP, k)); nprotect++;
    SEXP maxv = PROTECT(allocVector(REALSXP, k)); nprotect++;
    SEXP meanp = PROTECT(allocVector(REALSXP, k)); nprotect++;
    SEXP maxp = PROTECT(allocVector(REALSXP, k)); nprotect++;
    for (int j = 0; j < k; ++j) {
        INTEGER(startv)[j] = starts[j]; INTEGER(endv)[j] = ends[j]; INTEGER(nrun)[j] = nisi[j];
        REAL(meanv)[j] = meanisi[j]; REAL(maxv)[j] = maxisi[j]; REAL(meanp)[j] = meanpct[j]; REAL(maxp)[j] = maxpctout[j];
    }
    SEXP out = PROTECT(allocVector(VECSXP, 7)); nprotect++;
    SET_VECTOR_ELT(out, 0, startv); SET_VECTOR_ELT(out, 1, endv); SET_VECTOR_ELT(out, 2, nrun);
    SET_VECTOR_ELT(out, 3, meanv); SET_VECTOR_ELT(out, 4, maxv); SET_VECTOR_ELT(out, 5, meanp); SET_VECTOR_ELT(out, 6, maxp);
    SEXP names = PROTECT(allocVector(STRSXP, 7)); nprotect++;
    SET_STRING_ELT(names, 0, mkChar("start_isi")); SET_STRING_ELT(names, 1, mkChar("end_isi"));
    SET_STRING_ELT(names, 2, mkChar("n_isi")); SET_STRING_ELT(names, 3, mkChar("mean_ISI_sec"));
    SET_STRING_ELT(names, 4, mkChar("max_ISI_sec")); SET_STRING_ELT(names, 5, mkChar("mean_ISI_pct"));
    SET_STRING_ELT(names, 6, mkChar("max_ISI_pct"));
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(nprotect);
    return out;
}
