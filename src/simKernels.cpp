// Vectorized and optionally OpenMP-parallelised kernels for the
// rLifeContingencies / rLifeContingenciesXyz simulation payoff computations.
//
// Rationale
// ---------
// The scalar kernels in lifecontingenciesRcpp.cpp (.fAxnCpp, .fIAxnCpp, …)
// are called from R via sapply(deathsTimeX, .fXCpp, …). For large n the
// per-element .Call() overhead dominates the actual arithmetic. These
// vectorized kernels take the whole deathsTimeX vector in a single call and
// run the loop in C++, which cuts the overhead to a single interop call and
// opens the door to loop-level parallelism via OpenMP.
//
// Because deaths are sampled upstream in R by rLife(), no RNG runs inside
// these kernels — the parallel region reads only a const double* and writes
// to disjoint output slots, so OpenMP is safe without any thread-local RNG
// machinery.
//
// OpenMP is used when available and off by default; callers opt in with the
// nthreads argument. When the compiler does not provide OpenMP (e.g. default
// clang on macOS), the code degrades to a plain sequential loop and nthreads
// is ignored.

#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <vector>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

namespace {

inline void check_positive_frequency(double k) {
  if (!std::isfinite(k) || k <= 0.0) {
    stop("k must be a finite positive number");
  }
}

inline int resolve_threads(int nthreads) {
#ifdef _OPENMP
  if (nthreads <= 0) {
    return 1;
  }
  const int hw = omp_get_max_threads();
  return std::min(nthreads, hw > 0 ? hw : 1);
#else
  (void)nthreads;
  return 1;
#endif
}

inline double axn_value(double T, double y, double n, double i, double m,
                        double k, bool advance) {
  const double K = T - y;
  if (std::isnan(K)) {
    return K;  // propagate NA/NaN lifetimes instead of treating them as survivors
  }
  if (K < m) {
    return 0.0;
  }
  const double step = 1.0 / k;
  const double upper = std::min(m + n - step, K);
  if (upper < m) {
    return 0.0;
  }
  const double offset = advance ? 0.0 : step;
  // closed-form geometric sum over times = {m, m+step, ..., upper}:
  //   sum_{j=0}^{J} step * v^(m + offset + j*step),  v = 1 / (1 + i)
  //   = step * v^(m+offset) * expm1(N*ln_vs) / expm1(ln_vs),  ln_vs = step * ln(v)
  // expm1 keeps full accuracy when the step is tiny (v^step ~ 1), where the
  // naive (1 - r^N) / (1 - r) loses all significant digits. The number of terms
  // N is kept as a double on purpose: converting it to an integer type is
  // undefined behaviour for huge / infinite values (n = Inf, tiny steps) and it
  // is only ever used as an exponent.
  const double n_terms = std::floor((upper - m) / step + 1e-9) + 1.0;
  if (!(n_terms >= 1.0)) {
    return 0.0;
  }
  const double ln_v = -std::log1p(i);
  const double ln_vs = step * ln_v;
  const double first = std::exp(ln_v * (m + offset));
  double total;
  if (ln_vs == 0.0) {            // i == 0: no discounting
    total = first * n_terms;
  } else {
    total = first * std::expm1(n_terms * ln_vs) / std::expm1(ln_vs);
  }
  return total * step;
}

// Remaining-lifetime of the status for row r of a column-major nrow x ncol
// matrix: min (joint-life) or max (last-survivor) of (death time - issue age).
// Indices are R_xlen_t: c * nrow overflows int for matrices with more than
// 2^31 elements. Requires ncol >= 1 (checked by the callers).
inline double status_lifetime(const double* Mp, R_xlen_t r, R_xlen_t nrow,
                              int ncol, const double* yv, bool joint) {
  double value = Mp[r] - yv[0];
  for (int c = 1; c < ncol; ++c) {
    const double v = Mp[r + static_cast<R_xlen_t>(c) * nrow] - yv[c];
    if (joint ? (v < value) : (v > value)) {
      value = v;
    }
  }
  return value;
}

inline void check_status_matrix(const NumericMatrix& M, const NumericVector& y) {
  if (M.ncol() < 1) {
    stop("deathsTimeXyz must have at least one column");
  }
  if (y.size() != M.ncol()) {
    stop("y must have length matching the number of columns of deathsTimeXyz");
  }
}

} // namespace

// ---------------------------------------------------------------------------
// Single-life vectorised payoffs
// ---------------------------------------------------------------------------

// [[Rcpp::export(name=".fExnCppVec")]]
NumericVector fExnCppVec(NumericVector T, double y, double n, double i,
                         int nthreads = 1) {
  const R_xlen_t sz = T.size();
  NumericVector out(sz);
  const double* Tp = REAL(T);
  double* op = REAL(out);
  const double disc = std::pow(1.0 + i, -n);
  const double threshold = y + n;

  const int nt = resolve_threads(nthreads);
  (void)nt;
#ifdef _OPENMP
#pragma omp parallel for num_threads(nt) if (nt > 1 && sz > 1024)
#endif
  for (R_xlen_t idx = 0; idx < sz; ++idx) {
    op[idx] = (Tp[idx] < threshold) ? 0.0 : disc;
  }
  return out;
}

// [[Rcpp::export(name=".fAxnCppVec")]]
NumericVector fAxnCppVec(NumericVector T, double y, double n, double i,
                         double m, double k = 1, int nthreads = 1) {
  check_positive_frequency(k);
  const R_xlen_t sz = T.size();
  NumericVector out(sz);
  const double* Tp = REAL(T);
  double* op = REAL(out);

  const double low = y + m;
  const double high = y + m + n - 1.0 / k;
  const double base = 1.0 + i;
  const double shift = -y + 1.0 / k;

  const int nt = resolve_threads(nthreads);
  (void)nt;
#ifdef _OPENMP
#pragma omp parallel for num_threads(nt) if (nt > 1 && sz > 1024)
#endif
  for (R_xlen_t idx = 0; idx < sz; ++idx) {
    const double Ti = Tp[idx];
    if (Ti >= low && Ti <= high) {
      op[idx] = std::pow(base, -(Ti + shift));
    } else {
      op[idx] = 0.0;
    }
  }
  return out;
}

// [[Rcpp::export(name=".fIAxnCppVec")]]
NumericVector fIAxnCppVec(NumericVector T, double y, double n, double i,
                          double m, double k = 1, int nthreads = 1) {
  check_positive_frequency(k);
  const R_xlen_t sz = T.size();
  NumericVector out(sz);
  const double* Tp = REAL(T);
  double* op = REAL(out);

  const double low = y + m;
  const double high = y + m + n - 1.0 / k;
  const double base = 1.0 + i;

  const int nt = resolve_threads(nthreads);
  (void)nt;
#ifdef _OPENMP
#pragma omp parallel for num_threads(nt) if (nt > 1 && sz > 1024)
#endif
  for (R_xlen_t idx = 0; idx < sz; ++idx) {
    const double Ti = Tp[idx];
    if (Ti >= low && Ti <= high) {
      const double increment = Ti - (y + m) + 1.0 / k;
      op[idx] = increment * std::pow(base, -(Ti - y + 1.0 / k));
    } else {
      op[idx] = 0.0;
    }
  }
  return out;
}

// [[Rcpp::export(name=".fDAxnCppVec")]]
NumericVector fDAxnCppVec(NumericVector T, double y, double n, double i,
                          double m, double k = 1, int nthreads = 1) {
  check_positive_frequency(k);
  const R_xlen_t sz = T.size();
  NumericVector out(sz);
  const double* Tp = REAL(T);
  double* op = REAL(out);

  const double low = y + m;
  const double high = y + m + n - 1.0 / k;
  const double base = 1.0 + i;

  const int nt = resolve_threads(nthreads);
  (void)nt;
#ifdef _OPENMP
#pragma omp parallel for num_threads(nt) if (nt > 1 && sz > 1024)
#endif
  for (R_xlen_t idx = 0; idx < sz; ++idx) {
    const double Ti = Tp[idx];
    if (Ti >= low && Ti <= high) {
      const double increment = n - (Ti - (y + m) + 1.0 / k);
      op[idx] = increment * std::pow(base, -(Ti - y + 1.0 / k));
    } else {
      op[idx] = 0.0;
    }
  }
  return out;
}

// [[Rcpp::export(name=".fAExnCppVec")]]
NumericVector fAExnCppVec(NumericVector T, double y, double n, double i,
                          double k = 1, int nthreads = 1) {
  check_positive_frequency(k);
  const R_xlen_t sz = T.size();
  NumericVector out(sz);
  const double* Tp = REAL(T);
  double* op = REAL(out);

  const double low = y;
  const double high = y + n - 1.0 / k;
  const double base = 1.0 + i;
  const double endow = std::pow(base, -n);

  const int nt = resolve_threads(nthreads);
  (void)nt;
#ifdef _OPENMP
#pragma omp parallel for num_threads(nt) if (nt > 1 && sz > 1024)
#endif
  for (R_xlen_t idx = 0; idx < sz; ++idx) {
    const double Ti = Tp[idx];
    if (Ti >= low && Ti <= high) {
      op[idx] = std::pow(base, -(Ti - y + 1.0 / k));
    } else {
      op[idx] = endow;
    }
  }
  return out;
}

// [[Rcpp::export(name=".faxnCppVec")]]
NumericVector faxnCppVec(NumericVector T, double y, double n, double i,
                         double m, double k = 1, bool advance = true,
                         int nthreads = 1) {
  check_positive_frequency(k);
  const R_xlen_t sz = T.size();
  NumericVector out(sz);
  const double* Tp = REAL(T);
  double* op = REAL(out);

  const int nt = resolve_threads(nthreads);
  (void)nt;
#ifdef _OPENMP
#pragma omp parallel for num_threads(nt) if (nt > 1 && sz > 1024)
#endif
  for (R_xlen_t idx = 0; idx < sz; ++idx) {
    op[idx] = axn_value(Tp[idx], y, n, i, m, k, advance);
  }
  return out;
}

// ---------------------------------------------------------------------------
// Multi-life vectorised payoffs. deathsTimeXyz is an n x numTables matrix;
// each row is one simulated life group.
// ---------------------------------------------------------------------------

// [[Rcpp::export(name=".fAxyznCppVec")]]
NumericVector fAxyznCppVec(NumericMatrix deathsTimeXyz, NumericVector y,
                           double n, double i, double m, double k = 1,
                           bool joint = true, int nthreads = 1) {
  check_positive_frequency(k);
  check_status_matrix(deathsTimeXyz, y);
  const R_xlen_t nrow = deathsTimeXyz.nrow();
  const int ncol = deathsTimeXyz.ncol();
  NumericVector out(nrow);

  const std::vector<double> yv(y.begin(), y.end());
  const double* yp = yv.data();
  const double* Mp = REAL(deathsTimeXyz);
  double* op = REAL(out);

  const double low = m;
  const double high = m + n - 1.0 / k;
  const double base = 1.0 + i;

  const int nt = resolve_threads(nthreads);
  (void)nt;
#ifdef _OPENMP
#pragma omp parallel for num_threads(nt) if (nt > 1 && nrow > 1024)
#endif
  for (R_xlen_t r = 0; r < nrow; ++r) {
    const double K = status_lifetime(Mp, r, nrow, ncol, yp, joint);
    if (K >= low && K <= high) {
      op[r] = std::pow(base, -(K + 1.0 / k));
    } else {
      op[r] = 0.0;
    }
  }
  return out;
}

// [[Rcpp::export(name=".faxyznCppVec")]]
NumericVector faxyznCppVec(NumericMatrix deathsTimeXyz, NumericVector y,
                           double n, double i, double m, double k = 1,
                           bool joint = true, bool advance = true,
                           int nthreads = 1) {
  check_positive_frequency(k);
  check_status_matrix(deathsTimeXyz, y);
  const R_xlen_t nrow = deathsTimeXyz.nrow();
  const int ncol = deathsTimeXyz.ncol();
  NumericVector out(nrow);

  const std::vector<double> yv(y.begin(), y.end());
  const double* yp = yv.data();
  const double* Mp = REAL(deathsTimeXyz);
  double* op = REAL(out);

  const int nt = resolve_threads(nthreads);
  (void)nt;
#ifdef _OPENMP
#pragma omp parallel for num_threads(nt) if (nt > 1 && nrow > 1024)
#endif
  for (R_xlen_t r = 0; r < nrow; ++r) {
    // axn_value takes (T, y): the status lifetime K is already T - y.
    op[r] = axn_value(status_lifetime(Mp, r, nrow, ncol, yp, joint),
                      0.0, n, i, m, k, advance);
  }
  return out;
}

// Reports whether the shared object was built with OpenMP support, used by
// R to decide whether a non-trivial nthreads value is honoured.
// [[Rcpp::export(name=".hasOpenMP")]]
bool hasOpenMP() {
#ifdef _OPENMP
  return true;
#else
  return false;
#endif
}
