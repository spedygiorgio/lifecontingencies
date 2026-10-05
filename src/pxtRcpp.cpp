#include <Rcpp.h>
using namespace Rcpp;

// Compute survival probabilities from a life table using the same
// fractional-age assumptions implemented by pxt() in R.
// [[Rcpp::export(.pxtCpp)]]
NumericVector pxtCpp(NumericVector x, NumericVector t, NumericVector lx,
                     double omega, int fractional_method) {
  // R_xlen_t, not int: long vectors (> 2^31 - 1 elements) would be truncated.
  const R_xlen_t nx = x.size();
  const R_xlen_t nt = t.size();
  const R_xlen_t n = std::max(nx, nt);

  if (nx == 0 || nt == 0) {
    return NumericVector(0);
  }

  NumericVector out(n);

  // Recycle x and t directly instead of materialising rep(x, n) and
  // rep(t, n). This avoids two temporary allocations for large vectors.
  for (R_xlen_t i = 0; i < n; ++i) {
    const double xi = x[i % nx];
    const double ti = t[i % nt];

    if (xi < 0 || ti < 0) {
      stop("Check x or t domain");
    }

    const double floor_x = std::floor(xi);
    const double eps_x = xi - floor_x;
    const double u = ti + eps_x;
    const double floor_u = std::floor(u);
    const double eps_u = u - floor_u;

    // Ages are kept as doubles and bounds-checked in get_lx(): converting huge,
    // infinite or NaN values to int is undefined behaviour, and indexing lx
    // beyond its length is an out-of-bounds read when omega >= length(lx).
    const double ix = floor_x;
    const double ix1 = ix + 1.0;
    const double ixu = ix + floor_u;
    const double ixu1 = ixu + 1.0;

    const double lx_len = static_cast<double>(lx.size());
    auto get_lx = [&](double age) {
      // NaN fails the first comparison and is treated as out of range.
      if (!(age >= 0.0) || age > omega || age >= lx_len) {
        return 0.0;
      }
      return lx[static_cast<R_xlen_t>(age)];
    };

    const double lfx = get_lx(ix);
    if (lfx == 0.0) {
      out[i] = 0.0;
      continue;
    }

    const double lfx1 = get_lx(ix1);
    const double lfxu = get_lx(ixu);
    const double lfxu1 = get_lx(ixu1);

    const double floor_u_p_floor_x = lfxu / lfx;
    const double one_p_floor_xu =
      (lfxu == 0.0) ? 0.0 : lfxu1 / lfxu;
    const double one_p_floor_x = lfx1 / lfx;

    double u_p_floor_x;
    if (fractional_method == 0) { // linear
      u_p_floor_x = floor_u_p_floor_x *
        (1.0 - eps_u * (1.0 - one_p_floor_xu));
    } else if (fractional_method == 1) { // constant force
      u_p_floor_x = floor_u_p_floor_x *
        std::pow(one_p_floor_xu, eps_u);
    } else { // hyperbolic
      u_p_floor_x = floor_u_p_floor_x * one_p_floor_xu /
        (1.0 - (1.0 - eps_u) * (1.0 - one_p_floor_xu));
    }

    double eps_x_p_floor_x;
    if (fractional_method == 0) {
      eps_x_p_floor_x = 1.0 - eps_x * (1.0 - one_p_floor_x);
    } else if (fractional_method == 1) {
      eps_x_p_floor_x = std::pow(one_p_floor_x, eps_x);
    } else {
      eps_x_p_floor_x = one_p_floor_x /
        (1.0 - (1.0 - eps_x) * (1.0 - one_p_floor_x));
    }

    out[i] = u_p_floor_x / eps_x_p_floor_x;
  }

  return out;
}

// Exact native port of the life-table branch of pxt() in R.
//
// `lx` holds the survivors for the consecutive ages minAge, ..., omega followed
// by a terminal 0 for age omega + 1 (that is, c(object@lx, 0)). Ages outside
// this range are "missing", as in the name-based lookup of the R code, and
// every one-year ratio that turns out NA/NaN is replaced by 0 before the
// fractional-age adjustment. The arithmetic below follows the R expressions
// term by term (including R_pow for `^`), so results are bit-for-bit identical
// to the former R implementation, degenerate NaN cases included.
// [[Rcpp::export(.pxtLifetableCpp)]]
NumericVector pxtLifetableCpp(NumericVector x, NumericVector t, NumericVector lx,
                              double minAge, int fractional_method) {
  const R_xlen_t nx = x.size();
  const R_xlen_t nt = t.size();
  if (nx == 0 || nt == 0) {
    return NumericVector(0);
  }
  const R_xlen_t n = std::max(nx, nt);
  const double maxAge = minAge + static_cast<double>(lx.size()) - 1.0;

  auto get_lx = [&](double age) {
    // NaN ages fail both comparisons and are treated as missing.
    if (!(age >= minAge && age <= maxAge)) {
      return NA_REAL;
    }
    return lx[static_cast<R_xlen_t>(age - minAge)];
  };
  auto na_to_zero = [](double v) { return ISNAN(v) ? 0.0 : v; };

  NumericVector out(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    const double xi = x[i % nx];
    const double ti = t[i % nt];

    const double floorx = std::floor(xi);
    const double eps_x = xi - floorx;
    const double u = ti + eps_x;
    const double flooru = std::floor(u);
    const double eps_u = u - flooru;

    const double l_floorx = get_lx(floorx);
    const double l_floorxp1 = get_lx(floorx + 1);
    const double l_floorxu = get_lx(floorx + flooru);
    const double l_floorxup1 = get_lx(floorx + flooru + 1);

    const double flooru_p_floorx = na_to_zero(l_floorxu / l_floorx);
    const double one_p_floorxu = na_to_zero(l_floorxup1 / l_floorxu);
    const double one_p_floorx = na_to_zero(l_floorxp1 / l_floorx);

    double u_p_floorx, eps_x_p_floorx;
    if (fractional_method == 0) { // linear
      u_p_floorx = flooru_p_floorx * (1 - eps_u * (1 - one_p_floorxu));
      eps_x_p_floorx = 1 - eps_x * (1 - one_p_floorx);
    } else if (fractional_method == 1) { // constant force
      u_p_floorx = flooru_p_floorx * R_pow(one_p_floorxu, eps_u);
      eps_x_p_floorx = R_pow(one_p_floorx, eps_x);
    } else { // hyperbolic
      u_p_floorx = flooru_p_floorx * one_p_floorxu /
        (1 - (1 - eps_u) * (1 - one_p_floorxu));
      eps_x_p_floorx = one_p_floorx /
        (1 - (1 - eps_x) * (1 - one_p_floorx));
    }
    out[i] = u_p_floorx / eps_x_p_floorx;
  }
  return out;
}
