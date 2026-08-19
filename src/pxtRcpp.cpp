#include <Rcpp.h>
using namespace Rcpp;

// Compute survival probabilities from a life table using the same
// fractional-age assumptions implemented by pxt() in R.
// [[Rcpp::export(.pxtCpp)]]
NumericVector pxtCpp(NumericVector x, NumericVector t, NumericVector lx,
                     double omega, int fractional_method) {
  const int nx = x.size();
  const int nt = t.size();
  const int n = std::max(nx, nt);

  if (nx == 0 || nt == 0) {
    return NumericVector(0);
  }

  NumericVector out(n);

  // Recycle x and t directly instead of materialising rep(x, n) and
  // rep(t, n). This avoids two temporary allocations for large vectors.
  for (int i = 0; i < n; ++i) {
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

    // Ages are integer indices into lx; keep the boundary behaviour of the
    // original kernel while avoiding a per-iteration lambda and floor calls.
    const int ix = static_cast<int>(floor_x);
    const int ix1 = ix + 1;
    const int ixu = ix + static_cast<int>(floor_u);
    const int ixu1 = ixu + 1;

    auto get_lx = [&](int age) {
      if (age < 0 || age > omega || age == omega + 1) {
        return 0.0;
      }
      return lx[age];
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
