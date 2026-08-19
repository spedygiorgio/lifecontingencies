#include <Rcpp.h>
using namespace Rcpp;

// Compute survival probabilities from a life table using the same
// fractional-age assumptions implemented by pxt() in R.
// [[Rcpp::export(.pxtCpp)]]
NumericVector pxtCpp(NumericVector x, NumericVector t, NumericVector lx,
                     double omega, int fractional_method) {
  int n = std::max(x.size(), t.size());
  NumericVector out(n);
  NumericVector xx = rep(x, n);
  NumericVector tt = rep(t, n);

  for (int i = 0; i < n; ++i) {
    double xi = xx[i];
    double ti = tt[i];
    if (xi < 0 || ti < 0) stop("Check x or t domain");

    double floor_x = std::floor(xi);
    double eps_x = xi - floor_x;
    double u = ti + eps_x;
    double floor_u = std::floor(u);
    double eps_u = u - floor_u;

    auto L = [&](double age) {
      if (age < 0 || age > omega + 1) return 0.0;
      if (age == omega + 1) return 0.0;
      double a = std::floor(age);
      if (a < 0 || a > omega) return 0.0;
      return lx[(int)a];
    };

    double lfx = L(floor_x);
    if (lfx == 0.0) {
      out[i] = 0.0;
      continue;
    }

    double lfx1 = L(floor_x + 1);
    double lfxu = L(floor_x + floor_u);
    double lfxu1 = L(floor_x + floor_u + 1);

    double floor_u_p_floor_x = lfxu / lfx;
    double one_p_floor_xu = (lfxu == 0.0) ? 0.0 : lfxu1 / lfxu;
    double one_p_floor_x = lfx1 / lfx;

    double u_p_floor_x;
    if (fractional_method == 0) { // linear
      u_p_floor_x = floor_u_p_floor_x * (1.0 - eps_u * (1.0 - one_p_floor_xu));
    } else if (fractional_method == 1) { // constant force
      u_p_floor_x = floor_u_p_floor_x * std::pow(one_p_floor_xu, eps_u);
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
