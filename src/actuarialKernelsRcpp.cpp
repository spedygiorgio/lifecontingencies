#include <Rcpp.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

namespace {

inline double lx_at(const NumericVector& lx, int age, int omega) {
  if (age < 0 || age > omega) return 0.0;
  return lx[age];
}

inline double survival_one(double x, double t, const NumericVector& lx,
                           int omega, int fractional_method) {
  if (x < 0.0 || t < 0.0) stop("Check x or t domain");

  const double floor_x = std::floor(x);
  const double eps_x = x - floor_x;
  const double u = t + eps_x;
  const double floor_u = std::floor(u);
  const double eps_u = u - floor_u;

  const int ix = static_cast<int>(floor_x);
  const int ix1 = ix + 1;
  const int ixu = ix + static_cast<int>(floor_u);
  const int ixu1 = ixu + 1;

  const double lfx = lx_at(lx, ix, omega);
  if (lfx == 0.0) return 0.0;

  const double lfx1 = lx_at(lx, ix1, omega);
  const double lfxu = lx_at(lx, ixu, omega);
  const double lfxu1 = lx_at(lx, ixu1, omega);

  const double floor_u_p_floor_x = lfxu / lfx;
  const double one_p_floor_xu = (lfxu == 0.0) ? 0.0 : lfxu1 / lfxu;
  const double one_p_floor_x = lfx1 / lfx;

  double u_p_floor_x;
  double eps_x_p_floor_x;

  if (fractional_method == 0) { // linear
    u_p_floor_x = floor_u_p_floor_x *
      (1.0 - eps_u * (1.0 - one_p_floor_xu));
    eps_x_p_floor_x = 1.0 - eps_x * (1.0 - one_p_floor_x);
  } else if (fractional_method == 1) { // constant force
    u_p_floor_x = floor_u_p_floor_x * std::pow(one_p_floor_xu, eps_u);
    eps_x_p_floor_x = std::pow(one_p_floor_x, eps_x);
  } else { // hyperbolic
    u_p_floor_x = floor_u_p_floor_x * one_p_floor_xu /
      (1.0 - (1.0 - eps_u) * (1.0 - one_p_floor_xu));
    eps_x_p_floor_x = one_p_floor_x /
      (1.0 - (1.0 - eps_x) * (1.0 - one_p_floor_x));
  }

  if (eps_x_p_floor_x == 0.0) return 0.0;
  return u_p_floor_x / eps_x_p_floor_x;
}

inline int n_periods(double n, double k) {
  return static_cast<int>(std::llround(n * k));
}

inline void check_common(const NumericVector& x, const NumericVector& n,
                         const NumericVector& m, double k) {
  if (!std::isfinite(k) || k < 1.0) stop("k must be a finite number >= 1");
  if (x.size() == 0 || n.size() == 0 || m.size() == 0) stop("Empty actuarial input");
}

} // namespace

// [[Rcpp::export(name=".axnCpp")]]
NumericVector axnCpp(NumericVector x, NumericVector n, NumericVector m,
                     double i, double k, int payment, int fractional_method,
                     double power, NumericVector lx, double omega) {
  check_common(x, n, m, k);
  const R_xlen_t out_n = std::max({x.size(), n.size(), m.size()});
  NumericVector out(out_n);
  const int omega_i = static_cast<int>(std::floor(omega));

  for (R_xlen_t j = 0; j < out_n; ++j) {
    const double age = x[j % x.size()];
    const double term = n[j % n.size()];
    const double defer = m[j % m.size()];
    if (age < 0.0 || term < 0.0 || defer < 0.0)
      stop("Check x, n or m");
    if (term <= 0.0) {
      out[j] = 0.0;
      continue;
    }

    const int periods = n_periods(term, k);
    if (periods <= 0) {
      out[j] = 0.0;
      continue;
    }

    double total = 0.0;
    for (int p = 0; p < periods; ++p) {
      const double base_time = defer +
        (payment == 0 ? (p + 1.0) / k : static_cast<double>(p) / k);
      const double survival = survival_one(age, base_time, lx, omega_i,
                                           fractional_method);
      const double discounted = (1.0 / k) *
        std::pow(1.0 + i, -base_time);
      total += std::pow(discounted, power) * survival;
    }
    out[j] = total;
  }
  return out;
}

// [[Rcpp::export(name=".AxnCpp")]]
NumericVector AxnCpp(NumericVector x, NumericVector n, NumericVector m,
                     double i, double k, int fractional_method, double power,
                     NumericVector lx, double omega) {
  check_common(x, n, m, k);
  const R_xlen_t out_n = std::max({x.size(), n.size(), m.size()});
  NumericVector out(out_n);
  const int omega_i = static_cast<int>(std::floor(omega));

  for (R_xlen_t j = 0; j < out_n; ++j) {
    const double age = x[j % x.size()];
    const double term = n[j % n.size()];
    const double defer = m[j % m.size()];
    if (age < 0.0 || term < 0.0 || defer < 0.0)
      stop("Check x, n or m");
    if (term <= 0.0) {
      out[j] = 0.0;
      continue;
    }

    const int periods = n_periods(term, k);
    if (periods <= 0) {
      out[j] = 0.0;
      continue;
    }

    double total = 0.0;
    for (int p = 0; p < periods; ++p) {
      const double t = defer + static_cast<double>(p) / k;
      const double p_survive = survival_one(age, t, lx, omega_i,
                                             fractional_method);
      const double p_survive_next = survival_one(age, t + 1.0 / k, lx,
                                                 omega_i, fractional_method);
      const double death_prob = std::max(0.0, p_survive - p_survive_next);
      const double claim_time = t + 1.0 / k;
      const double discount = std::pow(1.0 + i, -claim_time);
      total += std::pow(discount, power) * death_prob;
    }
    out[j] = total;
  }
  return out;
}

// [[Rcpp::export(name=".AExnCpp")]]
NumericVector AExnCpp(NumericVector x, NumericVector n, double i, double k,
                      int fractional_method, double power,
                      NumericVector lx, double omega) {
  if (x.size() == 0 || n.size() == 0)
    stop("Empty actuarial input");
  if (!std::isfinite(k) || k < 1.0)
    stop("k must be a finite number >= 1");

  const R_xlen_t out_n = std::max(x.size(), n.size());
  NumericVector out(out_n);
  const int omega_i = static_cast<int>(std::floor(omega));

  for (R_xlen_t j = 0; j < out_n; ++j) {
    const double age = x[j % x.size()];
    const double term = n[j % n.size()];
    if (age < 0.0 || term < 0.0) stop("Check x or n");
    if (term == 0.0) {
      out[j] = 1.0;
      continue;
    }

    const int periods = n_periods(term, k);
    double insurance = 0.0;
    for (int p = 0; p < periods; ++p) {
      const double t = static_cast<double>(p) / k;
      const double p0 = survival_one(age, t, lx, omega_i,
                                     fractional_method);
      const double p1 = survival_one(age, t + 1.0 / k, lx, omega_i,
                                     fractional_method);
      const double death_prob = std::max(0.0, p0 - p1);
      const double claim_time = t + 1.0 / k;
      insurance += std::pow(std::pow(1.0 + i, -claim_time), power) * death_prob;
    }

    const double survival = survival_one(age, term, lx, omega_i,
                                         fractional_method);
    const double pure_endowment = std::pow(std::pow(1.0 + i, -term), power) *
      survival;
    out[j] = insurance + pure_endowment;
  }
  return out;
}
