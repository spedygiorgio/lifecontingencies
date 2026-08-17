#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

namespace {

inline void check_same_length(const NumericVector& x,
                              const NumericVector& y,
                              const char* message) {
  if (x.size() != y.size()) {
    stop(message);
  }
}

inline void check_same_length(const NumericVector& x,
                              const NumericVector& y,
                              const NumericVector& z,
                              const char* message) {
  if (x.size() != y.size() || x.size() != z.size()) {
    stop(message);
  }
}

inline void check_positive_frequency(double k) {
  if (!std::isfinite(k) || k <= 0.0) {
    stop("k must be a finite positive number");
  }
}

} // namespace

// [[Rcpp::export(name=".mult3sum")]]
double mult3sum(NumericVector x, NumericVector y, NumericVector z)
{
  check_same_length(x, y, z, "x, y and z must have the same length");

  double total = 0.0;
  R_xlen_t n = x.size();

  for (R_xlen_t i = 0; i < n; ++i) {
    total += x[i] * y[i] * z[i];
  }
  return total;
}

// [[Rcpp::export(name=".mult2sum")]]
double mult2sum(NumericVector x, NumericVector y)
{
  check_same_length(x, y, "x and y must have the same length");

  double total = 0.0;
  R_xlen_t n = x.size();

  for (R_xlen_t i = 0; i < n; ++i) {
    total += x[i] * y[i];
  }
  return total;
}

// [[Rcpp::export(name=".presentValueC")]]
double presentValueC(NumericVector cashFlows,
                     NumericVector timeIds,
                     NumericVector interestRates,
                     NumericVector probabilities,
                     double power = 1.0) {
  if (cashFlows.size() != timeIds.size() ||
      cashFlows.size() != interestRates.size() ||
      cashFlows.size() != probabilities.size()) {
    stop("cashFlows, timeIds, interestRates and probabilities must have the same length");
  }

  double total = 0.0;
  R_xlen_t n = cashFlows.size();

  for (R_xlen_t i = 0; i < n; ++i) {
    if (power == 1.0) {
      // Common case: avoid the second pow() entirely.
      double discountFactor =
        std::pow(1.0 + interestRates[i], -timeIds[i]);
      total += cashFlows[i] * discountFactor * probabilities[i];
    } else {
      // Algebraically combine the two discounting powers into one pow().
      // This reduces the expensive power evaluations from two to one while
      // preserving the same mathematical expression.
      double discountFactor =
        std::pow(1.0 + interestRates[i], -timeIds[i] * power);
      double term = std::pow(cashFlows[i], power) *
        discountFactor * probabilities[i];
      total += term;
    }
  }

  return total;
}

// [[Rcpp::export(name=".fExnCpp")]]
double fExnCpp(double T, double y, double n, double i)
{
  double out;
  if(T < y + n)
    out = 0;
  else
    out = std::pow(1 + i, -n);
  return out;
}

// [[Rcpp::export(name=".fAxnCpp")]]
double fAxnCpp(double T, double y, double n, double i, double m, double k=1)
{
  check_positive_frequency(k);

  double out = 0;
  if ((T >= y + m) && (T <= y + m + n - 1 / k))
    out = std::pow(1 + i, -(T - y + 1 / k));
  else
    out = 0;
  return out;
}

// [[Rcpp::export(name=".fIAxnCpp")]]
double fIAxnCpp(double T, double y, double n, double i, double m, double k=1) {
  check_positive_frequency(k);

  double out;
  if ((T >= y + m) && (T <= y + m + n - 1 / k))
    out = (T - (y + m) + 1 / k) *
      std::pow(1 + i, -(T - y + 1 / k));
  else
    out = 0;
  return out;
}

// [[Rcpp::export(name=".fDAxnCpp")]]
double fDAxnCpp(double T, double y, double n, double i, double m, double k=1) {
  check_positive_frequency(k);

  double out;
  if ((T >= y + m) && (T <= y + m + n - 1 / k))
    out = (n - (T - (y + m) + 1 / k)) *
      std::pow(1 + i, -(T - y + 1 / k));
  else
    out = 0;
  return out;
}

// [[Rcpp::export(name=".fAExnCpp")]]
double fAExnCpp(double T, double y, double n, double i, double k=1)
{
  check_positive_frequency(k);

  double out;
  if ((T >= y) && (T <= y + n - 1 / k))
    out = std::pow(1 + i, -(T - y + 1 / k));
  else
    out = std::pow(1 + i, -n);
  return out;
}
