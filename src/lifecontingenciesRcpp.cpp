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
  const double* xp = REAL(x);
  const double* yp = REAL(y);
  const double* zp = REAL(z);

  for (R_xlen_t i = 0; i < n; ++i) {
    total += xp[i] * yp[i] * zp[i];
  }
  return total;
}

// [[Rcpp::export(name=".mult2sum")]]
double mult2sum(NumericVector x, NumericVector y)
{
  check_same_length(x, y, "x and y must have the same length");

  double total = 0.0;
  R_xlen_t n = x.size();
  const double* xp = REAL(x);
  const double* yp = REAL(y);

  for (R_xlen_t i = 0; i < n; ++i) {
    total += xp[i] * yp[i];
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
  const double* cashFlowsPtr = REAL(cashFlows);
  const double* timeIdsPtr = REAL(timeIds);
  const double* interestRatesPtr = REAL(interestRates);
  const double* probabilitiesPtr = REAL(probabilities);

  if (power == 1.0) {
    // Common case: no per-element branch and one pow() per observation.
    for (R_xlen_t i = 0; i < n; ++i) {
      double discountFactor =
        std::pow(1.0 + interestRatesPtr[i], -timeIdsPtr[i]);
      total += cashFlowsPtr[i] * discountFactor * probabilitiesPtr[i];
    }
  } else if (power == 2.0) {
    // Avoid pow() for the common integer square case.
    for (R_xlen_t i = 0; i < n; ++i) {
      double discountFactor =
        std::pow(1.0 + interestRatesPtr[i], -timeIdsPtr[i] * power);
      double cashFlow = cashFlowsPtr[i];
      total += cashFlow * cashFlow * discountFactor * probabilitiesPtr[i];
    }
  } else if (power == 3.0) {
    // Avoid pow() for the common integer cube case.
    for (R_xlen_t i = 0; i < n; ++i) {
      double discountFactor =
        std::pow(1.0 + interestRatesPtr[i], -timeIdsPtr[i] * power);
      double cashFlow = cashFlowsPtr[i];
      total += cashFlow * cashFlow * cashFlow *
        discountFactor * probabilitiesPtr[i];
    }
  } else {
    // General power: preserve the mathematical formulation with one
    // discounting pow() per observation.
    for (R_xlen_t i = 0; i < n; ++i) {
      double discountFactor =
        std::pow(1.0 + interestRatesPtr[i], -timeIdsPtr[i] * power);
      double term = std::pow(cashFlowsPtr[i], power) *
        discountFactor * probabilitiesPtr[i];
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
