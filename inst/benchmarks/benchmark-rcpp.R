library(lifecontingencies)

# Development benchmark only: compare the historical R implementation that
# was replaced by presentValueC() with the current native implementation.
# This file is intentionally not run by testthat or R CMD check.
#
# The benchmark uses bench::mark() rather than elapsed wall-clock times from
# system.time(). This avoids zero elapsed times, Inf/NaN speed-ups, and gives
# a more stable comparison on GitHub Actions runners.
if (!requireNamespace("bench", quietly = TRUE)) {
  stop("Package 'bench' is required to run this development benchmark.")
}

reference_present_value <- function(cashFlows, timeIds, interestRates,
                                    probabilities, power = 1) {
  v <- (1 + interestRates)^(-timeIds)
  sum((cashFlows^power) * (v^power) * probabilities)
}

set.seed(2026)

benchmark_case <- function(n, power = 1, scalar_rate = FALSE) {
  cashFlows <- rnorm(n, mean = 100, sd = 25)
  timeIds <- seq_len(n)
  interestRates <- if (scalar_rate) 0.03 else runif(n, 0.01, 0.06)
  rates_cpp <- rep(interestRates, length.out = n)
  probabilities <- runif(n, 0.5, 1)

  # Check numerical equivalence before timing.
  expected <- reference_present_value(
    cashFlows, timeIds, interestRates, probabilities, power
  )
  actual <- lifecontingencies:::.presentValueC(
    cashFlows, timeIds, rates_cpp, probabilities, power
  )
  stopifnot(all.equal(actual, expected, tolerance = 1e-10))

  r_expr <- quote(
    reference_present_value(
      cashFlows, timeIds, interestRates, probabilities, power
    )
  )
  cpp_expr <- quote(
    lifecontingencies:::.presentValueC(
      cashFlows, timeIds, rates_cpp, probabilities, power
    )
  )

  timing <- bench::mark(
    R = eval(r_expr),
    Rcpp = eval(cpp_expr),
    iterations = 30,
    check = FALSE,
    time_unit = "ms"
  )

  r_median <- as.numeric(timing$median[timing$expression == "eval(r_expr)"])
  cpp_median <- as.numeric(timing$median[timing$expression == "eval(cpp_expr)"])

  data.frame(
    n = n,
    power = power,
    scalar_rate = scalar_rate,
    iterations = 30,
    r_median_ms = r_median,
    cpp_median_ms = cpp_median,
    speedup = r_median / cpp_median
  )
}

results <- do.call(rbind, c(
  lapply(c(100, 1000, 10000, 100000), benchmark_case, power = 1),
  lapply(c(100, 1000, 10000, 100000), benchmark_case, power = 2),
  lapply(c(1000, 10000, 100000), benchmark_case,
         power = 1, scalar_rate = TRUE)
))

print(results, row.names = FALSE)
