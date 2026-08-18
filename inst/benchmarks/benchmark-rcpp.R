library(lifecontingencies)

# Development benchmark only: compare the historical R implementation that
# was replaced by presentValueC() with the current native implementation.
# This file is intentionally not run by testthat or R CMD check.
#
# bench::mark() is used instead of coarse wall-clock measurements so that
# small cases do not produce zero elapsed times or Inf/NaN speed-ups.
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

  timing <- bench::mark(
    R = reference_present_value(
      cashFlows, timeIds, interestRates, probabilities, power
    ),
    Rcpp = lifecontingencies:::.presentValueC(
      cashFlows, timeIds, rates_cpp, probabilities, power
    ),
    iterations = 30,
    check = FALSE,
    time_unit = "ms"
  )

  # bench::mark() stores the expression labels as the actual expressions.
  # Use the fixed row positions because the benchmark contains exactly two
  # expressions and avoids brittle string matching against deparsed calls.
  r_median <- as.numeric(timing$median[[1]])
  cpp_median <- as.numeric(timing$median[[2]])

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

cat("\nSummary (n >= 10000):\n")
summary_rows <- results[results$n >= 10000, ]
print(summary_rows[, c("n", "power", "scalar_rate", "r_median_ms",
                       "cpp_median_ms", "speedup")], row.names = FALSE)
