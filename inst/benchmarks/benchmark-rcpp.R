library(lifecontingencies)

# Development benchmark only: compare the historical R implementation that
# was replaced by presentValueC() with the current native implementation.
# This file is intentionally not run by testthat or R CMD check.

reference_present_value <- function(cashFlows, timeIds, interestRates,
                                    probabilities, power = 1) {
  v <- (1 + interestRates)^(-timeIds)
  sum((cashFlows^power) * (v^power) * probabilities)
}

set.seed(2026)

benchmark_case <- function(n, power = 1, scalar_rate = FALSE, reps = 20) {
  cashFlows <- rnorm(n, mean = 100, sd = 25)
  timeIds <- seq_len(n)
  interestRates <- if (scalar_rate) 0.03 else runif(n, 0.01, 0.06)
  probabilities <- runif(n, 0.5, 1)
  rates_cpp <- rep(interestRates, length.out = n)

  # Check numerical equivalence before timing.
  expected <- reference_present_value(
    cashFlows, timeIds, interestRates, probabilities, power
  )
  actual <- lifecontingencies:::.presentValueC(
    cashFlows, timeIds, rates_cpp, probabilities, power
  )
  stopifnot(all.equal(actual, expected, tolerance = 1e-10))

  r_time <- system.time(replicate(
    reps,
    reference_present_value(
      cashFlows, timeIds, interestRates, probabilities, power
    )
  ))

  cpp_time <- system.time(replicate(
    reps,
    lifecontingencies:::.presentValueC(
      cashFlows, timeIds, rates_cpp, probabilities, power
    )
  ))

  data.frame(
    n = n,
    power = power,
    scalar_rate = scalar_rate,
    reps = reps,
    r_elapsed = unname(r_time[["elapsed"]]),
    cpp_elapsed = unname(cpp_time[["elapsed"]]),
    speedup = unname(r_time[["elapsed"]]) /
      unname(cpp_time[["elapsed"]])
  )
}

results <- do.call(rbind, c(
  lapply(c(100, 1000, 10000, 100000), benchmark_case, power = 1),
  lapply(c(100, 1000, 10000, 100000), benchmark_case, power = 2),
  lapply(c(1000, 10000, 100000), benchmark_case,
         power = 1, scalar_rate = TRUE)
))

print(results, row.names = FALSE)
