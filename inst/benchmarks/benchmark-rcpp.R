library(lifecontingencies)

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
  lapply(c(100, 1000, 10000, 100000), benchmark_case, power = 3),
  lapply(c(1000, 10000, 100000), benchmark_case,
         power = 1, scalar_rate = TRUE)
))

print(results, row.names = FALSE)

cat("\nSummary (n >= 10000):\n")
summary_rows <- results[results$n >= 10000, ]
print(summary_rows[, c("n", "power", "scalar_rate", "r_median_ms",
                       "cpp_median_ms", "speedup")], row.names = FALSE)
