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

benchmark_sum_case <- function(n) {
  x <- rnorm(n)
  y <- rnorm(n)
  z <- rnorm(n)

  expected2 <- sum(x * y)
  expected3 <- sum(x * y * z)
  actual2 <- lifecontingencies:::.mult2sum(x, y)
  actual3 <- lifecontingencies:::.mult3sum(x, y, z)
  stopifnot(all.equal(actual2, expected2, tolerance = 1e-10))
  stopifnot(all.equal(actual3, expected3, tolerance = 1e-10))

  timing <- bench::mark(
    R_mult2 = sum(x * y),
    Rcpp_mult2 = lifecontingencies:::.mult2sum(x, y),
    R_mult3 = sum(x * y * z),
    Rcpp_mult3 = lifecontingencies:::.mult3sum(x, y, z),
    iterations = 30,
    check = FALSE,
    time_unit = "ms"
  )

  data.frame(
    n = n,
    r_mult2_median_ms = as.numeric(timing$median[[1]]),
    cpp_mult2_median_ms = as.numeric(timing$median[[2]]),
    mult2_speedup = as.numeric(timing$median[[1]]) /
      as.numeric(timing$median[[2]]),
    r_mult3_median_ms = as.numeric(timing$median[[3]]),
    cpp_mult3_median_ms = as.numeric(timing$median[[4]]),
    mult3_speedup = as.numeric(timing$median[[3]]) /
      as.numeric(timing$median[[4]])
  )
}

present_value_results <- do.call(rbind, c(
  lapply(c(100, 1000, 10000, 100000), benchmark_case, power = 1),
  lapply(c(100, 1000, 10000, 100000), benchmark_case, power = 2),
  lapply(c(100, 1000, 10000, 100000), benchmark_case, power = 3),
  lapply(c(1000, 10000, 100000), benchmark_case,
         power = 1, scalar_rate = TRUE)
))

sum_results <- do.call(rbind, lapply(
  c(1000, 10000, 100000), benchmark_sum_case
))

cat("\nPresent-value benchmark:\n")
print(present_value_results, row.names = FALSE)

cat("\nPresent-value summary (n >= 10000):\n")
summary_rows <- present_value_results[present_value_results$n >= 10000, ]
print(summary_rows[, c("n", "power", "scalar_rate", "r_median_ms",
                       "cpp_median_ms", "speedup")], row.names = FALSE)

cat("\nVector multiplication/sum benchmark:\n")
print(sum_results, row.names = FALSE)