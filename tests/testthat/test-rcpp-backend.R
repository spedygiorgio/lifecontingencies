library(testthat)

reference_present_value <- function(cashFlows, timeIds, interestRates,
                                    probabilities, power = 1) {
  v <- (1 + interestRates)^(-timeIds)
  sum((cashFlows^power) * (v^power) * probabilities)
}

test_that("presentValueC matches the historical R implementation", {
  cases <- list(
    list(c(100, -30, 50, 70), c(0, 0.5, 3, 6.25), 0.035,
         c(1, 0.9, 0.8, 0.65), 1),
    list(c(10, 10, 10, 10, 10, 10, 110), 1:7,
         c(0.02, 0.021, 0.0225, 0.024, 0.0255, 0.027, 0.028),
         rep(0.995, 7), 1),
    list(c(3, 5, 7), c(1, 2, 3), c(0.01, 0.015, 0.02),
         c(0.9, 0.8, 0.7), 2)
  )

  for (case in cases) {
    names(case) <- c("cashFlows", "timeIds", "interestRates",
                     "probabilities", "power")
    expected <- do.call(reference_present_value, case)
    rates <- rep(case$interestRates, length.out = length(case$timeIds))
    case$interestRates <- rates
    actual <- do.call(lifecontingencies:::.presentValueC, case)
    expect_equal(actual, expected, tolerance = 1e-12)
  }
})

test_that("presentValueC remains numerically stable on randomized inputs", {
  set.seed(123)
  for (power in c(1, 2, 3)) {
    cf <- rnorm(50, mean = 5, sd = 20)
    times <- seq(0.25, by = 0.25, length.out = length(cf))
    rates <- runif(length(cf), 0.005, 0.06)
    probs <- runif(length(cf), 0.2, 1)

    expected <- reference_present_value(cf, times, rates, probs, power)
    actual <- lifecontingencies:::.presentValueC(
      cf, times, rates, probs, power
    )
    expect_equal(actual, expected, tolerance = 1e-10)
  }
})

test_that("native vector kernels reject incompatible lengths", {
  expect_error(lifecontingencies:::.mult2sum(c(1, 2), c(1)))
  expect_error(lifecontingencies:::.mult3sum(c(1, 2), c(1), c(1, 2)))
  expect_error(
    lifecontingencies:::.presentValueC(c(1, 2), c(1, 2), c(0.03), c(1, 1))
  )
})

test_that("native actuarial kernels reject invalid payment frequencies", {
  for (bad_k in c(0, -1, NA_real_, NaN, Inf)) {
    expect_error(
      lifecontingencies:::.fAxnCpp(40, 30, 10, 0.04, 2, bad_k)
    )
  }
})
