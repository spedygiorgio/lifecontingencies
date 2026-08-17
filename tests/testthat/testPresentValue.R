library(lifecontingencies)

context("Present value engine")

# This is intentionally the R implementation that predates .presentValueC.
# It is kept here as an independent numerical reference for the native kernel.
reference_present_value <- function(cashFlows, timeIds, interestRates,
                                    probabilities, power = 1) {
  rates <- rep(interestRates, length.out = length(timeIds))
  discounts <- (1 + rates)^(-timeIds)
  sum((cashFlows^power) * (discounts^power) * probabilities)
}

test_that("presentValue handles non-unit probabilities and fractional times", {
  cf <- c(100, -30, 50, 70)
  times <- c(0, 0.5, 3, 6.25)
  probs <- c(1, 0.9, 0.8, 0.65)
  expected <- reference_present_value(cf, times, 0.035, probs)
  pv <- presentValue(cashFlows = cf,
                     timeIds = times,
                     interestRates = 0.035,
                     probabilities = probs)
  expect_equal(pv, expected, tolerance = 1e-12)
})

test_that("presentValue supports time-varying interest rates", {
  cf <- c(rep(10, 6), 110)
  times <- seq(1, 7)
  rates <- c(0.02, 0.021, 0.0225, 0.024, 0.0255, 0.027, 0.028)
  probs <- rep(0.995, length(cf))
  expected <- reference_present_value(cf, times, rates, probs)
  pv <- presentValue(cashFlows = cf,
                     timeIds = times,
                     interestRates = rates,
                     probabilities = probs)
  expect_equal(pv, expected, tolerance = 1e-12)
})

test_that("presentValue replicates scalar interest rate recycling", {
  set.seed(123)
  cf <- rnorm(50, mean = 5, sd = 20)
  times <- seq(0.25, by = 0.25, length.out = length(cf))
  probs <- runif(length(cf), min = 0.2, max = 1)
  rate <- 0.018
  expected <- reference_present_value(cf, times, rate, probs)
  pv <- presentValue(cashFlows = cf,
                     timeIds = times,
                     interestRates = rate,
                     probabilities = probs)
  expect_equal(pv, expected, tolerance = 1e-10)
})

test_that("presentValue honours the power argument", {
  cf <- c(3, 5, 7)
  times <- c(1, 2, 3)
  probs <- c(0.9, 0.8, 0.7)
  rate <- c(0.01, 0.015, 0.02)
  for (p in c(0.5, 1, 2)) {
    expected <- reference_present_value(cf, times, rate, probs, power = p)
    pv <- presentValue(cashFlows = cf,
                       timeIds = times,
                       interestRates = rate,
                       probabilities = probs,
                       power = p)
    expect_equal(pv, expected, tolerance = 1e-12)
  }
})

test_that("presentValue defaults probabilities to one", {
  cf <- c(2, 4, 6)
  times <- c(0, 1, 2)
  rate <- 0.03
  explicit <- presentValue(cashFlows = cf,
                           timeIds = times,
                           interestRates = rate,
                           probabilities = rep(1, length(cf)))
  implicit <- presentValue(cashFlows = cf,
                           timeIds = times,
                           interestRates = rate)
  expect_identical(implicit, explicit)
})

# Test the C++ kernel itself against the historical R implementation.  The
# rates are recycled exactly as presentValue() does before calling the kernel.
test_that("presentValueC agrees with the historical R implementation", {
  cases <- list(
    list(
      cf = c(100, -30, 50, 70),
      times = c(0, 0.5, 3, 6.25),
      rates = 0.035,
      probs = c(1, 0.9, 0.8, 0.65),
      power = 1
    ),
    list(
      cf = c(10, 20, 30, 110),
      times = c(1, 2, 4, 7),
      rates = c(0.02, 0.021, 0.024, 0.028),
      probs = c(0.995, 0.99, 0.97, 0.95),
      power = 1
    ),
    list(
      cf = c(3, 5, 7),
      times = c(1, 2, 3),
      rates = c(0.01, 0.015, 0.02),
      probs = c(0.9, 0.8, 0.7),
      power = 2
    )
  )

  for (case in cases) {
    rates <- rep(case$rates, length.out = length(case$times))
    expected <- reference_present_value(
      case$cf, case$times, case$rates, case$probs, case$power
    )
    actual <- lifecontingencies:::.presentValueC(
      cashFlows = case$cf,
      timeIds = case$times,
      interestRates = rates,
      probabilities = case$probs,
      power = case$power
    )
    expect_equal(actual, expected, tolerance = 1e-12)
  }
})

test_that("presentValueC agrees with the R reference over randomized cases", {
  set.seed(20260817)

  for (rep in seq_len(100)) {
    n <- sample(1:50, 1)
    cf <- rnorm(n, mean = 10, sd = 25)
    times <- sort(runif(n, min = 0, max = 30))
    rates <- sample(c(0.01, 0.015, 0.02, 0.025), sample(1:3, 1), replace = TRUE)
    probs <- runif(n, min = 0.1, max = 1)
    power <- sample(c(0.5, 1, 2), 1)

    expected <- reference_present_value(cf, times, rates, probs, power)
    actual <- lifecontingencies:::.presentValueC(
      cashFlows = cf,
      timeIds = times,
      interestRates = rep(rates, length.out = n),
      probabilities = probs,
      power = power
    )

    expect_equal(actual, expected, tolerance = 1e-12)
  }
})
