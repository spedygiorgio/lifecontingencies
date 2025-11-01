library(lifecontingencies)

context("Present value engine")

reference_present_value <- function(cashFlows, timeIds, interestRates, probabilities, power = 1) {
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
