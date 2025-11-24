library(testthat)

context("presentValue")

test_that("presentValue computes simple discounted sum correctly", {
  cf <- c(10, 10, 10)
  t  <- c(1, 2, 3)
  i  <- 0.03

  expected <- sum(cf * (1 + i) ^ (-t))
  result <- presentValue(cashFlows = cf, timeIds = t, interestRates = i, probabilities = c(1,1,1))

  expect_equal(result, expected)
})

test_that("presentValue works with vector interestRates and missing probabilities", {
  cf <- c(5, 15, 20)
  t  <- c(1, 2, 3)
  i_vec <- rep(0.04, length(t))

  expected <- sum(cf * (1 + i_vec) ^ (-t))
  # omit probabilities (should assume ones)
  result <- presentValue(cashFlows = cf, timeIds = t, interestRates = i_vec)

  expect_equal(result, expected)
})

test_that("presentValue errors on mismatched lengths", {
  cf <- c(10,20)
  t  <- c(1,2,3)
  i  <- 0.03

  expect_error(presentValue(cashFlows = cf, timeIds = t, interestRates = i))
  expect_error(presentValue(cashFlows = c(1,2,3), timeIds = c(1,2,3), interestRates = i, probabilities = c(1,1)))
})
