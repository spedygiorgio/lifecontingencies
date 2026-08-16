library(testthat)
library(lifecontingencies)

context("Italian actuarial handout: actuarial examples")

# The handout's mortality basis is the ISTAT 1998 male table, published in 2002.
# It must not be replaced by demoIta$SIM02: the latter is a different table.
italy1998_male_act <- italy1998_male_act

# The numerical expectations below are recomputed from the verified ISTAT lx
# fixture with the actuarial expressions stated in the handout. This avoids
# mixing source-table values with demoIta's SIM02 series.

test_that("Exercise 4: deferred capital", {
  expect_equal(round(30000 * Exn(italy1998_male_act, x = 30, n = 20), 2), 13187.36)
})

test_that("Exercises 5-6: deferred capital", {
  expect_equal(round(16000 * Exn(italy1998_male_act, x = 35, n = 30), 2), 4197.73)
  expect_equal(round(26000 / Exn(italy1998_male_act, x = 40, n = 22), 2), 68773.66)
})

test_that("Exercises 9, 11 and 12: deferred capital", {
  expect_equal(round(15000 / Exn(italy1998_male_act, x = 32, n = 20), 2), 34313.28)
  expect_equal(round(15000 / Exn(italy1998_male_act, x = 50, n = 20), 2), 41976.69)
  expect_equal(round(20000 * Exn(italy1998_male_act, x = 27, n = 23) +
                   35000 * Exn(italy1998_male_act, x = 27, n = 33), 2), 16435.41)
})

test_that("Exercise 13: whole-life annuity present values", {
  expect_equal(round(20000 * axn(italy1998_male_act, x = 46), 2), 358810.82)
  expect_equal(round(20000 * axn(italy1998_male_act, x = 46, payment = "immediate"), 2),
               338810.82)
})

test_that("Exercise 14: deferred whole-life annuity", {
  expect_equal(round(12000 * axn(italy1998_male_act, x = 36, m = 24), 2), 57445.04)
})

test_that("Exercise 15: temporary annuity present values", {
  expect_equal(round(12000 * axn(italy1998_male_act, x = 36, n = 25,
                                 payment = "immediate"), 2), 182391.98)
  expect_equal(round(12000 * axn(italy1998_male_act, x = 36, n = 20, m = 12), 2),
               98477.82)
})

test_that("Exercise 16: temporary annuity present values", {
  expect_equal(round(22000 * axn(italy1998_male_act, x = 46, n = 25,
                                 payment = "immediate"), 2), 320504.33)
  expect_equal(round(22000 * axn(italy1998_male_act, x = 46, n = 20, m = 2), 2),
               271918.85)
})

test_that("Exercise 17: whole-life, temporary and deferred death insurance", {
  expect_equal(round(100000 * Axn(italy1998_male_act, x = 28), 2), 16691.88)
  expect_equal(round(100000 * Axn(italy1998_male_act, x = 28, n = 37), 2), 5767.75)
  expect_equal(round(100000 * Axn(italy1998_male_act, x = 28, n = 37,
                                  m = 37), 2), 10917.03)
})

test_that("Exercise 18: death benefit increasing by age band", {
  a1 <- Axn(italy1998_male_act, x = 38, n = 22)
  a2 <- Axn(italy1998_male_act, x = 38, n = 10, m = 22)
  a3 <- Axn(italy1998_male_act, x = 38, m = 32)
  apv <- a1 + 2 * a2 + 3 * a3
  capital <- 30000 / (sqrt(1.04) * apv)

  expect_equal(round(capital, 2), 52885.21)
  expect_equal(round(2 * capital, 2), 105770.42)
  expect_equal(round(3 * capital, 2), 158655.63)
})

test_that("Exercise 19: annual premium for a simple endowment", {
  death_apv <- Axn(italy1998_male_act, x = 28, n = 30) * sqrt(1.04)
  life_apv <- Exn(italy1998_male_act, x = 28, n = 30)
  pure_single_premium <- 45000 * (life_apv + death_apv)
  annual_premium <- pure_single_premium /
    axn(italy1998_male_act, x = 28, n = 30)

  expect_equal(round(annual_premium, 2), 821.74)
})

test_that("Exercise 20: combined pension and death cover", {
  pension_apv <- 12000 * axn(italy1998_male_act, x = 40, m = 25)
  death_apv <- 100000 * Axn(italy1998_male_act, x = 40, n = 25) * sqrt(1.04)
  pure_single_premium <- pension_apv + death_apv
  loaded_premium <- pure_single_premium * 1.30

  expect_equal(round(pure_single_premium, 2), 52475.31)
  expect_equal(round(loaded_premium, 2), 68217.90)
})
