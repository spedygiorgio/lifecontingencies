library(testthat)
library(lifecontingencies)

data(demoIta)
sim02 <- demoIta$SIM02
sim02 <- sim02[!is.na(sim02) & sim02 != 0]
sim02act <- new("actuarialtable", x = seq(0, length(sim02) - 1),
                lx = sim02, interest = 0.04,
                name = "Italy males 2002 (4%)")

context("Cere-Spelta / Italian actuarial handout: actuarial examples")

# Values below are the rounded numerical results reported in the handout.
test_that("Deferred capital exercises 4-6, 9, 11 and 12", {
  expect_equal(round(30000 * Exn(sim02act, x = 30, n = 20), 2), 13184.89)
  expect_equal(round(16000 * Exn(sim02act, x = 35, n = 30), 2), 6460.01)
  expect_equal(round(26000 / Exn(sim02act, x = 40, n = 22), 2), 68772.60)
  expect_equal(round(15000 / Exn(sim02act, x = 32, n = 20), 2), 34314.00)
  expect_equal(round(15000 / Exn(sim02act, x = 50, n = 20), 2), 41976.00)
  expect_equal(round(20000 * Exn(sim02act, x = 27, n = 23) +
                   35000 * Exn(sim02act, x = 27, n = 33), 2), 16437.00)
})

test_that("Exercises 13-16: life annuity present values", {
  # Exercise 13: age 46, EUR 20,000, whole-life annuity due and immediate.
  expect_equal(round(20000 * axn(sim02act, x = 46), 2), 281634.84)
  expect_equal(round(20000 * axn(sim02act, x = 46, payment = "immediate"), 2),
               265936.61)

  # Exercise 14: age 36, EUR 12,000, 24-year deferred whole-life annuity due.
  expect_equal(round(12000 * axn(sim02act, x = 36, m = 24), 2), 57445.20)

  # Exercise 15: age 36, EUR 12,000.
  expect_equal(round(12000 * axn(sim02act, x = 36, n = 25,
                                 payment = "immediate"), 2), 182391.60)
  expect_equal(round(12000 * axn(sim02act, x = 36, n = 20, m = 12), 2),
               98478.00)

  # Exercise 16: age 46, EUR 22,000.
  expect_equal(round(22000 * axn(sim02act, x = 46, n = 25,
                                 payment = "immediate"), 2), 320504.80)
  expect_equal(round(22000 * axn(sim02act, x = 46, n = 20, m = 2), 2),
               271917.80)
})

test_that("Exercise 17: whole-life, temporary and deferred death insurance", {
  expect_equal(round(100000 * Axn(sim02act, x = 28), 2), 16690.00)
  expect_equal(round(100000 * Axn(sim02act, x = 28, n = 37), 2), 5770.00)
  expect_equal(round(100000 * Axn(sim02act, x = 28, n = 37,
                                  m = 37), 2), 10920.00)
})

test_that("Exercise 18: death benefit increasing by age band", {
  a1 <- Axn(sim02act, x = 38, n = 22)
  a2 <- Axn(sim02act, x = 38, n = 10, m = 22)
  a3 <- Axn(sim02act, x = 38, m = 32)
  apv <- a1 + 2 * a2 + 3 * a3
  capital <- 30000 / (sqrt(1.04) * apv)

  expect_equal(round(capital, 2), 52887.89)
  expect_equal(round(2 * capital, 2), 105775.79)
  expect_equal(round(3 * capital, 2), 158663.69)
})

test_that("Exercise 19: annual premium for a simple endowment", {
  death_apv <- Axn(sim02act, x = 28, n = 30) * sqrt(1.04)
  life_apv <- Exn(sim02act, x = 28, n = 30)
  pure_single_premium <- 45000 * (life_apv + death_apv)
  annual_premium <- pure_single_premium /
    axn(sim02act, x = 28, n = 30)

  expect_equal(round(annual_premium, 2), 823.50)
})

test_that("Exercise 20: combined pension and death cover", {
  pension_apv <- 12000 * axn(sim02act, x = 40, m = 25)
  death_apv <- 100000 * Axn(sim02act, x = 40, n = 25) * sqrt(1.04)
  pure_single_premium <- pension_apv + death_apv
  loaded_premium <- pure_single_premium * 1.30

  expect_equal(round(pure_single_premium, 2), 50914.38)
  expect_equal(round(loaded_premium, 2), 66188.69)
})
