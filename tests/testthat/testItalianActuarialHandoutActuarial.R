library(testthat)
library(lifecontingencies)

context("Italian actuarial handout: actuarial examples")

# The handout uses the ISTAT 1998 Italian male table, published in 2002,
# rather than demoIta$SIM02. The fixture is defined in helper-italian-handout.R.
italy1998_male_act <- italy1998_male_act

# p. 29: Exercise 4, pure endowment age 30, term 20, EUR 30,000.
test_that("Exercise 4: deferred capital", {
  expect_equal(round(30000 * Exn(italy1998_male_act, x = 30, n = 20), 2), 13184.89)
})

# p. 33: Exercise 5, pure endowment age 35, term 30, EUR 16,000.
# p. 33: Exercise 6, age 40, premium EUR 26,000, term 22; solve for capital.
test_that("Exercises 5-6: deferred capital", {
  expect_equal(round(16000 * Exn(italy1998_male_act, x = 35, n = 30), 2), 6460.01)
  expect_equal(round(26000 / Exn(italy1998_male_act, x = 40, n = 22), 2), 68772.60)
})

# p. 35-36: Exercise 9, age 32, EUR 15,000, 20-year pure endowment;
# Exercise 12 repeats it at age 50.
# p. 35: Exercise 11, two pure endowments for ages 27 -> 50 and 60.
test_that("Exercises 9, 11 and 12: deferred capital", {
  expect_equal(round(15000 / Exn(italy1998_male_act, x = 32, n = 20), 2), 34314.00)
  expect_equal(round(15000 / Exn(italy1998_male_act, x = 50, n = 20), 2), 41976.00)
  expect_equal(round(20000 * Exn(italy1998_male_act, x = 27, n = 23) +
                   35000 * Exn(italy1998_male_act, x = 27, n = 33), 2), 16437.00)
})

# p. 42: Exercise 13, age 46, EUR 20,000, whole-life annuity due and immediate.
test_that("Exercise 13: whole-life annuity present values", {
  expect_equal(round(20000 * axn(italy1998_male_act, x = 46), 2), 281634.84)
  expect_equal(round(20000 * axn(italy1998_male_act, x = 46, payment = "immediate"), 2),
               265936.61)
})

# p. 45: Exercise 14, age 36, EUR 12,000, 24-year deferred whole-life annuity due.
test_that("Exercise 14: deferred whole-life annuity", {
  expect_equal(round(12000 * axn(italy1998_male_act, x = 36, m = 24), 2), 57445.20)
})

# pp. 48-49: Exercise 15, age 36, EUR 12,000; temporary immediate and deferred annuities.
test_that("Exercise 15: temporary annuity present values", {
  expect_equal(round(12000 * axn(italy1998_male_act, x = 36, n = 25,
                                 payment = "immediate"), 2), 182391.60)
  expect_equal(round(12000 * axn(italy1998_male_act, x = 36, n = 20, m = 12), 2),
               98478.00)
})

# p. 49: Exercise 16, age 46, EUR 22,000; temporary immediate and deferred annuities.
test_that("Exercise 16: temporary annuity present values", {
  expect_equal(round(22000 * axn(italy1998_male_act, x = 46, n = 25,
                                 payment = "immediate"), 2), 320504.80)
  expect_equal(round(22000 * axn(italy1998_male_act, x = 46, n = 20, m = 2), 2),
               271917.80)
})

# p. 59: Exercise 17, age 28, EUR 100,000; whole-life, 37-year temporary,
# and deferred whole-life death insurance. The handout pays at end of year of death,
# so Axn is used directly (the half-year correction is introduced only later on p. 60).
test_that("Exercise 17: whole-life, temporary and deferred death insurance", {
  expect_equal(round(100000 * Axn(italy1998_male_act, x = 28), 2), 16690.00)
  expect_equal(round(100000 * Axn(italy1998_male_act, x = 28, n = 37), 2), 5770.00)
  expect_equal(round(100000 * Axn(italy1998_male_act, x = 28, n = 37,
                                  m = 37), 2), 10920.00)
})

# p. 61: Exercise 18, age 38, benefits C / 2C / 3C by age band.
# The handout explicitly introduces the half-year death-payment correction on p. 60,
# so the APV is multiplied by sqrt(1.04).
test_that("Exercise 18: death benefit increasing by age band", {
  a1 <- Axn(italy1998_male_act, x = 38, n = 22)
  a2 <- Axn(italy1998_male_act, x = 38, n = 10, m = 22)
  a3 <- Axn(italy1998_male_act, x = 38, m = 32)
  apv <- a1 + 2 * a2 + 3 * a3
  capital <- 30000 / (sqrt(1.04) * apv)

  expect_equal(round(capital, 2), 52887.89)
  expect_equal(round(2 * capital, 2), 105775.79)
  expect_equal(round(3 * capital, 2), 158663.69)
})

# p. 66: Exercise 19, annual premium for a 30-year simple endowment at age 28.
# Death benefit is adjusted by the half-year factor introduced on p. 60.
test_that("Exercise 19: annual premium for a simple endowment", {
  death_apv <- Axn(italy1998_male_act, x = 28, n = 30) * sqrt(1.04)
  life_apv <- Exn(italy1998_male_act, x = 28, n = 30)
  pure_single_premium <- 45000 * (life_apv + death_apv)
  annual_premium <- pure_single_premium /
    axn(italy1998_male_act, x = 28, n = 30)

  expect_equal(round(annual_premium, 2), 823.50)
})

# p. 67: Exercise 20, age 40, deferred pension plus death cover and 30% loading.
test_that("Exercise 20: combined pension and death cover", {
  pension_apv <- 12000 * axn(italy1998_male_act, x = 40, m = 25)
  death_apv <- 100000 * Axn(italy1998_male_act, x = 40, n = 25) * sqrt(1.04)
  pure_single_premium <- pension_apv + death_apv
  loaded_premium <- pure_single_premium * 1.30

  expect_equal(round(pure_single_premium, 2), 50914.38)
  expect_equal(round(loaded_premium, 2), 66188.69)
})
