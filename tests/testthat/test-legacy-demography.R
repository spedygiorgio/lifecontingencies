library(testthat)
library(lifecontingencies)

data(soa08Act)
context("Legacy demographic regression tests")

test_that("pxt supports historical fractional interpolation abbreviations", {
  for (frac in c("const", "exp", "lin", "unif", "hyp", "Bal", "harm"))
    expect_no_error(pxt(soa08Act, x = 90, t = 3/2, frac = frac))
  for (frac in c("const", "exp", "lin", "unif", "hyp", "Bal", "harm"))
    expect_no_error(qxt(soa08Act, x = 90, t = 3/2, frac = frac))
  for (type in c("cur", "Kx", "con", "Tx"))
    expect_no_error(exn(soa08Act, x = 90, type = type))
})

test_that("multiple-life demographic abbreviations remain supported", {
  expect_no_error(pxyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10.33, frac = "lin"))
  expect_no_error(pxyzt(list(soa08Act, soa08Act, soa08Act), x = c(55, 50, 45), t = 10.33,
                        frac = c("unif", "Bal", "exp")))
  for (status in c("jo", "Last-Survi", "la"))
    expect_no_error(pxyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10.33, status = status))
  expect_no_error(exyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10.33, status = "la", type = "K"))
  expect_no_error(exyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10.33, status = "jo", type = "T"))
})

test_that("invalid demographic arguments are rejected", {
  expect_error(pxt(soa08Act, x = 90, t = 2, frac = "foo1"))
  expect_error(pxt(soa08Act, x = 90, t = 2, frac = TRUE))
  expect_error(qxt(soa08Act, x = 90, t = 2, frac = "foo1"))
  expect_error(qxt(soa08Act, x = 90, t = 2, frac = TRUE))
  expect_error(exn(soa08Act, x = 90, type = "foo3"))
  expect_error(pxyzt(list(soa08Act, soa08Act, soa08Act), x = c(55, 50, 45), t = 10.33,
                     frac = c("foo1", "Bal", "exp")))
  expect_error(pxyzt(list(soa08Act, soa08Act, soa08Act), x = c(55, 50, 45), t = 10.33,
                     frac = c("unif", FALSE, "exp")))
  expect_error(pxyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10.33, status = "foo2"))
  expect_error(pxyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10.33, status = log(3)))
  expect_error(exyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10.33, status = "lol", type = "K"))
  expect_error(exyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10.33, status = "jo", type = 12345))
})

test_that("pxt agrees with the historical implementation for fractional ages and times", {
  pXt_old <- Vectorize(lifecontingencies:::pxtold, "x")
  pxT_old <- Vectorize(lifecontingencies:::pxtold, "t")
  check_x <- function(frac) {
    ages <- seq(1, 100, by = 1/4)
    expect_lt(sum(abs(pxt(soa08Act, x = ages, t = 1/3, fractional = frac) -
                      pXt_old(soa08Act, x = ages, t = 1/3, fractional = frac))), 1e-6)
  }
  check_t <- function(frac) {
    times <- seq(1, 30, by = 1/4)
    expect_lt(sum(abs(pxt(soa08Act, x = 2, t = times, fractional = frac) -
                      pxT_old(soa08Act, x = 2, t = times, fractional = frac))), 1e-6)
  }
  for (frac in c("linear", "harm", "exp")) {
    check_x(frac)
    check_t(frac)
  }
})

test_that("pxt works for non-consecutive and high ages", {
  set.seed(123)
  ages <- rpois(10, 45)
  expect_no_error(pxt(soa08Act, x = ages, t = 1))
  expect_no_error(pxt(soa08Act, x = 135:145, t = 1))
})
