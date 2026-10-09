library(testthat)
library(lifecontingencies)

context("exn / exyzt: curtate and complete expectation definitions")

data(soa08Act)

test_that("exn curtate is the (temporary) sum of kpx", {
  for (n in c(1, 10, 30)) {
    expect_equal(exn(soa08Act, 40, n, "curtate"),
                 sum(sapply(1:n, function(k) pxt(soa08Act, 40, k))), tolerance = 1e-12)
  }
  # whole lifespan: e_x = sum_{k>=1} lx+k / lx
  lx <- soa08Act@lx
  expect_equal(exn(soa08Act, 40), sum(lx[soa08Act@x > 40]) / lx[soa08Act@x == 40], tolerance = 1e-12)
})

test_that("exn complete is nLx/lx = trapezoid of tpx and Tx/lx without n", {
  expect_equal(exn(soa08Act, 40, type = "complete"), Tx(soa08Act, 40) / soa08Act@lx[soa08Act@x == 40],
               tolerance = 1e-12)
  expect_equal(exn(soa08Act, 50, 20, "complete"),
               Lxt(soa08Act, 50, 20) / soa08Act@lx[soa08Act@x == 50], tolerance = 1e-12)
  # trapezoidal rule on the survival function
  s <- c(1, sapply(1:20, function(k) pxt(soa08Act, 50, k)))
  expect_equal(exn(soa08Act, 50, 20, "complete"), sum((s[-1] + s[-21]) / 2), tolerance = 1e-12)
})

test_that("complete = curtate + 0.5 (1 - npx) under UDD", {
  for (n in c(1, 10, 30, 60)) {
    expect_equal(exn(soa08Act, 40, n, "complete"),
                 exn(soa08Act, 40, n, "curtate") + 0.5 * (1 - pxt(soa08Act, 40, n)), tolerance = 1e-12)
  }
  # whole lifespan: np = 0, so complete = curtate + 0.5
  expect_equal(exn(soa08Act, 40, type = "complete"), exn(soa08Act, 40) + 0.5, tolerance = 1e-12)
})

test_that("exyzt complete expectation uses 0.5 (1 - tpxyz) and agrees with exn for a single life", {
  for (n in c(1, 10, 30)) {
    expect_equal(exyzt(list(soa08Act), 40, t = n, type = "Tx"),
                 exn(soa08Act, 40, n, "complete"), tolerance = 1e-12)
  }
  tabs <- list(soa08Act, soa08Act)
  ages <- c(55, 50)
  expect_equal(exyzt(tabs, ages, t = 10, type = "Tx"),
               exyzt(tabs, ages, t = 10, type = "Kx") +
                 0.5 * (1 - pxyzt(tabs, ages, 10, status = "joint")), tolerance = 1e-12)
  # unrestricted term: tpxy = 0 so the correction is the usual 0.5
  expect_equal(exyzt(tabs, ages, type = "Tx"), exyzt(tabs, ages, type = "Kx") + 0.5, tolerance = 1e-12)
})
