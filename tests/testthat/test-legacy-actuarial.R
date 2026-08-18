library(testthat)
library(lifecontingencies)

data(soa08Act)

context("Legacy actuarial regression tests")

test_that("abbreviated actuarial arguments remain supported", {
  expect_no_error(axn(soa08Act, x = 90, type = "E"))
  expect_no_error(axn(soa08Act, x = 90, type = "exp"))
  expect_no_error(axn(soa08Act, x = 90, type = "S", payment = "adv"))
  expect_no_error(axn(soa08Act, x = 90, type = "sto", payment = "arr"))
  expect_no_error(Iaxn(soa08Act, x = 90, type = "E"))
  expect_no_error(Axn(soa08Act, x = 90, type = "E"))
  expect_no_error(IAxn(soa08Act, x = 90, type = "E"))
  expect_no_error(DAxn(soa08Act, x = 90, type = "E"))
  expect_no_error(Exn(soa08Act, x = 90, n = 5, type = "E"))
  expect_no_error(axyzn(list(soa08Act, soa08Act, soa08Act), x = 90:88, type = "E"))
  expect_no_error(Axyzn(list(soa08Act, soa08Act, soa08Act), x = 90:88, type = "E"))
  expect_no_error(axyzn(list(soa08Act, soa08Act), x = 60:61, status = "j"))
  expect_no_error(axyzn(list(soa08Act, soa08Act), x = 60:61, status = "l"))
  expect_no_error(Axyzn(list(soa08Act, soa08Act), x = 60:61, status = "j"))
  expect_no_error(Axyzn(list(soa08Act, soa08Act), x = 60:61, status = "l"))
  expect_error(axn(soa08Act, x = 90, type = "foo1"))
})

test_that("multiple-table annuity vectorization agrees with scalar implementation", {
  data(soaLt)
  tab <- with(soaLt, new("actuarialtable", interest = 0.06,
                          x = x, lx = Ix, name = "SOA2008"))
  tables <- list(tab, tab, tab)
  result <- sapply(10:90, function(y) {
    axyznvect(tables, x = c(y, y + 1, y + 2)) ==
      axyzn(tables, x = c(y, y + 1, y + 2))
  })
  expect_true(all(result))
})

test_that("Axn agrees with the historical implementation", {
  Axn_old_x <- Vectorize(lifecontingencies:::Axnold, "x")
  Axn_old_n <- Vectorize(lifecontingencies:::Axnold, "n")
  Axn_old_m <- Vectorize(lifecontingencies:::Axnold, "m")

  for (k in c(1, 2, 4)) {
    expect_equal(Axn(soa08Act, x = 30:35 + 0.5, n = 10, i = 0.06, k = k),
                 Axn_old_x(soa08Act, x = 30:35 + 0.5, n = 10, i = 0.06, k = k),
                 tolerance = 1e-10)
  }
  expect_equal(Axn(soa08Act, x = 33, n = 1:20, i = 0.06),
               Axn_old_n(soa08Act, x = 33, n = 1:20, i = 0.06),
               tolerance = 1e-10)
  expect_equal(Axn(soa08Act, x = 33, n = 20, m = 0:10, i = 0.06),
               Axn_old_m(soa08Act, x = 33, n = 20, m = 0:10, i = 0.06),
               tolerance = 1e-10)
})

test_that("axn agrees with the historical implementation", {
  axn_old_x <- Vectorize(lifecontingencies:::axnold, "x")
  axn_old_n <- Vectorize(lifecontingencies:::axnold, "n")
  axn_old_m <- Vectorize(lifecontingencies:::axnold, "m")

  for (payment in c("due", "arrears", "immediate", "advance")) {
    expect_equal(axn(soa08Act, x = 1:100, payment = payment),
                 axn_old_x(soa08Act, x = 1:100, payment = payment), tolerance = 1e-10)
    expect_equal(axn(soa08Act, x = 33, n = 10:30, payment = payment),
                 axn_old_n(soa08Act, x = 33, n = 10:30, payment = payment), tolerance = 1e-10)
    expect_equal(axn(soa08Act, x = 33, n = 30, m = 0:10, payment = payment),
                 axn_old_m(soa08Act, x = 33, m = 0:10, n = 30, payment = payment), tolerance = 1e-10)
  }
})

test_that("historical actuarial capital benchmarks remain valid", {
  expect_lt(abs(Axn(soa08Act, x = 30, i = 0.06, k = 4) - 0.1048), 5e-4)
  AxncheckR <- function(object, x, m, n) {
    i <- object@interest
    f <- function(t) pxt(object, x = x, t = t) * qxt(object, x = x + t, t = 1)
    prob <- sapply(m:(m + n - 1), f)
    rowSums(prob / ((1 + i)^(m + 1):(m + n)))
  }
  expect_equal(Axn(soa08Act, x = 65:66, n = 1, m = 1),
               AxncheckR(soa08Act, x = 65:66, m = 1, n = 1), tolerance = 1e-10)
})
