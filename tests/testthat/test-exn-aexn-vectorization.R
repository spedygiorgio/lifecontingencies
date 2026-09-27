library(testthat)
library(lifecontingencies)

context("Exn and AExn vectorized over age, term and interest")

data("soa08Act", package = "lifecontingencies", envir = environment())

scalar_loop <- function(f, x, n, ...) {
  len <- max(length(x), length(n))
  x <- rep_len(x, len); n <- rep_len(n, len)
  vapply(seq_len(len), function(j) f(soa08Act, x = x[j], n = n[j], ...), numeric(1))
}

test_that("Exn: a vector call equals the element-by-element call", {
  x <- c(20, 35, 50, 65, 80, 100)
  n <- c(10, 20, 30, 5, 15, 25)
  expect_equal(Exn(soa08Act, x, n), scalar_loop(Exn, x, n), tolerance = 1e-14)
  expect_equal(Exn(soa08Act, x, n, power = 2), scalar_loop(Exn, x, n, power = 2),
               tolerance = 1e-14)
  expect_equal(Exn(soa08Act, x, n, i = 0.03), scalar_loop(Exn, x, n, i = 0.03),
               tolerance = 1e-14)
})

test_that("AExn: a vector call equals the element-by-element call", {
  x <- c(20, 35, 50, 65, 80)
  n <- c(10, 20, 30, 5, 15)
  expect_equal(AExn(soa08Act, x, n), scalar_loop(AExn, x, n), tolerance = 1e-14)
  expect_equal(AExn(soa08Act, x, n, k = 12), scalar_loop(AExn, x, n, k = 12),
               tolerance = 1e-14)
  expect_equal(AExn(soa08Act, x, n, power = 2), scalar_loop(AExn, x, n, power = 2),
               tolerance = 1e-14)
})

test_that("scalar arguments are recycled against vector ones", {
  expect_equal(Exn(soa08Act, 40, c(5, 10, 20)), scalar_loop(Exn, 40, c(5, 10, 20)))
  expect_equal(Exn(soa08Act, c(30, 40, 50), 10), scalar_loop(Exn, c(30, 40, 50), 10))
  expect_equal(AExn(soa08Act, c(30, 40, 50), 10), scalar_loop(AExn, c(30, 40, 50), 10))
  expect_length(Exn(soa08Act, 40:59, 10), 20)
})

test_that("interest can vary by element", {
  expect_equal(Exn(soa08Act, 40, 20, i = c(0.03, 0.05)),
               c(Exn(soa08Act, 40, 20, i = 0.03), Exn(soa08Act, 40, 20, i = 0.05)))
  expect_equal(AExn(soa08Act, c(40, 50), 20, i = c(0.03, 0.05)),
               c(AExn(soa08Act, 40, 20, i = 0.03), AExn(soa08Act, 50, 20, i = 0.05)))
})

test_that("zero terms, missing n and empty input", {
  expect_equal(AExn(soa08Act, c(40, 50, 60), c(0, 10, 0)),
               c(1, AExn(soa08Act, 50, 10), 1))
  expect_equal(Exn(soa08Act, c(40, 50), 0), c(1, 1))
  omega <- getOmega(soa08Act)
  x <- c(30, 60, 90)
  expect_equal(AExn(soa08Act, x),
               vapply(x, function(a) AExn(soa08Act, a, omega - a - 1), numeric(1)))
  expect_length(Exn(soa08Act, numeric(0), 10), 0)
})

test_that("validation is unchanged", {
  expect_error(Exn(soa08Act, n = 10), "age")
  expect_error(Exn(soa08Act, 40), "term")
  expect_error(AExn(soa08Act, c(40, -1), 10), "Negative")
  expect_error(AExn(soa08Act, 40, 10, k = 0), "Periods")
})

test_that("stochastic draws return one value per element", {
  set.seed(123)
  out <- Exn(soa08Act, c(40, 50, 60), 10, type = "ST")
  expect_length(out, 3)
  expect_true(all(out %in% c(0, 1.06^-10)))
  expect_length(AExn(soa08Act, c(40, 50), 10, type = "ST"), 2)
})
