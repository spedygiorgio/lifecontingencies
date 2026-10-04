library(testthat)
library(lifecontingencies)

context("Published examples: curtate and complete expectation of life (exn, exyzt)")

## Worked examples from the actuarial literature whose life table can be rebuilt
## exactly (or to negligible error) at integer ages:
##
## [Finan]  Finan M.B., "A Reader's Guide to Calculus ... / Exam MLC study manual"
##          (Arkansas Tech Univ.), chapters 20 and 23 (project copy 65735704-Exam-MLC-Finan.pdf):
##          Ex. 20.17, 20.19, 23.20-23.24.
## [SOA]    Society of Actuaries, Exam MLC sample questions (2014 spring): Q65
##          (piecewise constant force) and the temporary-expectation question with
##          tq0 = t^2/10000 (listed as question 2.4 in the SOA MLC/LTAM question sets).
## [AMLCR]  Dickson, Hardy, Waters, Actuarial Mathematics for Life Contingent Risks,
##          Exercise 2.1: F0(t) = 1 - (1 - t/105)^(1/5): e50 = 45.83, curtate 45.18.
##
## Package conventions: the complete expectation is evaluated under UDD,
## i.e. by the trapezoidal rule on tpx (exn(type="complete") = nLx/lx). It is exact
## for a linear lx (de Moivre) and otherwise approximates the integral.

mk <- function(x, lx, name) new("lifetable", x = x, lx = lx, name = name)
deMoivre <- function(w) mk(0:w, w - (0:w), paste("de Moivre", w))

test_that("[Finan Ex. 23.24] curtate expectation from a small life table extract", {
  t80 <- mk(80:86, c(250, 217, 161, 107, 62, 28, 0), "Finan 23.24")
  expect_equal(exn(t80, 80), 2.3, tolerance = 1e-12)
  # The book prints e_{80:3} = 1.64 but (217 + 161 + 107)/250 = 1.94 (typo in the book).
  expect_equal(exn(t80, 80, 3), (217 + 161 + 107) / 250, tolerance = 1e-12)
  expect_equal(round(exn(t80, 80, 3), 2), 1.94)
  # complete expectation of the same extract: sum(Lx)/l80 with Lx = (lx + lx+1)/2
  expect_equal(exn(t80, 80, type = "complete"), (sum(c(250, 217, 161, 107, 62, 28)) - 125) / 250,
               tolerance = 1e-12)
})

test_that("[Finan Ex. 20.17, 20.19] complete expectation under uniform / de Moivre models", {
  # X uniform on [0, 90]: complete expectation at 30 is 30
  expect_equal(exn(deMoivre(90), 30, type = "complete"), 30, tolerance = 1e-12)
  # de Moivre omega = 100: e30 = (omega - 30)/2 = 35; after the breakthrough omega = 108: 39
  expect_equal(exn(deMoivre(100), 30, type = "complete"), 35, tolerance = 1e-12)
  expect_equal(exn(deMoivre(108), 30, type = "complete"), 39, tolerance = 1e-12)
})

test_that("[Finan Ex. 23.20-23.22] temporary complete expectation, de Moivre", {
  # general formula: e_{x:n} = n - n^2 / (2 (omega - x))
  for (case in list(c(30, 40, 95), c(25, 11, 100), c(40, 20, 100))) {
    x <- case[1]; n <- case[2]; w <- case[3]
    expect_equal(exn(deMoivre(w), x, n, "complete"), n - n^2 / (2 * (w - x)), tolerance = 1e-12)
  }
  # Ex. 23.21: e_{30:40} = 27.692 gives omega = 95
  expect_equal(round(exn(deMoivre(95), 30, 40, "complete"), 3), 27.692)
  # Ex. 23.22: Audra, standard 11-year temporary complete expectation
  expect_equal(round(exn(deMoivre(100), 25, 11, "complete"), 4), 10.1933)
})

test_that("[Finan Ex. 23.23] s(x) = 1 - (0.01x)^2: e_{30:50} = 37.18", {
  x <- 0:100
  tb <- mk(x, round(1e8 * (1 - (0.01 * x)^2)), "Finan 23.23")
  expect_equal(round(exn(tb, 30, 50, "complete"), 2), 37.18)
})

test_that("[SOA MLC Q65] piecewise constant force: e_{25:25} = 15.6", {
  H <- function(x) ifelse(x < 40, 0.04 * x, 1.6 + 0.05 * (x - 40))
  ages <- 0:200
  tb <- mk(ages, 1e8 * exp(-H(ages)), "SOA Q65")
  exact <- (1 - exp(-0.6)) / 0.04 + exp(-0.6) * (1 - exp(-0.5)) / 0.05   # 15.5985
  expect_equal(round(exact, 1), 15.6)
  expect_equal(exn(tb, 25, 25, "complete"), exact, tolerance = 1e-3)
  expect_equal(round(exn(tb, 25, 25, "complete"), 1), 15.6)
})

test_that("[SOA MLC] tq0 = t^2/10000: temporary complete expectation e_{75:10} = 8.21", {
  x <- 0:100
  tb <- mk(x, 1e8 * (1 - x^2 / 1e4), "SOA t^2/10000")
  expect_equal(round(exn(tb, 75, 10, "complete"), 2), 8.21)
})

test_that("[AMLCR Ex. 2.1] curtate 45.18 exact; complete 45.83 within the UDD approximation", {
  x <- 0:104
  tb <- mk(x, 1e8 * (1 - x / 105)^0.2, "AMLCR 2.1")
  expect_equal(round(exn(tb, 50), 2), 45.18)                       # curtate: exact
  exactComplete <- 105 / 1.2 * (1 - 50 / 105)                      # 45.8333
  expect_equal(round(exactComplete, 2), 45.83)
  # steep survival function near omega: the trapezoid (UDD) value is curtate + 0.5
  expect_equal(exn(tb, 50, type = "complete"), exn(tb, 50) + 0.5, tolerance = 1e-12)
  expect_lt(abs(exn(tb, 50, type = "complete") - exactComplete), 0.2)
})

test_that("exyzt complete expectation of joint / last-survivor status, finite and infinite term", {
  # independent de Moivre lives (50) and (60), omega = 100: tpxy = (1 - t/50)(1 - t/40)
  # closed-form integrals (not tabulated in the literature, derived analytically)
  tabs <- list(deMoivre(100), deMoivre(100))
  ages <- c(50, 60)
  ejoint <- function(t) integrate(function(u) (1 - u / 50) * (1 - u / 40), 0, t)$value
  for (t in c(10, 20, 40)) {
    expect_equal(exyzt(tabs, ages, t = t, type = "Tx"), ejoint(t), tolerance = 5e-4)
  }
  expect_equal(exyzt(tabs, ages, type = "Tx"), ejoint(40), tolerance = 5e-4)       # whole lifespan
  # last-survivor: e50 + e60 - e(joint) = 25 + 20 - 14.6667
  expect_equal(exyzt(tabs, ages, type = "Tx", status = "last"), 25 + 20 - ejoint(40), tolerance = 5e-4)
  # curtate version is the plain sum of tpxy
  expect_equal(exyzt(tabs, ages, t = 10, type = "Kx"),
               sum(sapply(1:10, function(k) pxyzt(tabs, ages, k, status = "joint"))), tolerance = 1e-12)
})
