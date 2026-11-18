## tests/testthat/test-mdt-negative-survival.R
## Regression test for GH #34 ("Negative survival probability").
##
## A multiple-decrement table must not imply a negative survival
## probability. This happens when a decrement count is negative, or when
## the survivors column (lx) is driven below zero by decrements that
## exceed the survivors at some age. The `mdt` validity method now rejects
## such tables at construction time instead of letting the bad values
## propagate into pxt()/qxt() and surface as negative probabilities.

library(lifecontingencies)

context("mdt negative survival probability guard (GH #34)")

test_that("a negative decrement count is rejected", {
  ## sum(death) == lx[1] so the balance check passes; the -10 entry is the
  ## only thing wrong and must be caught on its own.
  badDecrement <- data.frame(
    x     = 0:2,
    lx    = c(100, 40, 50),
    death = c(60, -10, 50)
  )
  expect_error(
    new("mdt", name = "negativeDecrement", table = badDecrement),
    regexp = "Decrement counts must be non-negative"
  )
})

test_that("a negative survivors (lx) value is rejected", {
  ## Decrements are all non-negative and balance to lx[1]; the inconsistent,
  ## negative lx at age 1 is what implies a negative survival probability.
  badSurvivors <- data.frame(
    x     = 0:2,
    lx    = c(100, -5, 0),
    death = c(100, 0, 0)
  )
  expect_error(
    new("mdt", name = "negativeSurvivors", table = badSurvivors),
    regexp = "negative survival probability"
  )
})

test_that("a well-formed decrement table still constructs", {
  goodTable <- data.frame(
    x     = 0:2,
    lx    = c(100, 70, 40),
    death = c(30, 30, 40)
  )
  obj <- new("mdt", name = "wellFormed", table = goodTable)
  expect_s4_class(obj, "mdt")
  ## Derived one-year survival probabilities stay in [0, 1].
  expect_true(all(pxt(obj, x = 0, t = 0:2) >= 0))
  expect_true(all(pxt(obj, x = 0, t = 0:2) <= 1))
})
