library(testthat)
library(lifecontingencies)

context("Legacy financial regression tests")

test_that("financial type abbreviations remain supported", {
  expect_equal(annuity(i = 0.05, n = 5, type = "due"), annuity(i = 0.05, n = 5, type = "adv"))
  expect_equal(annuity(i = 0.05, n = 5, type = "arr"), annuity(i = 0.05, n = 5, type = "im"))
  expect_equal(accumulatedValue(i = 0.05, n = 5, type = "arr"), accumulatedValue(i = 0.05, n = 5, type = "im"))
  expect_equal(decreasingAnnuity(i = 0.05, n = 5, type = "arr"), decreasingAnnuity(i = 0.05, n = 5, type = "im"))
  expect_equal(decreasingAnnuity(i = 0.05, n = 5, type = "due"), decreasingAnnuity(i = 0.05, n = 5, type = "adv"))
  expect_equal(increasingAnnuity(i = 0.05, n = 5, type = "arr"), increasingAnnuity(i = 0.05, n = 5, type = "im"))
})

test_that("invalid financial types are rejected", {
  expect_error(annuity(i = 0.05, n = 5, type = "foo1"))
})
