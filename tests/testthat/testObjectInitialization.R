library(lifecontingencies)

context("Object Initialization")

test_that("Unequal lengths and population at risk is non-increasing", {
  x <- 0:5
  lx <- c(100, 75, 50, 51, 12)
  
  expect_error(new("lifetable", x = x, lx = lx))
})

test_that("Increasing x", {
  x <- c(0, 2, 1, 3)
  lx <- c(100, 50, 75, 25)
  tbl <- lifetable(x = x, lx = lx)
  
  expect_equal(tbl@x, sort(x))
  expect_equal(tbl@lx, sort(lx, decreasing = TRUE))
})

test_that("Zeros and NAs in lx are removed", {
  x <- 0:4

  lx <- c(100, 75, 50, 25, 0)
  tbl <- lifetable(x = x, lx = lx)
  expect_equal(tbl@x, c(0, 1, 2, 3))
  expect_equal(tbl@lx, c(100, 75, 50, 25))
  
  lx <- c(100, NA, 50, 25, 12)
  tbl <- lifetable(x = x, lx = lx)
  expect_equal(tbl@x, c(0, 2, 3, 4))
  expect_equal(tbl@lx, c(100, 50, 25, 12))
  
  lx <- c(100, NA, 50, 25, 0)
  tbl <- lifetable(x = x, lx = lx)
  expect_equal(tbl@x, c(0, 2, 3))
  expect_equal(tbl@lx, c(100, 50, 25))

  lx <- c(100, NA, 50, NA, 0)
  tbl <- lifetable(x = x, lx = lx)
  expect_equal(tbl@x, c(0, 2))
  expect_equal(tbl@lx, c(100, 50))
  
})