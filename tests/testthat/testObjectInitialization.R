library(lifecontingencies)

context("Object Initialization")



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