library(testthat)
library(lifecontingencies)

context("Legacy pxt class regression tests")

test_that("pxt and pXt agree for lifetable and actuarialtable", {
  x <- 0:4
  lx <- c(4832555, 4821937, 4810206, 4797185, 4782737)
  lt <- new("lifetable", name = "ValdezExample", x = x, lx = lx)
  at <- new("actuarialtable", name = "ValdezExample", x = x, lx = lx, interest = 0.02)
  pXt <- Vectorize(pxt, "x")
  expect_equal(pxt(lt, x = 0:2, t = 3), pXt(lt, x = 0:2, t = 3))
  expect_equal(pxt(at, x = 0:2, t = 3), pXt(at, x = 0:2, t = 3))
})

test_that("pxt and qxt support mdt decrements", {
  tab <- data.frame(
    x = 50:54,
    lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
    heart = c(5168, 5363, 5618, 5929, 6277),
    accidents = c(1157, 1206, 1443, 1679, 2152),
    other = c(4293, 5162, 5960, 6840, 7631)
  )
  mdt <- new("mdt", name = "ValdezExample", table = tab)
  expect_equal(dxt(mdt, x = 51, t = 2, decrement = "other"), 11122)
  expect_equal(round(pxt(mdt, x = 50, t = 3), 5), 0.99268)
  expect_equal(round(qxt(mdt, x = 50, t = 3, decrement = "heart"), 5), 0.00334)
  expect_equal(pxt(mdt, x = 50:51, t = 3), Vectorize(pxt, "x")(mdt, x = 50:51, t = 3))
})

test_that("strange non-consecutive ages and plateaus remain well-defined", {
  x_bad <- seq(0, 100, by = 10)
  lx <- c(100, 99, 98, 97, 96, 93, 86, 73, 50, 17)
  expect_error(new("lifetable", x = x_bad, lx = lx))

  tab <- new("lifetable", x = 0:9, lx = lx)
  expect_no_error(pxt(tab, x = 0:10, t = 1))

  plateau <- new("lifetable", x = 0:10,
                 lx = c(100, 99, 98, 97, 96, 96, 96, 96, 96, 96, 96))
  expect_no_error(pxt(plateau, x = 0:10, t = 1))
})
