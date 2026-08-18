library(testthat)
library(lifecontingencies)

context("US 1979-1981 life table demographic regression tests")

test_that("US7981 one-year and multiple-year probabilities agree with reference functions", {
  x <- 0:109
  lx <- c(100000,
    98000 + c(740, 648, 584, 535, 495, 459, 426, 396, 370, 347, 328, 309, 285, 248, 196, 129, 047),
    97000 + c(953, 851, 741, 623, 499, 370, 240, 110),
    96000 + c(982, 856, 730, 604, 477, 350, 220, 88),
    95000 + c(951, 808, 655, 492, 317, 129),
    94000 + c(926, 706, 465, 201),
    93000 + c(913, 599, 256),
    92000 + c(882, 472, 21), 91526, 90986, 90402, 89771, 89087, 88348, 87551, 86695, 85776, 84789,
    83726, 82726, 81348, 80024, 78609, 77107, 75520, 73846, 72082, 70218, 68248, 66165, 63972, 61673,
    59279, 56799, 54239, 51599, 48878, 46071, 43180, 40208, 37172, 34095, 31012, 27960, 24961, 22038,
    19235, 16598, 14154, 11908, 9863, 8032, 6424, 5043, 3884, 2939, 2185, 1598, 1150, 815, 570, 393,
    267, 179, 119, 78, 51, 33)
  tab <- new("lifetable", x = x, lx = lx)
  tol <- (.Machine$double.eps)^(1/3)
  pXt <- Vectorize(pxt, "x")
  qXt <- Vectorize(qxt, "x")
  pxT <- Vectorize(pxt, "t")
  qxT <- Vectorize(qxt, "t")
  ref_pxqx <- lifecontingencies:::getpxqx(lx)
  expect_lt(sum(abs(pXt(tab, x = head(x, -1), t = 1) - ref_pxqx[, "px"])), tol)
  expect_lt(sum(abs(qXt(tab, x = head(x, -1), t = 1) - ref_pxqx[, "qx"])), tol)
  for (age in c(5, 10, 15, 20, 25, 30, 60, 70)) {
    ref <- lifecontingencies:::getkpxqxk(lx, x = age)
    expect_lt(sum(abs(pxT(tab, x = age, t = 0:(max(x) - age)) - ref[, "kpx"])), tol)
  }
})

test_that("US7981 fractional-year probabilities agree with reference functions", {
  x <- 0:109
  lx <- c(100000, 98000 + c(740, 648, 584, 535, 495, 459, 426, 396, 370, 347, 328, 309, 285, 248, 196, 129, 047),
    97000 + c(953, 851, 741, 623, 499, 370, 240, 110), 96000 + c(982, 856, 730, 604, 477, 350, 220, 88),
    95000 + c(951, 808, 655, 492, 317, 129), 94000 + c(926, 706, 465, 201), 93000 + c(913, 599, 256),
    92000 + c(882, 472, 21), 91526, 90986, 90402, 89771, 89087, 88348, 87551, 86695, 85776, 84789,
    83726, 82726, 81348, 80024, 78609, 77107, 75520, 73846, 72082, 70218, 68248, 66165, 63972, 61673,
    59279, 56799, 54239, 51599, 48878, 46071, 43180, 40208, 37172, 34095, 31012, 27960, 24961, 22038,
    19235, 16598, 14154, 11908, 9863, 8032, 6424, 5043, 3884, 2939, 2185, 1598, 1150, 815, 570, 393,
    267, 179, 119, 78, 51, 33)
  tab <- new("lifetable", x = x, lx = lx)
  tol <- (.Machine$double.eps)^(1/3)
  myt <- 1:30/8
  for (spec in list(c("linear", "linear"), c("hyperbolic", "balducci"), c("constant force", "constant"))) {
    expect_lt(sum(abs(pxt(tab, x = 10, t = myt, fractional = spec[1]) -
                       lifecontingencies:::gettpx(lx, x = 10, k = myt, fractional = spec[2]))), tol)
    expect_lt(sum(abs(qxt(tab, x = 10, t = myt, fractional = spec[1]) -
                       lifecontingencies:::gettqx(lx, x = 10, k = myt, fractional = spec[2]))), tol)
  }
})

test_that("US7981 residual life expectancy agrees with the reference", {
  x <- 0:109
  lx <- c(100000, 98000 + c(740, 648, 584, 535, 495, 459, 426, 396, 370, 347, 328, 309, 285, 248, 196, 129, 047),
    97000 + c(953, 851, 741, 623, 499, 370, 240, 110), 96000 + c(982, 856, 730, 604, 477, 350, 220, 88),
    95000 + c(951, 808, 655, 492, 317, 129), 94000 + c(926, 706, 465, 201), 93000 + c(913, 599, 256),
    92000 + c(882, 472, 21), 91526, 90986, 90402, 89771, 89087, 88348, 87551, 86695, 85776, 84789,
    83726, 82726, 81348, 80024, 78609, 77107, 75520, 73846, 72082, 70218, 68248, 66165, 63972, 61673,
    59279, 56799, 54239, 51599, 48878, 46071, 43180, 40208, 37172, 34095, 31012, 27960, 24961, 22038,
    19235, 16598, 14154, 11908, 9863, 8032, 6424, 5043, 3884, 2939, 2185, 1598, 1150, 815, 570, 393,
    267, 179, 119, 78, 51, 33)
  tab <- new("lifetable", x = x, lx = lx)
  ex <- function(age) sum(lifecontingencies:::getkpxqxk(lx, x = age)[-1, "kpx"])
  expect_lt(sum(sapply(0:20, function(age) abs(ex(0) - exn(tab, 0)))),
            (.Machine$double.eps)^(1/3))
})
