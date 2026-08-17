library(testthat)
library(lifecontingencies)

data(soa08Act)
context("Legacy multiple-life regression tests")

test_that("pxyzt agrees with the historical implementation for vectorized ages", {
  tables <- list(soa08Act, soa08Act, soa08Act)
  x <- cbind(1:120, 11:20, 91:120)
  old <- function(x, fractional)
    sapply(1:NROW(x), function(i)
      lifecontingencies:::pxyztold(x = x[i,], tablesList = tables, t = 1/4,
                                   fractional = fractional))
  for (frac in c("linear", "harm", "exp")) {
    expect_lt(sum(abs(pxyzt(tables, x = x, t = 1/4, fractional = frac) - old(x, frac))), 1e-6)
  }
})

test_that("pxyzt agrees with the historical implementation for vectorized times", {
  tables <- list(soa08Act, soa08Act, soa08Act)
  x <- cbind(1:120, 11:20, 91:120)
  t <- cbind(1:20, 1:20, 1:20)
  t <- rbind(t, t, t, t, t, t) + 1/2
  old <- function(frac)
    sapply(1:NROW(x), function(i)
      lifecontingencies:::pxyztold(x = x[i,], tablesList = tables, t = t[i,1],
                                   fractional = frac))
  for (frac in c("linear", "harm", "exp")) {
    expect_lt(sum(abs(pxyzt(tables, x = x, t = t, fractional = frac) - old(frac))), 1e-6)
  }
})
