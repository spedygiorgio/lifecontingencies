library(testthat)
library(lifecontingencies)

data(demoIta)

# The handout uses the 2002 Italian male mortality table throughout the
# worked examples in this section.
sim02 <- demoIta$SIM02
sim02 <- sim02[!is.na(sim02) & sim02 != 0]
sim02lt <- new("lifetable", x = seq(0, length(sim02) - 1), lx = sim02,
               name = "Italy males 2002")

context("Cere-Spelta / Italian actuarial handout: demographic examples")

test_that("Exercise 1: one-life survival and death probabilities", {
  expect_equal(round(pxt(sim02lt, x = 40, t = 1), 6), 0.998404)
  expect_equal(round(pxt(sim02lt, x = 40, t = 25), 6), 0.857098)
  expect_equal(round(1 - pxt(sim02lt, x = 40, t = 30), 6), 0.235609)
  expect_equal(round(pxt(sim02lt, x = 40, t = 30) *
                   qxt(sim02lt, x = 70, t = 1), 6), 0.038231)
})

test_that("Exercise 2: independent two-life survival probabilities", {
  expect_equal(
    round(pxyzt(list(sim02lt, sim02lt), x = c(30, 36), t = c(15, 15),
                status = "joint"), 6),
    0.945320
  )
  expect_equal(
    round(pxyzt(list(sim02lt, sim02lt), x = c(30, 36), t = c(40, 40),
                status = "last"), 6),
    0.762276
  )
})
