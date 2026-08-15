library(testthat)
library(lifecontingencies)

data(demoIta)

# Source: Tecnica attuariale per le assicurazioni, pp. 14 and 21-23.
# The handout identifies the mortality basis as "Tavola di mortalità Italia maschi 2002".
# Note: p. 14 is a screenshot of the ISTAT source page and shows "Periodo dei dati: Anno 1998"
# and "Anno di edizione: 2002"; this distinction should be kept in mind when comparing
# the handout with demoIta$SIM02.
sim02 <- demoIta$SIM02
sim02 <- sim02[!is.na(sim02) & sim02 != 0]
sim02lt <- new("lifetable", x = seq(0, length(sim02) - 1), lx = sim02,
               name = "Italy males 2002")

context("Italian actuarial handout: demographic examples")

# Handout p. 21: Exercise 1.
test_that("Exercise 1: one-life survival and death probabilities", {
  expect_equal(round(pxt(sim02lt, x = 40, t = 1), 6), 0.998404)
  expect_equal(round(pxt(sim02lt, x = 40, t = 25), 6), 0.857098)
  expect_equal(round(1 - pxt(sim02lt, x = 40, t = 30), 6), 0.235609)
  expect_equal(round(pxt(sim02lt, x = 40, t = 30) *
                   qxt(sim02lt, x = 70, t = 1), 6), 0.038231)
})

# Handout p. 22: Exercise 2.
# The actuarial identities are tested explicitly as well as through pxyzt().
# This makes it possible to distinguish a multiple-life implementation issue from
# a difference between the handout mortality table and demoIta$SIM02.
test_that("Exercise 2: independent two-life survival probabilities", {
  p_joint <- pxt(sim02lt, x = 30, t = 15) *
    pxt(sim02lt, x = 36, t = 15)
  p_last <- 1 - qxt(sim02lt, x = 30, t = 40) *
    qxt(sim02lt, x = 36, t = 40)

  expect_equal(round(p_joint, 6), 0.945320)
  expect_equal(round(p_last, 6), 0.762276)

  expect_equal(
    round(pxyzt(list(sim02lt, sim02lt), x = c(30, 36), t = c(15, 15),
                status = "joint"), 6),
    round(p_joint, 6)
  )
  expect_equal(
    round(pxyzt(list(sim02lt, sim02lt), x = c(30, 36), t = c(40, 40),
                status = "last"), 6),
    round(p_last, 6)
  )
})
