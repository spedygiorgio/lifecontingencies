library(testthat)
library(lifecontingencies)

data(demoIta)

# Source: Tecnica attuariale per le assicurazioni, pp. 14 and 21.
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

# Handout p. 22: Exercise 2 is intentionally not included as a numerical
# regression benchmark. The source reports l_x values that cannot be reconciled
# with the available ISTAT 1998 table or with demoIta$SIM02. In particular,
# the handout solution uses l30=97776, l36=97031, l45=95592 and l51=93821.
# Keeping this exercise as a test would therefore make the benchmark depend on
# an unreconciled mortality basis rather than on an independently reproducible
# input table.
