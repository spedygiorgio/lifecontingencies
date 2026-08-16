library(testthat)
library(lifecontingencies)

# Source: ISTAT, Tavole di mortalità della popolazione italiana,
# Tavola 1.12, Maschi, Anno 1998. The table was published in 2002 and is
# the mortality basis shown in the handout "Tecnica attuariale per le
# assicurazioni", p. 14 and used for the calculations on p. 21.
#
# The handout calls it "Tavola di mortalità Italia maschi 2002", but the
# source page reports Periodo dei dati: Anno 1998 and Anno di edizione: 2002.
# We therefore reproduce the source table explicitly rather than using
# demoIta$SIM02, whose values do not match the handout.
#
# lx is extracted from the published ISTAT table (initial radix 100,000).
# Keeping the source values here makes the benchmark independent of any
# other mortality table shipped with the package.
italy1998_male_lx <- c(
  100000, 99403, 99369, 99340, 99316, 99296, 99278, 99261, 99246, 99230,
  99215, 99199, 99182, 99162, 99137, 99104, 99060, 99004, 98934, 98853,
  98764, 98669, 98572, 98472, 98370, 98271, 98174, 98077, 97981, 97881,
  97776, 97665, 97548, 97423, 97295, 97164, 97031, 96896, 96757, 96615,
  96465, 96311, 96148, 95976, 95793, 95592, 95364, 95110, 94826, 94514,
  94175, 93821, 93436, 93009, 92523, 91985, 91372, 90707, 89988, 89210,
  88369, 87448, 86428, 85297, 84045, 82680, 81171, 79525, 77753, 75822,
  73737, 71485, 69051, 66484, 63813, 60993, 58044, 55004, 51934, 48768,
  45403, 41715, 37831, 33942, 30212, 26734, 23394, 20198, 17146, 14330,
  11765, 9475, 7465, 5765, 4362, 3223, 2311, 1593, 1060, 678,
  417, 245, 137, 72, 36, 17, 7, 3, 1, 0,
  0, 0, 0, 0, 0
)

italy1998_male <- new(
  "lifetable",
  x = seq_along(italy1998_male_lx) - 1,
  lx = italy1998_male_lx,
  name = "Italy males 1998 (ISTAT, published 2002)"
)

context("Italian actuarial handout: demographic examples")

# Handout p. 21: Exercise 1.
test_that("Exercise 1: one-life survival and death probabilities", {
  # p_40 = l_41 / l_40
  expect_equal(round(pxt(italy1998_male, x = 40, t = 1), 6), 0.998404)

  # _25p_40 = l_65 / l_40
  expect_equal(round(pxt(italy1998_male, x = 40, t = 25), 6), 0.857098)

  # _30q_40 = (l_40 - l_70) / l_40
  expect_equal(round(1 - pxt(italy1998_male, x = 40, t = 30), 6), 0.235609)

  # Probability of surviving from age 40 to 80 and then dying between
  # exact ages 80 and 81: _40p_40 * q_80 = d_80 / l_40.
  expect_equal(
    round(
      pxt(italy1998_male, x = 40, t = 40) *
        qxt(italy1998_male, x = 80, t = 1),
      6
    ),
    0.038231
  )
})

# The source values used in Exercise 2 are also present in the same ISTAT
# table (l30=97776, l36=97031, l45=95592, l51=93821), so this fixture can be
# reused when the remaining demographic exercises are added.
