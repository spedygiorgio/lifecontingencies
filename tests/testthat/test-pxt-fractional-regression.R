library(testthat)
library(lifecontingencies)

# AMLCR Exercise 3.2 (Dickson, Hardy & Waters, Actuarial Mathematics
# for Life Contingent Risks): published life-table extract and solutions.
# The table is used here as an independent numerical oracle for fractional
# survival probabilities; the expected values are not computed by pxt().

test_that("pxt fractional-age probabilities agree with published values", {
  x <- 52:60
  lx <- c(89948, 89089, 88176, 87208, 86181,
          85093, 83940, 82719, 81429)
  table <- new("lifetable", name = "AMLCRExercise3.2", x = x, lx = lx)

  # Published results (solutions to Exercise 3.2):
  # 0.2 q_52.4: 0.00191732 (UDD), 0.00191733 (constant force)
  # 5.7 p_52.4: 0.9354217  (UDD), 0.9354230  (constant force)
  # 3.2|2.5 q_52.4: 0.0309572 (UDD), 0.0309502 (constant force)
  expect_equal(
    pxt(table, x = 52.4, t = 0.2, fractional = "linear"),
    1 - 0.0019173165603473263,
    tolerance = 1e-10
  )
  expect_equal(
    pxt(table, x = 52.4, t = 0.2, fractional = "constant force"),
    1 - 0.0019173306705693172,
    tolerance = 1e-10
  )

  expect_equal(
    pxt(table, x = 52.4, t = 5.7, fractional = "linear"),
    0.935421698041614,
    tolerance = 1e-10
  )
  expect_equal(
    pxt(table, x = 52.4, t = 5.7, fractional = "constant force"),
    0.935423024785478,
    tolerance = 1e-10
  )

  # The deferred probability is included as an end-to-end check of
  # fractional starting age, fractional duration, and composition of
  # survival/death probabilities.
  expect_equal(
    pxt(table, x = 52.4, t = 3.2, fractional = "linear") *
      (1 - pxt(table, x = 55.6, t = 2.5, fractional = "linear")),
    0.030957185138230,
    tolerance = 1e-10
  )
  expect_equal(
    pxt(table, x = 52.4, t = 3.2, fractional = "constant force") *
      (1 - pxt(table, x = 55.6, t = 2.5, fractional = "constant force")),
    0.030950242839583,
    tolerance = 1e-10
  )
})

test_that("hyperbolic fractional-age probabilities are stable", {
  x <- 52:60
  lx <- c(89948, 89089, 88176, 87208, 86181,
          85093, 83940, 82719, 81429)
  table <- new("lifetable", name = "AMLCRExercise3.2", x = x, lx = lx)

  # Hyperbolic/Balducci values are independently calculated from the same
  # published life-table extract and the standard fractional-age formula.
  expect_equal(
    pxt(table, x = 52.4, t = 0.2, fractional = "hyperbolic"),
    0.998075302932531,
    tolerance = 1e-10
  )
  expect_equal(
    pxt(table, x = 52.4, t = 5.7, fractional = "hyperbolic"),
    0.935403622195132,
    tolerance = 1e-10
  )
  expect_equal(
    pxt(table, x = 52.4, t = 3.2, fractional = "hyperbolic") *
      (1 - pxt(table, x = 55.6, t = 2.5, fractional = "hyperbolic")),
    0.030974217743776,
    tolerance = 1e-10
  )
})
