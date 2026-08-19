library(testthat)
library(lifecontingencies)

# Independent regression tests for fractional-age probabilities.
#
# The numerical oracles below are derived from published actuarial examples,
# using the standard UDD (linear), constant-force, and Balducci (hyperbolic)
# formulas independently of pxt().

# ============================================================
# Test 1: published example with fractional starting and ending ages
#
# q26 = 0.0213, q27 = 0.0232, q28 = 0.0254
# Target: _1.75 p_26.5
# ============================================================

test_that("published fractional-age example agrees with all assumptions", {
  q26 <- 0.0213
  q27 <- 0.0232
  q28 <- 0.0254

  expected <- c(
    linear =
      (1 - 0.5 * q26 / (1 - 0.5 * q26)) *
      (1 - q27) *
      (1 - 0.25 * q28),
    `constant force` =
      (1 - q26)^0.5 *
      (1 - q27) *
      (1 - q28)^0.25,
    hyperbolic =
      (1 - 0.5 * q26) *
      (1 - q27) *
      ((1 - q28) / (1 - 0.75 * q28))
  )

  table <- new(
    "lifetable",
    name = "PublishedFractionalAgeExample",
    x = 26:29,
    lx = 100000 * cumprod(c(1, 1 - q26, 1 - q27, 1 - q28))
  )

  actual <- c(
    linear = pxt(
      table, x = 26.5, t = 1.75, fractional = "linear"
    ),
    `constant force` = pxt(
      table, x = 26.5, t = 1.75, fractional = "constant force"
    ),
    hyperbolic = pxt(
      table, x = 26.5, t = 1.75, fractional = "hyperbolic"
    )
  )

  expect_equal(actual, expected, tolerance = 1e-10)

  expect_equal(
    actual,
    c(
      linear = 0.9601491859139839,
      `constant force` = 0.9601454912633326,
      hyperbolic = 0.9601412856598195
    ),
    tolerance = 1e-10
  )
})

# ============================================================
# Test 2: isolate the two fractional endpoints of the same example
# ============================================================

test_that("fractional starting and ending age components are stable", {
  q26 <- 0.0213
  q28 <- 0.0254

  table <- new(
    "lifetable",
    name = "PublishedFractionalAgeExample",
    x = 26:29,
    lx = 100000 * cumprod(c(1, 1 - q26, 1 - 0.0232, 1 - q28))
  )

  expected_start <- c(
    linear = 1 - 0.5 * q26 / (1 - 0.5 * q26),
    `constant force` = (1 - q26)^0.5,
    hyperbolic = 1 - 0.5 * q26
  )

  actual_start <- c(
    linear = pxt(table, 26.5, 0.5, fractional = "linear"),
    `constant force` = pxt(table, 26.5, 0.5, fractional = "constant force"),
    hyperbolic = pxt(table, 26.5, 0.5, fractional = "hyperbolic")
  )

  expect_equal(actual_start, expected_start, tolerance = 1e-10)

  expected_end <- c(
    linear = 1 - 0.25 * q28,
    `constant force` = (1 - q28)^0.25,
    hyperbolic = (1 - q28) / (1 - 0.75 * q28)
  )

  actual_end <- c(
    linear = pxt(table, 28, 0.25, fractional = "linear"),
    `constant force` = pxt(table, 28, 0.25, fractional = "constant force"),
    hyperbolic = pxt(table, 28, 0.25, fractional = "hyperbolic")
  )

  expect_equal(actual_end, expected_end, tolerance = 1e-10)
})

# ============================================================
# Test 3: PFA92C20 example
#
# Published values:
#   l63 = 9775.888
#   l65 = 9703.708
#   q62 = 0.002885
#   q65 = 0.004681
# Target: _3 p_62.5
# ============================================================

test_that("PFA92C20 fractional-age example agrees with published data", {
  q62 <- 0.002885
  q65 <- 0.004681
  l63 <- 9775.888
  l65 <- 9703.708

  l62 <- l63 / (1 - q62)
  l64 <- l65 / (1 - q65)
  l66 <- l65 * (1 - q65)

  table <- new(
    "lifetable",
    name = "PFA92C20",
    x = 62:66,
    lx = c(l62, l63, l64, l65, l66)
  )

  expected <- c(
    linear =
      ((1 - q62) / (1 - 0.5 * q62)) *
      (l65 / l63) *
      (1 - 0.5 * q65),
    `constant force` =
      (1 - q62)^0.5 *
      (l65 / l63) *
      (1 - q65)^0.5
  )

  actual <- c(
    linear = pxt(table, 62.5, 3, fractional = "linear"),
    `constant force` = pxt(
      table, 62.5, 3, fractional = "constant force"
    )
  )

  expect_equal(actual, expected, tolerance = 1e-10)

  expect_equal(
    actual,
    c(
      linear = 0.9888627180661481,
      `constant force` = 0.9888611485707346
    ),
    tolerance = 1e-10
  )
})
