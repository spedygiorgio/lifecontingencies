library(testthat)
library(lifecontingencies)

# Independent regression tests for fractional-age probabilities.
#
# Numerical oracles are derived from published actuarial examples,
# using UDD (the package's "linear" interpolation), constant force,
# and Balducci (the package's "hyperbolic" interpolation).
#
# References:
#   - Dickson, Hardy & Waters, Actuarial Mathematics for Life
#     Contingent Risks, fractional-age examples.
#   - Finan, actuarial mathematics Example 24.14, q26 = 0.0213,
#     q27 = 0.0232, q28 = 0.0254.
#   - PFA92C20 published mortality-table example.
#   - Charpentier & Dutang, Actuariat avec R, Section 4.5.1,
#     example based on the French TD 88-90 mortality table.
#
# The tests intentionally keep the public pxt() API untouched and
# derive the expected values independently of pxt().

# ============================================================
# Test 1: published fractional-age example
#
# _1.75 p_26.5
# ============================================================

test_that("published fractional-age example agrees with all assumptions", {
  q26 <- 0.0213
  q27 <- 0.0232
  q28 <- 0.0254

  # For x = 26.5 and t = 1.75, pxt() decomposes the probability as
  #
  #   _1.75 p_26.5 = _2 p_26 * _0.25 p_28 / _0.5 p_26.
  #
  # This ratio construction is important when the starting age is
  # fractional; simply multiplying _0.5 p_26.5 by p27 and _0.25 p28
  # is not generally the same expression under an interpolation
  # assumption.
  expected <- c(
    linear =
      ((1 - q26) * (1 - q27) * (1 - 0.25 * q28)) /
      (1 - 0.5 * q26),
    `constant force` =
      ((1 - q26) * (1 - q27) * (1 - q28)^0.25) /
      (1 - q26)^0.5,
    hyperbolic =
      ((1 - q26) * (1 - q27) *
        ((1 - q28) / (1 - 0.75 * q28))) /
      ((1 - q26) / (1 - 0.5 * q26))
  )

  table <- new(
    "lifetable",
    name = "Finan_Example_24_14",
    x = 26:29,
    lx = 100000 * cumprod(c(1, 1 - q26, 1 - q27, 1 - q28))
  )

  actual <- c(
    linear = pxt(table, x = 26.5, t = 1.75, fractional = "linear"),
    `constant force` = pxt(
      table, x = 26.5, t = 1.75, fractional = "constant force"
    ),
    hyperbolic = pxt(
      table, x = 26.5, t = 1.75, fractional = "hyperbolic"
    )
  )

  expect_equal(actual, expected, tolerance = 1e-10)

  # Published rounded values from the actuarial example.
  expect_equal(
    actual,
    c(
      linear = 0.9601492,
      `constant force` = 0.9601455,
      hyperbolic = 0.9601413
    ),
    tolerance = 5e-7
  )
})

# ============================================================
# Test 2: isolate the fractional endpoints of the same example
# ============================================================

test_that("fractional starting and ending age components are stable", {
  q26 <- 0.0213
  q28 <- 0.0254

  table <- new(
    "lifetable",
    name = "Finan_Example_24_14",
    x = 26:29,
    lx = 100000 * cumprod(c(1, 1 - q26, 1 - 0.0232, 1 - q28))
  )

  # Fractional starting age: _0.5 p_26.5.
  expected_start <- c(
    linear = (1 - q26) / (1 - 0.5 * q26),
    `constant force` = (1 - q26)^0.5,
    hyperbolic = 1 - 0.5 * q26
  )

  actual_start <- c(
    linear = pxt(table, 26.5, 0.5, fractional = "linear"),
    `constant force` = pxt(
      table, 26.5, 0.5, fractional = "constant force"
    ),
    hyperbolic = pxt(
      table, 26.5, 0.5, fractional = "hyperbolic"
    )
  )

  expect_equal(actual_start, expected_start, tolerance = 1e-10)

  # Fractional ending age: _0.25 p_28.
  expected_end <- c(
    linear = 1 - 0.25 * q28,
    `constant force` = (1 - q28)^0.25,
    hyperbolic = (1 - q28) / (1 - 0.75 * q28)
  )

  actual_end <- c(
    linear = pxt(table, 28, 0.25, fractional = "linear"),
    `constant force` = pxt(
      table, 28, 0.25, fractional = "constant force"
    ),
    hyperbolic = pxt(
      table, 28, 0.25, fractional = "hyperbolic"
    )
  )

  expect_equal(actual_end, expected_end, tolerance = 1e-10)
})

# ============================================================
# Test 3: PFA92C20 published-data example
#
# _3 p_62.5
# ============================================================

test_that("PFA92C20 fractional-age example agrees with published data", {
  q62 <- 0.002885
  q65 <- 0.004681
  l63 <- 9775.888
  l65 <- 9703.708

  # Reconstruct the adjacent life-table entries from the published
  # q-values.  l66 is required because 62.5 + 3 = 65.5, so the final
  # fractional interval is the 65-to-66 interval.
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

  # The source table is published to limited numerical precision;
  # 1e-6 is therefore more appropriate than machine precision.
  expect_equal(actual, expected, tolerance = 1e-6)

  expect_equal(
    actual,
    c(
      linear = 0.9888627180661481,
      `constant force` = 0.9888611485707346
    ),
    tolerance = 1e-6
  )
})

# ============================================================
# Test 4: Dickson, Hardy & Waters examples
#
# _0.4 q_40.2 and _0.7 q_70.6
# ============================================================

test_that("published fractional-age qxt examples are stable", {
  # Example with p40 = 0.999473.
  p40 <- 0.999473
  table40 <- new(
    "lifetable",
    name = "DHW_Example",
    x = 40:41,
    lx = c(100000, 100000 * p40)
  )

  expected40 <- c(
    linear =
      1 - (1 - 0.6 * (1 - p40)) /
      (1 - 0.2 * (1 - p40)),
    `constant force` = 1 - p40^0.4
  )

  actual40 <- c(
    linear = qxt(table40, 40.2, 0.4, fractional = "linear"),
    `constant force` = qxt(
      table40, 40.2, 0.4, fractional = "constant force"
    )
  )

  expect_equal(actual40, expected40, tolerance = 1e-10)
  expect_equal(actual40, c(linear = 0.0002108222,
                            `constant force` = 0.0002108333),
               tolerance = 5e-10)

  # Example with q70 = 0.010413 and q71 = 0.011670.
  q70 <- 0.010413
  q71 <- 0.011670
  l70 <- 100000
  l71 <- l70 * (1 - q70)
  l72 <- l71 * (1 - q71)

  table70 <- new(
    "lifetable",
    name = "DHW_Example",
    x = 70:72,
    lx = c(l70, l71, l72)
  )

  expected70_cf <- 1 - (1 - q70)^0.4 * (1 - q71)^0.3
  expected70_udd <-
    1 - ((1 - q70) / (1 - 0.6 * q70)) *
    (1 - 0.3 * q71)

  actual70 <- c(
    `constant force` = qxt(
      table70, 70.6, 0.7, fractional = "constant force"
    ),
    linear = qxt(table70, 70.6, 0.7, fractional = "linear")
  )

  expect_equal(actual70["constant force"], expected70_cf, tolerance = 1e-10)
  expect_equal(actual70["linear"], expected70_udd, tolerance = 1e-10)
})

# ============================================================
# Test 5: TD 88-90 / TD8890 published example
#
# _0.5 p_90
# ============================================================

test_that("TD8890 published fractional-age example is reproduced", {
  # Official French TD 88-90 table, Arrêté du 27 avril 1993,
  # Annexe (Table de mortalité TD 88-90), Légifrance.
  #
  # For the published Charpentier & Dutang example at age 90 only
  # l90 and l91 are required.  The official table gives:
  #
  #   l90 = 9389
  #   l91 = 7438
  #
  # Charpentier & Dutang, Actuariat avec R, Section 4.5.1,
  # report for _0.5 p_90:
  #
  #   linear          0.8961018
  #   constant force  0.8900582
  #   hyperbolic      0.8840554
  #
  # The values below are independently reconstructed from l90/l91.
  td8890 <- new(
    "lifetable",
    name = "TD8890",
    x = 90:91,
    lx = c(9389, 7438)
  )

  p90 <- 7438 / 9389

  expected <- c(
    linear = 0.5 * p90 + 0.5,
    `constant force` = p90^0.5,
    hyperbolic = p90 / (1 - 0.5 * (1 - p90))
  )

  actual <- c(
    linear = pxt(td8890, 90, 0.5, fractional = "linear"),
    `constant force` = pxt(
      td8890, 90, 0.5, fractional = "constant force"
    ),
    hyperbolic = pxt(
      td8890, 90, 0.5, fractional = "hyperbolic"
    )
  )

  expect_equal(actual, expected, tolerance = 1e-12)

  expect_equal(
    actual,
    c(
      linear = 0.8961018,
      `constant force` = 0.8900582,
      hyperbolic = 0.8840554
    ),
    tolerance = 5e-8
  )
})

# ============================================================
# Test 6: complementarity of pxt() and qxt()
# ============================================================

test_that("pxt and qxt are complementary for fractional ages", {
  q26 <- 0.0213
  q27 <- 0.0232
  q28 <- 0.0254

  table <- new(
    "lifetable",
    name = "Finan_Example_24_14",
    x = 26:29,
    lx = 100000 * cumprod(c(1, 1 - q26, 1 - q27, 1 - q28))
  )

  for (method in c("linear", "constant force", "hyperbolic")) {
    p <- pxt(table, 26.5, 1.75, fractional = method)
    q <- qxt(table, 26.5, 1.75, fractional = method)
    expect_equal(p + q, 1, tolerance = 1e-12)
  }
})
