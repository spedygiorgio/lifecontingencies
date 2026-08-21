library(testthat)
library(lifecontingencies)

# Verified fractional-age regression tests for pxt()/qxt().
#
# The numerical oracles below are independent of pxt()/qxt() and use
# published actuarial examples where the source data were independently
# verified locally.  The public pxt()/qxt() API is not changed.
#
# Deliberately excluded: the previously reconstructed _1.75 p_26.5 golden
# example.  Its underlying source table was not sufficiently verified for
# use as a regression oracle.

# ============================================================
# DHWS: _0.4 q_40.2
# ============================================================

test_that("DHWS _0.4 q_40.2 is stable", {
  p40 <- 0.999473
  table40 <- new(
    "lifetable",
    name = "DHWS_Example",
    x = 40:41,
    lx = c(100000, 100000 * p40)
  )

  expected <- c(
    linear =
      1 - (1 - 0.6 * (1 - p40)) /
      (1 - 0.2 * (1 - p40)),
    `constant force` = 1 - p40^0.4
  )

  actual <- c(
    linear = qxt(table40, 40.2, 0.4, fractional = "linear"),
    `constant force` = qxt(
      table40, 40.2, 0.4, fractional = "constant force"
    )
  )

  expect_equal(actual, expected, tolerance = 1e-10)
  expect_equal(
    actual,
    c(
      linear = 0.0002108222,
      `constant force` = 0.0002108333
    ),
    tolerance = 5e-10
  )
})

# ============================================================
# DHWS: _0.7 q_70.6
# ============================================================

test_that("DHWS _0.7 q_70.6 is stable", {
  q70 <- 0.010413
  q71 <- 0.011670
  l70 <- 100000
  l71 <- l70 * (1 - q70)
  l72 <- l71 * (1 - q71)

  table70 <- new(
    "lifetable",
    name = "DHWS_Example",
    x = 70:72,
    lx = c(l70, l71, l72)
  )

  expected_cf <- 1 - (1 - q70)^0.4 * (1 - q71)^0.3

  # Under UDD:
  # _0.7 p_70.6 = _0.4 p_70.6 * _0.3 p_71
  # _0.4 p_70.6 = (1-q70)/(1-0.6*q70)
  expected_udd <-
    1 - ((1 - q70) / (1 - 0.6 * q70)) *
    (1 - 0.3 * q71)

  actual <- c(
    `constant force` = qxt(
      table70, 70.6, 0.7, fractional = "constant force"
    ),
    linear = qxt(table70, 70.6, 0.7, fractional = "linear")
  )

  expected <- c(
    `constant force` = expected_cf,
    linear = expected_udd
  )

  # Compare the complete named vector so that the regression test also
  # checks the labels and cannot fail because of named-scalar structure.
  expect_equal(actual, expected, tolerance = 1e-10)
})

# ============================================================
# PFA92C20: _3 p_62.5
# ============================================================

test_that("PFA92C20 _3 p_62.5 is stable", {
  q62 <- 0.002885
  q65 <- 0.004681
  l63 <- 9775.888
  l65 <- 9703.708

  # 62.5 + 3 = 65.5, hence l66 is required for the last fractional
  # interval.
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

  # Published source values have limited precision.
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
# TD 88-90: _0.5 p_90
# ============================================================

test_that("TD8890 _0.5 p_90 is stable", {
  q90 <- 0.2198892

  table <- new(
    "lifetable",
    name = "TD8890",
    x = 90:91,
    lx = c(100000, 100000 * (1 - q90))
  )

  expected <- c(
    linear = 1 - 0.5 * q90,
    `constant force` = (1 - q90)^0.5,
    hyperbolic = (1 - q90) / (1 - 0.5 * q90)
  )

  actual <- c(
    linear = pxt(table, 90, 0.5, fractional = "linear"),
    `constant force` = pxt(
      table, 90, 0.5, fractional = "constant force"
    ),
    hyperbolic = pxt(
      table, 90, 0.5, fractional = "hyperbolic"
    )
  )

  expect_equal(actual, expected, tolerance = 1e-10)
})

# ============================================================
# Structural identity: pxt() + qxt() = 1
# ============================================================

# Use a complete explicit synthetic table.  It covers all requested
# intervals, including x = 26.5, so the test does not depend on the
# boundary of any published example table.
identity_ages <- 20:92
identity_lx <- 100000 * exp(
  -0.001 * (identity_ages - 40) -
  0.00001 * (identity_ages - 40)^2
)

identity_table <- new(
  "lifetable",
  name = "FractionalAgeIdentity",
  x = identity_ages,
  lx = identity_lx
)

test_that("pxt() + qxt() = 1 for fractional ages", {
  cases <- data.frame(
    x = c(40.2, 70.6, 26.5, 62.5),
    t = c(0.4, 0.7, 1.75, 3)
  )

  for (i in seq_len(nrow(cases))) {
    for (method in c("linear", "constant force", "hyperbolic")) {
      p <- pxt(
        identity_table,
        x = cases$x[i],
        t = cases$t[i],
        fractional = method
      )
      q <- qxt(
        identity_table,
        x = cases$x[i],
        t = cases$t[i],
        fractional = method
      )

      expect_false(
        is.nan(p),
        info = sprintf(
          "pxt() returned NaN: x=%.2f, t=%.2f, method=%s",
          cases$x[i], cases$t[i], method
        )
      )
      expect_false(
        is.nan(q),
        info = sprintf(
          "qxt() returned NaN: x=%.2f, t=%.2f, method=%s",
          cases$x[i], cases$t[i], method
        )
      )
      expect_equal(p + q, 1, tolerance = 1e-12)
    }
  }
})
