# ============================================================
# VERIFIED FRACTIONAL-AGE REGRESSION TESTS FOR pxt() / qxt()
#
# This file intentionally contains only examples that have been
# independently verified locally.
#
# Included:
#   1. DHWS _0.4 q_40.2
#   2. DHWS _0.7 q_70.6, constant force
#   3. DHWS _0.7 q_70.6, UDD
#   4. PFA92C20 _3 p_62.5
#   5. TD 88-90 _0.5 p_90
#   6. pxt() + qxt() = 1 using a complete synthetic table
#
# Deliberately excluded:
#   _1.75 p_26.5
#
# Reason:
#   The numerical values previously used for that example were
#   not backed by a sufficiently verified reconstruction of the
#   underlying published table. It must NOT become a golden test
#   until the source table is recovered and independently checked.
#
# No public API or tolerance is changed by these tests.
# ============================================================

library(testthat)
library(lifecontingencies)


# ============================================================
# TEST 1
#
# DHWS fractional-age example:
#
#   p40 = 0.999473
#
# Calculate:
#
#   _0.4 q_40.2
#
# under UDD and constant force.
# ============================================================

p40 <- 0.999473

l40 <- 100000
l41 <- l40 * p40

table40 <- new(
  "lifetable",
  name = "DHWS_Example_3_2_3_6",
  x = 40:41,
  lx = c(l40, l41)
)

# UDD:
#
# _0.4 p_40.2 =
#   _0.6 p_40 / _0.2 p_40
#
# where:
#
# _u p_40 = 1 - u*q40.

expected40_udd <- 1 -
  (1 - 0.6 * (1 - p40)) /
  (1 - 0.2 * (1 - p40))

# Constant force:
#
# _0.4 p_40.2 = p40^0.4.

expected40_cf <- 1 - p40^0.4

actual40 <- c(
  linear = qxt(
    table40,
    x = 40.2,
    t = 0.4,
    fractional = "linear"
  ),
  `constant force` = qxt(
    table40,
    x = 40.2,
    t = 0.4,
    fractional = "constant force"
  )
)

test_that("DHWS _0.4 q_40.2 is stable", {

  expect_equal(
    actual40,
    c(
      linear = expected40_udd,
      `constant force` = expected40_cf
    ),
    tolerance = 1e-12
  )

  # Published rounded magnitude.
  expect_lt(
    abs(unname(actual40["linear"]) - 2.108e-4),
    5e-7
  )
})


# ============================================================
# TEST 2
#
# DHWS example:
#
#   q70 = 0.010413
#   q71 = 0.011670
#
# Calculate:
#
#   _0.7 q_70.6
#
# under constant force.
# ============================================================

q70 <- 0.010413
q71 <- 0.011670

l70 <- 100000
l71 <- l70 * (1 - q70)
l72 <- l71 * (1 - q71)

table70 <- new(
  "lifetable",
  name = "DHWS_Example_3_7",
  x = 70:72,
  lx = c(l70, l71, l72)
)

expected70_cf <-
  1 -
  (1 - q70)^0.4 *
  (1 - q71)^0.3

actual70_cf <- qxt(
  table70,
  x = 70.6,
  t = 0.7,
  fractional = "constant force"
)

test_that("DHWS _0.7 q_70.6 constant force is stable", {

  expect_equal(
    actual70_cf,
    expected70_cf,
    tolerance = 1e-12
  )

  # Published rounded answer: 0.007679.
  expect_lt(
    abs(actual70_cf - 0.007679),
    5e-6
  )
})


# ============================================================
# TEST 3
#
# Same DHWS example, under UDD.
#
# Important decomposition:
#
#   _0.7 p_70.6
#     =
#   _0.4 p_70.6 * _0.3 p_71
#
# Under UDD:
#
#   _0.4 p_70.6
#     =
#   _0.6 p_70 / _0.4 p_70
#     =
#   (1-q70)/(1-0.6*q70)
#
# and:
#
#   _0.3 p_71 = 1 - 0.3*q71.
# ============================================================

expected70_udd <-
  1 -
  ((1 - q70) / (1 - 0.6 * q70)) *
  (1 - 0.3 * q71)

actual70_udd <- qxt(
  table70,
  x = 70.6,
  t = 0.7,
  fractional = "linear"
)

test_that("DHWS _0.7 q_70.6 UDD is stable", {

  expect_equal(
    actual70_udd,
    expected70_udd,
    tolerance = 1e-12
  )
})


# ============================================================
# TEST 4
#
# PFA92C20:
#
#   _3 p_62.5
#
# Published data used:
#
#   l63 = 9775.888
#   l65 = 9703.708
#   q62 = 0.002885
#   q65 = 0.004681
#
# Since:
#
#   62.5 + 3 = 65.5
#
# l66 is required for the final fractional interval.
# ============================================================

q62 <- 0.002885
q65 <- 0.004681

l63 <- 9775.888
l65 <- 9703.708

l62 <- l63 / (1 - q62)
l64 <- l65 / (1 - q65)

# Required for the interval 65 -> 65.5.
l66 <- l65 * (1 - q65)

table_pfa <- new(
  "lifetable",
  name = "PFA92C20",
  x = 62:66,
  lx = c(
    l62,
    l63,
    l64,
    l65,
    l66
  )
)

# Verify that the reconstructed integer-age probabilities are
# internally consistent with the published q values.

expect_equal(
  l63 / l62,
  1 - q62,
  tolerance = 1e-12
)

expect_equal(
  l65 / l64,
  1 - q65,
  tolerance = 1e-12
)

expect_equal(
  l66 / l65,
  1 - q65,
  tolerance = 1e-12
)

# Independent UDD calculation:
#
# _3 p_62.5 =
#   _0.5 p_62.5 * _2 p_63 * _0.5 p_65
#
# with:
#
# _0.5 p_62.5 =
#   (1-q62)/(1-0.5*q62)
#
# and:
#
# _0.5 p_65 = 1 - 0.5*q65.

expected_pfa_linear <-
  ((1 - q62) /
    (1 - 0.5 * q62)) *
  (l65 / l63) *
  (1 - 0.5 * q65)

# Independent constant-force calculation.

expected_pfa_cf <-
  (1 - q62)^0.5 *
  (l65 / l63) *
  (1 - q65)^0.5

actual_pfa <- c(
  linear = pxt(
    table_pfa,
    x = 62.5,
    t = 3,
    fractional = "linear"
  ),
  `constant force` = pxt(
    table_pfa,
    x = 62.5,
    t = 3,
    fractional = "constant force"
  )
)

test_that("PFA92C20 _3 p_62.5 is stable", {

  expect_equal(
    actual_pfa,
    c(
      linear = expected_pfa_linear,
      `constant force` = expected_pfa_cf
    ),
    tolerance = 1e-10
  )
})


# ============================================================
# TEST 5
#
# TD 88-90:
#
#   _0.5 p_90
#
# Published values used in the locally verified reference.
#
# The three assumptions give distinct values.
# ============================================================

# The table is reconstructed from the verified TD 88-90
# mortality data used in the previous local validation.

q90 <- 0.2198892

l90 <- 100000
l91 <- l90 * (1 - q90)

table_td8890 <- new(
  "lifetable",
  name = "TD8890",
  x = 90:91,
  lx = c(l90, l91)
)

# Independent formulas:
#
# UDD:
#   _0.5 p_90 = 1 - 0.5*q90
#
# Constant force:
#   _0.5 p_90 = (1-q90)^0.5
#
# Balducci:
#   _0.5 p_90 = (1-q90)/(1-0.5*q90)

expected_td8890 <- c(
  linear =
    1 - 0.5 * q90,

  `constant force` =
    (1 - q90)^0.5,

  hyperbolic =
    (1 - q90) /
    (1 - 0.5 * q90)
)

actual_td8890 <- c(
  linear = pxt(
    table_td8890,
    x = 90,
    t = 0.5,
    fractional = "linear"
  ),
  `constant force` = pxt(
    table_td8890,
    x = 90,
    t = 0.5,
    fractional = "constant force"
  ),
  hyperbolic = pxt(
    table_td8890,
    x = 90,
    t = 0.5,
    fractional = "hyperbolic"
  )
)

test_that("TD88-90 _0.5 p_90 is stable", {

  expect_equal(
    actual_td8890,
    expected_td8890,
    tolerance = 1e-10
  )
})


# ============================================================
# TEST 6
#
# Structural identity:
#
#     _t p_x + _t q_x = 1
#
# Use a complete, explicit synthetic life table.
#
# This is deliberately independent from the published examples.
# ============================================================

# ============================================================
# TEST 6
#
# Structural identity:
#
#     _t p_x + _t q_x = 1
#
# Complete synthetic table covering every test interval.
# ============================================================

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
    x = c(
      40.2,
      70.6,
      26.5,
      62.5
    ),
    t = c(
      0.4,
      0.7,
      1.75,
      3
    )
  )
  
  methods <- c(
    "linear",
    "constant force",
    "hyperbolic"
  )
  
  for (i in seq_len(nrow(cases))) {
    
    for (method in methods) {
      
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
          cases$x[i],
          cases$t[i],
          method
        )
      )
      
      expect_false(
        is.nan(q),
        info = sprintf(
          "qxt() returned NaN: x=%.2f, t=%.2f, method=%s",
          cases$x[i],
          cases$t[i],
          method
        )
      )
      
      expect_equal(
        p + q,
        1,
        tolerance = 1e-12
      )
    }
  }
})

cat("\n")
cat("============================================================\n")
cat("VERIFIED FRACTIONAL-AGE REGRESSION TESTS COMPLETED\n")
cat("============================================================\n")
