library(testthat)
library(lifecontingencies)

# ============================================================
# CORNER-CASE TESTS FOR pxt()
#
# 15 cases covering the first five corner-case categories:
#   1. integer x, integer t
#   2. fractional x, integer t
#   3. integer x, fractional t
#   4. fractional x, fractional t
#   5. t = 0
#
# The expected values are computed by an independent oracle;
# pxt() is never used to construct the expected value.
#
# "linear" = UDD
# "constant force" = constant force
# "hyperbolic" = Balducci
#
# LOCAL VALIDATION FILE:
# Do not add this file to the package test suite until all
# cases have been reviewed and confirmed.
# ============================================================

ages <- 40:92

# Synthetic but strictly positive mortality rates.
qx <- 0.001 +
  0.00005 * (ages - 40) +
  0.000001 * (ages - 40)^2

lx <- 100000 * c(1, cumprod(1 - qx[-length(qx)]))

corner_table <- new(
  "lifetable",
  name = "pxt_corner_case_table",
  x = ages,
  lx = lx
)

# ------------------------------------------------------------
# Fractional survival over a single integer-age interval.
# ------------------------------------------------------------

fractional_survival <- function(q, u, method) {
  stopifnot(u >= 0, u <= 1)

  if (u == 0)
    return(1)

  switch(
    method,
    linear = 1 - u * q,
    `constant force` = (1 - q)^u,
    hyperbolic = (1 - q) / (1 - (1 - u) * q),
    stop("Unknown fractional-age method.")
  )
}

# ------------------------------------------------------------
# Independent oracle for _t p_x.
#
# For x = k+s, s in [0,1):
#
#   _t p_x = _{s+t}p_k / _s p_k
#
# The numerator is split into complete integer years plus
# a final fractional interval. No call to pxt() is made here.
# ------------------------------------------------------------

reference_pxt <- function(table, x, t, method) {
  stopifnot(
    length(x) == 1L,
    length(t) == 1L,
    is.finite(x),
    is.finite(t),
    t >= 0
  )

  if (t == 0)
    return(1)

  if (x < min(table@x) || x + t > max(table@x))
    stop("interval outside table range")

  k <- floor(x)
  s <- x - k
  u <- s + t

  whole <- floor(u)
  frac <- u - whole

  numerator <- 1

  if (whole > 0) {
    numerator <- table@lx[match(k + whole, table@x)] /
      table@lx[match(k, table@x)]
  }

  if (frac > 0) {
    q <- 1 -
      table@lx[match(k + whole + 1, table@x)] /
      table@lx[match(k + whole, table@x)]

    numerator <- numerator *
      fractional_survival(q, frac, method)
  }

  denominator <- 1

  if (s > 0) {
    q0 <- 1 -
      table@lx[match(k + 1, table@x)] /
      table@lx[match(k, table@x)]

    denominator <- fractional_survival(q0, s, method)
  }

  numerator / denominator
}

methods <- c("linear", "constant force", "hyperbolic")

# ============================================================
# 1. INTEGER x, INTEGER t
#
# In this case all fractional-age assumptions must agree:
#
#   _t p_x = l_(x+t) / l_x
# ============================================================

test_that("corner cases: integer x and integer t", {

  cases <- data.frame(
    x = c(40, 40, 90),
    t = c(1, 5, 1)
  )

  for (i in seq_len(nrow(cases))) {
    x <- cases$x[i]
    t <- cases$t[i]

    expected <- corner_table@lx[match(x + t, corner_table@x)] /
      corner_table@lx[match(x, corner_table@x)]

    for (method in methods) {
      actual <- pxt(
        corner_table, x = x, t = t, fractional = method
      )

      cat(sprintf(
        "\nCASE 1.%d | x=%g t=%g | %s\nactual=%.15f expected=%.15f\n",
        i, x, t, method, actual, expected
      ))

      expect_equal(actual, expected, tolerance = 1e-12)
      expect_equal(
        actual,
        reference_pxt(corner_table, x, t, method),
        tolerance = 1e-12
      )
    }
  }
})

# ============================================================
# 2. FRACTIONAL x, INTEGER t
# ============================================================

test_that("corner cases: fractional x and integer t", {

  cases <- data.frame(
    x = c(40.5, 40.25, 90.5),
    t = c(1, 5, 1)
  )

  for (i in seq_len(nrow(cases))) {
    x <- cases$x[i]
    t <- cases$t[i]

    for (method in methods) {
      expected <- reference_pxt(
        corner_table, x, t, method
      )

      actual <- pxt(
        corner_table, x = x, t = t, fractional = method
      )

      cat(sprintf(
        "\nCASE 2.%d | x=%g t=%g | %s\nactual=%.15f expected=%.15f difference=%.3e\n",
        i, x, t, method, actual, expected, actual - expected
      ))

      expect_equal(actual, expected, tolerance = 1e-12)
    }
  }
})

# ============================================================
# 3. INTEGER x, FRACTIONAL t
# ============================================================

test_that("corner cases: integer x and fractional t", {

  cases <- data.frame(
    x = c(40, 40, 90),
    t = c(0.5, 0.25, 0.5)
  )

  for (i in seq_len(nrow(cases))) {
    x <- cases$x[i]
    t <- cases$t[i]

    for (method in methods) {
      expected <- reference_pxt(
        corner_table, x, t, method
      )

      actual <- pxt(
        corner_table, x = x, t = t, fractional = method
      )

      cat(sprintf(
        "\nCASE 3.%d | x=%g t=%g | %s\nactual=%.15f expected=%.15f difference=%.3e\n",
        i, x, t, method, actual, expected, actual - expected
      ))

      expect_equal(actual, expected, tolerance = 1e-12)
    }
  }
})

# ============================================================
# 4. FRACTIONAL x, FRACTIONAL t
#
# Particularly important:
#
#   40.50 -> 41 -> 42 -> 42.25
#
# and a case close to the upper part of the table.
# ============================================================

test_that("corner cases: fractional x and fractional t", {

  cases <- data.frame(
    x = c(40.25, 40.50, 89.50),
    t = c(0.50, 1.75, 0.75)
  )

  for (i in seq_len(nrow(cases))) {
    x <- cases$x[i]
    t <- cases$t[i]

    for (method in methods) {
      expected <- reference_pxt(
        corner_table, x, t, method
      )

      actual <- pxt(
        corner_table, x = x, t = t, fractional = method
      )

      cat(sprintf(
        "\nCASE 4.%d | x=%g t=%g | %s\nactual=%.15f expected=%.15f difference=%.3e\n",
        i, x, t, method, actual, expected, actual - expected
      ))

      expect_equal(actual, expected, tolerance = 1e-12)
    }
  }
})

# ============================================================
# 5. ZERO DURATION
#
# For every x and every fractional-age assumption:
#
#   _0 p_x = 1
# ============================================================

test_that("corner cases: t = 0", {

  x_values <- c(40, 40.5, 90.5)

  for (i in seq_along(x_values)) {
    x <- x_values[i]

    for (method in methods) {
      actual <- pxt(
        corner_table, x = x, t = 0, fractional = method
      )

      cat(sprintf(
        "\nCASE 5.%d | x=%g t=0 | %s\nactual=%.15f\n",
        i, x, method, actual
      ))

      expect_equal(actual, 1, tolerance = 1e-15)
    }
  }
})

cat("\n")
cat("============================================================\n")
cat("ALL 15 pxt() CORNER CASES PASSED\n")
cat("============================================================\n")
