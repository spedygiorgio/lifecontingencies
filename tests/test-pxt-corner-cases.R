require(lifecontingencies)

data("soa08Act")

# -----------------------------------------------------------------------------
# Corner-case regression tests for the native pxtCpp kernel.
#
# These tests deliberately exercise boundaries and vector-shape behaviour that
# are easy to get wrong in a native implementation.  The public pxt() function
# remains the reference for cases where its behaviour is well-defined.
# -----------------------------------------------------------------------------

lx <- soa08Act@lx
omega <- getOmega(soa08Act)

# Helper: compare the native kernel with the public implementation.
check_pxt <- function(x, t, fractional) {
  expected <- pxt(soa08Act, x = x, t = t, fractional = fractional)
  actual <- lifecontingencies:::.pxtCpp(
    x, t, lx, omega,
    switch(fractional,
           linear = 0L,
           `constant force` = 1L,
           hyperbolic = 2L)
  )
  stopifnot(all.equal(actual, expected, tolerance = 1e-12))
}

# -----------------------------------------------------------------------------
# t = 0: survival over a zero-length interval is exactly one for valid ages.
# Test integer and fractional starting ages under all assumptions.
# -----------------------------------------------------------------------------
for (fractional in c("linear", "constant force", "hyperbolic")) {
  check_pxt(c(0, 30, 40.5, omega - 1, omega), 0, fractional)
}

# -----------------------------------------------------------------------------
# Integer ages and exact integer durations should reduce to the ordinary
# life-table ratio, including the final attainable age.
# -----------------------------------------------------------------------------
for (fractional in c("linear", "constant force", "hyperbolic")) {
  check_pxt(c(0, 30, 60, omega - 1), c(1, 5, 10, 1), fractional)
}

# -----------------------------------------------------------------------------
# Fractional starting ages: exercise eps_x = 0 and non-zero eps_x, including
# a transition to the final attainable age.
# -----------------------------------------------------------------------------
for (fractional in c("linear", "constant force", "hyperbolic")) {
  check_pxt(
    c(30, 30.5, 60.25, omega - 1.5),
    c(1, 1, 2, 1),
    fractional
  )
}

# -----------------------------------------------------------------------------
# Crossing the terminal age: probabilities beyond omega + 1 must be zero.
# This includes both integer and fractional durations.
# -----------------------------------------------------------------------------
for (fractional in c("linear", "constant force", "hyperbolic")) {
  check_pxt(
    c(omega - 1, omega - 1, omega - 0.5),
    c(2, 2.5, 1),
    fractional
  )
}

# -----------------------------------------------------------------------------
# Recycling: x and t of different lengths must produce the same result as
# explicitly recycled vectors.  This is especially important for the C++
# implementation because it performs recycling inside the native loop.
# -----------------------------------------------------------------------------
for (fractional in c("linear", "constant force", "hyperbolic")) {
  method <- switch(fractional,
                   linear = 0L,
                   `constant force` = 1L,
                   hyperbolic = 2L)

  x <- c(30.5, 45.25, 70.75)
  t <- c(1.5, 2.25)

  actual <- lifecontingencies:::.pxtCpp(x, t, lx, omega, method)
  expected <- lifecontingencies:::.pxtCpp(
    rep(x, length.out = 3),
    rep(t, length.out = 3),
    lx, omega, method
  )
  stopifnot(all.equal(actual, expected, tolerance = 1e-12))

  check_pxt(x, t, fractional)
}

# -----------------------------------------------------------------------------
# Scalar/vector recycling in the other direction.
# -----------------------------------------------------------------------------
for (fractional in c("linear", "constant force", "hyperbolic")) {
  check_pxt(c(30.5, 45.25, 70.75), 2.5, fractional)
  check_pxt(40.5, c(0.5, 1, 2.5, 5), fractional)
}

# -----------------------------------------------------------------------------
# Empty native inputs: the native kernel returns an empty numeric vector when
# either input is empty.  The public wrapper intentionally rejects length-zero
# inputs, so this checks the native API directly.
# -----------------------------------------------------------------------------
for (fractional_method in 0:2) {
  stopifnot(identical(
    lifecontingencies:::.pxtCpp(numeric(0), 1, lx, omega, fractional_method),
    numeric(0)
  ))
  stopifnot(identical(
    lifecontingencies:::.pxtCpp(30, numeric(0), lx, omega, fractional_method),
    numeric(0)
  ))
}

# -----------------------------------------------------------------------------
# Invalid negative ages/durations must fail rather than silently indexing the
# life table with an invalid age.
# -----------------------------------------------------------------------------
for (fractional_method in 0:2) {
  stopifnot(inherits(
    try(
      lifecontingencies:::.pxtCpp(-0.5, 1, lx, omega, fractional_method),
      silent = TRUE
    ),
    "try-error"
  ))

  stopifnot(inherits(
    try(
      lifecontingencies:::.pxtCpp(30, -1, lx, omega, fractional_method),
      silent = TRUE
    ),
    "try-error"
  ))
}

# -----------------------------------------------------------------------------
# Exact boundary at omega + 1: starting below the terminal age and surviving
# exactly to omega + 1 must be zero, matching the closed life-table convention.
# -----------------------------------------------------------------------------
for (fractional in c("linear", "constant force", "hyperbolic")) {
  check_pxt(omega - 1, 2, fractional)
}
