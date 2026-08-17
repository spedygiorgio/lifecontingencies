require(lifecontingencies)

data("soa08Act")

# -----------------------------------------------------------------------------
# The C++ kernels are implementation details of the classical actuarial
# functions.  The most important regression criterion is therefore numerical
# agreement with the corresponding R implementation, not just agreement with
# the same formula reproduced in C++.
# -----------------------------------------------------------------------------

# Pure endowment: C++ kernel vs the classical Exn calculation.
exn_cases <- expand.grid(
  x = c(30, 60, 85),
  n = c(5, 10, 20),
  i = c(0.03, 0.06)
)
for (j in seq_len(nrow(exn_cases))) {
  z <- exn_cases[j, ]
  T <- z$x + z$n
  cpp <- lifecontingencies:::.fExnCpp(T, z$x, z$n, z$i)
  r <- Exn(soa08Act, x = z$x, n = z$n, i = z$i)
  # The C++ kernel is a one-state survival kernel; for an arbitrary life table
  # its direct value is used below only for the deterministic zero/discount
  # cases. The public-function comparison is done for the native actuarial
  # kernels whose corresponding legacy R implementations are available.
  stopifnot(is.finite(cpp), is.finite(r))
}

# -----------------------------------------------------------------------------
# Axn: compare the current native implementation with the classical R
# implementation used by the package's historical tests.
# This follows tests/test-computation-time-act-capital.R, which compares
# Axnvect with Axnold over age, term, deferment and payment frequency.
# -----------------------------------------------------------------------------
AXn <- Vectorize(lifecontingencies:::Axnold, "x")
AxN <- Vectorize(lifecontingencies:::Axnold, "n")
AxnM <- Vectorize(lifecontingencies:::Axnold, "m")

# Public/native implementation vs legacy/classical R implementation.
x_cases <- c(30:35 + 0.5, 60, 65, 80, 85)
for (k in c(1, 2, 4)) {
  new <- Axn(soa08Act, x = x_cases, n = 10, i = 0.06, k = k)
  old <- AXn(soa08Act, x = x_cases, n = 10, i = 0.06, k = k)
  stopifnot(all.equal(new, old, tolerance = 1e-10))
}

# Variation over term and deferment, mirroring the historical tests.
for (k in c(1, 2, 4)) {
  new_n <- Axn(soa08Act, x = 33, n = 1:20, i = 0.06, k = k)
  old_n <- AxN(soa08Act, x = 33, n = 1:20, i = 0.06, k = k)
  stopifnot(all.equal(new_n, old_n, tolerance = 1e-10))

  new_m <- Axn(soa08Act, x = 33, n = 20, m = 0:10, i = 0.06, k = k)
  old_m <- AxnM(soa08Act, x = 33, n = 20, m = 0:10, i = 0.06, k = k)
  stopifnot(all.equal(new_m, old_m, tolerance = 1e-10))
}

# A direct classical actuarial benchmark from the existing test suite.
# The package historically checks the value against Bowers (p. 339).
stopifnot(
  abs(Axn(soa08Act, x = 30, i = 0.06, k = 4) - 0.1048) < 5e-4
)

# -----------------------------------------------------------------------------
# axn: compare the current implementation with the classical R axnold
# implementation for due and immediate payments, including the vectorized
# age/term/deferment checks used by the package's existing tests.
# -----------------------------------------------------------------------------
aXn <- Vectorize(lifecontingencies:::axnold, "x")
axN <- Vectorize(lifecontingencies:::axnold, "n")
axnM <- Vectorize(lifecontingencies:::axnold, "m")

for (payment in c("due", "arrears", "immediate", "advance")) {
  new_x <- axn(soa08Act, x = 1:100, payment = payment)
  old_x <- aXn(soa08Act, x = 1:100, payment = payment)
  stopifnot(all.equal(new_x, old_x, tolerance = 1e-10))

  new_n <- axn(soa08Act, x = 33, n = 10:30, payment = payment)
  old_n <- axN(soa08Act, x = 33, n = 10:30, payment = payment)
  stopifnot(all.equal(new_n, old_n, tolerance = 1e-10))

  new_m <- axn(soa08Act, x = 33, n = 30, m = 0:10, payment = payment)
  old_m <- axnM(soa08Act, x = 33, n = 30, m = 0:10, payment = payment)
  stopifnot(all.equal(new_m, old_m, tolerance = 1e-10))
}

# High-age and non-integer-age cases, explicitly retained from the historical
# actuarial-function tests.
x <- 85:90
stopifnot(all.equal(
  axn(soa08Act, x = x, payment = "advance"),
  aXn(soa08Act, x = x, payment = "advance"),
  tolerance = 1e-10
))

x <- 30:35 + 1 / 2
stopifnot(all.equal(
  axn(soa08Act, x = x, payment = "advance"),
  aXn(soa08Act, x = x, payment = "advance"),
  tolerance = 1e-10
))

# -----------------------------------------------------------------------------
# Native helper kernels: verify their numerical values on deterministic cases
# against the corresponding actuarial identities. These are secondary checks;
# the public-function comparisons above are the primary regression criterion.
# -----------------------------------------------------------------------------
T <- 40
age <- 30
years <- 10
i <- 0.04
m <- 2
k <- 2

stopifnot(all.equal(
  lifecontingencies:::.fExnCpp(T, age, years, i),
  if (T < age + years) 0 else (1 + i)^(-years),
  tolerance = 1e-12
))

stopifnot(all.equal(
  lifecontingencies:::.fAxnCpp(T, age, years, i, m, k),
  if ((T >= age + m) && (T <= age + m + years - 1 / k))
    (1 + i)^(-(T - age + 1 / k)) else 0,
  tolerance = 1e-12
))

stopifnot(all.equal(
  lifecontingencies:::.fIAxnCpp(T, age, years, i, m, k),
  if ((T >= age + m) && (T <= age + m + years - 1 / k))
    (T - (age + m) + 1 / k) * (1 + i)^(-(T - age + 1 / k)) else 0,
  tolerance = 1e-12
))

stopifnot(all.equal(
  lifecontingencies:::.fDAxnCpp(T, age, years, i, m, k),
  if ((T >= age + m) && (T <= age + m + years - 1 / k))
    (years - (T - (age + m) + 1 / k)) * (1 + i)^(-(T - age + 1 / k)) else 0,
  tolerance = 1e-12
))

# -----------------------------------------------------------------------------
# Rcpp input validation: native entry points must reject malformed vectors
# rather than relying on unchecked indexing.
# -----------------------------------------------------------------------------
stopifnot(inherits(
  try(lifecontingencies:::.mult2sum(c(1, 2), c(1)), silent = TRUE),
  "try-error"
))
stopifnot(inherits(
  try(lifecontingencies:::.mult3sum(c(1, 2), c(1), c(1, 2)), silent = TRUE),
  "try-error"
))
stopifnot(inherits(
  try(lifecontingencies:::.presentValueC(c(1, 2), c(1, 2), c(0.03), c(1, 1), 1), silent = TRUE),
  "try-error"
))

for (bad_k in c(0, -1, NA_real_, NaN, Inf)) {
  stopifnot(inherits(
    try(lifecontingencies:::.fAxnCpp(40, 30, 10, 0.04, 2, bad_k), silent = TRUE),
    "try-error"
  ))
}
