require(lifecontingencies)

# Regression tests for the native Rcpp kernels.
# These tests intentionally exercise valid inputs only so that the numerical
# behaviour can be compared before and after the hardening changes.

# -----------------------------------------------------------------------------
# Vector kernels
# -----------------------------------------------------------------------------
x <- c(1, 2, 3)
y <- c(4, 0.5, -1)
z <- c(0.5, 2, 3)

stopifnot(all.equal(lifecontingencies:::.mult2sum(x, y), sum(x * y)))
stopifnot(all.equal(lifecontingencies:::.mult3sum(x, y, z), sum(x * y * z)))

# -----------------------------------------------------------------------------
# Present value kernel
# -----------------------------------------------------------------------------
cf <- c(100, 50, 25)
times <- c(1, 2, 4)
rates <- c(0.03, 0.03, 0.05)
probs <- c(0.9, 0.8, 0.7)

pv_expected <- sum(cf * (1 + rates)^(-times) * probs)
pv_actual <- lifecontingencies:::.presentValueC(
  cashFlows = cf,
  timeIds = times,
  interestRates = rates,
  probabilities = probs,
  power = 1
)
stopifnot(all.equal(pv_actual, pv_expected, tolerance = 1e-12))

pv2_expected <- sum(cf^2 * (1 + rates)^(-2 * times) * probs)
pv2_actual <- lifecontingencies:::.presentValueC(
  cashFlows = cf,
  timeIds = times,
  interestRates = rates,
  probabilities = probs,
  power = 2
)
stopifnot(all.equal(pv2_actual, pv2_expected, tolerance = 1e-12))

# Public R wrapper must continue to agree with the native kernel.
stopifnot(all.equal(
  presentValue(cf, times, rates, probs),
  pv_actual,
  tolerance = 1e-12
))

# -----------------------------------------------------------------------------
# Actuarial kernels
# -----------------------------------------------------------------------------
T <- 40
age <- 30
years <- 10
i <- 0.04
m <- 2
k <- 2

expected_exn <- if (T < age + years) 0 else (1 + i)^(-years)
stopifnot(all.equal(
  lifecontingencies:::.fExnCpp(T, age, years, i),
  expected_exn,
  tolerance = 1e-12
))

expected_axn <- if ((T >= age + m) && (T <= age + m + years - 1 / k)) {
  (1 + i)^(-(T - age + 1 / k))
} else 0
stopifnot(all.equal(
  lifecontingencies:::.fAxnCpp(T, age, years, i, m, k),
  expected_axn,
  tolerance = 1e-12
))

expected_iaxn <- if ((T >= age + m) && (T <= age + m + years - 1 / k)) {
  (T - (age + m) + 1 / k) * (1 + i)^(-(T - age + 1 / k))
} else 0
stopifnot(all.equal(
  lifecontingencies:::.fIAxnCpp(T, age, years, i, m, k),
  expected_iaxn,
  tolerance = 1e-12
))

expected_daxn <- if ((T >= age + m) && (T <= age + m + years - 1 / k)) {
  (years - (T - (age + m) + 1 / k)) * (1 + i)^(-(T - age + 1 / k))
} else 0
stopifnot(all.equal(
  lifecontingencies:::.fDAxnCpp(T, age, years, i, m, k),
  expected_daxn,
  tolerance = 1e-12
))

expected_aexn <- if ((T >= age) && (T <= age + years - 1 / k)) {
  (1 + i)^(-(T - age + 1 / k))
} else {
  (1 + i)^(-years)
}
stopifnot(all.equal(
  lifecontingencies:::.fAExnCpp(T, age, years, i, k),
  expected_aexn,
  tolerance = 1e-12
))

# Boundary checks for the piecewise actuarial kernels.
T_in <- age + m
T_out <- age + m + years
stopifnot(lifecontingencies:::.fAxnCpp(T_in, age, years, i, m, k) != 0)
stopifnot(lifecontingencies:::.fAxnCpp(T_out, age, years, i, m, k) == 0)

# -----------------------------------------------------------------------------
# Public presentValue input checks already implemented in R.
# Keep these as regression tests while the native backend is hardened.
# -----------------------------------------------------------------------------
stopifnot(inherits(
  try(presentValue(c(1, 2), c(1), 0.03, c(1, 1)), silent = TRUE),
  "try-error"
))
stopifnot(inherits(
  try(presentValue(c(1, 2), c(1, 2), 0.03, c(1)), silent = TRUE),
  "try-error"
))

# Native backend rejects malformed vector inputs instead of relying on
# unchecked indexing.
stopifnot(inherits(
  try(lifecontingencies:::.mult2sum(c(1, 2), c(1)), silent = TRUE),
  "try-error"
))
stopifnot(inherits(
  try(lifecontingencies:::.mult3sum(c(1, 2), c(1), c(1, 2)), silent = TRUE),
  "try-error"
))
stopifnot(inherits(
  try(lifecontingencies:::.presentValueC(
    c(1, 2), c(1, 2), c(0.03), c(1, 1), 1
  ), silent = TRUE),
  "try-error"
))

# Invalid payment frequencies are rejected by the native actuarial kernels.
for (bad_k in c(0, -1, NA_real_, NaN, Inf)) {
  stopifnot(inherits(
    try(lifecontingencies:::.fAxnCpp(40, 30, 10, 0.04, 2, bad_k), silent = TRUE),
    "try-error"
  ))
}
