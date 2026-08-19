require(lifecontingencies)

data("soa08Act")

# -----------------------------------------------------------------------------
# Regression tests for the Rcpp backend.
#
# The primary criterion is numerical agreement with the corresponding
# historical R implementation, where that implementation exists.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# presentValue / presentValueC
#
# This is the R implementation that presentValueC() replaced in PR #39:
#   v <- (1 + interestRates)^(-timeIds)
#   sum(((cashFlows^power) * (v^power)) * probabilities)
#
# Keep this reference independent from the C++ implementation.
# -----------------------------------------------------------------------------
reference_present_value <- function(cashFlows, timeIds, interestRates,
                                    probabilities, power = 1) {
  v <- (1 + interestRates)^(-timeIds)
  sum(((cashFlows^power) * (v^power)) * probabilities)
}

test_cases <- list(
  list(
    cashFlows = c(100, -30, 50, 70),
    timeIds = c(0, 0.5, 3, 6.25),
    interestRates = 0.035,
    probabilities = c(1, 0.9, 0.8, 0.65),
    power = 1
  ),
  list(
    cashFlows = c(10, 10, 10, 10, 10, 10, 110),
    timeIds = 1:7,
    interestRates = c(0.02, 0.021, 0.0225, 0.024, 0.0255, 0.027, 0.028),
    probabilities = rep(0.995, 7),
    power = 1
  ),
  list(
    cashFlows = c(3, 5, 7),
    timeIds = c(1, 2, 3),
    interestRates = c(0.01, 0.015, 0.02),
    probabilities = c(0.9, 0.8, 0.7),
    power = 2
  )
)

for (case in test_cases) {
  expected <- do.call(reference_present_value, case)
  actual_public <- do.call(presentValue, case)

  # The public R wrapper recycles interestRates before calling the native
  # kernel. Reproduce that historical behaviour before testing .presentValueC.
  case_cpp <- case
  case_cpp$interestRates <- rep(
    case$interestRates,
    length.out = length(case$timeIds)
  )
  actual_cpp <- do.call(lifecontingencies:::.presentValueC, case_cpp)

  stopifnot(all.equal(actual_public, expected, tolerance = 1e-12))
  stopifnot(all.equal(actual_cpp, expected, tolerance = 1e-12))
}

# Randomized regression test against the historical R implementation.
set.seed(123)
for (power in c(1, 2, 3)) {
  cf <- rnorm(50, mean = 5, sd = 20)
  times <- seq(0.25, by = 0.25, length.out = length(cf))
  rates <- runif(length(cf), min = 0.005, max = 0.06)
  probs <- runif(length(cf), min = 0.2, max = 1)

  expected <- reference_present_value(cf, times, rates, probs, power)
  actual_cpp <- lifecontingencies:::.presentValueC(
    cf, times, rates, probs, power
  )

  stopifnot(all.equal(actual_cpp, expected, tolerance = 1e-10))
}

# Scalar interest rate recycling, as performed by the public R wrapper.
cf <- rnorm(50, mean = 5, sd = 20)
times <- seq(0.25, by = 0.25, length.out = length(cf))
probs <- runif(length(cf), min = 0.2, max = 1)
expected <- reference_present_value(cf, times, 0.018, probs)
stopifnot(all.equal(
  presentValue(cf, times, 0.018, probs),
  expected,
  tolerance = 1e-10
))

# -----------------------------------------------------------------------------
# Axn: current implementation vs the historical Axnold implementation.
#
# This follows tests/test-computation-time-act-capital.R.
# -----------------------------------------------------------------------------
Axn_old_x <- Vectorize(lifecontingencies:::Axnold, "x")
Axn_old_n <- Vectorize(lifecontingencies:::Axnold, "n")
Axn_old_m <- Vectorize(lifecontingencies:::Axnold, "m")

x_cases <- c(30:35 + 0.5, 60, 65, 80, 85)

for (k in c(1, 2, 4)) {
  new <- Axn(soa08Act, x = x_cases, n = 10, i = 0.06, k = k)
  old <- Axn_old_x(soa08Act, x = x_cases, n = 10, i = 0.06, k = k)
  stopifnot(all.equal(new, old, tolerance = 1e-10))
}

for (k in c(1, 2, 4)) {
  new_n <- Axn(soa08Act, x = 33, n = 1:20, i = 0.06, k = k)
  old_n <- Axn_old_n(soa08Act, x = 33, n = 1:20, i = 0.06, k = k)
  stopifnot(all.equal(new_n, old_n, tolerance = 1e-10))

  new_m <- Axn(soa08Act, x = 33, n = 20, m = 0:10, i = 0.06, k = k)
  old_m <- Axn_old_m(soa08Act, x = 33, n = 20, m = 0:10, i = 0.06, k = k)
  stopifnot(all.equal(new_m, old_m, tolerance = 1e-10))
}

# Historical actuarial benchmark from the existing test suite (Bowers p. 339).
stopifnot(
  abs(Axn(soa08Act, x = 30, i = 0.06, k = 4) - 0.1048) < 5e-4
)

# Explicit cases from the historical test:
# basic deferred insurance, non-integer ages and k > 1.
AxncheckR <- function(object, x, m, n) {
  i <- object@interest
  f <- function(t)
    pxt(object, x = x, t = t) * qxt(object, x = x + t, t = 1)
  prob <- sapply(m:(m + n - 1), f)
  rowSums(prob / ((1 + i)^(m + 1):(m + n)))
}

stopifnot(all.equal(
  Axn(soa08Act, x = 65:66, n = 1, m = 1),
  AxncheckR(soa08Act, x = 65:66, m = 1, n = 1),
  tolerance = 1e-10
))

x <- 30:35 + 1 / 2
stopifnot(all.equal(
  Axn(soa08Act, x = x),
  Axn_old_x(soa08Act, x = x),
  tolerance = 1e-10
))

# -----------------------------------------------------------------------------
# axn: current implementation vs the historical axnold implementation.
# -----------------------------------------------------------------------------
axn_old_x <- Vectorize(lifecontingencies:::axnold, "x")
axn_old_n <- Vectorize(lifecontingencies:::axnold, "n")
axn_old_m <- Vectorize(lifecontingencies:::axnold, "m")

for (payment in c("due", "arrears", "immediate", "advance")) {
  new_x <- axn(soa08Act, x = 1:100, payment = payment)
  old_x <- axn_old_x(soa08Act, x = 1:100, payment = payment)
  stopifnot(all.equal(new_x, old_x, tolerance = 1e-10))

  new_n <- axn(soa08Act, x = 33, n = 10:30, payment = payment)
  old_n <- axn_old_n(soa08Act, x = 33, n = 10:30, payment = payment)
  stopifnot(all.equal(new_n, old_n, tolerance = 1e-10))

  new_m <- axn(soa08Act, x = 33, n = 30, m = 0:10, payment = payment)
  old_m <- axn_old_m(soa08Act, x = 33, m = 0:10, n = 30, payment = payment)
  stopifnot(all.equal(new_m, old_m, tolerance = 1e-10))
}

x <- 85:90
stopifnot(all.equal(
  axn(soa08Act, x = x, payment = "advance"),
  axn_old_x(soa08Act, x = x, payment = "advance"),
  tolerance = 1e-10
))

x <- 30:35 + 1 / 2
stopifnot(all.equal(
  axn(soa08Act, x = x, payment = "advance"),
  axn_old_x(soa08Act, x = x, payment = "advance"),
  tolerance = 1e-10
))

# -----------------------------------------------------------------------------
# Native helper kernels: deterministic checks of their numerical identities.
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

stopifnot(all.equal(
  lifecontingencies:::.fAExnCpp(T, age, years, i, k),
  if ((T >= age) && (T <= age + years - 1 / k))
    (1 + i)^(-(T - age + 1 / k))
  else
    (1 + i)^(-years),
  tolerance = 1e-12
))

# -----------------------------------------------------------------------------
# Native input validation.
# -----------------------------------------------------------------------------
stopifnot(all.equal(
  lifecontingencies:::.mult2sum(c(1, 2, 3), c(4, 5, 6)),
  sum(c(1, 2, 3) * c(4, 5, 6)),
  tolerance = 1e-12
))

stopifnot(all.equal(
  lifecontingencies:::.mult3sum(c(1, -2, 3), c(4, 5, -6), c(2, 3, 4)),
  sum(c(1, -2, 3) * c(4, 5, -6) * c(2, 3, 4)),
  tolerance = 1e-12
))

stopifnot(inherits(
  try(lifecontingencies:::.mult2sum(c(1, 2), c(1)), silent = TRUE),
  "try-error"
))

stopifnot(inherits(
  try(lifecontingencies:::.mult3sum(c(1, 2), c(1), c(1, 2)), silent = TRUE),
  "try-error"
))

stopifnot(inherits(
  try(
    lifecontingencies:::.presentValueC(
      c(1, 2), c(1, 2), c(0.03), c(1, 1), 1
    ),
    silent = TRUE
  ),
  "try-error"
))

for (bad_k in c(0, -1, NA_real_, NaN, Inf)) {
  stopifnot(inherits(
    try(
      lifecontingencies:::.fAxnCpp(40, 30, 10, 0.04, 2, bad_k),
      silent = TRUE
    ),
    "try-error"
  ))
}