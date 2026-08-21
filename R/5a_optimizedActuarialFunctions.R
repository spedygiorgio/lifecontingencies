#############################################################################
# Optimized implementations of the classical one-life APVs.
#
# Keep the public API and presentValue() as the final valuation layer.  These
# implementations optimize the R path without introducing separate Rcpp
# kernels for axn/Axn/AExn.  pxt() retains its existing Rcpp backend.
#############################################################################

axn <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
                k = 1, type = "EV", power = 1, payment = "advance", ...) {
  if (!(class(actuarialtable) %in% c("lifetable", "actuarialtable")))
    stop("Error! Only lifetable, actuarialtable classes are accepted")

  type <- testtyperesarg(type)
  payment <- testpaymentarg(payment)
  if (missing(x)) stop("Missing x")
  if (length(k) > 1) {
    k <- k[1]
    warning("k should be of length 1, it takes the first value")
  }
  if (missing(m)) m <- 0
  if (missing(n))
    n <- pmax(ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k, 0)

  if (length(x) <= 0) stop("x is of length zero")
  if (any(x < 0, n < 0, m < 0)) stop("Check x, n or m")

  ntot <- max(length(x), length(n), length(m))
  if (length(x) != ntot || length(n) != ntot || length(m) != ntot) {
    if (length(x) != ntot)
      warning("x argument has been recycled to match the maximum length of x, m and n")
    if (length(n) != ntot)
      warning("n argument has been recycled to match the maximum length of x, m and n")
    if (length(m) != ntot)
      warning("m argument has been recycled to match the maximum length of x, m and n")
    x <- rep(x, length.out = ntot)
    n <- rep(n, length.out = ntot)
    m <- rep(m, length.out = ntot)
  }

  if (type == "ST") {
    out <- numeric(ntot)
    for (j in seq_len(ntot)) {
      out[j] <- rLifeContingencies(
        n = 1, lifecontingency = "axn", object = actuarialtable,
        x = x[j], t = n[j], i = i, m = m[j], k = k, payment = payment
      )
    }
    return(out)
  }
  if (type != "EV") stop("wrong result type")

  # Optimization 1: cache deterministic payment/time grids by (n, m).
  keys <- paste(n, m, sep = "\r")
  key_levels <- unique(keys)
  grids <- vector("list", length(key_levels))
  names(grids) <- key_levels
  for (g in seq_along(key_levels)) {
    idx <- match(key_levels[g], keys)
    if (n[idx] <= 0) {
      grids[[g]] <- list(payments = numeric(0), times = numeric(0))
    } else {
      times <- if (payment == "immediate")
        m[idx] + seq(from = 1 / k, to = n[idx], by = 1 / k)
      else if (payment == "due")
        m[idx] + seq(from = 0, to = n[idx] - 1 / k, by = 1 / k)
      else stop("wrong payment type")
      grids[[g]] <- list(payments = rep(1 / k, length(times)), times = times)
    }
  }

  # Optimization 2: preallocate the result and use one tight loop instead of
  # constructing a nested closure and an sapply result for each observation.
  out <- numeric(ntot)
  for (j in seq_len(ntot)) {
    grid <- grids[[keys[j]]]
    if (!length(grid$times)) {
      out[j] <- 0
      next
    }
    probs <- pxt(actuarialtable, x[j], grid$times, ...)
    out[j] <- presentValue(
      cashFlows = grid$payments,
      timeIds = grid$times,
      interestRates = i,
      probabilities = probs,
      power = power
    )
  }
  out
}

Axn <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
                k = 1, type = "EV", power = 1, ...) {
  if (!(class(actuarialtable) %in% c("lifetable", "actuarialtable")))
    stop("Error! Only lifetable, actuarialtable classes are accepted")

  type <- testtyperesarg(type)
  if (missing(x)) stop("Missing x")
  if (length(k) > 1) {
    k <- k[1]
    warning("k should be of length 1, it takes first value")
  }
  if (missing(m)) m <- 0
  if (missing(n))
    n <- pmax(ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k, 0)

  if (length(x) <= 0) stop("x is of length zero")
  if (any(x < 0, n < 0, m < 0)) stop("Check x, n or m")

  ntot <- max(length(x), length(n), length(m))
  if (length(x) != ntot || length(n) != ntot || length(m) != ntot) {
    if (length(x) != ntot)
      warning("x argument has been recycled to match the maximum length of x, m and n")
    if (length(n) != ntot)
      warning("n argument has been recycled to match the maximum length of x, m and n")
    if (length(m) != ntot)
      warning("m argument has been recycled to match the maximum length of x, m and n")
    x <- rep(x, length.out = ntot)
    n <- rep(n, length.out = ntot)
    m <- rep(m, length.out = ntot)
  }

  if (type == "ST") {
    out <- numeric(ntot)
    for (j in seq_len(ntot)) {
      out[j] <- rLifeContingencies(
        n = 1, lifecontingency = "Axn", object = actuarialtable,
        x = x[j], t = n[j], i = i, m = m[j], k = k
      )
    }
    return(out)
  }
  if (type != "EV") stop("wrong result type")

  keys <- paste(n, m, sep = "\r")
  key_levels <- unique(keys)
  grids <- vector("list", length(key_levels))
  names(grids) <- key_levels
  for (g in seq_along(key_levels)) {
    idx <- match(key_levels[g], keys)
    if (n[idx] <= 0) {
      grids[[g]] <- list(payments = numeric(0), times = numeric(0))
    } else {
      times <- m[idx] + seq(from = 0, to = n[idx] - 1 / k, by = 1 / k)
      grids[[g]] <- list(payments = rep(1, length(times)), times = times)
    }
  }

  out <- numeric(ntot)
  for (j in seq_len(ntot)) {
    grid <- grids[[keys[j]]]
    if (!length(grid$times)) {
      out[j] <- 0
      next
    }
    times <- grid$times
    # q_{x+t}^{(1/k)} = 1 - p_{x+t}^{(1/k)} under the same fractional
    # convention, so p_x(t)q_{x+t}(1/k) = p_x(t)-p_x(t+1/k).
    p0 <- pxt(actuarialtable, x[j], times, ...)
    p1 <- pxt(actuarialtable, x[j], times + 1 / k, ...)
    probs <- p0 - p1
    out[j] <- presentValue(
      cashFlows = grid$payments,
      timeIds = times + 1 / k,
      interestRates = i,
      probabilities = probs,
      power = power
    )
  }
  out
}

# AExn deliberately keeps the existing decomposition: this preserves the
# public semantics while automatically benefiting from the optimized Axn().
AExn <- function(actuarialtable, x, n, i = actuarialtable@interest, k = 1,
                 type = "EV", power = 1) {
  if (!(class(actuarialtable) %in% c("lifetable", "actuarialtable")))
    stop("Error! Need an actuarial actuarialtable")
  if (missing(x)) stop("Error! Need age!")
  type <- testtyperesarg(type)
  if (k < 1) stop("Error! Periods in a year shall be no less than 1")
  if (missing(n)) n <- getOmega(actuarialtable) - x - 1
  if (any(x < 0, n < 0)) stop("Error! Negative parameters")

  if (type == "EV") {
    return(Axn(actuarialtable, x = x, n = n, i = i, m = 0,
               k = k, type = "EV", power = power) +
             Exn(actuarialtable, x = x, n = n, i = i,
                 type = "EV", power = power))
  }
  if (type == "ST") {
    return(rLifeContingencies(
      n = 1, lifecontingency = "AExn", object = actuarialtable,
      x = x, t = n, i = i, k = k
    ))
  }
  stop("wrong result type")
}
