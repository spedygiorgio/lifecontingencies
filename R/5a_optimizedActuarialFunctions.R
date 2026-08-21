# Optimized implementations of the three standard APVs.
# These wrappers preserve the public API while moving the EV inner loops to
# one C++ call per actuarial function.  The original R implementations remain
# available in 5_actuarialFunctions.R and the legacy scalar versions in 5b/.

.actuarial_fractional_method <- function(dots) {
  fractional <- if (!is.null(dots$fractional)) dots$fractional else "linear"
  fractional <- testfractionnalarg(fractional)
  switch(fractional,
         "linear" = 0L,
         "constant force" = 1L,
         "hyperbolic" = 2L,
         stop("Unsupported fractional assumption"))
}

.actuarial_lx <- function(actuarialtable) {
  if (!(class(actuarialtable) %in% c("lifetable", "actuarialtable")))
    stop("Error! Only lifetable and actuarialtable classes are accepted")
  as.numeric(actuarialtable@lx)
}

.axn_r_fallback <- function(actuarialtable, x, n, i, m, k, power, payment, dots) {
  one <- function(j) {
    if (n[j] <= 0) return(0)
    if (payment == "immediate")
      times <- m[j] + seq(from = 1 / k, to = n[j], by = 1 / k)
    else
      times <- m[j] + seq(from = 0, to = n[j] - 1 / k, by = 1 / k)
    probs <- do.call(pxt, c(list(object = actuarialtable, x = x[j], t = times), dots))
    presentValue(rep(1 / k, length(times)), times, i, probs, power)
  }
  vapply(seq_along(x), one, numeric(1))
}

#' Optimized survival annuity.
#' @keywords internal
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
  if (length(x) == 0 || length(n) == 0 || length(m) == 0)
    stop("x, n and m must not be empty")

  ntot <- max(length(x), length(n), length(m))
  x <- rep(x, length.out = ntot)
  n <- rep(n, length.out = ntot)
  m <- rep(m, length.out = ntot)
  if (any(!is.finite(x)) || any(!is.finite(n)) || any(!is.finite(m)))
    stop("infinite values provided in x, n or m")
  if (any(x < 0, n < 0, m < 0)) stop("Check x, n or m")

  if (type == "ST") {
    return(vapply(seq_len(ntot), function(j)
      rLifeContingencies(n = 1, lifecontingency = "axn",
                         object = actuarialtable, x = x[j], t = n[j],
                         i = i, m = m[j], k = k, payment = payment),
      numeric(1)))
  }

  dots <- list(...)
  fractional_method <- .actuarial_fractional_method(dots)
  if (!is.null(dots$decrement))
    return(.axn_r_fallback(actuarialtable, x, n, i, m, k, power, payment, dots))

  .axnCpp(x, n, m, i, k, if (payment == "immediate") 0L else 1L,
          fractional_method, power, .actuarial_lx(actuarialtable),
          getOmega(actuarialtable))
}

#' Optimized life insurance APV.
#' @keywords internal
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
  if (length(x) == 0 || length(n) == 0 || length(m) == 0)
    stop("x, n and m must not be empty")

  ntot <- max(length(x), length(n), length(m))
  x <- rep(x, length.out = ntot)
  n <- rep(n, length.out = ntot)
  m <- rep(m, length.out = ntot)
  if (any(!is.finite(x)) || any(!is.finite(n)) || any(!is.finite(m)))
    stop("infinite values provided in x, n or m")
  if (any(x < 0, n < 0, m < 0)) stop("Check x, n or m")

  if (type == "ST") {
    return(vapply(seq_len(ntot), function(j)
      rLifeContingencies(n = 1, lifecontingency = "Axn",
                         object = actuarialtable, x = x[j], t = n[j],
                         i = i, m = m[j], k = k), numeric(1)))
  }

  dots <- list(...)
  fractional_method <- .actuarial_fractional_method(dots)
  if (!is.null(dots$decrement)) {
    one <- function(j) {
      if (n[j] <= 0) return(0)
      times <- m[j] + seq(from = 0, to = n[j] - 1 / k, by = 1 / k)
      probs <- do.call(pxt, c(list(object = actuarialtable, x = x[j], t = times), dots)) *
        do.call(qxt, c(list(object = actuarialtable,
                            x = x[j] + times, t = 1 / k), dots))
      presentValue(rep(1, length(times)), times + 1 / k, i, probs, power)
    }
    return(vapply(seq_len(ntot), one, numeric(1)))
  }

  .AxnCpp(x, n, m, i, k, fractional_method, power,
          .actuarial_lx(actuarialtable), getOmega(actuarialtable))
}

#' Optimized n-year endowment insurance APV.
#' @keywords internal
AExn <- function(actuarialtable, x, n, i = actuarialtable@interest,
                 k = 1, type = "EV", power = 1) {
  if (!(class(actuarialtable) %in% c("lifetable", "actuarialtable")))
    stop("Error! Need an actuarial actuarialtable")
  if (missing(x)) stop("Error! Need age!")
  type <- testtyperesarg(type)
  if (k < 1) stop("Error! Periods in a year shall be no less than 1")
  if (missing(n)) n <- getOmega(actuarialtable) - x - 1
  if (any(x < 0, n < 0)) stop("Error! Negative parameters")

  if (type == "ST") {
    if (length(x) != 1 || length(n) != 1)
      stop("type='ST' requires scalar x and n")
    return(rLifeContingencies(n = 1, lifecontingency = "AExn",
                              object = actuarialtable, x = x, t = n,
                              i = i, k = k))
  }

  x <- as.numeric(x)
  n <- rep(n, length.out = length(x))
  if (any(n == 0)) {
    out <- numeric(length(x))
    zero <- n == 0
    out[zero] <- 1
    if (any(!zero)) {
      out[!zero] <- .AExnCpp(x[!zero], n[!zero], i, k, 0L, power,
                             .actuarial_lx(actuarialtable), getOmega(actuarialtable))
    }
    return(out)
  }

  .AExnCpp(x, n, i, k, 0L, power,
           .actuarial_lx(actuarialtable), getOmega(actuarialtable))
}
