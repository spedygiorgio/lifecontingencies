# Vectorized implementations of the standard single-life APVs.
#
# These wrappers deliberately reuse the existing vectorized pxt() backend.
# The important optimization is to build the complete payment grid once and
# evaluate all survival/death probabilities in bulk, instead of calling pxt()
# and presentValue() once per contract.

.apv_recycle <- function(x, n, m) {
  ntot <- max(length(x), length(n), length(m))
  list(
    x = rep(x, length.out = ntot),
    n = rep(n, length.out = ntot),
    m = rep(m, length.out = ntot),
    ntot = ntot
  )
}

.apv_fractional_method <- function(fractional) {
  fractional <- testfractionnalarg(fractional)
  match(fractional, c("linear", "constant force", "hyperbolic")) - 1L
}

.apv_group_sum <- function(values, id, n) {
  as.numeric(rowsum(values, id, reorder = FALSE))
}

axn <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
                k = 1, type = "EV", power = 1, payment = "advance", ...) {
  if (!(class(actuarialtable) %in% c("lifetable", "actuarialtable")))
    stop("Error! Only lifetable, actuarialtable classes are accepted")
  if (missing(x))
    stop("Missing x")
  if (length(x) == 0)
    stop("x is of length zero")
  if (length(k) != 1 || !is.numeric(k) || !is.finite(k) || k <= 0)
    stop("k must be a finite positive scalar")

  type <- testtyperesarg(type)
  payment <- testpaymentarg(payment)
  if (missing(m))
    m <- 0
  if (missing(n))
    n <- pmax(ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k, 0)

  z <- .apv_recycle(x, n, m)
  x <- z$x
  n <- z$n
  m <- z$m
  ntot <- z$ntot

  if (any(!is.finite(x)) || any(!is.finite(n)) || any(!is.finite(m)))
    stop("infinite or missing values provided in x, n or m")
  if (any(x < 0 | n < 0 | m < 0))
    stop("Check x, n or m")

  if (type == "ST") {
    return(vapply(seq_len(ntot), function(j) {
      rLifeContingencies(
        n = 1, lifecontingency = "axn", object = actuarialtable,
        x = x[j], t = n[j], i = i, m = m[j], k = k, payment = payment
      )
    }, numeric(1)))
  }

  lengths <- as.integer(round(n * k))
  out <- numeric(ntot)
  positive <- lengths > 0
  if (!any(positive))
    return(out)

  ids <- rep(which(positive), lengths[positive])
  if (payment == "immediate") {
    times <- unlist(Map(function(mj, nj) {
      mj + seq(from = 1 / k, to = nj, by = 1 / k)
    }, m[positive], n[positive]), use.names = FALSE)
  } else if (payment == "due") {
    times <- unlist(Map(function(mj, nj) {
      mj + seq(from = 0, to = nj - 1 / k, by = 1 / k)
    }, m[positive], n[positive]), use.names = FALSE)
  } else {
    stop("wrong payment type")
  }

  x_rows <- x[ids]
  i_rows <- rep(i, length.out = length(times))
  probabilities <- pxt(
    actuarialtable, x = x_rows, t = times, ...
  )
  values <- (1 / k)^power *
    (1 + i_rows)^(-times * power) * probabilities

  out[positive] <- .apv_group_sum(values, ids, ntot)
  out
}

Axn <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
                k = 1, type = "EV", power = 1, ...) {
  if (!(class(actuarialtable) %in% c("lifetable", "actuarialtable")))
    stop("Error! Only lifetable, actuarialtable classes are accepted")
  if (missing(x))
    stop("Missing x")
  if (length(x) == 0)
    stop("x is of length zero")
  if (length(k) != 1 || !is.numeric(k) || !is.finite(k) || k <= 0)
    stop("k must be a finite positive scalar")

  type <- testtyperesarg(type)
  if (missing(m))
    m <- 0
  if (missing(n))
    n <- pmax(ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k, 0)

  z <- .apv_recycle(x, n, m)
  x <- z$x
  n <- z$n
  m <- z$m
  ntot <- z$ntot

  if (any(!is.finite(x)) || any(!is.finite(n)) || any(!is.finite(m)))
    stop("infinite or missing values provided in x, n or m")
  if (any(x < 0 | n < 0 | m < 0))
    stop("Check x, n or m")

  if (type == "ST") {
    return(vapply(seq_len(ntot), function(j) {
      rLifeContingencies(
        n = 1, lifecontingency = "Axn", object = actuarialtable,
        x = x[j], t = n[j], i = i, m = m[j], k = k
      )
    }, numeric(1)))
  }

  lengths <- as.integer(round(n * k))
  out <- numeric(ntot)
  positive <- lengths > 0
  if (!any(positive))
    return(out)

  ids <- rep(which(positive), lengths[positive])
  times <- unlist(Map(function(mj, nj) {
    mj + seq(from = 0, to = nj - 1 / k, by = 1 / k)
  }, m[positive], n[positive]), use.names = FALSE)

  x_rows <- x[ids]
  i_rows <- rep(i, length.out = length(times))
  survival <- pxt(
    actuarialtable, x = x_rows, t = times, ...
  )
  one_period_survival <- pxt(
    actuarialtable, x = x_rows + times, t = rep(1 / k, length(times)), ...
  )
  death_probability <- survival * (1 - one_period_survival)
  values <- (1 + i_rows)^(-(times + 1 / k) * power) * death_probability

  out[positive] <- .apv_group_sum(values, ids, ntot)
  out
}

AExn <- function(actuarialtable, x, n, i = actuarialtable@interest, k = 1,
                 type = "EV", power = 1, ...) {
  if (!(class(actuarialtable) %in% c("lifetable", "actuarialtable")))
    stop("Error! Only lifetable, actuarialtable classes are accepted")
  if (missing(x))
    stop("Missing x")
  if (length(x) == 0)
    stop("x is of length zero")
  if (length(k) != 1 || !is.numeric(k) || !is.finite(k) || k <= 0)
    stop("k must be a finite positive scalar")
  type <- testtyperesarg(type)
  if (missing(n))
    n <- pmax(getOmega(actuarialtable) - x - 1, 0)

  z <- .apv_recycle(x, n, rep(0, length(x)))
  x <- z$x
  n <- z$n
  ntot <- z$ntot

  if (any(!is.finite(x)) || any(!is.finite(n)))
    stop("infinite or missing values provided in x or n")
  if (any(x < 0 | n < 0))
    stop("Check x or n")

  if (type == "ST") {
    return(vapply(seq_len(ntot), function(j) {
      rLifeContingencies(
        n = 1, lifecontingency = "AExn", object = actuarialtable,
        x = x[j], t = n[j], i = i, m = 0, k = k
      )
    }, numeric(1)))
  }

  lengths <- as.integer(round(n * k))
  insurance <- numeric(ntot)
  positive <- lengths > 0

  if (any(positive)) {
    ids <- rep(which(positive), lengths[positive])
    times <- unlist(Map(function(nj) {
      seq(from = 0, to = nj - 1 / k, by = 1 / k)
    }, n[positive]), use.names = FALSE)
    x_rows <- x[ids]
    i_rows <- rep(i, length.out = length(times))
    survival <- pxt(actuarialtable, x = x_rows, t = times, ...)
    one_period_survival <- pxt(
      actuarialtable, x = x_rows + times,
      t = rep(1 / k, length(times)), ...
    )
    death_probability <- survival * (1 - one_period_survival)
    values <- (1 + i_rows)^(-(times + 1 / k) * power) * death_probability
    insurance[positive] <- .apv_group_sum(values, ids, ntot)
  }

  endowment_probability <- pxt(actuarialtable, x = x, t = n, ...)
  endowment <- (1 + rep(i, length.out = ntot))^(-n * power) * endowment_probability
  insurance + endowment
}
