#############################################################################
#   Copyright (c) 2018 Giorgio A. Spedicato
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the
#   Free Software Foundation, Inc.,
#   59 Temple Place, Suite 330, Boston, MA 02111-1307, USA
#
#############################################################################
###
###         decrement functions
###


# t=0 is handled explicitly below (returns 0 decrements): verified by the
# regression tests in tests/testthat/testMultipleDecrements.R, covering
# dxt/qxt/pxt with and without an explicit `decrement`.

#decrement specific function

.dxt.mdt<-function(object=object, x=x, time=time, decrement=decrement ) {
	out<-0
	if (time==0) return(0) #no decrement after just 0 seconds
	if(missing(decrement)) {
		decrement.cols<-which(!(names(object@table) %in% c("lx","x")))
	} else {
		# Validates the name(s) (an unknown decrement is an error, not a silent
		# 0) and accepts several decrements at once, whose counts are summed.
		decrement <- .mdtDecrementNames(object, decrement)
		decrement.cols<-which(names(object@table) %in% decrement)
	}
		# Integer part of the duration, plus a linear (UDD) share of the next
		# year's decrements for a fractional duration.
		intTime <- floor(time)
		fracTime <- time - intTime
		if (intTime > 0) {
			ages2consider<-x+0:(intTime-1)
			age.rows<-which(object@table$x %in% ages2consider)
			out<-sum(object@table[age.rows,decrement.cols])
		}
		if (fracTime > 0) {
			nextRow <- which(object@table$x == x + intTime)
			if (length(nextRow) == 1)
				out <- out + fracTime * sum(object@table[nextRow, decrement.cols])
		}
	invisible(out)
}

.qxt.mdt<-function(object,x,t,decrement) {
	out<-0
	ageIndex<-which(object@table$x==x)
	lx<-object@table$lx[ageIndex]
	dx<-ifelse(missing(decrement),.dxt.mdt(object=object,x=x, time=t),.dxt.mdt(object=object,x=x, time = t,decrement=decrement))
	out<-dx/lx
	invisible(out)
}


#' Return Associated single decrement from absolute rate of decrement
#'
#' @param object a mdj object 
#' @param x age
#' @param t period (default 1)
#' @param decrement type (necessary)
#' @param qx.prime single ASDT decrement of which corresponding decrement is desired
#' @param other.qx.prime ASDT decrements other than \code{qx.prime}
#' 
#'
#' @return a single value (AST)
#' 
#' @examples 
#' #Creating the valdez mdf
#' 
#' valdezDf<-data.frame(
#' x=c(50:54),
#' lx=c(4832555,4821937,4810206,4797185,4782737),
#' hearth=c(5168, 5363, 5618, 5929, 6277),
#' accidents=c(1157, 1206, 1443, 1679,2152),
#' other=c(4293,5162,5960,6840,7631))
#' valdezMdt<-new("mdt",name="ValdezExample",table=valdezDf) 
#' 
#' qxt.prime.fromMdt(object=valdezMdt,x=53,decrement="other")
#' 
#' #Finan example 67.2
#' 
#' qxt.fromQxprime(qx.prime = 0.01,other.qx.prime = c(0.03,0.06))
#' 
qxt.prime.fromMdt<-function(object, x, t=1, decrement) {
  out <- NA
  if (missing(decrement)) stop("Error! decrement must be specified");
  fraction <- (qxt(object = object,x = x,t=1,decrement = decrement)/qxt(object = object,x = x,t=1))
  out <- 1 - (1-qxt(object = object,x = x,t=t, fractional="linear"))^fraction
  return(out)
}

#closure needed to get the function to be integrate

.getFunctionIns<-function(qxVector) {
  myFun<-function(s) {
    temp<-qxVector*rep(s,length(qxVector))
    out<-prod(1-temp)
    return(out)
  }
  return(myFun)
}

#' @describeIn qxt.prime.fromMdt Obtain decrement from single decrements
qxt.fromQxprime <-function(qx.prime, other.qx.prime, t=1) {
  function2Integrate<-.getFunctionIns(qxVector=other.qx.prime)
  #manually compute the integral
  #subdivisions=1000L
  #myOut<-seq(from=0, to=t, length.out = subdivisions)
  #temp<-sapply(myOut, function2Integrate)/subdivisions
  #integral<-sum(temp)
  integral<-integrate(f = Vectorize(function2Integrate),lower = 0,upper = t)$value
  out<-qx.prime*integral
  return(out)
}

#' Extract the full Associated Single Decrement Table (ASDT) from an mdt object
#'
#' \code{independentRatesFromMdt} returns a matrix of ASDT independent rates
#' \eqn{q'^{(j)}_x} for every combination of age and decrement in the supplied
#' multiple-decrement table.
#'
#' The independent rate for decrement \eqn{j} at age \eqn{x} is obtained under
#' the Uniform Distribution of Deaths (UDD) assumption as
#' \deqn{q'^{(j)}_x = 1 - \bigl(1 - q^{(\tau)}_x\bigr)^{q^{(j)}_x / q^{(\tau)}_x},}
#' which is the same formula used by \code{\link{qxt.prime.fromMdt}} for a single
#' age/decrement pair.  \code{independentRatesFromMdt} is a convenience wrapper
#' that applies this extraction to all ages and all decrements at once, returning
#' the result as a tidy matrix.
#'
#' @param object An \code{mdt} object.
#' @param x Optional numeric vector of ages to include.  Defaults to all ages
#'   tabulated in \code{object} except the last. Note that this includes the
#'   synthetic ages that \code{new("mdt", ...)} adds below the lowest age
#'   supplied (e.g. ages 0-49 for a table given from age 50), whose
#'   decrements are all attributed to the first cause: pass \code{x}
#'   explicitly to restrict the result to the ages actually supplied.
#' @param t Period (default 1).
#'
#' @return A numeric matrix with one row per age and one column per decrement.
#'   Row names are the ages, column names the decrement identifiers.
#'
#' @seealso \code{\link{qxt.prime.fromMdt}} for a single age/decrement pair,
#'   \code{\link{buildMdtFromIndependentRates}} for the inverse operation.
#'
#' @examples
#' valdezDf <- data.frame(
#'   x = 50:54,
#'   lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
#'   heart = c(5168, 5363, 5618, 5929, 6277),
#'   accidents = c(1157, 1206, 1443, 1679, 2152),
#'   other = c(4293, 5162, 5960, 6840, 7631))
#' valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)
#'
#' # ASDT matrix on the ages actually supplied
#' independentRatesFromMdt(valdezMdt, x = 50:54)
#'
#' # Subset of ages
#' independentRatesFromMdt(valdezMdt, x = 50:52)
#'
#' @export
independentRatesFromMdt <- function(object, x, t = 1) {
  if (!is(object, "mdt")) stop("Error! Need an mdt object")
  decrements <- getDecrements(object)
  allAges <- object@table$x
  # by default, use all ages except the terminal one (which has 0 survivors remaining)
  if (missing(x)) {
    x <- allAges[-length(allAges)]
  } else {
    bad <- x[!(x %in% allAges)]
    if (length(bad) > 0)
      stop("Ages not in table: ", paste(bad, collapse = ", "))
  }
  out <- matrix(NA_real_, nrow = length(x), ncol = length(decrements),
                dimnames = list(as.character(x), decrements))
  for (j in decrements) {
    for (i in seq_along(x)) {
      out[i, j] <- qxt.prime.fromMdt(object = object, x = x[i], t = t,
                                       decrement = j)
    }
  }
  out
}


#' Build an mdt object from a matrix of independent (ASDT) rates
#'
#' \code{buildMdtFromIndependentRates} constructs a multiple-decrement table
#' (\code{\linkS4class{mdt}}) from a matrix of independent single-decrement rates
#' \eqn{q'^{(j)}_x}, the inverse of \code{\link{independentRatesFromMdt}}.
#'
#' @details
#' For each age the combined (absolute) rate of decrement \eqn{j} is obtained by
#' the UDD-based integration formula
#' \deqn{q^{(j)}_x = q'^{(j)}_x \int_0^1 \prod_{i \ne j} \bigl(1 - s\,q'^{(i)}_x\bigr)\,ds,}
#' which is the same formula used by \code{\link{qxt.fromQxprime}} for a single
#' age.  The resulting absolute rates are multiplied by \eqn{l^{(\tau)}_x} to
#' obtain the decrement counts, and the survivorship column is computed
#' recursively from \eqn{p^{(\tau)}_x = \prod_j (1 - q'^{(j)}_x)}.
#'
#' @param x Integer vector of ages (must be consecutive and start at 0 after
#'   internal completion by \code{new("mdt", ...)}).  If missing, ages
#'   \code{0:(nrow(qx.primes)-1)} are used.
#' @param qx.primes Numeric matrix of independent rates.  Rows correspond to
#'   ages in \code{x}, columns to decrement causes. Column names, if any, become
#'   the decrement identifiers in the resulting table; otherwise generic names
#'   \code{d1, d2, \ldots} are used.
#' @param radix Radix (initial cohort size). Default 100 000.
#' @param name Character string for the table name. Default \code{"ASDT-built mdt"}.
#'
#' @return An \code{mdt} object.
#'
#' @seealso \code{\link{independentRatesFromMdt}} for the reverse extraction,
#'   \code{\link{qxt.fromQxprime}} for the single-age formula.
#'
#' @examples
#' # Finan (2014) Example 67.4:
#' # Three decrements (death, disability, retirement) at ages 60-61.
#' qp <- matrix(c(0.010, 0.030, 0.100,
#'                 0.013, 0.050, 0.200), nrow = 2, byrow = TRUE,
#'               dimnames = list(NULL, c("death", "disability", "retirement")))
#' mdt674 <- buildMdtFromIndependentRates(x = 60:61, qx.primes = qp,
#'                                         radix = 1000, name = "Finan 67.4")
#' print(mdt674)
#'
#' @export
buildMdtFromIndependentRates <- function(x, qx.primes, radix = 100000,
                                          name = "ASDT-built mdt") {
  if (!is.matrix(qx.primes))
    qx.primes <- as.matrix(qx.primes)
  nAges <- nrow(qx.primes)
  nDecr <- ncol(qx.primes)
  if (nDecr < 1) stop("At least one decrement column is required")
  if (missing(x)) x <- seq(0L, nAges - 1L)
  if (length(x) != nAges)
    stop("Length of 'x' must equal nrow(qx.primes)")
  # Column names for decrements
  dnames <- if (!is.null(colnames(qx.primes))) colnames(qx.primes) else
    paste0("d", seq_len(nDecr))

  # Compute absolute rates from the independent rates using qxt.fromQxprime()
  qx.abs <- matrix(0, nrow = nAges, ncol = nDecr)
  for (i in seq_len(nAges)) {
    for (j in seq_len(nDecr)) {
      others <- qx.primes[i, -j, drop = TRUE]
      qx.abs[i, j] <- qxt.fromQxprime(qx.prime = qx.primes[i, j],
                                        other.qx.prime = others, t = 1)
    }
  }

  # Build lx and dx columns
  lx <- numeric(nAges)
  lx[1] <- radix
  for (i in seq_len(nAges - 1)) {
    ptau <- prod(1 - qx.primes[i, ])
    lx[i + 1] <- lx[i] * ptau
  }

  dx <- matrix(0, nrow = nAges, ncol = nDecr)
  for (i in seq_len(nAges)) {
    dx[i, ] <- lx[i] * qx.abs[i, ]
  }

  tbl <- data.frame(x = x, lx = lx)
  for (j in seq_len(nDecr)) {
    tbl[[dnames[j]]] <- dx[, j]
  }
  new("mdt", name = name, table = tbl)
}


#MDT ACTUARIAL FUNCTIONS

#' @title Multiple decrement insurances and annuities
#' @rdname  multidecrins
#'
#' @description \code{Axn.mdt} gives the actuarial present value (APV) of a
#'   term insurance on a multiple decrement table, paying at the end of the
#'   year of decrement a benefit that may depend on the cause of decrement.
#'   \code{axn.mdt} gives the APV of an annuity payable while the insured is
#'   still in the active (no decrement yet) state.
#'
#' @param object an \code{mdt} object
#' @param x policyholder's age (an integer age tabulated in \code{object})
#' @param n contract duration in years. If missing, the cover runs to the end
#'   of the table, i.e. \eqn{n = \omega + 1 - x - m}.
#' @param i interest rate
#' @param decrement decrement(s) covered: one or more names (or column
#'   indices) among \code{getDecrements(object)}. If missing, every decrement
#'   is covered (insurance on the total decrement \eqn{(\tau)}).
#' @param benefits benefit amounts, one per element of \code{decrement}
#'   (recycled). Default 1, i.e. a unit benefit for every covered cause.
#' @param m deferment period in years (default 0).
#' @param k number of annuity payments per year (default 1). Survival
#'   probabilities at fractional durations are interpolated linearly in
#'   \eqn{l^{(\tau)}_x}.
#' @param payment \code{"advance"} (or \code{"due"}, default) or
#'   \code{"arrears"} (or \code{"immediate"}).
#'
#' @details With \eqn{v = (1+i)^{-1}} the insurance APV is
#' \deqn{\sum_{j \in J} b_j \sum_{h=0}^{n-1} v^{m+h+1}\,{}_{m+h}p^{(\tau)}_x\, q^{(j)}_{x+m+h},}
#' which for a single cause reduces to the historical \code{Axn.mdt}. The
#' annuity APV is \eqn{\frac1k \sum_{h} v^{t_h}\, {}_{t_h}p^{(\tau)}_x}, with
#' payment times \eqn{t_h = m, m+1/k, \ldots, m+n-1/k} (in advance) or
#' \eqn{m+1/k, \ldots, m+n} (in arrears). Benefit premiums and reserves follow
#' from the equivalence principle as ratios/differences of the two.
#'
#' @return A numeric vector of APVs (one per element of \code{x}).
#'
#' @references Finan, M. B. (2014). \emph{A Reading of the Theory of Life
#'   Contingency Models: A Preparation for Exam MLC/3L}, Sections 68-69.
#'
#' @examples
#' # Finan (2014), Example 69.1: 3-year term on (16), i = 10%
#' myTable <- data.frame(x = 16:18, lx = c(20000, 17600, 14520),
#'                       da = c(1300, 1870, 2380), doc = c(1100, 1210, 1331))
#' myMdt <- new("mdt", table = myTable, name = "Finan 69.1")
#' A <- Axn.mdt(myMdt, x = 16, n = 3, i = 0.10, decrement = "doc")
#' a <- axn.mdt(myMdt, x = 16, n = 3, i = 0.10)
#' 20000 * A / a   # level annual premium: 1250
#'
#' # Finan (2014), Example 68.1: benefit 1 for cause 1, 2 for cause 2
#' t681 <- data.frame(x = 50:51, lx = c(1200, 800),
#'                    d1 = c(100, 200), d2 = c(300, 300))
#' m681 <- new("mdt", table = t681)
#' Axn.mdt(m681, x = 50, n = 2, i = 0.5, decrement = c("d1", "d2"),
#'         benefits = c(1, 2))  # 0.6852
#'
#' @export
Axn.mdt <- function(object, x, n, i, decrement, benefits = 1, m = 0) {
  if (!is(object, "mdt")) stop("Error! Needed Mdt")
  if (missing(x)) stop("Error! Missing x")
  if (missing(i)) stop("Error! Missing interest rate i")
  decrement <- if (missing(decrement)) getDecrements(object) else
    .mdtDecrementNames(object, decrement)
  if (length(benefits) > length(decrement))
    stop("Error! More benefits than decrements")
  benefits <- rep(benefits, length.out = length(decrement))
  if (missing(n)) n <- getOmega(object) + 1 - x - m
  nn <- max(length(x), length(n), length(m))
  x <- rep(x, length.out = nn); n <- rep(n, length.out = nn)
  m <- rep(m, length.out = nn)
  if (any(n < 0 | m < 0)) stop("Error! Check n or m")

  one <- function(x, n, m) {
    if (n == 0) return(0)
    seqk <- m + seq(from = 0, to = n - 1, by = 1) # period start
    times <- seqk + 1                              # end-of-year payments
    # pxt()/qxt() are vectorised over t and x: one call per decrement.
    pxk <- pxt(object = object, x = x, t = seqk)
    qmat <- vapply(decrement, function(d)
      qxt(object = object, x = x + seqk, t = 1, decrement = d),
      numeric(length(seqk)))
    probs <- pxk * as.numeric(matrix(qmat, nrow = length(seqk)) %*% benefits)
    presentValue(cashFlows = rep(1, length(times)), timeIds = times,
                 interestRates = i, probabilities = probs, power = 1)
  }
  unname(mapply(one, x, n, m))
}

#' @rdname multidecrins
#' @export
axn.mdt <- function(object, x, n, i, m = 0, k = 1, payment = "advance") {
  if (!is(object, "mdt")) stop("Error! Needed Mdt")
  if (missing(x)) stop("Error! Missing x")
  if (missing(i)) stop("Error! Missing interest rate i")
  payment <- testpaymentarg(payment)
  if (length(k) != 1 || !is.finite(k) || k <= 0)
    stop("k must be a finite positive scalar")
  if (missing(n)) n <- getOmega(object) + 1 - x - m
  nn <- max(length(x), length(n), length(m))
  x <- rep(x, length.out = nn); n <- rep(n, length.out = nn)
  m <- rep(m, length.out = nn)
  if (any(n < 0 | m < 0)) stop("Error! Check n or m")

  one <- function(x, n, m) {
    npay <- round(n * k)
    if (npay == 0) return(0)
    steps <- seq_len(npay) / k
    times <- if (payment == "due") m + steps - 1 / k else m + steps
    probs <- pxt(object = object, x = x, t = times)
    sum((1 + i)^(-times) * probs) / k
  }
  unname(mapply(one, x, n, m))
}


#' Convert an mdt to long (time, status) format for survival analysis
#'
#' \code{mdtToLong} reshapes a multiple decrement table into an aggregated
#' long data set with one row per (exit time, cause) and a \code{count}
#' column, ready for competing-risks tools such as
#' \code{survival::survfit(Surv(time, status) ~ 1, weights = count)}
#' (Aalen-Johansen cumulative incidence) or for any analysis based on
#' \code{survival::Surv}.
#'
#' @param object an \code{mdt} object.
#' @param x entry age of the cohort (default: the lowest tabulated age, 0).
#' @param t length of the follow-up in years (default: to the end of the
#'   table). Lives still in the table at \code{x + t} are right censored.
#' @param exitTime where in the year of age decrements are placed:
#'   \code{"end"} (default, time \eqn{k+1} for exits in year \eqn{k}) or
#'   \code{"mid"} (time \eqn{k + 1/2}, consistent with UDD).
#' @param dropZero logical: drop rows with zero count (default \code{TRUE}).
#'
#' @return A \code{data.frame} with columns \code{time} (years since age
#'   \code{x}), \code{age} (age at exit or censoring), \code{status} (a
#'   factor whose first level, \code{"censored"}, is followed by the
#'   decrement names, as expected by \code{survival::Surv} for multi-state
#'   data) and \code{count} (number of lives, possibly non-integer).
#'
#' @details With \code{exitTime = "end"} the Aalen-Johansen estimate of the
#' cumulative incidence of cause \eqn{j} at time \eqn{k} computed on the
#' weighted long data coincides with \code{qxt(object, x, k, decrement = j)};
#' see the multiple decrement vignette.
#'
#' @examples
#' valdezDf <- data.frame(
#'   x = 50:54,
#'   lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
#'   heart = c(5168, 5363, 5618, 5929, 6277),
#'   accidents = c(1157, 1206, 1443, 1679, 2152),
#'   other = c(4293, 5162, 5960, 6840, 7631))
#' valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)
#' long <- mdtToLong(valdezMdt, x = 50, t = 5)
#' head(long)
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   fit <- survival::survfit(survival::Surv(time, status) ~ 1,
#'                            data = long, weights = count)
#'   summary(fit, times = 1:5)$pstate
#' }
#'
#' @export
mdtToLong <- function(object, x, t, exitTime = c("end", "mid"),
                      dropZero = TRUE) {
  if (!is(object, "mdt")) stop("Error! Need an mdt object")
  exitTime <- match.arg(exitTime)
  tbl <- object@table
  ages <- tbl$x
  if (missing(x)) x <- min(ages)
  if (length(x) != 1 || !(x %in% ages))
    stop("Error! x must be a single age tabulated in the mdt")
  if (missing(t)) t <- getOmega(object) + 1 - x
  if (length(t) != 1 || t < 0 || t %% 1 != 0)
    stop("Error! t must be a single non-negative integer")
  decrements <- getDecrements(object)
  rows <- which(ages >= x & ages < x + t)
  offset <- if (exitTime == "end") 1 else 0.5
  nr <- length(rows)
  long <- data.frame(
    time = rep(ages[rows] - x + offset, times = length(decrements)),
    age = rep(ages[rows] + offset, times = length(decrements)),
    status = rep(decrements, each = nr),
    count = unlist(lapply(decrements, function(d) tbl[[d]][rows]),
                   use.names = FALSE),
    stringsAsFactors = FALSE
  )
  # survivors still in the table at x + t are censored there
  survivors <- if (x + t > getOmega(object)) 0 else tbl$lx[ages == x + t]
  long <- rbind(long, data.frame(time = t, age = x + t, status = "censored",
                                 count = survivors,
                                 stringsAsFactors = FALSE))
  if (dropZero) long <- long[long$count > 0, , drop = FALSE]
  long$status <- factor(long$status, levels = c("censored", decrements))
  long <- long[order(long$time, long$status), , drop = FALSE]
  rownames(long) <- NULL
  long
}
