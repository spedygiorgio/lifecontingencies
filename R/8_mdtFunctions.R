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
		if (is.numeric(decrement)) decrement<-getDecrements(object)[decrement]
		# Guard against a mistyped/unknown decrement name: without this check
		# decrement.cols would silently be integer(0) and the function would
		# return 0 instead of signalling the error, unlike pxt()/qxt() which
		# already validate decrement via stopifnot().
		if (!(decrement %in% names(object@table)))
			stop("Error! Not recognized decrement type")
		decrement.cols<-which(names(object@table)==decrement)
	}
		ages2consider<-x+0:(time-1)
		age.rows<-which(object@table$x %in% ages2consider)
		out<-sum(object@table[age.rows,decrement.cols])
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
#'   tabulated in \code{object} except the last (which has no decrement data).
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
#' # Full ASDT matrix
#' independentRatesFromMdt(valdezMdt)
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
#' (\code{\link{mdt}}) from a matrix of independent single-decrement rates
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

#' @title Multiple decrement life insurance
#' @rdname  multidecrins
#' 
#' @description Function to evaluate multiple decrement insurances
#'
#' @param object an \code{mdt} or \code{actuarialtable} object
#'
#' @param x policyholder's age
#' @param n contract duration
#' @param i interest rate
#' @param decrement decrement category 
#'
#' @return The scalar representing APV of the insurance
#' 
#' @section Warning: The function is experimental and very basic. Testing is still needed. Use at own risk!
#' 
#' @examples 
#' #creates a temporary mdt
#' myTable<-data.frame(x=41:43,lx=c(800,776,752),d1=rep(8,3),d2=rep(16,3))
#' myMdt<-new("mdt",table=myTable,name="ciao")
#' Axn.mdt(myMdt, x=41,n=2,i=.05,decrement="d2")


Axn.mdt<-function(object,x,n,i, decrement) {
  if (missing(n)) n <- getOmega(object)-x-1
  if (missing(decrement)) return(Axn(actuarialtable = object, x=x, n=n,i=i))
  
  if (!is(object,'mdt')) stop("Error! Needed Mdt")
  if (!(decrement %in% getDecrements(object))) stop("Error! Not recognized decrement type")
  
  seqk <- seq(from=0, to=n-1, by=1) #period start
  times <- 1+seqk #period when payments are due
  payments<-rep(1,length(times)) #payment sequence
  seqx <- x+seqk
  
  # pxt()/qxt() already accept a full vector of ages/times (they recycle
  # x and t to a common length internally), so a single vectorised call
  # replaces length(seqk) redundant scalar calls -- same pattern already
  # used in IAxn()/DAxn() (R/5_actuarialFunctions.R).
  pxk <- pxt(object=object, x=x, t=seqk)
  qxkp1 <- qxt(object=object, x=(x+seqk), t=1, decrement=decrement)
  probs <- pxk * qxkp1
  out<-presentValue(cashFlows=payments, timeIds=times, interestRates=i, probabilities=probs,power=1)
  return(out)
}

