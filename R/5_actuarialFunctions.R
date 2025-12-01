#############################################################################
#   Copyright (c) 2018 Giorgio A. Spedicato, Christophe Dutang
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
###         actuarial functions
###




#' Endowment, insurance, pure endowment, and survival annuity APVs (shared topic)
#'
#' This help page groups four classical life-contingency present values:
#' \itemize{
#'   \item \strong{Exn}: pure endowment, pays 1 at time \code{n} if alive.
#'   \item \strong{Axn}: term/whole life insurance, pays 1 at death within \code{n} years
#'         (or up to limiting age if \code{n} is missing), with fractional claim timing.
#'   \item \strong{AExn}: n-year endowment insurance, i.e. \code{Axn + Exn}.
#'   \item \strong{axn}: survival annuity (immediate/due), with deferment \code{m} and \code{k} payments per year.
#' }
#'
#' @section \code{Exn} — Pure endowment:
#' Computes the actuarial present value (APV) of a pure endowment that pays 1 at
#' time \code{n} provided survival to \code{x+n}.
#'
#' @param actuarialtable A \code{lifetable} or \code{actuarialtable} object.
#' @param x Attained age at inception.
#' @param n Contract length in years. If missing, for \code{Exn} and \code{Axn} it is set to
#'   \code{pmax(ceiling((getOmega(actuarialtable)+1 - x - m)*k)/k, 0)}; for \code{AExn} it defaults to
#'   \code{getOmega(actuarialtable) - x - 1}. (See function-specific details below.)
#' @param i Annual effective interest rate. Defaults to \code{actuarialtable@interest}.
#' @param type Output type: \code{"EV"} (expected value, default) or \code{"ST"} (one stochastic
#'   realization via \code{rLifeContingencies}).
#' @param power Power of the discounted payoff before expectation (default 1).
#'
#' @return A numeric value (or vector for vectorized inputs): the APV in expected value,
#'   or one simulated realization when \code{type="ST"}.
#'
#' @details
#' \strong{Exn}: \eqn{E_x^n = v^n \, {}_np_x} with \eqn{v=(1+i)^{-1}}.
#'
#' \strong{Axn}: With fractional claims,
#' \deqn{A_{\overline{n}|}^{(k)} = \sum_{j=1}^{nk} v^{t_j + 1/k}\; {}_{t_j}p_x\; q_{x+t_j}^{(1/k)},}
#' where \eqn{t_j = m + (j-1)/k}, computed via \code{pxt(\dots)} and \code{qxt(\dots, t=1/k)}.
#'
#' \strong{AExn}: returns \code{Axn(...) + Exn(...)} with aligned arguments.
#'
#' \strong{axn}: Survival annuity with payment timing \code{"immediate"} (arrears) or \code{"due"} (advance),
#' deferment \code{m} and \code{k} payments per year (see function-specific parameters).
#'
#' @references
#' Bowers, N. L., Gerber, H. U., Hickman, J. C., Jones, D. A., Nesbitt, C. J. (1997).
#' \emph{Actuarial Mathematics}, 2nd ed., SOA.
#'
#' @seealso \code{\link{Axn}}, \code{\link{AExn}}, \code{\link{axn}}, \code{\link{Exn}}
#'
#' @examples
#' ## Common setup used in legacy docs
#' data(soaLt)
#' soa08Act <- with(soaLt, new("actuarialtable", interest=0.06, x=x, lx=Ix, name="SOA2008"))
#'
#' ## Exn (pure endowment)
#' Exn(soa08Act, x=30, n=35)
#'
#' ## Axn (term / whole life insurance)
#' # 10-year term, semiannual claims:
#' Axn(soa08Act, x=50, n=10, k=2)
#' # Whole life (n inferred), monthly:
#' Axn(soa08Act, x=30, k=12)
#'
#' ## AExn = Axn + Exn  (legacy book-check)
#' AExn(soa08Act, x=35, n=30, i=0.06)
#' Exn(soa08Act, x=35, n=30, i=0.06) + Axn(soa08Act, x=35, n=30, i=0.06)
#'
#' ## axn (survival annuity, legacy example)
#' # Life-long annuity for age 65:
#' axn(soa08Act, x=65)
#'
#' @family life-contingency APVs
#' @name endowment_trio
#' @aliases Exn Axn AExn axn
#' @rdname endowment_trio
#' @export
Exn <- function(actuarialtable, x, n, i = actuarialtable@interest, type = "EV", power = 1)
  {
    interest <- i
    out <- numeric(1)
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable") #request an actuarial actuarialtable
    type <- testtyperesarg(type)
    prob = pxt(actuarialtable,x,n)
    #discount = (1 + interest) ^ (-n)
    #defines the outputs
    if (type == "EV")
      out = presentValue(
        cashFlows = 1, timeIds = n,
        interestRates = interest, probabilities = prob,power = power
      )
    else if (type == "ST")
      out = rLifeContingencies(
        n = 1,lifecontingency = "Exn",
        object = actuarialtable, x = x,t = n,i = interest, m = 1,k = 1
      )
    #out=discount^2*prob*(1-prob)
    return(out)
}


#' @rdname endowment_trio
#'
#' @param payment Payment timing for annuities: \code{"advance"} (aka due) or \code{"immediate"} (aka arrears). (axn)
#' @details
#' \strong{axn} — Survival annuity (immediate/due), with deferment \code{m} and \code{k} fractional payments.
#' For \code{type="EV"} the annuity is computed as
#' \deqn{a_{\overline{n}|}^{(k)} = \sum_{j=1}^{nk} \frac{1}{k}\, v^{t_j}\, {}_{t_j}p_x,}
#' where \eqn{t_j} are the payment times depending on \code{payment} and \code{m}.
#'
#' @examples
#' ## axn specific legacy examples
#' # Immediate (arrears) vs due (advance), quarterly, 15-year term deferred 5 years:
#' axn(soa08Act, x=60, n=15, m=5, k=4, payment="immediate")
#' axn(soa08Act, x=60, n=15, m=5, k=4, payment="due")
#' # Vectorization over x/n:
#' axn(soa08Act, x=c(60,65), n=c(10,20), k=12, payment="due")
#'
#' @export
axn <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
          k = 1, type = "EV", power = 1, payment = "advance", ...)
{
  # Input validation: check actuarialtable class
  if (!(class(actuarialtable) %in% c("lifetable","actuarialtable")))
  stop("Error! Only lifetable, actuarialtable classes are accepted")
  
  # Validate and normalize type and payment arguments
  type <- testtyperesarg(type)
  payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
  
  # Check required argument x
  if (missing(x))
  stop("Missing x argument")
  
  # Ensure k is scalar
  if(length(k) > 1)
  {
  k <- k[1]
  warning("k should be of length 1, it takes the first value")
  }
  
  # Set default values for optional arguments
  if (missing(m))
  m <- 0
  if (missing(n))
  {
  # Default n: remaining lifetime from x+m, put zero if x+m > omega+1
  n <- pmax(ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k, 0)
  }
  
  # Basic parameter checks
  if (any(x < 0, n < 0, m < 0))
  stop("Check x, n or m")
  if(length(x) <= 0)
  stop("x is of length zero")
  
  # Recycle arguments to match maximum length
  ntot <- max(length(n), length(x), length(m))
  if(length(n) != length(x) || length(n) != length(m))
  {
  if(length(m) != ntot)
    warnings("m argument has been recycled to match the maximum length of x, m and n")
  if(length(n) != ntot)
    warnings("n argument has been recycled to match the maximum length of x, m and n")
  if(length(x) != ntot)
    warnings("x argument has been recycled to match the maximum length of x, m and n")
  m <- rep(m, length.out=ntot)
  n <- rep(n, length.out=ntot)
  x <- rep(x, length.out=ntot)
  if(any(is.infinite(x), is.infinite(n), is.infinite(m)))
    stop("infinite values provided in x, n or m")
  if(any(x < 0, n < 0, m < 0))
    stop("(strictly) negative values provided in x, n or m")
  }
  
  # Compute expected value or stochastic realization
  if (type == "EV")
  {
  # Helper: compute immediate annuity for j-th element
  single_axn_immediate <- function(j)
  {  
    if(n[j] <= 0)
    return(0)
    # Payments in arrears: at times 1/k, 2/k, ..., n
    payments <- rep(1 / k, n[j] * k)
    times <- m[j] + seq(from = 1/k, to = n[j], by = 1/k)
    probs <- pxt(actuarialtable, x[j], times, ...)
    presentValue(payments, times, i, probs, power)
  }
  
  # Helper: compute due annuity for j-th element
  single_axn_due <- function(j)
  {  
    if(n[j] <= 0)
    return(0)
    # Payments in advance: at times 0, 1/k, ..., n-1/k
    payments <- rep(1 / k, n[j] * k)
    times <- m[j] + seq(from = 0, to = n[j]-1/k, by = 1/k)
    probs <- pxt(actuarialtable, x[j], times, ...)
    presentValue(payments, times, i, probs, power)
  }
  
  # Select payment timing and compute for all elements
  if(payment == "immediate")
    out <- sapply(1:ntot, single_axn_immediate)
  else if(payment == "due")
    out <- sapply(1:ntot, single_axn_due)
  else
    stop("wrong payment type")
    
  } else if (type == "ST") {
  # Stochastic simulation: generate random realizations
  rng_axn <- function(j)
    rLifeContingencies(
    n = 1, lifecontingency = "axn", object = actuarialtable, 
    x = x[j], t = n[j], i = i, m = m[j], k = k, payment=payment)
  out <- sapply(1:ntot, rng_axn)
  
  } else
  stop("wrong result type")
  
  out  
}




#shall write the Rd file
axyn <- function(tablex, tabley, x,y, n,i, m,k = 1, status = "joint", type = "EV", payment =
             "advance")
  {
    .Deprecated("axyzn")
    out <- numeric(1)
    if (missing(tablex))
      stop("Error! Need table for X life")
    if (missing(tabley))
      stop("Error! Need table for Y life")
    if (missing(x))
      stop("Error! Need age for X!")
    if (missing(y))
      stop("Error! Need age for Y!")
    type <- testtyperesarg(type)
    payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
    status <- teststatusarg(status)
    
    if (missing(m))
      m = 0
    if (missing(n))
      n = max(getOmega(tablex) - x,getOmega(tabley) - y) - m #maximum sequence of payments
    if (tablex@interest != tabley@interest) {
      warning("Warning! Intesters differ between tablex and tabley. Using average")
    }
    if (!missing(i))
      interest = i
    else
      interest = 0.5 * (tablex@interest + tabley@interest) #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
    if (any(x < 0,y < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    #computation of quantities, assuming fractional payments
    payments = rep(1 / k,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    if (payment == "immediate")
      times = times + 1 / k
    
    xVec = c(x,y)
    tablesList = list(tablex, tabley)
    
    for (i in 1:length(times))
      probs[i] = pxyzt(
        tablesList = tablesList,x = xVec,
        t = times[i],status = status
      )
    #discounts=(1+actuarialtable@interest)^-times
    #out<-sum(payments*discounts*probs)
    if (type == "EV") {
      out <-
        presentValue(
          cashFlows = payments, timeIds = times, interestRates = interest, probabilities =
            probs
        )
    } else	if (type == "ST") {
      out = 0
      for (i in 1:length(times))
        out = out + 1 / k * rbinom(n = 1, size = 1, prob = probs[i]) * (1 + interest) ^
          -times[i]
    }
    return(out)
  }


axyznvect <- function(tablesList, x, n, i, m, k = 1, status = "joint", type = "EV",
                      power =1, payment = "advance", ...)
{
  #initial checkings
  numTables = length(tablesList)
  if (missing(x))
    stop("Missing x")
  if(!is.numeric(x))
    stop("Error! x should be a numeric vector or a numeric matrix")
  else if(is.vector(x))
  {  
    if(length(x) == 1)
      x <- rep(x, length.out=numTables)
    if (length(x) != numTables)
      stop("Error! Initial ages vector length does not match with number of lives")
  }else if(is.matrix(x))
  {
    if(NCOL(x) != numTables)
      stop("Error! Age matrix colum number does not match with number of lives")
  }else
    stop("Error! x should be a numeric vector or a numeric matrix")
  
  if(is.vector(x))
    x <- t(as.matrix(x))
  #from here, x is a matrix where NCOL(x) == numTables
  nbcomput <- NROW(x)
  
  classlist <- sapply(tablesList, class)
  if(any(!classlist %in% c("lifetable", "actuarialtable")))
    stop("Error! A list of lifetable objects is required")
  
  type <- testtyperesarg(type)
  payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
  status <- teststatusarg(status)
  
  #class(object) %in% c("lifetable","actuarialtable") 
  if(length(k) > 1)
  {
    k <- k[1]
    warnings("k should be of length 1, it takes first value")
  }
  if (missing(m))
    m <- 0
  if (any(x < 0, m < 0))
    stop("Check x, n or m")
  
  if(length(x) <= 0)
    stop("x is of length zero")
  if(length(t) <= 0)
    stop("t is of length zero")
  
  missingn <- missing(n)
  if(!missingn)
  {
    if(length(n) <= 0)
      stop("n is of length zero")
    if(!is.numeric(n))
      stop("Error! n should be a numeric vector")
  }else
  {
    computn <- function(idrow)
    {
      getn <- function(j)
        ceiling((getOmega(tablesList[[j]]) + 1 - x[idrow, j] - m) * k) / k 
      myn <- max(sapply(1:numTables, getn))
    }
    n <- sapply(1:nbcomput, computn)
  }
  
  #recycle some parameters
  if(length(m) != nbcomput)
  {
    m <- rep(m, length.out = nbcomput)
    warnings("m argument has been recycled to match the row number of x")
  }
  if(length(k) != nbcomput)
  {
    k <- rep(k, length.out = nbcomput)
    warnings("k argument has been recycled to match the row number of x")
  }
  if(length(n) != nbcomput)
  {
    n <- rep(n, length.out = nbcomput)
    warnings("n argument has been recycled to match the row number of x")
  }
  
  
  if (!missing(i))
  {
    if(!is.numeric(i))
      stop("Error! i should be a numeric vector")
    interest = i
  }else 
  {
    interest <- sapply(1:numTables, function(j) tablesList[[j]]@interest)
  }
  if(length(interest) > 1)
  {
    interest <- mean(interest)
    warnings("i argument is the average value of interest values")
  }
  
  
  #computation of quantities, assuming fractional payments
  allpayments <- matrix(1 / k, nrow=nbcomput, ncol=n * k)
  
  computtime <- function(idrow)
  {
    if(payment == "immediate")
    {  
      times <- m[idrow] + seq(from = 1/k, to = n[idrow], by = 1/k)
    }else if(payment == "due")
    {  
      times <- m[idrow] + seq(from = 0, to = n[idrow]-1/k, by = 1/k)
    }
    else
      stop("wrong payment type")
    times
  }
  alltime <- t(sapply(1:nbcomput, computtime))
  
  computprob <- function(idrow)
  {
    #replicate time over lifetables
    valt <- replicate(numTables, alltime[idrow,])
    valx <- x[idrow,]
    pxyzt(tablesList, valx, valt, status=status, ...)
  }
  allprob <- t(sapply(1:nbcomput, computprob))
  if(NROW(allprob) != nbcomput)
    stop("wrong probabilities computed")
  
  res <- sapply(1:nbcomput, function(idrow)
    presentValue(allpayments[idrow,], alltime[idrow,], interest, allprob[idrow,], power))
  
  res
}

axyzn <- function(tablesList, x, n,i, m,k = 1, status = "joint", type = "EV",power =
             1, payment = "advance")
  {
    out <- numeric(1)
    #initial checkings
    numTables = length(tablesList)
    if (length(x) != numTables)
      stop("Error! Initial ages vector length does not match with number of lives")
    for (j in 1:numTables) {
      if (!(class(tablesList[[j]]) %in% c("lifetable", "actuarialtable")))
        stop("Error! A list of lifetable objects is required")
    }
    type <- testtyperesarg(type)
    payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
    status <- teststatusarg(status)
    
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(m))
      m = 0
    
    if (missing(n)) {
      n = 0
      for (j in 1:numTables)
        n = (max(n,(getOmega(tablesList[[j]]) - x[j])))
      n = ceiling((n+1 - m)*k)/k
      #n=n-m-1 patch by Reinhold
    }
    
    if (!missing(i))
      interest = i
    else {
      temp = 0
      for (j in 1:numTables)
        temp = temp + tablesList[[j]]@interest
      interest = temp / numTables
    }
    
    if (any(x < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    #computation of quantities, assuming fractional payments
    payments = rep(1 / k,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    if (payment == "immediate")
      times = times + 1 / k
    for (j in 1:length(times))
      probs[j] = pxyzt(
        tablesList = tablesList,x = x,
        t = times[j],status = status
      )
    discounts = (1 + interest) ^ -times #prima asteriskato
    #out<-sum(payments*discounts*probs)
    if (type == "EV") {
      out <-
        presentValue(
          cashFlows = payments, timeIds = times, interestRates = interest, probabilities =
            probs,power = power
        )
      #out=.C("add3", x=as.double(payments), y=as.double(discounts),z=as.double(probs),n=as.integer(length(probs)),out=numeric(1))$out
    } else	if (type == "ST") {
      out = rLifeContingenciesXyz(
        n = 1,lifecontingency = "axyz", tablesList = tablesList, x = x,t = n,i =
          i, m = m,k = k, status = status, payment = payment
      )
    }
    return(out)
  }

#tablesList=list(soa08Act,soa08Act);x=c(67,65);n=20;k=11

# objectX=soa08Act
# objectY=soa08Act
# tx=65
# ty=63
# n=10

# aX=axn(soa08Act, x=tx, n=n)
# aY=axn(soa08Act, x=ty, n=n)
# axynJ=axyn(tablex=objectX,tabley=objectY, x=tx, y=ty,n=n, status="joint")
# axynL=axyn(objectX,objectY, x=tx,  y=ty,n=n, status="last", type="EV")
# axynL
# (aX+aY-axynJ)-axynL #minimum departure

#function to obtain the Life Insurance
#actuarialtable: an actuarial actuarialtable object
#x: beginnin life age
#m: deferring term
#type: output requested: default expected value



#' @rdname endowment_trio
#'
#' @param m Deferment (years). Default 0. Vector accepted. (Axn/axn)
#' @param k Fractional periods per year (\eqn{k \ge 1}). Default 1. Must be scalar. (Axn/axn/AExn insurance leg)
#' @param ... Extra args forwarded to mortality helpers (\code{pxt}, \code{qxt}), e.g. fractional assumptions. (Axn)
#'
#' @details
#' \strong{Axn} — Life insurance (term / whole life), fractional claim times.
#' Vectorized in \code{x}, \code{n}, \code{m}. \code{k} must be scalar.
#'
#' @export
Axn <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
                    k = 1, type = "EV", power = 1, ...)
{
  #checks
  if (!(class(actuarialtable) %in% c("lifetable","actuarialtable")))
    stop("Error! Only lifetable, actuarialtable classes are accepted")
  
  type <- testtyperesarg(type)
  
  #class(object) %in% c("lifetable","actuarialtable") 
  if (missing(x))
    stop("Missing x")
  if(length(k) > 1)
  {
    k <- k[1]
    warnings("k should be of length 1, it takes first value")
  }
  if (missing(m))
    m <- 0
  if (missing(n))
  {
    #put zero if x+m > omega+1
    n <- pmax(ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k, 0)
  }
  if (any(x < 0, n < 0, m < 0))
    stop("Check x, n or m")
  
  if(length(x) <= 0)
    stop("x is of length zero")
  if(length(t) <= 0)
    stop("t is of length zero")
  
  ntot <- max(length(n), length(x), length(m))
  if(length(n) != length(x) || length(n) != length(m))
  {
    if(length(m) != ntot)
      warnings("m argument has been recycled to match the maximum length of x, m and n")
    if(length(n) != ntot)
      warnings("n argument has been recycled to match the maximum length of x, m and n")
    if(length(x) != ntot)
      warnings("x argument has been recycled to match the maximum length of x, m and n")
    m <- rep(m, length.out=ntot)
    n <- rep(n, length.out=ntot)
    x <- rep(x, length.out=ntot)
    if(any(is.infinite(x), is.infinite(n), is.infinite(m)))
      stop("infinite values provided in x, n or m")
    if(any(x < 0, n < 0, m < 0))
      stop("(strictly) negative values provided in x, n or m")
  }
  
  
  if (type == "EV")
  {
    single_Axn <- function(j)
    {  
      if(n[j] <= 0)
        return(0)
      #the payment is fixed
      payments <- rep(1, n[j] * k)
      times <- m[j] + seq(from = 0, to = (n[j]-1/k), by = 1/k)
      probs <- pxt(actuarialtable, x=x[j], t=times, ...) * qxt(actuarialtable, x=x[j]+times, t=1/k, ...)
      presentValue(payments, timeIds=(times + 1/k), interestRates=i, 
                   probabilities=probs, power=power)
    }
    out <- sapply(1:ntot, single_Axn)
    
    
  } else if (type == "ST") {
    
    rng_Axn <- function(j)
      rLifeContingencies(
        n = 1, lifecontingency = "Axn", object = actuarialtable, 
        x = x[j], t = n[j], i = i, m = m[j], k = k)
    out <- sapply(1:ntot, rng_Axn)
  } else
    stop("wrong result type")
  out  
}

Axyn <- function(tablex, x,tabley, y, n,i, m, k = 1, status = "joint", type = "EV")
  {
    .Deprecated("Axyzn")
    out <- numeric(1)
    if (any(missing(tablex),missing(tabley)))
      stop("Error! Need tables")
    if (any(missing(x),missing(y)))
      stop("Error! Need ages!")
    type <- testtyperesarg(type)
    status <- teststatusarg(status)
    
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(m))
      m = 0
    if (missing(n))
      n = max(getOmega(tablex) - x,getOmega(tabley) - y) - m - 1
    
    if (tablex@interest != tabley@interest) {
      warning("Warning! Intesters differ between tablex and tabley. The average will be used")
    }
    if (!missing(i))
      interest = i
    else
      interest = 0.5 * (tablex@interest + tabley@interest) #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
    
    if (n == 0)
      return(0)
    if (any(x < 0,y < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    
    #perform calculations
    
    payments = rep(1,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    startAgex = x
    startAgey = y
    tablesList = list(tablex, tabley)
    for (i in 1:length(times))
      probs[i] = .qxyznt(
        tablesList = tablesList,x = c(startAgex,startAgey),n = times[i],t = 1 /
          k, status = status
      )
    discounts = (1 + interest) ^ -(times + 1 / k)
    
    if (type == "EV") {
      out <- sum(payments * discounts * probs)
    } else if (type == "ST") {
      out = 0
      for (i in 1:length(times))
      {
        out = ((1 + interest) ^ -(times[i] + 1 / k)) * rbinom(n = 1, size = 1, prob =
                                                                probs[i])
        if (out > 0)
          break
      }
    }
    #warning("Warning: this function in deprecated. Use Axyzn instead!")
    return(out)
  }


Axyzn <- function(tablesList, x, n,i, m, k = 1, status = "joint", type = "EV",power =
             1)
  {
    out = numeric(1)
    #initial checkings
    numTables = length(tablesList)
    if (length(x) != numTables)
      stop("Error! Initial ages vector length does not match with number of lives")
    for (j in 1:numTables) {
      if (!(class(tablesList[[j]]) %in% c("lifetable", "actuarialtable")))
        stop("Error! A list of lifetable objects is required")
    }
    type <- testtyperesarg(type)
    status <- teststatusarg(status)
    
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(m))
      m = 0
    
    if (missing(n)) {
      n = 0
      for (j in 1:numTables)
        n = (max(n,(getOmega(tablesList[[j]]) - x[j])))
      n = n - m - 1
#       n = n - m + 1 #Rosa patch
    }
    
    if (!missing(i))
      interest = i
    else {
      temp = 0
      for (j in 1:numTables)
        temp = temp + tablesList[[j]]@interest
      interest = temp / numTables
    }
    
    if (n == 0)
      return(0)
    if (any(x < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    
    #perform calculations
    
    payments = rep(1,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    
    
    for (j in 1:length(times))
      probs[j] = .qxyznt(
        tablesList = tablesList,x = x,n = times[j],t = 1 / k, status = status
      )
    discounts = (1 + interest) ^ -(times + 1 / k)
    
    if (type == "EV") {
      out <- sum(((payments * discounts) ^ power) * probs)
      #out=.C("add3", x=as.double(payments), y=as.double(discounts),z=as.double(probs),n=as.integer(length(payments)),out=numeric(1))$out
    } else if (type == "ST") {
      out = 0
      out = rLifeContingenciesXyz(
        n = 1,lifecontingency = "Axyz",
        tablesList = tablesList, x = x,t = n,i = i, m = m,k = k,status = status
      )
    }
    return(out)
  }

#Axyzn(tablesList, x, n, status="joint", type="ST")


# tablex=soa08Act
# tabley=soa08Act
# x=65
# y=63
# n=10
# i=0.06
# m=0
#
# Ax=Axn(soa08Act, x=x,n=n)
# Ay=Axn(soa08Act, x=y,n=n)
# AxynJ=Axyn(tablex=soa08Act, x=x,tabley=soa08Act, y=y, n=n,i=i, m=m, k=1, status="joint", type="EV")
# AxynL=Axyn(tablex=soa08Act, x=x,tabley=soa08Act, y=y, n=n,i=i, m=m, k=1, status="last", type="EV")
# AxynJ
# AxynL
#Ax+Ay-AxynJ


#n-year term whole life
#recursive function
# actuarialtable=soa08Act
# x=50
# n=10
# i=0.06
# m=0
# k=2
#' Life insurance with arithmetic-variation benefit (increasing/decreasing, fractional claims)
#'
#' This help page groups two term life insurances with **arithmetic variation** of the benefit,
#' both with optional deferment \code{m} and \code{k} fractional claim times (claims at end of subperiods):
#' \itemize{
#'   \item \strong{IAxn}: \emph{increasing} arithmetic term insurance (benefit grows linearly with time);
#'   \item \strong{DAxn}: \emph{decreasing} arithmetic term insurance (benefit declines linearly with time).
#' }
#'
#' @section \code{IAxn} — Increasing arithmetic term:
#' Computes the APV of an n-year **increasing** term insurance on a life aged \code{x}, with \code{k} fractional
#' claim times and optional deferment \code{m}. The benefit at the \eqn{j}-th subperiod equals \eqn{j/k}.
#'
#' @param actuarialtable A \code{lifetable} or \code{actuarialtable} object.
#' @param x Attained age at inception.
#' @param n Coverage length in years. If missing, it is set to \code{getOmega(actuarialtable) - x - m}.
#' @param i Annual effective interest rate. Defaults to \code{actuarialtable@interest}.
#' @param m Deferment (years). Default 0.
#' @param k Fractional periods per year (\eqn{k \ge 1}). Default 1.
#' @param type Output type: \code{"EV"} (expected value, default) or \code{"ST"} (one stochastic
#'   realization via \code{rLifeContingencies}).
#' @param power Power applied to discounted cash flows before expectation (default 1).
#'
#' @return A numeric value: the APV (or one simulated realization if \code{type="ST"}).
#'
#' @details
#' Let \eqn{t_j = m + (j-1)/k}, \eqn{j=1,\dots,nk}. With **fractional claims at end of subperiods**,
#' the EV implementations follow the pattern already used in \code{\link{Axn}}:
#'
#' \strong{IAxn} (increasing):
#' \deqn{ \mathrm{IA}_{\overline{n}|}^{(k)} = \sum_{j=1}^{nk} \Big(\frac{j}{k}\Big)\,
#'         v^{t_j + 1/k}\; {}_{t_j}p_x\; q_{x+t_j}^{(1/k)}, }
#' where \eqn{v=(1+i)^{-1}}, computed via \code{pxt(\dots)} and \code{qxt(\dots, t=1/k)}.
#'
#' \strong{DAxn} (decreasing) is analogous with benefit \eqn{(n - (j-1)/k)}; see its subsection below.
#'
#' @section Fractional timing conventions:
#' For **insurance** benefits in this package, fractional claims are assumed to occur at the
#' **end** of each subperiod (i.e., at \eqn{t_j + 1/k}). This matches the implementation that
#' multiplies survival to \eqn{t_j} and a fractional death probability over the next subperiod:
#' \deqn{ v^{t_j + 1/k}\; {}_{t_j}p_x\; q_{x+t_j}^{(1/k)}. }
#' By contrast, **annuities** use a payment-timing flag (\code{"immediate"} vs \code{"due"}) which
#' changes the evaluation times; insurance here has a fixed claim timing at end-subperiod.
#'
#' @references
#' Bowers, N. L., Gerber, H. U., Hickman, J. C., Jones, D. A., Nesbitt, C. J. (1997).
#' \emph{Actuarial Mathematics}, 2nd ed., SOA.
#'
#' @seealso \code{\link{Axn}} (level benefit), \code{\link{AExn}}, \code{\link{Exn}}, \code{\link{axn}}
#'
#' @examples
#' ## Setup (legacy examples)
#' data(soaLt)
#' soa08Act <- with(soaLt, new("actuarialtable", interest=0.06, x=x, lx=Ix, name="SOA2008"))
#'
#' ## IAxn: increasing arithmetic term, 10 years, age 25 (legacy)
#' IAxn(actuarialtable = soa08Act, x = 25, n = 10)
#'
#' ## More examples (k>1 and deferment)
#' IAxn(actuarialtable = soa08Act, x = 40, n = 20, k = 12)     # monthly claims
#' IAxn(actuarialtable = soa08Act, x = 40, n = 15, m = 5, k = 4) # deferred 5y, quarterly
#'
#' @family life-contingency APVs
#' @name arithmetic_variation_insurances
#' @aliases IAxn DAxn
#' @rdname arithmetic_variation_insurances
#' @export
IAxn <- function(actuarialtable, x, n,i = actuarialtable@interest, m = 0, k = 1, type =
             "EV",power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    if (missing(m))
      m = 0
    if (missing(x))
      stop("Error! Need age!")
    type <- testtyperesarg(type)
    
    if (missing(n))
      n = getOmega(actuarialtable) - x - m #n=getOmega(actuarialtable)-x-m-1
    
    if (any(x < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    #we have n*k possible payment times
    payments = seq(from = 1 / k, to = n, by = 1 / k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    startAge = x #we start from x
    for (i in 1:length(times))
      probs[i] = (
        pxt(object = actuarialtable, x = startAge,t = times[i]) * qxt(
          object = actuarialtable, x = startAge + times[i],t = 1 / k
        )
      )
    discounts = (1 + interest) ^ -(times + 1 / k)
    
    if (type == "EV") {
      #		payments=seq(from=1, to=n, by=1)
      #		probs=numeric(n)
      #		times=m+seq(from=0, to=(n-1),by=1)
      #		for(i in 1:length(times)) probs[i]=(pxt(actuarialtable, x,times[i])*qxt(actuarialtable,
      #								x+times[i],1))
      #		discounts=(1+interest)^-(times+1)
      out <- sum(((payments * discounts) ^ power) * probs)
    } else if (type == "ST") {
      out = rLifeContingencies(
        n = 1,lifecontingency = "IAxn", object = actuarialtable, x = x,t = n,i =
          interest, m = m,k = k
      ) #!fix: prima = 1
    }
    return(out)
  }

#' @rdname arithmetic_variation_insurances
#'
#' @section \code{DAxn} — Decreasing arithmetic term:
#' Computes the APV of an n-year **decreasing** term insurance on a life aged \code{x}, with \code{k} fractional
#' claim times and optional deferment \code{m}. The benefit at the \eqn{j}-th subperiod equals \eqn{n - (j-1)/k}.
#'
#' @details
#' \strong{DAxn} (decreasing):
#' \deqn{ \mathrm{DA}_{\overline{n}|}^{(k)} = \sum_{j=1}^{nk} \Big(n - \frac{j-1}{k}\Big)\,
#'         v^{t_j + 1/k}\; {}_{t_j}p_x\; q_{x+t_j}^{(1/k)}, \qquad t_j = m + \frac{j-1}{k}. }
#' See “Fractional timing conventions” above for claim timing assumptions.
#'
#' @examples
#' ## DAxn: decreasing arithmetic term, 10 years, age 25 (legacy)
#' DAxn(actuarialtable = soa08Act, x = 25, n = 10)
#' ## More examples (k>1 and deferment)
#' DAxn(actuarialtable = soa08Act, x = 45, n = 10, k = 2)       # semiannual
#' DAxn(actuarialtable = soa08Act, x = 45, n = 12, m = 3, k = 12) # deferred 3y, monthly
#'
#' @export
DAxn <- function(actuarialtable, x, n,i = actuarialtable@interest, m = 0, k = 1, type =
             "EV",power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    if (missing(x))
      stop("Error! Need age!")
    type <- testtyperesarg(type)
    if (missing(m))
      m = 0
    if (missing(n))
      n = getOmega(actuarialtable) - x - m #n=getOmega(actuarialtable)-x-m-1
    
    
    payments = seq(from = n, to = 1 / k, by = -1 / k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    startAge = x #we start from x
    for (i in 1:length(times))
      probs[i] = (
        pxt(object = actuarialtable, x = startAge,t = times[i]) * qxt(
          object = actuarialtable, x = startAge + times[i],t = 1 / k
        )
      )
    discounts = (1 + interest) ^ -(times + 1 / k)
    
    if (type == "EV") {
      out <- sum(((payments * discounts) ^ power) * probs)
    } else if (type == "ST")
    {
      out = rLifeContingencies(
        n = 1,lifecontingency = "DAxn",
        object = actuarialtable, x = x,t = n,i = interest, m = m,k = k
      )
    }
    return(out)
  }

# n=100000
# lifecontingency="DAxn"
# actuarialtable=soa08Act
# object=actuarialtable
# x=40
# t=20
# i=soa08Act@interest
# m=0
# k=1

# outs<-rLifeContingencies(n,lifecontingency, object, x,t,i=i,
# m=m,k=k, parallel=TRUE)
# APV=IAxn(object, x=x,n=t, k=k)
# mean(outs)
# APV
# t.test(x=outs, mu=APV)



#n-year increasing
#recursive function
Iaxn <- function(actuarialtable, x, n,i = actuarialtable@interest, m = 0, type =
             "EV", power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    type <- testtyperesarg(type)
    if (missing(m))
      m = 0
    if (missing(x))
      stop("Error! Need age!")
    #m is set equal to zero at the moment
    if (missing(n))
      n = getOmega(actuarialtable) - x - m #n=getOmega(actuarialtable)-x-m-1
    
    #i an interest rate is provided the provided interest rate overrides the
    #actuarialtable interest rate
    payments = numeric(n)
    probs = numeric(n)
    times = numeric(n)
    discounts = numeric(n)
    
    payments = seq(from = 1, to = n, by = 1)
    times = m + seq(from = 0, to = (n - 1),by = 1)
    for (i in 1:length(times))
      probs[i] = pxt(actuarialtable, x,times[i])
    discounts = (1 + interest) ^ -(times)
    out <- sum(((payments * discounts) ^ power) * probs)
    return(out)
  }



#' @rdname endowment_trio
#'
#' @details
#' \strong{AExn} — n-year endowment insurance, computed as \code{Axn + Exn}.
#'
#' @export
AExn <- function(actuarialtable, x, n,i = actuarialtable@interest, k = 1, type =
             "EV",power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    if (missing(x))
      stop("Error! Need age!")
    type <- testtyperesarg(type)
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(n))
      n = getOmega(actuarialtable) - x - 1
    
    if (n == 0)
      return(1)
    if (any(x < 0,n < 0))
      stop("Error! Negative parameters")
    
    if (type == "EV") {
      out <-
        Axn(
          actuarialtable = actuarialtable, x = x, n = n, i = i,m = 0, k = k,type =
            "EV",power = power
        ) + Exn(
          actuarialtable = actuarialtable, x = x, n = n, i = i,
          type = "EV",power = power
        )
    } else if (type == "ST") {
      out = rLifeContingencies(
        n = 1,lifecontingency = "AExn",
        object = actuarialtable, x = x,t = n,i = interest, k = k
      )
    }
    return(out)
  }


#x=35
#t=30
#n=200000
#object=soa08Act
#lifecontingency="EAxn"
#i=0.06
#out<-rLifeContingencies(n=20000,lifecontingency="Axn", object=soa08Act, x=35,
#		t=30,i=0.06,k=1, parallel=TRUE)
