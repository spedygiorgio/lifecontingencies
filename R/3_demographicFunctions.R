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
###         demographic functions
###



## ---------------------------------------------------------------------------
## dxt(), pxt() and qxt() are S4 generics (since 1.6.3) with methods for
## "lifetable" (hence also "actuarialtable", which extends it) and "mdt".
## Previously they were plain functions dispatching by hand on class(object);
## the signatures and the numerical results are unchanged. A new table class
## only needs to register its own methods. The "ANY" methods keep the
## historical error message for unsupported objects.
## ---------------------------------------------------------------------------

.UNSUPPORTED_TABLE_MSG <- "Error! Only lifetable, actuarialtable or mdt classes are accepted"

#number of deaths between age x and x+t
setGeneric("dxt", function(object, x, t, decrement) standardGeneric("dxt"),
           signature = "object")

setMethod("dxt", "ANY", function(object, x, t, decrement)
  stop(.UNSUPPORTED_TABLE_MSG))

setMethod("dxt", "mdt", function(object, x, t, decrement) {
  if (missing(x))
    stop("Error! Missing x")
  if (missing(t))
    t <- 1
  if (!missing(decrement))
    .dxt.mdt(object = object, x = x, time = t, decrement = decrement)
  else
    .dxt.mdt(object = object, x = x, time = t)
})

# lx at the given ages, looked up by name with match(). Ages beyond the last
# tabulated one have no survivors (lx = 0); other absent ages give NA.
.lxAtAges <- function(object, ages) {
  lx <- object@lx[match(ages, object@x)]
  lx[is.na(lx) & !is.na(ages) & ages > max(object@x)] <- 0
  lx
}

# Deaths between x and x+t for integer t (open last interval: everybody
# alive at x dies by omega).
.dxtLifetableInt <- function(object, x, t, omega) {
  lx <- .lxAtAges(object, x)
  out <- lx - .lxAtAges(object, x + t)
  beyond <- (x + t) > omega
  out[beyond] <- lx[beyond]
  out
}

# Vectorised over x and t (recycled to a common length); fractional t is
# interpolated linearly: d(x, k + f) = d(x, k) + f * d(x + k, 1).
setMethod("dxt", "lifetable", function(object, x, t, decrement) {
  if (missing(x))
    stop("Error! Missing x")
  if (missing(t))
    t <- 1
  omega <- getOmega(object)
  n <- max(length(x), length(t))
  x <- rep(x, length.out = n)
  t <- rep(t, length.out = n)
  fracPart <- t %% 1
  intPart <- t - fracPart
  out <- .dxtLifetableInt(object, x, intPart, omega)
  frac <- which(fracPart != 0)
  if (length(frac) > 0)
    out[frac] <- out[frac] + fracPart[frac] *
      .dxtLifetableInt(object, x[frac] + intPart[frac], 1, omega)
  out
})

#survival probability between age x and x+t
setGeneric("pxt",
           function(object, x, t, fractional = "linear", decrement)
             standardGeneric("pxt"),
           signature = "object")

setMethod("pxt", "ANY",
          function(object, x, t, fractional = "linear", decrement)
            stop(.UNSUPPORTED_TABLE_MSG))

# Shared argument checks and x/t recycling for the pxt() methods.
.pxtPrepareArgs <- function(x, t) {
  if (any(x < 0, t < 0))
    stop("Check x or t domain")
  if(length(x) <= 0)
    stop("x is of length zero")
  if(length(t) <= 0)
    stop("t is of length zero")
  n <- max(length(t), length(x))
  if(length(t) != length(x))
  {
    if(length(t) > 1 && length(x) > 1)
      warnings("t and x arguments have been recycled to match the maximum length of x and t")
    t <- rep(t, length.out=n)
    x <- rep(x, length.out=n)
  }
  list(x = x, t = t, n = n)
}

# Validates a (possibly multiple) decrement specification against an mdt and
# returns the corresponding column names.
.mdtDecrementNames <- function(object, decrement) {
  stopifnot(class(decrement) %in% c("numeric", "integer", "character"))
  if (is.numeric(decrement))
    decrement <- getDecrements(object)[decrement]
  if (length(decrement) == 0 || anyNA(decrement) ||
      !all(decrement %in% getDecrements(object)))
    stop("Error! Not recognized decrement type")
  unique(decrement)
}

# Probability tq_x^(J) of leaving an mdt within t years because of any of the
# decrements in J, vectorised over x and t. Integer ages are required;
# fractional durations are interpolated linearly (UDD within the year), i.e.
# tq_x^(J) = [sum_{k<floor(t)} d_{x+k}^(J) + frac(t) d_{x+floor(t)}^(J)] / l_x.
#
# This replaces an optimised branch of pxt() that rebuilt a pseudo "lx" from
# the decrement-specific counts alone and returned wrong results (NaN or
# values outside [0,1]) whenever the first row of the table had no zero cells.
.qxtDecrementMdt <- function(object, x, t, decrement) {
  tbl <- object@table
  dj <- rowSums(as.matrix(tbl[, decrement, drop = FALSE]))
  ages <- tbl$x
  nr <- length(ages)
  cumd <- c(0, cumsum(dj))
  ix <- match(x, ages)
  if (anyNA(ix))
    stop("Error! Ages must be integers tabulated in the mdt: ",
         paste(unique(x[is.na(ix)]), collapse = ", "))
  t0 <- floor(t)
  fr <- t - t0
  endRow <- pmin(ix + t0, nr + 1L)          # exclusive end in 1..nr+1
  num <- cumd[endRow] - cumd[ix]
  fracRow <- ix + t0
  fracD <- ifelse(fr > 0 & fracRow <= nr, dj[pmin(fracRow, nr)], 0)
  as.numeric((num + fr * fracD) / tbl$lx[ix])
}

# Survival probability from an (x, lx) series with the package's fractional
# age/duration conventions; used by the mdt method (total decrement).
.pxtFromLxSeries <- function(myx, mylx, omega, x, t, fractional) {
  names(mylx) <- paste0("x", c(myx, omega+1))

  #adjustment when age x is not an integer
  floorx <- floor(x) #compute floor(x)
  eps_x <- x - floorx #compute epsilon x
  u <- t+eps_x #add epsilon x to time t
  flooru <- floor(u) #compute floor(u)
  eps_u <- u - flooru #compute epsilon t

  #get l_floor(x) and consecutive
  l_floorx <- mylx[paste0("x", floorx)]
  l_floorxp1 <- mylx[paste0("x", floorx+1)]
  #get l_floor(x+t) and consecutive
  l_floorxu <- mylx[paste0("x", floorx+flooru)]
  l_floorxup1 <- mylx[paste0("x", floorx+flooru+1)]

  #compute one-year survival probabilites
  flooru_p_floorx <- l_floorxu / l_floorx
  floorup1_p_floorx <- l_floorxup1 / l_floorx
  one_p_floorxu <- l_floorxup1 / l_floorxu
  one_p_floorx <- l_floorxp1 / l_floorx

  #may contains NA if x or x+t is above omega => set to 0
  flooru_p_floorx[is.na(flooru_p_floorx)] <- 0
  floorup1_p_floorx[is.na(floorup1_p_floorx)] <- 0
  one_p_floorxu[is.na(one_p_floorxu)] <- 0
  one_p_floorx[is.na(one_p_floorx)] <- 0

  #adjustment when t is not integer
  if (fractional == "linear") {
    u_p_floorx <- flooru_p_floorx * (1 - eps_u*(1-one_p_floorxu))
      #equivalent to eps_u * floorup1_p_floorx + (1 - eps_u) * flooru_p_floorx
  } else if (fractional == "constant force") {
    u_p_floorx <- flooru_p_floorx * one_p_floorxu^eps_u
  } else if (fractional == "hyperbolic") {
    u_p_floorx <- flooru_p_floorx * one_p_floorxu / (1 - (1-eps_u)*(1-one_p_floorxu))
  }

  #adjustment when age x is not an integer (otherwise equal 1)
  if (fractional == "linear") {
    eps_x_p_floorx <-  1 - eps_x * (1-one_p_floorx)
  } else if (fractional == "constant force") {
    eps_x_p_floorx <- one_p_floorx^eps_x
  } else if (fractional == "hyperbolic") {
    eps_x_p_floorx <- one_p_floorx / (1 - (1-eps_x)*(1-one_p_floorx))
  }
  as.numeric(u_p_floorx / eps_x_p_floorx)
}

setMethod("pxt", "lifetable",
          function(object, x, t, fractional = "linear", decrement) {
  fractional <- testfractionnalarg(fractional)
  if (missing(x))
    stop("Missing x")
  if (missing(t))
    t <- 1 #default 1
  a <- .pxtPrepareArgs(x, t)
  # Native kernel: an exact port of the name-based lookup and fractional-age
  # adjustment (same results bit for bit, NaN cases included).
  method <- switch(fractional, "linear" = 0L, "constant force" = 1L,
                   "hyperbolic" = 2L)
  .pxtLifetableCpp(a$x, a$t, c(object@lx, 0), object@x[1], method)
})

setMethod("pxt", "mdt",
          function(object, x, t, fractional = "linear", decrement) {
  fractional <- testfractionnalarg(fractional)
  if (missing(x))
    stop("Missing x")
  if (missing(t))
    t <- 1 #default 1
  a <- .pxtPrepareArgs(x, t)
  if (!missing(decrement)) {
    decrement <- .mdtDecrementNames(object, decrement)
    if (fractional != "linear" && any(a$t %% 1 != 0))
      warning("Decrement-specific probabilities on an mdt use linear (UDD) interpolation for fractional t; 'fractional' is ignored")
    return(1 - .qxtDecrementMdt(object, a$x, a$t, decrement))
  }
  .pxtFromLxSeries(myx = object@table$x, mylx = c(object@table$lx, 0),
                   omega = getOmega(object), x = a$x, t = a$t,
                   fractional = fractional)
})

#survival probability between age x and x+t
pxtold <- function(object, x, t, fractional = "linear", decrement)
{
  out <- NULL
  #checks
  if (!(class(object) %in% c("lifetable","actuarialtable","mdt")))
    stop("Error! Only lifetable, actuarialtable or mdt classes are accepted")
  
  fractional <- testfractionnalarg(fractional)
  
  if (is(object,"mdt")) {
    #specific function for multiple decrements
    out <-
      ifelse(
        missing(decrement), 1 - .qxt.mdt(object = object,x = x,t = t),
        1 - .qxt.mdt(object = object,x = x,t = t,decrement = decrement)
      )
    return(out)
  }
  
  #class(object) %in% c("lifetable","actuarialtable") 
  if (missing(x))
    stop("Missing x")
  if (any(x < 0, t < 0))
    stop("Check x or t domain")
  if (missing(t))
    t = 1 #default 1
  omega = getOmega(object)
  #if the starting age is fractional apply probability laws
  if ((x - floor(x)) > 0) {
    integerAge = floor(x)
    excess = x - floor(x)
    texcess_p_floorx <- pxtold(object = object, x = integerAge, 
      fractional=fractional, t = excess + t, decrement = decrement)
    
    excess_p_floorx <- pxtold(object = object, x = integerAge, 
      fractional=fractional, t = excess, decrement = decrement
    )
    out = texcess_p_floorx / excess_p_floorx
    return(out)
  }
  #Rosa Corrales Patch
 # if ((object@lx[omega] > 0) &&
  #    (x + t) == (omega + 1)) {
  #  out <- 1 / object@lx[which(object@x == x)]
  #} else {
    #before x+t>=omega
    if ((x + t) >= omega + 1)
      return(0)

  if((x + t) > omega){ # x + t is between last lx > 0 and 0

    z <- t %% 1 #the fraction of year
    #linearly interpolates if fractional age
    pl <-
      object@lx[which(object@x == floor(t + x))] / object@lx[which(object@x ==
                                                                     x)] # Kevin Owens: fix on this line, moving it out of the linear if statement so it can be used in other assumptions
    if (fractional == "linear") {
      ph <- 0
      out <- z * ph + (1 - z) * pl
    } else if (fractional == "constant force") {
      out <- pl * pxtold(object = object, x = (x + floor(t)),t = 1, fractional=fractional) ^ z # fix on this line
    } else if (fractional == "hyperbolic") {
      out <-
        pl * pxtold(object = object, x = (x + floor(t)),t = 1, fractional=fractional) / (1 - (1 - z) * qxtold(
          object = object, x = (x + floor(t)),t = 1
        )) # Kevin Owens: fix on this line
    }
  }
  else # x + t is less that or equal to omega
    #fractional ages
  {
    if ((t %% 1) == 0)
    { 
      out <-
        object@lx[which(object@x == t + x)] / object@lx[which(object@x == x)]
    }else {
      z <- t %% 1 #the fraction of year
      #linearly interpolates if fractional age
      pl <-
        object@lx[which(object@x == floor(t + x))] / object@lx[which(object@x ==
                                                                       x)] # Kevin Owens: fix on this line, moving it out of the linear if statement so it can be used in other assumptions
      if (fractional == "linear") {
        ph <-
          object@lx[which(object@x == ceiling(t + x))] / object@lx[which(object@x ==
                                                                           x)]
        out <- z * ph + (1 - z) * pl
      } else if (fractional == "constant force") {
        out <- pl * pxtold(object = object, x = (x + floor(t)),t = 1, fractional=fractional) ^ z # fix on this line
      } else if (fractional == "hyperbolic") {
        out <-
          pl * pxtold(object = object, x = (x + floor(t)),t = 1, fractional=fractional) / (1 - (1 - z) * qxtold(
            object = object, x = (x + floor(t)),t = 1, fractional=fractional
          )) # Kevin Owens: fix on this line
      }
    }
  }

  #  }
  return(out)
}


.forceOfMortality <- function(object,x)
{
	out<-NULL
	#checks
	if(!is(object, "lifetable")) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	#force of mortality
	out<-log(pxt(object=object, x=x, t=1))
	return(out)
}

#' @rdname other-demographic-functions
#' @param t duration of the calculation
#' @param fxt correction constant, default 0.5
#' @aliases Lxt
#' @examples 
#' data(soaLt)
#' soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
#' x=x,lx=Ix,name="SOA2008"))
#' Lxt(soa08Act, 67,10)
#' @export
Lxt <- function(object, x,t=1,fxt=0.5)
{

	out<-NULL
	#checks

	if(!is(object, "lifetable")) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(any(x<0,t<0)) stop("Check x or t domain")

	ages=seq(from=x, to=x+t-1, by=1)
	lifes=numeric(length(ages))
	lifes=.lxAtAges(object, ages)
	deaths=dxt(object, ages, 1)
	toSum=lifes-fxt*deaths
	out=sum(toSum)
	return(out)
}

#' @rdname other-demographic-functions
#' @aliases Tx
#' @title Various demographic functions
#'
#' @param object a \code{lifetable} or \code{actuarialtable} object
#' @param x age of the subject
#' @param fxt fraction of the year of age lived by those who die within the
#'   year (so that \eqn{L_x = l_x - fxt\, d_x}, matching \code{\link{Lxt}}).
#'   Defaults to \code{0.5} (uniform distribution of deaths).
#' @param axOmega mean number of years lived in the last, open-ended age
#'   interval by those still alive at the last tabulated age \eqn{\omega}
#'   (so that \eqn{L_\omega = axOmega \cdot l_\omega}). The default,
#'   \code{1 - fxt}, reproduces the historical behaviour of the function
#'   (the last interval is closed assuming survivors live on average half a
#'   year longer when \code{fxt = 0.5}). Published life tables that leave the
#'   last interval open set \eqn{L_\omega = l_\omega / m_\omega}: pass
#'   \code{axOmega = 1 / mOmega} to reproduce them. Only relevant when
#'   \eqn{l_\omega > 0}.
#' @details \code{Tx} il the sum of years lived since age \code{x} by the population of the life table, it is the sum of \code{Lx}. The function is provided as is,
#' without any warranty regarding the accuracy of calculations. Use at own risk.
#' @return A numeric value
#' @references 	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
#' @author Giorgio Alfredo Spedicato.
#' @examples
#' #assumes SOA example life table to be load
#' data(soaLt)
#' soa08Act=with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
#' Tx(soa08Act, 67)
#' @export
Tx <- function(object,x,fxt=0.5,axOmega=1-fxt)
{
	out<-NULL
 	#checks
	if(!is(object, "lifetable"))
	  stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	omega <- getOmega(object)
	#Tx(x) = sum_{k=x}^{omega-1} (l_k - fxt*d_k) + axOmega * l_omega.
	#Computed with a single vectorised pass over the lx series (previous
	#implementation called Lxt() once per age in [x, omega], and Lxt() itself
	#loops and calls dxt() once per age, making it O(n^2) in the remaining
	#ages). With the defaults (fxt = 0.5, axOmega = 1 - fxt = 0.5) this is
	#identical to the historical closed-interval behaviour: the last age
	#contributes l_omega - 0.5*l_omega = 0.5*l_omega.
	idx <- which(object@x >= x & object@x <= omega)
	lxRange <- object@lx[idx]
	n <- length(lxRange)
	if(n == 1L) return(axOmega * lxRange[1L])
	dxInterior <- lxRange[-n] - lxRange[-1]
	out <- sum(lxRange[-n] - fxt*dxInterior) + axOmega * lxRange[n]
	return(out)
}

#' @title Central mortality rate
#' @description This function returns the central mortality rate demographic function.
#' @param object a \code{lifetable} or \code{actuarialtable} object
#' @param x subject's age
#' @param t period on which the rate is evaluated
#' @return A numeric value representing the central mortality rate between age \eqn{x} and \eqn{x+t}.
#' @references Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
#' @examples 
#' #assumes SOA example life table to be load
#' data(soaLt)
#' soa08Act=with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
#' #compare mx and qx 
#' mxt(soa08Act, 60,10)
#' qxt(soa08Act, 60,10)
#' @export
mxt <- function(object,x,t)
{
	out<-NULL
	#checks
	if(missing(t)) t<-1 #default 1
	if(!is(object, "lifetable")) 
	  stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(any(x<0,t<0)) stop("Check x or t domain")

	deaths=dxt(object,x,t)
	lived=Lxt(object,x,t)
	out=deaths/lived
	return(out)
}

#death probability
setGeneric("qxt",
           function(object, x, t, fractional = "linear", decrement)
             standardGeneric("qxt"),
           signature = "object")

setMethod("qxt", "ANY",
          function(object, x, t, fractional = "linear", decrement)
            stop(.UNSUPPORTED_TABLE_MSG))

# Same body for lifetable and mdt: complement of pxt().
.qxtComplement <- function(object, x, t, fractional = "linear", decrement) {
	if(missing(x))
	  stop("Missing x")
	if(missing(t))
	  t<-1 #default 1
	if(any(x<0,t<0))
	  stop("Check x or t domain")
	#complement of pxt
	1 - pxt(object=object, x=x, t=t, fractional=fractional, decrement=decrement)
}

setMethod("qxt", "lifetable", .qxtComplement)
setMethod("qxt", "mdt", .qxtComplement)

qxtold <- function(object, x, t, fractional="linear", decrement)
{
  out<-NULL
  #checks
  if(!(class(object) %in% c("lifetable","actuarialtable","mdt"))) 
    stop("Error! Only lifetable, actuarialtable or mdt classes are accepted")
  if(missing(x)) 
    stop("Missing x")
  if(any(x<0,t<0)) 
    stop("Check x or t domain")
  if(missing(t)) 
    t<-1 #default 1
  #complement of pxt
  out <- 1-pxtold(object=object, x=x, t=t, fractional=fractional, decrement=decrement)
  return(out)
}


#' Expected residual life.
#'
#' Expected future lifetime of a life aged \eqn{x}, either over the whole remaining
#' lifespan or over a temporary period of \eqn{n} years.
#'
#' @param object A lifetable/actuarialtable object.
#' @param x Attained age
#' @param n Length (in years) of the period over which the expected lifetime is computed,
#' i.e. a temporary expectation. Assumed omega - x + 1 (the whole remaining lifespan) whether missing.
#' @param type Either \code{"Tx"}, \code{"complete"} or \code{"continuous"} for the complete
#' (continuous) future lifetime, \code{"Kx"} or \code{"curtate"} for the curtate future lifetime
#' (can be abbreviated). Default is \code{"curtate"}.
#' @param fxt fraction of the year of age lived by those who die within the year, used by the
#' \code{"complete"} branch (see \code{\link{Lxt}} and \code{\link{Tx}}). Defaults to \code{0.5}
#' (uniform distribution of deaths); ignored by the \code{"curtate"} branch.
#' @param axOmega mean number of years lived in the last, open-ended age interval, used by the
#' \code{"complete"} branch when the period reaches the last tabulated age (see \code{\link{Tx}}).
#' Defaults to \code{1 - fxt}, which reproduces the historical closed-interval behaviour.
#'
#' @details
#' For \code{type = "curtate"} the function returns the (temporary) curtate expectation of life
#' \deqn{e_{x:\overline{n}|} = \sum_{k=1}^{n} {}_k p_x ,}
#' that is the expected number of complete future years lived by (x) within the next \eqn{n} years.
#' With \eqn{n} missing it is the curtate expectation of life \eqn{e_x = E[K_x]}.
#'
#' For \code{type = "complete"} the function returns the (temporary) complete expectation of life
#' \deqn{\mathring{e}_{x:\overline{n}|} = \int_0^n {}_t p_x \, dt = \frac{{}_nL_x}{l_x} ,}
#' evaluated under the uniform distribution of deaths (UDD) assumption within each year, i.e.
#' \eqn{L_x = l_x - 0.5 d_x} (see \code{\link{Lxt}}). With \eqn{n} missing it is
#' \eqn{\mathring{e}_x = T_x / l_x = E[T_x]}.
#'
#' Under UDD the two quantities are related by
#' \eqn{\mathring{e}_{x:\overline{n}|} = e_{x:\overline{n}|} + 0.5\,(1 - {}_n p_x)}, which reduces to
#' \eqn{\mathring{e}_x = e_x + 0.5} when \eqn{n} covers the whole remaining lifespan.
#'
#' The last tabulated age \eqn{\omega} is treated as a closed interval: those alive at \eqn{\omega}
#' are assumed to die on average half a year later. Published tables that close the table with an open
#' interval (e.g. \eqn{L_{\omega} = l_{\omega}/m_{\omega}}, as in the NCHS tables) can therefore
#' show a slightly larger complete life expectancy.
#'
#' @return A numeric value representing the expected life span.
#' @author Giorgio Alfredo Spedicato
#' @references 	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
#' Jones, D.A. and Nesbitt, C.J.
#' @seealso \code{\linkS4class{lifetable}}, \code{\link{Tx}}, \code{\link{Lxt}}
#'
#' @examples
#' #loads and show
#' data(soa08Act)
#' #curtate expectation of life at birth
#' exn(object=soa08Act, x=0)
#' #complete expectation of life at birth (curtate + 0.5 under UDD)
#' exn(object=soa08Act, x=0,type="complete")
#' #temporary 20-year expectations at age 50
#' exn(object=soa08Act, x=50, n=20)
#' exn(object=soa08Act, x=50, n=20, type="complete")
#' @export
exn <- function(object,x,n,type="curtate",fxt=0.5,axOmega=1-fxt) {
	out<-NULL
	#checks
	if(!is(object, "lifetable")) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) x=0
	omega <- getOmega(object)
	fullLife <- missing(n)
	if(missing(n)) n=omega-x +1 #to avoid errors
	if(n==0) return(0)
	type <- testtypelifearg(type)

	if(type=="Kx"){
	# pxt() accetta gia' un vettore di tempi: una sola chiamata al posto
	# di n chiamate scalari (ognuna con validazione S4 ripetuta).
	out <- sum(pxt(object, x, 1:n))
	} else {
		lx=object@lx[which(object@x==x)]
		# Whole remaining lifespan (n missing, or n reaching past omega):
		# route through Tx() so the open-interval assumption (fxt, axOmega)
		# is honoured. With the defaults this equals the historical
		# Lxt(x, omega-x+1)/lx. A strictly temporary period uses Lxt().
		if(fullLife || (x + n > omega)) out=Tx(object=object, x=x, fxt=fxt, axOmega=axOmega)/lx
		else out=Lxt(object=object, x=x,t=n,fxt=fxt)/lx
	}
	return(out)
}

##################two life ###########


pxyt <- function(objectx, objecty,x,y,t, status="joint")
{
  .Deprecated("pxyzt")
  out<-NULL
	
	#checks
	if(!is(objectx, "lifetable")) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(!is(objecty, "lifetable")) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(missing(y)) stop("Missing y")
	if(missing(t)) t=1 #default 1
	if(any(x<0,y<0,t<0)) stop("Check x, y and t domain")
	#joint survival status
  status <- teststatusarg(status)

	pxy=pxt(objectx, x,t)*pxt(objecty,y,t)
	if(status=="joint") out=pxy else out=pxt(objectx, x,t)+pxt(objecty,y,t)-pxy 
	return(out)
}

qxyt <- function(objectx, objecty,x,y,t, status="joint")
{
  .Deprecated("qxyzt")
  out<-NULL
	#checks
	if(!is(objectx, "lifetable")) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(!is(objecty, "lifetable")) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(missing(y)) stop("Missing y")
	if(missing(t)) t=1 #default 1
	if(any(x<0,y<0,t<0)) stop("Check x, y and t domain")
	out=1-pxyt(objectx=objectx, objecty=objecty,x=x,y=y,t=t, status=status)
	return(out)
}

#to check

exyt<-function(objectx, objecty,x,y,t,status="joint")
{
  .Deprecated("exyzt")
  out<-NULL
	#checks
	if(!is(objectx, "lifetable")) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(!is(objecty, "lifetable")) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(missing(y)) stop("Missing y")
	maxTime=max(getOmega(objectx)-x, getOmega(objecty)-y)  #maximum number of years people can live togeter
	if(missing(t)) t=maxTime
	if(any(x<0,y<0,t<0)) stop("Check x, y and t domain")
	toSum=min(t,maxTime) #max number of years to sum
	times=1:toSum
	probs=numeric(length(times))
	#out=1-pxyt(objectx=objectx, objecty=objecty,x=x,y=y,t=t, status=status)
	for(i in 1:length(times)) probs[i]=pxyt(objectx=objectx, objecty=objecty,x=x,y=y,t=times[i], status=status)
	out=sum(probs)
	return(out)
}

probs2lifetable <- function(probs, radix=10000, type="px", name="ungiven")
{
	if(any(probs>1) | any(probs<0)) stop("Error: probabilities must lie between 0 an 1")
	if(!(type %in% c("px","qx"))) stop("Error: type must be either px or qx")
	if(type=="px" & probs[length(probs)]!=0) probs[length(probs)+1]=0;
	if(type=="qx" & probs[length(probs)]!=1) probs[length(probs)+1]=1;
	#one-year survival factor for each row; lx[i] = radix * prod of the
	#survival factors of all preceding rows. cumprod() computes the whole
	#series in one vectorised pass, replacing the explicit for loop that
	#recomputed lx[i] from lx[i-1] one age at a time.
	survivalFactors <- if(type=="px") probs else 1-probs
	n <- length(survivalFactors)
	lx <- radix * cumprod(c(1, survivalFactors[-n]))
	out=new("lifetable",x=seq(0,length(probs)-1), lx=lx, name=name)
	return(out)
}

#multiple life new function
pxyzt <- function(tablesList, x, t, status="joint", 
                      fractional=rep("linear",length(tablesList)), ...)
{
  #fractional list can be either missing or a string length of character one
  if(length(fractional)==1) 
  {
    temp <- fractional;
    fractional <- rep(temp, length.out =length(tablesList))
  }
  
  fractional <- sapply(fractional, testfractionnalarg)
  status <- teststatusarg(status)
  
  #initial checkings
  if (missing(x))
    stop("Missing x")
  if (!is.numeric(x))
    stop("non numeric x")
  if (missing(t))
    t = 1 #default 1
  #convert argument to matrix
  numTables <- length(tablesList)
  if(is.vector(x) && is.vector(t))
  {
    if(length(x) != numTables)
      stop("Error! Initial ages vector length does not match with number of lives")
    if(length(t) == 1)
      t <- rep(t, length.out=length(x))
    if(length(t) != numTables)
      stop("Error! Initial time vector length does not match with number of lives")
    valx <- t(as.matrix(x))
    valt <- t(as.matrix(t))
  }else if(is.matrix(x) && is.vector(t))
  {
    if(NCOL(x) != numTables)
      stop("Error! Initial ages matrix does not have as much column as number of lives")
    if(length(t) == 1)
      t <- rep(t, length.out=NCOL(x))
    if(length(t) != numTables)
      stop("Error! Initial time vector length does not match with number of lives")
    valx <- as.matrix(x)
    valt <- matrix(t, nrow=NROW(valx), ncol=numTables, byrow=TRUE)
  }else if(is.vector(x) && is.matrix(t))
  {
    if(length(x) != numTables)
      stop("Error! Initial ages vector length does not match with number of lives")
    if(NCOL(t) != numTables)
      stop("Error! Initial time matrix does not have as much column as number of lives")
    valt <- as.matrix(t)
    valx <- matrix(x, nrow=NROW(valt), ncol=numTables, byrow=TRUE)
  }else
  {
    if(NCOL(x) != numTables)
      stop("Error! Initial ages matrix does not have as much column as number of lives")
    if(NCOL(t) != numTables)
      stop("Error! Initial time matrix does not have as much column as number of lives")
    if(NROW(x) != NROW(t))
      warnings("t and x arguments have been recycled to match the maximum row number of x and t")
    n <- max(NROW(x), NROW(t))
    id2select <- rep(1:NROW(x), length.out=n)
    valx <- x[id2select, ]
    id2select <- rep(1:NROW(t), length.out=n)
    valt <- t[id2select, ]
  }
  #computation
  allpxt <- sapply(1:numTables, function(i)
                   pxt(tablesList[[i]], valx[,i], valt[,i], fractional = fractional[i], ...)
  )
  
  #the survival probability is the cumproduct of the single survival probabilities
  if(status == "joint")
  {
    if(is.vector(allpxt))
      out <- prod(allpxt)
    else 
      out <- apply(allpxt, 1, prod)
  }else{ #last survivor status
    allqxt <- 1-allpxt 
    if(is.vector(allpxt))
      out <- 1-prod(allqxt)
    else
      out <- 1-apply(allqxt, 1, prod)
  }
  return(out)
}

#multiple life new function
pxyztold <- function(tablesList,x,t, status="joint",fractional=rep("linear",length(tablesList)),...)
{
	out=1
	#fractional list can be either missing or a string length of character one
	if(length(fractional)==1) {temp<-fractional;fractional=rep(temp,length(tablesList))}
	
	fractional <- sapply(fractional, testfractionnalarg)
	status <- teststatusarg(status)
	
	#initial checkings
	
	numTables=length(tablesList)
	if(length(x)!=numTables) stop("Error! Initial ages vector length does not match with number of lives")
	for(i in 1:numTables) {
		if(!(class(tablesList[[i]]) %in% c("lifetable", "actuarialtable"))) stop("Error! A list of lifetable objects is required")
	}
	#the survival probability is the cumproduct of the single survival probabilities
	if(status=="joint")
	{
		for(i in 1:numTables) out=out*pxt(object=tablesList[[i]],x=x[i],t=t,fractional=fractional[i],...)
	} else { #last survivor status
		#calculate first qx the return the difference
		temp=1
		for(i in 1:numTables) temp=temp*qxt(object=tablesList[[i]],x=x[i],t=t,fractional=fractional[i],...)
		out=1-temp
	}
	return(out)
}
#the death probability
qxyzt <- function(tablesList,x,t, status="joint",fractional=rep("linear",length(tablesList)),...)
{
	out=numeric(1)
	out=1-pxyzt(tablesList=tablesList,x=x,t=t, status=status,...)
	return(out)
}

#probability to die between time n and n+t
.qxnt<-function(object, x,n,t=1,...)
{
	out <- numeric(1)
	out <- pxtold(object=object,x=x,t=n,...)*qxtold(object=object,x=x+n,t=t)
	return(out)
}


.qxyznt <- function(tablesList,x,n,t=1, status="joint")
{
	numTables <- length(tablesList)
	# n is now allowed to be a vector: build the (length(n) x numTables)
	# deferral-time matrix once, so a caller looping over many deferral
	# times can get all probabilities from a single pxyzt()/qxyzt() call
	# instead of one call per time point (each of which repeats S4
	# dispatch and argument validation). Scalar n keeps working exactly
	# as before (a 1-row matrix).
	tmat <- matrix(n, nrow = length(n), ncol = numTables)
	if(status=="joint")
	{
		y <- tmat + matrix(x, nrow = length(n), ncol = numTables, byrow = TRUE)
		out <- pxyzt(tablesList=tablesList,x=x,t=tmat, status=status) *
		       qxyzt(tablesList=tablesList,x=y,t=t, status=status)
	} else { #last
		ymat <- tmat + t
		out <- pxyzt(tablesList=tablesList,x=x,t=tmat, status=status) -
		       pxyzt(tablesList=tablesList,x=x,t=ymat, status=status)
	}
	return(out)
}

#curtate expectation of future lifetime

exyzt <- function(tablesList,x,t=Inf, status="joint",type="Kx",...)
{
	#initial checkings
	numTables=length(tablesList)
	if(length(x)!=numTables) stop("Error! Initial ages vector length does not match with number of lives")
	for(i in 1:numTables) {
	if(!(class(tablesList[[i]]) %in% c("lifetable", "actuarialtable"))) stop("Error! A list of lifetable objects is required")
	}
	type <- testtypelifearg(type)
	status <- teststatusarg(status)
	
	#get the max omega
	maxAge=0 
	for(i in 1:numTables)
	{
		maxAge=max(maxAge, getOmega(tablesList[[i]]))
	}
	minAge=min(x)
	term=0
	#curtate expectation of future lifetime
	if(missing(t)||is.infinite(t)) term=maxAge-minAge+1 else term=t
	#perform the calculation
	# pxyzt() accetta t come matrice (term x numTables): una sola chiamata
	# vettoriale al posto di `term` chiamate scalari accumulate nel loop.
	tmat <- matrix(1:term, nrow=term, ncol=numTables)
	pxyzVec <- pxyzt(tablesList=tablesList, x=x, t=tmat, status=status,...)
	out <- sum(pxyzVec)
	# Complete expectation under UDD (trapezoidal rule):
	#   int_0^n tp dt = sum_{k=1}^n kp + 0.5*(1 - np).
	# The correction is 0.5 only when np = 0 (term covering the whole lifespan);
	# for a finite term it must be 0.5*(1 - np).
	if(type=="Tx") out <- out + 0.5*(1 - pxyzVec[length(pxyzVec)])
	return(out)
}

#' @name mx2qx
#' @title Mortality rates to Death probabilities
#' 
#' @description Function to convert mortality rates to probabilities of death
#'
#' @details Function to convert mortality rates to probabilities of death
#' 
#' @param mx mortality rates vector
#' @param ax the average number of years lived between ages x and x +1 by individuals who die in that interval
#' 
#' @return A vector of death probabilities
#' @examples 
#' 
#' #using some recursion
#' qx2mx(mx2qx(.2))
#' 
#' @seealso \code{mxt}, \code{qxt}, \code{qx2mx}

mx2qx <- function(mx, ax = 0.5)
{
  out <- mx / (1 +  (1 - ax)*mx)
  return(out)
}

#' @name qx2mx
#' @title Death Probabilities to Mortality Rates
#' 
#' @description Function to convert death probabilities to mortality rates
#'
#' @details Function to convert death probabilities to mortality rates
#' 
#' @param qx death probabilities
#' @param ax the average number of years lived between ages x and x +1 by individuals who die in that interval
#' 
#' @return A vector of mortality rates
#' @examples 
#' data(soa08Act)
#' soa08qx<-as(soa08Act,"numeric")
#' soa08mx<-qx2mx(qx=soa08qx)
#' soa08qx2<-mx2qx(soa08mx)
#' @seealso \code{mxt}, \code{qxt}, \code{mx2qx}

qx2mx <- function(qx, ax=0.5) {
  out <- qx/(1+ax*qx-qx)
  return(out)
}
