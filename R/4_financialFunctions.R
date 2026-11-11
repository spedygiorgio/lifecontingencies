#############################################################################
#   Copyright (c) 2022 Giorgio A. Spedicato
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
###         financial functions
###


#TO DO: check here http://www.mysmu.edu/faculty/yktse/FMA/S_FMA_1.pdf
#TO DO: add k to increasing and decreasing annuities function


#' Present value of a series of cash flows
#'
#' Evaluate the present value, or the actuarial present value when occurrence
#' \code{probabilities} are supplied, of a vector of cash flows discounted at a
#' given set of interest rates. This is the computational kernel on top of
#' which the actuarial APV machinery (\code{\link{Axn}}, \code{\link{axn}},
#' \code{\link{Exn}}, \code{\link{IAxn}}, \code{\link{DAxn}}, ...) is built.
#'
#' @param cashFlows Numeric vector of cash flows, coherent in length with
#'   \code{timeIds}.
#' @param timeIds Numeric vector of time points at which the \code{cashFlows}
#'   are due.
#' @param interestRates Either a scalar effective interest rate or a numeric
#'   vector of the same length as \code{timeIds}: entry \code{j} discounts the
#'   \code{j}-th cash flow back to time 0.
#' @param probabilities Optional numeric vector of occurrence probabilities,
#'   one per element of \code{cashFlows}. If missing a vector of 1's
#'   (certainty) is assumed, which turns the output into a plain financial
#'   present value.
#' @param power Numeric exponent applied to each discounted (and
#'   probability-weighted) cash flow before summation. The default \code{1}
#'   returns a plain APV; higher integer powers are used by the package to
#'   compute higher moments of the present value, hence the name.
#'
#' @details
#' With \code{power = 1}, the function implements
#' \deqn{\mathrm{APV} = \sum_j (1 + i_j)^{-t_j}\, CF_j\, p_j,}
#' where \code{(t_j, CF_j, i_j, p_j)} are the aligned entries of
#' \code{timeIds}, \code{cashFlows}, \code{interestRates} and
#' \code{probabilities}. A missing \code{timeIds} is interpreted as the
#' single time \eqn{t = 1} (a warning is emitted so that silent
#' misalignments can be caught). The numerical work is delegated to the C++
#' kernel \code{.presentValueC()} for performance.
#'
#' @return A numeric scalar: the (actuarial) present value of the cash-flow
#'   series.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @note This simple function is the kernel on which the package's actuarial
#'   and financial mathematics rest.
#' @seealso \code{\link{annuity}}, \code{\link{Axn}}, \code{\link{axn}}
#' @examples
#' # Three $10 payments at the end of years 1-3, certainty, flat 3% rate:
#' cf <- c(10, 10, 10)
#' t  <- c(1, 2, 3)
#' p  <- c(1, 1, 1)
#' presentValue(cashFlows = cf, timeIds = t, interestRates = 0.03,
#'              probabilities = p)
#' @export
presentValue <- function(cashFlows, timeIds, interestRates, probabilities, power = 1)
{
	# initialize output
	out <- 0

	# if timeIds omitted, warn and assume a single time point at t = 1
	if (missing(timeIds)) {
		warning("Warning: missing time vector")
		timeIds <- 1
	}

	# if probabilities omitted, assume certainty (vector of 1's)
	if (missing(probabilities)) {
		probabilities <- rep(1, length(cashFlows))
	} else {
		if (length(cashFlows) != length(probabilities)) stop("Error! Probabilities must have same length of cash flows")
	}

	# dimensionality checks
	if (!(length(cashFlows) == length(timeIds))) stop("Error! check dimensionality of cash flow and time ids vectors")
	if ((length(interestRates) > 1) & (length(interestRates) != length(timeIds))) warning("Interest rates incoherent with time ids")

	# expand interestRates if it's a single scalar to the full time vector
	interestRates <- rep(interestRates, length.out = length(timeIds))

	# power is preserved for specialised actuarial computations (default = 1)
	out <- .presentValueC(cashFlows = cashFlows,
												timeIds = timeIds,
												interestRates = interestRates,
												probabilities = probabilities,
												power = power)
	return(out)
}




#' Duration and convexity of a series of cash flows
#'
#' \code{duration} returns the Macaulay (or modified) duration of a stream of
#' cash flows; \code{convexity} returns its convexity. Both are the classical
#' first- and second-order sensitivity measures of present value to a small
#' parallel shift of the interest rate.
#'
#' @param cashFlows A numeric vector of cash-flow amounts.
#' @param timeIds Numeric vector of payment times, same length as
#'   \code{cashFlows}.
#' @param i Nominal (APR) interest rate convertible \code{k}-thly. Pass the
#'   effective annual rate when \code{k = 1}.
#' @param k Compounding frequency for the nominal interest rate (periods per
#'   year); default \code{1}.
#' @param macaulay Logical. \code{TRUE} (default) returns the Macaulay
#'   duration; \code{FALSE} returns the modified duration
#'   \eqn{D^{mod} = D / (1 + i/k)}.
#'
#' @details
#' With \eqn{v = (1 + i/k)^{-1}} and \eqn{t'_j = t_j k}, the Macaulay
#' duration is
#' \deqn{D = \frac{\sum_j t_j\, CF_j\, v^{t'_j}}{\sum_j CF_j\, v^{t'_j}} ,}
#' and the convexity is
#' \deqn{C = \frac{\sum_j t_j (t_j + 1/k)\, CF_j\, v^{t'_j}}
#'                {\sum_j CF_j\, v^{t'_j}} (1 + i/k)^{-2} .}
#'
#' @return A numeric scalar: the (Macaulay or modified) duration, or the
#'   convexity, of the cash-flow series.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @export
#'
#' @examples
#' # Duration and convexity of a coupon bond (10 per year, 100 at maturity)
#' cf <- c(10, 10, 10, 10, 10, 110)
#' t  <- c(1, 2, 3, 4, 5, 6)
#' duration(cf, t, i = 0.03)
#' convexity(cf, t, i = 0.03)
duration<-function(cashFlows, timeIds,i, k=1,macaulay=TRUE)
{
  out=0
  if(missing(timeIds)) #check coherence on time id vector
  {	warning("Warning: missing time vector")
    timeIds=1
  }
  
  if(!(length(cashFlows)==length(timeIds))) stop("Error! check dimensionality of cash flow and time ids vectors") #check dimensionality of cash flows
  
  interestRates<-rep(i/k,length.out=length(timeIds))
  #computing present value
  ts=timeIds*k
  v=(1+interestRates)^-(ts)
  pv<-sum((cashFlows*v))
  #pv=.C("add2", x=as.double(cashFlows), y=as.double(v),n=as.integer(length(cashFlows)),out=numeric(1))$out
  #computing weighted time
  weightedTime <- sum((cashFlows*v*ts))
  #weightedTime=.C("add3", x=as.double(cashFlows), y=as.double(v),z=as.double(ts),n=as.integer(length(cashFlows)),out=numeric(1))$out
  out <- weightedTime/pv	
  # if(macaulay==FALSE) out=out else out=out/(1+i/k) 
  # return(out)
  if (macaulay == TRUE)
    out <- out
  else 
    out <- out/(1 + i/k)
  return(out)
}


#convexity


#' @rdname duration
convexity=function(cashFlows, timeIds,i,k=1)
{
	out=0
	if(missing(timeIds)) #check coherence on time id vector
	{	warning("Warning: missing time vector")
		timeIds=1
	}
	
	if(!(length(cashFlows)==length(timeIds))) stop("Error! check dimensionality of cash flow and time ids vectors") #check dimensionality of cash flows
	
	interestRates=rep(i/k,length.out=length(timeIds))
	#calcola il valora attuale
	v=(1+interestRates)^-(timeIds*k)
	pv=sum((cashFlows*v))
	#pv=.C("add2", x=as.double(cashFlows), y=as.double(v),n=as.integer(length(cashFlows)),out=numeric(1))$out
	#calcola il tempo medio ponderato
	
	weightedTime=sum((cashFlows*v*timeIds*(timeIds+1/k)))
	#weightedTime=.C("add3", x=as.double(cashFlows), y=as.double(v),z=as.double(timeIds*(timeIds+1/k)),n=as.integer(length(cashFlows)),out=numeric(1))$out
	
	out=(weightedTime/pv)*(1+i/k)^-2
	
	return(out)
}

#' Present value of an annuity-certain
#'
#' Compute the present value of a stream of fixed payments separated by equal
#' intervals of time. Returns the perpetuity value when \code{n = Inf}.
#'
#' @param i Effective interest rate expressed in decimal form (e.g. \code{0.03}
#'   for 3\%). May also be a vector of per-period rates with the same length as
#'   the implied payment schedule.
#' @param n Number of payment periods. \code{Inf} returns the value of a
#'   perpetuity (either immediate or due, depending on \code{type}).
#' @param m Deferment period. Default \code{0}.
#' @param k Number of payments per year. A payment of \eqn{1/k} is assumed in
#'   each sub-period, so that the total yearly outflow stays equal to 1.
#' @param type Payment timing. \code{"immediate"} (default) pays at the end of
#'   each period; \code{"due"} pays at the beginning. \code{"arrears"} and
#'   \code{"advance"} are the respective aliases (can be abbreviated).
#'
#' @details
#' For an annuity-immediate the first payment occurs at time \eqn{1/k}; for an
#' annuity-due the first payment occurs at time \eqn{0}. The present value is
#' obtained by delegating to \code{\link{presentValue}} with the appropriate
#' payment schedule rather than closed-form formulas, which keeps the output
#' consistent across other APV functions that reuse the same kernel.
#'
#' @return A numeric scalar: the present value of the annuity.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @note The value returned derives from direct discounting of the cash flow
#'   schedule, not from closed-form formulas such as
#'   \eqn{a^{(m)}_{\overline{n}|} = (1 - v^n)/i^{(m)}}. When \code{m > 1},
#'   the per-period payment is \eqn{1/m}.
#' @seealso \code{\link{accumulatedValue}}, \code{\link{increasingAnnuity}},
#'   \code{\link{decreasingAnnuity}}
#' @examples
#' # Present value of 1 per year, paid in arrears, for 10 years at 3%:
#' annuity(i = 0.03, n = 10)
#' # Same but paid monthly (1/12 at the end of each month):
#' annuity(i = 0.03, n = 10, k = 12)
#' # Perpetuity due:
#' annuity(i = 0.05, n = Inf, type = "due")
#' @export
annuity=function(i, n,m=0,k=1, type="immediate")
{
	#checks
	if(missing(i)) stop("Error! Missing effective interest rates") 
	if(missing(n)) stop("Error! Missing periods")
	if(m<0) stop("Error! Negative deferring period") 
	if(k<1) stop("Error! Payment frequency must be greater or equal than 1") 
  type <- testpaymentarg(type)
	
	if(is.infinite(n)) {
		out=ifelse(type=="immediate",1/i,1/interest2Discount(i))
		return(out)
	} 
	
	if(n==0) return(0)
	ieff=i #i ? il tasso effettivo
	if(type=="immediate") timeIds=seq(from=1/k, to=n, by=1/k)+m
	else timeIds=seq(from=0, to=n-1/k, by=1/k)+m #due
	
	iRate=rep(ieff,length.out=n*k)
	out=presentValue(cashFlows=rep(1/k,length.out=n*k),interestRates = iRate, 
			timeIds=timeIds)
	return(out)
}

#' Decreasing annuity-certain
#'
#' Present value of a decreasing annuity with payment stream \eqn{n, n-1,
#' \ldots, 1}. Payments occur at times \eqn{1, 2, \ldots, n} for an
#' annuity-immediate, or at \eqn{0, 1, \ldots, n-1} for an annuity-due.
#'
#' @param i Effective interest rate.
#' @param n Number of periods.
#' @param type Payment timing: \code{"immediate"} (default) or \code{"due"};
#'   \code{"arrears"} and \code{"advance"} are the respective aliases.
#'
#' @return A numeric scalar: the present value of the decreasing cash flow.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @note Internally calls \code{\link{presentValue}}.
#' @seealso \code{\link{annuity}}, \code{\link{increasingAnnuity}},
#'   \code{\link{DAxn}}
#' @examples
#' # Present value of 10, 9, 8, ..., 1 payable at the end of each year
#' # for 10 years at 3%:
#' decreasingAnnuity(i = 0.03, n = 10)
#' # Check against the direct sum:
#' sum((10:1) / (1 + 0.03)^(1:10))
#' @export
decreasingAnnuity=function(i, n,type="immediate")
{
	out=NULL
	if(missing(n)) stop("Error! Need number of periods")
	if(missing(i)) stop("Error! Need interest rate")
	type <- testpaymentarg(type)
	
	paymentsSeq=numeric(n)
	timeIds=numeric(n)
	paymentsSeq=seq(from=n, to=1,by=-1)
	timeIds=seq(from=1, to=n, by=1)
	if(type=="due") 
	{
		timeIds=seq(from=0, to=n-1,by=1)
	}
	out=presentValue(cashFlows=paymentsSeq, timeIds=timeIds, interestRates=i)
	
	return(out)
}
#' Increasing annuity-certain
#'
#' Present value of an increasing annuity with payment stream \eqn{1, 2,
#' \ldots, n}. Payments occur at times \eqn{1, 2, \ldots, n} for an
#' annuity-immediate, or at \eqn{0, 1, \ldots, n-1} for an annuity-due.
#'
#' @param i Effective interest rate.
#' @param n Number of periods.
#' @param type Payment timing: \code{"immediate"} (default) or \code{"due"};
#'   \code{"arrears"} and \code{"advance"} are the respective aliases.
#'
#' @return A numeric scalar: the present value of the increasing cash flow.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @note Internally calls \code{\link{presentValue}}.
#' @seealso \code{\link{annuity}}, \code{\link{decreasingAnnuity}},
#'   \code{\link{IAxn}}
#' @examples
#' # Present value of 1, 2, 3, ..., 10 payable at the end of each year
#' # for 10 years at 3%:
#' increasingAnnuity(i = 0.03, n = 10)
#' @export
increasingAnnuity=function(i, n,type="immediate")
{
	out=NULL
	if(missing(n)) stop("Error! Need periods")
	if(missing(i)) stop("Error! Need interest rate")
	type <- testpaymentarg(type)
	
	paymentsSeq=numeric(n)
	paymentsSeq=seq(from=1, to=n,by=1)
	timeIds=seq(from=1, to=n, by=1)
	if(type=="due") 
		{
			timeIds=seq(from=0, to=n-1,by=1)
		}
	out=presentValue(cashFlows=paymentsSeq, timeIds=timeIds, interestRates=i)
#	out=(annuity(i=i, n=n, type="due")-n*(1+i)^-n)/i
#	if(type=="due") out=out*(1+i)
	return(out)
}

#' Accumulated value of an increasing annuity
#'
#' Future (accumulated) value at time \code{n} of an increasing annuity with
#' payment stream \eqn{1, 2, \ldots, n}. Equivalent to capitalising
#' \code{\link{increasingAnnuity}} by \eqn{(1+i)^n}.
#'
#' @param i Effective interest rate.
#' @param n Number of periods.
#' @param type \code{"immediate"} (default) or \code{"due"}.
#'
#' @return A numeric scalar.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @seealso \code{\link{accumulatedValue}}, \code{\link{increasingAnnuity}}
#' @examples
#' Isn(n = 10, i = 0.03)
#' @export
Isn=function(i,n,type="immediate"){
	out=NULL
	out=(1+i)^n*increasingAnnuity(i=i,n=n,type=type)
	return(out)
}

#' Accumulated value of an annuity-certain
#'
#' Future value at time \code{n} of a stream of equally spaced unit payments.
#' With the standard identity
#' \eqn{s_{\overline{n}|} = (1 + i)^n\, a_{\overline{n}|}}, this is a thin
#' wrapper around \code{\link{annuity}}.
#'
#' @param i Effective interest rate.
#' @param n Number of payment periods.
#' @param m Deferment period. Default \code{0}.
#' @param k Number of payments per year. Default \code{1}.
#' @param type Payment timing: \code{"immediate"} (default) or \code{"due"};
#'   \code{"arrears"} and \code{"advance"} are the respective aliases.
#'
#' @return A numeric scalar: the accumulated value.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @seealso \code{\link{annuity}}
#' @examples
#' # Future value of 1 per year, paid in arrears, for 10 years at 3%:
#' accumulatedValue(i = 0.03, n = 10)
#' @export
accumulatedValue=function(i, n, m=0,k=1, type="immediate")
{
	if(is.infinite(n)) return(1/i)
	if(missing(i)) stop("Error! Missing interest rates")
  type <- testpaymentarg(type)
  
#	if(type=="immediate") timeIds=seq(from=1, to=n, by=1)
#	else timeIds=seq(from=0, to=n-1, by=1) #due
#	timeIds=-timeIds
#	iRate=rep(i,length.out=n)
#	out=presentValue(cashFlows=rep(1,length.out=n),i = iRate, timeIds=timeIds)
	out=(1+i)^n*annuity(i=i,n=n,k=k,m=m,type=type)
	return(out)
}

#' Convert between effective, nominal, convertible and discount rates
#'
#' Convert between an effective annual rate (also called "real" rate here)
#' and the corresponding nominal rate convertible \eqn{k}-thly (also called
#' "convertible" rate), for either interest or discount. Convertible rates
#' are synonyms of nominal rates, so \code{convertible2Effective} and
#' \code{effective2Convertible} are thin wrappers of \code{nominal2Real} and
#' \code{real2Nominal} respectively, provided for naming consistency with
#' different textbook traditions.
#'
#' @param i The rate to be converted.
#' @param k The original / target compounding frequency (periods per year).
#' @param type \code{"interest"} (default) for interest rates or
#'   \code{"nominal"} for discount rates. (Historical naming retained for
#'   backward compatibility.)
#'
#' @details
#' With \code{type = "interest"}, \code{nominal2Real(i, k)} returns
#' \eqn{(1 + i/k)^k - 1}; with \code{type = "nominal"} it returns
#' \eqn{1 - (1 - i/k)^k} (the discount analogue). \code{real2Nominal}
#' inverts the respective expression.
#'
#' @return A numeric scalar.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @note Convertible rates are synonyms of nominal rates.
#' @seealso \code{\link{real2Nominal}}
#' @rdname nominal-real-convertible
#' @aliases convertible2Effective real2Nominal nominal2Real effective2Convertible
#'
#' @examples
#' # A nominal annual rate of 0.12 convertible monthly is equivalent to an
#' # effective annual rate of
#' nominal2Real(i = 0.12, k = 12, "interest")
#' @export
nominal2Real=function(i, k=1, type="interest")
{
	out <- NULL
	if(type=="interest") 
	  out <- (1+i/k)^k-1 
	else 
		out<- 1-(1-i/k)^k
	return(out)
}
#' @rdname nominal-real-convertible
#' @export
convertible2Effective=function(i, k=1, type="interest")
{
	return(nominal2Real(i=i,k=k,type=type))
}
#' @rdname nominal-real-convertible
#' @export
real2Nominal=function(i, k=1, type="interest")
{
	if(type=="interest") out=((1+i)^(1/k)-1)*k else
		out=k*(1-(1-i)^(1/k))
	return(out)
}
#' @rdname nominal-real-convertible
#' @export
effective2Convertible=function(i, k=1, type="interest")
{
	return(real2Nominal(i=i,k=k,type=type))
}


#' Convert between interest rate and force of interest
#'
#' These functions switch between an effective interest rate \eqn{i} and the
#' corresponding force of interest (intensity) \eqn{\delta}, using the
#' standard identities \eqn{\delta = \log(1 + i)} and \eqn{i = e^{\delta} - 1}.
#'
#' @param intensity Force of interest (continuous-time intensity).
#'
#' @details
#' Simple financial-mathematics formulas:
#' \deqn{i = e^{\delta} - 1, \qquad \delta = \log(1 + i).}
#'
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @seealso \code{\link{real2Nominal}}, \code{\link{nominal2Real}}
#'
#' @return A numeric scalar.
#'
#' @rdname intensity-interest
#' @aliases interest2Intensity
#'
#' @examples
#' # A force of interest of 0.02 corresponds to an effective rate of
#' intensity2Interest(intensity = 0.02)
#' @export
intensity2Interest=function(intensity)
{
	out=exp(intensity*1)-1
	return(out)
}
#' @rdname intensity-interest
#' @param i interest rate
#' @examples
#' #an interest rate equal to 0.02 corresponds to a force of interest of of 
#' interest2Intensity(i=0.02)
#' @export
interest2Intensity=function(i)
{
	out=log(1+i)
	return(out)
}

#' Convert between interest and discount rates
#'
#' Switch between an effective interest rate \eqn{i} and the corresponding
#' effective discount rate \eqn{d} using the standard identity
#' \deqn{d = \frac{i}{1 + i}, \qquad i = \frac{d}{1 - d}.}
#'
#' @param i Effective interest rate.
#'
#' @return A numeric scalar.
#' @references Broverman, S.A. (2008). \emph{Mathematics of Investment and
#'   Credit} (Fourth Edition), ACTEX Publications.
#' @author Giorgio Alfredo Spedicato
#' @seealso \code{\link{intensity2Interest}}, \code{\link{nominal2Real}}
#' @examples
#' # An effective interest rate of 4% corresponds to a discount rate of
#' interest2Discount(i = 0.04)
#' @rdname discount-interest
#' @aliases discount2Interest
#' @export
interest2Discount=function(i)
{
	return(i/(1+i))
}

#' @param d Effective discount rate.
#' @rdname discount-interest
#' @examples
#' # The inverse direction:
#' discount2Interest(d = 0.04)
#' @export
discount2Interest=function(d)
{
	return(d/(1-d))
}
