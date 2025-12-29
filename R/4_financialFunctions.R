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


#' Present value of a series of cash flows.
#'
#' Evaluate the present value (or actuarial present value when
#' probabilities are provided) of a vector of cash flows occurring at
#' given time instants and discounted by provided interest rates.
#'
#' @title Present value of a series of cash flows
#' @description This function evaluates the present values of a series of cash flows,
#' given occurrence time. Probabilities of occurrence can also be taken into account.
#' @param cashFlows Numeric vector of cash flows. Must be coherent with \code{timeIds}.
#' @param timeIds Numeric vector of time points where \code{cashFlows} are due.
#' @param interestRates A single numeric interest rate or a numeric vector of the same
#' length as \code{timeIds} used to discount cash flows.
#' @param probabilities Optional numeric vector of occurrence probabilities. If
#' missing, a vector of ones (certainty) is assumed.
#' @param power Numeric power applied to discount and cash flows. Defaults to 1.
#' @details \code{probabilities} is optional; when omitted a sequence of 1's with the
#' same length as \code{timeIds} is assumed. Interest rate may be a fixed number
#' or a vector of the same size as \code{timeIds}. The \code{power} parameter is
#' normally unused except in special actuarial evaluations.
#' @return A numeric scalar representing the present value of the cash flow vector,
#' or the actuarial present value if \code{probabilities} are provided.
#' @references Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @note This simple function is the kernel working core of the package. Actuarial and financial mathematics ground on it.
#' @examples
#' # simple example
#' cf <- c(10,10,10)    # $10 payments one per year for three years
#' t  <- c(1,2,3)       # years
#' p  <- c(1,1,1)       # payments certainty
#' presentValue(cashFlows = cf, timeIds = t, interestRates = 0.03, probabilities = p)
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




#' Compute the duration or the convexity of a series of CF
#'
#' @param cashFlows A vector representing the cash flows amounts. 
#' @param timeIds Cash flows times
#' @param i APR interest, i.e. nominal interest rate compounded m-thly.
#' @param k Compounding frequency for the nominal interest rate.
#' @param macaulay Use the Macaulay formula
#' @references Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
#' @details The Macaulay duration is defined  as 
#' \eqn{\sum\limits_t^{T} \frac{t*CF_{t}\left( 1 + \frac{i}{k} \right)^{ - t*k}}{P}}, 
#' while  \eqn{\sum\limits_{t}^{T} t*\left( t + \frac{1}{k} \right) * CF_t \left(1 + \frac{y}{k} \right)^{ - k*t - 2}}

#'
#' @return A numeric value representing either the duration or the convexity of the cash flow series
#' @export
#'
#' @examples
#' #evaluate the duration/convexity of a coupon payment
#' cf=c(10,10,10,10,10,110)
#' t=c(1,2,3,4,5,6)
#' duration(cf, t, i=0.03)
#' convexity(cf, t, i=0.03)
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

#annuity function
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

#decreasing annuity
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
#increasing annuity
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

Isn=function(i,n,type="immediate"){
	out=NULL
	out=(1+i)^n*increasingAnnuity(i=i,n=n,type=type)
	return(out)
}

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

#' @rdname nominal-real-convertible
#' @aliases convertible2Effective
#' @aliases real2Nominal
#' @aliases nominal2Real
#' @title Functions to switch from nominal / effective / convertible rates
#'
#' @param i The rate to be converted.
#' @param k The original / target compounding frequency.
#' @param type Either "interest" (default) or "nominal".
#' 
#' @details \code{effective2Convertible} and \code{convertible2Effective} wrap the other two functions.
#'
#' @return A numeric value.
#' @references Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
#' @note Convertible rates are synonims of nominal rates
#' @seealso \code{\link{real2Nominal}}
#'
#' @examples
#' #a nominal rate of 0.12 equates an APR of
#' nominal2Real(i=0.12, k = 12, "interest")
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


#' @title Functions to switch from interest to intensity and vice versa.
#' @description There functions switch from interest to intensity and vice - versa.
#' @rdname intensity-interest
#' @aliases interest2Intensity
#'
#' @param intensity Intensity rate
#' @details Simple financial mathematics formulas are applied.
#' @references Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
#' @author Giorgio A. Spedicato
#' @seealso \code{\link{real2Nominal}}, \code{\link{nominal2Real}} 
#'
#' @return A numeric value.
#'
#' @examples
#' # a force of interest of 0.02 corresponds to an APR of 
#' intensity2Interest(intensity=0.02)
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

#convert the interest to discount
interest2Discount=function(i)
{
	return(i/(1+i))
}

#convert the discount to interest
discount2Interest=function(d)
{
	return(d/(1-d))
}
